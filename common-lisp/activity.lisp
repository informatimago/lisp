;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               activity.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             common-lisp
;;;;USER-INTERFACE:     common-lisp
;;;;DESCRIPTION
;;;;
;;;;    This package implements a kind of co-routine monitor.
;;;;
;;;;    An activity is a closure that is called at specified times (T+k*P).
;;;;    It should return before processing can go on.  This package is
;;;;    implemented in pure Common Lisp and allows to schedule independent
;;;;    "tasks" portably, as long as you can split each task in small chunks,
;;;;    timewise.
;;;;
;;;;    See the comment at the end of this file for a example of use.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-11-12 <PJB> Improved. Added schedulers and various time-base.
;;;;    2003-03-01 <PJB> Created.
;;;;BUGS
;;;;    When the firsts activities in the queue are past their time by
;;;;    a long time (ie. more than their period), they're rescheduled
;;;;    without taking into account the activities behind that may
;;;;    have shorter periods, therefore we may have a long sleep
;;;;    before normal activity scheduling resumes.
;;;;
;;;;    The state diagram of the activities is not clear.
;;;;    Not intensively tested/valided :-(
;;;;
;;;;    Instead of sleeping, we should have an option to use select/poll
;;;;    (eg socket-status in clisp). See if it can be done with the
;;;;    time-base mechanisms.
;;;;
;;;;    No provision for compatibility with non-threading server options
;;;;    such as UCW's that can work with clisp's socket-status. 
;;;;
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2003 - 2007
;;;;    mailto:pjb@informatimago.com
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************


(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ACTIVITY"
  (:USE "COMMON-LISP")
  (:EXPORT
   "MAKE-ACTIVITY" "CURRENT-ACTIVITY" "ACTIVITYP" "ACTIVITY-YIELD"
   "ALL-ACTIVITIES" "DESTROY-ACTIVITY" "ACTIVITY-RUN"
   "ACTIVITY-NAME" "ACTIVITY-CLOSURE"
   "ACTIVITY-SCHEDULED-TIME" "ACTIVITY-PERIOD"
   "ACTIVITY-DROPABLE-P" "ACTIVITY-EXACT-P"
   ;; Utility:
   "PRINT-ACTIVITIES" "GET-TIME")
  (:DOCUMENTATION
   "This package implements a kind of co-routine monitor.
    Activities are closure that get called periodically by a scheduler.

    Copyright Pascal J. Bourguignon 2003 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ACTIVITY")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACTIVITIES
;;;



;; An activity either:
;; - use universal-time
;; - use internal-real-time
;; - use internal-run-time (we don't use interrupts so
;;                          we cannot schedule precisely).
;; Also, either:
;; - start + as-often-as-possible  (idle tasks)
;; - start + k * period
;;
;; And:
;; - n times
;; - between start-time and end-time
;;
;;
;; (exact count)           (inexact count)         (inexact count)
;; (any time)              (only within period)    (only exact time)
;; (not droppable)         (droppable)             (droppable)
;;
;; |.....|.....|.....|     |.....|.....|.....|     |.....|.....|.....|
;; x     x     x     x     x     x     x     x     x     x     x     x
;;
;; |.....|.....|.....|     |.....|.....|.....|     |.....|.....|.....|
;;   x      x      x x       x      x     x  x       -      -      - x
;;
;; |.....|.....|.....|     |.....|.....|.....|     |.....|.....|.....|
;;      x        x x x          x        - x x          -        - - x
;;
;; x = execute
;; - = drop that time.
;;
;; (Only exact time) allows to execute the tasks only during the first
;; time slice of the period. But then we could have periods starting
;; at different times:
;;
;; |.....|.....|.....|     |.....|.....|.....|     |.....|.....|.....|
;; xyz   xyz   xyz   xyz   xyz   xyz   xyz   xyz   x--   x--   x--   x--   
;;
;; |.....|.....|.....|     |.....|.....|.....|     |.....|.....|.....|
;; x     x     x     x     x     x     x     x     x     x     x     x     
;; .|.....|.....|.....     .|.....|.....|.....     .|.....|.....|.....
;;  y     y     y           y     y     y           y     y     y     
;; ..|.....|.....|....     ..|.....|.....|....     ..|.....|.....|....
;;   z     z     z           z     z     z           z     z     z    
;;

(defun schedule (current-time scheduled-time period dropablep exactp idlep precision)
  "
RETURN: (values runp next-scheduled-time)
NOTE:   exactp => dropablep
        idlep <=> (zerop period)
"
  (when (or (floatp current-time)
            (floatp scheduled-time)
            (floatp period))
    (setf current-time   (truncate current-time   precision)
          scheduled-time (truncate scheduled-time precision)
          period         (truncate period         precision))
    (when (zerop period) (setf period 1)))
  (cond
    ((< current-time scheduled-time)
     (values nil (* scheduled-time precision))) ; not yet
    (idlep
     (values t (* current-time precision)))
    (exactp
     (values (= current-time scheduled-time)
             (* (+ scheduled-time period) precision)))
    (dropablep
     ;; (assert (<= scheduled-time current-time))
     (let ((next-scheduled-time (+ scheduled-time period)))
       (if (< current-time next-scheduled-time)
         (values t   (* next-scheduled-time precision))
         (values nil
                 (* (+ current-time
                       (- period
                          (mod (- current-time scheduled-time) period)))
                    precision)))))
    (t
     ;; (possible (< (+ scheduled-time period) current-time))
     ;; We increment by period to keep the exact count.
     (values t (* (+ scheduled-time period) precision)))))



(let ((next-activity-id 0))
  (defun get-next-activity-id () (incf next-activity-id)))


(DEFVAR *CURRENT-ACTIVITY* NIL
  "The current activity.")

(defvar *scheduler*)

(DEFCLASS ACTIVITY ()
  ((id
    :reader activity-id
    :initform (get-next-activity-id)
    :type integer
    :documentation "A unique ID for activities.")
   (NAME
    :ACCESSOR ACTIVITY-NAME
    :INITARG :NAME
    :INITFORM "Unnamed"
    :TYPE     STRING
    :DOCUMENTATION "A label for the activity.")
   (CLOSURE
    :ACCESSOR ACTIVITY-CLOSURE
    :INITARG :CLOSURE
    :INITFORM (FUNCTION (LAMBDA ()))
    :TYPE     FUNCTION
    :DOCUMENTATION "The closure to be run periodically for this activity.")
   (scheduler
    :accessor activity-scheduler
    :initarg :scheduler
    :initform *scheduler*
    :type scheduler
    :documentation "The scheduler that schedules this activity.")
   (SCHEDULED-TIME
    :initarg :scheduled-time
    :ACCESSOR ACTIVITY-SCHEDULED-TIME
    :INITFORM 0
    :TYPE     real
    :DOCUMENTATION "The scheduled time this activity should run")
   (PERIOD
    :ACCESSOR ACTIVITY-PERIOD
    :INITARG :PERIOD
    :INITFORM 0
    :TYPE     real
    :DOCUMENTATION "The period of this activity, expressed in seconds.
If zero, then the activity is run as often as possible.")
   (dropable
    :accessor activity-dropable-p
    :initarg :dropable
    :initform nil
    :type     boolean
    :documentation
    "Whether the activity should be skipped instead of scheduled too late.")
   (exact
    :accessor activity-exact-p
    :initarg :exact
    :initform nil
    :type     boolean
    :documentation "Whether the activity should be run only on exact time."))
  (:DOCUMENTATION "An activity to be scheduled.

"))


(defmethod initialize-instance :after ((self activity) &rest args
                                       &key (start-in 0 start-in-p)
                                       (start-at 0 start-at-p)
                                       &allow-other-keys)
  (declare (ignore args))
  (when (and start-in-p start-at-p)
    (error ":START-IN and :START-AT are mutually exclusive."))
  (cond
    (start-in-p (setf (slot-value self 'next-time)
                      (+ (get-universal-time) start-in)))
    (start-at-p (setf (slot-value self 'next-time) start-at)))
  self)


(DEFMETHOD PRINT-OBJECT ((SELF ACTIVITY) STREAM)
  (PRINT-UNREADABLE-OBJECT (SELF STREAM :TYPE T :IDENTITY T)
    (FORMAT STREAM ":NAME ~S " (ACTIVITY-NAME SELF))
    (FORMAT STREAM ":scheduled-TIME ~A " (ACTIVITY-scheduled-TIME SELF))
    (FORMAT STREAM ":PERIOD ~A " (ACTIVITY-PERIOD SELF))
    (FORMAT STREAM ":dropable ~A " (ACTIVITY-dropable-p SELF))
    (FORMAT STREAM ":exact ~A " (ACTIVITY-exact-p SELF)))
  SELF)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TIME BASES
;;;
;;; We want universal-time, real-time and run-time, expressed in seconds,
;;; and for real-time, offset to match the universal-time (therefore
;;; being just the universal-time, but with more precision).
;;; Times are expressed in seconds as double-float.
;;;

(defparameter *internal-time-unit*
  (coerce (/ INTERNAL-TIME-UNITS-PER-SECOND) 'double-float)
  "The internal time slice, in seconds, as a DOUBLE-FLOAT.")

(defparameter *precise-real-time-offset*
  (loop
     :with now = (get-universal-time)
     :while (= now (get-universal-time))
     :finally (return
                (- now (* (GET-INTERNAL-REAL-TIME) *internal-time-unit*))))
  "Contains the number of seconds that must be added to:
      (/ (GET-INTERNAL-REAL-TIME) INTERNAL-TIME-UNITS-PER-SECOND)
to get the current universal-time with the higher internal time precision.")

(defun get-real-time ()
  "
RETURN: The universal-time (in seconds), offset by the
        internal-real-time fraction.
"
  (+ *precise-real-time-offset*
     (* (GET-INTERNAL-REAL-TIME) *internal-time-unit*)))

(defun get-run-time ()
  (* (get-internal-run-time) *internal-time-unit*))


(deftype time-base ()
  "A value that designates one of the timebases."
  '(member :universal-time :real-time :run-time))

(defgeneric precision (timebase)
  (:documentation "Return the number of seconds (or fraction of a second)
that is the minimum non-zero difference between two calls to GET-TIME.")
  (:method ((timebase t)) (error "Invalid TIMEBASE: ~S" timebase))
  (:method ((timebase (eql :universal-time))) 1.0d0)
  (:method ((timebase (eql :real-time)))      *internal-time-unit*)
  (:method ((timebase (eql :run-time)))       *internal-time-unit*))

(defgeneric get-time (timebase)
  (:documentation "Return current number of seconds since epoch.")
  (:method ((timebase t)) (error "Invalid TIMEBASE: ~S" timebase))
  (:method ((timebase (eql :universal-time))) (get-universal-time))
  (:method ((timebase (eql :real-time)))      (get-real-time))
  (:method ((timebase (eql :run-time)))       (get-run-time)))

(defgeneric wait-delay (timebase delay)
  (:documentation "Sleep for DELAY seconds.

The default is to use CL:SLEEP.

A 'persistent' scheduler could, when DELAY is big, save the image,
setup the system to be launched again after the DELAY is elapsed
and restart the scheduling then.")
  (:method ((timebase t) delay) (when (plusp delay) (sleep delay))))



(defmethod get-time ((self activity))
  (get-time (scheduler-time-base (activity-scheduler self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCHEDULER
;;;

(defvar *debug* nil)

#+debug-com.informatimago.common-lisp.activity
(defvar *in-terminal-p*
  (and (string/= "dumb" (ext:getenv "TERM"))
       (intersection
        (COM.INFORMATIMAGO.COMMON-LISP.LIST:ENSURE-LIST
         (COM.INFORMATIMAGO.COMMON-LISP.STREAM:bare-stream
          *terminal-io*))
        (COM.INFORMATIMAGO.COMMON-LISP.LIST:ENSURE-LIST
         (COM.INFORMATIMAGO.COMMON-LISP.STREAM:bare-stream
          *standard-output*)))))

(defun formatalot (control-string &rest arguments)
  (let ((*print-pretty* nil))
    #-debug-com.informatimago.common-lisp.activity
    (apply (function format) *trace-output* control-string arguments)
    #+debug-com.informatimago.common-lisp.activity
    (dolist (line (com.informatimago.common-lisp.string:split-string
                   (apply (function format) nil control-string arguments)
                   #(#\newline)))
      (if *in-terminal-p*
          (format *trace-output*
                  "~A~A~A~A"
                  (map 'string (function code-char)
                       (COM.INFORMATIMAGO.COMMON-LISP.ECMA048:CR))
                  (map 'string (function code-char)
                       (COM.INFORMATIMAGO.COMMON-LISP.ECMA048:EL 3))
                  (map 'string (function code-char)
                       (COM.INFORMATIMAGO.COMMON-LISP.ECMA048:CR))
                  line)
          (format *trace-output* "~A~%" line)))))


(defmacro with-debug (debug &body body)
  (if debug
      `(macrolet
           ((debug-format (control-string &rest arguments)
              `(formatalot "ACTIVITY: ~A"
                                   (format nil ,control-string ,@arguments))))
         ,@body)
      `(macrolet ((debug-format (control-string &rest arguments)
                    `(progn)))
         ,@body)))


(defclass scheduler ()
  ((activities
    :accessor scheduler-activities
    :initarg :activities
    :initform '()
    :type list                ; of activity (possibly a circular list)
    :documentation "The list of activities scheduled by this scheduler.")
   (time-base
    :accessor scheduler-time-base
    :initarg :time-base
    :initform :universal-time
    :type time-base
    :documentation "The time-base used by this scheduler."))
  (:documentation "The base class for schedulers."))

(defgeneric PRINT-scheduler-ACTIVITIES (scheduler &OPTIONAL STREAM))
(defgeneric scheduler-all-activities (scheduler))
(defgeneric schedule-activity   (scheduler activity))
(defgeneric unschedule-activity (scheduler activity))
(defgeneric run (scheduler &key one-step until))

(defvar *rescheduled* nil
  "Ugly hack:
If an idle activity creates a new periodic class, or otherwise changes
the schedule, the idle-scheduler needs to yield the hand to it's
time-scheduler.")

(defmethod ACTIVITY-SCHEDULED-TIME :after ((activity activity))
  (setf *rescheduled* t))


(defclass idle-scheduler (scheduler)
  ((main-scheduler
    :accessor idle-scheduler-main-scheduler
    :initarg :main-scheduler
    :initform nil
    :type (or null scheduler)
    :documentation "The scheduler that should be given the activities that
are not idle activities anymore (ie with period>0."))
  (:documentation "The Idle Scheduler just runs the activities
in round-robin fashion for "))

(defmethod scheduler-all-activities ((scheduler idle-scheduler))
  (copy-list (scheduler-activities scheduler)))

(defmethod schedule-activity ((scheduler idle-scheduler) (activity activity))
  (setf *rescheduled* t)
  (cond
    ((zerop (activity-period activity))
     (if (scheduler-activities scheduler)
         (push activity (scheduler-activities scheduler))
         (setf (scheduler-activities scheduler) (list activity)
               (cdr (scheduler-activities scheduler))
               (scheduler-activities scheduler))))
    ((idle-scheduler-main-scheduler scheduler)
     (schedule-activity (idle-scheduler-main-scheduler scheduler)
                        activity)))
  ;; otherwise silently ignore it.
  scheduler)

(defmethod unschedule-activity ((scheduler idle-scheduler) (activity activity))
  (let ((queue (scheduler-activities scheduler)))
    ;; let's remove activity from the circular list queue.
    (cond
      ((null queue))                    ; nothing to do
      ((eq (cdr queue) queue)
       (when (eq (car queue) activity)
         (setf queue nil
               (activity-scheduler activity) nil)))
      (t (loop
            :for cur :on (cdr queue)
            :until (or (eq (car cur) activity)
                       (eq cur queue))
            :finally (when (eq (car cur) activity)
                       (setf (cdr cur) (cddr cur)
                             (activity-scheduler activity) nil)))))
    (setf (scheduler-activities scheduler) queue))
  scheduler)

(defmethod run ((self idle-scheduler) &KEY ONE-STEP until)
  "
DO:         Run idle activities.
ONE-STEP:   If true, runs only one activity (or don't sleep).
UNTIL:  Time until which activities must be run.
*DEBUG*:    Print debugging information.
"
  (let ((*rescheduled* nil))
    (macrolet ((run-step (debug)
                 `(with-debug ,debug
                    (LET ((activity (pop (scheduler-activities self))))
                      (debug-format "Will run idle activity ~S.~%"
                                    (activity-name activity))
                      (let ((*CURRENT-ACTIVITY* activity))
                        (catch 'activity-yield
                          (FUNCALL (ACTIVITY-CLOSURE activity))))
                      (debug-format "Did  run idle activity ~S.~%"
                                    (activity-name activity))
                      (when (and (activity-scheduler activity)
                                 (plusp (activity-period activity)))
                        ;; becomes a periodic activity.
                        (debug-format "Activity ~S changed to periodic.~%"
                                      (activity-name activity))
                        (unschedule-activity self activity)
                        (schedule-activity self activity))))))
      (if one-step
          (cond
            ((or (null until)
                 (< (get-time (scheduler-time-base self)) until))
             (if *debug*
                 (with-debug t
                   (if (scheduler-activities self)
                       (run-step t)
                       (debug-format "No idle activity.~%")))
                 (when (scheduler-activities self)
                   (run-step nil))))
            (t
             (when *debug*
               (with-debug t
                 (debug-format "No time to run idle activities.~%")))))
          (cond
            ((null until)
             (if *debug*
                 ;; idle tasks don't loop
                 (if (scheduler-activities self)
                     (run-step t)
                     (with-debug t (debug-format "No idle activity.~%")))
                 (run-step nil)))
            ((< (get-time (scheduler-time-base self)) until)
             (if *debug*
                 (with-debug t
                   (loop
                      :while (and (scheduler-activities self)
                                  (< (get-time (scheduler-time-base self)) until)
                                  (not *rescheduled*))
                      :do (run-step t)
                      :finally
                      (when (< (get-time (scheduler-time-base self)) until)
                        (debug-format
                         "No idle activity, will sleep ~A seconds, until ~A.~%"
                         (- until (get-time (scheduler-time-base self)))
                         until)
                        (wait-delay (scheduler-time-base self)
                                    (- until (get-time (scheduler-time-base self)))))))
                 (loop
                    :while (and (scheduler-activities self)
                                (< (get-time (scheduler-time-base self)) until)
                                (not *rescheduled*))
                    :do (run-step nil)
                    :finally
                    (when (< (get-time (scheduler-time-base self)) until)
                      (wait-delay (scheduler-time-base self)
                                  (- until (get-time (scheduler-time-base self))))))))
            (t
             (when *debug*
               (with-debug t
                 (debug-format "No time to run idle activities.~%")))))))))



(defclass time-scheduler (scheduler)
  ((idle-scheduler
    :accessor time-scheduler-idle-scheduler
    :initarg :idle-scheduler
    :type scheduler
    :documentation
    "The scheduler used when there's nothing to run for a while."))
  (:documentation "The time scheduler runs activities at given times."))

(defgeneric time-scheduler-idle-activities (scheduler))

(defmethod time-scheduler-idle-activities ((scheduler time-scheduler))
  (scheduler-activities (time-scheduler-idle-scheduler scheduler)))

(defmethod scheduler-all-activities ((scheduler time-scheduler))
  (append (time-scheduler-idle-activities scheduler)
          (copy-list (scheduler-activities scheduler))))

(defmethod schedule-activity ((scheduler time-scheduler) (activity activity))
  "
DO:  Insert the activity in the right scheduler queue at the right place.

DO:             COMPUTE THE NEW NEXT-TIME, TRYING TO AVOID TIME DRIFT,
                BUT ENSURING THAT NEXT-TIME IS IN THE PRESENT-FUTURE.
SEE ALSO:       SCHEDULE.
PRE:            (= LAST (NEXT-TIME SELF))
                (= NOW  (GET-UNIVERSAL-TIME))
POST:           (MAX NOW (= (NEXT-TIME SELF) (+ LAST (activity-PERIOD SELF))))
"
  (setf *rescheduled* t)
  (if (plusp (activity-period activity))
      (let ((queue (cons nil (scheduler-activities scheduler)))
            (next-time (activity-scheduled-time activity)))
        (do ((place queue (cdr place)))
            ((or (null (cdr place))
                 (< next-time (activity-scheduled-time (cadr place))))
             (push activity (cdr place))))
        (setf (scheduler-activities scheduler) (cdr queue)))
      (schedule-activity (time-scheduler-idle-scheduler scheduler) activity))
  scheduler)

(defmethod unschedule-activity ((scheduler time-scheduler) (activity activity))
  (setf *rescheduled* t)
  (setf (scheduler-activities scheduler)
        (delete activity (scheduler-activities scheduler))
        (activity-scheduler activity) nil)
  scheduler)

(defmethod run ((self time-scheduler) &KEY ONE-STEP until)
  "
DO:         Run timed activities.
ONE-STEP:   If true, runs only one activity (or don't sleep).
UNTIL:  Time until which activities must be run.
*DEBUG*:    Print debugging information.
"
  (let ((*rescheduled* nil))
    (with-slots (idle-activities scheduled-activities time-base) self
      (macrolet
          ((run-step (debug)
             `(with-debug ,debug
                (LET* ((activity       (CAR (scheduler-activities self)))
                       (scheduled-TIME (ACTIVITY-scheduled-TIME activity))
                       (CURRENT-TIME   (get-time (scheduler-time-base self))))
                  (debug-format "Now is ~D, current activity is ~S~%"
                                CURRENT-TIME (activity-name activity))
                  (if (< CURRENT-TIME scheduled-time)
                      (progn
                        (debug-format "Running idle scheduler for ~A seconds.~%"
                                      (- scheduled-time CURRENT-TIME))
                        (run (time-scheduler-idle-scheduler self)
                             :until scheduled-time))
                      (multiple-value-bind (runp next-scheduled-time)
                          (schedule current-time
                                    scheduled-time
                                    (activity-period activity)
                                    (activity-dropable-p activity)
                                    (activity-exact-p activity)
                                    (zerop (activity-period activity))
                                    (precision (scheduler-time-base self)))
                        (setf (activity-scheduled-time activity) next-scheduled-time)
                        (when runp
                          (setf (scheduler-activities self)
                                (cdr (scheduler-activities self)))
                          (debug-format "Will run periodic activity ~S.~%"
                                        (activity-name activity))
                          (let ((*CURRENT-ACTIVITY* activity))
                            (catch 'activity-yield
                              (FUNCALL (ACTIVITY-CLOSURE activity))))
                          (debug-format "Did  run periodic activity ~S.~%"
                                        (activity-name activity))
                          (when (activity-scheduler activity)
                            (schedule-activity (activity-scheduler activity) activity))
                          (debug-format "Queue after rescheduling:~%")
                          ,(when debug
                                 `(PRINT-scheduler-ACTIVITIES self *TRACE-OUTPUT*)))))))))
        (if ONE-STEP
            (if *debug*
                (run-step t)
                (run-step nil))
            (cond
              ((null until)
               (if *debug*
                   (LOOP
                      :while (or (scheduler-activities self)
                                 (scheduler-activities
                                  (time-scheduler-idle-scheduler self)))
                      :do (run-step t))
                   (LOOP
                      :while (or (scheduler-activities self)
                                 (scheduler-activities
                                  (time-scheduler-idle-scheduler self)))
                      :do (run-step nil))))
              ((< (get-time (scheduler-time-base self)) until)
               (if *debug*
                   (LOOP
                      :while (and (< (get-time (scheduler-time-base self))
                                     until)
                                  (or (scheduler-activities self)
                                      (scheduler-activities
                                       (time-scheduler-idle-scheduler self))))
                      :do (run-step t))
                   (LOOP
                      :while (and (< (get-time (scheduler-time-base self))
                                     until)
                                  (or (scheduler-activities self)
                                      (scheduler-activities
                                       (time-scheduler-idle-scheduler self))))
                      :do (run-step nil))))
              (t
               (when *debug*
                 (with-debug t
                   (debug-format "No more time to run activities.~%"))))))))))




(defun make-scheduler (time-base)
  (check-type time-base time-base)
  (make-instance 'time-scheduler
      :time-base time-base
      :idle-scheduler (make-instance 'idle-scheduler
                          :time-base time-base)))

(defparameter *scheduler* (make-scheduler :real-time #|:universal-time|#))


(DEFUN ACTIVITY-RUN (&KEY ONE-STEP until)
  (run *scheduler* :one-step one-step :until until))



(defun format-for-field (activities field width precision)
  (if (some (lambda (act) (floatp (funcall field act))) activities)
    ;; ~V,VF
    (format nil "~~~D,~DF" width (truncate (- (log precision 10))))
    (format nil "~~~DD" width)))

(defmethod PRINT-scheduler-ACTIVITIES ((scheduler time-scheduler)
                                        &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  (LET ((NOW (get-time (scheduler-time-base scheduler)))
        (line-cs
         (let ((precision      (precision (scheduler-time-base scheduler)))
               (all-activities (scheduler-all-activities scheduler)))
           (format nil "~~&~~4D ~~8A ~A ~A ~~:[.~~;D~~]~~:[.~~;E~~] ~~S~~%"
                   (format-for-field all-activities
                                     (function activity-scheduled-time)
                                     16 precision)
                   (format-for-field all-activities
                                     (function activity-period)
                                     16 precision))))
        (title-cs  "~&~4A ~8A ~16A ~16A ~:[.~;D~]~:[.~;E~] ~S~%"))
    #+debug-com.informatimago.common-lisp.activity
    (when *in-terminal-p*
      (format stream "~A" (map 'string (function code-char)
                               (COM.INFORMATIMAGO.COMMON-LISP.ECMA048:RIS))))
    (flet ((line ()
             (FORMAT stream
                     "~&~4,,,'-A ~8,,,'-A ~16,,,'-A ~16,,,'-A -- ~32,,,'-A~%"
                     "" "" "" "" "")))
      (line)
      (FORMAT stream title-cs
              "ID" "STATE" "SCHEDULED TIME" "PERIOD" "D" "E" "NAME")
      (line)
      ;; TODO: Check if any period is float, and then format with 6 decimal digits.
      (flet ((print-activity (act &optional state)
               (FORMAT STREAM line-cs
                       (activity-id act)
                       (or state (if (EQ ACT *CURRENT-ACTIVITY*)
                                     "CURRENT" "ACTIVE"))
                       (- (ACTIVITY-scheduled-TIME ACT) NOW)
                       (ACTIVITY-PERIOD ACT)
                       (activity-dropable-p act)
                       (activity-exact-p act)
                       (ACTIVITY-NAME ACT))))
        (loop
           :with queue = (if *current-activity*
                             (if (member *current-activity*
                                         (scheduler-activities scheduler))
                                 (scheduler-activities scheduler)
                                 (cons *current-activity*
                                       (scheduler-activities scheduler)))
                             (scheduler-activities scheduler))
           :for acts :on queue
           :for act = (car acts)
           :do (print-activity act)
           :until (eq (cdr acts) queue))
        (loop
           :with queue =  (scheduler-activities
                           (time-scheduler-idle-scheduler scheduler))
           :for acts :on queue
           :for act = (car acts)
           :do (print-activity act "IDLE")
           :until (eq (cdr acts) queue)))
      (line)))
  (VALUES))


(defun print-activities (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  (print-scheduler-activities *scheduler* stream))


(defun make-activity (function &key (name "Unnamed activity")
                      (start-at 0 start-at-p)
                      (start-in 0 start-in-p)
                      (period 1)
                      (dropable nil)
                      (exact nil)
                      (idle nil)
                      ((:scheduler *scheduler*) *scheduler*))
  "
FUNCTION:   A closure, that will be repeatitively called at specified times.
NAME:       A string, naming the activity.
PERIOD:     A real indicating the period of the activity.
            If 0 then run it when idle.
START-AT:   (mutually exclusive with START-IN)
            The first universal-time at which the activity should run.
START-IN:   (mutually exclusive with START-AT)
            The number of seconds from now when the activity should be
            run first.
            If none of START-AT or START-IN is specified, the start
            time is now.
*SCHEDULER* The scheduler used to run this activity.
"
  (when (and start-in-p start-at-p)
    (error ":START-IN and :START-AT are mutually exclusive."))
  (let ((activity
         (make-instance 'activity
             :name name
             :closure function
             :scheduler (if idle
                            (time-scheduler-idle-scheduler *scheduler*)
                            *scheduler*)
             :scheduled-time (cond
                               (start-in-p (+ (get-time (scheduler-time-base
                                                         *scheduler*))
                                              start-in))
                               (start-at-p start-at)
                               (t          (get-time (scheduler-time-base
                                                      *scheduler*))))
             :period period
             :dropable dropable
             :exact exact)))
    (schedule-activity (activity-scheduler activity) activity)
    activity))

(defun current-activity ()
  *current-activity*)

(defun activityp (object)
  (typep object 'activity))

(defun activity-yield ()
  (throw 'activity-yield nil))

(defun all-activities ()
  "
RETURN:  A new list of all the activities.
"
  (scheduler-all-activities *scheduler*))

(defgeneric FIND-ACTIVITY-BY-ID (scheduler activity-id))

(defmethod find-activity-by-id ((scheduler idle-scheduler) id)
  (find id (scheduler-activities scheduler) :key (function activity-id)))

(defmethod find-activity-by-id ((scheduler time-scheduler) id)
  (or (find id (scheduler-activities scheduler) :key (function activity-id))
      (find id (scheduler-activities
                (time-scheduler-idle-scheduler scheduler))
            :key (function activity-id))))

(defun destroy-activity (activity)
  "
DO: Remove the activity from the scheduling queue.
"
  (let ((scheduler (activity-scheduler activity)))
    (if (integerp activity)
        (unschedule-activity scheduler (find-activity-by-id scheduler activity))
        (unschedule-activity scheduler activity)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing and Debugging

(define-condition debugger-invocation (condition)
  ((format-control :accessor debugger-invocation-format-control
                   :initarg :format-control
                   :initform "Debugger invocation"
                   :type string)
   (format-arguments :accessor debugger-invocation-format-arguments
                     :initarg :format-arguments
                     :initform '()
                     :type list))
  (:documentation
   "SBCL expects for INVOKE-DEBUGGER, objects of type CL:CONDITION,
not mere 'condition' objects.")
  (:report (lambda (condition stream)
             (apply (function format) stream
                    (debugger-invocation-format-control condition)
                    (debugger-invocation-format-arguments condition)))))

(defun cdebugger (&optional (reason "User request"))
  (restart-case
      (invoke-debugger (make-condition 'debugger-invocation
                                       :format-control "~A"
                                       :forma-arguments (list reason)))
    (continue ()
      :report "Continue"
      (return-from cdebugger))))


(defmacro define-menu (name title &rest items)
  `(defun ,name ()
     (loop
        (flet ((exit-menu-loop (&optional result)
                 (return-from ,name result)))
          (block try-again
            (format *query-io* "~2%Menu ~A~2%" ,title)
            (format *query-io* "~:{   ~A) ~A~%~}" ',items)
            (format *query-io* "~%Your choice: ")
            (let ((choice (string-trim " " (read-line *query-io*))))
              (format *query-IO* "~%")
              (case (or (and (string= "" choice)
                             (let ((item (find :default ',items
                                               :key (function fourth))))
                               (if item
                                   (first item)
                                   (progn
                                     (format *query-io* "~%Invalid choice~%")
                                     (return-from try-again)))))
                        (aref choice 0))
                ,@(mapcar (lambda (item) `((,(first item)) ,(third item))) items)
                (otherwise (format *query-io* "~%Invalid choice~%") ))))))))

(define-menu act-created-menu
    "Activity Created"
  (#\g "Go on"               (exit-menu-loop)                  :default)
  (#\d "Invoke the debugger" (block debugger
                               (restart-case
                                 (invoke-debugger
                                  (make-condition 'debugger-invocation
                                                  :format-control "User request"))
                                 (menu ()
                                       :report "Back to the menu"
                                       (return-from debugger))
                                 (goon ()
                                   :report "Go on"
                                   (exit-menu-loop)))))
  (#\p "Print activities"    (print-scheduler-activities *scheduler*)))


(defun test (&key debug)
  (let ((start (get-universal-time)))
    (macrolet
        ((run (&body body)
           `(lambda ()
              (formatalot "~12D :name ~30S :period ~3D~%"
                          (- (get-universal-time) start)
                          (activity-name   (current-activity))
                          (activity-period (current-activity)))
              ,@body))
         (mkact (&rest args)
           `(progn
              (when debug
                (formatalot "Before creating a new ")
                (print-scheduler-activities *scheduler*)
                (formatalot "Let's create the new activity."))
              (prog1 (make-activity ,@args)
                (when debug
                  (print-scheduler-activities *scheduler*)
                  (act-created-menu))))))
      (format t "~%")
      (mkact (run (return-from test))
             :name "stopper"
             :start-in 60)
      (mkact (run
              ;; (cdebugger "Check increment period...")
              (incf (activity-period (current-activity))))
             :name "period increasing from 0"
             :period 0)
      (mkact (let ((times 11))
               (run
                (let ((act (current-activity)))
                  (case (decf times)
                    ((10)
                     (setf (activity-period act) 30))
                    ((9)
                     (setf (activity-period act) 2)
                     (setf (activity-scheduled-time act)
                           (+ (get-time act) 2)))
                    ((0)
                     (destroy-activity act))))))
             :name "period 2 between 30 and 50"
             :period 30)
      (mkact (run)
             :name "period 10"
             :period 10)
      (mkact (run)
             :name "period 7"
             :period 7)
      (mkact (run)
             :name "period 5"
             :period 5)
      (mkact (run)
             :name "period 5'"
             :period 5)
      (let ((activity (mkact (run)
                             :name "period 5\", to be destroyed in 15s"
                             :period 5)))
        (mkact (run (if (activity-scheduler activity)
                        (destroy-activity activity)
                        (destroy-activity (current-activity))))
               :name "Destroyer of [period 5\", to be destroyed in 15s]"
               :start-in 15)))
    (PRINT-scheduler-ACTIVITIES *scheduler*)
    (let ((*debug* debug)) (activity-run))
    (values)))



;;;; THE END ;;;;
