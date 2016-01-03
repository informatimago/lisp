;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               activity.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             common-lisp
;;;;USER-INTERFACE:     common-lisp
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2015
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ACTIVITY"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.TIME")
  (:export
   "MAKE-ACTIVITY" "CURRENT-ACTIVITY" "ACTIVITYP" "ACTIVITY-YIELD"
   "ALL-ACTIVITIES" "DESTROY-ACTIVITY" "ACTIVITY-RUN"
   "ACTIVITY-NAME" "ACTIVITY-CLOSURE"
   "ACTIVITY-SCHEDULED-TIME" "ACTIVITY-PERIOD"
   "ACTIVITY-DROPABLE-P" "ACTIVITY-EXACT-P"
   ;; Utility:
   "PRINT-ACTIVITIES" "GET-TIME")
  (:documentation
   "
This package implements a kind of co-routine monitor.

An activity is a closure that is called at specified times (T+k*P).
It should return before processing can go on.  This package is
implemented in pure Common Lisp and allows to schedule independent
\"tasks\" portably, as long as you can split each task in small
chunks, timewise.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2003 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ACTIVITY")



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


(defvar *current-activity* nil
  "The current activity.")

(defvar *scheduler*)



(defgeneric activity-closure (activity)
  (:documentation "RETURN: the closure executed each time the activity is scheduled."))

(defgeneric activity-dropable-p (activity)
  (:documentation "RETURN: Whether the activity should be skipped instead of scheduled too late."))

(defgeneric activity-exact-p (activity)
  (:documentation "RETURN: Whether the activity should be run only on exact time."))

(defgeneric activity-name (activity)
  (:documentation "RETURN: A label for the activity."))

(defgeneric activity-period (activity)
  (:documentation "
RETURN: The period of this activity, expressed in seconds.  If zero,
        then the activity is run as often as possible."))

(defgeneric activity-scheduled-time (activity)
  (:documentation "RETURN: The scheduled time this activity should run."))




(defclass activity ()
  ((id
    :reader activity-id
    :initform (get-next-activity-id)
    :type integer
    :documentation "A unique ID for activities.")
   (name
    :accessor activity-name
    :initarg :name
    :initform "Unnamed"
    :type     string
    :documentation "A label for the activity.")
   (closure
    :accessor activity-closure
    :initarg :closure
    :initform (function (lambda ()))
    :type     function
    :documentation "The closure to be run periodically for this activity.")
   (scheduler
    :accessor activity-scheduler
    :initarg :scheduler
    :initform *scheduler*
    :type scheduler
    :documentation "The scheduler that schedules this activity.")
   (scheduled-time
    :initarg :scheduled-time
    :accessor activity-scheduled-time
    :initform 0
    :type     real
    :documentation "The scheduled time this activity should run.")
   (period
    :accessor activity-period
    :initarg :period
    :initform 0
    :type     real
    :documentation "The period of this activity, expressed in seconds.
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
  (:documentation "An activity to be scheduled.

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


(defmethod print-object ((self activity) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":NAME ~S " (activity-name self))
    (format stream ":scheduled-TIME ~A " (activity-scheduled-time self))
    (format stream ":PERIOD ~A " (activity-period self))
    (format stream ":dropable ~A " (activity-dropable-p self))
    (format stream ":exact ~A " (activity-exact-p self)))
  self)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TIME BASES
;;;
;;; We want universal-time, real-time and run-time, expressed in seconds,
;;; and for real-time, offset to match the universal-time (therefore
;;; being just the universal-time, but with more precision).
;;; Times are expressed in seconds as double-float.
;;;

(deftype time-base ()
  "A value that designates one of the timebases."
  '(member :universal-time :real-time :run-time))


(defgeneric precision (timebase)
  (:documentation "Return the number of seconds (or fraction of a second)
that is the minimum non-zero difference between two calls to GET-TIME.")
  (:method ((timebase t))
    (declare (ignorable timebase))
    (error "Invalid TIMEBASE: ~S" timebase))
  (:method ((timebase (eql :universal-time)))
    (declare (ignorable timebase))
    1.0d0)
  (:method ((timebase (eql :real-time)))
    (declare (ignorable timebase))
    *internal-time-unit*)
  (:method ((timebase (eql :run-time)))
    (declare (ignorable timebase))
    *internal-time-unit*))


(defgeneric get-time (timebase)
  (:documentation "
RETURN:     Current number of seconds since epoch.
TIMEBASE:   :universal-time to get the time from (get-univeral-time),
            :real-time      to get the time from (get-internal-real-time),
            :run-time       to get the time from (get-internal-run-time).
            (in all cases, the time is in number of seconds since the epoch).")
  (:method ((timebase t))
    (declare (ignorable timebase))
    (error "Invalid TIMEBASE: ~S" timebase))
  (:method ((timebase (eql :universal-time)))
    (declare (ignorable timebase))
    (get-universal-time))
  (:method ((timebase (eql :real-time)))
    (declare (ignorable timebase))
    (get-real-time))
  (:method ((timebase (eql :run-time)))
    (declare (ignorable timebase))
    (get-run-time)))


(defgeneric wait-delay (timebase delay)
  (:documentation "Sleep for DELAY seconds.

The default is to use CL:SLEEP.

A 'persistent' scheduler could, when DELAY is big, save the image,
setup the system to be launched again after the DELAY is elapsed
and restart the scheduling then.")
  (:method ((timebase t) delay)
    (declare (ignorable timebase))
    (when (plusp delay) (sleep delay))))



(defmethod get-time ((self activity))
  (get-time (scheduler-time-base (activity-scheduler self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCHEDULER
;;;


#+debug-com.informatimago.common-lisp.cesarum.activity
(defvar *in-terminal-p*
  (and (string/= "dumb" (ext:getenv "TERM"))
       (intersection
        (ensure-list
         (com.informatimago.common-lisp.cesarum.stream:bare-stream
          *terminal-io*))
        (ensure-list
         (com.informatimago.common-lisp.cesarum.stream:bare-stream
          *standard-output*)))))


#+debug-com.informatimago.common-lisp.cesarum.activity
(defun formatalot (control-string &rest arguments)
  (let ((*print-pretty* nil))
    ;; #-debug-com.informatimago.common-lisp.cesarum.activity
    ;; (apply (function format) *trace-output* control-string arguments)
    ;; #+debug-com.informatimago.common-lisp.cesarum.activity
    (dolist (line (com.informatimago.common-lisp.cesarum.string:split-string
                   (apply (function format) nil control-string arguments)
                   #(#\newline)))
      (if *in-terminal-p*
          (format *trace-output*
                  "~A~A~A~A"
                  (map 'string (function code-char)
                    (com.informatimago.common-lisp.cesarum.ecma048:cr))
                  (map 'string (function code-char)
                    (com.informatimago.common-lisp.cesarum.ecma048:el 3))
                  (map 'string (function code-char)
                    (com.informatimago.common-lisp.cesarum.ecma048:cr))
                  line)
          (format *trace-output* "~A~%" line)))))


(defmacro with-debug (&body body)
  #+debug-com.informatimago.common-lisp.cesarum.activity
  `(macrolet ((debug-format (control-string &rest arguments)
                `(formatalot "ACTIVITY: ~A"
                             (format nil ,control-string ,@arguments))))
     ,@body)
  #-debug-com.informatimago.common-lisp.cesarum.activity
  `(macrolet ((debug-format (control-string &rest arguments)
                (declare (ignore control-string arguments))
                `(progn)))
     ,@body))


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


(defgeneric print-scheduler-activities (scheduler &optional stream))
(defgeneric scheduler-all-activities (scheduler))
(defgeneric schedule-activity   (scheduler activity))
(defgeneric unschedule-activity (scheduler activity))
(defgeneric run (scheduler &key one-step until))

(defvar *rescheduled* nil
  "Ugly hack:
If an idle activity creates a new periodic class, or otherwise changes
the schedule, the idle-scheduler needs to yield the hand to it's
time-scheduler.")

(defmethod activity-scheduled-time :after ((activity activity))
  (declare (ignore activity))
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
  (list-elements (scheduler-activities scheduler)))

(defmethod schedule-activity ((scheduler idle-scheduler) (activity activity))
  (setf *rescheduled* t)
  (cond
    ((zerop (activity-period activity))
     (if (scheduler-activities scheduler)
         (push activity (scheduler-activities scheduler))
         (setf (scheduler-activities scheduler) (list activity)
               (cdr (scheduler-activities scheduler)) (scheduler-activities scheduler))))
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


(defmethod run ((self idle-scheduler) &key one-step until)
  "
DO:         Run idle activities.
ONE-STEP:   If true, runs only one activity (or don't sleep).
UNTIL:      Time until which activities must be run.
"
  (with-debug
      (let ((*rescheduled* nil))
        (flet ((run-step ()
                 (let ((activity (pop (scheduler-activities self))))
                   (debug-format "Will run idle activity ~S.~%"
                                 (activity-name activity))
                   (let ((*current-activity* activity))
                     (catch 'activity-yield
                       (funcall (activity-closure activity))))
                   (debug-format "Did  run idle activity ~S.~%"
                                 (activity-name activity))
                   (when (and (activity-scheduler activity)
                              (plusp (activity-period activity)))
                     ;; becomes a periodic activity.
                     (debug-format "Activity ~S changed to periodic.~%"
                                   (activity-name activity))
                     (unschedule-activity self activity)
                     (schedule-activity self activity)))))
          (if one-step
              (cond
                ((or (null until)
                     (< (get-time (scheduler-time-base self)) until))
                 (if (scheduler-activities self)
                     (run-step)
                     (debug-format "No idle activity.~%")))
                (t
                 (debug-format "No time to run idle activities.~%")))
              (cond
                ((null until)
                 ;; idle tasks don't loop
                 (if (scheduler-activities self)
                     (run-step)
                     (debug-format "No idle activity.~%")))
                ((< (get-time (scheduler-time-base self)) until)
                 (loop
                   :while (and (scheduler-activities self)
                               (< (get-time (scheduler-time-base self)) until)
                               (not *rescheduled*))
                   :do (run-step)
                   :finally (when (< (get-time (scheduler-time-base self)) until)
                              (debug-format "No idle activity, will sleep ~A seconds, until ~A.~%"
                                            (- until (get-time (scheduler-time-base self)))
                                            until)
                              (wait-delay (scheduler-time-base self)
                                          (- until (get-time (scheduler-time-base self)))))))
                (t
                 (debug-format "No time to run idle activities.~%"))))))))



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
  (list-elements (scheduler-activities (time-scheduler-idle-scheduler scheduler))))

(defmethod scheduler-all-activities ((scheduler time-scheduler))
  (nconc (time-scheduler-idle-activities scheduler)
         (list-elements (scheduler-activities scheduler))))

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

(defmethod run ((self time-scheduler) &key one-step until)
  "
DO:         Run timed activities.
ONE-STEP:   If true, runs only one activity (or don't sleep).
UNTIL:  Time until which activities must be run.
"
  (with-debug
      (let ((*rescheduled* nil))
        ;; with-slots (idle-activities scheduled-activities time-base) self
        (flet ((run-step ()
                 (let* ((activity       (car (scheduler-activities self)))
                        (scheduled-time (activity-scheduled-time activity))
                        (current-time   (get-time (scheduler-time-base self))))
                   (debug-format "Now is ~D, current activity is ~S~%"
                                 current-time (activity-name activity))
                   (if (< current-time scheduled-time)
                       (progn
                         (debug-format "Running idle scheduler for ~A seconds.~%"
                                       (- scheduled-time current-time))
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
                           (let ((*current-activity* activity))
                             (catch 'activity-yield
                               (funcall (activity-closure activity))))
                           (debug-format "Did  run periodic activity ~S.~%"
                                         (activity-name activity))
                           (when (activity-scheduler activity)
                             (schedule-activity (activity-scheduler activity) activity))
                           (debug-format "Queue after rescheduling:~%")
                           #+debug-com.informatimago.common-lisp.cesarum.activity
                           (print-scheduler-activities self *trace-output*)))))))
          (if one-step
              (run-step)
              (cond
                ((null until)
                 (loop
                   :while (or (scheduler-activities self)
                              (scheduler-activities
                               (time-scheduler-idle-scheduler self)))
                   :do (run-step)))
                ((< (get-time (scheduler-time-base self)) until)
                 (loop
                   :while (and (< (get-time (scheduler-time-base self))
                                  until)
                               (or (scheduler-activities self)
                                   (scheduler-activities
                                    (time-scheduler-idle-scheduler self))))
                   :do (run-step)))
                (t
                 (debug-format "No more time to run activities.~%"))))))))



(defun make-scheduler (time-base)
  (check-type time-base time-base)
  (make-instance 'time-scheduler
                 :time-base time-base
                 :idle-scheduler (make-instance 'idle-scheduler
                                                :time-base time-base)))

(defparameter *scheduler* (make-scheduler :real-time #|:universal-time|#))


(defun activity-run (&key one-step until)
  "
DO:         Runs the scheduler.
ONE-STEP:   If true, runs only one activity (or don't sleep).
UNTIL:      Time (in universal-time seconds) until which activities
            must be run.
"
  (run *scheduler* :one-step one-step :until until))



(defun format-for-field (activities field width precision)
  (if (some (lambda (act) (floatp (funcall field act))) activities)
      ;; ~V,VF
      (format nil "~~~D,~DF" width (truncate (- (log precision 10))))
      (format nil "~~~DD" width)))

(defmethod print-scheduler-activities ((scheduler time-scheduler)
                                       &optional (stream *standard-output*))
  (let ((now (get-time (scheduler-time-base scheduler)))
        (line-cs
          (let ((precision      (precision (scheduler-time-base scheduler)))
                (all-activities (scheduler-all-activities scheduler)))
            (print (list 'all-activities all-activities)) (finish-output)
            (format nil "~~&~~4D ~~8A ~A ~A ~~:[.~~;D~~]~~:[.~~;E~~] ~~S~~%"
                    (format-for-field all-activities
                                      (function activity-scheduled-time)
                                      16 precision)
                    (format-for-field all-activities
                                      (function activity-period)
                                      16 precision))))
        (title-cs  "~&~4A ~8A ~16A ~16A ~:[.~;D~]~:[.~;E~] ~S~%"))
    #+debug-com.informatimago.common-lisp.cesarum.activity
    (when *in-terminal-p*
      (format stream "~A" (map 'string (function code-char)
                            (com.informatimago.common-lisp.cesarum.ecma048:ris))))
    (flet ((line ()
             (format stream
                     "~&~4,,,'-A ~8,,,'-A ~16,,,'-A ~16,,,'-A -- ~32,,,'-A~%"
                     "" "" "" "" "")))
      (line)
      (format stream title-cs
              "ID" "STATE" "SCHEDULED TIME" "PERIOD" "D" "E" "NAME")
      (line)
      ;; TODO: Check if any period is float, and then format with 6 decimal digits.
      (flet ((print-activity (act &optional state)
               (format stream line-cs
                       (activity-id act)
                       (or state (if (eq act *current-activity*)
                                     "CURRENT" "ACTIVE"))
                       (- (activity-scheduled-time act) now)
                       (activity-period act)
                       (activity-dropable-p act)
                       (activity-exact-p act)
                       (activity-name act))))
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
  (values))


(defun print-activities (&optional (stream *standard-output*))
  "
DO:         Prints on the STREAM a formated list of activities.
STREAM:     An output stream to which the list of activities is
            printed.  Defaults to *STANDARD-OUTPUT*.
"
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
  "RETURN: The current activity."
  *current-activity*)


(defun activityp (object)
  "
RETURN: Whether the OBJECT is an instance of the ACTIVITY class (or
        one of its subclasses).
"
  (typep object 'activity))


(defun activity-yield ()
  "
DO:         Returns control to the scheduler.
NOTE:       This may be called from an activity closure to return
            early to the scheduler.
"
  (throw 'activity-yield nil))


(defun all-activities ()
  "
RETURN:  A new list of all the activities.
"
  (scheduler-all-activities *scheduler*))



(defgeneric find-activity-by-id (scheduler activity-id))


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



;;;; THE END ;;;;
