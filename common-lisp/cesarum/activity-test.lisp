;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               activity-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test activity.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-03-01 <PJB> Extracted from activity.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ACTIVITY.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ACTIVITY")
  (:export "TEST/ALL" "INTERACTIVE-TEST"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ACTIVITY.TEST")


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
             (format *query-io* "~%")
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


(defun interactive-test (&key debug)
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
    (print-scheduler-activities *scheduler*)
    (activity-run)
    (values)))


(define-test test/all ()
  )


;;;; THE END ;;;;
