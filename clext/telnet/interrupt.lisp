;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               interrupt.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Manage thread interruption.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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

(defpackage "COM.INFORMATIMAGO.CLEXT.INTERRUPT"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS")
  (:export "INTERRUPT-CONDITION"
           "INTERRUPT-CONDITION-SOURCE"
           "*DEBUG-ON-INTERRUPT*"
           "WITH-INTERRUPT-HANDLER"
           "WITH-RESUME-RESTART"
           "SIGNAL-INTERRUPT"))
(in-package "COM.INFORMATIMAGO.CLEXT.INTERRUPT")

(define-condition interrupt-condition (condition)
  ((source :initarg :source
           :initform nil
           :reader interrupt-condition-source)
   (action :initarg :action
           :initform :resume
           :reader interrupt-condition-action))
  (:report (lambda (condition stream)
             (format stream "Interrupted~@[ from ~A~]"
                     (interrupt-condition-source condition)))))

(defvar *debug-on-interrupt* nil
  "Whether all interruptions should invoke the debugger
\(instead of invoking the RESUME restart).")

(defun interrupt-handler (condition)
  ;; (format t "~S ~A ~%~S = ~A~%"
  ;;         'interrupt-handler condition
  ;;         '*debug-on-interrupt* *debug-on-interrupt*)
  ;; (force-output)
  (if (or *debug-on-interrupt*
          (eql :debug (interrupt-condition-action condition)))
      (break "~@[~:(~A~) ~]Interrupt"
             (interrupt-condition-source condition))
      (invoke-restart (find-restart 'resume condition))))

(defmacro with-interrupt-handler (&body body)
  "Evaluates body in an environment where a handler for the INTERRUPT-CONDITION is set,
to call the INTERRUPT-HANDLER function,
and where a WITH-RESUME-RESTART macro is bound to set up a RESUME restart."
  `(handler-bind ((interrupt-condition
                    (function interrupt-handler)))
     (macrolet ((with-resume-restart ((&optional (format-control "Resume")
                                                 &rest format-arguments)
                                      &body body)
                  `(with-simple-restart (resume ,format-control ,@format-arguments)
                     ,@body)))
       ,@body)))

(declaim (notinline caller))
(defun caller ()
  "Return the name of the function that called the caller of caller."
  #+ccl (third (ccl:backtrace-as-list))
  #-ccl nil)

(defun signal-interrupt (thread &key (action :resume) source)
  "Interrupt the thread with an INTERRUPT-CONDITION initialized with the ACTION and SOURCE.
If ACTION is :RESUME and *DEBUG-ON-INTERRUPT* is false,
the INTERRUPT-HANDLER will invoke the RESUME restart.
If ACTION is :DEBUG or  *DEBUG-ON-INTERRUPT* is true,
the INTERRUPT-HANDLER will invoke the debugger
thru a call to BREAK."
  (check-type action (member :resume :debug))
  (let ((source (or source (caller))))
    (bt:interrupt-thread thread (function signal)
                         (make-condition 'interrupt-condition
                                         :action action
                                         :source source))))

;;;; THE END ;;;;
