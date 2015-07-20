;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               kill-thread.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Exports threads manipulation commands.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-23 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(defpackage "COM.INFORMATIMAGO.TOOLS.THREAD"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS")
  (:export "LIST-THREADS" "KILL-THREAD"
            "PERIODICALLY" "DO-PERIODICALLY" "DONE"))
(in-package "COM.INFORMATIMAGO.TOOLS.THREAD")

(defun periodically (period thunk &key (name "Peridic Task") initially finally)
  (bt:make-thread (lambda ()
                    (when initially (funcall initially))
                    (catch :exit-periodically
                      (loop (sleep period) (funcall thunk)))
                    (when finally (funcall finally)))
                  :name name
                  :initial-bindings (list (cons '*standard-output* *standard-output*)
                                          (cons '*standard-input* *standard-input*)
                                          (cons '*error-output* *error-output*)
                                          (cons '*trace-output* *trace-output*)
                                          (cons '*terminal-io* *terminal-io*))))

(defmacro do-periodically ((period &key (name "Periodic Task") initially finally)
                           &body body)
  `(periodically ,period (flet ((done () (throw :exit-periodically nil)))
                           (lambda () ,@body))
                 :name ,name
                 :initially (lambda () ,initially)
                 :finally (lambda () ,finally)))

(defun list-threads (&optional (threads (bt:all-threads)) (*standard-output* *standard-output*))
  (loop
    :named menu
    :for i :from 1
    :for thread :in threads
    :do (format *standard-output* "~&~2D) ~A~%" i thread))
  (values))

(defun kill-thread (&optional thread (*query-io* *query-io*))
  (if thread
      (bt:destroy-thread thread)
      (loop
        :named select
        :do (let ((threads (bt:all-threads)))
              (list-threads threads *query-io*)
              (format *query-io* "~&Number of thread to kill (or 0 to abort): ")
              (let ((choice (let ((*read-eval* nil)) (read *query-io*))))
                (cond
                  ((not (integerp choice)))
                  ((zerop choice)
                   (format *query-io* "~&Aborted.~%")
                   (return-from select))
                  ((<= 1 choice (length threads))
                   (bt:destroy-thread (nth (1- choice) threads))
                   (return-from select)))
                (format *query-io* "~&Invalid answer, try again.~%")))))
  (values))


;; (loop :repeat 3 :do (bt:make-thread (lambda () (sleep 2232))))
;; (kill-thread)

;;;; THE END ;;;;

