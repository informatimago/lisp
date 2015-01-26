;;;;**************************************************************************
;;;;FILE:               count-fork.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Testing fork and signals in clisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-05-02 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2006 - 2015
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
;;;;**************************************************************************

(defparameter *run* t)


(defun install-signal-handler (signum handler)
  (let ((oldhan (linux:|set-signal-handler| signum handler))
        (sigset (second (multiple-value-list
                          (linux:|sigaddset| (second (multiple-value-list
                                                      (linux:|sigemptyset|)))
                                 signum)))))
    (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| sigset)
    (values signum oldhan sigset)))


(defun restore-signal-handler (signum oldhan sigset)
  (linux:|set-signal-handler| signum oldhan)
  (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| sigset))

  
(defmacro with-signal-handler (signum handler &body body)
  (let ((voldhan (gensym))
        (vsignum (gensym))
        (vsigset (gensym)))
    `(let* ((,vsignum ,signum)
            (,voldhan (linux:|set-signal-handler| ,vsignum ,handler))
            (,vsigset (second (multiple-value-list
                               (linux:|sigaddset| 
                                      (second (multiple-value-list
                                               (linux:|sigemptyset|)))
                                      ,vsignum)))))
      (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| ,vsigset)
      (unwind-protect (progn ,@body)
        (linux:|set-signal-handler| ,vsignum ,voldhan)
        (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| ,vsigset)))))


(ffi:def-c-struct itimerval
  (interval-sec  ffi:long)
  (interval-mic  ffi:long)
  (value-sec     ffi:long)
  (value-mic     ffi:long))

(defconstant +itimer-real+    0)
(defconstant +itimer-virtual+ 1)
(defconstant +itimer-prof+    2)

(ffi:def-call-out
 setitimer
 (:name "setitimer")
 (:arguments (which ffi:int :in) (value (ffi:c-ptr itimerval) :in)
  (ovalue (ffi:c-ptr-null itimerval) :in))
 (:return-type ffi:int)
 (:language :stdc)
 (:library "/lib/libc.so.6"))
                   
       
(defun main ()
  (setf *run* t)
  (sleep 1.0)
  (with-signal-handler linux:|SIGVTALRM| 
    (lambda (signum) (declare (ignore signum)) (setf *run* nil))
    (ffi:with-c-var (i 'itimerval (make-itimerval :interval-sec 0
                                                  :interval-mic 0
                                                  :value-sec 0
                                                  :value-mic 10000))
      (setitimer +itimer-virtual+ i nil))
    (loop
       :with counter = 0
       :with failed  = 0
       :while *run*
       :do (let ((res (linux:fork)))
               (cond ((null res) (incf failed))
                     ((= 0 res) (linux:exit 0))
                     (t (incf counter))))
       :finally (format t "forked ~D times (failed ~D times)~%"
                          counter failed)
       (finish-output))))

