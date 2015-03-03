;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               missing.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements CL standard operators missing from MoCL.
;;;;
;;;;    !!!! NOTICE THE LICENSE OF THIS FILE !!!!
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-03-01 <PJB> Created.
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

(defpackage "COM.INFORMATIMAGO.MOCL.KLUDGES.MISSING"
  (:use "COMMON-LISP")
  (:shadow "*TRACE-OUTPUT*"
           "*LOAD-VERBOSE*"
           "*LOAD-PRINT*"
           "ARRAY-DISPLACEMENT"
           "CHANGE-CLASS"
           "COMPILE"
           "COMPLEX"
           "ENSURE-DIRECTORIES-EXIST"
           "FILE-WRITE-DATE"
           "INVOKE-DEBUGGER" "*DEBUGGER-HOOK*"
           "LOAD"
           "LOGICAL-PATHNAME-TRANSLATIONS"
           "MACHINE-INSTANCE"
           "MACHINE-VERSION"
           "NSET-DIFFERENCE"
           "RENAME-FILE"
           "SUBSTITUTE-IF"
           "TRANSLATE-LOGICAL-PATHNAME"
           "PRINT-NOT-READABLE"
           "PRINT-NOT-READABLE-OBJECT")
  (:export "*TRACE-OUTPUT*"
           "*LOAD-VERBOSE*"
           "*LOAD-PRINT*"
           "ARRAY-DISPLACEMENT"
           "CHANGE-CLASS"
           "COMPILE"
           "COMPLEX"
           "ENSURE-DIRECTORIES-EXIST"
           "FILE-WRITE-DATE"
           "INVOKE-DEBUGGER" "*DEBUGGER-HOOK*"
           "LOAD"
           "LOGICAL-PATHNAME-TRANSLATIONS"
           "MACHINE-INSTANCE"
           "MACHINE-VERSION"
           "NSET-DIFFERENCE"
           "RENAME-FILE"
           "SUBSTITUTE-IF"
           "TRANSLATE-LOGICAL-PATHNAME"
           "PRINT-NOT-READABLE"
           "PRINT-NOT-READABLE-OBJECT")
  (:documentation "

Implements CL standard operators missing from MoCL.

LEGAL

    AGPL3
    
    Copyright Pascal J. Bourguignon 2015 - 2015
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

"))
(in-package "COM.INFORMATIMAGO.MOCL.KLUDGES.MISSING")

;; CHANGE-CLASS       ;; CLOS!
;; COMPLEX           ;; all complex is missing.


(defvar *load-verbose* nil)
(defvar *load-print*   nil)
(defvar *trace-output* *standard-output*)

(define-condition print-not-readable (error)
  ((object :initarg :object :reader print-not-readable-object))
  (:report (lambda (condition stream)
             (let ((*print-readably* nil))
               (format stream "Object to printable readably ~S"
                       (print-not-readable-object condition))))))

(defun ARRAY-DISPLACEMENT (array)
  ;; if not provided, then displaced array don't exist!
  (declare (ignore array))
  (values nil 0))


;; COMPILE           ;; required to implement minimal compilation.

(defun LOAD (filespec &key verbose print if-does-not-exist external-format)
  )


(defun ENSURE-DIRECTORIES-EXIST (pathspec &key verbose)
  (error "~S not implemented yet" 'ENSURE-DIRECTORIES-EXIST)
  (let ((created nil))
   (values pathspec created)))

(defun RENAME-FILE (filespec new-name)
  (error "~S not implemented yet" 'RENAME-FILE)
  (let (defaulted-new-name old-truename new-truename)
   (values defaulted-new-name old-truename new-truename)))

(defun FILE-WRITE-DATE (pathspec)
  (declare (ignore pathspec))
  nil)

(defvar *debugger-hook* nil)

(defun INVOKE-DEBUGGER (condition)
  (when *debugger-hook*
    (let ((saved-hook *debugger-hook*)
          (*debugger-hook* nil))
       (funcall saved-hook condition)))
  (rt:formatd "Debugger invoked on condition ~A; aborting." condition)
  (rt:quit))

(defvar *hosts* '())

(defun LOGICAL-PATHNAME-TRANSLATIONS (host)
  (cdr (assoc host *hosts* :test (function equalp))))

(defun (setf LOGICAL-PATHNAME-TRANSLATIONS) (new-translations host)
  (let ((entry (assoc host *hosts* :test (function equalp))))
    (if entry
        (setf (cdr entry) (copy-tree new-translations))
        (push (cons (nstring-upcase (copy-seq host))
                    (copy-tree new-translations))
              *hosts*))))

(defun TRANSLATE-LOGICAL-PATHNAME (pathname &key &allow-other-keys)
  (error "~S not implemented yet" 'TRANSLATE-LOGICAL-PATHNAME)
  pathname)

(defun MACHINE-INSTANCE ()
  ;; TODO: find the hostname of the machine, or some other machine identification.
  #+android "Android"
  #+ios     "iOS")

(defun MACHINE-VERSION ()
  ;; TODO: find the hardware version, or some other machine version.
  #+android "0.0"
  #+ios     "0.0")

;; Clozure Common Lisp            --> ("larissa.local" "MacBookAir6,2")
;; CLISP                          --> ("larissa.local [192.168.7.8]" "X86_64")
;; ECL                            --> ("larissa.local" NIL)
;; SBCL                           --> ("larissa.local" "Intel(R) Core(TM) i7-4650U CPU @ 1.70GHz")


(defun NSET-DIFFERENCE (list-1 list-2 &rest rest &key key test test-not)
  (declare (ignore key test test-not))
  (apply (function set-difference) list-1 list-2 rest))

(defun nsubstitute-if (new-item predicate sequence &key from-end start end count key)
  (let* ((length (length sequence))
         (start  (or start 0))
         (end    (or end lengh))
         (key    (or key (function identity))))
    (assert (<= 0 start end length))
    (etypecase sequence
      (list   (cond
                (from-end
                 (nreverse (nsubstitute-if new-item predicate (nreverse sequence)
                                           :start (- length end) :end (- length start)
                                           :count count :key key)))
                (count
                 (when (plusp count)
                   (loop
                     :repeat (- end start)
                     :for current :on (nthcdr start sequence)
                     :do (when (funcall predicate (funcall key (car current)))
                           (setf (car current) new-item)
                           (decf count)
                           (when (zerop count)
                             (return))))))
                (t
                 (loop
                   :repeat (- end start)
                   :for current :on (nthcdr start sequence)
                   :do (when (funcall predicate (funcall key (car current)))
                         (setf (car current) new-item))))))
      (vector (if from-end
                  (if count
                      (when (plusp count)
                        (loop
                          :for i :from (1- end) :downto start
                          :do (when (funcall predicate (funcall key (aref sequence i)))
                                (setf (aref sequence i) new-item)
                                (decf count)
                                (when (zerop count)
                                  (return)))))
                      (loop
                        :for i :from (1- end) :downto start
                        :do (when (funcall predicate (funcall key (aref sequence i)))
                              (setf (aref sequence i) new-item))))
                  (if count
                      (when (plusp count)
                        (loop
                          :for i :from start :below end
                          :do (when (funcall predicate (funcall key (aref sequence i)))
                                (setf (aref sequence i) new-item)
                                (decf count)
                                (when (zerop count)
                                  (return)))))
                      (loop
                        :for i :from start :below end
                        :do (when (funcall predicate (funcall key (aref sequence i)))
                              (setf (aref sequence i) new-item)))))))
    sequence))

(defun SUBSTITUTE-IF (new-item predicate sequence &rest rest &key from-end start end count key)
  (apply (function nsubstitute-if) new-item predicate (copy-seq sequence) rest))

(defun NSUBSTITUTE-IF-NOT (new-item predicate sequence &rest rest &key from-end start end count key)
  (apply (function nsubstitute-if) new-item (complement predicate) sequence rest))

(defun SUBSTITUTE-IF-NOT (new-item predicate sequence &rest rest &key from-end start end count key)
  (apply (function nsubstitute-if) new-item (complement predicate) (copy-seq sequence) rest))





;; Warning: Function ASDF:FIND-SYSTEM is referenced but not defined.
;; Warning: Function ASDF:GETENV is referenced but not defined.
;; Warning: Function ASDF:RUN-SHELL-COMMAND is referenced but not defined.



;;;; THE END ;;;;
