;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               simple-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines a simple test tool.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-12-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2012
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

(in-package "COM.INFORMATIMAGO.SIMPLE-TEST")


(defvar *debug-on-error*          nil
  "Whether an error in a test should go to the debugger.")
(defvar *success-count*           0
  "The total number of successful tests.")
(defvar *failure-count*           0
  "The total number of failed tests.")


;; Private:
(defvar *last-success-p*          nil)
(defvar *current-test*            nil)
(defvar *current-test-printed-p*  nil)
(defvar *report-string*           "")
(defparameter *cr*                #\return)


(defun progress-start (test-name)
  (setf *success-count*  0
        *failure-count*  0
        *last-success-p* nil
        *report-string*  (make-array 8
                                     :element-type 'character
                                     :adjustable t
                                     :fill-pointer 0)
        *current-test*   test-name
        *current-test-printed-p* nil)
  (values))


(defun progress-report (new-last-succcess-p)
  (if (setf *last-success-p* new-last-succcess-p)
      (format t "~A" (aref *report-string* (1- (length *report-string*))))
      (format t "~&~A" *report-string*))
  (finish-output)
  (values))


(defun progress-success (compare expression result)
  (declare (ignorable compare expression result))
  (incf *success-count*)
  (vector-push-extend #\. *report-string*)
  (progress-report t))


(defun progress-failure (compare expression expected-result result)
  (incf *failure-count*)
  (vector-push-extend #\! *report-string*)
  (unless *current-test-printed-p*
    (setf  *current-test-printed-p* t)
    (format t "~&~A" *current-test*))
  (format t "~&Failure:     expression: ~S~@
             ~&           evaluates to: ~S~@
             ~&           which is not  ~A~@
             ~& to the expected result: ~S~%"
          expression result compare expected-result)
  (progress-report nil))


(defun progress-tally (test-name success-count failure-count)
  (flet ((genline (name)
           (format nil "~44A ~4D ~4A ~4D ~4A ~5D ~A"
                   name
                   success-count "succ" ; (format nil "success~[es~;~:;es~]," success-count)
                   failure-count "fail" ; (format nil "failure~P," failure-count)
                   (+ success-count failure-count)
                   "test"; (format nil "test~P." (+ success-count failure-count))
                   )))
    (let* ((test-name (string test-name))
           (data (genline ""))
           (nlen (length test-name)))
      
      (format t "~&~A~%" 
              (if (and (< nlen (+ 44 4)) (char= #\space (aref data nlen)))
                  (progn
                    (replace data test-name)
                    data)
                  (genline test-name)))))
  (finish-output)
  (values))


(defmacro test (compare expression expected)
  "Evaluates a test EXPRESSION and compare the result with EXPECTED (evaluated) using the COMPARE operator.
EXAMPLE:  (test equal (list 1 2 3) '(1 2 3))
"
  (let ((vresult   (gensym "RESULT-"))
        (vexpected (gensym "EXPECTED-")))
    `(let ((,vresult   (if *debug-on-error*
                           (handler-bind
                               ((error (function invoke-debugger)))
                             ,expression)
                           (handler-case
                               ,expression
                             (error (err) (list 'error (princ-to-string err))))))
           (,vexpected ,expected))
       (if (,compare ,vresult ,vexpected)
           (progress-success ',compare ',expression ,vexpected)
           (progress-failure ',compare ',expression ,vexpected ,vresult)))))


(defmacro define-test (name parameters &body body)
  "Like DEFUN, but wraps the body in test reporting boilerplate."
  `(defun ,name ,parameters
     (multiple-value-bind (successes failures)
         (let ((*success-count* 0)
               (*failure-count* 0))
           (progress-start ',name)
           (locally ,@body)
           (progress-tally ',name *success-count* *failure-count*)
           (values *success-count* *failure-count*))
       (incf *success-count* successes)
       (incf *failure-count* failures)
       (if (zerop *failure-count*)
           :success
           :failure))))

(defmacro with-debugger-on-error (&body body)
  `(let ((*debug-on-error* t))
     ,@body))

;;;; THE END ;;;;
