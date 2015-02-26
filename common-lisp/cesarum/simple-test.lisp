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
;;;;    2015-01-25 <PJB> Added format-control/arguments to
;;;;                     progress-failure and macros callint it.
;;;;    2010-12-14 <PJB> Created.
;;;;BUGS
;;;;
;;;;    - we should use source-form to parse lambda-list for define-test.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")
  (:export "*DEBUG-ON-ERROR*" "WITH-DEBUGGER-ON-ERROR"
           "DEFINE-TEST" "TEST" "ASSERT-TRUE" "ASSERT-FALSE" "EXPECT-CONDITION"

           "*VERBOSE-TALLY*"  "*VERBOSE-PROGRESS*"
           "PROGRESS-START"
           "PROGRESS-SUCCESS" "PROGRESS-FAILURE-MESSAGE" "PROGRESS-FAILURE"
           "PROGRESS-TALLY")
  (:documentation "
This package defines a simple test tool.

   (define-test <test-name> (<test-arguments>)
     (assert-true   <expr> (<place>…) \"message ~A\" <arguments>…)
     (assert-false  <expr> (<place>…) \"message ~A\" <arguments>…)
     (if <test>
        (progress-success)
        (progress-failure-message '<expr> \"message ~A\" <arguments>…)))

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2010 - 2015
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST")


(defvar *debug-on-error*          nil
  "Whether an error in a test should go to the debugger.")
(defvar *success-count*           0
  "The total number of successful tests.")
(defvar *failure-count*           0
  "The total number of failed tests.")

(defvar *verbose-tally* t
  "Whether to print the number of successful, failed and performed tests.")
(defvar *verbose-progress* nil
  "Whether to display dots or exclamation points while testing.")

(defvar *test-output* *standard-output*)

;; Private:
(defvar *last-success-p*          nil)
(defvar *current-test-name*       nil)
(defvar *current-test-parameters* nil)
(defvar *current-test-printed-p*  nil)
(defvar *report-string*           "")
(defparameter *cr*                #\return)


(defun progress-start ()
  (setf *success-count*  0
        *failure-count*  0
        *last-success-p* nil
        *report-string*  (make-array 8
                                     :element-type 'character
                                     :adjustable t
                                     :fill-pointer 0))
  (values))


(defun verbose (default)
  (and default
       (or (not *load-pathname*)
           *load-verbose*
           (and (find-package "ASDF")
                (find-symbol "*ASDF-VERBOSE*" "ASDF")
                (symbol-value (find-symbol "*ASDF-VERBOSE*" "ASDF")))
           (and (find-package "QUICKLISP")
                (find-symbol "*QUICKLOAD-VERBOSE*" "QUICKLISP-CLIENT")
                (symbol-value (find-symbol "*QUICKLOAD-VERBOSE*" "QUICKLISP-CLIENT"))))))


(defun progress-report (new-last-succcess-p)
  (setf *last-success-p* new-last-succcess-p)
  (when (verbose *verbose-progress*)
    (if *last-success-p*
        (format *test-output* "~A" (aref *report-string* (1- (length *report-string*))))
        (format *test-output* "~&~A" *report-string*))
    (finish-output *test-output*))
  (values))


(defun progress-success ()
  (incf *success-count*)
  (vector-push-extend #\. *report-string*)
  (progress-report t))


(defun current-test-identification (&optional max-length)
  (let ((*print-circle* nil))
    (if (or (null max-length) (null *current-test-parameters*))
        (format nil "(~{~S~^ ~})" (cons *current-test-name* *current-test-parameters*))
        (let* ((items (mapcar (lambda (parameter)
                                (let ((label (let ((*package* (if (and (symbolp parameter)
                                                                       (symbol-package parameter))
                                                                  (symbol-package parameter)
                                                                  *package*)))
                                               (format nil "~S" parameter))))
                                  (list (length label) label)))
                              (cons *current-test-name* *current-test-parameters*)))
               (idlength (+ 1 (length items) (reduce (function +) items :key (function first))))
               (candidates (sort (butlast (loop
                                            :for cell :on items
                                            :collect cell))
                                 (function >)
                                 :key (function caadr))))
          (loop
            :while (and candidates (< max-length idlength))
            :do (progn
                  (decf idlength (1- (caadar candidates)))
                  (setf (car (cdadar candidates)) "…")
                  (pop candidates))
            :finally (return (format nil "(~{~A~^ ~})" (mapcar (function second) items))))))))


(defun test/current-test-identification ()
  (assert (equal (let ((*current-test-name* 'hello-world)
                       (*current-test-parameters* '()))
                   (current-test-identification  nil))
                 "(hello-world)"))

  (assert (equal (let ((*current-test-name* 'hello-world)
                       (*current-test-parameters* '((1 2 3 4) "howdy doo dabadaboo" #(a b c d e f))))
                   (current-test-identification  nil))
                 "(hello-world (1 2 3 4) \"howdy doo dabadaboo\" #(a b c d e f))"))

  (assert (equal (let ((*current-test-name* 'hello-world)
                       (*current-test-parameters* '((1 2 3 4) "howdy doo dabadaboo" #(a b c d e f))))
                   (current-test-identification  1))
                 "(hello-world … … …)"))

  (assert (equal (let ((*current-test-name*  'test/non-empty-vector-with-too-little-data)
                       (*current-test-parameters* '()))
                   (current-test-identification  20))
                 "(test/non-empty-vector-with-too-little-data)"))
  :success)



(defun progress-failure-message (expression message &rest arguments)
  (incf *failure-count*)
  (vector-push-extend #\! *report-string*)
  (unless *current-test-printed-p*
    (setf  *current-test-printed-p* t)
    (format *test-output* "~&~A" (current-test-identification)))
  (format *test-output* "~&Failure:     expression: ~S~@
                         ~&~?~%"
          expression message arguments)
  (progress-report nil))


(defun progress-failure (compare expression expected-result result
                         &optional places format-control &rest format-arguments)
  (progress-failure-message expression "~&           evaluates to: ~S~@
                                        ~&           which is not  ~A~@
                                        ~& to the expected result: ~S~@
                                        ~{~&~23A: ~S~}~@[~@
                                        ~&~?~]"
                            result compare expected-result places
                            format-control format-arguments))



(defun progress-tally (success-count failure-count)
  (when (verbose *verbose-tally*)
    (let ((name-max-length 40))
     (flet ((genline (name)
              (format nil "~VA~5D ~A,~3D ~9A~:[,~3D ~8A~;~]."
                      name-max-length name
                      (+ success-count failure-count)
                      (format nil "test~P" (+ success-count failure-count))
                      success-count (format nil "success~[es~;~:;es~]" success-count)
                      (zerop failure-count)
                      failure-count (format nil "failure~P" failure-count))))
       (format *test-output* "~&~A~%"
               (genline  (current-test-identification name-max-length)))
       (finish-output *test-output*)
       ;; (let* ((test-name (current-test-identification name-max-length))
       ;;        (data (genline ""))
       ;;        (nlen (length test-name)))
       ;;   (format *test-output* "~&~A~%" 
       ;;           (if (and (< nlen (+ name-max-length 4)) (char= #\space (aref data nlen)))
       ;;               (progn
       ;;                 (replace data test-name)
       ;;                 data)
       ;;               (genline (concatenate 'string (subseq test-name 0 43) "…"))))
       ;;   (finish-output *test-output*))
       )))
  (values))



(defmacro assert-true (expression &optional places format-control &rest format-arguments)
  "Evaluates a test EXPRESSION and check it returns true.
EXAMPLE:  (assert-true (= 2 (+ 1 1))))
"
  (let ((vresult   (gensym "RESULT-")))
    `(let ((,vresult   (if *debug-on-error*
                           (handler-bind
                               ((error (function invoke-debugger)))
                             ,expression)
                           (handler-case
                               ,expression
                             (error (err) (list 'error (princ-to-string err)))))))
       (if ,vresult
           (progress-success)
           (progress-failure 'equivalent ',expression 't ,vresult
                             (list ,@(mapcan (lambda (place) `(',place ,place)) places))
                             ,format-control ,@format-arguments)))))


(defmacro assert-false (expression &optional places format-control &rest format-arguments)
  "Evaluates a test EXPRESSION and check it returns NIL
EXAMPLE:  (assert-false (/= 2 (+ 1 1))))
"
  `(assert-true (not ,expression) ,places ,format-control ,@format-arguments))


(defmacro expect-condition (condition-class expression)
  "Evaluates a test EXPRESSION and check that it signals a condition of the specified CONDITION-CLASS.
EXAMPLE:  (expect-condition division-by-zero (/ 1 0))
"
  (let ((body (gensym)))
    `(flet ((,body ()
                   ,expression
                   (progress-failure-message ',expression
                                             "Didn't signal the expected ~S condition."
                                             ',condition-class)))
       (if *debug-on-error*
           (block expect
             (handler-bind
                 ((,condition-class (lambda (condition)
                                      (declare (ignore condition))
                                      (progress-success)
                                      (return-from expect)))
                  (t (function invoke-debugger)))
               (,body)))
           (handler-case
               (,body)
             (,condition-class ()
               (progress-success))
             (t (condition)
               (progress-failure-message ',expression
                                         "Signaled an unexpected ~S condition instead of ~S."
                                         condition
                                         ',condition-class)))))))



(defmacro test (compare expression expected &optional places format-control &rest format-arguments)
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
           (progress-success)
           (progress-failure ',compare ',expression ,vexpected ,vresult
                             (list ,@(mapcan (lambda (place) `(',place ,place)) places))
                             ,format-control
                             ,@format-arguments)))))


(defmacro define-test (name parameters &body body)
  "Like DEFUN, but wraps the body in test reporting boilerplate."
  (let ((mandatory (loop
                     :for param :in parameters
                     :until (member param lambda-list-keywords)
                     :collect param)))
    (multiple-value-bind (docstrings declarations forms) (parse-body :lambda body)
      `(defun ,name ,parameters
         ,@docstrings
         ,@declarations
         (multiple-value-bind (successes failures)
             (let ((*success-count* 0)
                   (*failure-count* 0)
                   (*current-test-name*        ',name)
                   (*current-test-parameters* (list ,@mandatory))
                   (*current-test-printed-p*  nil))
               (progress-start)
               (progn ,@forms)
               (progress-tally *success-count* *failure-count*)
               (values *success-count* *failure-count*))
           (incf *success-count* successes)
           (incf *failure-count* failures)
           (if (zerop failures)
               :success
               :failure))))))

(defmacro with-debugger-on-error (&body body)
  `(let ((*debug-on-error* t))
     ,@body))

;;;; THE END ;;;;
