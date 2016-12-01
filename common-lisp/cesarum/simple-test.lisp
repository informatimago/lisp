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
;;;;    2015-03-08 <PJB> Added with-debugger-on-failure.
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
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.TIME")
  #+mocl (:shadowing-import-from "COM.INFORMATIMAGO.MOCL.KLUDGES.MISSING"
                          "*TRACE-OUTPUT*"
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
                          "TRANSLATE-LOGICAL-PATHNAME")
  (:export "*DEBUG-ON-ERROR*" "WITH-DEBUGGER-ON-ERROR"
           "*DEBUG-ON-FAILURE*" "WITH-DEBUGGER-ON-FAILURE"
           "DEFINE-TEST" "CHECK" "ASSERT-TRUE" "ASSERT-FALSE" "EXPECT-CONDITION"
           "TEST-MESSAGE" "*TEST-OUTPUT*" "*VERBOSE-TALLY*"  "*VERBOSE-PROGRESS*"
           "TESTING" "SLOW-TEST"
           "PROGRESS-START"
           "PROGRESS-SUCCESS" "PROGRESS-FAILURE-MESSAGE" "PROGRESS-FAILURE"
           "PROGRESS-TALLY"
           ;; deprecated:
           "TEST")
  (:documentation "
This package defines a simple test tool.

   (define-test <test-name> (<test-arguments>)
     (check = (fact 3) 6)
     (assert-true   <expr> (<place>…) \"message ~A\" <arguments>…)
     (assert-false  <expr> (<place>…) \"message ~A\" <arguments>…)
     (if <test>
        (progress-success)
        (progress-failure-message '<expr> \"message ~A\" <arguments>…)))


Tests can be run in the scope of a WITH-DEBUGGER-ON-ERROR or a
WITH-DEBUGGER-ON-FAILURE macro, to enter the debugger when an error is
signaled during the test, or if a test fails.  This may be useful to
debug the test or the failure.

    (with-debugger-on-failure
       (test/all))

    ;; single shot testing:
    (testing
       (check = (fact 3) 6))

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
(defvar *debug-on-failure*        nil
  "Whether a failure in a test should go to the debugger.")
(defvar *success-count*           0
  "The total number of successful tests.")
(defvar *failure-count*           0
  "The total number of failed tests.")

(defvar *verbose-tally* t
  "Whether to print the number of successful, failed and performed tests.")
(defvar *verbose-progress* nil
  "Whether to display dots or exclamation points while testing.")

(defvar *test-output* (make-synonym-stream '*standard-output*))

;; Private:
(defvar *last-success-p*          nil)
(defvar *current-test-name*       nil)
(defvar *current-test-parameters* nil)
(defvar *current-test-printed-p*  nil)
(defvar *report-string*           "")
(defparameter *cr*                #\return)


(defun progress-start ()
  "Resets the progress counters (success/failure counts)."
  (setf *success-count*  0
        *failure-count*  0
        *last-success-p* nil
        *report-string*  (make-array 8
                                     :element-type 'character
                                     :adjustable t
                                     :fill-pointer 0))
  (values))


(defun verbose (default)
  "Returns whether the test must be verbose.
We conjoin the DEFAULT parameter with *LOAD-VERBOSE*, ASDF-VERBOSE*
and QUICKLOAD-VERBOSE* when available."
  (and default
       (or (not *load-pathname*)
           *load-verbose*
           (and (find-package "ASDF")
                (find-symbol "*ASDF-VERBOSE*" "ASDF")
                (symbol-value (find-symbol "*ASDF-VERBOSE*" "ASDF")))
           (and (find-package "QUICKLISP")
                (find-symbol "*QUICKLOAD-VERBOSE*" "QUICKLISP-CLIENT")
                (symbol-value (find-symbol "*QUICKLOAD-VERBOSE*" "QUICKLISP-CLIENT"))))))

(defun test-message (format-control &rest format-arguments)
  "Formats the parameters on the *TEST-OUTPUT* when running the test verbosely
cf. VERBOSE, *VERBOSE-PROGRESS*"
  (when (verbose t)
    (format *test-output* "~&~?~%" format-control format-arguments)
    (force-output *test-output*)))

(defun progress-report (new-last-succcess-p)
  "Prints on *TEST-OUTPUT* the current progress report."
  (setf *last-success-p* new-last-succcess-p)
  (when (verbose *verbose-progress*)
    (if *last-success-p*
        (format *test-output* "~A" (aref *report-string* (1- (length *report-string*))))
        (format *test-output* "~&~A" *report-string*))
    (force-output *test-output*))
  (values))


(defun progress-success ()
  "Indicate one more successful test."
  (incf *success-count*)
  (vector-push-extend #\. *report-string*)
  (progress-report t))


(defun current-test-identification (&optional max-length)
  "Returns the identification of the current test (over a maximum length of MAX-LENGTH)"
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



(define-condition test-failure ()
  ((expression :initarg :expression
               :reader test-failure-expression)
   (message    :initarg :message
               :initform ""
               :reader test-failure-message)
   (arguments  :initarg :arguments
               :initform '()
               :reader test-failure-arguments))
  (:report (lambda (condition stream)
             (format stream "Failure on expression: ~S~%~?"
                     (test-failure-expression condition)
                     (test-failure-message condition)
                     (test-failure-arguments condition)))))




(defun progress-failure-message (expression message &rest arguments)
  "Indicates one more failed test, with a formatted MESSAGE and ARGUMENTS."
  (incf *failure-count*)
  (vector-push-extend #\! *report-string*)
  (unless *current-test-printed-p*
    (setf  *current-test-printed-p* t)
    (format *test-output* "~&~A" (current-test-identification)))
  (format *test-output* "~&Failure:     expression: ~S~@
                         ~&~?~%"
          expression message arguments)
  (force-output *test-output*)
  (progress-report nil)
  (when *debug-on-failure*
    (invoke-debugger (make-condition 'test-failure
                                     :expression expression
                                     :message message
                                     :arguments arguments))))


(defun progress-failure (compare expression expected-result result
                         &optional places format-control &rest format-arguments)
  "Indicates one more failed test, reporting the expression, the
expected and actual results, and the relevant places, in addition to a
formatted message."
  (progress-failure-message expression "~&           evaluates to: ~S~@
                                        ~&           which is not  ~A~@
                                        ~& to the expected result: ~S~@
                                        ~{~&~23A: ~S~}~@[~@
                                        ~&~?~]~&"
                            result compare expected-result places
                            format-control format-arguments))



(defun progress-tally (success-count failure-count)
  "When testing verbosely, prints the test tally, SUCCESS-COUNT and
FAILURE-COUNT."
  (when (verbose *verbose-tally*)
    (let ((name-max-length 40))
      (flet ((write-tally (name)
               (format *test-output* "~&~VA~@[~%~0@*~V<~>~3@*~]~
                       ~5D test~:*~P,~:*~[~; ~;~]~
                       ~4D success~:*~[es~;~:;es~]~
                       ~:[,~2:*~[~;  ~;~]~*~4D failure~:*~P~;~].~%"
                       name-max-length name (< name-max-length (length name))
                       (+ success-count failure-count)
                       success-count
                       (zerop failure-count)
                       failure-count)))
        (write-tally (current-test-identification name-max-length))
        (force-output *test-output*)
        ;; (let* ((test-name (current-test-identification name-max-length))
        ;;        (data (genline ""))
        ;;        (nlen (length test-name)))
        ;;   (format *test-output* "~&~A~%"
        ;;           (if (and (< nlen (+ name-max-length 4)) (char= #\space (aref data nlen)))
        ;;               (progn
        ;;                 (replace data test-name)
        ;;                 data)
        ;;               (genline (concatenate 'string (subseq test-name 0 43) "…"))))
        ;;   (force-output *test-output*))
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
CONDITION-CLASS: evaluated to a class name.
EXAMPLE:        (expect-condition 'division-by-zero (/ 1 0))
"
  (let ((body (gensym))
        (vcondition-class (gensym)))
    `(let ((,vcondition-class ,condition-class))
       (flet ((,body ()
                ,expression
                (progress-failure-message ',expression
                                          "Didn't signal the expected ~S condition."
                                          ,vcondition-class)))
         (if *debug-on-error*
             (block expect
               (handler-bind
                   ((error (lambda (condition)
                             (if (typep condition ,vcondition-class)
                                 (progn
                                   (progress-success)
                                   (return-from expect))
                                 (invoke-debugger condition)))))
                 (,body)))
             (handler-case
                 (,body)
               (error (condition)
                 (if (typep condition ,vcondition-class)
                     (progress-success)
                     (progress-failure-message
                      ',expression
                      "Signaled an unexpected ~S condition instead of ~S."
                      condition
                      ,vcondition-class)))))))))


(defmacro test (compare expression expected &optional places format-control &rest format-arguments)
  "Deprecated, use CHECK instead."
  (warn "~S is deprecated, use CHECK instead." 'test)
  `(check ,compare ,expression ,expected ,places ,format-control ,@format-arguments))

(defmacro check (compare expression expected &optional places format-control &rest format-arguments)
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



(defmacro testing (&body body)
  "Evaluates the body while tallying test successes and failures.

The functions PROGRESS-SUCCESS, PROGRESS-FAILURE and
PROGRESS-FAILURE-MESSAGE (eg. thru the macros ASSERT-TRUE,
ASSERT-FALSE, EXPECT-CONDITION and TEST), should only be called in the
dynamic context established by this TESTING macro.

cf. DEFINE-TEST.
"
  `(multiple-value-bind (successes failures)
       (let ((*success-count* 0)
             (*failure-count* 0)
             (*current-test-printed-p*  nil))
         (progress-start)
         (progn ,@body)
         (progress-tally *success-count* *failure-count*)
         (values *success-count* *failure-count*))
     (incf *success-count* successes)
     (incf *failure-count* failures)
     (if (zerop failures)
         :success
         :failure)))


(defmacro define-test (name parameters &body body)
  "Like DEFUN, but wraps the body in test reporting boilerplate.
cf. TESTING."
  (let ((mandatory (loop
                     :for param :in parameters
                     :until (member param lambda-list-keywords)
                     :collect param)))
    (multiple-value-bind (docstrings declarations forms) (parse-body :lambda body)
      `(defun ,name ,parameters
         ,@docstrings
         ,@declarations
         (let ((*current-test-name*        ',name)
               (*current-test-parameters* (list ,@mandatory)))
           (testing ,@forms))))))

(defmacro with-debugger-on-error (&body body)
  "When running tests in the dynamic context established by this macro,
errors will invoke the debugger instead of failing the test immediately."
  `(let ((*debug-on-error* t))
     ,@body))

(defmacro with-debugger-on-failure (&body body)
    "When running tests in the dynamic context established by this macro,
failures will invoke the debugger instead of failing the test immediately."
  `(let ((*debug-on-failure* t))
     ,@body))

(defun slow-test* (expected-time thunk)
  (let ((result))
    (test-message "Next test should take about ~A." (format-time expected-time))
    (let ((time (chrono-real-time (setf result (funcall thunk)))))
      (test-message "Test took ~A" (format-time time)))
    result))

(defmacro slow-test (expected-time &body body)
  `(slow-test* ,expected-time (lambda () ,@body)))

;;;; THE END ;;;;
