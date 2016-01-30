;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               stepper-functions.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An internal package of the Common Lisp stepper.
;;;;    This package exports the stepper generator functions
;;;;    and defines the stepper interactive functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-10-08 <PJB> Corrected step-{break,unbreak}-{entry,exit}.
;;;;    2012-08-09 <PJB> Extracted from stepper.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
;; Only the step-disabled function is read in the cl-stepper package;
;; the rest of the file is read in the .stepper.internal package.
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER")


;; When instrumenting is disabled with a declaration, all the forms and
;; subforms in the scope must use the CL operators instead of the stepper
;; macros.  Since subforms and their expansions may use those stepper
;; macros, we would need a code walker to process them.  A simpler and
;; more portable solution is to use macrolet and to shadow all those
;; stepper macros.

(cl:defun com.informatimago.common-lisp.lisp.stepper.internal:step-disabled (form)
  "
RETURN:         A form where FORM is evaluated in an environment where
                all the stepper special operator macros expand to CL
                special operators.
"
  `(cl:macrolet
       ((function (name) (cl:if (and (consp name)
                                     (eq 'lambda (first name)))
                                `(cl:function (cl:lambda ,@(rest (first name))))
                                `(cl:function ,name)))
        (quote (literal) `(cl:quote ,literal))
        (if (&whole form test then &optional else)
            (declare (ignorable test then else))
            `(cl:if ,@(rest form)))
        (block (&whole form name &body body)
          (declare (ignorable name body))
          `(cl:block ,@(rest form)))
        (return-from (&whole form name &optional result)
          (declare (ignorable name result))
          `(cl:return-from ,@(rest form)))
        (catch (&whole form object &body body)
          (declare (ignorable object body))
          `(cl:catch ,@(rest form)))
        (throw (&whole form object result)
          (declare (ignorable object result))
          `(cl:throw ,@(rest form)))
        (unwind-protect (&whole form protected &body cleanup)
          (declare (ignorable protected cleanup))
          `(cl:unwind-protect ,@(rest form)))
        (tagbody (&whole form &body body)
           (declare (ignorable body))
           `(cl:tagbody ,@(rest form)))
        (go (tag) `(cl:go ,tag))
        (flet (&whole form (&rest bindings) &body body)
          (declare (ignorable bindings body))
          `(cl:flet ,@(rest form)))
        (labels (&whole form (&rest bindings) &body body)
          (declare (ignorable bindings body))
          `(cl:labels ,@(rest form)))
        (macrolet (&whole form (&rest bindings) &body body)
          (declare (ignorable bindings body))
          `(cl:macrolet ,@(rest form)))
        (symbol-macrolet (&whole form (&rest bindings) &body body)
          (declare (ignorable bindings body))
          `(cl:symbol-macrolet ,@(rest form)))
        (let (&whole form (&rest bindings) &body body)
          (declare (ignorable bindings body))
          `(cl:let ,@(rest form)))
        (let* (&whole form (&rest bindings) &body body)
          (declare (ignorable bindings body))
          `(cl:let* ,@(rest form)))
        (setq (&whole form var val &rest pairs)
          (declare (ignorable var val pairs))
          `(cl:setq ,@(rest form)))
        (multiple-value-call (&whole form function-form &rest arguments)
          (declare (ignore function-form arguments))
          `(cl:multiple-value-call ,@(rest form)))
        (multiple-value-prog1 (&whole form result-form &body body)
          (declare (ignore result-form body))
          `(cl:multiple-value-prog1 ,@(rest form)))
        (locally (&whole form &body body)
          (declare (ignore body))
          `(cl:locally ,@(rest form)))
        (progn (&whole form &body body)
               (declare (ignore body))
               `(cl:progn ,@(rest form)))
        (progv (&whole form symbols values &body body)
            (declare (ignore symbols values body))
          `(cl:progv ,@(rest form)))
        (the (&whole form value-type expression)
          (declare (ignore value-type expression))
          `(cl:the ,@(rest form)))
        (eval-when (&whole form (&rest situations) &body body)
          (declare (ignore situations body))
          `(cl:eval-when ,@(rest form)))
        (load-time-value (&whole form expression &optional read-only-p)
          (declare (ignore expression read-only-p))
          `(cl:load-time-value ,@(rest form))))
     ,form))



(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.INTERNAL")


(defvar *step-mode* :run
  "
May be :run, :function :trace or :step.

:run       don't print anything, just evaluate the forms.

:function just prints the function calls and their results.

:trace    just prints the forms and their results as they are evaluted.

:step     prints the form, then ask the user what to do (step over,
          step into, trace, run).

When break-points are implemented, :run, :function and :trace will run
until a break-point is reached.

")


(defvar *step-max-trace-depth* nil
  "The maximum depth of function calls that should be traced.  When
more than that depth of calls occur, the *step-mode* switches to
:run.")

(defvar *step-current-trace-depth* 0
  "The current depth of instrumented function calls.")


;; (STEP-TRACE f)        T           T           F           F
;; (STEP-NOTRACE f)      T           F           T           F
;; *STEP-MODE*       :r :t :s    :r :t :s    :r :t :s    :r :t :s
;; ----------------------------------------------------------------
;; Action:            r  r  s     t  t  s     r  r  s     r  t  s
;; r = run, t = trace, s = step (in or over).


(defvar *trace-functions* '()
  "A list of function names that we must trace with the stepper.
SEE: STEP-TRACE, STEP-UNTRACE.")

(defvar *break-functions-entry* '()
  "A list of function names that we must break into the stepper upon entry.
SEE: STEP-BREAK-ENTRY, STEP-UNBREAK-ENTRY.")

(defvar *break-functions-exit* '()
  "A list of function names that we must break into the stepper upon exit.
SEE: STEP-BREAK-EXIT, STEP-UNBREAK-EXIT.")



(defmacro step-trace-function (&rest fnames)
  "
DO:             Enable tracing of functions named by FNAMES.

FNAMES:         A list of function names.

NOTE:           The functions must have been compiled with the operators from
                the CL-STEPPER package not the CL package.

RETURN:         The list of function names added.
"
  ;; We use :test 'equal for (setf fname).
  `(setf *trace-functions*
         (delete-duplicates (union *trace-functions* ',fnames
                                   :test (function equal))
                            :test (function equal))))


(defmacro step-untrace-function (&rest fnames)
  "
DO:             Disable tracing of functions named by FNAMES.

FNAMES:         A list of function names.

NOTE:           The functions must have been compiled with the operators from
                the CL-STEPPER package not the CL package.

RETURN:         The list of step-traced functions remaining.
"
  `(setf *trace-functions* (set-difference *trace-functions* ',fnames
                                           :test (function equal))))


(defmacro step-break-entry (&rest fnames)
  "
DO:             Enable breaking on entry of functions named by FNAMES.

FNAMES:         A list of function names.

NOTE:           The functions must have been compiled with the operators from
                the CL-STEPPER package not the CL package.

RETURN:         The list of function names added.
"
  `(setf *break-functions-entry*
         (delete-duplicates (union *break-functions-entry* ',fnames
                                   :test (function equal))
                            :test (function equal))))


(defmacro step-unbreak-entry (&rest fnames)
  "
DO:             Disable breaking on entry of functions named by FNAMES.

FNAMES:         A list of function names.

NOTE:           The functions must have been compiled with the operators from
                the CL-STEPPER package not the CL package.

RETURN:         The list of step-break-entry functions remaining.
"
  `(setf *break-functions-entry* (set-difference *break-functions-entry* ',fnames
                                                 :test (function equal))))


(defmacro step-break-exit (&rest fnames)
  "
DO:             Enable breaking on exit of functions named by FNAMES.

FNAMES:         A list of function names.

NOTE:           The functions must have been compiled with the operators from
                the CL-STEPPER package not the CL package.

RETURN:         The list of function names added.
"
  `(setf *break-functions-exit*
         (delete-duplicates (union *break-functions-exit* ',fnames
                                   :test (function equal))
                            :test (function equal))))


(defmacro step-unbreak-exit (&rest fnames)
  "
DO:             Disable breaking on exit of functions named by FNAMES.

FNAMES:         A list of function names.

NOTE:           The functions must have been compiled with the operators from
                the CL-STEPPER package not the CL package.

RETURN:         The list of step-break-entry functions remaining.
"
  `(setf *break-functions-exit* (set-difference *break-functions-exit* ',fnames
                                                :test (function equal))))



(defvar *step-level* 0
  "The level.")

(defvar *step-package*         (find-package :cl-user)
  "The package bound to *PACKAGE* while printing tracing logs.")

(defvar *step-print-readably* nil
  "The value bound to *PRINT-READABLY* while printing tracing logs.")


(defvar *step-print-length*    10
  "The value bound to *PRINT-LENGTH* while printing tracing logs.")

(defvar *step-print-level*      3
  "The value bound to *PRINT-LEVEl* while printing tracing logs.")

(defvar *step-print-case*     :downcase
  "The value bound to *PRINT-CASE* while printing tracing logs.")

(defvar *step-trace-output* (make-synonym-stream '*trace-output*)
  "The stream where the stepper traces are written to.")

(defmacro with-step-printing (&body body)
  `(let ((*print-length*   *step-print-length*)
         (*print-level*    *step-print-level*)
         (*print-readably* *step-print-readably*)
         (*print-case*     *step-print-case*)
         (*package*        *step-package*)
         (*step-mode*     :run))
     ,@body))



;; Tracing steps:

(defmacro with-parens (stream &body body)
  (let ((vstream (gensym)))
    `(let ((,vstream ,stream))
       (unwind-protect
            (progn
              (format ,vstream "~&~V<~>(" *step-level*)
              ,@body)
         (format ,vstream ")")))))


(defun will-step (form &optional (stream *step-trace-output*))
  (with-step-printing
      (format stream "Will evaluate ~S" form)))




(defun did-bind (variable value &optional (stream *step-trace-output*))
  "
RETURN: VALUE
"
  (unless (eq :run *step-mode*)
    (with-step-printing
        (format stream "~&~V<~> (Bind ~16A to ~S)" *step-level* variable value)))
  value)

(defun print-step-results (results &optional (stream *step-trace-output*))
  (when results
    (with-step-printing
        (let ((start "==>"))
          (dolist (result results)
            (format stream "~%~V<~>~A ~S" *step-level* start result)
            (setf start "   "))))))

(defun did-step (form results &optional (stream *step-trace-output*))
  (with-step-printing
      (format stream "~&~V<~> Evaluation of ~S returned ~:[no result~;~R result~:P~]"
              *step-level* form results (length results)))
  (if (= 1 (length results))
      (format stream " ==> ~S" (first results))
      (print-step-results results stream)))

(defun did-tag (tag &optional (stream *step-trace-output*))
  (unless (eq :run *step-mode*)
    (with-step-printing
        (format stream "~&~V<~> (Passed tag ~S)" *step-level* tag))))


;; Interactive stepping:

(define-condition step-condition (condition)
  ((message :initarg :message :initform "Step" :reader step-message))
  (:report (lambda (condition stream)
             (format stream "~A" (step-message condition)))))

(defun step-choice (&optional thunk)
  (when thunk (funcall thunk *step-trace-output*))
  (with-step-printing
      (format *query-io* "~&~V<~> ~{~A~^, ~}?"
              *step-level*
              '("Step Into (s, si, RET)" "Step over (so)"
                "Trace (t)" "Function (f)" "Run (r)"
                "List (l)" "Eval (e)"  "Debugger (d)"
                "Abort (a, q)")))
  (let ((answer (string-trim " " (progn (finish-output *query-io*) (read-line *query-io*)))))
    (cond
      ((member answer '("" "s" "si") :test (function string-equal))
       :step-into)
      ((string-equal answer "so") :step-over)
      ((string-equal answer "f")  :function)
      ((string-equal answer "t")  :trace)
      ((string-equal answer "r")  :run)
      ((string-equal answer "l")  :list)
      ((string-equal answer "e")  :eval)
      ((string-equal answer "d")
       (restart-case
           (progn (invoke-debugger (make-condition 'step-condition
                                                   :message (if thunk
                                                                (with-output-to-string (out)
                                                                  (funcall thunk out))
                                                                "Step")))
                  :step-into)
         (step-into     () :report "Step Into"      (progn :step-into))
         (step-over     () :report "Step Over"      (progn :step-over))
         (step-trace    () :report "Trace"          (progn :trace))
         (step-function () :report "Function"       (progn :function))
         (step-run      () :report "Run"            (progn :run))
         (abort         () :Report "Abort Stepping" (progn :abort))))
      ((member answer '("a" "q") :test (function string-equal))
       :abort)
      (t
       (step-choice thunk)))))


;; Instrumentation:
;; The step-* functions are called by macros to generate the stepping
;; code. Usually, calling a call-step-* function that does the actual
;; work.

(defun substitute-ignorable (declarations)
  (mapcar (lambda (declaration)
            (destructuring-bind (declare &rest items) declaration
              `(,declare
                ,@(mapcar (lambda (item)
                            (if (consp item)
                                (destructuring-bind (op &rest args) item
                                  (if (eq 'ignore op)
                                      `(ignorable ,@args)
                                      item))
                                item))
                          items))))
          declarations))

(assert (equalp
         (substitute-ignorable '((declare (type q x) (ignore x))
                                 (declare (ignore z))
                                 (declare (type p z))
                                 (declare thing)))
         '((declare (type q x) (ignorable x))
           (declare (ignorable z))
           (declare (type p z))
           (declare thing))))



(defun trivial-atom-p (atom)
  "Trivial atoms are either (quote something) forms, or atoms that are self-evaluating."
  (typecase atom
    (keyword        t) ; self evaluating
    ((member nil t) t) ; self evaluating
    (symbol         nil) ; variable or symbol-macro
    (cons           (eq 'cl:quote (car atom)))
    (t              t))) ; self evaluating



(defun print-form (label form)
  (handler-case
      (let ((*print-pretty* t))
        (format *step-trace-output* "~&~S~2%" form))
    (error (err)
      (format *step-trace-output* "~&Cannot print the ~A for ~A~%" label err))))


(defvar       *rep-history* 0)
(defparameter *rep-eof*     (gensym "EOF"))
(defun read-eval-print ()
  "
DO:        Implements a minimalist CL Read-Eval-Print body.
"
  (catch 'repl
    (finish-output *standard-output*)
    (finish-output *trace-output*)
    (finish-output *error-output*)
    (finish-output *debug-io*)
    (format *debug-io* "~%~A[~D]> " (package-name *package*) (incf *rep-history*))
    (finish-output *debug-io*)
    (handling-errors
     (setf - (read *debug-io* nil *rep-eof*))
     (when (or (eq - *rep-eof*)
               (and (listp -)
                    (null (rest -))
                    (member (first -) '(quit  exit continue)
                            :test (function string-equal))))
       (return-from read-eval-print))
     (let ((results (multiple-value-list (eval -))))
       (setf +++ ++   ++ +   + -
             /// //   // /   / results
             *** **   ** *   * (first /)))
     (finish-output *standard-output*)
     (finish-output *trace-output*)
     (finish-output *error-output*)
     (format *debug-io* "~& --> ~{~S~^ ;~%     ~}~%" /))
    (format *debug-io* "~%")
    (finish-output *debug-io*)))


(defmacro doagain (&body body)
  (let ((again (gensym "again")))
    `(values-list
      (block ,again
        (tagbody
           ,again
           (return-from ,again (multiple-value-list
                                (macrolet ((again () `(go ,',again)))
                                  ,@body))))))))


(defun call-step-atom (atom thunk)
  (case *step-mode*
    ((:run :function)
     (funcall thunk))
    (:trace
     (let ((results (let ((*step-level* (1+ *step-level*)))
                      (multiple-value-list (funcall thunk)))))
       (if (= 1 (length results))
           (unless (trivial-atom-p atom)
             (with-parens *step-trace-output* 
               (with-step-printing
                   (format *step-trace-output* "~S ==> ~S" atom (first results)))))
           (with-parens *step-trace-output* 
             (with-step-printing
                 (did-step atom results))))
       (values-list results)))
    (:step
     (flet ((do-step ()
              (let ((results (let ((*step-level* (1+ *step-level*)))
                               (multiple-value-list (funcall thunk)))))
                (with-step-printing
                    (if (= 1 (length results))
                        (if (trivial-atom-p atom) 
                            (format *step-trace-output* "~&~V<~> (--> ~S)" *step-level* atom)
                            (format *step-trace-output* "~&~V<~> (~S ==> ~S)"
                                    *step-level* atom (first results)))
                        ;; (will-step display-form)
                        (did-step atom results)))
                (values-list results))))
       (with-parens *step-trace-output*
         (doagain
          (ecase (step-choice (lambda (out) (will-step atom out)))
            (:abort     (throw 'abort-stepping nil))
            (:eval      (read-eval-print)            (again))
            (:list      (print-form "atom" atom)     (again))
            (:run       (setf *step-mode* :run)      (funcall thunk))
            (:function  (setf *step-mode* :function) (funcall thunk))
            (:trace     (setf *step-mode* :trace)    (do-step))
            (:step-over (let ((*step-mode* :run))    (do-step)))
            (:step-into                              (do-step)))))))))


(defun step-atom (object)
  `(call-step-atom ',object (lambda () ,object)))

(defun step-simple-form (actual &optional (form actual))
  `(call-step-atom ',form (lambda () ,actual)))


(defun call-simple-step (thunk display-form)
  (flet ((do-step ()
           (will-step display-form)
           (let ((results (let ((*step-level* (1+ *step-level*)))
                            (multiple-value-list (funcall thunk)))))
             (did-step display-form results)
             (values-list results))))
    (case *step-mode*
      ((:run :function)  (funcall thunk))
      (:trace  (with-parens *step-trace-output* (do-step)))
      (:step   (with-parens *step-trace-output*
                 (doagain
                  (ecase (step-choice (lambda (out) (will-step display-form out)))
                    (:abort     (throw 'abort-stepping nil))
                    (:eval      (read-eval-print)                (again))
                    (:list      (print-form "form" display-form) (again))
                    (:run       (setf *step-mode* :run)          (funcall thunk))
                    (:function  (setf *step-mode* :function)     (do-step))
                    (:trace     (setf *step-mode* :trace)        (do-step))
                    (:step-over (let ((*step-mode* :run))        (do-step)))
                    (:step-into                                  (do-step)))))))))


(defun simple-step (form &optional (display-form form))
  `(call-simple-step (lambda () ,form) ',display-form))


(defun step-body (where body env)
  (multiple-value-bind (docstring declarations body) (parse-body where body)
    (append docstring
            (substitute-ignorable declarations)
            (mapcar (lambda (form)
                      (step-expression form env))
                    body))))


(defun call-step-function (name specializers pnames pvals thunk)
  (labels ((report-enter (out)
             (with-step-printing
                 (format out "Entering ~:[anonymous ~;~]function ~:*~:[~;~:*~S~]"
                         (if specializers (cons name specializers) name)))
             (let ((*step-level* (1+ *step-level*)))
               (mapc (function did-bind) pnames pvals)))
           (report-exit (non-local-exit results out)
             (with-step-printing
                 (format out "~&~V<~> Exiting  ~:[anonymous ~;~]function ~:*~:[~;~:*~S ~]~
                          ~:[returned ~:[no result~;~R result~:P~]~;by non-local exit.~]"
                         *step-level* (if specializers (cons name specializers) name)
                         non-local-exit results (length results)))
             (if (= 1 (length results))
                 (format out " ==> ~S" (first results))
                 (print-step-results results out)))
           (do-step ()
             (let ((results        '())
                   (non-local-exit t))
               (unwind-protect
                    (setf results (let ((*step-level* (1+ *step-level*)))
                                    (multiple-value-list (funcall thunk)))
                          non-local-exit nil)
                 (unless (eq *step-mode* :run)
                   (report-exit non-local-exit results *step-trace-output*))
                 (when (member name *break-functions-exit* :test (function equal))
                   (doagain
                    (ecase (step-choice (lambda (out) (declare (ignore out))))
                      (:abort     (throw 'abort-stepping nil))
                      (:eval      (read-eval-print) (again))
                      (:list      (print-form "function call" (cons name (mapcar (lambda (val) `(quote ,val)) pvals)))
                                  (again))
                      (:run       (setf *step-mode* :run))
                      (:function  (setf *step-mode* :function))
                      (:trace     (setf *step-mode* :trace))
                      (:step-over (setf *step-mode* :step))
                      (:step-into (setf *step-mode* :step))))))
               (values-list results)))
           (choice (report)
             (doagain
              (ecase (step-choice report)
                (:abort     (throw 'abort-stepping nil))
                (:eval      (read-eval-print) (again))
                (:list      (print-form "function call" (cons name (mapcar (lambda (val) `(quote ,val)) pvals)))
                            (again))
                (:run       (setf *step-mode* :run)      (do-step))
                (:function  (setf *step-mode* :function) (do-step))
                (:trace     (setf *step-mode* :trace)    (do-step))
                (:step-over (let ((*step-mode* :run))    (do-step)))
                (:step-into                              (do-step))))))
    (let ((*step-current-trace-depth* (1+ *step-current-trace-depth*)))
      (if (member name *break-functions-entry* :test (function equal))
          (with-parens *step-trace-output*
            (choice (function report-enter)))
          (case *step-mode*
            ((:run)
             ;; (print (list (not (not (member name *trace-functions* :test (function equal)))) name *trace-functions*))
             (if (member name *trace-functions* :test (function equal))
                 (let ((*step-mode* :trace)
                       (*step-current-trace-depth* 0)) ; reset it
                   (with-parens *step-trace-output*
                     (report-enter *step-trace-output*)
                     (do-step)))
                 (do-step)))
            ((:function :trace)
             (if (and *step-max-trace-depth*
                      (< *step-max-trace-depth* *step-current-trace-depth*))
                 (let ((*step-mode* :run))
                   (do-step))
                 (with-parens *step-trace-output*
                   (report-enter *step-trace-output*)
                   (do-step))))
            ((:step)
             (with-parens *step-trace-output*
               (choice (function report-enter)))))))))



(declaim (declaration stepper))
(pushnew :com.informatimago.common-lisp.lisp.cl-stepper *features*)

(defun stepper-declaration-p (declarations keyword)
  (let ((result
         (dolist (declaration declarations)
           (let ((decl (and (consp declaration)
                            (eq 'declare (first declaration))
                            (find-if (lambda (specifier)
                                       (and (consp specifier)
                                            (eq 'stepper (first specifier))
                                            (member keyword (rest specifier))))
                                     (rest declaration)))))
             (when decl (return decl))))))
    ;; (format *step-trace-output* "~&(~S ~S ~S) --> ~S~%"
    ;;         'stepper-declaration-p declarations keyword result)
    result))

;; (stepper-declaration-p '((declare (ignorable object) (stepper disable))) 'disable)
;; (stepper-declaration-p '((declare (type integer x)) (declare (stepper trace))) 'trace)



(defun step-function (kind name lambda-list body env)
  "
KIND:           A lambda-list kind (:ordinary for functions,
                :specialized for methods,
                :destructuring for macros).

NAME:           The name of the defined function or macro.

LAMBDA-FORM:    A lambda form.

BODY:           A list of forms, the body of the function.

RETURN:         A stepping body.
"
  (let* ((lambda-list  (parse-lambda-list lambda-list kind))
         (parameters   (mapcar (function parameter-name)
                               (lambda-list-parameters lambda-list)))
         (specializers (when (eq kind :specialized)
                         (mapcar (lambda (parameter)
                                   (when (parameter-specializer-p parameter)
                                     (parameter-specializer parameter)))
                                  (lambda-list-parameters lambda-list)))))
    (multiple-value-bind (docstring declarations real-body) (parse-body :lambda body)
      (if (stepper-declaration-p declarations 'disable)
          (append docstring
                  declarations
                  (list (step-disabled `(progn ,@real-body))))
          (append docstring
                  (substitute-ignorable declarations)
                  (let ((form `(call-step-function
                                ',name ',specializers ',parameters (list ,@parameters)
                                (lambda ()
                                  ,@(if name
                                        `((block ,(if (consp name) (second name) name)
                                            ;; inner block for non-local exit.
                                            ,@(step-body :progn real-body env)))
                                        (step-body :progn real-body env))))))
                    (list
                     (if (stepper-declaration-p declarations 'trace)
                         `(let ((*step-mode* :trace))
                            ,form)
                         form))))))))


(defun step-lambda (lambda-form &key (kind :ordinary) name environment)
  "

ENV:            An environment.

LAMBDA-FORM:    A lambda form.

KIND:           A lambda-list kind (:ordinary for functions, :generic
                for generic functions, :specialized for methods,
                :destructuring for macros, etc).

NAME:           The name of the defined function or macro.

RETURN:         A stepping lambda-form from the LAMBDA-FORM.

"
  (destructuring-bind (lambda lambda-list &body body) lambda-form
    (declare (ignore lambda))
    `(lambda ,lambda-list
       ,@(step-function kind name lambda-list body environment))))


(defun step-bindings (mode bindings form env)
  (flet ((binding-step (var expr)
           (if (eq :sequential mode)
               `(,var (did-bind ',var ,(step-expression expr env)))
               `(,var ,(step-expression expr env)))))
    (mapcar (lambda (binding)
              (cond
                ((symbolp binding)
                 (binding-step binding 'nil))
                ((atom binding)
                 (error "Invalid atom ~S in binding list of ~S"
                        binding form))
                ((< 2 (length binding))
                 (error "Invalid binding ~S in binding list of ~S"
                        binding form))
                (t
                 (binding-step (first binding) (second binding)))))
            bindings)))


(defmacro symbol-reference (symbol &environment env)
  (let ((expansion  (macroexpand symbol env)))
    (if (eq symbol expansion)
        (step-atom symbol)
        (step-expression expansion env))))

(defmacro self-evaluating (object)
  (step-atom object))

(defun step-function-call (form env)
  (destructuring-bind (function-name &rest arguments) form
    (if (consp function-name)
        (if (member (first function-name)
                    '(com.informatimago.common-lisp.lisp.stepper:lambda lambda))
            `(progn
               ;; (print '(2) *step-trace-output*)
               (simple-step `(,(step-lambda function-name :environment env)
                               ,@(mapcar (lambda (argument) (step-expression argument env))
                                         arguments))
                            form))
            (error "Invalid object used as function name ~S in function call ~S"
                   function-name form))
        `(progn
           ;; (print '(3) *step-trace-output*)
           ,(simple-step `(,function-name
                           ,@(mapcar (lambda (argument) (step-expression argument env))
                                     arguments))
                         form)))))


(defun step-expression (form env)
  ;; Operators in CL-STEPPER are macros, so they're taken care of
  ;; automatically.
  (cond
    ((symbolp form)  `(symbol-reference ,form))
    ;; The other atoms are unchanged:
    ((atom form)     `(self-evaluating ,form))
    ;; Now we have a list.  
    (t
     (case (first form)
       
       ;; First we check the real CL special operators:
       ;; We just step them wholesale. (If there are macros inside
       ;; they'll be expanded and we may step them.
       ((function quote)
        (step-simple-form form))
       ((block catch eval-when flet go if labels let let*
               load-time-value locally macrolet multiple-value-call
               multiple-value-prog1 progn progv return-from setq
               symbol-macrolet tagbody the throw unwind-protect)
        (simple-step form))

       ;; Next we check for the stepper macros.  Since they already
       ;; expand to simple-step, we just use them as is, unless
       ;; they're toplevelness protected forms:
       ((com.informatimago.common-lisp.lisp.stepper:function
         com.informatimago.common-lisp.lisp.stepper:quote)
        (step-simple-form form))
       ((com.informatimago.common-lisp.lisp.stepper:block
            com.informatimago.common-lisp.lisp.stepper:catch
          ;; com.informatimago.common-lisp.lisp.stepper:eval-when
          com.informatimago.common-lisp.lisp.stepper:flet
          com.informatimago.common-lisp.lisp.stepper:go
          com.informatimago.common-lisp.lisp.stepper:if
          com.informatimago.common-lisp.lisp.stepper:labels
          com.informatimago.common-lisp.lisp.stepper:let
          com.informatimago.common-lisp.lisp.stepper:let*
          com.informatimago.common-lisp.lisp.stepper:load-time-value
          ;; com.informatimago.common-lisp.lisp.stepper:locally
          com.informatimago.common-lisp.lisp.stepper:macrolet
          com.informatimago.common-lisp.lisp.stepper:multiple-value-call
          com.informatimago.common-lisp.lisp.stepper:multiple-value-prog1
          ;; com.informatimago.common-lisp.lisp.stepper:progn
          com.informatimago.common-lisp.lisp.stepper:progv
          com.informatimago.common-lisp.lisp.stepper:return-from
          com.informatimago.common-lisp.lisp.stepper:setq
          com.informatimago.common-lisp.lisp.stepper:symbol-macrolet
          com.informatimago.common-lisp.lisp.stepper:tagbody
          com.informatimago.common-lisp.lisp.stepper:the
          com.informatimago.common-lisp.lisp.stepper:throw
          com.informatimago.common-lisp.lisp.stepper:unwind-protect)
        form)
       (otherwise
        (if (macro-function (first form) env)
            ;; For a macro, we let the host CL expand it:
            (simple-step form)
            ;; For a function, we step the arguments:
            (step-function-call form env)))))))



;;;; THE END ;;;;
