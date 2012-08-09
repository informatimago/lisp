;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               stepper-functions.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An internal packageo of the Common Lisp stepper.
;;;;    This package exports the stepper interactive functions,
;;;;    and the stepper generator functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-08-09 <PJB> Extracted from stepper.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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



(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.INTERACTIVE")


(defvar *step-mode* :run
  "May be :run, :trace or :step.

:run     don't print anything, just evaluate the forms.  

:trace   just prints the forms and their results as they are evaluted.

:step    prints the form, then ask the user what to do (step over,
         step into, trace, run).

When break-points are implemented, :run and :trace will run until a
break-point is reached.

")


(defvar *trace-functions* '()
  "A list of function names that we must trace with the stepper.")

(defvar *break-functions-entry* '()
  "A list of function names that we must break into the stepper upon entry.")

(defvar *break-functions-exit* '()
  "A list of function names that we must break into the stepper upon exit.")



(defmacro step-trace (&rest fnames)
  "
RETURN: the list of function names added.
"
  `(mapc (lambda (fname) (pushnew fname *trace-functions* :test (function equal)))
         ',fnames))


(defmacro step-untrace (&rest fnames)
  "
RETURN: The list of step-traced functions remaining.
"
  `(progn
     (mapc (lambda (fname) (setf *trace-functions* (delete fname *trace-functions* :test (function equal))))
          ',fnames)
     *trace-functions*))


(defmacro step-break-entry (&rest fnames)
  "
RETURN: the list of function names added.
"
  `(mapc (lambda (fname) (pushnew fname *break-entry-functions* :test (function equal)))
         ',fnames))


(defmacro step-unbreak-entry (&rest fnames)
  "
RETURN: The list of step-break-entryd functions remaining.
"
  `(progn
     (mapc (lambda (fname) (setf *break-entry-functions* (delete fname *break-entry-functions* :test (function equal))))
          ',fnames)
     *break-entry-functions*))


(defmacro step-break-exit (&rest fnames)
  "
RETURN: the list of function names added.
"
  `(mapc (lambda (fname) (pushnew fname *break-exit-functions* :test (function equal)))
         ',fnames))


(defmacro step-unbreak-exit (&rest fnames)
  "
RETURN: The list of step-break-exitd functions remaining.
"
  `(progn
     (mapc (lambda (fname) (setf *break-exit-functions* (delete fname *break-exit-functions* :test (function equal))))
          ',fnames)
     *break-exit-functions*))






(defvar *step-level* 0
  "The level.")

(defvar *step-package*         (find-package :cl-user))
(defvar *step-print-length*    10)
(defvar *step-print-level*      3)
(defvar *step-print-readably* nil)
(defvar *step-print-case*     :downcase)

(defmacro with-step-printing (&body body)
  `(let ((*print-length*   *step-print-length*)
         (*print-level*    *step-print-level*)
         (*print-readably* *step-print-readably*)
         (*print-case*     *step-print-case*)
         (*package*        *step-package*))
     ,@body))

;; Tracing steps:

(defun will-step (form &optional (stream *trace-output*))
  (with-step-printing
      (format stream "~&~V<~>Will evaluate ~S~%" *step-level* form)))

(defun did-bind (variable value &optional (stream *trace-output*))
  "
RETURN: VALUE
"
  (with-step-printing
      (format stream "~&~V<~>Bind ~16A to ~S~%" *step-level* variable value))
  value)

(defun print-step-results (results &optional (stream *trace-output*))
  (when results
    (with-step-printing
        (let ((start "-->"))
          (dolist (result results)
            (format stream "~V<~>~A ~S~%" *step-level* start result)
            (setf start "   "))))))

(defun did-step (form results &optional (stream *trace-output*))
  (with-step-printing
      (format stream "~&~V<~>Evaluation of ~S returned ~:[no result~;~R result~:P~]~%"
              *step-level* form results (length results)))
  (print-step-results results))

(defun did-tag (tag &optional (stream *trace-output*))
  (with-step-printing
      (format stream "~&~V<~>Passed tag ~S~%" *step-level* tag)))


;; Interactive stepping:

(define-condition step-condition (condition)
  ((message :initarg :message :initform "Step" :reader step-message))
  (:report (lambda (condition stream)
               (format stream "~A" (step-message condition)))))

(defun step-choice (&optional thunk)
  (when thunk (funcall thunk *trace-output*))
  (with-step-printing
      (format *query-io* "~V<~>~{~A~^, ~}?"
              *step-level*
              '("Step Into (s, si, RET)" "Step over (so)" "Trace (t)"
                "Run (r)" "Debugger (d)" "Abort (a, q)")))
  (let ((answer (string-trim " " (read-line *query-io*))))
    (cond
      ((member answer '("" "s" "si") :test (function string-equal))
       :step-into)
      ((string-equal answer "so")
       :step-over)
      ((string-equal answer "t")
       :trace)
      ((string-equal answer "r")
       :run)
      ((string-equal answer "d")
       (restart-case
           (progn (invoke-debugger (make-condition 'step-condition
                                                   :message (if thunk
                                                              (with-output-to-string (out)
                                                                (funcall thunk out))
                                                              "Step1")))
                  :step-into)
         (step-into  () :report "Step Into"      (progn :step-into))
         (step-over  () :report "Step Over"      (progn :step-over))
         (step-trace () :report "Trace"          (progn :trace))
         (step-run   () :report "Run"            (progn :run))
         (abort      () :Report "Abort Stepping" (progn :abort))))
      ((member answer '("a" "q") :test (function string-equal))
       :abort)
      (t
       (step-choice thunk)))))


;; Instrumentation:

(defun call-simple-step (thunk display-form)
  (flet ((do-step ()
           (will-step display-form)
           (let ((results (let ((*step-level* (1+ *step-level*)))
                            (multiple-value-list (funcall thunk)))))
             (did-step display-form results)
             (values-list results))))
    (case *step-mode*
      (:run    (funcall thunk))
      (:trace  (do-step))
      (:step   (ecase (step-choice (lambda (out) (will-step display-form out)))
                 (:abort     (throw 'abort-stepping nil))
                 (:run       (setf *step-mode* :run)   (funcall thunk))
                 (:trace     (setf *step-mode* :trace) (do-step))
                 (:step-into (do-step))
                 (:step-over (let ((*step-mode* :run)) (do-step))))))))

(defun simple-step (form &optional (display-form form))
  `(call-simple-step (lambda () ,form) ',display-form))


(defun step-body (where body env)
  (multiple-value-bind (docstring declarations body) (parse-body where body)
    (append (when docstring (list docstring))
            declarations
            (mapcar (lambda (form) (step-expression form env)) body))))


(defun call-step-function (name pnames pvals thunk)
  (labels ((report-enter (out)
             (with-step-printing
                 (format out "~&~V<~>Entering ~:[anonymous ~;~]function ~:*~:[~;~:*~S~]~%"
                         *step-level* name))
             (let ((*step-level* (1+ *step-level*)))
               (mapc (function did-bind) pnames pvals)))
           (report-exit (non-local-exit results out)
             (with-step-printing
                 (format out "~&~V<~>Exiting  ~:[anonymous ~;~]function ~:*~:[~;~:*~S ~]~
                          ~:[returned ~:[no result~;~R result~:P~]~;by non-local exit.~]~%"
                         *step-level* name non-local-exit results (length results)))
             (print-step-results results))
           (do-step ()
             (let ((results        '())
                   (non-local-exit t))
               (unwind-protect
                   (setf results (let ((*step-level* (1+ *step-level*)))
                                   (multiple-value-list (funcall thunk)))
                         non-local-exit nil)
                 (if (eq :run *step-mode*)
                   (when (member name *break-functions-exit* :test (function equal))
                     (choice (lambda (out) (report-exit non-local-exit results out))))
                   (report-exit non-local-exit results *trace-output*)))
               (values-list results)))
           (choice (report)
             (ecase (step-choice report)
               (:abort     (throw 'abort-stepping nil))
               (:run       (setf *step-mode* :run)   (do-step))
               (:trace     (setf *step-mode* :trace) (do-step))
               (:step-into (do-step))
               (:step-over (let ((*step-mode* :run)) (do-step))))))
    (if (member name *break-functions-entry* :test (function equal))
      (choice (function report-enter))
      (case *step-mode*
        (:run     (if (member name *trace-functions*)
                    (let ((*step-mode* :trace))
                      (report-enter *trace-output*)
                      (do-step))
                    (do-step)))
        (:trace   (report-enter *trace-output*)
                  (do-step))
        (:step    (choice (function report-enter)))))))


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
  (let ((parameters (mapcar (function parameter-name)
                            (lambda-list-parameters
                             (parse-lambda-list lambda-list kind)))))
    `(call-step-function ',name ',parameters (list ,@parameters)
                         ,(if name
                              `(lambda () (block ,name ,@(step-body :lambda body env)))
                              `(lambda () ,@(step-body :lambda body env))))))


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
    `(cl:lambda ,lambda-list
         ,(step-function kind name lambda-list body environment))))


(defun step-bindings (mode bindings form env)
  (flet ((binding-step (var expr)
           (if (eq :sequential mode)
             `(,var (did-bind ',var ,(step-expression expr env)))
             `(,var ,(step-expression expr env)))))
    (mapcar (lambda (binding)
                (cond
                  ((symbolp binding)
                   `(,binding
                     ,(binding-step binding 'nil)))
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
      (simple-step symbol)
      (step-expression expansion env))))

(defmacro self-evaluating (object)
  (simple-step object))

(defun step-function-call (form env)
  (destructuring-bind (function-name &rest arguments) form
    (if (consp function-name)
      (if (member (first function-name)
                  '(com.informatimago.common-lisp.lisp.common-lisp-stepper:lambda cl:lambda))
        (simple-step `(,(step-lambda function-name :environment env)
                        ,@(mapcar (lambda (argument) (step-expression argument env))
                                  arguments))
                     form)
        (error "Invalid object used as function name ~S in function call ~S"
               function-name form))
      (simple-step `(,function-name
                     ,@(mapcar (lambda (argument) (step-expression argument env))
                               arguments))
                   form))))

(defun step-expression (form env)

  ;; Operators in stepper-cl are macros, so they're taken care of
  ;; automatically.
  
  (cond
    ((symbolp form)  `(symbol-reference ,form))
    ;; The other atoms are unchanged:
    ((atom form)     `(self-evaluating ,form))
    ;; Now we have a list.  
    (t
     (case (first form)
       ;; First we check the real CL special operators:
       ((block catch eval-when flet function go if labels let let*
               load-time-value locally macrolet multiple-value-call
               multiple-value-prog1 progn progv quote return-from setq
               symbol-macrolet tagbody the throw unwind-protect)
        ;; then we just step it wholesale. (If there are macros inside
        ;; they'll be expanded and we may step them.
        (simple-step form))
       (otherwise
        (if (macro-function (first form) env)
          ;; For a macro, we let the host CL expand it:
          (simple-step form)
          ;; For a fucntion, we step the arguments:
          (step-function-call form env)))))))

;;;; THE END ;;;;
