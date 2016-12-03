;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               stepper.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements a Common Lisp stepper.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-08-03 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE")
  (:nicknames "STEPPER")
  (:shadow "FUNCTION-LAMBDA-EXPRESSION" "MACRO-FUNCTION" "COMPILER-MACRO-FUNCTION")
  (:shadow "STEP")
  (:export "STEP")
  (:documentation "
Implements a Common Lisp stepper.
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER")


#|Random notes.

Terminology
===========

The term "to trace" here doen't mean CL:TRACE, but executing forms
while printing each subexpressions before evaluating them and printing
their results once they return (or informing of a non-local exit).
The same printing is done while stepping, but interrupted by user
interaction to let him decide what to do next (step into, step over,
trace or "run").

The terms "to run" here means to evaluate the form (or the remaining
of the evaluation of the form) without printing any trace.

Whenever breakpoints are implemented however, tracing and running
would stop at the next breakpoint encountered.



Principle of operation
======================


Since we don't have yet the infrastructure to manage editing units
(text source along with sexp source, etc), we'll take a few shortcuts.

The main problem is to get at the source sexp for the functions and
methods to be stepped and traced.  For this, we cannot count on
``function-lambda-expression``, and it would be largely insufficient
anyways: we want to implement the CL:STEP API allowing to step into a
toplevel or stand-alone form, and we may want to trace or step into
the loading of a lisp file too.


Eager Option
------------

In the eager option, we instrument all the toplevel forms we read as
soon as we read them.

- we need to implement a LOAD function.
- difficulties with ASDF which tries to compile everything first, so:
- we need to implement a COMPILE-FILE function too.
- and of course, we may want to provide a REPL (⚠ slime).


Lazy Option
-----------

In the lazy option, we only instrument the forms and the functions or
methods needed to perform he stepping or tracing.  This require
keeping around the source forms.  Note this means definiting reader
macros (to keep text source) or macros (as done by IBCL).


Macro Option
------------

IBCL only keeps the source of predefined macros (a subset of the CL
def* macros), so we may miss user defined macros (define-such-and-such…),
unless they expand to CL def* macros.

With a macro that would expand to something like: ::

    (progn
      (setf (gethash 'some-key *some-hash*) (some-object (lambda () 'some-code)))
      (defun something ()
        'some-code))

we would instrument the function ``something``, but not the anonymous
function or code possibly returned by the ``some-object`` function.

But we can define alternate special operators as macro too.

The big advantage of this solution is that it allows to capture
closures without effort: ::

  (shadow '(let defun))
  (defmacro let (bindings &body body)
    `(cl:let ,(generate-stepping-bindings bindings)
        ,@(generate-stepping-body body)))
  (defmacro defun (name lambda-list &body body)
    `(cl:defun ,name ,lambda-list
        ,@(generate-stepping-function name lambda-list body)))
  (let ((x 42))
    (defun g () x)
    (defun s (z) (setf x z)))


    (and assert case ccase check-type cond ctypecase decf declaim defclass
    defconstant defgeneric define-compiler-macro define-condition
    define-method-combination define-modify-macro define-setf-expander
    define-symbol-macro defmacro defmethod defpackage defparameter defsetf
    defstruct deftype defun defvar destructuring-bind do do*
    do-all-symbols do-external-symbols do-symbols dolist dotimes ecase
    etypecase formatter handler-bind handler-case ignore-errors in-package
    incf lambda loop loop-finish multiple-value-bind multiple-value-list
    multiple-value-setq nth-value or pop pprint-logical-block
    print-unreadable-object prog prog* prog1 prog2 psetf psetq push
    pushnew remf restart-bind restart-case return rotatef setf shiftf step
    time trace typecase unless untrace when with-accessors
    with-compilation-unit with-condition-restarts with-hash-table-iterator
    with-input-from-string with-open-file with-open-stream
    with-output-to-string with-package-iterator with-simple-restart
    with-slots with-standard-io-syntax)

Note
----------

In ccl, a macro like cl:defmethod expands to implementation specific
special operators (ccl:nfunction) or even, to calls to internal
functions passing lambda expressions or other code chunks as data.
This would prevent their instrumenting by IBCL-like macros.


Operation
-------------


We should detect closure functions, by searching for undefined free
variables, and avoid redefining them.

|#
;;;----------------------------------------------------------------------
;;;
;;; Environment
;;;


;; variable symbol-macro constant
;; function macro special-operator compiler-macro
;; declare
;; block
;; tag

(defclass binding ()
  ((name :initarg :name :accessor binding-name
         :documentation "A name.  Usually a symbol or a list (SETF name).
Binding names are compared with EQUAL.")))



(defclass var-space-binding (binding)
  ())

(defclass variable-binding (var-space-binding)
  ((value    :initarg :value   :accessor variable-value)
   (specialp :initarg :special :initform nil            :reader variable-special-p)))

(defclass constant-binding (variable-binding)
  ())

(defclass symbol-macro-binding (var-space-binding)
  ((expansion :initarg :expansion :accessor symbol-macro-expansion)))



(defclass fun-space-binding (binding)
  ())

(defclass function-binding (fun-space-binding)
  ((lambda-expression :initarg :lambda-expression :accessor function-lambda-expression)))

(defclass macro-binding (fun-space-binding)
  ((function  :initarg :function :accessor macro-function)))

(defclass compiler-macro-binding (fun-space-binding)
  ((function  :initarg :function :accessor compiler-macro-function)))

(defclass special-operator (fun-space-binding)
  ())


(defclass tag-binding (binding)
  ()
  (:documentation "This binds a go tag (symbol or integer)."))


(defclass block-binding (binding)
  ()
  (:documentation "This binds a symbol to a block."))



(defclass environment ()
  ((next        :initform nil :accessor environment-next      :initarg :next)
   (var-space   :initform '() :accessor environment-var-space)
   (fun-space   :initform '() :accessor environment-fun-space)
   (tag-space   :initform '() :accessor environment-tag-space)
   (block-space :initform '() :accessor environment-block-space)))


(defgeneric environment-depth (environment)
  (:method ((env null))
    0)
  (:method ((env environment))
    (1+ (environment-depth (environment-next env)))))




(defmethod print-object ((self binding) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~{~S~^ ~}" (list :name (binding-name self))))
  self)

(defmethod print-object ((self variable-binding) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~{~S~^ ~}" (list :name (binding-name self)
                                     :value (if (slot-boundp self 'value)
                                              (variable-value self)
                                              :#<UNBOUND>)
                                     :specialp (variable-special-p self))))
  self)

(defmethod print-object ((self symbol-macro-binding) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~{~S~^ ~}" (list :name (binding-name self)
                                     :expansion (symbol-macro-expansion self))))
  self)

(defmethod print-object ((self environment) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~{~S~^ ~}"
            (list :depth (environment-depth self)
                  :var-space-count   (length (environment-var-space self))
                  :fun-space-count   (length (environment-fun-space self))
                  :tag-space-count   (length (environment-tag-space self))
                  :block-space-count (length (environment-block-space self)))))
  self)





(defparameter *global-environment* (make-instance 'environment))


(define-condition duplicate-binding-error (error)
  ((space       :initarg :space       :reader duplicate-binding-space)
   (old-binding :initarg :old-binding :reader duplicate-binding-old-binding)
   (new-binding :initarg :new-binding :reader duplicate-binding-new-binding))
  (:report (lambda (condition stream)
               (format stream "Duplicate ~(~A~) binding: old = ~S ; new = ~S"
                       (duplicate-binding-space       condition)
                       (duplicate-binding-old-binding condition)
                       (duplicate-binding-new-binding condition)))))



(defun %add-binding (new env space old-bindings adder)
  (let ((old (find (binding-name new) old-bindings
                   :key (function binding-name)
                   :test (function equal))))
    (if old
      (error 'duplicate-binding-error :space space
             :old-binding old :new-binding new)
      (funcall adder)))
  env)


(defgeneric add-binding (binding environment)
  (:documentation "

BINDING:        An instance of BINDING.

ENVIRONMENT:    An instance of ENVIRONMENT or NIL (denoting *GLOBAL-ENVIRONMENT*).

DO:             Adds the BINDING in the corresponding name space of
                ENVIRONMENT.  If there is already a binding with the
                same name in the same name space, then a
                DUPLICATE-BINDING-ERROR is signaled.

RETURN:         The environment denoted by ENVIRONMENT.
")
  (:method (binding (env null))
    (declare (ignorable env))
    (add-binding binding *global-environment*))
  (:method ((binding var-space-binding) (env environment))
    (%add-binding binding env
                  :variable (environment-var-space env)
                  (lambda () (push binding (environment-var-space env)))))
  (:method ((binding fun-space-binding) (env environment))
    (%add-binding binding env
                  :function (environment-fun-space env)
                  (lambda () (push binding (environment-fun-space env)))))
  (:method ((binding tag-binding) (env environment))
    (%add-binding binding env
                  :tag (environment-tag-space env)
                  (lambda () (push binding (environment-tag-space env)))))
  (:method ((binding block-binding) (env environment))
    (%add-binding binding env
                  :block (environment-block-space env)
                  (lambda () (push binding (environment-block-space env))))))


(defgeneric find-binding-1 (space name environment)
  (:documentation "

SPACE:          One of: :VAR-SPACE :FUN-SPACE, or :VARIABLE :CONSTANT
                :SYMBOL-MACRO :FUNCTION :MACRO :COMPILER-MACRO
                :SPECIAL-OPERATOR.

NAME:           A binding name.

ENVIRONMENT:    An instance of ENVIRONMENT or NIL (denoting *GLOBAL-ENVIRONMENT*).

DO:             Finds in the ENVIRONMENT (not the next ones) a binding
                with the same NAME in the name space indicated by
                SPACE.  If SPACE is not :VAR-SPACE or :FUN-SPACE then
                the type of the binding found is checked before
                returning the binding.

RETURN:         A binding named NAME in the name space SPACE of the
                ENVIRONMENT, or NIL if none is found.

")
  (:method (space name (env null))
    (find-binding-1 space name *global-environment*))
  (:method (space name (env environment))
    (let ((binding
           (find name (ecase space
                        ((:var-space :variable :constant :symbol-macro)
                         (environment-var-space env))
                        ((:fun-space :function :macro :compiler-macro :special-operator)
                         (environment-fun-space env))
                        ((:tag-space :tag)
                         (environment-tag-space env))
                        ((:block-space :block)
                         (environment-block-space env)))
                 :key (function binding-name)
                 :test (function equal))))
      (when binding
        (case space
          ((:variable)         (check-type binding variable-binding))
          ((:constant)         (check-type binding constant-binding))
          ((:symbol-macro)     (check-type binding symbol-macro-binding))
          ((:function)         (check-type binding function-binding))
          ((:macro)            (check-type binding macro-binding))
          ((:compiler-macro)   (check-type binding compiler-macro-binding))
          ((:special-operator) (check-type binding special-operator-binding))
          ((:block)            (check-type binding block-binding))
          ((:tag)              (check-type binding tag-binding)))
        binding))))


(defgeneric find-binding (space name environment)
  (:documentation "

SPACE:          One of: :VAR-SPACE :FUN-SPACE :TAG-SPACE :BLOCK-SPACE
                or one of :VARIABLE :CONSTANT :SYMBOL-MACRO :FUNCTION
                :MACRO :COMPILER-MACRO :SPECIAL-OPERATOR :TAG :BLOCK.

NAME:           A binding name.

ENVIRONMENT:    An instance of ENVIRONMENT or NIL (denoting *GLOBAL-ENVIRONMENT*).

DO:             Finds in the ENVIRONMENT or the next ones, a binding
                with the same NAME in the name space indicated by
                SPACE.  If SPACE is not :VAR-SPACE or :FUN-SPACE then
                the type of the binding found is checked before
                returning the binding.

RETURN:         A binding named NAME in the name space SPACE, or NIL
                if none is found.

")
  (:method (space name (env null))
    (find-binding-1 space name *global-environment*))
  (:method (space name (env environment))
    (or (find-binding-1 space name env)
        (find-binding space name (environment-next env)))))




#||

(add-binding (make-instance 'variable-binding :name 'x :value 42) nil)
(add-binding (make-instance 'variable-binding :name 'z :value #c(15 3)) nil)
(add-binding (make-instance 'macro-binding :name 'm
:function (lambda (whole environment)
(declare (ignorable environment))
(block m
(destructuring-bind (macro-name &rest args) whole
(declare (ignorable macro-name))
`(progn ,@args)))))
nil)

(let ((env (make-instance 'environment)))
(add-binding (make-instance 'function-binding
:name 'm
:lambda-expression '(lambda (x)
(block 'm (list 'm x 'm))))
env)
(list *global-environment*
(find-binding   :fun-space 'm nil)
(find-binding   :fun-space 'm env)
(find-binding-1 :var-space 'x nil)
(find-binding-1 :var-space 'x env)
(find-binding   :var-space 'x env)))
(#<environment :depth 1 :var-space-count 2 :fun-space-count 1 #x302001DF04BD>
#<macro-binding :name m #x302001E2E0BD>
#<function-binding :name m #x30200204F0CD>
#1=#<variable-binding :name x :value 42 :specialp nil #x302001E2F8CD>
nil
#1#)

||#


(defun extend-environment (environment &key var-space fun-space tag-space block-space)
  (let ((env (make-instance 'environment
               :next environment)))
    (dolist (var var-space)
      (add-binding var env))
    (dolist (fun fun-space)
      (add-binding fun env))
    (dolist (tag tag-space)
      (add-binding tag env))
    (dolist (block block-space)
      (add-binding block env))
    env))




(defgeneric uncompile-global (object)

  (:method ((self variable-binding))
    (if (variable-special-p self)
      (if (slot-boundp self 'value)
        `(defparameter ,(binding-name self) ,(list 'quote (variable-value self)))
        `(defvar ,(binding-name self)))
      `(deflexical ,(binding-name self)
           ,@(if (slot-boundp self 'value)
                 (list (list 'quote (variable-value self)))))))

  (:method ((self constant-binding))
    `(defconstant ,(binding-name self) ,(list 'quote (selfiable-value self))))

  (:method ((self symbol-macro-binding))
    `(define-symbol-macro ,(binding-name self) ,(symbol-macro-expansion self)))

  (:method ((self function-binding))
    (destructuring-bind (lambada lambda-list (blog name &body body))
        (function-lambda-expression self)
      (declare (ignore lambada blog name))
      `(defun ,(binding-name self) ,lambda-list ,@body)))

  (:method ((self macro-binding))
    (destructuring-bind (lambada lambda-list (blog name &body body))
        (function-lambda-expression self)
      (declare (ignore lambada blog name))
      `(defun ,(binding-name self) ,lambda-list ,@body)))

  )






;; (uncompile-global (find-binding :var-space 'x nil))
;; (deflexical x '42)



(defgeneric uncompile-environment (enviroment)
  (:method ((env null))
    ()
    (uncompile-environment *global-environment*))
  (:method ((env environment))
    ()
    ))

;; (find-binding :fun-space 'm
;;               (extend-environment
;;                (extend-environment
;;                 (extend-environment
;;                  nil
;;                  :fun-space (list
;;                              (make-instance 'function-binding
;;                                :name 'f
;;                                :lambda-expression '(lambda () (block f :outer)))
;;                              (make-instance 'function-binding
;;                                :name 'g
;;                                :lambda-expression '(lambda () (block g :outer)))
;;                              (make-instance 'function-binding
;;                                :name 'm
;;                                :lambda-expression '(lambda () (block m :outer)))))
;;                 :fun-space (list
;;                             (make-instance 'macro-binding
;;                               :name 'm
;;                               :function (cl:macro-function (defmacro m (&args)
;;                                                              `(progn ,args))))))
;;                :fun-space (list
;;                            (make-instance 'function-binding
;;                              :name 'g
;;                              :lambda-expression '(lambda () (block g :inner))))))


(defun macro-function-p (name env)
  (let* ((env (or env *global-environment*))
         (binding (find-binding :fun-space name env)))
    (cond
      ((null binding)
       (cl:macro-function name))
      ((typep binding 'macro-binding)
       binding))))



;;;----------------------------------------------------------------------
;;;
;;; Stepping & Tracing
;;;


(defvar *eval-step-mode* :trace
  "May be :run, :trace or :step.

:run     don't print anything, just evaluate the forms.

:trace   just prints the forms and their results as they are evaluted.

:step    prints the form, then ask the user what to do (step over,
         step into, trace, run).

When break-points are implemented, :run and :trace will run until a
break-point is reached.

")


(defvar *eval-step-level* 0
  "The level.")

(defun eval-will-step (form)
  (format *trace-output* "~&~V<~>Will evaluate ~S~%" *eval-step-level* form))

(defun eval-print-results (results)
  (when results
    (let ((start "-->"))
      (dolist (result results)
        (format *trace-output* "~V<~>~A ~S~%" *eval-step-level* start result)
        (setf start "   ")))))

(defun eval-did-step (form results)
  (format *trace-output* "~&~V<~>Evaluation of ~S returned ~:[no result~;~R result~:P~]~%"
          *eval-step-level* form results (length results))
  (eval-print-results results))


(defun eval-choice (&optional thunk)
  (when thunk (funcall thunk))
  (format *query-io* "~V<~>Step Into (s, si, RET), Step over (so), Trace (t), Run (r), Abort (a, q)? "
          *eval-step-level*)
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
      ((member answer '("a" "q") :test (function string-equal))
       :abort)
      (t
       (eval-choice form)))))



(defun call-eval-atom (thunk display-form)
  (flet ((do-step ()
           (eval-will-step display-form)
           (let ((results (multiple-value-list (funcall thunk))))
             (eval-did-step display-form results)
             (values-list results))))
    (case *eval-step-mode*
      (:run
       (funcall thunk))
      (:trace
       (do-step))
      (:step
       (ecase (eval-choice (lambda () (eval-will-step display-form)))
         (:abort     (throw 'abort-stepping nil))
         (:run       (setf *eval-step-mode* :run)   (funcall thunk))
         (:trace     (setf *eval-step-mode* :trace) (do-step))
         (:step-into (do-step))
         (:step-over (do-step)))))))

(defun eval-atom (atom &optional (display-atom atom))
  `(call-eval-atom (lambda () ,atom) ',display-atom))


(defun call-eval-step (thunk display-form)
  (flet ((do-step ()
           (eval-will-step display-form)
           (let ((results (let ((*eval-step-level* (1+ *eval-step-level*)))
                            (multiple-value-list (funcall thunk)))))
             (eval-did-step display-form results)
             (values-list results))))
    (case *eval-step-mode*
      (:run
       (funcall thunk))
      (:trace
       (do-step))
      (:step
       (ecase (eval-choice (lambda () (eval-will-step display-form)))
         (:abort  (throw 'abort-stepping nil))
         (:run
          (setf *eval-step-mode* :run)
          (funcall thunk))
         (:trace
          (setf *eval-step-mode* :trace)
          (do-step))
         (:step-into
          (do-step))
         (:step-over
          (let ((*eval-step-mode* :run))
            (do-step))))))))

(defun eval-step (form &optional (display-form form))
  (if (atom form)
    `(call-eval-atom (lambda () ,form) ',display-form)
    `(call-eval-step (lambda () ,form) ',display-form)))



(defun call-eval-step-function (name pnames pvals thunk)
  (labels ((report-enter (out)
             (format out "~&~V<~>Entering ~:[anonymous ~;~]function ~:*~:[~;~:*~S~]~%"
                     *eval-step-level* name)
             (loop :for pname :in pnames :for pval :in pvals :do
               (format out "~V<~>  ~16A = ~S~%" *eval-step-level* pname pval)))
           (report-exit (non-local-exit results)
             (format *trace-output* "~&~V<~>Exiting  ~:[anonymous ~;~]function ~:*~:[~;~S ~]~
                          ~:[returned ~:[no result~;~R result~:P~]~;by non-local exit.~]~%"
                     *eval-step-level* name non-local-exit results (length results))
             (eval-print-results results))
           (do-step ()
             (let ((non-local-exit t)
                   (results '()))
               (unwind-protect
                   (progn (setf results (let ((*eval-step-level* (1+ *eval-step-level*)))
                                             (multiple-value-list (funcall thunk)))
                                non-local-exit nil))
                 (report-exit non-local-exit results))
               (values-list results))))
    (case *eval-step-mode*
      (:run
       (funcall thunk))
      (:trace
       (report-enter *trace-output*)
       (do-step))
      (:step
       (ecase (eval-choice (lambda () (report-enter *query-io*)))
         (:abort  (throw 'abort-stepping nil))
         (:run
          (setf *eval-step-mode* :run)
          (funcall thunk))
         (:trace
          (setf *eval-step-mode* :trace)
          (do-step))
         (:step-into
          (do-step))
         (:step-over
          (let ((*eval-step-mode* :run))
            (do-step))))))))

(defun eval-step-lambda (env lambda-form &key (kind :ordinary) name)
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
    (let* ((parameters (mapcar (function parameter-name)
                               (lambda-list-parameters
                                (parse-lambda-list lambda-list kind))))
           (env (extend-environment env :var-space (mapcar
                                                    (lambda (pname)
                                                        (make-instance 'variable-binding
                                                          :name pname))
                                                    parameters)))
           (non-local-exit (gensym)))
      `(lambda ,lambda-list
           (call-eval-step-function
            ',name ',parameters (list ,@parameters)
            (lambda () ,@(eval-step-body env body)))))))



(defun eval-step-body (env body)
  (mapcar (lambda (form)
              (eval-expression env form))
          body))





;;;----------------------------------------------------------------------
;;;
;;; Special operators
;;;




(defparameter *special-operators* (make-hash-table))

(defmacro define-special-operator ((name &rest destructuring-lambda-list) (env) &body body)
  (let ((vform (gensym)))
    `(setf (gethash ',name *special-operators*)
           (lambda (,env ,vform)
               (declare (ignorable ,env))
             (block ,name
               (destructuring-bind (,name ,@destructuring-lambda-list) ,vform
                 (declare (ignorable ,name))
                 ,@body))))))

(defun call-special-operator (op env form)
  (let ((fun (gethash op *special-operators*)))
    (unless fun
      (error "No such special operator ~S for form ~S" op form))
    (funcall fun env form)))




(define-special-operator (function funame) (env)
  (eval-step `(cl:function ,funame)
             `(function ,funame)))


(define-special-operator (quote literal) (env)
  (eval-step `(cl:quote ,literal)
             `(quote ,literal)))


(define-special-operator (if test then &optional (else nil elsep)) (env)
  (eval-step `(cl:if ,(eval-expression env test)
                ,(eval-expression env then)
                ,(eval-expression env else))
             `(if ,test ,then ,@(when elsep (list else)))))


(define-special-operator (block name &body body) (env)
  (let ((env (extend-environment env :block-space (list
                                                   (make-instance 'block-binding
                                                     :name name)))))
    (eval-step `(cl:block ,name
                  ,@(eval-step-body env body))
               `(block ,name ,@body))))


(define-special-operator (return-from name &optional result) (env)
  (if (find-binding :block name env)
    (eval-step `(cl:return-from ,name ,(eval-expression env result))
               `(return-from ,name ,result))
    (error "~S but there's no block ~S in scope." `(return-from ,name ,result) name)))


(define-special-operator (catch object &body body) (env)
  (eval-step `(cl:catch ,(eval-expression env object)
                ,@(eval-step-body env body))
             `(catch ,object ,@body)))


(define-special-operator (throw object result) (env)
  (eval-step `(cl:throw ,(eval-expression env object) ,(eval-expression env result))
             `(throw ,object ,result)))


(define-special-operator (unwind-protect protected &body cleanup) (env)
  (eval-step `(cl:unwind-protect ,(eval-expression env protected)
                ,@(eval-step-body env cleanup))
             `(unwind-protect ,protected ,@cleanup)))


(define-special-operator (tagbody &body body) (env)
  (let* ((tags (remove-if-not (lambda (item)
                                  (or (symbolp item) (integerp item)))
                              body))
         (env (extend-environment env
                                  :tag-space (mapcar (lambda (tag)
                                                         (make-instance 'tag-binding :name tag))
                                                     tags))))
    (eval-step `(cl:tagbody
                   ,@(mapcan (lambda (form)
                                 (if (or (symbolp form) (integerp form))
                                   `(,form
                                      (trace-step "Passed tag ~S" ',form))
                                   (list (eval-expression env form))))
                             body))
               `(tagbody ,@body))))


(define-special-operator (go tag) (env)
  (if (find-binding :tag tag env)
    (eval-step `(cl:go ,tag) `(go ,tag))
    (error "~S but there's no tag ~S in scope." `(go ,tag) tag)))


(define-special-operator (flet (&rest bindings) &body body) (env)
  ;; TODO:
  (let* ((funs (mapcar (lambda (fun)
                           (make-instance 'function-binding
                             :name (first fun)
                             :lambda-expression `(lambda (second fun)
                                                     ;; get docstring and declarations from body here.
                                                     (block ,(first fun)
                                                       ;; body:
                                                       ,@(rest (rest fun))))))
                       bindings))
         (env (extend-environment env :fun-space funs)))
    (eval-step `(cl:flet ,bindings
                  ,@(eval-step-body env body))
               `(flet ,bindings ,@body))))

(define-special-operator (labels (&rest bindings) &body body) (env)
  ;; TODO:
  (eval-step `(cl:labels ,bindings
                ,@(eval-step-body env body))
             `(labels ,bindings ,@body)))


(define-special-operator (setq var val &rest pairs) (env)
  (cond
    ((null pairs)
     (let ((sm (find-binding :var-space var env)))
       (if (typep sm 'symbol-macro-binding)
         (call-special-operator 'setf env `(setf ,(symbol-macro-expansion sm) ,val))
         (eval-step `(cl:setq ,var ,(eval-expression env val))
                    `(setq ,var ,val)))))
    ((oddp (length pairs))
     (error "An odd number of arguments given to SETQ in ~S" `(setq ,var ,val ,pairs)))
    (t
     `(cl:progn
        ,(call-special-operator 'setq env `(setq ,var ,val))
        ,@(loop
            :for (var val) :on pairs :by (function cddr)
            :collect (call-special-operator 'setq env `(setq ,var ,val)))))))


(define-special-operator (let (&rest bindings) &body body) (env)
  (if (null bindings)
    (call-special-operator 'locally env `(locally ,@body))
    (let* ((vars (mapcar (lambda (binding)
                             (make-instance 'variable-binding
                               :name (cond
                                       ((symbolp binding)
                                        binding)
                                       ((atom binding)
                                        (error "Invalid atom ~S in binding list of ~S"
                                               binding `(let ,bindings ,@body)))
                                       ((/= 2 (length bindings))
                                        (error "Invalid binding ~S in binding list of ~S"
                                               binding `(let ,bindings ,@body)))
                                       (t
                                        (first binding)))))
                         bindings))
           (new-env (extend-environment env :var-space vars)))
      (eval-step `(cl:let ,(mapcar (lambda (binding)
                                       (if (symbolp binding)
                                         binding
                                         `(,(first binding) ,(eval-step (second binding)))))
                                   bindings)
                    ,@(eval-step-body new-env body))
                 `(cl:let ,bindings ,@body)))))


(define-special-operator (let* (&rest bindings) &body body) (env)
  (if (null bindings)
    (call-special-operator 'locally env `(locally ,@body))
    (call-special-operator 'let env `(let (,(first bindings))
                                       (let* (,(rest bindings))
                                         ,@body)))))


(define-special-operator (multiple-value-call  function-form &rest arguments) (env)
  (eval-step `(apply ,(eval-expression env function-form)
                     (append ,@(mapcar (lambda (argument)
                                           `(multiple-value-list ,(eval-expression env argument)))
                                       arguments)))
             `(multiple-value-call ,function-form ,@arguments)))


(define-special-operator (multiple-value-prog1 result-form &body body) (env)
  (let ((result (gensym)))
    (eval-step `(let ((,result (multiple-value-list ,(eval-expression env result-form))))
                  ,@(eval-step-body body)
                  (values-list ,result))
               `(multiple-value-prog1 ,result-form ,@body))))


(define-special-operator (progn &body body) (env)
  (eval-step `(cl:progn
                ,@(eval-step-body env body))
             `(progn ,@body)))


(define-special-operator (progv symbols values &body body) (env)
  )

(define-special-operator (locally &body body) (env)
  (eval-step `(cl:locally
                  ;; TODO: deal with declarations
                  ,@(eval-step-body env body))
             `(locally ,@body)))


(define-special-operator (the value-type expression) (env)
  ;; TODO: Check the semantics of (the (values t) (values 1 2 3))
  ;;       --> It seems (values t) == (VALUES INTEGER &REST T)
  ;; TODO: Handle (values &rest) in value-type.
  (let ((results (gensym))
        (temp (gensym)))
    (eval-step
     `(cl:let ((results (cl:multiple-value-list ,(eval-expression env expression))))
        ,(if (and (listp value-type)
                  (eq 'values (first value-type)))
             `(let ((,temp ,results))
                ,@(mapcar (lambda (value-type)
                              `(check-type (pop ,temp) ,value-type))
                          (rest value-type)))
             `(check-type ,(first result) ,value-type))
        (cl:the ,value-type (values-list ,results)))
     `(the ,value-type ,expression))))


(define-special-operator (eval-when (&rest situations) &body body) (env)
  (eval-step `(cl:eval-when (,@situations) ,@(eval-step-body env body))
             `(eval-when (,@situations) ,@body)))


(define-special-operator (symbol-macrolet (&rest bindings) &body body) (env))

(define-special-operator (macrolet (&rest bindings) &body body) (env))

(define-special-operator (load-time-value expression &optional read-only-p) (env))




(define-special-operator (symbol-reference symbol) (env)
  (declare (ignore env)) ; for now
  (let ((binding (find-binding :var-space symbol env)))
    (etypecase binding
      ((or null variable-binding)
       (eval-atom symbol))
      (symbol-macro-binding
       (eval-atom symbol) ; for now.
       ;; TODO:
       ;; (eval-expression env expansion)
       ))))


(define-special-operator (self-evaluating object) (env)
  (declare (ignore env))
  (eval-atom object))


(define-special-operator (macro-call (macro-name &rest arguments)) (env)
  ;; find macro function
  ;; macroexpand
  )








(define-special-operator (function-call (&whole form function-name &rest arguments)) (env)
  (if (consp function-name)
    (if (eq 'lambda (first function-name))
      (eval-step `(,(eval-step-lambda env function-name)
                    ,@(mapcar (lambda (argument) (eval-expression env argument))
                              arguments))
                 form)
      (error "Invalid object used as function name ~S in function call ~S"
             function-name form))
    (let ((binding (find-binding :fun-space function-name env)))
      (etypecase binding
        ((or null function-binding)
         (eval-step `(,function-name
                      ,@(mapcar (lambda (argument) (eval-expression env argument))
                                arguments))
                    form))))))



#+ccl (define-special-operator (ccl:compiler-let (&rest bindings) &body body) (env)
        (call-special-operator 'let env `(let ,bindings ,@body)))



(defun eval-expression (env form)
  (cond
    ((symbolp form)
     (call-special-operator 'symbol-reference env `(symbol-reference ,form)))
    ;; The other atoms are unchanged:
    ((atom form)
     (call-special-operator 'self-evaluating env `(self-evaluating ,form)))
    ;; Now we have a list.
    (t
     (case (first form)
       ;; First we check the special operators:
       ((block catch eval-when flet function go if labels let let*
               load-time-value locally macrolet multiple-value-call
               multiple-value-prog1 progn progv quote return-from setq
               symbol-macrolet tagbody the throw unwind-protect)
        (call-special-operator (first form) env form))
       (otherwise
        (if (macro-function-p (first form) env)
          (call-special-operator 'macro-call    env `(macro-call ,form))
          (call-special-operator 'function-call env `(function-call ,form))))))))


;;;----------------------------------------------------------------------
;;;
;;; Stepper
;;;

(defmacro step (form)
  `(catch 'abort-stepping
     ,(eval-expression nil `(progn ,form))))




;; (setf *print-circle* nil)
;; (eval-step-lambda nil '(lambda (a b &optional o &rest r &key k1 k2 ((kk3 k3) nil k3p) &aux a1 a2)))
;; (lambda (a b &optional o &rest r &key k1 k2 ((kk3 k3) nil k3p) &aux a1 a2)
;;     (call-eval-step-function 'nil '#1=(a b o r k1 k2 k3 a1 a2) (list . #1#) (lambda nil)))
;;
;; (pprint (eval-step-lambda
;;          nil
;;          '(lambda (a b &optional o &rest r &key k1 k2 ((kk3 k3) nil k3p) &aux a1 a2)
;;            (let ((c 1) (b 2)) (if (< a b) (+ (* b 3) (truncate 10 3)) (print 'hello))))))
;;
;;
;; (lambda (a b &optional o &rest r &key k1 k2 ((kk3 k3) nil k3p) &aux a1 a2)
;;     (call-eval-step-function
;;      'nil
;;      '(a b o r k1 k2 k3 a1 a2)
;;      (list a b o r k1 k2 k3 a1 a2)
;;      (lambda nil
;;          (call-eval-step
;;           (lambda nil
;;               (let ((c (call-eval-atom (lambda nil 1) '1)) (b (call-eval-atom (lambda nil 2) '2)))
;;                 (call-eval-step
;;                  (lambda nil
;;                      (if (call-eval-step
;;                           (lambda nil
;;                               (< (call-eval-atom (lambda nil a) 'a) (call-eval-atom (lambda nil b) 'b)))
;;                           '(< a b))
;;                        (call-eval-step
;;                         (lambda nil
;;                             (+ (call-eval-step
;;                                 (lambda nil
;;                                     (* (call-eval-atom (lambda nil b) 'b)
;;                                      (call-eval-atom (lambda nil 3) '3)))
;;                                 '(* b 3))
;;                              (call-eval-step
;;                               (lambda nil
;;                                   (truncate (call-eval-atom (lambda nil 10) '10)
;;                                    (call-eval-atom (lambda nil 3) '3)))
;;                               '(truncate 10 3))))
;;                         '(+ (* b 3) (truncate 10 3)))
;;                        (call-eval-step
;;                         (lambda nil (print (call-eval-step (lambda nil 'hello) ''hello)))
;;                         '(print 'hello))))
;;                  '(if (< a b) (+ (* b 3) (truncate 10 3)) (print 'hello)))))
;;           '(let ((c 1) (b 2)) (if (< a b) (+ (* b 3) (truncate 10 3)) (print 'hello)))))))

;;;; THE END ;;;;
