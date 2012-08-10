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


(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER")


;;;----------------------------------------------------------------------
;;;
;;; Special operators
;;;

(cl:defmacro define-special-operator ((name &rest lambda-list) prefix &body body)
  `(cl:defmacro ,name ,(append prefix lambda-list) ,@body))


(define-special-operator (function name) (&whole form &environment env)
  (cl:if (and (consp name)
              (eq 'lambda (first name)))
    (simple-step (step-expression name env) form)
    (simple-step `(cl:function ,name)   form)))


(define-special-operator (quote literal) (&whole form)
    (simple-step `(cl:quote ,literal) form))


(define-special-operator (if test then &optional (else nil elsep)) (&whole form &environment env)
    (simple-step `(cl:if ,(step-expression test env)
                    ,(step-expression then env)
                    ,(step-expression else env))
                 form))


(define-special-operator (block name &body body) (&whole form &environment env)
    (simple-step `(cl:block ,name
                    ,@(step-body :progn body env))
                 form))


(define-special-operator (return-from name &optional result) (&whole form &environment env)
  (simple-step `(cl:return-from ,name ,(step-expression result env))
               form))


(define-special-operator (catch object &body body) (&whole form &environment env)
  (simple-step `(cl:catch ,(step-expression object env)
                  ,@(step-body :progn body env))
               form))


(define-special-operator (throw object result) (&whole form &environment env)
  (simple-step `(cl:throw ,(step-expression object env) ,(step-expression result env))
               form))


(define-special-operator (unwind-protect protected &body cleanup) (&whole form &environment env)
  (simple-step `(cl:unwind-protect ,(step-expression protected env)
                ,@(step-body :progn cleanup env))
               form))


(define-special-operator (tagbody &body body) (&whole form &environment env)
  (simple-step `(cl:tagbody
                   ,@(mapcan (cl:lambda (form)
                                 (cl:if (or (symbolp form) (integerp form))
                                   (list form
                                         (did-tag form))
                                   (list (step-expression form env))))
                             body))
               form))


(define-special-operator (go tag) (&whole form)
  (simple-step `(cl:go ,tag) form))


(define-special-operator (flet (&rest bindings) &body body) (&whole form &environment env)
  (simple-step
   `(cl:flet ,(mapcar (cl:lambda (fun)
                          (destructuring-bind (name lambda-list &body body) fun
                            `(,name ,lambda-list
                                    ,@(step-function :ordinary name lambda-list body env))))
                      bindings)
      ,@(step-body :locally body env))
   form))

(define-special-operator (labels (&rest bindings) &body body) (&whole form &environment env)
  (simple-step
   `(cl:labels ,(mapcar (cl:lambda (fun)
                            (destructuring-bind (name lambda-list &body body) fun
                              `(,name ,lambda-list
                                      ,@(step-function :ordinary name lambda-list body env))))
                        bindings)
      ,@(step-body :locally body env))
   form))


(define-special-operator (setq var val &rest pairs) (&environment env)
  (cond
    ((null pairs)
     (cl:if (eql var (macroexpand var env))
       (simple-step `(cl:setq ,var ,(step-expression val env))
                    `(setq ,var ,val))
       (simple-step (macroexpand `(setf ,var ,val) env)
                    `(setq ,var ,val))))
    ((oddp (length pairs))
     (error "An odd number of arguments given to SETQ in ~S" `(setq ,var ,val ,pairs)))
    (t
     `(cl:progn
        ,(macroexpand `(setq ,var ,val) env)
        ,@(loop
            :for (var val) :on pairs :by (function cddr)
            :collect (macroexpand `(setq ,var ,val) env))))))



(define-special-operator (let (&rest bindings) &body body) (&whole form &environment env)
  (multiple-value-bind (ds declarations body) (parse-body :locally body)
    (declare (ignore ds))
    (simple-step `(cl:let ,(step-bindings :parallel bindings form env)
                    ;; TODO: When we did-bind the variable, they should not be declared ignore
                    ;;       so replace those declarations by ignorable.
                    ,@(substitute-ignorable declarations)
                    (unless (eq *step-mode* :run)
                      ,@(mapcar (cl:lambda (binding)
                                    (cl:let ((var (cl:if (atom binding)
                                                    binding
                                                    (first binding))))
                                      `(did-bind ',var ,var)))
                                bindings))
                    ,@(step-body :progn body env))
                 form)))


(define-special-operator (let* (&rest bindings) &body body) (&whole form &environment env)
  (simple-step `(cl:let* ,(step-bindings :sequential bindings form env)
                  ,@(step-body :locally body env))
               form))


(define-special-operator (multiple-value-call  function-form &rest arguments) (&whole form &environment env)
  (simple-step
   `(apply ,(step-expression function-form env)
           (append ,@(mapcar (cl:lambda (argument)
                                 `(cl:multiple-value-list ,(step-expression argument env)))
                             arguments)))
   form))


(define-special-operator (multiple-value-prog1 result-form &body body) (&whole form &environment env)
  (cl:let ((result (gensym)))
    (simple-step
     `(cl:let ((,result (cl:multiple-value-list ,(step-expression result-form env))))
        ,@(step-body :progn body env)
        (values-list ,result))
     form)))


(define-special-operator (progn &body body) (&whole form &environment env)
  (simple-step `(cl:progn
                ,@(step-body :progn body env))
               form))


(define-special-operator (progv symbols values &body body) (&whole form &environment env)
  (cl:let ((vsym (gensym))
           (vval (gensym)))
    (simple-step `(cl:let ((,vsym  ,(step-expression symbols env))
                           (,vval  ,(step-expression values env)))
                    (cl:progv ,vsym ,vval
                      (mapc (cl:function did-bind) ,vsym ,vval)
                      ,@(step-body :progn body env)))
                 form)))


(define-special-operator (locally &body body) (&whole form &environment env)
  (simple-step `(cl:locally
                    ,@(step-body :locally body env))
               form))



(define-special-operator (the value-type expression) (&environment env)
  ;; TODO: Check the semantics of (the (values t) (values 1 2 3))
  ;;       --> It seems (values t) == (VALUES INTEGER &REST T)
  ;; TODO: Handle (values &rest) in value-type.
  (cl:let ((results (gensym))
           (temp    (gensym)))
    (simple-step
     `(cl:let ((,results (cl:multiple-value-list ,(step-expression expression env))))
        ,(cl:if (and (listp value-type)
                     (eq 'values (first value-type)))
                `(cl:let ((,temp ,results))
                   ,@(mapcar (cl:lambda (value-type)
                                 `(check-type (pop ,temp) ,value-type))
                             (rest value-type)))
                `(check-type ,(first results) ,value-type))
        (cl:the ,value-type (values-list ,results)))
     `(the ,value-type ,expression))))


(define-special-operator (eval-when (&rest situations) &body body) (&whole form &environment env)
  (simple-step `(cl:eval-when (,@situations)
                  ,@(step-body :progn body env))
               form))


(define-special-operator (symbol-macrolet (&rest bindings) &body body) (&whole form &environment env)
  (simple-step `(cl:symbol-macrolet ,bindings
                  ,@(step-body :locally body env))
               form))

(define-special-operator (macrolet (&rest bindings) &body body) (&whole form &environment env)
  (simple-step `(cl:macrolet ,bindings
                  ,@(step-body :locally body env))
               form))

(define-special-operator (load-time-value expression &optional read-only-p) (&whole form &environment env)
  (simple-step `(cl:load-time-value ,(step-expression expression env) ,read-only-p)
               form))




;;;----------------------------------------------------------------------
;;;
;;; Macros
;;;

(cl:defmacro defun (name lambda-list &body body &environment env)
  `(cl:defun ,name ,lambda-list
     ,@(step-function :ordinary name lambda-list body env)))


(cl:defmacro defgeneric (name lambda-list &rest options &environment env)
  `(cl:defgeneric ,name ,lambda-list
     ,@(mapcar (cl:lambda (option)
                   (cl:if (and (consp option)
                               (eq :method (car option)))
                     (cl:let* ((arguments (rest option))
                               (qualifiers (loop
                                             :while (not (listp (first arguments)))
                                             :collect (pop arguments))))
                       (destructuring-bind (lambda-list &body body) arguments
                        `(:method ,@qualifiers ,lambda-list
                                  ,@(step-function :specialized name lambda-list body env))))
                     option))
               options)))


(cl:defmacro defmethod (name &rest arguments &environment env)
  (cl:let ((qualifiers (loop
                      :while (not (listp (first arguments)))
                      :collect (pop arguments))))
    (destructuring-bind (lambda-list &body body) arguments
      `(cl:defmethod ,name ,@qualifiers ,lambda-list
                     ,@(step-function :specialized name lambda-list body env)))))


(cl:defmacro lambda (&whole form &environment env lambda-list &body body)
  (declare (ignorable lambda-list body))
  (simple-step `(cl:function ,(step-lambda form :environment env))
               form))


;;;----------------------------------------------------------------------
;;;
;;; Stepper
;;;

(cl:defmacro step (form &optional (mode :step) &environment env)
  `(cl:catch 'abort-stepping
     (cl:let ((*step-mode* ,mode)
              (*step-package* *package*))
       ,(step-expression form env))))


;; ;; Let's forward the class:
;; 
;; (defclass function (cl:function)
;;   ())
;;
;; Doesn't work for meta-classes…

;;;; the END ;;;;
