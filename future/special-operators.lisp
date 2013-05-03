


(define-special-operator (function name)
    (cl:function name))


(define-special-operator (quote literal) 
    (cl:quote literal))


(define-special-operator (if test then &optional (else nil elsep))
    (cl:if (expression test) (expression then) (expression else)))


(define-special-operator (block name &body body) 
  (cl:block name (unsplice-progn-body body)))


(define-special-operator (return-from name &optional result) 
  (cl:return-from name (expression result)))


(define-special-operator (catch object &body body) 
  (cl:catch (expression object) (unspliace-progn-body body)))


(define-special-operator (throw object result) 
  (cl:throw (expression object) (expression result)))
p

(define-special-operator (unwind-protect protected &body cleanup) 
    (cl:unwind-protect (expression protected)
      (unsplice-progn-body cleanup)))


(define-special-operator (tagbody &body body) 
    (cl:tagbody (unsplice-tagbody body)))


(define-special-operator (go tag) 
  (cl:go tag))


(define-special-operator (flet (&rest bindings) &body body)
    (cl:flet
        (map bindings (name lambda-list &body body)
             (name lambda-list (unsplice-lambda-body body)))
        (unsplice-locally-body body)))

(define-special-operator (labels (&rest bindings) &body body)
    (cl:labels
        (map bindings (name lambda-list &body body)
             (name lambda-list (unsplice-lambda-body body)))
      (unsplice-locally-body body)))


(define-special-operator (setq var val &rest pairs)
    (cl:progn
      (cl:setq var (expression val))
      (map pairs (var val)
           (cl:setq var (expression val)))))



(define-special-operator (let (&rest bindings) &body body) 
    (cl:let
        (map bindings (or (var val)
                          (var)
                          var)
             (var (expression (or val nil))))
      (unsplice-locally-body body)))

(define-special-operator (let* (&rest bindings) &body body) 
    (cl:let*
        (map bindings (or (var val)
                          (var)
                          var)
             (var (expression (or val nil))))
      (unsplice-locally-body body)))


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


(define-special-operator (progn &body body) (&environment env)
  ;; We must preserve toplevelness.
  `(cl:progn
     ,@(step-body :progn body env)))


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
  (multiple-value-bind (ds declarations real-body) (parse-body :locally body)
    (declare (ignore ds real-body))
    (cl:if (stepper-disabled-p declarations)
      `(cl:locally ,@(rest form))
      (simple-step `(cl:locally
                        ,@(step-body :locally body env))
                   form))))



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


(define-special-operator (eval-when (&rest situations) &body body) (&environment env)
  ;; We must preserve toplevelness.
  `(cl:eval-when (,@situations)
     ,@(step-body :progn body env)))


(define-special-operator (symbol-macrolet (&rest bindings) &body body) (&whole form &environment env)
  (multiple-value-bind (ds declarations real-body) (parse-body :locally body)
    (declare (ignore ds real-body))
    (cl:if (stepper-disabled-p declarations)
      `(cl:symbol-macrolet ,@(rest form))
      (simple-step `(cl:symbol-macrolet ,bindings
                      ,@(step-body :locally body env))
                   form))))

(define-special-operator (macrolet (&rest bindings) &body body) (&whole form &environment env)
  (multiple-value-bind (ds declarations real-body) (parse-body :locally body)
    (declare (ignore ds real-body))
    (cl:if (stepper-disabled-p declarations)
      `(cl:macrolet ,@(rest form))
      (simple-step `(cl:macrolet ,bindings
                        ,@(step-body :locally body env))
                   form))))

(define-special-operator (load-time-value expression &optional read-only-p) (&whole form &environment env)
  (simple-step `(cl:load-time-value ,(step-expression expression env) ,read-only-p)
               form))

