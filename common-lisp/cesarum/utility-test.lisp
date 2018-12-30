(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")

(defun test/get-option ()
  (mapc (lambda (expected-arguments)
          (destructuring-bind (expected arguments) expected-arguments
            (handler-case
                (let ((result (apply (function get-option) arguments)))
                  (if (equal expected result)
                      result
                      (error "For ~S, expected ~S, got ~S" (cons 'get-option arguments) expected result)))
              (error (err)
                (if (eql expected 'error)
                    err
                    (error "For ~S, expected ~S, got ~S" (cons 'get-option arguments) expected err))))))
        '((nil                         (:foo (:bar (:quux))))
          (nil                         (:foo (:bar (:quux))           :list))
          (:symbol                     (:foo (:bar :foo (:quux))))
          (error                       (:foo (:bar :foo (:quux) :foo)))
          (:singleton                  (:foo (:bar (:foo) (:quux))))
          ((nil)                       (:foo (:bar (:foo nil) (:quux))))
          ((foo)                       (:foo (:bar (:foo foo) (:quux))))
          ((foo bar baz)               (:foo (:bar (:foo foo bar baz) (:quux))))
          ((:foo)                      (:foo (:bar :foo (:quux))                   :list))
          ((:foo :foo)                 (:foo (:bar :foo (:quux) :foo)              :list))
          (((:foo) :foo (:foo foo))    (:foo (:bar (:foo) (:quux) :foo (:foo foo)) :list))))
  :success)

(defun test/all ()
  (test/get-option))


(mapcar
 (lambda (expected-arguments)
   (destructuring-bind (expected arguments) expected-arguments
     (handler-case
         (let ((results (multiple-value-list
                         (apply (function parse-structure-name-and-options) arguments))))
           results
           #-(and)
           (if (equal expected results)
               results
               (error "For ~S, expected ~S, got ~S" (cons 'parse-structure-name-and-options arguments) expected results)))
       (error (err)
         (if (eql expected 'error)
             err
             (error "For ~S, expected ~S, got ~S" (cons 'parse-structure-name-and-options arguments) expected err))))))
 '(
   (()  (point))
   ))

(assert
 (equalp
  (mapcar (lambda (arguments)
            (print
             (mapcan (function list)
                     '(:name :conc-name :constructors :copier :include :initial-offset :predicate :print-function :print-object :structure-type-p :structure-type)
                     (multiple-value-list (apply (function parse-structure-name-and-options) arguments)))))
          '((point)
            ((point))
            ((point :conc-name))
            ((point (:conc-name)))
            ((point (:conc-name nil)))
            ((point (:conc-name pt-)))
            ((point :copier))
            ((point (:copier)))
            ((point (:copier nil)))
            ((point (:copier copy-pt)))

            ((point :conc-name  :copier))
            ((point (:conc-name) (:copier)))
            ((point (:conc-name nil) (:copier nil)))
            ((point (:conc-name pt-)  (:copy copy-pt)))
           
            ((point :constructor))
            ((point (:constructor)))
            ((point (:constructor nil)))
            ((point (:constructor %make-pt)))
            ((point :constructor (:constructor %make-pt) (:constructor %make-a-pt (x y))))
            ((point (:print-function)))
            ((point (:print-function nil)))
            ((point (:print-function print-pt)))
            ((point :print-object))
            ((point (:print-object)))
            ((point (:print-object nil)))
            ((point (:print-object print-pt)))
            ))

  '((:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name || :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name || :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name || :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name pt- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier nil :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-pt :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name || :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name || :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name || :constructors (make-point) :copier nil :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name pt- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors nil :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (%make-pt) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point %make-pt (%make-a-pt (x y))) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function print-pt :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object nil :structure-type-p nil :structure-type nil) 
    (:name point :conc-name point- :constructors (make-point) :copier copy-point :include nil :initial-offset nil :predicate point-p :print-function nil :print-object print-pt :structure-type-p nil :structure-type nil))))


#-(and)
(progn
  (pprint (macroexpand-1 '(define-structure-class point x y)))
  (pprint (macroexpand-1 '(define-structure-class (point :conc-name) x y)))
  (pprint (macroexpand-1 '(define-structure-class (point (:conc-name)) x y)))
  (pprint (macroexpand-1 '(define-structure-class (point (:conc-name nil)) x y)))
  (pprint (macroexpand-1 '(define-structure-class (point (:conc-name pt-)) x y)))

  (pprint (macroexpand-1 '(define-structure-class (point :predicate) x y)))
  (pprint (macroexpand-1 '(define-structure-class (point (:predicate)) x y)))
  (pprint (macroexpand-1 '(define-structure-class (point (:predicate nil)) x y)))
  (pprint (macroexpand-1 '(define-structure-class (point (:predicate ptp)) x y)))
  )
