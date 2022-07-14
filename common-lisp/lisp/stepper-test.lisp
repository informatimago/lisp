(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.TEST")

;;;
;;; Unit tests:
;;;

(define-test test/step-atom ()
  (check equal
         (cl-stepper::step-atom 42)
         '(com.informatimago.common-lisp.lisp.stepper.internal::call-step-atom
          '42
          (lambda nil 42))))

(define-test test/step-simple-form ()
  (check equal
         (cl-stepper::step-simple-form '(function sin))
         '(com.informatimago.common-lisp.lisp.stepper.internal::call-step-atom
           '#'sin
           (lambda nil #'sin)))
  (check equal
         (cl-stepper::step-simple-form '(quote 33))
         '(com.informatimago.common-lisp.lisp.stepper.internal::call-step-atom
           ''33
           (lambda nil '33)))
  (check equal
         (cl-stepper::step-simple-form '(cl-stepper:function sin))
         '(com.informatimago.common-lisp.lisp.stepper.internal::call-step-atom
           '(com.informatimago.common-lisp.lisp.stepper:function sin)
           (lambda nil (com.informatimago.common-lisp.lisp.stepper:function sin))))
  (check equal
         (cl-stepper::step-simple-form '(cl-stepper:quote 33))
         '(com.informatimago.common-lisp.lisp.stepper.internal::call-step-atom
           '(com.informatimago.common-lisp.lisp.stepper:quote 33)
           (lambda nil (com.informatimago.common-lisp.lisp.stepper:quote 33)))))

(define-test test/simple-step ()
  (check equal
         (cl-stepper::simple-step '(print 1))
         '(com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
           (lambda nil (print 1))
           '(print 1))))

(define-test test/step-body ()
  (check equal
         (cl-stepper::step-body :locally '((print 1) (print 2) (print 3)) nil)
         '((progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                   (lambda nil (print (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 1)))
                   '(print 1)))
           (progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                   (lambda nil (print (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 2)))
                   '(print 2)))
           (progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                   (lambda nil (print (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 3)))
                   '(print 3))))))


(define-test test/step-function-call ()
  (check equal
         (com.informatimago.common-lisp.lisp.stepper.internal::step-function-call '(print 1) nil)
         '(progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                  (lambda nil (print (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 1)))
                  '(print 1))))
  (check equal
         (com.informatimago.common-lisp.lisp.stepper.internal::step-function-call '((lambda (x) (print x) 42)) nil)
         '(progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                  (lambda nil
                    ((lambda (x)
                       (com.informatimago.common-lisp.lisp.stepper.internal::call-step-function
                        'nil
                        'nil
                        '(x)
                        (list x)
                        (lambda nil
                          (progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                                  (lambda nil
                                    (print (com.informatimago.common-lisp.lisp.stepper.internal::symbol-reference x)))
                                  '(print x)))
                          (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 42))))))
                  '((lambda (x) (print x) 42)))))
  (check equal
         (com.informatimago.common-lisp.lisp.stepper.internal::step-function-call '((cl-stepper:lambda (x) (print x) 42)) nil)
         '(progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                  (lambda nil
                    ((lambda (x)
                       (com.informatimago.common-lisp.lisp.stepper.internal::call-step-function
                        'nil
                        'nil
                        '(x)
                        (list x)
                        (lambda nil
                          (progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                                  (lambda nil
                                    (print (com.informatimago.common-lisp.lisp.stepper.internal::symbol-reference x)))
                                  '(print x)))
                          (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 42))))))
                  '((com.informatimago.common-lisp.lisp.stepper:lambda (x) (print x) 42))))))

(define-test test/step-expression ()
  (check equal
         (cl-stepper::step-expression '(cl-stepper:progn (print 1) (print 2)) nil)

         '(com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
           (lambda nil
             (progn (progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                            (lambda nil (print (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 1)))
                            '(print 1)))
                    (progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                            (lambda nil (print (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 2)))
                            '(print 2)))))
           '(progn
             (progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                     (lambda nil (print (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 1)))
                     '(print 1)))
             (progn (com.informatimago.common-lisp.lisp.stepper.internal::call-simple-step
                     (lambda nil (print (com.informatimago.common-lisp.lisp.stepper.internal::self-evaluating 2)))
                     '(print 2)))))))

;;;
;;; Integration tests:
;;;

(define-test test/step-abort ()
  (handler-case
      #1=(with-input-from-string (in (format nil "a~%"))
           (with-output-to-string (out)
             (let* ((*query-io* (make-two-way-stream in out))
                    (cl-stepper:*step-trace-output* *query-io*))
               (cl-stepper:step
                   (cl-stepper:progn (print 1)
                                     (print 2))))))
      (:no-error (&rest ignored)
        (declare (ignore ignored))
        (progress-success))
      (control-error (err)
        (progress-failure-message '#1# "Aborting the stepping signals ~A" err))))


(define-test test/all ()
  (test/step-atom)
  (test/step-simple-form)
  (test/simple-step)
  (test/step-body)
  (test/step-function-call)
  (test/step-expression)
  (test/step-abort))

;; (test/all)

