(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.PACKAGE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.PACKAGE")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.PACKAGE.TEST")

(define-test test/check-disjoints ()
  (assert-true (null (check-disjoints (list "S1" "S2" "S3")
                                      (list (list "P1" (list "P1A" "P1B" "P1C"))
                                            (list "P2" (list "P2A" "P2B" "P2C")))
                                      (list (list "P3" (list "I1A" "I1B" "I1C"))
                                            (list "P4" (list "I2A" "I2B" "I2C")))
                                      (list "I1" "I2" "I3")
                                      (list "E1" "E2" "E3"))))

  (assert-true (null (check-disjoints (list "S1" "S2" "S3")
                                      '()
                                      (list (list "P3" (list "I1A" "I1B" "I1C"))
                                            (list "P4" (list "I2A" "I2B" "I2C")))
                                      '()
                                      (list "E1" "E2" "E3"))))

  (assert-true (nth-value 1 (ignore-errors (check-disjoints (list "S1" "S2" "S3")
                                                            (list (list "P1" (list "P1A" "P1B" "P1C"))
                                                                  (list "P2" (list "P2A" "P2B" "P2C" "S3")))
                                                            (list (list "P3" (list "I1A" "I1B" "I1C"))
                                                                  (list "P4" (list "I2A" "I2B" "I2C")))
                                                            (list "I1" "I2" "I3")
                                                            (list "E1" "E2" "E3")))))

  (assert-true (null (check-disjoints (list "S1" "S2" "S3")
                                      (list (list "P1" (list "P1A" "P1B" "P1C"))
                                            (list "P2" (list "P2A" "P2B" "P2C")))
                                      (list (list "P3" (list "I1A" "I1B" "I1C"))
                                            (list "P4" (list "I2A" "I2B" "I2C")))
                                      (list "I1" "I2" "I3")
                                      (list "E1" "E2" "E3" "S2")))))

(define-test test/declarations/body ()
    (assert-true (equal (mapcar (lambda (body) (list (declarations body) (body body)))
                                '(()
                                  ((declare (ignore x)))
                                  ((declare (ignore x)) (declare (ignore y)))
                                  ((print w) (print z))
                                  ((declare (ignore x)) (print w) (print z))
                                  ((declare (ignore x)) (declare (ignore y)) (print w) (print z))))
                        '((nil nil)
                          (((declare (ignore x))) nil)
                          (((declare (ignore x)) (declare (ignore y))) nil)
                          (nil ((print w) (print z)))
                          (((declare (ignore x))) ((print w) (print z)))
                          (((declare (ignore x)) (declare (ignore y))) ((print w) (print z)))))))

(define-test test/all ()
  (test/check-disjoints)
  (test/declarations/body))

;;;; THE END ;;;;

