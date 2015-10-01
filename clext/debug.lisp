(defpackage "COM.INFORMATIMAGO.CLEXT.DEBUG"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS")
  (:shadow "WITH-LOCK-HELD")
  (:export "TR" "WITH-LOCK-HELD"))
(in-package "COM.INFORMATIMAGO.CLEXT.DEBUG")


(defvar *tr-lock*   (make-lock "trace"))
(defvar *tr-output* *standard-output*)
(defun tr (fc &rest a)
  (bt:with-lock-held (*tr-lock*)
    (format *tr-output* "~&~30A: ~?~&" (thread-name (current-thread))  fc a)))


(defmacro with-lock-held ((place) &body body)
  (let ((lock (gensym)))
    `(let ((,lock ,place))
       (tr "will acquire lock ~A" (ccl:lock-name ,lock))
       (unwind-protect
            (bt:with-lock-held (,lock)
              (tr "acquired lock ~A" (ccl:lock-name ,lock))
              ,@body)
         (tr "released lock ~A" (ccl:lock-name ,lock))))))

;;;; THE END ;;;;
