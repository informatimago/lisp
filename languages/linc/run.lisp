(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COMMON-LISP-USER")
(defparameter *compile-linc-c-files* t)

(load (make-pathname :name "generate" :type "lisp" :version nil
                      :defaults (or *load-truename*
                                    (error "This file must be loaded as source."))))

;; (cc (translate-linc-file "test-expressions.sexpc" :print t :verbose t)
;;     :output "test-expressions.o" :to :object :options '("-Werror" "-Wall"))
