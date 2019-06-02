(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(translate-linc-file "test-include.sexpc" :print t :verbose t)

(cc (translate-linc-file "test-types.sexpc" :print t :verbose t)
    :output "test-types.o" :to :object :options '("-Werror" "-Wall"))
