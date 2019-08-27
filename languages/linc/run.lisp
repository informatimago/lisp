(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(cl-user::import-commands)
(cd #P"~/src/public/lisp/languages/linc/")

(dolist (test-sexpc '("test-include.sexpc"
                      "test-macros.sexpc"
                      "test-types.sexpc"
                      "test-expressions.sexpc"
                      "test-statements.sexpc"))
  (cc (translate-linc-file test-sexpc :print t :verbose t)
      :output (make-pathname :type "o" :defaults test-sexpc :case :local)
      :to :object
      :options '("-Werror" "-Wall")))

;; (ql:quickload :com.informatimago.languages.linc)
(cc (translate-linc-file "test-expressions.sexpc" :print t :verbose t)
    :output "test-expressions.o" :to :object :options '("-Werror" "-Wall"))
