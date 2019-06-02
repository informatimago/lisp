(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(assert (string= (with-output-to-string (*c-out*)
           (generate (parse-pointer-type  '(|pointer| |restrict| (|pointer| |const| |int|)))))
         "int * const * restrict"))
