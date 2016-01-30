(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(in-package "COMMON-LISP-USER")

(cd (make-pathname :name nil :type nil :version nil
                    :defaults (load-time-value *load-pathname*)))
(pushnew (pwd) asdf:*central-registry*)

(ql:quickload :com.informatimago.common-lisp.virtual-file-system)

(in-package "VFS-USER")
