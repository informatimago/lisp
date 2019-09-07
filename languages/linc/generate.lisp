(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COMMON-LISP-USER")
(defvar *compile-linc-c-files* nil)
(defparameter *this-directory* (make-pathname :name nil :type nil :version nil
                                              :defaults (or *load-truename*
                                                            (error "This file must be loaded as source."))))
(cd cl-user::*this-directory*)
(pushnew *this-directory* asdf:*central-registry* :test (function equalp))
(ql:quickload :com.informatimago.languages.linc)
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(defparameter *test-files* '("test-include.sexpc"
                             "test-macros.sexpc"
                             "test-types.sexpc"
                             "test-expressions.sexpc"
                             "test-statements.sexpc"))

(defun call-dofiles (files thunk)
  (dolist (path files)
    (funcall thunk path
             ;; (make-pathname :name (pathname-name path) :type
             ;;                       (pathname-type path) :version
             ;;                       nil :defaults
             ;;                       cl-user::*this-directory*)
             )))

(defmacro dofiles ((var test-files) &body body)
  `(call-dofiles ,test-files (lambda (,var) ,@body)))

(if cl-user::*compile-linc-c-files*
    (dofiles (test-sexpc *test-files*)
      (cl-user::cc (translate-linc-file test-sexpc :print t :verbose t)
                   :output (make-pathname :type "o" :defaults test-sexpc :case :local)
                   :to :object
                   :options '("-Werror" "-Wall")))
    (dofiles (test-sexpc *test-files*)
      (translate-linc-file test-sexpc :print t :verbose t)))
