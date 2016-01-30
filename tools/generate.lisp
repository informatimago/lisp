(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
;; (quick-load-all)

(defun save-program (name &key init-file main)
  #+ccl (ccl::save-application name
                               :toplevel-function (when main
                                                    (lambda ()
                                                      (handler-case
                                                          (funcall main (rest ccl:*command-line-argument-list*))
                                                        (error (err)
                                                          (finish-output *standard-output*)
                                                          (finish-output *trace-output*)
                                                          (format *error-output* "~%~A~%" err)
                                                          (finish-output *error-output*)
                                                          (ccl:quit 1)))
                                                      (finish-output *standard-output*)
                                                      (finish-output *trace-output*)
                                                      (finish-output *error-output*)
                                                      (ccl:quit 0)))
                               :init-file init-file
                               :mode #o755
                               :prepend-kernel t
                               :error-handler t)
  #-(or ccl) (error "Not implemented for ~A" (lisp-implementation-type)))
