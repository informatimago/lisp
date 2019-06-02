(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(defun linc-eval (form)
  0)

(defun load-linc-file (input-file &key output-file
                                    (verbose *compile-verbose*)
                                    (print *compile-print*)
                                    (external-format :default)
                                    (if-does-not-exist :error))
  (let ((*package*         (com.informatimago.common-lisp.interactive.interactive:mkupack
                            :name "com.informatimago.languages.linc.c-"
                            :use '("COM.INFORMATIMAGO.LANGUAGES.LINC.C")))
        (*readtable*       (copy-readtable com.informatimago.languages.linc::*c-readtable*))
        (*compile-verbose* verbose)
        (*compile-print*   print)
        (warnings-p        nil)
        (failure-p         nil))
    (with-open-file (input input-file
                           :external-format external-format
                           :if-does-not-exist (when if-does-not-exist :error))
      (handler-bind ((warning       (lambda (condition)
                                        (declare (ignore condition))
                                        (incf warnings-p)
                                        nil))
                       (style-warning (lambda (condition)
                                        (declare (ignore condition))
                                        nil))
                       (error         (lambda (condition)
                                        (format *error-output* "~&;; ~A~%" condition)
                                        (invoke-restart (find-restart 'continue-translation condition)))))
          (loop
            :for form := (read input nil input)
            :until (eql form input)
            :do (when print
                  (format t "~&~S~%" form))
                (let ((result  (linc-eval form)))
                  (when verbose
                    (format t "~&-> ~S~%" result)))
            :finally (return t))))))

;;;; THE END ;;;;
