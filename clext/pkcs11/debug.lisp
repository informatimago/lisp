(in-package "COM.INFORMATIMAGO.CLEXT.PKCS11")

(defvar *dump-prefix* "")
(defun dump-vector (vector &key print-characters)
  (let ((*print-circle* nil)
        (size (length vector)))
    (loop
      :for i :from 0 :by 16
      :while (< i size)
      :do (format t "~&~A~16,'0X: " *dump-prefix* i)
          (loop
            :repeat 16
            :for j :from i
            :if (< j size)
              :do (format t "~2,'0X " (aref vector j))
            :else
              :do (write-string "   "))
          (when print-characters
           (loop
             :repeat 16
             :for j :from i
             :if (< j size)
               :do  (format t "~C" (let ((code (aref vector j)))
                                     (if (<= 32 code 126)
                                         (code-char code)
                                         #\.)))
             :else
               :do (write-string " "))))
    :finally (terpri)))

             ;; (print (list :ok) *trace-output*) (finish-output *trace-output*)
             ;; (let ((*template* template))
             ;;   (declare (special *template*))
             ;;   (proclaim '(special *template*))
             ;;   (com.informatimago.common-lisp.interactive.interactive:repl))

                 ;; (print '(:attribute-sensitive :attribute-type-invalid :buffer-too-small) *trace-output*)
                 ;; (print (list 'get-attribute-value  (list 'template-decode template)) *trace-output*)
                 ;; (finish-output *trace-output*)
                 ;; (let ((*template* template)
                 ;;       (*error* err))
                 ;;   (declare (special *template* *error*))
                 ;;   (proclaim '(special *template* *error*))
;;   (com.informatimago.common-lisp.interactive.interactive:repl))

(defun resume ()
  (com.informatimago.common-lisp.interactive.interactive:repl-exit))

(defun pause (bindings message &rest arguments)
  (format t "~&~?~%" message arguments)
  (format t "Type (resume) to resume.~%")
  (progv
      (mapcar (function first)  bindings)
      (mapcar (function second) bindings)
    (com.informatimago.common-lisp.interactive.interactive:repl)))

;;;; THE END ;;;;
