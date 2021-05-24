(define-condition interrupt-condition (condition)
  ((source :initarg :source
           :initform nil
           :reader interrupt-condition-source))
  (:report (lambda (condition stream)
             (format stream "Interrupted~@[ from ~A~]"
                     (interrupt-condition-source condition)))))

(defvar *debug-on-interrupt* nil)

(defun interrupt-handler (condition)
  (format t "~S ~A ~%~S = ~A~%"
          'interrupt-handler condition
          '*debug-on-interrupt* *debug-on-interrupt*)
  (force-output)
  (if *debug-on-interrupt*
      (break "~@[~:(~A~) ~]Interrupt"
             (interrupt-condition-source condition))
      (invoke-restart (find-restart 'resume condition))))

(defmacro with-interrupt-handler (&body body)
  `(handler-bind ((interrupt-condition
                    (function interrupt-handler)))
     (macrolet ((with-resume-restart (&body body)
                  `(with-simple-restart (resume "Resume")
                     ,@body)))
       ,@body)))

(defun caller ()
  #+ccl (third (ccl:backtrace-as-list))
  #-ccl nil)

(defun signal-interrupt (thread &optional source)
  (let ((source (or source (caller))))
   (bt:interrupt-thread thread (function signal)
                        (make-condition 'interrupt-condition
                                        :source source))))



(defun test/interrupt (&optional debug-on-interrupt)
  ;; we must set the global variable for the benefit of the running thread.
  (setf *debug-on-interrupt* debug-on-interrupt)
  (let* ((iota (bt:make-thread (lambda ()
                                 (unwind-protect
                                      (with-interrupt-handler
                                        (loop
                                          :for i :from 1
                                          :do (with-resume-restart
                                                  (sleep 1)
                                                (princ i) (princ " ")
                                                (finish-output))))
                                   (princ "Iota Done") (terpri)
                                   (finish-output)))
                               :initial-bindings (list (cons '*standard-output* *standard-output*))
                               :name "iota runner")))
    (sleep 10)
    (signal-interrupt iota "keyboard")
    (princ "Interrupter Complete.") (terpri) (force-output)
    (unless *debug-on-interrupt*
      (sleep 5)
      (bt:destroy-thread iota)
      (princ "Killed iota runner.") (terpri) (force-output))))


;; #-(and)
;; (map nil 'print
;; (sort (map 'list
;;            (lambda (name)
;;              (let ((ce (babel::get-character-encoding name)))
;;                (list (babel::enc-name ce)
;;                      (babel::enc-max-units-per-char ce))))
;;            (babel::list-character-encodings))  (function <) :key (function second)))
