(define-condition interrupt-signal-condition (condition)
  ()
  (:report "interrupt signal"))

(defvar *debug-on-interrupt* nil)

(defun keyboard-interrupt (condition)
  ;; (format t "~S ~A ~%~S = ~A~%"
  ;;         'keyboard-interrupt condition
  ;;         '*debug-on-interrupt* *debug-on-interrupt*)
  ;; (finish-output)
  (if *debug-on-interrupt*
      (break "Keyboard Interrupt")
      (invoke-restart (find-restart 'resume condition))))

(defun test/interrupt (&optional debug-on-interrupt)
  ;; we must set the global variable for the benefit of the running thread.
  (setf *debug-on-interrupt* debug-on-interrupt)
  (let* ((iota (bt:make-thread (lambda ()
                                 (unwind-protect
                                      (handler-bind ((interrupt-signal-condition
                                                       (function keyboard-interrupt)))
                                        (loop
                                          :for i :from 1
                                          :do (with-simple-restart (resume "Resume Loop")
                                                (sleep 1)
                                                (princ i) (princ " ")
                                                (finish-output))))
                                   (princ "Done") (terpri)
                                   (finish-output)))
                               :initial-bindings (list (cons '*standard-output* *standard-output*))
                               :name "iota runner")))
    (sleep 10)
    (bt:interrupt-thread iota
                         (function signal)
                         (make-condition 'interrupt-signal-condition))
    (princ "Complete.") (terpri) (finish-output)))




;; #-(and)
;; (map nil 'print
;; (sort (map 'list
;;            (lambda (name) 
;;              (let ((ce (babel::get-character-encoding name)))
;;                (list (babel::enc-name ce) 
;;                      (babel::enc-max-units-per-char ce))))
;;            (babel::list-character-encodings))  (function <) :key (function second)))

