(ql:quickload :bordeaux-threads)

(defun list-threads (&optional (threads (bt:all-threads)) (*standard-output* *standard-output*))
  (loop
    :named menu
    :for i :from 1
    :for thread :in threads
    :do (format *standard-output* "~&~2D) ~A~%" i thread))
  (values))

(defun kill-thread (&optional thread (*query-io* *query-io*))
  (if thread
      (bt:destroy-thread thread)
      (loop
        :named select
        :do (let ((threads (bt:all-threads)))
              (list-threads threads *query-io*)
              (format *query-io* "~&Number of thread to kill (or 0 to abort): ")
              (let ((choice (let ((*read-eval* nil)) (read *query-io*))))
                (cond
                  ((not (integerp choice)))
                  ((zerop choice)
                   (format *query-io* "~&Aborted.~%")
                   (return-from select))
                  ((<= 1 choice (length threads))
                   (bt:destroy-thread (nth (1- choice) threads))
                   (return-from select)))
                (format *query-io* "~&Invalid answer, try again.~%")))))
  (values))


;; (loop :repeat 3 :do (bt:make-thread (lambda () (sleep 2232))))
;; (kill-thread)
