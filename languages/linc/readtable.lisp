(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(define-condition simple-stream-error (stream-error simple-condition)
  ()
  (:report (lambda (condition stream)
             (format stream "~?"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))))

(defparameter *c-readtable-without-reader-macros*
  (let ((rt (copy-readtable nil)))
    (set-syntax-from-char #\# #\a rt)
    (setf (readtable-case rt) :preserve)
    rt))

(defvar *c-readtable*)

(defun read-c-sexp-list (stream)
  (let ((*package*   (load-time-value (find-package *c-package-name*)))
        (*readtable* *c-readtable*))
    (loop
      :with c-sexps := '()
      :for ch := (peek-char t stream)
      :do (case ch
            ((#\})
             (read-char stream)
             (return (nreverse c-sexps)))
            ((#\;)
             (read-line stream)
             (return (read-c-sexp-list stream)))
            ((#\#)
             (read-char stream)
             (if (char= #\| (peek-char nil stream))
                 (let ((object-after-comment (read stream)))
                   (if (eql *c-closing-brace* object-after-comment)
                       (return (nreverse c-sexps))
                       (push object-after-comment c-sexps)))
                 (progn
                   (unread-char #\# stream)
                   (push (read stream) c-sexps))))
            (otherwise
             (push (read stream) c-sexps))))))

(defun reader-macro-c-sexp-list (stream ch)
  (declare (ignore ch))
  (read-c-sexp-list stream))

(defun reader-dispatching-macro-c-sexp-list (stream ch sub)
  (declare (ignore ch sub))
  (cons *c-progn* (read-c-sexp-list stream)))

(defun reader-dispatching-macro-c-preprocessor-token (stream ch sub)
  (declare (ignore ch))
  (let ((*readtable* *c-readtable-without-reader-macros*))
    (with-input-from-string (prefix (format nil "#~C" sub))
      (read (make-concatenated-stream prefix stream)))))

(defparameter *c-spaces* #(#\space #\tab #\newline #\page))

(defun read-dot-and-ellipsis (stream)
  (let ((buffer (make-array 80 :element-type 'character
                               :fill-pointer 0
                               :adjustable t)))
    (vector-push #\. buffer)
    (loop
      :for ch := (peek-char nil stream)
      :while (char= #\. ch)
      :do (vector-push-extend (read-char stream) buffer)
      :finally (return
                 (multiple-value-bind (fun non-terminating-p) (get-macro-character ch)
                   (if (if fun non-terminating-p (not (find ch *c-spaces*)))
                       (with-input-from-string (buffer-stream buffer)
                         (let ((input (make-concatenated-stream buffer-stream stream))
                               (*readtable* *c-readtable-without-reader-macros*))
                           (read input)))
                       (case (length buffer)
                         ((1 3) (intern buffer))
                         (otherwise (error 'simple-stream-error
                                           :stream stream
                                           :format-control "Invalid token ~S"
                                           :format-arguments (list buffer))))))))))

(defun reader-macro-dot-and-ellipsis (stream ch)
  (declare (ignore ch))
  (read-dot-and-ellipsis stream))

(defun set-c-sexp-reader-macros (readtable)
  (set-macro-character #\"                            (function read-c-string)                             nil readtable)
  (set-macro-character #\.                            (function reader-macro-dot-and-ellipsis)             t   readtable)
  (set-macro-character *c-opening-brace*              (function reader-macro-c-sexp-list)                  nil readtable)
  ;; (set-dispatch-macro-character #\# #\i               (function reader-dispatching-macro-c-preprocessor-token) readtable)
  ;; (set-dispatch-macro-character #\# #\e               (function reader-dispatching-macro-c-preprocessor-token) readtable)
  ;; (set-dispatch-macro-character #\# *c-opening-brace* (function reader-dispatching-macro-c-sexp-list)          readtable)
  readtable)

(defmacro enable-c-sexp-reader-macros (&optional (readtable '*readtable*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-c-sexp-reader-macros ,readtable)))

(defparameter *c-readtable*
  (let ((rt (copy-readtable *c-readtable-without-reader-macros*)))
    (enable-c-sexp-reader-macros rt)
    rt)
  "Readtable to read S-expified C code.")

(defun print-c-sexp-form (form &optional (*standard-output* *standard-output*))
  (let ((*package* (load-time-value (find-package *c-package-name*)))
        (*readtable* *c-readtable*)
        (*print-right-margin* 72))
    (write-string "{")
    (pprint form)
    (terpri)
    (write-line "}")
    (values)))

;;;; THE END ;;;;
