(in-package :cliki)

(defclass strip-html-stream (buffered-output-stream) 
  ((output-stream :initarg :output-stream
		  :accessor strip-html-stream-output-stream)))

(defun end-of-tag (buffer start)
  (let ((in-string-p nil) (escaped-p nil))
    (loop for i from start below (length buffer)
	  for c = (elt buffer i)
	  do
	  (cond
	    (escaped-p
	     (setf escaped-p nil))
	    ((and in-string-p (eql c #\"))
	     (setf in-string-p nil))
	    (in-string-p
	     nil)
	    ((eql c #\>)
	     (return  (1+ i)))
	    ((eql c #\")
	     (setf in-string-p t))))))

(defun write-sequence-graphic-chars (b stream &key (start 0) end)
  (let ((end (or end (length b))))
    (loop
     (let* ((w-pos (position-if-not #'graphic-char-p b :start start :end end))
	    (w-end (and w-pos (position-if
			       #'graphic-char-p b :start w-pos :end end))))
       (unless (< start end) (return))
       (write-sequence b stream :start start :end (or w-pos end))
       (unless (and w-pos w-end) (return))
       (princ #\Space stream)
       (setf start w-end)))))

(defmethod buffered-output-stream-write-buffer ((stream strip-html-stream)
						&optional force-p)
  (let* ((b (buffered-output-stream-buffer stream))
	 (out (strip-html-stream-output-stream stream))
	 (start 0) (open 0) (close 0))
    (loop
     (setf open (position #\< b :start start))
     (write-sequence-graphic-chars b out :start start :end open)
     (unless open (return (length b)))
     (setf close (end-of-tag b open))
     (let ((name (subseq b open (position #\Space b :start open))))
       (if (member name
		   '("br" "hr" "h1" "h2" "h3" "h4" "h5" "h6" "p" "div")
		   :test #'string-equal)
	   (terpri out)
	   (princ #\Space out)))
     (unless close
       (if force-p
	   (setf close (length b)) ;exit
	   (return open)))
     (setf start close))))


#|
(with-open-file (i "/var/www/cliki/cirCLe")
  (let ((o (make-instance 'strip-html-stream :output-stream *standard-output*)))
    (do ((c (read-char i nil nil)  (read-char i nil nil)))
	((null c) nil)
      (write-char c o))
    (close o)))
|#
