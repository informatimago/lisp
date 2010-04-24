(in-package :cliki)

;;; and given all that, here's the elided-stream.

(defclass elided-stream (buffered-output-stream)
  ((important-words :initarg :important-words
		    :accessor elided-stream-important-words)
   (context-characters :initarg :context-characters
		       :initform 50
		       :accessor elided-stream-context-characters)
   (output-stream :initarg :output-stream
		  :accessor elided-stream-output-stream)))

(defun find-elision-boundary-backward (buffer index max)
  (let* ((para (or (position #\Newline buffer
			     :start (max 0 (- index max))
			     :end index :from-end t) 0))
	 (sentence (or (position #\. buffer
				 :start (max 0 (- index max))
				 :end index :from-end t) 0))
	 (stop (max (- index max) para  sentence)))
    (or (position-if-not #'word-char-p
			 buffer :start (min (1+ stop) index) :end index)
	stop)))

(defun find-elision-boundary-forward (buffer index max)
  (let* ((max (if (> (+ index max) (length buffer))
		  (- (length buffer) index)
		  max))
	 (end (+ index max))
	 (para (or (position #\Newline buffer :start index :end end) end))
	 (sentence (or (position #\. buffer :start index :end end) end))
	 (stop (min end para sentence)))
    (or (position-if-not #'word-char-p
			 buffer :start index :end  stop :from-end t)
	stop)))

(defmethod elided-stream-highlight-words ((stream elided-stream) start-index end-index words-start)
  (let ((b (buffered-output-stream-buffer stream))
	(max (elided-stream-context-characters stream))
	(out-stream (elided-stream-output-stream stream)))
    (write-sequence
     b out-stream
     :start
     (loop for i in words-start
	   for end = (position-if-not
		      #'alphanumericp b :start i)
	   and last-end =
	   (find-elision-boundary-backward b (car words-start) max)
	   then end
	   do (progn
		(write-sequence b out-stream :start last-end :end i)
		(write-sequence "<b>" out-stream)
		(write-sequence b out-stream :start i :end end)
		(write-sequence "</b>" out-stream))
	   finally (return end))
     :end (find-elision-boundary-forward b (car (last words-start)) max))
    nil))

(defun search-word (word buffer start)
  (let ((stem (stem-for-word word)))
    (loop
     (let* ((p (search stem buffer :start2 start :test #'char-equal) )
	    (end (and p (position-if-not #'word-char-p buffer :start p))))
       (if p
	   (if (and
		(or (< p 1) (not (word-char-p (elt buffer (1- p)))))
		(equal stem (stem-for-word (subseq buffer p end))))
	       (return-from search-word (values p  (+ p (length word))))
	       (setf start (1+ p)))
	   (return-from search-word nil))))))



(defmethod buffered-output-stream-write-buffer ((stream elided-stream)
						&optional force-p)
  (let* ((b (buffered-output-stream-buffer stream))
	 (out (elided-stream-output-stream stream))
	 (words
	  (remove-if-not (lambda (x) (> (length x) 0))
			 (elided-stream-important-words stream)))
	 (threshold (elided-stream-context-characters stream))
	 (end-index 0)
	 (start-index 0)
	 interesting-indices iword next-iword)
    (labels ((find-interesting-word (start)
	       (unless (< start (length b))
		 (return-from find-interesting-word nil))
	       (loop for w in words
		     for p = (search-word w b start)
		     when p minimize p into min
		     and collect t into foundp
		     finally (return (and foundp min)))))
      (loop
       (setf iword (find-interesting-word end-index))
       ;;(format t "iword  ~A ~A~%" iword (subseq b (- iword 8) (+ iword 8)))
       (unless iword
	 ;(format t "no iword in this block ~S~%" (subseq b end-index))
	 (return (if force-p (length b) (- (length b) threshold))))
       (setf start-index (- iword threshold)
	     next-iword iword)
       (setf interesting-indices (list next-iword))
       (loop
	(setf end-index (+ next-iword threshold)
	      next-iword (find-interesting-word (1+ next-iword)))
	(when (or (not next-iword)
		  (> next-iword (+ threshold end-index))) ; sure?
	  (return))
       (push next-iword interesting-indices))
       ;(format t "~&next-iword ~A start-index ~A~%" next-iword start-index)
       (unless force-p
	 (when (and next-iword
		    (> next-iword (- (length b) threshold)))
	   ;; there is another word, but it's too near the end to decide
	   ;; whether to print it in the current batch, so wait for more data
	   (return start-index)))
       (format out " ... ") 
       (elided-stream-highlight-words stream start-index end-index
				      (nreverse interesting-indices))
       ))))


#|
(with-open-file (i "/var/www/cliki/cirCLe")
  (let ((o (make-instance 'elided-stream :output-stream *standard-output*)))
    (do ((c (read-char i nil nil)  (read-char i nil nil)))
	((null c) nil)
     (write-char (if (graphic-char-p c) c #\Space) o))
    (close o)))
|#
