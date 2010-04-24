(in-package :cliki)

;;; buffered-output-stream - see also elided-stream, a demonstration of its
;;; purpose


;;; FIXME other ways to suck -

;;; 0) doesn't support quite a lot of the gray streams protocol
;;; 1) if the search string occurs a lot, so that it can't find
;;; suitable elision points, buffer writes will fail and the buffer
;;; may grow quite large (potentially up to the total length of all the
;;; input)


(defgeneric buffered-output-stream-write-buffer (stream &optional force-p))

;;; internal
(defmethod buffered-output-stream-write-some ((stream buffered-output-stream)
					      &optional force-p)
  (let ((chars-written (buffered-output-stream-write-buffer stream force-p)))
    (if (> chars-written 0)
	(let ((b (buffered-output-stream-buffer stream )))
	  (if (> (length b) chars-written)
	      (replace b b :start1 0 :start2 chars-written))
	  (setf (fill-pointer b) (- (length b) chars-written)
		(buffered-output-stream-temporary-write-threshold stream)
		 (buffered-output-stream-write-threshold stream)))
	(incf (buffered-output-stream-temporary-write-threshold stream)
	      (buffered-output-stream-write-threshold stream)))))

(defmethod stream-start-line-p ((stream buffered-output-stream))
  (eql (stream-line-column stream) 0))

(defmethod print-object ((object buffered-output-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A characters buffered"
	    (length (buffered-output-stream-buffer object)))))

(defmethod stream-write-string ((stream buffered-output-stream) string
				&optional (start 0) (end (length string)))
  (unless end (setf end (length string)))
  (let ((buffer (buffered-output-stream-buffer stream))
	(extend-by (max (- end start)
			(buffered-output-stream-extend-by stream))))
    (loop for i from start below end
	  do (vector-push-extend (elt string i) buffer extend-by))
    (when (> (length buffer) (buffered-output-stream-write-threshold stream))
      (buffered-output-stream-write-some stream)))
  string)

(defmethod stream-write-char ((stream buffered-output-stream) character)
  (let ((buffer (buffered-output-stream-buffer stream)))
    (vector-push-extend character buffer
			(buffered-output-stream-extend-by stream))
    (when (> (length buffer)
	     (buffered-output-stream-temporary-write-threshold stream))
      (buffered-output-stream-write-some stream)))
  character)

;;; subclasses should probably avoid specialising these two methods
(defmethod stream-finish-output ((stream buffered-output-stream))
  (buffered-output-stream-write-some stream t))
(defmethod stream-force-output ((stream buffered-output-stream))
  (buffered-output-stream-write-some stream t))

(defmethod stream-clear-output :after ((stream buffered-output-stream))
  (let ((b (buffered-output-stream-buffer stream)))
    (setf (fill-pointer b) 0)))

(defmethod close :after ((stream buffered-output-stream) &key abort)
  (if abort
      (stream-clear-output stream)
      (stream-force-output stream)))

