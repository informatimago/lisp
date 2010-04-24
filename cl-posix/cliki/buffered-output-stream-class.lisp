(in-package :cliki)

;;; buffered-output-stream - see also elided-stream, a demonstration of its
;;; purpose


(defclass buffered-output-stream (fundamental-character-output-stream)
  ((buffer :initarg buffer
	   :initform (make-array 4096 :element-type 'character
				 :adjustable t
				 :fill-pointer 0)
	   :reader buffered-output-stream-buffer)
   ;; internal
   (write-threshold :initform 1024 :initarg :write-threshold
		    :accessor buffered-output-stream-write-threshold)
   (temporary-write-threshold
    :initform 1024
    :accessor buffered-output-stream-temporary-write-threshold)
   (extend-by :initarg :extend-by :initform 512 :accessor buffered-output-stream-extend-by)

   (line-column :initform 0 :accessor stream-line-column)))
