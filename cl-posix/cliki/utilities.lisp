(in-package :cliki)

(defmacro aif (test then else)
  `(let ((it ,test)) (if it ,then ,else)))

(defun integer-for (thing &key (default 0) (start 0) end)
  (or (parse-integer (or thing "") :start start :end end :junk-allowed t) 
      default))

(defun string-prefix-p (short long)
  (let ((m (mismatch short long)))
    (or (not m) (= m (length short)))))

(defmacro with-page-surround ((cliki request title &optional head) &body forms)
  `(cliki-page-surround ,cliki ,request
			(lambda (out) ,@forms)
			:title ,title :head ,head))
