(in-package :cliki)

(defun version-open-p (cliki page version user)
  (and (eql version (Car (page-versions page)))
       (> (file-write-date (page-pathname page :version version))
	  (- (get-universal-time) (* 5 60)))
       (destructuring-bind (date title user- descr)
	   (find-recent-change  cliki (page-title page))
	 (declare (ignore date title descr))
	 (string= user user-))))

(defmethod check-page-save-allowed ((cliki cliki-instance) page version user)
  (unless
      (or (not (page-versions page))
	  (version-open-p cliki page version user)
	  (= version (1+ (car (page-versions page)))))
    (signal 'cliki-page-save-rejected
	    :client-message
	    "Simultaneous edit: a newer version of this page already exists")))

(defun can-save-as-version-p (cliki page version user)
  (handler-case 
      (progn (check-page-save-allowed cliki page version user) t)
    (cliki-page-save-rejected () nil)))



