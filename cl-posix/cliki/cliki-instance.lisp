(in-package :cliki)

(defun canonise-title (title)
  "Return the key for the pages hash for the document with title TITLE"
  (if (zerop (length title))
      nil
      (nstring-downcase (substitute #\Space #\_ title))))

(defmethod find-page ((cliki cliki-view) title)
  (gethash (or (canonise-title title)
	       (string-downcase (cliki-default-page-name cliki)))
	   (cliki-pages cliki)))

(defmethod find-page-or-placeholder ((cliki cliki-view) titles)
  (or (some (lambda (x) (find-page cliki x)) titles )
      (let ((p
	     (make-instance 'cliki-page :title (car titles)
			    :pathname nil ; no pathname
			    :names titles
			    :versions nil
			    :cliki cliki)))
	(dolist (title titles)
	  (setf (gethash (canonise-title title) (cliki-pages cliki)) p))
	p)))

(defun name-for-url (url)
  (let* ((path (url-path url))
	 (slash (position #\/ path :from-end t))
	 (dothtml (search ".html" path :from-end t)))
    (urlstring-unescape (subseq path (if slash (1+ slash) 0) dothtml))))

#||
* (name-for-url (parse-urlstring "http://www.foo.com/blah/bAng%20banG.html"))
"bAng banG"
* (name-for-url (parse-urlstring "http://www.foo.com/blah/bAng%20banGhtml"))
"bAng banGhtml"
* (name-for-url (parse-urlstring "http://www.foo.com/blah/bAn.g%20banGhtml"))
"bAn.g banGhtml"
* (name-for-url (parse-urlstring "http://www.foo.com/blah/bAn.g%20banG.html"))
"bAn.g banG"
||# 

(defmethod cliki-idf ((cliki cliki-instance) term)
  (gethash term (slot-value cliki 'idf)))

;;; XXX this doesnt give an entirely correct answer because it counts
;;; aliases twice
(defmethod cliki-number-of-documents ((cliki cliki-view))
  (hash-table-count (cliki-pages cliki)))

(defun update-idf (cliki)
  "Update idf, using page-tf for each page and summing stuff.  page-tf
is set by update-page-indices (at startup and after edits).  "
  (let ((idf (make-hash-table :test 'equal)))
    (loop for document being the hash-values of (cliki-pages cliki)
	  do
	  (let* ((frequencies (page-index document :tf))
		 (terms (mapcar #'car frequencies)))
	    (dolist (term terms)
	      (let ((x (gethash term idf)))
		(setf (gethash term idf) (1+ (or x 0)))))))
    (setf (slot-value cliki 'idf) idf)))
  

(defmethod cliki-load-page ((cliki cliki-instance) pathname)
  (let* ((titles-f (merge-pathnames (make-pathname :type "titles") pathname))
	 (titles (with-open-file (i titles-f :direction :input) (read i)))
	 (meta (merge-pathnames (make-pathname :type "index") pathname))
	 (versions
	  (sort 
	   (remove-if #'zerop
		      (mapcar 
		       (lambda (x) 
			 (or (parse-integer (or (pathname-type x) "")
					    :junk-allowed t) 0))
		       (directory
			(make-pathname :type :wild :defaults pathname))))
	   #'>))
	 (page (find-page-or-placeholder cliki titles)))
    (setf (page-versions page) (or versions (list 0))
	  (page-pathname page) pathname)
    (cond ((probe-file meta)
	   ; ... do stuff to reload the index data
	   )
	  (t (update-page-indices cliki page)))
    page))

(defmethod shared-initialize
    :after ((cliki cliki-instance) slot-names &rest initargs)
    (declare (ignorable initargs))
  (setf (cliki-data-directory cliki) (pathname (cliki-data-directory cliki)))
  (let ((files (remove-if-not
		#'pathname-name
		(directory
		 (merge-pathnames "*.titles"
				  (cliki-data-directory cliki))))))
    (let ((*default-pathname-defaults* (cliki-data-directory cliki)))
      (dolist (f files) 
	(cliki-load-page
	 cliki (merge-pathnames (make-pathname :name (pathname-name f))))
	(format t "Loaded page ~A~%" f)))
    (update-idf cliki)
    (restore-recent-changes cliki)))

(defmethod shared-initialize
    :after ((cliki cliki-view) slot-names &rest initargs)
  (declare (ignorable initargs))
  (let ((edit-handler (make-instance 'edit-handler :cliki cliki)))
    (install-handler cliki edit-handler "edit/" nil))
  
  (install-handler cliki 'cliki-list-all-pages-handler "admin/all-pages" t)
  (install-handler cliki
		   (lambda (request)
		     (request-send-headers request
					   :content-type "text/css")
		     (cliki-css-text cliki (request-stream request))
		     t)
		   "admin/cliki.css" t)

  (install-handler cliki
		   (lambda (request)
		     (request-send-error request 404 "not found"))
		   "favicon.ico" nil)
  (install-handler cliki 'cliki-search-handler "admin/search" nil)
  (install-handler cliki `(view-recent-changes) "Recent%20Changes" nil)
  (install-handler cliki `(rdf-recent-changes) "recent-changes.rdf" t)
  (install-handler cliki `(sexp-recent-changes) "recent-changes.sexp" t)
  (install-handler cliki `(view-recent-changes) "Recent+Changes" nil))
  

;; XXX is this reasonable?  I don't think so, really
(defmethod handle-request-authentication ((handler cliki-view)
					  method request)
  (setf (request-user request) (request-cookie request "auth-username")))

(defmethod request-cliki ((request request))
  (labels ((find-handler (handlers)
	     (cond ((null handlers) nil)
		   ((typep (caar handlers) 'cliki-view)
		    (caar handlers))
		   (t (find-handler (cdr handlers))))))
    (find-handler (request-handled-by request))))
		    
(defmethod handle-request :around ((handler cliki-view) request)
  (handler-case 
      (call-next-method)
    (http-error (c)
      (request-send-headers request
			    :response-code (http-error-code c)
			    :response-text (http-error-message c) )
      (with-page-surround (handler request (symbol-name (type-of c)))
	(format cliki::out "~A" (http-error-client-message c)))
      (signal 'response-sent))))
			    
(defmethod handle-request-response ((handler cliki-view)
				    (method (eql :head))
				    request )
  (or
   (call-next-method)
   (multiple-value-bind (page title) (find-page-or-redirect handler request)
     (cond
       (page
	(request-send-headers request :last-modified 
			      (page-last-modified page))
	t)
       (t nil)))))
   
(defmethod handle-request-response ((handler cliki-view)
				    (method (eql :get))
				    request )
  (or
   (call-next-method)
   (multiple-value-bind (page title) (find-page-or-redirect handler request)
     (let* ((u (request-url request))
	    (version (integer-for (car (url-query-param u "v"))
				  :default :newest)))
       (cond
	 ((url-query-param u "source")
	  (view-page-source request page title :version version))
	 ((url-query-param u "download")
	  (let ((d (and page (page-index page :package))))
	    (when d
	      (request-redirect
	       request
	       (merge-url (parse-urlstring "http://ww.telent.net/cclan/")
			  (caar d)))
	      t)))
	 (t
	  (view-page handler request page title :version version)))))))


