(in-package :cliki)
(defmethod cliki-default-page-name ((cliki cliki-view)) nil "index")
(defmethod find-page-or-redirect ((cliki cliki-view)
				  (request request))
  (let* ((search-string (name-for-url (request-url request)))
	 (query (url-query (request-url request)))
         (actual (find-page cliki search-string)))
    (cond
      ((not actual)			; no page found
       (values nil search-string))
      ((string= search-string (page-title actual)) ;same name
       (values actual (page-title actual)))
      (t				;other name, redirect the browser
       (request-redirect request
			 (merge-url
			  (page-url cliki actual)
			  (if query (format nil "?~A" query) "")))
       (signal 'response-sent)))))

;; user is some kind of object which represents a user.  If your cliki-instance
;; subclass makes it anything other than a string, it needs to provide 
;; methods for these
(defgeneric cliki-user-name (cliki user))
(defmethod cliki-user-name ((cliki cliki-view) user)
  user)

(defgeneric cliki-user-cookie (cliki user))
(defmethod cliki-user-cookie ((cliki cliki-view) user)
  (format nil "username=~A; path=~A; expires=~A; domain=~A"
	  (urlstring-escape (cliki-user-name cliki user))
	  (url-path (cliki-url-root cliki))
	  "Sun, 01-Jun-2036 00:00:01 GMT"
	  (url-host (cliki-url-root cliki))))

(defmethod cliki-user-cookie ((cliki cliki-view) (user (eql nil)))
  (format nil "username=; path=~A; expires=~A; domain=~A"
	  (url-path (cliki-url-root cliki))
	  "Mon, 32-Jul-2001 00:00:01 GMT"
	  (url-host (cliki-url-root cliki))))
