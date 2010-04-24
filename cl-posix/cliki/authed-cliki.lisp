(in-package :cliki)

;; user auth, originally developed for entomotomy

(defstruct user name password)

(defmethod find-user ((cliki authed-cliki-handler) name)
  (find name (cliki-users cliki)
	:key #'user-name
	:test #'string=))

(defmethod cliki-user-name ((cliki authed-cliki-handler) user)
  (user-name user))

(defmethod cliki-user-cookie ((cliki authed-cliki-handler) user)
  (format nil "auth-username=~A:~A; path=~A; expires=~A; domain=~A"
	  (urlstring-escape (user-name user))
	  (urlstring-escape (user-password user))
	  (url-path (cliki-url-root cliki))
	  "Sun, 01-Jun-2036 00:00:01 GMT"
	  (url-host (cliki-url-root cliki))))

(defun login-handler (request)
  (let* ((cliki (request-cliki request))
	 (url (request-url request))
	 (body (request-body request))
	 (name (body-param "NAME" body))
	 (from (body-param "FROM" body))
	 (password (body-param "PASSWORD" body))
	 (user (find-user cliki name)))
    (cond ((and user
		(string= password (user-password user)))
	   (request-send-headers request :set-cookie (cliki-user-cookie cliki user))
	   (request-redirect 
	    request (parse-urlstring from)
	    :set-cookie (cliki-user-cookie cliki user)))
	  (t (request-send-error request 401)))
    (signal 'response-sent)))

(defmethod shared-initialize :after ((handler authed-cliki-handler)
				     slot-names &rest initargs)
  (change-class (find-handler handler "edit/" nil)
		'authed-cliki-edit-handler)
  (with-open-file (i (merge-pathnames
		      (make-pathname :name "users" :type "dat"
				     :directory '(:relative "admin"))
		      (cliki:cliki-data-directory handler)))
    (let ((*package* #.*package*))
      (setf (cliki-users handler) (read i))))
  (install-handler handler 'login-handler "admin/login" nil))

(defmethod handle-request-authentication ((handler authed-cliki-edit-handler)
					 method request)
  (let* ((u (request-cookie request "auth-username"))
	 (colon (position #\: u))
	 (username (urlstring-unescape (subseq u 0 colon)))
	 (password (and colon (urlstring-unescape (subseq u (1+ colon)))))
	 (cliki (cliki:request-cliki request))
	 (user (find-user cliki username)))
    (unless (and username password user
		 (string= password (user-password user)))
      (request-send-headers request)
      (princ
       (html
	`(html (head (title "Login"))
	  (body (h1 "Authentication required")
	   (p "We need you to provide a username and password before you can edit this page")
	   ,@(if password `((p "Incorrect password provided")))
	   ((form :method post :action
		  ,(format nil "~Aadmin/login"
			   (urlstring
			    (cliki:cliki-url-root cliki))))
	    (p "Name " ((input :name :name :type :text :size 30)))
	    (p "Password"
	       ((input :name :password :type :text :size 30)))
	    (p ((input :name :from :type :hidden :value
		       ,(urlstring (request-url request))))
	       ((input :name login :value "Login" :type submit)))))))
       (request-stream request))
      (signal 'response-sent))
    (setf (request-user request) user)))