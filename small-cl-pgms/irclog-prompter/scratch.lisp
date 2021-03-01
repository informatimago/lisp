(install-prompt-functions)


(progn
  (let ((resource (gethash "lisp" *cached-resources*)))
   (let ((url   (cached-resource-url resource))
         (start (cached-resource-previous-length resource)))
     (multiple-value-bind (contents status headers uri stream do-close reason)
         (drakma:http-request url
                              :external-format-in :latin-1
                              :keep-alive t :close nil
                              :range '(0 0))
       (declare (ignorable uri))
       (let ((end (ignore-errors
                   (let ((content-range (cdr (assoc :content-range headers))))
                     (parse-integer content-range :start (1+ (position #\/ content-range)))))))
         (if (and (= 206 status) end)
             (progn
               (multiple-value-setq (contents status headers uri stream do-close reason)
                 (drakma:http-request url
                                      :external-format-in :latin-1
                                      :range (list start end)))
               (unwind-protect
                    (if (= status 200)
                        (setf (cached-resource-previous-length resource) end
                              (cached-resource-headers  resource) headers
                              (cached-resource-contents resource) contents)
                        (error "Could not fetch the resource ~S for ~D ~A~%"
                               (cached-resource-url resource) status reason))
                 (when do-close (close stream))))
            
             (error "Could not fetch length of resource ~S for ~D ~A~%"
                    (cached-resource-url resource) status reason))))))

  (cached-resource-contents (gethash "lisp" *cached-resources*))

(initialize-cached-resources)
(get-new-messages))


(initialize-cached-resources)
(install-prompt-functions)
(cached-resource-contents (gethash "lisp" *cached-resources*))


(get-new-messages)
