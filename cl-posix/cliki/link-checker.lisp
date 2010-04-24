(in-package :cliki)

;;; CLiki external link checker

#+cmu
(progn
  (alien:def-alien-routine alarm c-call:void (seconds alien:integer :in))
  (defmacro with-enabled-interrupts (&rest args)
    `(system:with-enabled-interrupts ,@args)))

#+sbcl
(progn
  (sb-alien:def-alien-routine alarm sb-c-call:void
			      (seconds sb-alien:integer :in))
  (defmacro with-enabled-interrupts (&rest args)
    `(sb-sys:with-enabled-interrupts ,@args)))


;;; It's network code, so there's a billion ways it could go wrong.
;;; For the record:

;;; - If the url scheme is not http, or the port is non-standard => still there
;;; - can't parse the URL => dead
;;; - dns `permanent' error => dead
;;; - dns `retry later' error => transient fault
;;; - 4xx or 5xx => dead
;;; - 2xx or 3xx => still there (we don't follow redirects, cos we're lazy)
;;; - network timeout => transient fault

;;; - transient fault lasting longer than a week => dead

;;; we return (values (or t :transient nil) error-string)

(defun get-http-response (urlstring)
  (unless (and (> (length urlstring) 5) (string-equal urlstring "http:" :end1 5))
    (return-from get-http-response t))
  (handler-case 
   (let* ((url (araneida:parse-urlstring urlstring nil))
          (host (and url (url-host url)))
          (port (and url (url-port url))))
     (unless url (return-from get-http-response (values nil "Unparseable URL")))
     (unless (and (string-equal (url-scheme url) "HTTP")
                  (member port '(80 81 8080 8000 8001)))
       (return-from get-http-response t))
     (let ((s (make-inet-socket :stream :tcp)))
       (with-enabled-interrupts
        ((14 (lambda (a b c) (sockets::socket-error "alarm timed out"))))
        (unwind-protect
            (progn
              (alarm 10)
              (socket-connect
               s (car (host-ent-addresses (get-host-by-name host))) port)
              (let ((stream (socket-make-stream s :input t :output t :buffering :full)))
                (format stream "HEAD ~A HTTP/1.0~%Host: ~A:~A~%~%"
                        ;; XXX this is actually broken for URLs with
                        ;; query strings
                        (url-path url) host port)
                (force-output stream)
                (format t "HEAD ~A HTTP/1.0~%Host: ~A:~A~%~%"
                        ;; XXX this is actually broken for URLs with
                        ;; query strings
                        (url-path url) host port)
                (destructuring-bind (protocol code &optional text)
                    (araneida::split (read-line stream) 3)
                  (declare (ignore protocol))
                  (let ((number (parse-integer code :junk-allowed t)))
                    (if (<= 200 number 399) t
                      (values nil (format nil "~A ~A" number (or text ""))))))))
          (socket-close s)
          (alarm 0))))
     ;; don't process if non-standard port, just return "success" 
     t)
   (sockets::socket-error (c) (values :transient (symbol-name (type-of c))))
   ;; name service errors which are likely to be (a) transient or (b) our fault
   (sockets::netdb-internal-error (c) (values :transient (symbol-name (type-of c))))
   (sockets::try-again-error (c) (values :transient (symbol-name (type-of c))))
   ;; other kinds of error
   (sockets::host-not-found-error (c) (values nil (symbol-name (type-of c))))
   (sockets::name-service-error (c) (values nil (symbol-name (type-of c))))))


;;; note that we manage to lose any "exciting" attributes on A that we
;;; notice are dead.  People shouldn't be using them anyway, so we
;;; can reasonably claim that's policy
(defun check-link (a)
  (destructuring-bind ((name &key href class &allow-other-keys) &body body) a
    (declare (ignore name))
    (unless (string-equal class "DEAD")
      (multiple-value-bind (success reason) (get-http-response href)
        (unless success
          (values
           `((:a :href ,href :class "DEAD") ,@body)
           reason))))))

(defun traverse-tree (tree test)
  (when tree
    (when (consp tree)
      (traverse-tree (car tree) test)
      (traverse-tree (cdr tree) test))
    (funcall test tree))
  tree)

(defun annotate-links-on-page (title root)
  (let* ((pathname (merge-pathnames title root))
         (document
          (with-open-file (in pathname :direction :input)
            (net.html.parser:parse-html in))))
    (traverse-tree document
                   (lambda (el)
                     (when (consp el)
                       (let ((a (car el)))
                         (when (and (consp a) (consp (car a)) (eq (caar a) :A))
                           (multiple-value-bind (bad why) (check-link a)
                             (when bad
                               (setf (car el) bad)
                               (setf (cdr el)
                                     (cons
                                      `(:i "(*(dead link) ["  ,why "] "
                                           ,(araneida::universal-time-to-rfc-date
                                             (get-universal-time))
                                           ")")
                                      (cdr el))))))))))
    (with-open-file (out pathname :direction :output)
      (dolist (tree document)
        (araneida::html-stream out tree)))))
