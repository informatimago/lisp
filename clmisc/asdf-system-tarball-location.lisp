;;;; -*- coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               asdf-system-tarball-location.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     Common Lisp REPL
;;;;DESCRIPTION
;;;;
;;;;
;;;;    This is mostly rendered obsolete by libcl and quicklisp.
;;;;
;;;;    
;;;;    This file extends the ASDF-SYSTEM with a cache for tarball locations.
;;;;
;;;;    We fetch from cliki.net the url of all the asdf installable tarballs,
;;;;    and store it in a local cache.  When we asdf-install:install a system
;;;;    the function DOWNLOAD-FILES-FOR-PACKAGE will first check the local
;;;;    cache, and if a location is found here, it uses it instead of going
;;;;    to the cliki.
;;;;
;;;;    The user can also override or add his own systems and tarballs
;;;;    associations in the cache.
;;;;
;;;;    Usage: (asc- = Asdf System Cache)
;;;;
;;;;        (asc-update-cliki-tarball-locations :verbose t)
;;;;        ; Updates the local cache with the tarball locations on cliki.
;;;;        ; This doesn't modify the personnal associations.
;;;;
;;;;        (asc-system-list)
;;;;        ; gives a list of all known asdf-installable system names.
;;;;
;;;;        (download-all-tarballs :verbose t)
;;;;        ; Download all the tarballs and signatures of the asdf systems
;;;;        ; listed by asc-system-list
;;;;
;;;;        (lsasc)
;;;;        ; Prints the same list, with the URL of the tarballs
;;;;        ; (and the last response code obtained when finding the URL
;;;;        ; when it's not 200),
;;;;        ; and whether the asdf system is installed and/or loaded.
;;;;
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-10-14 <PJB> Created.
;;;;BUGS
;;;;
;;;;    There's a dependency on the COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-PARSER.PARSE-HTML
;;;;    package from the COM.INFORMATIMAGO.COMMON-LISP asdf system which is
;;;;    not asdf-installable (yet).
;;;;    http://darcs.informatimago.com/darcs/public/lisp/
;;;;
;;;;    We should keep the last-modified date from the header.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2012
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;***************************************************************************

(unless (find-package :asdf-install)
  (asdf:oos 'asdf:load-op :asdf-install))
(unless (find-package :com.informatimago.common-lisp.html-generator.html-parser.parse-html)
  (asdf:oos 'asdf:load-op :com.informatimago.common-lisp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASDF

(in-package :asdf)
;; (cl:unless (cl:find-symbol "EXPORT") (cl:use-package :cl))
(export '(system-version
          ;; low level but usefull stuff:
          parse-name-version))


(defun parse-name-version (name)
  "
DO:      Parse the NAME with the regexp: \".*[-_]\([0-9]+\(\.[0-9]+\)+\)$\"
RETURN:  a list containing the version numbers;
         a prefix substring containing the name up to the version separator;
         a suffix substring containing the version.
         When a version cannot be scanned, the first two results are NIL,
         and the last is the suffix string down to the scanning error.
"
  (macrolet ((collect ()
               `(handler-case           ; check that at least on digit
                                        ; is present between dots.
                    (progn
                      (push (parse-integer name :start (1+ p) :end e
                                           :junk-allowed nil) n)
                      (setf e p))
                  (parse-error (err)
                    (return-from parse-name-version
                      (values nil nil (subseq name p)))))))
    (loop
       :with n = '()
       :with e =  (length name)
       :for p :from (1- e) :downto 0
       :for s = p 
       :for ch = (aref name p)
       :while (or (digit-char-p ch) (char= #\. ch))
       :do (when (char= #\. ch) (collect)) 
       :finally (return (if (or (char= #\_ ch) (char= #\- ch))
                            (progn (collect)
                                   (values n (subseq name 0 s) (subseq name s)))
                            (values nil nil (subseq name s)))))))

(defun test-parse-version ()
  (dolist (test '(("abc-1.2.3"   (1 2 3) "abc" "-1.2.3")
                  ("abc-123"     (123)   "abc" "-123")
                  ("-123"        (123)   ""    "-123")
                  ("abc_1.2.3"   (1 2 3) "abc" "_1.2.3")
                  ("abc_123"     (123)   "abc" "_123")
                  ("_123"        (123)   ""    "_123")
                  ("abc_1.2.3p3" nil     nil   "p3")
                  ("abc-1..3"    nil     nil   "..3")
                  ("1.2.3"       nil     nil   "1.2.3")
                  ("123"         nil     nil   "123")
                  ))
    (assert (equalp (multiple-value-list (parse-name-version (first test)))
                    (rest test)))))

(defun lastcar (x) (car (last x)))

(defun system-version (system-designator)
  "
RETURN:  A version for the system designated by SYSTEM-DESIGNATOR.
         If the system doesn't have a version slot bound, we look
         at the pathname and extract the version from the directory name.
         If it has both and they don't match, a warning is issued.
"
  (let* ((system  (find-system system-designator))
         (version-in-dir
          (multiple-value-list
           (parse-name-version
            (lastcar (pathname-directory
                      (truename (system-definition-pathname system)))))))
         (version-in-asd
          (and (slot-boundp  system 'version)
               (multiple-value-list
                (parse-name-version
                 (concatenate 'string "-"
                              (slot-value system 'version)))))))
    ;;     d   a   d=a
    ;;     nil nil t    no version
    ;;     v1  nil nil  v1
    ;;     nil v1  nil  v1
    ;;     v1  v1  t    v1
    ;;     v1  v2  nil  v1 warning
    (cond ((and (null (first version-in-dir))  (null (first version-in-asd)))
           nil)
          ((and (first version-in-dir) (first version-in-asd))
           (unless (equal (first version-in-dir) (first version-in-asd))
             (warn "Version of ASD system ~A differs in ASD file (~A) from ~
                    directory name (~A)"
                   system-designator
                   (subseq (third version-in-asd) 1)
                   (subseq (third version-in-dir) 1)))
           (subseq (third version-in-dir) 1))
          ((first version-in-dir) (subseq (third version-in-dir) 1))
          (t                      (subseq (third version-in-asd) 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASDF-INSTALL
;;;

(in-package :asdf-install)
(use-package :com.informatimago.common-lisp.html-generator.html-parser.parse-html)
(export '(*asdf-system-list-url* *asdf-system-list-id*
          *asdf-system-cache-file* *asdf-system-cache-directory*
          *connect-timeout* *read-header-timeout*
          lsasc asc-system-list asc-system-tarball-location
          asc-update-cliki-tarball-locations
          download-tarball download-all-tarballs))

;;; User configurable parameters:

(defvar *asdf-system-cache-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative "ASDF-SYSTEM-TARBALLS")
                  :name nil :type nil :version nil
                  :case :common :defaults (user-homedir-pathname))
   (user-homedir-pathname) nil)
  "Pathname to the directory where the tarballs are stored.")

(defvar *asdf-system-cache-file*
  (merge-pathnames
   (make-pathname :name "CACHE" :type "DATA" :version nil
                  :case :common :defaults *asdf-system-cache-directory*)
   *asdf-system-cache-directory* nil)
  "Pathname to the cache file.")


(defvar *connect-timeout*     30 "Timeout to connect to a HTTP server.")
(defvar *read-header-timeout* 60 "Timeout to read a HTTP header line.")


(defvar *asdf-system-list-url* "http://www.cliki.net/ASDF-Install"
  "The URL of the cliki page where the list of asdf-installable
    systems is given.")

(defvar *asdf-system-list-id*  "asdf-installable-packages"
  "The HTML ID identifying the asdf-installable systems.")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Patch to asdf-install/port.lisp
;; We add a timeout on connection (implemented for clisp).
;; It looks like sbcl doesn't need it (or allow it to be set).
(defun make-stream-from-url (url)
  #+:sbcl
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket
             :type :stream
             :protocol :tcp)))
    (sb-bsd-sockets:socket-connect
     s (car (sb-bsd-sockets:host-ent-addresses
             (sb-bsd-sockets:get-host-by-name (url-host url))))
     (url-port url))
    (sb-bsd-sockets:socket-make-stream 
     s
     :input t 
     :output t
     :buffering :full
     :external-format :iso-8859-1))
  #+:cmu
  (sys:make-fd-stream (ext:connect-to-inet-socket (url-host url) (url-port url))
                      :input t :output t :buffering :full)
  #+:lispworks
  (comm:open-tcp-stream (url-host url) (url-port url)
                        #+(and :lispworks :win32) :element-type
                        #+(and :lispworks :win32) '(unsigned-byte 8))
  #+:allegro
  (socket:make-socket :remote-host (url-host url)
                      :remote-port (url-port url))
  #+:clisp
  (socket:socket-connect
   (url-port url) (url-host url)
   :external-format (ext:make-encoding :charset 'charset:iso-8859-1
                                       :line-terminator :unix)
   :timeout *connect-timeout*)
  #+:openmcl
  (ccl:make-socket :remote-host (url-host url)
                   :remote-port (url-port url))
  #+:digitool
  (ccl::open-tcp-stream (url-host url) (url-port url)
                        :element-type 'unsigned-byte))


;; Patch to asdf-install/installer.lisp
;; We add a timeout on reading the header lines.
;; Not implemented for digitool, but possibly it could use
;; the common implementation.
(defun url-connection (url)
  (let ((stream))
    (if (= (mismatch url "file://") 7)
        (setf stream (open (subseq url 7)
                           :if-does-not-exist nil
                           :external-format
                           #+clisp charset:iso-8859-1
                           #+sbcl :iso-8859-1
                           #- (or clisp sbcl) :default))
        (let ((host (url-host url)))
          (setf stream (make-stream-from-url (or *proxy* url)))
          (format stream
            "GET ~A HTTP/1.0~C~CHost: ~A~C~CCookie: CCLAN-SITE=~A~C~C"
            (request-uri url) #\return #\linefeed
            host #\return #\linefeed
            *cclan-mirror* #\return #\linefeed)
          (when (and *proxy-passwd* *proxy-user*)
            (format stream "Proxy-Authorization: Basic ~A~C~C"
                    (base64-encode
                     (format nil "~A:~A" *proxy-user* *proxy-passwd*))
                    #\return #\linefeed))
          (format stream "~C~C" #\return #\linefeed)
          (force-output stream)))
    (unless stream (error 'download-error :url url :response 404))
    (flet (#-:digitool
           (read-header-line ()
             (let ((line (make-array 16
                                     :element-type 'character
                                     :adjustable t
                                     :fill-pointer 0)))
               (loop
                  :with start-time = (get-universal-time)
                  #-clisp :with #-clisp state #-clisp = #-clisp 0
                  :for ch = (read-char-no-hang stream)
                  :while (< (- (get-universal-time) start-time)
                            *read-header-timeout*)
                  :do
                  #-clisp
                  (ecase state
                    ((0)                ; normal character
                     (cond
                       ((null ch)                         (sleep 0.01))
                       ((= #|ASCII_CR|#13 (char-code ch)) (setf state 1))
                       (t                 (vector-push-extend ch line))))
                    ((1)                ; got CR, expecting LF
                     (cond
                       ((null ch)                         (sleep 0.01))
                       ((= #|ASCII_LF|#10 (char-code ch))
                        (return-from read-header-line line))
                       (t    ; anything else: we push the CR and go on
                        (vector-push-extend (code-char 13) line)
                        (setf state (if (= 13 (char-code ch)) 1 0))))))
                  #+clisp               ; has no CR+LF! :-(
                  (cond
                    ((null ch)
                     (sleep 0.01))
                    ((char= #\newline ch)
                     (return-from read-header-line line))
                    (t
                     (vector-push-extend ch line)))
                  :finally (error "TIMEOUT while reading header line ~S"
                                  line))))
           #+:digitool
           (read-header-line (&aux (line (make-array 16
                                                     :element-type 'character
                                                     :adjustable t
                                                     :fill-pointer 0))
                                   (byte nil))
             (multiple-value-bind (reader arg)
                 (ccl::stream-reader stream)
               (loop (setf byte (funcall reader arg))
                  (case byte
                    ((nil)
                     (return))
                    ((#.(char-code #\return)
                        #.(char-code #\linefeed))
                     (case (setf byte (funcall reader arg))
                       ((nil #.(char-code #\return)
                             #.(char-code #\linefeed)))
                       (t (ccl:stream-untyi stream byte)))
                     (return))
                    (t
                     (vector-push-extend (code-char byte) line))))
               (when (or byte (plusp (length line)))
                 line))))
      (list
       (let* ((l (read-header-line))
              (space (position #\space l)))
         (parse-integer l :start (1+ space) :junk-allowed t))
       (loop for line = (read-header-line)
          until (or (null line)
                    (zerop (length line))
                    (eql (elt line 0) (code-char 13)))
          collect
          (let ((colon (position #\: line)))
            (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
                  (string-trim (list #\space (code-char 13))
                               (subseq line (1+ colon))))))
       stream))))


;; Patch to download-files-for-package
;; We use the url returned by FIND-SYSTEM-TARBALL-LOCATION.
;; We process also file:// scheme in find-system-tarball-location.
(defun download-files-for-package (package-name-or-url file-name)
  (let ((url (copy-resource-at-url package-name-or-url file-name :verbose t)))
    (terpri)
    (restart-case 
        (verify-gpg-signature/url url file-name)
      (skip-gpg-check (&rest rest)
        :report "Don't check GPG signature for this package"
        (declare (ignore rest))
        nil))))


(defun copy-resource-at-url (package-name-or-url file-name &key (verbose nil))
  (multiple-value-bind (response headers stream url)
      (find-system-tarball-location package-name-or-url)
    (when (>= response 400)
      (error 'download-error :url url :response response))
    (let ((length (parse-integer (or (cdr (assoc :content-length headers)) "")
                                 :junk-allowed t)))
      (when verbose
        (format t "Downloading ~:[some unknown number of ~;~:*~A~] ~
                   byte~:*~P from ~A to ~A ..." length url file-name)
        (force-output))
      #+:clisp (setf (stream-element-type stream) '(unsigned-byte 8))
      (with-open-file 
          #-(and allegro-version>= (not (version>= 8 0)))
          (o file-name :direction :output
             #+(or :clisp :digitool (and :lispworks :win32))
             :element-type
             #+(or :clisp :digitool (and :lispworks :win32))
             '(unsigned-byte 8)
             #+:sbcl :external-format #+:sbcl :latin1
             :if-exists :supersede)
          #+(and allegro-version>= (not (version>= 8 0)))
          (o file-name :direction :output :if-exists :supersede)
          #+(or :cmu :digitool)
          (copy-stream stream o)
          #-(or :cmu :digitool)
          (if length
              (let ((buf (make-array length
                                     :element-type
                                     (stream-element-type stream))))
                #-:clisp (read-sequence buf stream)
                #+:clisp (ext:read-byte-sequence buf stream :no-hang nil)
                (write-sequence buf o))
              (copy-stream stream o))))
    (close stream)
    (values url headers)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From: process-html.lisp

(defun map-tags (fun html)
  (cond ((atom html))
        ((keywordp (car html)) 
         (funcall fun html) 
         (map-tags fun (cddr html)))
        (t (map nil  (lambda (item) (map-tags fun item)) html))))


(defun tags-at-path (tag-path html)
  "
TAG-PATH:  A list of tag keywords or (:tag predicate).
HTML:      A tree of html nodes (:tag attributes html...)
RETURN:    The nodes found by walking HTML following TAG-PATH.
"
  (flet ((filter-out
          (tag html)
          (remove-if (if (atom tag)
                       (lambda (node) 
                         (or (atom node)
                             (not (eq tag (html-tag node)))))
                       (lambda (node)
                         (or (atom node)
                             (not (eq (first tag) (html-tag node)))
                             (not (funcall (second tag) node)))))  html)))
    (cond
     ((atom html)           nil)
     ((null tag-path)       html)
     ((null (cdr tag-path)) (filter-out (car tag-path) html))
     (t (tags-at-path (cdr tag-path) (mapcan (function html-contents)
                                         (filter-out (car tag-path) html)))))))

(defun collect-nodes (html predicate)
  "
RETURN: The list of subnodes* of html for which predicate is true.
"
  (let ((result '()))
    (map-tags
     (lambda (node) (when (funcall predicate node) (push node result))) html)
    (nreverse result)))


(defun collect-nodes-t (html tag)
  "
RETURN:  The list of subnodes* of HTML whose tag is TAG.
"
  (collect-nodes html (lambda (node) (eq tag (html-tag node)))))


(defparameter cntavp (make-hash-table :test (function equalp)))

(defun collect-nodes-tav (html tag attribute value)
  "
RETURN:  The list of subnodes* of HTML whose tag is TAG and that has an
         attribute ATTRIBUTE whose value is VALUE.
"
  (unless (gethash (list tag attribute value) cntavp)
    (setf (gethash (list tag attribute value) cntavp) t)
    #+ (or) (print (list 'collect-nodes-tav tag attribute value)))
  (collect-nodes html
                 (lambda (node)
                   (and (eq tag (html-tag node))
                        (html-attribute node attribute)
                        (string= (html-attribute node attribute) value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fetch-url (url &key (verbose nil))
  "
RETURN: A byte vector containing the contents of the resource 
        at the given HTTP URL.
"
  (destructuring-bind (response headers stream)
      (block got
        (loop
           (destructuring-bind (response headers stream)
               (asdf-install::url-connection url)
             (when verbose
               (let ((*print-pretty* nil))
                 (format t "~2%;;; URL = ~S~%;;; RESPONSE = ~S~%~
                               ;;; HEADERS = ~{~S~%~^;;;           ~}~%"
                   url response headers)
                 (force-output)))
             (unless (member response '(301 302))	       
               (return-from got (list response headers stream)))
             (close stream)
             (setf url (cdr (assoc :location headers))))))
    (when (>= response 400)
      (error 'download-error :url url :response response))
    (let ((length (parse-integer (or (cdr (assoc :content-length headers)) "")
                                 :junk-allowed t)))
      (when verbose
        (format t "~&;;; Downloading ~A bytes~%;;;        from ~A~%"
                (or length "some unknown number of") url)
        (force-output))
      #+:clisp (setf (stream-element-type stream) '(unsigned-byte 8))
      (let ((buffer 
             (with-open-stream (stream stream)
               (if length
                   (let ((buffer (make-array
                                  length
                                  :element-type (stream-element-type stream))))
                     #+:clisp (ext:read-byte-sequence buffer stream :no-hang nil)
                     #-:clisp (read-sequence buffer stream)
                     buffer)
                   (loop
                      :with start = 0
                      :with buffer = (make-array
                                      4096
                                      :adjustable t :fill-pointer 4096
                                      :element-type (stream-element-type stream))
                      :for size =
                      #+:clisp (ext:read-byte-sequence buffer stream
                                                       :start start
                                                       :end (length buffer)
                                                       :no-hang nil)
                      #-:clisp (read-sequence buffer stream
                                              :start start
                                              :end (length buffer))
                      :until (< size (length buffer))
                      :do (setf start size
                                buffer (adjust-array
                                        buffer (+ 4096 (length buffer))
                                        :fill-pointer (+ 4096 (length buffer))
                                        :element-type (array-element-type
                                                       buffer)))
                      :finally (setf (fill-pointer buffer) size)
                      (return buffer))))))
        (when verbose
          (format t "~&;;; Read ~A byte~:*~P.~%" (length buffer))
          (force-output))
        buffer))))


(defun fetch-asdf-system-list (&key (verbose nil))
  "
RETURN: A list of system names found on cliki.
"
  (mapcar (lambda (a-tag) (html-attribute a-tag :href))
          (collect-nodes-tav
           (collect-nodes-tav
            (html-attributes 
             (parse-html-string
              (let ((data (fetch-url *asdf-system-list-url* :verbose verbose)))
                (if (stringp data)
                    data
                    (ext:convert-string-from-bytes data charset:iso-8859-1)))))
            :div :id  *asdf-system-list-id*)
           :a :class  "internal")))


(defun url-of-asdf-system (package-name-or-url &key (verbose nil))
  "
DO:       Finds on cliki the url of the tarball of the package-name-or-url,
          following the HTTP indirections.
RETURN:   url ; headers ; stream
"
  (loop
     :named got
     :with url = (if (= (mismatch package-name-or-url "http://") 7)
                     package-name-or-url
                     (format nil "http://www.cliki.net/~A?download"
                             package-name-or-url))
     :do (destructuring-bind (response headers stream)
             (handler-case (asdf-install::url-connection url)
               (error () (list 408 '() nil)))
           (when verbose
             (let ((*print-pretty* nil))
               (format t "~2%;;; URL = ~S~%;;; RESPONSE = ~S~%~
                             ;;; HEADERS = ~{~S~%~^;;;           ~}~%"
                 url response headers)
               (force-output)))
           (when stream (close stream))
           (unless (member response '(301 302))	       
             (return-from got (values url response headers)))
           (setf url (cdr (assoc :location headers))))))



(defstruct (asdf-system-information
             (:constructor make-asi)
             (:copier copy-asi)
             (:conc-name asi-))
  name

  remote-response
  remote-url
  remote-last-modified

  cached-url     ; (file://)
  cached-last-modified

  version
  author
  maintainer
  description
  long-description)


(defvar *asdf-system-cache-empty* '()
  "An empty asdf system cache.")

(defvar *asdf-system-cache*     *asdf-system-cache-empty*
  "In-memory cache.
A list of asdf-system-information records.
These records may come from cliki or be added by the user.
")

(defvar *asdf-system-timestamp* (get-universal-time)
  "timestamp of the *asdf-system-cache*")
;;; The timestamp could be used to detect when the file changed under us
;;; to reload it only when needed.  It's not implemented yet.


(defun asc-load ()
  "
DO:     Load the *ASDF-SYSTEM-CACHE* from th *ASDF-SYSTEM-CACHE-FILE*.
"
  (with-open-file (input *asdf-system-cache-file*
                             :if-does-not-exist nil)
    (if input
        (setf *asdf-system-cache*     (read input nil *asdf-system-cache-empty*)
              *asdf-system-timestamp* (file-write-date input))
        (setf *asdf-system-cache*     *asdf-system-cache-empty*
              *asdf-system-timestamp* (get-universal-time)))))

(defun asc-save ()
  "
DO:     Write the *ASDF-SYSTEM-CACHE* into *ASDF-SYSTEM-CACHE-FILE*.
"
  (ensure-directories-exist  *asdf-system-cache-file*)
  (with-open-file (output *asdf-system-cache-file*
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
    (let ((*print-readably*)) (print *asdf-system-cache* output)))
  (setf *asdf-system-timestamp* (file-write-date *asdf-system-cache-file*)))


(defun asc-system-informations (system-name)
  "
RETURN:    The entry in the cache for the asdf system named SYSTEM-NAME.
"
  (unless *asdf-system-cache*
    (asc-load))
  (find system-name *asdf-system-cache*
        :key (function asi-name) :test (function string-equal)))


(defun (setf asc-system-informations) (informations system-name)
  "
DO:  Set the informations of an existing asdf-system or add a new asdf-system
     in the cache.
"
  (let ((sl (member system-name *asdf-system-cache*
                    :key (function asi-name) :test (function string-equal))))
    (if sl
        (setf (car sl) informations)
        (push informations *asdf-system-cache*)))
  (asc-save))



(defun asc-update-cliki-tarball-locations (&key (verbose nil))
  "
DO:     Updates the cache with the asdf-systems known by www.cliki.net.
"
  (asc-load)
  (dolist (system-name (fetch-asdf-system-list :verbose verbose))
    (let ((asi  (asc-system-informations system-name)))
      (multiple-value-bind (url response headers)
          (url-of-asdf-system asdf-system-name :verbose verbose)
        (if asi
            (setf (asi-remote-response      asi)  response
                  (asi-remote-url           asi)  url
                  (asi-remote-last-modified asi) (cdr (assoc :last-modified
                                                             headers)))
            (setf (asc-system-informations system-name)
                  (make-asi :name system-name
                            :remote-response response
                            :remote-url url
                            :remote-last-modified (cdr (assoc :last-modified
                                                              headers))))))))
  (asc-save))


(defun asc-system-list ()
  "RETURN: A list of the system names of the systems known to ASC."
  (mapcar (function asi-name) *asdf-system-cache*))


(defun find-system-tarball-location (package-name-or-url)
  "
RETURN: If PACKAGE-NAME-OR-URL is an url string, 
        then PACKAGE-NAME-OR-URL
        else the url to the asdf-system tarball, 
             or if not known, of the cliki.net download link.
PACKAGE-NAME-OR-URL:  Either a http:// url, 
                          or a file:// url,
                          or a asdf system name.
"
  (let ((url (if (or (= (mismatch package-name-or-url "http://") 7)
                     (= (mismatch package-name-or-url "file://") 7))
                 package-name-or-url
                 (or (let ((asi (asc-system-informations package-name-or-url)))
                       (and asi (or (asi-cached-url asi) (asi-remote-url asi))))
                     (format nil "http://www.cliki.net/~A?download"
                             package-name-or-url)))))
    (if (= (mismatch url "file://") 7)
        (let ((stream (open (subseq url 7)
                            :if-does-not-exist nil
                            :external-format
                            #+clisp charset:iso-8859-1
                            #+sbcl :iso-8859-1
                            #- (or clisp sbcl) :default)))
          (if stream
              (values 200 '() stream url)
              (values 404 '() nil    url)))
        (block got
          (loop
             (destructuring-bind (response headers stream) (url-connection url)
               (unless (member response '(301 302))	       
                 (return-from got (values response headers stream url)))
               (close stream)
               (setf url (cdr (assoc :location headers)))))))))


(defun prepare-informations (pattern informations)
  (let ((name (asi-name informations)))
    (when (or (null pattern)
              #+clisp (regexp:match pattern name :ignore-case t)
              #-clisp (search pattern name :test (function char-equal)))
      (list
       (list*
        name
        (/= 200 (asi-remote-response information))
        (asi-remote-response information)
        (asi-remote-url informations)
        (asi-cached-url informations)
        (let ((system (ignore-errors (asdf:find-system name))))
          (if system
              (list
               (namestring
                (truename (asdf:component-relative-pathname system)))
               (namestring
                (truename (asdf:system-definition-pathname system)))
               (or (asdf:system-version system)
                   "Unknown")
               (or (ignore-errors (asdf:system-author system))
                   "Anonymous")
               (or (ignore-errors (asdf:system-maintainer system))
                   "Nobody")
               (or (ignore-errors (asdf:system-description system))
                   "No description")
               (or (ignore-errors (asdf:system-long-description system))
                   "No long description")
               (or (ignore-errors (asdf:system-licence system))
                   "Unspecified (better consider it proprietary)"))
              (list
               nil nil
               (or (asi-version          informations) "Unknown")
               (or (asi-author           informations) "Anonymous")
               (or (asi-maintainer       informations) "Nobody")
               (or (asi-description      informations) "No description")
               (or (asi-long-description informations) "No long description")
               (or (asi-licence          informations)
                   "Unspecified (better consider it proprietary)")))))))))

(defun lsasc (&key (name nil) (output *standard-output*))
  "
DO:     Prints on the *standard-output* the list of the systems known to ASC.
OUTPUT: The stream onto which the output is printed.
NAME:   A string (or a regexp in clisp) used to select the system-names listed.
"
  (let ((data (sort
               (mapcan
                (lambda (informations) (prepare-informations name informations))
                *asdf-system-cache*)
               (function string-lessp) :key (function first))))
    (force-output *error-output*)
    (force-output *trace-output*)
    (force-output *standard-output*)
    (format output "~2%ASDF-SYSTEMS~2%")
    (format output
      "~:{~%  ~A~
   ~:[~*~;~%     Response:          ~A~]~
          ~%     Tarball:           ~A~
  ~:[~;~:*~%     Cached tarball:    ~A~]~
     ~:[~*~%     NOT INSTALLED        ~
     ~;~:*~%     Installed in:      ~A~
          ~%     System definition: ~A~
        ~]~%     Version:           ~A~
          ~%     Author:            ~A~
          ~%     Maintainer:        ~A~
          ~%     Description:       ~A~
          ~%     Description long:  ~A~
          ~%     License:           ~A~
          ~%~}" data)
    (force-output output))
  (values))




(defun extract-tarball (tarball-pathname install-directory)
  "
DO:     Extract the tarball archive at TARBALL-PATHNAME in the INSTALL-DIRECTORY.
RETURN: The output of the tar command.
"
  #-(or :win32 :mswindows)
  (return-output-from-program
   *gnu-tar-program*  (list "-C"    (namestring (truename install-directory))
                            "-xzvf" (namestring (truename tarball-pathname))))
  #+(or :win32 :mswindows)
  ;; This is silly! clisp can be compiled on MS-Windows without cygwin!!!
  (return-output-from-program
   *cygwin-bash-program*
   (list "-l" "-c" (format nil "\"tar -C    \\\"`cygpath '~A'`\\\" ~
                            -xzvf \\\"`cygpath '~A'`\\\"\""
                           (namestring (truename install-directory))
                           (namestring (truename tarball-pathname))))))


(defun delete-directory (path)
  #-(or :win32 :mswindows)
  (return-output-from-program "rm" (list "-rf" (namestring (truename path))))
  #+(or :win32 :mswindows)
  ;; This is silly! clisp can be compiled on MS-Windows without cygwin!!!
  (return-output-from-program
   *cygwin-bash-program*
   (list "-l" "-c" (format nil "\"rm -rf \\\"`cygpath '~A'`\\\""
                           (namestring (truename path))))))



(defun ensure-this-directory-exists (directory)
  "
DO:  Ensures the given directory exists.
"
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :name "TEST" :type "TEST" :version nil
                   :defaults directory)
    directory nil)))


(defmacro successfully (&body body)
  `(handler-case (values t (multiple-value-list (progn ,@body))) (error () nil)))


(defmacro define-safe-accessor (name slot &optional (default-value nil))
  `(defun ,name (self)
     (if (slot-boundp self ',slot) (slot-value self ',slot) ,default-value)))

(define-safe-accessor safe-system-version          asdf::version     "0.0.0")
(define-safe-accessor safe-system-description      asdf::description)
(define-safe-accessor safe-system-long-description asdf::long-description)
(define-safe-accessor safe-system-author           asdf::author)
(define-safe-accessor safe-system-maintainer       asdf::maintainer)
(define-safe-accessor safe-system-licence          asdf::licence)


(defun download-signature (system-name tarball-path &key (verbose nil))
  (let* ((signature-url
          (format nil "~A.asc" (asi-remote-url
                                (asc-system-informations system-name))))
         (signature-path
          (format nil "~A.asc" (namestring (truename tarball-path))))
         (data (fetch-url signature-url :verbose verbose)))
    (when data
      (with-open-file
          (asc signature-path
               :direction :output
               :element-type (if (stringp data)
                                 'character
                                 '(unsigned-byte 8))
               :external-format (if (stringp data)
                                    #+clisp (ext:make-encoding
                                             :charset charset:iso-8859-1
                                             :line-terminator :unix)
                                    #+sbcl :iso-8859-1
                                    #-(or clisp sbcl) :default
                                    :default)
               :if-does-not-exist :create
               :if-exists :supersede)
        (write-sequence data asc))
      (when verbose
        (format t "~&;;; Stored signature for ~A at~%;;;    ~A~%"
          system-name (namestring (truename signature-path)))))))


(defmacro masking-system (name &body body)
  ;; Unfortunately loading one asdf system may define several asdf systems.
  ;; Unbelievable but true...
  ;; Fortunately, this doesn't matter for us since we only
  ;; find systems by their installation name.
  (let ((vbody    (gensym))
        (vname    (gensym))
        (vsaved   (gensym))
        (vpresent (gensym)))
    `(let ((,vname ,name))
       (flet ((,vbody () ,@body))
         (multiple-value-bind (,vname ,vpresent)
             (gethash ,vname asdf::*defined-systems*)
           (if ,vpresent
               (unwind-protect
                    (progn (remhash ,vname asdf::*defined-systems*)
                           (,vbody))
                 (setf (gethash ,vname asdf::*defined-systems*) ,vsaved))
               (progn (,vbody)
                      (remhash ,vname asdf::*defined-systems*))))))))


(defun parse-date (date)
  "
DO:     Parses a date with the format \"Tue, 11 Jan 2005 00:44:11 GMT\"
BUG:    The zone is not taken into account.
RETURN: The date as a universal time.
"
  (print date)
  (destructuring-bind (dow day month year hour minute second zone)
      (split-sequence:split-sequence-if (lambda (x) (position x " ,:")) date
                                        :remove-empty-subseqs t)
    (declare (ignore dow zone))
    (encode-universal-time
     (parse-integer second)
     (parse-integer minute)
     (parse-integer hour)
     (parse-integer day)
     (1+ (position month #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                   :test (function string-equal)))
     (parse-integer year)
     0)))


(defun download-tarball (system-name &key (verbose nil))
  (let ((temp-tarball (merge-pathnames
                       (make-pathname :name "TEMP" :type "TGZ" :version nil
                                      :case :common
                                      :defaults *asdf-system-cache-directory*)
                       *asdf-system-cache-directory* nil))
        (temp-dir     (merge-pathnames
                       (make-pathname :directory '(:relative "TEMP")
                                      :name nil :type nil :version nil
                                      :case :common
                                      :defaults *asdf-system-cache-directory*)
                       *asdf-system-cache-directory* nil)))
    (ensure-this-directory-exists temp-dir)
    (when verbose
      (format "~&;;; Downloading tarball for ~A~%" system-name)
      (force-output))
    (handler-case
        (multiple-value-bind (url headers)
            (copy-resource-at-url
             (asi-remote-url (asc-system-informations system-name))
             temp-tarball :verbose t)
          (when verbose
            (format t "~&;;; Extracting tarball for ~A~%" system-name)
            (force-output))
          (extract-tarball temp-tarball temp-dir)
          (masking-system system-name
            (let* ((asd-files (directory
                               (merge-pathnames
                                (make-pathname
                                 :directory '(:relative :wild-inferiors)
                                 :name :wild :type "ASD" :version :wild
                                 :case :common :defaults temp-dir)
                                temp-dir nil)))
                   (asdf:*central-registry*
                    (nconc
                     (mapcar (lambda (path)
                               (make-pathname :name nil :type nil :version nil
                                              :defaults path)) asd-files)
                     asdf:*central-registry*))
                   (system
                    (cond
                      ((null asd-files)
                       (error "No ASD file in tarball for system ~A"
                              system-name))
                      ((asdf:find-system system-name))
                      ((null (rest asd-files))
                       (asdf:find-system (first asd-files)))
                      (t (error "Several ASD files in tarball for system ~A"))))
                   (system-name (asdf:component-name system))
                   (tarball-path (merge-pathnames
                                  (make-pathname
                                   :name (format nil "~:@(~A-~A~)"
                                                 system-name
                                                 (safe-system-version system))
                                   :type "TGZ" :version nil :case :common
                                   :defaults *asdf-system-cache-directory*)
                                  *asdf-system-cache-directory* nil)))
              (ignore-errors (delete-file tarball-path))
              (rename-file temp-tarball tarball-path)
              (delete-directory temp-dir)
              (when verbose
                (format t "~&;;; Stored tarball for ~A at~%;;;    ~A~%"
                        system-name (namestring (truename tarball-path)))
                (force-output))
              (download-signature system-name tarball-path :verbose verbose)
              (setf (asc-system-informations system-name)
                    (make-asi
                     :name                 system-name
                     :remote-response      200
                     :remote-url           url
                     :remote-last-modified (parse-date
                                            (cdr (assoc :last-modified
                                                        (print headers))))
                     :cached-url (format nil "file://~A"
                                         (namestring (truename tarball-path)))
                     :cached-last-modified (file-write-date tarball-path)
                     :version          (safe-system-version          system)
                     :description      (safe-system-description      system)
                     :long-description (safe-system-long-description system)
                     :author           (safe-system-author           system)
                     :maintainer       (safe-system-maintainer       system)
                     :licence          (safe-system-licence          system))))))
      (error (err)
        (format t "~&;;; Couldn't download tarball for system ~A~%" system-name)
        (format t "~&;;; error = ~A ~:*~S~%" err)
        (force-output)
        (error err)))))


(defun download-all-tarballs (&key (verbose nil))
  (dolist (system-name (asc-system-list))
    (ignore-errors
      (download-tarball (string-downcase system-name) :verbose verbose))))








;; Local Variable:
;; eval: (cl-indent 'masking-system 1)
;; End:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
