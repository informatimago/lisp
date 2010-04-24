;;;; -*- coding:utf-8 -*-
;;****************************************************************************
;;FILE:               web-cache.lisp
;;LANGUAGE:           Common-Lisp
;;SYSTEM:             Common-Lisp
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    This package implements a cache for web pages, 
;;    mapping urls to HTML text and parsed tree.
;;    
;;AUTHORS
;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2004-11-20 <PJB> Created.
;;BUGS
;;    On clisp rename-file is not atomic.
;;LEGAL
;;    GPL
;;    
;;    Copyright Pascal J. Bourguignon 2004 - 2004
;;    
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;    
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.PARSE-HTML"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.WEB-CACHE"
  (:USE "COM.INFORMATIMAGO.PA.PROCESS-HTML"
        "COM.INFORMATIMAGO.PA.HTTP-CLIENT"
        "COMMON-LISP")
  (:EXPORT "*CACHE-DIRECTORY-PATH*" "SYNCHRONIZE-CACHE" "FORGET-ALL"
           "FORGET-URI" "FREE-PARSED-HTML-AT-URI" "FREE-RESOURCE-AT-URI"
           "GET-PARSED-HTML-AT-URI" "GET-RESOURCE-AT-URI")
  (:DOCUMENTATION
   "
    This package implements a cache for web pages, 
    mapping urls to HTML text and parsed tree.
    Copyright Pascal J. Bourguignon 2004 - 2004
    
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.WEB-CACHE")




(defclass web-cache ()
  ((directory-path
    :reader   cache-directory-path
    :initarg :directory-path
    :type     pathname
    :initform (make-pathname
               :directory (append (pathname-directory 
                                   (user-homedir-pathname)) 
                                  '("WEB-CACHE"))
               :defaults (user-homedir-pathname))
    :documentation "Path to the directory where the cache data is stored.")
   (index-file-path
    :reader   cache-index-file-path
    :type     (or null pathname)
    :initform nil
    :documentation "Path to the cache index file.")
   (index
    :accessor cache-index
    :type     hash-table
    :initform (make-cache-index)
    :documentation "An equal hash table mapping urls to cache-entries")
   (cache-date
    :accessor cache-date
    :type     cons
    :initform (cons 0 0)
    :documentation "The date (universal time) and increment of
                    last modification of the index map.")
   (resource-file-type 
    :accessor cache-resource-file-type
    :initarg :resource-file-type
    :type     string
    :initform "DAT")
   (parsed-html-file-type 
    :accessor cache-parsed-html-file-type
    :initarg :parsed-html-file-type
    :type     string
    :initform "LHT")
   )
  (:documentation "A cache for web pages."));;web-cache


(defmethod set-directory-path ((self web-cache) new-path)
  (assert (typep new-path 'pathname))
  (setf (slot-value self 'directory-path) new-path
        (slot-value self 'index-file-path)
        (make-pathname :name "CACHE" :type "IDX" 
                       :defaults new-path)))

(defsetf 'directory-path 'set-directory-path)


(defun compare-cache-date (a b)
  "
Time stamps (cache date) are (unversal-time . increment).
"
  (cond
    ((null a) (if b -1 0))
    ((null b) 1)
    ((< (car a) (car b)) -1)
    ((> (car a) (car b)) 1)
    (t (cond ((< (cdr a) (cdr b)) -1)
             ((> (cdr a) (cdr b)) 1)
             (t 0)))))


(defmethod touch-cache-date ((self web-cache))
  (if (and (cache-date self) (= (car (cache-date self)) (get-universal-time)))
      (incf (cdr (cache-date self)))
      (setf (cache-date sefl) (cons (get-universal-time) 0))))
          

(defstruct cache-entry 
  "A cache index entry, mapping an url with the date the resource 
was fetched and the file-name of the files where the resource and
the parsed html are stored, and references to these data when they 
are loaded in core."
  (url              ""  :type string)
  (date-fetched     0   :type (integer 0)) ; the universal time.
  (file-name        ""  :type string)   ; name of the files.
  (header           nil :type list)     ; http header
  (resource-p       nil :type boolean)  ; resource is loaded.
  (resource         nil :type (or string (vector (unsigned-byte 8))))
  (parsed-html-p    nil :type boolean)  ; parsed-html is loaded.
  (parsed-html      nil :type list))     ; the parsed html tree.


(defun make-cache ()
  (make-hash-table :test (function equal)))


(defun cache-enter (cache entry)
  "
DO:     Enter the entry into the CACHE.
"
  (setf (gethash (cache-entry-url entry) cache) entry))


;; cache-index file:
;; header: (:type :web-cache-index :date cache-date)
;; record: (:url str :date-fetched int :file-name str :header lst)


(defun cache-header-p (header)
  "
RETURN: Whether HEADER is a cache index file header.
"
  (and (listp header)
       (eq (getf header :type) :web-cache-index)
       (consp (getf header :date))
       (typep (car (getf header :date)) '(integer 0))
       (typep (cdr (getf header :date)) '(integer 0))))


(defun cache-record-p (record)
  "
RETURN: Whether RECORD is a cache index file record.
"
  (and (listp record)
       (typep (getf record :date-fetched)     '(integer 0))
       (typep (getf record :url)              'string)
       (typep (getf record :file-name)        'string)
       (typep (getf record :header)           'list)))


(defmethod cache-index-read-date ((self web-cache))
  "
RETURN: If the file (cache-index-file-path self) exists 
        and is a cache index file,
        then the cache-date of the cache index file,
        else NIL.
"
  (with-open-file (index (cache-index-file-path self)
                         :direction :input :if-does-not-exist nil)
    (when index
      (with-standard-io-syntax
        (let* ((*read-eval* nil)
               (header (read index nil nil)))
          (and (cache-header-p header)
               (getf header :date)))))))


(defmethod load-cache ((self web-cache))
  "
DO:     Load the cache index from the file (cache-index-file-path self).
"
  (with-open-file (index (cache-index-file-path self)
                         :direction :input :if-does-not-exist :error)
    (when index
      (with-standard-io-syntax
        (let* ((*read-eval* nil)
               (header (read index nil nil))
               (cache  (make-cache))
               (cache-date))
          (when (cache-header-p header)
            (setf cache-date (getf header :date))
            (loop for record = (read index nil nil)
                  while (cache-record-p record)
                  do (cache-enter
                      cache (make-cache-entry
                             :url          (getf record :url) 
                             :date-fetched (getf record :date-fetched)
                             :file-name    (getf record :file-name)
                             :header       (getf record :header)))
                  finally (when record
                            (error "Invalid cache index record: ~S" record))))
          (setf (cache-index self) cache
                (cache-date  self) cache-date))))));;load-cache


(defmethod save-cache ((self web-cache))
  "
DO:     Save the cache index to the file (cache-index-file-path self).
"
  (ensure-directories-exist (cache-index-file-path))
  (with-open-file (index (make-pathname :type "NEW"
                                        :defaults (cache-index-file-path self))
                         :direction         :output
                         :if-does-not-exist :create
                         :if-exists         :supersede)
    (prin1 `(:type :web-cache-index :date ,(cache-date self)) index)
    (terpri index)
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (prin1 `(:url          ,(cache-entry-url v)
                :date-fetched ,(cache-entry-date-fetched v)
                :file-name    ,(cache-entry-file-name v)
                :header       ,(cache-entry-header v)) index)
       (terpri index))
     (cache-index self))
    (handler-case (rename-file index (cache-index-file-path self))
      ;; CLISP<=2.33.2 RENAME-FILE is buggy:
      (file-error (err)
        (unless (and (equal (cache-index-file-path self)
                            (file-error-pathname err))
                     (probe-file (cache-index-file-path self)))
          (signal err))
        (let ((old (make-pathname :type "OLD" 
                                  :defaults (cache-index-file-path self))))
          (when (probe-file old) (delete-file old))
          (rename-file (cache-index-file-path self) old)
          (unwind-protect (rename-file index (cache-index-file-path self))
            (when (and (probe-file old)
                       (probe-file (cache-index-file-path self)))
              (ignore-errors (delete-file (cache-index-file-path self))))
            (rename-file old (cache-index-file-path self))
            (signal err)))))));;save-cache


(defmethod synchronize-cache ((self web-cache))
  "
DO:     Ensure the cache index in core and on disk are synchronized.
"
  (let ((disk-date (cache-index-read-date self)))
    (case (compare-cache-date (cache-date self) disk-date)
      ((-1) (load-cache self))
      ((1)  (save-cache self)))))


(defun make-new-name (type directory)
  (loop for name = (format nil "~36,4,'0R~36,4,'0R" 
                           (random (expt 36 4)) 
                           (mod (get-universal-time) (expt 36 4)))
        for count from 0
        if (<= 512 count) then (error "Cannot come with a unique file name.")
        until (ignore-errors
                (not (probe-file (make-pathname 
                                  :name name :type type
                                  :directory directory))))
        finally (return name)))


(defun cache-file-path (directory name type)
  (make-pathname :name name :type type
                 :directory (append (pathname-directory directory)
                                    (list (subseq name 0 1) (subseq name 1 2)))
                 :defaults directory))


(defmethod FREE-RESOURCE-AT-URI ((self web-cache) URI)
  (let ((entry (gethash uri (cache-index self))))
    (when entry
      (setf (cache-entry-resource   entry) nil
            (cache-entry-resource-p entry) nil))))


(defmethod FREE-PARSED-HTML-AT-URI ((self web-cache) URI)
  (let ((entry (gethash uri (cache-index self))))
    (when entry
      (setf (cache-entry-parsed-html   entry) nil
            (cache-entry-parsed-html-p entry) nil))))


(defmethod FORGET-URI ((self web-cache) URI)
  "
DO:     Forget the resource at URI; delete the cached files.
"
  (synchronize-cache)
  (let ((entry (gethash uri (cache-index self))))
    (when entry
      (ignore-errors
        (delete-file (cache-file-path (cache-entry-file-name entry) 
                                      (cache-resource-file-type self)
                                      (cache-directory-path self))))
      (ignore-errors
        (delete-file (cache-file-path (cache-entry-file-name entry) 
                                      (cache-parsed-html-file-type self)
                                      (cache-directory-path self))))
      (remhash  uri (cache-index self))
      (touch-cache-date self))))


(defmethod FORGET-ALL ((self web-cache))
  "
DO:     Forget all the URI, deleting all the cached files.
"
  (maphash (lambda (uri entry)
             (declare (ignore entry)) 
              (forget-uri self uri))
           (cache-index self))
  (synchronize-cache self))



;; (defstruct cache-entry 
;;   "A cache index entry, mapping an url with the date the resource 
;; was fetched and the file-name of the files where the resource and
;; the parsed html are stored, and references to these data when they 
;; are loaded in core."
;;   (url              ""  :type string)
;;   (date-fetched     0   :type (integer 0)) ; the universal time.
;;   (file-name        ""  :type string)   ; name of the files.
;;   (header           nil :type list)     ; http header
;;   (resource-p       nil :type boolean)  ; resource is loaded.
;;   (resource         nil :type (or string (vector (unsigned-byte 8))))
;;   (parsed-html-p    nil :type boolean)  ; parsed-html is loaded.
;;   (parsed-html      nil :type list))
;; (cache-enter
;;                       cache (make-cache-entry
;;                              :url          (getf record :url) 
;;                              :date-fetched (getf record :date-fetched)
;;                              :file-name    (getf record :file-name)
;;                              :header       (getf record :header)))


(defmethod put-resource-at-uri ((self web-cache) uri resource-file)
  (let ((entry (gethash uri (cache-index self))))
    (if entry
        (setf (cache-entry-reresouce-file))
        (not-implemented-yet))))


(defun get-resource-at-uri (uri)
  (unless *cache* (synchronize-cache))
  (when *cache*
    (gethash uri *cache*))
  (unless entry
    (multiple-value-bind (header body) (FETCH-RESOURCE-AT-URI uri)
      )))


(define-message html-page-req (sender uri))
(define-message html-page-rep (sender page-ref))
(define-message html-tree-req (sender uri))
(define-message html-tree-rep (sender tree-ref))


(send (make-instance 'html-page-req :sender myself :uri uri))

(loop for mesg = (message-receive-sexp queue +cache-message-type+)
 (case (car mesg)
   ((:get-html-page)
    (let* ((sender (first mesg))
           (uri    (second mesg))
           (page   (get-resource-at-uri uri)))
      (if page 
           ;; TODO: actually copy page to shared memory and send only a reference.
          (message-send-sexp queue sender (list :html-page uri page))
          (progn
            ;; if the request is already in the queue, then forget it.
            ;; if it comes from somebody else, then keep it
            ;; keep the request in a queue:
            (save-request mesg)
            ;; only proceed if the uri is not in the request queue.
            (message-send-sexp queue *fetcher* (list :fetch-uri uri))))))
   ((:get-html-tree)
    ;; about the same, but if the tree is not in the cache, check first for
    ;; the page and skip fetching: just request processing
    )
   ((:fetched-resource)
    )))


(defun get-parsed-html-at-uri (uri)
  
  )



;; (ensure-directories-exist  (cache-index-file-path))
;; (close (open (cache-index-file-path) :direction :output :if-does-not-exist :create))

;;;; web-cache.lisp                   --                     --          ;;;;
