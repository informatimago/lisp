;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               cache.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A generic disk-based cache.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-01-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CACHE"
  (:USE "COMMON-LISP")
  (:EXPORT "CACHE-EXPIRE-ALL" "CACHE-EXPIRE" "CACHE-EXPIRATION" "CACHE-GET"
           "SYNCHRONIZE-CACHE" "MAKE-CACHE" "CACHE-PRODUCER" "CACHE-VALUE-FILE-TYPE"
           "CACHE-INDEX-FILE-PATH" "CACHE-DIRECTORY-PATH" "CACHE")
  (:DOCUMENTATION
   "
    A generic disk-based cache.

    Copyright Pascal J. Bourguignon 2005 - 2005
    
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CACHE")



;; (make-cache directory  producer &key value-file-type) --> cache
;; ;;(producer key) --> value ; expiration
;; (cache-get        cache key) --> value ; (or nil :cached :fetched)
;; (cache-expiration cache key) --> expiration (or null universal-time)
;; (cache-expire     cache key &key keep-file)
;; (cache-expire-all cache     &key keep-files)
;; 
;; key [expiration] value
;; 
;; The cache index is loaded in RAM


(defclass cache ()
  ((cache-directory-path
    :reader   cache-directory-path
    :initarg :directory-path
    :type     pathname
    :initform (make-pathname
               :directory (append (pathname-directory (user-homedir-pathname))
                                  '("CACHE"))
               :defaults  (user-homedir-pathname))
    :documentation "Path to the directory where the cache data is stored.")
   (cache-index-file-path
    :reader   cache-index-file-path
    :type     pathname
    :documentation "Path to the cache index file.")
   (cache-value-file-type 
    :reader   cache-value-file-type 
    :initarg :value-file-type
    :type     string
    :initform "DAT"
    :documentation "The type used for value files in the cache directory.")
   (cache-producer
    :reader   cache-producer
    :initarg :producer
    :type     '(function (t) (values t (integer 0)))
    ;; (producer key) --> value ; expiration
    :documentation "The function used to get fresh data. 
     Returns two values: the data and the expiration universal time.")
   (index
    :type     hash-table
    :initform (make-index)
    :documentation "An equal hash table mapping urls to cache-entries")
   (date
    :type     cons
    :initform (cons 0 0)
    :documentation "The date (universal time) and increment of
                    last modification of the index map."))
  (:documentation "A generic disk-based cache."))


;; cache-index file: header record*
;; header: (:type :cache-index :date cache-date :value-file-type str)
;; record: (:key readable :file-name str :fetch-date int :expire-date int)

(DEFGENERIC set-cache-directory-path (self new-path))
(DEFGENERIC touch-cache-date (self))
(DEFGENERIC cache-index-read-date (self))
(DEFGENERIC load-cache (self))
(DEFGENERIC save-cache (self))
(DEFGENERIC synchronize-cache (self))
(DEFGENERIC old-value-file-path (self name))
(DEFGENERIC new-value-file-path (self))
(DEFGENERIC cache-expiration (self key))
(DEFGENERIC cache-expire (self key &key keep-file))
(DEFGENERIC cache-expire-all (self &key keep-files))
(DEFGENERIC cache-map-entries (self result-type function))


(defmethod initialize-instance :after ((self cache) &rest args)
  (declare (ignore args))
  (set-cache-directory-path self (cache-directory-path self))
  self)


(defmethod set-cache-directory-path ((self cache) new-path)
  (assert (typep new-path 'pathname))
  (setf (slot-value self 'cache-directory-path)  new-path
        (slot-value self 'cache-index-file-path) (make-pathname 
                                                  :name "CACHE" :type "IDX" 
                                                  :defaults new-path)))


(defsetf cache-directory-path set-cache-directory-path)


(defun compare-cache-date (a b)
  "
A,B:    Time stamps (cache date) are (universal-time . increment).
RETURN: -1 <=> a<b, 0 <=> a=b, +1 <=> a>b
"
  (cond
    ((null a) (if b -1 0))
    ((null b) 1)
    ((< (car a) (car b)) -1)
    ((> (car a) (car b)) 1)
    (t (cond ((< (cdr a) (cdr b)) -1)
             ((> (cdr a) (cdr b)) 1)
             (t 0)))))


(defmethod touch-cache-date ((self cache))
  "
DO: Update the date of the cache in core.
"
  (symbol-macrolet ((date (slot-value self 'date)))
    (if (and date (= (car date) (get-universal-time)))
        (incf (cdr date))
        (setf date (cons (get-universal-time) 0)))))
          

(defstruct entry 
  "A cache index entry, mapping a key with the date the resource 
was fetched and the file-name of the files where the resource and
the parsed html are stored, and references to these data when they 
are loaded in core."
  (key         nil)
  (value       nil)
  (value-p     nil :type boolean)       ; value is loaded.
  (file-name   ""  :type string)        ; name of the file.
  (fetch-date  0   :type (integer 0))   ; universal time.
  (expire-date 0   :type (integer 0)))  ; universal time.


(defun make-index ()            (make-hash-table :test (function equal)))
(defun index-put  (index entry) (setf (gethash (entry-key entry) index) entry))
(defun index-get  (index key)   (gethash key index))
(defun index-remove (index key) (remhash key index))
(defun index-map-entries (result-type fun index)
  (ecase result-type
    ((nil)
     (maphash (lambda (k v) 
                (declare (ignore k))
                (funcall fun v)) index)
     nil)
    ((list)
     (let ((result '()))
       (maphash (lambda (k v) 
                  (declare (ignore k))
                  (push (funcall fun v) result)) index)
       (nreverse result)))
    ((vector string)
     (let ((result (if (eq 'vector result-type)
                       (make-array (list (hash-table-count index)))
                       (make-string (hash-table-count index))))
           (i      -1))
       (maphash (lambda (k v) 
                  (declare (ignore k))
                  (setf (aref result (incf i)) (funcall fun v))) index)
       result))))


(defun cache-header-p (header)
  "
RETURN: Whether HEADER is a cache index file header.
"
  ;; header: (:type :cache-index :date cache-date :value-file-type str)
  (and (listp header)
       (eq (getf header :type) :cache-index)
       (stringp    (getf header :value-file-type))
       (consp      (getf header :date))
       (typep (car (getf header :date)) '(integer 0))
       (typep (cdr (getf header :date)) '(integer 0))))


(defun cache-record-p (record)
  "
RETURN: Whether RECORD is a cache index file record.
"
  ;; record: (:key readable :file-name str :fetch-date int :expire-date int)
  (and (listp record)
       (getf record :key)
       (stringp    (getf record :file-name))
       (integerp   (getf record :fetch-date))
       (integerp   (getf record :expire-date))))


(defmethod cache-index-read-date ((self cache))
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


(defmethod load-cache ((self cache))
  "
DO:     Load the cache index from the file (cache-index-file-path self).
"
  (format *trace-output* "~&Loading cache ~S~%" (cache-index-file-path self))
  (with-open-file (file (cache-index-file-path self)
                        :direction :input :if-does-not-exist :error)
    (with-standard-io-syntax
      (let* ((*read-eval* nil)
             (header (read file nil nil))
             (index  (make-index))
             (cache-date))
        (when (cache-header-p header)
          (setf cache-date (getf header :date))
          (loop for record = (read file nil nil)
             while (cache-record-p record)
             do (index-put index
                           (make-entry
                            :key          (getf record :key) 
                            :file-name    (getf record :file-name)
                            :fetch-date   (getf record :fetch-date)
                            :expire-date  (getf record :expire-date)))
             finally (when record
                       (error "Invalid cache index record: ~S" record)))
          (setf (slot-value self 'cache-value-file-type) 
                (getf header :value-file-type)
                (slot-value self 'index) index
                (slot-value self 'date)  cache-date))))))


(defun safe-rename-file (old-name new-name)
  (handler-case (rename-file old-name new-name)
    ;; CLISP<=2.33.2 RENAME-FILE is buggy:
    (file-error (err)
      (unless (and (equal new-name (file-error-pathname err))
                   (probe-file new-name))
        (signal err))
      (let ((old (make-pathname :type "OLD" :defaults old-name)))
        (when (probe-file old) (delete-file old))
        (rename-file new-name old)
        (unwind-protect (rename-file old-name new-name)
          (if (and (probe-file old) (probe-file new-name))
              (ignore-errors (delete-file old))
              (progn (rename-file old new-name)
                     (signal err))))))))


(defmethod save-cache ((self cache))
  "
DO:     Save the cache index to the file (cache-index-file-path self).
"
  (format *trace-output* "~&Saving cache ~S~%" (cache-index-file-path self))
  (ensure-directories-exist (cache-index-file-path self))
  (let ((tmp-name (make-pathname :type "NEW"
                                 :defaults (cache-index-file-path self))))
    (with-open-file (file tmp-name
                          :direction         :output
                          :if-does-not-exist :create
                          :if-exists         :supersede)
      (prin1 `(:type :cache-index 
                     :date ,(slot-value self 'date)
                     :value-file-type ,(cache-value-file-type self)) file)
      (terpri file)
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (prin1 `(:key          ,(entry-key v)
                                        :file-name    ,(entry-file-name v)
                                        :fetch-date   ,(entry-fetch-date v)
                                        :expire-date  ,(entry-expire-date v)) file)
                 (terpri file))
               (slot-value self 'index)))
    (safe-rename-file tmp-name (cache-index-file-path self))))


(defmethod synchronize-cache ((self cache))
  "
DO:     Ensure the cache index in core and on disk are synchronized.
"
  (let ((disk-date (cache-index-read-date self)))
    (case (compare-cache-date (slot-value self 'date) disk-date)
      ((-1) (load-cache self))
      ((1)  (save-cache self)))))


(defun value-file-path (directory name type)
  (make-pathname :name name :type type
                 :directory (append (pathname-directory directory)
                                    (list (subseq name 0 1) (subseq name 1 2)))
                 :defaults directory))


(defmethod old-value-file-path ((self cache) name)
  (value-file-path (cache-directory-path self) 
                   name
                   (cache-value-file-type self)))


(defmethod new-value-file-path ((self cache))
  (loop with dire = (cache-directory-path self)
     with type = (cache-value-file-type self)
     for count from 0
     for name = (format nil "~36,4,'0R~36,4,'0R" 
                        (random (expt 36 4)) 
                        (mod (get-universal-time) (expt 36 4)))
     for path = (value-file-path dire name type)
     until (ignore-errors (not (probe-file path)))
     do (when (<= 512 count) 
          (error "Cannot come with a unique file name."))
     finally (return path)))


(defun make-cache (directory producer &key value-file-type)
  "
DO:     Make a new cache instance.
NOTE:   Send synchronize
"
  (if value-file-type
      (make-instance 'cache :directory-path directory :producer producer
                     :value-file-type value-file-type)
      (make-instance 'cache :directory-path directory :producer producer)))


(defgeneric cache-get (cache key))

(defmethod cache-get        ((self cache) key)
  (synchronize-cache self)
  (let ((entry (index-get (slot-value self 'index) key)))
    (cond
      ((or (null entry)                 ; no entry ==> fetch
           (< (entry-expire-date entry) (get-universal-time)))
       (if (null entry)
           (format *trace-output*
             "~&(cache-get ~S): No cache entry ==> fetch~%" key)
           (format *trace-output*
             "~&(cache-get ~S): expired (~A<~A) ==> fetch~%"
             key (entry-expire-date entry) (get-universal-time)))
       #+(or)(invoke-debugger (make-condition 'simple-error
                                              :format-control "~&~S not in ~S~%"
                                              :format-arguments (list key (slot-value self 'index) self)))
       (multiple-value-bind (value expire) (funcall (cache-producer self) key)
         (let* ((path (if entry 
                          (old-value-file-path self (entry-file-name entry))
                          (new-value-file-path self)))
                (entry (make-entry :key key
                                   :value value
                                   :value-p t
                                   :file-name (pathname-name path)
                                   :fetch-date (get-universal-time)
                                   :expire-date expire)))
           (ensure-directories-exist path)
           (with-open-file (out path :direction :output 
                                :if-exists :supersede
                                :if-does-not-exist :create)
             (with-standard-io-syntax (print value out)))
           (index-put (slot-value self 'index) entry)
           (touch-cache-date self)
           (synchronize-cache self)
           (values value :fetched))))
      ((entry-value-p entry)            ; ==> in core
       (format *trace-output* "~&(cache-get ~S): got it in core~%" key)
       (values (entry-value entry) :in-core))
      (t                                ; ==> read from disk
       (with-open-file (in (old-value-file-path self (entry-file-name entry))
                           :direction :input 
                           :if-does-not-exist :error)
         (let ((value (with-standard-io-syntax 
                        (let ((*read-eval* nil)) (read in)))))
           (setf (entry-value entry) value
                 (entry-value-p entry) t)
           (format *trace-output* "~&(cache-get ~S): read from disk~%" key)
           (values value :on-disk)))))))


(defmethod cache-expiration ((self cache) key)
  ;; --> expiration (or null universal-time)
  (synchronize-cache self)
  (let ((entry (index-get (slot-value self 'index) key)))
    (when entry (entry-expire-date entry)))) 


(defmethod cache-expire     ((self cache) key &key (keep-file nil))
  (synchronize-cache self)
  (let* ((index (slot-value self 'index))
         (entry (index-get index key)))
    (when entry
      (unless keep-file
        (delete-file (old-value-file-path self (entry-file-name entry))))
      (index-remove index key)
      (touch-cache-date self)
      (synchronize-cache self))))


(defmethod cache-expire-all ((self cache) &key (keep-files nil))
  (synchronize-cache self)
  (unless keep-files
    (cache-map-entries self nil (lambda (entry) 
                                  (delete-file 
                                   (old-value-file-path 
                                    self (entry-file-name entry))))))
  (setf (slot-value self 'index) (make-index))
  (touch-cache-date self)
  (synchronize-cache self))


(defmethod cache-map-entries ((self cache) result-type function)
  (index-map-entries result-type function (slot-value self 'index)))





(defvar *test-counter* 0)
(defvar *test-cache*   nil)
(defvar *test-cache-2* nil)

#+clisp
(defun cache-test ()
  (ext:shell "rm -rf /tmp/cache")
  (setf *test-counter* 0)
  (let ((delay 7))
    (flet ((producer (key) (values (format nil "~A-~A" key 
                                           (incf *test-counter* ))
                                   (+ delay (get-universal-time)))))
      (setf *test-cache* (make-cache #P"/tmp/cache/" (function producer) 
                                     :value-file-type "SYM"))
      (assert (string= (cache-get *test-cache* :one)   "ONE-1"))
      (assert (string= (cache-get *test-cache* :two)   "TWO-2"))
      (assert (string= (cache-get *test-cache* :three) "THREE-3"))
      (assert (string= (cache-get *test-cache* :one)   "ONE-1"))
      (assert (string= (cache-get *test-cache* :two)   "TWO-2"))
      (assert (string= (cache-get *test-cache* :three) "THREE-3"))
      (setf *test-cache-2* (make-cache #P"/tmp/cache/" (function producer)))
      (assert (string= (cache-get *test-cache-2* :one)   "ONE-1"))
      (assert (string= "SYM" (cache-value-file-type *test-cache-2*)))
      (format t "~2&filled:~%")(finish-output)
      (ext:shell "find /tmp/cache -type f -print|sort")
      (cache-expire *test-cache* :one)
      (cache-expire *test-cache* :two :keep-file t)
      (format t "~2&expired :one and :two:~%")(finish-output)
      (ext:shell "find /tmp/cache -type f -print|sort")
      (assert (string= (cache-get *test-cache* :one)   "ONE-4"))
      (format t "~2&expirations~%~:{~15A in ~4D seconds~%~}"
              (cache-map-entries *test-cache*
                                 'list (lambda (entry)
                                         (list
                                          (entry-key entry)
                                          (- (entry-expire-date entry)
                                             (get-universal-time))))))
      (format t "~2&waiting ~D s expiration of :one and :three:~%" delay)
      (finish-output)
      (sleep (1+ delay))
      (assert (string= (cache-get *test-cache* :one)   "ONE-5"))
      (assert (string= (cache-get *test-cache* :three) "THREE-6"))
      (cache-expire-all *test-cache*)
      (format t "~2&expired all~%")(finish-output)
      (ext:shell "find /tmp/cache -type f -print|sort")
      (assert (string= (cache-get *test-cache* :one)   "ONE-7"))
      (assert (string= (cache-get *test-cache* :three) "THREE-8"))
      (assert (string= (cache-get *test-cache-2* :one)   "ONE-7"))
      (assert (string= (cache-get *test-cache-2* :three) "THREE-8"))
      (cache-map-entries *test-cache* nil (function print))))
  (values))




#||

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

||#


;;;; cache.lisp                   --                     --          ;;;;
