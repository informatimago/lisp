;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               apple-file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a codecs for AppleSingle and AppleDouble file formats.
;;;;    http://kaiser-edv.de/documents/AppleSingle_AppleDouble.pdf
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-05-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
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
;;;;**************************************************************************


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.APPLE-FILE.APPLE-FILE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET"
                          "INCLUDE""MERGE""UNION""INTERSECTION")

  (:export
   "APPLE-FILE-REAL-NAME"
   "APPLE-FILE-COMMENT"
   "APPLE-FILE-ICON-B&W"
   "APPLE-FILE-ICON-COLOR"
   "APPLE-FILE-CREATION-DATE"
   "APPLE-FILE-MODIFICATION-DATE"
   "APPLE-FILE-BACKUP-DATE"
   "APPLE-FILE-ACCESS-DATE"
   "APPLE-FILE-FINDER-CREATOR"
   "APPLE-FILE-FINDER-TYPE"
   "APPLE-FILE-FINDER-LOCATION"
   "APPLE-FILE-FINDER-ICON-ID"
   "APPLE-FILE-FINDER-FOLDER"
   "APPLE-FILE-FINDER-PUT-AWAY-FOLDER"
   "APPLE-FILE-MACINTOSH-PROTECTED"
   "APPLE-FILE-MACINTOSH-LOCKED"
   "APPLE-FILE-PRODOS-ACCESS"
   "APPLE-FILE-PRODOS-TYPE"
   "APPLE-FILE-PRODOS-AUXILIARY-TYPE"
   "APPLE-FILE-MSDOS-ATTRIBUTES"
   "APPLE-FILE-AFP-BACKUP-NEEDED"
   "APPLE-FILE-AFP-SYSTEM"
   "APPLE-FILE-AFP-MULTI-USER"
   "APPLE-FILE-AFP-INVISIBLE"
   "APPLE-FILE-AFP-DIRECTORY-ID"))
  
(in-package "COM.INFORMATIMAGO.COMMON-LISP.APPLE-FILE.APPLE-FILE")



(deftype octet () '(unsigned-byte 8))
(defun equal-type (a b) (and (subtypep a b) (subtypep b a)))


;;----------------------------------------------------------------------
;; Apple Single & Apple Double File Format
;;----------------------------------------------------------------------

(defconstant +apple-format-version+       #x00020000)
(defconstant +apple-single-magic-number+  #x00051600)
(defconstant +apple-double-magic-number+  #x00051607)


(defenum predefined-entry-id
  (data-fork-id 1)
  resource-fork-id
  real-name
  comment
  icon-b&w
  icon-color
  (file-dates-info 8)
  finder-info
  macintosh-file-info
  prodos-file-info
  msdos-file-info
  short-name
  afp-file-info
  directory-id)

(defparameter *symbolic-entry-ids* #(nil
                                     :data-fork-id 
                                     :resource-fork-id
                                     :real-name
                                     :comment
                                     :icon-b&w
                                     :icon-color
                                     nil
                                     :file-dates-info 
                                     :finder-info
                                     :macintosh-file-info
                                     :prodos-file-info
                                     :msdos-file-info
                                     :short-name
                                     :afp-file-info
                                     :directory-id))

(defun symbolicate-entry-id (entry-id)
  (when (<= 0 entry-id (1- (length *symbolic-entry-ids*)))
    (aref *symbolic-entry-ids* entry-id)))




(defclass apple-file ()
  ((kind        :initform :apple-single
                :initarg  :kind
                :accessor header-kind
                :reader   apple-file-header-kind
                :type     (member :apple-single :apple-double :apple-triple))
   (magic       :initform 0
                :initarg  :magic
                :accessor header-magic
                :reader   apple-file-header-magic)
   (version     :initform 0
                :initarg  :version
                :accessor header-version
                :reader   apple-file-header-version)
   (entries     :initform '()
                :initarg  :entries
                :accessor header-entries
                :reader   apple-file-header-entries)
   (info-stream :initform nil
                :initarg  :info-stream
                :accessor header-info-stream
                :reader   apple-file-info-stream)
   (direction   :initform :input
                :initarg  :direction
                :reader   apple-file-direction)))


(defstruct entry
  kind
  id
  offset
  length
  decoded)

(defstruct (file-dates-info
             (:conc-name file-))
  creation-date
  modification-date
  backup-date
  access-date)

(defstruct finder-info
  type
  creator
  flags 
  location.y
  location.x
  folder 
  icon-id 
  script 
  xflags 
  comment-id
  put-away-folder)

(defstruct (macintosh-file-info
             (:conc-name file-))
  protected
  locked)

(defstruct (prodos-file-info
             (:conc-name file-))
  access
  type
  auxiliary-type)

(defstruct (msdos-file-info
             (:conc-name file-))
  msdos-attributes)

(defstruct (afp-file-info
             (:conc-name file-))
  backup-needed
  system
  multi-user
  invisible
  directory-id)

(defun get-ubyte (bytes offset)
  (aref bytes offset))

(defun get-ushort (bytes offset)
  (let ((hi (aref bytes offset))
        (lo (aref bytes (1+ offset))))
    (+ (ash hi 8) lo)))

(defun get-u3bytes (bytes offset)
  (let ((hi (aref bytes offset))
        (mi (aref bytes (+ 1 offset)))
        (lo (aref bytes (+ 2 offset))))
    (+ (ash (+ (ash hi 8) mi) 8) lo)))

(defun get-ulong (bytes offset)
  (let ((hi (get-ushort bytes offset))
        (lo (get-ushort bytes (+ offset 2))))
    (+ (ash hi 16) lo)))

(defun get-byte (bytes offset)
  (let ((ubyte (get-ubyte bytes offset)))
    (if (< ubyte #x80)
      ubyte
      (- ubyte #x100))))

(defun get-short (bytes offset)
  (let ((ushort (get-ushort bytes offset)))
    (if (< ushort #x8000)
      ushort
      (- ushort #x10000))))

(defun get-long (bytes offset)
  (let ((ulong (get-ulong bytes offset)))
    (if (< ulong #x80000000)
      ulong
      (- ulong #x100000000))))

(defun read-ushort (stream)
  (let ((hi (read-byte stream))
        (lo (read-byte stream)))
    (+ (ash hi 8) lo)))

(defun read-ulong (stream)
  (let ((hi (read-ushort stream))
        (lo (read-ushort stream)))
    (+ (ash hi 16) lo)))


(defgeneric decode-entry-data (kind data))



(defun check-ranges (header)
  (let ((file-set  (make-instance 'index-set))
        (entry-set (make-instance 'index-set)))
    (dolist (entry (header-entries header))
      (assign-empty entry-set)
      (include entry-set  (make-range :start (entry-offset entry)
                                      :count (entry-length entry)))
      (if (emptyp (intersection 'index-set file-set entry-set))
        (merge file-set entry-set)
        (report-collision header entry)))))

(defun report-collision (header  entry)
  (declare (ignore entry))
  ;; TODO:
  (error "Some entries collide in ~S." header))


(defun read-header (stream kind)
  (check-type stream file-stream)
  (assert (equal-type 'octet (stream-element-type stream)))
  (let ((magic (read-ulong stream))
        (expected-magic (ecase kind
                          (:apple-single +apple-single-magic-number+)
                          (:apple-double +apple-double-magic-number+))))
    (assert (= magic expected-magic))
    (let ((version (read-ulong stream)))
      (assert (= version +apple-format-version+)))
    (loop :repeat 16 :do (read-byte stream))
    (let* ((entry-count (read-ushort stream))
           (header (make-instance 'apple-file
                     :kind kind
                     :magic magic
                     :version +apple-format-version+
                     :entries (loop
                                :repeat entry-count
                                :collect (let* ((entry-id (read-ulong stream))
                                                (offset   (read-ulong stream))
                                                (length   (read-ulong stream))
                                                (kind     (symbolicate-entry-id entry-id))
                                                (entry    (make-entry
                                                           :kind kind
                                                           :id entry-id
                                                           :offset offset
                                                           :length length)))
                                           entry))
                     :info-stream stream)))
      (dolist (entry (header-entries header))
        (when (entry-kind entry)
          (setf (entry-decoded entry)
                (decode-entry-data (entry-kind entry) (read-entry-data stream entry)))))
      header)))


(defun read-entry-data (stream entry)
  (file-position stream (entry-offset entry))
  (let ((data (make-array (entry-length entry) :element-type 'octet)))
    (let ((read-size (read-sequence data stream)))
      (assert (= (entry-length entry) read-size)
              () "entry-length=~A read-size=~A"
              (entry-length entry) read-size)
      data)))



(defmethod decode-entry-data ((kind (eql :data-fork-id)) data)
  data)

(defmethod decode-entry-data ((kind (eql :resource-fork-id)) data)
  (warn "TBD decode-entry-data :resource-fork-id")
  data)

(defmethod decode-entry-data ((kind (eql :real-name)) data)
  (map 'string (function code-char) data))

(defmethod decode-entry-data ((kind (eql :short-name)) data)
  (map 'string (function code-char) data))

(defmethod decode-entry-data ((kind (eql :comment)) data)
  (map 'string (function code-char) data))

(defmethod decode-entry-data ((kind (eql :icon-b&w)) data)
  data)

(defmethod decode-entry-data ((kind (eql :icon-color)) data)
  data)



(defun to-lisp-date (macdate)
  (+ macdate (load-time-value (encode-universal-time 0 0 0 1 1 2000 0))))

(defun to-mac-date (universal-time)
  (- universal-time (load-time-value (encode-universal-time 0 0 0 1 1 2000 0))))


(defmethod decode-entry-data ((kind (eql :file-dates-info)) data)
  (make-file-dates-info
   :creation-date     (to-lisp-date (get-long data  0))
   :modification-date (to-lisp-date (get-long data  4))
   :backup-date       (to-lisp-date (get-long data  8))
   :access-date       (to-lisp-date (get-long data 12))))


(defun decode-fdflag (word)
  (append (loop :for (key flag) :in '((:is-alias 15)
                                      (:is-invisible 14)
                                      (:has-bundle 13)
                                      (:name-locked 12)
                                      (:is-stationery 11)
                                      (:has-custom-icon 10)
                                      (:has-been-inited 8)
                                      (:has-no-inits 7)
                                      (:is-shared 6))
            :when (logbitp flag word) :collect key
            :when (logbitp flag word) :collect t)
          (list :label (case (ldb (byte 3 1) word)
                         (0 nil)
                         (1 :red)
                         (2 :orange)
                         (3 :yellow)
                         (4 :green)
                         (5 :blue)
                         (6 :violet)
                         (7 :gray)))))


(defmethod decode-entry-data ((kind (eql :finder-info)) data)
  (let ((fdtype       (get-ulong  data  0))
        (fdcreator    (get-ulong  data  4))
        (fdflags      (decode-fdflag (get-ushort data  8)))
        (fdlocation.y (get-short  data 10))
        (fdlocation.x (get-short  data 12))
        (fdfolder     (get-short  data 14))
        (fdiconid     (get-short  data 16))
        (fdscript     (get-byte   data 24))
        (fdxflags     (get-byte   data 25))
        (fdcomment    (get-short  data 26))
        (fdputaway    (get-long   data 28)))
    (make-finder-info
     :type fdtype
     :creator fdcreator
     :flags fdflags
     :location.y fdlocation.y
     :location.x fdlocation.x
     :folder fdfolder
     :icon-id fdiconid
     :script fdscript
     :xflags fdxflags
     :comment-id fdcomment
     :put-away-folder fdputaway)))

(defmethod decode-entry-data ((kind (eql :macintosh-file-info)) data)
  (let ((flag (get-ulong data 0)))
    (make-macintosh-file-info
     :protected (logbitp 1 flag)
     :locked (logbitp 0 flag))))

(defmethod decode-entry-data ((kind (eql :prodos-file-info)) data)
  (make-prodos-file-info
   :access (get-ushort data 0)
   :type (get-ushort data 2)
   :auxiliary-type (get-ulong data 4)))

(defmethod decode-entry-data ((kind (eql :msdos-file-info)) data)
  (make-msdos-file-info
   :msdos-attributes (get-ushort data 0)))

(defmethod decode-entry-data ((kind (eql :afp-file-info)) data)
  (let ((flag (get-ulong data 0)))
    (make-afp-file-info
     :backup-needed (logbitp 6 flag)
     :system (logbitp 2 flag)
     :multi-user (logbitp 1 flag)
     :invisible (logbitp 0 flag))))

(defmethod decode-entry-data ((kind (eql :directory-id)) data)
  (get-ulong data 0))



;;----------------------------------------------------------------------
;; Resources
;;----------------------------------------------------------------------

(defenum system-resource-type-id
  DRVR-id
  WDEF-id
  MDEF-id
  CDEF-id
  PDEF-id
  PACK-id
  reserved-6-id
  reserved-7-id)

(defun make-owned-system-resource-id (resource-type-id owner-id sub-id)
  (dpb sub-id (byte 5 0)
       (dpb owner-id (byte 6 5)
            (dpb resource-type-id (byte 3 11)
                 #xc000))))

(defun owned-system-resource-id-p     (id) (= (ldb (byte 2 14) id) 3))
(defun owned-system-resource-type-id  (id) (ldb (byte 3 11) id))
(defun owned-system-resource-owner-id (id) (ldb (byte 6  5) id))
(defun owned-system-resource-sub-id   (id) (ldb (byte 5  0) id))

;; (owned-system-resource-id-p #xd182)
;; (owned-system-resource-sub-id #xd182)
;; (owned-system-resource-owner-id #xd182)
;; (owned-system-resource-type-id #xd182)


(defenum resource-attributes
  (res-sys-heap 64)
  (res-purgeable 32)
  (res-locked 16)
  (res-protected 8)
  (res-preload 4)
  (res-changed 2))

(defun decode-resource-attributes (attributes)
  (loop
    :for key :in '(:changed :preload :protected :locked :purgeable :system-heap)
    :for bit = 2 :then (* 2 bit)
    :unless (zerop (logand bit attributes))
    :collect key))

;; header 16 byte
;; resreved 112 bytes
;; application data 128 bytes
;; resource data
;; resource map


(defstruct resource-header
  data-offset
  map-offset
  data-length
  map-length
  resource)

(defun resource-header (resource-data)
  (make-resource-header
   :resource resource-data
   :data-offset (get-ulong resource-data 0)
   :map-offset (get-ulong resource-data 4)
   :data-length (get-ulong resource-data 8)
   :map-length (get-ulong resource-data 12)))

(defun resource-application-data (resource-header)
  (subseq (resource-header-resource resource-header) 128 256))

;; (defun resource-list (resource-header)
;;   (let* ((start    (resource-header-data-offset resource-header))
;;          (end      (+ start (resource-header-data-length resource-header)))
;;          (resource (resource-header-resource resource-header)))
;;     (loop
;;       :while (< start end)
;;       :for resource-length = (get-ulong resource start)
;;       :collect (subseq resource (+ 4 start) (+ 4 start resource-length)))))

(defstruct resource-map
  file-attributes
  type-list-offset ; offsets from start of resource-header
  name-list-offset)

(defstruct resource
  type
  id
  name
  attributes
  data)

(defun get-resource-name (resource name-list-offset offset)
  (when (/= -1 offset)
    (loop
      :with len = (aref resource (+ name-list-offset offset))
      :with name = (make-array len :element-type 'character)
      :for i :below len
      :for ch = (code-char (aref resource (+ name-list-offset offset i 1)))
      :do (setf (aref name i) ch)
      :finally (return name))))

(defun get-resource-data (resource start)
  (let ((len (get-ulong resource start)))
    (subseq resource (+ start 4)  (+ start 4 len))))

(defun resources (resource-header)
  (assert (<= 28 (resource-header-map-length resource-header)))
  (let* ((start            (resource-header-map-offset resource-header))
         (end              (+ start (resource-header-map-length resource-header)))
         (resource         (resource-header-resource resource-header))
         ;; (file-attributes  (get-ushort resource (+ start 22)))
         (type-list-offset (+ start (get-short resource (+ start 24))))
         (name-list-offset (+ start (get-short resource (+ start 26))))
         (data-offset      (resource-header-data-offset resource-header)))
    (loop
      :repeat (1+ (get-ushort resource type-list-offset))
      :for type-offset :from (+ type-list-offset 2) :by 8
      :while (< type-offset end)
      :collect (let ((resource-type   (get-ulong resource type-offset))
                     (resource-count  (1+ (get-ushort resource (+ type-offset 4))))
                     (resource-offset (+ type-list-offset (get-short resource (+ type-offset 6)))))
                 (loop
                   :repeat resource-count
                   :for reference-offset :from resource-offset :by 12
                   :collect (make-resource
                             :type resource-type
                             :id (get-ushort resource reference-offset)
                             :name (get-resource-name resource name-list-offset
                                                      (get-short resource (+ reference-offset 2)))
                             :attributes (decode-resource-attributes (aref resource (+ reference-offset 4)))
                             :data (get-resource-data resource (+ data-offset (get-u3bytes resource (+ reference-offset 5))))))))))



;; (defvar resource-file #(0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 30 84 104 105 115 32 114 101 115 111 117 114 99 101 32 102 111 114 107 32 105 110 116 101 110 116 105 111 110 97 108 108 121 32 108 101 102 116 32 98 108 97 110 107 32 32 32 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 30 0 0 0 0 0 0 0 0 0 28 0 30 255 255))
;; (resource-header resource-file)
;; (resource-list (resource-header resource-file))
;; (resources (resource-header resource-file))
;; (resources (resource-header (header-) resource-file))

;; (let ((resources (resources (resource-header (entry-decoded
;;                                               (find :resource-fork-id
;;                                                     (header-entries (with-open-file (stream (first (directory  #P"/home/pjb/works/patchwork/examples/B/._*.*"))
;;                                                                                             :element-type 'octet)
;;                                                                       (read-header stream :apple-double)))
;; 
;;                                                     :key (function entry-kind)))))))
;;   (mapcar (lambda (resources)
;;               (mapcar (lambda (resource)
;;                           (format-signature (resource-type resource)))
;;                resources))
;;           resources))


;; (("FRED" "FRED") ("MPSR"))
;; ((#S(resource :type 1179796804 :id 2 :name nil :attributes nil :data #(0 1 0 4 9 0 1 0 0 0))
;;     #S(resource :type 1179796804 :id 3 :name nil :attributes nil :data #(0 0 6 77 111 110 97 99 111)))
;;  (#S(resource :type 1297109842 :id 1005 :name nil :attributes nil :data #(0 9 77 111 110 97 99 111 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6 0 8 0 44 0 6 1 240 2 168 0 44 0 6 1 240 2 168 0 0 0 0 0 0 0 188 0 0 0 188 0 0 0 0 1 0))))
;; 
;;  (map 'string 'code-char #(0 0 6 77 111 110 97 99 111))"  Monaco"

;;----------------------------------------------------------------------
;; APPLE-FILE
;;----------------------------------------------------------------------




(define-condition file-type-error (simple-error)
  ())






(defgeneric apple-file-fork-pathname (path format fork)
  (:documentation "
RETURN: the pathname of the specified FORK of the file at PATH, assuming a file in FORMAT.
PATH:   A pathname designator.
FORMAT: (member :apple-single :apple-double :apple-triple)
FORK:   (member :info :data :resource)
")
  (:method ((apple-file apple-file) format fork)
    (apple-file-fork-pathname (apple-file-info-stream apple-file) format fork))
  (:method ((stream file-stream) format fork)
    (apple-file-fork-pathname (pathname stream) format fork))
  (:method ((path string) format fork)
    (apple-file-fork-pathname (pathname path) format fork))
  (:method ((info-path pathname) (format (eql :apple-single)) fork)
    (declare (ignore fork))
    info-path)
  (:method ((info-path pathname) (format (eql :apple-double)) fork)
    (let ((name (pathname-name info-path)))
      (ecase fork
        ((:info :resource)
         (if (string= "._" name :end2 (min 2 (length name)))
           info-path
           (make-pathname :name (format nil "._~A" name) :defaults info-path)))
        ((:data)
         (if (string= "._" name :end2 (min 2 (length name)))
           (make-pathname :name (subseq name 2) :defaults info-path)
           info-path)))))
  (:method ((info-path pathname) (format (eql :apple-triple)) fork)
    (make-pathname :type (ecase fork
                           ((:info) "info")
                           ((:data) "data")
                           ((:resource) "rsrc"))
                   :case :local
                   :defaults info-path)))


(defun tree-structure-and-leaf-difference (a b &key (test (function eql)))
  (cond
    ((and (null a) (null b)) '=)
    ((or (null a) (null b)) `(/= ,a ,b))
    ((and (atom a) (atom b))
     (if (funcall test a b)
         '=
         `(/= ,a ,b)))
    ((or (atom a) (atom b)) `(/= ,a ,b))
    (t (cons (tree-structure-and-leaf-difference (car a) (car b) :test test)
             (tree-structure-and-leaf-difference (cdr a) (cdr b) :test test)))))

(defun test/apple-file-fork-pathname ()
  #+unix
  (let ((*default-pathname-defaults* #P"/"))
    (assert
     (tree-structure-and-leaf-difference 
      (mapcar (lambda (format)
                (mapcar (lambda (fork)
                          (apple-file-fork-pathname (make-pathname :name "test" :type "single" :case :local)
                                                    format fork))
                        '(:info :data :resource)))
              '(:apple-single :apple-double :apple-triple))
      (list (list (make-pathname :name "test" :type "single" :case :local)
                  (make-pathname :name "test" :type "single" :case :local)
                  (make-pathname :name "test" :type "single" :case :local))
            (list (make-pathname :name "._test" :type "single" :case :local)
                  (make-pathname :name "test" :type "single" :case :local)
                  (make-pathname :name "._test" :type "single" :case :local))
            (list (make-pathname :name "test" :type "info" :case :local)
                  (make-pathname :name "test" :type "data" :case :local)
                  (make-pathname :name "test" :type "rsrc" :case :local)))
      :test 'pathname-match-p)))
  :success)




;; (open-apple-file pathname) --> apple-file
;; (close-apple-file apple-file)
;; (apple-file-data-fork-stream apple-file :direction :external-format :element-type :if-does-not-exist :if-exists) --> stream
;; (apple-file-resource-fork-stream apple-file :direction :if-does-not-exist :if-exists) --> stream
;; (apple-file-resources apple-file) --> resources


(defun open-apple-file (pathname &key (direction :input) (if-does-not-exist :error))
  (assert (eq direction :input) () "non :input direction not supported yet.")
  (flet ((get-header (info-path format)
           (let* ((stream (open info-path
                                :direction :input
                                :if-does-not-exist nil
                                :element-type 'octet))
                  (header (when stream
                            (file-position stream 0)
                            (ignore-errors (read-header stream format)))))
             (when header
               (setf (header-info-stream header) stream))
             header)))
    (let ((header (or (get-header (apple-file-fork-pathname pathname :apple-single :info) :apple-single)
                      (get-header (apple-file-fork-pathname pathname :apple-double :info) :apple-double)
                      (get-header (apple-file-fork-pathname pathname :apple-triple :info) :apple-triple))))
      (if header
        (make-instance 'apple-file
          :header header
          :info-stream (header-info-stream header)
          :direction direction)
        (case if-does-not-exist
          (:error (error 'file-error :pathname pathname))
          (otherwise if-does-not-exist))))))


(defgeneric close-apple-file (apple-file)
  (:method ((apple-file apple-file))
    (close (header-info-stream  apple-file))))

(defun apple-file-data-fork (apple-file
                             &key (direction :input)
                               (external-format :default)
                               (element-type 'character)
                               (if-does-not-exist :error)
                               if-exists)
  (let ((data-path (apple-file-fork-pathname apple-file (apple-file-header-kind apple-file) :data))
        (info-path (apple-file-fork-pathname apple-file (apple-file-header-kind apple-file) :info)))
    (if (equalp data-path info-path)
        (error "~S not implemented for apple-triple files yet." 'apple-file-data-fork)
        (open data-path
              :direction direction
              :external-format external-format
              :element-type element-type
              :if-does-not-exist if-does-not-exist
              :if-exists if-exists))))

(defun apple-file-resource-fork (apple-file)
  (declare (ignore apple-file))
  (error "~S not implemented yet" 'apple-file-resource-fork))



;;----------------------------------------------------------------------
;; APPLE-FILE attributes
;;----------------------------------------------------------------------

(defmacro define-attribute (name entry-key docstring &rest readers)
  (labels ((wrap-readers (readers form)
             (if (null readers)
                 form
                 (wrap-readers (rest readers) `(,(first readers) ,form)))))
    `(defun ,name (apple-file)
       ,docstring
       (let ((entry (find ,entry-key (header-entries apple-file) :key (function entry-kind))))
         (when entry ,(wrap-readers readers '(entry-decoded entry)))))))


(defun format-signature (signature)
  (format nil "~C~C~C~C"
          (code-char (ldb (byte 8 24) signature))
          (code-char (ldb (byte 8 16) signature))
          (code-char (ldb (byte 8  8) signature))
          (code-char (ldb (byte 8  0) signature))))
(defun and-format-signature (creator) (values creator (format-signature creator)))
(defun location-x-y (finfo) (cons (finder-info-location.x finfo) (finder-info-location.y finfo)))

(define-attribute apple-file-real-name              :real-name           "RETURN: NIL or the real name string in the APPLE-FILE.")                                                                  
(define-attribute apple-file-comment                :comment             "RETURN: NIL or the comment string in the APPLE-FILE.")                                                                   
(define-attribute apple-file-icon-b&w               :icon-b&w            "RETURN: NIL or the black & white icon data (byte vector) in the APPLE-FILE.")                                            
(define-attribute apple-file-icon-color             :icon-color          "RETURN: NIL or the color icon data (byte vector) in the APPLE-FILE.")                                                    
(define-attribute apple-file-creation-date          :file-dates-info     "RETURN: NIL or the creation date (lisp universal-time) in the APPLE-FILE."     file-creation-date)                       
(define-attribute apple-file-modification-date      :file-dates-info     "RETURN: NIL or the modification date (lisp universal-time) in the APPLE-FILE." file-modification-date)                   
(define-attribute apple-file-backup-date            :file-dates-info     "RETURN: NIL or the backup date (lisp universal-time) in the APPLE-FILE."       file-backup-date)                         
(define-attribute apple-file-access-date            :file-dates-info     "RETURN: NIL or the access date (lisp universal-time) in the APPLE-FILE."       file-access-date)                         
(define-attribute apple-file-finder-creator         :finder-info         "RETURN: NIL or the creator (as integer and as string) of the APPLE-FILE."      finder-info-creator and-format-signature) 
(define-attribute apple-file-finder-type            :finder-info         "RETURN: NIL or the type (as integer and as string) of the APPLE-FILE."         finder-info-type    and-format-signature) 
(define-attribute apple-file-finder-location        :finder-info         "RETURN: NIL or the X, Y coordinates of the icon of the APPLE-FILE."            location-x-y)                         
(define-attribute apple-file-finder-icon-id         :finder-info         "RETURN: NIL or the icon ID of the APPLE-FILE."                                 finder-info-icon-id)                      
(define-attribute apple-file-finder-folder          :finder-info         "RETURN: NIL or the folder ID of the APPLE-FILE."                               finder-info-folder)                       
(define-attribute apple-file-finder-put-away-folder :finder-info         "RETURN: NIL or the put away folder ID of the APPLE-FILE."                      finder-info-put-away-folder)              
(define-attribute apple-file-macintosh-protected    :macintosh-file-info "RETURN: NIL or the protected flag of the APPLE-FILE."                          file-protected)
(define-attribute apple-file-macintosh-locked       :macintosh-file-info "RETURN: NIL or the locked flag of the APPLE-FILE."                             file-locked)
(define-attribute apple-file-prodos-access          :prodos-file-info    "RETURN: NIL or the PRODOS access code of the APPLE-FILE."                      file-access)
(define-attribute apple-file-prodos-type            :prodos-file-info    "RETURN: NIL or the PRODOS type code of the APPLE-FILE."                        file-type)
(define-attribute apple-file-prodos-auxiliary-type  :prodos-file-info    "RETURN: NIL or the PRODOS auxiliary type code of the APPLE-FILE."              file-auxiliary-type)
(define-attribute apple-file-msdos-attributes       :msdos-file-info     "RETURN: NIL or the MSDOS attributes of the APPLE-FILE."                        file-msdos-attributes)
(define-attribute apple-file-afp-backup-needed      :afp-file-info       "RETURN: NIL or the AFP backup needed flag of the APPLE-FILE."                  file-backup-needed)
(define-attribute apple-file-afp-system             :afp-file-info       "RETURN: NIL or the AFP system flag of the APPLE-FILE."                         file-system)
(define-attribute apple-file-afp-multi-user         :afp-file-info       "RETURN: NIL or the AFP multi-user flag of the APPLE-FILE."                     file-multi-user)
(define-attribute apple-file-afp-invisible          :afp-file-info       "RETURN: NIL or the AFP invisible flag of the APPLE-FILE."                      file-invisible)
(define-attribute apple-file-afp-directory-id       :afp-file-info       "RETURN: NIL or the AFP directory ID of the APPLE-FILE."                        file-directory-id)



;;----------------------------------------------------------------------
;;
;;----------------------------------------------------------------------


(defun lsattr (path)
  (format t "~A attributes:~%" path)
  (with-open-file (stream path :element-type 'octet)
    (let ((header (read-header stream :apple-double)))
      (dolist (attribute '(apple-file-real-name              
                           apple-file-comment                
                           apple-file-icon-b&w               
                           apple-file-icon-color             
                           apple-file-creation-date          
                           apple-file-modification-date      
                           apple-file-backup-date            
                           apple-file-access-date            
                           apple-file-finder-creator         
                           apple-file-finder-type            
                           apple-file-finder-location        
                           apple-file-finder-icon-id         
                           apple-file-finder-folder          
                           apple-file-finder-put-away-folder 
                           apple-file-macintosh-protected    
                           apple-file-macintosh-locked       
                           apple-file-prodos-access          
                           apple-file-prodos-type            
                           apple-file-prodos-auxiliary-type  
                           apple-file-msdos-attributes       
                           apple-file-afp-backup-needed      
                           apple-file-afp-system             
                           apple-file-afp-multi-user         
                           apple-file-afp-invisible          
                           apple-file-afp-directory-id))
        (let ((name   (subseq (string-downcase attribute) #.(length "apple-file-")))
              (values (multiple-value-list (funcall attribute header))))
          (when (first values)
            (format t "~30A ~{~A~^ ~}~%" name values))))
      (let ((resources (resources (resource-header (entry-decoded (find :resource-fork-id (header-entries header) :key (function entry-kind)))))))
        (dolist (resources resources)
          (format t "Resource type: ~A" (format-signature (resource-type (first resources))))
          (if (string= "STR " (format-signature (resource-type (first resources))))
  
            (dolist (resource resources)
              (format t "~&    ~A[~A]~@[(~A)~]: ~S~%" (resource-id resource) (length (resource-data resource)) (resource-name resource)
                      (map 'string 'code-char (subseq (resource-data resource) 1))))          
            (dolist (resource resources)
              (format t " ~A[~A]~@[(~A)~]" (resource-id resource) (length (resource-data resource)) (resource-name resource))))
          (terpri)))))
  (values))


;; (dolist (path  (directory  #P"/home/pjb/works/patchwork/examples/B/._*.*"))
;;  (lsattr path)
;;  (terpri))


;; (defun make-apple-double-pathname (pathname)
;;   (apple-file-fork-pathname pathname :apple-double :info))
;; (make-apple-double-pathname "toto")
;;
;; (first (directory  #P"/home/pjb/works/patchwork/examples/B/._*.*"))
;; #P"/home/pjb/works/patchwork/examples/B/\\._ ''PW-functionals.lib copie"
;; ;; (remove-if-not (function probe-file)
;; ;;                (directory  #P"/home/pjb/works/patchwork/examples/B/*.*")
;; ;;                :key (function make-apple-double-pathname))
;; (with-open-file (stream #P"/home/pjb/works/patchwork/examples/B/._%deÌsordre"
;;                         :element-type 'octet)
;;   (let ((header (read-header stream :apple-double)))
;;     (dolist (entry (header-entries header))
;;       (print (entry-kind entry))
;;       (print (read-entry-data stream entry)))))
;; 
;; (with-open-file (stream #P"/home/pjb/works/patchwork/examples/B/._%deÌsordre"
;;                         :element-type 'octet)
;;    (read-header stream :apple-double))
;; 
;; ;; (format-signature 1413830740)
;; ;; "TEXT"
;; ;; (format-signature 1128483890)
;; ;; "CCL2"
;; 
;; 
;; #S(header :kind :apple-double :magic 333319 :version 131072
;;           :entries (#S(entry :kind :finder-info :id 9 :offset 50 :length 3760
;;                              :decoded #S(finder-info :type 1413830740 :creator 1128483890 :flags (:has-been-inited t :label nil) :location.y -1 :location.x -1 :folder 0 :icon-id 0 :script 0 :xflags 0 :comment-id 0 :put-away-folder 23779))
;;                       
;;                       #S(entry :kind :resource-fork-id :id 2 :offset 3810 :length 286
;;                                :decoded #(0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 30 84 104 105 115 32 114 101 115 111 117 114 99 101 32 102 111 114 107 32 105 110 116 101 110 116 105 111 110 97 108 108 121 32 108 101 102 116 32 98 108 97 110 107 32 32 32 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 30 0 0 0 0 0 0 0 0 0 28 0 30 255 255))))

(defun test/all ()
 (test/apple-file-fork-pathname))

(test/all)


;;;; THE END ;;;;
