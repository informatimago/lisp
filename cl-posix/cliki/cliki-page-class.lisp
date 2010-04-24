(in-package :cliki)

;;; XXX should initialize some of these to e.g. (list) or (make-hash-table)


(defclass cliki-page ()
  ((title :accessor page-title :initarg :title)
   (names :accessor page-names :initarg :names)
   (cliki :accessor page-cliki :initarg :cliki)
   (versions :accessor page-versions :initarg :versions)
   (indices :accessor page-indices :initarg :indices :initform nil)
   (first-sentence :accessor page-first-sentence :initarg :sentence 
		   :initform nil)
   (pathname :initarg :pathname)	;note no accessor
   (backlinks :accessor page-backlinks :initform nil)
   (topics :accessor page-topics :initform nil)
   (last-modified :initform (get-universal-time))
   ))


