(in-package :cliki)

(defclass cliki-view (araneida:dispatching-handler)
  ((title :reader cliki-title :initarg :title :initform "CLiki")
   ;; XXX is this still needed?
   (url-root :accessor cliki-url-root :initarg :url-root)))

  
(defclass cliki-instance (cliki-view)
  ;; caller should initialize data-directory, title, url-root; the
  ;; rest will be sorted out internally
  ((data-directory :accessor cliki-data-directory :initarg :data-directory)
   (recent-changes :accessor cliki-recent-changes :initform (list))
   (idf)
   (pages :accessor cliki-pages :initform (make-hash-table :test 'equal))
   (short-forms :accessor cliki-short-forms
		:initarg :short-forms
		:initform '((#\_ :link)
			    (#\* :topic)
			    (#\/ :legacy-search)
			    (#\# ((#\H :clhs)))			    
			    (#\: :long-form)))))


