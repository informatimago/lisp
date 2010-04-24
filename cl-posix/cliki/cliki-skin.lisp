(in-package :cliki)

(defclass cliki-skin (cliki-view)
  ((real-cliki :initarg :real-cliki :reader skin-real-cliki)))

;;; we don't provide cliki-data-directory, as it's only used by save-pages,
;;; so we've delegated that at a higher level

(defmacro defdelegate (form name ((instance class) &rest args))
  `(,form ,name ((,instance ,class) ,@args)
	  (,name (skin-real-cliki ,instance) ,@args)))

;; note that this means
;; (not (eq (page-cliki (find-page skin "index")) skin))
(defdelegate defmethod cliki-pages ((cliki cliki-skin)))

(defdelegate defmethod cliki-recent-changes ((cliki cliki-skin)))
(defdelegate defmethod cliki-idf ((cliki cliki-skin) term))
(defdelegate defmethod cliki-short-forms ((cliki cliki-skin) ))

(defmethod save-page ((cliki cliki-skin) request &optional title)
  (save-page (skin-real-cliki cliki) request title))