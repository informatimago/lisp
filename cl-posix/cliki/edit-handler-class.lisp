(in-package :cliki)

(defclass edit-handler (handler)
  ((cliki :accessor handler-cliki :initarg :cliki)))

