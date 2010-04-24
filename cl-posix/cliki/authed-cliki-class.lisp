(in-package :cliki)
(defclass authed-cliki-handler (cliki-instance)
  ((users :initform nil :accessor cliki-users)))
(defclass authed-cliki-edit-handler (edit-handler) ())
