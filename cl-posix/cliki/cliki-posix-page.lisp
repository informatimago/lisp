(in-package :cliki)

(defclass cliki-posix-page (cliki-page)
  ((posix-type  :accessor page-posix-type :initarg :posix-type
                :type (member :function :header))
   (posix-name  :accessor page-posix-name :initarg :posix-name
                :type string)))


