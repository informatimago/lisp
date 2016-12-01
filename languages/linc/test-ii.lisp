
(defclass a ()
  ())

(defclass b ()
  ((b :initarg :b :accessor b)))

;; (defmethod shared-initialize :before
;;     ((self b) slot-names &rest initargs &key &allow-other-keys)
;;
;;   (print `(= (slot-boundp ,self 'b) , (slot-boundp self 'b)))
;;   (print `(shared-initialize :before ((,self b) ,slot-names &rest ,initargs &key &allow-other-keys)))
;;   (print `(=  (next-method-p) ,(next-method-p)))
;;   (when (next-method-p)
;;     (apply (function call-next-method) self slot-names
;;            :b 2 (remf initargs :b))))

(defmethod initialize-instance :before ())

(inspect  (make-instance 'b :b 1))
(inspect *)
