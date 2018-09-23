(defpackage "COM.INFORMATIMAGO.CLEXT.PKCS11.CFFI-DEBUG"
  (:use "COMMON-LISP" "CFFI")
  (:shadow "FOREIGN-ALLOC" "FOREIGN-FREE")
  (:export "FOREIGN-ALLOC" "FOREIGN-FREE"
           "*TRACE*"))
(in-package "COM.INFORMATIMAGO.CLEXT.PKCS11.CFFI-DEBUG")

(defparameter *allocated* (make-hash-table))
(defparameter *freed*     (make-hash-table))
(defvar *trace* nil)

(defun foreign-alloc (type
                      &rest keyargs
                      &key (initial-element nil) (initial-contents nil)
                        (count 1) null-terminated-p)
  (declare (ignorable initial-element initial-contents null-terminated-p))
  (let ((ptr  (apply (function cffi:foreign-alloc) type keyargs))
        (size (* (foreign-type-size type) count)))
    (setf (gethash (pointer-address ptr) *allocated*) size)
    (when *trace*
      (format *trace-output* "~&(foreign-alloc ~S ~{~S~^ ~}) -> ~S~%"
              type keyargs ptr))
    ptr))


(defun foreign-free (ptr)
  (when *trace* (format *trace-output* "~&(foreign-free ~S)~%" ptr))
  (let* ((address  (pointer-address ptr))
         (size     (gethash address *allocated*)))
    (if size
        (progn
          (setf (gethash address *freed*) size)
          (remhash address *allocated*)
          (cffi:foreign-free ptr))
        (let ((size (gethash address *freed*)))
          (if size
              (warn "Double free of ~S (size = ~S)" ptr size)
              (progn
                (warn "Freeing unallocated pointer ~S" ptr)
                (cffi:foreign-free ptr)))))))

;;;; THE END ;;;;
