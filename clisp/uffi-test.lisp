;;;; -*- coding:utf-8 -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi:def-call-out string-in
    (:name "uffi_test_string_in")
  (:arguments (value ffi:c-string))
  (:return-type nil)
  (:language :stdc)
  (:library "/home/pascal/src/common/clisp/uffi-test.so"))


(ffi:def-call-out string-out
    (:name "uffi_test_string_out")
  (:arguments
   (value      ffi:c-pointer :in)
   (max-length ffi:uint32    :in))
  (:return-type nil)
  (:language :stdc)
  (:library "/home/pascal/src/common/clisp/uffi-test.so"))


(defun foreign-string-length (foreign-string)
  (do ((len 0 (1+ len)))
      ((= 0 (ffi:element (ffi:foreign-value foreign-string) len))
       len))) ;;foreign-string-length


(defun convert-from-foreign-string (foreign-string
                                    &key length (null-terminated-p t))
  "
DO:                 Builds a Lisp string from a foreign string.
                    Can translate ASCII and binary strings.
FOREIGN-STRING:     A foreign string.
LENGTH:             The length of the foreign string to convert.
                    The default is the length of the string until
                    a NULL character is reached.
NULL-TERMINATED-P:  A boolean flag with a default value of T.
                    When true, the string is converted until the first
                    NULL character is reached.
RETURN:             A Lisp string.
URL:        <http://uffi.b9.com/manual/convert-from-foreign-string.html>
URL:        <http://clisp.sourceforge.net/impnotes.html#encoding>
"
  (let ((byte-vector (make-array (list (if (or null-terminated-p (null length))
                                           (foreign-string-length foreign-string)
                                           length))
                                 :element-type '(unsigned-byte 8)))
        (foreign-type `(ffi:c-array ffi:uchar ,(list length))))
    (dotimes (i (length byte-vector))
      (setf (aref byte-vector i)
            (ffi:element (ffi:foreign-value foreign-string) i)))
    (ext:convert-string-from-bytes byte-vector custom:*foreign-encoding*)
    )) ;;CONVERT-FROM-FOREIGN-STRING


(defun h-string-out (max-length)
  (ffi:with-foreign-object
      (result `(ffi:c-array ffi:uchar ,(1+ max-length)))
    (string-out result max-length)
    (convert-from-foreign-string result))) ;;h-string-out


(ffi:def-call-out string-result
    (:name "uffi_test_string_result")
  (:return-type ffi:c-string)
  (:language :stdc)
  (:library "/home/pascal/src/common/clisp/uffi-test.so"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi:def-call-out sint32-in
    (:name "uffi_test_sint32_in")
  (:arguments (value ffi:sint32))
  (:return-type nil)
  (:language :stdc)
  (:library "/home/pascal/src/common/clisp/uffi-test.so"))


(ffi:def-call-out sint32-out
    (:name "uffi_test_sint32_out")
  (:arguments (value (ffi:c-ptr ffi:sint32) :out))
  (:return-type nil)
  (:language :stdc)
  (:library "/home/pascal/src/common/clisp/uffi-test.so"))


(ffi:def-call-out sint32-result
    (:name "uffi_test_sint32_result")
  (:return-type ffi:sint32)
  (:language :stdc)
  (:library "/home/pascal/src/common/clisp/uffi-test.so"))


uffi                 ffi-arg                      ffi-ret
==================== ============================ ============================
:sint32              ffi:sint32                   ffi:sint32
(* :sint32)          (ffi:c-ptr ffi:sint32)       (ffi:c-ptr ffi:sint32)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi:def-c-struct record-t
  (character     ffi:character)
  (number        ffi:int)
  (single-float  ffi:single-float)
  (small-string  (ffi:c-array ffi:character 16))
  (big-string    ffi:c-string))

;;(ffi:c-array-ptr ffi:character) --> (simple-vector size)

(ffi:def-call-out record-in
    (:name "uffi_test_record_in")
  (:arguments (value (ffi:c-ptr record-t)))
  (:return-type nil)
  (:language :stdc)
  (:library "/home/pascal/src/common/clisp/uffi-test.so"))


(ffi:def-call-out record-out
    (:name "uffi_test_record_out")
  (:arguments (value ffi:c-pointer))
  (:return-type nil)
  (:language :stdc)
  (:library "/home/pascal/src/common/clisp/uffi-test.so"))


(defun h-record-out ()
  (ffi:with-foreign-object (result 'record-t)
    (record-out (ffi:foreign-address result))
    (make-record-t
     :character (ffi:slot (ffi:foreign-value result) 'character)
     :number (ffi:slot (ffi:foreign-value result) 'number)
     :single-float (ffi:slot (ffi:foreign-value result) 'single-float)
     :small-string (ffi:slot (ffi:foreign-value result) 'small-string)
     :big-string (ffi:slot (ffi:foreign-value result) 'big-string)))) ;;h-record-out


(ffi:def-call-out record-result
    (:name "uffi_test_record_result")
  (:return-type (ffi:c-ptr record-t))
  (:language :stdc)
  (:library "/home/pascal/src/common/clisp/uffi-test.so"))


;;;; THE END ;;;;
