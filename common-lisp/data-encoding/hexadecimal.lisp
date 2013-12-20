;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               hexadecimal.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Encode and decode hexadecimal strings.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-10-06 <PJB> Created.
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.HEXADECIMAL"
  (:use "COMMON-LISP")
  (:export "BYTES-TO-HEXADECIMAL-STRING" "BYTES-FROM-HEXADECIMAL-STRING")
  (:documentation
   "

This package exports functions to encode and decode byte vector buffer
into hexadecimal strings.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2013 - 2013
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.HEXADECIMAL")


(defun decode-byte (digit-per-byte byte-sex sex-width base input)
  (let ((buffer (make-string digit-per-byte)))
    (ecase byte-sex
      (:big-endian
       (loop
         :for i :below digit-per-byte
         :do (setf (aref buffer i) (read-char input))))
      (:little-endian

       ;; ddccbba --> abbccdd
       (loop
         :with s = 0
         :with i = (- digit-per-byte sex-width)
         :repeat digit-per-byte
         :do
         (setf (aref buffer i) (read-char input))
         (incf i)
         (incf s)
         (when (<= sex-width s)
           (setf s 0)
           (decf i (* 2 sex-width))
           (when (minusp i)
             (setf i 0))))))
    (parse-integer buffer :radix base :junk-allowed nil)))

(assert (= (with-input-from-string (input "0123456789abcdef")
             (decode-byte 4 :big-endian 2 16 input))
           #x0123))

(assert (= (with-input-from-string (input "0123456789abcdef")
             (decode-byte 4 :little-endian 2 16 input))
           #x2301))


(defun decode (string element-type digit-per-byte byte-sex sex-width base)
  (let ((bytes (make-array (ceiling (length string) digit-per-byte)
                           :element-type (or element-type
                                             (list 'unsigned-byte
                                                   (integer-length (1- (expt base digit-per-byte))))))))
    (with-input-from-string (input string)
      (loop
        :for i :below (length bytes)
        :do (setf (aref bytes i) (decode-byte digit-per-byte byte-sex sex-width base input))))
    bytes))

(assert (string=
         (map 'string 'code-char (decode "6b7569706572"
                                         '(unsigned-byte 8)
                                         2 :big-endian 1 16))
         "kuiper"))
(assert (string=
         (map 'string 'code-char (decode "4d49542d4d414749432d434f4f4b49452d31"
                                         '(unsigned-byte 8)
                                         2 :big-endian 1 16))
         "MIT-MAGIC-COOKIE-1"))






(defun encode-byte (byte padding byte-sex sex-width base case output)
  (let ((digits (format nil (if (eq case :downcase)
                              "~(~V,V,'0R~)"
                              "~:@(~V,V,'0R~)") base padding byte)))
    (ecase byte-sex
      (:big-endian
       (princ digits output))
      (:little-endian
       (loop
         :for end :downfrom (length digits) :by sex-width
         :for start = (max 0 (- end sex-width))
         :while (plusp end)
         :do (princ (subseq digits start end) output))))))

(assert (string= (with-output-to-string (output)
                   (encode-byte #xbabeface00 10 :little-endian 4 16 :upcase output))
                 "CE00BEFABA"))


(defun encode (byte-vector padding byte-sex sex-width base case)
  (with-output-to-string (output)
    (loop
      :with byte-type = (array-element-type byte-vector)
      :for byte :across byte-vector
      :do (encode-byte byte padding byte-sex sex-width base case output))))


(assert (string= (encode #(#xba #xbe #xfa #xce) 2 :little-endian 1 16 :downcase)
          "abebafec"))
(assert (string= (encode #(#xbabeface #xb19b00b5 #xdeadface) 12 :little-endian 2 16 :downcase)
                 "cefabeba0000b5009bb10000cefaadde0000"))


(defun validate-integer (object)
  (check-type object integer)
  object)

(defun parse-element-type (element-type)
  (when (atom element-type)
    (error "Expected an element-type specifying the byte size, not ~S." element-type))
  (case (first element-type)
    ((unsigned-byte)
     (values (validate-integer (second element-type))
             nil))
    ((signed-byte
      (values (validate-integer (second element-type))
              t)))
    ((integer)
     (let ((lower (validate-integer (second element-type)))
           (upper (validate-integer (third  element-type))))
       (assert (<= lower upper) () "Invalid integer type ~S" element-type)
       (if (minusp lower)
         (values (1+ (integer-length (if (minusp upper)
                                       (1- (abs lower))
                                       (max (1- (abs lower)) upper))))
                 t)
         (values (integer-length (max (abs lower) (abs upper)))
                 nil))))
    (otherwise
     (error "Expected an UNSIGNED-BYTE, SIGNED-BYTE or INTEGER element-type specifying the byte size, not ~S." element-type))))


;; (parse-element-type '(integer 0 15))
;; (parse-element-type '(integer -1 9))



(defun bytes-to-hexadecimal-string (byte-vector &key (element-type nil) (padding nil) (byte-sex :big-endian) (case :downcase))
  "

ELEMENT-TYPE: The element-type of the BYTE-VECTOR (if NIL, then
              (ARRAY-ELEMENT-TYPE BYTE-VECTOR) is used.

PADDING:      When NIL, the number of hexadecimal digits per byte is
              the minimum required (a vector of (unsigned-byte 3)
              would use one hexadecimal digit per element).  Otherwise
              it's at least PADDING.

BYTE-SEX:     When more than one octet are needed to store the bytes,
              they're ordered according to the byte-sex:

                 :big-endian    most significant octets first.
                 :little-endian least significant octets first.

              Notice that the quads in the octets are always stored first.

RETURN:       A string containing the hexadecimal digits representing the vector.

"
  (multiple-value-bind (bits signed)
      (parse-element-type (or element-type (array-element-type byte-vector)))
    (when signed (error "Not implemented yet"))
    (encode byte-vector (or padding (ceiling bits 4)) byte-sex 2. 16. case)))


#-(and)
(bytes-to-hexadecimal-string (coerce (loop for i from 0 to 255 collect i) 'vector)
                             :element-type '(unsigned-byte 8)
                             :padding 4
                             :byte-sex :little-endian
                             :case :upcase)







(defun bytes-from-hexadecimal-string (string &key (element-type nil) (padding nil) (byte-sex :big-endian) (case :downcase))
  "

ELEMENT-TYPE: The element-type of the BYTE-VECTOR (if NIL, then
              (unsigned-byte 8) is used).

PADDING:      When NIL, the number of hexadecimal digits per byte is
              the minimum required (a vector of (unsigned-byte 3)
              would use one hexadecimal digit per element).  Otherwise
              it's at least PADDING.

BYTE-SEX:     When more than one octet are needed to store the bytes,
              they're ordered according to the byte-sex:

                 :big-endian    most significant octets first.
                 :little-endian least significant octets first.

              Notice that the quads in the octets are always stored first.

RETURN:       A string containing the hexadecimal digits representing the vector.

"
  (multiple-value-bind (bits signed) (parse-element-type (or element-type '(unsigned-byte 8)))
    (when signed (error "Not implemented yet"))
    (decode string
            (or element-type '(unsigned-byte 8))
            (or padding (ceiling bits 4))
            byte-sex 2. 16.)))


(assert (equalp
         (bytes-from-hexadecimal-string "4d49542d4d414749432d434f4f4b49452d31")
         #(77 73 84 45 77 65 71 73 67 45 67 79 79 75 73 69 45 49)))

;;;; THE END ;;;;
