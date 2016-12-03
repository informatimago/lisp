;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bencode.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-12-08 <PJB> Created, inspired from Alex Schroeder's emacs lisp 'bencode.el'.
;;;;BUGS
;;;;    We don't deal with encodings, relying on CHAR-CODE and CODE-CHAR.
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.BENCODE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (:export "BENCODE-TO-STRING"           "BDECODE-FROM-STRING"
           "BENCODE-TO-CHARACTER-STREAM" "BDECODE-FROM-CHARACTER-STREAM"
           "BENCODE-TO-BINARY-STREAM"    "BDECODE-FROM-BINARY-STREAM"
           "*KEY-MAP-EXCEPTIONS*" "*TORRENT-KEY-MAP-EXCEPTIONS*")
  (:documentation "

Bencoding is a way to encode integers, strings, lists, and hash-tables
as strings (serialization), and bdecoding does the reverse operation.
It is part of the torrent metafile specification at
http://bittorrent.org/beps/bep_0003.html

Notice that torrent files are binary files, since they contain
strings with octets instead of characters.


Data encoded and decoded
------------------------


+----------------------+----------------------+----------------------+
| Lisp type            | BEncoded type        | Lisp type            |
+----------------------+----------------------+----------------------+
| integer              | integer (i)          | integer              |
+----------------------+----------------------+----------------------+
| octets               | string  (:)          | string or octets¹    |
+----------------------+----------------------+----------------------+
| string               | string  (:)          | string or octets¹    |
+----------------------+----------------------+----------------------+
| symbol               | string  (:)          | string²              |
+----------------------+----------------------+----------------------+
| character            | string  (:)          | string               |
+----------------------+----------------------+----------------------+
| hash-table³          | dict    (d)          | hash-table           |
+----------------------+----------------------+----------------------+
| list                 | list    (l)          | list                 |
+----------------------+----------------------+----------------------+


¹: OCTETS are a vector of (unsigned-byte 8), if a string contains only
   the standard characters, then it's read as a string, otherwise as an
   OCTETS.

²: in a dict, key strings can be converted into symbols.

³: all keys must be string designators.



Key map
-------

Encoded dictionaries use strings as key.  Sometimes, the string
contains spaces, sometimes it contains dashes.  It could contain any
characters.

By default, we map keywords to strings by merely downcasing them (and
vice-versa, by interning the upcased string in the KEYWORD package).

To map keywords to a different string, we use key-map-exceptions.
Since dictionaries may contain other dictionaries as values, an entry
in the key-map-exceptions may specify another key-map-exceptions to be
used when reading the value of the dictionary entry.


A key-map-exceptions is an a-list mapping keywords to either:

- a string, or
- a p-list containing the keys :string and/or :key-map.

The string substitutes the keyword in the serialized dictionary; the
key-map is another key-map exceptions a-list to be used when reading
the value of that dictionary entry.

*TORRENT-KEY-MAP-EXCEPTIONS* is the key-map-exceptions that can be
 used to read and write torrent files, binding it to *KEY-MAP-EXCEPTIONS*.



Decoding BEncoded strings or streams
------------------------------------

::

   (with-open-stream (torrent \"example.torrent\" :element-type '(unsigned-byte 8))
     ;; torrent files contain binary 'strings'...
     (let ((*key-map-exceptions* *torrent-key-map-exceptions*))
       (bdecode-from-binary-stream torrent)))

   (with-open-stream (stream \"file.bencoded\")
     (bdecode-from-character-stream torrent))

   (bdecode-from-string \"li43e4:spaml4:spam4:eggsed3:cow3:moo4:spam4:eggsed4:spaml1:a1:beee\")


Encoding BEncoded strings or streams
------------------------------------

::

   (with-open-stream (torrent \"example.torrent\"
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
     ;; torrent files contain binary 'strings'...
     (let ((*key-map-exceptions* *torrent-key-map-exceptions*))
       (bencode-to-binary-stream object torrent)))

   (with-open-stream (stream \"file.bencoded\"
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
     (bencode-to-character-stream object stream))


    (bencode-to-string (list 42 '(\"aa\" bb #\\c :dd)
                         (hashtable :elements '((one 1)  (\"two\"  \"2\") (:three-and-one-third (x x x))))
                         (hashtable :elements '((:one 1) (:two \"2\")     (:three-and-one-third (x x x))))))
   --> \"li42el2:aa2:BB1:c2:DDed3:onei1e19:three-and-one-thirdl1:X1:X1:Xe3:two1:2ed3:onei1e19:three-and-one-thirdl1:X1:X1:Xe3:two1:2ee\"



License:

    AGPL3

    Copyright Pascal J. Bourguignon 2010 - 2012

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.BENCODE")




(defvar *key-map-exceptions* nil
  "Current key-map exceptions.

A key-map-exceptions is an a-list mapping keywords to either:

- a string, or
- a p-list containing the keys :string and/or :key-map.

The string substitutes the keyword in the serialized dictionary; the
key-map is another key-map exceptions a-list to be used when reading
the value of that dictionary entry.

")




(defparameter *torrent-key-map-exceptions*
  '((:created-by . "created by")
    (:creation-date . "creation date")
    (:info . (:string "info"
              :key-map ((:piece-length . "piece length")))))
  "Key-map exceptions for torrent files.")



(defun key-map-entry-for-key (keyword)
  (assoc keyword *key-map-exceptions*))

(defun key-map-entry-with-string (key-string)
  (find-if (lambda (entry)
             (string= key-string
                      (if (stringp (cdr entry))
                          (cdr entry)
                          (getf (cdr entry) :string))))
           *key-map-exceptions*))

(defun key-map-entry-keyword (entry)
  (car entry))

(defun key-map-entry-string (entry)
  (if (stringp (cdr entry))
      (cdr entry)
      (getf (cdr entry) :string)))

(defun key-map-entry-key-map (entry)
  (if (stringp (cdr entry))
      nil
      (getf (cdr entry) :key-map)))

(defun key-map-exception (keyword)
  (key-map-entry-key-map (key-map-entry-for-key keyword)))

(defun encode-key-map-exception (keyword)
  "Searches in *KEY-MAP-EXCEPTIONS* an exceptional mapping of the keyword KEYWORD."
  (key-map-entry-string (key-map-entry-for-key keyword)))

(defun decode-key-map-exception (key-string)
  "Searches in *KEY-MAP-EXCEPTIONS* an exceptional mapping of the string KEY-STRING."
  (key-map-entry-keyword (key-map-entry-with-string key-string)))



(defun encode-key (keyword)
  (or (encode-key-map-exception keyword)
      (string-downcase keyword)))

(defun decode-key (key-string)
  (or (decode-key-map-exception key-string)
      (intern (string-upcase key-string) "KEYWORD")))



;;;------------------------------------------------------------------------
;;;
;;; STREAM-WRAPPER
;;;

(defclass stream-wrapper ()
  ((stream :initarg :stream :reader get-stream)))

(defmethod print-object ((self stream-wrapper) stream)
  (let ((*print-readably* nil))
   (print-unreadable-object (self stream :identity t :type t)
     (format stream ":stream ~S" (if (slot-boundp self 'stream)
                                     (slot-value self 'stream)
                                     "#<UNBOUND>"))))
  self)

(defgeneric stream-write-string (string stream-wrapper))
(defgeneric stream-write-octets (octets stream-wrapper))

(defgeneric stream-peek-char   (stream-wrapper &optional eof-error-p eof-value))
(defgeneric stream-read-char   (stream-wrapper &optional eof-error-p eof-value))
(defgeneric stream-read-string (string stream-wrapper))
(defgeneric stream-read-octets (octets stream-wrapper))


;;;------------------------------------------------------------------------
;;;
;;; BINARY-STREAM
;;;

;; NOTE: for BINARY-STREAM we encode and decode the characters with
;;       CODE-CHAR annd CHAR-CODE.  Instead we should use encoding
;;       functions including utf-8 and other coding systems.

(defclass binary-stream (stream-wrapper)
  ((peeked-bytes
    :initform nil
    :documentation "When stream-peek-char is used the bytes read are stored here.
They'll be read or peeked again before further reading of the stream.")))

(defmethod stream-write-string (string (stream binary-stream))
  (write-sequence (map 'vector (function char-code) string) (get-stream stream))
  string)

(defmethod stream-write-octets (octets (stream binary-stream))
  (write-sequence octets (get-stream stream))
  octets)


(defmethod stream-peek-char   ((stream binary-stream) &optional (eof-error-p t) eof-value)
  (with-slots (peeked-bytes) stream
    (unless peeked-bytes
      (let ((byte (read-byte (get-stream stream) nil nil)))
        (cond
          (byte (push byte peeked-bytes))
          (eof-error-p (error 'end-of-file :stream (get-stream stream)))
          (t (return-from stream-peek-char eof-value)))))
    (code-char (elt peeked-bytes 0))))

(defmethod stream-read-char   ((stream binary-stream) &optional (eof-error-p t) eof-value)
  (with-slots (peeked-bytes) stream
    (code-char (if peeked-bytes
                   (pop peeked-bytes)
                   (read-byte (get-stream stream) eof-error-p eof-value)))))

(defmethod stream-read-string (string (stream binary-stream))
  (let ((octets (make-array (length string) :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (stream-read-octets octets stream)
    (map-into string (function code-char) octets)
    string))

(defmethod stream-read-octets (octets (stream binary-stream))
  (with-slots (peeked-bytes) stream
    (let ((start 0))
      (when peeked-bytes
        (replace octets (reverse peeked-bytes))
        (setf start (length peeked-bytes)
              peeked-bytes nil))
      (let ((read-size (read-sequence octets (get-stream stream) :start start))
            (size (- (length octets) start)))
        (assert (= size read-size) () "Could not read the ~D bytes of the string." size)
        octets))))

;;;------------------------------------------------------------------------
;;;
;;; CHARACTER-STREAM
;;;

(defclass character-stream (stream-wrapper)
  ())

(defmethod stream-write-string (string (stream character-stream))
  (write-string string (get-stream stream))
  string)

(defmethod stream-write-octets (octets (stream character-stream))
  (write-sequence (map 'vector (function code-char) octets) (get-stream stream))
  octets)


(defmethod stream-peek-char   ((stream character-stream) &optional (eof-error-p t) eof-value)
  (peek-char nil (get-stream stream) eof-error-p eof-value))

(defmethod stream-read-char   ((stream character-stream) &optional (eof-error-p t) eof-value)
  (read-char (get-stream stream) eof-error-p eof-value))

(defmethod stream-read-string (string (stream character-stream))
  (let ((read-size (read-sequence string (get-stream stream))))
    (assert (= (length string) read-size)
            () "Could not read the ~D bytes of the string." (length string))
    string))

(defmethod stream-read-octets (octets (stream character-stream))
  (let* ((size      (length octets))
         (string    (make-string size))
         (read-size (read-sequence string (get-stream stream))))
    (assert (= size read-size)
            () "Could not read the ~D bytes of the string." size)
    (map-into octets (function char-code) string)))


;;;------------------------------------------------------------------------
;;;
;;; Serializer
;;;

(defgeneric encode-object (value stream))


(defmethod encode-object ((value integer) stream)
  (stream-write-string (format nil "i~Ae" value) stream)
  value)

(defmethod encode-object ((value string) stream)
  (stream-write-string (format nil "~A:" (length value)) stream)
  (stream-write-string value stream)
  value)

(defmethod encode-object ((value vector) stream)
  (assert (every (lambda (byte) (typep byte '(integer 0 255))) value))
  (stream-write-string (format nil "~A:" (length value)) stream)
  (stream-write-octets value stream)
  value)

(defmethod encode-object ((value symbol) stream)
  (encode-object (string value) stream)
  value)

(defmethod encode-object ((value character) stream)
  (encode-object (string value) stream)
  value)

(defmethod encode-object ((value list) stream)
  (stream-write-string "l" stream)
  (dolist (item value)
    (encode-object item stream))
  (stream-write-string "e" stream)
  value)


(defmethod encode-object ((value hash-table) stream)
  (stream-write-string "d" stream)
  (loop
     :for (key-string key . val)
     :in (sort (mapcar (lambda (entry) (cons (encode-key (car entry)) entry))
                       (hash-table-entries value))
               ;; http://bittorrent.org/beps/bep_0003.html
               ;;   "Keys must be strings and appear in sorted order
               ;;    (sorted as raw strings, not alphanumerics)."
               ;; http://en.wikipedia.org/wiki/Bencode
               ;;   "All keys must be byte strings and must appear in lexicographical order."
               ;; What does this mean? The encoding is not specified!
               ;; This probably mean that string< is wrong anyways...
               (function string<)
               :key (function car))
     :do (progn
           (encode-object key-string stream)
           (let ((*key-map-exceptions* (key-map-exception key)))
             (encode-object val stream))))
  (stream-write-string "e" stream)
  value)

(defun encode-octets (octets binary-stream)
  (stream-write-string (format nil "~A:" (length octets)) binary-stream)
  (write-sequence octets binary-stream))



;;;------------------------------------------------------------------------
;;;
;;; Deserializer
;;;

(defun decode-raw-cardinal (stream)
  "Read a sequence of digits, converting them into a positive integer.
There must be at least one digit."
  (if (digit-char-p (stream-peek-char stream nil nil))
      (loop
         :with n = 0
         :while (digit-char-p (stream-peek-char stream nil nil))
         :do (setf n (+ (* 10 n) (digit-char-p (stream-read-char stream))))
         :finally (return n))
      (error "Expected an integer in stream, but got ~A instead."
             (stream-read-char stream nil nil))))

(defun decode-raw-integer (stream)
  "Reads a integer composed of an optional minus sign, and a sequence of at least one digit."
  (if (char= #\- (stream-peek-char stream))
      (progn
        (stream-read-char stream)
        (- (decode-raw-cardinal stream)))
      (decode-raw-cardinal stream)))

(defun eat-end (stream context &rest arguments)
  (let ((end (stream-read-char stream t)))
    (unless (char-equal #\e end)
      (error "Expected #\\e instead of ~S after ~?" end context arguments))))

(defun looking-at-end (stream)
  (char-equal #\e (stream-peek-char stream nil #\return)))

(defun decode-integer (stream)
  (assert (char-equal #\i (stream-read-char stream)))
  (let ((value (decode-raw-integer stream)))
    (check-type value integer)
    (eat-end stream "the integer ~A" value)
    value))

(defun decode-octets (stream)
  (let* ((len (decode-raw-cardinal stream)))
    (check-type len integer)
    (let ((separator (stream-read-char stream)))
      (assert (char= #\: separator) ()
              "Expected a colon after the length of the string, not ~C" separator))
    (stream-read-octets (make-array len :element-type '(unsigned-byte 8) :initial-element 0)
                        stream)))


(defparameter *character-codes*
  (map 'vector (function char-code)
       ;; Standard characters:
       " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))

(defun character-code-p (code)
  (find code *character-codes*))

(defun decode-string (stream)
  (let* ((len (decode-raw-cardinal stream)))
    (check-type len integer)
    (let ((separator (stream-read-char stream)))
      (assert (char= #\: separator) ()
              "Expected a colon after the length of the string, not ~C" separator))
    (let ((octets (stream-read-octets (make-array len
                                                  :element-type '(unsigned-byte 8)
                                                  :initial-element 0)
                                      stream)))
      (if (every (function character-code-p) octets)
          (map 'string (function code-char) octets)
          octets))))


(defun decode-list (stream)
  (assert (char-equal #\l (stream-read-char stream)))
  (loop
     :until (looking-at-end stream)
     :collect (decode-object stream)
     :finally (eat-end stream "a list")))

(defun decode-dictionary (stream)
  (assert (char-equal #\d (stream-read-char stream)))
  (loop
     :with dict = (make-hash-table)
     :until (looking-at-end stream)
     :do (let ((key (decode-key (decode-object stream))))
           (let ((*key-map-exceptions* (key-map-exception key)))
             (setf (gethash key dict) (decode-object stream))))
     :finally (progn (eat-end stream "a dictionary")
                     (return dict))))

(defun decode-object (stream)
  (let ((type-char (stream-peek-char stream)))
    (case (char-downcase type-char)
      ((#\i)                                     (decode-integer    stream))
      ((#\d)                                     (decode-dictionary stream))
      ((#\l)                                     (decode-list       stream))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (decode-string     stream))
      (otherwise
       (stream-read-char  stream)
       (error "Unexpected type character in bencoded stream: ~C" type-char)))))

(defun decode (stream)
  "Reads a BEncoded object from the STREAM stream-wrapper."
  (with-standard-io-syntax
    (decode-object stream)))


;;;------------------------------------------------------------------------
;;;
;;; Entry points.
;;;

(defun bdecode-from-binary-stream (input)
  "Reads a BEncoded object from the INPUT binary stream."
  (decode (make-instance 'binary-stream :stream input)))

(defun bencode-to-binary-stream (object output)
  "Writes a BEncoded OBJECT to the OUTPUT binary stream."
  (encode-object object (make-instance 'binary-stream :stream output)))


(defun bdecode-from-character-stream (input)
  "Reads a BEncoded object from the INPUT character stream."
  (decode (make-instance 'character-stream :stream input)))

(defun bencode-to-character-stream (object output)
  "Writes a BEncoded OBJECT to the OUTPUT character stream."
  (encode-object object (make-instance 'character-stream :stream output)))

(defun bencode-to-string (object)
  "Returns a string containing the BEncoded OBJECT."
  (with-output-to-string (output)
    (bencode-to-character-stream object output)))

(defun bdecode-from-string (bencoded-string)
  "Returns the object BEncoded in the BENCODED-STRING."
  (with-input-from-string (input bencoded-string)
    (bdecode-from-character-stream input)))




;;;; THE END ;;;;
