;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               peek-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a class named PEEK-STREAM that encapsulates
;;;;    a stream and a buffer in such a way that reading, peeking or
;;;;    unreading characters can be done in any number and in any order.
;;;;    
;;;;    We don't use gray stream to keep it pure Common-Lisp.
;;;;    The I/O methods are GETCHAR, UNGETCHAR and NEXTCHAR to avoid
;;;;    name clashes with un-generic READ-CHAR, UNREAD-CHAR and PEEK-CHAR.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-07 <PJB> Corrected mod-incf and mod-decf.
;;;;    2004-09-06 <PJB> Extracted from parse-html.
;;;;BUGS
;;;;    Does not implement other I/O than these three character input methods.
;;;;    NEXTCHAR can be called only once in a row.
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2012
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM"
  (:use "COMMON-LISP")
  (:export "PEEK-STREAM" "NEXTCHAR" "UNGETCHAR" "GETCHAR" "READLINE")
  (:documentation
   "
    This package exports a class named PEEK-STREAM that encapsulates
    a stream and a buffer in such a way that reading, peeking or
    unreading characters can be done in any number and in any order.
    
    We don't use gray stream to keep it pure Common-Lisp.
    The I/O methods are GETCHAR, UNGETCHAR and NEXTCHAR to avoid
    name clashes with un-generic READ-CHAR, UNREAD-CHAR and PEEK-CHAR.

    Copyright Pascal J. Bourguignon 2004 - 2004
   
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
    "))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM")


(defgeneric extend-buffer (self))
(defgeneric getchar (self))
(defgeneric ungetchar (self ch))
(defgeneric nextchar (self))
(defgeneric readline (self))


(defclass peek-stream ()
  ((instre :reader   instre :initarg :stream  :type stream)
   (next   :accessor next   :initform  8 :type fixnum)
   (head   :accessor head   :initform  8 :type fixnum)
   (tail   :accessor tail   :initform  8 :type fixnum)
   (buffer :accessor buffer :initform (make-array '(16)
                                                  :adjustable t
                                                  :element-type 'character)))
  (:documentation "More than on character may be peeked and unread from this."))



;; | | | | | | |C|o|m|m|o|n|-|L| | | | |
;;              ^     ^         ^
;;              |     |         |
;;              |     |         +--- tail
;;              |     +------------- next
;;              +--------------------head


(defmethod print-object ((self peek-stream) (stream stream))
  (format stream "#<PEEK-STREAM: \"~{~A~}\" (H:~D N:~D T:~D) ~S>"
          (if (< (tail self) (head self))
              (list (subseq (buffer self) (head self))
                    (subseq (buffer self) 0 (tail self)))
              (list (subseq (buffer self) (head self) (tail self))))
          (head self) (next self) (tail self)
          (instre self)))


(defmacro mod-incf (modulo place &optional (increment 1) &environment env)
  "Increments the PLACE by INCREMENT modulo MODULO."
  (multiple-value-bind (temps vals stores store-form access-form) (get-setf-expansion place env)
    `(let* (,@(mapcar (function list) temps vals)
            (,(first stores) (mod (+ ,access-form ,increment) ,modulo)))
       ,store-form)))


(defmacro mod-decf (modulo place &optional (decrement 1) &environment env)
  "Decrements the PLACE by DECREMENT modulo MODULO."
  (multiple-value-bind (temps vals stores store-form access-form) (get-setf-expansion place env)
    `(let* (,@(mapcar (function list) temps vals)
            (,(first stores) (mod (- ,access-form ,decrement) ,modulo)))
       ,store-form)))


(defmethod extend-buffer ((self peek-stream))
  (let ((old-length (length (buffer self))))
    (adjust-array (buffer self) (list (* 2 old-length)))
    (replace (buffer self) (buffer self)
             :start1 old-length :start2 0 :end2 (tail self))
    (mod-incf (length (buffer self)) (tail self) old-length)))
  

(defmethod getchar ((self peek-stream))
  "
RETURN:  The next character from SELF.
         (It can be a character newly read from the encapsulated stream,
          or a character buffered by NEXTCHAR or UNGETCHAR).
"
  (if (= (head self) (tail self))
      (read-char (instre self) nil nil)
      (prog1 (aref (buffer self) (head self))
        (mod-incf (length (buffer self)) (head self))
        (setf (next self) (head self)))))


(defmethod ungetchar ((self peek-stream) (ch null))
  ch)

(defmethod ungetchar ((self peek-stream) (ch character))
  "
DO:      Put the character CH in front of the input buffer.
         It does not need to be the same as any character read from SELF.
RETURN:  CH.
"
  (mod-decf (length (buffer self)) (head self))
  (when (= (head self) (tail self))
    (mod-incf (length (buffer self)) (head self))
    (extend-buffer self)
    (mod-decf (length (buffer self)) (head self)))
  (setf (aref (buffer self) (head self)) ch
        (next self) (head self))
  ch)


;; ungetchar ==> (decf head), put char at (aref buffer head), next:=head
;; getchar   ==> get char at (aref buffer head), (incf head), next:=head
;;               head==tail ==> read-char
;; nextchar  ==> get char at (aref buffer next), (incf next)
;;               next==tail ==> read-char,
;;                              put ch at (aref buffer tail),
;;                              (incf tail) (incf next)


(defmethod nextchar ((self peek-stream))
  "
RETURN:  The character that will be read soon by GETCHAR, or NIL when EOF.
         (equalp (loop repeat N collect (nextchar ps))
                 (loop repeat N collect (getchar  ps)))
"
  (if (/= (next self) (tail self))
      (prog1 (aref (buffer self) (next self))
        (mod-incf (length (buffer self)) (next self)))
      (let ((ch (read-char (instre self) nil nil)))
        (when ch
          (setf (aref (buffer self) (tail self)) ch)
          (mod-incf (length (buffer self)) (tail self))
          (when (= (head self) (tail self))
            (mod-decf (length (buffer self)) (tail self))
            (extend-buffer self)
            (mod-incf (length (buffer self)) (tail self)))
          (setf (next self) (tail self)))
        ch)))


(defmethod readline ((self peek-stream))
  "
RETURN:  A whole line read from the peek-stream, or NIL in case of end of stream.
"
  (when (nextchar self)
   (loop
     :with line = (make-array 80 :element-type 'character :adjustable t :fill-pointer 0)
     :for ch = (getchar self)
     :while ch
     :do (if  (char= ch #\Newline)
              (loop-finish)
              (vector-push-extend ch line (length line)))
     :finally (return line))))


(defun test ()
  (dotimes (n 10)
    (with-input-from-string (in "ComMon-Lisp")
      (let* ((ps (make-instance 'peek-stream :stream in))
             (nc (loop repeat n for ch = (nextchar ps)
                    collect ch into result finally (return result)))
             (gc (loop repeat n for ch = (getchar  ps)
                    collect ch into result finally (return result))))
        (assert (equal nc gc)))))
  (with-input-from-string (in "ComMon-Lisp")
    (let ((ps (make-instance 'peek-stream :stream in))
          c1 c2 c3)
      (assert (equal (list (getchar ps) (getchar ps) (getchar ps))
                     '(#\c #\o #\m)))
      (setf c1 (getchar ps) c2 (getchar ps) c3 (getchar ps))
      (assert (equal (list c1 c2 c3 (nextchar ps))
                     '(#\m #\o #\n #\-)))
      (ungetchar ps c3)(ungetchar ps c2)(ungetchar ps c1)
      (assert (equal (list (getchar ps) (getchar ps) (getchar ps))
                     '(#\m #\o #\n)))
      (assert (equal (list  (getchar ps) (getchar ps) (getchar ps))
                     '(#\- #\l #\i)))))
  (with-input-from-string (in "Common-Lisp")
    (let ((ps (make-instance 'peek-stream :stream in))
          c1 c2 c3)
      (assert (equal (list (getchar ps) (getchar ps) (getchar ps))
                     '(#\c #\o #\m)))
      (setf c1 (getchar ps) c2 (getchar ps))
      (assert (equal (list c1 c2 (nextchar ps))
                     '(#\m #\o #\n)))
      (setf c3 (getchar ps))
      (assert (equal (list c3 (nextchar ps))
                     '(#\n #\-)))
      (ungetchar ps c3)(ungetchar ps c2)(ungetchar ps c1)
      (assert (equal (list (getchar ps) (getchar ps) (getchar ps))
                     '(#\m #\o #\n)))
      (assert (equal (list (getchar ps) (getchar ps) (getchar ps))
                     '(#\- #\l #\i)))))
  (values))


;;;; THE END ;;;;
