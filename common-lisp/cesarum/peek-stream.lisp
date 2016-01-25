;;;; -*- mode:lisp;coding:utf-8 -*-
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
;;;;    2012-07-15 <PJB> Re-implemented NEXTCHAR with same semantics as PEEKCHAR.
;;;;                     (The old NEXTCHAR behavior is available as GET-FUTURE-CHAR).
;;;;    2012-02-07 <PJB> Corrected mod-incf and mod-decf.
;;;;    2004-09-06 <PJB> Extracted from parse-html.
;;;;BUGS
;;;;    Does not implement other I/O than these three character input methods.
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2016
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
;;;;****************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM"
  (:use "COMMON-LISP")
  (:export "PEEK-STREAM" "PEEK-STREAM-SPACES"
           "NEXTCHAR" "UNGETCHAR" "GETCHAR" "READLINE"
           "GET-FUTURE-CHAR"
           "PEEK-STREAM-STREAM")
  (:documentation
   "

This package exports a class named PEEK-STREAM that encapsulates a
stream and a buffer in such a way that reading, peeking or unreading
characters can be done in any number and in any order.

We don't use gray stream to keep it pure Common-Lisp.  The I/O methods
are GETCHAR, UNGETCHAR and NEXTCHAR to avoid name clashes with
un-generic READ-CHAR, UNREAD-CHAR and PEEK-CHAR.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2004 - 2015
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM")


(defgeneric extend-buffer (peek-stream)
  (:documentation "INTERNAL"))

(defgeneric getchar (peek-stream)
  (:documentation "RETURN: The next character in the PEEK-STREAM, advancing."))

(defgeneric ungetchar (peek-stream ch)
  (:documentation "DO: Unread the character CH from the PEEK-STREAM."))


(defgeneric peek-stream-spaces (peek-stream)
  (:documentation "
RETURN:         A sequence of characters to be considered whitespace
                by (nextchar peek-stream T).
"))

(defgeneric (setf peek-stream-spaces) (new-whitespaces peek-stream)
  (:documentation "
DO:             Changes the sequence of characters to be considered
                whitespace by (nextchar peek-stream T).
"))

(defgeneric nextchar (peek-stream &optional peek-type)
  (:documentation "
DO:             Just like CL:PEEK-CHAR.

                If peek-type is not supplied or NIL, peek-char returns
                the next character to be read from input-stream,
                without actually removing it from input-stream. The
                next time input is done from input-stream, the
                character will still be there.

                If  peek-type is T, then peek-char skips over
                whitespace[2] characters, but not comments, and then
                performs the peeking operation on the next
                character. The last character examined, the one that
                starts an object, is not removed from  input-stream.

                If peek-type is a CHARACTER, then peek-char skips over
                input characters until a character that is char= to
                that character is found; that character is left in
                input-stream.

RETURN:         The same character that will be returned by the next
                (getchar self).

NOTE:           There's no conforming way to determine whether a
                character has the whitespace[2] syntax in the current
                *readtable*.  Therefore we use instead the
                PEEK-STREAM-SPACES method to get the list of spaces.
"))

(defgeneric get-future-char (peek-stream)
  (:documentation "
DO:             Read a character from the underlying stream and keep
                it in the buffer for GETCHAR.

RETURN:         The character that has been read, or NIL when EOF
                (which doesn't mean GETCHAR would return NIL).

NOTE:           We have:
                    (equalp (loop repeat N collect (nextchar ps))
                            (loop repeat N collect (getchar  ps)))
                but not:
                    (equalp (loop repeat N collect (getchar  ps))
                            (loop repeat N collect (nextchar ps)))
SEE ALSO:       NEXTCHAR.
"))

(defgeneric readline (peek-stream)
  (:documentation "RETURN: A string containing the read line."))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun char-name-supported-p (name)
     (ignore-errors (read-from-string (format nil "#\\~A" name)))))

#+mocl
(defvar *spaces* " ")
#-mocl
(defvar *spaces*
  (let ((spaces '()))
    (dolist (name '("Vt" "Page" "Linefeed" "Return" "Tab" "Newline" "Space"))
      (let ((ch (char-name-supported-p name)))
        (when ch (push ch spaces))))
    (coerce spaces 'string)))


(defclass peek-stream ()
  ((instre :reader   instre :initarg :stream  :type stream
           :reader peek-stream-stream)
   (next   :accessor next   :initform  8 :type fixnum)
   (head   :accessor head   :initform  8 :type fixnum)
   (tail   :accessor tail   :initform  8 :type fixnum)
   (buffer :accessor buffer :initform (make-array '(16)
                                                  :adjustable t
                                                  :element-type 'character))
   (spaces :accessor peek-stream-spaces :initarg :spaces :initform *spaces* :type sequence))
  (:documentation "More than one character may be peeked and unread from this."))



;; | | | | | | |C|o|m|m|o|n|-|L| | | | |
;;              ^     ^         ^
;;              |     |         |
;;              |     |         +--- tail
;;              |     +------------- next
;;              +--------------------head


(defmethod print-object ((self peek-stream) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "\"~{~A~}\" (H:~D N:~D T:~D) ~S"
            (if (< (tail self) (head self))
                (list (subseq (buffer self) (head self))
                      (subseq (buffer self) 0 (tail self)))
                (list (subseq (buffer self) (head self) (tail self))))
            (head self) (next self) (tail self)
            (instre self)))
  self)


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


;; ungetchar       ==> (decf head), put char at (aref buffer head), next:=head
;; nextchar        ==> get char at (aref buffer head).
;; getchar         ==> get char at (aref buffer head), (incf head), next:=head
;;                     head==tail ==> read-char
;; get-future-char ==> get char at (aref buffer next), (incf next)
;;                     next==tail ==> read-char,
;;                                    put ch at (aref buffer tail),
;;                                    (incf tail) (incf next)

(defmethod nextchar ((self peek-stream) &optional (peek-type nil))
  (case peek-type
    ((nil)
     (if (/= (next self) (tail self))
       (aref (buffer self) (next self))
       (let ((ch (read-char (instre self) nil nil)))
         (when ch
           (setf (aref (buffer self) (tail self)) ch)
           (mod-incf (length (buffer self)) (tail self))
           (when (= (head self) (tail self))
             (mod-decf (length (buffer self)) (tail self))
             (extend-buffer self)
             (mod-incf (length (buffer self)) (tail self))))
         ch)))
    ((t)
     (loop
       :with spaces = (peek-stream-spaces self)
       :for ch = (nextchar self)
       :while (find ch spaces)
       :do (getchar self)
       :finally (return ch)))
    (otherwise
     (loop
       :for ch = (nextchar self)
       :until (char= ch peek-type)
       :do (getchar self)
       :finally (return ch)))))


(defmethod get-future-char ((self peek-stream))
  (prog1 (nextchar self)
    (assert (/= (next self) (tail self)))
    (mod-incf (length (buffer self)) (next self))))


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
              (return line)
              (vector-push-extend ch line (length line)))
     :finally (return line))))

;;;; THE END ;;;;
