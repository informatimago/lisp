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
;;;;    2004-09-06 <PJB> Extracted from parse-html.
;;;;BUGS
;;;;    Does not implement other I/O than these three character input methods.
;;;;    NEXTCHAR can be called only once in a row.
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2004
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PEEK-STREAM"
  (:USE "COMMON-LISP")
  (:EXPORT "NEXTCHAR" "UNGETCHAR" "GETCHAR" "PEEK-STREAM")
  (:DOCUMENTATION
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
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PEEK-STREAM")


(DEFGENERIC EXTEND-BUFFER (SELF))
(DEFGENERIC GETCHAR (SELF))
(DEFGENERIC UNGETCHAR (SELF CH))
(DEFGENERIC NEXTCHAR (SELF))


(DEFCLASS PEEK-STREAM ()
  ((INSTRE :READER   INSTRE :INITARG :STREAM  :TYPE STREAM)
   (NEXT   :ACCESSOR NEXT   :INITFORM  8 :TYPE FIXNUM)
   (HEAD   :ACCESSOR HEAD   :INITFORM  8 :TYPE FIXNUM)
   (TAIL   :ACCESSOR TAIL   :INITFORM  8 :TYPE FIXNUM)
   (BUFFER :ACCESSOR BUFFER :INITFORM (MAKE-ARRAY '(16)
                                                  :ADJUSTABLE T
                                                  :ELEMENT-TYPE 'CHARACTER)))
  (:DOCUMENTATION "More than on character may be peeked and unread from this.")
  ) ;;PEEK-STREAM



;; | | | | | | |C|o|m|m|o|n|-|L| | | | |
;;              ^     ^         ^
;;              |     |         |
;;              |     |         +--- tail
;;              |     +------------- next
;;              +--------------------head


(DEFMETHOD PRINT-OBJECT ((SELF PEEK-STREAM) (STREAM STREAM))
  (FORMAT STREAM "#<PEEK-STREAM: \"~{~A~}\" (H:~D N:~D T:~D) ~S>"
          (IF (< (TAIL SELF) (HEAD SELF))
              (LIST (SUBSEQ (BUFFER SELF) (HEAD SELF))
                    (SUBSEQ (BUFFER SELF) 0 (TAIL SELF)))
              (LIST (SUBSEQ (BUFFER SELF) (HEAD SELF) (TAIL SELF))))
          (HEAD SELF) (NEXT SELF) (TAIL SELF)
          (INSTRE SELF))) ;;PRINT-OBJECT


(DEFMACRO MOD-INCF (MODULO PLACE &OPTIONAL (INCREMENT 1))
  "
BUG:  Evaluates PLACE several times.
"
  `(SETF ,PLACE (MOD (+ ,PLACE ,INCREMENT) ,MODULO))) ;;MOD-INCF


(DEFMACRO MOD-DECF (MODULO PLACE &OPTIONAL (DECREMENT 1))
  "
BUG:  Evaluates PLACE several times.
"
  `(SETF ,PLACE (MOD (- ,PLACE ,DECREMENT) ,MODULO))) ;;MOD-DECF


(DEFMETHOD EXTEND-BUFFER ((SELF PEEK-STREAM))
  (LET ((OLD-LENGTH (LENGTH (BUFFER SELF))))
    (ADJUST-ARRAY (BUFFER SELF) (LIST (* 2 OLD-LENGTH)))
    (REPLACE (BUFFER SELF) (BUFFER SELF)
             :START1 OLD-LENGTH :START2 0 :END2 (TAIL SELF))
    (MOD-INCF (LENGTH (BUFFER SELF)) (TAIL SELF) OLD-LENGTH))) ;;EXTEND-BUFFER
  

(DEFMETHOD GETCHAR ((SELF PEEK-STREAM))
  "
RETURN:  The next character from SELF.
         (It can be a character newly read from the encapsulated stream,
          or a character buffered by NEXTCHAR or UNGETCHAR).
"
  (IF (= (HEAD SELF) (TAIL SELF))
      (READ-CHAR (INSTRE SELF) NIL NIL)
      (PROG1 (AREF (BUFFER SELF) (HEAD SELF))
        (MOD-INCF (LENGTH (BUFFER SELF)) (HEAD SELF))
        (SETF (NEXT SELF) (HEAD SELF))))) ;;GETCHAR


(DEFMETHOD UNGETCHAR ((SELF PEEK-STREAM) (CH CHARACTER))
  "
DO:      Put the character CH in front of the input buffer.
         It does not need to be the same as any character read from SELF.
"
  (MOD-DECF (LENGTH (BUFFER SELF)) (HEAD SELF))
  (WHEN (= (HEAD SELF) (TAIL SELF))
    (MOD-INCF (LENGTH (BUFFER SELF)) (HEAD SELF))
    (EXTEND-BUFFER SELF)
    (MOD-DECF (LENGTH (BUFFER SELF)) (HEAD SELF)))
  (SETF (AREF (BUFFER SELF) (HEAD SELF)) CH
        (NEXT SELF) (HEAD SELF))) ;;UNGETCHAR


;; ungetchar ==> (decf head), put char at (aref buffer head), next:=head
;; getchar   ==> get char at (aref buffer head), (incf head), next:=head
;;               head==tail ==> read-char
;; nextchar  ==> get char at (aref buffer next), (incf next)
;;               next==tail ==> read-char,
;;                              put ch at (aref buffer tail),
;;                              (incf tail) (incf next)


(DEFMETHOD NEXTCHAR ((SELF PEEK-STREAM))
  "
RETURN:  The character that will be read soon by GETCHAR, or NIL when EOF.
         (equalp (loop repeat N for ch = (nextchar ps)
                       collect ch into result finally (return result))
                 (loop repeat N for ch = (getchar  ps)
                       collect ch into result finally (return result)))
"
  (IF (/= (NEXT SELF) (TAIL SELF))
      (PROG1 (AREF (BUFFER SELF) (NEXT SELF))
        (MOD-INCF (LENGTH (BUFFER SELF)) (NEXT SELF)))
      (LET ((CH (READ-CHAR (INSTRE SELF) NIL NIL)))
        (WHEN CH
          (SETF (AREF (BUFFER SELF) (TAIL SELF)) CH)
          (MOD-INCF (LENGTH (BUFFER SELF)) (TAIL SELF))
          (WHEN (= (HEAD SELF) (TAIL SELF))
            (MOD-DECF (LENGTH (BUFFER SELF)) (TAIL SELF))
            (EXTEND-BUFFER SELF)
            (MOD-INCF (LENGTH (BUFFER SELF)) (TAIL SELF)))
          (SETF (NEXT SELF) (TAIL SELF)))
        CH))) ;;NEXTCHAR


(DEFUN TEST ()
  (DOTIMES (N 10)
    (WITH-INPUT-FROM-STRING (IN "ComMon-Lisp")
      (LET* ((PS (MAKE-INSTANCE 'PEEK-STREAM :STREAM IN))
             (NC (LOOP REPEAT N FOR CH = (NEXTCHAR PS)
                    COLLECT CH INTO RESULT FINALLY (RETURN RESULT)))
             (GC (LOOP REPEAT N FOR CH = (GETCHAR  PS)
                    COLLECT CH INTO RESULT FINALLY (RETURN RESULT))))
        (ASSERT (EQUAL NC GC)))))
  (WITH-INPUT-FROM-STRING (IN "ComMon-Lisp")
    (LET ((PS (MAKE-INSTANCE 'PEEK-STREAM :STREAM IN))
          C1 C2 C3)
      (ASSERT (EQUAL (LIST (GETCHAR PS) (GETCHAR PS) (GETCHAR PS))
                     '(#\C #\O #\M)))
      (SETF C1 (GETCHAR PS) C2 (GETCHAR PS) C3 (GETCHAR PS))
      (ASSERT (EQUAL (LIST C1 C2 C3 (NEXTCHAR PS))
                     '(#\M #\O #\N #\-)))
      (UNGETCHAR PS C3)(UNGETCHAR PS C2)(UNGETCHAR PS C1)
      (ASSERT (EQUAL (LIST (GETCHAR PS) (GETCHAR PS) (GETCHAR PS))
                     '(#\M #\O #\N)))
      (ASSERT (EQUAL (LIST  (GETCHAR PS) (GETCHAR PS) (GETCHAR PS))
                     '(#\- #\L #\I)))))
  (WITH-INPUT-FROM-STRING (IN "Common-Lisp")
    (LET ((PS (MAKE-INSTANCE 'PEEK-STREAM :STREAM IN))
          C1 C2 C3)
      (ASSERT (EQUAL (LIST (GETCHAR PS) (GETCHAR PS) (GETCHAR PS))
                     '(#\C #\O #\M)))
      (SETF C1 (GETCHAR PS) C2 (GETCHAR PS))
      (ASSERT (EQUAL (LIST C1 C2 (NEXTCHAR PS))
                     '(#\M #\O #\N)))
      (SETF C3 (GETCHAR PS))
      (ASSERT (EQUAL (LIST C3 (NEXTCHAR PS))
                     '(#\N #\-)))
      (UNGETCHAR PS C3)(UNGETCHAR PS C2)(UNGETCHAR PS C1)
      (ASSERT (EQUAL (LIST (GETCHAR PS) (GETCHAR PS) (GETCHAR PS))
                     '(#\M #\O #\N)))
      (ASSERT (EQUAL (LIST (GETCHAR PS) (GETCHAR PS) (GETCHAR PS))
                     '(#\- #\L #\I)))))
  (VALUES))
      


;;;; peek-stream.lisp                 --                     --          ;;;;
