;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               rfc2822.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    RFC0822/RFC2822 support funtions.
;;;;    
;;;;    RFC822      STANDARD FOR THE FORMAT OF ARPA INTERNET TEXT MESSAGES
;;;;    RFC2822     Internet Message Format
;;;;   
;;;;    RFC822 in fixnum words:
;;;;   
;;;;    In transmission, message lines are separated by CRLF.
;;;;    Header lines are separated from body lines by an empty line (CRLFCRLF).
;;;;    Header lines may be cut by replacing any space or tab by CRLF, (space or tab).
;;;;    Field name consists of any ASCII printable character but space and colon,
;;;;    followed by a colon.
;;;;    Field body begins immediately after the colon. (Customary space included).
;;;;    NOTE: rfc2822 forbid spaces between field name and colon,
;;;;          but it IS possible in rfc822 to insert spaces here.
;;;;          (For example, see Annex A of RFC822).
;;;;   
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-09-01 <PJB> Made use of iso6429.
;;;;    2004-08-17 <PJB> Created (fetched basic functions from antispam.lisp).
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
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
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.ECMA048"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.RFC2822" (:USE "COMMON-LISP")
            (:EXPORT "REMOVE-COMMENTS" "REMOVE-SPACES" "UNQUOTE")
            (:DOCUMENTATION
             "RFC0822/RFC2822 support funtions.

    Copyright Pascal J. Bourguignon 2004 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.RFC2822")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-verbose* nil)) (ecma048:generate-all-functions-in-ecma048)))


(DEFPARAMETER +SPACE+  (character " ")        "An ASCII SPACE character.")
(DEFPARAMETER +TAB+    (CODE-CHAR ecma048:ht) "An ASCII TABULATION character.")
(DEFPARAMETER +CR+     (CODE-CHAR ecma048:cr) "An ASCII CARRIAGE-RETURN.")
(DEFPARAMETER +LF+     (CODE-CHAR ecma048:lf) "An ASCII LINE-FEED.")


(DEFUN UNQUOTE (VALUE)
  "
RETURN:    A STRING WHERE THE QUOTES AND ESCAPES ARE MOVED.
NOTE:      IT IS ASSUMED THAT THE VALUE CONTAINS ONLY ONE STRING, OR NONE.
EXAMPLE:   ''A string \'' with \'' a quoted word.''
           becomes: A string '' with '' a quoted word.
"
  (MACROLET ((WHEN-CHAR (CH) (IF (STRINGP CH)
                                 `(CHAR= (CHARACTER ,CH) (AREF VALUE I))
                                 `(CHAR=  ,CH            (AREF VALUE I))))
             (COPY () `(PROGN (SETF (AREF TEMP J) (AREF VALUE I))
                              (INCF I) (INCF J)))
             (SKIP () `(INCF I)))
    (DO ((TEMP (MAKE-STRING (LENGTH VALUE)))
         (I 0)
         (J 0)
         (STATE   '(:OUT))) ;; :QUOTE :COMMENT
        ((<= (LENGTH VALUE) I)  (SUBSEQ TEMP 0 J))
      (CASE (CAR STATE)
        ((:OUT)
         (COND
           ((WHEN-CHAR "\\")                       (SKIP) (COPY))
           ((WHEN-CHAR "\"")                       (SKIP) (PUSH :QUOTE STATE))
           (T                                      (COPY))))
        ((:QUOTE)
         (COND
           ((WHEN-CHAR "\\")                       (SKIP) (COPY))
           ((WHEN-CHAR "\"")                       (SKIP) (POP STATE))
           (T                                      (COPY)))) ))))



(DEFUN REMOVE-SPACES (VALUE)
  "
RETURN:     A string with unquoted spaces and tabulations removed.
"
  (MACROLET ((WHEN-CHAR (CH) (IF (STRINGP CH)
                                 `(CHAR= (CHARACTER ,CH) (AREF VALUE I))
                                 `(CHAR=  ,CH            (AREF VALUE I))))
             (COPY () `(PROGN (SETF (AREF TEMP J) (AREF VALUE I))
                              (INCF I) (INCF J)))
             (SKIP () `(INCF I)))
    (DO ((TEMP (MAKE-STRING (LENGTH VALUE)))
         (I 0)
         (J 0)
         (STATE   '(:OUT))) ;; :QUOTE :COMMENT
        ((<= (LENGTH VALUE) I)  (SUBSEQ TEMP 0 J))
      (CASE (CAR STATE)
        ((:OUT)
         (COND
           ((WHEN-CHAR "\\")                       (COPY) (COPY))
           ((WHEN-CHAR "\"")                       (COPY) (PUSH :QUOTE STATE))
           ((OR (WHEN-CHAR " ") (WHEN-CHAR +TAB+)) (SKIP))
           (T                                      (COPY))))
        ((:QUOTE)
         (COND
           ((WHEN-CHAR "\\")                       (COPY) (COPY))
           ((WHEN-CHAR "\"")                       (COPY) (POP STATE))
           (T                                      (COPY)))) ))))



(DEFUN REMOVE-COMMENTS (VALUE)
  "
RETURN:   A STRING WITH THE RFC822 COMMENTS REMOVED.
"
;;;      comment     =  "(" *(ctext / quoted-pair / comment) ")"
;;;      ctext       =  <any CHAR excluding "(",     ; => may be folded
;;;                      ")", "\" & CR, & including
;;;                      linear-white-space>
;;;      quoted-pair =  "\" CHAR                     ; may quote any char
;;;      linear-white-space =  1*([CRLF] LWSP-char)  ; semantics = SPACE
;;;                                                  ; CRLF => folding
;;;      CHAR        =  <any ASCII character>        ; (  0-177,  0.-127.)
;;;
;;;      qtext       =  <any CHAR excepting <">,     ; => may be folded
;;;                      "\" & CR, and including
;;;                      linear-white-space>
;;;      quoted-string = <"> *(qtext/quoted-pair) <">; Regular qtext or
;;;                                                  ;   quoted chars.
  (MACROLET ((WHEN-CHAR (CH) (IF (STRINGP CH)
                                 `(CHAR= (CHARACTER ,CH) (AREF VALUE I))
                                 `(CHAR=  ,CH            (AREF VALUE I))))
             (COPY () `(PROGN (SETF (AREF TEMP J) (AREF VALUE I))
                              (INCF I) (INCF J)))
             (SKIP () `(INCF I)))
    (DO ((TEMP (MAKE-STRING (LENGTH VALUE)))
         (I 0)
         (J 0)
         (STATE   '(:OUT))) ;; :QUOTE :COMMENT
        ((<= (LENGTH VALUE) I)  (SUBSEQ TEMP 0 J))
      (CASE (CAR STATE)
        ((:OUT)
         (COND
           ((WHEN-CHAR "\\")    (COPY) (COPY))
           ((WHEN-CHAR "\"")    (COPY) (PUSH :QUOTE    STATE))
           ((WHEN-CHAR "(")     (SKIP) (PUSH :COMMENT  STATE))
           (T                   (COPY))))
        ((:QUOTE) 
         (COND
           ((WHEN-CHAR "\\")    (COPY) (COPY))
           ((WHEN-CHAR "\"")    (COPY) (POP STATE))
           (T                   (COPY))))
        ((:COMMENT) 
         (COND
           ((WHEN-CHAR "\\")    (SKIP) (SKIP))
           ((WHEN-CHAR "(")     (SKIP) (PUSH :COMMENT STATE))
           ((WHEN-CHAR ")")     (SKIP) (POP STATE))
           (T                   (SKIP))))))))

 
;;;; rfc2822.lisp                     --                     --          ;;;;
