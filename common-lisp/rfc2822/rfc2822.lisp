;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               rfc2822.lisp
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
;;;;    2005-09-01 <PJB> Made use of ecma048/iso6429.
;;;;    2004-08-17 <PJB> Created (fetched basic functions from antispam.lisp).
;;;;BUGS
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")

(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048"))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.RFC2822.RFC2822"
  (:use "COMMON-LISP")
  (:export "REMOVE-COMMENTS" "REMOVE-SPACES" "UNQUOTE")
  (:documentation
   "RFC0822/RFC2822 support funtions.

RFC0822/RFC2822 support funtions.

RFC822      STANDARD FOR THE FORMAT OF ARPA INTERNET TEXT MESSAGES
RFC2822     Internet Message Format

RFC822 in fixnum words:

In transmission, message lines are separated by CRLF.
Header lines are separated from body lines by an empty line (CRLFCRLF).
Header lines may be cut by replacing any space or tab by CRLF, (space or tab).
Field name consists of any ASCII printable character but space and colon,
followed by a colon.
Field body begins immediately after the colon. (Customary space included).
NOTE: rfc2822 forbid spaces between field name and colon,
 but it IS possible in rfc822 to insert spaces here.
 (For example, see Annex A of RFC822).


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2004 - 2012
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.RFC2822.RFC2822")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-verbose* nil)) (com.informatimago.common-lisp.cesarum.ecma048:generate-all-functions-in-ecma048)))


(defparameter +space+  (character " ")        "An ASCII SPACE character.")
(defparameter +tab+    (code-char com.informatimago.common-lisp.cesarum.ecma048:ht) "An ASCII TABULATION character.")
(defparameter +cr+     (code-char com.informatimago.common-lisp.cesarum.ecma048:cr) "An ASCII CARRIAGE-RETURN.")
(defparameter +lf+     (code-char com.informatimago.common-lisp.cesarum.ecma048:lf) "An ASCII LINE-FEED.")


(defun unquote (value)
  "
RETURN:    A string where the quotes and escapes are moved.
NOTE:      It is assumed that the value contains only one string, or none.
EXAMPLE:   (unquote \"A string \\\\\" with \\\\\" a quoted word.\")
           -->  \"A string \\\" with \\\" a quoted word.\"
"

  (macrolet ((when-char (ch) (if (stringp ch)
                                 `(char= (character ,ch) (aref value i))
                                 `(char=  ,ch            (aref value i))))
             (copy () `(progn (setf (aref temp j) (aref value i))
                              (incf i) (incf j)))
             (skip () `(incf i)))
    (do ((temp (make-string (length value)))
         (i 0)
         (j 0)
         (state   '(:out))) ;; :QUOTE :COMMENT
        ((<= (length value) i)  (subseq temp 0 j))
      (case (car state)
        ((:out)
         (cond
           ((when-char "\\")                       (skip) (copy))
           ((when-char "\"")                       (skip) (push :quote state))
           (t                                      (copy))))
        ((:quote)
         (cond
           ((when-char "\\")                       (skip) (copy))
           ((when-char "\"")                       (skip) (pop state))
           (t                                      (copy)))) ))))



(defun remove-spaces (value)
  "
RETURN:     A string with unquoted spaces and tabulations removed.
"
  (macrolet ((when-char (ch) (if (stringp ch)
                                 `(char= (character ,ch) (aref value i))
                                 `(char=  ,ch            (aref value i))))
             (copy () `(progn (setf (aref temp j) (aref value i))
                              (incf i) (incf j)))
             (skip () `(incf i)))
    (do ((temp (make-string (length value)))
         (i 0)
         (j 0)
         (state   '(:out))) ;; :QUOTE :COMMENT
        ((<= (length value) i)  (subseq temp 0 j))
      (case (car state)
        ((:out)
         (cond
           ((when-char "\\")                       (copy) (copy))
           ((when-char "\"")                       (copy) (push :quote state))
           ((or (when-char " ") (when-char +tab+)) (skip))
           (t                                      (copy))))
        ((:quote)
         (cond
           ((when-char "\\")                       (copy) (copy))
           ((when-char "\"")                       (copy) (pop state))
           (t                                      (copy)))) ))))



(defun remove-comments (value)
  "
RETURN:   A string with the RFC822 comments removed.
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
  (macrolet ((when-char (ch) (if (stringp ch)
                                 `(char= (character ,ch) (aref value i))
                                 `(char=  ,ch            (aref value i))))
             (copy () `(progn (setf (aref temp j) (aref value i))
                              (incf i) (incf j)))
             (skip () `(incf i)))
    (do ((temp (make-string (length value)))
         (i 0)
         (j 0)
         (state   '(:out))) ;; :QUOTE :COMMENT
        ((<= (length value) i)  (subseq temp 0 j))
      (case (car state)
        ((:out)
         (cond
           ((when-char "\\")    (copy) (copy))
           ((when-char "\"")    (copy) (push :quote    state))
           ((when-char "(")     (skip) (push :comment  state))
           (t                   (copy))))
        ((:quote) 
         (cond
           ((when-char "\\")    (copy) (copy))
           ((when-char "\"")    (copy) (pop state))
           (t                   (copy))))
        ((:comment) 
         (cond
           ((when-char "\\")    (skip) (skip))
           ((when-char "(")     (skip) (push :comment state))
           ((when-char ")")     (skip) (pop state))
           (t                   (skip))))))))

 
;;;; THE END ;;;;
