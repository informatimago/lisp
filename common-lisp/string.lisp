;;;; -*- coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:              string.lisp
;;;;LANGUAGE:          common-lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This package exports some string utility functions.
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-10-20 <PJB> Moved displaced-vector to
;;;;                     com.informatimago.common-lisp.array.
;;;;    2005-09-01 <PJB> Made use of ISO6429 to improve portability.
;;;;    2004-10-15 <PJB> Added STRING-JUSTIFY-LEFT.
;;;;    2004-10-14 <PJB> Added STRING-PAD, DEFTRANSLATION and LOCALIZE.
;;;;    2004-03-31 <PJB> Added SPLIT-ESCAPED-STRING.
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2005
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;*****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.ECMA048"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.STRING"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
  (:EXPORT
   "LOCALIZE" "DEFTRANSLATION" "STRING-JUSTIFY-LEFT" "STRING-PAD"
   "PREFIXP" "SUFFIXP"
   "SPLIT-NAME-VALUE" "STRING-REPLACE" "UNSPLIT-STRING" "SPLIT-STRING"
   "SPLIT-ESCAPED-STRING" "IMPLODE-STRING" "EXPLODE-STRING"
   "CONCATENATE-STRINGS")
  (:DOCUMENTATION
   "This package exports some string processing functions.

    Copyright Pascal J. Bourguignon 2002 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.STRING")



(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-verbose* nil)) (ecma048:generate-all-functions-in-ecma048)))


(defun concatenate-strings (list-of-strings)
  "
LIST-OF-STRINGS: Each element may be either a string,
                 or a list containing a string, and a start and end position
                 denoting a substring.
RETURN:          A string containing the concatenation of the strings
                 of the LIST-OF-STRINGS.
"
  (flet ((slength (string)
           (if (stringp string)
                      (length string)
                      (- (or (third string) (length (first string)))
                         (second string)))))
    (loop
       :with result = (make-string (loop :for s :in list-of-strings
                                      :sum (slength s)))
       :for pos = 0
       :then (+ pos (slength string))
       :for string :in list-of-strings
       :do (if (stringp string)
               (replace result string :start1 pos)
               (replace result (first string) :start1 pos
                        :start2 (second string) :end2 (third string)))
       :finally (return result))))


(DEFUN EXPLODE-STRING (STRING)
  "
return a new list containing the character in the sequence string.
"
  (MAP 'LIST (FUNCTION CHARACTER) STRING))


(DEFUN IMPLODE-STRING (CHAR-SEQ)
  "
RETURN: A new string containing the characters in the sequence CHAR-SEQ.
"
  (MAP 'STRING (FUNCTION CHARACTER) CHAR-SEQ))


(DEFINE-COMPILER-MACRO IMPLODE-STRING (&WHOLE FORM  CHAR-SEQ)
  "
RETURN:  An optimized form for compiled code.
NOTE:    Unfortunately clisp does to take into account compiler-macros
         even when compiling...
"
  (DECLARE (IGNORE FORM))
  (WITH-GENSYMS (SEQ)
    `(LET ((,SEQ ,CHAR-SEQ))
       (TYPECASE ,SEQ
         (STRING     (COPY-SEQ ,SEQ))
         (LIST       (DO ((RESULT (MAKE-STRING (LENGTH ,SEQ)))
                          (I 0 (1+ I))
                          (SEQU ,SEQ  (CDR SEQU)))
                         ((NULL SEQU) RESULT)
                       (SETF (CHAR RESULT I) (CHARACTER (CAR SEQU)))))
         (OTHERWISE  (DO ((RESULT (MAKE-STRING (LENGTH ,SEQ)))
                          (I 0 (1+ I))
                          (MAX (LENGTH ,SEQ)))
                         ((>= I MAX) RESULT)
                       (SETF (CHAR RESULT I) (CHARACTER (AREF ,SEQ I)))))) )))


(DEFUN SPLIT-ESCAPED-STRING (STRING ESCAPE SEPARATOR)
  "
DO:      Split the string on the separator character.
         It may be escaped with the escape character.
RETURN:  A list of substrings of string.
"
  (UNLESS (SIMPLE-STRING-P STRING)  (SETF STRING    (COPY-SEQ STRING)))
  (UNLESS (CHARACTERP ESCAPE)       (SETF ESCAPE    (CHARACTER ESCAPE)))
  (UNLESS (CHARACTERP SEPARATOR)    (SETF SEPARATOR (CHARACTER SEPARATOR)))
  (DO ((RESULT   ())
       (ESCAPED  NIL)
       (START    0)
       (CURR     0))
      ((>= CURR (LENGTH STRING))
       (IF (AND (NULL RESULT) (= 0 CURR))
           NIL
           (PROGN (PUSH (SUBSEQ STRING START CURR) RESULT)
                  (NREVERSE RESULT))))
    (IF ESCAPED
        (PROGN (INCF CURR)
               (SETF ESCAPED NIL))
        (COND
          ((CHAR= (AREF STRING CURR) ESCAPE)   (INCF CURR) (SETF ESCAPED T))
          ((CHAR= (AREF STRING CURR) SEPARATOR)
           (PUSH (SUBSEQ STRING START CURR) RESULT)
           (INCF CURR)
           (SETF START CURR))
          (T (INCF CURR))))))


(DEFUN SPLIT-STRING (STRING &OPTIONAL (SEPARATORS " "))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (UNLESS (SIMPLE-STRING-P STRING)     (SETQ STRING     (COPY-SEQ STRING)))
  (UNLESS (SIMPLE-STRING-P SEPARATORS) (SETQ SEPARATORS (COPY-SEQ SEPARATORS)))
  (LET ((CHUNKS  '())
        (POSITION 0)
        (NEXTPOS  0)
        (STRLEN   (LENGTH STRING)) )
    (DECLARE (TYPE SIMPLE-STRING STRING SEPARATORS))
    (LOOP WHILE (< POSITION STRLEN)
       DO
       (LOOP WHILE (AND (< NEXTPOS STRLEN)
                        (NOT (POSITION (CHAR STRING NEXTPOS) SEPARATORS)))
          DO (SETQ NEXTPOS (1+ NEXTPOS))
          )
       (PUSH (SUBSEQ STRING POSITION NEXTPOS) CHUNKS)
       (SETQ POSITION (1+ NEXTPOS))
       (SETQ NEXTPOS  POSITION)
       )
    (NREVERSE CHUNKS)))


(DEFUN UNSPLIT-STRING (STRING-LIST &OPTIONAL SEPARATOR)
  "
DO:         The inverse than split-string.
            If no separator is provided then a simple space is used.
SEPARATOR:  (OR NULL STRINGP CHARACTERP)
"
  (COND
    ((NULL SEPARATOR)
     (SETQ SEPARATOR " "))
    ((CHARACTERP SEPARATOR)
     (SETQ SEPARATOR (MAKE-STRING 1 :INITIAL-ELEMENT SEPARATOR)))
    ((NOT (STRINGP SEPARATOR))
     (ERROR "unsplit-string: separator must be a string or a char.")))
  (APPLY 'CONCATENATE 'STRING (LIST-INSERT-SEPARATOR STRING-LIST SEPARATOR)))


(defun string-replace (string pattern replace &key (test (function char=)))
  "
RETURN:   A string build from STRING where all occurences of PATTERN 
          are replaced by the REPLACE string.
TEST:     The function used to compare the elements of the PATTERN
          with the elements of the STRING.
"
  (concatenate-strings
   (loop
      :with pattern-length = (length pattern)
      :for start = 0 :then (+ pos pattern-length)
      :for pos = (search pattern string :start2 start :test test)
      :if pos :collect (list string start pos) 
      :and    :collect replace
      :else   :collect (list string start)
      :while pos))) 



(DEFUN SPLIT-NAME-VALUE (STRING)
  "
RETURN:  a cons with two substrings of string such as:
         (string= (concat (car res) \"=\" (cdr res)) string)
         and (length (car res)) is minimum.
"
  (UNLESS (SIMPLE-STRING-P STRING) (SETQ STRING (COPY-SEQ STRING)))
  (LET ((POSITION 0)
        (STRLEN   (LENGTH STRING)) )
    (DECLARE (TYPE SIMPLE-STRING STRING))
    (LOOP WHILE (AND (< POSITION STRLEN)
                     (CHAR/= (CHARACTER "=") (AREF STRING POSITION)))
       DO (SETQ POSITION (1+ POSITION)))
    (IF (< POSITION STRLEN)
        (CONS (SUBSEQ STRING 0 POSITION) (SUBSEQ STRING (1+ POSITION) STRLEN))
        NIL)))


(defun prefixp (prefix string &key (start 0) (end nil) (test (function char=)))
  "
PREFIX:  A sequence.
STRING:  A sequence.
START:   The start of the substring of STRING to consider. Default: 0.
END:     The end   of the substring of STRING to consider. Default: NIL.
TEST:    A function to compare the elements of the strings.
RETURN:  Whether PREFIX is a prefix of the (substring STRING START END).
"
  (let ((mis (mismatch prefix string :start2 start :end2 end :test test)))
    (or (null mis) (<= (length prefix) mis))))


(defun suffixp (suffix string &key (start 0) (end nil) (test (function char=)))
  "
SUFFIX:  A sequence.
STRING:  A sequence.
START:   The start of the substring of STRING to consider. Default: 0.
END:     The end   of the substring of STRING to consider. Default: NIL.
TEST:    A function to compare the elements of the strings.
RETURN:  Whether SUFFIX is a suffix of the (substring STRING START END).
"
  (zerop (or (mismatch suffix string :start2 start :end2 end :test test
                       :from-end t) 0)))


(defun string-pad (string length &key (padchar " ") (justification :left))
  "
PADCHAR:        A character designator (string, symbol or chararcter).
JUSTIFICATION:  :LEFT, :CENTER, or :RIGHT where to place the string.
DO:             Append the PADCHAR before, after or at both end of string
                to pad it to length.
RETURN:         A padded string.
"
  (let ((slen (length string)))
    (if (<= length slen)
        string
        (let ((result
               (MAKE-STRING length :INITIAL-ELEMENT 
                            (etypecase padchar
                              (character padchar)
                              (string (aref padchar 0))
                              (symbol (aref (string padchar) 0))))))
          (case justification
            ((:left)   (replace result string :start1 0))
            ((:right)  (replace result string :start1 (- length slen)))
            ((:center) (replace result string
                                :start1 (truncate (- length slen) 2)))
            (otherwise (error "Invalid justification parameter: ~S (should be ~
                              :LEFT, :CENTER, or :RIGHT)" 
                              justification)))))))

;; Found on cll or #lisp?
;; ;; --> justify
;; (defun wrap-long-string (long-string column-width)
;;   (let ((cw (list column-width)))
;;     (setf (cdr cw) cw)
;;     (format nil "~{~<~%~1,v:;~a~>~^ ~}"
;;            (mapcan #'list cw (split-sequence long-string #\Space)))))


(defun string-justify-left (string &optional (width 72) (left-margin 0))
  "
RETURN: a left-justified string built from string. 
NOTE:   The default width is 72 characters, the default left-margin is 0. 
        The width is counted from column 0.
        The word separators are those of split-string: [ \\f\\t\\n\\r\\v]+, 
        which means that the string is justified as one paragraph."
  (unless (stringp string)
    (error "STRING-JUSTIFY-LEFT: The first argument must be a STRING."))
  (unless (and (integerp width) (integerp left-margin))
    (error "STRING-JUSTIFY-LEFT: The optional arguments must be INTEGER."))
  (let* ((margin (make-string left-margin :initial-element (character " ")))
         (splited (delete "" (split-string
                              string
                              (map 'vector (function code-char)
                                   (list (char-code #\space) ecma048:ht
                                         ecma048:cr ecma048:lf
                                         ecma048:ff ecma048:vt)))
                          :test (function string=)))
         (col left-margin)
         (justified (list (subseq margin 0 col)))
         (separator ""))
    (dolist (word splited)
      (when (< 0 (length word))
        (if (<= width (+ col (length word)))
            (progn (push #(#\newline) justified)
                   (push margin justified)
                   (push word justified)
                   (setf col (+ left-margin (length word))))
            (progn (push separator justified)
                   (push word justified)
                   (incf col (+ 1 (length word))))))
      (setf separator " "))
    ;; Pad with spaces up to width.
    (when (< col width)
      (push (make-string (- width col) :initial-element (character " "))
            justified))
    (apply (function concatenate) 'string (nreverse justified))))


(DEFMACRO DEFTRANSLATION (TABLE TEXT LANGUAGE TRANSLATION
                                &REST LANGS-TRANS)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (boundp (quote ,table))
       (DEFVAR ,TABLE (MAKE-HASH-TABLE :TEST (FUNCTION EQUAL))))
     (SETF ,@(DO ((LT (list* LANGUAGE TRANSLATION LANGS-TRANS))
                  (RESULT '()))
                 ((NULL LT) (NREVERSE RESULT))
                 (PUSH  `(GETHASH (CONS ,TEXT ,(POP LT)) ,TABLE) RESULT)
                 (push (LET ((TRANS (POP LT)))
                         (IF (EQ TRANS :IDEM)
                             `,TEXT
                             `,TRANS)) result)))))


(DEFUN LOCALIZE (TABLE LANGUAGE TEXT)
  "
RETURN: A version of the TEXT in the given LANGUAGE, 
        or in english if LANGUAGE is not found,
        or TEXT itself if none found.
"
  (or (gethash (cons text language) table)
      (gethash (cons text :en) table)
      text))


