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
;;;;                     com.informatimago.common-lisp.cesarum.array.
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

(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048"))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export
   "LOCALIZE" "DEFTRANSLATION" "STRING-JUSTIFY-LEFT" "STRING-PAD"
   "PREFIXP" "SUFFIXP"
   "SPLIT-NAME-VALUE" "STRING-REPLACE" "UNSPLIT-STRING" "SPLIT-STRING"
   "SPLIT-ESCAPED-STRING" "IMPLODE-STRING" "EXPLODE-STRING"
   "CONCATENATE-STRINGS")
  (:documentation
   "This package exports some string processing functions.

    Copyright Pascal J. Bourguignon 2002 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")



(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-verbose* nil))
    (com.informatimago.common-lisp.cesarum.ecma048:generate-all-functions-in-ecma048)))


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


(defun explode-string (string)
  "
return a new list containing the character in the sequence string.
"
  (map 'list (function character) string))


(defun implode-string (char-seq)
  "
RETURN: A new string containing the characters in the sequence CHAR-SEQ.
"
  (map 'string (function character) char-seq))


(define-compiler-macro implode-string (&whole form  char-seq)
  "
RETURN:  An optimized form for compiled code.
NOTE:    Unfortunately clisp does to take into account compiler-macros
         even when compiling...
"
  (declare (ignorable form))
  (with-gensyms (seq)
    `(let ((,seq ,char-seq))
       (typecase ,seq
         (string     (copy-seq ,seq))
         (list       (do ((result (make-string (length ,seq)))
                          (i 0 (1+ i))
                          (sequ ,seq  (cdr sequ)))
                         ((null sequ) result)
                       (setf (char result i) (character (car sequ)))))
         (otherwise  (do ((result (make-string (length ,seq)))
                          (i 0 (1+ i))
                          (max (length ,seq)))
                         ((>= i max) result)
                       (setf (char result i) (character (aref ,seq i)))))) )))


(defun split-escaped-string (string escape separator)
  "
DO:      Split the string on the separator character.
         It may be escaped with the escape character.
RETURN:  A list of substrings of string.
"
  (unless (simple-string-p string)  (setf string    (copy-seq string)))
  (unless (characterp escape)       (setf escape    (character escape)))
  (unless (characterp separator)    (setf separator (character separator)))
  (do ((result   ())
       (escaped  nil)
       (start    0)
       (curr     0))
      ((>= curr (length string))
       (if (and (null result) (= 0 curr))
           nil
           (progn (push (subseq string start curr) result)
                  (nreverse result))))
    (if escaped
        (progn (incf curr)
               (setf escaped nil))
        (cond
          ((char= (aref string curr) escape)   (incf curr) (setf escaped t))
          ((char= (aref string curr) separator)
           (push (subseq string start curr) result)
           (incf curr)
           (setf start curr))
          (t (incf curr))))))


(defun split-string (string &optional (separators " "))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (let ((string     (if (simple-string-p string)
                        string
                        (copy-seq string)))
        (separators (if (simple-string-p separators)
                        separators
                        (copy-seq separators)))
        (chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)) )
    (declare (type simple-string string separators))
    (loop :while (< position strlen)
          :do (loop :while (and (< nextpos strlen)
                                (not (position (char string nextpos) separators)))
                    :do (setq nextpos (1+ nextpos)))
              (push (subseq string position nextpos) chunks)
              (setq position (1+ nextpos))
              (setq nextpos  position))
    (nreverse chunks)))


(defun unsplit-string (string-list &optional (separator " ")
                       &key (adjustable nil) (fill-pointer nil) (size-increment 0))
  "
DO:             The inverse than split-string.
                If no separator is provided then a simple space is used.
SEPARATOR:      (OR NULL STRINGP CHARACTERP)
ADJUSTABLE:     Create the string as an adjustable array.
FILL-POINTER:   Add a fill pointer to the string.
SIZE-INCREMENT: Add it to the size needed for the result.
"
  (if string-list
      (let* ((separator
              (cond
                ((null separator)        " ")
                ((characterp separator)  (make-string 1 :initial-element separator))
                ((not (stringp separator))
                 (error "~S: separator must be a string or a character, not a ~S."
                        'unsplit-string (type-of separator)))
                (t separator)))
             (seplen (length separator))
             (size   (+ (reduce (function +) string-list :key (function length))
                        (* seplen (1- (length string-list)))))
             (result (make-array (+ size-increment size)
                                 :element-type 'character
                                 :adjustable adjustable
                                 :fill-pointer (case fill-pointer
                                                 ((nil) nil)
                                                 ((t)   size)
                                                 (otherwise fill-pointer))))
             (start  (length (first string-list))))
        (replace result (first string-list))
        (dolist (string (rest string-list) result)
          (replace result separator :start1 start) (incf start seplen)
          (replace result string   :start1 start) (incf start (length string))))
      (make-array size-increment
                  :element-type 'character
                  :adjustable adjustable
                  :fill-pointer (if fill-pointer 0 nil))))


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



(defun split-name-value (string)
  "
RETURN:  a cons with two substrings of string such as:
         (string= (concat (car res) \"=\" (cdr res)) string)
         and (length (car res)) is minimum.
"
  
  (let ((string (if (simple-string-p string)
                    string
                    (copy-seq string)))
        (position 0)
        (strlen   (length string)) )
    (declare (type simple-string string))
    (loop :while (and (< position strlen)
                     (char/= (character "=") (aref string position)))
          :do (setq position (1+ position)))
    (if (< position strlen)
        (cons (subseq string 0 position) (subseq string (1+ position) strlen))
        nil)))


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
               (make-string length :initial-element 
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
                                   (list (char-code #\space)
                                         com.informatimago.common-lisp.cesarum.ecma048:ht
                                         com.informatimago.common-lisp.cesarum.ecma048:cr
                                         com.informatimago.common-lisp.cesarum.ecma048:lf
                                         com.informatimago.common-lisp.cesarum.ecma048:ff
                                         com.informatimago.common-lisp.cesarum.ecma048:vt)))
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


(defmacro deftranslation (table text language translation
                                &rest langs-trans)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (boundp (quote ,table))
       (defvar ,table (make-hash-table :test (function equal))))
     (setf ,@(do ((lt (list* language translation langs-trans))
                  (result '()))
                 ((null lt) (nreverse result))
                 (push  `(gethash (cons ,text ,(pop lt)) ,table) result)
                 (push (let ((trans (pop lt)))
                         (if (eq trans :idem)
                             `,text
                             `,trans)) result)))))


(defun localize (table language text)
  "
RETURN: A version of the TEXT in the given LANGUAGE, 
        or in english if LANGUAGE is not found,
        or TEXT itself if none found.
"
  (or (gethash (cons text language) table)
      (gethash (cons text :en) table)
      text))


;;;; THE END ;;;;
