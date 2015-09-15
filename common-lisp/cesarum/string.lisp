;;;; -*- coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:              string.lisp
;;;;LANGUAGE:          common-lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This package exports some string and string-designator utility functions.
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-09-15 <PJB> prefixp and suffixp moved to sequence,
;;;;                     became generic functions; added methods for
;;;;                     string designators.
;;;;    2013-07-02 <PJB> Added designator types, upgraded some
;;;;                     functions to take more specifically string or
;;;;                     character designators. Added some tests.
;;;;    2012-08-10 <PJB> Improved split-string and string-justify-left.
;;;;    2006-10-20 <PJB> Moved displaced-vector to
;;;;                     com.informatimago.common-lisp.cesarum.array.
;;;;    2005-09-01 <PJB> Made use of ISO6429 to improve portability.
;;;;    2004-10-15 <PJB> Added STRING-JUSTIFY-LEFT.
;;;;    2004-10-14 <PJB> Added STRING-PAD, DEFTRANSLATION and LOCALIZE.
;;;;    2004-03-31 <PJB> Added SPLIT-ESCAPED-STRING.
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2002 - 2015
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
;;;;*****************************************************************************

(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048"))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE")
  (:export
   "STRING-DESIGNATOR" "CHARACTER-DESIGNATOR"
   "NO-LOWER-CASE-P" "NO-UPPER-CASE-P" "MIXED-CASE-P"
   "LOCALIZE" "DEFTRANSLATION" "STRING-JUSTIFY-LEFT" "STRING-PAD"
   "PREFIXP" "SUFFIXP"
   "SPLIT-NAME-VALUE" "STRING-REPLACE" "UNSPLIT-STRING" "SPLIT-STRING"
   "SPLIT-ESCAPED-STRING" "IMPLODE-STRING" "EXPLODE-STRING"
   "IMPLODE" "EXPLODE"
   "CONCATENATE-STRINGS"
   "MAPCONCAT")
  (:documentation
   "

This package exports some string processing functions.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2002 - 2015
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun symbol-of-name-of-length=n (n)
    "RETURN: A symbol naming a predicate for a symbol of name of length = N."
    #-(and) (flet ((predicate (object)
                     (and (symbolp object)
                          (= n (length (symbol-name object))))))
              (let ((name (gensym "symbol-of-name-of-length=n-predicate")))        
                (setf (symbol-function name) (function predicate))
                name))
    #-(and) (eval `(defun ,(gensym "symbol-of-name-of-length=n-predicate") (object)
                     (and (symbolp object)
                          (= ,n (length (symbol-name object))))))
    (intern (format nil "SYMBOL-OF-NAME-OF-LENGTH=~D-P" n)
            (load-time-value (or (find-package #1="COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
                                 (error "Could not find the package named ~S" '#1#)))))

  );;eval-when

(defmacro define-string-designator-satisfies-function (n)
  `(eval-when  (:compile-toplevel :load-toplevel :execute)
     (defun ,(symbol-of-name-of-length=n n)
         (object)
       (and (symbolp object)
            (= ,n (length (symbol-name object)))))))

(define-string-designator-satisfies-function 1)
(define-string-designator-satisfies-function 2)
(define-string-designator-satisfies-function 4)

(deftype string-designator (&optional length)
  "
STRING-DESIGNATOR       is the type of string designators.
\(STRING-DESIGNATOR n)  is the type of string designators of strings of length n.

NOTE:    characters are all designators of strings of length 1,
         therefore (STRING-DESIGNATOR n) with n/=1 doesn't designate a
         CHARACTER.
"
  (case length
    ;; sbcl binds * to length for 'string-designator ; is this conforming?
    ((nil *)   '(or character string symbol))
    ((1)       `(or character (string 1) (satisfies ,(symbol-of-name-of-length=n 1))))
    (otherwise `(or (string ,length) (satisfies ,(symbol-of-name-of-length=n length))))))



(deftype character-designator ()
  "
CHARACTER-DESIGNATOR is the type of character or designators of
                     strings of length 1.
"
  ;; note: (subtypep 'character '(string-designator 1)), but it's
  ;; expected to be more efficient this way:
  
  '(or character (string-designator 1)))



(defun mapconcat (function sequence separator)
  "

FUNCTION:   This function is applied on each element of sequence and
            shall return a string designator.

SEQUENCE:   A sequence.

SEPARATOR:  A string designator.

RETURN:     A string containing the concatenation of the strings
            designated by the results of FUNCTION applied on each
            element of SEQUENCE, with SEPARATOR inserted between each
            of them.

"
  (let* ((strings   (map (if (vectorp sequence)
                             'vector
                             'list)
                      (lambda (item) (string (funcall function item)))
                      sequence))
         (separator (string separator))
         (seplen (length separator))
         (totlen (if (zerop (length strings))
                     0
                     (+ (reduce (function +) strings :key (function length) :initial-value 0)
                        (* seplen (1- (length strings))))))
         (result (make-string totlen)))
    (let ((start 0))
      (map nil (lambda (string)
                 (replace result string :start1 start)
                 (incf start (length string))
                 (unless (<= totlen start)
                   (replace result separator :start1 start)
                   (incf start seplen)))
        strings))
    result))

(defun concatenate-strings (list-of-string-designators)
  "
LIST-OF-STRING-DESIGNATORS:
                 EACH element may be either a string-designator,
                 or a list containing a string-designator, and a start and end position
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
      :with strings = (mapcar (lambda (item)
                                (if (consp item)
                                    (list (string (first item))
                                          (second item)
                                          (third item))
                                    (string item)))
                              list-of-string-designators)
      :with result = (make-string (reduce (function +) strings :key (function slength)))
      :for pos = 0
      :then (+ pos (slength string))
      :for string :in strings
      :do (if (stringp string)
              (replace result string :start1 pos)
              (replace result (first string) :start1 pos
                       :start2 (second string) :end2 (third string)))
      :finally (return result))))




(defun explode-string (character-designators &optional (result-type 'list))
  "
RETURN:         A new sequence of type RESULT-TYPE, containing the
                characters in the sequence CHARACTER-DESIGNATORS.
RESULT-TYPE:    A sequence type accepted by MAP.  Default: LIST.
"
  (check-type character-designators sequence)
  (map result-type (function character) character-designators))


(defun implode-string (character-designators)
  "
RETURN: A new string containing the characters in the sequence CHARACTER-DESIGNATORS.
NOTE:   (implode-string cds) == (explode-string cds 'string)
"
  (check-type character-designators sequence)
  (map 'string (function character) character-designators))


(define-compiler-macro implode-string (&whole form  character-designators)
  "
RETURN:  An optimized form for compiled code.
NOTE:    Unfortunately some implementations don't take into account
         compiler-macros even when compiling.
"
  (declare (ignorable form))
  (with-gensyms (seq)
    `(let ((,seq ,character-designators))
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




(defgeneric explode (object &optional result-type)
  (:documentation "
RETURN:         A sequence of character of type RESULT-TYPE containing
                the character of the OBJECT.
RESULT-TYPE:    A sequence type accepted by MAP (not NIL). Default: LIST.
OBJECT:         Can be a string, a symbol (its symbol-name is exploded),
                or a random object (its prin1 representation is exploded).
")
  (:method ((object symbol) &optional (result-type 'list))
    (explode-string (symbol-name object) result-type))
  (:method ((object string) &optional (result-type 'list))
    (explode-string object result-type))
  (:method ((object vector) &optional (result-type 'list))
    (explode-string (implode-string object) result-type))
  (:method ((object t) &optional (result-type 'list))
    (explode-string (prin1-to-string object) result-type)))
 

(defun implode (char-seq &optional (result-type 'symbol) (package *package*))
  "
RETURN:         An object of type RESULT-TYPE made with the character
                in the CHAR-SEQ sequence. Default: SYMBOL.
RESULT-TYPE:    SYMBOL (default), or STRING, or another type, in which
                case the object is obtained from reading from the
                string obtained from the implosion of the characters
                CHAR-SEQ.
PACKAGE:        When RESULT-TYPE is SYMBOL, then the package where the
                symbol is interned. Default: *PACKAGE*.
"
  (case result-type
    (string (implode-string char-seq))
    (symbol (intern (implode-string char-seq) package))
    (otherwise  (let ((object (read-from-string (implode-string char-seq))))
                  (assert (typep object result-type) ()
                          "~S is of type ~S which is not the expected type ~S"
                          object (type-of object) result-type)
                  object))))


(defun split-escaped-string (string-designator escape separator)
  "
STRING-DESIGNATOR:  A string designator.
ESCAPE:             A character designator.
SEPARATOR:          A character designator.
DO:                 Split the STRING-DESIGNATOR on the SEPARATOR
                    character.  It may be escaped with the ESCAPE
                    character, in which case it's not split.
RETURN:             A list of substrings of the string denoted by
                    STRING-DESIGNATOR.  
"
  (let ((string    (string string-designator))
        (escape    (character escape))
        (separator (character separator)))
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
            (t (incf curr)))))))


(defun split-string (string &optional (separators " ") (remove-empty nil))
  "
STRING:         A sequence.

SEPARATOR:      A sequence.

RETURN:         A list of subsequence of STRING, split upon any element of SEPARATORS.
                Separators are compared to elements of the STRING with EQL.

NOTE:           It's actually a simple split-sequence now.

EXAMPLES:       (split-string '(1 2 0 3 4 5 0 6 7 8 0 9) '(0))
                --> ((1 2) (3 4 5) (6 7 8) (9))
                (split-string #(1 2 0 3 4 5 0 6 7 8 0 9) #(0))
                --> (#(1 2) #(3 4 5) #(6 7 8) #(9))
                (split-string \"1 2 0 3 4 5 0 6 7 8\" '(#\space #\0))
                --> (\"1\" \"2\" \"\" \"\" \"3\" \"4\" \"5\" \"\" \"\" \"6\" \"7\" \"8\")
"
  (loop
    :with strlen = (length string)
    :for position = 0 :then (1+ nextpos)
    :for nextpos = (position-if (lambda (e) (find e separators)) string :start position)
    :unless (and remove-empty
                 (or (and (= position strlen) (null nextpos ))
                     (eql position nextpos)))
    :collect (subseq string position nextpos)
    :while (and nextpos (< position strlen))))


(defun unsplit-string (string-list separator &key (adjustable nil) (fill-pointer nil) (size-increment 0))
  "
DO:             The inverse than split-string.
SEPARATOR:      (OR STRINGP CHARACTERP)
ADJUSTABLE:     Create the string as an adjustable array.
FILL-POINTER:   Add a fill pointer to the string.
SIZE-INCREMENT: Add it to the size needed for the result.
"
  (check-type separator (or character string))
  (if string-list
      (let* ((separator (string separator))
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


;; Methods for strings provide a different default test.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun combinations (n elements)
    (if (zerop n)
        '(())
        (mapcan (lambda (subcombs)
                  (mapcar (lambda (element)
                            (cons element subcombs))
                          elements))
                (combinations (1- n) elements)))))

(defmacro define-string-designator-methods (name (&rest lambda-list) (&rest string-designator-parameters)
                                            &body body)
  (flet ((substitute-parameters (lambda-list parameters)
           (mapcar (lambda (formal-parameter)
                     (if (atom formal-parameter)
                         (let ((typed-parameter (assoc formal-parameter parameters)))
                           (or typed-parameter formal-parameter))
                         formal-parameter))
                   lambda-list)))
    `(progn
       ,@(loop
           :for comb :in (combinations (length string-designator-parameters)
                                       '(character symbol string))
           :unless (every (lambda (class) (eql class 'string)) comb)
             :collect `(defmethod ,name
                           ,(substitute-parameters lambda-list
                             (mapcar (function list) string-designator-parameters comb))
                         ,@body))
       (defmethod ,name ,(substitute-parameters lambda-list
                          (mapcar (lambda (parameter) (list parameter 'string))
                           string-designator-parameters))
         (call-next-method)))))

(define-string-designator-methods prefixp (prefix sequence &key (start 0) (end nil) (test (function char=)))
    (prefix sequence)
  (prefixp (string prefix) (string sequence) :start start :end end :test test))

(define-string-designator-methods suffixp (suffix sequence &key (start 0) (end nil) (test (function char=)))
    (suffix sequence)
  (suffixp (string suffix) (string sequence) :start start :end end :test test))




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


(defun string-justify-left (string &optional (width 72) (left-margin 0) (separators #(#\Space #\Newline)))
  "
RETURN:         A left-justified string built from string.

WIDTH:          The maximum width of the generated lines.  Default is 72 characters.

LEFT-MARGIN:    The left margin, filled with spaces.  Default is 0 characters.

SEPARATORS:     A sequence containing the characters on which to split the words.
                Default: #\(#\space #\newline).
"
  (check-type string string)
  (check-type width integer)
  (check-type left-margin integer)
  (let* ((margin    (make-string left-margin :initial-element (character " ")))
         (splited   (split-string string separators t))
         (col       left-margin)
         (justified (list (subseq margin 0 col)))
         (separator ""))
    (dolist (word splited)
      (if (<= width (+ col (length word)))
          (progn (push #(#\newline) justified)
                 (push margin justified)
                 (push word justified)
                 (setf col (+ left-margin (length word))))
          (progn (push separator justified)
                 (push word justified)
                 (incf col (+ 1 (length word)))))
      (setf separator " "))
    ;; ;; Pad with spaces up to width.
    ;; (when (< col width)
    ;;   (push (make-string (- width col) :initial-element (character " "))
    ;;         justified))
    (apply (function concatenate) 'string (nreverse justified))))


(defun no-lower-case-p (string-designator)
  "
RETURN:         Whether the string denoted by STRING-DESIGNATOR
                contains no lower case character.
"
  (notany (function lower-case-p) (string string-designator)))


(defun no-upper-case-p (string-designator)
  "
RETURN:         Whether the string denoted by STRING-DESIGNATOR
                contains no upper case character.
"
  (notany (function upper-case-p) (string string-designator)))


(defun mixed-case-p (string-designator)
  "
RETURN:         Whether the string denoted by STRING-DESIGNATOR
                contains both upper case characters and lower case
                characters.  If there are characters that are
                both-case-p, then mixed-case-p returns true.
"
  (let ((string (string string-designator)))
    (and (some (function upper-case-p) string)
         (some (function lower-case-p) string))))



(defmacro deftranslation (table text language translation
                                &rest langs-trans)
  "
DO:             Define a translation table.
TABLE:          A symbol naming a variable to be bound to the
                translation table (with defvar).
TEXT:           A string containing the localizable text.
LANGUAGE:       A keyword denoting a language.
TRANSLATION:    A translation of the TEXT in the LANGUAGE.
LANGS-TRANS:    Other couples language translation.
EXAMPLE:        (deftranslation *words* \"car\" :fr \"automobile\"
                                                :es \"coche\")
                (localize *words* :fr \"car\")
                --> \"automobile\"
SEE ALSO:       LOCALIZE
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defvar ,table (make-hash-table :test (function equal)))
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
RETURN:     A version of the TEXT in the given LANGUAGE, 
            or in english if LANGUAGE is not found,
            or TEXT itself if none found.
SEE ALSO:   DEFTRANSLATION
"
  (or (gethash (cons text language) table)
      (gethash (cons text :en) table)
      text))


;;;; THE END ;;;;
