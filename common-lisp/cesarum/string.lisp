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
;;;;    Copyright Pascal J. Bourguignon 2002 - 2013
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
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export
   "STRING-DESIGNATOR" "CHARACTER-DESIGNATOR"
   "NO-LOWER-CASE-P" "NO-UPPER-CASE-P" "MIXED-CASE-P"
   "LOCALIZE" "DEFTRANSLATION" "STRING-JUSTIFY-LEFT" "STRING-PAD"
   "PREFIXP" "SUFFIXP"
   "SPLIT-NAME-VALUE" "STRING-REPLACE" "UNSPLIT-STRING" "SPLIT-STRING"
   "SPLIT-ESCAPED-STRING" "IMPLODE-STRING" "EXPLODE-STRING"
   "IMPLODE" "EXPLODE"
   "CONCATENATE-STRINGS")
  (:documentation
   "

This package exports some string processing functions.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2002 - 2013
    
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
  (let ((*compile-verbose* nil))
    (com.informatimago.common-lisp.cesarum.ecma048:generate-all-functions-in-ecma048)))



(defun symbol-of-name-of-length=1 (object)
  "PREDICATE of symbols of name of length = 1"
  (and (symbolp object)
       (= 1 (length (symbol-name object)))))

(defun symbol-of-name-of-length=n (n)
  "RETURN: A symbol naming a predicate for a symbol of name of length = N."
  (flet ((predicate (object)
           (and (symbolp object)
                (= n (length (symbol-name object))))))
    (let ((name (gensym)))
      (setf (symbol-function name) (function predicate))
      name)))

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
    ((1)       '(or character (string 1) (satisfies symbol-of-name-of-length=1)))
    (otherwise `(or (string ,length) (satisfies ,(symbol-of-name-of-length=n length))))))


(defun test/string-designator ()
  (assert (typep "toto" 'string-designator))
  (assert (typep 'toto  'string-designator))
  (assert (typep #\t    'string-designator))
  (assert (not (typep 42 'string-designator)))
  (assert (not (typep #(#\a #\b) 'string-designator)))
  (assert (not (typep '(#\a #\b) 'string-designator)))
  (assert (typep "t"    '(string-designator 1)))
  (assert (typep 't     '(string-designator 1)))
  (assert (typep #\t    '(string-designator 1)))
  (assert (typep "toto" '(string-designator 4)))
  (assert (typep 'toto  '(string-designator 4)))
  (assert (not (typep "toto" '(string-designator 2))))
  (assert (not (typep 'toto  '(string-designator 2))))
  (assert (not (typep #\t    '(string-designator 2))))
  (assert (not (typep 42 '(string-designator 1))))
  (assert (not (typep #(#\a #\b) '(string-designator 2))))
  (assert (not (typep '(#\a #\b) '(string-designator 2))))
  :success)


(deftype character-designator ()
  "
CHARACTER-DESIGNATOR is the type of character or designators of
                     strings of length 1.
"
  ;; note: (subtypep 'character '(string-designator 1)), but it's
  ;; expected to be more efficient this way:
  
  '(or character (string-designator 1)))



(defun test/character-designator ()
  (assert (typep "t"    'character-designator))
  (assert (typep 't     'character-designator))
  (assert (typep #\t    'character-designator))
  (assert (not (typep "toto" 'character-designator)))
  (assert (not (typep 'toto  'character-designator)))
  (assert (not (typep 42     'character-designator)))
  (assert (not (typep #(#\a) 'character-designator)))
  (assert (not (typep '(#\a) 'character-designator)))
  :success)



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


(defun test/concatenate-strings ()
  (assert (equal "" (concatenate-strings '())))
  (assert (equal "" (concatenate-strings '(""))))
  (assert (equal "" (concatenate-strings '("" "" ""))))
  (assert (equal "" (concatenate-strings '(("" 0 0) ("abc" 0 0) ("abc" 1 1) (#\a 0 0)))))
  (assert (equal "abc" (concatenate-strings '("abc"))))
  (assert (equal "abc" (concatenate-strings '("a" "b" "c"))))
  (assert (equal "abc" (concatenate-strings '(#\a #\b #\c))))
  (assert (equal "abc" (concatenate-strings '(|a| |b| |c|))))
  (assert (equal "abc" (concatenate-strings '(|a| "b" #\c))))
  (assert (equal "abcdef" (concatenate-strings '("ab" "cd" "ef"))))
  (assert (equal "abcdef" (concatenate-strings '(("abcdef" 0 2) ("abcdef" 2 4) ("abcdef" 4 6)))))
  (assert (equal "abcdef" (concatenate-strings '(#\a #\b #\c "def"))))
  :succes)






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


(defun test/implode-explode ()
  ;; implode-string
  (assert (string= "" (implode-string "")))
  (assert (string= "" (implode-string #())))
  (assert (string= "" (implode-string '())))
  #-sbcl (assert (null (ignore-errors (implode-string 42))))
  (assert (string= "ABC" (implode-string "ABC")))
  (assert (string= "ABC" (implode-string #(#\A #\B #\C))))
  (assert (string= "ABC" (implode-string '(#\A #\B #\C))))
  (assert (null (ignore-errors (implode-string '(42)))))
  ;; explode-string
  (assert (eq      (explode-string "")         nil))
  (assert (eq      (explode-string "" 'list)   nil))
  (assert (string= (explode-string "" 'string) ""))
  (assert (equalp  (explode-string "" 'vector) #()))
  (assert (equal  (explode-string "ABC")       '(#\A #\B #\C)))
  (assert (equal  (explode-string "ABC" 'list) '(#\A #\B #\C)))
  (assert (and  (every 'char= (explode-string "ABC" 'vector) #(#\A #\B #\C))
                (= (length (explode-string "ABC" 'vector)) (length  #(#\A #\B #\C)))))
  (assert (string= (explode-string "ABC" 'string) "ABC"))
  ;; implode a string
  (assert (eq      (implode "" 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING") '||))
  (assert (eq      (implode "ABC" 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING") 'ABC))
  (assert (eq      (implode "ABC" 'symbol :keyword) ':ABC))
  (assert (string= (implode "" 'string) ""))
  (assert (string= (implode "ABC" 'string) "ABC"))
  (assert (equal   (implode "(1 2 3)" 'list) '(1 2 3))) 
  (assert (equal   (implode "NIL" 'list) '()))
  (assert (equalp  (implode "#(1 2 3)" 'vector) #(1 2 3))) 
  ;; implode a vector
  (assert (eq      (implode #() 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING") '||))
  (assert (eq      (implode #(#\A #\B #\C) 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING") 'ABC))
  (assert (eq      (implode #(#\A #\B #\C) 'symbol :keyword) ':ABC))
  (assert (string= (implode #() 'string) ""))
  (assert (string= (implode #(#\A #\B #\C) 'string) "ABC"))
  (assert (equal   (implode #(#\( #\1 #\space #\2  #\space #\3 #\)) 'list) '(1 2 3))) 
  (assert (equal   (implode #(#\N #\I #\L) 'list) '()))
  (assert (equalp  (implode #(#\# #\( #\1 #\space #\2  #\space #\3 #\)) 'vector) #(1 2 3))) 
  ;; implode a list
  (assert (eq      (implode '() 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING") '||))
  (assert (eq      (implode '(#\A #\B #\C) 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING") 'ABC))
  (assert (eq      (implode '(#\A #\B #\C) 'symbol :keyword) ':ABC))
  (assert (string= (implode '() 'string) ""))
  (assert (string= (implode '(#\A #\B #\C) 'string) "ABC"))
  (assert (equal   (implode '(#\( #\1 #\space #\2  #\space #\3 #\)) 'list) '(1 2 3))) 
  (assert (equal   (implode '(#\N #\I #\L) 'list) '()))
  (assert (equalp  (implode '(#\# #\( #\1 #\space #\2  #\space #\3 #\)) 'vector) #(1 2 3)))
  ;; explode
  (assert (equal  (explode 'hello) '(#\H #\E #\L #\L #\O)))
  (assert (equal  (explode 'hello 'list) '(#\H #\E #\L #\L #\O)))
  (assert (equalp (explode 'hello 'vector) #(#\H #\E #\L #\L #\O)))
  (assert (equalp (explode 'hello 'string) "HELLO"))
  (assert (equal  (explode "HELLO") '(#\H #\E #\L #\L #\O)))
  (assert (equal  (explode "HELLO" 'list) '(#\H #\E #\L #\L #\O)))
  (assert (equalp (explode "HELLO" 'vector) #(#\H #\E #\L #\L #\O)))
  (assert (equalp (explode "HELLO" 'string) "HELLO"))
  (assert (equalp (explode #(#\H #\E #\L #\L #\O)) '(#\H #\E #\L #\L #\O)))
  (assert (equalp (explode #(#\H #\E #\L #\L #\O) 'list) '(#\H #\E #\L #\L #\O)))
  (assert (equalp (explode #(#\H #\E #\L #\L #\O) 'vector) #(#\H #\E #\L #\L #\O)))
  (assert (equalp (explode #(#\H #\E #\L #\L #\O) 'string) "HELLO"))
  :success)


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


(defun test ()
  (test/string-designator)
  (test/character-designator)
  (test/concatenate-strings)
  (test/implode-explode))

(test)


;;;; THE END ;;;;
