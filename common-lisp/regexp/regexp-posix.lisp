;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               regexp-posix.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             UNIX
;;;;USER-INTERFACE:     UNIX
;;;;NOWEB:              t
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;USAGE
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-01-02 <PJB> Implemented POSIX regexp parser.
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
;;;;****************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export
   ;; CLISP REGEXP API:
   "REGEXP-MATCH" "REGEXP-QUOTE" "MATCH-STRING" "MATCH-END"
   "MATCH-START" "MATCH"
   ;; POSIX API:
   "REGEXEC" "REGCOMP" "RM-EO" "RM-SO" "REGMATCH-T"
   "RE-NSUB" "REGEX-T" "REGOFF-T" "SIZE-T")
  (:documentation
   "

NOT COMPLETE YET.

This package implement POSIX Regular Expressions in Common-Lisp.
This is interesting because it's available on any Common-Lisp platform
while external C regexp libraries or internals are available or not,
and not always implement these same syntax or semantic.


Posix Regexp implemented in Common-Lisp.

See specifications at:
http://www.opengroup.org/onlinepubs/007904975/basedefs/xbd_chap09.html

This is a strict implementation that will work both in clisp
(Common-Lisp) and emacs (with cl and pjb-cl Common-Lisp extensions).

This implementation is entirely in lisp, contrarily to what regexp
packages are available under clisp or emacs.  Thus it has the advantage
of portability and availability (you don't have to compile or link
a lisp system written in some barbarous language, and you get the same
regexp features in all programs including this module).


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2002 - 2012
    
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX.KEYWORD"
  (:nicknames "RK")
  (:use)
  (:export
   "COLLATING-SYMBOL" "EQUIVALENCE-CLASS" "CHARACTER-CLASS"
   "RANGE" "ANY" "L-ANCHOR" "R-ANCHOR" "MATCHING" "NON-MATCHING" "BACKREF"
   "SUBEXP" "SEQUENCE" "REPEAT" "REPEAT-SHY" "INFINITY" "ALTERNATIVE"
   "B-ANCHOR" "E-ANCHOR"
   "ITEM" "SET-SEQUENCE")
  (:documentation "
This package gathers and exports regexp keywords.

    ALTERNATIVE
    ANY
    BACKREF
    B-ANCHOR
    CHARACTER-CLASS
    COLLATING-SYMBOL
    E-ANCHOR
    EQUIVALENCE-CLASS
    INFINITY
    ITEM
    L-ANCHOR
    MATCHING
    NON-MATCHING
    R-ANCHOR
    RANGE
    REPEAT
    REPEAT-SHY
    SEQUENCE
    SET-SEQUENCE
    SUBEXP


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2002 - 2012
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX")

#||
regexp --> (or character
              rk:any
              rk:l-anchor
              rk:r-anchor
              (rk:matching mexpr...)
              (rk:non-matching mexpr...)
              (rk:backref  integer)
              (rk:subexp   regex)
              (rk:sequence regex...)
              (rk:repeat  integer (or integer rk:infinity) bexp)
              (rk:alternative regex...))

(rk:collating-symbol coll-elem)
(rk:equivalence-class coll-elem)
(rk:character-class class-name)
(rk:range start end)
||#


(defun lessp (a b)
  (if (or (eq a 'rk:infinity) (eq b 'rk:infinity))
      nil
      (< a b))) ;;LESSP



(defmacro if^2 (c1 c2 tt tf ft ff)  `(if ,c1 (if ,c2 ,tt ,tf) (if ,c2 ,ft ,ff)))
;; (if^2 cond1 cond2
;;      (t t)      (t f)
;;      (f t)      (f f))
(defmacro top (stack) `(car ,stack))


(defmacro invariant (condition &body body)
  `(progn (assert ,condition) (prog1 (progn ,@body) (assert ,condition))))




(defun pjb-re-split-string (string &optional separators)
  "
DOES:       Splits STRING into substrings where there are matches
            for SEPARATORS.
RETURNS:    A list of substrings.
separators: A regexp matching the sub-string separators.
            Defaults to \"[ \f\t\n\r\v]+\".
NOTE:       Current implementation only accepts as separators
            a literal string containing only one character.
"
  (let ((sep (aref separators 0))
        (chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string))
        )
    (loop while (< position strlen)
       do
       (loop while (and (< nextpos strlen)
                        (char/= sep (aref string nextpos)))
          do (setq nextpos (1+ nextpos)))
       (push (subseq string position nextpos) chunks)
       (setq position (1+ nextpos))
       (setq nextpos  position))
    (nreverse chunks)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String Scanner
;; --------------
;;


(defstruct sc
  (string           "" :type string)
  (position         0  :type integer)
  (bracket-position 0  :type integer)
  (expression-start () :type list))


(defun sc-length (sc) (length (sc-string sc)))
(defun sc-eos    (sc) (>= (sc-position sc) (sc-length sc)))


(defun sc-curr-char (sc)
  "
RETURN:  The current character, or nil if EOS.
"
  (if (sc-eos sc)
      nil
      (char (sc-string sc) (sc-position sc))))


(defun sc-next-char (sc)
  "
RETURN:  The next character, or nil if EOS.
"
  (if (< (1+ (sc-position sc)) (sc-length sc))
      (char (sc-string sc) (1+ (sc-position sc)))
      nil))


(defun sc-after-next-char (sc)
  "
RETURN:  The after next character, or nil if EOS.
"
  (if (< (+ 2 (sc-position sc)) (sc-length sc))
      (char (sc-string sc) (+ 2 (sc-position sc)))
      nil))


(defun sc-advance (sc &optional (increment 1))
  "
PRE:     (= p (sc-position sc))
POST:    (= (min (sc-length sc) (+ p increment)) (sc-position sc))
RETURN:  The character at position p+increment or nil if EOS.
"
  (setf (sc-position sc) (min (sc-length sc) (+ (sc-position sc) increment)))
  (sc-curr-char sc))


(defun sc-looking-at (sc substring)
  "
"
  (string= (sc-string sc) substring
           :start1 (sc-position sc)
           :end1   (min (sc-length sc) (+ (sc-position sc) (length substring)))))


(defun sc-scan-to (sc substring)
  "
RETURN:  the substring of (sc-string sc) starting from current position
         to the position just before the first occurence of the substring
         found from this position.

PRE:     (= p      (sc-position sc))
POST:    (and (<=  p (sc-position sc))
              (or (and (< (sc-position sc) (length (sc-string sc)))
                       (string= substring (substring (sc-string sc)
                                                      p (sc-position sc))))
                  (= (sc-position sc) (length (sc-string sc))))
              (forall i between p and (1- (sc-position sc))
                  (string/= substring (substring (sc-string sc)
                                          i (+ i (length substring))))))
"
  (let* ((start (sc-position sc))
         (end   (search substring (sc-string sc) :start2 start)))
    (if end
        (progn
          (setf (sc-position sc)  end)
          (subseq (sc-string sc) start end))
        nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regular Expression Parsing Utilities
;; ------------------------------------


(defun length>1 (list) (cdr list))
(defun errorp   (expr) (and (consp expr) (eq :error (car expr))))

(define-condition parsing-error (error)
  ((arguments :initarg :arguments :accessor parsing-error-arguments)))
(defun err (&rest args)
  ;; (error 'parsing-error :arguments (copy-list args))
  (cons :error (copy-list args)))

(defun re-integer (sc)
  "
DO:     Parses an integer.
RETURN: The integer, or NIL.
"
  (do ((start (sc-position sc)))
      ((or (sc-eos sc) (not (digit-char-p (sc-curr-char sc) 10)))
       (if (< start (sc-position sc))
           (parse-integer (sc-string sc) :start start :end (sc-position sc)
                          :radix 10 :junk-allowed nil)
           nil))
    (sc-advance sc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing: Bracket Expression
;; ---------------------------


(defun be-collating-symbol (sc)
  "
DO:     Parses a collating-symbol.
RETURN: (rk:collating-symbol coll-elem)
        or (:error message)
        or nil if not looking at '[='.
NOTE:
   collating_symbol : Open_dot COLL_ELEM_SINGLE Dot_close
                    | Open_dot COLL_ELEM_MULTI Dot_close
                    | Open_dot META_CHAR Dot_close ;
   COLL_ELEM_SINGLE and META_CHAR form a partition of all characters.
"
  (if (sc-looking-at sc "[.")
      (progn
        (sc-advance sc 2)
        (let ((collating-symbol (sc-scan-to sc ".]")))
          (if collating-symbol
              (progn
                (sc-advance sc 2)
                (list 'rk:collating-symbol collating-symbol))
              (err "Missing a closing '.]' after '[.'."))))
      nil)) ;;BE-COLLATING-SYMBOL


(defun be-equivalence-class (sc)
  "
DO:     Parses an equivalence class.
RETURN: (rk:equivalence-class coll-elem)
        or (:error message)
        or nil if not looking at '[='.
NOTE:
   equivalence_class : Open_equal COLL_ELEM_SINGLE Equal_close
                     | Open_equal COLL_ELEM_MULTI Equal_close ;

   META_CHAR
    One of the characters:
    ^   When found first in a bracket expression
    -   When found anywhere but first (after an initial '^' , if any)
        or last in a bracket expression, or as the ending range point
        in a range expression
    ]   When found anywhere but first (after an initial '^' , if any)
        in a bracket expression

   Inside an equivalence-class:
   ^ is not first in a bracket expression             ==> ^ is not meta-char
   ] is not first in a bracket expression                 ==> ] is meta-char
   - isn't first, shouldn't be last, isn't ending a range ==> - is meta-char
"
  (if (sc-looking-at sc "[=")
      (progn
        (sc-advance sc 2)
        (if (or (sc-looking-at sc "-") (sc-looking-at sc "]"))
            (err (format nil "Invalid meta-char ~S in equivalence class."
                         (sc-curr-char sc)))
            (let ((equivalence-class (sc-scan-to sc "=]")))
              (if equivalence-class
                  (progn
                    (sc-advance sc 2)
                    (list 'rk:equivalence-class equivalence-class))
                  (err "Missing a closing '=]' after '[='.")))))
      nil)) ;;BE-EQUIVALENCE-CLASS


(defun be-character-class (sc)
  "
DO:     Parses a character class
RETURN: (rk:character-class class-name)
        or (:error message)
        or nil if not looking at '[:'.
NOTES:
   character_class : Open_colon class_name Colon_close ;
"
  (if (sc-looking-at sc "[:")
      (progn
        (sc-advance sc 2)
        (let ((class-name (sc-scan-to sc ":]")))
          (if class-name
              (progn
                (sc-advance sc 2)
                (list 'rk:character-class class-name))
              (err "Missing a closing ':]' after '[:'."))))
      nil)) ;;BE-CHARACTER-CLASS


(defun be-end-range (sc)
  "
DO:     Parses an end-range.
RETURN: character or (rk:collating-symbol coll-elem)
NOTES:
   end_range      : COLL_ELEM_SINGLE
                  | collating_symbol ;
   COLL_ELEM_SINGLE
    Any single-character collating element, unless it is a META_CHAR.
   META_CHAR
    One of the characters:
    ^   When found first in a bracket expression
    -   When found anywhere but first (after an initial '^' , if any)
        or last in a bracket expression, or as the ending range point
        in a range expression
    ]   When found anywhere but first (after an initial '^' , if any)
        in a bracket expression
"
  (let ((coll-sym (be-collating-symbol sc)))
    (cond
      (coll-sym coll-sym)
      ((sc-eos sc) nil)
      ((and (= (sc-position sc) (sc-bracket-position sc))
            (sc-looking-at sc "-"))
       (prog1 (sc-curr-char sc) (sc-advance sc)))
      ((sc-looking-at sc "-]")
       (prog1 (sc-curr-char sc) (sc-advance sc)))
      ((sc-looking-at sc "-") nil)
      ((sc-looking-at sc "]") nil)
      (t
       (prog1 (sc-curr-char sc) (sc-advance sc))))))


(defun be-start-range (sc)
  "
DO:     Parses a start-range.
RETURN: character or (rk:collating-symbol coll-elem)
        or nil if not looking at a start-range.
NOTES:
   start_range    : end_range '-' ;
"
  (let ((start  (sc-position sc))
        (result (be-end-range sc)))
    (cond
      ((null result)          (setf (sc-position sc) start) nil)
      ((errorp result)                                      result)
      ((sc-looking-at sc "-") (sc-advance sc)               result)
      (t                      (setf (sc-position sc) start) nil))))


(defun be-range-expression (sc)
  "
DO:     Parses a range-expression.
RETURN: (rk:range start end) or nil of not looking at a range-expression.
NOTES:
   range_expression : start_range end_range
                    | start_range '-' ;
"
  (let ((start       (sc-position sc))
        (range-start (be-start-range sc)))
    (cond
      ((null range-start) nil)
      ((errorp range-start) range-start)
      ((sc-looking-at sc "-")
       (list 'rk:range range-start (character "-")))
      (t (let ((range-end (be-end-range sc)))
           (cond
             ((null range-end)   (setf (sc-position sc) start) nil)
             ((errorp range-end) range-end) ;; error or not error?
             (t (list 'rk:range range-start range-end))))))))


(defun be-single-expression (sc)
  "
DO:      Parses a single-expression.
RETURN:  (or (rk:equivalence-class ec) (rk:character-class cc)
             (rk:collating-symbol cs)  character
             nil)
NOTES:
   single_expression : end_range
                     | character_class
                     | equivalence_class ;
"
  (let ((start  (sc-position sc))
        (se     (be-character-class sc))
        (errpos nil)
        (err    nil))
    (cond
      ((null se)   (setf se  (be-equivalence-class sc)))
      ((errorp se) (setf errpos (or errpos (sc-position sc))
                         (sc-position sc) start
                         err (or err se)
                         se  (be-equivalence-class sc)))
      (t           (return-from be-single-expression se)))
    (cond
      ((null se)   (setf se  (be-end-range sc)))
      ((errorp se) (setf errpos (or errpos (sc-position sc))
                         (sc-position sc) start
                         err (or err se)
                         se  (be-end-range sc)))
      (t           (return-from be-single-expression se)))
    (cond
      ((null se)   nil)
      ((errorp se) (setf errpos (or errpos (sc-position sc))
                         err (or err se)
                         (sc-position sc) errpos)    err)
      (t           se))))


(defun be-expression-term (sc)
  "
DO:
RETURN:  (or (rk:equivalence-class ec) (rk:character-class cc)
             (rk:collating-symbol cs)  character
             (rk:range start end)
             nil)
NOTES:
   expression_term : single_expression
                   | range_expression ;
"
  (let ((start  (sc-position sc))
        (et     (be-range-expression sc))
        (errpos nil)
        (err    nil))
    (cond
      ((null et)   (setf et  (be-single-expression sc)))
      ((errorp et) (setf errpos (or errpos (sc-position sc))
                         (sc-position sc) start
                         err (or err et)
                         et  (be-single-expression sc)))
      (t           (return-from be-expression-term et)))
    (cond
      ((null et)   nil)
      ((errorp et) (setf errpos (or errpos (sc-position sc))
                         err (or err et)
                         (sc-position sc) errpos)    err)
      (t           et))))


(defun be-follow-list (sc)
  "
DO:
RETURN:  (:follow-list expression...)
         or (:error message)
         or  nil
   follow_list    :             expression_term
                  | follow_list expression_term  ;
"
  (do ((expression-term (be-expression-term sc) (be-expression-term sc))
       (follow-list     nil))
      ((or (null expression-term) (errorp expression-term))
       (if (errorp expression-term)
           expression-term ;; (:error ...)
           (and follow-list (cons :follow-list (nreverse follow-list)))))
    (push expression-term follow-list)))


(defun be-bracket-list (sc)
  "
DO:      Parses a bracket-list.
RETURN:  (:follow-list expression...) or nil.
NOTES:
   bracket_list   : follow_list
                  | follow_list '-' ;
"
  (let ((follow-list (be-follow-list sc)))
    (cond
      ((null follow-list) nil)
      ((errorp follow-list) follow-list)
      (t (or (and (sc-looking-at sc "-")
                  (prog1
                      (nconc follow-list (list (sc-curr-char sc)))
                    (sc-advance sc)))
             follow-list)))))


(defun be-bracket-expression (sc)
  "
DO:      Parses a bracket-expression.
RETURN:  (rk:matching expression...) or (rk:non-matching expression...)
         or (:error message) or nil.
NOTES:
   bracket_expression : '[' matching_list ']'
                      | '[' nonmatching_list ']' ;
   matching_list  : bracket_list  ;
   nonmatching_list : '^' bracket_list ;
"
  (let ((start (sc-position sc))
        be)
    (cond
      ((sc-looking-at sc "[^")
       (sc-advance sc 2)
       (setf be 'rk:non-matching))
      ((sc-looking-at sc "[")
       (sc-advance sc)
       (setf be 'rk:matching))
      (t (return-from be-bracket-expression nil)))
    (setf (sc-bracket-position  sc) (sc-position sc))
    (let ((matching-list (be-bracket-list sc)))
      (cond
        ((null matching-list)   (setf (sc-position sc) start) nil)
        ((errorp matching-list) matching-list)
        ((sc-looking-at sc "]")
         (sc-advance sc)
         (cons be (cdr matching-list)))
        (t (err "Missing ']' in bracket expression."))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Regular Expression
;; ------------------------


(defun bre-dupl-symbol (sc)
  "
DO:     Parse
RETURN: (rk:repeat min max) with min,max in (or rk:infinity (integer 0))
        or (:error message)
        or nil if not looking at a re-dupl-symbol.
NOTES:
RE_dupl_symbol : '*'
               | Back_open_brace DUP_COUNT               Back_close_brace
               | Back_open_brace DUP_COUNT ','           Back_close_brace
               | Back_open_brace DUP_COUNT ',' DUP_COUNT Back_close_brace ;
"
  (cond
    ((sc-looking-at sc "*") (sc-advance sc) (list 'rk:repeat 0 'rk:infinity))
    ((sc-looking-at sc "{")
     (sc-advance sc)
     (let ((min (re-integer sc)))
       (cond
         ((null min) (err "Missing duplication count after '{'."))
         ((sc-looking-at sc "}")  (sc-advance sc)  (list 'rk:repeat min min))
         ((sc-looking-at sc ",")  (sc-advance sc)
          (if (sc-looking-at sc "}")
              (progn  (sc-advance sc)  (list 'rk:repeat min 'rk:infinity))
              (let ((max (re-integer sc)))
                (cond
                  ((null max)
                   (err (format nil "Invalid ~S character in {...}."
                                        (sc-curr-char sc))))
                  ((sc-looking-at sc "}")
                   (sc-advance sc)
                   (list 'rk:repeat min max))
                  ((sc-eos sc)
                   (err "Missing '}'."))
                  (t (err (format nil "Invalid ~S character in {...}."
                                          (sc-curr-char sc))))))))
         ((sc-eos sc)            (err "Missing '}'."))
         (t (err (format nil "Invalid ~S character in {...}."
                         (sc-curr-char sc)))))
       ))
    (t nil)))


(defun bre-one-char-or-coll-elem (sc)
  "
DO:      Parses s single character or a coll-elem regexp.
RETURN:  (or (rk:matching ...) (rk:non-matching ...) rk:any character)
NOTES:
one_char_or_coll_elem_RE : ORD_CHAR
               | QUOTED_CHAR
               | '.'
               | bracket_expression ;
QUOTED_CHAR     \^    \.   \*    \[    \$    \\
ORD_CHAR        any but SPEC_CHAR
SPEC_CHAR
    For basic regular expressions, one of the following special characters:
    .    Anywhere outside bracket expressions
    \    Anywhere outside bracket expressions
    [    Anywhere outside bracket expressions
    ^    When used as an anchor (see BRE Expression Anchoring )
         or when first in a bracket expression
    $    When used as an anchor
    *    Anywhere except first in an entire RE,
         anywhere in a bracket expression, directly following '\(' ,
         directly following an anchoring '^'.
   ==> ORD_CHAR excludes . \ [ and * but when first of the expression.
"
  (cond
    ((sc-eos sc) nil)
    ;; quoted-char:
    ((and (char= (character "\\") (sc-curr-char sc))
          (position (sc-next-char sc) "^.*[$\\"))
     (sc-advance sc)
     (prog1 (sc-curr-char sc) (sc-advance sc)))
    ;; dot:
    ((sc-looking-at sc ".")
     (sc-advance sc)
     'rk:any)
    ;; bracket expression:
    ((sc-looking-at sc "[")
     (be-bracket-expression sc)) ;; [ is not an ord-char anyway.
    ;; not looking at one-char-or-coll-elem-re:
    ((or (sc-looking-at sc "\\")
         (sc-looking-at sc "[")
         (and (sc-expression-start sc)                 (sc-looking-at sc "*"))
         (and (= (1+ (sc-position sc)) (sc-length sc)) (sc-looking-at sc "$")))
     nil) ;; spec-char
    ((and (sc-expression-start sc)
          (or (sc-looking-at sc "^") (sc-looking-at sc "$")))
     (err (format nil "Invalid ~S character inside '\\(' and '\\)'."
                  (sc-curr-char sc))))
    (t
     (prog1  (sc-curr-char sc) (sc-advance sc)))))


(defun bre-nondupl-re (sc)
  "
nondupl_RE     : one_char_or_coll_elem_RE
               | Back_open_paren RE_expression Back_close_paren
               | BACKREF ;
"
  (cond
    ((sc-looking-at sc "\\(")
     (sc-advance sc 2)
     (push  (sc-position sc) (sc-expression-start sc))
     (let ((expression (prog1 (bre-expression sc)
                         (pop (sc-expression-start sc)))))
       (cond
         ((null expression)
          (err "Missing a regular expression after '\\('."))
         ((errorp expression)
          expression)
         ((sc-looking-at sc "\\)")
          (sc-advance sc 2)
          (list 'rk:subexp expression))
         (t
          (err "Missing '\\)'.")))))
    ((and (sc-looking-at sc "\\") (digit-char-p (sc-next-char sc) 10))
     (sc-advance sc 2)
     (list 'rk:backref (parse-integer (sc-string sc) :start (1- (sc-position sc))
                                      :end (sc-position sc))))
    (t (bre-one-char-or-coll-elem sc))))


(defun bre-simple-re (sc)
  "
simple_RE      : nondupl_RE
               | nondupl_RE RE_dupl_symbol ;
"
  (let ((expression (bre-nondupl-re sc)))
    (cond
      ((null expression) nil)
      ((errorp expression) expression)
      (t (let ((dupl (bre-dupl-symbol sc)))
           (cond
             ((null dupl) expression)
             ((errorp dupl) dupl)
             (t (nconc dupl (list expression)))))))))


(defun bre-expression (sc)
  "
RE_expression  :               simple_RE
               | RE_expression simple_RE ;
"
  (do ((simple-re        (bre-simple-re sc) (bre-simple-re sc))
       (expression-list   nil))
      ((or (null simple-re) (errorp simple-re))
       (cond
         ((errorp simple-re)
          simple-re) ;; (:error ...)
         ((length>1 expression-list)
          (cons 'rk:sequence (nreverse expression-list)))
         (t
          (car expression-list))))
    (push simple-re expression-list)))


(defun bre-basic-reg-exp (sc)
  "
basic_reg_exp  :          RE_expression
               | L_ANCHOR
               |                        R_ANCHOR
               | L_ANCHOR               R_ANCHOR
               | L_ANCHOR RE_expression
               |          RE_expression R_ANCHOR
               | L_ANCHOR RE_expression R_ANCHOR
               ;
"
  (let ((sequence (list))
        (re-expression))
    (when (sc-looking-at sc "^")
      (setf sequence (list 'rk:l-anchor))
      (sc-advance sc))
    (setf re-expression (bre-expression sc))
    (cond
      ((null re-expression))
      ((errorp re-expression) (return-from bre-basic-reg-exp re-expression))
      (t (setf sequence (append sequence
                                (if (and (listp re-expression)
                                         (eq 'rk:sequence (car re-expression)))
                                    (cdr re-expression)
                                    (list re-expression))))))
    (when (sc-looking-at sc "$")
      (setq sequence (nconc sequence (list 'rk:r-anchor)))
      (sc-advance sc))
    (if (length>1 sequence)
        (cons 'rk:sequence sequence)
        (car sequence))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extended Regular Expression
;; ---------------------------


(defun ere-dupl-symbol (sc)
  "
DO:     Parse
RETURN: (rk:repeat min max) with min,max in (or rk:infinity (integer 0))
        or (:error message)
        or nil if not looking at a re-dupl-symbol.
NOTES:
ERE_dupl_symbol    : '*'
                   | '+'
                   | '?'
                   | '{' DUP_COUNT               '}'
                   | '{' DUP_COUNT ','           '}'
                   | '{' DUP_COUNT ',' DUP_COUNT '}'  ;
"
  (cond
    ((sc-looking-at sc "*") (sc-advance sc) (list 'rk:repeat 0 'rk:infinity))
    ((sc-looking-at sc "+") (sc-advance sc) (list 'rk:repeat 1 'rk:infinity))
    ((sc-looking-at sc "?") (sc-advance sc) (list 'rk:repeat 0 1))
    ((sc-looking-at sc "{")
     (sc-advance sc)
     (let ((min (re-integer sc)))
       (cond
         ((null min) (err "Missing duplication count after '{'."))
         ((sc-looking-at sc "}")  (sc-advance sc)  (list 'rk:repeat min min))
         ((sc-looking-at sc ",")  (sc-advance sc)
          (if (sc-looking-at sc "}")
              (progn  (sc-advance sc)  (list 'rk:repeat min 'rk:infinity))
              (let ((max (re-integer sc)))
                (cond
                  ((null max)
                   (err (format nil "Invalid ~S character in {...}."
                                        (sc-curr-char sc))))
                  ((sc-looking-at sc "}") (sc-advance sc)  (list 'rk:repeat min max))
                  ((sc-eos sc)           (err "Missing '}'."))
                  (t (err (format nil "Invalid ~S character in {...}."
                                          (sc-curr-char sc))))))))
         ((sc-eos sc)            (err "Missing '}'."))
         (t (err (format nil "Invalid ~S character in {...}."
                                 (sc-curr-char sc)))))
       ))
    (t nil)))


(defun ere-one-char-or-coll-elem (sc)
  "
DO:      Parses s single character or a coll-elem regexp.
RETURN:  (or (rk:matching ...) (rk:non-matching ...) rk:any character)
NOTES:
one_char_or_coll_elem_ERE  : ORD_CHAR
                   | QUOTED_CHAR
                   | '.'
                   | bracket_expression ;
QUOTED_CHAR     \^    \.    \[    \$    \(    \)    \|
                \*    \+    \?    \{    \\
ORD_CHAR        any but SPEC_CHAR
SPEC_CHAR
    For basic regular expressions, one of the following special characters:
    .    Anywhere outside bracket expressions
    \    Anywhere outside bracket expressions
    [    Anywhere outside bracket expressions
    ^    When used as an anchor (see BRE Expression Anchoring )
         or when first in a bracket expression
    $    When used as an anchor
    *    Anywhere except first in an entire RE,
         anywhere in a bracket expression, directly following '\(' ,
         directly following an anchoring '^'.

    ^    .    [    $    (    )    |    *    +    ?    {    \


   ==> ORD_CHAR excludes . \ [ and * but when first of the expression.
"
  (cond
    ((sc-eos sc) nil)
    ;; quoted-char:
    ((and (char= (character "\\") (sc-curr-char sc))
          (position (sc-next-char sc) "^.[$()|*+?{\\"))
     (sc-advance sc)
     (prog1 (sc-curr-char sc) (sc-advance sc)))
    ;; dot:
    ((sc-looking-at sc ".")
     (sc-advance sc)
     'rk:any)
    ;; bracket expression:
    ((sc-looking-at sc "[")
     (be-bracket-expression sc)) ;; [ is not an ord-char anyway.
    ;; spec-char:
    ((position (sc-curr-char sc) "^.[$()|*+?{\\") nil) ;;spec-char
    (t (prog1  (sc-curr-char sc) (sc-advance sc)))))


(defun ere-expression (sc)
  "
ERE_expression     : one_char_or_coll_elem_ERE
                   | '^'
                   | '$'
                   | '(' extended_reg_exp ')'
                   | ERE_expression ERE_dupl_symbol ;
"
  (let ((expression
         (cond
           ((and (null (sc-expression-start sc)) (sc-looking-at sc "^"))
            (sc-advance sc)  'rk:l-anchor)
           ((and (null (sc-expression-start sc)) (sc-looking-at sc "$"))
            (sc-advance sc)  'rk:r-anchor)
           ((sc-looking-at sc "(")
            (sc-advance sc)
            (push (sc-position sc) (sc-expression-start sc))
            (let ((ere (prog1 (ere-extended-reg-exp sc)
                         (pop (sc-expression-start sc)))))
              (cond
                ((null ere)
                 (err (format nil "Expected a regular expression after ~
                                  '(', not ~S." (sc-curr-char sc))))
                ((errorp ere) ere)
                ((sc-looking-at sc ")")
                 (sc-advance sc)
                 (list 'rk:subexp ere))
                (t (err "Missing ')' after '('.")))))
           (t  (ere-one-char-or-coll-elem sc)))))
    (cond
      ((null expression) nil)
      ((errorp expression) expression)
      (t (let ((dupl (ere-dupl-symbol sc)))
           (cond
             ((null dupl) expression)
             ((errorp dupl) dupl)
             (t (nconc dupl (list expression)))))))))


(defun ere-branch (sc)
  "
ERE_branch         :            ERE_expression
                   | ERE_branch ERE_expression ;
"
  (do ((expression        (ere-expression sc) (ere-expression sc))
       (expression-list   nil))
      ((or (null expression) (errorp expression))
       (cond
         ((errorp expression)
          expression) ;; (:error ...)
         ((length>1 expression-list)
          (cons 'rk:sequence (nreverse expression-list)))
         (t
          (car expression-list))))
    (push expression expression-list)))


(defun ere-extended-reg-exp (sc)
  "
extended_reg_exp   :                      ERE_branch
                   | extended_reg_exp '|' ERE_branch ;
"
  (do ((branch
        (ere-branch sc)
        (if (sc-looking-at sc "|")
            (let ((start (sc-position sc))
                  (branch (progn (sc-advance sc) (ere-branch sc))))
              (if (null branch)
                  (progn (setf (sc-position sc) start) nil)
                  branch))))
       (branch-list   nil))
      ((or (null branch) (errorp branch))
       (cond
         ((errorp branch)
          branch) ;; (:error ...)
         ((length>1 branch-list)
          (cons 'rk:alternative (nreverse branch-list)))
         (t
          (car branch-list))))
    (push branch branch-list)))


(defun parse-basic-re (restring)
  (let* ((sc  (make-sc :string restring))
         (re  (bre-basic-reg-exp sc)))
    (cond
      ((errorp re) (err restring (sc-position sc) (second re)))
      ((sc-eos sc) re)
      (t           (err restring (sc-position sc)
                         "Junk after basic regular expression.")))))


(defun parse-extended-re (restring)
  (let* ((sc  (make-sc :string restring))
         (re  (ere-extended-reg-exp sc)))
    (cond
      ((errorp re) (err restring (sc-position sc) (second re)))
      ((sc-eos sc) re)
      (t           (err restring (sc-position sc)
                         "Junk after extended regular expression.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRACKET EXPRESSIONS
;; -------------------


;; 
;; Bracket  expressions can  be implemented  with  a list  of ranges  and
;; elements, or  with a bitset.   Since some char-code-limit may  be huge
;; (21 bits  for Unicode UCS4,  or even more  for other codes),  we don't
;; want to allocate bit sets so big (128 KB or more).
;; 
;; On the other  hand, most bracket expresions will  come from user input
;; (regular expressions), and  will be less than 80  bytes of source.  So
;; it seems that a nice limit for  the bitset size would be about 80 byte
;; worth. Hence the maximum bit set size used here: 1024.
;; 
;; 
;; In Common-Lisp, string< definition is directly based on char<.
;; (char< a b) is defined as (< (char-code a) (char-code b)).  There is no
;; provision for collating symbols, or equivalence classes, nor for
;; locales. (We should implement them, and add locale based comparisons).
;; There is however, support for character classes in the form of predicates.
;; 
;; collating symbols:
;; [.ch.] [.ll.]
;; 
;; equivalence classes:
;; [=é=] == [=e=]
;; 
;; character classes:
;; [:alnum:]   [:cntrl:]   [:lower:]   [:space:]
;; [:alpha:]   [:digit:]   [:print:]   [:upper:]
;; [:blank:]   [:graph:]   [:punct:]   [:xdigit:]
;; 
;; 
;; [:alnum:]  (alphanumericp ch)
;; [:alpha:]  (alpha-char-p ch)
;; [:blank:]  (or (char= #\SPACE ch) (char= #\TAB ch))
;; [:cntrl:]  (not (graphic-char-p ch))
;; [:digit:]  (digit-char-p ch 10)
;; [:graph:]  (and (graphic-char-p ch) (not (char= (character " ") ch)))
;; [:lower:]  (lower-case-p ch)
;; [:print:]  (graphic-char-p ch)
;; [:punct:]  (and (graphic-char-p ch) (not (alphanumericp ch)))
;; [:space:] <space> <form-feed> <newline> <carriage-return> <tab> <vertical-tab>
;; [:upper:]  (upper-case-p ch)
;; [:xdigit:] (digit-char-p ch 16)
;; 
;; 
;; upper or lower ==> alpha
;; alpha ==> not ( cntrl or digit or punct or space )
;; alnum <=> ( alpha or digit )
;; space <== blank or <space> or <form-feed> or <newline> or <carriage-return> or <tab> or <vertical-tab>
;; space ==> not ( upper or lower or alpha or digit or graph or xdigit )
;; cntrl ==> not ( alpha or print )
;; cntrl ==> not ( upper or lower or alpha or digit or graph or xdigit or punct or print )
;; punct ==> not ( <space> or alpha or digit or cntrl )
;; punct ==> not ( <space> or alpha or digit or cntrl or upper or lower or xdigit)
;; graph <== upper or lower or alpha or digit or xdigit or punct
;; graph ==> not cntrl
;; print <=> graph or <space>
;; print <== upper or lower or alpha or digit or xdigit or punct or graph or <space>
;; print ==> not cntrl
;; xdigit <=> or( 0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f )
;; blank <== <space> or <tab>



(defparameter *character-classes*
  ;; POSIX ==> ASCII.
  (list
   (list "alnum"  (function alphanumericp))
   (list "alpha"  (function alpha-char-p))
   (list "blank"  (lambda (ch) (or (char= (code-char 32) ch)
                                   (char= (code-char 9) ch))))
   (list "cntrl"  (complement (function graphic-char-p)))
   (list "digit"  (lambda (ch) (digit-char-p ch 10)))
   (list "graph"  (lambda (ch) (and (graphic-char-p ch)
                                    (not (char= (code-char 32) ch)))))
   (list "lower"  (function lower-case-p))
   (list "print"  (function graphic-char-p))
   (list "punct"  (lambda (ch) (and (graphic-char-p ch) (not (alphanumericp ch)))))
   (list "space"  (lambda (ch) (member (char-code ch) '(32 9 10 11 12 13))))
   (list "upper"  (function upper-case-p))
   (list "xdigit" (lambda (ch) (digit-char-p ch 16))))) ;;*CHARACTER-CLASSES*

;;   0 NUL   1 SOH   2 STX   3 ETX   4 EOT   5 ENQ   6 ACK   7 BEL
;;   8 BS    9 HT   10 LF   11 VT   12 FF   13 CR   14 SO   15 SI
;;  16 DLE  17 DC1  18 DC2  19 DC3  20 DC4  21 NAK  22 SYN  23 ETB
;;  24 CAN  25 EM   26 SUB  27 ESC  28 FS   29 GS   30 RS   31 US


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; charset
;; -------

(defclass charset ()
  ()
  (:documentation "An abstract class for a character set.
This class defines the interface to whatever charset implementation."))


(defun make-charset ()
  "
RETURN:  An instance of a subclass of charset, selected according to
         the value of char-code-limit.
"
  (make-instance (if (<= char-code-limit 1024) 'charset-bitmap 'charset-range)))


(defgeneric add-char        (charset character))
(defgeneric add-range       (charset char-min char-max))
(defgeneric add-class       (charset char-class-name))
(defgeneric inverse         (charset))
(defgeneric contains-char-p (charset character))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; charset-bitmap
;; --------------


(defclass charset-bitmap (charset)
  ((bits :accessor bits :type (array bit (*))))
  (:documentation
   "A character set representation based on bit array.
This is representation may be used when char-code-limit is 'small'."))


;; NOTE: Micro-optimization:
;;          (aref (bits self) ch) == 0  <=> ch is present
;;          (aref (bits self) ch) == 1  <=> ch is absent
;;       This allow us to skip a not in contains-char-p...


(defmethod initialize-instance ((self charset-bitmap) &rest args)
  (declare (ignore args))
  (setf (bits self) (make-array (list char-code-limit)
                                :element-type 'bit
                                :initial-element 1))
  self) ;;INITIALIZE-INSTANCE


(defmethod add-char        ((self charset-bitmap) (ch character))
  (setf (aref (bits self) (char-code ch)) 0))


(defmethod add-range       ((self charset-bitmap)
                            (min character) (max character))
  (do ((bits (bits self))
       (limit (char-code max))
       (ch (char-code min) (1+ ch)) )
      ((>= ch limit))
    (setf (aref bits ch) 0)))


(defmethod add-class       ((self charset-bitmap) (char-class-name string))
  (let ((ccf (second (assoc char-class-name *character-classes*
                            :test (function string=)))))
    (unless ccf (error "Invalid character class ~S." char-class-name))
    (do ((bits (bits self))
         (ch 0 (1+ ch)))
        ((>= ch char-code-limit))
      (when (funcall ccf ch) (setf (aref bits ch) 0)))))


(defmethod inverse         ((self charset-bitmap))
  "
DO:     complements the set.
"
  (do ((bits (bits self))
       (ch 0 (1+ ch)))
      ((>= ch char-code-limit))
    (setf (aref bits ch) (1- (aref bits ch)))))
 


(defmethod contains-char-p ((self charset-bitmap) (ch character))
  (zerop (aref (bits self) (char-code ch))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; charset-range
;; -------------


;; ranges
;; ------
;;
;; A range = an integer n for [n,n]  or a cons (min . max) for [min,max].

(defstruct range %min %max)
;; (DEFMACRO MAKE-RANGE (MIN MAX) `(CONS ,MIN ,MAX))
;; (DEFMACRO RANGE-MIN  (RANGE)   `(IF (INTEGERP ,RANGE) ,RANGE (CAR ,RANGE)))
;; (DEFMACRO RANGE-MAX  (RANGE)   `(IF (INTEGERP ,RANGE) ,RANGE (CDR ,RANGE)))
(defgeneric range-min (range)
  (:method ((range range))  (range-%min range))
  (:method ((range cons))   (car range))
  (:method ((range number)) range))
(defgeneric range-max (range)
  (:method ((range range))  (range-%max range))
  (:method ((range cons))   (cdr range))
  (:method ((range number)) range))
(defgeneric (setf range-min) (new-value range)
  (:method (new-value (range range)) (setf (range-%min range) new-value))
  (:method (new-value (range cons))  (setf (car range)        new-value)))
(defgeneric (setf range-max) (new-value range)
  (:method (new-value (range range)) (setf (range-%max range) new-value))
  (:method (new-value (range cons))  (setf (car range)        new-value)))

(defun range-after-last (range)
  (1+ (if (numberp range) range (range-max range))))


(defun range-contains-p  (range n)
  (if (integerp range)
      (= range n)
      (<= (range-min range) n (range-max  range))))


;; range sets
;; ----------
;;
;; A range set is an ordered set of disjoint ranges terminated with (nil)
;; Moreover, the distance between two ranges must be >=2.
;;
;; '( (1 . 3)     5    (7 . 13)   (32 . 44)  (nil) )
;;    01234       56   7-14       15-45       46-


(defun make-range-set (&optional (range-list nil)) (append range-list '((nil))))
(defun range-set-guard-p  (rs)                     (equal '(nil) (car rs)))


(defun range-set-seek (rs n)
  "
RETURN: The last cons whose cdr+1 is >= n.
"
  (do ((rs rs (cdr rs)))
      ((or (range-set-guard-p rs) (<= n (range-after-last (car rs))))
       rs)))


(defun range-set-add-number (rs n)
  (if (range-set-guard-p rs)
      ;; empty range set
      (progn (push n rs) rs)
      (let ((prev (range-set-seek rs n)))
        (cond
          ((< (1+ n) (range-min (car prev)))
           ;; new singleton
           (if (eq prev rs)
               (setf rs (push n prev))
               (push n prev)))
          ((= (1+ n) (range-min (car prev)))
           ;; extend lower
           (setf (range-min (car prev)) n))
          ((= (1- n) (range-max (car prev)))
           ;; extend upper
           (setf (range-max (car prev)) n))
          ;; otherwise (range-contains-p (car prev) n)
          ;;            inside
          )
        rs))) ;;RANGE-SET-ADD-NUMBER


(defun range-set-copy (rs &optional (copy nil))
  (cond
    ((null rs) (error "Not guarded range set encountered."))
    ((range-set-guard-p rs)
     (push (car rs) copy) (nreverse copy))
    ((integerp (car rs))
     (range-set-copy (cdr rs) (cons (car rs) copy)))
    (t
     (range-set-copy (cdr rs) (cons (cons (range-min (car rs))
                                          (range-max (car rs))) copy)))))


(defun range-set-union (rsa rsb &optional (min nil) (max nil) (union nil))
  (cond
    ((null rsa) (error "Not guarded range set encountered."))
    ((null rsb) (error "Not guarded range set encountered."))
    ((and (range-set-guard-p rsa) (range-set-guard-p rsb))
     ;; union = (min . max) U union
     (when max
       (push (if (= min max) min (cons min max)) union))
     (nconc (nreverse union) (make-range-set)) )
    ((or (range-set-guard-p rsa) (range-set-guard-p rsb))
     ;; union = (min . max) U union U (rsa or rsb)
     (when (range-set-guard-p rsa)
       (psetf  rsa rsb rsb rsa))
     ;; union = (min . max) U union U rsa
     (if max
         (if (<= (+ 2 max) (range-min (car rsa)))
             (range-set-union rsa rsb nil nil
                              (push (if (= min max) min (cons min max)) union))
             (range-set-union (cdr rsa) rsb min (max max (range-max (car rsa)))
                              union))
         ;; union = union U rsa
         (nconc (nreverse union) (range-set-copy rsa))))
    (max
     (assert (and (not (range-set-guard-p rsa)) (not (range-set-guard-p rsb))))
     (cond
       ((and (<= (+ 2 max) (range-min (car rsa)))
             (<= (+ 2 max) (range-min (car rsb))))
        (range-set-union rsa rsb nil nil
                         (push (if (= min max) min (cons min max)) union)))
       ((> (+ 2 max) (range-min (car rsa)))
        (range-set-union (cdr rsa) rsb min (max max (range-max (car rsa))) union))
       ((> (+ 2 max) (range-min (car rsb)))
        (range-set-union rsa (cdr rsb) min (max max (range-max (car rsb))) union))
       ))
    (t ;; initial
     (let ((min (min (range-min (car rsa)) (range-min (car rsb)))))
       (range-set-union rsa rsb  min min union)))))




(defun range-set-contains-p (rs n)
  (let ((prev (range-set-seek rs n)))
    (and prev (not (range-set-guard-p prev)) (range-contains-p (car prev) n))))


(defun make-range-set-vector (rs)
  (make-array (list (1- (length rs))) :initial-contents (butlast rs)))


;; (setq v (make-array '(11) :initial-contents (mapcar (lambda (x) (* 3 x)) '( 1 2 3 4 5 6 7 8 9 10 11 ))))
;; (mapcar (lambda (k) (multiple-value-list (dichotomy-search v k (lambda (a b) (cond ((< a b) -1) ((> a b) 1) (t 0)))))) '(0 1 2 3 4 5 6 7 8 9 30 31 32 33 34 35 ))



;; charset-range
;; -------------


(defclass charset-range (charset)
  ( (range-set    :accessor range-set
                  :type list
                  :initform (make-range-set))
   (range-vector :accessor range-vector
                 :type (or null vector)
                 :initform nil)
    (char-classes :accessor char-classes
                  :type list
                  :initform '())
    (complemented :accessor complemented
                  :type boolean
                  :initform nil) )
  (:documentation
   "A character set representation based on binary trees of ranges
and additional list of character classes.
This is representation may be used when char-code-limit is 'big'."))


(defun rccompare (range cc)
  (cond ((< cc (range-min range)) -1)
        ((> cc (range-max range)) +1)
        (t 0)))


(defmethod add-char        ((self charset-range) (ch character))
  (setf (range-vector self) nil)
  (range-set-add-number (range-set self) (char-code ch)))


(defmethod add-range       ((self charset-range)
                            (min character) (max character))
  (setf (range-vector self) nil)
  (setf (range-set self)
        (range-set-union (range-set self)
                         (make-range-set (list (make-range (char-code min)
                                                           (char-code max)))))))


(defmethod add-class       ((self charset-range) (char-class-name string))
  (let ((ccf (second (assoc char-class-name *character-classes*
                            :test (function string=)))))
    (unless ccf (error "Invalid character class ~S." char-class-name))
    (pushnew ccf (char-classes self))))


(defmethod inverse         ((self charset-range))
  "
DO:     complements the set.
"
  (setf (complemented self) (not (complemented self))))


(defmethod contains-char-p ((self charset-range) (ch character))
  (let ((code (char-code ch))
        (result nil))
    (when (null (range-vector self))
      (setf (range-vector self) (make-range-vector (range-set self))))
    (multiple-value-bind (found index order)
        (dichotomy-search (range-vector self) code (function rccompare))
      (declare (ignore order))
      (setf result (and found (range-contains-p (aref (range-vector self) index)
                                                code))))
    (unless result
      (setf result (some (lambda (classf) (funcall classf code))
                         (char-classes self))))
    (if (complemented self) (not result) result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching regular expression
;; ---------------------------


(defun lighten (obj)
  (cond
    ((symbolp obj) (symbol-name obj))
    ((consp obj)   (cons (lighten (car obj)) (lighten (cdr obj))))
    (t             obj))) ;;LIGHTEN





(defstruct (rnode
            (:print-function print-rnode))
  "A rnode represent a compiled regexp node"
  ;; code:
  (matchf   nil)
  (token    nil)
  (children nil :type (or null (array #+lispworks t 
                                      #-lispworks rnode
                                      (*)))))

;; (equiv (null children) (not (< 0 (length children)))))


(defun print-rnode (node stream level)
  (declare (ignore level))
  (format stream "<~A~{ ~S~}>"
          (lighten (rnode-token node))
          (map 'list (function identity) (rnode-children node))))



(defmacro with-rnode (node &body body)
  `(with-slots ((matchf   matchf)
                (token    token)
                (children children)) ,node
     ,@body))


(defmacro rnode-match (node state env)
  (if (symbolp node)
      `(funcall (rnode-matchf ,node) ,node ,state ,env)
      `(let ((node ,node))
         (funcall (rnode-matchf node) node ,state ,env))))


(defun rnode-walk (node fun)
  (funcall fun node)
  (map nil (lambda (node) (rnode-walk node fun)) (rnode-children node)))


(defstruct (rstate (:print-function
                    (lambda (state stream level)
                      (declare (ignore level))
                      (format stream "<rstate :try ~S :start ~S :end ~S>"
                              (rstate-try   state)
                              (rstate-start state)
                              (rstate-end   state)))))
  "State data used when matching a rnode."
  (try      nil)
  (start    0   :type (or null (integer 0)))
  (end      0   :type (or null (integer 0)))) ;;RSTATE


(defmacro with-rstate (state &body body)
  `(with-slots ((start    start)
                (end      end)
                (try      try)) ,state
     ,@body))


(defun rstate-retry (state position)
  (with-rstate state (setf start position end nil try nil)))


(defmacro try-once (&body body)
  `(if try nil (progn (setf try t) ,@body)))


(defmacro try (&rest clauses)
  "
SYNTAX:  (try (initially [sexp|(immediately-then)]...)
              (then sexp...))
"
  (let ((initial nil)
        (then    nil))
    (do ((clauses clauses (cdr clauses)))
        ((null clauses))
      (cond
        ((string-equal (caar clauses) :initially)
         (if initial
             (error "Can't have two (initially ...) in try.")
             (setf initial (cdar clauses))))
        ((string-equal (caar clauses) :then)
         (if then
             (error "Can't have two (then ...) in try.")
             (setf then (cdar clauses))))
        (t
         (error "Invalid clause (~S) in try." (caar clauses)))))
    (if initial
        (if then
            ;; both initial and then:
            (with-gensyms (label-try label-then)
              `(block ,label-try
                 (tagbody
                    (unless try
                      (macrolet ((immediately-then () `(go ,',label-then)))
                        (setf try t)
                        (return-from ,label-try (progn ,@initial))))
                    ,label-then
                    (return-from ,label-try (progn ,@then))))) 
            ;; initial alone:
            `(unless try ,@initial))
        (if then
            ;; then alone:
            `(when try ,@then)
            ;; nothing
            (values)))))


(defstruct renv
  "An renv gather the environment (variables) used to run a compiled
regexp matched, ie. rnode."
  (equalf   (function equal)) ;; use equalp for case insensitive.
  ;; equalf must take two sequence arguments
  ;; and accept :start1 :end1 :start2 :end2 keys.
  (newlinepf (lambda (ch) (eql #\NEWLINE ch)))
  (sequence ""  :type vector) ;; renv-set-sequence sets length and position too.
  (length   0   :type (integer 0))
  (position 0   :type (integer 0))
  (subexps  nil :type (or null (vector cons *)))
  (regexp   nil :type (or null rnode)) ;; renv-set-regexp sets subexps too.
  (bol      t   :type boolean)
  (eol      t   :type boolean)
  (newline  t   :type boolean))


(defmacro with-renv (env &body body)
  `(with-slots ((equalf     equalf   )
                (newlinepf  newlinepf)
                (sequence   sequence )
                (length     length   )
                (position   position )
                (subexps    subexps  )
                (regexp     regexp   )
                (bol        bol      )
                (eol        eol      )
                (newline    newline  )) ,env
     ,@body))


(defun subexp-filled-p (subexp)         subexp)
(defmacro subexp-clear (subexp)         `(pop ,subexp))
(defmacro subexp-set (subexp start end) `(push (cons ,start ,end) ,subexp))
(defmacro subexp-start (subexp)         `(car (top ,subexp)))
(defmacro subexp-end   (subexp)         `(cdr (top ,subexp)))


(defun renv-set-sequence (env new-seq)
  (with-renv env
    (setf sequence (cond
                     ((vectorp new-seq) new-seq)
                     ((listp   new-seq) (make-array (list (length new-seq))
                                                    :initial-contents new-seq))
                     (t (error "Can match only vectors and lists.")))
          length   (length sequence)
          position 0)))


(defun renv-set-regexp (env regexp)
  (let ((cregexp (if (rnode-p regexp) regexp  (compile-regexp regexp)))
        (subexp-num  0))
    (rnode-walk cregexp (lambda (node)
                          (with-rnode node
                            (when (and (consp token)
                                       (eq 'rk:subexp (car token)))
                              (setf (cdr token) subexp-num)
                              (incf subexp-num)))))
    (with-renv env
      (setf regexp  cregexp
            subexps (make-array (list subexp-num)))
      (dotimes (i subexp-num)
        (setf (aref subexps i) nil)))))



;;(dolist (s '(collating-symbol equivalence-class character-class range any l-anchor r-anchor matching non-matching backref subexp sequence repeat infinity alternative b-anchor e-anchor item set-sequence)) (insert (format "(defun rmatch-%s (node env)\n)\n\n\n" s)))


(defmacro with-rens (env node state &body body)
  `(with-renv ,env
     (with-rnode ,node
       (with-rstate ,state
         ,@body))))


(defun rmatch-b-anchor (node state env)
  "
Beginning of string anchor.
"
  (declare (ignorable node))
  (with-rens env node state
    (try-once
      (when (zerop position)
        (setf end position)
        t))))


(defun rmatch-e-anchor (node state env)
  "
End of string anchor.
"
  (declare (ignorable node))
  (with-rens env node state
    (try-once
      (when (= length position)
        (setf end position)
        t))))


(defun rmatch-l-anchor (node state env)
  "
Beginning of line anchor.
"
  (declare (ignorable node))
  (with-rens env node state
    (try-once
      (if (or (and bol (= 0 position))
              (and newline
                   (< 1 position)
                   (funcall newlinepf (aref sequence (1- position)))))
          (progn (setf end position) t)
          nil))))


(defun rmatch-r-anchor (node state env)
  "
End of line anchor.
"
  (declare (ignorable node))
  (with-rens env node state
    (try-once
      (if (or (and eol (= length position))
              (and newline
                   (< (1+ position) (length sequence))
                   (funcall newlinepf (aref sequence (1+ position)))))
          (progn (setf end position) t)
          nil))))


 (defun rmatch-any (node state env)
  (declare (ignorable node))
  (with-rens env node state
    (try-once
      (if (or (<= length position)
              (and newline ;; don't match newline
                   (funcall newlinepf (aref sequence position))))
          nil
          (progn
            (incf position)
            (setf end position)
            t)))))



(defun rmatch-item (node state env)
  (declare (ignorable node))
  (with-rens env node state
    (try-once
      (if (and (< position length)
               (funcall equalf token (aref sequence position)))
          (progn
            (incf position)
            (setf end position)
            t)
          nil))))


(defun rmatch-sequence (node state env)
  (with-rens env node state
    (if children
        (invariant (or (null try)
                       (and (integerp try)
                            (or (= 0 try) (= try (1- (length children))))))
          (let (p n)
            ;; try = ( n state[n-1] state[n-1] ... state[0] )
            ;; n is the number of children matched in sequence so far.
            ;; follows the states of the n children in reverse order (stack).
            ;; We exit with either 0 or (1- (length children)).
            ;;
            ;; The 'position' pointer advances and tracks back, walking the
            ;; tree of matching child position until we find one (or more,
            ;; in following tries) matching sequence.
            (try (initially  (setf n 0)
                             (push (make-rstate :start position) try))
                 (then       (setf n (pop try))))
            (setf p (rnode-match (aref children n) (top try) env))
            (while (or (and p (< (1+ n) (length children)))
                       (and (not p) (<= 0 (1- n))))
              (if p
                  (progn
                    (incf n)
                    (push (make-rstate :start position) try))
                  (progn ;; backtrack
                    (decf n)
                    (pop try)
                    (setf position (rstate-start (top try)))))
              (setf p (rnode-match (aref children n) (top try) env)))
            (push n try)
            (setf end position)
            p))
        (try-once ;; no child -- empty sequence -- match once.
          (setf end position)
          t))))


;; shy
;; 
;; /
;; 0
;; 1
;; 2
;; 00
;; 01
;; 02
;; 10
;; 11
;; 12
;; 20
;; 21
;; 22
;; 000
;; 001
;; 002
;; 010
;; 011
;; 012
;; 200
;; 201
;; 202
;; 210
;; 211
;; 212
;; 220
;; 221
;; 222


(defun rmatch-repeat-shy (node state env)
  "
DO:    match min to max repeatition, smallest first.
"
  (with-rens env node state
             (if (and children (lessp 0 (cdr token)))
                 (let ((min (car token))
                       (max (cdr token))
                       (child (aref children 0))
                       p n)
                   (if (lessp max min)
                       nil ;; never match.
                       ;; try = ( n state[n-1] state[n-1] ... state[0] )
                       ;; n is the number of children matched in sequence so far.
                       ;; follows the states of the n children in reverse order (stack).
                       ;;
                       ;; The 'position' pointer advances and tracks back, walking the
                       ;; tree of matching child position until we find one (or more,
                       ;; in following tries) matching sequence.
                       (tagbody
                        :init      (try (initially
                                         (setf try nil n 0  p position)
                                         (if (< n min)
                                             (go :fill)
                                             (go :match)))
                                        (then
                                         (when (eq :failed try)
                                           (return-from rmatch-repeat-shy nil))
                                         (setf n (pop try))
                                         (if (= 0 n)
                                             (go :add)
                                             (go :increment))))
                        :fill      (progn
                                     (push (make-rstate :start p) try)
                                     (incf n)
                                     (setf p (rnode-match child (top try) env))
                                     (cond
                                       ((not p)   (go :remove-1))
                                       ((< n min) (go :fill))
                                       (t         (go :match)))) ;; n=min
                        :add       (progn
                                     (push (make-rstate :start p) try)
                                     (incf n)
                                     (setf p (rnode-match child (top try) env))
                                     (if p
                                         (go :match)
                                         (go :remove-1)))
                        :remove-1  (progn
                                     (pop try)
                                     (decf n)
                                     (cond
                                       ((<= min n)  (go :match))
                                       ((= 0 n)     (go :fail))
                                       (t           (go :increment))))
                        :increment (progn
                                     (setf p (rnode-match child (top try) env))
                                     (cond
                                       ((not p)       (go :remove-1))
                                       ((< n min)     (go :fill))
                                       ((lessp n max) (go :add))
                                       (t             (go :match))))
                        :match     (progn
                                     (setf end position)
                                     (push n try)
                                     (return-from rmatch-repeat-shy p))
                        :fail      (progn
                                     (setf try :failed)
                                     (return-from rmatch-repeat-shy nil)) )))
                 (try-once ;; max=0 or no child -- empty sequence -- match once.
                  (setf end position)
                  t))))


;; "(a.{0,4}){1,}"
;; "axxxayyyazzzbzattttbaaabaa"
;;  --------====-                   shy-1
;;  --------------=====-            shy-2
;;  ----------------------=-        shy-3
;; "(a.*)*b"
;; "axxxayyyazzzbzattttbaaabaa"
;;  =======================-        greedy-1
;;  ===================-            greedy-2
;;  ============-                   greedy-3


;; greedy:
;; {} is greedy!
;;
;; 00x
;; 010x
;; 0110
;; 0111
;; 1000
;; 101x
;; 11x
;; 1000
;; 1001
;; ...
;; 1110
;; 1111
;; 111
;; 11
;; 1
;; /


(defun rmatch-repeat-greedy (node state env)
  "
DO:    match min to max repeatition, greatest first.
"
  (with-rens env node state
             (if (and children (lessp 0 (cdr token)))
                 (let ((min (car token))
                       (max (cdr token))
                       (child (aref children 0))
                       p n)
                   (if (lessp max min)
                       nil ;; never match.
                       ;; try = ( n state[n-1] state[n-1] ... state[0] )
                       ;; n is the number of children matched in sequence so far.
                       ;; follows the states of the n children in reverse order (stack).
                       ;;
                       ;; The 'position' pointer advances and tracks back, walking the
                       ;; tree of matching child position until we find one (or more,
                       ;; in following tries) matching sequence.
                       (tagbody
                        :init      (try (initially
                                         (setf try nil n 0  p position)
                                         (go :fill))
                                        (then
                                         (when (eq :failed try)
                                           (return-from rmatch-repeat-greedy nil))
                                         (setf n (pop try))
                                         (go :increment)))
                        :fill      (progn
                                     (assert (lessp n max))
                                     (push (make-rstate :start p) try)
                                     (incf n)
                                     (setf p (rnode-match child (top try) env))
                                     (cond
                                       ((not p)       (go :remove-1))
                                       ((lessp n max) (go :fill))
                                       (t             (go :match)))) ;; n=max
                        :remove-1  (progn
                                     (pop try)
                                     (decf n)
                                     (cond
                                       ((<= min n)  (go :match))
                                       ((= 0 n)     (go :fail))
                                       (t           (go :increment))))
                        :increment (progn
                                     (setf p (rnode-match child (top try) env))
                                     (cond
                                       ((not p)       (go :remove-1))
                                       ((lessp n max) (go :fill))
                                       (t             (go :match))))
                        :match     (progn
                                     (setf end position)
                                     (push n try)
                                     (return-from rmatch-repeat-greedy p))
                        :fail      (progn
                                     (setf try :failed)
                                     (return-from rmatch-repeat-greedy nil)) )))
                 (try-once ;; max=0 or no child -- empty sequence -- match once.
                  (setf end position)
                  t))))
         

(defun rmatch-alternative (node state env)
  (with-rens env node state
             (if children
                 (progn
                   (try
                    ;; try = (index of alternative tried.
                    (initially (setf try (cons 0 (make-rstate :start position)))))
                   (loop
                      (let ((child (aref children (car try))))
                        (if (rnode-match child (cdr try) env)
                            (progn
                              (setf end position)
                              (return-from rmatch-alternative t))
                            (progn
                              (incf (car try))
                              (if (< (car try) (length children))
                                  (rstate-retry (cdr try) (rstate-start (cdr try)))
                                  (return-from rmatch-alternative nil)))))))
                 (try-once ;; no child -- empty alternative -- match once.
                  (setf end position)
                  t))))


(defun rmatch-subexp (node state env)
  (with-rens env node state
             ;; token = (cons 'rk:subexp subexp-index)
             ;; one child
             (if children
                 (let ((index (cdr token))
                       (child (aref children 0)))
                   (try
                    (initially
                     (setf try (make-rstate :start position)))
                    (then
                     (when (eq :failed try)
                       (return-from rmatch-subexp nil))))
                   (let ((p (rnode-match child try env)))
                     (if p
                         (progn
                           (setf end position)
                           (subexp-set (aref subexps index) start end))
                         (progn
                           (setf try :failed)
                           (subexp-clear (aref subexps index))))
                     p))
                 (let ((index (cdr token)))
                   (try
                    ;; no child : match once
                    (initially (subexp-set   (aref subexps index) position position) t)
                    (then      (subexp-clear (aref subexps index))   nil))))))


(defun rmatch-backref (node state env)
  (with-rens env node state
             (try-once
              (let ((index token))
                (if (and (numberp index) (<= 0 index) (< index (length subexps))
                         (subexp-filled-p (aref subexps index)))
                    (let* ((match (aref subexps index))
                           (e (+ position (- (subexp-end   match)
                                             (subexp-start match)))))
                      (if (and (<= e length)
                               (funcall equalf sequence sequence
                                        :start1 (subexp-start match)
                                        :end1   (subexp-end   match)
                                        :start2 position
                                        :end2   e))
                          (progn (setf end e) t)
                          nil))
                    (error "Invalid back reference (\\~S)." index))))))


(defun rmatch-matching (node state env)
  (with-rens env node state
             (try-once
              (if (and (< position length)
                       (contains-char-p token (aref sequence position)))
                  (progn
                    (incf position)
                    (setf end position)
                    t)
                  nil))))


(defun rmatch-non-matching (node state env)
  (with-rens env node state
             (try-once
              (if (or (<= length position)
                      (and newline ;; don't match newline
                           (funcall newlinepf (aref sequence position)))
                      (contains-char-p token (aref sequence position)))
                  nil
                  (progn
                    (incf position)
                    (setf end position)
                    t)))))


(defparameter *match-function-alist*
  `(
    (rk:b-anchor             . ,(function rmatch-b-anchor))
    (rk:e-anchor             . ,(function rmatch-e-anchor))
    (rk:l-anchor             . ,(function rmatch-l-anchor))
    (rk:r-anchor             . ,(function rmatch-r-anchor))
    (rk:any                  . ,(function rmatch-any))
    (rk:item                 . ,(function rmatch-item))
    (rk:sequence             . ,(function rmatch-sequence))
    (rk:repeat               . ,(function rmatch-repeat-greedy))
    (rk:repeat-shy           . ,(function rmatch-repeat-shy))
    (rk:alternative          . ,(function rmatch-alternative))
    (rk:subexp               . ,(function rmatch-subexp))
    (rk:backref              . ,(function rmatch-backref))
    ;; ---
    (rk:matching             . ,(function rmatch-matching))
    (rk:non-matching         . ,(function rmatch-non-matching))))
    ;;    (rk:collating-symbol     . ,(function rmatch-collating-symbol))
    ;;    (rk:equivalence-class    . ,(function rmatch-equivalence-class))
    ;;    (rk:character-class      . ,(function rmatch-character-class))
    ;;    (rk:range                . ,(function rmatch-range))))


(defun find-match-function (node)
  (let ((ass (assoc node *match-function-alist*)))
    (if ass (cdr ass) (cdr (assoc 'rk:item *match-function-alist*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mexpr --> (or
;;            character
;;            (rk:collating-symbol  string/collating-element)
;;            (rk:equivalence-class string/equivalence-class)
;;            (rk:character-class   string/class-name)
;;            (rk:range (or coll-elem-single collating-symbol)/start
;;                      (or coll-elem-single collating-symbol)/end))

(defun compile-bracket-expression (regexp)
  "
RETURN: The charset described by the regex, either a rk:matching
        or a rk:non-matching. The charset is not complemented for
        rk:non-matching, this should be done by match function.

regexp --> (or (rk:matching     mexpr...)
               (rk:non-matching mexpr...)
mexpr --> (or
           character
           (rk:collating-symbol  string/collating-element)
           (rk:equivalence-class string/equivalence-class)
           (rk:character-class   string/class-name)
           (rk:range (or coll-elem-single collating-symbol)/start
                     (or coll-elem-single collating-symbol)/end))
NOTE: We don't compile bracket expressions for other atoms than characters!
"
  (let ((charset (make-charset)))
    (do ((items (cdr regexp) (cdr items)))
        ((null items) charset)
      (cond
        ((characterp (car items))
         (add-char charset (car items)))
        ((atom (car items))
         (error "Invalid atom in bracket expression ~S." (car items)))
        ((eq (caar items) 'rk:collating-symbol)
         (error "Collating symbols are not implemented yet."))
        ((eq (caar items) 'rk:equivalence-class)
         (error "Equivalence classes are not implemented yet."))
        ((eq (caar items) 'rk:character-class)
         (add-class charset (second (car items))))
        ((eq (caar items) 'rk:range)
         (let ((min (second (car items)))
               (max (third  (car items))) )
           (when (or (listp min) (listp max))
             (error "Collating symbols are not implemented yet."))
           (add-range charset min max)))
        (t (error "Unexpected item in bracket expression ~S." (car items)))))))


(defun compile-regexp (regexp)
  "
RETURN:  A rnode representing the compiled regexp.
regexp --> (or character
              rk:any
              rk:l-anchor
              rk:r-anchor
              (rk:matching mexpr...)
              (rk:non-matching mexpr...)
              (rk:backref  integer)
              (rk:subexp   regex)
              (rk:sequence regex...)
              (rk:repeat  integer (or integer rk:infinity) bexp)
              (rk:alternative regex...))
"
  (macrolet
      ((mnode
           (mfkey token &optional (children nil))
         `(make-rnode
           :matchf (find-match-function ,mfkey)
           :token ,token
           :children (when ,children
                       (make-array (list (length ,children))
                                   :initial-contents
                                   (mapcar (function compile-regexp) ,children))))))
    (if (listp regexp)
        (case (car regexp)
          ((rk:backref)
           (mnode (first regexp) (second regexp)))
          ((rk:subexp)
           (mnode (car regexp) (cons (car regexp) 0) (cdr regexp)))
          ((rk:repeat rk:repeat-shy)
           (when (< 1 (length (cdddr regexp)))
             (error "Found a ~A with more than one child." (car regexp)))
           (mnode (first regexp)
                  (cons (second regexp) (third regexp)) (cdddr regexp)))
          ((rk:matching rk:non-matching)
           (mnode (first regexp) (compile-bracket-expression regexp)))
          (otherwise
           (mnode (car regexp) (car regexp) (cdr regexp))))
        (mnode regexp regexp))))


(defun count-subexp (regexp)
  "
RETURN: The number of subexp found in regexp
"
  (let ((count 0))
    (labels ((walk (regexp)
               (when (listp regexp)
                 (case (car regexp)
                   ((rk:sequence rk:alternative)
                    (map nil (function walk) (cdr regexp)))
                   ((rk:repeat rk:repeat-shy)
                    (map nil (function walk) (cdddr regexp)))
                   ((rk:subexp)
                    (incf count)
                    (map nil (function walk) (cdr regexp))))) ))
      (walk regexp))
    count))



(defstruct match
  "This structure stores a (start,end) couple specifying the range matched
by a group (or the whole regexp)."
  (start nil :type (or null integer))
  (end   nil :type (or null integer)))


(defun match-string (string match)
  "Extracts the substring of STRING corresponding to a given pair of
start and end indices. The result is shared with STRING.
If you want a freshly consed string, use copy-string
or (coerce (match-string ...) 'simple-string)."
  (subseq string (match-start match) (match-end match)))


(defun regexp-quote (string)
  (declare (ignore string))
  (error "Not Implemented Yet: REGEXP-QUOTE~%" ))


(defun make-range-vector (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun pjb-re-parse-whole-regexp (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun pjb-re-decorate-tree (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun pjb-re-collect-groups (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun pjb-re-init (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun pjb-re-match (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun pjb-re-slot-begin (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun pjb-re-slot-end (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))


(defun match (regexp string &optional start end)
  "Common-Lisp: This function returns as first value a match structure
containing the indices of the start and end of the first match for the
regular expression REGEXP in STRING, or nil if there is no match.
If START is non-nil, the search starts at that index in STRING.
If END is non-nil, only (subseq STRING START END) is considered.
The next values are match structures for every '\(...\)' construct in REGEXP,
in the order that the open parentheses appear in REGEXP.


start:   the first character of STRING to be considered (defaults to 0)
end:     the after last character of STRING to be considered
         (defaults to (length string)).
RETURN:  index of start of first match for REGEXP in STRING, nor nil.
"
  (unless start (setq start 0))
  (unless end   (setq end (length string)))
  (when (< end start) (setq end start))
  ;; TODO: What to do when start or end are out of bounds ?

  (let* ((syn-tree
          (pjb-re-parse-whole-regexp
           (make-sc :string
                    (concatenate 'string "\\`.*?\\(" regexp "\\).*?\\'"))))
         (dec-tree (pjb-re-decorate-tree syn-tree string))
         (groups  (pjb-re-collect-groups dec-tree))
         )

    (pjb-re-init dec-tree start)
    (pjb-re-match dec-tree)
    ;; there's nowhere to backtrack at the top level...
    (values-list (mapcar (lambda (g)
                           (let ((s (pjb-re-slot-begin g))
                                 (e (pjb-re-slot-end g)) )
                             (if (and s e)
                                 (make-match :start s :end e)
                                 (make-match :start nil :end nil))))
                         groups))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POSIX API
;;; ---------


(deftype   size-t   () "Used for sizes of objects." '(integer 0 *))


(deftype   regoff-t ()
  "The type regoff_t shall be defined as a signed integer type that
can hold the largest value that can be stored in either a type off_t
or type ssize_t. "
  'integer) ;;REGOFF-T

  
(defstruct (regex-t
             (:conc-name "RE-"))
  "
NSUB:  Number of parenthesized subexpressions.
"
  (nsub 0 :type size-t)
  ;; the rest is private!
  (env         nil :type (or null renv))
  (extended    nil :type boolean)
  (ignore-case nil :type boolean)
  (nosub       nil :type boolean)
  (newline     nil :type boolean))


(defstruct (regmatch-t
             (:conc-name "RM-"))
  "
SO:  Byte offset from start of string to start of substring.
EO:  Byte offset from start of string of the first character
     after the end of substring.
"
  (so -1 :type regoff-t)
  (eo -1 :type regoff-t))


;; int regcomp(regex_t *restrict preg,const char *restrict pattern,
;;       int cflags);
;; --> regcomp

;;size_t regerror(int errcode, const regex_t *restrict preg,
;;       char *restrict errbuf, size_t errbuf_size);
;; not implemented (we use lisp conditions)

;;int regexec(const regex_t *restrict preg, const char *restrict string,
;;       size_t nmatch, regmatch_t pmatch[restrict], int eflags);
;; --> regexec

;;void regfree(regex_t *preg);
;; not implemented (we use lisp garbage collector)


(defun regcomp (pattern
                &key (extended nil) (ignore-case nil) (nosub nil) (newline nil))
  "
RETURN:  A regex-t representing the compiled regular expression PATTERN.
RAISE:   An ERROR condition, in case of syntax error.
"
  (let ((regexp  (if extended
                     (parse-extended-re pattern)
                     (parse-basic-re    pattern))))
    (if (errorp regexp)
        (error "~D:~A" (third regexp) (fourth regexp))
        (let ((env (make-renv :equalf (if ignore-case
                                          (function equalp) (function equal))
                              :newline newline)))
          (renv-set-regexp env
                           `(rk:sequence rk:b-anchor
                                         (rk:repeat-shy 0 rk:infinity rk:any)
                                         (rk:subexp ,regexp)
                                         (rk:repeat-shy 0 rk:infinity rk:any)
                                         rk:e-anchor))
          (make-regex-t :nsub (count-subexp regexp) :env env
                        :extended extended :ignore-case ignore-case
                        :nosub  nosub :newline newline)))))


(defun regexec (regex string &key (nmatch nil) (bol t) (eol t))
  "
RETURN:  match ;
         (or (not match) (null nmatch) (zerop nmatch) (re-nosub regex)) ==> nil
         (eq t nmatch) ==> A vector of regmatch-t with (1+ (re-nsub regex))
                           items
         (numberp nmatch) ==> A vector of regmatch-t with nmatch items.
WARNING: Entry #0 of the result vector is always the start and end of the
         whole expression.  To get the start and end of the last subexpression
         you need to pass :nmatch (1+ (re-nsub regex)) [or T].
"
  (when (or (null nmatch)
            (re-nosub regex)
            (and (numberp nmatch) (zerop nmatch)))
    (setf nmatch nil))
  (with-slots ((env env)) regex
    (setf (renv-bol env) bol
          (renv-eol env) eol)
    (renv-set-sequence env string)
    (if (rnode-match (renv-regexp env) (make-rstate) env)
        (values t ;; matches
                (if (null nmatch)
                    nil
                    (let* ((size (if (numberp nmatch)
                                     nmatch ;;(min nmatch(length(renv-subexps env)))
                                     (length (renv-subexps env))))
                           (result (make-array (list size)
                                               :element-type 'regmatch-t)))
                      (dotimes (i size)
                        (setf (aref result i)
                              (if (< i (length (renv-subexps env)))
                                  (let ((se (aref (renv-subexps env) i)))
                                    (if (subexp-filled-p se)
                                        (make-regmatch-t :so (subexp-start se)
                                                         :eo (subexp-end   se))
                                        (make-regmatch-t)))
                                  (make-regmatch-t))))
                      result)))
        (values nil     ;; does not match
                nil))))


                                     
;;;; THE END ;;;;
