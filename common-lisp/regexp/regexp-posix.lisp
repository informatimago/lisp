;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               regexp-posix.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             UNIX
;;;;USER-INTERFACE:     UNIX
;;;;NOWEB:              t
;;;;DESCRIPTION
;;;;
;;;;    Posix Regexp implemented in Common-Lisp.
;;;;
;;;;    See specifications at:
;;;;    http://www.opengroup.org/onlinepubs/007904975/basedefs/xbd_chap09.html
;;;;
;;;;    This is a strict implementation that will work both in clisp
;;;;    (Common-Lisp) and emacs (with cl and pjb-cl Common-Lisp extensions).
;;;;
;;;;    This implementation is entirely in lisp, contrarily to what regexp
;;;;    packages are available under clisp or emacs.  Thus it has the advantage
;;;;    of portability and availability (you don't have to compile or link
;;;;    a lisp system written in some barbarous language, and you get the same
;;;;    regexp features in all programs including this module).
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
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2004
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
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "COMMON-LISP")
  (:EXPORT
   ;; CLISP REGEXP API:
   "REGEXP-MATCH" "REGEXP-QUOTE" "MATCH-STRING" "MATCH-END"
   "MATCH-START" "MATCH"
   ;; POSIX API:
   "REGEXEC" "REGCOMP" "RM-EO" "RM-SO" "REGMATCH-T"
   "RE-NSUB" "REGEX-T" "REGOFF-T" "SIZE-T")
  (:DOCUMENTATION
   "This package implement POSIX Regular Expressions in Common-Lisp.
This is interesting because it's available on any Common-Lisp platform
while external C regexp libraries or internals are available or not,
and not always implement these same syntax or semantic.
See specifications at:
   http://www.opengroup.org/onlinepubs/007904975/basedefs/xbd_chap09.html
"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX.KEYWORD"
  (:NICKNAMES "RK")
  (:USE)
  (:EXPORT
   "COLLATING-SYMBOL" "EQUIVALENCE-CLASS" "CHARACTER-CLASS"
   "RANGE" "ANY" "L-ANCHOR" "R-ANCHOR" "MATCHING" "NON-MATCHING" "BACKREF"
   "SUBEXP" "SEQUENCE" "REPEAT" "REPEAT-SHY" "INFINITY" "ALTERNATIVE"
   "B-ANCHOR" "E-ANCHOR"
   "ITEM" "SET-SEQUENCE")
  (:DOCUMENTATION "This package gathers and exports regexp keywords."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX")

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


(DEFUN LESSP (A B)
  (IF (OR (EQ A 'RK:INFINITY) (EQ B 'RK:INFINITY))
      NIL
      (< A B))) ;;LESSP



(DEFMACRO IF^2 (C1 C2 TT TF FT FF)  `(IF ,C1 (IF ,C2 ,TT ,TF) (IF ,C2 ,FT ,FF)))
;; (if^2 cond1 cond2
;;      (t t)      (t f)
;;      (f t)      (f f))
(DEFMACRO TOP (STACK) `(CAR ,STACK))


(DEFMACRO INVARIANT (CONDITION &BODY BODY)
  `(PROGN (ASSERT ,CONDITION) (PROG1 (PROGN ,@BODY) (ASSERT ,CONDITION))))




(DEFUN PJB-RE-SPLIT-STRING (STRING &OPTIONAL SEPARATORS)
  "
DOES:       Splits STRING into substrings where there are matches
            for SEPARATORS.
RETURNS:    A list of substrings.
separators: A regexp matching the sub-string separators.
            Defaults to \"[ \f\t\n\r\v]+\".
NOTE:       Current implementation only accepts as separators
            a literal string containing only one character.
"
  (LET ((SEP (AREF SEPARATORS 0))
        (CHUNKS  '())
        (POSITION 0)
        (NEXTPOS  0)
        (STRLEN   (LENGTH STRING))
        )
    (LOOP WHILE (< POSITION STRLEN)
       DO
       (LOOP WHILE (AND (< NEXTPOS STRLEN)
                        (CHAR/= SEP (AREF STRING NEXTPOS)))
          DO (SETQ NEXTPOS (1+ NEXTPOS)))
       (PUSH (SUBSEQ STRING POSITION NEXTPOS) CHUNKS)
       (SETQ POSITION (1+ NEXTPOS))
       (SETQ NEXTPOS  POSITION))
    (NREVERSE CHUNKS)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String Scanner
;; --------------
;;


(DEFSTRUCT SC
  (STRING           "" :TYPE STRING)
  (POSITION         0  :TYPE INTEGER)
  (BRACKET-POSITION 0  :TYPE INTEGER)
  (EXPRESSION-START () :TYPE LIST))


(DEFUN SC-LENGTH (SC) (LENGTH (SC-STRING SC)))
(DEFUN SC-EOS    (SC) (>= (SC-POSITION SC) (SC-LENGTH SC)))


(DEFUN SC-CURR-CHAR (SC)
  "
RETURN:  The current character, or nil if EOS.
"
  (IF (SC-EOS SC)
      NIL
      (CHAR (SC-STRING SC) (SC-POSITION SC))))


(DEFUN SC-NEXT-CHAR (SC)
  "
RETURN:  The next character, or nil if EOS.
"
  (IF (< (1+ (SC-POSITION SC)) (SC-LENGTH SC))
      (CHAR (SC-STRING SC) (1+ (SC-POSITION SC)))
      NIL))


(DEFUN SC-AFTER-NEXT-CHAR (SC)
  "
RETURN:  The after next character, or nil if EOS.
"
  (IF (< (+ 2 (SC-POSITION SC)) (SC-LENGTH SC))
      (CHAR (SC-STRING SC) (+ 2 (SC-POSITION SC)))
      NIL))


(DEFUN SC-ADVANCE (SC &OPTIONAL (INCREMENT 1))
  "
PRE:     (= p (sc-position sc))
POST:    (= (min (sc-length sc) (+ p increment)) (sc-position sc))
RETURN:  The character at position p+increment or nil if EOS.
"
  (SETF (SC-POSITION SC) (MIN (SC-LENGTH SC) (+ (SC-POSITION SC) INCREMENT)))
  (SC-CURR-CHAR SC))


(DEFUN SC-LOOKING-AT (SC SUBSTRING)
  "
"
  (STRING= (SC-STRING SC) SUBSTRING
           :START1 (SC-POSITION SC)
           :END1   (MIN (SC-LENGTH SC) (+ (SC-POSITION SC) (LENGTH SUBSTRING)))))


(DEFUN SC-SCAN-TO (SC SUBSTRING)
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
  (LET* ((START (SC-POSITION SC))
         (END   (SEARCH SUBSTRING (SC-STRING SC) :START2 START)))
    (IF END
        (PROGN
          (SETF (SC-POSITION SC)  END)
          (SUBSEQ (SC-STRING SC) START END))
        NIL)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regular Expression Parsing Utilities
;; ------------------------------------


(DEFUN LENGTH>1 (LIST) (CDR LIST))
(DEFUN ERRORP   (EXPR) (AND (CONSP EXPR) (EQ :ERROR (CAR EXPR))))

(define-condition parsing-error (error)
  ((arguments :initarg :arguments :accessor parsing-error-arguments)))
(defun err (&rest args)
  ;; (error 'parsing-error :arguments (copy-list args))
  (cons :error (copy-list args)))

(DEFUN RE-INTEGER (SC)
  "
DO:     Parses an integer.
RETURN: The integer, or NIL.
"
  (DO ((START (SC-POSITION SC)))
      ((OR (SC-EOS SC) (NOT (DIGIT-CHAR-P (SC-CURR-CHAR SC) 10)))
       (IF (< START (SC-POSITION SC))
           (PARSE-INTEGER (SC-STRING SC) :START START :END (SC-POSITION SC)
                          :RADIX 10 :JUNK-ALLOWED NIL)
           NIL))
    (SC-ADVANCE SC)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing: Bracket Expression
;; ---------------------------


(DEFUN BE-COLLATING-SYMBOL (SC)
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
  (IF (SC-LOOKING-AT SC "[.")
      (PROGN
        (SC-ADVANCE SC 2)
        (LET ((COLLATING-SYMBOL (SC-SCAN-TO SC ".]")))
          (IF COLLATING-SYMBOL
              (PROGN
                (SC-ADVANCE SC 2)
                (LIST 'RK:COLLATING-SYMBOL COLLATING-SYMBOL))
              (err "Missing a closing '.]' after '[.'."))))
      NIL)) ;;BE-COLLATING-SYMBOL


(DEFUN BE-EQUIVALENCE-CLASS (SC)
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
  (IF (SC-LOOKING-AT SC "[=")
      (PROGN
        (SC-ADVANCE SC 2)
        (IF (OR (SC-LOOKING-AT SC "-") (SC-LOOKING-AT SC "]"))
            (ERR (FORMAT NIL "Invalid meta-char ~S in equivalence class."
                         (SC-CURR-CHAR SC)))
            (LET ((EQUIVALENCE-CLASS (SC-SCAN-TO SC "=]")))
              (IF EQUIVALENCE-CLASS
                  (PROGN
                    (SC-ADVANCE SC 2)
                    (LIST 'RK:EQUIVALENCE-CLASS EQUIVALENCE-CLASS))
                  (ERR "Missing a closing '=]' after '[='.")))))
      NIL)) ;;BE-EQUIVALENCE-CLASS


(DEFUN BE-CHARACTER-CLASS (SC)
  "
DO:     Parses a character class
RETURN: (rk:character-class class-name)
        or (:error message)
        or nil if not looking at '[:'.
NOTES:
   character_class : Open_colon class_name Colon_close ;
"
  (IF (SC-LOOKING-AT SC "[:")
      (PROGN
        (SC-ADVANCE SC 2)
        (LET ((CLASS-NAME (SC-SCAN-TO SC ":]")))
          (IF CLASS-NAME
              (PROGN
                (SC-ADVANCE SC 2)
                (LIST 'RK:CHARACTER-CLASS CLASS-NAME))
              (ERR "Missing a closing ':]' after '[:'."))))
      NIL)) ;;BE-CHARACTER-CLASS


(DEFUN BE-END-RANGE (SC)
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
  (LET ((COLL-SYM (BE-COLLATING-SYMBOL SC)))
    (COND
      (COLL-SYM COLL-SYM)
      ((SC-EOS SC) NIL)
      ((AND (= (SC-POSITION SC) (SC-BRACKET-POSITION SC))
            (SC-LOOKING-AT SC "-"))
       (PROG1 (SC-CURR-CHAR SC) (SC-ADVANCE SC)))
      ((SC-LOOKING-AT SC "-]")
       (PROG1 (SC-CURR-CHAR SC) (SC-ADVANCE SC)))
      ((SC-LOOKING-AT SC "-") NIL)
      ((SC-LOOKING-AT SC "]") NIL)
      (T
       (PROG1 (SC-CURR-CHAR SC) (SC-ADVANCE SC))))))


(DEFUN BE-START-RANGE (SC)
  "
DO:     Parses a start-range.
RETURN: character or (rk:collating-symbol coll-elem)
        or nil if not looking at a start-range.
NOTES:
   start_range    : end_range '-' ;
"
  (LET ((START  (SC-POSITION SC))
        (RESULT (BE-END-RANGE SC)))
    (COND
      ((NULL RESULT)          (SETF (SC-POSITION SC) START) NIL)
      ((ERRORP RESULT)                                      RESULT)
      ((SC-LOOKING-AT SC "-") (SC-ADVANCE SC)               RESULT)
      (T                      (SETF (SC-POSITION SC) START) NIL))))


(DEFUN BE-RANGE-EXPRESSION (SC)
  "
DO:     Parses a range-expression.
RETURN: (rk:range start end) or nil of not looking at a range-expression.
NOTES:
   range_expression : start_range end_range
                    | start_range '-' ;
"
  (LET ((START       (SC-POSITION SC))
        (RANGE-START (BE-START-RANGE SC)))
    (COND
      ((NULL RANGE-START) NIL)
      ((ERRORP RANGE-START) RANGE-START)
      ((SC-LOOKING-AT SC "-")
       (LIST 'RK:RANGE RANGE-START (CHARACTER "-")))
      (T (LET ((RANGE-END (BE-END-RANGE SC)))
           (COND
             ((NULL RANGE-END)   (SETF (SC-POSITION SC) START) NIL)
             ((ERRORP RANGE-END) RANGE-END) ;; error or not error?
             (T (LIST 'RK:RANGE RANGE-START RANGE-END))))))))


(DEFUN BE-SINGLE-EXPRESSION (SC)
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
  (LET ((START  (SC-POSITION SC))
        (SE     (BE-CHARACTER-CLASS SC))
        (ERRPOS NIL)
        (ERR    NIL))
    (COND
      ((NULL SE)   (SETF SE  (BE-EQUIVALENCE-CLASS SC)))
      ((ERRORP SE) (SETF ERRPOS (OR ERRPOS (SC-POSITION SC))
                         (SC-POSITION SC) START
                         ERR (OR ERR SE)
                         SE  (BE-EQUIVALENCE-CLASS SC)))
      (T           (RETURN-FROM BE-SINGLE-EXPRESSION SE)))
    (COND
      ((NULL SE)   (SETF SE  (BE-END-RANGE SC)))
      ((ERRORP SE) (SETF ERRPOS (OR ERRPOS (SC-POSITION SC))
                         (SC-POSITION SC) START
                         ERR (OR ERR SE)
                         SE  (BE-END-RANGE SC)))
      (T           (RETURN-FROM BE-SINGLE-EXPRESSION SE)))
    (COND
      ((NULL SE)   NIL)
      ((ERRORP SE) (SETF ERRPOS (OR ERRPOS (SC-POSITION SC))
                         ERR (OR ERR SE)
                         (SC-POSITION SC) ERRPOS)    ERR)
      (T           SE))))


(DEFUN BE-EXPRESSION-TERM (SC)
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
  (LET ((START  (SC-POSITION SC))
        (ET     (BE-RANGE-EXPRESSION SC))
        (ERRPOS NIL)
        (ERR    NIL))
    (COND
      ((NULL ET)   (SETF ET  (BE-SINGLE-EXPRESSION SC)))
      ((ERRORP ET) (SETF ERRPOS (OR ERRPOS (SC-POSITION SC))
                         (SC-POSITION SC) START
                         ERR (OR ERR ET)
                         ET  (BE-SINGLE-EXPRESSION SC)))
      (T           (RETURN-FROM BE-EXPRESSION-TERM ET)))
    (COND
      ((NULL ET)   NIL)
      ((ERRORP ET) (SETF ERRPOS (OR ERRPOS (SC-POSITION SC))
                         ERR (OR ERR ET)
                         (SC-POSITION SC) ERRPOS)    ERR)
      (T           ET))))


(DEFUN BE-FOLLOW-LIST (SC)
  "
DO:
RETURN:  (:follow-list expression...)
         or (:error message)
         or  nil
   follow_list    :             expression_term
                  | follow_list expression_term  ;
"
  (DO ((EXPRESSION-TERM (BE-EXPRESSION-TERM SC) (BE-EXPRESSION-TERM SC))
       (FOLLOW-LIST     NIL))
      ((OR (NULL EXPRESSION-TERM) (ERRORP EXPRESSION-TERM))
       (IF (ERRORP EXPRESSION-TERM)
           EXPRESSION-TERM ;; (:error ...)
           (AND FOLLOW-LIST (CONS :FOLLOW-LIST (NREVERSE FOLLOW-LIST)))))
    (PUSH EXPRESSION-TERM FOLLOW-LIST)))


(DEFUN BE-BRACKET-LIST (SC)
  "
DO:      Parses a bracket-list.
RETURN:  (:follow-list expression...) or nil.
NOTES:
   bracket_list   : follow_list
                  | follow_list '-' ;
"
  (LET ((FOLLOW-LIST (BE-FOLLOW-LIST SC)))
    (COND
      ((NULL FOLLOW-LIST) NIL)
      ((ERRORP FOLLOW-LIST) FOLLOW-LIST)
      (T (OR (AND (SC-LOOKING-AT SC "-")
                  (PROG1
                      (NCONC FOLLOW-LIST (LIST (SC-CURR-CHAR SC)))
                    (SC-ADVANCE SC)))
             FOLLOW-LIST)))))


(DEFUN BE-BRACKET-EXPRESSION (SC)
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
  (LET ((START (SC-POSITION SC))
        BE)
    (COND
      ((SC-LOOKING-AT SC "[^")
       (SC-ADVANCE SC 2)
       (SETF BE 'RK:NON-MATCHING))
      ((SC-LOOKING-AT SC "[")
       (SC-ADVANCE SC)
       (SETF BE 'RK:MATCHING))
      (T (RETURN-FROM BE-BRACKET-EXPRESSION NIL)))
    (SETF (SC-BRACKET-POSITION  SC) (SC-POSITION SC))
    (LET ((MATCHING-LIST (BE-BRACKET-LIST SC)))
      (COND
        ((NULL MATCHING-LIST)   (SETF (SC-POSITION SC) START) NIL)
        ((ERRORP MATCHING-LIST) MATCHING-LIST)
        ((SC-LOOKING-AT SC "]")
         (SC-ADVANCE SC)
         (CONS BE (CDR MATCHING-LIST)))
        (T (ERR "Missing ']' in bracket expression."))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Regular Expression
;; ------------------------


(DEFUN BRE-DUPL-SYMBOL (SC)
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
  (COND
    ((SC-LOOKING-AT SC "*") (SC-ADVANCE SC) (LIST 'RK:REPEAT 0 'RK:INFINITY))
    ((SC-LOOKING-AT SC "{")
     (SC-ADVANCE SC)
     (LET ((MIN (RE-INTEGER SC)))
       (COND
         ((NULL MIN) (ERR "Missing duplication count after '{'."))
         ((SC-LOOKING-AT SC "}")  (SC-ADVANCE SC)  (LIST 'RK:REPEAT MIN MIN))
         ((SC-LOOKING-AT SC ",")  (SC-ADVANCE SC)
          (IF (SC-LOOKING-AT SC "}")
              (PROGN  (SC-ADVANCE SC)  (LIST 'RK:REPEAT MIN 'RK:INFINITY))
              (LET ((MAX (RE-INTEGER SC)))
                (COND
                  ((NULL MAX)
                   (ERR (FORMAT NIL "Invalid ~S character in {...}."
                                        (SC-CURR-CHAR SC))))
                  ((SC-LOOKING-AT SC "}")
                   (SC-ADVANCE SC)
                   (LIST 'RK:REPEAT MIN MAX))
                  ((SC-EOS SC)
                   (err "Missing '}'."))
                  (T (ERR (FORMAT NIL "Invalid ~S character in {...}."
                                          (SC-CURR-CHAR SC))))))))
         ((SC-EOS SC)            (ERR "Missing '}'."))
         (T (ERR (FORMAT NIL "Invalid ~S character in {...}."
                         (SC-CURR-CHAR SC)))))
       ))
    (T NIL)))


(DEFUN BRE-ONE-CHAR-OR-COLL-ELEM (SC)
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
  (COND
    ((SC-EOS SC) NIL)
    ;; quoted-char:
    ((AND (CHAR= (CHARACTER "\\") (SC-CURR-CHAR SC))
          (POSITION (SC-NEXT-CHAR SC) "^.*[$\\"))
     (SC-ADVANCE SC)
     (PROG1 (SC-CURR-CHAR SC) (SC-ADVANCE SC)))
    ;; dot:
    ((SC-LOOKING-AT SC ".")
     (SC-ADVANCE SC)
     'RK:ANY)
    ;; bracket expression:
    ((SC-LOOKING-AT SC "[")
     (BE-BRACKET-EXPRESSION SC)) ;; [ is not an ord-char anyway.
    ;; not looking at one-char-or-coll-elem-re:
    ((OR (SC-LOOKING-AT SC "\\")
         (SC-LOOKING-AT SC "[")
         (AND (SC-EXPRESSION-START SC)                 (SC-LOOKING-AT SC "*"))
         (AND (= (1+ (SC-POSITION SC)) (SC-LENGTH SC)) (SC-LOOKING-AT SC "$")))
     NIL) ;; spec-char
    ((AND (SC-EXPRESSION-START SC)
          (OR (SC-LOOKING-AT SC "^") (SC-LOOKING-AT SC "$")))
     (ERR (FORMAT NIL "Invalid ~S character inside '\\(' and '\\)'."
                  (SC-CURR-CHAR SC))))
    (T
     (PROG1  (SC-CURR-CHAR SC) (SC-ADVANCE SC)))))


(DEFUN BRE-NONDUPL-RE (SC)
  "
nondupl_RE     : one_char_or_coll_elem_RE
               | Back_open_paren RE_expression Back_close_paren
               | BACKREF ;
"
  (COND
    ((SC-LOOKING-AT SC "\\(")
     (SC-ADVANCE SC 2)
     (PUSH  (SC-POSITION SC) (SC-EXPRESSION-START SC))
     (LET ((EXPRESSION (PROG1 (BRE-EXPRESSION SC)
                         (POP (SC-EXPRESSION-START SC)))))
       (COND
         ((NULL EXPRESSION)
          (ERR "Missing a regular expression after '\\('."))
         ((ERRORP EXPRESSION)
          EXPRESSION)
         ((SC-LOOKING-AT SC "\\)")
          (SC-ADVANCE SC 2)
          (LIST 'RK:SUBEXP EXPRESSION))
         (T
          (ERR "Missing '\\)'.")))))
    ((AND (SC-LOOKING-AT SC "\\") (DIGIT-CHAR-P (SC-NEXT-CHAR SC) 10))
     (SC-ADVANCE SC 2)
     (LIST 'RK:BACKREF (PARSE-INTEGER (SC-STRING SC) :START (1- (SC-POSITION SC))
                                      :END (SC-POSITION SC))))
    (T (BRE-ONE-CHAR-OR-COLL-ELEM SC))))


(DEFUN BRE-SIMPLE-RE (SC)
  "
simple_RE      : nondupl_RE
               | nondupl_RE RE_dupl_symbol ;
"
  (LET ((EXPRESSION (BRE-NONDUPL-RE SC)))
    (COND
      ((NULL EXPRESSION) NIL)
      ((ERRORP EXPRESSION) EXPRESSION)
      (T (LET ((DUPL (BRE-DUPL-SYMBOL SC)))
           (COND
             ((NULL DUPL) EXPRESSION)
             ((ERRORP DUPL) DUPL)
             (T (NCONC DUPL (LIST EXPRESSION)))))))))


(DEFUN BRE-EXPRESSION (SC)
  "
RE_expression  :               simple_RE
               | RE_expression simple_RE ;
"
  (DO ((SIMPLE-RE        (BRE-SIMPLE-RE SC) (BRE-SIMPLE-RE SC))
       (EXPRESSION-LIST   NIL))
      ((OR (NULL SIMPLE-RE) (ERRORP SIMPLE-RE))
       (COND
         ((ERRORP SIMPLE-RE)
          SIMPLE-RE) ;; (:error ...)
         ((LENGTH>1 EXPRESSION-LIST)
          (CONS 'RK:SEQUENCE (NREVERSE EXPRESSION-LIST)))
         (T
          (CAR EXPRESSION-LIST))))
    (PUSH SIMPLE-RE EXPRESSION-LIST)))


(DEFUN BRE-BASIC-REG-EXP (SC)
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
  (LET ((SEQUENCE (LIST))
        (RE-EXPRESSION))
    (WHEN (SC-LOOKING-AT SC "^")
      (SETF SEQUENCE (LIST 'RK:L-ANCHOR))
      (SC-ADVANCE SC))
    (SETF RE-EXPRESSION (BRE-EXPRESSION SC))
    (COND
      ((NULL RE-EXPRESSION))
      ((ERRORP RE-EXPRESSION) (RETURN-FROM BRE-BASIC-REG-EXP RE-EXPRESSION))
      (T (SETF SEQUENCE (APPEND SEQUENCE
                                (IF (AND (LISTP RE-EXPRESSION)
                                         (EQ 'RK:SEQUENCE (CAR RE-EXPRESSION)))
                                    (CDR RE-EXPRESSION)
                                    (LIST RE-EXPRESSION))))))
    (WHEN (SC-LOOKING-AT SC "$")
      (SETQ SEQUENCE (NCONC SEQUENCE (LIST 'RK:R-ANCHOR)))
      (SC-ADVANCE SC))
    (IF (LENGTH>1 SEQUENCE)
        (CONS 'RK:SEQUENCE SEQUENCE)
        (CAR SEQUENCE))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extended Regular Expression
;; ---------------------------


(DEFUN ERE-DUPL-SYMBOL (SC)
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
  (COND
    ((SC-LOOKING-AT SC "*") (SC-ADVANCE SC) (LIST 'RK:REPEAT 0 'RK:INFINITY))
    ((SC-LOOKING-AT SC "+") (SC-ADVANCE SC) (LIST 'RK:REPEAT 1 'RK:INFINITY))
    ((SC-LOOKING-AT SC "?") (SC-ADVANCE SC) (LIST 'RK:REPEAT 0 1))
    ((SC-LOOKING-AT SC "{")
     (SC-ADVANCE SC)
     (LET ((MIN (RE-INTEGER SC)))
       (COND
         ((NULL MIN) (ERR "Missing duplication count after '{'."))
         ((SC-LOOKING-AT SC "}")  (SC-ADVANCE SC)  (LIST 'RK:REPEAT MIN MIN))
         ((SC-LOOKING-AT SC ",")  (SC-ADVANCE SC)
          (IF (SC-LOOKING-AT SC "}")
              (PROGN  (SC-ADVANCE SC)  (LIST 'RK:REPEAT MIN 'RK:INFINITY))
              (LET ((MAX (RE-INTEGER SC)))
                (COND
                  ((NULL MAX)
                   (ERR (FORMAT NIL "Invalid ~S character in {...}."
                                        (SC-CURR-CHAR SC))))
                  ((SC-LOOKING-AT SC "}") (SC-ADVANCE SC)  (LIST 'RK:REPEAT MIN MAX))
                  ((SC-EOS SC)           (ERR "Missing '}'."))
                  (T (ERR (FORMAT NIL "Invalid ~S character in {...}."
                                          (SC-CURR-CHAR SC))))))))
         ((SC-EOS SC)            (ERR "Missing '}'."))
         (T (ERR (FORMAT NIL "Invalid ~S character in {...}."
                                 (SC-CURR-CHAR SC)))))
       ))
    (T NIL)))


(DEFUN ERE-ONE-CHAR-OR-COLL-ELEM (SC)
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
  (COND
    ((SC-EOS SC) NIL)
    ;; quoted-char:
    ((AND (CHAR= (CHARACTER "\\") (SC-CURR-CHAR SC))
          (POSITION (SC-NEXT-CHAR SC) "^.[$()|*+?{\\"))
     (SC-ADVANCE SC)
     (PROG1 (SC-CURR-CHAR SC) (SC-ADVANCE SC)))
    ;; dot:
    ((SC-LOOKING-AT SC ".")
     (SC-ADVANCE SC)
     'RK:ANY)
    ;; bracket expression:
    ((SC-LOOKING-AT SC "[")
     (BE-BRACKET-EXPRESSION SC)) ;; [ is not an ord-char anyway.
    ;; spec-char:
    ((POSITION (SC-CURR-CHAR SC) "^.[$()|*+?{\\") NIL) ;;spec-char
    (T (PROG1  (SC-CURR-CHAR SC) (SC-ADVANCE SC)))))


(DEFUN ERE-EXPRESSION (SC)
  "
ERE_expression     : one_char_or_coll_elem_ERE
                   | '^'
                   | '$'
                   | '(' extended_reg_exp ')'
                   | ERE_expression ERE_dupl_symbol ;
"
  (LET ((EXPRESSION
         (COND
           ((AND (NULL (SC-EXPRESSION-START SC)) (SC-LOOKING-AT SC "^"))
            (SC-ADVANCE SC)  'RK:L-ANCHOR)
           ((AND (NULL (SC-EXPRESSION-START SC)) (SC-LOOKING-AT SC "$"))
            (SC-ADVANCE SC)  'RK:R-ANCHOR)
           ((SC-LOOKING-AT SC "(")
            (SC-ADVANCE SC)
            (PUSH (SC-POSITION SC) (SC-EXPRESSION-START SC))
            (LET ((ERE (PROG1 (ERE-EXTENDED-REG-EXP SC)
                         (POP (SC-EXPRESSION-START SC)))))
              (COND
                ((NULL ERE)
                 (ERR (FORMAT NIL "Expected a regular expression after ~
                                  '(', not ~S." (SC-CURR-CHAR SC))))
                ((ERRORP ERE) ERE)
                ((SC-LOOKING-AT SC ")")
                 (SC-ADVANCE SC)
                 (LIST 'RK:SUBEXP ERE))
                (T (ERR "Missing ')' after '('.")))))
           (T  (ERE-ONE-CHAR-OR-COLL-ELEM SC)))))
    (COND
      ((NULL EXPRESSION) NIL)
      ((ERRORP EXPRESSION) EXPRESSION)
      (T (LET ((DUPL (ERE-DUPL-SYMBOL SC)))
           (COND
             ((NULL DUPL) EXPRESSION)
             ((ERRORP DUPL) DUPL)
             (T (NCONC DUPL (LIST EXPRESSION)))))))))


(DEFUN ERE-BRANCH (SC)
  "
ERE_branch         :            ERE_expression
                   | ERE_branch ERE_expression ;
"
  (DO ((EXPRESSION        (ERE-EXPRESSION SC) (ERE-EXPRESSION SC))
       (EXPRESSION-LIST   NIL))
      ((OR (NULL EXPRESSION) (ERRORP EXPRESSION))
       (COND
         ((ERRORP EXPRESSION)
          EXPRESSION) ;; (:error ...)
         ((LENGTH>1 EXPRESSION-LIST)
          (CONS 'RK:SEQUENCE (NREVERSE EXPRESSION-LIST)))
         (T
          (CAR EXPRESSION-LIST))))
    (PUSH EXPRESSION EXPRESSION-LIST)))


(DEFUN ERE-EXTENDED-REG-EXP (SC)
  "
extended_reg_exp   :                      ERE_branch
                   | extended_reg_exp '|' ERE_branch ;
"
  (DO ((BRANCH
        (ERE-BRANCH SC)
        (IF (SC-LOOKING-AT SC "|")
            (LET ((START (SC-POSITION SC))
                  (BRANCH (PROGN (SC-ADVANCE SC) (ERE-BRANCH SC))))
              (IF (NULL BRANCH)
                  (PROGN (SETF (SC-POSITION SC) START) NIL)
                  BRANCH))))
       (BRANCH-LIST   NIL))
      ((OR (NULL BRANCH) (ERRORP BRANCH))
       (COND
         ((ERRORP BRANCH)
          BRANCH) ;; (:error ...)
         ((LENGTH>1 BRANCH-LIST)
          (CONS 'RK:ALTERNATIVE (NREVERSE BRANCH-LIST)))
         (T
          (CAR BRANCH-LIST))))
    (PUSH BRANCH BRANCH-LIST)))


(DEFUN PARSE-BASIC-RE (RESTRING)
  (LET* ((SC  (MAKE-SC :STRING RESTRING))
         (RE  (BRE-BASIC-REG-EXP SC)))
    (COND
      ((ERRORP RE) (ERR RESTRING (SC-POSITION SC) (SECOND RE)))
      ((SC-EOS SC) RE)
      (T           (ERR RESTRING (SC-POSITION SC)
                         "Junk after basic regular expression.")))))


(DEFUN PARSE-EXTENDED-RE (RESTRING)
  (LET* ((SC  (MAKE-SC :STRING RESTRING))
         (RE  (ERE-EXTENDED-REG-EXP SC)))
    (COND
      ((ERRORP RE) (ERR RESTRING (SC-POSITION SC) (SECOND RE)))
      ((SC-EOS SC) RE)
      (T           (ERR RESTRING (SC-POSITION SC)
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



(DEFPARAMETER *CHARACTER-CLASSES*
  ;; POSIX ==> ASCII.
  (LIST
   (LIST "alnum"  (FUNCTION ALPHANUMERICP))
   (LIST "alpha"  (FUNCTION ALPHA-CHAR-P))
   (LIST "blank"  (LAMBDA (CH) (OR (CHAR= (CODE-CHAR 32) CH)
                                   (CHAR= (CODE-CHAR 9) CH))))
   (LIST "cntrl"  (COMPLEMENT (FUNCTION GRAPHIC-CHAR-P)))
   (LIST "digit"  (LAMBDA (CH) (DIGIT-CHAR-P CH 10)))
   (LIST "graph"  (LAMBDA (CH) (AND (GRAPHIC-CHAR-P CH)
                                    (NOT (CHAR= (CODE-CHAR 32) CH)))))
   (LIST "lower"  (FUNCTION LOWER-CASE-P))
   (LIST "print"  (FUNCTION GRAPHIC-CHAR-P))
   (LIST "punct"  (LAMBDA (CH) (AND (GRAPHIC-CHAR-P CH) (NOT (ALPHANUMERICP CH)))))
   (LIST "space"  (LAMBDA (CH) (MEMBER (CHAR-CODE CH) '(32 9 10 11 12 13))))
   (LIST "upper"  (FUNCTION UPPER-CASE-P))
   (LIST "xdigit" (LAMBDA (CH) (DIGIT-CHAR-P CH 16))))) ;;*CHARACTER-CLASSES*

;;   0 NUL   1 SOH   2 STX   3 ETX   4 EOT   5 ENQ   6 ACK   7 BEL
;;   8 BS    9 HT   10 LF   11 VT   12 FF   13 CR   14 SO   15 SI
;;  16 DLE  17 DC1  18 DC2  19 DC3  20 DC4  21 NAK  22 SYN  23 ETB
;;  24 CAN  25 EM   26 SUB  27 ESC  28 FS   29 GS   30 RS   31 US


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; charset
;; -------

(DEFCLASS CHARSET ()
  ()
  (:DOCUMENTATION "An abstract class for a character set.
This class defines the interface to whatever charset implementation."))


(DEFUN MAKE-CHARSET ()
  "
RETURN:  An instance of a subclass of charset, selected according to
         the value of char-code-limit.
"
  (MAKE-INSTANCE (IF (<= CHAR-CODE-LIMIT 1024) 'CHARSET-BITMAP 'CHARSET-RANGE)))


(DEFGENERIC ADD-CHAR        (CHARSET CHARACTER))
(DEFGENERIC ADD-RANGE       (CHARSET CHAR-MIN CHAR-MAX))
(DEFGENERIC ADD-CLASS       (CHARSET CHAR-CLASS-NAME))
(DEFGENERIC INVERSE         (CHARSET))
(DEFGENERIC CONTAINS-CHAR-P (CHARSET CHARACTER))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; charset-bitmap
;; --------------


(DEFCLASS CHARSET-BITMAP (CHARSET)
  ((BITS :ACCESSOR BITS :TYPE (ARRAY BIT (*))))
  (:DOCUMENTATION
   "A character set representation based on bit array.
This is representation may be used when char-code-limit is 'small'."))


;; NOTE: Micro-optimization:
;;          (aref (bits self) ch) == 0  <=> ch is present
;;          (aref (bits self) ch) == 1  <=> ch is absent
;;       This allow us to skip a not in contains-char-p...


(DEFMETHOD INITIALIZE-INSTANCE ((SELF CHARSET-BITMAP) &REST ARGS)
  (DECLARE (IGNORE ARGS))
  (SETF (BITS SELF) (MAKE-ARRAY (LIST CHAR-CODE-LIMIT)
                                :ELEMENT-TYPE 'BIT
                                :INITIAL-ELEMENT 1))
  SELF) ;;INITIALIZE-INSTANCE


(DEFMETHOD ADD-CHAR        ((SELF CHARSET-BITMAP) (CH CHARACTER))
  (SETF (AREF (BITS SELF) (CHAR-CODE CH)) 0))


(DEFMETHOD ADD-RANGE       ((SELF CHARSET-BITMAP)
                            (MIN CHARACTER) (MAX CHARACTER))
  (DO ((BITS (BITS SELF))
       (LIMIT (CHAR-CODE MAX))
       (CH (CHAR-CODE MIN) (1+ CH)) )
      ((>= CH LIMIT))
    (SETF (AREF BITS CH) 0)))


(DEFMETHOD ADD-CLASS       ((SELF CHARSET-BITMAP) (CHAR-CLASS-NAME STRING))
  (LET ((CCF (SECOND (ASSOC CHAR-CLASS-NAME *CHARACTER-CLASSES*
                            :TEST (function STRING=)))))
    (UNLESS CCF (ERROR "Invalid character class ~S." CHAR-CLASS-NAME))
    (DO ((BITS (BITS SELF))
         (CH 0 (1+ CH)))
        ((>= ch CHAR-CODE-LIMIT))
      (WHEN (FUNCALL CCF CH) (SETF (AREF BITS CH) 0)))))


(DEFMETHOD INVERSE         ((SELF CHARSET-BITMAP))
  "
DO:     complements the set.
"
  (DO ((BITS (BITS SELF))
       (CH 0 (1+ CH)))
      ((>= CH CHAR-CODE-LIMIT))
    (SETF (AREF BITS CH) (1- (AREF BITS CH)))))
 


(DEFMETHOD CONTAINS-CHAR-P ((SELF CHARSET-BITMAP) (CH CHARACTER))
  (ZEROP (AREF (BITS SELF) (CHAR-CODE CH))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; charset-range
;; -------------


;; ranges
;; ------
;;
;; A range = an integer n for [n,n]  or a cons (min . max) for [min,max].

(defstruct range min max)
;; (DEFMACRO MAKE-RANGE (MIN MAX) `(CONS ,MIN ,MAX))
;; (DEFMACRO RANGE-MIN  (RANGE)   `(IF (INTEGERP ,RANGE) ,RANGE (CAR ,RANGE)))
;; (DEFMACRO RANGE-MAX  (RANGE)   `(IF (INTEGERP ,RANGE) ,RANGE (CDR ,RANGE)))


(DEFUN RANGE-AFTER-LAST (RANGE)
  (1+ (IF (NUMBERP RANGE) RANGE (RANGE-MAX RANGE))))


(DEFUN RANGE-CONTAINS-P  (RANGE N)
  (IF (INTEGERP RANGE)
      (= RANGE N)
      (<= (RANGE-MIN RANGE) N (RANGE-MAX  RANGE))))


;; range sets
;; ----------
;;
;; A range set is an ordered set of disjoint ranges terminated with (nil)
;; Moreover, the distance between two ranges must be >=2.
;;
;; '( (1 . 3)     5    (7 . 13)   (32 . 44)  (nil) )
;;    01234       56   7-14       15-45       46-


(DEFUN MAKE-RANGE-SET (&OPTIONAL (RANGE-LIST NIL)) (APPEND RANGE-LIST '((NIL))))
(DEFUN RANGE-SET-GUARD-P  (RS)                     (EQUAL '(NIL) (CAR RS)))


(DEFUN RANGE-SET-SEEK (RS N)
  "
RETURN: The last cons whose cdr+1 is >= n.
"
  (DO ((RS RS (CDR RS)))
      ((OR (RANGE-SET-GUARD-P RS) (<= N (RANGE-AFTER-LAST (CAR RS))))
       RS)))


(DEFUN RANGE-SET-ADD-NUMBER (RS N)
  (IF (RANGE-SET-GUARD-P RS)
      ;; empty range set
      (PROGN (PUSH N RS) RS)
      (LET ((PREV (RANGE-SET-SEEK RS N)))
        (COND
          ((< (1+ N) (RANGE-MIN (CAR PREV)))
           ;; new singleton
           (IF (EQ PREV RS)
               (SETF RS (PUSH N PREV))
               (PUSH N PREV)))
          ((= (1+ N) (RANGE-MIN (CAR PREV)))
           ;; extend lower
           (SETF (RANGE-MIN (CAR PREV)) N))
          ((= (1- N) (RANGE-MAX (CAR PREV)))
           ;; extend upper
           (SETF (RANGE-MAX (CAR PREV)) N))
          ;; otherwise (range-contains-p (car prev) n)
          ;;            inside
          )
        RS))) ;;RANGE-SET-ADD-NUMBER


(DEFUN RANGE-SET-COPY (RS &OPTIONAL (COPY NIL))
  (COND
    ((NULL RS) (ERROR "Not guarded range set encountered."))
    ((RANGE-SET-GUARD-P RS)
     (PUSH (CAR RS) COPY) (NREVERSE COPY))
    ((INTEGERP (CAR RS))
     (RANGE-SET-COPY (CDR RS) (CONS (CAR RS) COPY)))
    (T
     (RANGE-SET-COPY (CDR RS) (CONS (CONS (RANGE-MIN (CAR RS))
                                          (RANGE-MAX (CAR RS))) COPY)))))


(DEFUN RANGE-SET-UNION (RSA RSB &OPTIONAL (MIN NIL) (MAX NIL) (UNION NIL))
  (COND
    ((NULL RSA) (ERROR "Not guarded range set encountered."))
    ((NULL RSB) (ERROR "Not guarded range set encountered."))
    ((AND (RANGE-SET-GUARD-P RSA) (RANGE-SET-GUARD-P RSB))
     ;; union = (min . max) U union
     (WHEN MAX
       (PUSH (IF (= MIN MAX) MIN (CONS MIN MAX)) UNION))
     (NCONC (NREVERSE UNION) (MAKE-RANGE-SET)) )
    ((OR (RANGE-SET-GUARD-P RSA) (RANGE-SET-GUARD-P RSB))
     ;; union = (min . max) U union U (rsa or rsb)
     (WHEN (RANGE-SET-GUARD-P RSA)
       (PSETF  RSA RSB RSB RSA))
     ;; union = (min . max) U union U rsa
     (IF MAX
         (IF (<= (+ 2 MAX) (RANGE-MIN (CAR RSA)))
             (RANGE-SET-UNION RSA RSB NIL NIL
                              (PUSH (IF (= MIN MAX) MIN (CONS MIN MAX)) UNION))
             (RANGE-SET-UNION (CDR RSA) RSB MIN (MAX MAX (RANGE-MAX (CAR RSA)))
                              UNION))
         ;; union = union U rsa
         (NCONC (NREVERSE UNION) (RANGE-SET-COPY RSA))))
    (MAX
     (ASSERT (AND (NOT (RANGE-SET-GUARD-P RSA)) (NOT (RANGE-SET-GUARD-P RSB))))
     (COND
       ((AND (<= (+ 2 MAX) (RANGE-MIN (CAR RSA)))
             (<= (+ 2 MAX) (RANGE-MIN (CAR RSB))))
        (RANGE-SET-UNION RSA RSB NIL NIL
                         (PUSH (IF (= MIN MAX) MIN (CONS MIN MAX)) UNION)))
       ((> (+ 2 MAX) (RANGE-MIN (CAR RSA)))
        (RANGE-SET-UNION (CDR RSA) RSB MIN (MAX MAX (RANGE-MAX (CAR RSA))) UNION))
       ((> (+ 2 MAX) (RANGE-MIN (CAR RSB)))
        (RANGE-SET-UNION RSA (CDR RSB) MIN (MAX MAX (RANGE-MAX (CAR RSB))) UNION))
       ))
    (T ;; initial
     (LET ((MIN (MIN (RANGE-MIN (CAR RSA)) (RANGE-MIN (CAR RSB)))))
       (RANGE-SET-UNION RSA RSB  MIN MIN UNION)))))


(DEFUN TEST-RANGE-SET-UNION ()
  (MAP NIL (LAMBDA (TEST)
             (LET ((U (RANGE-SET-UNION (FIRST TEST)  (SECOND TEST)))
                   (V (RANGE-SET-UNION (SECOND TEST) (FIRST TEST))))
               (UNLESS (AND (EQUAL U V) (EQUAL U (THIRD TEST)))
                 (FORMAT T "ERROR:~%a=  ~S~%b=  ~S~%e=  ~S~%u=  ~S~%v=  ~S~2%"
                         (FIRST TEST)  (SECOND TEST) (THIRD TEST) U V))))
       '(
         ( (1 (NIL))
          (3 (NIL))
          (1 3 (NIL)) )
         ( (1 (NIL))
          (2 (NIL))
          ((1 . 2) (NIL)) )
         ( ((1 . 3) (NIL))
          ((5 . 7) (NIL))
          ((1 . 3) (5 . 7) (NIL)) )
         ( ((1 . 3) (NIL))
          ((4 . 6) (NIL))
          ((1 . 6) (NIL)) )
         ( ((1 . 4) (NIL))
          ((4 . 6) (NIL))
          ((1 . 6) (NIL)) )
         ( ((1 . 4) (NIL))
          ((3 . 6) (NIL))
          ((1 . 6) (NIL)) )
         ( ((1 . 4) (NIL))
          ((1 . 6) (NIL))
          ((1 . 6) (NIL)) )
         ( ((1 . 4) (NIL))
          ((0 . 6) (NIL))
          ((0 . 6) (NIL)) )
         ( ((1 . 3) (5 . 7) (9 . 11) (NIL))
          ((3 . 5) (7 . 9) (11 . 13) (NIL))
          ((1 . 13) (NIL)) )
         ( ((1 . 3) (5 . 7) (9 . 11) (NIL))
          (4 8 12 (NIL))
          ((1 . 12) (NIL)) )
         ( ((2 . 6) (10 . 14) (18 . 22) (NIL))
          (0  8 16 34 (NIL))
          (0 (2 . 6) 8 (10 . 14) 16 (18 . 22) 34 (NIL)) ))))


(DEFUN RANGE-SET-CONTAINS-P (RS N)
  (LET ((PREV (RANGE-SET-SEEK RS N)))
    (AND PREV (NOT (RANGE-SET-GUARD-P PREV)) (RANGE-CONTAINS-P (CAR PREV) N))))


(DEFUN MAKE-RANGE-SET-VECTOR (RS)
  (MAKE-ARRAY (LIST (1- (LENGTH RS))) :INITIAL-CONTENTS (BUTLAST RS)))


;; (setq v (make-array '(11) :initial-contents (mapcar (lambda (x) (* 3 x)) '( 1 2 3 4 5 6 7 8 9 10 11 ))))
;; (mapcar (lambda (k) (multiple-value-list (dichotomy-search v k (lambda (a b) (cond ((< a b) -1) ((> a b) 1) (t 0)))))) '(0 1 2 3 4 5 6 7 8 9 30 31 32 33 34 35 ))



;; charset-range
;; -------------


(DEFCLASS CHARSET-RANGE (CHARSET)
  ( (RANGE-SET    :ACCESSOR RANGE-SET
                  :TYPE LIST
                  :INITFORM (MAKE-RANGE-SET))
   (RANGE-VECTOR :ACCESSOR RANGE-VECTOR
                 :TYPE (OR NULL VECTOR)
                 :INITFORM NIL)
    (CHAR-CLASSES :ACCESSOR CHAR-CLASSES
                  :TYPE LIST
                  :INITFORM '())
    (COMPLEMENTED :ACCESSOR COMPLEMENTED
                  :TYPE BOOLEAN
                  :INITFORM NIL) )
  (:DOCUMENTATION
   "A character set representation based on binary trees of ranges
and additional list of character classes.
This is representation may be used when char-code-limit is 'big'."))


(DEFUN RCCOMPARE (RANGE CC)
  (COND ((< CC (RANGE-MIN RANGE)) -1)
        ((> CC (RANGE-MAX RANGE)) +1)
        (T 0)))


(DEFMETHOD ADD-CHAR        ((SELF CHARSET-RANGE) (CH CHARACTER))
  (SETF (RANGE-VECTOR SELF) NIL)
  (RANGE-SET-ADD-NUMBER (RANGE-SET SELF) (CHAR-CODE CH)))


(DEFMETHOD ADD-RANGE       ((SELF CHARSET-RANGE)
                            (MIN CHARACTER) (MAX CHARACTER))
  (SETF (RANGE-VECTOR SELF) NIL)
  (SETF (RANGE-SET SELF)
        (RANGE-SET-UNION (RANGE-SET SELF)
                         (MAKE-RANGE-SET (LIST (MAKE-RANGE (CHAR-CODE MIN)
                                                           (CHAR-CODE MAX)))))))


(DEFMETHOD ADD-CLASS       ((SELF CHARSET-RANGE) (CHAR-CLASS-NAME STRING))
  (LET ((CCF (SECOND (ASSOC CHAR-CLASS-NAME *CHARACTER-CLASSES*
                            :TEST (function STRING=)))))
    (UNLESS CCF (ERROR "Invalid character class ~S." CHAR-CLASS-NAME))
    (PUSHNEW CCF (CHAR-CLASSES SELF))))


(DEFMETHOD INVERSE         ((SELF CHARSET-RANGE))
  "
DO:     complements the set.
"
  (SETF (COMPLEMENTED SELF) (NOT (COMPLEMENTED SELF))))


(DEFMETHOD CONTAINS-CHAR-P ((SELF CHARSET-RANGE) (CH CHARACTER))
  (LET ((CODE (CHAR-CODE CH))
        (RESULT NIL))
    (WHEN (NULL (RANGE-VECTOR SELF))
      (SETF (RANGE-VECTOR SELF) (MAKE-RANGE-VECTOR (RANGE-SET SELF))))
    (MULTIPLE-VALUE-BIND (FOUND INDEX ORDER)
        (DICHOTOMY-SEARCH (RANGE-VECTOR SELF) CODE (FUNCTION RCCOMPARE))
      (DECLARE (IGNORE ORDER))
      (SETF RESULT (AND FOUND (RANGE-CONTAINS-P (AREF (RANGE-VECTOR SELF) INDEX)
                                                CODE))))
    (UNLESS RESULT
      (SETF RESULT (SOME (LAMBDA (CLASSF) (FUNCALL CLASSF CODE))
                         (CHAR-CLASSES SELF))))
    (IF (COMPLEMENTED SELF) (NOT RESULT) RESULT)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching regular expression
;; ---------------------------


(DEFUN LIGHTEN (OBJ)
  (COND
    ((SYMBOLP OBJ) (SYMBOL-NAME OBJ))
    ((CONSP OBJ)   (CONS (LIGHTEN (CAR OBJ)) (LIGHTEN (CDR OBJ))))
    (T             OBJ))) ;;LIGHTEN


(DEFUN PRINT-RNODE (NODE STREAM LEVEL)
  (declare (ignore level))
  (FORMAT STREAM "<~A~{ ~S~}>"
          (LIGHTEN (RNODE-TOKEN NODE))
          (MAP 'LIST (FUNCTION IDENTITY) (RNODE-CHILDREN NODE))))


(DEFSTRUCT (RNODE
             (:PRINT-FUNCTION PRINT-RNODE))
  "A rnode represent a compiled regexp node"
  ;; code:
  (MATCHF   NIL)
  (TOKEN    NIL)
  (CHILDREN NIL :TYPE (OR NULL (ARRAY RNODE (*)))))
;; (equiv (null children) (not (< 0 (length children)))))



(DEFMACRO WITH-RNODE (NODE &BODY BODY)
  `(WITH-SLOTS ((MATCHF   MATCHF)
                (TOKEN    TOKEN)
                (CHILDREN CHILDREN))
       ,NODE ,@BODY))


(DEFMACRO RNODE-MATCH (NODE STATE ENV)
  (IF (SYMBOLP NODE)
      `(FUNCALL (RNODE-MATCHF ,NODE) ,NODE ,STATE ,ENV)
      `(LET ((NODE ,NODE))
         (FUNCALL (RNODE-MATCHF NODE) NODE ,STATE ,ENV))))


(DEFUN RNODE-WALK (NODE FUN)
  (FUNCALL FUN NODE)
  (MAP NIL (LAMBDA (NODE) (RNODE-WALK NODE FUN)) (RNODE-CHILDREN NODE)))


(DEFSTRUCT (RSTATE (:PRINT-FUNCTION
                    (LAMBDA (STATE STREAM LEVEL)
                      (declare (ignore level))
                      (FORMAT STREAM "<rstate :try ~S :start ~S :end ~S>"
                              (rstate-TRY   STATE)
                              (rstate-START STATE)
                              (rstate-END   STATE)))))
  "State data used when matching a rnode."
  (TRY      NIL)
  (START    0   :TYPE (OR NULL (INTEGER 0)))
  (END      0   :TYPE (OR NULL (INTEGER 0)))) ;;RSTATE


(DEFMACRO WITH-RSTATE (STATE &BODY BODY)
  `(WITH-SLOTS ((START    START)
                (END      END)
                (TRY      TRY))
       ,STATE ,@BODY)) ;;WITH-RSTATE


(DEFUN RSTATE-RETRY (STATE POSITION)
  (WITH-RSTATE STATE (SETF START POSITION END NIL TRY NIL)))


(DEFMACRO TRY-ONCE (&BODY BODY)
  `(IF TRY NIL (PROGN (SETF TRY T) ,@BODY)))


(DEFMACRO TRY (&REST CLAUSES)
  "
SYNTAX:  (try (initially [sexp|(immediately-then)]...)
              (then sexp...))
"
  (LET ((INITIAL NIL)
        (THEN    NIL))
    (DO ((CLAUSES CLAUSES (CDR CLAUSES)))
        ((NULL CLAUSES))
      (COND
        ((STRING-EQUAL (CAAR CLAUSES) :INITIALLY)
         (IF INITIAL
             (ERROR "Can't have two (initially ...) in try.")
             (SETF INITIAL (CDAR CLAUSES))))
        ((STRING-EQUAL (CAAR CLAUSES) :THEN)
         (IF THEN
             (ERROR "Can't have two (then ...) in try.")
             (SETF THEN (CDAR CLAUSES))))
        (T
         (ERROR "Invalid clause (~S) in try." (CAAR CLAUSES)))))
    (IF INITIAL
        (IF THEN
            ;; both initial and then:
            (WITH-GENSYMS (LABEL-TRY LABEL-THEN)
              `(BLOCK ,LABEL-TRY
                 (TAGBODY
                    (UNLESS TRY
                      (MACROLET ((IMMEDIATELY-THEN () `(GO ,',LABEL-THEN)))
                        (SETF TRY T)
                        (RETURN-FROM ,LABEL-TRY (PROGN ,@INITIAL))))
                    ,LABEL-THEN
                    (RETURN-FROM ,LABEL-TRY (PROGN ,@THEN))))) 
            ;; initial alone:
            `(UNLESS TRY ,@INITIAL))
        (IF THEN
            ;; then alone:
            `(WHEN TRY ,@THEN)
            ;; nothing
            (VALUES)))))


(DEFSTRUCT RENV
  "An renv gather the environment (variables) used to run a compiled
regexp matched, ie. rnode."
  (EQUALF   (FUNCTION EQUAL)) ;; use equalp for case insensitive.
  ;; equalf must take two sequence arguments
  ;; and accept :start1 :end1 :start2 :end2 keys.
  (NEWLINEPF (COMPILE NIL (LAMBDA (CH) (EQL #\NEWLINE CH))))
  (SEQUENCE ""  :TYPE VECTOR) ;; renv-set-sequence sets length and position too.
  (LENGTH   0   :TYPE (INTEGER 0))
  (POSITION 0   :TYPE (INTEGER 0))
  (SUBEXPS  NIL :TYPE (OR NULL (VECTOR CONS *)))
  (REGEXP   NIL :TYPE (OR NULL RNODE)) ;; renv-set-regexp sets subexps too.
  (BOL      T   :TYPE BOOLEAN)
  (EOL      T   :TYPE BOOLEAN)
  (NEWLINE  T   :TYPE BOOLEAN)
  )


(DEFMACRO WITH-RENV (ENV &BODY BODY)
  `(WITH-SLOTS ((EQUALF     EQUALF   )
                (NEWLINEPF  NEWLINEPF)
                (SEQUENCE   SEQUENCE )
                (LENGTH     LENGTH   )
                (POSITION   POSITION )
                (SUBEXPS    SUBEXPS  )
                (REGEXP     REGEXP   )
                (BOL        BOL      )
                (EOL        EOL      )
                (NEWLINE    NEWLINE  ))
       ,ENV ,@BODY))


(DEFUN SUBEXP-FILLED-P (SUBEXP)         SUBEXP)
(DEFMACRO SUBEXP-CLEAR (SUBEXP)         `(POP ,SUBEXP))
(DEFMACRO SUBEXP-SET (SUBEXP START END) `(PUSH (CONS ,START ,END) ,SUBEXP))
(DEFMACRO SUBEXP-START (SUBEXP)         `(CAR (TOP ,SUBEXP)))
(DEFMACRO SUBEXP-END   (SUBEXP)         `(CDR (TOP ,SUBEXP)))


(DEFUN RENV-SET-SEQUENCE (ENV NEW-SEQ)
  (WITH-RENV ENV
    (SETF SEQUENCE (COND
                     ((VECTORP NEW-SEQ) NEW-SEQ)
                     ((LISTP   NEW-SEQ) (MAKE-ARRAY (LIST (LENGTH NEW-SEQ))
                                                    :INITIAL-CONTENTS NEW-SEQ))
                     (T (ERROR "Can match only vectors and lists.")))
          LENGTH   (LENGTH SEQUENCE)
          POSITION 0)))


(DEFUN RENV-SET-REGEXP (ENV REGEXP)
  (LET ((CREGEXP (IF (RNODE-P REGEXP) REGEXP  (COMPILE-REGEXP REGEXP)))
        (SUBEXP-NUM  0))
    (RNODE-WALK CREGEXP (LAMBDA (NODE)
                          (WITH-RNODE NODE
                            (WHEN (AND (CONSP TOKEN)
                                       (EQ 'RK:SUBEXP (CAR TOKEN)))
                              (SETF (CDR TOKEN) SUBEXP-NUM)
                              (INCF SUBEXP-NUM)))))
    (WITH-RENV ENV
      (SETF REGEXP  CREGEXP
            SUBEXPS (MAKE-ARRAY (LIST SUBEXP-NUM)))
      (DOTIMES (I SUBEXP-NUM)
        (SETF (AREF SUBEXPS I) NIL)))))



;;(dolist (s '(collating-symbol equivalence-class character-class range any l-anchor r-anchor matching non-matching backref subexp sequence repeat infinity alternative b-anchor e-anchor item set-sequence)) (insert (format "(defun rmatch-%s (node env)\n)\n\n\n" s)))


(DEFMACRO WITH-RENS (ENV NODE STATE &BODY BODY)
  `(WITH-RENV ,ENV
     (WITH-RNODE ,NODE
       (WITH-RSTATE ,STATE
         ,@BODY))))


(DEFUN RMATCH-B-ANCHOR (NODE STATE ENV)
  "
Beginning of string anchor.
"
  (WITH-RENS ENV NODE STATE
    (declare (ignorable node))
    (TRY-ONCE
      (IF (= 0 POSITION)
          (PROGN (SETF END POSITION) T)
          NIL))))


(DEFUN RMATCH-E-ANCHOR (NODE STATE ENV)
  "
End of string anchor.
"
  (WITH-RENS ENV NODE STATE
    (declare (ignorable node))
    (TRY-ONCE
      (IF (= LENGTH POSITION)
          (PROGN (SETF END POSITION) T)
          NIL))))


(DEFUN RMATCH-L-ANCHOR (NODE STATE ENV)
  "
Beginning of line anchor.
"
  (WITH-RENS ENV NODE STATE
    (declare (ignorable node))
    (TRY-ONCE
      (IF (OR (AND BOL (= 0 POSITION))
              (AND NEWLINE
                   (< 1 POSITION)
                   (FUNCALL NEWLINEPF (AREF SEQUENCE (1- POSITION)))))
          (PROGN (SETF END POSITION) T)
          NIL))))


(DEFUN RMATCH-R-ANCHOR (NODE STATE ENV)
  "
End of line anchor.
"
  (WITH-RENS ENV NODE STATE
    (declare (ignorable node))
    (TRY-ONCE
      (IF (OR (AND EOL (= LENGTH POSITION))
              (AND NEWLINE
                   (< (1+ POSITION) (LENGTH SEQUENCE))
                   (FUNCALL NEWLINEPF (AREF SEQUENCE (1+ POSITION)))))
          (PROGN (SETF END POSITION) T)
          NIL))))


(DEFUN RMATCH-ANY (NODE STATE ENV)
  (WITH-RENS ENV NODE STATE
    (declare (ignorable node))
    (TRY-ONCE
      (IF (OR (<= LENGTH POSITION)
              (AND NEWLINE ;; don't match newline
                   (FUNCALL NEWLINEPF (AREF SEQUENCE POSITION))))
          NIL
          (PROGN
            (INCF POSITION)
            (SETF END POSITION)
            T)))))


(DEFUN RMATCH-ITEM (NODE STATE ENV)
  (WITH-RENS ENV NODE STATE
    (declare (ignorable node))
    (TRY-ONCE
      (IF (AND (< POSITION LENGTH)
               (FUNCALL EQUALF TOKEN (AREF SEQUENCE POSITION)))
          (PROGN
            (INCF POSITION)
            (SETF END POSITION)
            T)
          NIL))))


(DEFUN RMATCH-SEQUENCE (NODE STATE ENV)
  (WITH-RENS ENV NODE STATE
    (IF CHILDREN
        (INVARIANT (OR (NULL TRY)
                       (AND (INTEGERP TRY)
                            (OR (= 0 TRY) (= TRY (1- (LENGTH CHILDREN))))))
          (LET (P N)
            ;; try = ( n state[n-1] state[n-1] ... state[0] )
            ;; n is the number of children matched in sequence so far.
            ;; follows the states of the n children in reverse order (stack).
            ;; We exit with either 0 or (1- (length children)).
            ;;
            ;; The 'position' pointer advances and tracks back, walking the
            ;; tree of matching child position until we find one (or more,
            ;; in following tries) matching sequence.
            (TRY (INITIALLY  (SETF N 0)
                             (PUSH (MAKE-RSTATE :START POSITION) TRY))
                 (THEN       (SETF N (POP TRY))))
            (SETF P (RNODE-MATCH (AREF CHILDREN N) (TOP TRY) ENV))
            (WHILE (OR (AND P (< (1+ N) (LENGTH CHILDREN)))
                       (AND (NOT P) (<= 0 (1- N))))
              (IF P
                  (PROGN
                    (INCF N)
                    (PUSH (MAKE-RSTATE :START POSITION) TRY))
                  (PROGN ;; backtrack
                    (DECF N)
                    (POP TRY)
                    (SETF POSITION (RSTATE-START (TOP TRY)))))
              (SETF P (RNODE-MATCH (AREF CHILDREN N) (TOP TRY) ENV)))
            (PUSH N TRY)
            (SETF END POSITION)
            P))
        (TRY-ONCE ;; no child -- empty sequence -- match once.
          (SETF END POSITION)
          T))))


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


(DEFUN RMATCH-REPEAT-SHY (NODE STATE ENV)
  "
DO:    match min to max repeatition, smallest first.
"
  (WITH-RENS ENV NODE STATE
             (IF (AND CHILDREN (LESSP 0 (CDR TOKEN)))
                 (LET ((MIN (CAR TOKEN))
                       (MAX (CDR TOKEN))
                       (CHILD (AREF CHILDREN 0))
                       P N)
                   (IF (LESSP MAX MIN)
                       NIL ;; never match.
                       ;; try = ( n state[n-1] state[n-1] ... state[0] )
                       ;; n is the number of children matched in sequence so far.
                       ;; follows the states of the n children in reverse order (stack).
                       ;;
                       ;; The 'position' pointer advances and tracks back, walking the
                       ;; tree of matching child position until we find one (or more,
                       ;; in following tries) matching sequence.
                       (TAGBODY
                        :INIT      (TRY (INITIALLY
                                         (SETF TRY NIL N 0  P POSITION)
                                         (IF (< N MIN)
                                             (GO :FILL)
                                             (GO :MATCH)))
                                        (THEN
                                         (WHEN (EQ :FAILED TRY)
                                           (RETURN-FROM RMATCH-REPEAT-SHY NIL))
                                         (SETF N (POP TRY))
                                         (IF (= 0 N)
                                             (GO :ADD)
                                             (GO :INCREMENT))))
                        :FILL      (PROGN
                                     (PUSH (MAKE-RSTATE :START P) TRY)
                                     (INCF N)
                                     (SETF P (RNODE-MATCH CHILD (TOP TRY) ENV))
                                     (COND
                                       ((NOT P)   (GO :REMOVE-1))
                                       ((< N MIN) (GO :FILL))
                                       (T         (GO :MATCH)))) ;; n=min
                        :ADD       (PROGN
                                     (PUSH (MAKE-RSTATE :START P) TRY)
                                     (INCF N)
                                     (SETF P (RNODE-MATCH CHILD (TOP TRY) ENV))
                                     (IF P
                                         (GO :MATCH)
                                         (GO :REMOVE-1)))
                        :REMOVE-1  (PROGN
                                     (POP TRY)
                                     (DECF N)
                                     (COND
                                       ((<= MIN N)  (GO :MATCH))
                                       ((= 0 N)     (GO :FAIL))
                                       (T           (GO :INCREMENT))))
                        :INCREMENT (PROGN
                                     (SETF P (RNODE-MATCH CHILD (TOP TRY) ENV))
                                     (COND
                                       ((NOT P)       (GO :REMOVE-1))
                                       ((< N MIN)     (GO :FILL))
                                       ((LESSP N MAX) (GO :ADD))
                                       (T             (GO :MATCH))))
                        :MATCH     (PROGN
                                     (SETF END POSITION)
                                     (PUSH N TRY)
                                     (RETURN-FROM RMATCH-REPEAT-SHY P))
                        :FAIL      (PROGN
                                     (SETF TRY :FAILED)
                                     (RETURN-FROM RMATCH-REPEAT-SHY NIL)) )))
                 (TRY-ONCE ;; max=0 or no child -- empty sequence -- match once.
                  (SETF END POSITION)
                  T))))


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


(DEFUN RMATCH-REPEAT-GREEDY (NODE STATE ENV)
  "
DO:    match min to max repeatition, greatest first.
"
  (WITH-RENS ENV NODE STATE
             (IF (AND CHILDREN (LESSP 0 (CDR TOKEN)))
                 (LET ((MIN (CAR TOKEN))
                       (MAX (CDR TOKEN))
                       (CHILD (AREF CHILDREN 0))
                       P N)
                   (IF (LESSP MAX MIN)
                       NIL ;; never match.
                       ;; try = ( n state[n-1] state[n-1] ... state[0] )
                       ;; n is the number of children matched in sequence so far.
                       ;; follows the states of the n children in reverse order (stack).
                       ;;
                       ;; The 'position' pointer advances and tracks back, walking the
                       ;; tree of matching child position until we find one (or more,
                       ;; in following tries) matching sequence.
                       (TAGBODY
                        :INIT      (TRY (INITIALLY
                                         (SETF TRY NIL N 0  P POSITION)
                                         (GO :FILL))
                                        (THEN
                                         (WHEN (EQ :FAILED TRY)
                                           (RETURN-FROM RMATCH-REPEAT-GREEDY NIL))
                                         (SETF N (POP TRY))
                                         (GO :INCREMENT)))
                        :FILL      (PROGN
                                     (ASSERT (LESSP N MAX))
                                     (PUSH (MAKE-RSTATE :START P) TRY)
                                     (INCF N)
                                     (SETF P (RNODE-MATCH CHILD (TOP TRY) ENV))
                                     (COND
                                       ((NOT P)       (GO :REMOVE-1))
                                       ((LESSP N MAX) (GO :FILL))
                                       (T             (GO :MATCH)))) ;; n=max
                        :REMOVE-1  (PROGN
                                     (POP TRY)
                                     (DECF N)
                                     (COND
                                       ((<= MIN N)  (GO :MATCH))
                                       ((= 0 N)     (GO :FAIL))
                                       (T           (GO :INCREMENT))))
                        :INCREMENT (PROGN
                                     (SETF P (RNODE-MATCH CHILD (TOP TRY) ENV))
                                     (COND
                                       ((NOT P)       (GO :REMOVE-1))
                                       ((LESSP N MAX) (GO :FILL))
                                       (T             (GO :MATCH))))
                        :MATCH     (PROGN
                                     (SETF END POSITION)
                                     (PUSH N TRY)
                                     (RETURN-FROM RMATCH-REPEAT-GREEDY P))
                        :FAIL      (PROGN
                                     (SETF TRY :FAILED)
                                     (RETURN-FROM RMATCH-REPEAT-GREEDY NIL)) )))
                 (TRY-ONCE ;; max=0 or no child -- empty sequence -- match once.
                  (SETF END POSITION)
                  T))))
         

(DEFUN RMATCH-ALTERNATIVE (NODE STATE ENV)
  (WITH-RENS ENV NODE STATE
             (IF CHILDREN
                 (PROGN
                   (TRY
                    ;; try = (index of alternative tried.
                    (INITIALLY (SETF TRY (CONS 0 (MAKE-RSTATE :START POSITION)))))
                   (LOOP
                      (LET ((CHILD (AREF CHILDREN (CAR TRY))))
                        (IF (RNODE-MATCH CHILD (CDR TRY) ENV)
                            (PROGN
                              (SETF END POSITION)
                              (RETURN-FROM RMATCH-ALTERNATIVE T))
                            (PROGN
                              (INCF (CAR TRY))
                              (IF (< (CAR TRY) (LENGTH CHILDREN))
                                  (RSTATE-RETRY (CDR TRY) (RSTATE-START (CDR TRY)))
                                  (RETURN-FROM RMATCH-ALTERNATIVE NIL)))))))
                 (TRY-ONCE ;; no child -- empty alternative -- match once.
                  (SETF END POSITION)
                  T))))


(DEFUN RMATCH-SUBEXP (NODE STATE ENV)
  (WITH-RENS ENV NODE STATE
             ;; token = (cons 'rk:subexp subexp-index)
             ;; one child
             (IF CHILDREN
                 (LET ((INDEX (CDR TOKEN))
                       (CHILD (AREF CHILDREN 0)))
                   (TRY
                    (INITIALLY
                     (SETF TRY (MAKE-RSTATE :START POSITION)))
                    (THEN
                     (WHEN (EQ :FAILED TRY)
                       (RETURN-FROM RMATCH-SUBEXP NIL))))
                   (LET ((P (RNODE-MATCH CHILD TRY ENV)))
                     (IF P
                         (PROGN
                           (SETF END POSITION)
                           (SUBEXP-SET (AREF SUBEXPS INDEX) START END))
                         (PROGN
                           (SETF TRY :FAILED)
                           (SUBEXP-CLEAR (AREF SUBEXPS INDEX))))
                     P))
                 (let ((index (cdr token)))
                   (TRY
                    ;; no child : match once
                    (INITIALLY (SUBEXP-SET   (AREF SUBEXPS INDEX) POSITION POSITION) T)
                    (THEN      (SUBEXP-CLEAR (AREF SUBEXPS INDEX))   NIL))))))


(DEFUN RMATCH-BACKREF (NODE STATE ENV)
  (WITH-RENS ENV NODE STATE
             (TRY-ONCE
              (LET ((INDEX TOKEN))
                (IF (AND (NUMBERP INDEX) (<= 0 INDEX) (< INDEX (LENGTH SUBEXPS))
                         (SUBEXP-FILLED-P (AREF SUBEXPS INDEX)))
                    (LET* ((MATCH (AREF SUBEXPS INDEX))
                           (E (+ POSITION (- (SUBEXP-END   MATCH)
                                             (SUBEXP-START MATCH)))))
                      (IF (AND (<= E LENGTH)
                               (FUNCALL EQUALF SEQUENCE SEQUENCE
                                        :START1 (SUBEXP-START MATCH)
                                        :END1   (SUBEXP-END   MATCH)
                                        :START2 POSITION
                                        :END2   E))
                          (PROGN (SETF END E) T)
                          NIL))
                    (ERROR "Invalid back reference (\\~S)." INDEX))))))


(DEFUN RMATCH-MATCHING (NODE STATE ENV)
  (WITH-RENS ENV NODE STATE
             (TRY-ONCE
              (IF (AND (< POSITION LENGTH)
                       (CONTAINS-CHAR-P TOKEN (AREF SEQUENCE POSITION)))
                  (PROGN
                    (INCF POSITION)
                    (SETF END POSITION)
                    T)
                  NIL))))


(DEFUN RMATCH-NON-MATCHING (NODE STATE ENV)
  (WITH-RENS ENV NODE STATE
             (TRY-ONCE
              (IF (OR (<= LENGTH POSITION)
                      (AND NEWLINE ;; don't match newline
                           (FUNCALL NEWLINEPF (AREF SEQUENCE POSITION)))
                      (CONTAINS-CHAR-P TOKEN (AREF SEQUENCE POSITION)))
                  NIL
                  (PROGN
                    (INCF POSITION)
                    (SETF END POSITION)
                    T)))))


(DEFPARAMETER *MATCH-FUNCTION-ALIST*
  `(
    (RK:B-ANCHOR             . ,(FUNCTION RMATCH-B-ANCHOR))
    (RK:E-ANCHOR             . ,(FUNCTION RMATCH-E-ANCHOR))
    (RK:L-ANCHOR             . ,(FUNCTION RMATCH-L-ANCHOR))
    (RK:R-ANCHOR             . ,(FUNCTION RMATCH-R-ANCHOR))
    (RK:ANY                  . ,(FUNCTION RMATCH-ANY))
    (RK:ITEM                 . ,(FUNCTION RMATCH-ITEM))
    (RK:SEQUENCE             . ,(FUNCTION RMATCH-SEQUENCE))
    (RK:REPEAT               . ,(FUNCTION RMATCH-REPEAT-GREEDY))
    (RK:REPEAT-SHY           . ,(FUNCTION RMATCH-REPEAT-SHY))
    (RK:ALTERNATIVE          . ,(FUNCTION RMATCH-ALTERNATIVE))
    (RK:SUBEXP               . ,(FUNCTION RMATCH-SUBEXP))
    (RK:BACKREF              . ,(FUNCTION RMATCH-BACKREF))
    ;; ---
    (RK:MATCHING             . ,(FUNCTION RMATCH-MATCHING))
    (RK:NON-MATCHING         . ,(FUNCTION RMATCH-NON-MATCHING))))
    ;;    (rk:collating-symbol     . ,(function rmatch-collating-symbol))
    ;;    (rk:equivalence-class    . ,(function rmatch-equivalence-class))
    ;;    (rk:character-class      . ,(function rmatch-character-class))
    ;;    (rk:range                . ,(function rmatch-range))))


(DEFUN FIND-MATCH-FUNCTION (NODE)
  (LET ((ASS (ASSOC NODE *MATCH-FUNCTION-ALIST*)))
    (IF ASS (CDR ASS) (CDR (ASSOC 'RK:ITEM *MATCH-FUNCTION-ALIST*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mexpr --> (or
;;            character
;;            (rk:collating-symbol  string/collating-element)
;;            (rk:equivalence-class string/equivalence-class)
;;            (rk:character-class   string/class-name)
;;            (rk:range (or coll-elem-single collating-symbol)/start
;;                      (or coll-elem-single collating-symbol)/end))

(DEFUN COMPILE-BRACKET-EXPRESSION (REGEXP)
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
  (LET ((CHARSET (MAKE-CHARSET)))
    (DO ((ITEMS (CDR REGEXP) (CDR ITEMS)))
        ((NULL ITEMS) CHARSET)
      (COND
        ((CHARACTERP (CAR ITEMS))
         (ADD-CHAR CHARSET (CAR ITEMS)))
        ((ATOM (CAR ITEMS))
         (ERROR "Invalid atom in bracket expression ~S." (CAR ITEMS)))
        ((EQ (CAAR ITEMS) 'RK:COLLATING-SYMBOL)
         (ERROR "Collating symbols are not implemented yet."))
        ((EQ (CAAR ITEMS) 'RK:EQUIVALENCE-CLASS)
         (ERROR "Equivalence classes are not implemented yet."))
        ((EQ (CAAR ITEMS) 'RK:CHARACTER-CLASS)
         (ADD-CLASS CHARSET (SECOND (CAR ITEMS))))
        ((EQ (CAAR ITEMS) 'RK:RANGE)
         (LET ((MIN (SECOND (CAR ITEMS)))
               (MAX (THIRD  (CAR ITEMS))) )
           (WHEN (OR (LISTP MIN) (LISTP MAX))
             (ERROR "Collating symbols are not implemented yet."))
           (ADD-RANGE CHARSET MIN MAX)))
        (T (ERROR "Unexpected item in bracket expression ~S." (CAR ITEMS)))))))


(DEFUN COMPILE-REGEXP (REGEXP)
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
  (MACROLET
      ((MNODE
           (MFKEY TOKEN &OPTIONAL (CHILDREN NIL))
         `(MAKE-RNODE
           :MATCHF (FIND-MATCH-FUNCTION ,MFKEY)
           :TOKEN ,TOKEN
           :CHILDREN (WHEN ,CHILDREN
                       (MAKE-ARRAY (LIST (LENGTH ,CHILDREN))
                                   :INITIAL-CONTENTS
                                   (MAPCAR (FUNCTION COMPILE-REGEXP) ,CHILDREN))))
         ))
    (IF (LISTP REGEXP)
        (CASE (CAR REGEXP)
          ((RK:BACKREF)
           (MNODE (FIRST REGEXP) (SECOND REGEXP)))
          ((RK:SUBEXP)
           (MNODE (CAR REGEXP) (CONS (CAR REGEXP) 0) (CDR REGEXP)))
          ((RK:REPEAT RK:REPEAT-SHY)
           (WHEN (< 1 (LENGTH (CDDDR REGEXP)))
             (ERROR "Found a ~A with more than one child." (CAR REGEXP)))
           (MNODE (FIRST REGEXP)
                  (CONS (SECOND REGEXP) (THIRD REGEXP)) (CDDDR REGEXP)))
          ((RK:MATCHING RK:NON-MATCHING)
           (MNODE (FIRST REGEXP) (COMPILE-BRACKET-EXPRESSION REGEXP)))
          (OTHERWISE
           (MNODE (CAR REGEXP) (CAR REGEXP) (CDR REGEXP))))
        (MNODE REGEXP REGEXP))))


(DEFUN COUNT-SUBEXP (REGEXP)
  "
RETURN: The number of subexp found in regexp
"
  (LET ((COUNT 0))
    (LABELS ((WALK (REGEXP)
               (WHEN (LISTP REGEXP)
                 (CASE (CAR REGEXP)
                   ((RK:SEQUENCE RK:ALTERNATIVE)
                    (MAP NIL (FUNCTION WALK) (CDR REGEXP)))
                   ((RK:REPEAT RK:REPEAT-SHY)
                    (MAP NIL (FUNCTION WALK) (CDDDR REGEXP)))
                   ((RK:SUBEXP)
                    (INCF COUNT)
                    (MAP NIL (FUNCTION WALK) (CDR REGEXP))))) ))
      (WALK REGEXP))
    COUNT))



(DEFSTRUCT MATCH
  "This structure stores a (start,end) couple specifying the range matched
by a group (or the whole regexp)."
  (START NIL :TYPE (OR NULL INTEGER))
  (END   NIL :TYPE (OR NULL INTEGER)))


(DEFUN MATCH-STRING (STRING MATCH)
  "Extracts the substring of STRING corresponding to a given pair of
start and end indices. The result is shared with STRING.
If you want a freshly consed string, use copy-string
or (coerce (match-string ...) 'simple-string)."
  (SUBSEQ STRING (MATCH-START MATCH) (MATCH-END MATCH)))


(DEFUN REGEXP-QUOTE (STRING)
  (DECLARE (IGNORE STRING))
  (ERROR "Not Implemented Yet: REGEXP-QUOTE~%" ))


(defun MAKE-RANGE-VECTOR (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun PJB-RE-PARSE-WHOLE-REGEXP (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun PJB-RE-DECORATE-TREE (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun PJB-RE-COLLECT-GROUPS (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun PJB-RE-INIT (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun PJB-RE-MATCH (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun PJB-RE-SLOT-BEGIN (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun PJB-RE-SLOT-END (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))


(DEFUN MATCH (REGEXP STRING &OPTIONAL START END)
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
  (UNLESS START (SETQ START 0))
  (UNLESS END   (SETQ END (LENGTH STRING)))
  (WHEN (< END START) (SETQ END START))
  ;; TODO: What to do when start or end are out of bounds ?

  (LET* ((SYN-TREE
          (PJB-RE-PARSE-WHOLE-REGEXP
           (MAKE-SC :string
                    (CONCATENATE 'STRING "\\`.*?\\(" REGEXP "\\).*?\\'"))))
         (DEC-TREE (PJB-RE-DECORATE-TREE SYN-TREE STRING))
         (GROUPS  (PJB-RE-COLLECT-GROUPS DEC-TREE))
         )

    (PJB-RE-INIT DEC-TREE START)
    (PJB-RE-MATCH DEC-TREE)
    ;; there's nowhere to backtrack at the top level...
    (VALUES-LIST (MAPCAR (LAMBDA (G)
                           (LET ((S (PJB-RE-SLOT-BEGIN G))
                                 (E (PJB-RE-SLOT-END G)) )
                             (IF (AND S E)
                                 (MAKE-MATCH :START S :END E)
                                 (MAKE-MATCH :START NIL :END NIL))))
                         GROUPS))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POSIX API
;;; ---------


(DEFTYPE   SIZE-T   () "Used for sizes of objects." '(INTEGER 0 *))


(DEFTYPE   REGOFF-T ()
  "The type regoff_t shall be defined as a signed integer type that
can hold the largest value that can be stored in either a type off_t
or type ssize_t. "
  'INTEGER) ;;REGOFF-T

  
(DEFSTRUCT (REGEX-T
             (:CONC-NAME "RE-"))
  "
NSUB:  Number of parenthesized subexpressions.
"
  (NSUB 0 :TYPE SIZE-T)
  ;; the rest is private!
  (ENV         NIL :TYPE (OR NULL RENV))
  (EXTENDED    NIL :TYPE BOOLEAN)
  (IGNORE-CASE NIL :TYPE BOOLEAN)
  (NOSUB       NIL :TYPE BOOLEAN)
  (NEWLINE     NIL :TYPE BOOLEAN))


(DEFSTRUCT (REGMATCH-T
             (:CONC-NAME "RM-"))
  "
SO:  Byte offset from start of string to start of substring.
EO:  Byte offset from start of string of the first character
     after the end of substring.
"
  (SO -1 :TYPE REGOFF-T)
  (EO -1 :TYPE REGOFF-T))


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


(DEFUN REGCOMP (PATTERN
                &KEY (EXTENDED NIL) (IGNORE-CASE NIL) (NOSUB NIL) (NEWLINE NIL))
  "
RETURN:  A regex-t representing the compiled regular expression PATTERN.
RAISE:   An ERROR condition, in case of syntax error.
"
  (LET ((REGEXP  (IF EXTENDED
                     (PARSE-EXTENDED-RE PATTERN)
                     (PARSE-BASIC-RE    PATTERN))))
    (IF (ERRORP REGEXP)
        (ERROR "~D:~A" (THIRD REGEXP) (FOURTH REGEXP))
        (LET ((ENV (MAKE-RENV :EQUALF (IF IGNORE-CASE
                                          (FUNCTION EQUALP) (FUNCTION EQUAL))
                              :NEWLINE NEWLINE)))
          (RENV-SET-REGEXP ENV
                           `(RK:SEQUENCE RK:B-ANCHOR
                                         (RK:REPEAT-SHY 0 RK:INFINITY RK:ANY)
                                         (RK:SUBEXP ,REGEXP)
                                         (RK:REPEAT-SHY 0 RK:INFINITY RK:ANY)
                                         RK:E-ANCHOR))
          (MAKE-REGEX-T :NSUB (COUNT-SUBEXP REGEXP) :ENV ENV
                        :EXTENDED EXTENDED :IGNORE-CASE IGNORE-CASE
                        :NOSUB  NOSUB :NEWLINE NEWLINE)))))


(DEFUN REGEXEC (REGEX STRING &KEY (NMATCH NIL) (BOL T) (EOL T))
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
  (WHEN (OR (NULL NMATCH)
            (RE-NOSUB REGEX)
            (AND (NUMBERP NMATCH) (ZEROP NMATCH)))
    (SETF NMATCH NIL))
  (WITH-SLOTS ((env ENV)) REGEX
    (SETF (RENV-BOL ENV) BOL
          (RENV-EOL ENV) EOL)
    (RENV-SET-SEQUENCE ENV STRING)
    (IF (RNODE-MATCH (RENV-REGEXP ENV) (MAKE-RSTATE) ENV)
        (VALUES T ;; matches
                (IF (NULL NMATCH)
                    NIL
                    (LET* ((SIZE (IF (NUMBERP NMATCH)
                                     NMATCH ;;(min nmatch(length(renv-subexps env)))
                                     (LENGTH (RENV-SUBEXPS ENV))))
                           (RESULT (MAKE-ARRAY (LIST SIZE)
                                               :ELEMENT-TYPE 'REGMATCH-T)))
                      (DOTIMES (I SIZE)
                        (SETF (AREF RESULT I)
                              (IF (< I (LENGTH (RENV-SUBEXPS ENV)))
                                  (LET ((SE (AREF (RENV-SUBEXPS ENV) I)))
                                    (IF (SUBEXP-FILLED-P SE)
                                        (MAKE-REGMATCH-T :SO (SUBEXP-START SE)
                                                         :EO (SUBEXP-END   SE))
                                        (MAKE-REGMATCH-T)))
                                  (MAKE-REGMATCH-T))))
                      RESULT)))
        (VALUES NIL     ;; does not match
                NIL))))


                                     
;;;; THE END ;;;;
