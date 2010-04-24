;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               regexp-emacs.lisp
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
;;;;    packages are available under clisp or emacs.  Thus it as the advantage
;;;;    of portability and availability (you don't have to compile or link
;;;;    a lisp system written in some barbarous language, and you get the same
;;;;    regexp features in all programs including this module).
;;;;
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2002
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.REGEXP-EMACS"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.TREE-TO-ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING" "COMMON-LISP")
  (:EXPORT "REGEXP-MATCH" "REGEXP-QUOTE" "MATCH-STRING" "MATCH-END"
           "MATCH-START" "MATCH")
  (:DOCUMENTATION
   "This package implement REGEXP in COMMON-LISP,
    which is interesting because then it's available on any COMMON-LISP platform
    whether the external C regexp library is available or not, and moreover,
    it's the same (that is, it's compatible) on all COMMON-LIST platforms.

    NOT COMPLETE YET.

    Copyright Pascal J. Bourguignon 2002 - 2002
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.REGEXP-EMACS")




;;; (DEFUN EMACS-STRING-MATCH (REGEXP STRING &OPTIONAL START)
;;;   "
;;; DO:      Plain emacs string-match
;;;          just returning same result as match.
;;; "
;;;   (LET* ((START (STRING-MATCH REGEXP STRING START))
;;;          (MD    (MATCH-DATA T))
;;;          (COUNT (1+ (LOOP FOR START = 0 THEN (+ 2 MATCH)
;;;                           FOR MATCH = (STRING-MATCH "\\\\(" REGEXP START)
;;;                           WHILE MATCH
;;;                           SUM 1 INTO COUNT
;;;                           FINALLY (RETURN COUNT)))) )
;;;     (VALUES-LIST 
;;;      (LOOP FOR I FROM 0 BELOW COUNT
;;;            FOR DATA = MD  THEN (CDDR DATA)
;;;            FOR S = (CAR DATA)
;;;            FOR E = (CADR DATA)
;;;            COLLECT (IF (AND S E)
;;;                        (MAKE-MATCH :START S :END E)
;;;                      (MAKE-MATCH :START NIL :END NIL))
;;;            INTO VALUES
;;;            FINALLY (RETURN VALUES)))
;;;     )) ;;emacs-string-match



(DEFUN PJB-RE-SPLIT-STRING (STRING &OPTIONAL SEPARATORS)
  "
DO:         Splits STRING into substrings where there are matches
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
          DO (SETQ NEXTPOS (1+ NEXTPOS))
          ) ;;loop
       (PUSH (SUBSEQ STRING POSITION NEXTPOS) CHUNKS)
       (SETQ POSITION (1+ NEXTPOS))
       (SETQ NEXTPOS  POSITION)
       ) ;;loop
    (NREVERSE CHUNKS)
    ) ;;let
  )   ;;PJB-RE-SPLIT-STRING




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regexp module
;; -------------
;; string scanner:
;;

(DEFUN MAKE-SC (STRING)
  (LET ((SC (MAKE-ARRAY '(3))))
    (SETF (AREF SC 0) STRING)
    (SETF (AREF SC 1) 0)
    (SETF (AREF SC 2) (LENGTH STRING))
    SC)
  ) ;;MAKE-SC


(DEFUN SC-STRING (SC)
  "
RETURN:  The string being scanned.
"
  (AREF SC 0)
  ) ;;SC-STRING


(DEFUN SC-POSITION (SC)
  "
RETURN:  The current position.
"
  (AREF SC 1)
  ) ;;SC-POSITION


(DEFUN SC-CURR-CHAR (SC)
  "
RETURN:  The current character, or nil if EOS.
"
  (IF (< (AREF SC 1) (AREF SC 2))
      (CHAR (AREF SC 0) (AREF SC 1))
      NIL)
  ) ;;SC-CURR-CHAR


(DEFUN SC-NEXT-CHAR (SC)
  "
RETURN:  The next character, or nil if EOS.
"
  (IF (< (1+ (AREF SC 1)) (AREF SC 2))
      (CHAR (AREF SC 0) (1+ (AREF SC 1)))
      NIL)
  ) ;;SC-NEXT-CHAR


(DEFUN SC-ADVANCE (SC)
  "
PRE:     (= p      (sc-position sc))
POST:    (= (1+ p) (sc-position sc))
RETURN:  The character at position 1+p.
"
  (IF (< (AREF SC 1) (AREF SC 2))
      (SETF (AREF SC 1) (1+ (AREF SC 1))))
  (SC-CURR-CHAR SC)
  ) ;;SC-ADVANCE


(DEFUN SC-SCAN-TO-CHAR (SC CHAR)
  "
RETURN:  the substring of (sc-string sc) starting from current position
         to the position just before the first character equal to `char'
         found from this position.

PRE:     (= p      (sc-position sc))
POST:    (and (<=  p (sc-position sc))
              (or (and (< (sc-position sc) (length (sc-string sc)))
                       (char= char (sc-curr-char sc)))
                  (= (sc-position sc) (length (sc-string sc))))
              (forall i between p and (1- (sc-position sc))
                  (char/= char (char (sc-string sc) i))))
"
  (LET ((S (AREF SC 0))
        (P (AREF SC 1))
        (L (AREF SC 2)))
    (LOOP WHILE (AND (< P L) (CHAR/= CHAR (CHAR S P)))
       DO (SETQ P (1+ P)))
    (PROG1 (SUBSEQ S (AREF SC 1) P)
      (SETF (AREF SC 1) P)))
  ) ;;SC-SCAN-TO-CHAR
          




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing regular expression strings
;; ----------------------------------
;; This produces a syntactical tree.
;;

(DEFUN PJB-RE-PARSE-SIMPLE (SC)
  "
DO:     Parses a regexp simple.
RETURN: A parse tree.

simple ::= '\\('   regexp '\\)' .    (:group     regexp)
simple ::= '\\(?:' regexp '\\)' .    (:shy-group regexp)
simple ::= '\\0' |'\\1' |'\\2' |'\\3' | '\\4'
          |'\\5' |'\\6' |'\\7' |'\\8' | '\\9' .
                                   (:reference number)
simple ::= regular-character .     regular-character
simple ::= '.' | '\\w' | '\\W' | '\\sC' | '\\SC' | '\\cC' | '\\CC' .
                                   :any-character
                                   :any-word-character
                                   :any-not-word-character
                                   (:any-syntax-class     class)
                                   (:any-not-syntax-class class)
                                   (:any-category         category)
                                   (:any-not-category     category)

simple ::= '\\=' | '\\b' | '\\B' | '\\<' | '\\>' .
                                   :empty-at-point    NEVER MATCH IN STRING!
                                   :empty-at-limit-of-word
                                   :empty-not-at-limit-of-word
                                   :empty-at-beginning-of-word
                                   :empty-at-end-of-word

simple ::= '^' | '\\`' .
                                   :empty-at-beginning-of-line
                                   :empty-at-beginning-of-string
simple ::= '$' | '\\'' .
                                   :empty-at-end-of-line
                                   :empty-at-end-of-string

simple ::= '\\$' | '\\^' | '\\.' | '\\*' | '\\+'
         | '\\?' | '\\[' | '\\]' | '\\\\' .
                                   regular-character

simple ::= '[' '^' character-set ']' .
                                   (:inverse-char-set char-or-char-interval )
simple ::= '['     character-set ']' .
                                   (:char-set         char-or-char-interval )
"
  (LET ((TREE NIL)
        CURR-CHAR TOKEN)
    (SETQ CURR-CHAR (SC-CURR-CHAR SC))
    (COND
      ((NULL CURR-CHAR)
       (SETQ TREE '(:ERROR "EOS")))

      ((SETQ TOKEN
             (CDR (ASSOC CURR-CHAR
                         (LIST
                          (CONS (CHARACTER '\.) :ANY-CHARACTER)
                          (CONS (CHARACTER '\^) :EMPTY-AT-BEGINNING-OF-LINE)
                          (CONS (CHARACTER '\$) :EMPTY-AT-END-OF-LINE)
                          )
                         :TEST (FUNCTION EQ))))
       (SC-ADVANCE SC)
       (SETQ TREE TOKEN))

      ((EQ (CHARACTER '\[) CURR-CHAR)
       ;; simple ::= '[' '^' character-set ']' .
       ;; (:inverse-char-set char-or-char-interval )
       ;; simple ::= '['     character-set ']' .
       ;; (:char-set         char-or-char-interval )
       ;;
       ;; charset ::= '[:alnum:]' | '[:cntrl:]' | '[:lower:]' | '[:space:]'
       ;;           | '[:alpha:]' | '[:digit:]' | '[:print:]' | '[:upper:]'
       ;;           | '[:blank:]' | '[:graph:]' | '[:punct:]' | '[:xdigit:]' .
       ;;
       ;; charset ::= '[' ['^'] [']'|'-']
       ;;              [ any-but-dorb
       ;;              | any-but-dorb '-' any-but-dorb [ '-' ] ]*  [ '-' ] ']' .
       ;;
       ;; any-but-dorb ::= <any character but dash or right bracket> .
       ;;
       ;; [x-]]  could be:     { x to ] }
       ;;        but that's:   { x , - } ]
       ;;
       ;; So, after the optional initial ']', we can search for the next ']'
       ;; and parse then. A missing closing ']' is an error.

       (ERROR "[charset] Not implemented yet.")
;;; (let ((set nil)
;;;             (min nil)
;;;             max)
;;;         (setq curr-char (sc-advance sc))
;;;         (if (char= (character '\^) curr-char)
;;;             (progn
;;;               (setq token :inverse-char-set)
;;;               (setq curr-char (sc-advance sc)))
;;;           (setq token :char-set ))

;;;         (if (char= (character '\]) curr-char)
;;;             (progn
;;;               (sc-advance sc)
;;;               (setq charsetstr (concatenate 'string
;;;                                  "]" (sc-scan-to-char sc (character '\]))))
;;;               )
;;;           (setq charsetstr (sc-scan-to-char sc (character '\]))))

;;; (string-match "[[:digit:][:punct:]]" "ABC123abc")

;;;         (setq charsetstr (sc-scan-to-char sc (character '\]))

;;;         (loop while (char/= curr-char (character '\]))
;;;               do
;;;               (if (char= (character '\-) curr-char)
;;;                   (if min
;;;                       (progn
;;;                         (setq curr-char (sc-advance sc))
;;;                         (if (char/= curr-char (character '\]))
;;;                             (progn
;;;                               (push min set)
;;;                               (push (character '\-) set))
;;;                           (push (cons min curr-char) set))
;;;                         (setq min nil))
;;;                     (push (character '\-) set))
;;;                 (setq min curr-char)
            

     
;;;                 ))))
       )


      ((EQ (CHARACTER '\\) CURR-CHAR)
       (UNLESS (OR (EQ (SC-NEXT-CHAR SC) (CHARACTER '\|))
                   (EQ (SC-NEXT-CHAR SC) (CHARACTER ")")))
         (SC-ADVANCE SC)
         (SETQ CURR-CHAR (SC-CURR-CHAR SC))
         (IF (SETQ TOKEN
                   (CDR
                    (ASSOC CURR-CHAR
                           (LIST
                            (CONS (CHARACTER '\w) :ANY-WORD-CHARACTER)
                            (CONS (CHARACTER '\W) :ANY-NOT-WORD-CHARACTER)
                            (CONS (CHARACTER '\=) :EMPTY-AT-POINT)
                            (CONS (CHARACTER '\b) :EMPTY-AT-LIMIT-OF-WORD)
                            (CONS (CHARACTER '\B) :EMPTY-NOT-AT-LIMIT-OF-WORD)
                            (CONS (CHARACTER '\<) :EMPTY-AT-BEGINNING-OF-WORD)
                            (CONS (CHARACTER '\>) :EMPTY-AT-END-OF-WORD)
                            (CONS (CHARACTER '\`) :EMPTY-AT-BEGINNING-OF-STRING)
                            (CONS (CHARACTER '\') :EMPTY-AT-END-OF-STRING)
                            (CONS (CHARACTER '\$) (CHARACTER '\$))
                            (CONS (CHARACTER '\^) (CHARACTER '\^))
                            (CONS (CHARACTER '\.) (CHARACTER '\.))
                            (CONS (CHARACTER '\*) (CHARACTER '\*))
                            (CONS (CHARACTER '\+) (CHARACTER '\+))
                            (CONS (CHARACTER '\?) (CHARACTER '\?))
                            (CONS (CHARACTER '\[) (CHARACTER '\[))
                            (CONS (CHARACTER '\]) (CHARACTER '\]))
                            (CONS (CHARACTER '\\) (CHARACTER '\\))
                            (CONS (CHARACTER '\0) '(:REFERENCE 0))
                            (CONS (CHARACTER '\1) '(:REFERENCE 1))
                            (CONS (CHARACTER '\2) '(:REFERENCE 2))
                            (CONS (CHARACTER '\3) '(:REFERENCE 3))
                            (CONS (CHARACTER '\4) '(:REFERENCE 4))
                            (CONS (CHARACTER '\5) '(:REFERENCE 5))
                            (CONS (CHARACTER '\6) '(:REFERENCE 6))
                            (CONS (CHARACTER '\7) '(:REFERENCE 7))
                            (CONS (CHARACTER '\8) '(:REFERENCE 8))
                            (CONS (CHARACTER '\9) '(:REFERENCE 9))
                            )
                           :TEST (FUNCTION EQ))))
             (PROGN
               (SETQ TREE TOKEN)
               (SC-ADVANCE SC))

             (COND
               ((EQ (CHARACTER "(") CURR-CHAR)
                ;; simple ::= '\('   regexp '\)' .    (:group     regexp)
                ;; simple ::= '\(?:' regexp '\)' .    (:shy-group regexp)
                (SC-ADVANCE SC)
                (IF (AND (EQ (CHARACTER '\?) (SC-CURR-CHAR SC))
                         (EQ (CHARACTER '\:) (SC-NEXT-CHAR SC)))
                    (PROGN
                      (SC-ADVANCE SC)
                      (SC-ADVANCE SC)
                      (SETQ TOKEN :SHY-GROUP)
                      )
                    (SETQ TOKEN :GROUP))
                (SETQ TREE (LIST TOKEN (PJB-RE-PARSE-REGEXP SC)))
                (IF (AND (EQ (CHARACTER '\\) (SC-CURR-CHAR SC))
                         (EQ (CHARACTER ")") (SC-NEXT-CHAR SC)))
                    (PROGN
                      (SC-ADVANCE SC)
                      (SC-ADVANCE SC))
                    (SETQ TREE
                          (LIST :ERROR
                                (FORMAT
                                    NIL
                                  "Invalid character at ~D '~A~A' expected '\\)'."
                                  (SC-POSITION SC)
                                  (SC-CURR-CHAR SC)
                                  (IF (SC-NEXT-CHAR SC)
                                      (SC-NEXT-CHAR SC)  ""))
                                TREE))) )

               ((SETQ TOKEN
                      (CDR (ASSOC CURR-CHAR
                                  (LIST
                                   (CONS (CHARACTER '\s) :ANY-SYNTAX-CLASS)
                                   (CONS (CHARACTER '\S) :ANY-NOT-SYNTAX-CLASS)
                                   (CONS (CHARACTER '\c) :ANY-CATEGORY)
                                   (CONS (CHARACTER '\C) :ANY-NOT-CATEGORY))
                                  :TEST (FUNCTION EQ))))
                (SC-ADVANCE SC)
                (SETQ CURR-CHAR (SC-CURR-CHAR SC))
                (IF CURR-CHAR
                    (PROGN
                      (SETQ TREE (LIST TOKEN CURR-CHAR))
                      (SC-ADVANCE SC))
                    (SETQ TREE '(:ERROR "EOS"))))
               ((EQ (CHARACTER '\|) (SC-NEXT-CHAR SC))

                )))))

      (T
       (SETQ TREE CURR-CHAR)
       (SC-ADVANCE SC)))

    TREE) ;;let
  )       ;;PJB-RE-PARSE-SIMPLE


(DEFUN PJB-RE-PARSE-ELEMENT (SC)
  "
DO:      Parses a regexp element.
RETURNS: A parse tree.

element ::= simple .               simple
element ::= simple '*' .           (:zero-or-more simple)
element ::= simple '+' .           (:one-or-more  simple)
element ::= simple '?' .           (:optional     simple)

element ::= simple '*?' .          (:non-greedy-zero-or-more simple)
element ::= simple '+?' .          (:non-greedy-one-or-more  simple)
element ::= simple '??' .          (:non-greedy-optional     simple)

element ::= simple '\{' number '\}' .
                                   (:repeat-exact   simple number)
element ::= simple '\{' number ',' [ number ] '\}' .
                                   (:repeat-between simple number [number])
"
  (LET (TREE SIMPLE CURR-CHAR)
    (SETQ SIMPLE (PJB-RE-PARSE-SIMPLE SC))
    (SETQ CURR-CHAR (SC-CURR-CHAR SC))
    (COND
      ((NULL CURR-CHAR)  (SETQ TREE SIMPLE))

      ((EQ (CHARACTER '\?) CURR-CHAR)
       (SC-ADVANCE SC)
       (IF (EQ (CHARACTER '\?) (SC-CURR-CHAR SC))
           (PROGN
             (SC-ADVANCE SC)
             (SETQ TREE (LIST :NON-GREEDY-OPTIONAL  SIMPLE)))
           (SETQ TREE (LIST :OPTIONAL SIMPLE))))

      ((EQ (CHARACTER '\*) CURR-CHAR)
       (SC-ADVANCE SC)
       (IF (EQ (CHARACTER '\?) (SC-CURR-CHAR SC))
           (PROGN
             (SC-ADVANCE SC)
             (SETQ TREE (LIST :NON-GREEDY-ZERO-OR-MORE  SIMPLE)))
           (SETQ TREE (LIST :ZERO-OR-MORE SIMPLE)))) 

      ((EQ (CHARACTER '\+) CURR-CHAR)
       (SC-ADVANCE SC)
       (IF (EQ (CHARACTER '\?) (SC-CURR-CHAR SC))
           (PROGN
             (SC-ADVANCE SC)
             (SETQ TREE (LIST :NON-GREEDY-ONE-OR-MORE  SIMPLE)))
           (SETQ TREE (LIST :ONE-OR-MORE SIMPLE))))

      ((AND (EQ (CHARACTER '\\) CURR-CHAR)
            (EQ (CHARACTER '\{) (SC-NEXT-CHAR SC)))
       (SC-ADVANCE SC)
       (SC-ADVANCE SC)
       (SETQ TREE '(:ERROR "\{...\} not implemented yet.")))

      (T                 (SETQ TREE SIMPLE)))
    TREE) ;;let
  )       ;;PJB-RE-PARSE-ELEMENT


(DEFUN PJB-RE-COLLAPSE-STRINGS (TREE)
  "
RETURNS: A new list where all sequences of characters are collapsed
         into strings. Signle characters are not collapsed.
NOTE:    Does not works recursively because recursive sequences are built
         bottom-up.
"
  (LOOP WITH RESULT = NIL
     WITH STRING = NIL
     FOR ITEM IN TREE
     DO
     (IF (CHARACTERP ITEM)
         (PUSH ITEM STRING)
         (PROGN
           (WHEN STRING
             (IF (= 1 (LENGTH STRING))
                 (PUSH (CAR STRING) RESULT)
                 (PUSH (implode-string (NREVERSE STRING)) RESULT))
             (SETQ STRING NIL))
           (PUSH ITEM RESULT)))
     FINALLY
     (WHEN STRING
       (IF (= 1 (LENGTH STRING))
           (PUSH (CAR STRING) RESULT)
           (PUSH (implode-string (NREVERSE STRING)) RESULT))
       (SETQ STRING NIL))
     (RETURN (NREVERSE RESULT)))
  ) ;;PJB-RE-COLLAPSE-STRINGS


(DEFUN PJB-RE-PARSE-SEQUENCE (SC)
  "
DO:      Parses a regexp sequence.
RETURNS: A parse tree.

sequence ::= element sequence  .  (:sequence element element ...)
sequence ::= element .             element
sequence ::= .                     nil
"
  (LET ((TREE NIL))
    (LOOP WHILE (AND (SC-CURR-CHAR SC)
                     (NOT (AND (EQ (CHARACTER '\\) (SC-CURR-CHAR SC))
                               (OR (EQ (SC-NEXT-CHAR SC) (CHARACTER '\|))
                                   (EQ (SC-NEXT-CHAR SC) (CHARACTER ")") )))))
       DO
       (PUSH (PJB-RE-PARSE-ELEMENT SC) TREE)
       ) ;;loop
    (CONS :SEQUENCE (PJB-RE-COLLAPSE-STRINGS (NREVERSE TREE)))
;;;     (if (<= (length tree) 1)
;;;         (car tree)
;;;       (progn
;;;         (setq tree (pjb-re-collapse-strings (nreverse tree)))
;;;         (if (<= (length tree) 1)
;;;             (car tree)
;;;           tree)))
    ) ;;let
  )   ;;PJB-RE-PARSE-SEQUENCE


(DEFUN PJB-RE-PARSE-REGEXP (SC)
  "
DO:      Parses a regexp.
RETURNS: A parse tree.
NOTE:    The result may contain the symbol :error followed by a string.

regexp ::= sequence '\|' regexp .   (:alternative sequence sequence...)
regexp ::= sequence .               sequence
"
  (LET (TREE)
    (SETQ TREE (LIST (PJB-RE-PARSE-SEQUENCE SC)))
    (LOOP WHILE (AND (EQ (CHARACTER '\\) (SC-CURR-CHAR SC))
                     (EQ (CHARACTER '\|) (SC-NEXT-CHAR SC)))
       DO
       (SC-ADVANCE SC)
       (SC-ADVANCE SC)
       (PUSH (PJB-RE-PARSE-SEQUENCE SC) TREE)
       ) ;;loop
    (IF (= 1 (LENGTH TREE))
        (CAR TREE)
        (CONS :ALTERNATIVE (NREVERSE TREE)))
    ) ;;let
  )   ;;PJB-RE-PARSE-REGEXP


(DEFUN PJB-RE-PARSE-WHOLE-REGEXP (SC)
  (LET ((TREE (PJB-RE-PARSE-REGEXP SC))
        (CURR-CHAR (SC-CURR-CHAR SC)))
    (IF CURR-CHAR
        (SETQ TREE
              (LIST :ERROR (FORMAT NIL "Syntax error at ~D (~A ~A)."
                                   (SC-POSITION SC)
                                   CURR-CHAR
                                   (IF (SC-NEXT-CHAR SC)
                                       (SC-NEXT-CHAR SC)  ""))
                    TREE)))
    TREE)
  ) ;;PJB-RE-PARSE-WHOLE-REGEXP



;; $^.*+?[]\

;; regexp ::= sequence '\|' regexp .   (:alternative sequence sequence...)
;; regexp ::= sequence .               sequence


;; sequence ::= element sequence  .  (:sequence element element ...)
;; sequence ::= element .             element
;; sequence ::= .                     nil

;;                                An element can be a string, corresponding to
;;                                a concatenated sequence of regular-character.

;; element ::= simple .               simple
;; element ::= simple '*' .           (:zero-or-more simple)
;; element ::= simple '+' .           (:one-or-more  simple)
;; element ::= simple '?' .           (:optional     simple)

;; element ::= simple '*?' .          (:non-greedy-zero-or-more simple)
;; element ::= simple '+?' .          (:non-greedy-one-or-more  simple)
;; element ::= simple '??' .          (:non-greedy-optional     simple)

;; element ::= simple '\{' number '\}' .
;;                                    (:repeat-exact   simple number)
;; element ::= simple '\{' number ',' [ number ] '\}' .
;;                                    (:repeat-between simple number [number])

;; simple ::= '\('   regexp '\)' .    (:group     regexp)
;; simple ::= '\(?:' regexp '\)' .    (:shy-group regexp)
;; simple ::= '\0' |'\1' |'\2' |'\3' |'\4' |'\5' |'\6' |'\7' |'\8' | '\9' .
;;                                    (:reference number)
;; simple ::= regular-character .     regular-character
;; simple ::= '.' | '\w' | '\W' | '\sC' | '\SC' | '\cC' | '\CC' .
;;                                    :any-character
;;                                    :any-word-character
;;                                    :any-not-word-character
;;                                    (:any-syntax-class     class)
;;                                    (:any-not-syntax-class class)
;;                                    (:any-category         category)
;;                                    (:any-not-category     category)

;; simple ::= '\=' | '\b' | '\B' | '\<' | '\>' .
;;                                    :empty-at-point   # NEVER MATCH IN STRING!
;;                                    :empty-at-limit-of-word
;;                                    :empty-not-at-limit-of-word
;;                                    :empty-at-beginning-of-word
;;                                    :empty-at-end-of-word

;; simple ::= '^' | '\`' .
;;                                    :empty-at-beginning-of-line
;;                                    :empty-at-beginning-of-string
;; simple ::= '$' | '\'' .
;;                                    :empty-at-end-of-line
;;                                    :empty-at-end-of-string

;; simple ::= '\$' | '\^' | '\.' | '\*' | '\+' | '\?' | '\[' | '\]' | '\\' .
;;                                    regular-character

;; simple ::= '[' '^' character-set ']' .
;;                                    (:inverse-char-set char-or-char-interval )
;; simple ::= '['     character-set ']' .
;;                                    (:char-set         char-or-char-interval )

;; char-or-char-interval is a sequence of regular-character
;;                                        or (cons min-char max-char).

;; character-set ::= initial-c-set rest-c-set .
;; initial-c-set ::= ']' | '-' .
;; initial-c-set ::= .
;; rest-c-set ::= .
;; rest-c-set ::= c-set-char rest-c-set .
;; rest-c-set ::= c-set-char '-' c-set-char rest-c-set .
;; rest-c-set ::= c-set-char '-' c-set-char '-' rest-c-set .
;; c-set-char ::= any-but-right-bracket-or-dash .





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matching a regexp tree to a string
;; ----------------------------------
;; 



(defvar PJB-RE-NEW-LINE (CODE-CHAR 10) "A new-line.")


(DEFMACRO PJB-RE-SLOT-NODE        (OBJ)        `(AREF ,OBJ 0))
(DEFMACRO PJB-RE-SLOT-MATCH       (OBJ)        `(AREF ,OBJ 1))
(DEFMACRO PJB-RE-SLOT-STRING      (OBJ)        `(AREF ,OBJ 2))
(DEFMACRO PJB-RE-SLOT-BEGIN       (OBJ)        `(AREF ,OBJ 3))
(DEFMACRO PJB-RE-SLOT-END         (OBJ)        `(AREF ,OBJ 4))
(DEFMACRO PJB-RE-SLOT-TRY         (OBJ)        `(AREF ,OBJ 5))
(DEFMACRO PJB-RE-SLOT-PRIVATE     (OBJ)        `(AREF ,OBJ 6))
(DEFMACRO PJB-RE-SLOT-CHILDREN    (OBJ)        `(AREF ,OBJ 7))

;;; (DEFMACRO PJB-RE-SLOT-BEGIN-SET   (OBJ VALUE) `(SETF (AREF ,OBJ 3) ,VALUE))
;;; (DEFMACRO PJB-RE-SLOT-END-SET     (OBJ VALUE) `(SETF (AREF ,OBJ 4) ,VALUE))
;;; (DEFMACRO PJB-RE-SLOT-TRY-SET     (OBJ VALUE) `(SETF (AREF ,OBJ 5) ,VALUE))
;;; (DEFMACRO PJB-RE-SLOT-PRIVATE-SET (OBJ VALUE) `(SETF (AREF ,OBJ 6) ,VALUE))
;;; (DEFSETF  PJB-RE-SLOT-BEGIN       PJB-RE-SLOT-BEGIN-SET)
;;; (DEFSETF  PJB-RE-SLOT-END         PJB-RE-SLOT-END-SET)
;;; (DEFSETF  PJB-RE-SLOT-TRY         PJB-RE-SLOT-TRY-SET)
;;; (DEFSETF  PJB-RE-SLOT-PRIVATE     PJB-RE-SLOT-PRIVATE-SET)

(DECLAIM (TYPE (FUNCTION (ARRAY) (FUNCTION (ARRAY) T)) PJB-RE-SLOT-MATCH))

(DEFMACRO PJB-RE-INIT (NODE POSITION)
  `(LET ((NODE ,NODE)
         (POSITION ,POSITION))
     (SETF (PJB-RE-SLOT-BEGIN NODE) POSITION)
     (SETF (PJB-RE-SLOT-TRY NODE) NIL)
     (SETF (PJB-RE-SLOT-END NODE) NIL)
     (values))) ;;PJB-RE-INIT
  

(DEFMACRO PJB-RE-MATCH (NODE)
  `(LET ((NODE ,NODE))
     (FUNCALL (PJB-RE-SLOT-MATCH NODE) NODE))
  ) ;;PJB-RE-MATCH





(DEFUN PJB-RE-CHARACTER-MATCH (NODE)
  "Matches a character.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P  (PJB-RE-SLOT-BEGIN NODE)) )
    (IF (PJB-RE-SLOT-TRY   NODE)
        ;; already tested. no more match:
        NIL
        ;; first test, let's see:
        (PROGN
          (SETF (PJB-RE-SLOT-TRY NODE) T)
          (IF (CHAR= (PJB-RE-SLOT-NODE NODE) (CHAR (PJB-RE-SLOT-STRING NODE) P))
              (PROGN
                (SETQ P (1+ P))
                (SETF (PJB-RE-SLOT-END NODE) P)
                P)
              NIL))))
  ) ;;PJB-RE-CHARACTER-MATCH



(DEFUN PJB-RE-STRING-MATCH (NODE)
  "Matches a string.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P  (PJB-RE-SLOT-BEGIN NODE)) )
    (IF (PJB-RE-SLOT-TRY   NODE)
        ;; already tested. no more match:
        NIL
        ;; first test, let's see:
        (LET* ((M   (PJB-RE-SLOT-NODE NODE))
               (LEN (LENGTH M))
               (E   (+ P LEN))
               (S   (PJB-RE-SLOT-STRING NODE)) )
          (SETF (PJB-RE-SLOT-TRY NODE) T)
          (UNLESS (AND (< E (LENGTH S))
                       (STRING= M S :START2 P :END2 E))
            (SETQ E NIL))
          (SETF (PJB-RE-SLOT-END NODE) E)
          E)))
  ) ;;PJB-RE-STRING-MATCH



(DEFUN PJB-RE-NULL-MATCH (NODE)
  "Matches a null.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (IF (PJB-RE-SLOT-TRY   NODE)
      ;; already tested. no more match:
      NIL
      ;; first test, let's see:
      (PROGN
        (SETF (PJB-RE-SLOT-TRY NODE) T)
        T ;; yes! we match.
        ))
  ) ;;PJB-RE-NULL-MATCH


(DEFUN PJB-RE-ALTERNATIVE-MATCH (NODE)
  "Matches a alternative.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE))
        (FOUND NIL) )
    (WHEN (NULL N) (SETQ N 0))
    (LOOP WHILE (AND (< N (LENGTH CHILDREN))
                     (NOT FOUND))
       DO
       (PJB-RE-INIT (AREF CHILDREN N) P)
       (SETQ FOUND (PJB-RE-MATCH (AREF CHILDREN N)))
       FINALLY
       (SETF (PJB-RE-SLOT-END NODE) FOUND)
       (SETF (PJB-RE-SLOT-TRY NODE) (1+ N))
       FINALLY (RETURN FOUND)) ;;loop
    )                          ;;let
  )                            ;;PJB-RE-ALTERNATIVE-MATCH


(DEFUN PJB-RE-ANY-CATEGORY-MATCH (NODE)
  "Matches a any-category.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-ANY-CATEGORY-MATCH


(DEFUN PJB-RE-ANY-CHARACTER-MATCH (NODE)
  "Matches a any-character.  That is, anything but a NEW-LINE!
RETURNS: nil when no match,
         or the next unmatched position when there's a match.

A period ( '.' ), when used outside a bracket expression, is a BRE
that shall match any character in the supported character set except
NUL.

"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE)) )
    (IF (PJB-RE-SLOT-TRY   NODE)
        ;; already tested. no more match:
        NIL
        (PROGN ;; first test, let's see:
          (SETF (PJB-RE-SLOT-TRY NODE) T)
          (IF (< P (LENGTH (PJB-RE-SLOT-STRING NODE)))
              (PROGN
                (SETQ P (1+ P))
                (SETF (PJB-RE-SLOT-END NODE) P))
              (SETF  (PJB-RE-SLOT-END NODE) NIL)))))
  ) ;;PJB-RE-ANY-CHARACTER-MATCH


(DEFUN PJB-RE-ANY-NOT-CATEGORY-MATCH (NODE)
  "Matches a any-not-category.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-ANY-NOT-CATEGORY-MATCH


(DEFUN PJB-RE-ANY-NOT-SYNTAX-CLASS-MATCH (NODE)
  "Matches a any-not-syntax-class.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-ANY-NOT-SYNTAX-CLASS-MATCH


(DEFUN PJB-RE-ANY-NOT-WORD-CHARACTER-MATCH (NODE)
  "Matches a any-not-word-character.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-ANY-NOT-WORD-CHARACTER-MATCH


(DEFUN PJB-RE-ANY-SYNTAX-CLASS-MATCH (NODE)
  "Matches a any-syntax-class.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-ANY-SYNTAX-CLASS-MATCH


(DEFUN PJB-RE-ANY-WORD-CHARACTER-MATCH (NODE)
  "Matches a any-word-character.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-ANY-WORD-CHARACTER-MATCH


(DEFUN PJB-RE-CHAR-SET-MATCH (NODE)
  "Matches a char-set.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))    
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-CHAR-SET-MATCH


(DEFUN PJB-RE-EMPTY-AT-BEGINNING-OF-LINE-MATCH (NODE)
  "Matches a empty-at-beginning-of-line.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-EMPTY-AT-BEGINNING-OF-LINE-MATCH


(DEFUN PJB-RE-EMPTY-AT-BEGINNING-OF-STRING-MATCH (NODE)
  "Matches a empty-at-beginning-of-string.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P  (PJB-RE-SLOT-BEGIN  NODE)) )
    (IF (PJB-RE-SLOT-TRY  NODE)
        ;; already tested. no more match:
        NIL
        ;; first test, let's see:
        (PROGN
          (SETF (PJB-RE-SLOT-TRY NODE) T)
          (IF (=  0 P) ;; TODO use a :start / :end for the string!
              (PROGN
                (SETF (PJB-RE-SLOT-END NODE) P)
                P)
              NIL))))
  ) ;;PJB-RE-EMPTY-AT-BEGINNING-OF-STRING-MATCH


(DEFUN PJB-RE-EMPTY-AT-BEGINNING-OF-WORD-MATCH (NODE)
  "Matches a empty-at-beginning-of-word.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-EMPTY-AT-BEGINNING-OF-WORD-MATCH


(DEFUN PJB-RE-EMPTY-AT-END-OF-LINE-MATCH (NODE)
  "Matches a empty-at-end-of-line.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-EMPTY-AT-END-OF-LINE-MATCH


(DEFUN PJB-RE-EMPTY-AT-END-OF-STRING-MATCH (NODE)
  "Matches a empty-at-end-of-string.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P  (PJB-RE-SLOT-BEGIN  NODE)) )
    (IF (PJB-RE-SLOT-TRY  NODE)
        ;; already tested. no more match:
        NIL
        ;; first test, let's see:
        (PROGN
          (SETF (PJB-RE-SLOT-TRY NODE) T)
          (IF (=  (LENGTH (PJB-RE-SLOT-STRING NODE)) P) ;; TODO use a :start / :end for the string!
              (PROGN
                (SETF (PJB-RE-SLOT-END NODE) P)
                P)
              NIL))))
  ) ;;PJB-RE-EMPTY-AT-END-OF-STRING-MATCH


(DEFUN PJB-RE-EMPTY-AT-END-OF-WORD-MATCH (NODE)
  "Matches a empty-at-end-of-word.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-EMPTY-AT-END-OF-WORD-MATCH


(DEFUN PJB-RE-EMPTY-AT-LIMIT-OF-WORD-MATCH (NODE)
  "Matches a empty-at-limit-of-word.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-EMPTY-AT-LIMIT-OF-WORD-MATCH


(DEFUN PJB-RE-EMPTY-AT-POINT-MATCH (NODE)
  "Matches a empty-at-point.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-EMPTY-AT-POINT-MATCH


(DEFUN PJB-RE-EMPTY-NOT-AT-LIMIT-OF-WORD-MATCH (NODE)
  "Matches a empty-not-at-limit-of-word.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-EMPTY-NOT-AT-LIMIT-OF-WORD-MATCH


(DEFUN PJB-RE-ERROR-MATCH (NODE)
  "Matches a error.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-ERROR-MATCH


(DEFUN PJB-RE-GROUP-MATCH (NODE)
  "Matches a group.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (CHILD     (AREF (PJB-RE-SLOT-CHILDREN NODE) 0)) )
    (IF (PJB-RE-SLOT-TRY   NODE)
        ;; already tested. no more match:
        NIL
        ;; first test, let's see:
        (PROGN
          (SETF (PJB-RE-SLOT-TRY NODE) T)
          (PJB-RE-INIT CHILD P)
          (SETQ P (PJB-RE-MATCH CHILD))
          (WHEN P
            (SETF (PJB-RE-SLOT-END NODE) P))
          P)))
  ) ;;PJB-RE-GROUP-MATCH


(DEFUN PJB-RE-INVERSE-CHAR-SET-MATCH (NODE)
  "Matches a inverse-char-set.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-INVERSE-CHAR-SET-MATCH


(DEFUN PJB-RE-NON-GREEDY-ONE-OR-MORE-MATCH (NODE)
  "Matches a non-greedy-one-or-more.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((N      (PJB-RE-SLOT-TRY   NODE))
        (P      (PJB-RE-SLOT-BEGIN NODE))
        (CHILD  (AREF (PJB-RE-SLOT-CHILDREN  NODE) 0)) )
    (COND
      ((NULL N) ;; first time
       (PJB-RE-INIT CHILD P)
       (SETQ P (PJB-RE-MATCH CHILD))
       (SETF (PJB-RE-SLOT-END NODE) P)
       (SETF (PJB-RE-SLOT-TRY NODE) (IF P :MORE :OVER))
       P)
      ((EQ :MORE N)
       (SETQ P (PJB-RE-SLOT-END NODE))
       (PJB-RE-INIT CHILD P)
       (SETQ P (PJB-RE-MATCH CHILD))
       (SETF (PJB-RE-SLOT-END NODE) P)
       (SETF (PJB-RE-SLOT-TRY NODE) (IF P :MORE :OVER))
       P)
      (T
       NIL)))
  ) ;;PJB-RE-NON-GREEDY-ONE-OR-MORE-MATCH


(DEFUN PJB-RE-NON-GREEDY-OPTIONAL-MATCH (NODE)
  "Matches a non-greedy-optional.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILD     (AREF (PJB-RE-SLOT-CHILDREN NODE) 0)) )
    (COND
      ((NULL N) ;; first time, let's be non greedy: match nothing.
       (SETF (PJB-RE-SLOT-END NODE) P)
       (SETF (PJB-RE-SLOT-TRY NODE) :SECOND) )
      ((EQ N :SECOND) ;; second time, we expect the child.
       (PJB-RE-INIT CHILD P)
       (SETQ P (PJB-RE-MATCH CHILD))
       (SETF (PJB-RE-SLOT-END NODE) P)
       (SETF (PJB-RE-SLOT-TRY NODE) :LAST) )
      (T ;; too late we don't match anything.
       (SETQ P NIL)
       (SETF (PJB-RE-SLOT-END NODE) P) ))
    P)
  ) ;;PJB-RE-NON-GREEDY-OPTIONAL-MATCH


(DEFUN PJB-RE-NON-GREEDY-ZERO-OR-MORE-MATCH (NODE)
  "Matches a non-greedy-zero-or-more.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((N      (PJB-RE-SLOT-TRY  NODE))
        (S      (PJB-RE-SLOT-STRING  NODE))
        (CHILD  (AREF (PJB-RE-SLOT-CHILDREN  NODE) 0)) )
    (COND
      ((NULL N) ;; case zero
       (SETQ N (PJB-RE-SLOT-BEGIN NODE))
       (SETF (PJB-RE-SLOT-END NODE) N)
       (SETF (PJB-RE-SLOT-TRY NODE) N)
       )
      ((EQ T N) ;; no more match
       )
      ((= N (LENGTH S))
       ;; match end of string with any number, but no more.
       (SETF (PJB-RE-SLOT-END NODE) N)
       (SETF (PJB-RE-SLOT-TRY NODE) T))
      (T
       (PJB-RE-INIT CHILD N)
       (SETQ N (PJB-RE-MATCH CHILD))
       (IF N
           (PROGN
             (SETF (PJB-RE-SLOT-END NODE) N)
             (SETF (PJB-RE-SLOT-TRY NODE) N))
           (PROGN
             (SETF (PJB-RE-SLOT-END NODE) NIL)
             (SETF (PJB-RE-SLOT-TRY NODE) T)))
       )) ;;cond
    N)    ;;let
  )       ;;PJB-RE-NON-GREEDY-ZERO-OR-MORE-MATCH


(DEFUN PJB-RE-OPTIONAL-MATCH (NODE)
  "Matches a optional.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILD     (AREF (PJB-RE-SLOT-CHILDREN NODE) 0)) )
    (COND
      ((NULL N) ;; first time, we expect the child.
       (PJB-RE-INIT CHILD P)
       (SETQ P (PJB-RE-MATCH CHILD))
       (SETF (PJB-RE-SLOT-END NODE) P)
       (SETF (PJB-RE-SLOT-TRY NODE) :SECOND) )
      ((EQ N :SECOND) ;; second time,  let's be non greedy: match nothing.
       (SETF (PJB-RE-SLOT-END NODE) P)
       (SETF (PJB-RE-SLOT-TRY NODE) :LAST) )
      (T ;; too late we don't match anything.
       (SETQ P NIL)
       (SETF (PJB-RE-SLOT-END NODE) P) ))
    P)
  ) ;;PJB-RE-OPTIONAL-MATCH


(DEFUN PJB-RE-ONE-OR-MORE-MATCH (NODE)
  "Matches a one-or-more.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-ONE-OR-MORE-MATCH


(DEFUN PJB-RE-REFERENCE-MATCH (NODE)
  "Matches a reference.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-REFERENCE-MATCH


(DEFUN PJB-RE-REPEAT-BETWEEN-MATCH (NODE)
  "Matches a repeat-between.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-REPEAT-BETWEEN-MATCH


(DEFUN PJB-RE-REPEAT-EXACT-MATCH (NODE)
  "Matches a repeat-exact.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-REPEAT-EXACT-MATCH


(DEFUN PJB-RE-SEQUENCE-MATCH (NODE)
  "Matches a sequence.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (WHEN CHILDREN
      (UNLESS N
        (SETQ N 0)
        (PJB-RE-INIT (AREF CHILDREN N) P))
      (SETQ P (PJB-RE-MATCH (AREF CHILDREN N)))
      (LOOP WHILE (OR (AND P (< (1+ N) (LENGTH CHILDREN)))
                      (AND (NOT P) (<= 0 (1- N))))
         DO
         (IF P
             (PROGN
               (SETQ N (1+ N))
               (PJB-RE-INIT (AREF CHILDREN N) P))
             (SETQ N (1- N)))
         (SETQ P (PJB-RE-MATCH (AREF CHILDREN N)))
         ) ;;loop
      ;; p       ==> (= (1+ n) (length children)) ==> 0 <= n < (length children)
      ;; (not p) ==>    (= -1 (1- n)) ==>  n=0    ==> 0 <= n < (length children)
      (SETF (PJB-RE-SLOT-TRY NODE) N)
      (SETF (PJB-RE-SLOT-END NODE) P)
      ) ;;when
    P)  ;;let
  )     ;;PJB-RE-SEQUENCE-MATCH


(DEFUN PJB-RE-SHY-GROUP-MATCH (NODE)
  "Matches a shy-group.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((P         (PJB-RE-SLOT-BEGIN NODE))
        (N         (PJB-RE-SLOT-TRY   NODE))
        (CHILDREN  (PJB-RE-SLOT-CHILDREN NODE)) )
    (declare (ignore p n children))
    (ERROR "Not Implemented Yet: ~S~%" (PJB-RE-SLOT-NODE NODE))
    )
  ) ;;PJB-RE-SHY-GROUP-MATCH


(DEFUN PJB-RE-ZERO-OR-MORE-MATCH (NODE)
  "Matches a zero-or-more.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (LET ((N      (PJB-RE-SLOT-TRY     NODE))
        (S      (PJB-RE-SLOT-STRING  NODE))
        (P      (PJB-RE-SLOT-BEGIN   NODE))
        (CHILD  (AREF (PJB-RE-SLOT-CHILDREN  NODE) 0)) )
    ;; Note: we should try to save all the previous matches (from zero to n)
    ;;       to backtrack faster, but we would need to save possibly a lot
    ;;       of recursive state for all the child subtree...
    (COND
      ((NULL N) ;; first time: match all we can.
       (SETQ N (LOOP WITH P = (PJB-RE-SLOT-BEGIN   NODE)
                  FOR N = 0 THEN (1+ N)
                  WHILE (AND P (< P (LENGTH S)))
                  DO
                  (PJB-RE-INIT CHILD P)
                  (SETQ P (PJB-RE-MATCH CHILD))
                  FINALLY (RETURN N)))
       ;; oops we did one too many.
       ;; let's redo it till the limit
       (SETF (PJB-RE-SLOT-TRY NODE) N)
       (PJB-RE-ZERO-OR-MORE-MATCH NODE))
      ((< N 0) ;; we tried everything.
       (SETF (PJB-RE-SLOT-END NODE) NIL))
      (T ;; match n-1 times.
       (LOOP FOR I FROM 1 BELOW N
          DO
          (PJB-RE-INIT CHILD P)
          (SETQ P (PJB-RE-MATCH CHILD)))
       (SETF (PJB-RE-SLOT-END NODE) P)
       (SETF (PJB-RE-SLOT-TRY NODE) (1- N))
       P)
      )) ;;let
  )      ;;PJB-RE-ZERO-OR-MORE-MATCH



(DEFUN PJB-RE-MAKE-PJB-RE-SYMBOL (KEY EXT)
  "
RETURN:     A symbol corresponding to one of the pjb-re-*-{init,match} 
            functions defined here.
ext:        A string, either \"init\" or \"match\".
key:        A keyword, one of those used in the regexp syntactic trees.

NOTE:
                                       emacs      Common-Lisp
            ----------------------  ------------  ------------
            (symbol-name 'key)        ''key''        ''KEY''
            (symbol-name :key)        '':key''       ''KEY''
            (eq 'key 'KEY)              nil             T
URL:        http://www.informatimago.com/local/lisp/HyperSpec/Body/02_cd.htm
            http://www.informatimago.com/local/lisp/HyperSpec/Body/f_intern.htm#intern
"
  (IF (STRING= "emacs" (LISP-IMPLEMENTATION-TYPE))
      (INTERN (STRING-DOWNCASE (FORMAT NIL "pjb-re-~s-~s"
                                       (SUBSEQ (SYMBOL-NAME KEY) 1) EXT)))
      (INTERN (STRING-UPCASE (FORMAT NIL "pjb-re-~a-~a" (SYMBOL-NAME KEY) EXT))
              (FIND-PACKAGE "PJB-REGEXP")))
  ) ;;PJB-RE-MAKE-PJB-RE-SYMBOL


(DEFUN PJB-RE-DECORATE-TREE (TREE STRING)
  "
RETURN:  A decorated tree that can be used for the matching the string.
"
  (TREE-DECORATE
   TREE
   (LAMBDA (NODE CHILDREN)
     (LET ((OBJ (MAKE-ARRAY '(9)))
           KEY)
       (COND
         ((NULL       NODE) (SETQ KEY :NULL))
         ((CHARACTERP NODE) (SETQ KEY :CHARACTER))
         ((STRINGP    NODE) (SETQ KEY :STRING))
         ((LISTP      NODE) (SETQ KEY :LIST))
         ((MEMBER NODE '(
                         :ALTERNATIVE :ANY-CATEGORY :ANY-CHARACTER
                         :ANY-NOT-CATEGORY :ANY-NOT-SYNTAX-CLASS
                         :ANY-NOT-WORD-CHARACTER :ANY-SYNTAX-CLASS
                         :ANY-WORD-CHARACTER :CHAR-SET
                         :EMPTY-AT-BEGINNING-OF-LINE
                         :EMPTY-AT-BEGINNING-OF-STRING
                         :EMPTY-AT-BEGINNING-OF-WORD
                         :EMPTY-AT-END-OF-LINE :EMPTY-AT-END-OF-STRING
                         :EMPTY-AT-END-OF-WORD :EMPTY-AT-LIMIT-OF-WORD
                         :EMPTY-AT-POINT :EMPTY-NOT-AT-LIMIT-OF-WORD
                         :ERROR :GROUP :INVERSE-CHAR-SET
                         :NON-GREEDY-ONE-OR-MORE :NON-GREEDY-OPTIONAL
                         :NON-GREEDY-ZERO-OR-MORE :OPTIONAL :ONE-OR-MORE
                         :REFERENCE :REPEAT-BETWEEN :REPEAT-EXACT
                         :SEQUENCE :SHY-GROUP :ZERO-OR-MORE)
                  :TEST (FUNCTION EQ))
          (SETQ KEY NODE))
         (T (ERROR "INTERNAL: Unexpected node in match tree: ~S !"
                   NODE)))
       (SETF (AREF OBJ 0) NODE)
       (SETF (AREF OBJ 1) (PJB-RE-MAKE-PJB-RE-SYMBOL KEY "match"))
       (SETF (AREF OBJ 2) STRING)
       (SETF (AREF OBJ 3) 0) ;; beg (start)
       (SETF (AREF OBJ 4) 0) ;; end
       (SETF (AREF OBJ 5) NIL) ;; try
       (SETF (AREF OBJ 6) NIL) ;; private
       (SETF (AREF OBJ 7) (WHEN CHILDREN
                            (MAKE-ARRAY (LIST (LENGTH CHILDREN))
                                        :INITIAL-CONTENTS CHILDREN)))
       OBJ)))
  ) ;;PJB-RE-DECORATE-TREE


(DEFUN PJB-RE-COLLECT-GROUPS (DEC-TREE &OPTIONAL GROUPS)
  (LET ((MAKE-GROUPS-FLAG (NOT GROUPS)))
    (UNLESS GROUPS
      (SETQ GROUPS (CONS :GROUPS NIL)))
    (IF (EQ :GROUP (PJB-RE-SLOT-NODE DEC-TREE))
        (PUSH DEC-TREE (CDR GROUPS)))
    (LOOP WITH CHILDREN = (PJB-RE-SLOT-CHILDREN DEC-TREE)
       FOR I FROM 0 BELOW (LENGTH CHILDREN)
       FOR CHILD = (AREF CHILDREN I)
       DO (PJB-RE-COLLECT-GROUPS CHILD GROUPS)
       ) ;;loop
    
    (IF MAKE-GROUPS-FLAG
        (NREVERSE (CDR GROUPS))
        NIL)
    )) ;;PJB-RE-COLLECT-GROUPS



(DEFSTRUCT MATCH
  "This structure stores a (start,end) couple specifying the range matched
by a group (or the whole regexp)."
  (START NIL :TYPE (OR NULL INTEGER))
  (END   NIL :TYPE (OR NULL INTEGER))
  ) ;;MATCH


(DEFUN MATCH-STRING (STRING MATCH)
  "Extracts the substring of STRING corresponding to a given pair of
start and end indices. The result is shared with STRING.
If you want a freshly consed string, use copy-string
or (coerce (match-string ...) 'simple-string)."
  (SUBSEQ STRING (MATCH-START MATCH) (MATCH-END MATCH))
  ) ;;MATCH-STRING


(DEFUN REGEXP-QUOTE (STRING)
  (declare (ignore string))
  (ERROR "Not Implemented Yet: REGEXP-QUOTE~%" )
  ) ;;REGEXP-QUOTE
 

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
           (MAKE-SC (CONCATENATE 'STRING "\\`.*?\\(" REGEXP "\\).*?\\'"))))
         (DEC-TREE (PJB-RE-DECORATE-TREE SYN-TREE STRING))
         (GROUPS  (PJB-RE-COLLECT-GROUPS DEC-TREE)) )
    (PJB-RE-INIT DEC-TREE START)
    (PJB-RE-MATCH DEC-TREE) 
    ;; there's nowhere to backtrack at the top level...
    (VALUES-LIST (MAPCAR (LAMBDA (G)
                           (LET ((S (PJB-RE-SLOT-BEGIN G))
                                 (E (PJB-RE-SLOT-END G)) )
                             (IF (AND S E)
                                 (MAKE-MATCH :START S :END E)
                                 (MAKE-MATCH :START NIL :END NIL))))
                         GROUPS)))) ;;MATCH

;;;; regexp-emacs.lisp                --                     --          ;;;;
