;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               regexp-emacs.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             UNIX
;;;;USER-INTERFACE:     UNIX
;;;;NOWEB:              t
;;;;DESCRIPTION
;;;;
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2002 - 2012
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-EMACS"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.TREE-TO-ASCII")
  (:export "REGEXP-MATCH" "REGEXP-QUOTE" "MATCH-STRING" "MATCH-END"
           "MATCH-START" "MATCH")
  (:documentation
   "
NOT COMPLETE YET.

This package implement REGEXP in COMMON-LISP, which is interesting
because then it's available on any COMMON-LISP platform whether the
external C regexp library is available or not, and moreover, it's the
same (that is, it's compatible) on all COMMON-LIST platforms.


Posix Regexp implemented in Common-Lisp.

See specifications at:
http://www.opengroup.org/onlinepubs/007904975/basedefs/xbd_chap09.html

This is a strict implementation that will work both in clisp
(Common-Lisp) and emacs (with cl and pjb-cl Common-Lisp extensions).

This implementation is entirely in lisp, contrarily to what regexp
packages are available under clisp or emacs.  Thus it as the advantage
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-EMACS")




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



(defun pjb-re-split-string (string &optional separators)
  "
DO:         Splits STRING into substrings where there are matches
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
        (strlen   (length string)))
    (loop :while (< position strlen)
          :do (loop :while (and (< nextpos strlen)
                                (char/= sep (aref string nextpos)))
                    :do (setq nextpos (1+ nextpos)))
              (push (subseq string position nextpos) chunks)
              (setq position (1+ nextpos))
              (setq nextpos  position))
    (nreverse chunks)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regexp module
;; -------------
;; string scanner:
;;

(defun make-sc (string)
  (let ((sc (make-array '(3))))
    (setf (aref sc 0) string)
    (setf (aref sc 1) 0)
    (setf (aref sc 2) (length string))
    sc))


(defun sc-string (sc)
  "
RETURN:  The string being scanned.
"
  (aref sc 0))


(defun sc-position (sc)
  "
RETURN:  The current position.
"
  (aref sc 1))


(defun sc-curr-char (sc)
  "
RETURN:  The current character, or nil if EOS.
"
  (if (< (aref sc 1) (aref sc 2))
      (char (aref sc 0) (aref sc 1))
      nil))


(defun sc-next-char (sc)
  "
RETURN:  The next character, or nil if EOS.
"
  (if (< (1+ (aref sc 1)) (aref sc 2))
      (char (aref sc 0) (1+ (aref sc 1)))
      nil))


(defun sc-advance (sc)
  "
PRE:     (= p      (sc-position sc))
POST:    (= (1+ p) (sc-position sc))
RETURN:  The character at position 1+p.
"
  (if (< (aref sc 1) (aref sc 2))
      (setf (aref sc 1) (1+ (aref sc 1))))
  (sc-curr-char sc))


(defun sc-scan-to-char (sc char)
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
  (let ((s (aref sc 0))
        (p (aref sc 1))
        (l (aref sc 2)))
    (loop :while (and (< p l) (char/= char (char s p)))
          :do (setq p (1+ p)))
    (prog1 (subseq s (aref sc 1) p)
      (setf (aref sc 1) p))))
          




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing regular expression strings
;; ----------------------------------
;; This produces a syntactical tree.
;;

(defun pjb-re-parse-simple (sc)
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
  (let ((tree nil)
        curr-char token)
    (setq curr-char (sc-curr-char sc))
    (cond
      ((null curr-char)
       (setq tree '(:error "EOS")))

      ((setq token
             (cdr (assoc curr-char
                         (list
                          (cons (character '\.) :any-character)
                          (cons (character '\^) :empty-at-beginning-of-line)
                          (cons (character '\$) :empty-at-end-of-line)
                          )
                         :test (function eq))))
       (sc-advance sc)
       (setq tree token))

      ((eq (character '\[) curr-char)
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

       (error "[charset] Not implemented yet.")
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


      ((eq (character '\\) curr-char)
       (unless (or (eq (sc-next-char sc) (character '\|))
                   (eq (sc-next-char sc) (character ")")))
         (sc-advance sc)
         (setq curr-char (sc-curr-char sc))
         (if (setq token
                   (cdr
                    (assoc curr-char
                           (list
                            (cons (character '\w) :any-word-character)
                            (cons (character '\W) :any-not-word-character)
                            (cons (character '\=) :empty-at-point)
                            (cons (character '\b) :empty-at-limit-of-word)
                            (cons (character '\B) :empty-not-at-limit-of-word)
                            (cons (character '\<) :empty-at-beginning-of-word)
                            (cons (character '\>) :empty-at-end-of-word)
                            (cons (character '\`) :empty-at-beginning-of-string)
                            (cons (character '\') :empty-at-end-of-string)
                            (cons (character '\$) (character '\$))
                            (cons (character '\^) (character '\^))
                            (cons (character '\.) (character '\.))
                            (cons (character '\*) (character '\*))
                            (cons (character '\+) (character '\+))
                            (cons (character '\?) (character '\?))
                            (cons (character '\[) (character '\[))
                            (cons (character '\]) (character '\]))
                            (cons (character '\\) (character '\\))
                            (cons (character '\0) '(:reference 0))
                            (cons (character '\1) '(:reference 1))
                            (cons (character '\2) '(:reference 2))
                            (cons (character '\3) '(:reference 3))
                            (cons (character '\4) '(:reference 4))
                            (cons (character '\5) '(:reference 5))
                            (cons (character '\6) '(:reference 6))
                            (cons (character '\7) '(:reference 7))
                            (cons (character '\8) '(:reference 8))
                            (cons (character '\9) '(:reference 9))
                            )
                           :test (function eq))))
             (progn
               (setq tree token)
               (sc-advance sc))

             (cond
               ((eq (character "(") curr-char)
                ;; simple ::= '\('   regexp '\)' .    (:group     regexp)
                ;; simple ::= '\(?:' regexp '\)' .    (:shy-group regexp)
                (sc-advance sc)
                (if (and (eq (character '\?) (sc-curr-char sc))
                         (eq (character '\:) (sc-next-char sc)))
                    (progn
                      (sc-advance sc)
                      (sc-advance sc)
                      (setq token :shy-group)
                      )
                    (setq token :group))
                (setq tree (list token (pjb-re-parse-regexp sc)))
                (if (and (eq (character '\\) (sc-curr-char sc))
                         (eq (character ")") (sc-next-char sc)))
                    (progn
                      (sc-advance sc)
                      (sc-advance sc))
                    (setq tree
                          (list :error
                                (format
                                    nil
                                  "Invalid character at ~D '~A~A' expected '\\)'."
                                  (sc-position sc)
                                  (sc-curr-char sc)
                                  (if (sc-next-char sc)
                                      (sc-next-char sc)  ""))
                                tree))) )

               ((setq token
                      (cdr (assoc curr-char
                                  (list
                                   (cons (character '\s) :any-syntax-class)
                                   (cons (character '\S) :any-not-syntax-class)
                                   (cons (character '\c) :any-category)
                                   (cons (character '\C) :any-not-category))
                                  :test (function eq))))
                (sc-advance sc)
                (setq curr-char (sc-curr-char sc))
                (if curr-char
                    (progn
                      (setq tree (list token curr-char))
                      (sc-advance sc))
                    (setq tree '(:error "EOS"))))
               ((eq (character '\|) (sc-next-char sc))

                )))))

      (t
       (setq tree curr-char)
       (sc-advance sc)))

    tree))


(defun pjb-re-parse-element (sc)
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
  (let (tree simple curr-char)
    (setq simple (pjb-re-parse-simple sc))
    (setq curr-char (sc-curr-char sc))
    (cond
      ((null curr-char)  (setq tree simple))

      ((eq (character '\?) curr-char)
       (sc-advance sc)
       (if (eq (character '\?) (sc-curr-char sc))
           (progn
             (sc-advance sc)
             (setq tree (list :non-greedy-optional  simple)))
           (setq tree (list :optional simple))))

      ((eq (character '\*) curr-char)
       (sc-advance sc)
       (if (eq (character '\?) (sc-curr-char sc))
           (progn
             (sc-advance sc)
             (setq tree (list :non-greedy-zero-or-more  simple)))
           (setq tree (list :zero-or-more simple)))) 

      ((eq (character '\+) curr-char)
       (sc-advance sc)
       (if (eq (character '\?) (sc-curr-char sc))
           (progn
             (sc-advance sc)
             (setq tree (list :non-greedy-one-or-more  simple)))
           (setq tree (list :one-or-more simple))))

      ((and (eq (character '\\) curr-char)
            (eq (character '\{) (sc-next-char sc)))
       (sc-advance sc)
       (sc-advance sc)
       (setq tree '(:error "\{...\} not implemented yet.")))

      (t                 (setq tree simple)))
    tree))


(defun pjb-re-collapse-strings (tree)
  "
RETURNS: A new list where all sequences of characters are collapsed
         into strings. Signle characters are not collapsed.
NOTE:    Does not works recursively because recursive sequences are built
         bottom-up.
"
  (loop
    :with result = nil
    :with string = nil
    :for item :in tree
    :do (if (characterp item)
            (push item string)
            (progn
              (when string
                (if (= 1 (length string))
                    (push (car string) result)
                    (push (implode-string (nreverse string)) result))
                (setq string nil))
              (push item result)))
    :finally (when string
               (if (= 1 (length string))
                   (push (car string) result)
                   (push (implode-string (nreverse string)) result))
               (setq string nil))
             (return (nreverse result))))


(defun pjb-re-parse-sequence (sc)
  "
DO:      Parses a regexp sequence.
RETURNS: A parse tree.

sequence ::= element sequence  .  (:sequence element element ...)
sequence ::= element .             element
sequence ::= .                     nil
"
  (let ((tree nil))
    (loop
      :while (and (sc-curr-char sc)
                  (not (and (eq (character '\\) (sc-curr-char sc))
                            (or (eq (sc-next-char sc) (character '\|))
                                (eq (sc-next-char sc) (character ")") )))))
      :do (push (pjb-re-parse-element sc) tree))
    (cons :sequence (pjb-re-collapse-strings (nreverse tree)))
;;;     (if (<= (length tree) 1)
;;;         (car tree)
;;;       (progn
;;;         (setq tree (pjb-re-collapse-strings (nreverse tree)))
;;;         (if (<= (length tree) 1)
;;;             (car tree)
;;;           tree)))
    ))


(defun pjb-re-parse-regexp (sc)
  "
DO:      Parses a regexp.
RETURNS: A parse tree.
NOTE:    The result may contain the symbol :error followed by a string.

regexp ::= sequence '\|' regexp .   (:alternative sequence sequence...)
regexp ::= sequence .               sequence
"
  (let (tree)
    (setq tree (list (pjb-re-parse-sequence sc)))
    (loop :while (and (eq (character '\\) (sc-curr-char sc))
                      (eq (character '\|) (sc-next-char sc)))
          :do (sc-advance sc)
              (sc-advance sc)
              (push (pjb-re-parse-sequence sc) tree))
    (if (= 1 (length tree))
        (car tree)
        (cons :alternative (nreverse tree)))))


(defun pjb-re-parse-whole-regexp (sc)
  (let ((tree (pjb-re-parse-regexp sc))
        (curr-char (sc-curr-char sc)))
    (if curr-char
        (setq tree
              (list :error (format nil "Syntax error at ~D (~A ~A)."
                                   (sc-position sc)
                                   curr-char
                                   (if (sc-next-char sc)
                                       (sc-next-char sc)  ""))
                    tree)))
    tree))



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



(defvar pjb-re-new-line (code-char 10) "A new-line.")


(defmacro pjb-re-slot-node        (obj)        `(aref ,obj 0))
(defmacro pjb-re-slot-match       (obj)        `(aref ,obj 1))
(defmacro pjb-re-slot-string      (obj)        `(aref ,obj 2))
(defmacro pjb-re-slot-begin       (obj)        `(aref ,obj 3))
(defmacro pjb-re-slot-end         (obj)        `(aref ,obj 4))
(defmacro pjb-re-slot-try         (obj)        `(aref ,obj 5))
(defmacro pjb-re-slot-private     (obj)        `(aref ,obj 6))
(defmacro pjb-re-slot-children    (obj)        `(aref ,obj 7))

;;; (DEFMACRO PJB-RE-SLOT-BEGIN-SET   (OBJ VALUE) `(SETF (AREF ,OBJ 3) ,VALUE))
;;; (DEFMACRO PJB-RE-SLOT-END-SET     (OBJ VALUE) `(SETF (AREF ,OBJ 4) ,VALUE))
;;; (DEFMACRO PJB-RE-SLOT-TRY-SET     (OBJ VALUE) `(SETF (AREF ,OBJ 5) ,VALUE))
;;; (DEFMACRO PJB-RE-SLOT-PRIVATE-SET (OBJ VALUE) `(SETF (AREF ,OBJ 6) ,VALUE))
;;; (DEFSETF  PJB-RE-SLOT-BEGIN       PJB-RE-SLOT-BEGIN-SET)
;;; (DEFSETF  PJB-RE-SLOT-END         PJB-RE-SLOT-END-SET)
;;; (DEFSETF  PJB-RE-SLOT-TRY         PJB-RE-SLOT-TRY-SET)
;;; (DEFSETF  PJB-RE-SLOT-PRIVATE     PJB-RE-SLOT-PRIVATE-SET)

(declaim (type (function (array) (function (array) t)) pjb-re-slot-match))

(defmacro pjb-re-init (node position)
  `(let ((node ,node)
         (position ,position))
     (setf (pjb-re-slot-begin node) position)
     (setf (pjb-re-slot-try node) nil)
     (setf (pjb-re-slot-end node) nil)
     (values)))
  

(defmacro pjb-re-match (node)
  `(let ((node ,node))
     (funcall (pjb-re-slot-match node) node)))





(defun pjb-re-character-match (node)
  "Matches a character.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p  (pjb-re-slot-begin node)) )
    (if (pjb-re-slot-try   node)
        ;; already tested. no more match:
        nil
        ;; first test, let's see:
        (progn
          (setf (pjb-re-slot-try node) t)
          (if (char= (pjb-re-slot-node node) (char (pjb-re-slot-string node) p))
              (progn
                (setq p (1+ p))
                (setf (pjb-re-slot-end node) p)
                p)
              nil)))))



(defun pjb-re-string-match (node)
  "Matches a string.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p  (pjb-re-slot-begin node)) )
    (if (pjb-re-slot-try   node)
        ;; already tested. no more match:
        nil
        ;; first test, let's see:
        (let* ((m   (pjb-re-slot-node node))
               (len (length m))
               (e   (+ p len))
               (s   (pjb-re-slot-string node)) )
          (setf (pjb-re-slot-try node) t)
          (unless (and (< e (length s))
                       (string= m s :start2 p :end2 e))
            (setq e nil))
          (setf (pjb-re-slot-end node) e)
          e))))



(defun pjb-re-null-match (node)
  "Matches a null.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (if (pjb-re-slot-try   node)
      ;; already tested. no more match:
      nil
      ;; first test, let's see:
      (progn
        (setf (pjb-re-slot-try node) t)
        t ;; yes! we match.
        )))


(defun pjb-re-alternative-match (node)
  "Matches a alternative.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node))
        (found nil) )
    (when (null n) (setq n 0))
    (loop :while (and (< n (length children))
                      (not found))
          :do (pjb-re-init (aref children n) p)
              (setq found (pjb-re-match (aref children n)))
          :finally (setf (pjb-re-slot-end node) found)
                   (setf (pjb-re-slot-try node) (1+ n))
          :finally (return found))))


(defun pjb-re-any-category-match (node)
  "Matches a any-category.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-any-character-match (node)
  "Matches a any-character.  That is, anything but a NEW-LINE!
RETURNS: nil when no match,
         or the next unmatched position when there's a match.

A period ( '.' ), when used outside a bracket expression, is a BRE
that shall match any character in the supported character set except
NUL.

"
  (let ((p         (pjb-re-slot-begin node)) )
    (if (pjb-re-slot-try   node)
        ;; already tested. no more match:
        nil
        (progn ;; first test, let's see:
          (setf (pjb-re-slot-try node) t)
          (if (< p (length (pjb-re-slot-string node)))
              (progn
                (setq p (1+ p))
                (setf (pjb-re-slot-end node) p))
              (setf  (pjb-re-slot-end node) nil))))))


(defun pjb-re-any-not-category-match (node)
  "Matches a any-not-category.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-any-not-syntax-class-match (node)
  "Matches a any-not-syntax-class.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-any-not-word-character-match (node)
  "Matches a any-not-word-character.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-any-syntax-class-match (node)
  "Matches a any-syntax-class.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-any-word-character-match (node)
  "Matches a any-word-character.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-char-set-match (node)
  "Matches a char-set.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))    
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-empty-at-beginning-of-line-match (node)
  "Matches a empty-at-beginning-of-line.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-empty-at-beginning-of-string-match (node)
  "Matches a empty-at-beginning-of-string.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p  (pjb-re-slot-begin  node)) )
    (if (pjb-re-slot-try  node)
        ;; already tested. no more match:
        nil
        ;; first test, let's see:
        (progn
          (setf (pjb-re-slot-try node) t)
          (if (=  0 p) ;; TODO use a :start / :end for the string!
              (progn
                (setf (pjb-re-slot-end node) p)
                p)
              nil)))))


(defun pjb-re-empty-at-beginning-of-word-match (node)
  "Matches a empty-at-beginning-of-word.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-empty-at-end-of-line-match (node)
  "Matches a empty-at-end-of-line.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-empty-at-end-of-string-match (node)
  "Matches a empty-at-end-of-string.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p  (pjb-re-slot-begin  node)) )
    (if (pjb-re-slot-try  node)
        ;; already tested. no more match:
        nil
        ;; first test, let's see:
        (progn
          (setf (pjb-re-slot-try node) t)
          (if (=  (length (pjb-re-slot-string node)) p) ;; TODO use a :start / :end for the string!
              (progn
                (setf (pjb-re-slot-end node) p)
                p)
              nil)))))


(defun pjb-re-empty-at-end-of-word-match (node)
  "Matches a empty-at-end-of-word.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-empty-at-limit-of-word-match (node)
  "Matches a empty-at-limit-of-word.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-empty-at-point-match (node)
  "Matches a empty-at-point.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-empty-not-at-limit-of-word-match (node)
  "Matches a empty-not-at-limit-of-word.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-error-match (node)
  "Matches a error.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-group-match (node)
  "Matches a group.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (child     (aref (pjb-re-slot-children node) 0)) )
    (if (pjb-re-slot-try   node)
        ;; already tested. no more match:
        nil
        ;; first test, let's see:
        (progn
          (setf (pjb-re-slot-try node) t)
          (pjb-re-init child p)
          (setq p (pjb-re-match child))
          (when p
            (setf (pjb-re-slot-end node) p))
          p))))


(defun pjb-re-inverse-char-set-match (node)
  "Matches a inverse-char-set.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-non-greedy-one-or-more-match (node)
  "Matches a non-greedy-one-or-more.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((n      (pjb-re-slot-try   node))
        (p      (pjb-re-slot-begin node))
        (child  (aref (pjb-re-slot-children  node) 0)) )
    (cond
      ((null n) ;; first time
       (pjb-re-init child p)
       (setq p (pjb-re-match child))
       (setf (pjb-re-slot-end node) p)
       (setf (pjb-re-slot-try node) (if p :more :over))
       p)
      ((eq :more n)
       (setq p (pjb-re-slot-end node))
       (pjb-re-init child p)
       (setq p (pjb-re-match child))
       (setf (pjb-re-slot-end node) p)
       (setf (pjb-re-slot-try node) (if p :more :over))
       p)
      (t
       nil))))


(defun pjb-re-non-greedy-optional-match (node)
  "Matches a non-greedy-optional.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (child     (aref (pjb-re-slot-children node) 0)) )
    (cond
      ((null n) ;; first time, let's be non greedy: match nothing.
       (setf (pjb-re-slot-end node) p)
       (setf (pjb-re-slot-try node) :second) )
      ((eq n :second) ;; second time, we expect the child.
       (pjb-re-init child p)
       (setq p (pjb-re-match child))
       (setf (pjb-re-slot-end node) p)
       (setf (pjb-re-slot-try node) :last) )
      (t ;; too late we don't match anything.
       (setq p nil)
       (setf (pjb-re-slot-end node) p) ))
    p))


(defun pjb-re-non-greedy-zero-or-more-match (node)
  "Matches a non-greedy-zero-or-more.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((n      (pjb-re-slot-try  node))
        (s      (pjb-re-slot-string  node))
        (child  (aref (pjb-re-slot-children  node) 0)) )
    (cond
      ((null n) ;; case zero
       (setq n (pjb-re-slot-begin node))
       (setf (pjb-re-slot-end node) n)
       (setf (pjb-re-slot-try node) n)
       )
      ((eq t n) ;; no more match
       )
      ((= n (length s))
       ;; match end of string with any number, but no more.
       (setf (pjb-re-slot-end node) n)
       (setf (pjb-re-slot-try node) t))
      (t
       (pjb-re-init child n)
       (setq n (pjb-re-match child))
       (if n
           (progn
             (setf (pjb-re-slot-end node) n)
             (setf (pjb-re-slot-try node) n))
           (progn
             (setf (pjb-re-slot-end node) nil)
             (setf (pjb-re-slot-try node) t)))))
    n))


(defun pjb-re-optional-match (node)
  "Matches a optional.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (child     (aref (pjb-re-slot-children node) 0)) )
    (cond
      ((null n) ;; first time, we expect the child.
       (pjb-re-init child p)
       (setq p (pjb-re-match child))
       (setf (pjb-re-slot-end node) p)
       (setf (pjb-re-slot-try node) :second) )
      ((eq n :second) ;; second time,  let's be non greedy: match nothing.
       (setf (pjb-re-slot-end node) p)
       (setf (pjb-re-slot-try node) :last) )
      (t ;; too late we don't match anything.
       (setq p nil)
       (setf (pjb-re-slot-end node) p) ))
    p))


(defun pjb-re-one-or-more-match (node)
  "Matches a one-or-more.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-reference-match (node)
  "Matches a reference.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-repeat-between-match (node)
  "Matches a repeat-between.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-repeat-exact-match (node)
  "Matches a repeat-exact.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-sequence-match (node)
  "Matches a sequence.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (when children
      (unless n
        (setq n 0)
        (pjb-re-init (aref children n) p))
      (setq p (pjb-re-match (aref children n)))
      (loop :while (or (and p (< (1+ n) (length children)))
                       (and (not p) (<= 0 (1- n))))
            :do (if p
                    (progn
                      (setq n (1+ n))
                      (pjb-re-init (aref children n) p))
                    (setq n (1- n)))
                (setq p (pjb-re-match (aref children n))))
      ;; p       ==> (= (1+ n) (length children)) ==> 0 <= n < (length children)
      ;; (not p) ==>    (= -1 (1- n)) ==>  n=0    ==> 0 <= n < (length children)
      (setf (pjb-re-slot-try node) n)
      (setf (pjb-re-slot-end node) p))
    p))


(defun pjb-re-shy-group-match (node)
  "Matches a shy-group.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((p         (pjb-re-slot-begin node))
        (n         (pjb-re-slot-try   node))
        (children  (pjb-re-slot-children node)) )
    (declare (ignore p n children))
    (error "Not Implemented Yet: ~S~%" (pjb-re-slot-node node))))


(defun pjb-re-zero-or-more-match (node)
  "Matches a zero-or-more.
RETURNS: nil when no match,
         or the next unmatched position when there's a match.
"
  (let ((n      (pjb-re-slot-try     node))
        (s      (pjb-re-slot-string  node))
        (p      (pjb-re-slot-begin   node))
        (child  (aref (pjb-re-slot-children  node) 0)) )
    ;; Note: we should try to save all the previous matches (from zero to n)
    ;;       to backtrack faster, but we would need to save possibly a lot
    ;;       of recursive state for all the child subtree...
    (cond
      ((null n) ;; first time: match all we can.
       (setq n (loop :with p = (pjb-re-slot-begin   node)
                     :for n = 0 :then (1+ n)
                     :while (and p (< p (length s)))
                     :do (pjb-re-init child p)
                         (setq p (pjb-re-match child))
                     :finally (return n)))
       ;; oops we did one too many.
       ;; let's redo it till the limit
       (setf (pjb-re-slot-try node) n)
       (pjb-re-zero-or-more-match node))
      ((< n 0) ;; we tried everything.
       (setf (pjb-re-slot-end node) nil))
      (t ;; match n-1 times.
       (loop :for i :from 1 :below n
             :do (pjb-re-init child p)
                 (setq p (pjb-re-match child)))
       (setf (pjb-re-slot-end node) p)
       (setf (pjb-re-slot-try node) (1- n))
       p))))



(defun pjb-re-make-pjb-re-symbol (key ext)
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
URL:        <http://www.informatimago.com/local/lisp/HyperSpec/Body/02_cd.htm>
            <http://www.informatimago.com/local/lisp/HyperSpec/Body/f_intern.htm#intern>
"
  (if (string= "emacs" (lisp-implementation-type))
      (intern (string-downcase (format nil "pjb-re-~s-~s"
                                       (subseq (symbol-name key) 1) ext)))
      (intern (string-upcase (format nil "pjb-re-~a-~a" (symbol-name key) ext))
              (find-package "PJB-REGEXP"))))


(defun pjb-re-decorate-tree (tree string)
  "
RETURN:  A decorated tree that can be used for the matching the string.
"
  (tree-decorate
   tree
   (lambda (node children)
     (let ((obj (make-array '(9)))
           key)
       (cond
         ((null       node) (setq key :null))
         ((characterp node) (setq key :character))
         ((stringp    node) (setq key :string))
         ((listp      node) (setq key :list))
         ((member node '(
                         :alternative :any-category :any-character
                         :any-not-category :any-not-syntax-class
                         :any-not-word-character :any-syntax-class
                         :any-word-character :char-set
                         :empty-at-beginning-of-line
                         :empty-at-beginning-of-string
                         :empty-at-beginning-of-word
                         :empty-at-end-of-line :empty-at-end-of-string
                         :empty-at-end-of-word :empty-at-limit-of-word
                         :empty-at-point :empty-not-at-limit-of-word
                         :error :group :inverse-char-set
                         :non-greedy-one-or-more :non-greedy-optional
                         :non-greedy-zero-or-more :optional :one-or-more
                         :reference :repeat-between :repeat-exact
                         :sequence :shy-group :zero-or-more)
                  :test (function eq))
          (setq key node))
         (t (error "INTERNAL: Unexpected node in match tree: ~S !"
                   node)))
       (setf (aref obj 0) node)
       (setf (aref obj 1) (pjb-re-make-pjb-re-symbol key "match"))
       (setf (aref obj 2) string)
       (setf (aref obj 3) 0) ;; beg (start)
       (setf (aref obj 4) 0) ;; end
       (setf (aref obj 5) nil) ;; try
       (setf (aref obj 6) nil) ;; private
       (setf (aref obj 7) (when children
                            (make-array (list (length children))
                                        :initial-contents children)))
       obj))))


(defun pjb-re-collect-groups (dec-tree &optional groups)
  (let ((make-groups-flag (not groups)))
    (unless groups
      (setq groups (cons :groups nil)))
    (if (eq :group (pjb-re-slot-node dec-tree))
        (push dec-tree (cdr groups)))
    (loop :with children = (pjb-re-slot-children dec-tree)
          :for i :from 0 :below (length children)
          :for child = (aref children i)
          :do (pjb-re-collect-groups child groups))
    (if make-groups-flag
        (nreverse (cdr groups))
        nil)))



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
           (make-sc (concatenate 'string "\\`.*?\\(" regexp "\\).*?\\'"))))
         (dec-tree (pjb-re-decorate-tree syn-tree string))
         (groups  (pjb-re-collect-groups dec-tree)) )
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

;;;; THE END ;;;;
