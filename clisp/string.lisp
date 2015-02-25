;;;; -*- coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:               string.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             clisp
;;;;USER-INTERFACE:     clisp
;;;;DESCRIPTION
;;;;
;;;;    This module exports string functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-01-30 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2015
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


(defpackage "COM.INFORMATIMAGO.CLISP.STRING"
  (:documentation "This module exports string functions.")
  (:use "COMMON-LISP"
        #+clisp "REGEXP"
        #-clisp "CL-PPCRE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "SPLIT-STRING" "UNSPLIT-STRING"
           "STRING-MATCH" "STRING-TO-NUMBER"
           "CAPITALIZATION"
           "*CASE-FOLD-SEARCH*" "REPLACE-REGEXP-IN-STRING"
           "SUBSTRING"))
(in-package "COM.INFORMATIMAGO.CLISP.STRING")



;; We have our own implementation of SPLIT-STRING using REGEXP,
;; specific to CLISP.

(defparameter split-string-default-separators
  (format nil "[ ~C~C~C~C~C]\\+"
          (code-char 9) (code-char 10) (code-char 11) (code-char 12)
          (code-char 13))
  "The default separators for split-string (HT, LF, VT, FF, CR, SP)")


(defun split-string (string &optional separators)
  "
NOTE:   This implementation uses he REGEXP package.
"
  (unless separators (setq separators split-string-default-separators))
  (let ((result (regexp:regexp-split separators string)))
    (if (string= "" (car result))
        (setq result (cdr result)))
    (if (string= "" (car (last result)))
        (setq result (nbutlast result)))
    result))

;; But we inherit UNSPLIT-STRING from COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING.


(defun string-match (regexp string &key (start 0) (end nil)
                     (case-sensitive nil)
                     (extended nil)
                     (newline nil)
                     (nosub nil))
  "An alias for REGEXP:MATCH."
  (regexp:match regexp string
                :start start :end end
                :ignore-case (not case-sensitive)
                :extended extended
                :newline newline :nosub nosub))



(defvar *case-fold-search* nil
  "Whether searches and matches should ignore case.
Used by: REPLACE-REGEXP-IN-STRING.
")



;;; CAPITALIZATION:
;;;
;;; 0  NIL
;;; 1   T
;;;
;;; 0  Upcase
;;; 1  Lowcase
;;; 2  Nocase
;;; 3  Special
;;;
;;; STATE:   (BOW,U/L/N/S)
;;;   Initial state: (NIL SP)
;;;
;;;   ((NIL UP) UP) --> (NIL UP) NOT NO-2C-WORD
;;;   ((NIL UP) LO) --> (NIL LO) NOT NO-2C-WORD NOT ALL-UPCASE
;;;   ((NIL UP) NO) --> (NIL NO) NOT NO-2C-WORD 
;;;   ((NIL UP) SP) --> (NIL SP)
;;;   ((NIL LO) UP) --> (NIL UP) NOT NO-2C-WORD NOT ALL-LOCASE NOT ALL-CAPITA 
;;;   ((NIL LO) LO) --> (NIL LO) NOT NO-2C-WORD
;;;   ((NIL LO) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   ((NIL LO) SP) --> (NIL SP)
;;;   ((NIL NO) UP) --> (NIL UP) NOT NO-2C-WORD
;;;   ((NIL NO) LO) --> (NIL LO) NOT NO-2C-WORD
;;;   ((NIL NO) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   ((NIL NO) SP) --> (NIL SP)
;;;   ((NIL SP) UP) --> ( T  UP) NOT ALL-LOCASE  
;;;   ((NIL SP) LO) --> ( T  LO) NOT ALL-UPCASE  NOT ALL-CAPITA
;;;   ((NIL SP) NO) --> ( T  NO)
;;;   ((NIL SP) SP) --> (NIL SP)
;;;   (( T  UP) UP) --> (NIL UP) NOT NO-2C-WORD NOT ALL-LOCASE NOT ALL-CAPITA
;;;   (( T  UP) LO) --> (NIL LO) NOT NO-2C-WORD NOT ALL-UPCASE  
;;;   (( T  UP) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   (( T  UP) SP) --> (NIL SP)
;;;   (( T  LO) UP) --> (NIL UP) NOT NO-2C-WORD NOT ALL-LOCASE
;;;   (( T  LO) LO) --> (NIL LO) NOT NO-2C-WORD NOT ALL-UPCASE  
;;;   (( T  LO) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   (( T  LO) SP) --> (NIL SP)
;;;   (( T  NO) UP) --> (NIL UP) NOT NO-2C-WORD NOT ALL-LOCASE NOT ALL-CAPITA 
;;;   (( T  NO) LO) --> (NIL LO) NOT NO-2C-WORD NOT ALL-UPCASE  
;;;   (( T  NO) NO) --> (NIL NO) NOT NO-2C-WORD
;;;   (( T  NO) SP) --> (NIL SP)
;;;    ( T  SP) is impossible.

(defparameter +capitalization-transitions+
  (make-array '(2 4 4)
              :initial-contents
              '((( (0 0 3)
                  (0 1 3 0)
                  (0 2 3)
                  (0 3) )
                 ( (0 0 3 1 2)
                  (0 1 3)
                  (0 2 3)
                  (0 3) )
                 ( (0 0 3)
                  (0 1 3)
                  (0 2 3)
                  (0 3) )
                 ( (1 0 1)
                  (1 1 0 2)
                  (1 2)
                  (0 3) ))
                (( (0 0 3 1 2)
                  (0 1 3 0)
                  (0 2 3)
                  (0 3) )
                 ( (0 0 3 1)
                  (0 1 3 0)
                  (0 2 3)
                  (0 3) )
                 ( (0 0 3 1 2)
                  (0 1 3 0)
                  (0 2 3)
                  (0 3) )
                 ( (0 0) ;; impossible state
                  (0 1)
                  (0 2)
                  (0 3) )))))



(defun capitalization (string)
  "
RETURN:  :LOWER :UPPER :CAPITALIZED or :WHATEVER
"
  (let ((all-upcase 0)
        (all-locase 1)
        (all-capita 2)
        (no-2c-word 3)
        (result     (make-array '(4) :initial-element t))
        (state      (cons 0 3)) )
    (map nil (lambda (ch)
               (let ((new-state (aref +capitalization-transitions+
                                      (car state) (cdr state)
                                      (cond 
                                        ((not (alpha-char-p ch)) 3)
                                        ((upper-case-p ch)       0)
                                        ((lower-case-p ch)       1)
                                        (t                       2)))))
                 (setf (car state) (pop new-state))
                 (setf (cdr state) (pop new-state))
                 (mapc (lambda (sym) (setf (aref result sym) nil)) new-state)
                 ))
         string)
    (cond ((aref result no-2c-word) :whatever)
          ((aref result all-upcase) :upper)
          ((aref result all-locase) :lower)
          ((aref result all-capita) :capitalized)
          (t                        :whatever))))





(defun emacs-bugged-string-capitalize (string)
  "
The string-capitalized that emacs implements in its replace-regexp-in-string
which is not even its capitalize (which is correct)!
Namely, it seems to  touch only the first character of each word.
"
  (do ((result (copy-seq string))
       (i 0 (1+ i))
       (sp t)
       (ch) )
      ((<= (length result) i) result)
    (setq ch (char result i))
    (if sp
        (when (alpha-char-p ch)
          (setf (char result i) (char-upcase ch))
          (setq sp nil))
        (when (not (alphanumericp ch))
          (setq sp t)))))



(defun replace-regexp-in-string
    (regexp rep string
     &optional (fixedcase nil) (literal nil) (subexp 0) (start 0)
     &key (case-sensitive (not *case-fold-search*))
     (extended nil) (newline nil) (nosub nil))
  "
NOTE:       emacs regexps are a mix between POSIX basic regexps
            and POSIX extended regexps.
            By default we'll use basic POSIX regexps, to keep '\\(...\\)'
            at the cost of the '+' repetition. The key parameters are
            passed to REGEXP:MATCH if specific behavior is needed.
            (We're not entirely compatible with emacs, but it's emacs which
            is wrong and we'll have to use another regexp package in emacs).

Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.
When REP is a function it's passed the while match 0, even if SUBEXP is not 0.

To replace only the first match (if any), make REGEXP match up to \'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"

If second arg FIXEDCASE is non-nil, do not alter case of replacement text.
Otherwise maybe capitalize the whole text, or maybe just word initials,
based on the replaced text.
If the replaced text has only capital letters
and has at least one multiletter word, convert NEWTEXT to all caps.
Otherwise if all words are capitalized in the replaced text,
capitalize each word in NEWTEXT.

If third arg LITERAL is non-nil, insert NEWTEXT literally.
Otherwise treat `\' as special:
  `\&' in NEWTEXT means substitute original matched text.
  `\N' means substitute what matched the Nth `\(...\)'.
       If Nth parens didn't match, substitute nothing.
  `\\' means insert one `\'.
Case conversion does not apply to these substitutions.

FIXEDCASE and LITERAL are optional arguments.

The optional fifth argument SUBEXP specifies a subexpression ;
it says to replace just that subexpression with NEWTEXT,
rather than replacing the entire matched text.
This is, in a vague sense, the inverse of using `\N' in NEWTEXT ;
`\N' copies subexp N into NEWTEXT, but using N as SUBEXP puts
NEWTEXT in place of subexp N.
This is useful only after a regular expression search or match,
since only regular expressions have distinguished subexpressions.
"
;;; REP       function(\0) --> NEWTEXT
;;; REP       string       --> NEWTEXT
;;;
;;; FIXEDCASE   T         identity
;;; FIXEDCASE   NIL       replaced text capitalization -> replacement
;;;
;;; LITERAL     T         identity
;;; LITERAL     NIL       substitute \&, \N, \\ and \x.
;;;
;;; SUBEXP      N         replaces only \N instead of \0
  (do ((done nil)
       (pieces '())
       (pos 0)
       (replacement)
       (replaced-match)
       (matches))
      (done
       (progn (push (subseq string pos) pieces)
              (apply (function concatenate) 'string (nreverse pieces))))
    (setq matches (multiple-value-list
                   (regexp:match regexp string
                                 :start start
                                 :ignore-case (not case-sensitive)
                                 :extended extended
                                 :newline newline
                                 :nosub nosub)))
    (if (and matches (car matches))
        (progn
          ;; -1- Find the replacement:
          (setq replacement
                (if (functionp rep)
                    (funcall rep (regexp:match-string string (car matches)))
                    rep))
          ;; -2- Process FIXEDCASE
          (when (or (< subexp 0) (<= (length matches) subexp))
            (error "Argument out of range SUBEXP=~A." subexp))
          (setq replaced-match (nth subexp matches))
          (unless fixedcase
            (let ((cap (capitalization
                        (regexp:match-string string replaced-match))) )
              (setq replacement
                    (funcall
                     (cond
                       ((eq cap :upper) (function string-upcase))
                       ((eq cap :lower) (function identity))
                       ;;                That's what emacs does...
                       ((eq cap :capitalized)
                        (function emacs-bugged-string-capitalize))
                       (t               (function identity)))
                     replacement))))
          ;; -3- Process LITERAL
          (unless literal
            ;; substitute \&, \N and \\.
            (setq replacement
                  (replace-regexp-in-string
                   "\\\\\\(.\\)"
                   (lambda (substr)
                     (cond
                       ((char= (char substr 1) (character "&"))
                        (regexp:match-string string (car matches)) )
                       ((digit-char-p (char substr 1))
                        (let ((n (parse-integer substr :start 1)))
                          (if (<= (length matches) n)
                              substr ;; How coherent emacs is!
                              (regexp:match-string string (nth n matches)))) )
                       ((char= (character "\\") (char substr 1))
                        (subseq substr 1) )
                       (t
                        (error "Invalid use of '\\' in replacement text ~W."
                               substr) )))
                   replacement t t)) )
          ;; -4- Replace.
          (push (subseq string pos
                        (regexp:match-start (nth subexp matches))) pieces)
          (push replacement pieces)
          (setq start
                (if (= 0 (length regexp))
                    (1+ start)
                    (regexp:match-end (car matches))))
          (setq pos (regexp:match-end (nth subexp matches)))
          (setq done (<= (length string) start)) )
        (progn
          (setq done t) ))))


(defun string-to-number (string &key (base 10) (start 0) (end nil))
  "
DO:         Converts the string to a number.
RETURN:     A number.
"
  ;; PARSE-INTEGER is for integers...
  (let ((result  (with-input-from-string
                     (stream string :start start :end end)
                   (let ((*read-base* base)) (read stream)))))
    (unless (numberp result)
      (error "Expected a number, not ~S." result))
    result))

;;;; THE END ;;;;
