;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               c11-scanner.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    C11 Scanner.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "COM.INFORMATIMAGO.LANGUAGES.C11.SCANNER")

(define-scanner c11-scanner
  ;; This scanner is not used by c11-parser, but by read-yacc.
  :terminals  (
               "_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex"
               "_Generic" "_Imaginary" "_Noreturn" "_Static_assert"
               "_Thread_local" "auto" "break" "case" "char" "const"
               "continue" "default" "do" "double" "else" "enum" "extern"
               "float" "for" "goto" "if" "inline" "int" "long" "register"
               "restrict" "return" "short" "signed" "sizeof" "static"
               "struct" "switch" "typedef" "union" "unsigned" "void"
               "volatile" "while"

               "^=" "|=" "-=" "<<=" ">>=" "&=" "&&" "||" "*=" "/=" "%="
               "+=" "->" "++" "--" "<<" ">>" "<=" ">=" "==" "!=" "(" ")"
               "," ":" ";" "." "..." "[" "]" "{" "}" "&" "*" "/" "+" "-" "~" "!"
               "%" "<" ">" "=" "^" "|" "?"

               (identifier "[a-zA-Z_$][a-zA-Z_$0-9]*")
               (hex        "0[xX][0-9A-Fa-f]+[uUlL]*")
               (oct        "0[0-7]+[uUlL]*")
               (dec        "[0-9]+[uUlL]*")
               (lchar      "L?'(\\.|[^\\'])+'")
               (flt1       "[0-9]+[Ee][-+]?[0-9]+[fFlL]?")
               (flt2       "[0-9]*\\.[0-9]+([Ee][-+]?[0-9]+)?[fFlL]?")
               (flt3       "[0-9]+\\.[0-9]*([Ee][-+]?[0-9]+)?[fFlL]?")
               (str        "L?\"(\\.|[^\\\"])*\""))
  :alphanumerics "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$"
  :spaces (load-time-value  (coerce (remove-duplicates
                                     '(#\Space
                                       #\Newline
                                       #+has-tab     #\Tab
                                       #+has-page    #\Page
                                       #+has-null    #\Null
                                       #+(and has-return   (not newline-is-return))   #\Return
                                       #+(and has-linefeed (not newline-is-linefeed)) #\Linefeed))
                                    'string)))



(defparameter *parser-package*
  (load-time-value (find-package "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER"))
  "Package where the token kinds are interned.")

(defparameter *symbol-package*
  (load-time-value (find-package "COM.INFORMATIMAGO.LANGUAGES.C11.C"))
  "Package where the identifiers of the C program are interned.")

(defparameter *c11-literal-tokens*
  '("_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex"
    "_Generic" "_Imaginary" "_Noreturn" "_Static_assert"
    "_Thread_local" "auto" "break" "case" "char" "const"
    "continue" "default" "do" "double" "else" "enum" "extern"
    "float" "for" "goto" "if" "inline" "int" "long" "register"
    "restrict" "return" "short" "signed" "sizeof" "static"
    "struct" "switch" "typedef" "union" "unsigned" "void"
    "volatile" "while"

    "^=" "|=" "-=" "<<=" ">>=" "&=" "&&" "||" "*=" "/=" "%="
    "+=" "->" "++" "--" "<<" ">>" "<=" ">=" "==" "!=" "(" ")"
    "," ":" ";" "." "..." "[" "]" "{" "}" "&" "*" "/" "+" "-" "~" "!"
    "%" "<" ">" "=" "^" "|" "?"))

(defparameter *c11-literal-tokens-map*
  (load-time-value
   (let ((table (make-hash-table :test 'equal)))
     (dolist (token *c11-literal-tokens* table)
       (setf (gethash token table)
             (let ((name (string-upcase token)))
               (when (char= (aref name 0) #\_)
                 (setf name (substitute #\- #\_ (subseq name 1))))
               (intern name *parser-package*)))))))

(defparameter *c11-regexp-tokens*
  ;; order matters
  '((string-literal
     (str        "^L?\"(\\.|[^\\\"])*\"$"))
    (i-constant
     (lchar      "^L?'(\\.|[^\\'])+'$"))
    (identifier
     (identifier "^[a-zA-Z_$][a-zA-Z_$0-9]*$"))
    (f-constant
     (flt1       "^[0-9]+[Ee][-+]?[0-9]+[fFlL]?$")
     (flt2       "^[0-9]*\\.[0-9]+([Ee][-+]?[0-9]+)?[fFlL]?$")
     (flt3       "^[0-9]+\\.[0-9]*([Ee][-+]?[0-9]+)?[fFlL]?$"))
    (i-constant
     (hex        "^0[xX][0-9A-Fa-f]+[uUlL]*$")
     (oct        "^0[0-7]+[uUlL]*$")
     (dec        "^[0-9]+[uUlL]*$"))))

(defvar *context*)

(defun upgrade-c11-token (token)
  (let* ((text    (token-text token))
         (literal (gethash text *c11-literal-tokens-map*)))
    (if literal
        (setf (token-kind token) literal)
        (let ((kind (first (find-if (lambda (entry)
                                      (some (lambda (regexp)
                                              (string-match (second regexp) text))
                                            (rest entry)))
                                    *c11-regexp-tokens*))))
          (if (eq kind 'identifier)
              (setf (token-symbol token) (intern (token-text token) *symbol-package*)
                    (token-kind   token) (cond
                                           ((typedef-name-p              *context* token) 'typedef-name)
                                           ((function-name-p             *context* token) 'func-name)
                                           ((enumeration-constant-name-p *context* token) 'enumeration-constant)
                                           (t                                             'identifier)))
              (setf (token-kind   token) kind))))
    token))

;;;; THE END ;;;;
