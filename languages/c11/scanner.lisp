;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               scanner.lisp
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

#-(and) ; we use the cpp-scanner.
(define-scanner c11-scanner
  :terminals  (
               "!" "!=" "%" "%=" "%>" "&" "&&" "&=" "(" ")"
               "*" "*=" "+" "++" "+=" "," "-" "--" "-=" "->" "." "..."
               "/" "/=" ":" ":>" ";" "<" "<%" "<:" "<<" "<<=" "<=" "="
               "==" ">" ">=" ">>" ">>=" "?" "[" "]" "^" "^=" "_Bool"
               "_Complex" "_Imaginary" "__asm__" "__builtin_va_list"
               "__const" "__inline" "__inline__" "__restrict" "asm"
               "auto" "break" "case" "char" "const" "continue"
               "default" "do" "double" "else" "enum" "extern" "float"
               "for" "goto" "if" "inline" "int" "long" "register"
               "restrict" "return" "short" "signed" "sizeof" "static"
               "struct" "switch" "typedef" "union" "unsigned" "void"
               "volatile" "while" "{" "|" "|=" "||" "}" "~" "~="
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


(defparameter *c11-literal-tokens*
  '("!" "!=" "%" "%=" "%>" "&" "&&" "&=" "(" ")"
    "*" "*=" "+" "++" "+=" "," "-" "--" "-=" "->" "." "..."
    "/" "/=" ":" ":>" ";" "<" "<%" "<:" "<<" "<<=" "<=" "="
    "==" ">" ">=" ">>" ">>=" "?" "[" "]" "^" "^=" "_Bool"
    "_Complex" "_Imaginary" "__asm__" "__builtin_va_list"
    "__const" "__inline" "__inline__" "__restrict" "asm"
    "auto" "break" "case" "char" "const" "continue"
    "default" "do" "double" "else" "enum" "extern" "float"
    "for" "goto" "if" "inline" "int" "long" "register"
    "restrict" "return" "short" "signed" "sizeof" "static"
    "struct" "switch" "typedef" "union" "unsigned" "void"
    "volatile" "while" "{" "|" "|=" "||" "}" "~" "~="))

(defparameter *c11-literal-tokens-map*
  (let ((table (make-hash-table :test 'equal)))
    (dolist (token *c11-literal-tokens* table)
      (setf (gethash token table) (intern token)))))

(defparameter *c11-regexp-tokens*
  ;; order matters
  '((|string_literal|
     (str        "L?\"(\\.|[^\\\"])*\""))
    (|i_constant|
     (lchar      "L?'(\\.|[^\\'])+'"))
    (|identifier|
     (identifier "[a-zA-Z_$][a-zA-Z_$0-9]*"))
    (|f_constant|
     (flt1       "[0-9]+[Ee][-+]?[0-9]+[fFlL]?")
     (flt2       "[0-9]*\\.[0-9]+([Ee][-+]?[0-9]+)?[fFlL]?")
     (flt3       "[0-9]+\\.[0-9]*([Ee][-+]?[0-9]+)?[fFlL]?"))
    (|i_constant|
     (hex        "0[xX][0-9A-Fa-f]+[uUlL]*")
     (oct        "0[0-7]+[uUlL]*")
     (dec        "[0-9]+[uUlL]*"))))

(defun compute-token-kind (token)
  (let ((text  (token-text token)))
    (or (gethash text *c11-literal-tokens-map*)
        (let ((kind (first (find-if (lambda (entry)
                                 (some (lambda (regexp)
                                         (string-match (format nil "^~A$" (second regexp)) text))
                                       (rest entry)))
                                    *c11-regexp-tokens*))))
          (if (eq kind '|identifier|)
              (cond
                ((typedef-name-p              *context* token) '|typedef_name|)
                ((function-name-p             *context* token) '|func_name|)
                ((enumeration-constant-name-p *context* token) '|enumeration_constant|)
                (t kind))
              kind)))))

;;;; THE END ;;;;
