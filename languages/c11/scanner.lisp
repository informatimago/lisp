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
  '((lchar      "L?'(\\.|[^\\'])+'")
    (str        "L?\"(\\.|[^\\\"])*\"")
    (identifier "[a-zA-Z_$][a-zA-Z_$0-9]*")
    (flt1       "[0-9]+[Ee][-+]?[0-9]+[fFlL]?")
    (flt2       "[0-9]*\\.[0-9]+([Ee][-+]?[0-9]+)?[fFlL]?")
    (flt3       "[0-9]+\\.[0-9]*([Ee][-+]?[0-9]+)?[fFlL]?")
    (hex        "0[xX][0-9A-Fa-f]+[uUlL]*")
    (oct        "0[0-7]+[uUlL]*")
    (dec        "[0-9]+[uUlL]*")))

(defun compute-token-kind (token)
  (let ((text  (token-text token)))
    (or (gethash text *c11-literal-tokens-map*)
        (first (find-if (lambda (entry)
                          (string-match (format nil "^~A$" (second entry)) text))
                        *c11-regexp-tokens*)))))


#-(and) (
         (mapcar 'compute-token-kind (subseq *tc* 0 100))
         (|typedef| |unsigned| |int| identifier \; |typedef| |signed| |char| identifier \; |typedef| |unsigned| |char| identifier \; |typedef| |short| identifier \; |typedef| |unsigned| |short| identifier \; |typedef| |int| identifier \; |typedef| |unsigned| |int| identifier \; |typedef| |long| |long| identifier \; |typedef| |unsigned| |long| |long| identifier \; |typedef| |long| identifier \; |typedef| |unsigned| |int| identifier \; |typedef| |int| identifier \; |typedef| |union| { |char| identifier [ dec ] \; |long| |long| identifier \; } identifier \; |typedef| identifier identifier \; |typedef| |int| identifier \; |typedef| |unsigned| |long| identifier \; |typedef| |__builtin_va_list| identifier \; |typedef| identifier identifier \; |typedef| identifier identifier \; |typedef| identifier)

         ("typedef" "unsigned" "int" "bool_bf" ";" "typedef" "signed" "char" "__int8_t" ";" "typedef" "unsigned" "char" "__uint8_t" ";" "typedef" "short" "__int16_t" ";" "typedef" "unsigned" "short" "__uint16_t" ";" "typedef" "int" "__int32_t" ";" "typedef" "unsigned" "int" "__uint32_t" ";" "typedef" "long" "long" "__int64_t" ";" "typedef" "unsigned" "long" "long" "__uint64_t" ";" "typedef" "long" "__darwin_intptr_t" ";" "typedef" "unsigned" "int" "__darwin_natural_t" ";" "typedef" "int" "__darwin_ct_rune_t" ";" "typedef" "union" "{" "char" "__mbstate8" "[" "128" "]" ";" "long" "long" "_mbstateL" ";" "}" "__mbstate_t" ";" "typedef" "__mbstate_t" "__darwin_mbstate_t" ";" "typedef" "int" "__darwin_ptrdiff_t" ";" "typedef" "unsigned" "long" "__darwin_size_t" ";" "typedef" "__builtin_va_list" "__darwin_va_list" ";" "typedef" "__darwin_ct_rune_t" "__darwin_wchar_t" ";" "typedef" "__darwin_wchar_t" "__darwin_rune_t" ";" "typedef" "__darwin_ct_rune_t")



         (let ((*readtable*               vacietis:c-readtable)
               (vacietis:*compiler-state* (vacietis:make-compiler-state)))
           (with-open-file (src #P"~/src/lisp/c/duff-device.c")
             (read src)))

         (defparameter *s* (make-instance 'c11-scanner :source (com.informatimago.common-lisp.cesarum.file:text-file-contents
                                                                #P"~/src/public/lisp/languages/cpp/tests/out.c")))
         (defparameter *t*
           (let ((scanner  (make-instance 'c11-scanner :source (com.informatimago.common-lisp.cesarum.file:text-file-contents
                                                                #P"~/src/public/lisp/languages/cpp/tests/out.c"))))
             (loop for token =  (scan-next-token scanner)
                   until (eq (token-kind token) 'com.informatimago.common-lisp.parser.scanner::<END\ OF\ SOURCE>)
                   collect (print token))))

         (defparameter *tc*
           (reduce (function append)
                   (reverse (com.informatimago.languages.cpp::context-output-lines
                             (cpp-e "/Users/pjb/src/public/lisp/languages/cpp/tests/emacs.c"
                                    :trace-includes t
                                    :defines '("__GNUC__" "4" "__STDC__" "1" "__x86_64__" "1")
                                    :includes '("/Users/pjb/src/macosx/emacs-24.5/src/")
                                    :include-bracket-directories '("/Users/pjb/src/macosx/emacs-24.5/src/"
                                                                   "/Users/pjb/src/macosx/emacs-24.5/lib/"
                                                                   "/Users/pjb/src/macosx/gcc-4.9.2/gcc/ginclude/" 
                                                                   "/usr/include/")
                                    :write-processed-lines nil)))
                   :initial-value '()))

         (dolist (token *tc*)
           (setf (token-kind token) (compute-token-kind token)))

         (defparameter *yacc*
           (let ((scanner  (make-instance 'c11-scanner :source (com.informatimago.common-lisp.cesarum.file:text-file-contents
                                                                #P"scanner.yacc"))))
             (loop for token =  (scan-next-token scanner)
                   until (eq (token-kind token) 'com.informatimago.common-lisp.parser.scanner::<END\ OF\ SOURCE>)
                   collect (print token))))

         )
;;;; THE END ;;;;
