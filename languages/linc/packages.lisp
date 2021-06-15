;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines the packages.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-12-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2007 - 2019
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
(cl:in-package "COMMON-LISP-USER")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))


(defpackage "COM.INFORMATIMAGO.LANGUAGES.LINC.C-RUNTIME.STDIO"
  (:use "COMMON-LISP")
  (:export "printf" "fopen" "fread" "fwrite" "fseek" "ftell" "fclose"))

(defpackage "COM.INFORMATIMAGO.LANGUAGES.LINC.C-RUNTIME"
  (:use "COMMON-LISP")
  (:export "INITIALIZE")
  (:export "ENVIRONMENT" "VARIABLE" "ARRAY" "POINTER"))


(defpackage "COM.INFORMATIMAGO.LANGUAGES.LINC.C"
  (:nicknames "COM.INFORMATIMAGO.LANGUAGES.LINC.C++")
  (:use)
  (:export
   ;; pre-processor directives
   "include"
   "#ifdef"
   "#ifndef"
   "#if"
   "#elif"
   "#else"
   "#endif"

   ;; toplevel forms
   "declare-structure"
   "declare-union"
   "declare-type"
   "declare-enumeration"
   "declare-constant"
   "declare-variable"
   "declare-function"

   "define-constant"
   "define-variable"
   "define-function"
   "define-macro"

   ;; statements:
   "label" "goto"
   "switch" "case" "default"
   "block" "let" "let*"
   "if" "while" "do" "for"
   "break" "continue" "return"
   "asm"

   ;; anyary operator:
   "progn"
   ;; ternary operator:
   "?"
   ;; binary operators:
   "=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "|=" "^="
   "||" "&&" "|" "^" "&" "==" "!=" "<" ">" "<=" ">=" "<<" ">>"
   "+" "-" "*" "/" "%" ".*" "->*"
   "." "->" "aref"
   ;; unary operators:
   "++" "--" "!" "~"
   "&" "sizeof"
   "post++" "post--"
   ;; +, - and * can be used as unary operators too.
   "cast" ","

   ;; types
   ;; "typedef"
   "extern" "static" "thread-local" "auto" "register" "const"
   "restrict" "volatile" "atomic" "void" "char" "short" "int" "long"
   "float" "double" "signed" "unsigned" "bool" "complex" "struct" "enum"
   "union" "atomic" "pointer" "array" "function"
   "bit"
   "inline" "noreturn"

   ;; ;; c++ extensions (not implemented yet).
   ;; "external"
   ;; "using" "namespace" "typename" "template"
   "new" "new[]" "delete" "delete[]"
   "::"
   ;; "absolute-scope" "scope"
   ))

(defpackage "COM.INFORMATIMAGO.LANGUAGES.LINC.C-SEXP-LANGUAGE"
  (:use "COMMON-LISP")
  (:export "INCLUDE"

           "DECLARE-STRUCTURE"
           "DECLARE-UNION"
           "DECLARE-TYPE"
           "DECLARE-ENUMERATION"
           "DECLARE-CONSTANT"
           "DECLARE-VARIABLE"
           "DECLARE-FUNCTION"

           "DEFINE-CONSTANT"
           "DEFINE-VARIABLE"
           "DEFINE-FUNCTION"
           "DEFINE-MACRO"

           ;; Statemetns:
           "LABEL" "GOTO"
           "SWITCH" "CASE" "DEFAULT"
           "BLOCK" "LET" "LET*"
           "IF" "WHILE" "DO" "FOR"
           "BREAK" "CONTINUE" "RETURN"
           "ASM"

           ;;
           "EXPORT"

           ;; Anyary operator:
           "PROGN"
           ;; Ternary operator:
           "?"
           ;; Binary operators:
           "=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "|=" "^="
           "||" "&&" "|" "^" "&" "==" "!=" "<" ">" "<=" ">=" "<<" ">>"
           "+" "-" "*" "/" "%" ".*" "->*"
           "." "->" "AREF"
           ;; Unary operators:
           "++" "--" "!" "~"
           "&" "SIZEOF"
           "POST++" "POST--"
           ;; +, - and * can be used as unary operators too.

           "CAST" ","
           "BIT" "INLINE" "NORETURN"

           ;; C++ extensions (not implemented yet).
           "EXTERN"
           "USING" "NAMESPACE" "TYPENAME" "TEMPLATE"
           "NEW" "NEW[]" "DELETE" "DELETE[]"
           "::"
           "ABSOLUTE-SCOPE" "SCOPE")

  (:shadow "CASE" "BLOCK" "LET" "IF" "DO"
           "BREAK" "CONTINUE" "RETURN"

           "=" "/=" "<" ">" "<=" ">=" "+" "-" "*" "/" "++"
           "AREF"))

(defpackage "COM.INFORMATIMAGO.LANGUAGES.LINC"
  (:use "COMMON-LISP")
  (:shadow "TYPEP" "FUNCTIONP" "DECLARATION")
  (:use "SPLIT-SEQUENCE")
  (:use "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:shadow "INCLUDE-FILE")
  (:use "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SYMBOL"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")
  (:use "COM.INFORMATIMAGO.LANGUAGES.LINC.C")
  (:shadowing-import-from "COMMON-LISP"
                          "+" "-" "*" "/"
                          "<" ">" "<=" ">="
                          "=" "/="
                          "++")
  (:export "COMPILE-LINC-FILE"
           "LOAD-LINC-FILE"
           "TRANSLATE-LINC-FILE"))

(in-package  "COM.INFORMATIMAGO.LANGUAGES.LINC")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *c-package-name*  "COM.INFORMATIMAGO.LANGUAGES.LINC.C")
  (defvar *c-opening-brace* #\{)
  (defvar *c-closing-brace* #\})
  (defvar *c-progn*         '|progn|))

;;;; THE END ;;;;
