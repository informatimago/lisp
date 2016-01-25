;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the packages for the C11 parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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

(defpackage "COM.INFORMATIMAGO.LANGUAGES.C11.TOKENS"
  (:use)
  (:import-from "COMMON-LISP"
                "*" ">=" "/" "-" "++" "+" ">" "=" "<" "<=" "/=")
  (:import-from "COM.INFORMATIMAGO.LANGUAGES.CPP"
                "IDENTIFIER" "STRING-LITERAL"
                ;; "TYPEDEF-NAME" "FUNC-NAME" 
                ;; "I-CONSTANT" "F-CONSTANT" "ENUM-NAME"
                )
  (:export "IDENTIFIER" "TYPEDEF-NAME" "FUNC-NAME" "STRING-LITERAL"
           "I-CONSTANT" "F-CONSTANT" "ENUM-NAME" 
           "STAR"
           ;; -

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
           "%" "<" ">" "=" "^" "|" "?")
  
  (:documentation "This package exports the token-kinds of the C11 terminal symbols."))

(defpackage "COM.INFORMATIMAGO.LANGUAGES.C11.CONTEXT"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.LANGUAGES.CPP"
        "COM.INFORMATIMAGO.LANGUAGES.C11.TOKENS"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")
  (:export "CONTEXT" "CONTEXT-C-IDENTIFIERS-PACKAGE"
           "CONTEXT-TYPEDEFS" "CONTEXT-FUNCTIONS" "CONTEXT-ENUMERATION-CONSTANTS"
           "CONTEXT-DECLARATION-SPECIFIERS" "*CONTEXT*" "TYPEDEF-NAME-P"
           "FUNCTION-NAME-P" "ENUMERATION-CONSTANT-NAME-P"
           "IDENTIFIER-IN-TABLE-P" "ENTER-TYPEDEF" "ENTER-FUNCTION"
           "ENTER-ENUMERATION-CONSTANT"))

(defpackage "COM.INFORMATIMAGO.LANGUAGES.C11.SCANNER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
        "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP"
        "COM.INFORMATIMAGO.LANGUAGES.C11.TOKENS"
        "COM.INFORMATIMAGO.LANGUAGES.C11.CONTEXT"
        "COM.INFORMATIMAGO.LANGUAGES.CPP")
  (:export "C11-SCANNER"
           "UPGRADE-C11-TOKEN"))

(defpackage "COM.INFORMATIMAGO.LANGUAGES.YACC.PARSER"
  (:use "COMMON-LISP"
        "YACC"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
        "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP"
        "COM.INFORMATIMAGO.LANGUAGES.CPP"
        "COM.INFORMATIMAGO.TOOLS.READER-MACRO"
        "COM.INFORMATIMAGO.LANGUAGES.C11.TOKENS"
        "COM.INFORMATIMAGO.LANGUAGES.C11.CONTEXT")
  (:export "C11-SCANNER" "READ-YACC")
  (:documentation "
This package exports a function to read yacc grammars,
returning a yacc:defgrammar form.
"))


(defpackage "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER"
  (:use ;; "CL-STEPPER"
        "COMMON-LISP"
        "COM.INFORMATIMAGO.RDP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
        "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP"
        "COM.INFORMATIMAGO.LANGUAGES.CPP"
        "COM.INFORMATIMAGO.TOOLS.READER-MACRO"
        "COM.INFORMATIMAGO.LANGUAGES.C11.TOKENS"
        "COM.INFORMATIMAGO.LANGUAGES.C11.CONTEXT"
        "COM.INFORMATIMAGO.LANGUAGES.C11.SCANNER")
  (:export "C11-PARSER"))


(defpackage "COM.INFORMATIMAGO.LANGUAGES.C11.C"
  (:use)
  (:documentation "Default package where the C identifiers are interned."))

;;;; THE END ;;;;
