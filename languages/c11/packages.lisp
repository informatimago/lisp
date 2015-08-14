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

(defpackage "COM.INFORMATIMAGO.LANGUAGES.C11.TOKENS"
  (:use)
  (:import-from "COMMON-LISP"
                "*" ">=" "/" "-" "++" "+" ">" "=" "<" "<=" "/=")
  (:export "identifier" "typedef_name" "func_name" "string_literal"
           "i_constant" "f_constant" "enum_name" "alignas" "alignof"
           "atomic" "generic" "noreturn" "static_assert"
           "thread_local" "case" "default" "if" "else" "switch"
           "while" "do" "for" "goto" "continue" "break" "return"
           "struct" "union" "enum" "..." "complex" "imaginary" "bool"
           "char" "short" "int" "long" "signed" "unsigned" "float"
           "double" "void" "const" "restrict" "volatile" "typedef"
           "extern" "static" "auto" "register" "inline" "sizeof" "^="
           "|=" "-=" "<<=" ">>=" "&=" "&&" "||" "*=" "/=" "%=" "+="
           "->" "++" "--" "<<" ">>" "<=" ">=" "==" "!=" "(" ")" ","
           ":" ";" "." "[" "]" "{" "}" "&" "*" "/" "+" "-" "~" "!" "%"
           "<" ">" "=" "^" "|" "?" "STAR")
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
        "COM.INFORMATIMAGO.LANGUAGES.C11.CONTEXT")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP"
                          "SPLIT-STRING")
  (:export "C11-SCANNER"
           "COMPUTE-TOKEN-KIND"))

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
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP"
                          "SPLIT-STRING")
  (:export "C11-SCANNER" "READ-YACC")
  (:documentation "
This package exports a function to read yacc grammars,
returning a yacc:defgrammar form.
"))


(defpackage "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER"
  (:use "CL-STEPPER"; "COMMON-LISP"
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
        "COM.INFORMATIMAGO.LANGUAGES.C11.CONTEXT")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP"
                          "SPLIT-STRING")
  (:export "C11-PARSER"))


(defpackage "COM.INFORMATIMAGO.LANGUAGES.C11.C"
  (:use)
  (:documentation "Default package where the C identifiers are interned."))

;;;; THE END ;;;;
