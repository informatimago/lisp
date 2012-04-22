;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               plisp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loader for the plisp package.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-10-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2004 - 2012
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
;;;;****************************************************************************

(defpackage "COM.1729.PLISP"
  (:use "COMMON-LISP")
  (:export "PS-INIT" "PS-COMPILE"))

(in-package "COM.1729.PLISP")


;;;  This builds an executable postscript compiler

(load "PACKAGES:COM;1729;PLISP;COMPILER;VARS")     ; has defvars
(load "PACKAGES:COM;1729;PLISP;COMPILER;MACROS")   ; useful macros
(load "PACKAGES:COM;1729;PLISP;COMPILER;TOP")      ; top level control
(load "PACKAGES:COM;1729;PLISP;COMPILER;COMPILE")  ; guts of the compilation process
(load "PACKAGES:COM;1729;PLISP;COMPILER;OUTPUT")   ; output routines
(load "PACKAGES:COM;1729;PLISP;COMPILER;DEFPS")    ; definitions of postscript functions
(load "PACKAGES:COM;1729;PLISP;COMPILER;ARGS")
(load "PACKAGES:COM;1729;PLISP;COMPILER;NAMES")
(load "PACKAGES:COM;1729;PLISP;COMPILER;FLOW")
(load "PACKAGES:COM;1729;PLISP;COMPILER;UTIL")

;; Simple-minded iteration through the common-lisp directory -
;; may need changing depending of OS

;; (LET ((DIR "PACKAGES:COM;1729;PLISP;COMMON-LISP;"))
;;     (DOLIST (X '("BIND" "CONTROL" "FUNCTIONAL" "LISP-UTIL"
;; 		 "LOOP" "MVALUES" "NUMERIC" "FOR"))
;;       (LOAD (CONCATENATE 'STRING DIR X))))

;; This only needs to be done once.  ps-init is in defps

(ps-init)



;;;; plisp.lisp                       --                     --          ;;;;
