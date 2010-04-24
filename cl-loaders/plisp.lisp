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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2004 - 2004
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(DEFPACKAGE "COM.1729.PLISP"
  (:USE "COMMON-LISP")
  (:EXPORT "PS-INIT" "PS-COMPILE"))

(IN-PACKAGE "COM.1729.PLISP")


;;;  This builds an executable postscript compiler

(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;VARS")     ; has defvars
(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;MACROS")   ; useful macros
(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;TOP")      ; top level control
(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;COMPILE")  ; guts of the compilation process
(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;OUTPUT")   ; output routines
(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;DEFPS")    ; definitions of postscript functions
(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;ARGS")
(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;NAMES")
(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;FLOW")
(LOAD "PACKAGES:COM;1729;PLISP;COMPILER;UTIL")

;; Simple-minded iteration through the common-lisp directory -
;; may need changing depending of OS

;; (LET ((DIR "PACKAGES:COM;1729;PLISP;COMMON-LISP;"))
;;     (DOLIST (X '("BIND" "CONTROL" "FUNCTIONAL" "LISP-UTIL"
;; 		 "LOOP" "MVALUES" "NUMERIC" "FOR"))
;;       (LOAD (CONCATENATE 'STRING DIR X))))

;; This only needs to be done once.  ps-init is in defps

(PS-INIT)



;;;; plisp.lisp                       --                     --          ;;;;
