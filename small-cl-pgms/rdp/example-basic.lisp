;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               example-basic.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An example grammar for the recusive descent parser generator.
;;;;    The actions are written in a pseudo basic
;;;;    to generate a pseudo basic parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-07-19 <PJB> Updated regexps, now we use extended regexps.
;;;;    2006-09-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2011
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
;;;;**************************************************************************


(defpackage "COM.INFORMATIMAGO.RDP.BASIC.EXAMPLE"
  (:use "COMMON-LISP" "COM.INFORMATIMAGO.RDP")
  (:export "PARSE-EXAMPLE"))
(in-package "COM.INFORMATIMAGO.RDP.BASIC.EXAMPLE")


(defgrammar example
    :target-language :basic
    :terminals ((ident   "[A-Za-z][A-Za-z0-9]*")
                ;; real must come first to match the longest first.
                (real    "[-+]?[0-9]+.[0-9]+([Ee][-+]?[0-9]+)?")
                (integer "[-+]?[0-9]+"))
    :start program
    :rules ((--> factor
                 (alt ident
                      number
                      (seq "(" expression ")" :action "RES=A2"))
                 :action "RES=A1")
            (--> number  (alt integer real) :action "RES=A1")
            (--> term
                 factor (rep (alt "*" "/") factor)
                 :action "NCAR=A1:NCDR=A2:CALL CONS")
            (--> expression
                 (opt (alt "+" "-"))
                 term
                 (rep (alt "+" "-") term
                      :action
                      "NCAR=A2:NCDR=NIL:CALL CONS"
                      "NCAR=A1:NCDR=RES:CALL CONS")
                 :action
                 "IF A1<>0 THEN"
                 "  NCAR=A2:NCDR=NIL:CALL CONS"
                 "  NCAR=A1:NCDR=RES:CALL CONS"
                 "  NCAR=RES"
                 "ELSE"
                 "  NCAR=A2"
                 "ENDIF"
                 "NCDR=A3:CALL CONS"
                 "TMP=RES"
                 "NSTRING$=\"+\":CALL MKSTR:NCAR=RES:NCDR=TMP:CALL CONS")
            (--> condition
                 (alt (seq "odd" expression
                           :action
                           "NCAR=A2:NCDR=NIL:CALL CONS:TMP=RES"
                           "NSTRING$=\"ODD\":CALL MKSTR"
                           "NCAR=RES:NCDR=TMP:CALL CONS")
                      (seq expression
                           (alt "=" "#" "<" "<=" ">" ">=")
                           expression
                           :action
                           "NCAR=A3:NCDR=NIL:CALL CONS"
                           "NCAR=A1:NCDR=RES:CALL CONS"
                           "NCAR=A2:NCDR=RES:CALL CONS"))
                 :action "RES=A1")
            (--> statement
                 (opt (alt (seq ident ":=" expression
                                :action
                                "NCAR=A3:NCDR=NIL:CALL CONS"
                                "NCAR=A1:NCDR=RES:CALL CONS"
                                "TMP=RES:NSTRING$=\"LET\":CALL MKSTR"
                                "NCAR=RES:NCDR=TMP:CALL CONS")
                           (seq "call" ident
                                :action
                                "NCAR=A2:NCDR=NIL:CALL CONS"
                                "TMP=RES:NSTRING$=\"CALL\":CALL MKSTR"
                                "NCAR=RES:NCDR=TMP:CALL CONS")
                           (seq "begin" statement
                                (rep ";" statement :action "RES=A2")
                                "end"
                                :action "NCAR=A2:NCDR=A3:CALL CONS")
                           (seq "if" condition "then" statement
                                :action
                                "NCAR=A4:NCDR=NIL:CALL CONS"
                                "NCAR=A2:NCDR=RES:CALL CONS"
                                "TMP=RES:NSTRING$=\"IF\":CALL MKSTR"
                                "NCAR=RES:NCDR=TMP:CALL CONS")
                           (seq "while" condition "do" statement
                                :action
                                "NCAR=A4:NCDR=NIL:CALL CONS"
                                "NCAR=A2:NCDR=RES:CALL CONS"
                                "TMP=RES:NSTRING$=\"WHILE\":CALL MKSTR"
                                "NCAR=RES:NCDR=TMP:CALL CONS")))
                 :action "RES=A1")
            (--> block
                 (opt "const" ident "=" number
                      (rep "," ident "=" number
                           :action
                           "NCAR=A4:NCDR=NIL:CALL CONS"
                           "NCAR=A2:NCDR=RES:CALL CONS")
                      ";"
                      :action
                      "NCAR=A4:NCDR=NIL:CALL CONS"
                      "NCAR=A2:NCDR=RES:CALL CONS"
                      "NCAR=RES:NCDR=A5:CALL CONS")
                 (opt "var" ident
                      (rep "," ident :action "RES=A2")
                      ";"
                      :action
                      "NCAR=A3:NCDR=NIL:CALL CONS"
                      "NCAR=A2:NCDR=RES:CALL CONS")
                 (rep "procedure" ident ";" block ";"
                      :action
                      "NCAR=A4:NCDR=NIL:CALL CONS"
                      "NCAR=A2:NCDR=RES:CALL CONS"
                      "TMP=RES:NSTRING$=\"PROCEDURE\":CALL MKSTR"
                      "NCAR=RES:NCDR=TMP:CALL CONS")
                 statement
                 :action
                 "NCAR=A4:NCDR=NIL:CALL CONS"
                 "NCAR=A3:NCDR=RES:CALL CONS"
                 "NCAR=A2:NCDR=RES:CALL CONS"
                 "NCAR=A1:NCDR=RES:CALL CONS"
                 "TMP=RES:NSTRING$=\"BLOCK\":CALL MKSTR"
                 "NCAR=RES:NCDR=TMP:CALL CONS")
            (--> program
                 block "." :action "RES=A1")))


;;;; THE END ;;;;

