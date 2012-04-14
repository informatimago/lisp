;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               example-lisp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An example grammar for the recusive descent parser generator.
;;;;    The actions are written in Lisp, to generate a lisp parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-07-19 <PJB> Updated regexps, now we use extended regexps.
;;;;    2011-07-19 <PJB> Added defpackage forms.  Added parsing example sources.
;;;;    2006-09-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
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


(defpackage "COM.INFORMATIMAGO.RDP.EXAMPLE"
  (:use "COMMON-LISP" "COM.INFORMATIMAGO.RDP")
  (:export "PARSE-EXAMPLE"))
(in-package "COM.INFORMATIMAGO.RDP.EXAMPLE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Language 
;;; taken from: http://en.wikipedia.org/wiki/Recursive_descent_parser
;;;

(defgrammar example
    :terminals ((ident   "[A-Za-z][A-Za-z0-9]*")
                ;; real must come first to match the longest first.
                (real    "[-+]?[0-9]+\\.[0-9]+([Ee][-+]?[0-9]+)?")
                (integer "[-+]?[0-9]+"))
    :start program
    :rules ((--> factor
                 (alt ident
                      number
                      (seq "(" expression ")" :action $2))
                 :action $1)
            (--> number  (alt integer real) :action $1)
            (--> term
                 factor (rep (alt "*" "/") factor)
                 :action `(,$1 . ,$2))
            (--> expression
                 (opt (alt "+" "-"))
                 term
                 (rep (alt "+" "-") term :action `(,$1 ,$2))
                 :action `(+ ,(if $1 `(,$1 ,$2) $2)  . ,$3))
            (--> condition
                 (alt (seq "odd" expression
                           :action `(oddp ,$2))
                      (seq expression
                           (alt "=" "#" "<" "<=" ">" ">=")
                           expression
                           :action `(,$2 ,$1 ,$3)))
                 :action $1)
            (--> statement
                 (opt (alt (seq ident ":=" expression
                                :action `(setf ,$1 ,$3))
                           (seq "call" ident
                                :action `(call ,$2))
                           (seq "begin" statement
                                (rep ";" statement
                                     :action $2)
                                "end"
                                :action `(,$2 . ,$3))
                           (seq "if" condition "then" statement
                                :action `(if ,$2 ,$4))
                           (seq "while" condition "do" statement
                                :action `(while ,$2 ,$4))))
                 :action $1)
            (--> block
                 (opt "const" ident "=" number
                      (rep "," ident "=" number
                           :action `(,$2 ,$4))
                      ";"
                      :action `((,$2 ,$4) . ,$5))
                 (opt "var" ident
                      (rep "," ident :action $2)
                      ";"
                      :action `(,$2 . ,$3))
                 (rep "procedure" ident ";" block ";"
                      :action `(procedure ,$2 ,$4))
                 statement
                 :action `(block ,$1 ,$2 ,$3 ,$4))
            (--> program
                 block "." :action $1)))



(assert (equal (COM.INFORMATIMAGO.RDP.EXAMPLE:PARSE-EXAMPLE
                "
    const abc = 123,
          pi=3.141592e+0; 
    var a,b,c; 
    procedure gcd; 
    begin 
        while a # b do 
        begin
             if a<b then b:=b-a ; 
             if a>b then a:=a-b 
        end 
    end;
begin 
    a:=42;
    b:=30.0;
    call gcd
end.")
               '(BLOCK (((IDENT "abc" 11) (INTEGER "123" 17)) ((IDENT "pi" 32) (REAL "3.141592e+0" 35))) ((IDENT "a" 57) (IDENT "b" 59) (IDENT "c" 61)) ((PROCEDURE (IDENT "gcd" 79) (BLOCK NIL NIL NIL ((WHILE (("#" "#" 112) (+ ((IDENT "a" 110))) (+ ((IDENT "b" 114)))) ((IF (("<" "<" 151) (+ ((IDENT "a" 150))) (+ ((IDENT "b" 152)))) (SETF (IDENT "b" 159) (+ ((IDENT "b" 162)) (("-" "-" 163) ((IDENT "a" 164)))))) (IF ((">" ">" 186) (+ ((IDENT "a" 185))) (+ ((IDENT "b" 187)))) (SETF (IDENT "a" 194) (+ ((IDENT "a" 197)) (("-" "-" 198) ((IDENT "b" 199)))))))))))) ((SETF (IDENT "a" 235) (+ ((INTEGER "42" 238)))) (SETF (IDENT "b" 246) (+ ((REAL "30.0" 249)))) (CALL (IDENT "gcd" 264))))))



(defpackage "COM.INFORMATIMAGO.RDP.EXAMPLE-WITHOUT-ACTION"
  (:use "COMMON-LISP" "COM.INFORMATIMAGO.RDP")
  (:export "PARSE-EXAMPLE-WITHOUT-ACTION"))
(in-package "COM.INFORMATIMAGO.RDP.EXAMPLE-WITHOUT-ACTION")

(defgrammar example-without-action
    :terminals ((ident   "[A-Za-z][A-Za-z0-9]*")
                ;; real must come first to match the longest first.
                (real    "[-+]?[0-9]+\\.[0-9]+([Ee][-+]?[0-9]+)?")
                (integer "[-+]?[0-9]+"))
    :start program
    :rules ((--> factor
                 (alt ident
                      number
                      (seq "(" expression ")")))
            (--> number  (alt integer real))
            (--> term
                 factor (rep (alt "*" "/") factor))
            (--> expression
                 (opt (alt "+" "-"))
                 term
                 (rep (alt "+" "-") term))
            (--> condition
                 (alt (seq "odd" expression)
                      (seq expression
                           (alt "=" "#" "<" "<=" ">" ">=")
                           expression)))
            (--> statement
                 (opt (alt (seq ident ":=" expression)
                           (seq "call" ident)
                           (seq "begin" statement
                                (rep ";" statement)
                                "end")
                           (seq "if" condition "then" statement)
                           (seq "while" condition "do" statement))))
            (--> block
                 (opt "const" ident "=" number
                      (rep "," ident "=" number) ";")
                 (opt "var" ident (rep "," ident) ";")
                 (rep "procedure" ident ";" block ";")
                 statement)
            (--> program
                 block ".")))


(assert (equal (COM.INFORMATIMAGO.RDP.EXAMPLE-WITHOUT-ACTION:PARSE-EXAMPLE-WITHOUT-ACTION
                "
    const abc = 123,
          pi=3.141592e+0; 
    var a,b,c; 
    procedure gcd; 
    begin 
        while a # b do 
        begin
             if a<b then b:=b-a ; 
             if a>b then a:=a-b 
        end 
    end;
begin 
    a:=42;
    b:=30.0;
    call gcd
end.")
               '(PROGRAM (BLOCK (("const" "const" 5) (IDENT "abc" 11) ("=" "=" 15) (NUMBER (INTEGER "123" 17)) ((("," "," 20) (IDENT "pi" 32) ("=" "=" 34) (NUMBER (REAL "3.141592e+0" 35)))) (";" ";" 46)) (("var" "var" 53) (IDENT "a" 57) ((("," "," 58) (IDENT "b" 59)) (("," "," 60) (IDENT "c" 61))) (";" ";" 62)) ((("procedure" "procedure" 69) (IDENT "gcd" 79) (";" ";" 82) (BLOCK NIL NIL NIL (STATEMENT (("begin" "begin" 89) (STATEMENT (("while" "while" 104) (CONDITION ((EXPRESSION NIL (TERM (FACTOR (IDENT "a" 110)) NIL) NIL) ("#" "#" 112) (EXPRESSION NIL (TERM (FACTOR (IDENT "b" 114)) NIL) NIL))) ("do" "do" 116) (STATEMENT (("begin" "begin" 128) (STATEMENT (("if" "if" 147) (CONDITION ((EXPRESSION NIL (TERM (FACTOR (IDENT "a" 150)) NIL) NIL) ("<" "<" 151) (EXPRESSION NIL (TERM (FACTOR (IDENT "b" 152)) NIL) NIL))) ("then" "then" 154) (STATEMENT ((IDENT "b" 159) (":=" ":=" 160) (EXPRESSION NIL (TERM (FACTOR (IDENT "b" 162)) NIL) ((("-" "-" 163) (TERM (FACTOR (IDENT "a" 164)) NIL)))))))) (((";" ";" 166) (STATEMENT (("if" "if" 182) (CONDITION ((EXPRESSION NIL (TERM (FACTOR (IDENT "a" 185)) NIL) NIL) (">" ">" 186) (EXPRESSION NIL (TERM (FACTOR (IDENT "b" 187)) NIL) NIL))) ("then" "then" 189) (STATEMENT ((IDENT "a" 194) (":=" ":=" 195) (EXPRESSION NIL (TERM (FACTOR (IDENT "a" 197)) NIL) ((("-" "-" 198) (TERM (FACTOR (IDENT "b" 199)) NIL)))))))))) ("end" "end" 210))))) NIL ("end" "end" 219)))) (";" ";" 222))) (STATEMENT (("begin" "begin" 224) (STATEMENT ((IDENT "a" 235) (":=" ":=" 236) (EXPRESSION NIL (TERM (FACTOR (NUMBER (INTEGER "42" 238))) NIL) NIL))) (((";" ";" 240) (STATEMENT ((IDENT "b" 246) (":=" ":=" 247) (EXPRESSION NIL (TERM (FACTOR (NUMBER (REAL "30.0" 249))) NIL) NIL)))) ((";" ";" 253) (STATEMENT (("call" "call" 259) (IDENT "gcd" 264))))) ("end" "end" 268)))) ("." "." 271))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
