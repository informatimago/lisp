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
;;;;    Copyright Pascal Bourguignon 2006 - 2012
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
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



(assert (equal (com.informatimago.rdp.example:parse-example
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
               '(block (((ident "abc" 11) (integer "123" 17)) ((ident "pi" 32) (real "3.141592e+0" 35))) ((ident "a" 57) (ident "b" 59) (ident "c" 61)) ((procedure (ident "gcd" 79) (block nil nil nil ((while (("#" "#" 112) (+ ((ident "a" 110))) (+ ((ident "b" 114)))) ((if (("<" "<" 151) (+ ((ident "a" 150))) (+ ((ident "b" 152)))) (setf (ident "b" 159) (+ ((ident "b" 162)) (("-" "-" 163) ((ident "a" 164)))))) (if ((">" ">" 186) (+ ((ident "a" 185))) (+ ((ident "b" 187)))) (setf (ident "a" 194) (+ ((ident "a" 197)) (("-" "-" 198) ((ident "b" 199)))))))))))) ((setf (ident "a" 235) (+ ((integer "42" 238)))) (setf (ident "b" 246) (+ ((real "30.0" 249)))) (call (ident "gcd" 264))))))



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


(assert (equal (com.informatimago.rdp.example-without-action:parse-example-without-action
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
               '(program (block (("const" "const" 5) (ident "abc" 11) ("=" "=" 15) (number (integer "123" 17)) ((("," "," 20) (ident "pi" 32) ("=" "=" 34) (number (real "3.141592e+0" 35)))) (";" ";" 46)) (("var" "var" 53) (ident "a" 57) ((("," "," 58) (ident "b" 59)) (("," "," 60) (ident "c" 61))) (";" ";" 62)) ((("procedure" "procedure" 69) (ident "gcd" 79) (";" ";" 82) (block nil nil nil (statement (("begin" "begin" 89) (statement (("while" "while" 104) (condition ((expression nil (term (factor (ident "a" 110)) nil) nil) ("#" "#" 112) (expression nil (term (factor (ident "b" 114)) nil) nil))) ("do" "do" 116) (statement (("begin" "begin" 128) (statement (("if" "if" 147) (condition ((expression nil (term (factor (ident "a" 150)) nil) nil) ("<" "<" 151) (expression nil (term (factor (ident "b" 152)) nil) nil))) ("then" "then" 154) (statement ((ident "b" 159) (":=" ":=" 160) (expression nil (term (factor (ident "b" 162)) nil) ((("-" "-" 163) (term (factor (ident "a" 164)) nil)))))))) (((";" ";" 166) (statement (("if" "if" 182) (condition ((expression nil (term (factor (ident "a" 185)) nil) nil) (">" ">" 186) (expression nil (term (factor (ident "b" 187)) nil) nil))) ("then" "then" 189) (statement ((ident "a" 194) (":=" ":=" 195) (expression nil (term (factor (ident "a" 197)) nil) ((("-" "-" 198) (term (factor (ident "b" 199)) nil)))))))))) ("end" "end" 210))))) nil ("end" "end" 219)))) (";" ";" 222))) (statement (("begin" "begin" 224) (statement ((ident "a" 235) (":=" ":=" 236) (expression nil (term (factor (number (integer "42" 238))) nil) nil))) (((";" ";" 240) (statement ((ident "b" 246) (":=" ":=" 247) (expression nil (term (factor (number (real "30.0" 249))) nil) nil)))) ((";" ";" 253) (statement (("call" "call" 259) (ident "gcd" 264))))) ("end" "end" 268)))) ("." "." 271))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
