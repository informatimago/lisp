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
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-07-19 <PJB> Updated regexps, now we use extended regexps.
;;;;    2011-07-19 <PJB> Added defpackage forms.  Added parsing example sources.
;;;;    2006-09-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2006 - 2015
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
  (:use
   "COMMON-LISP"
   ;;"CL-STEPPER"
   "COM.INFORMATIMAGO.RDP")
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



(assert (equal
         (com.informatimago.rdp.example:parse-example
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


         '(block (((ident "abc" 14) (integer "123" 20)) ((ident "pi" 13) (real "3.141592e+0" 25)))
           ((ident "a" 10) (ident "b" 12) (ident "c" 14))
           ((procedure (ident "gcd" 18)
             (block nil
               nil
               nil
               ((((while ((#1="#" #1# 18) (+ ((ident "a" 16))) (+ ((ident "b" 20))))
                    ((((if ((#2="<" #2# 19) (+ ((ident "a" 18))) (+ ((ident "b" 20))))
                           ((setf (ident "b" 27) (+ ((ident "b" 30)) ((#3="-" #3# 31) ((ident "a" 32))))))))
                      ((if ((#4=">" #4# 19) (+ ((ident "a" 18))) (+ ((ident "b" 20))))
                           ((setf (ident "a" 27) (+ ((ident "a" 30)) ((#5="-" #5# 31) ((ident "b" 32)))))))))))))))))
           ((((setf (ident "a" 6) (+ ((integer "42" 10))))) ((setf (ident "b" 6) (+ ((real "30.0" 12)))))
             ((call (ident "gcd" 13))))))))



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
               
               '(program
                 (block
                     ((#1="const" #1# 10) (ident "abc" 14) (#2="=" #2# 16) (number (integer "123" 20))
                      (((#3="," #3# 21) (ident "pi" 13) (#4="=" #4# 14) (number (real "3.141592e+0" 25)))) (#5=";" #5# 26))
                   ((#6="var" #6# 8) (ident "a" 10) (((#7="," #7# 11) (ident "b" 12)) ((#8="," #8# 13) (ident "c" 14)))
                    (#9=";" #9# 15))
                   (((#10="procedure" #10# 14) (ident "gcd" 18) (#11=";" #11# 19)
                     (block nil
                       nil
                       nil
                       (statement
                        (((#12="begin" #12# 10)
                          (statement
                           (((#13="while" #13# 14)
                             (condition
                              ((expression nil (term (factor (ident "a" 16)) nil) nil) (#14="#" #14# 18)
                               (expression nil (term (factor (ident "b" 20)) nil) nil)))
                             (#15="do" #15# 23)
                             (statement
                              (((#16="begin" #16# 14)
                                (statement
                                 (((#17="if" #17# 16)
                                   (condition
                                    ((expression nil (term (factor (ident "a" 18)) nil) nil) (#18="<" #18# 19)
                                     (expression nil (term (factor (ident "b" 20)) nil) nil)))
                                   (#19="then" #19# 25)
                                   (statement
                                    (((ident "b" 27) (#20=":=" #20# 29)
                                      (expression nil (term (factor (ident "b" 30)) nil)
                                                  (((#21="-" #21# 31) (term (factor (ident "a" 32)) nil))))))))))
                                (((#22=";" #22# 34)
                                  (statement
                                   (((#23="if" #23# 16)
                                     (condition
                                      ((expression nil (term (factor (ident "a" 18)) nil) nil) (#24=">" #24# 19)
                                       (expression nil (term (factor (ident "b" 20)) nil) nil)))
                                     (#25="then" #25# 25)
                                     (statement
                                      (((ident "a" 27) (#26=":=" #26# 29)
                                        (expression nil (term (factor (ident "a" 30)) nil)
                                                    (((#27="-" #27# 31) (term (factor (ident "b" 32)) nil))))))))))))
                                (#28="end" #28# 12)))))))
                          nil (#29="end" #29# 8)))))
                     (#30=";" #30# 9)))
                   (statement
                    (((#31="begin" #31# 6)
                      (statement
                       (((ident "a" 6) (#32=":=" #32# 8)
                         (expression nil (term (factor (number (integer "42" 10))) nil) nil))))
                      (((#33=";" #33# 11)
                        (statement
                         (((ident "b" 6) (#34=":=" #34# 8)
                           (expression nil (term (factor (number (real "30.0" 12))) nil) nil)))))
                       ((#35=";" #35# 13) (statement (((#36="call" #36# 9) (ident "gcd" 13))))))
                      (#37="end" #37# 4)))))
                 (#38="." #38# 5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
