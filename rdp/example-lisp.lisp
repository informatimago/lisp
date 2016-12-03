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
;;;;    Copyright Pascal J. Bourguignon 2006 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.RDP.EXAMPLE"
  (:use
   "COMMON-LISP"
   ;;"CL-STEPPER"
   "COM.INFORMATIMAGO.RDP"
   "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST")
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


(define-test test/rdp-example-lisp ()
  (check equal (parse-example "
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
         '(block (((ident "abc" 14) (integer "123" 20))
                  ((ident "pi" 13) (real "3.141592e+0" 25)))
           ((ident "a" 10) (ident "b" 12) (ident "c" 14))
           ((procedure (ident "gcd" 18)
             (block nil
               nil
               nil
               ((((while ((\# "#" 18) (+ ((ident "a" 16))) (+ ((ident "b" 20))))
                    ((((if ((< "<" 19) (+ ((ident "a" 18))) (+ ((ident "b" 20))))
                           ((setf (ident "b" 27)
                                  (+ ((ident "b" 30)) ((- "-" 31) ((ident "a" 32))))))))
                      ((if ((> ">" 19) (+ ((ident "a" 18))) (+ ((ident "b" 20))))
                           ((setf (ident "a" 27)
                                  (+ ((ident "a" 30))
                                     ((- "-" 31) ((ident "b" 32)))))))))))))))))
           ((((setf (ident "a" 6) (+ ((integer "42" 10)))))
             ((setf (ident "b" 6) (+ ((real "30.0" 12))))) ((call (ident "gcd" 13))))))
         )
  :success)



(defpackage "COM.INFORMATIMAGO.RDP.EXAMPLE-WITHOUT-ACTION"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.RDP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST")
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

(define-test test/rdp-example-lisp-without-action ()
  (check equal (parse-example-without-action "
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
           (block ((|const| "const" 10) (ident "abc" 14) (= "=" 16)
                   (number (integer "123" 20))
                   (((\, "," 21) (ident "pi" 13) (= "=" 14)
                     (number (real "3.141592e+0" 25))))
                   (\; ";" 26))
             ((|var| "var" 8) (ident "a" 10)
              (((\, "," 11) (ident "b" 12)) ((\, "," 13) (ident "c" 14))) (\; ";" 15))
             (((|procedure| "procedure" 14) (ident "gcd" 18) (\; ";" 19)
               (block nil
                 nil
                 nil
                 (statement
                  (((|begin| "begin" 10)
                    (statement
                     (((|while| "while" 14)
                       (condition
                        ((expression nil (term (factor (ident "a" 16)) nil) nil)
                         (\# "#" 18)
                         (expression nil (term (factor (ident "b" 20)) nil) nil)))
                       (|do| "do" 23)
                       (statement
                        (((|begin| "begin" 14)
                          (statement
                           (((|if| "if" 16)
                             (condition
                              ((expression nil (term (factor (ident "a" 18)) nil) nil)
                               (< "<" 19)
                               (expression nil (term (factor (ident "b" 20)) nil) nil)))
                             (|then| "then" 25)
                             (statement
                              (((ident "b" 27) (\:= ":=" 29)
                                               (expression nil (term (factor (ident "b" 30)) nil)
                                                           (((- "-" 31) (term (factor (ident "a" 32)) nil))))))))))
                          (((\; ";" 34)
                            (statement
                             (((|if| "if" 16)
                               (condition
                                ((expression nil (term (factor (ident "a" 18)) nil) nil)
                                 (> ">" 19)
                                 (expression nil (term (factor (ident "b" 20)) nil)
                                             nil)))
                               (|then| "then" 25)
                               (statement
                                (((ident "a" 27) (\:= ":=" 29)
                                                 (expression nil (term (factor (ident "a" 30)) nil)
                                                             (((- "-" 31)
                                                               (term (factor (ident "b" 32)) nil))))))))))))
                          (|end| "end" 12)))))))
                    nil (|end| "end" 8)))))
               (\; ";" 9)))
             (statement
              (((|begin| "begin" 6)
                (statement
                 (((ident "a" 6) (\:= ":=" 8)
                                 (expression nil (term (factor (number (integer "42" 10))) nil) nil))))
                (((\; ";" 11)
                  (statement
                   (((ident "b" 6) (\:= ":=" 8)
                                   (expression nil (term (factor (number (real "30.0" 12))) nil)
                                               nil)))))
                 ((\; ";" 13) (statement (((|call| "call" 9) (ident "gcd" 13))))))
                (|end| "end" 4)))))
           (|.| "." 5)))
  :success)


(in-package "COM.INFORMATIMAGO.RDP.EXAMPLE")

(defun test/all ()
  (com.informatimago.rdp.example::test/rdp-example-lisp)
  (com.informatimago.rdp.example-without-action::test/rdp-example-lisp-without-action))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
