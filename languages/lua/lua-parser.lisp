;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               lua-parser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the LUA 5.2 Parser.
;;;;    http://www.lua.org/manual/5.2/manual.html#9
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-15 <PJB> Created.
;;;;BUGS
;;;;
;;;;    This is unfinished.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LUA.PARSER")


(defgrammar lua
    :trace nil
    :scanner lua-scanner
    :terminals ((number "[0-9]+")
                (string "\"[^\"]*\"")
                (name  "[_A-Za-z][_A-Za-z0-9]*"))
    :start chunk
    :rules (

            (--> chunk
                 block)

            (--> block
                 (seq (rep stat) (opt retstat)))

            (--> stat
                 (alt ";"
                      (seq varlist "=" explist)
                      (seq functioncall) 
                      (seq label)
                      (seq "break")
                      (seq "goto" Name)
                      (seq "do" block "end")
                      (seq "while" exp "do" block "end")
                      (seq "repeat" block "until" exp)
                      (seq "if" exp "then" block (rep "elseif" exp "then" block) (opt "else" block) "end")

                      (seq "for" Name (alt for-name-=
                                           for-namelist))
                      
                      (seq "function" funcname funcbody)
                      (seq "local"
                           (alt (seq "function" Name funcbody)
                                (seq namelist (opt "=" explist))))))
            
            (--> for-name-=
                 (seq "=" exp "," exp (opt "," exp) "do" block "end"))
            (--> for-namelist
                 (seq namelist-cont "in" explist "do" block "end"))

            (--> retstat
                 (seq "return" (opt explist) (opt ";")))

            (--> label
                 (seq "::" Name "::"))
            
            (--> funcname
                 (seq Name (rep (seq "." Name)) (opt (seq ":" Name))))

            (--> varlist
                 (seq var (rep (seq "," var))))

            (--> namelist
                 (seq Name namelist-cont))

            (--> namelist-cont
                 (rep (seq "," Name)))

            (--> explist
                 (seq exp (rep (seq "," exp))))

            #-(and)
            (--> exp
                 (alt "nil" 
                      "false" 
                      "true" 
                      Number 
                      String 
                      "..." 
                      functiondef
                      prefixexp 
                      tableconstructor 
                      (seq exp binop exp) 
                      (seq unop exp)))

            (--> exp disjonction)
            (--> disjonction
                 (seq conjonction (rep "or" conjonction)))
            (--> conjonction
                 (seq comparaison (rep "and" comparaison)))
            (--> comparaison
                 (seq concatenation (rep (alt "<" ">" "<=" ">=" "~=" "==") concatenation)))
            (--> concatenation
                 (seq summation (rep ".." summation)))
            (--> summation
                 (seq term (rep (alt "+" "-") term)))
            (--> term
                 (seq factor (rep (alt "*" "/" "%") factor)))
            (--> factor
                 (seq (opt (alt "not" "#" "-")) exponentiation))
            (--> exponentiation
                 (seq simple (rep "^" simple)))
            (--> simple
                 (alt
                  "nil"
                  "false"
                  "true"
                  Number
                  String
                  "…"
                  functiondef
                  prefixexp
                  tableconstructor))



            #-(and)
            (--> prefixexp
                 (alt var
                      functioncall
                      "(" exp ")"))
            #-(and)
            (--> var
                 (alt Name
                      (seq prefixexp "[" exp "]")
                      (seq prefixexp "." Name)) )

            #-(and)
            (--> functioncall
                 (alt (seq prefixexp args)
                      (seq prefixexp ":" Name args)))

            #-(and)
            (--> callpart
                 (seq (opt (seq ":" name)) args))

            (--> indexpart
                 (alt (seq "[" exp "]")
                      (seq "." Name)))

            #-(and)
            (--> prefixexp
                 (alt
                  (seq (alt Name
                            (seq "(" exp ")"))
                       (rep (alt callpart
                                 indexpart)))))
            ;; TODO:
            (--> prefixexp
                 "prefix")

            ;; TODO:
            (--> var
                 Name)
            
            ;; TODO:
            (--> functioncall
                 (seq "funcall" Name "(" exp ")"))
            






            (--> args
                 (alt (seq "(" (opt explist) ")" )
                      tableconstructor 
                      String) )

            (--> functiondef
                 "function" funcbody)

            (--> funcbody
                 (seq "(" (opt parlist) ")" block "end"))

            (--> parlist
                 (alt (seq namelist (opt "," "...")) 
                      "..."))

            (--> tableconstructor
                 (seq "{" (opt fieldlist) "}"))

            (--> fieldlist
                 (seq field (rep fieldsep field) (opt fieldsep)))

            (--> field
                 (alt (seq "[" exp "]" "=" exp )
                      (seq Name "=" exp)
                      exp))

            (--> fieldsep
                 (alt "," 
                      ";"))
            
            #-(and)
            (--> binop
                 (alt "+" 
                      "-" 
                      "*" 
                      "/" 
                      "^" 
                      "%" 
                      ".." 
                      
                      "<" 
                      "<=" 
                      ">" 
                      ">=" 
                      "==" 
                      "~=" 
                      
                      "and" 
                      "or"))

            #-(and)
            (--> unop
                 (alt "-" 
                      "not" 
                      "#"))))



;; Operator precedence in Lua follows the table below, from lower to
;; higher priority: 
;;
;;      or
;;      and
;;      <     >     <=    >=    ~=    ==
;;      ..
;;      +     -
;;      *     /     %
;;      not   #     - (unary)
;;      ^
;; 
;; As usual, you can use parentheses to change the precedences of an
;; expression. The concatenation ('..') and exponentiation ('^')
;; operators are right associative. All other binary operators are left
;; associative. 

;;;; THE END ;;;;
