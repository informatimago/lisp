;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               lua-parser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the LUA Parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "COM.INFORMATIMAGO.LUA.PARSER")

#-(and)
(defgrammar lua
    :trace t
    :scanner lua-scanner
    :terminals ()
    :start chunk
    :rules (

            (--> chunk
                 (rep stat (opt ";")) (opt laststat (opt ";")))

            (--> block
                 chunk)

            (--> stat
                 (alt (seq varlist "=" explist) 
                      (seq functioncall) 
                      (seq "do" block "end")
                      (seq "while" exp "do" block "end")
                      (seq "repeat" block "until" exp)
                      (seq "if" exp "then" block (rep "elseif" exp "then" block) (opt "else" block) "end")
                      (seq "for" Name "=" exp "," exp (opt "," exp) "do" block "end")
                      (seq "for" namelist "in" explist "do" block "end")
                      (seq "function" funcname funcbody) 
                      (seq "local" "function" Name funcbody)
                      (seq "local" namelist (opt "=" explist))))

            (--> laststat
                 (alt (seq "return" (opt explist))
                      "break"))

            (--> funcname
                 Name (rep "." Name) (opt ":" Name))

            (--> varlist
                 var (rep "," var))

            (--> var
                 (alt Name
                      (seq prefixexp "[" exp "]")
                      (seq prefixexp "." Name)) )

            (--> namelist
                 Name (rep "," Name))

            (--> explist
                 (rep exp ",") exp)

            (--> exp
                 (alt "nil" 
                      "false" 
                      "true" 
                      Number 
                      String 
                      "..." 
                      function 
                      prefixexp 
                      tableconstructor 
                      (seq exp binop exp) 
                      (seq unop exp)))

            (--> prefixexp
                 (alt var 
                      functioncall 
                      (seq "(" exp ")")))

            (--> functioncall
                 (alt (seq prefixexp args)
                      (seq prefixexp ":" Name args)))

            (--> args
                 (alt (seq "(" (opt explist) ")" )
                      tableconstructor 
                      String) )

            (--> function
                 "function" funcbody)

            (--> funcbody
                 (seq "(" (opt parlist) ")" block "end"))

            (--> parlist
                 (alt (seq namelist (opt "," "...")) 
                      "..."))

            (--> tableconstructor
                 (seq "(rep " (opt fieldlist) ")"))

            (--> fieldlist
                 (seq field (rep fieldsep field) (opt fieldsep)))

            (--> field
                 (alt (seq "[" exp "]" "=" exp )
                      (seq Name "=" exp)
                      exp))

            (--> fieldsep
                 (alt "," 
                      ";"))

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

            (--> unop
                 (alt "-" 
                      "not" 
                      "#"))))




