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

(pprint (macroexpand-1 '(defgrammar example
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
                           block "." :action $1)))))

(("procedure" "begin" "while" "const" "call" "then" "odd" "end" "var" "if" "do")
 ("<=" ">=" ":=" "(" ")" "*" "/" "+" "-" "#" "<" ">" "=" "," ";" "."))

(ident "[A-Za-z][A-Za-z0-9]*") 
(real "[-+]?[0-9]+\\.[0-9]+([Ee][-+]?[0-9]+)?") 
(integer "[-+]?[0-9]+") 
(let ((com.informatimago.rdp::*linenum* 0)
      (#2=#:|grammar8374|
       (make-grammar :name
                     'example
                     :terminals
                     '((ident "[A-Za-z][A-Za-z0-9]*") (real "[-+]?[0-9]+\\.[0-9]+([Ee][-+]?[0-9]+)?")
                       (integer "[-+]?[0-9]+"))
                     :start
                     'program
                     :rules
                     '#9=(#77=(factor
                               (seq ((alt (ident number (seq (#45="(" expression #81=")") #82=($2)))))
                                #83=($1)))
                          #12=(number (seq ((alt (integer real))) #62=($1)))
                          #72=(term
                               (seq (factor (rep ((seq ((alt (#79="*" #78="/")) factor) #1=($0)))))
                                #80=((list* $1 $2))))
                          #48=(expression
                               (seq
                                ((opt ((seq ((alt (#46="+" #47="-"))) #1#))) term
                                 (rep ((seq ((alt (#73="+" #74="-")) term) #75=((list* $1 (list $2)))))))
                                #76=((list* '+ (list* (if $1 (list* $1 (list $2)) $2) $3)))))
                          #56=(condition
                               (seq
                                ((alt
                                  ((seq (#55="odd" expression) #63=((list* 'oddp (list $2))))
                                   (seq
                                    (expression (alt (#64="=" #65="#" #66="<" #67="<=" #68=">" #69=">="))
                                     expression)
                                    #70=((list* $2 (list* $1 (list $3))))))))
                                #71=($1)))
                          (statement
                           (seq
                            ((opt
                              ((seq
                                ((alt
                                  ((seq (ident #44=":=" expression) #49=((list* 'setf (list* $1 (list $3)))))
                                   (seq (#35="call" ident) #50=((list* 'call (list $2))))
                                   (seq
                                    (#36="begin" statement (rep ((seq (#51=";" statement) #52=($2))))
                                     #53="end")
                                    #54=((list* $2 $3)))
                                   (seq (#37="if" condition #57="then" statement)
                                    #58=((list* 'if (list* $2 (list $4)))))
                                   (seq (#38="while" condition #59="do" statement)
                                    #60=((list* 'while (list* $2 (list $4))))))))
                                #1#))))
                            #61=($1)))
                          (block (seq
                                  ((opt
                                    ((seq
                                      (#10="const" ident #11="=" number
                                       (rep
                                        ((seq (#14="," ident #15="=" number) #25=((list* $2 (list $4))))))
                                       #26=";")
                                      #27=((list* (list* $2 (list $4)) $5)))))
                                   (opt
                                    ((seq (#28="var" ident (rep ((seq (#29="," ident) #30=($2)))) #31=";")
                                      #32=((list* $2 $3)))))
                                   (rep
                                    ((seq (#33="procedure" ident #34=";" block #39=";")
                                      #40=((list* 'procedure (list* $2 (list $4)))))))
                                   statement)
                                  #42=((list* 'block (list* $1 (list* $2 (list* $3 (list $4))))))))
                          (program (seq (block #84=".") #85=($1))))
                     :scanner
                     't
                     :skip-spaces
                     't)))
  (setf (gethash (grammar-name #2#) com.informatimago.rdp::*grammars*) #2#)
  (com.informatimago.rdp::compute-all-terminals #2#)
  (com.informatimago.rdp::compute-all-non-terminals #2#)
  (com.informatimago.rdp::compute-first-follow #2#)
  nil
  (progn (setf (com.informatimago.rdp::grammar-scanner (gethash 'example com.informatimago.rdp::*grammars*))
               'example-scanner)
         (defclass example-scanner (rdp-scanner) nil)
         (defmethod scan-next-token ((com.informatimago.common-lisp.parser.scanner:scanner example-scanner)
                                     &optional com.informatimago.rdp::parser-data)
           "RETURN: (scanner-current-token scanner)"
           (declare (ignore com.informatimago.rdp::parser-data))
           (let (com.informatimago.rdp::match)
             (setf com.informatimago.rdp::match
                   (com.informatimago.rdp::string-match com.informatimago.rdp::*spaces*
                                                        (scanner-buffer
                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                        :start
                                                        (1- (scanner-column
                                                              com.informatimago.common-lisp.parser.scanner:scanner))))
             (when com.informatimago.rdp::match
               (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                     (1+ (com.informatimago.rdp::match-end 1 com.informatimago.rdp::match))))
             (let ((com.informatimago.rdp::pos
                    (1- (scanner-column com.informatimago.common-lisp.parser.scanner:scanner))))
               (cond ((scanner-end-of-source-p com.informatimago.common-lisp.parser.scanner:scanner)
                      (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                            (1+ (length (scanner-buffer
                                          com.informatimago.common-lisp.parser.scanner:scanner)))
                            (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                            "<END OF SOURCE>"
                            (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                            'com.informatimago.rdp::<END\ OF\ SOURCE>))
                     ((com.informatimago.rdp::scanner-end-of-line-p
                        com.informatimago.common-lisp.parser.scanner:scanner)
                      (advance-line com.informatimago.common-lisp.parser.scanner:scanner))
                     ((or (setf com.informatimago.rdp::match
                                (com.informatimago.rdp::string-match '"^(procedure|begin|while|const|call|then|odd|end|var|if|do)([^A-Za-z0-9]|$)"
                                                                     (scanner-buffer
                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                     :start
                                                                     com.informatimago.rdp::pos))
                          (setf com.informatimago.rdp::match
                                (com.informatimago.rdp::string-match '"^(\\<\\=|\\>\\=|\\:\\=|\\(|\\)|\\*|\\/|\\+|\\-|\\#|\\<|\\>|\\=|\\,|\\;|\\.)"
                                                                     (scanner-buffer
                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                     :start
                                                                     com.informatimago.rdp::pos)))
                      (let ((com.informatimago.rdp::text
                             (com.informatimago.rdp::match-string 1
                                                                  (scanner-buffer
                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                  com.informatimago.rdp::match)))
                        (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                              (1+ (com.informatimago.rdp::match-end 1 com.informatimago.rdp::match))
                              (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                              com.informatimago.rdp::text
                              (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                              com.informatimago.rdp::text)))
                     ((setf com.informatimago.rdp::match
                            (com.informatimago.rdp::string-match '"^([A-Za-z][A-Za-z0-9]*)"
                                                                 . #3=((scanner-buffer
                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                       :start
                                                                       com.informatimago.rdp::pos)))
                      (setf #4=(scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                            #5=(1+ (com.informatimago.rdp::match-end 1 com.informatimago.rdp::match))
                            #6=(scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                            #7=(com.informatimago.rdp::match-string 1
                                                                    (scanner-buffer
                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                    com.informatimago.rdp::match)
                            #8=(scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                            'ident))
                     ((setf com.informatimago.rdp::match
                            (com.informatimago.rdp::string-match '"^([-+]?[0-9]+\\.[0-9]+([Ee][-+]?[0-9]+)?)"
                                                                 . #3#))
                      (setf #4# #5# #6# #7# #8# 'real))
                     ((setf com.informatimago.rdp::match
                            (com.informatimago.rdp::string-match '"^([-+]?[0-9]+)" . #3#))
                      (setf #4# #5# #6# #7# #8# 'integer))
                     (t
                      (error 'com.informatimago.common-lisp.parser.scanner:scanner-error-invalid-character
                             :line
                             (scanner-line com.informatimago.common-lisp.parser.scanner:scanner)
                             :column
                             (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                             :state
                             (scanner-state com.informatimago.common-lisp.parser.scanner:scanner)
                             :current-token
                             (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                             :scanner
                             com.informatimago.common-lisp.parser.scanner:scanner
                             :invalid-character
                             (aref (scanner-buffer com.informatimago.common-lisp.parser.scanner:scanner)
                                   com.informatimago.rdp::pos)
                             :format-control
                             "Invalid character ~S at position: ~D~%~S~%~{~A --> ~S~}"
                             :format-arguments
                             (list (aref (scanner-buffer
                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                         com.informatimago.rdp::pos)
                                   (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   *non-terminal-stack*
                                   (assoc (first *non-terminal-stack*) '#9#)))))))))
  (defun example/parse-block #43=(com.informatimago.common-lisp.parser.scanner:scanner)
    "(block (seq ((opt ((seq (\"const\" ident \"=\" number (rep ((seq (\",\" ident \"=\" number) ((list* $2 (list $4)))))) \";\") ((list* (list* $2 (list $4)) $5))))) (opt ((seq (\"var\" ident (rep ((seq (\",\" ident) ($2)))) \";\") ((list* $2 $3))))) (rep ((seq (\"procedure\" ident \";\" block \";\") ((list* 'procedure (list* $2 (list $4))))))) statement) ((list* 'block (list* $1 (list* $2 (list* $3 (list $4))))))))"
    (com.informatimago.rdp::with-non-terminal
      block
      (let (($1
             (when (word-equal #13=(scanner-current-token
                                     com.informatimago.common-lisp.parser.scanner:scanner)
                               '#10#)
               (let (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner '#10#))
                     ($2 (accept com.informatimago.common-lisp.parser.scanner:scanner 'ident))
                     ($3 (accept com.informatimago.common-lisp.parser.scanner:scanner '#11#))
                     ($4
                      (if (member #16=(scanner-current-token
                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                  '(integer real)
                                  . #17=(:test #'word-equal))
                          (example/parse-number . #18=(com.informatimago.common-lisp.parser.scanner:scanner))
                          (error #19='unexpected-token-error
                                 :line
                                 #20=(scanner-line com.informatimago.common-lisp.parser.scanner:scanner)
                                 :column
                                 #21=(scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                 :scanner
                                 com.informatimago.common-lisp.parser.scanner:scanner
                                 :non-terminal-stack
                                 #22=(copy-list *non-terminal-stack*)
                                 :format-control
                                 #23="Unexpected token ~S~%~S~%~{~A --> ~S~}"
                                 :format-arguments
                                 (list #24=(scanner-current-token
                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                       *non-terminal-stack*
                                       '#12#))))
                     ($5
                      (loop :while (word-equal #13# '#14#)
                            :collect (let (($1
                                            (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                    '#14#))
                                           ($2
                                            (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                    'ident))
                                           ($3
                                            (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                    '#15#))
                                           ($4
                                            (if (member #16# '(integer real) . #17#)
                                                (example/parse-number . #18#)
                                                (error #19#
                                                       :line
                                                       #20#
                                                       :column
                                                       #21#
                                                       :scanner
                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                       :non-terminal-stack
                                                       #22#
                                                       :format-control
                                                       #23#
                                                       :format-arguments
                                                       (list #24# *non-terminal-stack* '#12#)))))
                                       (let (($0 (list $1 $2 $3 $4))
                                             (ident $2)
                                             (ident.1 $2)
                                             (number $4)
                                             (number.1 $4))
                                         (declare (ignorable $0 number.1 number ident.1 ident))
                                         . #25#))))
                      ($6 (accept com.informatimago.common-lisp.parser.scanner:scanner '#26#)))
                     (let (($0 (list $1 $2 $3 $4 $5 $6)) (ident $2) (ident.1 $2) (number $4) (number.1 $4))
                       (declare (ignorable $0 number.1 number ident.1 ident))
                       . #27#))))
             ($2
              (when (word-equal #13# '#28#)
                (let (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner '#28#))
                      ($2 (accept com.informatimago.common-lisp.parser.scanner:scanner 'ident))
                      ($3
                       (loop :while (word-equal #13# '#14#)
                             :collect (let (($1
                                             (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                     '#29#))
                                            ($2
                                             (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                     'ident)))
                                        (let (($0 (list $1 $2)) (ident $2) (ident.1 $2))
                                          (declare (ignorable $0 ident.1 ident))
                                          . #30#))))
                       ($4 (accept com.informatimago.common-lisp.parser.scanner:scanner '#31#)))
                      (let (($0 (list $1 $2 $3 $4)) (ident $2) (ident.1 $2))
                        (declare (ignorable $0 ident.1 ident))
                        . #32#))))
              ($3
               (loop :while (word-equal #13# '#33#)
                     :collect (let (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner '#33#))
                                    ($2 (accept com.informatimago.common-lisp.parser.scanner:scanner 'ident))
                                    ($3 (accept com.informatimago.common-lisp.parser.scanner:scanner '#34#))
                                    ($4
                                     (when (member #16# '(#35# #36# #37# #38# ident #33# #28# #10#) . #17#)
                                       (example/parse-block
                                         . #41=(com.informatimago.common-lisp.parser.scanner:scanner))))
                                    ($5 (accept com.informatimago.common-lisp.parser.scanner:scanner '#39#)))
                                (let (($0 (list $1 $2 $3 $4 $5))
                                      (ident $2)
                                      (ident.1 $2)
                                      (block $4)
                                      (block.1 $4))
                                  (declare (ignorable $0 block.1 block ident.1 ident))
                                  . #40#))))
               ($4
                (when (member #16# '(ident #38# #37# #36# #35#) . #17#) (example/parse-statement . #41#))))
              (let (($0 (list $1 $2 $3 $4)) (statement $4) (statement.1 $4))
                (declare (ignorable $0 statement.1 statement))
                . #42#))))
        (defun example/parse-statement #43#
          "(statement (seq ((opt ((seq ((alt ((seq (ident \":=\" expression) ((list* 'setf (list* $1 (list $3))))) (seq (\"call\" ident) ((list* 'call (list $2)))) (seq (\"begin\" statement (rep ((seq (\";\" statement) ($2)))) \"end\") ((list* $2 $3))) (seq (\"if\" condition \"then\" statement) ((list* 'if (list* $2 (list $4))))) (seq (\"while\" condition \"do\" statement) ((list* 'while (list* $2 (list $4)))))))) ($0))))) ($1)))"
          (com.informatimago.rdp::with-non-terminal
            statement
            (let (($1
                   (when (member #16# '(#38# #36# ident #35# #37#) . #17#)
                     (let (($1
                            (cond ((word-equal #13# 'ident)
                                   (let (($1
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  'ident))
                                         ($2
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  '#44#))
                                         ($3
                                          (if (member #16# '(ident #45# integer real #46# #47#) . #17#)
                                              (example/parse-expression . #18#)
                                              (error #19#
                                                     :line
                                                     #20#
                                                     :column
                                                     #21#
                                                     :scanner
                                                     com.informatimago.common-lisp.parser.scanner:scanner
                                                     :non-terminal-stack
                                                     #22#
                                                     :format-control
                                                     #23#
                                                     :format-arguments
                                                     (list #24# *non-terminal-stack* '#48#)))))
                                     (let (($0 (list $1 $2 $3))
                                           (ident $1)
                                           (ident.1 $1)
                                           (expression $3)
                                           (expression.1 $3))
                                       (declare (ignorable $0 expression.1 expression ident.1 ident))
                                       . #49#)))
                                  ((word-equal #13# '#35#)
                                   (let (($1
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  '#35#))
                                         ($2
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  'ident)))
                                     (let (($0 (list $1 $2)) (ident $2) (ident.1 $2))
                                       (declare (ignorable $0 ident.1 ident))
                                       . #50#)))
                                  ((word-equal #13# '#36#)
                                   (let (($1
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  '#36#))
                                         ($2
                                          (when (member #16# '(ident #38# #37# #36# #35#) . #17#)
                                            (example/parse-statement . #41#)))
                                         ($3
                                          (loop :while (word-equal #13# '#26#)
                                                :collect (let (($1
                                                                (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                        '#51#))
                                                               ($2
                                                                (when (member
                                                                       #16#
                                                                       '(ident #38# #37# #36# #35#)
                                                                       . #17#)
                                                                  (example/parse-statement . #41#))))
                                                           (let (($0 (list $1 $2))
                                                                 (statement $2)
                                                                 (statement.1 $2))
                                                             (declare (ignorable $0 statement.1 statement))
                                                             . #52#))))
                                          ($4
                                           (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                   '#53#)))
                                         (let (($0 (list $1 $2 $3 $4)) (statement $2) (statement.1 $2))
                                           (declare (ignorable $0 statement.1 statement))
                                           . #54#)))
                                   ((word-equal #13# '#37#)
                                    (let (($1
                                           (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                   '#37#))
                                          ($2
                                           (if (member #16# '(#55# #47# #46# real integer #45# ident) . #17#)
                                               (example/parse-condition . #18#)
                                               (error #19#
                                                      :line
                                                      #20#
                                                      :column
                                                      #21#
                                                      :scanner
                                                      com.informatimago.common-lisp.parser.scanner:scanner
                                                      :non-terminal-stack
                                                      #22#
                                                      :format-control
                                                      #23#
                                                      :format-arguments
                                                      (list #24# *non-terminal-stack* '#56#))))
                                          ($3
                                           (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                   '#57#))
                                          ($4
                                           (when (member #16# '(ident #38# #37# #36# #35#) . #17#)
                                             (example/parse-statement . #41#))))
                                      (let (($0 (list $1 $2 $3 $4))
                                            (condition $2)
                                            (condition.1 $2)
                                            (statement $4)
                                            (statement.1 $4))
                                        (declare (ignorable $0 statement.1 statement condition.1 condition))
                                        . #58#)))
                                   ((word-equal #13# '#38#)
                                    (let (($1
                                           (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                   '#38#))
                                          ($2
                                           (if (member #16# '(#55# #47# #46# real integer #45# ident) . #17#)
                                               (example/parse-condition . #18#)
                                               (error #19#
                                                      :line
                                                      #20#
                                                      :column
                                                      #21#
                                                      :scanner
                                                      com.informatimago.common-lisp.parser.scanner:scanner
                                                      :non-terminal-stack
                                                      #22#
                                                      :format-control
                                                      #23#
                                                      :format-arguments
                                                      (list #24# *non-terminal-stack* '#56#))))
                                          ($3
                                           (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                   '#59#))
                                          ($4
                                           (when (member #16# '(ident #38# #37# #36# #35#) . #17#)
                                             (example/parse-statement . #41#))))
                                      (let (($0 (list $1 $2 $3 $4))
                                            (condition $2)
                                            (condition.1 $2)
                                            (statement $4)
                                            (statement.1 $4))
                                        (declare (ignorable $0 statement.1 statement condition.1 condition))
                                        . #60#))))))
                           (let (($0 (list $1))) (declare (ignorable $0)) . #1#)))))
                  (let (($0 (list $1))) (declare (ignorable $0)) . #61#))))
          (defun example/parse-number #43#
            "(number (seq ((alt (integer real))) ($1)))"
            (com.informatimago.rdp::with-non-terminal
              number
              (let (($1
                     (cond ((word-equal #13# 'integer)
                            (accept com.informatimago.common-lisp.parser.scanner:scanner 'integer))
                           ((word-equal #13# 'real)
                            (accept com.informatimago.common-lisp.parser.scanner:scanner 'real)))))
                (let (($0 (list $1))) (declare (ignorable $0)) . #62#))))
          (defun example/parse-condition #43#
            "(condition (seq ((alt ((seq (\"odd\" expression) ((list* 'oddp (list $2)))) (seq (expression (alt (\"=\" \"#\" \"<\" \"<=\" \">\" \">=\")) expression) ((list* $2 (list* $1 (list $3)))))))) ($1)))"
            (com.informatimago.rdp::with-non-terminal
              condition
              (let (($1
                     (cond ((word-equal #13# '#55#)
                            (let (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner '#55#))
                                  ($2
                                   (if (member #16# '(ident #45# integer real #46# #47#) . #17#)
                                       (example/parse-expression . #18#)
                                       (error #19#
                                              :line
                                              #20#
                                              :column
                                              #21#
                                              :scanner
                                              com.informatimago.common-lisp.parser.scanner:scanner
                                              :non-terminal-stack
                                              #22#
                                              :format-control
                                              #23#
                                              :format-arguments
                                              (list #24# *non-terminal-stack* '#48#)))))
                              (let (($0 (list $1 $2)) (expression $2) (expression.1 $2))
                                (declare (ignorable $0 expression.1 expression))
                                . #63#)))
                           ((member #16# '(#47# #46# real integer #45# ident) . #17#)
                            (let (($1
                                   (if (member #16# '(ident #45# integer real #46# #47#) . #17#)
                                       (example/parse-expression . #18#)
                                       (error #19#
                                              :line
                                              #20#
                                              :column
                                              #21#
                                              :scanner
                                              com.informatimago.common-lisp.parser.scanner:scanner
                                              :non-terminal-stack
                                              #22#
                                              :format-control
                                              #23#
                                              :format-arguments
                                              (list #24# *non-terminal-stack* '#48#))))
                                  ($2
                                   (cond ((word-equal #13# '#15#)
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  '#64#))
                                         ((word-equal #13# '#65#)
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  '#65#))
                                         ((word-equal #13# '#66#)
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  '#66#))
                                         ((word-equal #13# '#67#)
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  '#67#))
                                         ((word-equal #13# '#68#)
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  '#68#))
                                         ((word-equal #13# '#69#)
                                          (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                  '#69#))))
                                  ($3
                                   (if (member #16# '(ident #45# integer real #46# #47#) . #17#)
                                       (example/parse-expression . #18#)
                                       (error #19#
                                              :line
                                              #20#
                                              :column
                                              #21#
                                              :scanner
                                              com.informatimago.common-lisp.parser.scanner:scanner
                                              :non-terminal-stack
                                              #22#
                                              :format-control
                                              #23#
                                              :format-arguments
                                              (list #24# *non-terminal-stack* '#48#)))))
                              (let (($0 (list $1 $2 $3)) (expression $1) (expression.1 $1) (expression.2 $3))
                                (declare (ignorable $0 expression.2 expression.1 expression))
                                . #70#))))))
                (let (($0 (list $1))) (declare (ignorable $0)) . #71#))))
          (defun example/parse-expression #43#
            "(expression (seq ((opt ((seq ((alt (\"+\" \"-\"))) ($0)))) term (rep ((seq ((alt (\"+\" \"-\")) term) ((list* $1 (list $2))))))) ((list* '+ (list* (if $1 (list* $1 (list $2)) $2) $3)))))"
            (com.informatimago.rdp::with-non-terminal
              expression
              (let (($1
                     (when (member #16# '(#47# #46#) . #17#)
                       (let (($1
                              (cond ((word-equal #13# '#46#)
                                     (accept com.informatimago.common-lisp.parser.scanner:scanner '#46#))
                                    ((word-equal #13# '#47#)
                                     (accept com.informatimago.common-lisp.parser.scanner:scanner '#47#)))))
                         (let (($0 (list $1))) (declare (ignorable $0)) . #1#))))
                    ($2
                     (if (member #16# '(real integer #45# ident) . #17#)
                         (example/parse-term . #18#)
                         (error #19#
                                :line
                                #20#
                                :column
                                #21#
                                :scanner
                                com.informatimago.common-lisp.parser.scanner:scanner
                                :non-terminal-stack
                                #22#
                                :format-control
                                #23#
                                :format-arguments
                                (list #24# *non-terminal-stack* '#72#))))
                    ($3
                     (loop :while (member #16# '(#47# #46#) . #17#)
                           :collect (let (($1
                                           (cond ((word-equal #13# '#46#)
                                                  (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                          '#73#))
                                                 ((word-equal #13# '#47#)
                                                  (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                          '#74#))))
                                          ($2
                                           (if (member #16# '(real integer #45# ident) . #17#)
                                               (example/parse-term . #18#)
                                               (error #19#
                                                      :line
                                                      #20#
                                                      :column
                                                      #21#
                                                      :scanner
                                                      com.informatimago.common-lisp.parser.scanner:scanner
                                                      :non-terminal-stack
                                                      #22#
                                                      :format-control
                                                      #23#
                                                      :format-arguments
                                                      (list #24# *non-terminal-stack* '#72#)))))
                                      (let (($0 (list $1 $2)) (term $2) (term.1 $2))
                                        (declare (ignorable $0 term.1 term))
                                        . #75#)))))
                    (let (($0 (list $1 $2 $3)) (term $2) (term.1 $2))
                      (declare (ignorable $0 term.1 term))
                      . #76#))))
            (defun example/parse-term #43#
              "(term (seq (factor (rep ((seq ((alt (\"*\" \"/\")) factor) ($0))))) ((list* $1 $2))))"
              (com.informatimago.rdp::with-non-terminal
                term
                (let (($1
                       (if (member #16# '(ident #45# integer real) . #17#)
                           (example/parse-factor . #18#)
                           (error #19#
                                  :line
                                  #20#
                                  :column
                                  #21#
                                  :scanner
                                  com.informatimago.common-lisp.parser.scanner:scanner
                                  :non-terminal-stack
                                  #22#
                                  :format-control
                                  #23#
                                  :format-arguments
                                  (list #24# *non-terminal-stack* '#77#))))
                      ($2
                       (loop :while (member #16# '(#78# #79#) . #17#)
                             :collect (let (($1
                                             (cond ((word-equal #13# '#79#)
                                                    (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                            '#79#))
                                                   ((word-equal #13# '#78#)
                                                    (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                            '#78#))))
                                            ($2
                                             (if (member #16# '(ident #45# integer real) . #17#)
                                                 (example/parse-factor . #18#)
                                                 (error #19#
                                                        :line
                                                        #20#
                                                        :column
                                                        #21#
                                                        :scanner
                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                        :non-terminal-stack
                                                        #22#
                                                        :format-control
                                                        #23#
                                                        :format-arguments
                                                        (list #24# *non-terminal-stack* '#77#)))))
                                        (let (($0 (list $1 $2)) (factor $2) (factor.1 $2))
                                          (declare (ignorable $0 factor.1 factor))
                                          . #1#)))))
                      (let (($0 (list $1 $2)) (factor $1) (factor.1 $1))
                        (declare (ignorable $0 factor.1 factor))
                        . #80#))))
              (defun example/parse-factor #43#
                "(factor (seq ((alt (ident number (seq (\"(\" expression \")\") ($2))))) ($1)))"
                (com.informatimago.rdp::with-non-terminal
                  factor
                  (let (($1
                         (cond ((word-equal #13# 'ident)
                                (accept com.informatimago.common-lisp.parser.scanner:scanner 'ident))
                               ((member #16# '(integer real) . #17#)
                                (if (member #16# '(integer real) . #17#)
                                    (example/parse-number . #18#)
                                    (error #19#
                                           :line
                                           #20#
                                           :column
                                           #21#
                                           :scanner
                                           com.informatimago.common-lisp.parser.scanner:scanner
                                           :non-terminal-stack
                                           #22#
                                           :format-control
                                           #23#
                                           :format-arguments
                                           (list #24# *non-terminal-stack* '#12#))))
                               ((word-equal #13# '#45#)
                                (let (($1
                                       (accept com.informatimago.common-lisp.parser.scanner:scanner '#45#))
                                      ($2
                                       (if (member #16# '(ident #45# integer real #46# #47#) . #17#)
                                           (example/parse-expression . #18#)
                                           (error #19#
                                                  :line
                                                  #20#
                                                  :column
                                                  #21#
                                                  :scanner
                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                  :non-terminal-stack
                                                  #22#
                                                  :format-control
                                                  #23#
                                                  :format-arguments
                                                  (list #24# *non-terminal-stack* '#48#))))
                                      ($3
                                       (accept com.informatimago.common-lisp.parser.scanner:scanner '#81#)))
                                  (let (($0 (list $1 $2 $3)) (expression $2) (expression.1 $2))
                                    (declare (ignorable $0 expression.1 expression))
                                    . #82#))))))
                    (let (($0 (list $1))) (declare (ignorable $0)) . #83#))))
              (defun example/parse-program #43#
                "(program (seq (block \".\") ($1)))"
                (com.informatimago.rdp::with-non-terminal
                  program
                  (let (($1
                         (when (member #16# '(#35# #36# #37# #38# ident #33# #28# #10#) . #17#)
                           (example/parse-block . #41#)))
                        ($2 (accept com.informatimago.common-lisp.parser.scanner:scanner '#84#)))
                    (let (($0 (list $1 $2)) (block $1) (block.1 $1))
                      (declare (ignorable $0 block.1 block))
                      . #85#))))
              (defun parse-example (com.informatimago.rdp::source)
                "
SOURCE: When the grammar has a scanner generated, or a scanner class
        name, SOURCE can be either a string, or a stream that will be
        scanned with the generated scanner.  Otherwise, it should be a
        SCANNER instance.
"
                (com.informatimago.rdp::with-non-terminal
                  example
                  (let ((com.informatimago.common-lisp.parser.scanner:scanner
                         (make-instance 'example-scanner :source com.informatimago.rdp::source)))
                    (advance-line com.informatimago.common-lisp.parser.scanner:scanner)
                    (prog1 (example/parse-program com.informatimago.common-lisp.parser.scanner:scanner)
                           (unless (scanner-end-of-source-p
                                     com.informatimago.common-lisp.parser.scanner:scanner)
                             (error 'parser-end-of-source-not-reached
                                    :line
                                    (scanner-line com.informatimago.common-lisp.parser.scanner:scanner)
                                    :column
                                    (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                    :grammar
                                    (grammar-named 'example)
                                    :scanner
                                    com.informatimago.common-lisp.parser.scanner:scanner
                                    :non-terminal-stack
                                    (copy-list *non-terminal-stack*)))))))
              'example)


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
               
               (error "we should get the real identifiers.")
               '(block (((ident "IDENT" 0) (integer "INTEGER" 0)) ((ident "IDENT" 0) (real "REAL" 0)))
                 ((ident "IDENT" 0) (ident "IDENT" 0) (ident "IDENT" 0))
                 ((procedure (ident "IDENT" 0)
                   (block nil
                     nil
                     nil
                     ((((while (("#" "#" 0) (+ ((ident "IDENT" 0))) (+ ((ident "IDENT" 0))))
                          ((((if (("<" "<" 0) (+ ((ident "IDENT" 0))) (+ ((ident "IDENT" 0))))
                                 ((setf (ident "IDENT" 0) (+ ((ident "IDENT" 0)) (("-" "-" 0) ((ident "IDENT" 0))))))))
                            ((if ((">" ">" 0) (+ ((ident "IDENT" 0))) (+ ((ident "IDENT" 0))))
                                 ((setf (ident "IDENT" 0)
                                        (+ ((ident "IDENT" 0)) (("-" "-" 0) ((ident "IDENT" 0)))))))))))))))))
                 ((((setf (ident "IDENT" 0) (+ ((integer "INTEGER" 0))))) ((setf (ident "IDENT" 0) (+ ((real "REAL" 0)))))
                   ((call (ident "IDENT" 0))))))
               ))



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
