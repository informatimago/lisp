;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               simple-cl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A simple subset of CL.
;;;;
;;;;    The reader is a very simple subset of the Common Lisp reader.
;;;;    We DO NOT implement the reader macros characters:
;;;;        ;         comment
;;;;        '         quote
;;;;        "         strings (with \ escapes)
;;;;        |\        single and multiple escapes in symbol names.
;;;;        ` , ,@    backquote, comma and comma-at.
;;;;        #         the dispatching macro character.
;;;;    we don't implement any of the dispatch reader macro,
;;;;    we don't implement packages nor package separators,
;;;;
;;;;    we only have fixnum, symbols, list and dotted lists.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2008-07-12 <PJB> Restricted the reader.
;;;;    2008-05-20 <PJB> Implemented simple-reader.
;;;;    2008-05-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2008 - 2012
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



(defun bf$print (obj)
  (let ((stack (list 'element obj)))
    (loop
       :while stack
       :do (let ((kind    (pop stack))
                 (cur-obj (pop stack)))
             (typecase cur-obj
               (cons
                (if (eql kind 'element)
                    (progn
                      (bf$$print-string "(")
                      (push (cdr cur-obj) stack)
                      (push 'list         stack)
                      (push (car cur-obj) stack)
                      (push 'element stack))
                    (progn
                      (bf$$print-string " ")
                      (push (cdr cur-obj) stack)
                      (push 'list         stack)
                      (push (car cur-obj) stack)
                      (push 'element stack))))
               (null
                (if (eql kind 'element)
                    (bf$$print-string "NIL")
                    (bf$$print-string ")")))
               (t
                (if (eql kind 'element)
                    (bf$$print-string (prin1-to-string cur-obj))
                    (progn (bf$$print-string " . ")
                           (bf$$print-string (prin1-to-string cur-obj))
                           (bf$$print-string ")")))))))
    output))



(setf text "(attributes (lock \"toto\"))")

(defun make-str (str) (copy-seq   str))
(defun make-sym (str) (intern     str))

(defvar *scanner-text*)
(defvar *scanner-position*)
(defun init-scanner (text)
  (setf *scanner-text* text
        *scanner-position* 0))
(defconstant +eot+ 'nil)
(defun eotp (ch) (eql +eot+ ch))
(defun next-char ()
  (when (< *scanner-position* (length *scanner-text*))
    (prog1 (aref *scanner-text* *scanner-position*)
      (incf *scanner-position*))))
(defun parse-token (kind token)
  (case kind
    ((symbol) (values 'symbol (make-sym token)))
    (otherwise
     (let ((value  (parse-integer token :junk-allowed t)))
       (if (string= token (prin1-to-string value))
           (values 'integer value)
           (values 'symbol  (make-sym token)))))))

(defun read-from-str (text)
  (init-scanner text)
  (let ((ch #\space)
        type value
        (stack '(parse done eot nil)))
    (macrolet ((stop () `(car stack))
               (spop ()
                 '(pop stack)
                 #-(and)
                 (let ((val (gensym)))
                   `(let ((,val (pop stack)))
                      (format t "(pop ~A) --> ~S~%~A = ~S~%~%"
                              'stack ,val 'stack stack)
                      ,val)))
               (spush (expr)
                 `(push ,expr stack)
                 #-(and)
                 (let ((vexpr (gensym)))
                   `(let ((,vexpr ,expr))
                      (format t "~A = ~S~%(push  ~S = ~S  ~A)~%"
                              'stack stack ',expr  ,vexpr 'stack)
                      (prog1 (push ,vexpr stack)
                        (format t  "~A = ~S~2%"  'stack stack))))))
      (loop
         :for state = (spop)
         :until (eql state 'done)
         :do (ecase state
               ((parse)
                (progn
                  (loop
                     :named skip-spaces
                     :while (and (not (eotp ch)) (or (char= #\space ch) (char= #\newline ch)))
                     :do (setf ch (next-char)))
                  (case ch
                    ((#.+eot+)
                     (spush nil)
                     (spush 'eot)
                     (spush 'done))
                    ((#\')
                     (spush 'quote)
                     (spush 'parse))
                    ((#\")
                     (loop
                        :with strstate = 'in-string
                        :with str = ""
                        :until (or (eotp ch) (eql strstate 'done))
                        :do (setf ch (next-char))
                        :do (ecase strstate
                              ((in-string)
                               (case ch
                                 ((#\") (setf strstate 'done))
                                 ((#\\) (setf strstate 'escaped))
                                 (otherwise (concat str ch))))
                              ((escaped)
                               (concat str ch)
                               (setf strstate 'in-string)))
                        :finally (if (eotp ch)
                                     (error "End-Of-Text in the string ~S" str)
                                     (progn
                                       (spush (make-str str))
                                       (spush 'string)
                                       (spush 'token)
                                       (setf ch (next-char))))))
                    ((#\()
                     (spush 'nil)
                     (spush 'list)
                     (spush 'parse)
                     (setf ch (next-char)))
                    ((#\))
                     (if (eql 'list (stop))
                         (progn (spop)
                                (spush (reverse (spop)))
                                (spush 'cons)
                                (spush 'token)
                                (setf ch (next-char)))
                         (error "An object cannot begin with #\\)")))
                    ((#\;)
                     (loop :named comment :until (or (scanner-eot) (char= #\newline (next-char))))
                     (spush 'parse))
                    ((#\` #\, #\#)
                     (error "Reader macro for character #\\~C is not implemented yet." ch))
                    (otherwise
                     (loop
                        :with kind = nil
                        :with tokenstate = 'in-token
                        :with token = ""
                        :until (eql tokenstate 'done)
                        :do (ecase tokenstate
                              ((in-token)
                               (case ch
                                 ((#.+eot+)
                                  (setf tokenstate 'done)
                                  (multiple-value-bind (type value) (parse-token kind token)
                                    (progn (spush value)
                                           (spush type)
                                           (spush 'token))))
                                 ((#\space #\' #\" #\; #\, #\` #\( #\) #\newline)
                                  (setf tokenstate 'done)
                                  (multiple-value-bind (type value) (parse-token kind token)
                                    (progn (spush value)
                                           (spush type)
                                           (spush 'token))))
                                 ((#\\)
                                  (setf tokenstate 'escape1 kind 'symbol)
                                  (setf ch (next-char)))
                                 ((#\|)
                                  (setf tokenstate 'escape2 kind 'symbol)
                                  (setf ch (next-char)))
                                 (otherwise
                                  (unless (or (eotp ch)
                                              (digit-char-p ch)
                                              (position ch "+-.EeSsFfDdLl"))
                                    (setf kind 'symbol))
                                  (concat token (char-upcase ch))
                                  (setf ch (next-char)))))
                              ((escape1)
                               (when (eotp ch)
                                 (error "Escaping EOT"))
                               (concat token ch)
                               (setf tokenstate 'in-token)
                               (setf ch (next-char)))
                              ((escape2)
                               (case ch
                                 ((#.+eot+) (error "EOT in multiple escape"))
                                 ((#\\)     (setf tokenstate 'escape21))
                                 ((#\|)     (setf tokenstate 'in-token))
                                 (otherwise (concat token ch)))
                               (setf ch (next-char)))
                              ((escape21)
                               (when (eotp ch)
                                 (error "Escaping EOT"))
                               (concat token ch)
                               (setf tokenstate 'escape2)
                               (setf ch (next-char)))))))))
               ((token)
                (let ((type  (spop))
                      (value (spop)))
                  (ecase (setf state (spop))
                    ((done)
                     (spop)
                     (spop)
                     (spush value)
                     (spush type)
                     (spush 'done))
                    ((list)
                     (spush (cons value (spop)))
                     (spush 'list)
                     (spush 'parse))
                    ((quote)
                     (spush (list 'quote value))
                     (spush 'cons)
                     (spush 'token))))))
         :finally (let ((type  (spop))
                        (value (spop)))
                    (return (values value *scanner-position*)))))))






;; ((#\` #<COMPILED-FUNCTION COM.INFORMATIMAGO.COMMON-LISP.READER::READER-MACRO-BACKQUOTE>        NIL NIL)
;;  (#\; #<COMPILED-FUNCTION COM.INFORMATIMAGO.COMMON-LISP.READER::READER-MACRO-LINE-COMMENT>     NIL NIL)
;;  (#\, #<COMPILED-FUNCTION COM.INFORMATIMAGO.COMMON-LISP.READER::READER-MACRO-COMMA>            NIL NIL)
;;  (#\) #<COMPILED-FUNCTION COM.INFORMATIMAGO.COMMON-LISP.READER::READER-MACRO-ERROR-START>      NIL NIL)
;;  (#\( #<COMPILED-FUNCTION COM.INFORMATIMAGO.COMMON-LISP.READER::READER-MACRO-LEFT-PARENTHESIS> NIL NIL)
;;  (#\' #<COMPILED-FUNCTION COM.INFORMATIMAGO.COMMON-LISP.READER::READER-MACRO-QUOTE>            NIL NIL)
;;  (#\" #<COMPILED-FUNCTION COM.INFORMATIMAGO.COMMON-LISP.READER::READER-MACRO-STRING>           NIL NIL))

;; comment ::= ';' line-char '\n' .
;; sexp ::= atom | '(' ')' | '(' sexp+ [ '.' sexp ] ')' | '\'''(' sexp ')'
;; atom ::= [-+]digit+ | [-+]digit+.digit+[[eE][-+]digit+] | '"' string-char* '"' | symbol-char(symbol-char|digit)* 

;;(trace next-char)
(dolist (test '((|symbol|  "|symbol|")
                (|symbol|  "\\s\\y\\m\\b\\o\\l and another")
                (|SYMBOL|  "symbol")
                (|SYMBOL|  "SYMBOL and another")
                (1234      "1234")
                (1234      "1234 integer")
                (1234      "01234 integer")
                ("string"  "\"string\"")
                ("string"  "\"string\" and something else")
                ("string\"with dblquote" 
                 "\"string\\\"with dblquote\" and something else")
                ( "string
with newline"
                  "\"string
with newline\" and something else")
                (() "()")
                (() "  (   )   ")
                ((1) "  ( 1  )   ")
                ((1 2 3 4) "  ( 1 2 3 4 )   ")
                ((|a| |bc| |def|) " (|a| |bc| \\d\\e\\f)   ")
                ((|attributes| (|lock| "toto")) "(|attr|\\i|butes| (|lock| \"toto\"))")
                ((a bc def) " (a bc def)   ")
                ((attributes (lock "toto")) "(attributes (lock \"toto\"))")))
  (let ((results  (multiple-value-list (read-from-str (second test)))))
    (format t "(read-from-str ~S) ~% --> ~{~S~^, ~}~%" (second test) results)
    (if (equal (first results) (first test))
        (format t "    as expected.~%")
        (format t "    BUT EXPECTED: ~S~%" (first test)))))





