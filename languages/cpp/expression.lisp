;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               expression.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Parses and evaluates cpp #if expressions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.CPP")

(defun integer-value (integer-token)
  (let* ((integer-text (token-text integer-token))
         (end          (length integer-text)))
    (loop :while (and (< 1 end) (find (aref integer-text (1- end)) "UL" :test (function char-equal)))
          :do (decf end))
    (cond
      ((and (< 2 (length integer-text))
            (string= "0x" integer-text :start2 0 :end2 2))
       (or (ignore-errors (parse-integer integer-text :start 2 :end end :radix 16 :junk-allowed nil))
           (progn
             (cpp-error integer-token "Invalid hexadecimal integer syntax in ~S" integer-text)
             0)))
      ((and (< 2 (length integer-text))
            (string= "0b" integer-text :start2 0 :end2 2))
       (or (ignore-errors (parse-integer integer-text :start 2  :end end :radix 2 :junk-allowed nil))
           (progn
             (cpp-error integer-token "Invalid binary integer syntax in ~S" integer-text)
             0)))
      ((and (< 1 (length integer-text))
            (string= "0" integer-text :start2 0 :end2 1))
       (or (ignore-errors (parse-integer integer-text :start 1  :end end :radix 8 :junk-allowed nil))
           (progn
             (cpp-error integer-token "Invalid octal integer syntax in ~S" integer-text)
             0)))
      (t
       (or (ignore-errors (parse-integer integer-text  :end end :junk-allowed nil))
           (progn
             (cpp-error integer-token "Invalid decimal integer syntax in ~S" integer-text)
             0))))))

(defun character-value (character-token)
  (let* ((character-text (token-text character-token))
         (character-string
           (with-input-from-string (in character-text :start 1)
             (read-c-string in #\'))))
    (unless (= 1 (length character-string))
      (cpp-error character-token "Invalid multi-byte character literal ~A" character-text)
      (when (zerop (length character-string))
        (setf character-string #(#\nul))))
    (char-code (aref character-string 0))))

(defun string-value (string-token)
  (with-input-from-string (in (token-text string-token) :start 1)
    (read-c-string in #\")))

(defmacro with-binary-op-parsers ((&rest definitions) (&rest functions) &body body)
  `(labels (,@(mapcar (lambda (definition)
                        (destructuring-bind (name subexpr &rest ops) definition
                          `(,name () (loop :with expr := (,subexpr)
                                           :while ,(if (= 1 (length ops))
                                                       `(,(first (first ops)) (peek))
                                                       `(let ((next (peek)))
                                                          (some (lambda (pred) (funcall pred next))
                                                                ',(mapcar (function first) ops))))
                                           :do (setf expr (list ,(if (= 1 (length ops))
                                                                     `(progn (eat) ',(second (first ops)))
                                                                     `(let ((op (eat)))
                                                                        (cond
                                                                          ,@(mapcar (lambda (pair)
                                                                                      `((,(first pair) op) (quote ,(second pair))))
                                                                                    ops))))
                                                                expr (,subexpr)))
                                           :finally (return expr)))))
                definitions)
            ,@functions)
     ,@body))

  #|

  integer constant
  character constant (interpreted as in C, as numbers)
  arithmetic operators + - * / % ^ & | ~  << >>  < <= > >= != ==  && || !
  macros are expanded
  defined(macro-name)
  _Pragma("pragma")
  identifiers = 0 (warning on -Wunder)

  calculations in the widest integer type known to the compiler; on most machines supported by GCC this is 64 bits

  operator precedence:
  2 + - ! ~ unary
  3 * / %
  4 + - binary
  5 <<>>
  6 < <= > >=
  7 == !=
  8 &
  9 ^
  10 |
  11 &&
  12 ||
  
  expr ::= expr12 .
  expr12 ::= expr11 [ '||' expr12 ] .
  expr11 ::= expr10 [ '&&' expr11 ] .
  expr10 ::= expr9 [ '|' expr10 ] .
  expr9 ::= expr8 [ '^' expr9 ] .
  expr8 ::= expr7 [ '&' expr8 ] .
  expr7 ::= expr6 [ ( '==' | '!=' ) expr7 ] .
  expr6 ::= expr5 [ ( '<' | '<=' | '>' | '>=' ) expr6 ] .
  expr5 ::= expr4 [ ( '<<' | '>>' ) expr5 ] .
  expr4 ::= expr3 [ ( '+' | '-' ) expr4 ] .
  expr3 ::= expr2 [ ( '*' | '/' | '%' ) expr3 ] .
  expr2 ::= [ '+' | '-' | '!' | '~' ] expr1 .
  expr1 ::= '_Pragma' '(' string ')'
          | 'defined' '(' identifier ')'
          | '(' expr12 ')'
          | integer | character | identifier .

  associativity of binary operations: left-to-right.

  |#

(defmacro cpp-and (a b)
  `(if (and (not (zerop ,a)) (not (zerop ,b)))
       1 0))

(defmacro cpp-or (a b)
  `(if (or (not (zerop ,a)) (not (zerop ,b)))
       1 0))

(defun cpp-not (a)   (if (zerop a) 1 0))
(defun cpp-<   (a b) (if (<  a b)  1 0))
(defun cpp-<=  (a b) (if (<= a b)  1 0))
(defun cpp->   (a b) (if (>  a b)  1 0))
(defun cpp->=  (a b) (if (>= a b)  1 0))
(defun cpp-=   (a b) (if (=  a b)  1 0))
(defun cpp-/=  (a b) (if (/= a b)  1 0))

(defun left-shift  (value offset)
  (ash value offset))

(defun right-shift (value offset)
  (ash value (- offset)))

(defgeneric parse-expression (context line))
(defmethod parse-expression ((context context) line)
  (with-binary-op-parsers ((expr12 expr11 (op-logior-p cpp-or))
                           (expr11 expr10 (op-logand-p cpp-and))
                           (expr10 expr9  (op-bitior-p logior))
                           (expr9  expr8  (op-bitxor-p logxor))
                           (expr8  expr7  (op-bitand-p logand))
                           (expr7  expr6  (op-eq-p cpp-=) (op-ne-p cpp-/=))
                           (expr6  expr5  (op-lt-p cpp-<) (op-le-p cpp-<=) (op-gt-p cpp->) (op-ge-p cpp->=))
                           (expr5  expr4  (op-left-shift-p left-shift) (op-right-shift-p right-shift))
                           (expr4  expr3  (op-plus-p +) (op-minus-p -))
                           (expr3  expr2  (op-times-p *) (op-divides-p truncate)  (op-remainder-p mod)))
    ((expr13 ()
             (let ((test (expr12)))
               (if (op-question-p (peek))
                   (progn
                     (eat)
                     (let ((then (expr12)))
                       (if (op-colon-p (peek))
                           (progn
                             (eat)
                             `(if (zerop ,test) ,(expr12) ,then))
                           (cpp-error (peek) "Expected a colon in ternary if expression, got ~S instead" (token-text (eat))))))
                   test)))
     (expr2 ()
            (let ((op (peek)))
              (cond ((op-plus-p op)   (eat) (expr1))
                    ((op-minus-p op)  (eat) `(- ,(expr1)))
                    ((op-lognot-p op) (eat) `(cpp-not ,(expr1)))
                    ((op-bitnot-p op) (eat) `(lognot ,(expr1)))
                    (t                      (expr1)))))
     (expr1 ()
            ;; expr2 ::= '_Pragma' '(' string ')'
            ;;         | 'defined' '(' identifier ')'
            ;;         | '(' expr12 ')'
            ;;         | identifier [ '(' arguments… ')' ]
            ;;         | integer
            ;;         | character .
            (let ((next (peek)))
              (cond
                ((openp next)
                 (eat)
                 (prog1 (expr13)
                   (if (closep (peek))
                       (eat)
                       (progn (cpp-error next "Missing close parenthesis in #if expression, got ~S instead" (token-text (eat)))
                              (return-from parse-expression 0)))))
                ((number-p next)
                 (integer-value (eat)))
                ((character-literal-p next)
                 (character-value (token-text (eat))))
                ((identifierp next)
                 (let ((identifier (eat)))
                   (scase (token-text identifier)
                          (("_Pragma")
                           (cpp-error identifier "_Pragma is forbidden in #if expressions")
                           0)
                          (otherwise ;; we've already macroexpanded
                           (when (option context :warn-on-undefined-identifier)
                             (cpp-warning identifier "~S is not defined" (token-text identifier)))
                           0))))
                (t (if next
                       (cpp-error next "token ~S is not valid in preprocessor expressions" (token-text next))
                       (cpp-error context "end of line reached before the end of the preprocessor expressions"))
                   (return-from parse-expression 0)))))
     (eat  () (pop   line))
     (peek () (first line)))
    (prog1 (expr13)
      (unless (null (peek))
        (cpp-error (peek) "missing binary operator before token ~S" (eat))))))



(defun test/integer-value ()
  (assert (equal (mapcar (function integer-value)
                         (list (make-number "42"          0 0 "-")
                               (make-number "0x42"        0 0 "-")
                               (make-number "0b101010"    0 0 "-")
                               (make-number "042"         0 0 "-")))
                 '(42 66 42 34)))
  :success)

(defun test/character-value ()
  (assert (equal (mapcar (function character-value)
                         (list (make-character-literal "'A'"      0 0 "-")
                               (make-character-literal "'\\x41'"  0 0 "-")
                               (make-character-literal "'\\n'"    0 0 "-")
                               (make-character-literal "'λ'"      0 0 "-")))
                 
                 '(65 65 10 955)))
  :success)

;;;; THE END ;;;;
