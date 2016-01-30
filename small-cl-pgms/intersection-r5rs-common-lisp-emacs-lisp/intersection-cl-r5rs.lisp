;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               intersection-cl-r5rs.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An intersection of R5RS and Common Lisp.
;;;;
;;;;    This is a Common-Lisp program that let one load scheme
;;;;    programs using only operators in the intersection with Common
;;;;    Lisp.
;;;;
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2009-07-26 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2009 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(cl:in-package :cl-user)

(defpackage "INTERSECTION-CL-R5RS"
  (:use "COMMON-LISP")
  (:export "QUOTE" "LAMBDA" "IF" "COND" "CASE"
           "AND" "OR" "NOT"
           "LET" "LET*" "DO"
           "=" "<" ">" "<=" ">="
           "MAX" "MIN"
           "+" "*" "-" "/"
           "FLOOR" "CEILING" "TRUNCATE" "ROUND"
           "RATIONALIZE"
           "EXP" "LOG" "SIN" "COS" "TAN" "ASIN" "ACOS" "ATAN" "SQRT" "EXPT"
           "GCD" "LCM"
           "NUMERATOR" "DENOMINATOR"
           "CONS"
           "CAR" "CDR"
           "CAAR" "CADR" "CDAR" "CDDR"
           "CAAAR" "CAADR" "CADAR" "CADDR" "CDAAR" "CDADR" "CDDAR" "CDDDR"
           "CAAAAR" "CAAADR" "CAADAR" "CAADDR" "CADAAR" "CADADR" "CADDAR" "CADDDR"
           "CDAAAR" "CDAADR" "CDADAR" "CDADDR" "CDDAAR" "CDDADR" "CDDDAR" "CDDDDR"
           "LIST" "LENGTH" "APPEND" "REVERSE"
           "CHAR-UPCASE" "CHAR-DOWNCASE"
           "MAKE-STRING"
           "VECTOR"
           "APPLY"
           "READ" "READ-CHAR" "PEEK-CHAR"
           "WRITE" "WRITE-CHAR"
           "LOAD")
  (:documentation "

This package exports the operators from COMMON-LISP that exist also in
R5RS scheme, with the same behavior (for some subset of the parameter
ranges).  If we take care of restricting our scheme, then we can write
code that can be evaluated with the same semantics both in scheme and
in Common Lisp.

It may be tried out in the INTERSECTION-CL-R5RS.USER package.

For examples:

    COND is CL:COND therefore we cannot use ELSE; instead write a true
    conditions, like (= 0 0).

    CASE is CL:CASE therefore we cannot use ELSE.

    LAMBDA is CL:LAMBDA therefore we cannot use a symbol or a dotted list for
    the lambda-list. On the other hand, CL:lambda also accepts
    extended parameter lists that scheme programs must refrain from
    using.

    MAKE-STRING is CL:MAKE-STRING that takes keyword arguments.
    Scheme programs must call it with only one argument and cannot
    pass an initial element.

    IF and COND are CL:IF and CL:COND and take only CL:NIL as
    false. Therefore scheme programs must restrict themselves to the
    available predicates as test expressions, and must avoid to give
    the empty list.

    While DEFINE and LETREC are missing, we can write:
      (let ((fact (lambda (fact x)
                     (if (< x 1)
                         1
                         (* x (apply fact fact (- x 1) '()))))))
         (apply fact fact 20 '()))
      --> 2432902008176640000

Note: are direly missing DEFINE, LETREC, NULL? and other predicates,
      See CL-R5RS-LIBRARY for a more complete subset of scheme.
"))


(defpackage "INTERSECTION-CL-R5RS.USER"
  (:use "INTERSECTION-CL-R5RS"))


(defpackage "INTERSECTION-CL-R5RS.LIBRARY"
  (:use "COMMON-LISP")
  (:shadow "IF" "COND" "CASE"
           "AND" "OR" "NOT"
           "=" "<" ">" "<=" ">="
           "MEMBER" "ASSOC" "STRING" "MAKE-STRING" "MAP" "EVAL"
           "READ" "READ-CHAR" "PEEK-CHAR"
           "WRITE" "WRITE-CHAR")
  (:export "QUOTE" "LAMBDA" "IF" "COND" "CASE"
           "AND" "OR" "NOT"
           "LET" "LET*" "DO"
           "=" "<" ">" "<=" ">="
           "MAX" "MIN"
           "+" "*" "-" "/"
           "FLOOR" "CEILING" "TRUNCATE" "ROUND"
           "RATIONALIZE"
           "EXP" "LOG" "SIN" "COS" "TAN" "ASIN" "ACOS" "ATAN" "SQRT" "EXPT"
           "GCD" "LCM"
           "NUMERATOR" "DENOMINATOR"
           "CONS"
           "CAR" "CDR"
           "CAAR" "CADR" "CDAR" "CDDR"
           "CAAAR" "CAADR" "CADAR" "CADDR" "CDAAR" "CDADR" "CDDAR" "CDDDR"
           "CAAAAR" "CAAADR" "CAADAR" "CAADDR" "CADAAR" "CADADR" "CADDAR" "CADDDR"
           "CDAAAR" "CDAADR" "CDADAR" "CDADDR" "CDDAAR" "CDDADR" "CDDDAR" "CDDDDR"
           "LIST" "LENGTH" "APPEND" "REVERSE"
           "CHAR-UPCASE" "CHAR-DOWNCASE"
           "MAKE-STRING"
           "VECTOR"
           "APPLY"
           "READ" "READ-CHAR" "PEEK-CHAR"
           "WRITE" "WRITE-CHAR"
           "LOAD"

           "SET!" "ELSE" "LETREC" "BEGIN" "DEFINE"

           "EQV?" "EQ?" "EQUAL?"
           "INTEGER?" "RATIONAL?" "REAL?" "COMPLEX?" "NUMBER?"
           "EXACT?" "INEXACT?"
           "ZERO?" "POSITIVE?" "NEGATIVE?" "ODD?" "EVEN?"
           "MAKE-RECTANGULAR" "MAKE-POLAR" "REAL-PART" "IMAG-PART" "MAGNITUDE" "ANGLE"
           "INEXACT->EXACT" "EXACT->INEXACT"
           "STRING->NUMBER" "NUMBER->STRING"
           "PAIR?" "SET-CAR!" "SET-CDR!" "LIST-TAIL" "LIST-REF"
           "MEMQ" "MEMV" "MEMBER" "ASSQ" "ASSV" "ASSOC"

           "SYMBOL?" "SYMBOL->STRING" "STRING->SYMBOL"
           
           "CHAR?"
           "CHAR=?" "CHAR<?" "CHAR>?" "CHAR<=?" "CHAR>=?" 
           "CHAR-CI=?" "CHAR-CI<?" "CHAR-CI>?" "CHAR-CI<=?" "CHAR-CI>=?"
           "CHAR-ALPHABETIC?" "CHAR-NUMERIC?" "CHAR-WHITESPACE?"
           "CHAR-UPPER-CASE?" "CHAR-LOWER-CASE?" "CHAR->INTEGER" "INTEGER->CHAR"
           "CHAR-UPCASE" "CHAR-DOWNCASE"

           "STRING?" "STRING" "STRING-LENGTH" "STRING-REF" "STRING-SET!"
           "STRING=?" "STRING<?" "STRING>?" "STRING<=?" "STRING>=?" 
           "STRING-CI=?" "STRING-CI<?" "STRING-CI>?" "STRING-CI<=?" "STRING-CI>=?"
           "SUBSTRING" "STRING-APPEND" "STRING->LIST" "LIST->STRING" "STRING-COPY"
           "STRING-FILL!"

           "VECTOR?" "MAKE-VECTOR" "VECTOR-LENGTH" "VECTOR-REF" "VECTOR-SET!"
           "VECTOR->LIST" "LIST->VECTOR" "VECTOR-FILL!"

           "PROCEDURE?" "MAP" "FOR-EACH" "DYNAMIC-WIND"
           "NULL-ENVIRONMENT" "SCHEME-REPORT-ENVIRONMENT" "INTERACTION-ENVIRONMENT" "EVAL"

           "CALL-WITH-INPUT-FILE" "INPUT-PORT?" "CURRENT-INPUT-PORT" "WITH-INPUT-FROM-FILE"
           "OPEN-INPUT-FILE" "CLOSE-INPUT-PORT"
           "CALL-WITH-OUTPUT-FILE" "OUTPUT-PORT?" "CURRENT-OUTPUT-PORT" "WITH-OUTPUT-FROM-FILE"
           "OPEN-OUTPUT-FILE" "CLOSE-OUTPUT-PORT"

           "EOF-OBJECT?" "CHAR-READY?"
           "DISPLAY" "NEWLINE"
           "TRANSCRIPT-ON" "TRANSCRIPT-OFF"
           
           ))

(defpackage "INTERSECTION-CL-R5RS.LIBRARY.USER"
  (:use "INTERSECTION-CL-R5RS.LIBRARY"))

(cl:in-package "INTERSECTION-CL-R5RS.LIBRARY")

(defstruct false) (defvar +false+ (make-false))
(defstruct true)  (defvar +true+  (make-true))
(defstruct eof)   (defvar +eof+   (make-eof))

(declaim (inline clbool scbool))
(defun clbool (scheme-boolean) (cl:not (eq +false+ scheme-boolean)))
(defun scbool (lisp-boolean)   (cl:if lisp-boolean +true+ +false+))

;; This is not correct, the value of the last expression must be returned, not its clbool:
(defmacro and (&rest arguments)
  (cl:cond
    ((null arguments)       '+true+)
    ((null (cdr arguments)) (car arguments))
    (t `(if ,(car arguments) (and ,@(cdr arguments)) +false+))))

(defmacro or (&rest arguments)
  (cl:cond
    ((null arguments)       '+false+)
    (t (let ((result (gensym)))
         `(let ((,result ,(car arguments)))
            (if ,result ,result (or ,@(cdr arguments))))))))

(defun    not (lisp-boolean)          (scbool (eq +false+ lisp-boolean)))


(defmacro set! (variable expression)
  `(setq ,variable ,expression))

(defmacro if (test then &optional else)
  `(cl:if (clbool ,test) ,then ,else))

(defmacro cond (&rest clauses)
  (let* ((temp (gensym))
         (temp-used nil)
         (form
           `(cl:cond
              ,@(mapcar
                 (lambda (clause)
                   (assert (and clause (listp clause)) ()
                           "COND clauses must be list containing at least a test expression. ~S is invalid.")
                   (cl:cond
                     ((eq 'else (first clause))
                      `(t ,@(rest clause)))
                     ((eq '=>   (second clause))
                      (setf temp-used t)
                      `((clbool  (setf ,temp  ,(first clause))) (funcall ,(third clause) ,temp)))
                     (t
                      `((clbool ,(first clause)) ,@(rest clause)))))
                 clauses))))
    (if temp-used
        `(let ((,temp)) ,form)
        form)))

(defmacro case (&rest clauses)
  `(cl:case
       ,@(mapcar
          (lambda (clause)
            (assert (and clause (listp clause)
                         (or (eq 'else (car clause))
                             (listp (car clause))))
                    ()
                    "CASE clauses must be list containing at least a test expression. ~S is invalid.")
            (cl:cond
              ((eq 'else (first clause))
               `(t ,@(rest clause)))
              (t
               clause)))
          clauses)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clparameters (parameters)
    (etypecase parameters
      (symbol `(&rest ,parameters))
      (list    (let ((last-cons (last parameters)))
                 (if (symbolp (cdr last-cons))
                     (append (butlast parameters) (list (car last-cons) '&rest (cdr last-cons)))
                     parameters))))))

(defmacro letrec (definitions &body body)
  (loop
     :for (name value) :in definitions
     :if (and (listp value) (eq 'lambda (first value)))
     :collect (destructuring-bind (lambda parameters &body body) value
                (declare (ignore lambda))
                `(,name ,(clparameters parameters) ,@body)) :into funs
     :else
     :collect `(,name ,value) :into vars
     :finally  (return `(let ,vars
                          (labels ,funs
                            ,@body)))))


(defmacro begin (&body body) `(progn ,@body))

(defmacro define-lexical-global (variable expression)
  (let ((global (gensym (cl:string variable))))
   `(progn
      (define-symbol-macro ,variable (symbol-value ',global))
      (setf ,variable ,expression)
      ',variable)))

(defmacro define (variable expression)
  (cl:if (cl:and (listp expression)  (eq 'lambda (first expression)))
      `(progn
         (defun ,variable ,@(rest expression))
         (define-lexical-global ,variable (function ,variable)))
      `(define-lexical-global ,variable ,expression)))


(defun eqv? (a b)
  (or (scbool (eql a b))
      ;; (cl:and (eq a +true+)  (eq b +true+))
      ;; (cl:and (eq a +false+) (eq b +false+))
      ;; (and (null? a) (null? b))
      (and (symbol? a) (symbol? b)
           (interned? a) (interned? b)
           (string=? (symbol->string a) (symbol->string b)))
      (and (number? a) (number? b) (= a b))
      (and (char? a) (char? b) (char=? a b))
      +false+))

(defun eq? (a b) (eqv? a b))

(defun equal? (a b)
  (or (eqv? a b)
      (and (pair? a) (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (and (vector? a) (vector? b)
           (cl:= (length a) (length b))
           (every (lambda (a b) (clbool (equal? a b))) a b))
      (and (string? a) (string? b)
           (string=? a b))))

(defun integer?  (a) (scbool (integerp a)))
(defun rational? (a) (scbool (rationalp a)))
(defun real?     (a) (scbool (realp a)))
(defun complex?  (a) (scbool (cl:or (realp a) (complexp a))))
(defun number?   (a) (scbool (numberp a)))

(defun exact?    (a) (scbool (cl:or (rationalp a)
                                    (and (complexp a)
                                         (rationalp (realpart a))
                                         (rationalp (imagpart a))))))
(defun inexat? (a) (not (exact? a)))
(defun =  (&rest arguments) (scbool (apply (function cl:=)  arguments)))
(defun <  (&rest arguments) (scbool (apply (function cl:<)  arguments)))
(defun >  (&rest arguments) (scbool (apply (function cl:>)  arguments)))
(defun <= (&rest arguments) (scbool (apply (function cl:<=) arguments)))
(defun >= (&rest arguments) (scbool (apply (function cl:>=) arguments)))

(defun zero?     (a) (scbool (zerop  a)))
(defun positive? (a) (scbool (plusp  a)))
(defun negative? (a) (scbool (minusp a)))
(defun odd?      (a) (scbool (oddp   a)))
(defun even?     (a) (scbool (evenp  a)))

(defun quotient  (a b) (values (truncate a b)))
(defun remainder (a b) (rem a b))
(defun modulo    (a b) (mod a b))


(defun make-rectangular (a b) (complex a b))
(defun make-polar (r a) (* r (cis a)))
(defun real-part (a) (realpart a))
(defun imag-part (a) (imagpart a))
(defun magnitude (a) (abs a))
(defun angle     (a) (phase a))

(defun exact->inexact (a) (+ 0.0 a))
(defun inexact->exact (a) (values (round a)))


(defun number->string (obj &optional (base 10.))
  (check-type obj number)
  (write-to-string obj 
                   :array nil :base base :case :downcase :circle t
                   :escape t :gensym t :length nil :level nil :lines nil
                   :miser-width nil  :pretty nil
                   :radix t :readably t :right-margin nil))


(defun string->number (s &optional (radix 10.))
  (let ((*read-eval* nil)
        (*read-base* radix)
        (*read-suppress* nil))
    (let ((result (read-from-string s)))
      (check-type result number)
      result)))

(defun pair? (a) (scbool (consp a)))
(defun null? (a) (scbool (null a)))
(defun set-car! (pair obj) (setf (car pair) obj))
(defun set-cdr! (pair obj) (setf (cdr pair) obj))
(defun list-tail (list k) (nthcdr k list))
(defun list-ref (list k) (nth k list))

(defun memq   (obj list) (cl:member obj list :test (lambda (a b) (clbool (eq?    a b)))))
(defun memv   (obj list) (cl:member obj list :test (lambda (a b) (clbool (eqv?   a b)))))
(defun member (obj list) (cl:member obj list :test (lambda (a b) (clbool (equal? a b)))))

(defun assq   (obj list) (cl:assoc obj list :test (lambda (a b) (clbool (eq?    a b)))))
(defun assv   (obj list) (cl:assoc obj list :test (lambda (a b) (clbool (eqv?   a b)))))
(defun assoc  (obj list) (cl:assoc obj list :test (lambda (a b) (clbool (equal? a b)))))

(defun symbol?   (obj) (scbool (symbolp obj)))
(defun interned? (obj) (scbool (cl:and (symbolp obj) (symbol-package obj))))
(defun symbol->string (sym) (symbol-name sym))
(defun string->symbol (name) (intern name))


(defun char?      (obj) (scbool (characterp obj)))
(defun char=?     (a b) (scbool (char=  a b)))
(defun char<?     (a b) (scbool (char<  a b)))
(defun char>?     (a b) (scbool (char>  a b)))
(defun char<=?    (a b) (scbool (char<= a b)))
(defun char>=?    (a b) (scbool (char>= a b)))
(defun char-ci=?  (a b) (scbool (char-equal    a b)))
(defun char-ci<?  (a b) (scbool (char-lessp    a b)))
(defun char-ci>?  (a b) (scbool (char-greaterp a b)))
(defun char-ci<=? (a b) (scbool (cl:not (char-greaterp a b))))
(defun char-ci>=? (a b) (scbool (cl:not (char-lessp    a b))))
(defun char-alphabetic? (a) (scbool (alpha-char-p a)))
(defun char-numeric?    (a) (scbool (digit-char a)))
(defun char-whitespace? (a) (scbool (find a #(#\space #\tab #\newline #\vt #\page #\return))))
(defun char-upper-case? (a) (char=? a (char-upcase   a)))
(defun char-lower-case? (a) (char=? a (char-downcase a)))
(defun char->integer    (a) (char-code a))
(defun integer->char    (a) (code-char a))

(defun string? (obj) (scbool (stringp obj)))
(defun make-string (k &optional (char #\space)) (cl:make-string k :initial-element char))
(defun string (&rest characters) (coerce characters 'cl:string))
(defun string-length (string) (length string))
(defun string-ref (string k) (aref string k))
(defun string-set! (string k char) (setf (aref string k) char))

(defun string=?     (a b) (check-type a cl:string) (check-type b cl:string) (scbool (string=  a b)))
(defun string<?     (a b) (check-type a cl:string) (check-type b cl:string) (scbool (string<  a b)))
(defun string>?     (a b) (check-type a cl:string) (check-type b cl:string) (scbool (string>  a b)))
(defun string<=?    (a b) (check-type a cl:string) (check-type b cl:string) (scbool (string<= a b)))
(defun string>=?    (a b) (check-type a cl:string) (check-type b cl:string) (scbool (string>= a b)))
(defun string-ci=?  (a b) (check-type a cl:string) (check-type b cl:string) (scbool (string-equal    a b)))
(defun string-ci<?  (a b) (check-type a cl:string) (check-type b cl:string) (scbool (string-lessp    a b)))
(defun string-ci>?  (a b) (check-type a cl:string) (check-type b cl:string) (scbool (string-greaterp a b)))
(defun string-ci<=? (a b) (check-type a cl:string) (check-type b cl:string) (scbool (cl:not (string-greaterp a b))))
(defun string-ci>=? (a b) (check-type a cl:string) (check-type b cl:string) (scbool (cl:not (string-lessp    a b))))

(defun substring (string start end) (subseq string start end))
(defun string-append (&rest strings) (apply (function concatenate) 'cl:string strings))
(defun string->list (string) (coerce string 'list))
(defun list->string (list) (coerce list 'cl:string))
(defun string-copy (string) (copy-seq string))
(defun string-fill! (string char) (fill string char))

(defun vector? (obj) (scbool (cl:and (vectorp obj) (cl:not (stringp obj)))))
(defun make-vector (k &optional fill) (make-array k :initial-element fill))
(defun vector-length (vector) (length vector))
(defun vector-ref (vector k) (aref vector k))
(defun vector-set! (vector k obj) (setf (aref vector k) obj))
(defun vector->list (vector) (coerce vector 'list))
(defun list->vector (list) (coerce list 'vector))
(defun vector-fill! (vector fill) (fill vector fill))

(defun procedure? (obj) (scbool (functionp obj)))
(defun map (proc &rest lists) (apply (function cl:map) 'list proc lists))
(defun for-each (proc &rest lists) (apply (function mapc)  proc lists))

(defun dynamic-wind (before thunk after)
  (unwind-protect
       (progn (funcall before)
              (funcall thunk))
    (funcall after)))

(defun null-environment          (version) (declare (ignore version)) nil)
(defun scheme-report-environment (version) (declare (ignore version)) nil)
(defun interaction-environment   (version) (declare (ignore version)) nil)
(defun eval (expression environment-specifier)
  (declare (ignore environment-specifier))
  (cl:eval expression))


(defun call-with-input-file (string proc)
  (with-open-file (stream string)
    (funcall proc stream)))

(defun call-with-output-file (string proc)
  (with-open-file (stream string
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (funcall proc stream)))

(defun input-port?  (obj) (and (streamp obj)) (INPUT-STREAM-P  obj))
(defun output-port? (obj) (and (streamp obj)) (OUTPUT-STREAM-P obj))
(defun current-input-port  () *standard-input*)
(defun current-output-port () *standard-output*)
(defun with-input-from-file (string thunk)
  (call-with-input-file string (lambda (stream)
                                 (let ((*standard-input* stream))
                                   (funcall thunk)))))
(defun with-output-from-file (string thunk)
  (call-with-output-file string (lambda (stream)
                                  (let ((*standard-output* stream))
                                    (funcall thunk)))))
(defun open-input-file  (filename) (open filename))
(defun open-output-file (filename) (open filename 
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create))

(defun close-input-port  (port) (close port))
(defun close-output-port (port) (close port))
(defun read      (&optional (port *standard-input*)) (cl:read port nil +eof+))
(defun read-char (&optional (port *standard-input*)) (cl:read-char port nil +eof+))
(defun peek-char (&optional (port *standard-input*)) (cl:peek-char nil port nil +eof+))
(defun eof-object? (obj) (scbool (eq obj +eof+)))
(defun char-ready? (&optional (port *standard-input*))
  (scbool (listen port)))

(defun write (obj &optional (port *standard-output*))
  (cl:write obj :stream port
            :array t :base 10. :case :downcase :circle t
            :escape t :gensym t :length nil :level nil :lines nil
            :miser-width nil  :pretty nil
            :radix t :readably t :right-margin nil))

(defun display (obj &optional (port *standard-output*))
  (cl:write obj :stream port
            :array t :base 10. :case :downcase :circle nil
            :escape nil :gensym nil :length nil :level nil :lines nil
            :miser-width nil  :pretty t
            :radix nil :readably nil :right-margin nil))

(defun newline (&optional (port *standard-output*))
  (terpri port))

(defun write-char (char &optional (port *standard-output*))
  (write-char char port))

(defun transcript-on  (filename) (dribble filename))
(defun transcript-off ()         (dribble))




(cl:in-package "INTERSECTION-CL-R5RS.USER")

(let* ((a 1)
       (b 4)
       (c 2))
  (if (= 0 a)
      (write "a must be non zero")
      (let ((d (- (* b b) (* 4 a c))))
        (cond
          ((< d 0)
           (write "No real solution")
           '())
          ((= d 0)
           (let ((sol (/ (- b) 2 a)))
             (write "One real solution ")
             (write sol)
             (list sol)))
          ((> d 0)
           (let ((s1 (/ (+ (- b) (sqrt d)) 2 a))
                 (s2 (/ (- (- b) (sqrt d)) 2 a)))
             (write "Two real solutions ")
             (write s1)
             (write " ")
             (write s2)
             (list s1 s2)))))))

;; (enter three numbers)


(cl:in-package "INTERSECTION-CL-R5RS.LIBRARY.USER")


(define a 3)
(begin
 (let ((a 1))
   (newline)
   (display a) (display " ")
   (set! a 2)
   (display a) (display " "))
 (display a)
 (newline)
 '())


(write '(cl:in-package "INTERSECTION-CL-R5RS.LIBRARY.USER"))

;;;; THE END ;;;;


