;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               linc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    LINC Is Not C, but almost.
;;;;
;;;;    The purpose is to be able to use Common Lisp
;;;;    at the meta-programming level to generate C sources.
;;;;    Linc programs can also be executed and debugged in the
;;;;    Common Lisp environment.
;;;;
;;;;    A linc file contains normal Common Lisp expressions,
;;;;    and linc expressions.  When compiling the linc file,
;;;;    the Common Lisp expressions are executed, which will
;;;;    generate a corresponding C source.
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-09-02 <PJB> Partial generation of expressions and statements.
;;;;    2005-10-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2005 - 2016
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
;;;;****************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")



(defparameter *name-map* (make-hash-table :test (function equal)))


(defgeneric generate-expression (expression))
(defgeneric generate-statement (expression &key same-line))
(defgeneric generate-identifier (expression))

(defun generate-parameter (parm)
  (generate-expression (second parm))
  (emit " ")
  (generate-identifier (first parm))
  (when (cddr parm)
    (emit "=")
    (generate-expression (third parm))))

(defun generate-definition (def)
  (ecase (first def)
    ((com.informatimago.languages.linc.c::defun)
     (destructuring-bind (name arguments result-type &rest body) (rest def)
       (emit :newline)
       (generate-expression result-type)
       (emit " ")
       (generate-identifier name)
       (emit "(")
       (when arguments
         (loop
            :for arg :in (rest arguments)
            :initially (generate-parameter (first arguments))
            :do (emit ",") (generate-parameter arg)))
       (emit ")")
       (generate-statement
        (if (and (null (cdr body))
                 (member (caar body) '(com.informatimago.languages.linc.c::let com.informatimago.languages.linc.c::block)))
            (first body)
            `(com.informatimago.languages.linc.c::block ,@body)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun pcond-variable-p (pattern)
    (and (symbolp pattern)
         (<= 1 (length (string pattern)))
         (char= #\? (aref (string pattern) 0))))

  (defun pcond-substitute-literals (pattern)
    "
DO:      Renames any atom found in pattern, collecting them in two
         binding a-list, one for the literals, another for the
         variables.
RETURN:  A destructuring-lambda-list; a literal a-list ; a variable a-list.
"
    (cond
      ((consp pattern)
       (multiple-value-bind (adll asal aval) (pcond-substitute-literals (car pattern))
         (multiple-value-bind (ddll dsal dval) (pcond-substitute-literals (cdr pattern))
           (values (cons adll ddll) (nconc asal dsal) (nconc aval dval)))))
      ((find pattern LAMBDA-LIST-KEYWORDS)
       (values pattern nil))
      ((pcond-variable-p pattern)
       (let ((var (gensym)))
         (values var nil (list (cons var pattern)))))
      ((null pattern)
       (values nil nil nil))
      (t
       (let ((var (gensym)))
         (values var (list (cons var pattern)) nil))))))

(defmacro pcond (expression &rest clauses)
  ;; The pattern variable are declared ignorable since depending on
  ;; the compiler, they may or may not used by potentially dead code.
  (let ((vexpression (gensym)))
    `(let ((,vexpression ,expression))
       (cond
         ,@(mapcar
            (lambda (clause)
              (multiple-value-bind (docstrings declarations body) (parse-body :locally (rest clause))
                (declare (ignore docstrings))
                (multiple-value-bind (dll sal val) (pcond-substitute-literals (first clause))
                  (let ((variables (mapcar (function cdr) val)))
                    `((let ,variables
                        (declare (ignorable ,@variables))
                        ,@declarations
                        (when (ignore-errors
                                (destructuring-bind ,dll ,vexpression
                                  (when (and
                                         ,@(mapcar
                                            (lambda (binding)
                                              `(equal ,(car binding) ',(cdr binding)))
                                            sal))
                                    (setf ,@(mapcan
                                             (lambda (binding)
                                               (list (cdr binding) (car binding)))
                                             val))
                                    t)))
                          ,@body)))))))
            clauses)))))

(defun generate-type (expression &key name)

  (ecase (first expression)

    ((com.informatimago.languages.linc.c::class
      com.informatimago.languages.linc.c::struct
      com.informatimago.languages.linc.c::union)
     (emit (format nil "~(~A~)" (first expression)))
     (cond
       ((listp (second expression))
        (when name
          (emit " ")
          (generate name))

        ;; (class (superclass...) :public|:protected|:private member...)
        ;; superclass ::= (classname [:virtual] [:public|:protected|:private])
        ;; superclass ::= classname

        (when (second expression)
          (emit ":")
          (generate-list ","
                         (lambda (superclass)
                             (if (listp superclass)
                               (case (length superclass)
                                 ((1) (generate (first superclass)))
                                 ((2 3)
                                  (emit (format nil "~(~{~A~^ ~}~)" (rest superclass)))
                                  (generate (first superclass)))
                                 (otherwise
                                  (error "Invalid syntax for a superclass: ~S" superclass)))
                               (generate superclass)))
                         (second expression)))
        (emit :fresh-line)
        (with-parens "{}"
          (dolist (member (rest (rest expression)))
            (if (member member '(:public :protected :private))
              (emit :fresh-line (format nil "~(~A~):" member) :newline)
              (generate member)))))
       (t
        (error "Not implemented yet, generation of type ~S" expression))))

    ((com.informatimago.languages.linc.c::enum)
     ;; (enum (blue 1) white red (yellow 10))
     (emit "enum")
     (when name
       (emit " ")
       (generate name))
     (with-parens "{}"
       (dolist (item (rest expression))
         (if (listp item)
           (case (length item)
             ((1)
              (generate (first item)) )
             ((2)
              (generate (first item)) (emit "=") (generate (second item)))
             (otherwise
              (error "Invalid syntax for an enum constant: ~S" item)))
           (generate item))
         (emit "," :newline))))))

(defun generate-declaration (?declaration)
  (pcond ?declaration

    ((ASM ?string)
     (emit "asm")
     (with-parens "()" (generate-expression ?string))
     (emit ";" :newline))

    ((NAMESPACE ?identifier &body ?namespace-body)
     (emit "namespace")
     (when ?identifier
       (emit " ")
       (generate-expression ?identifier))
     (with-parens "{}"
       (dolist (?declaration ?namespace-body)
         (generate-declaration ?declaration)))
     (emit :newline))

    ((NAMESPACE-ALIAS ?identifier ?qualified-namespace-specifier)
     (emit "namespace")
     (generate-expression ?identifier)
     (emit "=")
     (generate-expression ?qualified-namespace-specifier)
     (emit ";" :newline))

    ((USING (TYPENAME ?name))
     (emit "using" " " "typename" " ")
     (generate-expression ?name)
     (emit ";" :newline))

    ((USING (NAMESPACE ?name))
     (emit "using" " " "namespace" " ")
     (generate-expression ?name)
     (emit ";" :newline))

    ((USING (SCOPE ?name))
     (emit "using" " ")
     (generate-expression `(com.informatimago.languages.linc.c::scope ,?name))
     (emit ";" :newline))

    ((TEMPLATE (&rest ?template-parameter-list) ?declaration)
     (emit "template" "<")
     (generate-list ","
                    (function generate-expression)
                    ?template-parameter-list)
     (emit ">")
     (when ?template-parameter-list (emit :newline))
     (generate-declaration ?declaration))

    ((TEMPLATE EXPORT (&rest ?template-parameter-list) ?declaration)
     (emit "export" " " "template" "<")
     (generate-list ","
                    (function generate-expression)
                    ?template-parameter-list)
     (emit ">" :newline)
     (generate-declaration ?declaration))

    ((TEMPLATE ?declaration)
     (emit "template" " ")
     (generate-declaration ?declaration))

    ((EXTERN ?string ?declaration)
     (emit "extern" " ")
     (generate-expression ?string)
     (generate-declaration ?declaration))

    ((EXTERN ?string (&rest ?declarations))
     (emit "extern" " ")
     (generate-expression ?string)
     (with-parens "{}"
       (dolist (?declaration ?declarations)
         (generate-declaration ?declaration)))
     (emit :newline))

    ((&whole ?everything &rest ?anything)
     (error "Not a declaration: ~S" ?everything))))

(defun generate-preprocessor (expression)
  (flet ((gen-progn (expression)
           (if (and (listp expression) (eq '\#progn (first expression)))
             (dolist (expr (rest expression))
               (generate expr))
             (generate expression))))
    (if (atom expression)
      (error "Not a pre-processor expression: ~S" expression)
      (case (first expression)

        ((\#cond)
         (let ((op "#if")
               (clauses (rest expression)))
           (dolist (clause clauses)
             (destructuring-bind (condi &rest body) clause
               (if (find condi '(t (quote t)) :test (function equal))
                   (emit :fresh-line "#else" :newline)
                   (progn (emit :fresh-line op " ")
                          (generate-expression condi)
                          (emit :newline)
                          (setf op "#elif")))
               (dolist (item body)
                 (generate item))))))

        ((\#if \#ifdef \#ifndef)
         (destructuring-bind (\#test ?condition ?then &optional ?else) expression
           (emit (format nil "~(~A~)" \#test) " ")
           (generate-expression ?condition)
           (emit :fresh-line)
           (gen-progn ?then)
           (when ?else
             (emit :fresh-line "#else" :newline)
             (gen-progn ?else))
           (emit :fresh-line "#endif" :newline)))

        ((\#define)
         (destructuring-bind (?operator ?name &rest ?arguments) expression
           (declare (ignore ?operator))
             (if (listp ?name)
               (in-continuation-lines
                (emit "#define" " " (first ?name))
                (with-parens "()"
                  (generate-list "," (function generate-expression) (rest ?name)))
                (emit :newline)
                (dolist (item ?arguments) (generate item))
                (emit :fresh-line))
               (in-continuation-lines
                (emit "#define" " "  ?name)
                (dolist (item ?arguments) (generate item))
                (emit :fresh-line)))))

        ((\#include \#undef \#line \#error \#pragma \#)
         (destructuring-bind (?operator &rest ?arguments) expression
           (emit :fresh-line (format nil "~(~A~)" ?operator))
           (dolist (arg ?arguments)
             (emit  " ") (generate-expression arg))
           (emit :newline)))

        (otherwise
         (error "Not a pre-processor expression: ~S" expression))))))

(defmethod generate-expression (expression)
  #-(and)
  (com.informatimago.languages.linc.c::progn
   com.informatimago.languages.linc.c::callargs
   com.informatimago.languages.linc.c::?
   com.informatimago.languages.linc.c::=
   com.informatimago.languages.linc.c::*=
   com.informatimago.languages.linc.c::/=
   com.informatimago.languages.linc.c::%=
   com.informatimago.languages.linc.c::+=
   com.informatimago.languages.linc.c::-=
   com.informatimago.languages.linc.c::>>=
   com.informatimago.languages.linc.c::<<=
   com.informatimago.languages.linc.c::&=
   com.informatimago.languages.linc.c::^=
   com.informatimago.languages.linc.c::\|=
   com.informatimago.languages.linc.c::\|\|
   com.informatimago.languages.linc.c::&&
   com.informatimago.languages.linc.c::\|
   com.informatimago.languages.linc.c::^
   com.informatimago.languages.linc.c::&
   com.informatimago.languages.linc.c::==
   com.informatimago.languages.linc.c::!=
   com.informatimago.languages.linc.c::<
   com.informatimago.languages.linc.c::>
   com.informatimago.languages.linc.c::<=
   com.informatimago.languages.linc.c::>=
   com.informatimago.languages.linc.c::<<
   com.informatimago.languages.linc.c::>>
   com.informatimago.languages.linc.c::+
   com.informatimago.languages.linc.c::-
   com.informatimago.languages.linc.c::*
   com.informatimago.languages.linc.c::/
   com.informatimago.languages.linc.c::%
   com.informatimago.languages.linc.c::.*
   com.informatimago.languages.linc.c::->*
   com.informatimago.languages.linc.c::cast
   com.informatimago.languages.linc.c::++
   com.informatimago.languages.linc.c::--
   com.informatimago.languages.linc.c::!
   com.informatimago.languages.linc.c::~
   com.informatimago.languages.linc.c::deref
   com.informatimago.languages.linc.c::pointer
   com.informatimago.languages.linc.c::address
   com.informatimago.languages.linc.c::pos
   com.informatimago.languages.linc.c::neg
   com.informatimago.languages.linc.c::sizeof
   com.informatimago.languages.linc.c::new
   com.informatimago.languages.linc.c::delete
   com.informatimago.languages.linc.c::++post
   com.informatimago.languages.linc.c::--post
   com.informatimago.languages.linc.c::\.
   com.informatimago.languages.linc.c::->
   com.informatimago.languages.linc.c::aref
   com.informatimago.languages.linc.c::call
   com.informatimago.languages.linc.c::scope
   com.informatimago.languages.linc.c::literal
   com.informatimago.languages.linc.c::identifier))



(defmethod generate-c-sexp (expression)
  (let ((key (first expression)))
    (ecase key

      ((asm namespace namespace-alias using template extern)
       (generate-declaration expression))

      ((\#cond \#if \#ifdef \#ifndef \#include
               \#define \#undef \#line \#error \#pragma \#)
       (generate-preprocessor expression))

      ((com.informatimago.languages.linc.c::block
           com.informatimago.languages.linc.c::let
         com.informatimago.languages.linc.c::if
         com.informatimago.languages.linc.c::case
         com.informatimago.languages.linc.c::while
         com.informatimago.languages.linc.c::do
         com.informatimago.languages.linc.c::for
         com.informatimago.languages.linc.c::break
         com.informatimago.languages.linc.c::continue
         com.informatimago.languages.linc.c::return
         com.informatimago.languages.linc.c::goto)
       (generate-statement expression))

      ((com.informatimago.languages.linc.c::progn
         com.informatimago.languages.linc.c::callargs
         com.informatimago.languages.linc.c::?
         com.informatimago.languages.linc.c::=
         com.informatimago.languages.linc.c::*=
         com.informatimago.languages.linc.c::/=
         com.informatimago.languages.linc.c::%=
         com.informatimago.languages.linc.c::+=
         com.informatimago.languages.linc.c::-=
         com.informatimago.languages.linc.c::>>=
         com.informatimago.languages.linc.c::<<=
         com.informatimago.languages.linc.c::&=
         com.informatimago.languages.linc.c::^=
         com.informatimago.languages.linc.c::\|=
         com.informatimago.languages.linc.c::\|\|
         com.informatimago.languages.linc.c::&&
         com.informatimago.languages.linc.c::\|
         com.informatimago.languages.linc.c::^
         com.informatimago.languages.linc.c::&
         com.informatimago.languages.linc.c::==
         com.informatimago.languages.linc.c::!=
         com.informatimago.languages.linc.c::<
         com.informatimago.languages.linc.c::>
         com.informatimago.languages.linc.c::<=
         com.informatimago.languages.linc.c::>=
         com.informatimago.languages.linc.c::<<
         com.informatimago.languages.linc.c::>>
         com.informatimago.languages.linc.c::+
         com.informatimago.languages.linc.c::-
         com.informatimago.languages.linc.c::*\\         com.informatimago.languages.linc.c::/
         com.informatimago.languages.linc.c::%
         com.informatimago.languages.linc.c::.*
         com.informatimago.languages.linc.c::->*
         com.informatimago.languages.linc.c::cast
         com.informatimago.languages.linc.c::++
         com.informatimago.languages.linc.c::--
         com.informatimago.languages.linc.c::!
         com.informatimago.languages.linc.c::~
         com.informatimago.languages.linc.c::deref
         com.informatimago.languages.linc.c::pointer
         com.informatimago.languages.linc.c::address
         com.informatimago.languages.linc.c::pos
         com.informatimago.languages.linc.c::neg
         com.informatimago.languages.linc.c::sizeof
         com.informatimago.languages.linc.c::new
         com.informatimago.languages.linc.c::delete
         com.informatimago.languages.linc.c::++post
         com.informatimago.languages.linc.c::--post
         com.informatimago.languages.linc.c::\.
         com.informatimago.languages.linc.c::->
         com.informatimago.languages.linc.c::aref
         com.informatimago.languages.linc.c::call
         com.informatimago.languages.linc.c::scope
         com.informatimago.languages.linc.c::literal
         com.informatimago.languages.linc.c::identifier)
       (generate-expression expression)))))




(defmacro com.informatimago.languages.linc.c::when   (condi &body body)
  `(com.informatimago.languages.linc.c::if ,condi (com.informatimago.languages.linc.c::block ,@body)))

(defmacro com.informatimago.languages.linc.c::unless (condi &body body)
  `(com.informatimago.languages.linc.c::if (com.informatimago.languages.linc.c::not ,condi) (com.informatimago.languages.linc.c::block ,@body)))

(defmacro com.informatimago.languages.linc.c::setf (place expression &rest others)
  (if others
      `(expr-seq
        ,@(loop :for (p e) :on (list* place expression others) :by (function cddr)
                :collect `(com.informatimago.languages.linc.c::= ,p ,e)))
      `(com.informatimago.languages.linc.c::= ,place ,expression)))

(defmacro com.informatimago.languages.linc.c::let* (bindings &body body)
  (if (null bindings)
      `(com.informatimago.languages.linc.c::block ,@body)
      `(com.informatimago.languages.linc.c::let (,(first bindings))
         (com.informatimago.languages.linc.c::let* (rest bindings) ,@body))))

(defmacro com.informatimago.languages.linc.c::comment (&rest items)
  `(progn
     (emit :newline)
     (with-parens ("/*" "*/")
       ,@(mapcar (lambda (item) `(emit :fresh-line ,(format nil "~A" item)))
                 items)
       (emit :newline))))

(defmacro com.informatimago.languages.linc.c::define-function (name arguments result-type &body body)
  (com.informatimago.languages.linc::generate-definition
   `(com.informatimago.languages.linc.c::defun
        ,name ,arguments ,result-type
      (com.informatimago.languages.linc.c::block ,@body))))

(defmacro com.informatimago.languages.linc.c::c (&rest declarations)
  `(cl:block
       ,@(mapcar (function com.informatimago.languages.linc::generate-declaration)
                 declarations)))


(defun compile-linc-file (input-file &key verbose print
                          (if-does-not-exist :error)
                          (external-format :default)
                          output-file)
  (with-open-file (input input-file
                         :direction :input
                         :if-does-not-exist if-does-not-exist
                         :external-format external-format)
    (with-open-file (output (or output-file
                                (make-pathname
                                 :type "C" :case :common
                                 :defaults input-file))
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format external-format)
      (with-open-file (header (make-pathname
                               :type "H" :case :common
                               :defaults (or output-file input-file))
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format external-format)
        (let ((*c-out* output)
              (*h-out* header)) ;; TODO: not implemented yet.
          (declare (special *c-out* *h-out*))
          (warn "not implemented yet")
          (load input :verbose verbose :print print))))))



(defun repl ()
  (catch 'repl     ; allow for emergency exit with (throw 'com.informatimago.languages.linc::repl)
    (let ((*package* (find-package "C"))
          (*print-pretty* nil)
          (eof *standard-input*)
          (hist 0))
      (loop
         (incf hist)
         (format t "~%~A[~D]> " (package-name *package*) hist)
         (finish-output)
         (handling-errors
          (setf - (read *standard-input* nil eof))
          (cond
            ((or (eq - eof)
                 (and (atom -) (member - '(:quit :exit :continue)
                                       :test (function string-equal))))
             (return-from repl))
            ((and (atom -) (string-equal - :help))
             (format t "~2%==== LINC REPL ====~2%")
             (format t ":QUIT, :EXIT, :CONTINUE    Exit this REPL.~%")
             (format t ":HELP                      Prints this help.~%")
             (format t "Any other S-expression is interpreted tentatively both ~%~
                       as C expression and C statement, and corresponding C ~%~
                       code is printed~%"))
            (t
             (let ((res-expr (multiple-value-bind (val err)
                                 (ignore-errors
                                   (with-output-to-string (*c-out*)
                                     (generate-expression -)))
                               (if err err val)))
                   (res-stat (multiple-value-bind (val err)
                                 (ignore-errors
                                   (with-output-to-string (*c-out*)
                                     (generate-statement -)))
                               (if err err val))))
               (setf +++ ++   ++ +   + -
                     /// //   // /   / (list res-expr res-stat)
                     *** **   ** *   * (first /))
               (format t "~&expression --> ~A~%"  res-expr)
               (format t "~&statement  --> ~A~%"  res-stat))))
          (finish-output t))))))


;;;; THE END ;;;;
