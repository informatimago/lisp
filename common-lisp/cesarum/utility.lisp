;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              utility.lisp
;;;;LANGUAGE:          common-lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This package exports some utility & syntactic sugar functions & macros.
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2016-01-16 <PJB> Removed CHRONO. Use ...TIME:CHRONO-RUN-TIME instead.
;;;;    2016-01-04 <PJB> Added HASH-TABLE-SELECT.
;;;;    2015-08-19 <PJB> Renamed INCLUDE -> INCLUDE-FILE.
;;;;    2015-06-13 <PJB> Added CHRONO.
;;;;    2014-10-22 <PJB> Added HASH-TABLE-TO-SEXP and SEXP-TO-HASH-TABLE.
;;;;    2013-06-30 <PJB> Added FLOAT-{,C,E}TYPECASE; exported [-+]EPSILON.
;;;;    2008-06-24 <PJB> Added INCF-MOD and DECF-MOD.
;;;;    2007-12-01 <PJB> Removed PJB-ATTRIB macro (made it a flet of PJB-DEFCLASS).
;;;;    2007-07-07 <PJB> Added TRACING.
;;;;    2007-03-19 <PJB> Added HASHTABLE and PRINT-HASHTABLE (typo on purpose).
;;;;    2007-02-18 <PJB> Added NSUBSEQ.
;;;;    2005-03-30 <PJB> Added SIGN.
;;;;    2005-03-17 <PJB> Added DEFINE-IF-UNDEFINED
;;;;    2005-03-17 <PJB> Added COMPOSE & COMPOSE-AND-CALL.
;;;;    2005-03-09 <PJB> Added DEFENUM.
;;;;    2004-12-13 <PJB> Removed UNREADABLE-OBJECT (use PRINT-UNREADABLE-OBJECT).
;;;;    2004-10-10 <PJB> Added UNREADABLE-OBJECT class, & reordered definitions.
;;;;    2004-03-31 <PJB> Renamed DEFINE-WITH-STRUCTURE to DEFINE-WITH-OBJECT,
;;;;                     since behavior of WITH-SLOT on structures is undefined.
;;;;    2004-02-27 <PJB> Added DEFINE-WITH-STRUCTURE, FOR, VECTOR-INIT;
;;;;                     removed (REPEAT ...) --> (LOOP ...).
;;;;    2004-01-19 <PJB> Added INCLUDE.
;;;;    2003-10-23 <PJB> Added COMPUTE-CLOSURE.
;;;;    2003-01-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2003 - 2016
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
;;;;****************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SYMBOL")
  #+mocl (:shadowing-import-from "COM.INFORMATIMAGO.MOCL.KLUDGES.MISSING"
                                 "*TRACE-OUTPUT*"
                                 "*LOAD-VERBOSE*"
                                 "*LOAD-PRINT*"
                                 "ARRAY-DISPLACEMENT"
                                 "CHANGE-CLASS"
                                 "COMPILE"
                                 "COMPLEX"
                                 "ENSURE-DIRECTORIES-EXIST"
                                 "FILE-WRITE-DATE"
                                 "INVOKE-DEBUGGER" "*DEBUGGER-HOOK*"
                                 "LOAD"
                                 "LOGICAL-PATHNAME-TRANSLATIONS"
                                 "MACHINE-INSTANCE"
                                 "MACHINE-VERSION"
                                 "NSET-DIFFERENCE"
                                 "RENAME-FILE"
                                 "SUBSTITUTE-IF"
                                 "TRANSLATE-LOGICAL-PATHNAME"
                                 "PRINT-NOT-READABLE"
                                 "PRINT-NOT-READABLE-OBJECT")
  (:export
   "SIMPLE-PROGRAM-ERROR"
   "SIMPLE-PROGRAM-ERROR-FORMAT-CONTROL"
   "SIMPLE-PROGRAM-ERROR-FORMAT-ARGUMENTS"
   ;; 3 - EVALUATION AND COMPILATION
   "WITH-FUNCTIONS" "WITH-GENSYMS" "WSIOSBP" "PROGN-CONCAT"
   "CURRY" "RCURRY" "COMPOSE" "COMPOSE-AND-CALL"
   "/NTH-ARG" "/APPLY"
   "DEFINE-IF-UNDEFINED"  "INCLUDE-FILE" "FUNCTIONAL-PIPE"
   "FIRST-ARG" "SECOND-ARG" "THIRD-ARG" "FOURTH-ARG" "FIFTH-ARG"
   "SIXTH-ARG" "SEVENTH-ARG" "EIGHTH-ARG" "NINTH-ARG" "TENTH-ARG"
   "NTH-ARG"
   ;; 4 - TYPES AND CLASSES
   "DEFENUM" "OP-TYPE-OF"
   ;; 5 - DATA AND CONTROL FLOW
   "SAFE-APPLY" "WHILE" "UNTIL" "FOR"
   ;; 7 - OBJECTS
   "DEFINE-STRUCTURE-CLASS" "DEFINE-WITH-OBJECT" "PJB-DEFCLASS"
   "PRINT-PARSEABLE-OBJECT" "OBJECT-IDENTITY"
   "SLOTED-OBJECT" "SLOTS-FOR-PRINT" "EXTRACT-SLOTS"
   "GEN-EXTRACT-SLOTS"
   ;; 8 - STRUCTURES
   "DEFINE-WITH-STRUCTURE"
   ;; 9 - CONDITIONS
   "HANDLING-ERRORS"
   ;; 12 - NUMBERS
   "SIGN"
   "DISTINCT-FLOAT-TYPES" "FLOAT-TYPECASE" "FLOAT-CTYPECASE" "FLOAT-ETYPECASE"
   "+EPSILON" "-EPSILON"
   ;; 14 - CONSES
   "MAXIMIZE" "TOPOLOGICAL-SORT" "TRANSITIVE-CLOSURE"
   "COMPUTE-CLOSURE"       ; deprecated, renamed to transitive-closure
   "FIND-CYCLES" "FIND-SHORTEST-PATH"
   ;; 15 - ARRAYS
   "VECTOR-INIT" "UNDISPLACE-ARRAY" "DICHOTOMY-SEARCH"
   ;; 16 - STRINGS
   "CONCAT" "SCONC" "SCASE"
   ;; 17 - SEQUENCES
   "NSUBSEQ"
   ;; 18 - HASH-TABLES
   "HASH-TABLE-KEYS" "HASH-TABLE-VALUES"
   "HASH-TABLE-ENTRIES" "HASH-TABLE-SELECT"
   "HASH-TABLE-PATH"
   "COPY-HASH-TABLE" "MAP-INTO-HASH-TABLE"
   "HASHTABLE" "PRINT-HASHTABLE"
   "HASH-TABLE-TO-SEXP"
   "SEXP-TO-HASH-TABLE"
   ;;
   "DICHOTOMY"
   "TRACING" "TRACING-LET" "TRACING-LET*" "TRACING-LABELS"
   ;;
   "XOR" "EQUIV" "IMPLY"
   ;; "SET-EQUAL"
   )
  (:export "PARSE-STRUCTURE-DEFINITION")
  (:export "DMS-D" "D-DMS")
  (:documentation
   "

This package exports some utility & syntactic sugar functions and macros.



License:

    AGPL3

    Copyright Pascal J. Bourguignon 2003 - 2016

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3 - EVALUATION AND COMPILATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-functions ((&rest fnames) &body body)
  `(flet ,(mapcar (lambda (fname)
                    (if (listp fname)
                        (destructuring-bind (name &rest parameters) fname
                          `(,name ,parameters (funcall ,name ,@parameters)))
                        `(,fname (&rest arguments) (apply ,fname arguments))))
           fnames)
     (declare (inline ,@(mapcar (lambda (fname) (if (listp fname) (first fname) fname)) fnames)))
     ,@body))

#-:with-debug-gensym
(defmacro with-gensyms (syms &body body)
  "
DO:      Replaces given symbols with gensyms. Useful for creating macros.
NOTE:    This version by Paul Graham in On Lisp."
  `(let ,(mapcar (lambda (s) `(,s (gensym ,(string s)))) syms) ,@body))


#+:with-debug-gensym
(defpackage "COM.INFORMATIMAGO.GENSYMS" (:use))
#+:with-debug-gensym
(defmacro with-gensyms (syms &body body)
  "
DO:      Replaces given symbols with gensyms. Useful for creating macros.
NOTE:    This version by Paul Graham in On Lisp."
  `(let ,(mapcar
          (lambda (s) `(,s (intern (string (gensym ,(string s)))
                                   "COM.INFORMATIMAGO.GENSYMS"))) syms) ,@body))


(defun progn-concat (forms)
  "
DO:       Wraps the forms in a PROGN.  If they're PROGN forms,
          then their PROGN is unwrapped first.
"
  `(progn ,@(mapcan (lambda (form) (cond
                                     ((null form)
                                      '())
                                     ((and (listp form) (eq 'progn (first form)))
                                      (copy-list (rest form)))
                                     (t
                                      (list form))))
                    forms)))



(defmacro wsiosbp (&body body)
  "
Like with-standard-io-syntax but with the current package.
The *PACKAGE* is kept bound to the current package.
"
  (let ((vpack (gensym)))
    `(let ((,vpack *package*))
       (with-standard-io-syntax
         (let ((*package* ,vpack))
           ,@body)))))


(defmacro define-argument-selector (name argument-number)
  (let ((arguments (loop :for i :from 0 :to argument-number :collect (gensym))))
    `(defun ,name (,@(cdr arguments) &rest ,(car arguments))
       ,(format nil "RETURN: The ~:R argument." argument-number)
       (declare (ignore ,@(butlast arguments)))
       ,(car (last arguments)))))
(define-argument-selector first-arg   1)
(define-argument-selector second-arg  2)
(define-argument-selector third-arg   3)
(define-argument-selector fourth-arg  4)
(define-argument-selector fifth-arg   5)
(define-argument-selector sixth-arg   6)
(define-argument-selector seventh-arg 7)
(define-argument-selector eighth-arg  8)
(define-argument-selector ninth-arg   9)
(define-argument-selector tenth-arg   10)
(defun nth-arg (n &rest arguments)
  "RETURN: The Nth argument following N."
  (nth n arguments))

(defun curry (function &rest left-arguments)
  (lambda (&rest right-arguments)
    (apply function (append left-arguments right-arguments))))

(defun rcurry (function &rest right-arguments)
  (lambda (&rest left-arguments)
    (apply function (append left-arguments right-arguments))))

;; (defmacro curry (function &rest left-arguments)
;;   (let ((parameters (mapcar (lambda (arg) (gensym)) left-arguments))
;;         (right-arguments (gensym)))
;;     `(let ,(mapcar (function list) parameters left-arguments)
;;        (lambda (&rest ,right-arguments)
;;          (apply (function ,function) ,@parameters ,right-arguments)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compose-sexp (functions var)
    (if (null functions)
        var
        (list (car functions) (compose-sexp (cdr functions) var)))))

(defmacro compose (&rest functions)
  "
RETURN:     The functional composition of the FUNCTIONS.
EXAMPLE:    (compose abs sin cos) = (lambda (&rest args) (abs (sin (apply (function cos) args))))
NOTE:       (compose) = (function identity)
            (compose foo) = (function foo)
"
  (cond
    ((null functions)        `(function identity))
    ((null (cdr functions))  `(function ,(car functions)))
    (t `(lambda (&rest args)
          ,(compose-sexp (butlast functions)
                         `(apply (function ,(first (last functions)))
                                 args))))))


(defmacro compose-and-call (&rest functions-and-arg)
  "
DO:         Call the functional composition of the functions, on the
            argument.
EXAMPLE:    (compose-and-call abs sin cos 0.234) --> 0.8264353
"
  `(funcall ,((lambda (functions) (list 'lambda '(x) (compose-sexp functions 'x)))
              (butlast functions-and-arg))
            ,(car (last functions-and-arg))))

;; (funcall (compose 1+ sin 1-) 0)
;; (compose-and-call 1+ sin 1- 0)


(defun /nth-arg (n)
  (lambda (&rest args) (nth n args)))

(defun /apply (&rest funs)
  (lambda (&rest args)
    (mapcar (lambda (f) (apply f args)) funs)))


(defmacro define-if-undefined (&rest definitions)
  "Use this to conditionally define functions, variables, or macros that
may or may not be pre-defined in this Lisp.  This can be used to provide
CLtL2 compatibility for older Lisps.
WHO'S THE AUTHOR?"
  `(progn
     ,@(mapcar #'(lambda (def)
                   (let ((name (second def)))
                     `(unless (or (boundp ',name)
                                  (fboundp ',name)
                                  (special-form-p ',name)
                                  (macro-function ',name))
                        ,def)))
               definitions)))

#||
(define-if-undefined

(defmacro with-simple-restart (restart &rest body)
"Like PROGN, except provides control over restarts if there is an error."
(declare (ignore restart))
`(progn ,@body))

(defmacro done-mac () nil)
)

(defmacro uncond-mac () nil)

||#


;; (defun include-file (path)
;;   "
;; NOTE:    Untasty, but sometimes useful.
;; DO:      Read from the file at PATH all the sexps and returns a list of them
;;          prefixed with 'progn.
;; USAGE:   #.(include-file \"source.lisp\")
;; "
;;   (cons 'progn
;;         (with-open-file (file path :direction :input :if-does-not-exist :error)
;;           (do ((result '())
;;                (eof (gensym)))
;;               ((eq eof (car result)) (nreverse (cdr result)))
;;             (push (read file nil eof) result)))))


(defgeneric include-file (path-or-stream)
  (:documentation  "
NOTE:    Untasty, but sometimes useful.
DO:      Read from the file at PATH all the sexps and returns a list of them
         prefixed with 'progn.
USAGE:   #.(include-file \"source.lisp\")
")
  (:method ((path string))
    (include-file (pathname string)))
  (:method ((path pathname))
    (with-open-file (stream path :direction :input :if-does-not-exist :error)
      (include-file stream)))
  (:method ((stream stream))
    (loop
      :for form := (read stream nil stream)
      :until (eql form stream)
      :do (eval form))))


(defmacro functional-pipe (&body forms)
  "
Execute forms in sequence each in a lexical scope where *, ** and *** are bound
to the results of the last three previous forms.
Return the results of the last form.
"
  (let ((bindings (mapcar (lambda (form) (list (gensym) form)) forms)))
    `(let* ,(loop
              for (*** ** * current) on (list* '(nil) '(nil) '(nil) bindings)
              unless (null current)
                collect (list (first current)
                              (subst (first ***) '***
                                     (subst (first **) '**
                                            (subst (first *) '*
                                                   (second current))))))
       ,(first (first (last bindings))))))

;; (let ((*** nil) (** nil) (* nil))
;;   (let ((*** **) (** *) (* ,form))
;;     ...
;;     *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4 - TYPES AND CLASSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro defenum (name-and-options &rest constants)
  "
Define an named enumeration type, a set of constants with integer
values, and a label function to produce the name of the constants from
the numerical value.

NAME-AND-OPTIONS:

            The name of the enum type, or a list containing the name
            of the enum type and options (no option defined so far).
            The label function defined is named <enum-type-name>-LABEL

CONSTANTS:  The first element of CONSTANTS may be an optional docstring.
            Each constant is either a symbol naming the constant of the enum,
            (the value is then the successor of the previous value),
            or a list containing the constant name and the constant value.
"
  (let ((name (if (consp name-and-options)
                  (first name-and-options)
                  name-and-options)))
    (when (stringp (first constants))   ; docstring
      (pop constants))
    (let* ((min nil)
           (max nil)
           (defconstants (loop
                           :with val = -1
                           :for cname :in constants
                           :do (when (consp cname)
                                 (setf val (1- (second cname))
                                       cname (first cname)))
                           :collect `(defconstant ,cname ,(incf val)
                                       ,(format nil "~A enumeration value." name))
                           :do (if min (setf min (min min val)) (setf min val))
                               (if max (setf max (max max val)) (setf max val)))))
      (push `(defconstant ,(scat name '-min) ,min
               ,(format nil "The minimum ~A enumeration value." name))
            defconstants)
      (push `(defconstant ,(scat name '-max) ,max
               ,(format nil "The maximum ~A enumeration value." name))
            defconstants)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; define a ({NAME}-LABEL value) function.
         (defun ,(intern (wsiosbp (format nil "~A-LABEL" name))) (value)
           ,(format nil "Produce the name of the constant having the given VALUE.")
           (case value
             ,@(loop
                 :with val = -1
                 :for cname :in constants
                 :do (if (consp cname)
                         (setf val (second cname))
                         (incf val))
                 :collect `((,val) ',(if (consp cname)
                                         (first cname)
                                         cname)))
             (otherwise (format nil "#<~A:~D>" ',name value))))
         ;; define the constants.
         ,@defconstants
         ;; define the type.
         (deftype ,name ()
           "An enumeration type." ;; TODO: get a docstring from the parameters.
           '(member ,@(loop
                        :with val = -1
                        :for cname :in constants
                        :collect (if (consp cname)
                                     (setf val (second cname))
                                     (incf val)))))))))


(defun op-type-of (symbol &optional env)
  "
From: nikodemus@random-state.net
Newsgroups: comp.lang.lisp
Date: 29 Jul 2004 03:59:50 GMT
Message-ID: <ce9snm$4bp8o$1@midnight.cs.hut.fi>
"
  (if (fboundp symbol)
      (cond ((macro-function symbol env)
             'macro)
            ((special-operator-p symbol)
             'special-operator)
            ((compiled-function-p (symbol-function symbol))
             'compiled-function)
            (t
             'interpreted-function))
      (error "Symbol ~S is not an operator." symbol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5 - DATA AND CONTROL FLOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun safe-apply (fun &rest args)
  "
DO:    Call APPLY or REDUCE depending on the length of ARGS.
NOTE:  No prefix argument are allowed for REDUCE!
       (safe-apply 'concatenate 'string list-of-sequence) doesn't work!
       Use instead:
       (safe-apply (lambda (a b) (concatenate 'string a b)) list-of-sequence)
"
  (let ((arg-list (car (last args))))
    (if (< (+ (length args) (length arg-list)) call-arguments-limit)
        (apply  fun (nconc (butlast args) arg-list))
        (reduce fun (nconc (butlast args) arg-list)))))

(defmacro while (condition &body body)
  "While loop."
  `(do () ((not ,condition))  ,@body))

(defmacro until (condition &body body)
  "Until loop."
  `(do () (,condition)        ,@body))

(defmacro for ((var first last &optional (step nil stepp)) &body body)
  "For loop.
DO:    Repeat BODY with VAR bound to successive integer values from
       FIRST to LAST inclusive.
       If the optional STEP argument is abstent, then it is taken as 1 or -1
       depending on the order of FIRST and LAST.
       VAR is incremented by STEP and it stops when VAR goes above
       or below LAST depending on the sign of STEP.
"
  (when (and (not stepp) (numberp first) (numberp last))
    (setf step (if (<= first last) 1 -1)
          stepp t))
  (cond
    ((and stepp (numberp step))
     ;; Hardwired step and comparison:
     (check-type step real)
     (let ((firstvar (gensym "FIRST"))
           (lastvar  (gensym "LAST"))
           (cmp      (if (minusp step) '< '>)))
       `(let ((,firstvar ,first)
              (,lastvar ,last))
          (do ((,var ,firstvar (incf ,var ,step)))
              ((,cmp ,var ,lastvar))
            ,@body))))
    (stepp
     ;; Runtime evaluation of step and comparison:
     (let ((firstvar (gensym "FIRST"))
           (lastvar  (gensym "LAST"))
           (stepvar  (gensym "STEP"))
           (cmp      (gensym "CMP")))
       `(let* ((,firstvar ,first)
               (,lastvar  ,last)
               (,stepvar  ,step)
               (,cmp     (if (minusp ,stepvar)
                             (function <)
                             (function >))))
          (do ((,var ,firstvar (incf ,var ,stepvar)))
              ((funcall ,cmp ,var ,lastvar))
            ,@body))))
    (t
     ;; Runtime determination of step and comparison:
     (let ((firstvar (gensym "FIRST"))
           (lastvar  (gensym "LAST"))
           (stepvar  (gensym "STEP"))
           (cmp      (gensym "CMP")))
       `(let* ((,firstvar ,first)
               (,lastvar  ,last)
               (,stepvar  (if (<= ,firstvar ,lastvar) 1 -1))
               (,cmp      (if (minusp ,stepvar)
                              (function <)
                              (function >))))
          (do ((,var ,firstvar (incf ,var ,stepvar)))
              ((funcall ,cmp ,var ,lastvar))
            ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7 - OBJECTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro pjb-defclass (name super &rest args)
  "
This macro encapsulate DEFCLASS and allow the declaration of the attributes
in a shorter syntax.
ARGS  is a list of s-expr, whose car is either :ATT (to declare an attribute)
      or :DOC to give the documentation string of the class.
      (:OPT ...) is not implemented yet.
      (:ATT name type [ init-value [doc-string] | doc-string ]) defines
      an attribute named NAME, of type TYPE, with the given initial value
      and documentation strings.  An accessor and an initarg keyword of
      same NAME are also defined.
"
  (flet ((attrib (name type &rest args)
           "
This function outputs an attribute s-exp as used in defclass.
ARGS  may be of length 1 or 2.
      If (LENGTH ARGS) = 1
      then if the argument is a string,
           then it's taken as the documentation and the initial value is NIL
           else it's taken as the initial value and the documentation is NIL.
      else the first is the initial value and the second is the documentation.
The initarg an accessor are the same keyword built from the name.
"
           (let ((iarg (intern (if (symbolp name) (symbol-name name) name)
                               (find-package "KEYWORD")))
                 init doc)
             (cond  ((= 2 (length args))
                     (setq init (car  args)
                           doc  (cadr args)) )
                    ((= 1 (length args))
                     (if (stringp (car args))
                         (setq init nil
                               doc  (car args))
                         (setq init (car args)
                               doc  nil)) )
                    (t (error "Invalid attribute ~S"
                              `(:att ,name ,type ,@args))))
             (when (and (symbolp type) (null init))
               (setf type (list 'or 'null type)))
             (when (null doc)
               (setf doc (symbol-name name)))
             `(,name
               :initform ,init
               :initarg  ,iarg
               :accessor ,name
               :type     ,type
               :documentation ,doc))))
    (let ((fields  nil)
          (options nil))
      (do () ( (not args) )
        (cond ((eq :att (caar args))
               (push (apply (function attrib) (cdar args)) fields))
              ((eq :doc (caar args))
               (push (cons :documentation (cdar args)) options)))
        (setf args (cdr args)))
      (setf fields (nreverse fields))
      (setf options (nreverse options))
      `(defclass ,name ,super ,fields ,@options))))



#||

For :constructor, several options are allowed.


|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|
|                 |              |                      |                    |                         |                          |
|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|
| default name    | slot name    | slot name            | slot name          | specified prefix        |                          |
|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|
|                 |              |                      |                    |                         |                          |
|                 | :conc-name   | (:conc-name)         | (:conc-name nil)   | (:conc-name name)       |                          |
| struct-slot     | slot         | slot                 | slot               | nameslot                |                          |
|                 |              |                      |                    |                         |                          |
|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|
|                 |              |                      |                    |                         |                          |
|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|
| default name    | default name | default name         | no function        | specified name          | specified name           |
|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|
|                 |              |                      |                    |                         |                          |
|                 | :constructor | (:constructor)       | (:constructor nil) | (:constructor name)     | (:constructor name args) |
| make-struct     | make-struct  | make-struct          | no constructor     | name :slot              | name arglist             |
|                 |              |                      |                    |                         |                          |
|                 | :copier      | (:copier)            | (:copier nil)      | (:copier name)          |                          |
| copy-struct     | copy-struct  | copy-struct          | no copier          | name                    |                          |
|                 |              |                      |                    |                         |                          |
|                 | :predicate   | (:predicate)         | (:predicate nil)   | (:predicate name)       |                          |
| struct-p        | struct-p     | struct-p             | no predicate       | name                    |                          |
|                 |              |                      |                    |                         |                          |
|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|
|                 |              |                      |                    |                         |                          |
|                 |              | (:print-object)      |                    | (:print-object name)    |                          |
| implemntation-  |              | default print-object |                    | print-object calls name |                          |
| specific print  |              | method               |                    |                         |                          |
|                 |              |                      |                    |                         |                          |
|                 |              | (:print-function)    |                    | (:print-function name)  |                          |
| implementation- |              | default print-object |                    | print-object calls name |                          |
| specific print  |              | method               |                    |                         |                          |
|                 |              |                      |                    |                         |                          |
|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|
|                 | :named       |                      |                    |                         |                          |
| not named       | named        |                      |                    |                         |                          |
|                 |              |                      |                    |                         | (:include name slots*)   |
|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|
|                 |              |                      |                    | (:initial-offset value) |                          |
|                 |              |                      |                    | (:type type)            |                          |
|-----------------+--------------+----------------------+--------------------+-------------------------+--------------------------|

Using these results from GET-OPTION allows to distinguish all the cases:
|--------------+--------------+--------------+-------------+------------------+------------------|
|              | :foo         | (:foo)       | (:foo nil)  | (:foo name)      | (:foo name args) |
|--------------+--------------+--------------+-------------+------------------+------------------|
| nil          | :symbol      | :singleton   | (nil)       | (name)           | (name slots*)    |
|--------------+--------------+--------------+-------------+------------------+------------------|
| default name | slot name    | slot name    | slot name   | specified prefix |                  |
|--------------+--------------+--------------+-------------+------------------+------------------|
| default name | default name | default name | no function | specified name   | specified name   |
|--------------+--------------+--------------+-------------+------------------+------------------|

For conc-name:
default-name = (null option)
slot-name    = (or (atom option) (null (first option)))
name         = (and (consp option) (first option))

For others:
default-name = (atom option)
no-function  = (and (consp option) (null (first option)))
name         = (and (consp option) (first option))

||#


(defun option-key-p (key option)
  (eq key (if (symbolp option) option (car option))))

(defun get-option (key options &key list boolean)
  (let ((opts (remove key options :test-not (function option-key-p))))
    (cond
      (list               opts)
      (boolean
       (cond
         ((null opts)          nil)
         ((cddr opts)          (error "Expected only one ~S option, not ~{~S~^ ~}." key opts))
         ((symbolp (car opts)) t)
         (t                    (error "Expected a flag ~S option, not ~{~S~^ ~}." key opts))))
      ((null opts)        nil)
      ((null (rest opts)) ; a single option
       (let ((opt (first opts)))
         (cond
           ((symbolp opt)     :symbol)
           ((null (rest opt)) :singleton)
           (t                 (rest opt)))))
      (t
       (error "Expected only one ~S option, not ~{~S~^ ~}." key opts)))))

(defun make-conc-name (option prefix name suffix)
  (cond
    ((null option)
     (scat prefix name suffix))
    ((member option '(:symbol :singleton (nil)) :test (function equal))
     '||)
    ((and option (listp option) (car option))
     (car option))
    (t nil)))

(defun make-name (option prefix name suffix)
  (cond
    ((or (null option) (and option (not (listp option))))
     (scat prefix name suffix))
    ((and option (listp option) (car option))
     (car option))
    (t nil)))

(defun get-name (option)
  (if (and option (listp option))
      (car option)
      nil))

(defun parse-structure-name-and-options (name-and-options)
  (let ((name    (if (symbolp name-and-options)
                     name-and-options
                     (car name-and-options)))
        (options (if (symbolp name-and-options)
                     nil
                     (cdr name-and-options))))
    (let ((conc-name        (get-option :conc-name      options))
          (constructors     (get-option :constructor    options :list t))
          (copier           (get-option :copier         options))
          (predicate        (get-option :predicate      options))
          (print-function   (get-option :print-function options))
          (print-object     (get-option :print-object   options))
          (include          (get-option :include        options))
          (initial-offset   (get-option :initial-offset options))
          (structure-type-p (get-option :type           options))
          (structure-type   (get-option :type           options))
          (namedp           (get-option :named          options :boolean t)))
      (when (and print-object print-function)
        (error 'simple-program-error
               :format-control "Cannot have :print-object and :print-function options."))
      (when structure-type-p
        (unless (and (null (cdr structure-type))
                     (let ((structure-type (car structure-type)))
                       (or (eql structure-type 'list)
                           (eql structure-type 'vector)
                           (and (listp structure-type)
                                (eql (first structure-type) 'vector)
                                (cdr structure-type)
                                (typep (second structure-type) '(integer 0))
                                (null (cddr structure-type))))))
          (error 'simple-program-error
                 :format-control "Invalid structure :type option: ~S"
                 :format-arguments (list (car structure-type)))))
      (let ((conc-name      (make-conc-name conc-name ""      name "-"))
            (copier         (make-name      copier    "COPY-" name ""))
            (predicate      (make-name      predicate ""      name "-P"))
            (print-function (get-name print-function))
            (print-object   (get-name print-object))
            (constructors   (if (null constructors)
                                (list (make-name nil "MAKE-" name ""))
                                (mapcan (lambda (x)
                                          (cond
                                            ((or (symbolp x) (= 1 (length x)))
                                             (list (make-name nil "MAKE-" name "")))
                                            ((null (second x))
                                             nil)
                                            ((= 2 (length x))
                                             (list (second x)))
                                            (t
                                             (list (list (second x) (third x))))))
                                        constructors))))
        (values name conc-name constructors copier
                include initial-offset predicate
                print-function print-object
                (not (not structure-type-p)) (car structure-type)
                namedp)))))

(defun parse-structure-definition (name-and-options doc-and-slots)
  (multiple-value-bind (name conc-name constructors copier
                        include initial-offset predicate
                        print-function print-object
                        structure-type-p structure-type namedp)
      (parse-structure-name-and-options name-and-options)
    (let ((documentation (if (stringp (car doc-and-slots))
                             (car doc-and-slots)
                             nil))
          (slots        (if (stringp (car doc-and-slots))
                            (cdr doc-and-slots)
                            doc-and-slots)))
      (when (cdr include)
        (setf slots   (append (cddr include) slots)
              include (list (car include))))
      (let ((slot-names     (mapcar (lambda (s) (if (symbolp s) s (car s))) slots))
            (accessors      (mapcar (lambda (s) (make-name nil (or conc-name "")
                                                           (if (symbolp s) s (car s)) ""))
                                    slots)))
        (values name conc-name constructors copier
                include initial-offset predicate
                print-function print-object
                structure-type-p structure-type
                ;; --
                documentation slots slot-names accessors namedp)))))

(defun generate-class-structure (name conc-name constructors copier
                                 include initial-offset predicate
                                 print-function print-object
                                 structure-type-p structure-type
                                 documentation slots slot-names accessors)
  (declare (ignore structure-type-p structure-type initial-offset conc-name))
  ;; TODO: use conc-name
  ;; TODO: signal error on bad structure-type or structure-type-p
  ;; TODO: signal error on non-zero initial-offset
  `(progn
     (defclass ,name ,include
       ,(mapcar
         (lambda (slot accessor)
           (if (symbolp slot)
               `(,slot :accessor  ,accessor)
               (let* ((name        (first slot))
                      (initform-p  (cdr slot))
                      (initform    (car initform-p))
                      (type-p      (member :type (cddr slot)))
                      (type        (cadr type-p))
                      (read-only-p (member :read-only (cddr slot)))
                      (read-only   (cadr read-only-p)))
                 `(,name
                   ,(if (and read-only-p read-only) :reader :accessor)
                   ,accessor
                   ,@(when initform-p  (list :initform initform))
                   ,@(when type-p      (list :type     type))))))
         slots accessors)
       ,@(when documentation (list `(:documentation ,documentation))))
     ,@(mapcar
        (lambda (constructor)
          ;; generate a constructor.
          (if (symbolp constructor)
              (let ((preds (mapcar (lambda (x) (declare (ignore x)) (gensym))
                                   slot-names)))
                `(defun ,constructor
                     (&key ,@(mapcar (lambda (s p) (list s nil p)) slot-names preds))
                   (let ((args nil))
                     ,@(mapcar
                        (lambda (s p)
                          `(when ,p
                             (push ,s args)
                             (push ,(keywordize s) args)))
                        slot-names preds)
                     (apply (function make-instance) ',name args))))
              (let ((cname  (first  constructor))
                    (pospar (second constructor)))
                (declare (ignore pospar))
                (warn "~S does not implement this case yet."
                      'define-structure-class)
                `(defun ,cname (&rest args)
                   (declare (ignore args))
                   (error 'simple-program-error "~S does not implement this yet."
                          'define-structure-class)))))
        constructors)
     ,@(when copier
         (list `(defmethod ,copier ((self ,name))
                  (make-instance ',name
                                 ,@(mapcan
                                    (lambda (slot accessor)
                                      (list (keywordize slot) (list accessor 'self)))
                                    slot-names accessors)))))
     ,@(when predicate
         (list `(defmethod ,predicate (object)
                  (eq (type-of object) ',name))))
     ,@(when print-function
         (list `(defmethod print-object ((self ,name) stream)
                  (,print-function self stream 0))))
     ,@(when print-object
         (list `(defmethod print-object ((self ,name) stream)
                  (,print-object self stream))))))

(defmacro define-structure-class (name-and-options &rest doc-and-slots)
  "
DO:     Define a class implementing the structure API.
        This macro presents the same API as DEFSTRUCT, but instead of
        defining a structure, it defines a class, and the same functions
        as would be defined by DEFSTRUCT.

        The option :TYPE accepts LIST, VECTOR or STRUCTURE.
        When given, it falls back to CL:DEFSTRUCT.
        The DEFSTRUCT option :INITIAL-OFFSET is only supported
        when :TYPE is given.
"
  (multiple-value-bind (name conc-name constructors copier
                        include initial-offset predicate
                        print-function print-object
                        structure-type-p structure-type
                        documentation slots slot-names accessors)
      (parse-structure-definition name-and-options doc-and-slots)
    (if structure-type-p
        ;; For now, fall back to cl:defstruct.
        (if (eql structure-type 'structure)
            `(defstruct ,(remove :type name-and-options :key (lambda (x) (when (listp x) (first x))))
               ,@doc-and-slots)
            `(defstruct ,name-and-options
               ,@doc-and-slots))
        (generate-class-structure  name conc-name constructors copier
                                   include initial-offset predicate
                                   print-function print-object
                                   structure-type-p structure-type
                                   documentation slots slot-names accessors))))

(defmacro define-with-object (class-name slots)
  "
DO:       Define a macro: (WITH-{CLASS-NAME} object &body body)
          expanding to:   (with-slots ({slots}) object @body)
"
  `(defmacro
       ,(intern (with-standard-io-syntax (format nil "WITH-~A" class-name)))
       (object &body body)
     `(with-slots (quote ,,(mapcar (lambda (slot) (list slot slot)) slots))
          ,object ,@body)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing objects.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (declaration stepper))
(declaim (ftype (function ((or string symbol character) &rest others) symbol) keywordize))

;;;
;;; We have two way to print easily objects:
;;;
;;; 1- inherit from the SLOTED-OBJECT mixin class, and define a
;;;    SLOTS-FOR-PRINT method on your classes.
;;;
;;; 2- define a PRINT-OBJECT method on your classes using the macro
;;;    PRINT-PARSEABLE-OBJECT.
;;;

(defclass sloted-object ()
  ()
  (:documentation "
This is a mixin class providing generic SLOTS and PRINT-OBJECT
methods.
"))

(defgeneric extract-slots (object slots)
  (:documentation "
RETURN:         A plist slot values.
OBJECT:         A lisp object.
SLOTS:          A list of slot names.
")
  (:method (object slots)
    (assert (every (function symbolp) slots) (slots))
    (loop
      :for slot :in slots
      :collect (keywordize slot)
      :collect (if (slot-boundp object slot)
                   (slot-value object slot)
                   '#:unbound))))

(defgeneric slots-for-print (object)
  (:method-combination append)
  (:documentation "
This generic function collects a p-list describing the slots of the OBJECT.
The generic function EXTRACT-SLOTS can be used to build this p-list.
The APPEND method combination automatically appends the lists provided
by the SLOTS-FOR-PRINT methods on the various subclasses.
")
  (:method append ((object sloted-object))
    '()))

(defmethod print-object ((self sloted-object) stream)
  (declare (stepper disable))
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~{~S~^ ~}" (slots-for-print self)))
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun object-identity (object)
  "
RETURN:         A string containing the object identity as printed by
                PRINT-UNREADABLE-OBJECT.
"
  (declare (stepper disable))
  (let ((*step-mode* :run)
        (*print-readably* nil))
    (declare (special *step-mode*))
    (let ((ident
            (with-output-to-string (stream)
              (print-unreadable-object (object stream :type nil :identity t)))))
      (subseq ident 3 (1- (length ident))))))

(defun call-print-parseable-object (object stream type identity thunk)
  "
SEE:            PRINT-PARSEABLE-OBJECT
"
  (declare (stepper disable))
  (let ((*step-mode* :run))
    (declare (special *step-mode*))
    (if *print-readably*
        (error 'print-not-readable :object object)
        (progn
          (format stream "~S"
                  (append (when type
                            (list (class-name (class-of object))))
                          (funcall thunk object)
                          (when identity
                            (list :id (object-identity object)))))
          object))))

(defun gen-extract-slots (ovar slots)
  "
SEE:            PRINT-PARSEABLE-OBJECT
RETURN:         A form building a plist of slot values.
"
  (cons 'list
        (loop
          :for slot :in slots
          :collect  (if (symbolp slot)
                        (keywordize slot)
                        `(quote ,(first slot)))
          :collect  (if (symbolp slot)
                        `(if (slot-boundp ,ovar ',slot)
                             (slot-value ,ovar ',slot)
                             '#:unbound)
                        `(ignore-errors ,(second slot))))))

(defmacro print-parseable-object ((object stream &key (type t) identity) &rest slots)
  "

DO:             Prints on the STREAM the object as a list.  If all the
                objects printed inside it are printed readably or with
                PRINT-PARSEABLE-OBJECT, then that list should be
                readable, at least with *READ-SUPPRESS* set to T.

OBJECT:         Either a variable bound to the object to be printed,
                or a binding list (VARNAME OBJECT-EXPRESSION), in
                which case the VARNAME is bound to the
                OBJECT-EXPRESSION during the evaluation of the SLOTS.

STREAM:         The output stream where the object is printed to.

TYPE:           If true, the class-name of the OBJECT is printed as
                first element of the list.

IDENTITY:       If true, the object identity is printed as a string in
                the last position of the list.

SLOTS:          A list of either a symbol naming the slot, or a list
                (name expression), name being included quoted in the
                list, and the expression being evaluated to obtain the
                value.

RETURN:         The object that bas been printed (so that you can use
                it in tail position in PRINT-OBJECT conformingly).

EXAMPLE:        (print-parseable-object (object stream :type t :identity t)
                  slot-1
                  (:slot-2 (thing-to-list (slot-2 object)))
                  slot-3)
"
  `(locally (declare (stepper disable))
     ,(if (symbolp object)
          `(call-print-parseable-object ,object ,stream ,type ,identity
                                        (lambda (,object)
                                          (declare (ignorable ,object) (stepper disable))
                                          ,(gen-extract-slots object slots)))
          (destructuring-bind (ovar oval) object
            `(let ((,ovar ,oval))
               (call-print-parseable-object ,ovar ,stream ,type ,identity
                                            (lambda (,ovar)
                                              (declare (ignorable ,ovar) (stepper disable))
                                              ,(gen-extract-slots ovar slots))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8 - STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (DEFMACRO DEFINE-WITH-STRUCTURE (NAME-AND-OPTIONS SLOTS)
;;   "
;; NAME-AND-OPTIONS:  Either a structure name or a list (name . options).
;;           Valid options are: (:conc-name prefix).
;; DO:       Define a macro: (WITH-{NAME} object &body body)
;;           expanding to a symbol-macrolet embedding body where
;;           symbol macros are defined to access the slots.
;; "
;;   (LET* ((NAME      (IF (SYMBOLP NAME-AND-OPTIONS)
;;                         NAME-AND-OPTIONS (CAR NAME-AND-OPTIONS)))
;;          (CONC-NAME (IF (SYMBOLP NAME-AND-OPTIONS)
;;                         (CONCATENATE 'STRING (STRING NAME) "-")
;;                         (LET ((CONC-OPT (CAR (MEMBER :CONC-NAME
;;                                                      (CDR NAME-AND-OPTIONS)
;;                                                      :KEY (FUNCTION CAR)))))
;;                           (IF CONC-OPT
;;                               (SECOND CONC-OPT)
;;                               (CONCATENATE 'STRING (STRING NAME) "-"))))))
;;     `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;        (DEFMACRO
;;            ,(INTERN (WITH-STANDARD-IO-SYNTAX (FORMAT NIL "WITH-~A" NAME)))
;;            (OBJECT &BODY BODY)
;;          (IF (SYMBOLP OBJECT)
;;              `(SYMBOL-MACROLET
;;                   ,(MAPCAR
;;                     (LAMBDA (SLOT)
;;                       (LIST SLOT
;;                             (LIST
;;                              (INTERN (WITH-STANDARD-IO-SYNTAX
;;                                        (CONCATENATE 'STRING
;;                                          (STRING ',CONC-NAME) (STRING SLOT))))
;;                              OBJECT))) ',SLOTS)
;;                 ,@BODY)
;;              (LET ((OBJV (GENSYM)))
;;                `(LET ((,OBJV ,OBJECT))
;;                   (SYMBOL-MACROLET
;;                       ,(MAPCAR
;;                         (LAMBDA (SLOT)
;;                           (LIST SLOT
;;                                 (LIST
;;                                  (INTERN (WITH-STANDARD-IO-SYNTAX
;;                                            (CONCATENATE 'STRING
;;                                              (STRING ',CONC-NAME) (STRING SLOT))))
;;
;;                                  OBJV))) ',SLOTS)
;;                     ,@BODY)))))))) ;;DEFINE-WITH-STRUCTURE

(defmacro define-with-structure (name-and-options &rest slots)
  "
NAME-AND-OPTIONS:  Either a structure name or a list (name . options).
          Valid options are: (:conc-name prefix).
DO:       Define a macro: (WITH-{NAME} object &body body)
          expanding to a symbol-macrolet embedding body where
          symbol macros are defined to access the slots.
"
  (let* ((name      (if (symbolp name-and-options)
                        name-and-options (car name-and-options)))
         (conc-name (if (symbolp name-and-options)
                        (concatenate 'string (string name) "-")
                        (let ((conc-opt (car (member :conc-name
                                                     (cdr name-and-options)
                                                     :key (function car)))))
                          (if conc-opt
                              (second conc-opt)
                              (concatenate 'string (string name) "-")))))
         (slot-names (mapcar (lambda (slot) (if (listp slot) (car slot) slot))
                             slots)))
    `(progn
       (defstruct ,name-and-options ,@slots)
       (defmacro
           ,(intern (with-standard-io-syntax (format nil "WITH-~A" name)))
           (object &body body)
         (if (symbolp object)
             `(symbol-macrolet
                  ,(mapcar
                    (lambda (slot)
                      (list slot
                            (list
                             (intern (concatenate 'string (string ',conc-name) (string slot)))
                             object))) ',slot-names)
                ,@body)
             (let ((objv (gensym)))
               `(let ((,objv ,object))
                  (symbol-macrolet
                      ,(mapcar
                        (lambda (slot)
                          (list slot
                                (list
                                 (intern (concatenate 'string (string ',conc-name) (string slot)))
                                 objv))) ',slot-names)
                    ,@body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9 - CONDITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition simple-program-error (program-error)
  ((format-control   :initarg :format-control   :reader simple-program-error-format-control)
   (format-arguments :initarg :format-arguments :reader simple-program-error-format-arguments))
  (:report (lambda (condition stream)
             (format stream "~?"
                     (simple-program-error-format-control condition)
                     (simple-program-error-format-arguments condition)))))

(defmacro handling-errors (&body body)
  "
DO:       Execute the BODY with a handler for CONDITION and
          SIMPLE-CONDITION reporting the conditions.
"
  `(handler-case (progn ,@body)
     (simple-condition  (err)
       (format *error-output* "~&~A:~%~?~&"
               (class-name (class-of err))
               (simple-condition-format-control   err)
               (simple-condition-format-arguments err))
       (finish-output *error-output*))
     (condition (err)
       (format *error-output* "~&~A:~%~A~%" (class-name (class-of err)) err)
       (finish-output *error-output*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 12 - NUMBERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sign (n)
  "
RETURN: -1 if N is negative,
        +1 if N is positive,
         0 if N is 0.
"
  (cond ((zerop n) 0) ((plusp n) 1) (t -1)))


(defmacro incf-mod (&environment env place modulo &optional (increment 1))
  "INCF modulo MODULO"
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (when (cdr store-vars) (error "Can't expand this."))
    `(let* (,@(mapcar (function list) vars vals))
       (let ((,(car store-vars) (mod (+ ,reader-form ,increment) ,modulo)))
         ,writer-form))))


(defmacro decf-mod (&environment env place modulo &optional (decrement 1))
  "DECF modulo MODULO"
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (when (cdr store-vars) (error "Can't expand this."))
    `(let* (,@(mapcar (function list) vars vals))
       (let ((,(car store-vars) (mod (- ,reader-form ,decrement) ,modulo)))
         ,writer-form))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline type-equal-p))
  (defun type-equal-p (t1 t2)
    (and (subtypep t1 t2) (subtypep t2 t1)))

  (defun distinct-float-types ()
    "
RETURN: a subset of (long-float double-float single-float short-float)
that represents the partition of the float type for this
implementation.

There can be fewer than four internal representations for floats. If
there are fewer distinct representations, the following rules apply:

  • If there is only one, it is the type single-float. In this
    representation, an object is simultaneously of types single-float,
    double-float, short-float, and long-float.

  • Two internal representations can be arranged in either of the
    following ways:

      □ Two types are provided: single-float and short-float. An
        object is simultaneously of types single-float,  double-float,
        and long-float.

      □ Two types are provided: single-float and double-float. An
        object is simultaneously of types single-float and
        short-float, or double-float and long-float.

  • Three internal representations can be arranged in either of the
    following ways:

      □ Three types are provided: short-float, single-float, and
        double-float. An object can simultaneously be of  type
        double-float and long-float.

      □ Three types are provided: single-float, double-float, and
        long-float. An object can simultaneously be of  types
        single-float and short-float.

"

    ;; #+emacs
    ;; (insert
    ;;  (karnaugh '(s=i s=d s=l i=d i=l d=l)
    ;;            (list "1" "21" "22" "31" "32" "4"
    ;;                  (cons "i" (lambda (s=i s=d s=l i=d i=l d=l)
    ;;                              (and (==> (and s=i s=d) i=d)
    ;;                                   (==> (and s=i s=l) i=l)
    ;;                                   (==> (and s=i i=d) s=d)
    ;;                                   (==> (and s=i i=l) s=l)
    ;;
    ;;                                   (==> (and s=d s=l) d=l)
    ;;                                   (==> (and s=d i=d) s=i)
    ;;                                   (==> (and s=d i=l) s=l)
    ;;                                   (==> (and s=d d=l) s=l)
    ;;
    ;;                                   (==> (and s=l i=l) s=i)
    ;;                                   (==> (and s=l d=l) s=d)
    ;;
    ;;                                   (==> (and i=d i=l) d=l)
    ;;                                   (==> (and i=d d=l) i=l)
    ;;
    ;;                                   (==> (and s=i s=l) s=d)
    ;;                                   (==> (and s=l s=d) s=i)
    ;;
    ;;                                   (==> (not s=i) (not (or s=d s=l)))
    ;;                                   (==> (not s=d) (not s=l))
    ;;                                   (==> (not i=d) (not i=l))
    ;;                                   (==> (not d=l) (not i=l))
    ;;
    ;;                                   ))))))
    ;;
    ;; 1  short-float=single-float=double-float=long-float
    ;; 21 short-float | single-float=double-float=long-float
    ;; 22 short-float=single-float | double-float=long-float
    ;; 31 short-float | single-float | double-float=long-float
    ;; 32 short-float=single-float | double-float | long-float
    ;; 4  short-float | single-float | double-float | long-float
    ;; not conforming configuruations:
    ;; n1 short-float=single-float=double-float | long-float
    ;; n2 short-float | single-float=double-float | long-float
    ;;
    ;; +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
    ;; | s=i | s=d | s=l | i=d | i=l | d=l |  1  | 21  | 22  | 31  | 32  |  4  | n1  | n2  |
    ;; +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
    ;; | YES | YES | YES | YES | YES | YES |  v  |     |     |     |     |     |     |     |
    ;; | YES | YES |  NO | YES |  NO |  NO |     |     |     |     |     |     |  v  |     |
    ;; |  NO |  NO |  NO | YES | YES | YES |     |  v  |     |     |     |     |     |     |
    ;; |  NO |  NO |  NO | YES |  NO |  NO |     |     |     |     |     |     |     |  v  |
    ;; | YES |  NO |  NO |  NO |  NO | YES |     |     |  v  |     |     |     |     |     |
    ;; | YES |  NO |  NO |  NO |  NO |  NO |     |     |     |     |  v  |     |     |     |
    ;; |  NO |  NO |  NO |  NO |  NO | YES |     |     |     |  v  |     |     |     |     |
    ;; |  NO |  NO |  NO |  NO |  NO |  NO |     |     |     |     |     |  v  |     |     |
    ;; +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
    (let ((s=i (type-equal-p 'short-float 'single-float))
          (i=d (type-equal-p 'single-float 'double-float))
          (d=l (type-equal-p 'double-float 'long-float)))
      (if i=d
          (if s=i
              (if d=l
                  '(single-float) #|1|#
                  '(single-float long-float) #|n1|#)
              (if d=l
                  '(short-float single-float) #|21|#
                  '(short-float single-float long-float) #|n2|#))
          (if s=i
              (if d=l
                  '(single-float double-float) #|22|#
                  '(single-float double-float long-float) #|32|#)
              (if d=l
                  '(short-float single-float double-float) #|31|#
                  '(short-float single-float double-float long-float) #|4|#)))))


  (defun generate-distinct-float-types-typecase (operator expression clauses)
    (let ((types (distinct-float-types)))
      `(,operator ,expression
                  ,@(loop
                      :for (type . body) :in clauses
                      :when (member type types)
                        :collect `(,type ,@body))))))


(defmacro float-typecase (expression &body clauses)
  "
EXPRESSION: an expression evaluate to some value.

CLAUSES:    typecase clauses where the type is one of the standard
            FLOAT direct subtypes, ie. one of (SHORT-FLOAT
            SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT).

NOTE:      Implementations may conflate the various subtypes of FLOAT.
           When two float types are conflated, some implementation
           will signal a warning on any typecase that have them in
           separate clauses.  Since they're the same type, we can as
           well remove the duplicate clauses.

SEE:       CLHS Type SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT

DO:        Expands to a TYPECASE where only the clauses with unique
           float types are present.
"
  (generate-distinct-float-types-typecase 'typecase expression clauses))


(defmacro float-etypecase (expression &body clauses)
  "
EXPRESSION: an expression evaluate to some value.

CLAUSES:    etypecase clauses where the type is one of the standard
            FLOAT direct subtypes, ie. one of (SHORT-FLOAT
            SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT).

NOTE:      Implementations may conflate the various subtypes of FLOAT.
           When two float types are conflated, some implementation
           will signal a warning on any typecase that have them in
           separate clauses.  Since they're the same type, we can as
           well remove the duplicate clauses.

SEE:       CLHS Type SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT

DO:        Expands to a ETYPECASE where only the clauses with unique
           float types are present.
"
  (generate-distinct-float-types-typecase 'etypecase expression clauses))


(defmacro float-ctypecase (expression &body clauses)
  "
EXPRESSION: an expression evaluate to some value.

CLAUSES:    ctypecase clauses where the type is one of the standard
            FLOAT direct subtypes, ie. one of (SHORT-FLOAT
            SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT).

NOTE:      Implementations may conflate the various subtypes of FLOAT.
           When two float types are conflated, some implementation
           will signal a warning on any typecase that have them in
           separate clauses.  Since they're the same type, we can as
           well remove the duplicate clauses.

SEE:       CLHS Type SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT

DO:        Expands to a CTYPECASE where only the clauses with unique
           float types are present.
"
  (generate-distinct-float-types-typecase 'ctypecase expression clauses))


(defun +epsilon (float)
  "Returns the float incremented by the smallest increment possible."
  (multiple-value-bind (significand exponent sign) (decode-float float)
    (* sign (scale-float
             (if (minusp sign)
                 (- significand (float-etypecase float
                                                 (long-float   long-float-negative-epsilon)
                                                 (double-float double-float-negative-epsilon)
                                                 (single-float single-float-negative-epsilon)
                                                 (short-float  short-float-negative-epsilon)))
                 (+ significand (float-etypecase float
                                                 (long-float   long-float-epsilon)
                                                 (double-float double-float-epsilon)
                                                 (single-float single-float-epsilon)
                                                 (short-float  short-float-epsilon))))
             exponent))))

(defun -epsilon (float)
  "Returns the float incremented by the smallest increment possible."
  (multiple-value-bind (significand exponent sign) (decode-float float)
    (* sign (scale-float
             (if (minusp sign)
                 (+ significand (float-etypecase float
                                                 (long-float   long-float-negative-epsilon)
                                                 (double-float double-float-negative-epsilon)
                                                 (single-float single-float-negative-epsilon)
                                                 (short-float  short-float-negative-epsilon)))
                 (- significand (float-etypecase float
                                                 (long-float   long-float-epsilon)
                                                 (double-float double-float-epsilon)
                                                 (single-float single-float-epsilon)
                                                 (short-float  short-float-epsilon))))
             exponent))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 14 - CONSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun maximize (predicate list)
  "
RETURN: The maximum value and the item in list for which predicate
         is the maximum.
"
  (do ((max-value nil)
       (max-item  nil)
       (list list (cdr list))
       (value))
      ((null list) (values max-value max-item))
    (setq value (funcall predicate (car list)))
    (when (or (null max-value) (> value max-value))
      (setq max-value value
            max-item (car list))))) ;;MAXIMIZE


;; (DEFUN COMPUTE-CLOSURE (FUN SET)
;;   "
;; FUN:     set --> P(set)
;;           x |--> { y }
;; RETURN:  The closure of fun on the set.
;; NOTE:    Not a lisp closure!
;; EXAMPLE: (compute-closure (lambda (x) (list (mod (* x 2) 5))) '(1)) --> (2 4 3 1)
;; "
;;   (LOOP
;;      :FOR NEW-SET = (DELETE-DUPLICATES (UNION SET (MAPCAN FUN SET)))
;;      :WHILE (SET-EXCLUSIVE-OR NEW-SET SET)
;;      :DO (SETF SET NEW-SET)
;;      :FINALLY (RETURN NEW-SET)))


(defun compute-closure (fun set)
  (warn "The function ~S has been renamed ~S, please update your programs."
        'compute-closure 'transitive-closure)
  (transitive-closure fun set))

(defun transitive-closure (fun set &key (test 'eql) (use 'list))
  "
FUN:     set --> P(set)
          x |--> { y }
SET:     A sequence.
TEST:    EQL, EQUAL or EQUALP
USE:     Either HASH-TABLE or LIST; specifies the data structure used for the intermediary sets.
RETURN:  A list containing closure of fun on the set.
EXAMPLE: (transitive-closure (lambda (x) (list (mod (* x 2) 5))) '(1)) --> (3 4 2 1)
NOTE:    This version avoids calling FUN twice with the same argument.
"
  ;; current -> fun -> follows
  ;; closure + current -> closure
  ;; follows - closures -> current
  (ecase use
    (list
     (let ((closure '())
           (current '())
           (follows '()))
       (macrolet ((enter     (item  list) `(pushnew ,item ,list :test test))
                  (enter-all (items list) `(setf ,list (delete-duplicates (append ,items ,list) :test test))))
         (setf current (coerce set 'list))
         (loop
           :do (loop ;; current -> fun -> follows
                     :for item :in current
                       :initially (setf follows '())
                     :do (enter-all (funcall fun item) follows)
                         ;; closure + current -> closure
                         (enter item closure))
               (loop ;; follows - closures -> current
                     :for item  :in follows
                       :initially (setf current '())
                     :unless (member item closure :test test)
                       :do (enter item current))
           :while current
           :finally (return-from transitive-closure closure)))))
    (hash-table
     (let ((closure (make-hash-table :test test))
           (current (make-hash-table :test test))
           (follows (make-hash-table :test test)))
       (flet ((enter     (item  hash) (setf (gethash item hash) t))
              (enter-all (items hash) (map nil (lambda (item) (setf (gethash item hash) t)) items)))
         (declare (inline enter enter-all))
         (enter-all set current)
         (loop
           :do (loop ;; current -> fun -> follows
                     :for item :being :each :hash-key :in current
                       :initially (clrhash follows)
                     :do (enter-all (funcall fun item) follows)
                         ;; closure + current -> closure
                         (enter item closure))
               (loop ;; follows - closures -> current
                     :for item :being :each :hash-key :in follows
                       :initially (clrhash current)
                     :unless (gethash item closure)
                       :do (enter item current))
           :while (plusp (hash-table-count current))
           :finally (return-from transitive-closure
                      (loop
                        :for item :being :each :hash-key :in closure
                        :collect item))))))))


;; (array->list array) --> (coerce array 'list)
;; (DEFUN ARRAY->LIST (A) (MAP 'LIST (FUNCTION IDENTITY) A));;ARRAY->LIST

(defun topological-sort (nodes lessp)
  "
RETURN: A list of NODES sorted topologically according to
        the partial order function LESSP.
        If there are cycles (discounting reflexivity),
        then the list returned won't contain all the NODES.
"
  (loop
    :with sorted = '()
    :with incoming = (map 'vector (lambda (to)
                                    (loop
                                      :for from :in nodes
                                      :when (and (not (eq from to))
                                                 (funcall lessp from to))
                                        :sum 1))
                       nodes)
    :with q = (loop
                :for node :in nodes
                :for inco :across incoming
                :when (zerop inco)
                  :collect node)
    :while q
    :do (let ((n (pop q)))
          (push n sorted)
          (loop
            :for m :in nodes
            :for i :from 0
            :do (when (and (and (not (eq n m))
                                (funcall lessp n m))
                           (zerop (decf (aref incoming i))))
                  (push m q))))
    :finally (return (nreverse sorted))))



;; (mapc (function print-cycle) (find-cycles (list-all-packages)
;;                                           (function package-use-list)))

;; (find-cycles (list-all-packages) (function package-use-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 15 - ARRAYS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun vector-init (vector constructor)
  "
DO:      Sets all the slots in vector to the successive results of
         the function CONSTRUCTOR called with integers from 0 up
s         to the dimension of the VECTOR.
RETURN:  VECTOR
"
  (do ((index 0 (1+ index)))
      ((>= index (array-dimension vector 0)))
    (setf (aref vector index) (funcall constructor index)))
  vector)



(defun undisplace-array (array)
  "
RETURN:  The fundamental array and the start and end positions into
         it of a displaced array.
AUTHOR:  Erik Naggum <erik@naggum.no>
"
  (let ((length (length array))
        (start 0))
    (loop
      (multiple-value-bind (to offset) (array-displacement array)
        (if to
            (setq array to
                  start (+ start offset))
            (return (values array start (+ start length))))))))


(defun dichotomy (matchp start end)
  "

MATCHP: A function taking an integer between [START,END[, and
        returning an order (signed integer).
START:  The minimum integer.
END:    The maximum integer+1.
RETURN: (values found index order)
POST:	(<= start index (1- end))
        +-------------------+----------+-------+----------+
        | Case              |  found   | index |  order   |
        +-------------------+----------+-------+----------+
        | x < a[i]          |   FALSE  | start |  less    |
        | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |
        | x = a[i]          |   TRUE   |   i   |  equal   |
        | a[max] < x        |   FALSE  | end-1 |  greater |
        +-------------------+----------+-------+----------+
"
  (let* ((curmin start)
         (curmax end)
         (index  (truncate (+ curmin curmax) 2))
         (order  (funcall matchp index)))
    (loop :while (and (/= 0 order) (/= curmin index)) :do
      ;; (format t "~&min=~S  cur=~S  max=~S  <~S>~%" curmin index curmax (funcall matchp index))
      (if (minusp order)
          (setf curmax index)
          (setf curmin index))
      (setf index (truncate (+ curmin curmax) 2))
      (setf order (funcall matchp index)))
    ;; (format t "~&min=~S  cur=~S  max=~S  <~S> []~%" curmin index curmax (funcall matchp index))
    (when (and (< start index) (minusp order))
      (setf order 1)
      (decf index))
    (assert
     (or (minusp (funcall matchp index))
         (and (plusp (funcall matchp index))
              (or (>= (1+ index) end)
                  (minusp (funcall matchp (1+ index)))))
         (= (funcall matchp index) 0)))
    (values (zerop order) index order)))


(defun dichotomy-search (vector value compare &key
                                                (start 0) (end (length vector))
                                                (key (function identity)))
  "
PRE:	entry is the element to be searched in the table.
        (<= start end)
RETURN: (values found index order)
POST:	(<= start index end)
        +-------------------+----------+-------+----------+
        | Case              |  found   | index |  order   |
        +-------------------+----------+-------+----------+
        | x < a[min]        |   FALSE  |  min  |  less    |
        | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |
        | x = a[i]          |   TRUE   |   i   |  equal   |
        | a[max] < x        |   FALSE  |  max  |  greater |
        +-------------------+----------+-------+----------+
"
  (if (zerop (length vector))
      (values nil 0 -1)
      (let* ((curmin start)
             (curmax end)
             (index  (truncate (+ curmin curmax) 2))
             (order  (funcall compare value (funcall key (aref vector index)))) )
        (loop :while (and (/= 0 order) (/= curmin index)) :do
          ;; (FORMAT T "~&min=~S  cur=~S  max=~S   key=~S <~S> [cur]=~S ~%" CURMIN INDEX CURMAX VALUE (FUNCALL COMPARE VALUE (FUNCALL KEY (AREF VECTOR INDEX))) (AREF VECTOR INDEX))
          (if (< order 0)
              (setf curmax index)
              (setf curmin index))
          (setf index (truncate (+ curmin curmax) 2))
          (setf order  (funcall compare value (funcall key (aref vector index)))))
        (when (and (< start index) (< order 0))
          (setf order 1)
          (decf index))
        (assert (or (minusp (funcall compare value (funcall key (aref vector index))))
                    (and (plusp (funcall compare value (funcall key (aref vector index))))
                         (or (>= (1+ index) end)
                             (minusp (funcall compare value
                                              (funcall key (aref vector (1+  index)))))))
                    (zerop (funcall compare value (funcall key (aref vector index)))))

                (index)
                "The compare function is invalid: value<a[~D] or a[~:*~D]<value<a[~D] or a[~D]<value or a[~0@*~D]=value"
                index (1+ index) end)
        (values (= order 0) index order))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 16 - STRINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro sconc (&rest args)
  "Concatenate strings."
  `(concat ,@args))

(defun concat (&rest items)
  (with-output-to-string (*standard-output*)
    (dolist (item items)
      (typecase item
        (string   (write-string   item *standard-output*))
        (sequence (write-sequence item *standard-output*))
        (t        (with-standard-io-syntax (format *standard-output* "~A" item)))))))

(defmacro scase (keyform &rest clauses)
  "
DO:         A CASE, but for string keys. That is, it uses STRING= as test
            instead of the ''being the same'' test.
"
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond
         ,@(mapcar (lambda (clause)
                     (if (or (eq (car clause) 'otherwise) (eq (car clause) 't))
                         `(t ,@(cdr clause))
                         `((member ,key ',(car clause) :test (function string=))
                           ,@(cdr clause))))
                   clauses)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 17 - SEQUENCES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nsubseq (sequence start &optional (end nil))
  "
RETURN:  When the SEQUENCE is a vector, the SEQUENCE itself, or a dispaced
         array to the SEQUENCE.
         When the SEQUENCE is a list, it may destroy the list and reuse the
         cons cells to make the subsequence.
"
  (if (vectorp sequence)
      (if (and (zerop start) (or (null end) (= end (length sequence))))
          sequence
          (make-array (- (if end
                             (min end (length sequence))
                             (length sequence))
                         start)
                      :element-type (array-element-type sequence)
                      :displaced-to sequence
                      :displaced-index-offset start))
      (let ((result (nthcdr start sequence)))
        (when end
          ;; TODO: when end is bigger than (length sequence), this (setf cdr) fails:
          (setf (cdr (nthcdr (- end start -1) sequence)) nil))
        result)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 18 - HASH-TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-table-keys (table)
  "Returns a list of the keys in the TABLE."
  (let ((result '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k result)) table)
    result))

(defun hash-table-values (table)
  "Returns a list of the values in the TABLE."
  (let ((result '()))
    (maphash (lambda (k v) (declare (ignore k)) (push v result)) table)
    result))

(defun hash-table-entries (table)
  "Returns an a-list of the entries (key . val) in the TABLE."
  (let ((result '()))
    (maphash (lambda (k v) (push (cons k v) result)) table)
    result))

(defun hash-table-select (predicate table)
  "
RETURN: An a-list of  (k . v) from the TABLE
        such as (funcall PREDICATE k v) is true.
"
  (let ((result '()))
    (maphash (lambda (k v) (when (funcall predicate k v)
                             (push (cons k v) result)))
             table)
    result))

(defun hash-table-path (table &rest keys)
  "Given a hash-table that may contain other hash-table, walks down
the path of KEYS, returning the ultimate value"
  (if (null keys)
      table
      (apply (function hash-table-path) (gethash (first keys) table) (rest keys))))

(defun copy-hash-table (table)
  "
TABLE:  (OR NULL HASH-TABLE)
RETURN: If TABLE is NIL, then NIL,
        else a new HASH-TABLE with the same TEST, SIZE, REHASH-THRESHOLD
        REHASH-SIZE and KEY->VALUE associations than TABLE.
        (Neither the keys nor the values are copied).
"
  (check-type table (or null hash-table))
  (when table
    (let ((copy (make-hash-table
                 :test             (hash-table-test             table)
                 :size             (hash-table-size             table)
                 :rehash-threshold (hash-table-rehash-threshold table)
                 :rehash-size      (hash-table-rehash-size      table))))
      (maphash (lambda (k v) (setf (gethash k copy) v)) table)
      copy)))


(defun hashtable (&key (test (function eql))
                    (size nil sizep)
                    (rehash-size nil rehash-size-p)
                    (rehash-threshold nil rehash-threshold-p)
                    elements)
  "Creates a new hash-table, filled with the given ELEMENTS.
ELEMENTS must be a list of lists of two items, the key and the value.
Note: we use the name HASHTABLE to avoid name collision."
  (let ((table (apply (function make-hash-table)
                      :test test
                      (append (when sizep
                                (list :size size))
                              (when rehash-size-p
                                (list :rehash-size rehash-size))
                              (when rehash-threshold-p
                                (list :rehash-threshold rehash-threshold))))))
    (dolist (item elements table)
      (setf (gethash (first item) table) (second item)))))


(defun map-into-hash-table (sequence &key
                                       (key   (function identity))
                                       (value (function identity))
                                       (test  (function eql))
                                       (size nil sizep)
                                       (rehash-size nil rehash-size-p)
                                       (rehash-threshold nil rehash-threshold-p))
  "
Creates a new hash-table, filled with the associations obtained by
applying the function KEY and the function VALUE on each element of
the SEQUENCE.
The other key parameter are passed to MAKE-HASH-TABLE.
"
  (let ((table (apply (function make-hash-table)
                      :test test
                      (append (when sizep
                                (list :size size))
                              (when rehash-size-p
                                (list :rehash-size rehash-size))
                              (when rehash-threshold-p
                                (list :rehash-threshold rehash-threshold))))))
    (map nil (lambda (element)
               (setf (gethash (funcall key element) table) (funcall value element)))
      sequence)
    table))


(defun hash-table-to-sexp (table)
  "Returns a sexp containing the hash-table data."
  (list 'hash-table
        :test (hash-table-test table)
        :size (hash-table-count table)
        :rehash-size (hash-table-rehash-size table)
        :rehash-threashold (hash-table-rehash-threshold table)
        :elements (let ((entries '()))
                    (maphash (lambda (k v) (push (cons k v) entries)) table)
                    entries)))

(defun sexp-to-hash-table (sexp)
  "Create a new hash-table containing the data described in the sexp
\(produced by HASH-TABLE-TO-SEXP."
  (check-type sexp list)
  (assert (eq 'hash-table (first sexp)))
  (let* ((plist (rest sexp))
         (test (getf plist :test 'eql))
         (size (getf plist :size 8))
         (rehash-size (getf plist :rehash-size 1.5))
         (rehash-threshold (getf plist :rehash-threshold 0.85))
         (elements (getf plist :elements))
         (table (make-hash-table :test test
                                 :size size
                                 :rehash-size rehash-size
                                 :rehash-threshold rehash-threshold)))
    (loop :for (k . v) :in elements
          :do (setf (gethash k table) v))
    table))

(defun print-hashtable (table &optional (stream *standard-output*))
  "Prints readably the hash-table, using #. and the HASHTABLE function."
  (format stream "#.(HASHTABLE :TEST (FUNCTION ~S)  :SIZE ~D ~%~
                ~&             :REHASH-SIZE ~A :REHASH-THRESHOLD ~A~%~
                ~&   :ELEMENTS '("
          (hash-table-test table) (hash-table-count table)
          (hash-table-rehash-size table) (hash-table-rehash-threshold table))
  (maphash (lambda (k v) (format stream "~%(~S ~S)" k v)) table)
  (format stream "))")
  ;; (format stream "#.~S"
  ;;         `(let ((table (make-hash-table
  ;;                        :test (function
  ;;                               ,(case (hash-table-test table)
  ;;                                      #+clisp (EXT:FASTHASH-EQ 'eq)
  ;;                                      #+clisp (EXT:FASTHASH-EQL 'eql)
  ;;                                      #+clisp (EXT:FASTHASH-EQUAL 'equal)
  ;;                                      (otherwise  (hash-table-test table))))
  ;;                        :size ,(hash-table-size table))))
  ;;            (setf ,@(let ((assignments '()))
  ;;                         (maphash (lambda (k v)
  ;;                                      (push `(quote ,v) assignments)
  ;;                                    (push `(gethash ',k table) assignments))
  ;;                                  table)
  ;;                         assignments))
  ;;            table))
  table)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRACING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro tracing (&body body)
  "
TRACE works only on non-CL functions.
This macro will work somewhat on any form in body.
"
  `(progn
     ,@(mapcan
        (lambda (form)
          (let ((results (gensym)))
            (list
             `(format *trace-output* "~&~S~%" ',form)
             `(let ((,results (multiple-value-list ,form)))
                (format *trace-output* "~&--> ~{~S~^~%    ~}" ,results)
                (values-list ,results)))))
        body)))



;; (let ((a (1+ b))
;;       (b (1+ a)))
;;   (print (list a b)))
;;
;; (let ((#:a1 (let ((r (1+ b)))
;;               (format t "~S = ~S = ~S~%" '#:a1 '(1+ b) r)
;;               r))
;;       (#:b1 (let ((r (1+ a)))
;;               (format t "~S = ~S = ~S~%" '#:b1 '(1+ a) r)
;;               r))
;;       (a    (progn
;;               (format t "~S = ~S = ~S~%" 'a '#:a1 #:a1)
;;               #:a1))
;;       (b    (progn
;;               (format t "~S = ~S = ~S~%" 'b '#:b1 #:b1)
;;               #:b1)))
;;   (print (list a b)))

(defmacro tracing-let (clauses &body body)
  "
Like LET, but prints on the *trace-output* the value of the bindings.
"
  (let ((vals (mapcar (lambda (clause)
                        (gensym (symbol-name
                                 (if (symbolp clause) clause (first clause)))))
                      clauses))
        (res (gensym)))
    `(let ,(mapcar
            (lambda (val expr)
              `(,val (let ((,res ,expr))
                       (format *trace-output* "~&LET ~S = ~S --> ~S~%"
                               ',val ',expr ,res)
                       ,res)))
            vals
            (mapcar (lambda (clause) (if (symbolp clause) nil (second clause)))
                    clauses))
       (let ,(mapcar
              (lambda (var val)
                `(,var (progn
                         (format *trace-output* "~&LET ~S = ~S --> ~S~%"
                                 ',var ',val ,val)
                         ,val)))
              (mapcar (lambda (clause) (if (symbolp clause) clause (first clause)))
                      clauses)
              vals)
         ,@body))))


(defmacro tracing-let* (clauses &body body)
  "
Like LET*, but prints on the *trace-output* the value of the bindings.
"
  (if (null clauses)
      `(progn ,@body)
      `(tracing-let (,(first clauses))
         (tracing-let* ,(rest clauses) ,@body))))


(defmacro tracing-labels (defs &body body)
  "This macro is a replacement for LABELS that traces the calls of
the local functions."
  `(cl:labels
       ,(mapcar
         (lambda (def)
           (let ((arguments (make-argument-list
                             (parse-lambda-list (second def) :ordinary)))
                 (res (gensym "RESULTS")))
             (when (null (first (last arguments)))
               (setf arguments (butlast arguments)))
             `(,(first def) ,(second def)
               ,@(when (stringp (third def))
                   (list (third def)))
               (format *trace-output*
                       "~&Entering ~A (~{~{:~A ~S~}~^ ~})~%" ',(first def)
                       (list ,@(mapcar (lambda (arg) `(list ',arg ,arg))
                                arguments)))
               (unwind-protect
                    (let (,res)
                      (format *trace-output*
                              "~&Exiting ~A --> ~{~S~^; ~}~%"
                              ',(first def)
                              (setf ,res (multiple-value-list
                                          (progn ,@(cddr def)))))
                      (values-list ,res))
                 (format *trace-output*
                         "~&Unwinding ~A~%" ',(first def))))))
         defs)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binary decision tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun infix-to-tree (sequence)
    (labels ((itt (items start end)
               (cond
                 ((= start end)       nil)
                 ((= (1+ start) end)  (list (aref items start)))
                 (t (let ((pivot (truncate (/ (+ start end) 2))))
                      (list (aref items pivot)
                            (itt items start pivot)
                            (itt items (1+ pivot) end)))))))
      (let ((vect (coerce sequence 'vector)))
        (itt vect 0 (length vect)))))

  (defun map-tree-postfix (fun tree)
    (if (null tree)
        nil
        (funcall fun
                 (first tree)
                 (map-tree-postfix fun (second tree))
                 (map-tree-postfix fun (third  tree))))))



(defmacro decision-tree (expression &rest clauses)
  "
CLAUSES:  Each clause is of the forms:
          (less|:less . <body>)
          (<real> . <body>)
DO:       Evaluate the expression, which must be a real,
          and generate a binary decision tree to select the <body>
          of the clause whose limit is <= the expression and
          the next clause limit is > the expression.
"
  (let ((vexpr (gensym))
        (less (when (and (symbolp (first (first clauses)))
                         (string-equal 'less (first (first clauses))))
                (pop clauses)))
        (clauses (sort (coerce clauses 'vector) (function <)
                       :key (function car))))
    `(let ((,vexpr ,expression))
       ,(map-tree-postfix
         (let ((index -1))
           (flet ((gen-case ()
                    (incf index)
                    (if (zerop index)
                        `(progn ,@(cdr less))
                        `(progn ,@(cdr (aref clauses (1- index)))))))
             (lambda (node left right)
               (if (and (null left) (null right))
                   `(if (< ,vexpr ,(car node))
                        ,(gen-case)
                        ,(gen-case))
                   `(if (< ,vexpr ,(car node))
                        ,left
                        ,(if (null right)
                             (gen-case)
                             right))))))
         (infix-to-tree clauses)))))


(defun xor (a b)
  "Return A ⊻ B"
  (or (and a (not b)) (and (not a) b)))

(defun equiv (a b)
  "Return A ⇔ B"
  (eql (not a) (not b)))

(defun imply (p q)
  "Return P ⇒ Q"
  (or (not p) q))

;; (defun set-equal (a b)
;;   "Return A ⊂ B ∧ A ⊃ B"
;;   (and (subsetp a b) (subsetp b a)))



(defun d-dms (d)
  "Convert a decimal value into an integer and 60th and 3600th
encoded as a \"HH:MM:SS.sss\" string."
  (let* ((epsilon 0.000001d0)
         (hou (floor d))
         (min* (* 60.0d0 (- d hou)))
         (min  (floor min*))
         (sec* (* 60.0d0 (- min* min)))
         (sec))
    (if (< (- 60.0d0 epsilon) sec*)
        (progn
          (setf min (+ min 1.0))
          (if (<= 60.0d0 min)
              (setf min 0.0d0
                    hou (+ 1.0d0 hou)))
          (setf sec* (- (+ epsilon sec*) (floor (+ epsilon sec*))))))
    (setf sec sec*)
    (if (< (- sec (floor sec)) 0.000001d0)
        (format nil "~D:~2,'0D:~2,'0D" hou min  sec)
        (if (< (- 1.0d0 0.000001d0) (- sec (floor sec)))
            (format nil "~D:~2,'0D:~2,'0D" hou min  (+ 0.000001d0 sec))
            (format nil "~D:~2,'0D:~9,6,,,'0F" hou min sec)))))

(defun dms-d (arg &rest rest)
  "Convert value given as an integer and 60th and 3600th,
encoded as a  \"HH:MM:SS.sss\" string, into a decimal value."
  (if (stringp arg)
      (let* ((one (position #\: arg))
             (hou (parse-integer (subseq arg 0 one)))
             (two (if (null one) nil (position #\: arg :start (+ one 1))))
             (min (if (null one) 0d0 (parse-integer (subseq arg (+ one 1) two))))
             (sec (if (null two) 0d0 (let ((value (read-from-string (subseq arg (+ two 1)))))
                                       (assert (typep value 'real))
                                       value))))
        (if (null rest)
            (+ hou (/ min 60.0d0) (/ sec 3600.0d0))
            (error "DMS-D expects either one string or one to three numbers.")))
      (let ((hou arg)
            (min (if (null (first rest))  0d0 (first rest)))
            (sec (if (null (second rest)) 0d0 (second rest))))
        (+ hou (/ min 60.0d0) (/ sec 3600.0d0)))))


;;;; THE END ;;;;

