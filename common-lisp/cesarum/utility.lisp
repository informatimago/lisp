;;;; -*- coding:utf-8 -*-
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
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2003 - 2008
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM" )
  (:EXPORT
   ;; 3 - EVALUATION AND COMPILATION
   "WITH-GENSYMS" "WSIOSBP" "COMPOSE" "COMPOSE-AND-CALL"
   "DEFINE-IF-UNDEFINED"  "INCLUDE" "FUNCTIONAL-PIPE"
   "FIRST-ARG" "SECOND-ARG" "THIRD-ARG" "FOURTH-ARG" "FIFTH-ARG"
   "SIXTH-ARG" "SEVENTH-ARG" "EIGHT-ARG" "NINTH-ARG" "TENTH-ARG"
   ;; 4 - TYPES AND CLASSES
   "DEFENUM" "OP-TYPE-OF"
   ;; 5 - DATA AND CONTROL FLOW
   "SAFE-APPLY" "WHILE" "UNTIL" "FOR"
   ;; 7 - OBJECTS
   "DEFINE-STRUCTURE-CLASS" "DEFINE-WITH-OBJECT" "PJB-DEFCLASS"
   ;; 8 - STRUCTURES
   "DEFINE-WITH-STRUCTURE"
   ;; 9 - CONDITIONS
   "HANDLING-ERRORS"
   ;; 10 - SYMBOLS
   "MAKE-KEYWORD" "CONC-SYMBOL"
   ;; 12 - NUMBERS
   "SIGN"
   ;; 14 - CONSES
   "MAXIMIZE" "COMPUTE-CLOSURE" "TOPOLOGICAL-SORT"
   ;; 15 - ARRAYS
   "VECTOR-INIT" "UNDISPLACE-ARRAY" "DICHOTOMY-SEARCH"
   ;; 16 - STRINGS
   "CONCAT" "SCONC" "SCASE"
   ;; 17 - SEQUENCES
   "NSUBSEQ"
   ;; 18 - HASH-TABLES
   "HASH-TABLE-KEYS" "HASH-TABLE-ENTRIES" "HASH-TABLE-PATH"
   "COPY-HASH-TABLE"
   "HASHTABLE" "PRINT-HASHTABLE" 
   ;;
   "DICHOTOMY"
   "TRACING" "TRACING-LET" "TRACING-LET*" "TRACING-LABELS")
  (:DOCUMENTATION
   "This package exports some utility & syntactic sugar functions and macros.

    Copyright Pascal J. Bourguignon 2003 - 2008
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3 - EVALUATION AND COMPILATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#-:with-debug-gensym
(DEFMACRO WITH-GENSYMS (SYMS &BODY BODY)
  "
DO:      Replaces given symbols with gensyms. Useful for creating macros.
NOTE:    This version by Paul Graham in On Lisp."
  `(LET ,(MAPCAR (LAMBDA (S) `(,S (GENSYM ,(string s)))) SYMS) ,@BODY))


#+:with-debug-gensym
(defpackage "COM.INFORMATIMAGO.GENSYMS" (:USE))
#+:with-debug-gensym
(DEFMACRO WITH-GENSYMS (SYMS &BODY BODY)
  "
DO:      Replaces given symbols with gensyms. Useful for creating macros.
NOTE:    This version by Paul Graham in On Lisp."
  `(LET ,(MAPCAR
          (LAMBDA (S) `(,S (INTERN (STRING (GENSYM ,(string s)))
                                   "COM.INFORMATIMAGO.GENSYMS"))) SYMS) ,@BODY))


(defmacro wsiosbp (&body body)
  (let ((vpack (gensym)))
    `(let ((,vpack *package*))
       (with-standard-io-syntax
         (let ((*package* ,vpack))
           ,@body)))))


(defmacro define-argument-selector (name argument-number)
  (let ((arguments (loop :for i :from 0 :to argument-number :collect (gensym))))
    `(defun ,name (,@(cdr arguments) &rest ,(car arguments))
       (declare (ignore ,@(butlast arguments)))
       ,(car (last arguments)))))
(define-argument-selector first-arg   1)
(define-argument-selector second-arg  2)
(define-argument-selector third-arg   3)
(define-argument-selector fourth-arg  4)
(define-argument-selector fifth-arg   5)
(define-argument-selector sixth-arg   6)
(define-argument-selector seventh-arg 7)
(define-argument-selector eigth-arg   8)
(define-argument-selector ninth-arg   9)
(define-argument-selector tenth-arg   10)


(defun compose-sexp (functions var)
  (if (null functions)
      var
      (list (car functions) (compose-sexp (cdr functions) var))))


(defmacro COMPOSE (&rest functions)
  `(lambda (x) ,(compose-sexp functions 'x)))


(defmacro compose-and-call (&rest functions-and-arg)
  `(funcall ,((lambda (functions) (list 'lambda '(x) (compose-sexp functions 'x))) 
              (butlast functions-and-arg))
            ,(car (last functions-and-arg))))

;; (funcall (compose 1+ sin 1-) 0)
;; (compose-and-call 1+ sin 1- 0)



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


(DEFUN INCLUDE (PATH)
  "
NOTE:    Untasty, but sometimes useful.
DO:      Read from the file at PATH all the sexps and returns a list of them
         prefixed with 'progn.
USAGE:   #.(include \"source.lisp\")
"
  (CONS 'PROGN
        (WITH-OPEN-FILE (FILE PATH :DIRECTION :INPUT :IF-DOES-NOT-EXIST :ERROR)
          (DO ((RESULT '())
               (EOF (GENSYM)))
              ((EQ EOF (CAR RESULT)) (NREVERSE (CDR RESULT)))
            (PUSH (READ FILE NIL EOF) RESULT)))))



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
  (let ((name (if (consp name-and-options)
                  (first name-and-options)
                  name-and-options)))
    (when (stringp (first constants))
      (pop constants))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; define a ({NAME}-LABEL value) function.
       (defun ,(wsiosbp (intern (format nil "~A-LABEL" name)))
           (value)
         (case value
           ,@(loop
                for cname in constants
                with val = -1
                do (if (consp cname)
                       (setf val (second cname))
                       (incf val))
                collect `((,val) ',(if (consp cname)
                                       (first cname)
                                       cname)))
           (otherwise (format nil "#<~A:~D>" ',name value))))
       ;; define the constants.
       ,@(loop
            for cname in constants
            with val = -1
            do (when (consp cname)
                 (setf val (1- (second cname)) cname (first cname)))
            collect `(defconstant ,cname ,(incf val)))
       ;; define the type.
       (deftype ,name ()
         '(member ,@(loop
                       for cname in constants
                       with val = -1
                       do (if (consp cname)
                              (setf val (second cname))
                              (incf val))
                       collect val))))))


(DEFUN OP-TYPE-OF (SYMBOL &OPTIONAL ENV)
  "
From: nikodemus@random-state.net
Newsgroups: comp.lang.lisp
Date: 29 Jul 2004 03:59:50 GMT
Message-ID: <ce9snm$4bp8o$1@midnight.cs.hut.fi>
"
  (IF (FBOUNDP SYMBOL)
      (COND ((MACRO-FUNCTION SYMBOL ENV) 
             'MACRO)
            ((SPECIAL-OPERATOR-P SYMBOL) 
             'SPECIAL-OPERATOR)
            ((COMPILED-FUNCTION-P (SYMBOL-FUNCTION SYMBOL))
             'COMPILED-FUNCTION)
            (t
             'INTERPRETED-FUNCTION))
      (ERROR "Symbol ~S is not an operator." SYMBOL)))


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


(DEFMACRO WHILE (CONDITION &BODY BODY)
  "While loop."
  `(DO () ((NOT ,CONDITION))  ,@BODY))



(DEFMACRO UNTIL (CONDITION &BODY BODY)
  "Until loop."
  `(DO () (,CONDITION)        ,@BODY))



(DEFMACRO FOR ((VAR FIRST LAST . REST) &BODY BODY)
  "For loop.
DO:    Repeat BODY with VAR bound to successive integer values from 
       FIRST to LAST inclusive.
       If the optional STEP argument is abstent, then it is taken as 1 or -1
       depending on the order of FIRST and LAST.
       VAR is incremented by STEP and it stops when VAR goes above
       or below LAST depending on the sign of STEP.
"
  (LET ((FIRSTVAR (GENSYM "FIRST"))
        (LASTVAR  (GENSYM "LAST"))
        (STEPVAR  (GENSYM "STEP"))
        (STEP     (AND REST (CAR REST))))
    (WHEN (CDR REST) (ERROR "Too many forms in FOR parameters."))
    `(LET ((,FIRSTVAR ,FIRST)
           (,LASTVAR ,LAST)
           (,STEPVAR ,STEP))
       (IF (IF ,STEPVAR (< 0 ,STEPVAR) (<= ,FIRSTVAR ,LASTVAR))
           (PROGN  (SETF ,STEPVAR (OR ,STEPVAR 1))
                   (DO ((,VAR ,FIRSTVAR (INCF ,VAR ,STEPVAR)))
                       ((> ,VAR ,LASTVAR))
                     ,@BODY))
           (PROGN  (SETF ,STEPVAR (OR ,STEPVAR -1))
                   (DO ((,VAR ,FIRSTVAR (INCF ,VAR ,STEPVAR)))
                       ((< ,VAR ,LASTVAR))
                     ,@BODY))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7 - OBJECTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
(DEFMACRO PJB-DEFCLASS (NAME SUPER &REST ARGS)
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
  (flet ((ATTRIB (NAME TYPE &REST ARGS)
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
           (LET ((IARG (INTERN (IF (SYMBOLP NAME) (SYMBOL-NAME NAME) NAME)
                               (FIND-PACKAGE "KEYWORD")))
                 INIT DOC)
             (COND  ((= 2 (LENGTH ARGS))
                     (SETQ INIT (CAR  ARGS)
                           DOC  (CADR ARGS)) )
                    ((= 1 (LENGTH ARGS))
                     (IF (STRINGP (CAR ARGS))
                       (SETQ INIT NIL
                             DOC  (CAR ARGS))
                       (SETQ INIT (CAR ARGS)
                             DOC  NIL)) )
                    (T (ERROR "Invalid attribute ~S"
                              `(:att ,name ,type ,@args))))
             (when (AND (SYMBOLP TYPE) (NULL INIT))
               (setf TYPE (LIST 'OR 'NULL TYPE)))
             (when (NULL DOC)
               (setf DOC (SYMBOL-NAME NAME)))
             `(,NAME 
               :INITFORM ,INIT 
               :INITARG  ,IARG
               :ACCESSOR ,NAME
               :TYPE     ,TYPE
               :DOCUMENTATION ,DOC))))
    (LET ((FIELDS  NIL)
          (OPTIONS NIL))
      (DO () ( (NOT ARGS) )
        (COND ((EQ :ATT (CAAR ARGS))
               (PUSH (apply (function ATTRIB) (CDAR ARGS)) FIELDS))
              ((EQ :DOC (CAAR ARGS))
               (PUSH (CONS :DOCUMENTATION (CDAR ARGS)) OPTIONS)))
        (SETF ARGS (CDR ARGS)))
      (SETF FIELDS (NREVERSE FIELDS))
      (SETF OPTIONS (NREVERSE OPTIONS))
      `(DEFCLASS ,NAME ,SUPER ,FIELDS ,@OPTIONS)))) 




(DEFUN GET-OPTION (KEY OPTIONS &OPTIONAL LIST)
  (LET ((OPT (REMOVE-IF (LAMBDA (X) (NOT (EQ KEY (IF (SYMBOLP X) X (CAR X)))))
                        OPTIONS)))
    (COND
      (LIST OPT)
      ((NULL OPT) NIL)
      ((NULL (CDR OPT))
       (IF (SYMBOLP (CAR OPT)) T (CDAR OPT)))
      (T (ERROR "Expected only one ~A option."
                (IF (SYMBOLP (CAR OPT)) (CAR OPT) (CAAR OPT))))))) ;;GET-OPTION


(DEFUN MAKE-NAME (OPTION PREFIX NAME SUFFIX)
  (COND
    ((OR (NULL OPTION) (AND OPTION (NOT (LISTP OPTION))))
     (INTERN (WITH-STANDARD-IO-SYNTAX (FORMAT NIL "~A~A~A" PREFIX NAME SUFFIX))))
    ((AND OPTION (LISTP OPTION) (CAR OPTION))
     (CAR OPTION))
    (T NIL))) ;;MAKE-NAME


(DEFUN GET-NAME (OPTION)
  (IF (AND OPTION (LISTP OPTION))
      (CAR OPTION)
      NIL))

(declaim (ftype (function ((or string symbol character)) symbol) make-keyword))

(DEFMACRO DEFINE-STRUCTURE-CLASS (NAME-AND-OPTIONS &REST DOC-AND-SLOTS)
  "
DO:     Define a class implementing the structure API.
        This macro presents the same API as DEFSTRUCT, but instead of
        defining a structure, it defines a class, and the same functions
        as would be defined by DEFSTRUCT.
        The DEFSTRUCT options: :TYPE and :INITIAL-OFFSET are not supported.
"
  (LET (NAME OPTIONS DOCUMENTATION SLOTS SLOT-NAMES ACCESSORS
             CONC-NAME CONSTRUCTORS COPIER
             INCLUDE INITIAL-OFFSET PREDICATE
             PRINT-FUNCTION PRINT-OBJECT)
    (IF (SYMBOLP NAME-AND-OPTIONS)
        (SETF NAME    NAME-AND-OPTIONS
              OPTIONS NIL)
        (SETF NAME    (CAR NAME-AND-OPTIONS)
              OPTIONS (CDR NAME-AND-OPTIONS)))
    (IF (STRINGP (CAR DOC-AND-SLOTS))
        (SETF DOCUMENTATION (CAR DOC-AND-SLOTS)
              SLOTS         (CDR DOC-AND-SLOTS))
        (SETF DOCUMENTATION NIL
              SLOTS         DOC-AND-SLOTS))
    (SETF CONC-NAME      (GET-OPTION :CONC-NAME      OPTIONS)
          CONSTRUCTORS   (GET-OPTION :CONSTRUCTOR    OPTIONS :LIST)
          COPIER         (GET-OPTION :COPIER         OPTIONS)
          PREDICATE      (GET-OPTION :PREDICATE      OPTIONS)
          INCLUDE        (GET-OPTION :INCLUDE        OPTIONS)
          INITIAL-OFFSET (GET-OPTION :INITIAL-OFFSET OPTIONS)
          PRINT-FUNCTION (GET-OPTION :PRINT-FUNCTION OPTIONS)
          PRINT-OBJECT   (GET-OPTION :PRINT-OBJECT   OPTIONS))
    (WHEN (AND PRINT-OBJECT PRINT-FUNCTION)
      (ERROR "Cannot have :print-object and :print-function options."))
    (WHEN (CDR INCLUDE)
      (SETF SLOTS   (APPEND (CDDR INCLUDE) SLOTS)
            INCLUDE (LIST (CAR INCLUDE))))
    (SETF CONC-NAME (MAKE-NAME CONC-NAME ""      NAME "-")
          COPIER    (MAKE-NAME COPIER    "COPY-" NAME "")
          PREDICATE (MAKE-NAME PREDICATE ""      NAME "-P")
          PRINT-FUNCTION (GET-NAME PRINT-FUNCTION)
          PRINT-OBJECT   (GET-NAME PRINT-OBJECT))
    (SETF SLOT-NAMES (MAPCAR (LAMBDA (S) (IF (SYMBOLP S) S (CAR S))) SLOTS))
    (SETF ACCESSORS  (MAPCAR
                      (LAMBDA (S) (MAKE-NAME NIL (OR CONC-NAME "")
                                             (IF (SYMBOLP S) S (CAR S)) "")) SLOTS))
    (IF (NULL CONSTRUCTORS)
        (SETF CONSTRUCTORS (LIST (MAKE-NAME NIL "MAKE-" NAME "")))
        (SETF CONSTRUCTORS
              (MAPCAN (LAMBDA (X)
                        (COND
                          ((OR (SYMBOLP X) (= 1 (LENGTH X)))
                           (LIST (MAKE-NAME NIL "MAKE-" NAME "")))
                          ((NULL (SECOND X))
                           NIL)
                          ((= 2 (LENGTH X))
                           (LIST (SECOND X)))
                          (T
                           (LIST (LIST (SECOND X) (THIRD X)))))) CONSTRUCTORS)))
    `(PROGN
       (DEFCLASS ,NAME ,INCLUDE
         ,(MAPCAR
           (LAMBDA (SLOT ACCESSOR)
             (IF (SYMBOLP SLOT)
                 `(,SLOT :ACCESSOR  ,ACCESSOR)
                 (LET* ((NAME        (FIRST SLOT))
                        (INITFORM-P  (CDR SLOT))
                        (INITFORM    (CAR INITFORM-P))
                        (TYPE-P      (MEMBER :TYPE (CDDR SLOT)))
                        (TYPE        (CADR TYPE-P))
                        (READ-ONLY-P (MEMBER :READ-ONLY (CDDR SLOT)))
                        (READ-ONLY   (CADR READ-ONLY-P)))
                   `(,NAME
                     ,(IF (AND READ-ONLY-P READ-ONLY) :READER :ACCESSOR)
                     ,ACCESSOR
                     ,@(WHEN INITFORM-P  (LIST :INITFORM INITFORM))
                     ,@(WHEN TYPE-P      (LIST :TYPE     TYPE))))))
           SLOTS ACCESSORS)
         ,@(WHEN DOCUMENTATION (LIST `(:DOCUMENTATION ,DOCUMENTATION))))
       ,@(MAPCAR
          (LAMBDA (CONSTRUCTOR)
            ;; generate a constructor.
            (IF (SYMBOLP CONSTRUCTOR)
                (LET ((PREDS (MAPCAR (LAMBDA (X) (DECLARE (IGNORE X)) (GENSYM))
                                     SLOT-NAMES)))
                  `(DEFUN ,CONSTRUCTOR
                       (&KEY ,@(MAPCAR (LAMBDA (S P) (LIST S NIL P)) SLOT-NAMES PREDS))
                     (LET ((ARGS NIL))
                       ,@(MAPCAR
                          (LAMBDA (S P)
                            `(WHEN ,P
                               (PUSH ,S ARGS)
                               (PUSH ,(MAKE-KEYWORD S) ARGS)))
                          SLOT-NAMES PREDS)
                       (APPLY (FUNCTION MAKE-INSTANCE) ',NAME ARGS))))
                (LET ((CNAME  (FIRST  CONSTRUCTOR))
                      (POSPAR (SECOND CONSTRUCTOR)))
                  (DECLARE (IGNORE POSPAR))
                  (WARN "pjb-defclass does not implement this case yet.")
                  `(DEFUN ,CNAME (&rest args)
                     (declare (ignore args))
                     (error "pjb-defclass does not implement this yet.")))))
          CONSTRUCTORS)
       ,@(WHEN COPIER
               (LIST `(DEFMETHOD ,COPIER ((SELF ,NAME))
                        (MAKE-INSTANCE ',NAME
                          ,@(MAPCAN
                             (LAMBDA (SLOT ACCESSOR)
                               (LIST (MAKE-KEYWORD SLOT) (LIST ACCESSOR 'SELF)))
                             SLOT-NAMES ACCESSORS)))))
       ,@(WHEN PREDICATE
               (LIST `(DEFMETHOD ,PREDICATE (OBJECT)
                        (EQ (TYPE-OF OBJECT) ',NAME))))
       ,@(WHEN PRINT-FUNCTION
               (LIST `(DEFMETHOD PRINT-OBJECT ((SELF ,NAME) STREAM)
                        (,PRINT-FUNCTION SELF STREAM 0))))
       ,@(WHEN PRINT-OBJECT
               (LIST `(DEFMETHOD PRINT-OBJECT ((SELF ,NAME) STREAM)
                        (,PRINT-OBJECT SELF STREAM)))))))



(DEFMACRO DEFINE-WITH-OBJECT (CLASS-NAME SLOTS)
  "
DO:       Define a macro: (WITH-{CLASS-NAME} object &body body)
          expanding to:   (with-slots ({slots}) object @body)
"
  `(DEFMACRO
       ,(INTERN (WITH-STANDARD-IO-SYNTAX (FORMAT NIL "WITH-~A" CLASS-NAME)))
       (OBJECT &BODY BODY)
     `(WITH-SLOTS (QUOTE ,,(MAPCAR (LAMBDA (SLOT) (LIST SLOT SLOT)) SLOTS))
          ,OBJECT ,@BODY)))




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

(DEFMACRO DEFINE-WITH-STRUCTURE (NAME-AND-OPTIONS &rest SLOTS)
  "
NAME-AND-OPTIONS:  Either a structure name or a list (name . options).
          Valid options are: (:conc-name prefix).
DO:       Define a macro: (WITH-{NAME} object &body body)
          expanding to a symbol-macrolet embedding body where
          symbol macros are defined to access the slots.
"
  (LET* ((NAME      (IF (SYMBOLP NAME-AND-OPTIONS)
                      NAME-AND-OPTIONS (CAR NAME-AND-OPTIONS)))
         (CONC-NAME (IF (SYMBOLP NAME-AND-OPTIONS)
                      (CONCATENATE 'STRING (STRING NAME) "-")
                      (LET ((CONC-OPT (CAR (MEMBER :CONC-NAME
                                                   (CDR NAME-AND-OPTIONS)
                                                   :KEY (FUNCTION CAR)))))
                        (IF CONC-OPT
                          (SECOND CONC-OPT)
                          (CONCATENATE 'STRING (STRING NAME) "-")))))
         (slot-names (mapcar (lambda (slot) (if (listp slot) (car slot) slot)) 
                             slots)))
    `(PROGN
       (DEFSTRUCT ,NAME-AND-OPTIONS ,@SLOTS)
       (DEFMACRO
         ,(INTERN (WITH-STANDARD-IO-SYNTAX (FORMAT NIL "WITH-~A" NAME)))
         (OBJECT &BODY BODY)
         (IF (SYMBOLP OBJECT)
           `(SYMBOL-MACROLET
             ,(MAPCAR
               (LAMBDA (SLOT)
                 (LIST SLOT
                       (LIST
                        (INTERN (WITH-STANDARD-IO-SYNTAX 
                                    (CONCATENATE 'STRING
                                      (STRING ',CONC-NAME) (STRING SLOT))))
                        OBJECT))) ',SLOT-names)
             ,@BODY)
           (LET ((OBJV (GENSYM)))
             `(LET ((,OBJV ,OBJECT))
                (SYMBOL-MACROLET
                 ,(MAPCAR
                   (LAMBDA (SLOT)
                     (LIST SLOT
                           (LIST
                            (INTERN (WITH-STANDARD-IO-SYNTAX
                                        (CONCATENATE 'STRING
                                          (STRING ',CONC-NAME) (STRING SLOT))))
                            OBJV))) ',SLOT-names)
                 ,@BODY))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9 - CONDITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro handling-errors (&body body)
  `(HANDLER-CASE (progn ,@body)
     (simple-condition  (ERR) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&")
       (finish-output))
     (condition (ERR) 
       (format *error-output* "~&~A: ~%  ~S~%" (class-name (class-of err)) err)
       (finish-output))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10 - SYMBOLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN MAKE-KEYWORD (SYM)
  "
RETURN: A new keyword with SYM as name.
"
  (INTERN (STRING SYM) (FIND-PACKAGE "KEYWORD")))


(DEFUN CONC-SYMBOL (&REST ARGS)
  "
DO:      Concatenate the arguments and INTERN the resulting string.
NOTE:    The last two arguments maybe :PACKAGE <a-package>
         in which case the symbol is interned into the given package
         instead of *PACKAGE*.
"
  (LET ((PACKAGE *PACKAGE*))
    (WHEN (AND (<= 2 (LENGTH ARGS))
               (EQ :PACKAGE (CAR (LAST ARGS 2))))
      (SETF PACKAGE (CAR (LAST ARGS))
            ARGS (BUTLAST ARGS 2)))
    (INTERN (WITH-STANDARD-IO-SYNTAX
              (APPLY (FUNCTION CONCATENATE) 'STRING
                     (MAPCAR (FUNCTION STRING) ARGS)))
            PACKAGE)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 12 - NUMBERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SIGN (n) (cond ((zerop n) 0) ((plusp n) 1) (t -1)))


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


(defun +epsilon (float)
  "Returns the float incremented by the smallest increment possible."
  (multiple-value-bind (significand exponent sign) (decode-float float)
    (* sign (scale-float
             (if (minusp sign)
                 (- significand (etypecase float
                                  (long-float   long-float-negative-epsilon)
                                  (double-float double-float-negative-epsilon)
                                  (single-float single-float-negative-epsilon)
                                  (short-float  short-float-negative-epsilon)))
                 (+ significand (etypecase float
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
                  (+ significand (etypecase float
                                   (long-float   long-float-negative-epsilon)
                                   (double-float double-float-negative-epsilon)
                                   (single-float single-float-negative-epsilon)
                                   (short-float  short-float-negative-epsilon)))
                  (- significand (etypecase float
                                   (long-float   long-float-epsilon)
                                   (double-float double-float-epsilon)
                                   (single-float single-float-epsilon)
                                   (short-float  short-float-epsilon))))
              exponent))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 14 - CONSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN MAXIMIZE (PREDICATE LIST)
  "
RETURN: The maximum value and the item in list for which predicate
         is the maximum.
"
  (DO ((MAX-VALUE NIL)
       (MAX-ITEM  NIL)
       (LIST LIST (CDR LIST))
       (VALUE))
      ((NULL LIST) (VALUES MAX-VALUE MAX-ITEM))
    (SETQ VALUE (FUNCALL PREDICATE (CAR LIST)))
    (WHEN (OR (NULL MAX-VALUE) (> VALUE MAX-VALUE))
      (SETQ MAX-VALUE VALUE
            MAX-ITEM (CAR LIST))))) ;;MAXIMIZE


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
  "
FUN:     set --> P(set)
          x |--> { y }
RETURN:  The closure of fun on the set.
NOTE:    Not a lisp closure!
EXAMPLE: (compute-closure (lambda (x) (list (mod (* x 2) 5))) '(1)) --> (2 4 3 1)
NOTE:    This version avoids calling FUN twice with the same argument.
"
  (flet ((join (lists)
           (loop
             :with result = '()
             :for list :in lists
             :do (loop :for item :in list :do (push item result))
             :finally (print (return result)))))
    (loop
      :for follows = (delete-duplicates (join (mapcar fun set)))
      :then (delete-duplicates (join (cons follows (mapcar fun newbies))))
      :for newbies = (set-difference follows set)
      :while newbies
      :do (print (list 'newbies newbies))
      :do (setf set (append newbies set))
      :finally (return set))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 15 - ARRAYS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN VECTOR-INIT (VECTOR CONSTRUCTOR)
  "
DO:      Sets all the slots in vector to the successive results of
         the function CONSTRUCTOR called with integers from 0 up
         to the dimension of the VECTOR.
RETURN:  VECTOR
"
  (DO ((INDEX 0 (1+ INDEX)))
      ((>= INDEX (ARRAY-DIMENSION VECTOR 0)))
    (SETF (AREF VECTOR INDEX) (FUNCALL CONSTRUCTOR INDEX)))
  VECTOR) ;;VECTOR-INIT


(DEFUN UNDISPLACE-ARRAY (ARRAY)
  "
RETURN:  The fundamental array and the start and end positions into
         it of a displaced array.
AUTHOR:  Erik Naggum <erik@naggum.no>
"
  (LET ((LENGTH (LENGTH ARRAY))
        (START 0))
    (LOOP
       (MULTIPLE-VALUE-BIND (TO OFFSET) (ARRAY-DISPLACEMENT ARRAY)
         (IF TO
             (SETQ ARRAY TO
                   START (+ START OFFSET))
             (RETURN (VALUES ARRAY START (+ START LENGTH)))))))
  ) ;;UNDISPLACE-ARRAY


(defun dichotomy (matchp min max)
    "

MATCHP: A function taking an integer between START and END, and
        returning an order (signed integer).
MIN:    The minimum integer.
MAX:    The maximum integer.
RETURN: (values found index order)
POST:	(<= min index max)
        +-------------------+----------+-------+----------+----------------+
        | Case              |  found   | index |  order   |     Error      |
        +-------------------+----------+-------+----------+----------------+
        | x < a[i]          |   FALSE  |  min  |  less    |      0         |
        | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |      0         |
        | x = a[i]          |   TRUE   |   i   |  equal   |      0         |
        | a[max] < x        |   FALSE  |  max  |  greater |      0         |
        +-------------------+----------+-------+----------+----------------+
"
    (let* ((curmin min)
           (curmax max)
           (index  (truncate (+ curmin curmax) 2))
           (order  (funcall matchp index)))
      (loop :while (and (/= 0 order) (/= curmin index)) :do
         ;; (FORMAT T "~&min=~S  cur=~S  max=~S   key=~S <~S> [cur]=~S ~%" CURMIN INDEX CURMAX VALUE (FUNCALL COMPARE VALUE (FUNCALL KEY (AREF VECTOR INDEX))) (AREF VECTOR INDEX))
         (if (< order 0)
             (setf curmax index)
             (setf curmin index))
         (setf index (truncate (+ curmin curmax) 2))
         (setf order (funcall matchp index)))
      (when (and (< min index) (< order 0))
        (setf order 1)
        (decf index))
      (assert
       (or (< (funcall matchp index) 0)
           (and (> (funcall matchp index) 0)
                (or (>= (1+ index) max)
                    (< (funcall matchp (1+ index)) 0)))
           (= (funcall matchp index) 0)))
      (values (= order 0) index order)))


(DEFUN DICHOTOMY-SEARCH (VECTOR VALUE COMPARE &KEY
                         (START 0) (END (LENGTH VECTOR))
                         (KEY (FUNCTION IDENTITY)))
  "
PRE:	entry is the element to be searched in the table.
        (<= start end)
RETURN: (values found index order)
POST:	(<= start index end)
        +-------------------+----------+-------+----------+----------------+
        | Case              |  found   | index |  order   |     Error      |
        +-------------------+----------+-------+----------+----------------+
        | x < a[min]        |   FALSE  |  min  |  less    |      0         |
        | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |      0         |
        | x = a[i]          |   TRUE   |   i   |  equal   |      0         |
        | a[max] < x        |   FALSE  |  max  |  greater |      0         |
        +-------------------+----------+-------+----------+----------------+
"
  (LET* ((CURMIN START)
         (CURMAX END)
         (INDEX    (TRUNCATE (+ CURMIN CURMAX) 2))
         (ORDER  (FUNCALL COMPARE VALUE (FUNCALL KEY (AREF VECTOR INDEX)))) )
    (LOOP :WHILE (AND (/= 0 ORDER) (/= CURMIN INDEX)) :DO
       ;; (FORMAT T "~&min=~S  cur=~S  max=~S   key=~S <~S> [cur]=~S ~%" CURMIN INDEX CURMAX VALUE (FUNCALL COMPARE VALUE (FUNCALL KEY (AREF VECTOR INDEX))) (AREF VECTOR INDEX))
       (IF (< ORDER 0)
           (SETF CURMAX INDEX)
           (SETF CURMIN INDEX))
       (SETF INDEX (TRUNCATE (+ CURMIN CURMAX) 2))
       (SETF ORDER  (FUNCALL COMPARE VALUE (FUNCALL KEY (AREF VECTOR INDEX)))))
    (WHEN (AND (< START INDEX) (< ORDER 0))
      (SETF ORDER 1)
      (DECF INDEX))
    (assert
     (or (< (funcall compare value (funcall key (aref vector index))) 0)
         (and (> (funcall compare value (funcall key (aref vector index))) 0)
              (or (>= (1+ index) end)
                  (< (funcall compare value
                              (funcall key (aref vector (1+  index)))) 0)))
         (= (funcall compare value (funcall key (aref vector index))) 0)))
    (VALUES (= ORDER 0) INDEX ORDER)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 16 - STRINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFMACRO SCONC (&REST ARGS)
  "Concatenate strings."
  `(CONCATENATE 'STRING ,@ARGS))


(defun concat (&rest args)
  "Concatenate anything into a string."
  (apply (function concatenate) 'string
         (mapcar (lambda (item)
                   (if (typep item 'sequence) 
                       item
                       (format nil "~A" item))) args)))


(DEFMACRO SCASE (KEYFORM &REST CLAUSES)
  "
DO:         A CASE, but for string keys. That is, it uses STRING= as test
            insteand of the ''being the same'' test.
"
  (LET ((KEY (GENSYM "KEY")))
    `(LET ((,KEY ,KEYFORM))
       (COND
         ,@(MAPCAR (LAMBDA (CLAUSE)
                     (IF (OR (EQ (CAR CLAUSE) 'OTHERWISE) (EQ (CAR CLAUSE) 'T))
                         `(T ,@(CDR CLAUSE))
                         `((MEMBER ,KEY ',(CAR CLAUSE) :TEST (FUNCTION STRING=))
                           ,@(CDR CLAUSE))))
                   CLAUSES)))))


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
          (setf (cdr (nthcdr (- end start -1) sequence)) nil))
        result)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 18 - HASH-TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-table-keys (hash)
  "Returns a list of the keys in the hash-table."
  (let ((result '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k result)) hash)
    result))

(defun hash-table-entries (hash)
  "Returns an a-list of the entries (key . val) in the hash-table."
  (let ((result '()))
    (maphash (lambda (k v) (push (cons k v) result)) hash)
    result))

(defun hash-table-path (htable &rest keys)
  "Given a hash-table that may contain other hash-table, walks down
the path of KEYS, returning the ultimate value"
  (if (null keys)
      htable
      (apply (function hash-table-path) (gethash (first keys) htable) (rest keys))))

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
             `(,(first def) ,(second def)
                ,@(when (stringp (third def))
                        (list (third def)))
                (format *trace-output*
                  "~&Entering ~A (~@{:~A ~S~^ ~})~%" ',(first def)
                  ,@(mapcan (lambda (arg) (list `',arg arg)) arguments))
                (unwind-protect
                     (progn (format *trace-output*
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



;;;; utility.lisp                     --                     --          ;;;;
