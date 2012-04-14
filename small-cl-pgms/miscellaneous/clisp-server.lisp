#!/usr/local/bin/clisp -q -ansi  -on-error debug
;;;;**************************************************************************
;;;;FILE:               clisp-server.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This script implements a CL server and client.
;;;;
;;;;    The server (with the --server option)  waits for connections
;;;;    on port +PORT+ and then read one form, evaluates it, and sends 
;;;;    back the data printed and the multiple value resulting from 
;;;;    the form, or the error if raised.
;;;;
;;;;    The client either sends to the server the forms given on 
;;;;    the command line, or enter a REPL mode (with the --repl option),
;;;;    from which it reads forms, and sends them to the server.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-04-09 <PJB> 
;;;;BUGS
;;;;
;;;;    - The current protocol disconnect between each form evaluated.
;;;;    - We should use the SWANK protocol 
;;;;      (fetch swank client from McClimDesktop).
;;;;    - The restricted cl package is not free of security leaks yet.
;;;;    - It's planed to implement a virtual file system from which
;;;;      the file and stream operators would work.
;;;;    - It's planed to have a multi-server client, where the same forms
;;;;      are sent to various implementations of CL and the results
;;;;      sent back are compared and implementation dependent results
;;;;      are highlighed.
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
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
(eval-when (:compile-topleve :load-toplevel :execute)
  (setf custom:*load-echo* t))

(defpackage "RESTRICTED-COMMON-LISP"
  (:nicknames "RCL")
  (:use "COMMON-LISP")
  (:shadow "LIST-ALL-PACKAGES" "FIND-PACKAGE"))

(in-package "RESTRICTED-COMMON-LISP")

(defconstant *rcl-exports*
  '(
    &ALLOW-OTHER-KEYS &AUX &BODY  &ENVIRONMENT &KEY &OPTIONAL &REST &WHOLE

    ;; *BREAK-ON-SIGNALS*
    *COMPILE-FILE-PATHNAME*
    *COMPILE-FILE-TRUENAME*
    *COMPILE-PRINT*
    *COMPILE-VERBOSE*
    ;; *DEBUG-IO*
    ;; *DEBUGGER-HOOK*
    *DEFAULT-PATHNAME-DEFAULTS*
    *ERROR-OUTPUT*
    *FEATURES*
    *GENSYM-COUNTER*
    ;; *LOAD-PATHNAME*
    *LOAD-PRINT*
    ;; *LOAD-TRUENAME*
    *LOAD-VERBOSE*
    *MACROEXPAND-HOOK*
    ;; *MODULES*
    *PACKAGE*
    *PRINT-ARRAY*
    *PRINT-BASE*
    *PRINT-CASE*
    *PRINT-CIRCLE*
    *PRINT-ESCAPE*
    *PRINT-GENSYM*
    *PRINT-LENGTH*
    *PRINT-LEVEL*
    *PRINT-LINES*
    *PRINT-MISER-WIDTH*
    *PRINT-PPRINT-DISPATCH*
    *PRINT-PRETTY*
    *PRINT-RADIX*
    *PRINT-READABLY*
    *PRINT-RIGHT-MARGIN*
    ;; *QUERY-IO*
    *RANDOM-STATE*
    ;; *READ-BASE*
    ;; *READ-DEFAULT-FLOAT-FORMAT*
    ;; *READ-EVAL*
    ;; *READ-SUPPRESS*
    ;; *READTABLE*
    *STANDARD-INPUT*
    *STANDARD-OUTPUT*
    ;; *TERMINAL-IO*
    ;; *TRACE-OUTPUT*

    * ** ***
    + ++ +++
    -
    / // ///
    = /= < <= > >=
    1+ 1-


    ABORT
    ABS
    ACONS
    ACOS
    ACOSH
    ADD-METHOD
    ADJOIN
    ADJUST-ARRAY
    ADJUSTABLE-ARRAY-P
    ALLOCATE-INSTANCE
    ALPHA-CHAR-P
    ALPHANUMERICP
    AND
    APPEND
    APPLY
    APROPOS
    APROPOS-LIST
    AREF
    ARITHMETIC-ERROR
    ARITHMETIC-ERROR-OPERANDS
    ARITHMETIC-ERROR-OPERATION
    ARRAY
    ARRAY-DIMENSION
    ARRAY-DIMENSION-LIMIT
    ARRAY-DIMENSIONS
    ARRAY-DISPLACEMENT
    ARRAY-ELEMENT-TYPE
    ARRAY-HAS-FILL-POINTER-P
    ARRAY-IN-BOUNDS-P
    ARRAY-RANK
    ARRAY-RANK-LIMIT
    ARRAY-ROW-MAJOR-INDEX
    ARRAY-TOTAL-SIZE
    ARRAY-TOTAL-SIZE-LIMIT
    ARRAYP
    ASH
    ASIN
    ASINH
    ASSERT
    ASSOC
    ASSOC-IF
    ASSOC-IF-NOT
    ATAN
    ATANH
    ATOM
    BASE-CHAR
    BASE-STRING
    BIGNUM
    BIT
    BIT-AND
    BIT-ANDC1
    BIT-ANDC2
    BIT-EQV
    BIT-IOR
    BIT-NAND
    BIT-NOR
    BIT-NOT
    BIT-ORC1
    BIT-ORC2
    BIT-VECTOR
    BIT-VECTOR-P
    BIT-XOR
    BLOCK
    BOOLE
    BOOLE-1
    BOOLE-2
    BOOLE-AND
    BOOLE-ANDC1
    BOOLE-ANDC2
    BOOLE-C1
    BOOLE-C2
    BOOLE-CLR
    BOOLE-EQV
    BOOLE-IOR
    BOOLE-NAND
    BOOLE-NOR
    BOOLE-ORC1
    BOOLE-ORC2
    BOOLE-SET
    BOOLE-XOR
    BOOLEAN
    BOTH-CASE-P
    BOUNDP
    BREAK
    BROADCAST-STREAM
    BROADCAST-STREAM-STREAMS
    BUILT-IN-CLASS
    BUTLAST
    BYTE
    BYTE-POSITION
    BYTE-SIZE
    CAAAAR
    CAAADR
    CAAAR
    CAADAR
    CAADDR
    CAADR
    CAAR
    CADAAR
    CADADR
    CADAR
    CADDAR
    CADDDR
    CADDR
    CADR
    CALL-ARGUMENTS-LIMIT
    CALL-METHOD
    CALL-NEXT-METHOD
    CAR
    CASE
    CATCH
    CCASE
    CDAAAR
    CDAADR
    CDAAR
    CDADAR
    CDADDR
    CDADR
    CDAR
    CDDAAR
    CDDADR
    CDDAR
    CDDDAR
    CDDDDR
    CDDDR
    CDDR
    CDR
    CEILING
    CELL-ERROR
    CELL-ERROR-NAME
    CERROR
    CHANGE-CLASS
    CHAR
    CHAR-CODE
    CHAR-CODE-LIMIT
    CHAR-DOWNCASE
    CHAR-EQUAL
    CHAR-GREATERP
    CHAR-INT
    CHAR-LESSP
    CHAR-NAME
    CHAR-NOT-EQUAL
    CHAR-NOT-GREATERP
    CHAR-NOT-LESSP
    CHAR-UPCASE
    CHAR/=
    CHAR<
    CHAR<=
    CHAR=
    CHAR>
    CHAR>=
    CHARACTER
    CHARACTERP
    CHECK-TYPE
    CIS
    CLASS
    CLASS-NAME
    CLASS-OF
    CLEAR-INPUT
    CLEAR-OUTPUT
    CLOSE
    CLRHASH
    CODE-CHAR
    COERCE
    COMPILATION-SPEED
    COMPILE
    COMPILE-FILE
    COMPILE-FILE-PATHNAME
    COMPILED-FUNCTION
    COMPILED-FUNCTION-P
    COMPILER-MACRO
    COMPILER-MACRO-FUNCTION
    COMPLEMENT
    COMPLEX
    COMPLEXP
    COMPUTE-APPLICABLE-METHODS
    COMPUTE-RESTARTS
    CONCATENATE
    CONCATENATED-STREAM
    CONCATENATED-STREAM-STREAMS
    COND
    CONDITION
    CONJUGATE
    CONS
    CONSP
    CONSTANTLY
    CONSTANTP
    CONTINUE
    CONTROL-ERROR
    COPY-ALIST
    COPY-LIST
    COPY-PPRINT-DISPATCH
    COPY-READTABLE
    COPY-SEQ
    COPY-STRUCTURE
    COPY-SYMBOL
    COPY-TREE
    COS
    COSH
    COUNT
    COUNT-IF
    COUNT-IF-NOT
    CTYPECASE
    DEBUG
    DECF
    DECLAIM
    DECLARATION
    DECLARE
    DECODE-FLOAT
    DECODE-UNIVERSAL-TIME
    DEFCLASS
    DEFCONSTANT
    DEFGENERIC
    DEFINE-COMPILER-MACRO
    DEFINE-CONDITION
    DEFINE-METHOD-COMBINATION
    DEFINE-MODIFY-MACRO
    DEFINE-SETF-EXPANDER
    DEFINE-SYMBOL-MACRO
    DEFMACRO
    DEFMETHOD
    DEFPACKAGE
    DEFPARAMETER
    DEFSETF
    DEFSTRUCT
    DEFTYPE
    DEFUN
    DEFVAR
    DELETE
    DELETE-DUPLICATES
    ;; DELETE-FILE
    DELETE-IF
    DELETE-IF-NOT
    DELETE-PACKAGE
    DENOMINATOR
    DEPOSIT-FIELD
    DESCRIBE
    DESCRIBE-OBJECT
    DESTRUCTURING-BIND
    DIGIT-CHAR
    DIGIT-CHAR-P
    ;; DIRECTORY
    DIRECTORY-NAMESTRING
    DISASSEMBLE
    DIVISION-BY-ZERO
    DO
    DO*
    DO-ALL-SYMBOLS
    DO-EXTERNAL-SYMBOLS
    DO-SYMBOLS
    DOCUMENTATION
    DOLIST
    DOTIMES
    DOUBLE-FLOAT
    DOUBLE-FLOAT-EPSILON
    DOUBLE-FLOAT-NEGATIVE-EPSILON
    DPB
    ;; DRIBBLE
    DYNAMIC-EXTENT
    ECASE
    ECHO-STREAM
    ECHO-STREAM-INPUT-STREAM
    ECHO-STREAM-OUTPUT-STREAM
    ;; ED
    EIGHTH
    ELT
    ENCODE-UNIVERSAL-TIME
    END-OF-FILE
    ENDP
    ENOUGH-NAMESTRING
    ;; ENSURE-DIRECTORIES-EXIST
    ENSURE-GENERIC-FUNCTION
    EQ
    EQL
    EQUAL
    EQUALP
    ERROR
    ETYPECASE
    ;; EVAL
    EVAL-WHEN
    EVENP
    EVERY
    EXP
    EXPORT
    EXPT
    EXTENDED-CHAR
    FBOUNDP
    FCEILING
    FDEFINITION
    FFLOOR
    FIFTH
    FILE-AUTHOR
    FILE-ERROR
    FILE-ERROR-PATHNAME
    FILE-LENGTH
    FILE-NAMESTRING
    FILE-POSITION
    FILE-STREAM
    FILE-STRING-LENGTH
    FILE-WRITE-DATE
    FILL
    FILL-POINTER
    FIND
    ;; FIND-ALL-SYMBOLS
    FIND-CLASS           ; IS PROBABLY A SECURITY LEAK (Gray streams?)
    FIND-IF
    FIND-IF-NOT
    FIND-METHOD
    FIND-PACKAGE                        ; shadowed
    FIND-RESTART
    ;; FIND-SYMBOL
    FINISH-OUTPUT
    FIRST
    FIXNUM
    FLET
    FLOAT
    FLOAT-DIGITS
    FLOAT-PRECISION
    FLOAT-RADIX
    FLOAT-SIGN
    FLOATING-POINT-INEXACT
    FLOATING-POINT-INVALID-OPERATION
    FLOATING-POINT-OVERFLOW
    FLOATING-POINT-UNDERFLOW
    FLOATP
    FLOOR
    FMAKUNBOUND
    FORCE-OUTPUT
    FORMAT
    FORMATTER
    FOURTH
    FRESH-LINE
    FROUND
    FTRUNCATE
    FTYPE
    FUNCALL
    FUNCTION
    FUNCTION-KEYWORDS
    FUNCTION-LAMBDA-EXPRESSION
    FUNCTIONP
    GCD
    GENERIC-FUNCTION
    GENSYM
    GENTEMP
    GET
    GET-DECODED-TIME
    GET-DISPATCH-MACRO-CHARACTER
    GET-INTERNAL-REAL-TIME
    GET-INTERNAL-RUN-TIME
    GET-MACRO-CHARACTER
    GET-OUTPUT-STREAM-STRING
    GET-PROPERTIES
    GET-SETF-EXPANSION
    GET-UNIVERSAL-TIME
    GETF
    GETHASH
    GO
    GRAPHIC-CHAR-P
    HANDLER-BIND
    HANDLER-CASE
    HASH-TABLE
    HASH-TABLE-COUNT
    HASH-TABLE-P
    HASH-TABLE-REHASH-SIZE
    HASH-TABLE-REHASH-THRESHOLD
    HASH-TABLE-SIZE
    HASH-TABLE-TEST
    HOST-NAMESTRING
    IDENTITY
    IF
    IGNORABLE
    IGNORE
    IGNORE-ERRORS
    IMAGPART
    IMPORT
    ;; IN-PACKAGE
    INCF
    INITIALIZE-INSTANCE
    INLINE
    INPUT-STREAM-P
    ;; INSPECT
    INTEGER
    INTEGER-DECODE-FLOAT
    INTEGER-LENGTH
    INTEGERP
    INTERACTIVE-STREAM-P
    ;; INTERN
    INTERNAL-TIME-UNITS-PER-SECOND
    INTERSECTION
    INVALID-METHOD-ERROR
    INVOKE-DEBUGGER
    INVOKE-RESTART
    INVOKE-RESTART-INTERACTIVELY
    ISQRT
    KEYWORD
    KEYWORDP
    LABELS
    LAMBDA
    LAMBDA-LIST-KEYWORDS
    LAMBDA-PARAMETERS-LIMIT
    LAST
    LCM
    LDB
    LDB-TEST
    LDIFF
    LEAST-NEGATIVE-DOUBLE-FLOAT
    LEAST-NEGATIVE-LONG-FLOAT
    LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
    LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
    LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT
    LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
    LEAST-NEGATIVE-SHORT-FLOAT
    LEAST-NEGATIVE-SINGLE-FLOAT
    LEAST-POSITIVE-DOUBLE-FLOAT
    LEAST-POSITIVE-LONG-FLOAT
    LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
    LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
    LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT
    LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
    LEAST-POSITIVE-SHORT-FLOAT
    LEAST-POSITIVE-SINGLE-FLOAT
    LENGTH
    LET
    LET*
    LISP-IMPLEMENTATION-TYPE
    LISP-IMPLEMENTATION-VERSION
    LIST
    LIST*
    LIST-ALL-PACKAGES                   ; shadowed
    LIST-LENGTH
    LISTEN
    LISTP
    ;; LOAD
    LOAD-LOGICAL-PATHNAME-TRANSLATIONS
    LOAD-TIME-VALUE
    LOCALLY
    LOG
    LOGAND
    LOGANDC1
    LOGANDC2
    LOGBITP
    LOGCOUNT
    LOGEQV
    LOGICAL-PATHNAME
    LOGICAL-PATHNAME-TRANSLATIONS
    LOGIOR
    LOGNAND
    LOGNOR
    LOGNOT
    LOGORC1
    LOGORC2
    LOGTEST
    LOGXOR
    LONG-FLOAT
    LONG-FLOAT-EPSILON
    LONG-FLOAT-NEGATIVE-EPSILON
    LONG-SITE-NAME
    LOOP
    LOOP-FINISH
    LOWER-CASE-P
    MACHINE-INSTANCE
    MACHINE-TYPE
    MACHINE-VERSION
    MACRO-FUNCTION
    MACROEXPAND
    MACROEXPAND-1
    MACROLET
    MAKE-ARRAY
    MAKE-BROADCAST-STREAM
    MAKE-CONCATENATED-STREAM
    MAKE-CONDITION
    MAKE-DISPATCH-MACRO-CHARACTER
    MAKE-ECHO-STREAM
    MAKE-HASH-TABLE
    MAKE-INSTANCE
    MAKE-INSTANCES-OBSOLETE
    MAKE-LIST
    MAKE-LOAD-FORM
    MAKE-LOAD-FORM-SAVING-SLOTS
    MAKE-METHOD
    ;; MAKE-PACKAGE
    ;; MAKE-PATHNAME
    MAKE-RANDOM-STATE
    MAKE-SEQUENCE
    MAKE-STRING
    MAKE-STRING-INPUT-STREAM
    MAKE-STRING-OUTPUT-STREAM
    MAKE-SYMBOL
    MAKE-SYNONYM-STREAM
    MAKE-TWO-WAY-STREAM
    MAKUNBOUND
    MAP
    MAP-INTO
    MAPC
    MAPCAN
    MAPCAR
    MAPCON
    MAPHASH
    MAPL
    MAPLIST
    MASK-FIELD
    MAX
    MEMBER
    MEMBER-IF
    MEMBER-IF-NOT
    MERGE
    ;; MERGE-PATHNAMES
    METHOD
    METHOD-COMBINATION
    METHOD-COMBINATION-ERROR
    METHOD-QUALIFIERS
    MIN
    MINUSP
    MISMATCH
    MOD
    MOST-NEGATIVE-DOUBLE-FLOAT
    MOST-NEGATIVE-FIXNUM
    MOST-NEGATIVE-LONG-FLOAT
    MOST-NEGATIVE-SHORT-FLOAT
    MOST-NEGATIVE-SINGLE-FLOAT
    MOST-POSITIVE-DOUBLE-FLOAT
    MOST-POSITIVE-FIXNUM
    MOST-POSITIVE-LONG-FLOAT
    MOST-POSITIVE-SHORT-FLOAT
    MOST-POSITIVE-SINGLE-FLOAT
    MUFFLE-WARNING
    MULTIPLE-VALUE-BIND
    MULTIPLE-VALUE-CALL
    MULTIPLE-VALUE-LIST
    MULTIPLE-VALUE-PROG1
    MULTIPLE-VALUE-SETQ
    MULTIPLE-VALUES-LIMIT
    NAME-CHAR
    ;; NAMESTRING
    NBUTLAST
    NCONC
    NEXT-METHOD-P
    NIL
    NINTERSECTION
    NINTH
    NO-APPLICABLE-METHOD
    NO-NEXT-METHOD
    NOT
    NOTANY
    NOTEVERY
    NOTINLINE
    NRECONC
    NREVERSE
    NSET-DIFFERENCE
    NSET-EXCLUSIVE-OR
    NSTRING-CAPITALIZE
    NSTRING-DOWNCASE
    NSTRING-UPCASE
    NSUBLIS
    NSUBST
    NSUBST-IF
    NSUBST-IF-NOT
    NSUBSTITUTE
    NSUBSTITUTE-IF
    NSUBSTITUTE-IF-NOT
    NTH
    NTH-VALUE
    NTHCDR
    NULL
    NUMBER
    NUMBERP
    NUMERATOR
    NUNION
    ODDP
    ;; OPEN
    OPEN-STREAM-P
    OPTIMIZE
    OR
    OTHERWISE
    OUTPUT-STREAM-P
    PACKAGE
    PACKAGE-ERROR
    PACKAGE-ERROR-PACKAGE
    PACKAGE-NAME
    PACKAGE-NICKNAMES
    PACKAGE-SHADOWING-SYMBOLS
    PACKAGE-USE-LIST
    PACKAGE-USED-BY-LIST
    PACKAGEP
    PAIRLIS
    PARSE-ERROR
    PARSE-INTEGER
    PARSE-NAMESTRING
    PATHNAME
    PATHNAME-DEVICE
    PATHNAME-DIRECTORY
    PATHNAME-HOST
    PATHNAME-MATCH-P
    PATHNAME-NAME
    PATHNAME-TYPE
    PATHNAME-VERSION
    PATHNAMEP
    PEEK-CHAR
    PHASE
    PI
    PLUSP
    POP
    POSITION
    POSITION-IF
    POSITION-IF-NOT
    PPRINT
    PPRINT-DISPATCH
    PPRINT-EXIT-IF-LIST-EXHAUSTED
    PPRINT-FILL
    PPRINT-INDENT
    PPRINT-LINEAR
    PPRINT-LOGICAL-BLOCK
    PPRINT-NEWLINE
    PPRINT-POP
    PPRINT-TAB
    PPRINT-TABULAR
    PRIN1
    PRIN1-TO-STRING
    PRINC
    PRINC-TO-STRING
    PRINT
    PRINT-NOT-READABLE
    PRINT-NOT-READABLE-OBJECT
    PRINT-OBJECT
    PRINT-UNREADABLE-OBJECT
    ;; PROBE-FILE
    PROCLAIM
    PROG
    PROG*
    PROG1
    PROG2
    PROGN
    PROGRAM-ERROR
    PROGV
    PROVIDE
    PSETF
    PSETQ
    PUSH
    PUSHNEW
    QUOTE
    RANDOM
    RANDOM-STATE
    RANDOM-STATE-P
    RASSOC
    RASSOC-IF
    RASSOC-IF-NOT
    RATIO
    RATIONAL
    RATIONALIZE
    RATIONALP
    READ
    READ-BYTE
    READ-CHAR
    READ-CHAR-NO-HANG
    READ-DELIMITED-LIST
    READ-FROM-STRING
    READ-LINE
    READ-PRESERVING-WHITESPACE
    READ-SEQUENCE
    READER-ERROR
    READTABLE
    READTABLE-CASE
    READTABLEP
    REAL
    REALP
    REALPART
    REDUCE
    REINITIALIZE-INSTANCE
    REM
    REMF
    REMHASH
    REMOVE
    REMOVE-DUPLICATES
    REMOVE-IF
    REMOVE-IF-NOT
    REMOVE-METHOD
    REMPROP
    RENAME-FILE
    RENAME-PACKAGE
    REPLACE
    REQUIRE
    REST
    RESTART
    RESTART-BIND
    RESTART-CASE
    RESTART-NAME
    RETURN
    RETURN-FROM
    REVAPPEND
    REVERSE
    ROOM
    ROTATEF
    ROUND
    ROW-MAJOR-AREF
    RPLACA
    RPLACD
    SAFETY
    SATISFIES
    SBIT
    SCALE-FLOAT
    SCHAR
    SEARCH
    SECOND
    SEQUENCE
    SERIOUS-CONDITION
    SET
    SET-DIFFERENCE
    SET-DISPATCH-MACRO-CHARACTER
    SET-EXCLUSIVE-OR
    SET-MACRO-CHARACTER
    SET-PPRINT-DISPATCH
    SET-SYNTAX-FROM-CHAR
    SETF
    SETQ
    SEVENTH
    SHADOW
    SHADOWING-IMPORT
    SHARED-INITIALIZE
    SHIFTF
    SHORT-FLOAT
    SHORT-FLOAT-EPSILON
    SHORT-FLOAT-NEGATIVE-EPSILON
    SHORT-SITE-NAME
    SIGNAL
    SIGNED-BYTE
    SIGNUM
    SIMPLE-ARRAY
    SIMPLE-BASE-STRING
    SIMPLE-BIT-VECTOR
    SIMPLE-BIT-VECTOR-P
    SIMPLE-CONDITION
    SIMPLE-CONDITION-FORMAT-ARGUMENTS
    SIMPLE-CONDITION-FORMAT-CONTROL
    SIMPLE-ERROR
    SIMPLE-STRING
    SIMPLE-STRING-P
    SIMPLE-TYPE-ERROR
    SIMPLE-VECTOR
    SIMPLE-VECTOR-P
    SIMPLE-WARNING
    SIN
    SINGLE-FLOAT
    SINGLE-FLOAT-EPSILON
    SINGLE-FLOAT-NEGATIVE-EPSILON
    SINH
    SIXTH
    SLEEP
    SLOT-BOUNDP
    SLOT-EXISTS-P
    SLOT-MAKUNBOUND
    SLOT-MISSING
    SLOT-UNBOUND
    SLOT-VALUE
    SOFTWARE-TYPE
    SOFTWARE-VERSION
    SOME
    SORT
    SPACE
    SPECIAL
    SPECIAL-OPERATOR-P
    SPEED
    SQRT
    STABLE-SORT
    STANDARD
    STANDARD-CHAR
    STANDARD-CHAR-P
    STANDARD-CLASS
    STANDARD-GENERIC-FUNCTION
    STANDARD-METHOD
    STANDARD-OBJECT
    STEP
    STORAGE-CONDITION
    STORE-VALUE
    STREAM
    STREAM-ELEMENT-TYPE
    STREAM-ERROR
    STREAM-ERROR-STREAM
    STREAM-EXTERNAL-FORMAT
    STREAMP
    STRING
    STRING-CAPITALIZE
    STRING-DOWNCASE
    STRING-EQUAL
    STRING-GREATERP
    STRING-LEFT-TRIM
    STRING-LESSP
    STRING-NOT-EQUAL
    STRING-NOT-GREATERP
    STRING-NOT-LESSP
    STRING-RIGHT-TRIM
    STRING-STREAM
    STRING-TRIM
    STRING-UPCASE
    STRING/=
    STRING<
    STRING<=
    STRING=
    STRING>
    STRING>=
    STRINGP
    STRUCTURE
    STRUCTURE-CLASS
    STRUCTURE-OBJECT
    STYLE-WARNING
    SUBLIS
    SUBSEQ
    SUBSETP
    SUBST
    SUBST-IF
    SUBST-IF-NOT
    SUBSTITUTE
    SUBSTITUTE-IF
    SUBSTITUTE-IF-NOT
    SUBTYPEP
    SVREF
    SXHASH
    SYMBOL
    SYMBOL-FUNCTION
    SYMBOL-MACROLET
    SYMBOL-NAME
    SYMBOL-PACKAGE
    SYMBOL-PLIST
    SYMBOL-VALUE
    SYMBOLP
    SYNONYM-STREAM
    SYNONYM-STREAM-SYMBOL
    T
    TAGBODY
    TAILP
    TAN
    TANH
    TENTH
    TERPRI
    THE
    THIRD
    THROW
    TIME
    TRACE
    TRANSLATE-LOGICAL-PATHNAME
    TRANSLATE-PATHNAME
    TREE-EQUAL
    TRUENAME
    TRUNCATE
    TWO-WAY-STREAM
    TWO-WAY-STREAM-INPUT-STREAM
    TWO-WAY-STREAM-OUTPUT-STREAM
    TYPE
    TYPE-ERROR
    TYPE-ERROR-DATUM
    TYPE-ERROR-EXPECTED-TYPE
    TYPE-OF
    TYPECASE
    TYPEP
    UNBOUND-SLOT
    UNBOUND-SLOT-INSTANCE
    UNBOUND-VARIABLE
    UNDEFINED-FUNCTION
    UNEXPORT
    UNINTERN
    UNION
    UNLESS
    UNREAD-CHAR
    UNSIGNED-BYTE
    UNTRACE
    UNUSE-PACKAGE
    UNWIND-PROTECT
    UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
    UPDATE-INSTANCE-FOR-REDEFINED-CLASS
    UPGRADED-ARRAY-ELEMENT-TYPE
    UPGRADED-COMPLEX-PART-TYPE
    UPPER-CASE-P
    USE-PACKAGE
    USE-VALUE
    USER-HOMEDIR-PATHNAME
    VALUES
    VALUES-LIST
    VARIABLE
    VECTOR
    VECTOR-POP
    VECTOR-PUSH
    VECTOR-PUSH-EXTEND
    VECTORP
    WARN
    WARNING
    WHEN
    WILD-PATHNAME-P
    WITH-ACCESSORS
    WITH-COMPILATION-UNIT
    WITH-CONDITION-RESTARTS
    WITH-HASH-TABLE-ITERATOR
    WITH-INPUT-FROM-STRING
    ;; WITH-OPEN-FILE
    ;; WITH-OPEN-STREAM
    WITH-OUTPUT-TO-STRING
    ;; WITH-PACKAGE-ITERATOR
    WITH-SIMPLE-RESTART
    WITH-SLOTS
    WITH-STANDARD-IO-SYNTAX
    WRITE
    WRITE-BYTE
    WRITE-CHAR
    WRITE-LINE
    WRITE-SEQUENCE
    WRITE-STRING
    WRITE-TO-STRING
    Y-OR-N-P
    YES-OR-NO-P
    ZEROP
    ))

(export *rcl-exports*)

(defpackage "RESTRICTED-COMMON-LISP-USER"
  (:nicknames "RCL-USER")
  (:use  "RESTRICTED-COMMON-LISP"))

(in-package "RESTRICTED-COMMON-LISP")

(defvar *all-packages* (mapcar (function cl:find-package)
                               '("RESTRICTED-COMMON-LISP"
                                 "RESTRICTED-COMMON-LISP-USER"
                                 "KEYWORD"
                                 #+(and clisp regexp) "REGEXP")))
(defun list-all-packages () (copy-list *all-packages*))
(defun find-package (name)
  (let ((name (cl:find-package name)))
    (and (member name *all-packages*) name)))


(in-package "COMMON-LISP-USER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARSE-SEXP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cuts an input stream into strings containing one token.
;;;
;;; (with-input-from-string (in "a 1 \"s\" pac:sym | hi | \\abc #|com|# ;com
;;;  (1 2 3) #2A((a b) (c d)) #x123 #'fun")
;;;   (loop :for s = (parse-sexp in nil nil) :while s  :collect s))
;;; --> 
;;; ("a " "1 " "\"s\"" " pac:sym " "| hi |" " \\abc " "#|com|# ;com
;;;  (1 2 3)" " #2A((a b) (c d)) " "#x123 #'fun")

(define-condition sexp-end-of-file (end-of-file simple-condition)
  ())

#+(or)
(defconstant +character-syntax-types+
  ;; Other characters are constituents.
  (:whitespace      .  #( #\Space #\Newline #\Tab #\Return #\Linefeed #\Page ))
  (:multiple-escape             . "|")
  (:non-terminating-macro-char  . "#")
  (:single-escape               . "\\")
  (:terminating-macro-char      . "\"'(),;`"))

(defconstant +white-spaces+  
  '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page))
(defconstant +digits+ 
  '#.(loop :for i :from 0 :to 9 :collect (digit-char i)))

(defun parse-sexp (&optional (stream *standard-input*)
                   (eof-error-p t) (eof-value nil))
  "Read character on STREAM and collect them in a string,
parsing up to one sexp. 
If an EOF occurs during the parsing of an object, 
a SEXP-END-OF-FILE condition is raised.
If an EOF occurs before a sexp is read, 
then either a SEXP-END-OF-FILE condition is raised 
or the EOF-VALUE is returned according to EOF-ERROR-P.
Spaces, and comments are read and collected in the output string.
The syntax for strings, comment and lists is the default CL reader one.
No reader macro is processed.  #2A(a b c) is parsed as two tokens:
#2A and (a b c).
"
  ;; sexp ::= token
  ;; token ::= string | text | semi-colon-comment | sharp-pipe-comment
  ;; text ::= /([^ ]|\.)+/
  ;; string ::= /"([^"]|\.)*"/
  ;; semi-colon-comment ::= /;.*$/
  ;; sharp-pipe-comment ::= #| ( text-comment | sharp-pipe-comment )* |#
  (with-output-to-string (text)
    (loop
       :named parser
       :with state = :init
       :with stack = '()
       :for ch = (peek-char nil stream nil nil)
       :do (labels 
               ((accept (ch) (princ (read-char stream) text))
                (rerror (eof-error-p ctrlstr &rest args)
                  (if eof-error-p
                      (error (make-instance 'sexp-end-of-file
                               :format-control (concatenate 'string "~A: " 
                                                            ctrlstr)
                               :format-arguments (cons 'parse-sexp args)
                               :stream stream))
                      (return-from parse-sexp eof-value)))
                (eof-error (where)
                  (rerror t "input stream ~S ends within ~A" stream where))
                (end (token)
                  (case token
                    ((:eof) 
                     (rerror eof-error-p
                             "input stream ~S has reached its end"
                             stream))
                    ((:eof-object)   (eof-error "an object"))
                    ((:eof-string)   (eof-error "a string"))
                    ((:eof-sharp)    (eof-error
                                      "a read macro beginning with #\\#"))
                    ((:eof-comment)  (eof-error "a comment #| ... |#"))
                    ((:eof-single)   (eof-error 
                                      "a token after single escape character"))
                    ((:eof-multiple) (eof-error 
                                      "a token after multiple escape character"))
                    ((:close) 
                     (rerror t "READ from ~S: an object cannot start with #\\)"
                             stream))
                    (otherwise (loop-finish))))
                (shift (new-state) (setf state new-state))
                (spop () (funcall (pop stack))))
             (macrolet ((spush (return new-state)
                          `(progn (push (lambda () ,(if (keywordp return)
                                                        `(shift ,return)
                                                        return)) stack)
                                  (shift ,new-state))))
               (ecase state 
                 ((:init)
                  (case ch
                    (#.+white-spaces+ (accept ch))
                    ((#\;)         (accept ch) (spush :init     :semi-comment))
                    ((#\")         (accept ch) (spush (end :string) :string))
                    ((#\()         (accept ch) (spush (end :list)   :list))
                    ((#\))         (end :close))
                    ((#\' #\, #\`) (accept ch))
                    ((#\#)         (accept ch) (shift :init-sharp))
                    ((nil)         (end :eof))
                    ((#\\)         (spush (end :token) :token))
                    ((#\|)         (spush (end :token) :token))
                    (otherwise     (accept ch) (spush (end :token) :token))))
                 ((:init-sharp)
                  (case ch
                    ((#\|)       (accept ch) (spush :init        :comment))
                    ((nil)       (end :eof-sharp))
                    (otherwise   (accept ch) (spush (end :token) :sharp-token))))
                 ((:list-sharp)
                  (case ch
                    ((#\|)       (accept ch) (spush :list :comment))
                    ((nil)       (end :eof-sharp))
                    (otherwise   (accept ch) (spush :list :sharp-token))))
                 ((:semi-comment)
                  (case ch
                    ((#\newline) (accept ch) (spop))
                    ((nil)       (end (if (null stack) :eof :eof-object)))
                    (otherwise   (accept ch))))
                 ((:string)
                  (case ch
                    ((#\\)       (accept ch) (shift :string-escape))
                    ((#\")       (accept ch) (spop))
                    ((nil)       (end :eof-string))
                    (otherwise   (accept ch))))
                 ((:string-escape) 
                  (case ch
                    ((nil)       (end :eof-string))
                    (otherwise   (accept ch) (shift :string))))
                 ((:comment)
                  (case ch
                    ((#\|)       (accept ch) (shift :comment-end))
                    ((#\#)       (accept ch) (shift :comment-sharp))
                    ((nil)       (end :eof-comment))
                    (otherwise   (accept ch))))
                 ((:comment-end)
                  (case ch
                    ((#\#)       (accept ch) (spop))
                    ((nil)       (end :eof-comment))
                    (otherwise   (accept ch) (shift :comment))))
                 ((:comment-sharp)
                  (case ch
                    ((#\|)       (accept ch) (spush :comment  :comment))
                    ((nil)       (end :eof-comment))
                    (otherwise   (accept ch) (shift :comment))))
                 ((:list)
                  (case ch
                    (#.+white-spaces+ (accept ch))
                    ((#\;)         (accept ch) (spush :list :semi-comment))
                    ((#\")         (accept ch) (spush :list :string))
                    ((#\()         (accept ch) (spush :list :list))
                    ((#\#)         (accept ch) (shift :list-sharp))
                    ((#\))         (accept ch) (spop))
                    ((#\' #\, #\`) (accept ch))
                    ((nil)         (end :eof-object))
                    (otherwise     (accept ch) (spush :list :token))))
                 ((:token)
                  (case ch
                    (#.+white-spaces+ (accept ch) (spop))
                    ((#\;)            (accept ch) (spush (spop) :semi-comment))
                    ((#\\)            (accept ch) (spush :token :single-escape))
                    ((#\|)            (accept ch) (shift :multiple-escape))
                    ((#\")            (spop))
                    ((#\( #\))        (spop))
                    ((#\' #\, #\`)    (spop))
                    ((nil)            (spop))
                    (otherwise (accept ch))))
                 ((:sharp-token)
                  (case ch
                    (#.+digits+       (accept ch))
                    ((#\: #\@)        (accept ch) (shift :sharp-colon-at))
                    ((nil)            (end :eof-object))
                    (otherwise        (accept ch) (shift :sharp-rest))))
                 ((:sharp-colon-at)
                  (case ch
                    ((#\: #\@)        (accept ch))
                    ((nil)            (end :eof-object))
                    (otherwise        (accept ch) (shift :sharp-rest))))
                 ((:sharp-rest)
                  (case ch
                    (#.+white-spaces+ (accept ch) (spop))
                    ((#\\)     (accept ch) (spush :sharp-rest :single-escape))
                    ((#\|)     (accept ch) (spush :sharp-rest :multiple-escape))
                    ((#\()     (accept ch) (spush :sharp-rest :list))
                    ((#\))     (spop))
                    ((#\")     (spop))
                    ((nil)     (spop))
                    (otherwise (accept ch))))
                 ((:single-escape)
                  (case ch
                    ((nil) (end :eof-single))
                    (otherwise (accept ch) (spop))))
                 ((:multiple-escape)
                  (case ch
                    ((nil) (end :eof-multiple))
                    ((#\|) (accept ch) (spop))
                    ((#\\) (accept ch) (spush :multiple-escape :single-escape))
                    (otherwise (accept ch))))))))))


#||

(with-input-from-string (in "  ; comment
#| 1 1
tralala |#
 2  Hello   World! #\\GREEK_SMALL_LETTER_LAMDA #2A((a b) (c d ) ) zzz")
  (loop :for s = (parse-sexp in nil nil) :while s :collect s))

(with-input-from-string (in "  (  a ( d e f ) c )  ")
  (loop :for s = (parse-sexp in nil nil) :while s :collect s))
(with-input-from-string (in "(a (d e f) c)")
  (loop :for s = (parse-sexp in nil nil) :while s :collect s))


(mapcar 'read-from-string
        (with-input-from-string (in "  ; comment
#| 1 1
tralala |#
 2  Hello   World! #\\GREEK_SMALL_LETTER_LAMDA #2A((a #| b)#|hi|#
 (1|# 3) (c d ) ) zzz #x123ABC\"def\"")
          (loop :for s = (parse-sexp in nil nil) :while s  :collect s)))

||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant +port+ 19000)

(defun remote-eval (form-text)
  (with-open-stream (socket (socket:socket-connect +port+))
    (when (eql :output (socket:socket-status socket))
      (let ((*print-circle* t)
            (*print-readably* t))
        (princ form-text socket) (finish-output socket)
        (let ((results (read socket nil socket)))
          (cond ((atom results)
                 (format t "~&Got invalid result message: ~S~%"
                         results))
                ((eql :ok (car results))
                 (unless (= 0 (length (second results)))
                   (format t "~&Printed:  ~A" (second results)))
                 (unless (null (third results))
                   (format t "~&Returned: ~{~A~^ ;~%          ~}~%"
                           (third results))))
                ((eql :error (car results))
                 (format t "~&ERROR: ~A~%" (cdr results)))
                (t (format t "~&Got invalid result message: ~S~%"
                           results))))))))

(defun read-new-value ()
  (format *query-io* "Enter a new value: ")
  (multiple-value-list (eval (read *query-io*))))

(defmacro with-restricted-dynamic-environment (&body body)
  `(with-output-to-string (*standard-output*)
     (with-input-from-string (*standard-input* "")
       (let* ((*print-circle* t)
              (*print-readably* nil)
              (*error-output* *standard-output*)
              (*trace-output* *standard-output*)
              (*query-io* (MAKE-TWO-WAY-STREAM *standard-input*
                                               *standard-output*))
              (*terminal-io* *query-io*)
              (*package* (find-package "RCL-USER"))
              (*read-eval* nil)
              (-))
         (restart-case (progn ,@body)
           (abort          ()      :report "Aborted")
           (continue       ()      :report "Continued")
           (muffle-warning ()      :report "Warning muffled")
           (store-value    (value) :report "Value stored"
                                   :interactive read-new-value)
           (use-value      (value) :report "Value used"
                                   :interactive read-new-value))))))

(defun check-form/packages (form &key (in-packs '(:rcl-user :keyword)) 
                                 (out-packs '(:rcl :regexp)))
  (cond ((symbolp form)
         (dolist (pack in-packs)
           (multiple-value-bind (sym status)  
               (find-symbol (symbol-name form) pack)
             (when status
               (return-from check-form/packages nil))))
         (dolist (pack out-packs)
           (multiple-value-bind (sym status)  
               (find-symbol (symbol-name form) pack)
             (when (eql status :external)
               (return-from check-form/packages nil))))
         (symbol-package form))
        ((atom form) nil)
        (t (or (check-form/packages (car form))
               (check-form/packages (cdr form))))))

(defun server-rep (socket logout)
  (let* ((values)
         (output
          (with-restricted-dynamic-environment
              ;; (print (list logout *standard-output*))
              (let ((form (read socket nil socket)))
                (if (eql form socket)
                    (format logout "~&Got EOF~%")
                    (let ((pack (check-form/packages form)))
                      (if pack
                          (error "Unknown package ~S" pack)
                          (progn
                            (format logout "~&Got request to evaluate: ~S~%"
                                    form)
                            (setf values  (let ((- form))
                                            (multiple-value-list
                                             (eval form))))))))))))
    (let ((*print-circle* t)
          (*print-readably* nil))
      (print (list :ok output (mapcar (lambda (x) (format nil "~S" x)) values))
             socket)
      (terpri socket)
      (finish-output socket))))


(defun main ()
  (cond
    ((or (null ext:*args*)
         (member "--help"  ext:*args* :test (function string=)))
     (let ((name (pathname-name (load-time-value *load-pathname*))))
       (format t "~%~A --server &  # to start the server~%~
                 ~:*~A --repl      # to start a local REPL communicating~%~
                   ~VA             # with the server~%~
                ~3:*~A <sexp>      # to send one S-EXP to the server~2%"
               name (length name) "")))
    ((string= "--server" (or (first ext:*args*) ""))
     (let ((server (socket:socket-server +port+))
           (log-output *terminal-io*))
       (unwind-protect
            (loop
               (handler-case
                   (with-open-stream (socket (socket:socket-accept server))
                     (MULTIPLE-VALUE-BIND (remote-host remote-port)
                         (SOCKET:SOCKET-STREAM-PEER socket)
                       (format log-output "~&Accepted connection from ~A ~A~%"
                               remote-host remote-port))
                     (handler-case
                         (SERVER-REP socket log-output)
                       (t (err)
                         (let ((*print-circle* t)
                               (*print-readably* nil))
                           (print (cons :error (format nil "~A" err)) socket)
                           (terpri socket) (finish-output socket)))))
                 #+clisp
                 (SYSTEM::INTERRUPT-CONDITION (err) 
                   (format log-output "~&Got error: ~A~%"#|"~:*~S~%"|# err)
                   (format log-output "Exiting.~%")
                   (ext:quit))
                 (t (err) 
                   (format log-output "~&Got error: ~A~%"#|"~:*~S~%"|# err))))
         (socket:socket-server-close server))))
    ((string= "--repl" (or (first ext:*args*) ""))
     (loop
        (format t "~2%Welcome to the remote ~A REPL~2%" (lisp-implementation-type))
        (format t "Exit by typing: :QUIT or :EXIT alone.~2%")
        (handler-case
            (loop
               :for form-text = (progn
                                  (format t "~&LISP> ") (finish-output)
                                  (parse-sexp *standard-input* nil nil))
               :until (or (null form-text)
                          (member  (string-trim +white-spaces+ form-text)
                                   '(":QUIT" ":EXIT" "(QUIT)" "(EXIT)")
                                   :test (function string-equal)))
               :do (remote-eval form-text)
               :finally (return-from main))
          (error (err) (format *error-output* "~%~A~%" err)))))
    (t
     (dolist (form-text ext:*args*)
       (princ form-text)
       (remote-eval form-text)))))

(main)


;; (funcall (first (find-all-symbols "OPEN")) "/tmp/a")