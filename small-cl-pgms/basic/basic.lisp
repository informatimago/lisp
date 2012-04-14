;;****************************************************************************
;;FILE:               basic.lisp
;;LANGUAGE:           Common-Lisp
;;SYSTEM:             Common-Lisp
;;USER-INTERFACE:     standard i/o
;;DESCRIPTION
;;
;;    Quick, Dirty and Ugly BASIC.
;;
;;    This is a silly BASIC interpreter.  The lines are tokenized and stored
;;    as-is in an array indexed by the line number.  When interpreting the
;;    program, the instructions are parsed directly from there ; the
;;    expressions are parsed into trees which are then evaluated.
;;    The variables are stored into a hash table indexed by their
;;    identifier (symbol). Undefined variables are taken as 0 or "".
;;    We distinguish number and string variables depending on the presence
;;    of a '$' character in the last position of the variable identifier.
;;    Variables are reset by the command RUN. (A program can be restarted
;;    without losing the variable using the GOTO or GOSUB statements).
;;
;;    Commands are not distinguished from statements and may occur in a
;;    program. In particular, LOAD could be used to load a subprogram
;;    overlay, and takes a line number where to jump to. 
;;
;;    Programs are loaded and saved in source form.
;;
;;SYNOPSIS
;;
;;    (LOAD (COMPILE-FILE "BASIC.LISP"))
;;    (COM.INFORMATIMAGO.COMMON-LISP.BASIC:MAIN)
;;
;;
;;    command ::= number statements | statements .
;;    statements ::= statement { ':' statement } .
;;    statement ::=
;;            PRINT [ expression { ( ',' | ';' ) expression }
;;          | INPUT string identifier { ',' identifier }
;;          | READ  identifier { ',' identifier }
;;          | DATA  ( string | number ) { ',' ( string | number ) }
;;          | RESTORE [ expression ]
;;          | GOTO      expression
;;          | GOSUB expression
;;          | RETURN
;;          | STOP
;;          | REM whatever-up-to-the-end-of-line
;;          | identifier '=' expression
;;          | FOR identifier '=' expression TO expression [ STEP expression ]
;;          | NEXT [ identifier ]
;;          | IF condition THEN statements [ ':' ELSE statements ]
;;          | LIST
;;          | DIR [name.type]
;;          | SAVE string
;;          | LOAD string [ number ]
;;          | ERASE ( ALL | number { number } )
;;          | RUN
;;          | BYE
;;          .
;;    expression  ::= expression ( '+' | '-' ) term .
;;    term        ::= term       ( '*' | '/' | 'mod' ) fact .
;;    fact        ::= fact       ( '^' ) simp .
;;    simp        ::= number | string | identifier | '(' expression ')'
;;                  | ( '+' | '-' ) simp .
;;    condition   ::= disjonction .
;;    disjonction ::= disjonction { 'OR' conjonction }  | conjonction .
;;    conjonction ::= conjonction { 'AND' logicalnot }  | logicalnot .
;;    logicalnot  ::= comparaison | 'NOT' logicalnot | '(' disjonction ')'
;;    comparaison ::= expression ( '<' | '<=' | '>' | '>=' | '=' | '<>' )
;;                                 expression .
;;    identifier  ::= alpha { alphanum } [ '$' ].
;;    string      ::= '"' { any-character-but-double-quote } '"' .
;;    number      ::= digit { digit } .
;;
;;    The '+' operator can be used to concatenate strings.
;;
;;AUTHORS
;;    <PJB> Pascal Bourguignon
;;MODIFICATIONS
;;    2005-09-26 <PJB> Added missing :NICKNAMES.
;;    2003-05-19 <PJB> Created (in 2 days).
;;BUGS
;;    NOT IMPLEMENTED YET: scanning floating point.
;;                         scanning parenthesis (we have them in parser).
;;                         built-in functions: SIN COS ATAN EXP LOG
;;                                             LEFT$ MID$ RIGHT$ ...
;;                         arrays
;;
;;    This code would be happier with some factoring (basic-eval).
;;
;;    Some more testing could be used.
;;
;;    The program is stored in a fixed-size array (1000).
;;    Perhaps we should provide either for a bigger array
;;    or for a sparse structure (hash?).
;;
;;    Missing as a test case: a LISP interpreter implemented in BASIC.
;;    (Of course, this BASIC interpreter implemented in LISP should then
;;    be tested over the LISP interpreter implemented in BASIC :-).
;;
;;    Two-letter operators are not parsed correctly ("<>" --> "<>" and ">").
;;
;;LEGAL
;;    GPL
;;
;;    Copyright Pascal Bourguignon 2003 - 2003
;;    mailto:pjb@informatimago.com
;;
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************

(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.BASIC"
  (:NICKNAMES "BASIC")
  (:USE "COMMON-LISP")
  (:EXPORT "BASIC" "MAIN")
  );;BASIC
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.BASIC")


(DEFVAR *PROGRAM* (MAKE-ARRAY '(1000) :INITIAL-ELEMENT NIL))
(DEFVAR *STACK*   (MAKE-ARRAY '(100)
                              :INITIAL-ELEMENT NIL
                              :ADJUSTABLE T
                              :FILL-POINTER 0));;*STACK*
(DEFVAR *VARIABLES* (MAKE-HASH-TABLE :TEST (FUNCTION EQ) :SIZE 100))

(DEFVAR *CURRENT-LINE* 0)
(DEFVAR *DATA-PTR* (CONS 0 NIL) "marker for DATA/READ/RESTORE")


(DEFMACRO WHILE (CONDITION &BODY BODY) `(DO () ((NOT ,CONDITION)) ,@BODY))
(DEFMACRO UNTIL (CONDITION &BODY BODY) `(DO () (,CONDITION)       ,@BODY))


(DEFUN SPLIT-LINE (LINE)
  "
DO:         Split the line between the special characters:
            space , ; : < > <= >= = <>  + - * / ^
            as one token.  The special characters are enclosed  in pipes.
RETURN:     A list of token string (where spaces are removed) ;
            nil or an error message string.
NOTE:       No parentheses: yuck maths!  No dots in numbers: yuck maths!
"
  (DO ((I 0 (1+ I))
       (P 0)
       (PARTS ())
       (ERR NIL))
      ((<= (LENGTH LINE) I)
       (VALUES (PROGN (WHEN (< P (LENGTH LINE))
                        (PUSH (SUBSEQ LINE P (LENGTH LINE)) PARTS))
                      (NREVERSE PARTS)) ERR))
    (MACROLET ((PUSH-PART () `(WHEN (< P I)
                                (PUSH (SUBSEQ LINE P I) PARTS)
                                (SETQ P I))))
      (COND
       ((CHAR= (CHAR LINE I) (CHARACTER " "))
        (PUSH-PART)
        (INCF P))
       ((CHAR= (CHAR LINE I) (CHARACTER "\""))
        (PUSH-PART)
        (INCF I)
        (DO ()
            ((OR (<= (LENGTH LINE) I) (CHAR= (CHAR LINE I) (CHARACTER "\""))))
          (INCF I))
        (IF (< I (LENGTH LINE)) (INCF I))
        (PUSH-PART)
        (DECF I))
       ((POSITION (CHAR LINE I) ",;:=+-*/^")
        (PUSH-PART)
        (INCF P)
        (PUSH (FORMAT NIL "|~A|" (SUBSEQ LINE I P)) PARTS))
       ((CHAR= (CHAR LINE I) (CHARACTER "<"))
        (PUSH-PART)
        (IF (OR (CHAR= (CHAR LINE (1+ I)) (CHARACTER "="))
                (CHAR= (CHAR LINE (1+ I)) (CHARACTER ">")))
          (PROGN (PUSH (FORMAT NIL "|~A|" (SUBSEQ LINE I (+ I 2))) PARTS)
                 (SETQ P (INCF I)))
          (PROGN (INCF P)
                 (PUSH (FORMAT NIL "|~A|" (SUBSEQ LINE I P)) PARTS))))
       ((CHAR= (CHAR LINE I) (CHARACTER ">"))
        (PUSH-PART)
        (IF  (CHAR= (CHAR LINE (1+ I)) (CHARACTER "="))
          (PROGN (PUSH (FORMAT NIL "|~A|" (SUBSEQ LINE I (+ I 2))) PARTS)
                 (SETQ P (INCF I)))
          (PROGN (INCF P)
                 (PUSH (FORMAT NIL "|~A|" (SUBSEQ LINE I P)) PARTS))))
       ((OR (ALPHANUMERICP (CHAR LINE I))
            (CHAR= (CHARACTER "$") (CHAR LINE I))
            (CHAR= (CHARACTER "%") (CHAR LINE I))))
       (T
        (SETQ ERR (FORMAT NIL "INVALID CHARACTER: '~A' AT POSITION ~D."
                          (CHAR LINE I) I))
        (SETQ I (LENGTH LINE))))))
  );;SPLIT-LINE




(DEFUN FETCH-DATA ()
  "
RETURN:     The data found at or following *DATA-PTR*, or NIL if none remains.
DO:         Increments *DATA-PTR*, or issue an error (setting *CURRENT-LINE*).
"
  (WHILE (AND (< (CAR *DATA-PTR*) (ARRAY-DIMENSION *PROGRAM* 0))
              (NULL (CDR *DATA-PTR*)))
    (INCF (CAR *DATA-PTR*))
    (WHILE (AND (< (CAR *DATA-PTR*) (ARRAY-DIMENSION *PROGRAM* 0))
                (OR (NULL (AREF *PROGRAM* (CAR *DATA-PTR*)))
                    (NOT (EQ 'DATA (CAR (AREF *PROGRAM* (CAR *DATA-PTR*)))))))
      (INCF (CAR *DATA-PTR*)))
    (IF (AND (< (CAR *DATA-PTR*) (ARRAY-DIMENSION *PROGRAM* 0))
             (AREF *PROGRAM* (CAR *DATA-PTR*)))
      (SETF (CDR *DATA-PTR*) (CDR (AREF *PROGRAM* (CAR *DATA-PTR*))))))
  ;;(format t "data-ptr= ~S~%" *data-ptr*)
  (IF (NULL (CDR *DATA-PTR*))
    (PROGN  (BERROR "NO MORE DATA TO READ.") NIL)
    (IF (AND (CDR (CDR *DATA-PTR*))
             (OR (NULL (CDDR (CDR *DATA-PTR*)))
                 (NOT (EQ '|,| (CADR (CDR *DATA-PTR*))))
                 (NOT (OR (STRINGP (CAR (CDR *DATA-PTR*)))
                          (NUMBERP (CAR (CDR *DATA-PTR*)))))))
      (PROGN (BERROR "MALFORMED DATA LINE ~A." (CAR *DATA-PTR*))    NIL)
      (PROG1 (POP (CDR *DATA-PTR*)) (POP (CDR *DATA-PTR*))))))



(DEFMACRO PROTECT-BREAK (FORM)
  `(HANDLER-CASE
       (VALUES ,FORM)
     (T () (FORMAT T "~&BREAK~%") (SETQ *CURRENT-LINE* NIL) NIL)
     (:NO-ERROR (DATA) DATA)))


(DEFUN INPUT-DATA (TYPE)
  (COND
   ((EQ TYPE 'STRING) (PROTECT-BREAK (READ-LINE)))
   ((EQ TYPE 'NUMBER) (PROTECT-BREAK (READ)))))




(DEFUN FIND-FOR (VARIABLE)
  "
DO:         Finds the first entry in the stack that is a list beginning
            with :FOR and the VARIABLE, or just :FOR if VARIABLE is NIL.
            (compared with EQ).
NOTE:       If found, any entry above the found entry are poped.
RETURN:     NIL or the entry.
"
  (DO ((POINTER (1- (FILL-POINTER *STACK*)) (DECF POINTER)))
      ((OR (< POINTER 0)
           (AND (CONSP (AREF *STACK* POINTER))
                (EQ :FOR     (CAR    (AREF *STACK* POINTER)))
                (OR (NULL VARIABLE)
                    (EQ VARIABLE (SECOND (AREF *STACK* POINTER))))))
       (IF (< POINTER 0)
         NIL
         (PROGN
           (SETF (FILL-POINTER *STACK*) (1+ POINTER))
           (AREF *STACK* POINTER))))))


(DEFUN FIND-GOSUB ()
  "
DO:         Finds the first entry in the stack that is a list beginning
            with :GOSUB.
NOTE:       If found, any entry above the found entry are poped.
RETURN:     NIL or the entry.
"
  (DO ((POINTER (1- (FILL-POINTER *STACK*)) (DECF POINTER)))
      ((OR (< POINTER 0)
           (AND (CONSP (AREF *STACK* POINTER))
                (EQ :GOSUB     (CAR    (AREF *STACK* POINTER)))))
       (IF (< POINTER 0)
         NIL
         (PROGN
           (SETF (FILL-POINTER *STACK*) (1+ POINTER))
           (AREF *STACK* POINTER))))))



(DEFUN BERROR (FMT &REST ARGS)
  "
DO:         Prints an error message formated from fmt and args.
"
  (IF *CURRENT-LINE*
    (FORMAT T "~&ERROR LINE ~D: ~A~%"
            *CURRENT-LINE* (APPLY (FUNCTION FORMAT) NIL FMT ARGS))
    (FORMAT T "~&ERROR: ~A~%"  (APPLY (FUNCTION FORMAT) NIL FMT ARGS)))
  (SETQ *CURRENT-LINE* NIL))


(DEFUN CHECK-LINE (LINENUM)
  "
DO:         Check the line number and issue an error message.
RETURN:     Whether the linenum is a valid line number.
"
  (DECLARE (INTEGER LINENUM))
  (IF (OR (< LINENUM 1)
          (<= (ARRAY-DIMENSION *PROGRAM* 0) LINENUM))
    (PROGN (BERROR "LINE NUMBER OUT OF RANGE (1..~D)."
                   (ARRAY-DIMENSION *PROGRAM* 0))
           NIL)
    T))


(DEFUN FIND-LINE-OR-NEXT (LINENUM)
  "
PRE:       (check-line linenum)
RETURN:    If line linenum exists then line linenum
           else the line with the minimum line number greater than linenum
           or else nil.
"
  (IF (OR (<= LINENUM 0) (<= (ARRAY-DIMENSION *PROGRAM* 0) LINENUM))
    (PROGN (SETQ *CURRENT-LINE* NIL)
           NIL)
    (DO* ((LINENUM LINENUM (1+ LINENUM))
          (LINE (AREF *PROGRAM* LINENUM) (AREF *PROGRAM* LINENUM)) )
        ((OR LINE (= (ARRAY-DIMENSION *PROGRAM* 0) (1+ LINENUM)))
         (IF LINE
           (PROGN (SETQ *CURRENT-LINE* LINENUM)
                  LINE)
           (PROGN (SETQ *CURRENT-LINE* NIL)
                  NIL))))))




(DEFUN SLURP-EXPRESSION (TOKENS TERMINALS)
  "
DO:         Parse tokens until a terminal or end of list's found.
RETURN:     A list of tokens making an expression ;
            A cdr of tokens.
"
  (DO ((EXPR ())
       (TOKENS TOKENS (CDR TOKENS)))
      ((OR (NULL TOKENS)
           (MEMBER (CAR TOKENS) TERMINALS :TEST (FUNCTION EQ)))
       (VALUES (NREVERSE EXPR) TOKENS))
    (PUSH (CAR TOKENS) EXPR)))


;;; expr : term { [+|-] expr }
;;; term : fact { [*|/] term }
;;; fact : simple { ^ fact }
;;; simple : ident | number | ( expr ) .

(DEFUN PARSE-SIMP (SIMP)
  "
DO:         Parses a simple expression:
            simp ::= number | string | identifier | ( expr ) .
NOTE:       We're missing a function call: identifier ( expr { , expr } )
RETURN:     A parse tree or :ERROR ; a cdr of simp.
"
  (COND
   ((MEMBER (CAR SIMP) '(+ -))
    (MULTIPLE-VALUE-BIND (EXPR REST) (PARSE-SIMP (CDR SIMP))
      (IF (EQ :ERROR EXPR)
        (VALUES EXPR REST)
        (IF (EQ (CAR SIMP) '+)
          (VALUES EXPR REST)
          (VALUES (LIST 'NEG EXPR) REST)))))
   ((NUMBERP (CAR SIMP)) (VALUES (CAR SIMP) (CDR SIMP)))
   ((STRINGP (CAR SIMP)) (VALUES (CAR SIMP) (CDR SIMP)))
   ((SYMBOLP (CAR SIMP)) (VALUES (CAR SIMP) (CDR SIMP)))
   ((EQ '|(| (CAR SIMP))
    (MULTIPLE-VALUE-BIND (EXPR REST) (PARSE-EXPR (CDR SIMP))
      (IF (EQ '|)| (CAR REST))
        (VALUES EXPR (CDR REST))
        (PROGN
          (BERROR "MISSING A CLOSING PARENTHESE.")
          (VALUES :ERROR NIL)))))
   (T (BERROR "INVALID TOKEN IN EXPRESSION ~S." (CAR SIMP)))))



(DEFMACRO MAKE-PARSE-LEVEL (NAME OPERATORS NEXT)
  "
DO:         Generate a function named PARSE-{name} that parses the
            following rule:  name ::= name { operators next } .
            That functions will return a parse tree or :ERROR ; a cdr of expr.
"
  (LET ((PARSE-LEVEL-NAME (INTERN (FORMAT NIL "PARSE-~A" NAME)))
        (PARSE-NEXT-NAME  (INTERN (FORMAT NIL "PARSE-~A" NEXT))))
    `(DEFUN ,PARSE-LEVEL-NAME (EXPR)
       (LET ((RESULT))
         (MULTIPLE-VALUE-BIND (TERM REST) (,PARSE-NEXT-NAME EXPR)
           (SETQ RESULT TERM EXPR REST))
         (DO () ((OR (EQ :ERROR RESULT)
                     (NULL EXPR)
                     (NOT (MEMBER (CAR EXPR) ',OPERATORS
                                  :TEST (FUNCTION EQ)))))
           (MULTIPLE-VALUE-BIND (TERM REST) (,PARSE-NEXT-NAME (CDR EXPR))
             (IF (EQ :ERROR TERM)
               (SETQ RESULT :ERROR)
               (SETQ RESULT (LIST (CAR EXPR) RESULT TERM)
                     EXPR   REST))))
         (VALUES RESULT EXPR)))))

(DEFUN PARSE-LNOT (LNOT)
  "
DO:         Parses a simple logical expression:
            lnot ::= comp | NOT lnot | ( disj ).
RETURN:     A parse tree or :ERROR ; a cdr of expr.
"
  (COND
   ((EQ (CAR LNOT) 'NOT)
    (MULTIPLE-VALUE-BIND (EXPR REST) (PARSE-LNOT (CDR LNOT))
      (IF (EQ :ERROR EXPR)
        (VALUES EXPR REST)
        (VALUES (LIST 'NOT EXPR) REST))))
   ((EQ '|(| (CAR LNOT))
    (MULTIPLE-VALUE-BIND (EXPR REST) (PARSE-DISJ (CDR LNOT))
      (IF (EQ '|)| (CAR REST))
        (VALUES EXPR (CDR REST))
        (PROGN
          (BERROR "MISSING A CLOSING PARENTHESE.")
          (VALUES :ERROR NIL)))))
   (T (PARSE-COMP LNOT))))


(MAKE-PARSE-LEVEL FACT (^)       SIMP)
(MAKE-PARSE-LEVEL TERM (* / MOD) FACT)
(MAKE-PARSE-LEVEL EXPR (+ -)     TERM)
(MAKE-PARSE-LEVEL COMP (< <= > >= = <>) EXPR)
(MAKE-PARSE-LEVEL CONJ (AND) LNOT)
(MAKE-PARSE-LEVEL DISJ (OR)  CONJ)



(DEFUN BDIV (A B)
  "
RETURN: A floating-point division of a by b.
"
  (IF (EQUAL 0 B)
    (PROGN
      (BERROR "DIVISION BY ZERO.")
      NIL)
    (/ (FLOAT A) B)))




(DEFUN BOOLP (OPERAND)  (MEMBER OPERAND '(:TRUE :FALSE)))
(DEFUN BAND (A B) (AND (EQ :TRUE A) (EQ :TRUE B)))
(DEFUN BOR  (A B) (OR  (EQ :TRUE A) (EQ :TRUE B)))
(DEFUN BNOT (A)   (EQ :FALSE A))
(DEFUN BOOL (LISP-BOOL) (IF LISP-BOOL :TRUE :FALSE))

(DEFMACRO MAKE-COMPARISON (NAME OPERATOR NUMBER-OP STRING-OP)
  `(DEFUN ,NAME (A B)
     (COND
      ((AND (NUMBERP A) (NUMBERP B)) (BOOL (,NUMBER-OP A B)))
      ((AND (STRINGP A) (STRINGP B)) (BOOL (,STRING-OP A B)))
      (T (BERROR "INCOMPATIBLE OPERANDS FOR ~A." ',OPERATOR)))))

(MAKE-COMPARISON BLT <  <  STRING< )
(MAKE-COMPARISON BLE <= <= STRING<=)
(MAKE-COMPARISON BGT >  >  STRING> )
(MAKE-COMPARISON BGE >= >= STRING>=)
(MAKE-COMPARISON BEQ =  =  STRING= )
(MAKE-COMPARISON BNE <> /= STRING/=)


(DEFMACRO NUM-OP (OPERATOR OPERATION)
  "PRIVATE MACRO for BASIC-EVAL-TREE"
  `(LET ((LEFT  (BASIC-EVAL-TREE (SECOND TREE)))
         (RIGHT (BASIC-EVAL-TREE (THIRD  TREE))))
     (COND
      ((AND (NUMBERP LEFT) (NUMBERP RIGHT)) (,OPERATION LEFT RIGHT))
      (T (BERROR "INCOMPATIBLE OPERANDS FOR ~A." ',OPERATOR)    NIL))))

(DEFMACRO COMP-OP (OPERATOR OPERATION)
  "PRIVATE MACRO for BASIC-EVAL-TREE"
  `(LET ((LEFT  (BASIC-EVAL-TREE (SECOND TREE)))
         (RIGHT (BASIC-EVAL-TREE (THIRD  TREE))))
     (COND
      ((AND (NUMBERP LEFT) (NUMBERP RIGHT)) (,OPERATION LEFT RIGHT))
      ((AND (STRINGP LEFT) (STRINGP RIGHT)) (,OPERATION LEFT RIGHT))
      (T (BERROR "INCOMPATIBLE OPERANDS FOR ~A." ',OPERATION)    NIL))))

(DEFMACRO BOOL-OP (OPERATOR OPERATION)
  "PRIVATE MACRO for BASIC-EVAL-TREE"
  `(LET ((LEFT  (BASIC-EVAL-TREE (SECOND TREE)))
         (RIGHT (BASIC-EVAL-TREE (THIRD  TREE))))
     (COND
      ((AND (BOOLP LEFT) (BOOLP RIGHT)) (,OPERATION LEFT RIGHT))
      (T (BERROR "INCOMPATIBLE OPERANDS FOR ~A." ',OPERATION)     NIL))))



(DEFUN BASIC-EVAL-TREE (TREE)
  "
DO:         Evaluate an expression tree.
RETURN:     NIL or the computed value.
"
  (COND
   ((NUMBERP TREE) TREE)
   ((STRINGP TREE) TREE)
   ((SYMBOLP TREE)
    (LET ((VALUE (GETHASH TREE *VARIABLES*)))
      (UNLESS VALUE
        (SETQ VALUE
              (SETF (GETHASH TREE *VARIABLES*)
                    (IF (CHAR= (CHARACTER "$")
                               (CHAR (SYMBOL-NAME TREE)
                                     (1- (LENGTH (SYMBOL-NAME TREE)))))
                      "" 0))))
      VALUE))
   ((CONSP TREE)
    (CASE (CAR TREE)
      (-   (NUM-OP  -   -))
      (*   (NUM-OP  *   *))
      (/   (NUM-OP  /   BDIV))
      (^   (NUM-OP  ^   EXPT))
      (MOD (NUM-OP  MOD MOD))
      (AND (BOOL-OP AND BAND))
      (OR  (BOOL-OP OR  BOR))
      (<   (COMP-OP <   BLT))
      (<=  (COMP-OP <=  BLE))
      (>   (COMP-OP >   BGT))
      (>=  (COMP-OP >=  BGE))
      (=   (COMP-OP =   BEQ))
      (<>  (COMP-OP <>  BNE))
      (+ (LET ((LEFT  (BASIC-EVAL-TREE (SECOND TREE)))
               (RIGHT (BASIC-EVAL-TREE (THIRD  TREE))))
           (COND
            ((AND (STRINGP LEFT) (STRINGP RIGHT))
             (CONCATENATE 'STRING LEFT RIGHT))
            ((AND (NUMBERP LEFT) (NUMBERP RIGHT))      (+ LEFT RIGHT))
            (T (BERROR "INCOMPATIBLE OPERANDS FOR +.") NIL))))
      (NOT (LET ((LEFT  (BASIC-EVAL-TREE (SECOND TREE))))
             (COND
              ((BOOLP LEFT)                                   (BNOT LEFT))
              (T (BERROR "INCOMPATIBLE OPERANDS FOR UNARY NOT.") NIL))))
      (NEG (LET ((LEFT  (BASIC-EVAL-TREE (SECOND TREE))))
             (COND
              ((NUMBERP LEFT)                                    (- LEFT))
              (T (BERROR "INCOMPATIBLE OPERANDS FOR UNARY -.")   NIL))))
      (OTHERWISE (BERROR "UNEXPECTED OPERATOR ~A." (CAR TREE))   NIL)))
   (T (BERROR "UNEXPECTED OPERAND ~A." TREE)                     NIL)))



(DEFUN BASIC-EVAL-EXPRESSION (EXPR)
  "
DO:         Parses the BASIC expression EXPR and evaluates it.
RETURN:     NIL or the computed value.
"
  (MULTIPLE-VALUE-BIND (TREE REST) (PARSE-EXPR EXPR)
    (COND
     ((EQ :ERROR TREE)
      (BERROR "SYNTAX ERROR IN EXPRESSION ~A." EXPR)
      NIL)
     ((NULL REST)
      (BASIC-EVAL-TREE TREE))
     (T
      (BERROR "UNEXPECTED TOKEN IN EXPRESSION: ~A." (CAR REST))
      NIL))))



(DEFUN BASIC-EVAL-CONDITION (EXPR)
  "
DO:         Parses the BASIC condition EXPR and evaluates it.
RETURN:     NIL or the computed value.
"
  (MULTIPLE-VALUE-BIND (TREE REST) (PARSE-DISJ EXPR)
    (COND
     ((EQ :ERROR TREE)
      (BERROR "SYNTAX ERROR IN CONDITION ~A." EXPR)
      NIL)
     ((NULL REST)
      (BASIC-EVAL-TREE TREE))
     (T
      (BERROR "UNEXPECTED TOKEN IN CONDITION: ~A." (CAR REST))
      NIL))))


(DEFUN IDENTIFIERP  (SYM)
  (AND (SYMBOLP SYM)
       (ALPHA-CHAR-P (CHAR (SYMBOL-NAME SYM) 0))))


(DEFUN IDENTIFIER-TYPE (SYM)
  (CHAR (SYMBOL-NAME SYM) (1- (LENGTH (SYMBOL-NAME SYM)))))


(DEFUN CHECK-LIST-VAR (LISTVAR)
  "
DO:         Check that listvar is a list of identifier symbols separated
            by comas.
RETURN:     The list of identifier symbols without the comas.
"
  (DO ((LISTVAR LISTVAR (CDDR LISTVAR))
       (RESULT  '()))
      ((NULL LISTVAR) (NREVERSE RESULT))
    (COND
     ((NULL LISTVAR)
      (BERROR "EXPECTED A LIST OF VARIABLES SEPARATED BY COMAS.")
      (SETQ RESULT NIL LISTVAR NIL))
     ((NULL (CDR LISTVAR))
      (IF (IDENTIFIERP (CAR LISTVAR))
        (PUSH (CAR LISTVAR) RESULT)
        (PROGN
          (BERROR "EXPECTED A VARIABLE INSTEAD OF ~A." (CAR LISTVAR))
          (SETQ RESULT NIL LISTVAR NIL))))
     ((NULL (CDDR LISTVAR))
      (BERROR "MALFORMED LIST OF VARIABLES.")
      (SETQ RESULT NIL LISTVAR NIL))
     (T
      (IF (AND (IDENTIFIERP (CAR LISTVAR)) (EQ '|,| (CADR LISTVAR)))
        (PUSH (CAR LISTVAR) RESULT)
        (PROGN
          (IF (EQ '|,| (CADR LISTVAR))
            (BERROR "EXPECTED A VARIABLE INSTEAD OF ~A." (CAR LISTVAR))
            (BERROR "EXPECTED A COMA INSTEAD OF ~A." (CADR LISTVAR)))
          (SETQ RESULT NIL LISTVAR NIL)))))))


(DEFUN BASIC-EVAL (STATEMENT)
  "
DO:         Evaluate the statement,
            and the following if *current-line* is non nil.
RETURN:     NIL or :BYE.
"
  (LOOP
   ;; (format t "current-line=~S   token=~A:~A statement=~S~%"
   ;;         *current-line* (package-name (symbol-package (car statement)))
   ;;         (car statement) statement)
   ;; (format t "dir=~A:~A   EQUAL=~S~%" (package-name (symbol-package 'dir))
   ;;         'dir (equal 'dir (car statement)))
   (UNLESS STATEMENT (RETURN NIL))
   (CASE (CAR STATEMENT)
     ((PRINT)
      (MULTIPLE-VALUE-BIND (EXPR REST)
          (SLURP-EXPRESSION (CDR STATEMENT) '(|,| |;| |:|))
        (IF EXPR
          (LET ((VALUE (BASIC-EVAL-EXPRESSION EXPR)))
            (IF VALUE
              (PROGN
                (FORMAT T (CASE (CAR REST)
                            ((|,|) "~A ")
                            ((|;|) "~A")
                            (T "~A~%")) VALUE)
                (WHEN REST
                  (CASE (CAR REST)
                    ((|,| |;|) (BASIC-EVAL (CONS 'PRINT (CDR REST))))
                    ((NIL))
                    ((|:|)     (BASIC-EVAL (CDR REST)))
                    (OTHERWISE (BERROR "UNEXPECTED TOKEN '~A'.") ))))
              (SETQ *CURRENT-LINE* NIL))))))
     ((FOR)
      ;; FOR A = EXPR TO EXPR [ STEP EXPR ] :
      (LET* ((VARSYM (SECOND STATEMENT))
             (VARIABLE (IF (SYMBOLP VARSYM) (SYMBOL-NAME VARSYM) NIL))
             (VARTYPE (IF VARIABLE (CHAR VARIABLE (1- (LENGTH VARIABLE)))))
             (TARGET)
             (STEP)
             (REMAINDER)
             (LINENUM *CURRENT-LINE*))
        (IF (AND VARIABLE
                 (ALPHA-CHAR-P (CHAR VARIABLE 0))
                 (CHAR/= (CHARACTER "$") VARTYPE)
                 (EQ '= (THIRD STATEMENT)))
          ;; for a =
          (MULTIPLE-VALUE-BIND (ASSIGNMENT REST)
              (SLURP-EXPRESSION (CDR STATEMENT) '(TO))
            (IF (EQ 'TO (CAR REST))
              (MULTIPLE-VALUE-BIND (TARGET-EXPR RREST)
                  (SLURP-EXPRESSION (CDR REST) '(STEP |:|))
                (SETQ TARGET (BASIC-EVAL-EXPRESSION TARGET-EXPR))
                (IF TARGET
                  (IF (NUMBERP TARGET)
                    (IF (EQ (CAR RREST) 'STEP)
                      (MULTIPLE-VALUE-BIND (STEP-EXPR RRREST)
                          (SLURP-EXPRESSION (CDR RREST) '(|:|))
                        (SETQ STEP (BASIC-EVAL-EXPRESSION STEP-EXPR))
                        (IF (NUMBERP STEP)
                          (SETQ REMAINDER  RRREST)
                          (PROGN
                            (BERROR "INVALID STEP VALUE: MUST BE NUMERIC!")
                            (SETQ STEP NIL))))
                      (SETQ STEP 1
                            REMAINDER  RREST))
                    (PROGN
                      (BERROR "INVALID TARGET VALUE: MUST BE NUMERIC!")
                      (SETQ TARGET NIL)))))
              (BERROR "INVALID TOKEN AFTER ASSIGNMENT IN FOR: '~A'."
                      (CAR REST)))
            (WHEN STEP
              (VECTOR-PUSH-EXTEND
               (LIST :FOR VARSYM TARGET STEP LINENUM (CDR REMAINDER))
               *STACK* (ARRAY-DIMENSION *STACK* 0))
              (BASIC-EVAL (NCONC ASSIGNMENT REMAINDER))))
          (BERROR "FOR EXPECTS A NUMERIC VARIABLE ASSIGNMENT."))))
     ((NEXT)
      (IF (AND (< 2 (LENGTH STATEMENT)) (NOT (EQ '|:| (THIRD STATEMENT))))
        (BERROR "INVALID TOKEN AFTER NEXT: '~A'." (THIRD STATEMENT))
        (LET* ((VARSYM    (IF (EQ '|:| (SECOND STATEMENT))
                            NIL (SECOND STATEMENT)))
               (FOR-STATE (FIND-FOR VARSYM)))
          (IF FOR-STATE
            (LET ((VARSYM    (SECOND FOR-STATE))
                  (TARGET    (THIRD FOR-STATE))
                  (STEP      (FOURTH FOR-STATE))
                  (LINENUM   (FIFTH FOR-STATE))
                  (REMAINDER (SIXTH FOR-STATE))
                  (VALUE     (GETHASH VARSYM *VARIABLES*)))
              (SETQ VALUE (+ VALUE STEP))
              (SETF (GETHASH VARSYM *VARIABLES*) VALUE)
              (IF (IF (< 0 STEP) (<= VALUE TARGET) (<= TARGET VALUE))
                (PROGN ;; loop
                  (SETQ *CURRENT-LINE* LINENUM)
                  (BASIC-EVAL (OR REMAINDER '(REM))))
                (PROGN ;; exit loop
                  (VECTOR-POP *STACK*)
                  (BASIC-EVAL (IF VARSYM
                                (CDDDR STATEMENT)
                                (CDDR  STATEMENT))))))
            (IF (NULL VARSYM)
              (BERROR "NO 'FOR' LOOP.")
              (BERROR "NO 'FOR' LOOP WITH THIS VARIABLE ~A." VARSYM))))))
     ((IF) ;; if bool then .... else ...
      (MULTIPLE-VALUE-BIND (EXPR REST)
          (SLURP-EXPRESSION (CDR STATEMENT) '(THEN))
        (LET ((CONDITION (BASIC-EVAL-CONDITION EXPR)))
          (COND
           ((NULL CONDITION)) ;; error already issued
           ((BOOLP CONDITION)
            (IF (EQ (CAR REST) 'THEN)
              (IF (EQ :TRUE CONDITION)
                ;; run after then
                (BASIC-EVAL (CDR REST))
                ;; run after else
                (BASIC-EVAL (CDR (MEMBER 'ELSE REST))))
              (BERROR "EXPECTED 'THEN' AFTER 'IF' CONDITION, NOT '~A'."
                      (CAR REST))))
           (T
            (BERROR "INVALID BOOL EXPRESSION."))))))
     ((ELSE)) ;; ignored and skip the rest of the line.
     ((GOTO)
      (MULTIPLE-VALUE-BIND (EXPR REST)
          (SLURP-EXPRESSION (CDR STATEMENT) '(|:|))
        (LET ((VALUE (BASIC-EVAL-EXPRESSION EXPR)))
          (IF (AND VALUE (INTEGERP VALUE) (CHECK-LINE VALUE))
            (SETQ *CURRENT-LINE* (1- VALUE))
            (BERROR "INVALID TARGET LINE NUMBER IN GOTO.")))))
     ((GOSUB)
      (MULTIPLE-VALUE-BIND (EXPR REST)
          (SLURP-EXPRESSION (CDR STATEMENT) '(|:|))
        (LET ((VALUE (BASIC-EVAL-EXPRESSION EXPR)))
          (IF (AND VALUE (INTEGERP VALUE) (CHECK-LINE VALUE))
            (PROGN
              (VECTOR-PUSH-EXTEND
               (LIST :GOSUB *CURRENT-LINE* (CDR REST))
               *STACK* (ARRAY-DIMENSION *STACK* 0))
              (SETQ *CURRENT-LINE* (1- VALUE)))
            (BERROR "INVALID TARGET LINE NUMBER IN GOSUB.")))))
     ((RETURN)
      (LET* ((GOSUB-STATE (FIND-GOSUB)))
        (IF GOSUB-STATE
          (LET ((LINENUM   (SECOND GOSUB-STATE))
                (REMAINDER (THIRD  GOSUB-STATE)))
            (SETQ *CURRENT-LINE* LINENUM)
            (IF REMAINDER (BASIC-EVAL REMAINDER)))
          (BERROR "NO 'GOSUB' FOR 'RETURN'."))))
     ((INPUT)
      (LET ((STAT-LIST-VAR))
        (IF (STRINGP (SECOND STATEMENT))
          (LET ((SAVED *CURRENT-LINE*))
            (SETQ *CURRENT-LINE* NIL)
            (BASIC-EVAL (LIST 'PRINT (SECOND STATEMENT) '|;|))
            (SETQ *CURRENT-LINE* SAVED)
            (SETQ STAT-LIST-VAR (CDDR STATEMENT)))
          (PROGN
            (FORMAT T "> ")
            (SETQ STAT-LIST-VAR (CDR STATEMENT))))
        (MULTIPLE-VALUE-BIND (LISTVAR REST)
            (SLURP-EXPRESSION STAT-LIST-VAR '(|:|))
          (LET ((LISTSYM (CHECK-LIST-VAR LISTVAR)))
            (WHEN LISTSYM
              (DO* ((LISTSYM LISTSYM (CDR LISTSYM))
                    (VARSYM (CAR LISTSYM) (CAR LISTSYM))
                    (VARTYPE (IDENTIFIER-TYPE VARSYM) (IDENTIFIER-TYPE VARSYM))
                    (VALUE))
                  ((NULL LISTSYM))
                (SETQ VALUE (INPUT-DATA (IF (CHAR= (CHARACTER "$") VARTYPE)
                                          'STRING 'NUMBER)))
                (COND
                 ((NULL VALUE))
                 ;; the error is already issued and *current-line* nullified
                 ((AND (NUMBERP VALUE) (CHAR/= (CHARACTER "$") VARTYPE))
                  (SETF (GETHASH VARSYM *VARIABLES*) VALUE))
                 ((AND (STRINGP VALUE) (CHAR= (CHARACTER "$") VARTYPE))
                  (SETF (GETHASH VARSYM *VARIABLES*) VALUE))
                 (T (BERROR "TYPE MISMATCH FOR ~A." VARSYM)))))))))
     ((DATA)) ;; skip the rest of the line which is data.
     ((READ)
      (MULTIPLE-VALUE-BIND (LISTVAR REST)
          (SLURP-EXPRESSION (CDR STATEMENT) '(|:|))
        (LET ((LISTSYM (CHECK-LIST-VAR LISTVAR)))
          (WHEN LISTSYM
            (DO* ((LISTSYM LISTSYM (CDR LISTSYM))
                  (VARSYM (CAR LISTSYM) (CAR LISTSYM))
                  (VARTYPE (IDENTIFIER-TYPE VARSYM) (IDENTIFIER-TYPE VARSYM))
                  (VALUE))
                ((NULL LISTSYM))
              (SETQ VALUE (FETCH-DATA))
              (COND
               ((NULL VALUE))
               ;; the error is already issued and *current-line* nullified
               ((AND (NUMBERP VALUE) (CHAR/= (CHARACTER "$") VARTYPE))
                (SETF (GETHASH VARSYM *VARIABLES*) VALUE))
               ((AND (STRINGP VALUE) (CHAR= (CHARACTER "$") VARTYPE))
                (SETF (GETHASH VARSYM *VARIABLES*) VALUE))
               (T (BERROR "TYPE MISMATCH FOR ~A." VARSYM))))))))
     ((RESTORE)
      (LET* ((REST NIL)
             (LINENUM
              (MULTIPLE-VALUE-BIND (EXPR TSER)
                  (SLURP-EXPRESSION (CDR STATEMENT) '(|:|))
                (PROG1
                    (IF (NULL EXPR)
                      (IF (OR (NULL (CDR STATEMENT))
                              (EQ '|:| (CADR STATEMENT)))
                        1
                        (PROGN (BERROR "UNEXPECTED TOKEN AFTER RESTORE: ~A"
                                       (CADR STATEMENT))
                               NIL))
                      (BASIC-EVAL-EXPRESSION EXPR))
                  (SETQ REST (CDR TSER))))))
        (WHEN LINENUM
          (IF (CHECK-LINE LINENUM)
            (PROGN
              (SETQ *DATA-PTR* (CONS (1- LINENUM) NIL))
              (BASIC-EVAL (OR REST '(REM))))
            (BERROR "INVALID LINE NUMBER FOR READ: ~A" LINENUM)))))
     ((REM)) ;; ignored
     ((STOP)
      (SETQ *CURRENT-LINE* NIL))
     ((RUN)
      (SETF (FILL-POINTER *STACK*) 0)
      (SETQ *DATA-PTR* (CONS 0 NIL))
      (SETQ *VARIABLES* (MAKE-HASH-TABLE :TEST (FUNCTION EQ) :SIZE 100))
      (IF (AND (CDR STATEMENT) (INTEGERP (SECOND STATEMENT)))
        (WHEN (CHECK-LINE (SECOND STATEMENT))
          (BASIC-EVAL (OR (FIND-LINE-OR-NEXT (SECOND STATEMENT))
                          (FIND-LINE-OR-NEXT 1))))
        (BASIC-EVAL (FIND-LINE-OR-NEXT 1)))
      (SETQ *CURRENT-LINE* NIL))
     ((LIST)
      (DOTIMES (LINENUM (ARRAY-DIMENSION *PROGRAM* 0))
        (LET ((LINE (AREF *PROGRAM* LINENUM)))
          (WHEN LINE
            (FORMAT T "~4D " LINENUM)
            (MAPC (LAMBDA (TOKEN)
                    (IF (SYMBOLP TOKEN)
                      (FORMAT T "~A " (SYMBOL-NAME TOKEN))
                      (FORMAT T "~S " TOKEN))) LINE)
            (FORMAT T "~%")))))
     ((DIR)
      (format t "~{~A~%~}" (mapcar (function pathname-name)
                                   (directory "*.basic"))))
     ((SAVE)
      (IF (STRINGP (CADR STATEMENT))
        (WITH-OPEN-FILE (*STANDARD-OUTPUT*
                         (CADR STATEMENT) :DIRECTION :OUTPUT
                         :IF-EXISTS :SUPERSEDE :IF-DOES-NOT-EXIST :CREATE)
          (LET ((SAVED *CURRENT-LINE*))
            (SETQ *CURRENT-LINE* NIL)
            (BASIC-EVAL '(LIST))
            (SETQ *CURRENT-LINE* SAVED)))
        (BERROR "NOT A FILE NAME: ~S." (CADR STATEMENT))))
     ((LOAD)
      (IF (STRINGP (SECOND STATEMENT))
        (PROGN
          (WITH-OPEN-FILE (IN (CADR STATEMENT) :DIRECTION :INPUT
                              :IF-DOES-NOT-EXIST NIL)
            (IF (NULL IN)
              (BERROR "CAN'T FIND A FILE FILE NAMED: ~S." (CADR STATEMENT))
              (PROGN
                (SETQ *CURRENT-LINE* NIL)
                (BASIC-EVAL '(ERASE ALL))
                (DO ((LINE (READ-LINE IN NIL NIL) (READ-LINE IN NIL NIL)))
                    ((NOT LINE))
                  (BASIC-PROCESS-LINE LINE)))))
          (SETQ *CURRENT-LINE*
                (IF (AND (NUMBERP (THIRD STATEMENT))
                         (CHECK-LINE (THIRD STATEMENT)))
                  (1- (THIRD STATEMENT)) NIL)))
        (BERROR "NOT A FILE NAME: ~S." (SECOND STATEMENT))))
     ((ERASE)
      (MAPC (LAMBDA (LINENUM)
              (COND
               ((INTEGERP LINENUM)
                (WHEN (CHECK-LINE LINENUM)
                  (SETF (AREF *PROGRAM* LINENUM) NIL)))
               ((EQ 'ALL LINENUM)
                (DOTIMES (I (ARRAY-DIMENSION *PROGRAM* 0))
                  (SETF (AREF *PROGRAM* I) NIL)))
               (T (BERROR "NOT A LINE NUMBER: ~S." LINENUM))))
            (CDR STATEMENT)))
     ((BYE) (SETQ *CURRENT-LINE* NIL) (RETURN :BYE))
     (OTHERWISE
      (LET* ((VARSYM   (CAR STATEMENT))
             (VARIABLE (IF (SYMBOLP VARSYM) (SYMBOL-NAME VARSYM)   NIL))
             (VARTYPE  (IF VARIABLE (CHAR VARIABLE (1- (LENGTH VARIABLE))))))
        (IF (AND VARIABLE
                 (ALPHA-CHAR-P (CHAR VARIABLE 0))
                 (EQ '= (SECOND STATEMENT)))
          ;; assignment
          (MULTIPLE-VALUE-BIND (EXPR REST)
              (SLURP-EXPRESSION (CDDR STATEMENT) '(|:|))
            (IF (OR (NULL REST) (EQ (CAR REST) '|:|))
              (PROGN
                (LET ((VALUE (BASIC-EVAL-EXPRESSION EXPR)))
                  (COND
                   ((NULL VALUE))
                   ;; the error is already issued and *current-line* nullified
                   ((AND (NUMBERP VALUE) (CHAR/= (CHARACTER "$") VARTYPE))
                    (SETF (GETHASH VARSYM *VARIABLES*) VALUE))
                   ((AND (STRINGP VALUE) (CHAR= (CHARACTER "$") VARTYPE))
                    (SETF (GETHASH VARSYM *VARIABLES*) VALUE))
                   (T (BERROR "TYPE MISMATCH FOR ~A." VARIABLE))))
                (WHEN REST (BASIC-EVAL (CDR REST))))
              (BERROR "INVALID TOKEN ~S IN EXPRESSION." (CAR REST))))
          (BERROR "INVALID TOKEN ~S IN STATEMENT." (CAR STATEMENT)))))
     ) ;;case
   (IF *CURRENT-LINE*
     (PROGN
       (INCF *CURRENT-LINE*)
       (SETQ STATEMENT (FIND-LINE-OR-NEXT *CURRENT-LINE*)))
     (RETURN NIL))))


(DEFUN BASIC-PROCESS-LINE (LINE)
  "
DO:         Process one BASIC line.
"
  (MULTIPLE-VALUE-BIND (TOKENS ERR) (SPLIT-LINE LINE)
    (SETQ TOKENS (let ((*package* (find-package "BASIC")))
                   (MAPCAR (LAMBDA (ITEM) (READ-FROM-STRING ITEM)) TOKENS)))
    (COND (ERR (BERROR "~A" ERR))
          ((AND (< 0 (LENGTH TOKENS)) (INTEGERP (CAR TOKENS)))
           (WHEN (CHECK-LINE (CAR TOKENS))
             (SETF (AREF *PROGRAM* (CAR TOKENS)) (CDR TOKENS))))
          (T (SETQ *CURRENT-LINE* NIL)
             (BASIC-EVAL TOKENS)))))


(DEFUN BASIC ()
  "
DO:         Read a line and either execute it or store it in the program.
            Repeat until the BYE command is executed.
"
  (SETF (FILL-POINTER *STACK*) 0)
  (SETQ *DATA-PTR* (CONS 0 NIL))
  (FORMAT T "*** QUICK-DIRTY-AND-UGLY BASIC, VERSION 0.1 ***~%~
             COPYRIGHT PASCAL BOURGUIGNON 2003~%~
             QUICK-DIRTY-AND-UGLY BASIC COMES WITH *ABSOLUTELY NO WARRANTY*.~%~
             THIS IS FREE SOFTWARE, AND YOU ARE WELCOME TO REDISTRIBUTE IT~%~
             UNDER THE CONDITIONS LISTED IN THE GNU PUBLIC LICENSE.~4%")
  (BLOCK :TOP-LEVEL
    (LOOP
     (FORMAT T "~&> ")
     (LET ((LINE (READ-LINE *STANDARD-INPUT* NIL NIL)))
       (UNLESS LINE (RETURN-FROM :TOP-LEVEL))
       (IF (EQ :BYE (BASIC-PROCESS-LINE LINE))
         (RETURN-FROM :TOP-LEVEL)))))
  (VALUES))


(DEFUN MAIN (&REST ARGUMENTS)
  (DECLARE (IGNORE ARGUMENTS))
  (BASIC))


;;;; basic.lisp                       -- 2004-03-14 01:34:04 -- pascal   ;;;;
