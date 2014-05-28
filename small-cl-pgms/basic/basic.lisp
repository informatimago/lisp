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
;;    <PJB> Pascal J. Bourguignon
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
;;    Copyright Pascal J. Bourguignon 2003 - 2003
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.BASIC"
  (:nicknames "BASIC")
  (:use "COMMON-LISP")
  (:export "BASIC" "MAIN")
  );;BASIC
(in-package "COM.INFORMATIMAGO.COMMON-LISP.BASIC")


(defvar *program* (make-array '(1000) :initial-element nil))
(defvar *stack*   (make-array '(100)
                              :initial-element nil
                              :adjustable t
                              :fill-pointer 0));;*STACK*
(defvar *variables* (make-hash-table :test (function eq) :size 100))

(defvar *current-line* 0)
(defvar *data-ptr* (cons 0 nil) "marker for DATA/READ/RESTORE")


(defmacro while (condition &body body) `(do () ((not ,condition)) ,@body))
(defmacro until (condition &body body) `(do () (,condition)       ,@body))


(defun split-line (line)
  "
DO:         Split the line between the special characters:
            space , ; : < > <= >= = <>  + - * / ^
            as one token.  The special characters are enclosed  in pipes.
RETURN:     A list of token string (where spaces are removed) ;
            nil or an error message string.
NOTE:       No parentheses: yuck maths!  No dots in numbers: yuck maths!
"
  (do ((i 0 (1+ i))
       (p 0)
       (parts ())
       (err nil))
      ((<= (length line) i)
       (values (progn (when (< p (length line))
                        (push (subseq line p (length line)) parts))
                      (nreverse parts)) err))
    (macrolet ((push-part () `(when (< p i)
                                (push (subseq line p i) parts)
                                (setq p i))))
      (cond
       ((char= (char line i) (character " "))
        (push-part)
        (incf p))
       ((char= (char line i) (character "\""))
        (push-part)
        (incf i)
        (do ()
            ((or (<= (length line) i) (char= (char line i) (character "\""))))
          (incf i))
        (if (< i (length line)) (incf i))
        (push-part)
        (decf i))
       ((position (char line i) ",;:=+-*/^")
        (push-part)
        (incf p)
        (push (format nil "|~A|" (subseq line i p)) parts))
       ((char= (char line i) (character "<"))
        (push-part)
        (if (or (char= (char line (1+ i)) (character "="))
                (char= (char line (1+ i)) (character ">")))
          (progn (push (format nil "|~A|" (subseq line i (+ i 2))) parts)
                 (setq p (incf i)))
          (progn (incf p)
                 (push (format nil "|~A|" (subseq line i p)) parts))))
       ((char= (char line i) (character ">"))
        (push-part)
        (if  (char= (char line (1+ i)) (character "="))
          (progn (push (format nil "|~A|" (subseq line i (+ i 2))) parts)
                 (setq p (incf i)))
          (progn (incf p)
                 (push (format nil "|~A|" (subseq line i p)) parts))))
       ((or (alphanumericp (char line i))
            (char= (character "$") (char line i))
            (char= (character "%") (char line i))))
       (t
        (setq err (format nil "INVALID CHARACTER: '~A' AT POSITION ~D."
                          (char line i) i))
        (setq i (length line))))))
  );;SPLIT-LINE




(defun fetch-data ()
  "
RETURN:     The data found at or following *DATA-PTR*, or NIL if none remains.
DO:         Increments *DATA-PTR*, or issue an error (setting *CURRENT-LINE*).
"
  (while (and (< (car *data-ptr*) (array-dimension *program* 0))
              (null (cdr *data-ptr*)))
    (incf (car *data-ptr*))
    (while (and (< (car *data-ptr*) (array-dimension *program* 0))
                (or (null (aref *program* (car *data-ptr*)))
                    (not (eq 'data (car (aref *program* (car *data-ptr*)))))))
      (incf (car *data-ptr*)))
    (if (and (< (car *data-ptr*) (array-dimension *program* 0))
             (aref *program* (car *data-ptr*)))
      (setf (cdr *data-ptr*) (cdr (aref *program* (car *data-ptr*))))))
  ;;(format t "data-ptr= ~S~%" *data-ptr*)
  (if (null (cdr *data-ptr*))
    (progn  (berror "NO MORE DATA TO READ.") nil)
    (if (and (cdr (cdr *data-ptr*))
             (or (null (cddr (cdr *data-ptr*)))
                 (not (eq '|,| (cadr (cdr *data-ptr*))))
                 (not (or (stringp (car (cdr *data-ptr*)))
                          (numberp (car (cdr *data-ptr*)))))))
      (progn (berror "MALFORMED DATA LINE ~A." (car *data-ptr*))    nil)
      (prog1 (pop (cdr *data-ptr*)) (pop (cdr *data-ptr*))))))



(defmacro protect-break (form)
  `(handler-case
       (values ,form)
     (t () (format t "~&BREAK~%") (setq *current-line* nil) nil)
     (:no-error (data) data)))


(defun input-data (type)
  (cond
   ((eq type 'string) (protect-break (read-line)))
   ((eq type 'number) (protect-break (read)))))




(defun find-for (variable)
  "
DO:         Finds the first entry in the stack that is a list beginning
            with :FOR and the VARIABLE, or just :FOR if VARIABLE is NIL.
            (compared with EQ).
NOTE:       If found, any entry above the found entry are poped.
RETURN:     NIL or the entry.
"
  (do ((pointer (1- (fill-pointer *stack*)) (decf pointer)))
      ((or (< pointer 0)
           (and (consp (aref *stack* pointer))
                (eq :for     (car    (aref *stack* pointer)))
                (or (null variable)
                    (eq variable (second (aref *stack* pointer))))))
       (if (< pointer 0)
         nil
         (progn
           (setf (fill-pointer *stack*) (1+ pointer))
           (aref *stack* pointer))))))


(defun find-gosub ()
  "
DO:         Finds the first entry in the stack that is a list beginning
            with :GOSUB.
NOTE:       If found, any entry above the found entry are poped.
RETURN:     NIL or the entry.
"
  (do ((pointer (1- (fill-pointer *stack*)) (decf pointer)))
      ((or (< pointer 0)
           (and (consp (aref *stack* pointer))
                (eq :gosub     (car    (aref *stack* pointer)))))
       (if (< pointer 0)
         nil
         (progn
           (setf (fill-pointer *stack*) (1+ pointer))
           (aref *stack* pointer))))))



(defun berror (fmt &rest args)
  "
DO:         Prints an error message formated from fmt and args.
"
  (if *current-line*
    (format t "~&ERROR LINE ~D: ~A~%"
            *current-line* (apply (function format) nil fmt args))
    (format t "~&ERROR: ~A~%"  (apply (function format) nil fmt args)))
  (setq *current-line* nil))


(defun check-line (linenum)
  "
DO:         Check the line number and issue an error message.
RETURN:     Whether the linenum is a valid line number.
"
  (declare (integer linenum))
  (if (or (< linenum 1)
          (<= (array-dimension *program* 0) linenum))
    (progn (berror "LINE NUMBER OUT OF RANGE (1..~D)."
                   (array-dimension *program* 0))
           nil)
    t))


(defun find-line-or-next (linenum)
  "
PRE:       (check-line linenum)
RETURN:    If line linenum exists then line linenum
           else the line with the minimum line number greater than linenum
           or else nil.
"
  (if (or (<= linenum 0) (<= (array-dimension *program* 0) linenum))
    (progn (setq *current-line* nil)
           nil)
    (do* ((linenum linenum (1+ linenum))
          (line (aref *program* linenum) (aref *program* linenum)) )
        ((or line (= (array-dimension *program* 0) (1+ linenum)))
         (if line
           (progn (setq *current-line* linenum)
                  line)
           (progn (setq *current-line* nil)
                  nil))))))




(defun slurp-expression (tokens terminals)
  "
DO:         Parse tokens until a terminal or end of list's found.
RETURN:     A list of tokens making an expression ;
            A cdr of tokens.
"
  (do ((expr ())
       (tokens tokens (cdr tokens)))
      ((or (null tokens)
           (member (car tokens) terminals :test (function eq)))
       (values (nreverse expr) tokens))
    (push (car tokens) expr)))


;;; expr : term { [+|-] expr }
;;; term : fact { [*|/] term }
;;; fact : simple { ^ fact }
;;; simple : ident | number | ( expr ) .

(defun parse-simp (simp)
  "
DO:         Parses a simple expression:
            simp ::= number | string | identifier | ( expr ) .
NOTE:       We're missing a function call: identifier ( expr { , expr } )
RETURN:     A parse tree or :ERROR ; a cdr of simp.
"
  (cond
   ((member (car simp) '(+ -))
    (multiple-value-bind (expr rest) (parse-simp (cdr simp))
      (if (eq :error expr)
        (values expr rest)
        (if (eq (car simp) '+)
          (values expr rest)
          (values (list 'neg expr) rest)))))
   ((numberp (car simp)) (values (car simp) (cdr simp)))
   ((stringp (car simp)) (values (car simp) (cdr simp)))
   ((symbolp (car simp)) (values (car simp) (cdr simp)))
   ((eq '|(| (car simp))
    (multiple-value-bind (expr rest) (parse-expr (cdr simp))
      (if (eq '|)| (car rest))
        (values expr (cdr rest))
        (progn
          (berror "MISSING A CLOSING PARENTHESE.")
          (values :error nil)))))
   (t (berror "INVALID TOKEN IN EXPRESSION ~S." (car simp)))))



(defmacro make-parse-level (name operators next)
  "
DO:         Generate a function named PARSE-{name} that parses the
            following rule:  name ::= name { operators next } .
            That functions will return a parse tree or :ERROR ; a cdr of expr.
"
  (let ((parse-level-name (intern (format nil "PARSE-~A" name)))
        (parse-next-name  (intern (format nil "PARSE-~A" next))))
    `(defun ,parse-level-name (expr)
       (let ((result))
         (multiple-value-bind (term rest) (,parse-next-name expr)
           (setq result term expr rest))
         (do () ((or (eq :error result)
                     (null expr)
                     (not (member (car expr) ',operators
                                  :test (function eq)))))
           (multiple-value-bind (term rest) (,parse-next-name (cdr expr))
             (if (eq :error term)
               (setq result :error)
               (setq result (list (car expr) result term)
                     expr   rest))))
         (values result expr)))))

(defun parse-lnot (lnot)
  "
DO:         Parses a simple logical expression:
            lnot ::= comp | NOT lnot | ( disj ).
RETURN:     A parse tree or :ERROR ; a cdr of expr.
"
  (cond
   ((eq (car lnot) 'not)
    (multiple-value-bind (expr rest) (parse-lnot (cdr lnot))
      (if (eq :error expr)
        (values expr rest)
        (values (list 'not expr) rest))))
   ((eq '|(| (car lnot))
    (multiple-value-bind (expr rest) (parse-disj (cdr lnot))
      (if (eq '|)| (car rest))
        (values expr (cdr rest))
        (progn
          (berror "MISSING A CLOSING PARENTHESE.")
          (values :error nil)))))
   (t (parse-comp lnot))))


(make-parse-level fact (^)       simp)
(make-parse-level term (* / mod) fact)
(make-parse-level expr (+ -)     term)
(make-parse-level comp (< <= > >= = <>) expr)
(make-parse-level conj (and) lnot)
(make-parse-level disj (or)  conj)



(defun bdiv (a b)
  "
RETURN: A floating-point division of a by b.
"
  (if (equal 0 b)
    (progn
      (berror "DIVISION BY ZERO.")
      nil)
    (/ (float a) b)))




(defun boolp (operand)  (member operand '(:true :false)))
(defun band (a b) (and (eq :true a) (eq :true b)))
(defun bor  (a b) (or  (eq :true a) (eq :true b)))
(defun bnot (a)   (eq :false a))
(defun bool (lisp-bool) (if lisp-bool :true :false))

(defmacro make-comparison (name operator number-op string-op)
  `(defun ,name (a b)
     (cond
      ((and (numberp a) (numberp b)) (bool (,number-op a b)))
      ((and (stringp a) (stringp b)) (bool (,string-op a b)))
      (t (berror "INCOMPATIBLE OPERANDS FOR ~A." ',operator)))))

(make-comparison blt <  <  string< )
(make-comparison ble <= <= string<=)
(make-comparison bgt >  >  string> )
(make-comparison bge >= >= string>=)
(make-comparison beq =  =  string= )
(make-comparison bne <> /= string/=)


(defmacro num-op (operator operation)
  "PRIVATE MACRO for BASIC-EVAL-TREE"
  `(let ((left  (basic-eval-tree (second tree)))
         (right (basic-eval-tree (third  tree))))
     (cond
      ((and (numberp left) (numberp right)) (,operation left right))
      (t (berror "INCOMPATIBLE OPERANDS FOR ~A." ',operator)    nil))))

(defmacro comp-op (operator operation)
  "PRIVATE MACRO for BASIC-EVAL-TREE"
  `(let ((left  (basic-eval-tree (second tree)))
         (right (basic-eval-tree (third  tree))))
     (cond
      ((and (numberp left) (numberp right)) (,operation left right))
      ((and (stringp left) (stringp right)) (,operation left right))
      (t (berror "INCOMPATIBLE OPERANDS FOR ~A." ',operation)    nil))))

(defmacro bool-op (operator operation)
  "PRIVATE MACRO for BASIC-EVAL-TREE"
  `(let ((left  (basic-eval-tree (second tree)))
         (right (basic-eval-tree (third  tree))))
     (cond
      ((and (boolp left) (boolp right)) (,operation left right))
      (t (berror "INCOMPATIBLE OPERANDS FOR ~A." ',operation)     nil))))



(defun basic-eval-tree (tree)
  "
DO:         Evaluate an expression tree.
RETURN:     NIL or the computed value.
"
  (cond
   ((numberp tree) tree)
   ((stringp tree) tree)
   ((symbolp tree)
    (let ((value (gethash tree *variables*)))
      (unless value
        (setq value
              (setf (gethash tree *variables*)
                    (if (char= (character "$")
                               (char (symbol-name tree)
                                     (1- (length (symbol-name tree)))))
                      "" 0))))
      value))
   ((consp tree)
    (case (car tree)
      (-   (num-op  -   -))
      (*   (num-op  *   *))
      (/   (num-op  /   bdiv))
      (^   (num-op  ^   expt))
      (mod (num-op  mod mod))
      (and (bool-op and band))
      (or  (bool-op or  bor))
      (<   (comp-op <   blt))
      (<=  (comp-op <=  ble))
      (>   (comp-op >   bgt))
      (>=  (comp-op >=  bge))
      (=   (comp-op =   beq))
      (<>  (comp-op <>  bne))
      (+ (let ((left  (basic-eval-tree (second tree)))
               (right (basic-eval-tree (third  tree))))
           (cond
            ((and (stringp left) (stringp right))
             (concatenate 'string left right))
            ((and (numberp left) (numberp right))      (+ left right))
            (t (berror "INCOMPATIBLE OPERANDS FOR +.") nil))))
      (not (let ((left  (basic-eval-tree (second tree))))
             (cond
              ((boolp left)                                   (bnot left))
              (t (berror "INCOMPATIBLE OPERANDS FOR UNARY NOT.") nil))))
      (neg (let ((left  (basic-eval-tree (second tree))))
             (cond
              ((numberp left)                                    (- left))
              (t (berror "INCOMPATIBLE OPERANDS FOR UNARY -.")   nil))))
      (otherwise (berror "UNEXPECTED OPERATOR ~A." (car tree))   nil)))
   (t (berror "UNEXPECTED OPERAND ~A." tree)                     nil)))



(defun basic-eval-expression (expr)
  "
DO:         Parses the BASIC expression EXPR and evaluates it.
RETURN:     NIL or the computed value.
"
  (multiple-value-bind (tree rest) (parse-expr expr)
    (cond
     ((eq :error tree)
      (berror "SYNTAX ERROR IN EXPRESSION ~A." expr)
      nil)
     ((null rest)
      (basic-eval-tree tree))
     (t
      (berror "UNEXPECTED TOKEN IN EXPRESSION: ~A." (car rest))
      nil))))



(defun basic-eval-condition (expr)
  "
DO:         Parses the BASIC condition EXPR and evaluates it.
RETURN:     NIL or the computed value.
"
  (multiple-value-bind (tree rest) (parse-disj expr)
    (cond
     ((eq :error tree)
      (berror "SYNTAX ERROR IN CONDITION ~A." expr)
      nil)
     ((null rest)
      (basic-eval-tree tree))
     (t
      (berror "UNEXPECTED TOKEN IN CONDITION: ~A." (car rest))
      nil))))


(defun identifierp  (sym)
  (and (symbolp sym)
       (alpha-char-p (char (symbol-name sym) 0))))


(defun identifier-type (sym)
  (char (symbol-name sym) (1- (length (symbol-name sym)))))


(defun check-list-var (listvar)
  "
DO:         Check that listvar is a list of identifier symbols separated
            by comas.
RETURN:     The list of identifier symbols without the comas.
"
  (do ((listvar listvar (cddr listvar))
       (result  '()))
      ((null listvar) (nreverse result))
    (cond
     ((null listvar)
      (berror "EXPECTED A LIST OF VARIABLES SEPARATED BY COMAS.")
      (setq result nil listvar nil))
     ((null (cdr listvar))
      (if (identifierp (car listvar))
        (push (car listvar) result)
        (progn
          (berror "EXPECTED A VARIABLE INSTEAD OF ~A." (car listvar))
          (setq result nil listvar nil))))
     ((null (cddr listvar))
      (berror "MALFORMED LIST OF VARIABLES.")
      (setq result nil listvar nil))
     (t
      (if (and (identifierp (car listvar)) (eq '|,| (cadr listvar)))
        (push (car listvar) result)
        (progn
          (if (eq '|,| (cadr listvar))
            (berror "EXPECTED A VARIABLE INSTEAD OF ~A." (car listvar))
            (berror "EXPECTED A COMA INSTEAD OF ~A." (cadr listvar)))
          (setq result nil listvar nil)))))))


(defun basic-eval (statement)
  "
DO:         Evaluate the statement,
            and the following if *current-line* is non nil.
RETURN:     NIL or :BYE.
"
  (loop
   ;; (format t "current-line=~S   token=~A:~A statement=~S~%"
   ;;         *current-line* (package-name (symbol-package (car statement)))
   ;;         (car statement) statement)
   ;; (format t "dir=~A:~A   EQUAL=~S~%" (package-name (symbol-package 'dir))
   ;;         'dir (equal 'dir (car statement)))
   (unless statement (return nil))
   (case (car statement)
     ((print)
      (multiple-value-bind (expr rest)
          (slurp-expression (cdr statement) '(|,| |;| |:|))
        (if expr
          (let ((value (basic-eval-expression expr)))
            (if value
              (progn
                (format t (case (car rest)
                            ((|,|) "~A ")
                            ((|;|) "~A")
                            (t "~A~%")) value)
                (when rest
                  (case (car rest)
                    ((|,| |;|) (basic-eval (cons 'print (cdr rest))))
                    ((nil))
                    ((|:|)     (basic-eval (cdr rest)))
                    (otherwise (berror "UNEXPECTED TOKEN '~A'.") ))))
              (setq *current-line* nil))))))
     ((for)
      ;; FOR A = EXPR TO EXPR [ STEP EXPR ] :
      (let* ((varsym (second statement))
             (variable (if (symbolp varsym) (symbol-name varsym) nil))
             (vartype (if variable (char variable (1- (length variable)))))
             (target)
             (step)
             (remainder)
             (linenum *current-line*))
        (if (and variable
                 (alpha-char-p (char variable 0))
                 (char/= (character "$") vartype)
                 (eq '= (third statement)))
          ;; for a =
          (multiple-value-bind (assignment rest)
              (slurp-expression (cdr statement) '(to))
            (if (eq 'to (car rest))
              (multiple-value-bind (target-expr rrest)
                  (slurp-expression (cdr rest) '(step |:|))
                (setq target (basic-eval-expression target-expr))
                (if target
                  (if (numberp target)
                    (if (eq (car rrest) 'step)
                      (multiple-value-bind (step-expr rrrest)
                          (slurp-expression (cdr rrest) '(|:|))
                        (setq step (basic-eval-expression step-expr))
                        (if (numberp step)
                          (setq remainder  rrrest)
                          (progn
                            (berror "INVALID STEP VALUE: MUST BE NUMERIC!")
                            (setq step nil))))
                      (setq step 1
                            remainder  rrest))
                    (progn
                      (berror "INVALID TARGET VALUE: MUST BE NUMERIC!")
                      (setq target nil)))))
              (berror "INVALID TOKEN AFTER ASSIGNMENT IN FOR: '~A'."
                      (car rest)))
            (when step
              (vector-push-extend
               (list :for varsym target step linenum (cdr remainder))
               *stack* (array-dimension *stack* 0))
              (basic-eval (nconc assignment remainder))))
          (berror "FOR EXPECTS A NUMERIC VARIABLE ASSIGNMENT."))))
     ((next)
      (if (and (< 2 (length statement)) (not (eq '|:| (third statement))))
        (berror "INVALID TOKEN AFTER NEXT: '~A'." (third statement))
        (let* ((varsym    (if (eq '|:| (second statement))
                            nil (second statement)))
               (for-state (find-for varsym)))
          (if for-state
            (let ((varsym    (second for-state))
                  (target    (third for-state))
                  (step      (fourth for-state))
                  (linenum   (fifth for-state))
                  (remainder (sixth for-state))
                  (value     (gethash varsym *variables*)))
              (setq value (+ value step))
              (setf (gethash varsym *variables*) value)
              (if (if (< 0 step) (<= value target) (<= target value))
                (progn ;; loop
                  (setq *current-line* linenum)
                  (basic-eval (or remainder '(rem))))
                (progn ;; exit loop
                  (vector-pop *stack*)
                  (basic-eval (if varsym
                                (cdddr statement)
                                (cddr  statement))))))
            (if (null varsym)
              (berror "NO 'FOR' LOOP.")
              (berror "NO 'FOR' LOOP WITH THIS VARIABLE ~A." varsym))))))
     ((if) ;; if bool then .... else ...
      (multiple-value-bind (expr rest)
          (slurp-expression (cdr statement) '(then))
        (let ((condition (basic-eval-condition expr)))
          (cond
           ((null condition)) ;; error already issued
           ((boolp condition)
            (if (eq (car rest) 'then)
              (if (eq :true condition)
                ;; run after then
                (basic-eval (cdr rest))
                ;; run after else
                (basic-eval (cdr (member 'else rest))))
              (berror "EXPECTED 'THEN' AFTER 'IF' CONDITION, NOT '~A'."
                      (car rest))))
           (t
            (berror "INVALID BOOL EXPRESSION."))))))
     ((else)) ;; ignored and skip the rest of the line.
     ((goto)
      (multiple-value-bind (expr rest)
          (slurp-expression (cdr statement) '(|:|))
        (let ((value (basic-eval-expression expr)))
          (if (and value (integerp value) (check-line value))
            (setq *current-line* (1- value))
            (berror "INVALID TARGET LINE NUMBER IN GOTO.")))))
     ((gosub)
      (multiple-value-bind (expr rest)
          (slurp-expression (cdr statement) '(|:|))
        (let ((value (basic-eval-expression expr)))
          (if (and value (integerp value) (check-line value))
            (progn
              (vector-push-extend
               (list :gosub *current-line* (cdr rest))
               *stack* (array-dimension *stack* 0))
              (setq *current-line* (1- value)))
            (berror "INVALID TARGET LINE NUMBER IN GOSUB.")))))
     ((return)
      (let* ((gosub-state (find-gosub)))
        (if gosub-state
          (let ((linenum   (second gosub-state))
                (remainder (third  gosub-state)))
            (setq *current-line* linenum)
            (if remainder (basic-eval remainder)))
          (berror "NO 'GOSUB' FOR 'RETURN'."))))
     ((input)
      (let ((stat-list-var))
        (if (stringp (second statement))
          (let ((saved *current-line*))
            (setq *current-line* nil)
            (basic-eval (list 'print (second statement) '|;|))
            (setq *current-line* saved)
            (setq stat-list-var (cddr statement)))
          (progn
            (format t "> ")
            (setq stat-list-var (cdr statement))))
        (multiple-value-bind (listvar rest)
            (slurp-expression stat-list-var '(|:|))
          (let ((listsym (check-list-var listvar)))
            (when listsym
              (do* ((listsym listsym (cdr listsym))
                    (varsym (car listsym) (car listsym))
                    (vartype (identifier-type varsym) (identifier-type varsym))
                    (value))
                  ((null listsym))
                (setq value (input-data (if (char= (character "$") vartype)
                                          'string 'number)))
                (cond
                 ((null value))
                 ;; the error is already issued and *current-line* nullified
                 ((and (numberp value) (char/= (character "$") vartype))
                  (setf (gethash varsym *variables*) value))
                 ((and (stringp value) (char= (character "$") vartype))
                  (setf (gethash varsym *variables*) value))
                 (t (berror "TYPE MISMATCH FOR ~A." varsym)))))))))
     ((data)) ;; skip the rest of the line which is data.
     ((read)
      (multiple-value-bind (listvar rest)
          (slurp-expression (cdr statement) '(|:|))
        (let ((listsym (check-list-var listvar)))
          (when listsym
            (do* ((listsym listsym (cdr listsym))
                  (varsym (car listsym) (car listsym))
                  (vartype (identifier-type varsym) (identifier-type varsym))
                  (value))
                ((null listsym))
              (setq value (fetch-data))
              (cond
               ((null value))
               ;; the error is already issued and *current-line* nullified
               ((and (numberp value) (char/= (character "$") vartype))
                (setf (gethash varsym *variables*) value))
               ((and (stringp value) (char= (character "$") vartype))
                (setf (gethash varsym *variables*) value))
               (t (berror "TYPE MISMATCH FOR ~A." varsym))))))))
     ((restore)
      (let* ((rest nil)
             (linenum
              (multiple-value-bind (expr tser)
                  (slurp-expression (cdr statement) '(|:|))
                (prog1
                    (if (null expr)
                      (if (or (null (cdr statement))
                              (eq '|:| (cadr statement)))
                        1
                        (progn (berror "UNEXPECTED TOKEN AFTER RESTORE: ~A"
                                       (cadr statement))
                               nil))
                      (basic-eval-expression expr))
                  (setq rest (cdr tser))))))
        (when linenum
          (if (check-line linenum)
            (progn
              (setq *data-ptr* (cons (1- linenum) nil))
              (basic-eval (or rest '(rem))))
            (berror "INVALID LINE NUMBER FOR READ: ~A" linenum)))))
     ((rem)) ;; ignored
     ((stop)
      (setq *current-line* nil))
     ((run)
      (setf (fill-pointer *stack*) 0)
      (setq *data-ptr* (cons 0 nil))
      (setq *variables* (make-hash-table :test (function eq) :size 100))
      (if (and (cdr statement) (integerp (second statement)))
        (when (check-line (second statement))
          (basic-eval (or (find-line-or-next (second statement))
                          (find-line-or-next 1))))
        (basic-eval (find-line-or-next 1)))
      (setq *current-line* nil))
     ((list)
      (dotimes (linenum (array-dimension *program* 0))
        (let ((line (aref *program* linenum)))
          (when line
            (format t "~4D " linenum)
            (mapc (lambda (token)
                    (if (symbolp token)
                      (format t "~A " (symbol-name token))
                      (format t "~S " token))) line)
            (format t "~%")))))
     ((dir)
      (format t "~{~A~%~}" (mapcar (function pathname-name)
                                   (directory "*.basic"))))
     ((save)
      (if (stringp (cadr statement))
        (with-open-file (*standard-output*
                         (cadr statement) :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
          (let ((saved *current-line*))
            (setq *current-line* nil)
            (basic-eval '(list))
            (setq *current-line* saved)))
        (berror "NOT A FILE NAME: ~S." (cadr statement))))
     ((load)
      (if (stringp (second statement))
        (progn
          (with-open-file (in (cadr statement) :direction :input
                              :if-does-not-exist nil)
            (if (null in)
              (berror "CAN'T FIND A FILE FILE NAMED: ~S." (cadr statement))
              (progn
                (setq *current-line* nil)
                (basic-eval '(erase all))
                (do ((line (read-line in nil nil) (read-line in nil nil)))
                    ((not line))
                  (basic-process-line line)))))
          (setq *current-line*
                (if (and (numberp (third statement))
                         (check-line (third statement)))
                  (1- (third statement)) nil)))
        (berror "NOT A FILE NAME: ~S." (second statement))))
     ((erase)
      (mapc (lambda (linenum)
              (cond
               ((integerp linenum)
                (when (check-line linenum)
                  (setf (aref *program* linenum) nil)))
               ((eq 'all linenum)
                (dotimes (i (array-dimension *program* 0))
                  (setf (aref *program* i) nil)))
               (t (berror "NOT A LINE NUMBER: ~S." linenum))))
            (cdr statement)))
     ((bye) (setq *current-line* nil) (return :bye))
     (otherwise
      (let* ((varsym   (car statement))
             (variable (if (symbolp varsym) (symbol-name varsym)   nil))
             (vartype  (if variable (char variable (1- (length variable))))))
        (if (and variable
                 (alpha-char-p (char variable 0))
                 (eq '= (second statement)))
          ;; assignment
          (multiple-value-bind (expr rest)
              (slurp-expression (cddr statement) '(|:|))
            (if (or (null rest) (eq (car rest) '|:|))
              (progn
                (let ((value (basic-eval-expression expr)))
                  (cond
                   ((null value))
                   ;; the error is already issued and *current-line* nullified
                   ((and (numberp value) (char/= (character "$") vartype))
                    (setf (gethash varsym *variables*) value))
                   ((and (stringp value) (char= (character "$") vartype))
                    (setf (gethash varsym *variables*) value))
                   (t (berror "TYPE MISMATCH FOR ~A." variable))))
                (when rest (basic-eval (cdr rest))))
              (berror "INVALID TOKEN ~S IN EXPRESSION." (car rest))))
          (berror "INVALID TOKEN ~S IN STATEMENT." (car statement)))))
     ) ;;case
   (if *current-line*
     (progn
       (incf *current-line*)
       (setq statement (find-line-or-next *current-line*)))
     (return nil))))


(defun basic-process-line (line)
  "
DO:         Process one BASIC line.
"
  (multiple-value-bind (tokens err) (split-line line)
    (setq tokens (let ((*package* (find-package "BASIC")))
                   (mapcar (lambda (item) (read-from-string item)) tokens)))
    (cond (err (berror "~A" err))
          ((and (< 0 (length tokens)) (integerp (car tokens)))
           (when (check-line (car tokens))
             (setf (aref *program* (car tokens)) (cdr tokens))))
          (t (setq *current-line* nil)
             (basic-eval tokens)))))


(defun basic ()
  "
DO:         Read a line and either execute it or store it in the program.
            Repeat until the BYE command is executed.
"
  (setf (fill-pointer *stack*) 0)
  (setq *data-ptr* (cons 0 nil))
  (format t "*** QUICK-DIRTY-AND-UGLY BASIC, VERSION 0.1 ***~%~
             COPYRIGHT PASCAL J. BOURGUIGNON 2003~%~
             QUICK-DIRTY-AND-UGLY BASIC COMES WITH *ABSOLUTELY NO WARRANTY*.~%~
             THIS IS FREE SOFTWARE, AND YOU ARE WELCOME TO REDISTRIBUTE IT~%~
             UNDER THE CONDITIONS LISTED IN THE GNU PUBLIC LICENSE.~4%")
  (block :top-level
    (loop
     (format t "~&> ")
     (let ((line (read-line *standard-input* nil nil)))
       (unless line (return-from :top-level))
       (if (eq :bye (basic-process-line line))
         (return-from :top-level)))))
  (values))


(defun main (&rest arguments)
  (declare (ignore arguments))
  (basic))


;;;; basic.lisp                       -- 2004-03-14 01:34:04 -- pascal   ;;;;
