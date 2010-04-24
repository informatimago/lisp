;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pmatch.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Sexp Pattern Matcher.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-12-17 <PJB> Created.
;;;;BUGS
;;;;    pattern matcher and instantiation won't work with arrays/matrices,
;;;;    structures...
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2004
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
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PMATCH"
  (:USE "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
  (:EXPORT "MATCH-CASE" "COLLECT-VARIABLES" ":" "?/" "?*" "?+" "??" "?N" "?X"
           "?C" "?V" "?AX" "?AC" "?AV" "MATCH-DICT-MAP" "MATCH-STATE-DICT"
           "MATCH-STATE-FAILED-P" "MATCH")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "WITH-GENSYMS")
  (:DOCUMENTATION
   "Sexp Pattern Matcher

    Copyright Pascal J. Bourguignon 2003 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PMATCH")




(defun make-match-state (&key dict) dict)
(defun match-state-dict (ms) ms)

(DEFUN MATCH-STATE-FAIL     (MS raison)
  "PRIVATE"
  (make-match-state :dict  (list* :failed raison (MATCH-STATE-DICT MS))))


(DEFUN MATCH-STATE-FAILED-P (MS)
  "
RETURN: Whether the match failed.
"
  (EQ :FAILED (CAR (MATCH-STATE-DICT MS))) )


(DEFUN MATCH-STATE-RETRY    (MS)
  "PRIVATE" 
  (if (MATCH-STATE-FAILED-P MS)
      (make-match-state :dict (cdr (MATCH-STATE-DICT MS)))
      ms))


(DEFUN MATCH-DICT-MAP (MS FUNCTION)
  "
DO:     Calls FUNCTION (lambda (symbol value) ...) with all successive bindings,
        (unless matching state is failed).
RETURN: The list of results of the FUNCTION.
"
  (UNLESS (MATCH-STATE-FAILED-P MS)
    (MAPCAR (LAMBDA (BINDING) (FUNCALL FUNCTION (FIRST BINDING) (SECOND BINDING)))
            (MATCH-STATE-DICT MS))))


(DEFUN MATCH-DICT-ADD-BINDING (MS PAT EXP)
  "PRIVATE" 
  (LET* ((VAR (SECOND (CAR PAT)))
         (VAL (CAR EXP))
         (ASS (ASSOC VAR (MATCH-STATE-DICT MS))))
    (cond
      ((null ASS)                       ; a new binding:
       (make-match-state :dict (cons (CONS VAR VAL) (MATCH-STATE-DICT MS))))
      ((EQUALP (CDR ASS) VAL)           ; already there, same
       ms)
      (t                                ; already there, different
       (match-state-fail ms `(:mismatching-binding ,ass ,val))))))


(DEFMACRO DEFPATTERN (NAME PATTERN)
  (IF (SYMBOLP PATTERN)
      `(DEFUN ,NAME (PAT) ;; (?ac ...)
         "PRIVATE"
         (AND (LISTP PAT) (SYMBOLP (CAR PAT))
              (STRING= ',PATTERN (CAR PAT))))
      `(DEFUN ,NAME (PAT) ;; ((?n ...)...)
         "PRIVATE" 
         (AND (LISTP PAT) (LISTP (CAR PAT)) (SYMBOLP (CAAR PAT))
              (STRING= ',(CAR PATTERN) (CAAR PAT))))))


(DEFPATTERN PAT-ANONYMOUS-VARIABLE-P   ?AV)
(DEFPATTERN PAT-ANONYMOUS-CONSTANT-P   ?AC)
(DEFPATTERN PAT-ANONYMOUS-EXPRESSION-P ?AX)
(DEFPATTERN PAT-NAMED-P                (?N))
(DEFPATTERN PAT-VARIABLE-P             (?V))
(DEFPATTERN PAT-CONSTANT-P             (?C))
(DEFPATTERN PAT-EXPRESSION-P           (?X))
(DEFPATTERN PAT-OPTIONAL-P             (??))
(DEFPATTERN PAT-REPEAT-P               (?+))
(DEFPATTERN PAT-OPTIONAL-REPEAT-P      (?*))
(DEFPATTERN PAT-ALTERNATIVE-P          (?/))
(DEFPATTERN PAT-SQUELETON-EVAL-P       |:|)


(DEFUN EXP-VARIABLE-P (EXP) "PRIVATE" (AND (CONSP EXP)  (SYMBOLP   (CAR EXP))))
(DEFUN EXP-CONSTANT-P (EXP) "PRIVATE" (AND (CONSP EXP)  (CONSTANTP (CAR EXP))))


;; (?n n ?v) == (?v n)


;; (match '((?+ a b c)) '(a b c a b c))



;; pattern ::= term | ( pattern-seq ) .
;; pattern-seq ::= | pattern pattern-seq .
;; pattern-lst ::= | pattern pattern-lst .
;;
;; term      ::= ?av | ?ac | ?ax                    -- anonymous terms
;;             | (?v name) | (?c name) | (?x name)  -- named terms
;;             | (?n name pattern-seq)              -- named sequence
;;             | (?? pattern-seq )                  -- optional sequence
;;             | (?+ pattern-seq )                  -- repeat sequence
;;             | (?* pattern-seq )                  -- optional repeat sequence
;;             | (?/ pattern-lst )                  -- alternative
;;             | atom | compound .
;;
;; name     ::= symbol .
;;
;; atom     ::= symbol | string | character | number .
;; compound ::= list | array | structure .
;; list     ::= ( pattern-seq ) .
;; array    ::= #A( pattern-seq )
;;            | #1A( pattern-seq )
;;            | #2A( (pattern-seq)... )
;;            | #3A( ((pattern-seq)...)... )
;;            | ... .

(defun generate-all-follows (exp)
  "
RETURN: a list of possible follows from shortest to longuest.
"
  (DO ((REST  EXP (CDR REST)) ;; what should match after
       (LIST  '())            ;; what we match (reversed),
       ;;             we reverse only in the final result [F].
       (FRAME '()))
      ((NULL REST)
       (PUSH (LIST LIST REST) FRAME)
       FRAME)
    (PUSH (LIST LIST REST) FRAME)
    (PUSH (CAR REST) LIST)))


(DEFUN MATCH (PAT EXP &OPTIONAL (MS (MAKE-MATCH-STATE)))
  "
DO:        A pattern matcher accepting the following syntax:
             ?av        expects a symbol (variable).
             ?ac        expects a constant (constantp).
             ?ax        expects anything (one item).
             (?v n)     expects a symbol (variable)     and bind it.
             (?c n)     expects a constant (constantp)  and bind it.
             (?x n)     expects anything (one item)     and bind it.
             (?n n ...) expects anything (several item) and bind them.
             (?+ ...)   expects anything (one or more times).  AOB
             (?* ...)   expects anything (zero or more times). AOB
             (?? ...)   expects anything (zero or one time).
             ...        expects exactly ... (can be a sublist).
           AOB = All occurences bind.
RETURN:    A match-state structure.
SEE ALSO:  match-state-failed-p to check if the matching failed.
           match-state-dict     to get the binding dictionary.
"
  ;; The pattern and the expression may be atoms or lists,
  ;; but usually we process (car pat) and (car exp), to be able
  ;; to match several items (?+ and ?*).
  (COND
    ((MATCH-STATE-FAILED-P MS) MS)
    ((ATOM PAT)
     (IF (EQUAL PAT EXP)
         MS
         (MATCH-STATE-FAIL MS `(:different ,pat ,exp))))
    ((PAT-ANONYMOUS-CONSTANT-P PAT)
     (IF (EXP-CONSTANT-P EXP)
         (MATCH (CDR PAT) (CDR EXP) MS)
         (MATCH-STATE-FAIL MS `(:not-constant ,exp))))
    ((PAT-ANONYMOUS-VARIABLE-P PAT)
     (IF (EXP-VARIABLE-P EXP)
         (MATCH (CDR PAT) (CDR EXP) MS)
         (MATCH-STATE-FAIL MS `(:not-variable ,exp))))
    ((PAT-ANONYMOUS-EXPRESSION-P PAT)
     (IF (NULL EXP)
         (MATCH-STATE-FAIL MS `(:missing-expression))
         (MATCH (CDR PAT) (CDR EXP) MS)))
    ((PAT-CONSTANT-P PAT)
     (IF (EXP-CONSTANT-P EXP)
         (MATCH (CDR PAT) (CDR EXP) (MATCH-DICT-ADD-BINDING MS pat EXP))
         (MATCH-STATE-FAIL MS `(:not-constant ,exp))))
    ((PAT-VARIABLE-P PAT)
     (IF (EXP-VARIABLE-P EXP)
         (MATCH (CDR PAT) (CDR EXP) (MATCH-DICT-ADD-BINDING MS pat EXP))
         (MATCH-STATE-FAIL MS `(:not-variable ,exp))))
    ((PAT-EXPRESSION-P PAT)
     (IF (NULL EXP)
         (MATCH-STATE-FAIL MS `(:missing-expression))
         (MATCH (CDR PAT) (CDR EXP) (MATCH-DICT-ADD-BINDING MS pat EXP)) ))
    ((PAT-NAMED-P PAT)
     (loop
        for (list rest) in (generate-all-follows exp)
        for soe = (MATCH (CDR PAT) REST MS)
        for nms = (if (MATCH-STATE-FAILED-P soe)
                      soe
                      (let* ((list (reverse list))
                             (nms (match (cddar pat) list soe)))
                        (if (MATCH-STATE-FAILED-P nms)
                            nms
                            (match-dict-add-binding nms pat (list list)))))
        while (MATCH-STATE-FAILED-P nms)
        finally (return nms)))
    ((AND (PAT-REPEAT-P PAT) (NULL EXP))
     (MATCH-STATE-FAIL MS `(:missing-repeat ,pat)))
    ((OR (PAT-REPEAT-P PAT) (PAT-OPTIONAL-REPEAT-P PAT) (PAT-OPTIONAL-P PAT))
     (loop
        for (list rest) in (generate-all-follows exp)
        for soe = (MATCH (CDR PAT) REST MS)
        for nms = (if (MATCH-STATE-FAILED-P soe)
                      soe
                      (cond
                        ((PAT-REPEAT-P PAT)
                         ;; at least one (...2... already matches)
                         ;; ((?+ ...1...) ...2...)
                         ;; --> (...1... (?* ...1...) ...2...)
                         (match (append (cdar pat) (list (cons '?* (cdar pat))))
                                (reverse list) soe))
                        ((PAT-OPTIONAL-REPEAT-P PAT)
                         ;; zero or more (...2... already matches)
                         ;; ((?* ...1...) ...2...)
                         ;; --> (...1... (?* ...1...) ...2...)
                         ;; --> (...2...)
                         (let ((nms (match (append (cdar pat) (list (car pat)))
                                           (reverse list) soe)))
                           (if (MATCH-STATE-FAILED-P nms)
                               (match nil list soe)
                               nms)))
                        ((PAT-OPTIONAL-P PAT)
                         ;; zero or one (...2... already matches)
                         ;; ((?? ...1...) ...2...)
                         ;; --> (...1... ...2...)
                         ;; --> (...2...)
                         (let ((nms (match  (cdar pat) (REVERSE LIST) soe)))
                           (if (MATCH-STATE-FAILED-P nms)
                               (match  nil list soe)
                               nms)))))
        while (MATCH-STATE-FAILED-P nms)
        finally (return nms)))
    ((ATOM EXP)
     (MATCH-STATE-FAIL MS `(:unexpected-atom ,exp)))
    (T ;; both cars are sublists.
     (MATCH (CDR PAT) (CDR EXP) (MATCH (CAR PAT) (CAR EXP) MS)))))


(DEFUN EVALUATE (INSTANCE)
  "PRIVATE"
  (COND
    ((ATOM INSTANCE)               INSTANCE)
    ((AND (ATOM (CAR INSTANCE)) (STRING= :|| (CAR INSTANCE)))
     (EVAL (EVALUATE (SECOND INSTANCE))))
    (T (MAPCAR (FUNCTION EVALUATE) INSTANCE))))


(DEFUN DICT-VALUE (DICT NAME)
  "PRIVATE"
  (SECOND (ASSOC NAME DICT :TEST (FUNCTION STRING=))))


(DEFUN DICT-BOUNDP (DICT NAME)
  "PRIVATE"
  (AND (OR (SYMBOLP NAME) (STRINGP NAME))
       (ASSOC NAME DICT :TEST (FUNCTION STRING=))))
  

(DEFUN SUBST-BINDINGS (EXPR DICT)
  "PRIVATE"
  (COND
    ((ATOM EXPR) (LIST EXPR))
    ((AND (ATOM (FIRST EXPR)) (STRING= :||  (FIRST EXPR)))
     (IF (AND (ATOM (SECOND EXPR))
              (DICT-BOUNDP DICT (SECOND EXPR)))
         (LIST (DICT-VALUE DICT (SECOND EXPR)))
         (LIST (MAPCAN (LAMBDA (SUBEXPR) (SUBST-BINDINGS SUBEXPR DICT)) EXPR))))
    ((AND (ATOM (FIRST EXPR)) (STRING= :|@| (FIRST EXPR)))
     (COPY-SEQ (DICT-VALUE DICT (SECOND EXPR))))
    (T (LIST (MAPCAN (LAMBDA (SUBEXPR) (SUBST-BINDINGS SUBEXPR DICT))
                     EXPR)))))


(DEFUN INSTANCIATE (MS SKELETON)
  "PRIVATE
PRE:   (not (match-state-failed-p ms))
DO:    Instanciate the skeleton, substituting all occurence of (: var)
       with the value bound to var in the binding dictionary of MS,
       Occurences of (:@ var) are split in line like ,@ in backquotes.
       Then all remaining (: form) are evaluated (with eval) from the
       deepest first.
"
  (ASSERT (NOT (MATCH-STATE-FAILED-P MS)))
  (EVALUATE (FIRST (SUBST-BINDINGS SKELETON (MATCH-STATE-DICT MS)))))


(DEFUN COLLECT-VARIABLES (PAT)
  "
PAT:       A symbolic expression with the following syntax:
             (?v v)  expects a symbol (variable).
             (?c c)  expects a constant (constantp).
             (?x x)  expects anything (one item).
             (?+ l)  expects anything (one or more items).
             (?* l)  expects anything (zero or more items).
             other   expects exactly other (can be a sublist).
RETURN:    A list of the symbol used in the various (?. sym) items, 
           in no particular order, but with duplicates deleted.
"
  (DELETE-DUPLICATES
   (COND
     ((ATOM PAT)
      NIL)
     ((AND (ATOM (CAR PAT))
           (MEMBER (CAR PAT) '(?V ?C ?X ?+ ?*) :TEST (FUNCTION STRING=)))
      (LIST (CADR PAT)))
     (T
      (NCONC (COLLECT-VARIABLES (CAR PAT)) (COLLECT-VARIABLES (CDR PAT)))))))


(DEFMACRO MATCH-CASE (SEXP &REST CLAUSES)
  "
SEXP:    A symbolic expression, evaluated.
CLAUSES: A list of (pattern &body body)
         The pattern must be litteral. 
         Lexical variable names are extracted from it, and body is executed
         in a lexical environment where these names are bound to the matched
         subexpression of SEXP.
DO:      Execute the body of the clause whose pattern matches the SEXP,
         or whose pattern is a symbol string-equal to OTHERWISE.
EXAMPLE: (match-case expr
            ((add       (?x a) to   (?x b)) `(+ ,a ,b))
            ((multiply  (?x a) with (?x b)) `(* ,a ,b))
            ((substract (?x a) from (?x a)) 0)
            (otherwise                      :error))
"
  (WITH-GENSYMS (EX MS DC)
    `(LET ((,EX ,SEXP) (,MS) (,DC))
       (COND
         ,@(MAPCAR
            (LAMBDA (CLAUSE)
              (LET ((PAT (CAR CLAUSE)) (BODY (CDR CLAUSE)))
                (IF (AND (SYMBOLP PAT) (STRING-EQUAL "OTHERWISE" PAT))
                    `(T ,@BODY)
                    `((PROGN (SETF ,MS (MATCH ',PAT ,EX))
                             (NOT (MATCH-STATE-FAILED-P ,MS)))
                      (SETF ,DC (MATCH-STATE-DICT ,MS))
                      (LET ( ,@(MAPCAR
                                (LAMBDA (NAME) `(,NAME (CDR (ASSOC ',NAME ,DC)))) 
                                (COLLECT-VARIABLES PAT)) )
                        ,@BODY))))) CLAUSES)))))


;;;; pmatch.lisp                      --                     --          ;;;;
