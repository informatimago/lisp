;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               norvig-graph.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             clisp
;;;;USER-INTERFACE:     clisp
;;;;DESCRIPTION
;;;;    
;;;;    This file extracts the requires sexps from the norvig sources and
;;;;    builds a dependency graph to be displayed by dot(1).
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2003-05-16 <PJB>  Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2003
;;;;    mailto:pjb@informatimago.com
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


(LOAD "PACKAGE:COM;INFORMATIMAGO;COMMON-LISP;GRAPH")
(LOAD "PACKAGE:COM;INFORMATIMAGO;COMMON-LISP;GRAPH-DOT")
(LOAD "PACKAGE:COM;INFORMATIMAGO;COMMON-LISP;LIST")
(LOAD "PACKAGE:COM;INFORMATIMAGO;COMMON-LISP;UTILITY")
(USE-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GRAPH")
(USE-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GRAPH-DOT")
(USE-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.LIST")
(USE-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")

(DEFVAR DATA)
(DEFVAR G)

(SETQ DATA
      (MAPCAN
       (LAMBDA (FILE)
         (LET ((REQUIRES
                (WITH-OPEN-FILE (IN FILE :DIRECTION :INPUT)
                  (LET ((*READTABLE* (COPY-READTABLE NIL)))
                    (SET-DISPATCH-MACRO-CHARACTER
                     #\# #\. (LAMBDA (&REST ARGS) ARGS))
                    (DO* ((EOF (GENSYM "eof"))
                          (SEXP (READ IN NIL EOF) (READ IN NIL EOF))
                          (RESULT (LIST)))
                        ((EQ EOF SEXP) RESULT)
                      (WHEN (AND (CONSP SEXP) (EQ 'REQUIRES (CAR SEXP)))
                        (SETQ RESULT (NCONC (CDR SEXP) RESULT ))))))  ))
           (WHEN REQUIRES
             (LIST (CONS
                    (LET* ((NAME (FILE-NAMESTRING FILE))
                           (POSI (SEARCH ".lisp" NAME)))
                      (IF POSI (SUBSEQ NAME 0 POSI) NAME))
                    REQUIRES)))
           ))
       (DIRECTORY "NORVIG:*.LISP")))
(SETQ G (MAKE-INSTANCE 'GRAPH-CLASS))
(SET-PROPERTY G :NAME "NORVIG")
(ADD-NODES  G (MAPCAR
               (LAMBDA (NAME) (LET ((NODE (MAKE-INSTANCE 'ELEMENT-CLASS)))
                                (SET-PROPERTY NODE :NAME NAME)
                                NODE))
               (DELETE-DUPLICATES (FLATTEN DATA) :TEST (FUNCTION STRING=))))
(MAPC
 (LAMBDA (ARCS)
   (LET* ((FROM (CAR ARCS))
          (FROM-NODE (CAR (FIND-NODES-WITH-PROPERTY G :NAME FROM))))
     (MAPC
      (LAMBDA (TO)
        (LET ((TO-NODE (CAR (FIND-NODES-WITH-PROPERTY G :NAME TO))))
          (ADD-EDGE-BETWEEN-NODES G FROM-NODE TO-NODE)))
      (CDR ARCS))))
 DATA)

(LET ((FNAME "norvig"))
  (WITH-OPEN-FILE (OUT (FORMAT NIL "~A.dot" FNAME) :DIRECTION :OUTPUT
                       :IF-EXISTS :SUPERSEDE :IF-DOES-NOT-EXIST :CREATE)
    (PRINC (GENERATE-DOT G) OUT))
  (EXT:SHELL (FORMAT NIL "n=~A ; (dot -Tps ${n}.dot -o ${n}.ps;gv ${n}.ps)&"
                     FNAME))
;;;   (EXT:SHELL (FORMAT NIL "n=~A ; (tred ${n}.dot > ${n}-tred.dot ;~
;;;                           dot -Tps ${n}-tred.dot -o ${n}-tred.ps ;~
;;;                           gv ${n}-tred.ps) & " FNAME))
  )





;; Give a list of conflicts, symbol defineds in two files.
(MAPCON
 (LAMBDA (LEFT-REST)
   (LET ((LEFT (CAR LEFT-REST)))
     (MAPCAN (LAMBDA (RIGHT)
               ;; (FORMAT T "~2%LEFT = ~S~%RIGHT= ~S~%" (CDR LEFT) (CDR RIGHT))
               (LET ((RES (INTERSECTION (CDR LEFT) (CDR RIGHT)
                                        :TEST (FUNCTION STRING-EQUAL))))
                 (IF RES (LIST (CONS (CAR LEFT) (CONS (CAR RIGHT) RES))) NIL)))
             (CDR LEFT-REST))))
 (REMOVE-IF
  (LAMBDA (L) (= 1 (LENGTH L)))
  (MAPCAR
   (LAMBDA (FILE)
     (CONS FILE
           (MAPCAR
            (LAMBDA (ITEM)
              (COND
               ((SYMBOLP (SECOND ITEM))
                (SECOND ITEM))
               ((AND (CONSP   (SECOND ITEM))
                     (SYMBOLP (CAR (SECOND ITEM))))
                (CAR (SECOND ITEM)))
               (T NIL)))
            (WITH-OPEN-FILE (IN FILE :DIRECTION :INPUT)
              (LET ((*READTABLE* (COPY-READTABLE NIL)))
                (SET-DISPATCH-MACRO-CHARACTER
                 #\# #\. (LAMBDA (&REST ARGS) ARGS))
                (DO* ((EOF (GENSYM "eof"))
                      (SEXP (READ IN NIL EOF) (READ IN NIL EOF))
                      (RESULT ()))
                    ((EQ EOF SEXP) RESULT)
                  (WHEN (AND (CONSP SEXP)
                             (< 3 (LENGTH (STRING (CAR SEXP))))
                             (STRING-EQUAL
                              "DEF" (SUBSEQ (STRING (CAR SEXP)) 0 3)))
                    (PUSH SEXP RESULT))))))))
   (DIRECTORY "NORVIG:*.LISP"))))



 
(  

 ("eliza.lisp" "intro.lisp" MAPPEND)

 ("eliza.lisp" "eliza1.lisp" *ELIZA-RULES* MAPPEND ELIZA)

 ("eliza.lisp" "eliza-pm.lisp" ELIZA)

 ("eliza.lisp" "unifgram.lisp" PUNCTUATION-P)

 ("eliza.lisp" "auxfns.lisp" MAPPEND)

 ("prolog.lisp" "prolog1.lisp" VARIABLES-IN   SHOW-PROLOG-VARS
  TOP-LEVEL-PROVE PROVE PROVE-ALL ?- FIND-ANYWHERE-IF
  UNIQUE-FIND-ANYWHERE-IF RENAME-VARIABLES CLEAR-PREDICATE CLEAR-DB
  ADD-CLAUSE <- *DB-PREDICATES* PREDICATE GET-CLAUSES CLAUSE-BODY
  CLAUSE-HEAD)

 ("prolog.lisp" "krep2.lisp" SHOW-PROLOG-VARS TOP-LEVEL-PROVE PROVE
  PROVE-ALL)

 ("prolog.lisp" "prologc2.lisp" ARGS)

 ("prolog.lisp" "prologc1.lisp" ARGS)

 ("prolog.lisp" "krep.lisp" REPLACE-?-VARS)

 ("prolog.lisp" "prologc.lisp" TOP-LEVEL-PROVE ADD-CLAUSE <- ARGS)

 ("prolog.lisp" "compile3.lisp" ARGS)

 ("intro.lisp" "eliza1.lisp" MAPPEND)

 ("intro.lisp"   "auxfns.lisp" MAPPEND)

 ("search.lisp" "mycin.lisp" IS IS)

 ("search.lisp" "compile3.lisp" IS IS)

 ("search.lisp" "gps.lisp"   FIND-PATH)

 ("othello2.lisp" "othello.lisp" MOBILITY ALL-SQUARES)

 ("othello2.lisp" "overview.lisp" NODE)

 ("simple.lisp" "lexicon.lisp"   VERB NOUN)

 ("simple.lisp" "eliza1.lisp" RANDOM-ELT)

 ("simple.lisp"   "syntax3.lisp" *GRAMMAR*)

 ("simple.lisp" "syntax2.lisp" *GRAMMAR*)

 ("simple.lisp" "syntax1.lisp" *GRAMMAR*)

 ("simple.lisp"   "auxfns.lisp" RANDOM-ELT)

 ("compopt.lisp" "mycin-r.lisp" NIL)

 ("eliza1.lisp" "eliza-pm.lisp" USE-ELIZA-RULES ELIZA)

 ("eliza1.lisp"   "patmatch.lisp" SEGMENT-MATCH SEGMENT-MATCH
  SEGMENT-PATTERN-P   PAT-MATCH EXTEND-BINDINGS MATCH-VARIABLE
  PAT-MATCH EXTEND-BINDINGS   LOOKUP BINDING-VAL GET-BINDING FAIL
  VARIABLE-P)

 ("eliza1.lisp"   "auxfns.lisp" RANDOM-ELT MAPPEND MKLIST FLATTEN
  PAT-MATCH   EXTEND-BINDINGS MATCH-VARIABLE PAT-MATCH EXTEND-BINDINGS
  LOOKUP   GET-BINDING FAIL VARIABLE-P)

 ("eliza1.lisp" "cmacsyma.lisp"   VARIABLE-P)

 ("eliza1.lisp" "macsyma.lisp" VARIABLE-P)

 ("syntax3.lisp" "syntax2.lisp" INTEGERS 10*N+D INFIX-FUNCALL
  EXTEND-PARSE PARSE TERMINAL-TREE-P APPLY-SEMANTICS LEXICAL-RULES
  *OPEN-CATEGORIES* PARSER APPEND1 COMPLETE-PARSES FIRST-OR-NIL
  RULES-STARTING-WITH LEXICAL-RULES PARSE-LHS PARSE USE TREE RULE
  *GRAMMAR*)

 ("syntax3.lisp" "syntax1.lisp" EXTEND-PARSE PARSE   LEXICAL-RULES
  *OPEN-CATEGORIES* PARSER APPEND1 COMPLETE-PARSES
  RULES-STARTING-WITH LEXICAL-RULES PARSE-LHS PARSE USE RULE
  *GRAMMAR*)

 ("syntax3.lisp" "mycin.lisp" RULE)

 ("syntax3.lisp"   "loop.lisp" SUM REPEAT)

 ("syntax3.lisp" "unifgram.lisp" RULE)

 ("syntax3.lisp" "student.lisp" RULE)

 ("syntax3.lisp" "auxfns.lisp"   FIRST-OR-NIL)

 ("syntax3.lisp" "cmacsyma.lisp" RULE)

 ("syntax3.lisp"   "macsyma.lisp" RULE)

 ("syntax3.lisp" "compile3.lisp" ARG2)

 ("syntax3.lisp" "gps.lisp" USE)

 ("syntax2.lisp" "syntax1.lisp"   EXTEND-PARSE PARSE *OPEN-CATEGORIES*
  USE PARSER APPEND1   COMPLETE-PARSES RULES-STARTING-WITH
  LEXICAL-RULES PARSE-LHS PARSE   RULE *GRAMMAR*)

 ("syntax2.lisp" "mycin.lisp" RULE)

 ("syntax2.lisp"   "unifgram.lisp" RULE)

 ("syntax2.lisp" "student.lisp" RULE)

 ("syntax2.lisp" "auxfns.lisp" FIRST-OR-NIL)

 ("syntax2.lisp"   "cmacsyma.lisp" RULE)

 ("syntax2.lisp" "macsyma.lisp" RULE)

 ("syntax2.lisp" "gps.lisp" USE)

 ("syntax1.lisp" "mycin.lisp" RULE)

 ("syntax1.lisp" "unifgram.lisp" RULE)

 ("syntax1.lisp" "student.lisp"   RULE)

 ("syntax1.lisp" "cmacsyma.lisp" RULE)

 ("syntax1.lisp"   "macsyma.lisp" RULE)

 ("syntax1.lisp" "gps.lisp" USE)

 ("prolog1.lisp"   "krep2.lisp" SHOW-PROLOG-VARS TOP-LEVEL-PROVE
  PROVE-ALL PROVE)

 ("prolog1.lisp" "prologc.lisp" TOP-LEVEL-PROVE ADD-CLAUSE <-)

 ("mycin.lisp" "unifgram.lisp" RULE)

 ("mycin.lisp" "overview.lisp"   TRUE)

 ("mycin.lisp" "student.lisp" RULE)

 ("mycin.lisp"   "cmacsyma.lisp" RULE)

 ("mycin.lisp" "macsyma.lisp" RULE)

 ("mycin.lisp" "compile3.lisp" IS)

 ("loop.lisp" "overview.lisp"   WHILE)

 ("patmatch.lisp" "auxfns.lisp" MATCH-VARIABLE EXTEND-BINDINGS
  LOOKUP GET-BINDING VARIABLE-P FAIL PAT-MATCH)

 ("patmatch.lisp"   "cmacsyma.lisp" VARIABLE-P)

 ("patmatch.lisp" "macsyma.lisp"   VARIABLE-P)

 ("unifgram.lisp" "student.lisp" RULE)

 ("unifgram.lisp"   "cmacsyma.lisp" RULE)

 ("unifgram.lisp" "macsyma.lisp" RULE)

 ("krep2.lisp" "krep1.lisp" RETRIEVE MAPC-RETRIEVE INDEX)

 ("krep2.lisp" "krep.lisp" ADD-FACT INDEX)

 ("krep2.lisp"   "prologc.lisp" TOP-LEVEL-PROVE)

 ("krep1.lisp" "krep.lisp"   DTREE-INDEX INDEX)

 ("prologc2.lisp" "prologc1.lisp" COMPILE-CLAUSE   COMPILE-PREDICATE
  PROPER-LISTP HAS-VARIABLE-P COMPILE-ARG   COMPILE-UNIFY =
  DEF-PROLOG-COMPILER-MACRO PROLOG-COMPILER-MACRO   COMPILE-CALL
  COMPILE-BODY MAKE-= MAKE-PREDICATE MAKE-PARAMETERS ARGS
  RELATION-ARITY CLAUSES-WITH-ARITY PROLOG-COMPILE VAR *VAR-COUNTER*
  UNDO-BINDINGS!  SET-BINDING! *TRAIL* PRINT-VAR SET-BINDING! UNIFY!
  DEREF BOUND-P VAR UNBOUND)

 ("prologc2.lisp" "prologc.lisp"   BIND-UNBOUND-VARS
  MAYBE-ADD-UNDO-BINDINGS COMPILE-CLAUSE   COMPILE-PREDICATE
  PROPER-LISTP HAS-VARIABLE-P COMPILE-ARG   COMPILE-UNIFY =
  DEF-PROLOG-COMPILER-MACRO PROLOG-COMPILER-MACRO   COMPILE-CALL
  COMPILE-BODY MAKE-= MAKE-PREDICATE MAKE-PARAMETERS ARGS
  RELATION-ARITY CLAUSES-WITH-ARITY PROLOG-COMPILE VAR *VAR-COUNTER*
  UNDO-BINDINGS! SET-BINDING! *TRAIL* PRINT-VAR SET-BINDING! UNIFY!
  DEREF BOUND-P VAR UNBOUND)

 ("prologc2.lisp" "compile3.lisp" ARGS)

 ("prologc1.lisp" "prologc.lisp" PROPER-LISTP HAS-VARIABLE-P
  COMPILE-ARG COMPILE-UNIFY = DEF-PROLOG-COMPILER-MACRO
  PROLOG-COMPILER-MACRO COMPILE-CALL COMPILE-BODY MAKE-=
  COMPILE-CLAUSE MAKE-PREDICATE MAKE-PARAMETERS COMPILE-PREDICATE ARGS
  RELATION-ARITY CLAUSES-WITH-ARITY PROLOG-COMPILE VAR *VAR-COUNTER*
  UNDO-BINDINGS! SET-BINDING! *TRAIL* PRINT-VAR SET-BINDING! UNIFY!
  DEREF BOUND-P VAR UNBOUND)

 ("prologc1.lisp" "compile3.lisp" ARGS)

 ("overview.lisp" "auxfns.lisp" FIND-ALL)

 ("auxmacs.lisp"   "macsyma.lisp" FIND-ANYWHERE)

 ("auxmacs.lisp" "gps.lisp"   STARTS-WITH)

 ("student.lisp" "cmacsyma.lisp" PREFIX->INFIX   BINARY-EXP-P EXP-ARGS
  EXP-P EXP RULE)

 ("student.lisp" "macsyma.lisp"   PREFIX->INFIX BINARY-EXP-P EXP-ARGS
  EXP-P EXP RULE)

 ("auxfns.lisp"   "interp1.lisp" DELAY DELAY)

 ("auxfns.lisp" "cmacsyma.lisp"   VARIABLE-P)

 ("auxfns.lisp" "macsyma.lisp" VARIABLE-P PARTITION-IF)

 ("auxfns.lisp" "gps.lisp" MEMBER-EQUAL)

 ("prologc.lisp"   "compile3.lisp" ARGS)

 ("gps1.lisp" "gps.lisp" APPLY-OP APPROPRIATE-P   ACHIEVE GPS OP
  *OPS*)

 ("interp3.lisp" "interp2.lisp" INTERP)

 ("interp3.lisp" "interp1.lisp" INIT-SCHEME-PROC SCHEME INTERP)

 ("interp3.lisp" "compile3.lisp" SCHEME)

 ("interp2.lisp"   "interp1.lisp" INTERP)

 ("interp1.lisp" "compile3.lisp" SCHEME)

 ("interp1.lisp" "compile1.lisp" DEFINE)

 ("cmacsyma.lisp"   "macsyma.lisp" PREFIX->INFIX *INFIX->PREFIX-RULES*
  VARIABLE-P   INFIX->PREFIX BINARY-EXP-P EXP-ARGS EXP-P EXP RULE)

 ("compile3.lisp"   "compile2.lisp" *PRIMITIVE-FNS* OPTYMIZE
  INIT-SCHEME-COMP ASSEMBLE)

 ("compile3.lisp" "compile1.lisp" SHOW-FN)

 ("compile2.lisp"   "compile1.lisp" COMP-LAMBDA GEN-SET COMP-IF
  COMP-BEGIN COMP)

 )



;;;; norvig-graph.lisp                -- 2003-05-16 07:11:44 -- pascal   ;;;;
