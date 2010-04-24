;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               norvig.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             -
;;;;USER-INTERFACE:     -
;;;;DESCRIPTION
;;;;    
;;;;    Loaded for Norvig's code
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2003-05-14 <PJB> Created.
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


(defpackage "COM.NORVIG"
  (:use "COMMON-LISP")
  (:SHADOW "EXP"))
(in-package "COM.NORVIG")

(DEFPARAMETER CONFIGURATION
  '(GPS-SEARCH  ELISA-PM PROLOG-CP KREP SYNTAX-3 SCHEME-3 SCHEME-C3))


(DEFMACRO EXCLUSIVE (&REST E-B)
  (LET ((ETIQUETTES (MAPCAR (FUNCTION CAR) E-B))
        (REP (GENSYM "REP")))
    `(LET ((,REP (INTERSECTION CONFIGURATION ',ETIQUETTES)))
       (IF (= 1 (LENGTH ,REP))
         (PROGN
           (SETQ ,REP (CAR ,REP))
           (FORMAT T "~&CONFIGURED: ~A~%" ,REP))
         (SETQ ,REP
               (DO ((,REP NIL))
                   (,REP ,REP)
                 (FORMAT *QUERY-IO* "~&Please choose what to load from ~S: "
                         ',ETIQUETTES)
                 (SETQ ,REP (READ *QUERY-IO*))
                 (UNLESS (MEMBER ,REP ',ETIQUETTES) (SETQ ,REP NIL)))))
       (CASE ,REP
         ,@E-B))))




(DEFVAR *PAIP-SOURCE-FILES*
  (TRANSLATE-LOGICAL-PATHNAME (MAKE-PATHNAME
                               :HOST "NORVIG"
                               :DIRECTORY '(:ABSOLUTE)
                               :NAME NIL
                               :TYPE "LISP"))
  "The location of the source files for this book.
  CHANGE IT TO REFLECT THE LOCATION OF THE FILES ON YOUR COMPUTER.")


(LOAD "norvig:intro.lisp") ;; A few simple definitions
(LOAD "norvig:simple.lisp") ;; Random sentence generator (two versions)
(LOAD "norvig:overview.lisp") ;; 14 versions of LENGTH and other examples


(LOAD "norvig:auxmacs.lisp") ;; A few macros; load this first.

(UNLESS (FBOUNDP 'COMMON-LISP:LOOP)
  (LOAD "norvig:loop.lisp")) ;; Load if your Lisp doesn't support ANSI LOOP

(LOAD "norvig:auxfns.lisp") ;; Commonly used auxiliary functions.


(LOAD "norvig:search.lisp") ;; Search Utility
;;     "norvig:search.dat"      ;; Test data (examples) for above

(EXCLUSIVE
 (GPS-SIMPLE
  (LOAD "norvig:gps1.lisp") ;; Simple version of General Problem Solver
  ;;     "norvig:gps1.dat"      ;; Test data (examples) for above
  )
 (GPS
  (LOAD "norvig:gps1.lisp") ;; Simple version of General Problem Solver
  (LOAD "norvig:gps.lisp") ;; Final version of General Problem Solver
  ;;     "norvig:gps.dat"       ;; Test data (examples) for above
  )
 (GPS-SEARCH
  (LOAD "norvig:gps1.lisp") ;; Simple version of General Problem Solver
  (LOAD "norvig:gps.lisp") ;; Final version of General Problem Solver
  (LOAD "norvig:gps-srch.lisp") ;; Version of GPS using the search utility 
  ;;     "norvig:gps-srch.dat"  ;; Test data (examples) for above
  ))



(LOAD "norvig:patmatch.lisp") ;; Pattern Matching Utility
;;     "norvig:patmatch.dat"    ;; Test data (examples) for above


(EXCLUSIVE
 (ELISA-BASIC
  (LOAD "norvig:eliza1.lisp") ;; Basic version of Eliza program
  )
 (ELISA
  (LOAD "norvig:eliza.lisp") ;; Eliza with more rules; different reader
  )
 (ELISA-PM
  (LOAD "norvig:eliza-pm.lisp") ;; Version of Eliza using utilities
  (LOAD "norvig:cmacsyma.lisp") ;; Efficient Macsyma with canonical form
  ;;     "norvig:cmacsyma.dat"  ;; Test data (examples) for above
  ))


(LOAD "norvig:eliza1.lisp")   ;; Needed by The Student Program
(LOAD "norvig:student.lisp")  ;; The Student Program
;;     "norvig:student.dat"   ;; Test data (examples) for above

(LOAD "norvig:macsyma.lisp") ;; The Macsyma Program
(LOAD "norvig:macsymar.lisp") ;; Simplification & integration rules for Macsyma
;;     "norvig:macsyma.dat"   ;; Test data (examples) for above


(LOAD "norvig:unify.lisp") ;; Unification functions

(EXCLUSIVE
 (PROLOG1
  (LOAD "norvig:prolog1.lisp") ;; First version of Prolog interpreter
  ;;     "norvig:prolog1.dat"   ;; Test data (examples) for above
  )
 (PROLOG
  (LOAD "norvig:prolog.lisp") ;; Final version of Prolog interpreter
  ;;     "norvig:prolog.dat"    ;; Test data (examples) for above
  )
 (PROLOG-C1
  (LOAD "norvig:prologc1.lisp") ;; First version of Prolog compiler
  ;;     "norvig:prologc1.dat"  ;; Test data (examples) for above
  )
 (PROLOG-C2
  (LOAD "norvig:prologc2.lisp") ;; Second version of Prolog compiler
  ;;     "norvig:prologc2.dat"  ;; Test data (examples) for above
  )
 (PROLOG-C 
  (LOAD "norvig:prologc.lisp") ;; Final version of Prolog compiler
  ;;     "norvig:prologc.dat"   ;; Test data (examples) for above
  )
 (PROLOG-CP
  ;;(LOAD "norvig:prologcp.lisp") ;; Primitives for Prolog compiler
  ;;(LOAD "norvig:unifgram.lisp") ;; Unification Parser
  ;;     "norvig:unifgram.dat"  ;; Test data (examples) for above

  ;;(LOAD "norvig:lexicon.lisp") ;; Sample Lexicon of English
  ;;     "norvig:grammar.dat"   ;; Test data (examples) for above
  (LOAD "norvig:grammar.lisp") ;; Comprehensive grammar of English
  ))



(LOAD "norvig:clos.lisp") ;; Some object-oriented and CLOS code
;;     "norvig:clos.dat"      ;; Test data (examples) for above

(EXCLUSIVE
 (KREP-1
  (LOAD "norvig:krep1.lisp") ;; Knowledge Representation code: first version 
  ;;     "norvig:krep1.dat"   ;; Test data (examples) for above
  )
 (KREP-2
  (LOAD "norvig:krep2.lisp") ;; Knowledge Representation code w/ conjunctions
  )
 (KREP
  (LOAD "norvig:krep.lisp") ;; Final KR code: worlds and attached functions
  ))


(LOAD "norvig:mycin.lisp") ;; The Emycin expert system shell
(LOAD "norvig:mycin-r.lisp") ;; Some rules for a medical application of emycin
;;     "norvig:mycin.dat"     ;; Test data (examples) for above

(LOAD "norvig:waltz.lisp") ;; A Line-Labeling program using Waltz algorithm
;;     "norvig:waltz.dat"     ;; Test data (examples) for above

;; (LOAD "norvig:othello.lisp") ;; The Othello playing program & strategies
;;     "norvig:othello.dat"   ;; Test data (examples) for above
(LOAD "norvig:othello2.lisp") ;; Additional strategies for Othello
(LOAD "norvig:edge-tab.lisp") ;; Edge table for Iago strategy

(EXCLUSIVE
 (SYNTAX-1
  (LOAD "norvig:syntax1.lisp") ;; Syntactic Parser
  ;;     "norvig:syntax1.dat"   ;; Test data (examples) for above
  )
 (SYNTAX-2
  (LOAD "norvig:syntax2.lisp") ;; Syntactic Parser with semantics
  ;;     "norvig:syntax2.dat"   ;; Test data (examples) for above
  )
 (SYNTAX-3
  (LOAD "norvig:syntax3.lisp") ;; Syntactic Parser with semantics and pref.
  ;;     "norvig:syntax3.dat"   ;; Test data (examples) for above
  ))



(EXCLUSIVE
 (SCHEME-1
  (LOAD "norvig:interp1.lisp") ;; Scheme interpreter, incl. version with macros
  ;;     "norvig:interp1.dat"   ;; Test data (examples) for above
  )
 (SCHEME-2
  (LOAD "norvig:interp1.lisp") ;; Scheme interpreter, incl. version with macros
  (LOAD "norvig:interp2.lisp") ;; A tail recurive Scheme interpreter
  )
 (SCHEME-3
  (LOAD "norvig:interp1.lisp") ;; Scheme interpreter, incl. version with macros
  (LOAD "norvig:interp3.lisp") ;; A Scheme interpreter that handles call/cc
  ;;     "norvig:interp3.dat"   ;; Test data (examples) for above
  ))

(EXCLUSIVE
 (SCHEME-C1
  (LOAD "norvig:compile1.lisp") ;; Simple Scheme compiler
  )
 (SCHEME-C2
  (LOAD "norvig:compile2.lisp") ;; Compiler with tail recursion and primitives
  )
 (SCHEME-C3
  (LOAD "norvig:compile3.lisp") ;; Compiler with peephole optimizer
  (LOAD "norvig:compopt.lisp") ;; Peephole optimizers for compile3.lisp
  ;;     "norvig:compile.dat"  ;; Test data (examples) for all 3 versions above
  ))





#|
4  gps1.lisp            Simple version of General Problem Solver
4  gps1.dat             Test data (examples) for above
4  gps.lisp             Final version of General Problem Solver
4  gps.dat              Test data (examples) for above

5  eliza1.lisp          Basic version of Eliza program
5  eliza.lisp           Eliza with more rules; different reader

6  patmatch.lisp        Pattern Matching Utility
6  patmatch.dat         Test data (examples) for above
6  eliza-pm.lisp        Version of Eliza using utilities
6  search.lisp          Search Utility
6  search.dat           Test data (examples) for above
6  gps-srch.lisp        Version of GPS using the search utility 
6  gps-srch.dat         Test data (examples) for above

7  student.lisp         The Student Program
7  student.dat          Test data (examples) for above

8  macsyma.lisp         The Macsyma Program
8  macsymar.lisp        Simplification and integration rules for Macsyma
8  macsyma.dat          Test data (examples) for above

9-10                    <no files; important functions in auxfns.lisp>

11 unify.lisp           Unification functions
11 prolog1.lisp         First version of Prolog interpreter
11 prolog1.dat          Test data (examples) for above
11 prolog.lisp          Final version of Prolog interpreter
11 prolog.dat           Test data (examples) for above

12 prologc1.lisp        First version of Prolog compiler
12 prologc1.dat         Test data (examples) for above
12 prologc2.lisp        Second version of Prolog compiler
12 prologc2.dat         Test data (examples) for above
12 prologc.lisp         Final version of Prolog compiler
12 prologc.dat          Test data (examples) for above
12 prologcp.lisp        Primitives for Prolog compiler

13 clos.lisp            Some object-oriented and CLOS code
13 clos.dat             Test data (examples) for above

14 krep1.lisp           Knowledge Representation code: first version 
14 krep1.dat            Test data (examples) for above
14 krep2.lisp           Knowledge Representation code with conjunctions
14 krep.lisp            Final KR code: worlds and attached functions

15 cmacsyma.lisp        Efficient Macsyma with canonical form
15 cmacsyma.dat         Test data (examples) for above

16 mycin.lisp           The Emycin expert system shell
16 mycin-r.lisp         Some rules for a medical application of emycin
16 mycin.dat            Test data (examples) for above

17 waltz.lisp           A Line-Labeling program using the Waltz algorithm
17 waltz.dat            Test data (examples) for above

18 othello.lisp         The Othello playing program and some strategies
18 othello.dat          Test data (examples) for above
18 othello2.lisp        Additional strategies for Othello
18 edge-tab.lisp        Edge table for Iago strategy

19 syntax1.lisp         Syntactic Parser
19 syntax1.dat          Test data (examples) for above
19 syntax2.lisp         Syntactic Parser with semantics
19 syntax2.dat          Test data (examples) for above
19 syntax3.lisp         Syntactic Parser with semantics and preferences
19 syntax3.dat          Test data (examples) for above

20 unifgram.lisp        Unification Parser
20 unifgram.dat         Test data (examples) for above

21 grammar.lisp         Comprehensive grammar of English
21 lexicon.lisp         Sample Lexicon of English
21 grammar.dat          Test data (examples) for above

22 interp1.lisp         Scheme interpreter, including version with macros
22 interp1.dat          Test data (examples) for above
22 interp2.lisp         A tail recurive Scheme interpreter
22 interp3.lisp         A Scheme interpreter that handles call/cc
22 interp3.dat          Test data (examples) for above

23 compile1.lisp        Simple Scheme compiler
23 compile2.lisp        Compiler with tail recursion and primitives
23 compile3.lisp        Compiler with peephole optimizer
23 compopt.lisp         Peephole optimizers for compile3.lisp
23 compile.dat          Test data (examples) for all 3 versions above

|#

;;; (position C-h C-f
;;; Describe function (default position):

;;; (position C-c M-h
;;; --> jumps directly to the Hyperspec page.

;;;; norvig.lisp                      -- 2003-05-16 07:54:59 -- pascal   ;;;;
