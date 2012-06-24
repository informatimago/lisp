;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               norvig.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             -
;;;;USER-INTERFACE:     -
;;;;DESCRIPTION
;;;;    
;;;;    Loader for Norvig's code.
;;;;
;;;;
;;;;    wget ftp://www.informatimago.com/pub/lisp/norvig-paip-pjb.tar.gz
;;;;    tar zxvf norvig-paip-pjb.tar.gz
;;;;    wget http://norvig.com/paip/paip.zip
;;;;    mkdir norvig
;;;;    cd norvig
;;;;    unzip -x ../paip.zip
;;;;    patch -p1 < ../norvig-paip-pjb.patch
;;;;    pwd
;;;;    clisp -ansi -q norc
;;;;    ;; replace /home/… in the following strings by the current
;;;;    ;; directory printed by the above pwd command.
;;;;    (setf (logical-pathname-translations "NORVIG")
;;;;          `(("NORVIG:**;*.*"   "/home/…/norvig/**/*.*")
;;;;            ("NORVIG:**;*.*.*" "/home/…/norvig/**/*.*")))
;;;;    (load #P"../norvig-paip-pjb.lisp")
;;;;
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2012-06-20 <PJB> Made some corrections, added instructions.
;;;;    2003-05-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2012
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;****************************************************************************


(defpackage "COM.NORVIG"
  (:use "COMMON-LISP")
  (:shadow "EXP"))
(in-package "COM.NORVIG")

(defparameter configuration
  '(gps-search  elisa-pm prolog-cp krep syntax-3 scheme-3 scheme-c3))


(defmacro exclusive (&rest e-b)
  (let ((etiquettes (mapcar (function car) e-b))
        (rep (gensym "REP")))
    `(let ((,rep (intersection configuration ',etiquettes)))
       (if (= 1 (length ,rep))
         (progn
           (setq ,rep (car ,rep))
           (format t "~&CONFIGURED: ~A~%" ,rep))
         (setq ,rep
               (do ((,rep nil))
                   (,rep ,rep)
                 (format *query-io* "~&Please choose what to load from ~S: "
                         ',etiquettes)
                 (setq ,rep (read *query-io*))
                 (unless (member ,rep ',etiquettes) (setq ,rep nil)))))
       (case ,rep
         ,@e-b))))




(defparameter *paip-source-files*
  (translate-logical-pathname (make-pathname
                               :host "NORVIG"
                               :directory '(:absolute)
                               :name nil
                               :type "LISP"
                               :case :common))
  "The location of the source files for this book.
  CHANGE IT TO REFLECT THE LOCATION OF THE FILES ON YOUR COMPUTER.")


(load "norvig:intro.lisp") ;; A few simple definitions
(load "norvig:simple.lisp") ;; Random sentence generator (two versions)
(load "norvig:overview.lisp") ;; 14 versions of LENGTH and other examples


(load "norvig:auxmacs.lisp") ;; A few macros; load this first.

(unless (fboundp 'common-lisp:loop)
  (load "norvig:loop.lisp")) ;; Load if your Lisp doesn't support ANSI LOOP

(load "norvig:auxfns.lisp") ;; Commonly used auxiliary functions.


(load "norvig:search.lisp") ;; Search Utility
;;     "norvig:search.dat"      ;; Test data (examples) for above

(exclusive
 (gps-simple
  (load "norvig:gps1.lisp") ;; Simple version of General Problem Solver
  ;;     "norvig:gps1.dat"      ;; Test data (examples) for above
  )
 (gps
  (load "norvig:gps1.lisp") ;; Simple version of General Problem Solver
  (load "norvig:gps.lisp") ;; Final version of General Problem Solver
  ;;     "norvig:gps.dat"       ;; Test data (examples) for above
  )
 (gps-search
  (load "norvig:gps1.lisp") ;; Simple version of General Problem Solver
  (load "norvig:gps.lisp") ;; Final version of General Problem Solver
  (load "norvig:gps-srch.lisp") ;; Version of GPS using the search utility 
  ;;     "norvig:gps-srch.dat"  ;; Test data (examples) for above
  ))



(load "norvig:patmatch.lisp") ;; Pattern Matching Utility
;;     "norvig:patmatch.dat"    ;; Test data (examples) for above


(exclusive
 (elisa-basic
  (load "norvig:eliza1.lisp") ;; Basic version of Eliza program
  )
 (elisa
  (load "norvig:eliza.lisp") ;; Eliza with more rules; different reader
  )
 (elisa-pm
  (load "norvig:eliza-pm.lisp") ;; Version of Eliza using utilities
  (load "norvig:cmacsyma.lisp") ;; Efficient Macsyma with canonical form
  ;;     "norvig:cmacsyma.dat"  ;; Test data (examples) for above
  ))


(load "norvig:eliza1.lisp")   ;; Needed by The Student Program
(load "norvig:student.lisp")  ;; The Student Program
;;     "norvig:student.dat"   ;; Test data (examples) for above

(load "norvig:macsyma.lisp") ;; The Macsyma Program
(load "norvig:macsymar.lisp") ;; Simplification & integration rules for Macsyma
;;     "norvig:macsyma.dat"   ;; Test data (examples) for above


(load "norvig:unify.lisp") ;; Unification functions

(exclusive
 (prolog1
  (load "norvig:prolog1.lisp") ;; First version of Prolog interpreter
  ;;     "norvig:prolog1.dat"   ;; Test data (examples) for above
  )
 (prolog
  (load "norvig:prolog.lisp") ;; Final version of Prolog interpreter
  ;;     "norvig:prolog.dat"    ;; Test data (examples) for above
  )
 (prolog-c1
  (load "norvig:prologc1.lisp") ;; First version of Prolog compiler
  ;;     "norvig:prologc1.dat"  ;; Test data (examples) for above
  )
 (prolog-c2
  (load "norvig:prologc2.lisp") ;; Second version of Prolog compiler
  ;;     "norvig:prologc2.dat"  ;; Test data (examples) for above
  )
 (prolog-c 
  (load "norvig:prologc.lisp") ;; Final version of Prolog compiler
  ;;     "norvig:prologc.dat"   ;; Test data (examples) for above
  )
 (prolog-cp
  ;;(LOAD "norvig:prologcp.lisp") ;; Primitives for Prolog compiler
  ;;(LOAD "norvig:unifgram.lisp") ;; Unification Parser
  ;;     "norvig:unifgram.dat"  ;; Test data (examples) for above

  ;;(LOAD "norvig:lexicon.lisp") ;; Sample Lexicon of English
  ;;     "norvig:grammar.dat"   ;; Test data (examples) for above
  (load "norvig:grammar.lisp") ;; Comprehensive grammar of English
  ))



(load "norvig:clos.lisp") ;; Some object-oriented and CLOS code
;;     "norvig:clos.dat"      ;; Test data (examples) for above

(exclusive
 (krep-1
  (load "norvig:krep1.lisp") ;; Knowledge Representation code: first version 
  ;;     "norvig:krep1.dat"   ;; Test data (examples) for above
  )
 (krep-2
  (load "norvig:krep2.lisp") ;; Knowledge Representation code w/ conjunctions
  )
 (krep
  (load "norvig:krep.lisp") ;; Final KR code: worlds and attached functions
  ))


(load "norvig:mycin.lisp") ;; The Emycin expert system shell
(load "norvig:mycin-r.lisp") ;; Some rules for a medical application of emycin
;;     "norvig:mycin.dat"     ;; Test data (examples) for above

(load "norvig:waltz.lisp") ;; A Line-Labeling program using Waltz algorithm
;;     "norvig:waltz.dat"     ;; Test data (examples) for above

;; (LOAD "norvig:othello.lisp") ;; The Othello playing program & strategies
;;     "norvig:othello.dat"   ;; Test data (examples) for above
(load "norvig:othello2.lisp") ;; Additional strategies for Othello
(load "norvig:edge-tab.lisp") ;; Edge table for Iago strategy

(exclusive
 (syntax-1
  (load "norvig:syntax1.lisp") ;; Syntactic Parser
  ;;     "norvig:syntax1.dat"   ;; Test data (examples) for above
  )
 (syntax-2
  (load "norvig:syntax2.lisp") ;; Syntactic Parser with semantics
  ;;     "norvig:syntax2.dat"   ;; Test data (examples) for above
  )
 (syntax-3
  (load "norvig:syntax3.lisp") ;; Syntactic Parser with semantics and pref.
  ;;     "norvig:syntax3.dat"   ;; Test data (examples) for above
  ))



(exclusive
 (scheme-1
  (load "norvig:interp1.lisp") ;; Scheme interpreter, incl. version with macros
  ;;     "norvig:interp1.dat"   ;; Test data (examples) for above
  )
 (scheme-2
  (load "norvig:interp1.lisp") ;; Scheme interpreter, incl. version with macros
  (load "norvig:interp2.lisp") ;; A tail recurive Scheme interpreter
  )
 (scheme-3
  (load "norvig:interp1.lisp") ;; Scheme interpreter, incl. version with macros
  (load "norvig:interp3.lisp") ;; A Scheme interpreter that handles call/cc
  ;;     "norvig:interp3.dat"   ;; Test data (examples) for above
  ))

(exclusive
 (scheme-c1
  (load "norvig:compile1.lisp") ;; Simple Scheme compiler
  )
 (scheme-c2
  (load "norvig:compile2.lisp") ;; Compiler with tail recursion and primitives
  )
 (scheme-c3
  (load "norvig:compile3.lisp") ;; Compiler with peephole optimizer
  (load "norvig:compopt.lisp") ;; Peephole optimizers for compile3.lisp
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
