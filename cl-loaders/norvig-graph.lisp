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
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2003-05-16 <PJB>  Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2012
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


(load "PACKAGE:COM;INFORMATIMAGO;COMMON-LISP;GRAPH")
(load "PACKAGE:COM;INFORMATIMAGO;COMMON-LISP;GRAPH-DOT")
(load "PACKAGE:COM;INFORMATIMAGO;COMMON-LISP;LIST")
(load "PACKAGE:COM;INFORMATIMAGO;COMMON-LISP;UTILITY")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.GRAPH")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.GRAPH-DOT")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.LIST")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")

(defvar data)
(defvar g)

(setq data
      (mapcan
       (lambda (file)
         (let ((requires
                (with-open-file (in file :direction :input)
                  (let ((*readtable* (copy-readtable nil)))
                    (set-dispatch-macro-character
                     #\# #\. (lambda (&rest args) args))
                    (do* ((eof (gensym "eof"))
                          (sexp (read in nil eof) (read in nil eof))
                          (result (list)))
                        ((eq eof sexp) result)
                      (when (and (consp sexp) (eq 'requires (car sexp)))
                        (setq result (nconc (cdr sexp) result ))))))  ))
           (when requires
             (list (cons
                    (let* ((name (file-namestring file))
                           (posi (search ".lisp" name)))
                      (if posi (subseq name 0 posi) name))
                    requires)))
           ))
       (directory "NORVIG:*.LISP")))
(setq g (make-instance 'graph-class))
(set-property g :name "NORVIG")
(add-nodes  g (mapcar
               (lambda (name) (let ((node (make-instance 'element-class)))
                                (set-property node :name name)
                                node))
               (delete-duplicates (flatten data) :test (function string=))))
(mapc
 (lambda (arcs)
   (let* ((from (car arcs))
          (from-node (car (find-nodes-with-property g :name from))))
     (mapc
      (lambda (to)
        (let ((to-node (car (find-nodes-with-property g :name to))))
          (add-edge-between-nodes g from-node to-node)))
      (cdr arcs))))
 data)

(let ((fname "norvig"))
  (with-open-file (out (format nil "~A.dot" fname) :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
    (princ (generate-dot g) out))
  (ext:shell (format nil "n=~A ; (dot -Tps ${n}.dot -o ${n}.ps;gv ${n}.ps)&"
                     fname))
;;;   (EXT:SHELL (FORMAT NIL "n=~A ; (tred ${n}.dot > ${n}-tred.dot ;~
;;;                           dot -Tps ${n}-tred.dot -o ${n}-tred.ps ;~
;;;                           gv ${n}-tred.ps) & " FNAME))
  )





;; Give a list of conflicts, symbol defineds in two files.
(mapcon
 (lambda (left-rest)
   (let ((left (car left-rest)))
     (mapcan (lambda (right)
               ;; (FORMAT T "~2%LEFT = ~S~%RIGHT= ~S~%" (CDR LEFT) (CDR RIGHT))
               (let ((res (intersection (cdr left) (cdr right)
                                        :test (function string-equal))))
                 (if res (list (cons (car left) (cons (car right) res))) nil)))
             (cdr left-rest))))
 (remove-if
  (lambda (l) (= 1 (length l)))
  (mapcar
   (lambda (file)
     (cons file
           (mapcar
            (lambda (item)
              (cond
               ((symbolp (second item))
                (second item))
               ((and (consp   (second item))
                     (symbolp (car (second item))))
                (car (second item)))
               (t nil)))
            (with-open-file (in file :direction :input)
              (let ((*readtable* (copy-readtable nil)))
                (set-dispatch-macro-character
                 #\# #\. (lambda (&rest args) args))
                (do* ((eof (gensym "eof"))
                      (sexp (read in nil eof) (read in nil eof))
                      (result ()))
                    ((eq eof sexp) result)
                  (when (and (consp sexp)
                             (< 3 (length (string (car sexp))))
                             (string-equal
                              "DEF" (subseq (string (car sexp)) 0 3)))
                    (push sexp result))))))))
   (directory "NORVIG:*.LISP"))))



 
(  

 ("eliza.lisp" "intro.lisp" mappend)

 ("eliza.lisp" "eliza1.lisp" *eliza-rules* mappend eliza)

 ("eliza.lisp" "eliza-pm.lisp" eliza)

 ("eliza.lisp" "unifgram.lisp" punctuation-p)

 ("eliza.lisp" "auxfns.lisp" mappend)

 ("prolog.lisp" "prolog1.lisp" variables-in   show-prolog-vars
  top-level-prove prove prove-all ?- find-anywhere-if
  unique-find-anywhere-if rename-variables clear-predicate clear-db
  add-clause <- *db-predicates* predicate get-clauses clause-body
  clause-head)

 ("prolog.lisp" "krep2.lisp" show-prolog-vars top-level-prove prove
  prove-all)

 ("prolog.lisp" "prologc2.lisp" args)

 ("prolog.lisp" "prologc1.lisp" args)

 ("prolog.lisp" "krep.lisp" replace-?-vars)

 ("prolog.lisp" "prologc.lisp" top-level-prove add-clause <- args)

 ("prolog.lisp" "compile3.lisp" args)

 ("intro.lisp" "eliza1.lisp" mappend)

 ("intro.lisp"   "auxfns.lisp" mappend)

 ("search.lisp" "mycin.lisp" is is)

 ("search.lisp" "compile3.lisp" is is)

 ("search.lisp" "gps.lisp"   find-path)

 ("othello2.lisp" "othello.lisp" mobility all-squares)

 ("othello2.lisp" "overview.lisp" node)

 ("simple.lisp" "lexicon.lisp"   verb noun)

 ("simple.lisp" "eliza1.lisp" random-elt)

 ("simple.lisp"   "syntax3.lisp" *grammar*)

 ("simple.lisp" "syntax2.lisp" *grammar*)

 ("simple.lisp" "syntax1.lisp" *grammar*)

 ("simple.lisp"   "auxfns.lisp" random-elt)

 ("compopt.lisp" "mycin-r.lisp" nil)

 ("eliza1.lisp" "eliza-pm.lisp" use-eliza-rules eliza)

 ("eliza1.lisp"   "patmatch.lisp" segment-match segment-match
  segment-pattern-p   pat-match extend-bindings match-variable
  pat-match extend-bindings   lookup binding-val get-binding fail
  variable-p)

 ("eliza1.lisp"   "auxfns.lisp" random-elt mappend mklist flatten
  pat-match   extend-bindings match-variable pat-match extend-bindings
  lookup   get-binding fail variable-p)

 ("eliza1.lisp" "cmacsyma.lisp"   variable-p)

 ("eliza1.lisp" "macsyma.lisp" variable-p)

 ("syntax3.lisp" "syntax2.lisp" integers 10*n+d infix-funcall
  extend-parse parse terminal-tree-p apply-semantics lexical-rules
  *open-categories* parser append1 complete-parses first-or-nil
  rules-starting-with lexical-rules parse-lhs parse use tree rule
  *grammar*)

 ("syntax3.lisp" "syntax1.lisp" extend-parse parse   lexical-rules
  *open-categories* parser append1 complete-parses
  rules-starting-with lexical-rules parse-lhs parse use rule
  *grammar*)

 ("syntax3.lisp" "mycin.lisp" rule)

 ("syntax3.lisp"   "loop.lisp" sum repeat)

 ("syntax3.lisp" "unifgram.lisp" rule)

 ("syntax3.lisp" "student.lisp" rule)

 ("syntax3.lisp" "auxfns.lisp"   first-or-nil)

 ("syntax3.lisp" "cmacsyma.lisp" rule)

 ("syntax3.lisp"   "macsyma.lisp" rule)

 ("syntax3.lisp" "compile3.lisp" arg2)

 ("syntax3.lisp" "gps.lisp" use)

 ("syntax2.lisp" "syntax1.lisp"   extend-parse parse *open-categories*
  use parser append1   complete-parses rules-starting-with
  lexical-rules parse-lhs parse   rule *grammar*)

 ("syntax2.lisp" "mycin.lisp" rule)

 ("syntax2.lisp"   "unifgram.lisp" rule)

 ("syntax2.lisp" "student.lisp" rule)

 ("syntax2.lisp" "auxfns.lisp" first-or-nil)

 ("syntax2.lisp"   "cmacsyma.lisp" rule)

 ("syntax2.lisp" "macsyma.lisp" rule)

 ("syntax2.lisp" "gps.lisp" use)

 ("syntax1.lisp" "mycin.lisp" rule)

 ("syntax1.lisp" "unifgram.lisp" rule)

 ("syntax1.lisp" "student.lisp"   rule)

 ("syntax1.lisp" "cmacsyma.lisp" rule)

 ("syntax1.lisp"   "macsyma.lisp" rule)

 ("syntax1.lisp" "gps.lisp" use)

 ("prolog1.lisp"   "krep2.lisp" show-prolog-vars top-level-prove
  prove-all prove)

 ("prolog1.lisp" "prologc.lisp" top-level-prove add-clause <-)

 ("mycin.lisp" "unifgram.lisp" rule)

 ("mycin.lisp" "overview.lisp"   true)

 ("mycin.lisp" "student.lisp" rule)

 ("mycin.lisp"   "cmacsyma.lisp" rule)

 ("mycin.lisp" "macsyma.lisp" rule)

 ("mycin.lisp" "compile3.lisp" is)

 ("loop.lisp" "overview.lisp"   while)

 ("patmatch.lisp" "auxfns.lisp" match-variable extend-bindings
  lookup get-binding variable-p fail pat-match)

 ("patmatch.lisp"   "cmacsyma.lisp" variable-p)

 ("patmatch.lisp" "macsyma.lisp"   variable-p)

 ("unifgram.lisp" "student.lisp" rule)

 ("unifgram.lisp"   "cmacsyma.lisp" rule)

 ("unifgram.lisp" "macsyma.lisp" rule)

 ("krep2.lisp" "krep1.lisp" retrieve mapc-retrieve index)

 ("krep2.lisp" "krep.lisp" add-fact index)

 ("krep2.lisp"   "prologc.lisp" top-level-prove)

 ("krep1.lisp" "krep.lisp"   dtree-index index)

 ("prologc2.lisp" "prologc1.lisp" compile-clause   compile-predicate
  proper-listp has-variable-p compile-arg   compile-unify =
  def-prolog-compiler-macro prolog-compiler-macro   compile-call
  compile-body make-= make-predicate make-parameters args
  relation-arity clauses-with-arity prolog-compile var *var-counter*
  undo-bindings!  set-binding! *trail* print-var set-binding! unify!
  deref bound-p var unbound)

 ("prologc2.lisp" "prologc.lisp"   bind-unbound-vars
  maybe-add-undo-bindings compile-clause   compile-predicate
  proper-listp has-variable-p compile-arg   compile-unify =
  def-prolog-compiler-macro prolog-compiler-macro   compile-call
  compile-body make-= make-predicate make-parameters args
  relation-arity clauses-with-arity prolog-compile var *var-counter*
  undo-bindings! set-binding! *trail* print-var set-binding! unify!
  deref bound-p var unbound)

 ("prologc2.lisp" "compile3.lisp" args)

 ("prologc1.lisp" "prologc.lisp" proper-listp has-variable-p
  compile-arg compile-unify = def-prolog-compiler-macro
  prolog-compiler-macro compile-call compile-body make-=
  compile-clause make-predicate make-parameters compile-predicate args
  relation-arity clauses-with-arity prolog-compile var *var-counter*
  undo-bindings! set-binding! *trail* print-var set-binding! unify!
  deref bound-p var unbound)

 ("prologc1.lisp" "compile3.lisp" args)

 ("overview.lisp" "auxfns.lisp" find-all)

 ("auxmacs.lisp"   "macsyma.lisp" find-anywhere)

 ("auxmacs.lisp" "gps.lisp"   starts-with)

 ("student.lisp" "cmacsyma.lisp" prefix->infix   binary-exp-p exp-args
  exp-p exp rule)

 ("student.lisp" "macsyma.lisp"   prefix->infix binary-exp-p exp-args
  exp-p exp rule)

 ("auxfns.lisp"   "interp1.lisp" delay delay)

 ("auxfns.lisp" "cmacsyma.lisp"   variable-p)

 ("auxfns.lisp" "macsyma.lisp" variable-p partition-if)

 ("auxfns.lisp" "gps.lisp" member-equal)

 ("prologc.lisp"   "compile3.lisp" args)

 ("gps1.lisp" "gps.lisp" apply-op appropriate-p   achieve gps op
  *ops*)

 ("interp3.lisp" "interp2.lisp" interp)

 ("interp3.lisp" "interp1.lisp" init-scheme-proc scheme interp)

 ("interp3.lisp" "compile3.lisp" scheme)

 ("interp2.lisp"   "interp1.lisp" interp)

 ("interp1.lisp" "compile3.lisp" scheme)

 ("interp1.lisp" "compile1.lisp" define)

 ("cmacsyma.lisp"   "macsyma.lisp" prefix->infix *infix->prefix-rules*
  variable-p   infix->prefix binary-exp-p exp-args exp-p exp rule)

 ("compile3.lisp"   "compile2.lisp" *primitive-fns* optymize
  init-scheme-comp assemble)

 ("compile3.lisp" "compile1.lisp" show-fn)

 ("compile2.lisp"   "compile1.lisp" comp-lambda gen-set comp-if
  comp-begin comp)

 )



;;;; norvig-graph.lisp                -- 2003-05-16 07:11:44 -- pascal   ;;;;
