;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rdp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a simple recursive descent parser.
;;;;    
;;;;    http://en.wikipedia.org/wiki/Formal_grammar
;;;;    http://en.wikipedia.org/wiki/Recursive_descent_parser
;;;;    http://en.wikipedia.org/wiki/Parsing_expression_grammar
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-24 <PJB> Upgraded for use in com.informatimago.lse;
;;;;                     Changed to AGPL3 license.
;;;;    2011-01-12 <PJB> Added grammar parameter to functions
;;;;                     generating function names so that different
;;;;                     grammars with non-terminals named the same
;;;;                     don't collide.
;;;;    2006-09-09 <PJB> Created
;;;;BUGS
;;;;
;;;;    The First set of a non-terminal that can reduce to the empty string
;;;;    should include the Follow set of this non-terminal, but for this,
;;;;    we'd have to normalize the grammar rules, and then generate parsing
;;;;    functions that don't correspond directly to the source rules.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2006 - 2015
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
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.RDP")

(defstruct (grammar
            (:print-function
             (cl:lambda (object stream depth)
               (declare (ignore depth))
               (print-unreadable-object (object stream :type t :identity t)
                 (format stream "~A" (grammar-name object))))))
  name
  terminals          ; 
  start              ; non-terminal
  rules              ; list of (--> non-terminal [ rhs [:action . body])
  all-terminals      ; union of the terminals found in the rules and declared in terminals
  all-non-terminals
  %synthesized-non-terminals
  ;; ---
  (scanner     t)
  (skip-spaces t)
  ;; --- computed lazily:
  %normalized-grammar
  %firsts-sets
  %follow-sets)

(defstruct (normalized-grammar
            (:include grammar)
            (:constructor %make-normalized-grammar))
  %original-grammar)

(defgeneric grammar-constructor (grammar)
  (:method ((grammar grammar))            'make-grammar)
  (:method ((grammar normalized-grammar)) 'make-normalized-grammar))

(defgeneric grammar-normalized-grammar (grammar)
  (:method ((grammar grammar))
    (or (grammar-%normalized-grammar grammar)
        (setf (grammar-%normalized-grammar grammar) (normalize-grammar grammar))))
  (:method ((grammar normalized-grammar))
    grammar))

(defgeneric grammar-original-grammar (grammar)
  (:method ((grammar grammar))            grammar)
  (:method ((grammar normalized-grammar)) (normalized-grammar-%original-grammar grammar)))

(defun compute-first-follow (grammar)
  (setf (grammar-%firsts-sets grammar) (compute-firsts-sets grammar)
        (grammar-%follow-sets grammar) (compute-follow-sets grammar)))

(defgeneric grammar-firsts-sets (grammar)
  (:method ((grammar grammar))
    (grammar-firsts-sets (grammar-normalized-grammar grammar)))
  (:method ((grammar normalized-grammar))
    (unless (grammar-%firsts-sets grammar)
      (compute-first-follow grammar))
    (grammar-%firsts-sets grammar)))

(defgeneric grammar-follow-sets (grammar)
  (:method ((grammar grammar))
    (grammar-follow-sets (grammar-normalized-grammar grammar)))
  (:method ((grammar normalized-grammar))
    (unless (grammar-%follow-sets grammar)
      (compute-first-follow grammar))
    (grammar-%follow-sets grammar)))

(defgeneric dump-grammar (grammar)
  (:method ((grammar grammar))
    (format t "(~A~%"                     (grammar-constructor grammar))
    (format t "  :name ~S~%"              (grammar-name grammar))
    (format t "  :terminals ~S~%"         (grammar-terminals grammar))
    (format t "  :start ~S~%"             (grammar-start grammar))
    (format t "  :rules ~S~%"             (grammar-rules grammar))
    (format t "  :all-terminals ~S~%"     (grammar-all-terminals grammar))
    (format t "  :all-non-terminals ~S~%" (grammar-all-non-terminals grammar))
    (format t "  :scanner ~S~%"           (grammar-scanner grammar))
    (format t "  :skip-spaces ~S~%"       (grammar-skip-spaces grammar))
    (format t "  :firsts-sets ~S~%"       (grammar-firsts-sets grammar))
    (format t "  :follow-sets ~S~%"       (grammar-follow-sets grammar))
    (format t ")~%")
    grammar))



(defvar *grammars* (make-hash-table)
  "Records the variables defined with DEFGRAMMAR.
Use (GRAMMAR-NAMED name) to look up a grammar.")


(defun grammar-named (name)
  "Returns the grammar named NAME, or NIL if none."
  (gethash name *grammars*))

(defun register-grammar (grammar)
  (setf (gethash (grammar-name grammar) *grammars*) grammar))


#-sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation 'seq t) "

Grammar rule syntax:

    (SEQ term... [:action form...])

Parses if each of the term parse in sequence, once.

In the scope of the action, the terms are bound to variables named $1
$2 to $n, and for non-terminals, to variables named by the
non-terminal (suffixed by .1, .2, etc) if several occurence of the
same non-terminal are present in the sequence.

The default action returns all the parsed terms in a list.

"
        (documentation 'req t) "

Grammar rule syntax:

    (REQ term... [:action form...])

is equivalent to:

    (REQ (SEQ term... [:action form...]))

Parses if each of the term parse in sequence, 0 or more times.

Returns a list of the parsed sequences.

"
        (documentation 'opt t) "

Grammar rule syntax:

    (OPT term... [:action form...])

is equivalent to:

    (OPT (SEQ term... [:action form...]))

Parses if each of the term parse in sequence, 0 or 1 time.

Returns NIL or the the parsed sequence.

"
        (documentation 'alt t) "

Grammar rule syntax:

    (ALT term...)

Parses one of the terms (the first set of all the terms must be disjoint).

Returns parsed term.

"))


(defgeneric generate-boilerplate (target-language grammar &key trace)
  (:documentation "Generate the boilerplate code needed by the scanner and parser.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate.")
  (:method (target-language grammar &key trace)
    (declare (ignore target-language grammar trace))
    nil))


(defgeneric generate-scanner-for-grammar     (target-language grammar &key trace)
  (:documentation "Generate the scanner code,
when (grammar-scanner grammar) is T.  (grammar-scanner grammar) may
be the class-name of a scanner to use.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate."))


(defgeneric generate-parser      (target-language grammar &key trace)
  (:documentation "Generate the toplevel parser code.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate."))


(defgeneric generate-non-terminal-parser-function   (target-language grammar non-terminal &key trace)
  (:documentation "Generate the parser code for the given non-terminal.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate."))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generate the defgrammar expansion:
;;;

(defvar *linenum* 0)

(defun generate-grammar (name &key
                                terminals (scanner t) (skip-spaces t)
                                start rules
                                (target-language :lisp) (trace nil))
  "
SEE ALSO:   The docstring of DEFGRAMMAR.
RETURN:     A form that defines the grammar object and its parser functions.
"
  (let* ((clean-rules (clean-rules rules))
         (grammar     (make-normalized-grammar
                       :name name
                       :terminals terminals
                       :start start
                       :rules clean-rules
                       :scanner scanner
                       :skip-spaces skip-spaces))
         (*linenum* 0))
    (setf (grammar-name grammar) name)
    (register-grammar grammar)
    
    `(progn

       (register-grammar
        (make-normalized-grammar
         :name ',name
         :terminals ',terminals
         :start ',start
         :rules ',clean-rules
         :scanner ',scanner
         :skip-spaces ',skip-spaces))
       
       ,(generate-boilerplate          target-language grammar :trace trace)         
       ,(generate-scanner-for-grammar  target-language grammar :trace trace)

       ,@(mapcar (lambda (non-terminal)
                   (generate-non-terminal-parser-function target-language grammar non-terminal :trace trace))
                 (grammar-all-non-terminals grammar))
       
       ,(generate-parser target-language grammar :trace trace)
       ',name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities
;;;

(defun scat (&rest string-designators)
  "Interns the concatenation of the STRING-DESIGNATORS."
  (intern (apply (function concatenate) 'string
                 (mapcar (function string) string-designators))))
(defun dollar (n)
  "Interns a $-symbol number N."
  (scat "$" (prin1-to-string n)))

(defstruct (rhs (:type list))
  operator body actions)

(defparameter *empty-rhs* '(seq () ('()))
  "Represents ε.")

(defun empty-rhs-p (rhs)
  "Predicate for ε.
NOTE: we may have different empty RHS with actions."
  (and (listp rhs)
       (eql 'seq (rhs-operator rhs))
       (null (rhs-body rhs))))

(defun singleton-sequence-rhs-p (rhs)
  ;; (seq (postfix-expression-item) (action…))
  (and (listp rhs)
       (eql 'seq (rhs-operator rhs))
       (listp (rhs-body rhs))
       (= 1 (length (rhs-body rhs)))))

(defun right-recursive-rhs-p (non-terminal rhs)
  (and (listp rhs)
       (eql 'seq (rhs-operator rhs))
       (eql non-terminal (first (last (rhs-body rhs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleaning the grammar rules:
;;;
;;;
;;; When stored inside the grammar structure, rules must be of this
;;; cleaned form:
;;; 
;;; rule    := (<lhs> <rhs>) .
;;; lhs     := <non-terminal> .
;;; rhs     := (seq <items> <actions>)
;;;          | (rep <items>)
;;;          | (opt <items>)
;;;          | (alt <items>) .
;;; items   := ( <word>* ) .
;;; word    := <rhs> | <terminal> | <non-terminal> .
;;; actions := ( <form>* ) .
;;;
;;; - a SEQ rhs may have 0 or more items in its <items> list.
;;; - a REP rhs should have exactly 1 SEQ item in its <items> list.
;;; - a OPT rhs should have exactly 1 SEQ item in its <items> list.
;;; - a ALT rhs should have 1 or more items in its <items> list.
;;;
;;; ε is represented as (seq () ('nil))
;;; ε is not expected from the user rules, but is generated in
;;; normalized grammars.
;;;
;;;
;;; FIND-RHSES returns a list of <rhs>s.
;;; FIND-RHS   returns a <rhs>.
;;; Notice: if there are several rules for the same non-terminal,
;;; find-RHS returns a single ALT rhs with all the rhs of the rules of
;;; that non-terminal.
;;;


(defun split-action (rhs)
  "
RHS:    A rule right-hand-side, of the form: item* [:action action*].
RETURN: The sentence; the action forms.
"
  (declare (inline))
  (let ((separator (position :action rhs)))
    (if separator
        (values (subseq rhs 0 separator) (subseq rhs (1+ separator)))
        (values rhs                      nil))))

(defun clean-seq (expr)
  "RETURN: A cleaned version of the SEQ EXPR."
  (multiple-value-bind (rhs actions) (split-action (cdr expr))
    (let ((actions (or actions `(,(dollar 0))))
          (items    (mapcar (lambda (item) (clean item)) rhs)))
      (if (and (null actions) (or (null items) (null (cdr items))))
          (car items)
          (make-rhs :operator 'seq :body items :actions actions)))))

(defun clean-with-action (expr)
  "RETURN: A cleaned version of the the EXPR which must be either an OPT or a REP expression."
  (multiple-value-bind (rhs actions) (split-action (cdr expr))
    (if (null actions)
        (if (null rhs)
            *empty-rhs*
            (list (car expr) (list (clean-seq `(seq ,@rhs)))))
        (list (car expr) (list (clean-seq `(seq ,@rhs :action ,@actions)))))))

(defun clean-rep (expr) (clean-with-action expr))
(defun clean-opt (expr) (clean-with-action expr))

(defun clean-alt (expr)
  "RETURN: A cleaned version of the ALT EXPR."
  (assert (not (find :action expr))
          () "An ALT rule cannot have :ACTION~%Erroneous rule: ~S" expr)
  (let ((items (mapcar (function clean) (cdr expr))))
    (if (null (cdr items))
        (car items)
        `(alt ,(mapcan (lambda (item)
                         (if (and (consp item) (eql 'alt (car item)))
                             (second item)
                             (list item)))
                       items)))))

(defun clean (expr)
  "RETURN: A cleanred version of the expression EXPR,
which can be a terminal, a non-terminal or a SEQ, a REP, a OPT or an ALT expression."
  (if (atom expr)
      expr
      (ecase (car expr)
        ((seq) (clean-seq expr))
        ((rep) (clean-rep expr))
        ((alt) (clean-alt expr))
        ((opt) (clean-opt expr)))))


(defun clean-rules (rules)
  "
RULES:  a list of (--> non-terminal . rhs) rules.
RETURN: an equivalent, cleaned list of rules.
"
  (mapcar (lambda (rule)
            (destructuring-bind (--> non-terminal &rest items) rule
              (assert (string= --> '-->)
                      () "Rules should be written as (--> <non-terminal> <rhs> [:action <form>...])~%~
                          Invalid rule: ~S" rule)
              `(,non-terminal ,(clean
                                (cond
                                  ((find :action items)
                                   `(seq ,@items))
                                  ((and (= 1 (length items))
                                        (listp (first items)))
                                   (first items))
                                  (t
                                   `(seq ,@items :action `(,',non-terminal ,@,(dollar 0)))))))))
          rules))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric compute-all-terminals (grammar))
(defmethod compute-all-terminals ((grammar grammar))
  (labels  ((find-strings (items)
              (cond
                ((stringp items) (list items))
                ((atom items)    '())
                (t (mapcan (function find-strings) (second items))))))
    (setf (grammar-all-terminals grammar)
          (delete-duplicates
           (nconc
            (mapcar (function first) (grammar-terminals grammar))
            (mapcan (function find-strings)
                    (mapcar (function second) (grammar-rules grammar))))
           :test (function word-equal)))))


(defgeneric compute-all-non-terminals (grammar))
(defmethod compute-all-non-terminals ((grammar grammar))
  (labels ((find-symbols (items)
             (cond
               ((symbolp items) (list items))
               ((atom items)    '())
               (t (mapcan (function find-symbols) (second items))))))
    (setf (grammar-all-non-terminals grammar)
          (nset-difference
           (delete-duplicates
            (nconc
             (list (grammar-start grammar))
             (mapcar (function first) (grammar-rules grammar))
             (mapcan (function find-symbols)
                     (mapcar (function second) (grammar-rules grammar)))))
           (grammar-all-terminals grammar)))))


(defgeneric terminalp (grammar item))
(defmethod terminalp (grammar item)
  "
RETURN: whether ITEM is a terminal in the GRAMMAR.
"
  (member item (grammar-all-terminals grammar)
          :test (function word-equal)))


(defgeneric non-terminal-p (grammar item))
(defmethod non-terminal-p (grammar item)
  "
RETURN: whether ITEM is a non-terminal symbol in the GRAMMAR.
"  (member item (grammar-all-non-terminals grammar)))


(defgeneric find-rhses (grammar non-terminal))
(defmethod find-rhses (grammar non-terminal)
  "
RETURN: all the right-and-sides of rules with NON-TERMINAL as left-hand-side.
PRE:    (non-terminal-p non-terminal)
"
  (let ((rhses (mapcar (function second)
                       (remove-if-not (lambda (rule) (eql non-terminal (first rule)))
                                      (grammar-rules grammar)))))
    (if (null rhses)
        (error "~s is not a non-terminal in the grammar ~A"
               non-terminal (grammar-name grammar))
        rhses)))


(defgeneric find-rhs (grammar non-terminal))
(defmethod find-rhs (grammar non-terminal)
  "
RETURN: the right-hand-sides with NON-TERMINAL as left-hand-side as a
        single ALT expression (or just the right-hand-side if there's
        only one).
PRE:    (non-terminal-p non-terminal)
"
  (let ((rhses (find-rhses grammar non-terminal)))
    (if (null (cdr rhses))
        (car rhses)
        `(alt ,rhses))))


;;; To implement the follow-set function we need to put the grammar
;;; into a normal form.

(defgeneric nullablep (grammar rhs))
(defgeneric compute-firsts-sets (grammar))
(defgeneric compute-follow-sets (grammar))


(defmethod compute-firsts-sets ((grammar normalized-grammar))
  "  
DO:     Signals an error if there are duplicates in the first set of a non-terminal.
RETURN: A hash-table containing the firsts-set for each symbol of the
        grammar.  (terminals and non terminals).
"
  (let ((firsts-sets (make-hash-table :test (function equal))))
    (labels ((firsts-set (symbol)
               (let ((entry (gethash symbol firsts-sets)))
                 (cond (entry
                        (if (eq :error entry)
                            (error "There's a left recursion involving the symbol ~S in the grammar ~A"
                                   symbol (grammar-name grammar))
                            entry))
                       ((terminalp grammar symbol)
                        (setf (gethash symbol firsts-sets)
                              (list symbol)))
                       ((compute-firsts-set symbol)))))
             (compute-firsts-set (non-terminal)
               (setf (gethash non-terminal firsts-sets) :error)
               (let ((firsts-set '()))
                 (dolist (rhs (find-rhses grammar non-terminal))
                   (destructuring-bind (seq body &optional action) rhs
                     (declare (ignore seq action))
                     (if (null body)
                         (push nil firsts-set)
                         (loop
                           :with all-firsts := '()
                           :for item :in body
                           :for firsts := (firsts-set item)
                           :do      (setf all-firsts (union firsts (delete nil all-firsts)))
                           :while   (member nil firsts)
                           :finally (prependf firsts-set all-firsts)))))
                 (let ((unique-firsts-set  (remove-duplicates firsts-set :test (function equal))))
                   (assert (= (length firsts-set) (length unique-firsts-set))
                           () "There are duplicates in the first sets of the rules for the non-terminal ~S: ~S"
                           non-terminal (duplicates firsts-set))
                   (setf (gethash non-terminal firsts-sets) unique-firsts-set)))))
      (map nil (function firsts-set) (grammar-all-terminals grammar))
      (map nil (function firsts-set) (grammar-all-non-terminals grammar)))
    firsts-sets))


(defmethod nullablep ((grammar grammar) rhs)
  "
RHS:    A terminal or a non-terminal of the GRAMMAR, or a list thereof.
RETURN: Whether ε can be derived from rhs according to the GRAMMAR.
"
  (cond
    ((null  rhs)
     t)
    ((listp rhs)    
     (every (lambda (word) (nullablep grammar word)) rhs))
    ((terminalp grammar rhs)
     nil)
    ((non-terminal-p grammar rhs)
     (labels ((nullable-rhs-p (rhs)
                (if (atom rhs)
                    (nullablep grammar rhs)
                    (ecase (rhs-operator rhs)
                      ((seq) (every (lambda (item) (nullable-rhs-p item)) (rhs-body rhs)))
                      ((alt) (some  (lambda (item) (nullable-rhs-p item)) (rhs-body rhs)))
                      ((opt rep) t)))))
       (nullable-rhs-p (find-rhs grammar rhs))))))


(defvar *eof-symbol* (make-symbol "EOF")
  "The symbol used to denote the End-Of-Source in the follow-sets.")


(defmethod compute-follow-sets ((grammar normalized-grammar))
  "
RETURN: A hash-table containing the follow-set for each non-terminal
        of the grammar.
"
  (let ((base-constraints      '())
        (recursive-constraints '()))
    (flet ((firsts-set (item) (firsts-set grammar item)))
      ;; {$EOF$} ⊂ (follow-set start)
      (push `(subset (set ,*eof-symbol*) (follow-set ,(grammar-start grammar)))
            base-constraints)
      (dolist (rule (grammar-rules grammar))
        (destructuring-bind (non-terminal (seq symbols action)) rule
          (declare (ignore seq action))
          (when symbols
            (loop
              :for current :on symbols
              :for n = (car current)
              :for beta = (cdr current)
              :do (when (non-terminal-p grammar n)
                    (let ((m (firsts-set beta)))
                      (when beta
                        ;; (firsts-set beta)∖{ε} ⊂ (follow-set n)
                        (push `(subset (set ,@m) (follow-set ,n)) base-constraints))
                      (when (and (not (eql n non-terminal)) (nullablep grammar beta))
                        ;; (follow-set non-terminal) ⊂ (follow-set n)
                        (push (list non-terminal n) recursive-constraints)))))))))
    (let ((follow-sets (make-hash-table)))
      ;; initialize the follow-sets:
      (dolist (non-terminal (grammar-all-non-terminals  grammar))
        (setf (gethash non-terminal follow-sets) '()))

      ;; apply the base-constraints:
      (loop
        :for constraint :in base-constraints
        :do (destructuring-bind (subset (set &rest elements) (follow-set non-terminal)) constraint
              (declare (ignore subset set follow-set))
              (setf (gethash non-terminal follow-sets)
                    (union (gethash non-terminal follow-sets)
                           (remove nil elements)))))
      
      ;; resolve the recursive constraints:
      (solve-constraints recursive-constraints
                         (lambda (subset superset)
                           (let ((old-cardinal (length (gethash superset follow-sets))))
                             (setf (gethash superset follow-sets)
                                   (union (gethash subset   follow-sets)
                                          (gethash superset follow-sets)))
                             (/= (length (gethash superset follow-sets)) old-cardinal))))
      follow-sets)))


(defgeneric firsts-set (grammar non-terminal-or-sentence)
  (:documentation "RETURN: the firsts-set of the NON-TERMINAL-OR-SENTENCE in the GRAMMAR.")
  (:method ((grammar grammar) non-terminal-or-sentence)
    (firsts-set (grammar-normalized-grammar grammar) non-terminal-or-sentence))
  (:method ((grammar normalized-grammar) non-terminal-or-sentence)
    (let ((firsts-sets (grammar-firsts-sets grammar)))
      (labels ((firsts-set (item)
                 (cond
                   ((null item) nil)
                   ((atom item)
                    (multiple-value-bind (firsts-set presentp) (gethash item firsts-sets)
                      (if presentp
                          firsts-set
                          (error "~S is not a symbol of the grammar ~A"
                                 item (grammar-name grammar)))))
                   (t (loop
                        :with result = '()
                        :for itemus :in item
                        :for firsts-set = (firsts-set itemus)
                        :do (prependf result firsts-set)
                        :while (member nil firsts-set)
                        :finally (return (delete-duplicates result :test (function equal))))))))
        (firsts-set non-terminal-or-sentence)))))


(defgeneric follow-set (grammar non-terminal)
  (:documentation "RETURN: the follow-set of the NON-TERMINAL in the GRAMMAR.")
  (:method ((grammar grammar) non-terminal)
    (follow-set (grammar-normalized-grammar grammar) non-terminal))
  (:method ((grammar normalized-grammar) non-terminal)
    (or (gethash non-terminal (grammar-follow-sets grammar))
        (error "~S is not a non-terminal of the grammar ~A"
               non-terminal (grammar-name grammar)))))


(defgeneric rhs-firsts-set (grammar non-terminal rhs)
  (:method (grammar non-terminal rhs)
    (let ((fs (firsts-set grammar rhs)))
      (if (member nil fs)
          (remove-duplicates (union fs (follow-set grammar non-terminal)))
          fs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Normalization of the grammar.
;;;
;;;
;;; Each rule is put under the form:  a --> ε   or   a --> e a
;;;
;;; Normalized grammar rules:
;;;
;;; rule    := (<lhs> <rhs>) .
;;; lhs     := <non-terminal> .
;;; rhs     := (seq <items> <actions>) .
;;; items   := ( <word>* ) .
;;; word    := <rhs> | <terminal> | <non-terminal> .
;;; actions := ( <form>* ) .
;;; ε is represented as (seq () ('nil))
;;;

(defun make-new-non-terminal (base-nt non-terminals)
  (loop
    :for i :from 1
    :for new-nt = (scat base-nt '- (prin1-to-string i))
    :while (member new-nt non-terminals)
    :finally (return new-nt)))


(defun simplify-normalized-grammar-rules (rules non-terminals &key verbose)
  (flet ((used (nt rules)
           (loop :for (nil (nil rhs nil)) :in rules
                   :thereis (member nt rhs))))
    (loop
      :with $1action := (list (dollar 1))
      :for rule1 :in (remove-if-not (lambda (rule)
                                      (let ((rhs (second rule)))
                                        (and (singleton-sequence-rhs-p rhs)
                                             (member (first (rhs-body rhs)) non-terminals)
                                             (equalp $1action (rhs-actions rhs)))))
                                    rules)
      :for (nt1 (nil (nt2) nil)) := rule1
      :for prods := (remove nt2 rules :key (function first) :test-not (function eql))
      :for other-rules := (remove rule1 rules)
      :do (when verbose
            (format t "~&Considering rule ~S~%" rule1)
            (format t "~&  Non terminal ~S has ~D productions and is ~:[not ~;~]used elsewhere.~%" nt2 (length prods) (used nt2 other-rules)))
      :when (and (= 1 (length prods))
                 (not (used nt2 other-rules)))
        :do (let ((rule2 (first prods)))
              (setf rules (cons `(,nt1 ,(second rule2))
                                (remove rule2 other-rules))
                    non-terminals (remove nt2 non-terminals)))
            (when verbose
              (format t "~&  subtituted new rule ~S~%" (first rules)))
      :finally (return (values rules non-terminals)))))


(defun normalize-grammar-rules (rules non-terminals)
  "
DO:     Substitute any sub-expressions in the rhs with a new
        non-terminal and a new production.  Then replace the REP, OPT,
        and ALT rules with SEQ rules and new produtions.

RETURN: the new production set; the new non-terminal set
"
  (simplify-normalized-grammar-rules
   (loop
     :while rules 
     :collect (let ((rule (pop rules)))
                (destructuring-bind (nt rhs) rule
                  (labels ((new-rule (nt rhs)
                             (push nt non-terminals)
                             (push (list nt rhs) rules))
                           (process-item (item)
                             (if (listp item)
                                 (let ((new-nt (make-new-non-terminal nt non-terminals)))
                                   (new-rule new-nt item)
                                   new-nt)
                                 item)))
                    (let ((op (rhs-operator rhs)))
                      (ecase op
                        ((seq)
                         (destructuring-bind (op items actions) rhs
                           (list nt (list* op (mapcar (function process-item) items)
                                           (when actions (list actions))))))
                        ((rep)
                         ;; a --> (rep e)
                         ;; -------------
                         ;; a --> ε   :action '()
                         ;; a --> e a :action (cons $1 $2)
                         (destructuring-bind (op items) rhs
                           (declare (ignore op))
                           (assert (null (rest items)))
                           (let ((item (first items)))
                             (new-rule nt (if (singleton-sequence-rhs-p item)
                                              `(seq (,(process-item (first (second item))) ,nt)
                                                    ((cons (progn ,@(third item)) ,(dollar 2))))
                                              `(seq (,(process-item item) ,nt)
                                                    ((cons ,(dollar 1) ,(dollar 2)))))))
                           (list nt *empty-rhs*)))
                        ((opt)
                         ;; a --> (opt e)
                         ;; -------------
                         ;; a --> ε :action '()
                         ;; a --> e :action (list $1)
                         (destructuring-bind (op items) rhs
                           (declare (ignore op))
                           (assert (null (rest items)))
                           (let ((item (first items)))
                             (new-rule nt (if (singleton-sequence-rhs-p item)
                                              `(seq (,(process-item (first (second item))))
                                                    (,@(third item)))
                                              `(seq (,(process-item item))
                                                    ((list ,(dollar 1)))))))
                           (list  nt *empty-rhs*)))
                        ((alt)
                         ;; a --> (alt e₁ ... eν)
                         ;; -------------
                         ;; a --> e₁ :action $1
                         ;; ...
                         ;; a --> eν :action $1
                         (destructuring-bind (op items) rhs
                           (declare (ignore op))
                           (let ((new-items  (mapcar (function process-item) items)))
                             (dolist (new-item (rest new-items))
                               (new-rule nt (if (singleton-sequence-rhs-p new-item)
                                                `(seq (,(process-item (first (second new-item))))
                                                      ((progn ,@(third new-item))))
                                                `(seq (,new-item) (,(dollar 1))))))
                             (list nt (if (singleton-sequence-rhs-p (first new-items))
                                          `(seq (,(first (second (first new-items))))
                                                ((progn ,@(third (first new-items)))))
                                          `(seq (,(first new-items)) (,(dollar 1))))))))))))))
   non-terminals))


(defun normalized-rule-p (rule)
  (and (listp   rule)
       (=   3   (length rule))
       (eq 'seq (first  rule))
       (listp   (second rule))
       (listp   (third  rule))))

(defun normalized-rules-p (rules)
  (every (function normalized-rule-p) rules))

(defun make-normalized-grammar (&key name terminals start rules (scanner t) (skip-spaces t))
  "Return a new normalized grammar."
  (let ((new-grammar (%make-normalized-grammar
                      :name        name
                      :terminals   terminals
                      :start       start
                      :rules       rules
                      :scanner     scanner
                      :skip-spaces skip-spaces)))
    (compute-all-terminals     new-grammar)
    (compute-all-non-terminals new-grammar)
    (let ((original-non-terminals (grammar-all-non-terminals new-grammar)))
      (setf (grammar-rules new-grammar) (if (normalized-rules-p rules)
                                            rules
                                            (normalize-grammar-rules rules (grammar-all-non-terminals new-grammar))))
      (compute-all-non-terminals new-grammar)
      (setf (normalized-grammar-%synthesized-non-terminals new-grammar)
            (set-difference (grammar-all-non-terminals new-grammar)
                            original-non-terminals)))
    (compute-firsts-sets       new-grammar)
    (compute-follow-sets       new-grammar)
    new-grammar))


(defgeneric normalize-grammar (grammar)
  (:documentation "Return a new normalized grammar parsing the same language as GRAMMAR.")
  (:method ((grammar grammar))
    (let ((normalized-grammar (make-normalized-grammar
                               :name        (scat 'normalized- (grammar-name grammar)) 
                               :terminals   (grammar-terminals grammar)
                               :start       (grammar-start grammar)
                               :rules       (grammar-rules grammar)
                               :scanner     (grammar-scanner grammar)
                               :skip-spaces (grammar-skip-spaces grammar))))
      (register-grammar normalized-grammar)
      (setf (normalized-grammar-%original-grammar normalized-grammar) grammar)
      normalized-grammar))
  (:method ((grammar normalized-grammar))
    grammar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Elimination of left recursion.
;;;


;;; rule    := (<lhs> <rhs>) .
;;; lhs     := <non-terminal> .
;;; rhs     := (seq <items> <actions>) .
;;; items   := ( <word>* ) .
;;; word    := <rhs> | <terminal> | <non-terminal> .
;;; actions := ( <form>* ) .
;;; ε is represented as (seq () ('nil))


(defgeneric add-production (grammar non-terminal rhs))
(defgeneric remove-production (grammar non-terminal rhs))
(defgeneric eliminate-left-recursion (grammar))
(defgeneric eliminate-left-recursive-p (grammar))

(defmethod add-production ((grammar grammar) non-terminal rhs)
  (assert (not (terminalp grammar non-terminal)))
  (push (list non-terminal rhs) (grammar-rules grammar))
  (pushnew non-terminal (grammar-all-non-terminals grammar)))

(defmethod remove-production ((grammar grammar) non-terminal rhs)
  (setf (grammar-rules grammar) (remove-if (lambda (rule)
                                             (and (eql non-terminal (first  rule))
                                                  (eql rhs          (second rule))))
                                           (grammar-rules grammar))))

(defmethod eliminate-left-recursion ((grammar normalized-grammar))
  (loop
    :with non-terminals := (grammar-all-non-terminals grammar)
    :with nts  := (coerce non-terminals 'vector)
    :for i :below (length nts)
    :for nt1 := (aref nts i)
    :for prods := (find-rhses grammar nt1)
    :do (loop :for j :below i
              :for nt2 := (aref nts j)
              :do (loop :for prod1 :in (remove nt2 prods
                                               :key (lambda (rhs) (first (rhs-body rhs)))
                                               :test-not (function eq))
                        :for gamma := (rest (rhs-body prod1))
                        :do (remove-production grammar nt1 prod1)
                            (loop :for prod2 :in (find-rhses grammar nt2)
                                  :do (add-production grammar nt1 `(seq (,prod2 ,@gamma) ,(rhs-actions prod1))))))
        (loop
          :with nt1* := (make-new-non-terminal nt1 non-terminals)
          :for prod1 :in (find-rhses grammar nt1)
          :do (push nt1* non-terminals)
              (remove-production grammar nt1 prod1)
              (add-production grammar nt1* *empty-rhs*)
              (if (eq nt1 (first (rhs-body prod1)))
                  (add-production grammar nt1* `(seq ,(append (rest (rhs-body prod1)) (list nt1*))
                                                     (#:TODO-action???)))
                  (add-production grammar nt1 `(seq ,(append (rhs-body prod1) (list nt1*))
                                                    (#:TODO-action???)))))
    :finally (compute-all-non-terminals grammar))
  grammar)

(defmethod eliminate-left-recursive-p ((grammar normalized-grammar))
  (error "Not implemented yet."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generator -- LISP
;;;

(defgeneric gen-in-firsts               (target firsts))
(defgeneric gen-scanner-function-name   (target grammar))
(defgeneric gen-scanner-class-name      (target grammar))
(defgeneric gen-parse-function-name     (target grammar non-terminal))
(defgeneric generate-parsing-expression (target grammar non-terminal item))
(defgeneric generate-parsing-sequence (target grammar non-terminal rhs))
(defgeneric generate-non-terminal-parsing-expression (target grammar non-terminal))

;;;------------------------------------------------------------
;;; Scanner generator
;;;------------------------------------------------------------

(defmethod gen-scanner-function-name ((target (eql :lisp)) (grammar grammar))
  (scat "SCAN-" (grammar-name grammar)))

(defmethod gen-scanner-class-name ((target (eql :lisp)) (grammar grammar))
  (scat (grammar-name grammar) "-SCANNER"))

(defun gen-trace (fname form trace)
  (if trace
      `(progn
         ,form
         (trace ,fname))
      form))

(defun tracep (keyword trace)
  (or (eql keyword trace)
      (and (listp trace) (member keyword trace))))


(defparameter *spaces*
  (coerce '(#\space #\newline #\tab) 'string))

(defparameter *alphanumerics*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

(defmethod generate-scanner-for-grammar ((target (eql :lisp)) grammar &key (trace nil))
  (case (grammar-scanner grammar)
    ((t)
     (let* ((scanner-class-name (gen-scanner-class-name target grammar))
            (terminals          (mapcar (lambda (terminal)
                                          (etypecase terminal
                                            (string terminal)
                                            (symbol (let ((entry (find terminal (grammar-terminals grammar)
                                                                       :key (function first))))
                                                      (if entry
                                                          entry
                                                          (error "Undefined terminal ~S" terminal))))))
                                        (grammar-all-terminals grammar)))
            (form               (generate-scanner scanner-class-name
                                                  'buffered-scanner
                                                  terminals
                                                  (grammar-skip-spaces grammar)
                                                  *alphanumerics*
                                                  *spaces*)))
       (setf (grammar-scanner grammar) scanner-class-name)
       `(progn
          ,form
          (setf (grammar-scanner (grammar-named ',(grammar-name grammar))) ',scanner-class-name)
          ,@(when trace
              `((trace scan-next-token)))
          ',scanner-class-name)))
    (otherwise
     ;; Don't do anything
     `',(grammar-scanner grammar))))



;;;------------------------------------------------------------
;;; RDP Parser Generator
;;;------------------------------------------------------------

(defvar *non-terminal-stack* '()
  "For error reporting.")

(defmacro with-non-terminal ((non-terminal scanner) &body body)
  (declare (ignorable scanner))
  `(let ((*non-terminal-stack* (cons ',non-terminal *non-terminal-stack*)))
     ;; (print (list :token (scanner-current-token ,scanner)
     ;;              :non-terminals *non-terminal-stack*))
     ,@body))

(defun error-unexpected-token (scanner expected-tokens production)
  (restart-case
      (error 'unexpected-token-error
             :file    (scanner-file scanner)
             :line    (scanner-line scanner)
             :column  (scanner-column scanner)
             :state   (scanner-state  scanner)
             ;; :grammar (grammar-named ',(grammar-name grammar))
             :scanner scanner
             :non-terminal-stack (copy-list *non-terminal-stack*)
             :expected-token expected-tokens
             :format-control "Unexpected token ~A (~S)~@[~%Expected ~A~]~%~S~@[~%~{~A --> ~S~}~]"
             :format-arguments (list
                                (scanner-current-token scanner)
                                (scanner-current-text scanner)
                                expected-tokens
                                *non-terminal-stack*
                                production))
    (skip-token-and-continue ()
      :report (lambda (stream)
                (format stream "Skip token ~:[~A ~A~;~*<~A>~], and continue"
                        (string= (scanner-current-token scanner)
                                 (scanner-current-text scanner))
                        (scanner-current-token scanner)
                        (scanner-current-text scanner)))
      (scan-next-token scanner)
      (return-from error-unexpected-token :continue))
    (skip-tokens-until-expected-and-retry ()
      :report (lambda (stream)
                (format stream "Skip tokens until one expected ~A is found, and retry"
                        expected-tokens))
      (loop
        :for token := (scan-next-token scanner)
        :while (and token (if (listp expected-tokens)
                              (member token expected-tokens
                                      :test (function word-equal))
                              (word-equal token expected-tokens))))
      (return-from error-unexpected-token :retry))))


(defmethod gen-parse-function-name ((target (eql :lisp)) (grammar grammar) non-terminal)
  (scat (grammar-name grammar) "/PARSE-" non-terminal))

(defmethod gen-in-firsts ((target (eql :lisp)) firsts)
  (if (null (cdr firsts))
      `(word-equal (scanner-current-token scanner) ',(car firsts))
      `(member (scanner-current-token scanner) ',firsts
               :test (function word-equal))))

(defmacro retrying-until (until result expected-tokens production)
  `(loop
     :until ,until
     :do (ecase (error-unexpected-token scanner ',expected-tokens ',production)
           (:retry)
           (:continue (loop-finish)))
     :finally (return ,result)))



(defmethod generate-non-terminal-parser-function ((target (eql :lisp)) (grammar grammar) non-terminal &key (trace nil))
  (let* ((fname (gen-parse-function-name target grammar non-terminal))
         (form  `(defun ,fname (scanner)
                   ,(format nil "~S" `(--> ,non-terminal ,(find-rhs grammar non-terminal)))
                   ;; ,(format nil "~S" (assoc non-terminal (grammar-rules grammar)))
                   (with-non-terminal (,non-terminal scanner)
                     ,(generate-parsing-expression target grammar non-terminal (find-rhs grammar non-terminal))))))
    (gen-trace fname `(progn (fmakunbound ',fname) ,form) trace)))


(defmethod generate-parser ((target (eql :lisp)) (grammar grammar) &key (trace nil))
  (let* ((fname  (scat "PARSE-" (grammar-name grammar)))
         (form   `(defun ,fname (source)
                    "
SOURCE: When the grammar has a scanner generated, or a scanner class
        name, SOURCE can be either a string, or a stream that will be
        scanned with the generated scanner.  Otherwise, it should be a
        SCANNER instance.
"
                    (let ((scanner ,(if (grammar-scanner grammar)
                                        `(make-instance ',(grammar-scanner grammar) :source source)
                                        'source)))
                      (advance-line scanner)
                      (with-non-terminal (,(grammar-name grammar) scanner)
                        (prog1 (,(gen-parse-function-name target grammar (grammar-start grammar))
                                scanner)
                          (unless (scanner-end-of-source-p scanner)
                            (cerror "Continue"
                                    'parser-end-of-source-not-reached
                                    :file (scanner-file scanner)
                                    :line (scanner-line scanner)
                                    :column (scanner-column scanner)
                                    :grammar (grammar-named ',(grammar-name grammar))
                                    :scanner scanner
                                    :non-terminal-stack (copy-list *non-terminal-stack*)))))))))
    (gen-trace fname `(progn (fmakunbound ',fname) ,form) trace)))



;;;------------------------------------------------------------
;;; RDP Parser Generator for non-normalized grammars
;;;------------------------------------------------------------


(defgeneric named-item-p (grammar item)
  (:documentation "
Whether ITEM is a terminal or non-terminal symbol of the grammar that
should be bound for actions.
")
  (:method ((grammar grammar) item)
    (and (symbolp item)
         (or (non-terminal-p grammar item)
             (terminalp grammar item))))
  (:method ((grammar normalized-grammar) item)
    (and (symbolp item)
         (or (and (non-terminal-p grammar item)
                  (not (member item (normalized-grammar-%synthesized-non-terminals grammar))))
             (terminalp grammar item)))))


(defmethod generate-parsing-sequence ((target (eql :lisp)) (grammar grammar) non-terminal rhs)
  (destructuring-bind (seq items actions) rhs
    (declare (ignore seq))
    (let* ((dollars (loop :for i :from 1 :to (length items)
                          :collect (dollar i)))
           (ignorables dollars))
      `(let* (,@(let ((increments (make-hash-table)))
                  (mapcan (lambda (dollar item)
                            (cons
                             ;; $n variables are bound to
                             ;; synthesized parsed elements:
                             ;; let* is used, so $2 action
                             ;; can use $1 non-terminal.1 etc.
                             `(,dollar ,(generate-parsing-expression target grammar non-terminal item))
                             ;; additionally, non-terminal
                             ;; and non-terminal.n names are
                             ;; defined:
                             (when (named-item-p grammar item)
                               (let* ((index  (incf (gethash item increments 0)))
                                      (igno   (scat item "." (prin1-to-string index))))
                                 (pushnew item ignorables)
                                 (push    igno ignorables)
                                 (append (when (= 1 index)
                                           (list (list item dollar)))
                                         (list (list igno dollar)))))))
                          dollars items))
              ;; $0 collects all the parsed elements in a list as a default synthesized value.
              (,(dollar 0) (list ,@dollars)))
         (declare (ignorable ,(dollar 0) ,@ignorables))
         ,@actions))))


(defmethod generate-parsing-expression ((target (eql :lisp)) (grammar grammar) non-terminal rhs)
  ;; If we want to generate the parser directly from the grammar with
  ;; seq/rep/opt/alt, then we need to replicate the algorithm for the
  ;; firsts-set of sentences here. :-(
  ;; Unfortunately, the follow-set is not implemented here.
  (labels ((es-firsts-set (extended-rhs)
             ;; EXTENDED-RHS: a RHS that can contain any grammar operator: SEQ, REP, OPT, or ALT.
             ;; RETURN: the firsts set of the given extended-rhs
             (if (atom extended-rhs)
                 (firsts-set grammar extended-rhs)
                 (ecase (rhs-operator extended-rhs)
                   ((seq) (if (null (rhs-body extended-rhs))
                              '(nil)
                              (loop
                                :with all-firsts = '()
                                :for item :in (rhs-body extended-rhs)
                                :for firsts = (es-firsts-set item)
                                :do (setf all-firsts (union firsts (delete nil all-firsts)))
                                :while (member nil firsts)
                                :finally (return all-firsts))))
                   ((rep) (es-firsts-set (first (rhs-body extended-rhs))))
                   ((opt) (adjoin nil (es-firsts-set (first (rhs-body extended-rhs)))))
                   ((alt) (remove-duplicates (reduce (function append) (rhs-body rhs)
                                                     :key (function es-firsts-set))))))))
    (if (atom rhs)
        (cond
          ((terminalp grammar rhs)
           `(accept scanner ',rhs))
          ((non-terminal-p grammar rhs)
           `(,(gen-parse-function-name target grammar rhs) scanner))
          (t
           (error "Invalid item ~S found in rule for ~S" rhs non-terminal)))
        (ecase (rhs-operator rhs)
          ((seq)
           (generate-parsing-sequence target grammar non-terminal rhs))
          ((rep)
           `(loop
              :while ,(gen-in-firsts target (es-firsts-set (first (rhs-body rhs))))
              :collect ,(generate-parsing-expression target grammar non-terminal (first (rhs-body rhs)))))
          ((opt)
           `(when ,(gen-in-firsts target (remove nil (es-firsts-set (first (rhs-body rhs)))))
              ,(generate-parsing-expression target grammar non-terminal (first (rhs-body rhs)))))
          ((alt)
           (let* ((follows          '(#:follows))
                  (count-empty      0)
                  (empty-item       nil)
                  (empty-firsts-set nil)
                  (firsts-sets (mapcar (lambda (item)
                                         (let ((firsts-set (es-firsts-set item)))
                                           `(item ,item firsts ,firsts-set)
                                           (when (member nil firsts-set)
                                             (when (plusp count-empty)
                                               (error "More than one alternative has an empty derivation: ~S" rhs))
                                             (incf count-empty)
                                             (setf empty-item item
                                                   empty-firsts-set firsts-set))
                                           firsts-set))
                                       (rhs-body rhs))))
             (flet ((gen-clause (firsts-set next-sets item)
                      (declare (ignore next-sets))
                      `(,(gen-in-firsts target (remove nil firsts-set))
                        ,(generate-parsing-expression target grammar non-terminal item))))
               `(cond
                  ,@(let ((firsts-sets (remove empty-firsts-set firsts-sets)))
                      (mapcar (function gen-clause)
                              firsts-sets
                              (append (rest firsts-sets) follows)
                              (remove empty-item (rhs-body rhs))))
                  ,@(when (and empty-item (< 1 (length empty-firsts-set)))
                      `(,(gen-clause empty-firsts-set follows empty-item)))
                  ,(if empty-item
                       `(t (let ((,(dollar 0) '()))
                             (declare (ignorable ,(dollar 0)))
                             ,@(if (null (rhs-body empty-item))
                                   (rhs-actions empty-item)
                                   `(,(dollar 0)))))
                       `(t (error-unexpected-token scanner
                                                   ',(mapcan (lambda (item)
                                                               (copy-list (es-firsts-set item)))
                                                             (rhs-body rhs))
                                                   nil 
                                                   ;; ',(assoc rhs (grammar-rules grammar))
                                                   )))))))))))


;;;------------------------------------------------------------
;;; RDP Parser Generator for normalized grammars
;;;------------------------------------------------------------

(defmethod generate-parsing-expression ((target (eql :lisp)) (grammar normalized-grammar) non-terminal rhs)
  (cond
    ((terminalp grammar rhs)
     `(accept scanner ',rhs))
    ((non-terminal-p grammar rhs)
     `(,(gen-parse-function-name target grammar rhs) scanner))
    ((eql 'seq (rhs-operator rhs))
     (generate-parsing-sequence target grammar non-terminal rhs))
    (t (error "Invalid item ~S found in rule for ~S" rhs non-terminal))))

(defmethod generate-non-terminal-parsing-expression ((target (eql :lisp)) (grammar normalized-grammar) non-terminal)
  (let* ((rhses   (find-rhses grammar non-terminal))
         ;; (firsts  (firsts-set grammar non-terminal))
         ;; (follows (follow-set grammar non-terminal))
         (rule    `(--> ,non-terminal (alt ,@rhses))))
    ;; #|DEBUG|#(format t "(--> ~A ~{~%     ~A~})~%" non-terminal rhses)
    (labels ((firsts-set (rhs)
               (rhs-firsts-set grammar non-terminal (rhs-body rhs)))
             
             (generic ()
               (let* ((empty  (find-if   (function empty-rhs-p) rhses))
                      (rhses  (remove-if (function empty-rhs-p) rhses))
                      (firsts (mapcar    (function firsts-set)  rhses)))
                 ;; #|DEBUG|#(when empty (format t "  empty~%"))
                 `(cond ,@(mapcar (lambda (firsts rhs)
                                    (assert (eq 'seq (rhs-operator rhs)))
                                    `(,(gen-in-firsts target firsts)
                                      ,(generate-parsing-expression target grammar non-terminal rhs)))
                                  firsts rhses)
                        ,@(unless empty
                            `((t (error-unexpected-token
                                  scanner
                                  ',(remove-duplicates (reduce (function append) firsts))
                                  ',rule)))))))

             (check-disjoint (sets)
               (loop
                 :for ((aset . arhs) . rest) :on sets
                 :while rest
                 :do (loop
                       :for (bset . brhs) :in rest
                       :for inter := (intersection aset bset)
                       :do (when inter
                             (error "Non-disjoint rhses: ~%   ~A --> ~S~%   ~A --> ~S~%   Intersection: ~S~%"
                                    non-terminal arhs non-terminal brhs
                                    (intersection aset bset))))))

             (check-empties (empty base-firsts base-cases)
               (when (rest empty)
                 (error "More than one empty rule: ~%~{   ~A --> ~S~%~}"
                        (mapcar (lambda (rhs) (list non-terminal rhs)) empty)))
               (let ((empty-base-cases (loop
                                         :for firsts :in base-firsts
                                         :for rhs :in base-cases
                                         :when (member nil firsts)
                                           :collect rhs)))
                 (when (< (if empty 0 1) (length empty-base-cases))
                   (error "More than ~R rule reduces to empty: ~%~{   ~A --> ~S~%~}"
                          (if empty 0 1)
                          (mapcar (lambda (rhs) (list non-terminal rhs)) empty-base-cases)))))
             
             (gen-cond-clause (firsts rhs finish)
               `(,(gen-in-firsts target (remove nil firsts))
                 (push ,(generate-parsing-expression target grammar non-terminal
                                                     (if finish
                                                         rhs
                                                         `(seq ,(append (butlast (rhs-body rhs))
                                                                        (list *empty-rhs*))
                                                               ,(rhs-actions rhs))))
                       $items)
                 ,@(when finish
                     `((loop-finish)))))
             
             (generate-parse-loop (empty base-cases recursives)
               (assert recursives)
               (unless (or empty base-cases)
                 (error "Recursive rules without base case: ~%~{   ~A --> ~S~%~}"
                        (mapcar (lambda (rhs) (list non-terminal rhs)) rhses)))
               (let ((base-firsts (mapcar (function firsts-set) base-cases))
                     (recu-firsts (mapcar (function firsts-set) recursives)))
                 (check-empties empty base-firsts base-cases)               
                 (check-disjoint (mapcar (function cons)
                                         (append base-firsts recu-firsts)
                                         (append base-cases  recursives)))
                 
                 `(loop
                    :with $items := '()
                    :do (cond
                          ,@(mapcar (lambda (firsts rhs) (gen-cond-clause firsts rhs nil))
                                    recu-firsts recursives)
                          ,@(mapcar (lambda (firsts rhs) (gen-cond-clause firsts rhs t))
                                    base-firsts base-cases)
                          ,@(if (or empty (find-if (lambda (firsts) (member nil firsts)) base-firsts))
                                `((t (loop-finish)))
                                `((t (error-unexpected-token scanner
                                                             ',(remove-duplicates
                                                                (reduce (function append)
                                                                        (append base-firsts recu-firsts)))
                                                             ',rule)))))
                    :finally (return (reduce (function append)
                                             (nreverse $items)
                                             :initial-value nil))))))
      
      (if rhses
          (loop
            :for rhs :in rhses
            :if (empty-rhs-p rhs)
              :collect rhs :into empties
            :else :if (right-recursive-rhs-p non-terminal rhs)
                    :collect rhs :into recursives
            :else
              :collect rhs :into base-cases
            :finally
               (return (if recursives
                           (generate-parse-loop empties base-cases recursives)
                           (generic))))
          (error "Non-terminal ~S has no rule in grammar ~S"
                 non-terminal (grammar-name grammar))))))


(defmethod generate-non-terminal-parser-function ((target (eql :lisp)) (grammar normalized-grammar) non-terminal &key (trace nil))
  (let* ((fname (gen-parse-function-name target grammar non-terminal))
         (form  `(defun ,fname (scanner)
                   ,(format nil "~S" `(--> ,non-terminal ,(find-rhs grammar non-terminal)))
                   (with-non-terminal (,non-terminal scanner)
                     ,(generate-non-terminal-parsing-expression target grammar non-terminal)))))
    (gen-trace fname `(progn (fmakunbound ',fname) ,form) trace)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;

