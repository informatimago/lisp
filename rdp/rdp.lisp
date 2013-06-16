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
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
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
;;;;    Copyright Pascal Bourguignon 2006 - 2012
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
  name terminals start rules
  all-terminals
  all-non-terminals
  ;; ---
  (scanner     t)
  (skip-spaces t)
  ;; --- computed:
  first-function
  follow-function)




(defvar *grammars* (make-hash-table)
  "Records the variables defined with DEFGRAMMAR.
Use (GRAMMAR-NAMED name) to look up a grammar.")

(defun grammar-named (name)
  "Returns the grammar named NAME, or NIL if none."
  (gethash name *grammars*))




(defgeneric generate-boilerplate (target-language grammar &key trace)
  (:documentation "Generate the boilerplate code needed by the scanner and parser.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate.")
  (:method (target-language grammar &key trace)
    (declare (ignore target-language grammar trace))
    nil))


(defgeneric generate-scanner     (target-language grammar &key trace)
    (:documentation "Generate the scanner code,
when (grammar-scanner grammar) is T.  (grammar-scanner grammar) may
be the class-name of a scanner to use.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate."))


(defgeneric generate-nt-parser   (target-language grammar non-terminal &key trace)
    (:documentation "Generate the parser code for the given non-terminal.

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




;;; First, we define a grammar, with actions.
;;; The scanner and parser is generated at macro expansion time.

(defvar *linenum* 0)

(defun generate-grammar (name &key terminals (scanner t) (skip-spaces t)
                           start rules
                           (target-language :lisp) (trace nil))
    "
SEE ALSO:   The docstring of DEFGRAMMAR.
RETURN:     A form that defines the grammar object and its parser functions.
"
    (let* ((clean-rules (clean-rules rules))
           (grammar (make-grammar :name name
                                  :terminals terminals
                                  :start start
                                  :rules clean-rules
                                  :scanner scanner
                                  :skip-spaces skip-spaces))
           (*linenum* 0)
           (g (gensym "grammar")))
      (setf (gethash (grammar-name grammar) *grammars*) grammar)
      (compute-all-terminals     grammar)
      (compute-all-non-terminals grammar)
      (compute-first-follow      grammar)

      `(let ((*linenum* 0)
             (,g (make-grammar
                  :name ',name
                  :terminals ',terminals
                  :start ',start
                  :rules ',clean-rules
                  :scanner ',scanner
                  :skip-spaces ',skip-spaces)))
         (setf (gethash (grammar-name ,g) *grammars*) ,g)
         (compute-all-terminals     ,g)
         (compute-all-non-terminals ,g)
         (compute-first-follow      ,g)
         
         ,(generate-boilerplate target-language grammar :trace trace)         
         ,(generate-scanner     target-language grammar :trace trace)
         ,@(mapcar (lambda (non-terminal)
                     (generate-nt-parser target-language grammar non-terminal  :trace trace))
                   (grammar-all-non-terminals grammar))
         ,(generate-parser target-language grammar :trace trace)
         ',name)))



  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleaning of the grammar rules:
;;;

;; When stored inside the grammar structure, rules must be of the
;; cleaned form:
;; 
;; rule    := (<lhs> <rhs>) .
;; lhs     := <non-terminal> .
;; rhs     := (seq <items> <actions>)
;;          | (rep <items>)
;;          | (opt <items>)
;;          | (alt <items>) .
;; items   := ( <word>* ) .
;; word    := <rhs> | <terminal> | <non-terminal> .
;; actions := ( <form>* ) .
;;
;; - a SEQ rhs may have 0 or more items in its <items> list.
;; - a REP rhs should have exactly 1 SEQ item in its <items> list.
;; - a OPT rhs should have exactly 1 SEQ item in its <items> list.
;; - a ALT rhs should have 1 or more items in its <items> list.
;;
;; ε is represented as (seq () ('nil))
;; ε is not expected from the user rules, but is generated in
;; normalized grammars.
;;
;;
;; FIND-RULES returns a list of <rule>s.
;; FIND-RULE  returns a <rhs>.
;; Notice: if there are several rules for the same non-terminal,
;; find-rule returns a single ALT rhs with all the rhs of the rules of
;; that non-terminal.


(defun split-action (rhs)
  (declare (inline))
  (let ((separator (position :action rhs)))
    (if separator
        (values (subseq rhs 0 separator) (subseq rhs (1+ separator)))
        (values rhs                      nil))))

(defun clean-seq (expr)
  (multiple-value-bind (rhs actions) (split-action (cdr expr))
    (setf actions (or actions '($0)))
    (let ((items (mapcar (lambda (item) (clean item)) rhs)))
      (if (and (null actions) (or (null items) (null (cdr items))))
          (car items)
          (list 'seq items actions)))))

(defun clean-with-action (expr)
  (multiple-value-bind (rhs actions) (split-action (cdr expr))
    (if (null actions)
        (if (null rhs)
            '(seq () ('nil))
            `(,(car expr) (,(clean-seq `(seq ,@rhs)))))
        `(,(car expr) (,(clean-seq `(seq ,@rhs :action ,@actions)))))))

(defun clean-rep (expr) (clean-with-action expr))
(defun clean-opt (expr) (clean-with-action expr))

(defun clean-alt (expr)
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
  (if (atom expr)
      expr
      (ecase (car expr)
        ((seq) (clean-seq expr))
        ((rep) (clean-rep expr))
        ((alt) (clean-alt expr))
        ((opt) (clean-opt expr)))))


(defun clean-rules (rules)
  (mapcar (lambda (rule)
            (destructuring-bind (--> non-term &rest items) rule
               (assert (string= --> '-->) () "Rules should be written as (--> <non-terminal> <rhs>)~%Invalid rule: ~S" rule)
               `(,non-term ,(clean
                             (if (find :action items)
                                 `(seq ,@items)
                                 `(seq ,@items :action `(,',non-term ,@$0)))))))
          rules))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric word-equal (a b)
  (:method ((a t) (b t))           (eql a b))
  (:method ((a string) (b string)) (string= a b)))

(defun compute-all-terminals (grammar)
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


(defun compute-all-non-terminals (grammar)
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



;;; To implement the follow-set function we need to put the grammar
;;; into a normal form.
;;; We don't really need it to generate a simplistic recursive descent
;;; parser.


(defun terminalp (grammar item)
  (member item (grammar-all-terminals grammar)
          :test (function word-equal)))

(defun non-terminal-p (grammar item)
  (member item (grammar-all-non-terminals grammar)))


(defun find-rules (grammar non-terminal)
  "
RETURN: all the produtions with NON-TERMINAL as left-hand-side.
PRE:    (non-terminal-p non-terminal)
"
  (let ((rules (mapcar (function second)
                       (remove-if-not (lambda (rule) (eql non-terminal (first rule)))
                                      (grammar-rules grammar)))))
    (if (null rules)
        (error "~s is not a non-terminal in the grammar ~A"
               non-terminal (grammar-name grammar))
        rules)))

(defun find-rule (grammar non-terminal)
  "
RETURN: the productions with NON-TERMINAL as left-hand-side as a
        single ALT production (or just the production if there's only
        one).
PRE:    (non-terminal-p non-terminal)
"
  (let ((rules (find-rules grammar non-terminal)))
    (if (null (cdr rules))
        (car rules)
        `(alt ,rules))))


(define-modify-macro appendf (&rest args)  append "Append onto list")

(defun prepend (old &rest new-lists)
  (apply (function append) (append new-lists (list old))))
(define-modify-macro prependf (&rest args) prepend "Prepend onto list")



(defun compute-first-sets (grammar)
  "  
PRE:    The GRAMMAR must be normalized.
        (ie. containly only SEQ rules)

DO:     Signals an error if there are duplicates in the first set of a non-terminal.
RETURN: A hash-table containing the first-set for each symbol of the
        grammar.  (terminals and non terminals).
"
  (let ((first-sets (make-hash-table :test (function equal))))
    (labels ((first-set (symbol)
               (let ((entry (gethash symbol first-sets)))
                 (cond (entry
                        (if (eq :error entry)
                            (error "There's a left recursion involving the symbol ~S in the grammar ~A"
                                   symbol (grammar-name grammar))
                            entry))
                       ((terminalp grammar symbol)
                        (setf (gethash symbol first-sets)
                              (list symbol)))
                       ((compute-first-set symbol)))))
             (compute-first-set (non-terminal)
               (setf (gethash non-terminal first-sets) :error)
               (let ((first-set '()))
                 (dolist (rule (find-rules grammar non-terminal))
                   (destructuring-bind (seq sentence &optional action) rule
                     (declare (ignore seq action))
                     (if (null sentence)
                         (push nil first-set)
                         (loop
                           :with all-firsts = '()
                           :for item :in sentence
                           :for firsts = (first-set item)
                           :do      (setf all-firsts (union firsts (delete nil all-firsts)))
                           :while   (member nil firsts)
                           :finally (prependf first-set all-firsts)))))
                 (let ((unique-first-set  (remove-duplicates first-set :test (function equal))))
                   (assert (= (length first-set) (length unique-first-set))
                           () "There are duplicates in the first sets of the rules for the non-terminal ~S: ~S"
                           non-terminal (duplicates first-set))
                  (setf (gethash non-terminal first-sets) unique-first-set)))))
      (map nil (function first-set) (grammar-all-terminals grammar))
      (map nil (function first-set) (grammar-all-non-terminals grammar)))
    first-sets))


(defun compute-first-function (grammar)
  "
PRE:    The GRAMMAR must be normalized.
        (ie. containly only SEQ rules)
RETURN: The first-set function for the grammar symbols.
"
  (let ((first-sets (compute-first-sets grammar)))
    (setf (grammar-first-function grammar)
          (lambda (symbol-or-sequence)
            (labels ((first-set (item)
                       (cond
                         ((null item) nil)
                         ((atom item)
                          (multiple-value-bind (first-set presentp) (gethash item first-sets)
                            (if presentp
                                first-set
                                (error "~S is not a symbol of the grammar ~A"
                                       item (grammar-name grammar)))))
                         (t (loop
                               :with result = '()
                               :for item :in symbol-or-sequence
                               :for first-set = (first-set item)
                               :do (prependf result first-set)
                               :while (member nil first-set)
                               :finally (return (delete-duplicates result :test (function equal))))))))
              (first-set symbol-or-sequence))))))


(defun nullablep (grammar sentence)
  (cond
    ((null  sentence)
     t)
    ((listp sentence)    
     (every (lambda (word) (nullablep grammar word)) sentence))
    ((terminalp grammar sentence)
     nil)
    ((non-terminal-p grammar sentence)
     (labels ((nullable-rule-p (rule)
                (if (atom rule)
                    (nullablep grammar rule)
                    (ecase (first rule)
                      ((seq rep) (every (lambda (item) (nullable-rule-p item))
                                        (second rule)))
                      ((opt) t)
                      ((alt) (some (lambda (item) (nullable-rule-p item))
                                   (second rule)))))))
       (nullable-rule-p (find-rule grammar sentence))))))


(defvar *eof-symbol* (make-symbol "EOF")
  "The symbol used to denote the End-Of-Source in the follow-sets.")

(defun compute-follow-sets (grammar)
  "
PRE:    The GRAMMAR must be normalized.
        (ie. containly only SEQ rules)
RETURN: A hash-table containing the follow-set for each non-terminal
        of the grammar.
"
  (let ((base-constraints      '())
        (recursive-constraints '()))
    (flet ((first-set (item) (first-set grammar item)))
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
                     (let ((m (first-set beta)))
                       (when beta
                         ;; (first-set beta)∖{ε} ⊂ (follow-set n)
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


(defun compute-follow-function (grammar &optional non-terminals)
  "
PRE:    The GRAMMAR must be normalized.
        (ie. containly only SEQ rules)

NON-TERMINAL: When given, it's the list of non-terminals of the
              non-normalized grammar which we are interested in.

RETURN: The follow-set function for the grammar non-terminals.
"
  (let ((follow-sets
         (if non-terminals
             (let ((follow-sets (make-hash-table :size (length non-terminals)))
                   (normalized-follow-sets (compute-follow-sets grammar)))
               (dolist (non-terminal non-terminals follow-sets)
                 (setf (gethash non-terminal follow-sets)
                       (gethash non-terminal normalized-follow-sets))))
             (compute-follow-sets grammar))))
    ;; build the resulting function.
    (setf (grammar-follow-function grammar)
          (lambda (non-terminal)
            (or (gethash non-terminal follow-sets)
                (error "~S is not a non-terminal of the grammar ~A"
                       non-terminal (grammar-name grammar)))))))



(defun compute-first-follow (grammar)
  (let ((ng (normalize-grammar grammar)))
    (setf (grammar-first-function  grammar) (compute-first-function ng)
          (grammar-follow-function grammar)
          (compute-follow-function ng (grammar-all-non-terminals grammar)))
    grammar))
 
(defun first-set (grammar symbol)
  (unless (grammar-first-function grammar)
    (compute-first-follow grammar))
  (funcall (grammar-first-function grammar) symbol))

(defun follow-set (grammar non-terminal)
  (unless (grammar-follow-function grammar)
    (compute-first-follow grammar))
  (funcall (grammar-follow-function grammar) non-terminal))



;;; Follow set.
;;; Normalization of the grammar.
;;; Each rule is put under the form:  a --> ε   or   a --> e a


(defun make-new-non-terminal (base-nt non-terminals)
  (loop
     :for i :from 1
     :for new-nt = (intern (format nil "~A-~A" base-nt i))
     :while (member new-nt non-terminals)
     :finally (return new-nt)))


(defun normalize-grammar-rules (rules non-terminals)
  "
Substitute any sub-expressions in the rhs with a new non-terminal and
a new production.  Then replace the rep, opt, and alt rules with seq
rules and new produtions.  Returns the new production set.
"
  (values
   (loop
      :while rules ; :for is always evaluated before :while
      :collect (let ((rule (pop rules)))
                 (destructuring-bind (nt rhs) rule
                   (labels ((new-rule (rule)
                              (push (first rule) non-terminals)
                              (push rule rules))
                            (process-item (item)
                              (if (listp item)
                                  (let ((new-nt (make-new-non-terminal nt non-terminals)))
                                    (new-rule (list new-nt item))
                                    new-nt)
                                  item)))
                     (let ((op (first rhs)))
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
                            (new-rule (list nt '(seq () ('()))))
                            (list nt `(seq (,(process-item (first items)) ,nt)
                                           ((cons $1 $2))))))
                         ((opt)
                          ;; a --> (opt e)
                          ;; -------------
                          ;; a --> ε :action '()
                          ;; a --> e :action (list $1)
                          (destructuring-bind (op items) rhs
                            (declare (ignore op))
                            (new-rule (list nt '(seq () ('()))))
                            (list nt `(seq (,(process-item (first items)))
                                           ((list $1))))))
                         ((alt)
                          ;; a --> (alt e₁ ... eν)
                          ;; -------------
                          ;; a --> e₁ :action $0
                          ;; ...
                          ;; a --> eν :action $0
                          (destructuring-bind (op items) rhs
                            (declare (ignore op))
                            (let ((new-items  (mapcar (function process-item) items)))
                              (dolist (new-item (rest new-items)
                                       (list nt `(seq (,(first new-items)) (($0)))))
                                (new-rule (list nt `(seq (,new-item) (($0)))))))))))))))
   non-terminals))




(defun normalize-grammar (grammar)
  "Return a new normalized grammar parsing the same language as GRAMMAR."
  (let ((new-grammar (make-grammar
                      :name (intern (format nil "NORMALIZED-~A"
                                            (grammar-name grammar)))
                      :terminals (grammar-terminals grammar)
                      :start (grammar-start grammar)
                      :rules (normalize-grammar-rules (grammar-rules grammar)
                                                      (grammar-all-non-terminals grammar))
                      :skip-spaces (grammar-skip-spaces grammar))))
    (setf (gethash (grammar-name new-grammar) *grammars*) new-grammar)
    (compute-all-terminals     new-grammar)
    (compute-all-non-terminals new-grammar)
    new-grammar))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Regexp Portability Layer.
;;;
;;; Used by the code generated by the Lisp generator below.
;;;
;;; On clisp, the user can choose to use the REGEXP package instead of
;;; CL-PPCRE, by adjoining :use-regexp instead of :use-ppcre to
;;; *features* (see the beginning of this file). 



;; (:export "SPLIT-STRING"
;;          "STRING-MATCH" "MATCH-STRING" "MATCH-BEGINNING" "MATCH-END"
;;          "REGEXP-MATCh-ANY" "REGEXP-COMPILE" "REGEXP-QUOTE-EXTENDED")


(defun split-string (string regexp)
  #-(or use-ppcre (and clisp use-regexp))
  (error "Please implement ~S (perhaps push :use-ppcre on *features*)." 'split-string)
  #+(and clisp use-regexp) (regexp:regexp-split regexp string)
  #+use-ppcre (cl-ppcre:split regexp string))


(defvar *string-match-results* '())

#+(and clisp use-regexp)
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

(defun string-match (regexp string &key (start 0) (end nil))
  #-(or use-ppcre (and clisp use-regexp))
  (error "Please implement ~S (perhaps push :use-ppcre on *features*)." 'split-match)
  #+(and clisp use-regexp)
  (setf *string-match-results*
         (let ((results (if (stringp regexp)
                           (multiple-value-list
                            (regexp:match regexp string :start start :end end :extended t :ignore-case nil :newline t :nosub nil))
                           (regexp:regexp-exec (cdr regexp) string :start start :end end :return-type 'list))))
          (if (equal '(nil) results)
              nil
              results)))
  #+use-ppcre
  (setf *string-match-results*
        (let ((results (multiple-value-list
                        (if (stringp regexp)
                            (cl-ppcre:scan regexp       string :start start :end (or end (length string)))
                            (cl-ppcre:scan (cdr regexp) string :start start :end (or end (length string)))))))
          (if (equal '(nil) results)
              nil
              (destructuring-bind (as ae ss es) results
                  (list as ae
                        (concatenate 'vector (vector as) ss)
                        (concatenate 'vector (vector ae) es)))))))

(defun match-string (index string &optional (match-results *string-match-results*))
  #-(or use-ppcre (and clisp use-regexp))
  (error "Please implement ~S (perhaps push :use-ppcre on *features*)." 'match-string)
  #+(and clisp use-regexp)
  (let ((m (elt match-results index)))
    (when m (regexp:match-string string m)))
  #+use-ppcre
  (let ((start (ignore-errors (aref (elt match-results 2) index)))
        (end   (ignore-errors (aref (elt match-results 3) index))))
    (when (and start end)
      (subseq string start end))))

(defun match-beginning (index &optional (match-results *string-match-results*))
  #-(or use-ppcre (and clisp use-regexp))
  (error "Please implement ~S (perhaps push :use-ppcre on *features*)." 'match-beginning)
  #+(and clisp use-regexp)
  (let ((m (elt match-results index)))
    (when m (regexp:match-start m)))
  #+use-ppcre
  (ignore-errors (aref (elt match-results 2) index)))

(defun match-end (index &optional (match-results *string-match-results*))
  #-(or use-ppcre (and clisp use-regexp))
  (error "Please implement ~S (perhaps push :use-ppcre on *features*)." 'match-end)
  #+(and clisp use-regexp)
  (let ((m (elt match-results index)))
    (when m (regexp:match-end m)))
  #+use-ppcre
  (ignore-errors (aref (elt match-results 3) index)))

(defun regexp-match-any (groupsp)
  #- (or use-ppcre (and clisp use-regexp))
  (error "Please implement ~S (perhaps push :use-ppcre on *features*)." 'regexp-match-any)
  #+(and clisp use-regexp) (if groupsp "(.*)" ".*")
  #+use-ppcre              (if groupsp "(.*)" ".*"))

(defun regexp-compile (regexp)
  #- (or use-ppcre (and clisp use-regexp))
  (error "Please implement ~S (perhaps push :use-ppcre on *features*)." 'regexp-compile)
  #+(and clisp use-regexp) (regexp:regexp-compile regexp
                                                  :extended t
                                                  :ignore-case nil
                                                  :newline t
                                                  :nosub nil)
  #+use-ppcre (cl-ppcre:create-scanner regexp
                                       :case-insensitive-mode nil
                                       :multi-line-mode nil
                                       :extended-mode nil
                                       :destructive nil))

(defun regexp-quote-extended (string)
  ;; #+clisp regexp:regexp-quote doesn't quote extended regexps...
  ;;        (regexp:regexp-quote "(abc .*" t) --> "(abc \\.\\*"  instead of "\\(abc \\.\\*"
  #-use-ppcre
  (let* ((special-characters "^.[$()|*+?{\\")
         (increase (count-if (lambda (ch) (find ch special-characters)) string)))
     (if (zerop increase)
         string
         (let ((result (make-array (+ (length string) increase)
                                    :element-type 'character)))
           (loop
              :with i = -1
              :for ch :across string
              :do (if (find ch special-characters)
                      (setf (aref result (incf i)) #\\
                            (aref result (incf i)) ch)
                      (setf (aref result (incf i)) ch)))
           result)))
  #+use-ppcre
  (cl-ppcre:quote-meta-chars string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generator -- LISP
;;;

;; (defvar *boilerplate-generated* nil)
;; ;; (setf *boilerplate-generated* nil)
;; 
;; 
;; (defmethod generate-boilerplate ((target (eql :lisp)) (grammar grammar) &key (trace nil))
;;   (declare (ignore trace))
;;   (if *boilerplate-generated*
;;       nil
;;       (progn
;;         (setf *boilerplate-generated* t)
;;         `(progn
;; 
;;            (defvar *non-terminal-stack* '()
;;              "For error reporting.")
;; 
;;            (define-condition parser-error (error)
;;              ((line    :initarg :line    :initform 1   :reader parser-error-line)
;;               (column  :initarg :column  :initform 0   :reader parser-error-column)
;;               (grammar :initarg :grammar :initform nil :reader parser-error-grammar)
;;               (scanner :initarg :scanner :initform nil :reader parser-error-scanner)
;;               (non-terminal-stack :initarg :non-terminal-stack
;;                                   :initform '()
;;                                   :reader parser-error-non-terminal-stack)
;;               (format-control     :initarg :format-control
;;                                   :initform ""
;;                                   :reader parser-error-format-control)
;;               (format-arguments   :initarg :format-arguments
;;                                   :initform '()
;;                                   :reader parser-error-format-arguments))
;;              (:report print-parser-error))
;; 
;;            (defmethod print-parser-error ((err parser-error) stream)
;;              (format stream
;;                      "~&~@[~A:~]~D:~D: ~?~%"
;;                      (let ((source (scanner-source (parser-error-scanner err))))
;;                        (unless (stringp source) (ignore-errors (pathname source))))
;;                      (parser-error-line err)
;;                      (parser-error-column err)
;;                      (parser-error-format-control err)
;;                      (parser-error-format-arguments err)))
;; 
;;            (define-condition parser-end-of-source-not-reached (parser-error)
;;              ())
;; 
;;            (define-condition parser-error-unexpected-token (parser-error)
;;              ((expected-token :initarg :expected-token
;;                               :initform nil
;;                               :reader parser-error-expected-token)))
;; 
;; 
;;            (defclass rdp-scanner (scanner)
;;              ((buffer       :accessor scanner-buffer
;;                             :type     (or null string)
;;                             :initform nil)
;;               (current-text :accessor scanner-current-text
;;                             :initform "")))
;; 
;;            (defmethod scanner-current-token ((scanner rdp-scanner))
;;              (token-kind (call-next-method)))
;; 
;;            (defmethod scanner-end-of-source-p ((scanner rdp-scanner))
;;              (and (or (null (scanner-buffer scanner))
;;                       (<= (length (scanner-buffer scanner))
;;                           (scanner-column scanner)))
;;                   (let ((ps  (slot-value scanner 'stream)))
;;                    (not (ungetchar ps (getchar ps))))))
;; 
;;            (defmethod advance-line ((scanner rdp-scanner))
;;              "RETURN: The new current token, old next token"
;;              (cond
;;                ((scanner-end-of-source-p scanner)
;;                 #|End of File -- don't move.|#)
;;                ((setf (scanner-buffer scanner) (readline (slot-value scanner 'stream)))
;;                 ;; got a line -- advance a token.
;;                 (setf (scanner-column scanner) 0)
;;                 (incf (scanner-line   scanner))
;;                 (setf (scanner-current-token scanner) nil
;;                       (scanner-current-text  scanner) "")
;;                 (scan-next-token scanner))
;;                (t
;;                 ;; Just got EOF
;;                 (setf (scanner-current-token scanner) '|<END OF FILE>|
;;                       (scanner-current-text  scanner) "<END OF FILE>")))
;;              (scanner-current-token scanner))
;; 
;;            (defmethod accept ((scanner rdp-scanner) token)
;;              (if (word-equal token (scanner-current-token scanner))
;;                  (prog1 (list (token-kind (scanner-current-token scanner))
;;                               (scanner-current-text scanner)
;;                               (scanner-column scanner))
;;                    (scan-next-token scanner))
;;                  (error 'parser-error-unexpected-token
;;                         :line   (scanner-line scanner)
;;                         :column (scanner-column scanner)
;;                         :grammar (grammar-named ',(grammar-name grammar))
;;                         :scanner scanner
;;                         :non-terminal-stack (copy-list *non-terminal-stack*)
;;                         :expected-token token
;;                         :format-control "Expected ~S, not ~A (~S)~%~S~%~{~A --> ~S~}"
;;                         :format-arguments (list
;;                                            token
;;                                            (scanner-current-token scanner)
;;                                            (scanner-current-text scanner)
;;                                            *non-terminal-stack*
;;                                            (assoc (first *non-terminal-stack*)
;;                                                   ',(grammar-rules grammar))))))
;; 
;;            (defparameter *spaces*
;;              (format nil "^([~{~C~}]+)" '(#\space #\newline #\tab)))))))


(defvar *non-terminal-stack* '()
  "For error reporting.")

(defmacro with-non-terminal (non-terminal &body body)
  `(let ((*non-terminal-stack* (cons ',non-terminal *non-terminal-stack*)))
     ;; (print *non-terminal-stack*)
     ,@body))


(defgeneric gen-scanner-function-name (target grammar))
(defgeneric gen-scanner-class-name    (target grammar))
(defgeneric gen-parse-function-name   (target grammar non-terminal))
(defgeneric gen-in-firsts             (target firsts))
(defgeneric gen-parsing-statement     (target grammar item))

(defmethod gen-scanner-function-name ((target (eql :lisp)) (grammar grammar))
  (intern (format nil "~:@(SCAN-~A~)" (grammar-name grammar))))

(defmethod gen-scanner-class-name ((target (eql :lisp)) (grammar grammar))
  (intern (format nil "~:@(~A-SCANNER~)" (grammar-name grammar))))


(defun gen-trace (fname form trace)
  (if trace
      `(progn
         ,form
         (trace ,fname))
      form))

(defun tracep (keyword trace)
  (or (eql keyword trace)
      (and (listp trace) (member keyword trace))))


(defmethod generate-scanner ((target (eql :lisp)) grammar &key (trace nil))
  ;;
  ;; an-terminals  = literal terminals (given as string in rules), ending with an alphanumeric.
  ;; nan-terminals = literal terminals ending with something else than an alphanumeric.
  ;; nl-terminals  = non-literal terminals (specified in :terminals clauses).
  ;;
  ;; an-terminals are scanned by excluding alphanumerics directly after them.
  ;; "while" --> "(while)([^A-Za-z0-9]|$)"  so that "while whilemin" scans as <while> <identifier>.
  ;;
  ;; nl-terminals are processed in the order they're given in the :terminals clauses.
  ;;
  (case (grammar-scanner grammar)
    ((t)
     (let* ((scanner-class-name (gen-scanner-class-name target grammar))
            ;; Literal Alpha Numeric Terminals
            (an-terminals  (sort (remove-if-not
                                  (lambda (item)
                                    (and (stringp item)
                                         (alphanumericp (aref item (1- (length item))))))
                                  (grammar-all-terminals grammar))
                                 (function >) :key (function length)))
            ;; Literal Non Alpha Numeric Terminals
            (nan-terminals (sort (remove-if
                                  (lambda (item)
                                    (or (not (stringp item))
                                        (alphanumericp (aref item (1- (length item))))))
                                  (grammar-all-terminals grammar))
                                 (function >) :key (function length)))
            ;; Non Literal Terminals
            (nl-terminals (remove-if (function stringp) (grammar-terminals grammar)))
            ;; Regexps for all the Literal Alpha Numeric Terminals
            (lit-an-terminals-regexp
             (format nil "^(~{~A~^|~})([^A-Za-z0-9]|$)"
                     (mapcar (function regexp-quote-extended) an-terminals)))
            ;; Regexps for all the Literal Non Alpha Numeric Terminals
            (lit-nan-terminals-regexp
             (format nil "^(~{~A~^|~})"
                     (mapcar (function regexp-quote-extended)  nan-terminals)))
            (form  `(progn

                      (setf (grammar-scanner (gethash  ',(grammar-name grammar) *grammars*)) ',scanner-class-name)
                      
                      (defclass ,scanner-class-name  (rdp-scanner)
                        ())

                      (defmethod scan-next-token ((scanner ,scanner-class-name) &optional parser-data)
                        "RETURN: (scanner-current-token scanner)" 
                        (declare (ignore parser-data))
                        (let (match) 
                          ,@(when (grammar-skip-spaces grammar)
                                  `((setf match (string-match *spaces*
                                                              (scanner-buffer scanner)
                                                              :start (1- (scanner-column scanner))))
                                    (when match
                                      (setf (scanner-column scanner) (1+ (match-end 1 match))))))
                          (let ((pos (1- (scanner-column scanner))))
                            (cond
                              ;; end of source
                              ((scanner-end-of-source-p scanner)
                               (setf (scanner-column scanner)   (1+ (length (scanner-buffer scanner)))
                                     (scanner-current-text scanner)   "<END OF SOURCE>"
                                     (scanner-current-token scanner) '|<END OF SOURCE>|))
                              ;; end of line
                              ((scanner-end-of-line-p scanner)
                               (advance-line scanner))
                              ;; Literal Alpha Numeric and Non Alpha Numeric Terminals:
                              ,@(when (or an-terminals nan-terminals)
                                      ;; (print (list an-terminals nan-terminals))
                                      `(((or ,@(when an-terminals
                                                     `((setf match (string-match ',lit-an-terminals-regexp
                                                                                 (scanner-buffer scanner)
                                                                                 :start pos))))
                                             ,@(when nan-terminals
                                                     `((setf match (string-match ',lit-nan-terminals-regexp
                                                                                 (scanner-buffer scanner)
                                                                                 :start pos)))))
                                         (let ((text (match-string 1 (scanner-buffer scanner) match)))
                                           (setf (scanner-column scanner)        (1+ (match-end 1 match))
                                                 (scanner-current-text scanner)  text
                                                 (scanner-current-token scanner) text)))))
                              ;; Non Literal Terminals: we have a regexp for each terminal.
                              ,@(mapcar
                                 (lambda (terminal)
                                   ;; (print terminal)
                                   `(,(if (= 4 (length terminal))
                                          ;; (terminal-name match-regexp / exclude-regexp)
                                          `(and (setf match (string-match
                                                             ',(format nil "^(~A)" (second terminal))
                                                             (scanner-buffer scanner)
                                                             :start pos))
                                                (not (string-match ,(format nil "^(~A)" (fourth terminal))
                                                                   (scanner-buffer scanner)
                                                                   :start (match-end 1 match))))
                                          ;; (terminal-name match-regexp)
                                          `(setf match (string-match
                                                        ',(format nil "^(~A)" (second terminal))
                                                        (scanner-buffer scanner)
                                                        :start pos)))
                                      (setf (scanner-column scanner)        (1+ (match-end 1 match))
                                            (scanner-current-text scanner)  (match-string 1 (scanner-buffer scanner) match)
                                            (scanner-current-token scanner) ',(first terminal))))
                                 nl-terminals)
                              ;; Else we have an error:
                              (t
                               (error 'scanner-error-invalid-character
                                      :line   (scanner-line   scanner)
                                      :column (scanner-column scanner)
                                      :state  (scanner-state  scanner)
                                      :current-token (scanner-current-token scanner)
                                      :scanner scanner
                                      :invalid-character (aref (scanner-buffer scanner) pos)
                                      :format-control "Invalid character ~S at position: ~D~%~S~%~{~A --> ~S~}"
                                      :format-arguments (list
                                                         (aref (scanner-buffer scanner) pos)
                                                         (scanner-column scanner)
                                                         *non-terminal-stack*
                                                         (assoc (first *non-terminal-stack*)
                                                                ',(grammar-rules grammar))))))))))))
       (setf (grammar-scanner (grammar-named (grammar-name grammar))) scanner-class-name)
       (gen-trace 'scan-next-token form trace)))
    (otherwise
     #|Dont do anything|#
     `',(grammar-scanner grammar))))


(defmethod gen-parse-function-name ((target (eql :lisp)) (grammar grammar) non-terminal)
  (intern (format nil "~:@(~A/PARSE-~A~)" (grammar-name grammar) non-terminal)))

(defmethod gen-in-firsts ((target (eql :lisp)) firsts)
  (if (null (cdr firsts))
      `(word-equal (scanner-current-token scanner) ',(car firsts))
      `(member  (scanner-current-token scanner) ',firsts
                :test (function word-equal))))




;; (com.informatimago.rdp::find-rules (grammar-named 'normalized-encoding) 'request)
(defstruct sentence-node word attribute)

;; Not finished.
;; (defun attribute-rule (grammar nt item)
;;   (ecase item
;;     ((seq)  (loop
;;                :with result = (list (make-sentence-node :word nil :attribute (follow-set grammar nt)))
;;                :for word :in (reverse (second item))
;;                :do ))
;;     ((rep))
;;     ((opt))
;;     ((alt)))
;;   (if (null sentence)
;;       (list (make-sentence-node :word nil :attribute (follow-set grammar nt)))
;;       (let ((rest-sentence (process-sentence grammar nt (rest sentence))))
;;        (cons (make-sentence-node :word (first sentence)
;;                                  :attribute (if (nullablep grammar (first sentence))
;;                                                 (remove nil (union (first-set grammar (first sentence))
;;                                                                    (sentence-node-attribute (first rest-sentence))))
;;                                                 (first-set grammar (first sentence))))
;;              rest-sentence))))

(defun process-sentence (grammar nt sentence)
  (if (null sentence)
      (list (make-sentence-node :word nil :attribute (follow-set grammar nt)))
      (let ((rest-sentence (process-sentence grammar nt (rest sentence))))
       (cons (make-sentence-node :word (first sentence)
                                 :attribute (if (nullablep grammar (first sentence))
                                                (remove nil (union (first-set grammar (first sentence))
                                                                   (sentence-node-attribute (first rest-sentence))))
                                                (first-set grammar (first sentence))))
             rest-sentence))))


(defmethod gen-parsing-statement ((target (eql :lisp)) (grammar grammar) item)
  ;; If we want to generate the parser directly from the grammar with
  ;; seq/rep/opt/alt, then we need to replicate the algorithm for the
  ;; first-set of sentences here. :-(
  (labels ((es-first-set (extended-sentence)
             (if (atom extended-sentence)
                 (first-set grammar extended-sentence)
                 (ecase (car extended-sentence)
                   ((seq) (loop
                             :with all-firsts = '()
                             :for item :in (second extended-sentence)
                             :for firsts = (es-first-set item)
                             :do (setf all-firsts (union firsts (delete nil all-firsts)))
                             :while (member nil firsts)
                             :finally (return all-firsts)))
                   ((rep) (es-first-set (first (second extended-sentence))))
                   ((opt) (union '(nil) (es-first-set (first (second extended-sentence)))))
                   ((alt) (reduce (function union) (second extended-sentence)
                                  :key (function es-first-set)))))))
   (if (atom item)
       (if (terminalp grammar item)
           `(accept scanner ',item)
           (let* ((firsts (es-first-set item))
                  (emptyp (member nil firsts)))
             (if emptyp
                 `(when ,(gen-in-firsts target (remove nil firsts))
                    (,(gen-parse-function-name target grammar item) scanner))
                 `(if ,(gen-in-firsts target (remove nil firsts))
                      (,(gen-parse-function-name target grammar item) scanner)
                      (error 'unexpected-token-error
                             :line    (scanner-line scanner)
                             :column  (scanner-column scanner)
                             ;; :grammar (grammar-named ',(grammar-name grammar))
                             :scanner scanner
                             :non-terminal-stack (copy-list *non-terminal-stack*)
                             :format-control "Unexpected token ~S~%~S~%~{~A --> ~S~}"
                             :format-arguments (list
                                                (scanner-current-token scanner)
                                                *non-terminal-stack*
                                                ',(assoc item (grammar-rules grammar))))))))
       (ecase (car item)
         ((seq)
          (destructuring-bind (seq items actions) item
            (declare (ignore seq))
            (let ((dollars (loop
                             :for i :from 1 :to (length items)
                             :collect (intern (format nil "$~D" i))))
                  (ignorables '()))
              `(let ,(mapcar (lambda (dollar item)
                               `(,dollar ,(gen-parsing-statement target grammar item)))
                             dollars items)
                 (let (($0 (list ,@dollars))
                       ;; new:
                       ,@ (let ((increments (make-hash-table)))
                            (mapcan (lambda (dollar item)
                                      (when (and (symbolp item)
                                                 (or (non-terminal-p grammar item)
                                                     (terminalp grammar item)))
                                        (let* ((index  (incf (gethash item increments 0)))
                                               (igno   (intern (format nil "~:@(~A.~A~)" item index))))
                                          (pushnew item ignorables)
                                          (push    igno ignorables)
                                          (append (when (= 1 index)
                                                    (list (list item dollar)))
                                                  (list (list igno dollar))))))
                                    dollars items))
                       ;; ---
                       )
                   (declare (ignorable $0 ,@ignorables))
                   ,@actions)))))
         ((rep)
          `(loop
              :while ,(gen-in-firsts target (es-first-set (first (second item))))
              :collect ,(gen-parsing-statement target grammar (first (second item)))))
         ((opt)
          `(when ,(gen-in-firsts target (es-first-set (first (second item))))
             ,(gen-parsing-statement target grammar (first (second item)))))
         ((alt)
          `(cond
             ,@(mapcar (lambda (item)
                         `(,(gen-in-firsts target (es-first-set item))
                            ,(gen-parsing-statement target grammar item)))
                       (second item))))))))


(defmethod generate-nt-parser ((target (eql :lisp)) (grammar grammar) non-terminal &key (trace nil))
  (let* ((fname (gen-parse-function-name target grammar non-terminal))
         (form  `(defun ,fname (scanner)
                   ,(format nil "~S" (assoc non-terminal (grammar-rules grammar)))
                   (with-non-terminal ,non-terminal
                       ,(gen-parsing-statement target grammar (find-rule grammar non-terminal))))))
    (gen-trace fname `(progn (fmakunbound ',fname) ,form) trace)))


(defmethod generate-parser ((target (eql :lisp)) grammar &key (trace nil))
  (let* ((fname  (intern (format nil "~:@(PARSE-~A~)" (grammar-name grammar))))
         (form   `(defun ,fname (source)
                    "
SOURCE: When the grammar has a scanner generated, or a scanner class
        name, SOURCE can be either a string, or a stream that will be
        scanned with the generated scanner.  Otherwise, it should be a
        SCANNER instance.
"
                    (with-non-terminal ,(grammar-name grammar)
                      (let ((scanner ,(if (grammar-scanner grammar)
                                          `(make-instance ',(grammar-scanner grammar) :source source)
                                          'source)))
                        (advance-line scanner)
                        (prog1 (,(gen-parse-function-name target grammar (grammar-start grammar))
                                  scanner)
                          (unless (scanner-end-of-source-p scanner)
                            (error 'parser-end-of-source-not-reached
                                   :line (scanner-line scanner)
                                   :column (scanner-column scanner)
                                   :grammar (grammar-named ',(grammar-name grammar))
                                   :scanner scanner
                                   :non-terminal-stack (copy-list *non-terminal-stack*)))))))))
    (gen-trace fname `(progn (fmakunbound ',fname) ,form) trace)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
