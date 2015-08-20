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
  name terminals start rules
  all-terminals
  all-non-terminals
  ;; ---
  (scanner     t)
  (skip-spaces t)
  ;; --- computed:
  first-function
  follow-function)

(defun dump-grammar (grammar)
  (format t "(defgrammar ~S~%" (grammar-name grammar))
  (format t "  :terminals ~S~%" (grammar-terminals grammar))
  (format t "  :start ~S~%" (grammar-start grammar))
  (format t "  :rules ~S~%" (grammar-rules grammar))
  (format t "  :all-terminals ~S~%" (grammar-all-terminals grammar))
  (format t "  :all-non-terminals ~S~%" (grammar-all-non-terminals grammar))
  (format t "  :scanner ~S~%" (grammar-scanner grammar))
  (format t "  :skip-spaces ~S~%" (grammar-skip-spaces grammar))
  (format t "  :first-function ~S~%" (grammar-first-function grammar))
  (format t "  :follow-function ~S~%" (grammar-follow-function grammar))
  (format t ")~%")
  grammar)

(defvar *grammars* (make-hash-table)
  "Records the variables defined with DEFGRAMMAR.
Use (GRAMMAR-NAMED name) to look up a grammar.")

(defun grammar-named (name)
  "Returns the grammar named NAME, or NIL if none."
  (gethash name *grammars*))


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

")


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
         (grammar     (make-normalized-grammar
                       :name name
                       :terminals terminals
                       :start start
                       :rules clean-rules
                       :scanner scanner
                       :skip-spaces skip-spaces))
         (*linenum* 0)
         (ng (gensym "ng")))
    (setf (gethash (setf (grammar-name grammar) name) *grammars*) grammar)
    (compute-first-function  grammar)
    (compute-follow-function grammar)
    
    `(progn

       (make-normalized-grammar
        :name ',name
        :terminals ',terminals
        :start ',start
        :rules ',clean-rules
        :scanner ',scanner
        :skip-spaces ',skip-spaces)
       
       ,(generate-boilerplate          target-language grammar :trace trace)         
       ,(generate-scanner-for-grammar  target-language grammar :trace trace)

       ,@(mapcar (lambda (non-terminal)
                   ;; (generate-nt-parser target-language grammar non-terminal  :trace trace)
                   (generate-normalized-nt-parser target-language grammar non-terminal :trace trace))
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

(defun dollar (n) (intern (format nil "$~D" n)))

(defun clean-seq (expr)
  (multiple-value-bind (rhs actions) (split-action (cdr expr))
    (setf actions (or actions `(,(dollar 0))))
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
              (assert (string= --> '-->) () "Rules should be written as (--> <non-terminal> <rhs> [:action <form>...])~%~
                                            Invalid rule: ~S" rule)
              `(,non-term ,(clean
                            (if (find :action items)
                                `(seq ,@items)
                                `(seq ,@items :action `(,',non-term ,@,(dollar 0))))))))
          rules))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defun compute-first-sets (grammar)
  "  
PRE:    The GRAMMAR must be normalized.
        (ie. contains only SEQ rules)

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
                               :for itemus :in item
                               :for first-set = (first-set itemus)
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
               #-(and)
               (dolist (non-terminal non-terminals follow-sets)
                 (setf (gethash non-terminal follow-sets)
                       (gethash non-terminal normalized-follow-sets)))
               normalized-follow-sets)
             (compute-follow-sets grammar))))
    ;; build the resulting function.
    (setf (grammar-follow-function grammar)
          (lambda (non-terminal)
            (if (eql non-terminal :get-table)
                follow-sets
                (or (gethash non-terminal follow-sets)
                    (error "~S is not a non-terminal of the grammar ~A"
                           non-terminal (grammar-name grammar))))))))



(defun compute-first-follow (grammar &optional (ng (normalize-grammar grammar)))
  (setf (grammar-first-function  grammar) (compute-first-function ng)
        (grammar-follow-function grammar) (compute-follow-function ng (grammar-all-non-terminals grammar)))
  grammar)
 
(defun first-set (grammar symbol)
  (unless (grammar-first-function grammar)
    (compute-first-follow grammar))
  (funcall (grammar-first-function grammar) symbol))

(defun follow-set (grammar non-terminal)
  (unless (grammar-follow-function grammar)
    (compute-first-follow grammar))
  (funcall (grammar-follow-function grammar) non-terminal))


(defun sentence-first-set (grammar non-terminal sentence)
  (let ((fs (first-set grammar (first sentence))))
    (if (member nil fs)
        (remove-duplicates
         (union (if sentence
                    (sentence-first-set grammar non-terminal (rest sentence))
                    (follow-set grammar non-terminal))
                (remove nil fs)))
        fs)))

;;; Follow set.
;;; Normalization of the grammar.
;;; Each rule is put under the form:  a --> ε   or   a --> e a


(defun make-new-non-terminal (base-nt non-terminals)
  (loop
     :for i :from 1
     :for new-nt = (intern (format nil "~A-~A" base-nt i))
     :while (member new-nt non-terminals)
     :finally (return new-nt)))




(defparameter *empty-rule* '(seq () ('())))

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
                            (new-rule (list nt *empty-rule*))
                            (list nt `(seq (,(process-item (first items)) ,nt)
                                           ((cons ,(dollar 1) ,(dollar 2)))))))
                         ((opt)
                          ;; a --> (opt e)
                          ;; -------------
                          ;; a --> ε :action '()
                          ;; a --> e :action (list $1)
                          (destructuring-bind (op items) rhs
                            (declare (ignore op))
                            (new-rule (list nt *empty-rule*))
                            (list nt `(seq (,(process-item (first items)))
                                           ((list ,(dollar 1)))))))
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
                                                (list nt `(seq (,(first new-items)) (,(dollar 1)))))
                                (new-rule (list nt `(seq (,new-item) (,(dollar 1)))))))))))))))
   non-terminals))



(defun make-normalized-grammar (&key name terminals start (scanner t) rules (skip-spaces t))
  "Return a new normalized grammar parsing the same language as GRAMMAR."
  (let ((new-grammar (make-grammar
                      :name name
                      :terminals terminals
                      :start start
                      :scanner scanner
                      :rules rules
                      :skip-spaces skip-spaces)))
    (setf (gethash (grammar-name new-grammar) *grammars*) new-grammar)
    (compute-all-terminals     new-grammar)
    (compute-all-non-terminals new-grammar)
    (setf (grammar-rules new-grammar) (normalize-grammar-rules rules (grammar-all-non-terminals new-grammar)))
    (compute-all-non-terminals new-grammar)
    (compute-first-function    new-grammar)
    (compute-follow-function   new-grammar)
    new-grammar))


(defun normalize-grammar (grammar)
  "Return a new normalized grammar parsing the same language as GRAMMAR."
  (let ((new-grammar (make-grammar
                      :name (intern (format nil "NORMALIZED-~A"
                                            (grammar-name grammar)))
                      :terminals (grammar-terminals grammar)
                      :start (grammar-start grammar)
                      :scanner (grammar-scanner gramar)
                      :rules (normalize-grammar-rules (grammar-rules grammar)
                                                      (grammar-all-non-terminals grammar))
                      :skip-spaces (grammar-skip-spaces grammar))))
    (setf (gethash (grammar-name new-grammar) *grammars*) new-grammar)
    (compute-all-terminals     new-grammar)
    (compute-all-non-terminals new-grammar)
    new-grammar))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generator -- LISP
;;;

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


;; (com.informatimago.rdp::find-rules (grammar-named 'normalized-encoding) 'request)

;; (defstruct sentence-node word attribute)
;;
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
;;
;; (defun process-sentence (grammar nt sentence)
;;   (if (null sentence)
;;       (list (make-sentence-node :word nil :attribute (follow-set grammar nt)))
;;       (let ((rest-sentence (process-sentence grammar nt (rest sentence))))
;;        (cons (make-sentence-node :word (first sentence)
;;                                  :attribute (if (nullablep grammar (first sentence))
;;                                                 (remove nil (union (first-set grammar (first sentence))
;;                                                                    (sentence-node-attribute (first rest-sentence))))
;;                                                 (first-set grammar (first sentence))))
;;              rest-sentence))))


;;;------------------------------------------------------------
;;; RDP Parser Generator
;;;------------------------------------------------------------

(defvar *non-terminal-stack* '()
  "For error reporting.")

(defmacro with-non-terminal (non-terminal &body body)
  `(let ((*non-terminal-stack* (cons ',non-terminal *non-terminal-stack*)))
     ;; (print *non-terminal-stack*)
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
  (intern (format nil "~:@(~A/PARSE-~A~)" (grammar-name grammar) non-terminal)))

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
            (let* ((firsts          (es-first-set item))
                   (emptyp          (member nil firsts))
                   (expected-tokens (remove nil firsts)))
              (if emptyp
                  `(when ,(gen-in-firsts target expected-tokens)
                     (,(gen-parse-function-name target grammar item) scanner))
                  `(retrying-until ,(gen-in-firsts target expected-tokens)
                                   (,(gen-parse-function-name target grammar item) scanner)
                                   ',expected-tokens
                                   ',(assoc item (grammar-rules grammar))))))
        (ecase (car item)
          ((seq)
           (destructuring-bind (seq items actions) item
             (declare (ignore seq))
             (let ((dollars (loop :for i :from 1 :to (length items)
                                  :collect (dollar i)))
                   (ignorables '()))
               `(let ,(mapcar (lambda (dollar item)
                                `(,dollar ,(gen-parsing-statement target grammar item)))
                       dollars items)
                  (let ((,(dollar 0) (list ,@dollars))
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
                    (declare (ignorable ,(dollar 0) ,@ignorables))
                    ,@actions)))))
          ((rep)
           `(loop
              :while ,(gen-in-firsts target (es-first-set (first (second item))))
              :collect ,(gen-parsing-statement target grammar (first (second item)))))
          ((opt)
           `(when ,(gen-in-firsts target (es-first-set (first (second item))))
              ;;  ,(gen-parsing-statement target grammar (first (second item)))
              (,(gen-parse-function-name target grammar (first (second item))) scanner)))
          ((alt)
           `(cond
              ,@(mapcar (lambda (item)
                          `(,(gen-in-firsts target (es-first-set item))
                            ;; ,(gen-parsing-statement target grammar item)
                            (,(gen-parse-function-name target grammar item) scanner)))
                        (second item))
              (t
               (error-unexpected-token scanner
                                       ',(mapcan (lambda (item)
                                                   (copy-list (es-first-set item)))
                                                 (second item))
                                       ',(assoc item (grammar-rules grammar))))))))))


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
                            (cerror "Continue"
                                    'parser-end-of-source-not-reached
                                    :file (scanner-file scanner)
                                    :line (scanner-line scanner)
                                    :column (scanner-column scanner)
                                    :grammar (grammar-named ',(grammar-name grammar))
                                    :scanner scanner
                                    :non-terminal-stack (copy-list *non-terminal-stack*)))))))))
    (gen-trace fname `(progn (fmakunbound ',fname) ,form) trace)))


(defun empty-rule-p (rule)
  (equalp rule *empty-rule*))

(defun right-recursive-rule-p (rule non-terminal)
  (and (listp rule)
       (eql 'seq (first rule))
       (eql non-terminal (first (last (second rule))))))

(defmethod generate-normalized-nt-parser ((target (eql :lisp)) (grammar grammar) non-terminal &key (trace nil))
  (let* ((fname (gen-parse-function-name target grammar non-terminal))
         (rules (find-rules grammar non-terminal)))
    (flet ((generic ()
             `(cond ,@(mapcar (lambda (rule)
                                (let ((fs (sentence-first-set grammar non-terminal (second rule))))
                                  `(,(gen-in-firsts target fs)
                                    ,(gen-parsing-statement target grammar rule))))
                              rules)))
           (generate-parse-loop (rule)
             (destructuring-bind (non-terminal sentence actions) rule
               (let ((item (first sentence))
                     (fs (sentence-first-set grammar non-terminal (second rule))))
                 `(loop :while ,(gen-in-firsts target fs)
                        :collect ,(gen-parsing-statement target grammar item))))))
      (let* ((body  (case (length rules)
                      ((0) (error "Non-terminal ~S has no rule in grammar ~S" non-terminal (grammar-name grammar)))
                      ((1) (gen-parsing-statement target grammar (first rules)))
                      ((2) (cond
                             ((and (empty-rule-p (first rules))
                                   (right-recursive-rule-p (second rules) non-terminal))
                              (generate-parse-loop (second rules)))
                             ((and (empty-rule-p (second rules))
                                   (right-recursive-rule-p (first  rules) non-terminal))
                              (generate-parse-loop (first rules)))
                             (t
                              (generic))))
                      (otherwise
                       (generic))))
             (form  `(defun ,fname (scanner)
                       ,(format nil "~S" (assoc non-terminal (grammar-rules grammar)))
                       (with-non-terminal ,non-terminal
                         ,body))))
        (gen-trace fname `(progn (fmakunbound ',fname) ,form) trace)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
