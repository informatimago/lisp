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
;;;;    Copyright Pascal J. Bourguignon 2006 - 2016
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

(eval-when (:execute :compile-toplevel :load-toplevel)
  (setf *features* (cons :use-ppcre (set-difference *features* '(:use-ppcre :use-regexp)))))


(defpackage "COM.INFORMATIMAGO.RDP"
  (:use "COMMON-LISP")
  (:export "DEFGRAMMAR" "SEQ" "REP" "OPT" "ALT" "GRAMMAR-NAMED"
           "GENERATE-GRAMMAR"
           
           "GRAMMAR" "MAKE-GRAMMAR" "COPY-GRAMMAR"
           "GRAMMAR-NAME" "GRAMMAR-TERMINALS" "GRAMMAR-START" "GRAMMAR-RULES"
           "GRAMMAR-ALL-TERMINALS" "GRAMMAR-ALL-NON-TERMINALS"
           
           "FIND-RULE" "TERMINALP" "NON-TERMINAL-P" "FIRST-RHS" "FIRST-SET"

           "SCANNER" "MAKE-SCANNER" "COPY-SCANNER"
           "SCANNER-SOURCE" "SCANNER-FUNCTION" "SCANNER-POSITION"
           "SCANNER-CURRENT-TOKEN" "SCANNER-CURRENT-TEXT"
           "SCANNER-CURRENT-POSITION"
           "SCANNER-END-OF-SOURCE" "ACCEPT" "*SPACES*"
           ))
(in-package "COM.INFORMATIMAGO.RDP")



(defstruct grammar
  name terminals start rules
  all-terminals
  all-non-terminals)


(defvar *grammars* (make-hash-table)
  "Records the variables defined with DEFGRAMMAR.
Use (GRAMMAR-NAMED name) to look up a grammar.")

(defun grammar-named (name)
  "Returns the grammar named NAME, or NIL if none."
  (gethash name *grammars*))




(defgeneric generate-boilerplate (target-language grammar)
  (:documentation "Generate the boilerplate code needed by the scanner and parser.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate."))


(defgeneric generate-scanner     (target-language grammar)
    (:documentation "Generate the scanner code.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate."))


(defgeneric generate-nt-parser   (target-language grammar non-terminal)
    (:documentation "Generate the parser code for the given non-terminal.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate."))


(defgeneric generate-parser      (target-language grammar)
    (:documentation "Generate the toplevel parser code.

This code must be a single lisp form.  In the case of the :lisp
target-language, this form is the code of the boilerplate itself.  For
another language, this form is lisp code used to generate that other
language boilerplate."))




;;; First, we define a grammar, with actions.
;;; The scanner and parser is generated at macro expansion time.

(defvar *linenum* 0)


(defmacro defgrammar (name &key terminals start rules (target-language :lisp))
  "
DO:     This macros generates a simple scanner and recursive decent parser 
        for the language described by this grammar.
        For each <non-terminal> in the grammar, a function named
        <name>/PARSE-<non-terminal> is generated, in addition to 
        functions SCAN-<name> and PARSE-<name>.
        The grammar structure is also generated for run-time
        in the global special variable <name>.

TARGET-LANGUAGE:

        Specifies the language into which the code is generated, as a
        keyword.  The actions must still be written as lisp
        expressions, but to generate the language specified.  There
        must be a set of methods specialized on this target-language
        keyword.

SYNTAX:

    (defgrammar <name>
        :terminals (( <terminal>       \"regexp\") ...)
        :start        <non-terminal>
        :rules     ((--> <non-terminal> <items> ) ...))

    <items>            ::= | <item> <items>
    <item>             ::= <seq> | <rep> | <alt> | <opt>
                         | <non-terminal> | <literal-terminal> | <terminal>
    <seq>              ::= (SEQ <item> <items> <action>)
    <rep>              ::= (REP <item> <items> <action>)
    <opt>              ::= (OPT <item> <items> <action>)
    <alt>              ::= (ALT <item> <items>)
    <action>           ::= | :ACTION <forms>
    <forms>            ::= | <form> <forms>
    <form>             ::= form        -- any lisp form.
    <non-terminal>     ::= symbol      -- any lisp symbol (keywords reserved).
    <terminal>         ::= symbol      -- any lisp symbol (keywords reserved).
    <literal-terminal> ::= string      -- any lisp string.

SEMANTICS:

        The terminals are either named terminals listed in the :TERMINALS
        clause, or literal terminals written directly in the productions as 
        lisp strings.  They are matched as-is.

        An extended regular expression regex(7) may be given that
        will be matched by the scanner to infer the given terminal.
        The literal terminals are matched first, the longest first,
        and with ([A-Za-z0-9]|$) appended to terminals ending in a
        letter or digit (so that they don't match when part of a
        longer identifier).
        Then the regular expressions are matched in the order given.
         
        :START specifies the start non-terminal symbol.

        The non-terminal symbols are infered implicitely from the grammar rules.

        If there are more than one subforms, or an action,
        the REP and OPT forms take an implicit SEQ:
          (REP a b c :ACTION f)   -->  (REP (SEQ a b c :ACTION f))
          (OPT a b c :ACTION f)   -->  (OPT (SEQ a b c :ACTION f))
          (REP a :ACTION f)       -->  (REP (SEQ a :ACTION f))
          (OPT a :ACTION f)       -->  (OPT (SEQ a :ACTION f))
          (REP a)                 -->  (REP a)
          (OPT a)                 -->  (OPT a)

        Embedded ALT are flattened:
          (ALT a b (ALT c d) e f) --> (ALT a b c d e f)

        Actions are executed in a lexical environment where the symbols $1, $2,
        etc are bound to the results of the subforms. $0 is bound to the 
        list of the results of the subforms.
        
        The action for REP (normalized) is to return a possibly
        empty list of the results of its single subform repeated.
        (REP is 0 or more).

        The action for OPT (normalized) is to return either NIL,
        or the result of its single subform unchanged.

        The action for an ALT is to return the result of the selected 
        alternative unchanged.

        The default action for an internal SEQ is to return the list of the
        results of its subforms.

        The default action for an implicit SEQ for a given <non-terminal> lhs
        is to return the list of the results of its subforms prefixed by the
        <non-terminal> symbol.

TODO:   We could also flatten sequences without action, or even sequences with
        actions with renumbering.
"
  (let ((grammar (make-grammar :name name
                               :terminals terminals
                               :start start
                               :rules (normalize-rules rules)))
        (*linenum* 0)
        (g (gensym)))
    (compute-all-terminals     grammar)
    (compute-all-non-terminals grammar)
    `(progn
       (setf (gethash ',name *grammars*)
             (let ((,g (make-grammar
                        :name ',name
                        :terminals ',terminals
                        :start ',start
                        :rules ',(normalize-rules rules))))
               (compute-all-terminals     ,g)
               (compute-all-non-terminals ,g)
               ,g))
       
       ,(generate-boilerplate target-language grammar)
       ,(generate-scanner target-language grammar)
       ,@(mapcar (lambda (non-terminal)
                   (generate-nt-parser target-language grammar non-terminal))
                 (grammar-all-non-terminals grammar))
       ,(generate-parser target-language grammar))))



(defun generate-grammar (name &key terminals start rules (target-language :lisp) compile)
  "
SEE ALSO:   The docstring of DEFGRAMMAR.
DO:         This function defines and generate the grammar object at run-time.
RETURN:     The grammar object.
NOTE:       The grammar is not added to the *grammars* map.
"
  (let ((grammar (make-grammar :name name
                               :terminals terminals
                               :start start
                               :rules (normalize-rules rules)))
        (*linenum* 0))
    (compute-all-terminals     grammar)
    (compute-all-non-terminals grammar)
    (let ((code  `(progn
                    ,(generate-boilerplate target-language grammar)
                    ,(generate-scanner target-language grammar)
                    ,@(mapcar (lambda (non-terminal)
                                (generate-nt-parser target-language grammar non-terminal))
                              (grammar-all-non-terminals grammar))
                    ,(generate-parser target-language grammar))))
     (if compile
         (funcall #+mocl (coerce `(lambda () ,code) 'function)
                  #-mocl (compile nil `(lambda () ,code)))
         (eval code)))
    grammar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Normalization of  the grammar rules:
;;;

(declaim (inline split-action))
(defun split-action (rhs)
  (let ((separator (position :action rhs)))
    (if separator
        (values (subseq rhs 0 separator) (subseq rhs (1+ separator)))
        (values rhs                      nil))))


(defun normalize-seq (expr)
  (multiple-value-bind (rhs actions) (split-action (cdr expr))
    (setf actions (or actions '($0)))
    (let ((items (mapcar (lambda (item) (normalize item)) rhs)))
      (if (and (null actions) (or (null items) (null (cdr items))))
          (car items)
          (list 'seq items actions)))))

(defun normalize-with-action (expr)
  (multiple-value-bind (rhs actions) (split-action (cdr expr))
    (if (null actions)
        (cond ((null rhs) nil)
              ((null (cdr rhs)) `(,(car expr) ,(normalize (car rhs))))
              (t `(,(car expr) ,(normalize-seq `(seq ,@rhs)))))
         `(,(car expr) ,(normalize-seq `(seq ,@rhs :action ,@actions))))))

(defun normalize-rep (expr) (normalize-with-action expr))
(defun normalize-opt (expr) (normalize-with-action expr))

(defun normalize-alt (expr)
  (assert (not (find :action expr)))
  (let ((items (mapcar (function normalize) (cdr expr))))
    (if (null (cdr items))
        (car items)
        `(alt ,@(mapcan (lambda (item)
                          (cond ((atom item) (list item))
                                ((eql 'alt (car item)) (cdr items))
                                (t (list item))))
                        items)))))

(defun normalize (expr)
  (if (atom expr)
      expr
      (ecase (car expr)
        ((seq) (normalize-seq expr))
        ((rep) (normalize-rep expr))
        ((alt) (normalize-alt expr))
        ((opt) (normalize-opt expr)))))

(defun normalize-rules (rules)
  (mapcar (lambda (rule)
            (destructuring-bind (--> non-term &rest items) rule
               (assert (string= --> '-->))
               `(,non-term ,(normalize
                             (if (find :action items)
                                 `(seq ,@items)
                                 `(seq ,@items :action `(,',non-term ,@$0)))))))
          rules))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-equal (a b)
  (or (and (stringp a) (stringp b) (string= a b))
      (eql a b)))

(defun compute-all-terminals (grammar)
  (labels  ((find-strings (items)
              (cond
                ((stringp items) (list items))
                ((atom items)    '())
                (t (ecase (car items)
                     ((seq)
                      (mapcan (function find-strings) (second items)))
                     ((rep alt opt)
                      (mapcan (function find-strings) (cdr items))))))))
    (setf (grammar-all-terminals grammar)
          (delete-duplicates
           (append
            (mapcar (function first) (grammar-terminals grammar))
            (mapcan (function find-strings)
                    (mapcar (function second) (grammar-rules grammar))))
           :test (function word-equal)))))


(defun compute-all-non-terminals (grammar)
  (labels ((find-symbols (items)
             (cond
               ((symbolp items) (list items))
               ((atom items)    '())
               (t (ecase (car items)
                    ((seq)
                     (mapcan (function find-symbols) (second items)))
                    ((rep alt opt)
                     (mapcan (function find-symbols) (cdr items))))))))
    (setf (grammar-all-non-terminals grammar)
          (set-difference
           (delete-duplicates
            (append
             (list (grammar-start grammar))
             (mapcar (function first) (grammar-rules grammar))
             (mapcan (function find-symbols)
                     (mapcar (function second) (grammar-rules grammar)))))
           (grammar-all-terminals grammar)))))


;;; To implement the follow-set function we'd need to put the grammar
;;; into a normal form, which we don't really need to map it simplistically
;;; to recursive descent parser functions.

;; (defun follow-set (grammar non-terminal)
;;   "return the set of terminal symbols that may follow the non-terminal
;; in the grammar."
;;   (mapcar (lambda (rule)
;;             (destructuring-bind (nt expr) rule
;; 
;;               ))
;;           (grammar-rules grammar)))


(defun find-rule (grammar non-terminal)
  (let ((rules (mapcar (function second)
                       (remove-if-not (lambda (rule) (eql non-terminal (first rule)))
                                      (grammar-rules grammar)))))
    (cond
      ((null rules) (error "~s is not a non-terminal" non-terminal))
      ((null (cdr rules)) (car rules))
      (t `(alt ,@(normalize-alt rules))))))


(defun terminalp (grammar item)
  (member item (grammar-all-terminals grammar)
          :test (function word-equal)))

(defun non-terminal-p (grammar item)
  (member item (grammar-all-non-terminals grammar)))


(defun first-rhs (grammar item)
  (if (atom item)
      (if (terminalp grammar item)
          (list item)
          (first-set grammar item))
      (ecase (car item)
        ((seq) (loop
                  :with all-firsts = '()
                  :for items :in (second item)
                  :for firsts = (first-rhs grammar items)
                  :while (member nil firsts)
                  :do (setf all-firsts
                            (union firsts (delete nil all-firsts)))
                  :finally (setf all-firsts
                                 (union firsts (delete nil all-firsts)))
                  (return all-firsts)))
        ((rep opt) (cons nil (first-rhs grammar (second item))))
        ((alt) (mapcan (lambda (item) (first-rhs grammar item)) (rest item))))))


(defun first-set (grammar non-terminal)
  "return the set of terminal symbols by which the non-terminal may start
in the grammar."
  (delete-duplicates (first-rhs grammar (find-rule grammar non-terminal))
                       :test (function word-equal)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Regexp Abstraction Layer.
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

(defmethod generate-boilerplate ((target (eql :lisp)) (grammar grammar))
  (declare (ignore grammar))
  `(progn
     
    (defstruct scanner
      source
      function
      (position 0)
      (current-token nil)
      (current-text "")
      (current-position 0))

    (defun scanner-end-of-source (scanner)
      (<= (length (scanner-source scanner)) (scanner-position scanner)))

    (defun accept (scanner token)
      (if (word-equal token (scanner-current-token scanner))
          (prog1 (list (scanner-current-token scanner)
                       (scanner-current-text scanner)
                       (scanner-current-position scanner))
            (funcall (scanner-function scanner) scanner))
          (error "At position ~D, expected ~S, not ~S"
                 (scanner-current-position scanner)
                 token
                 (scanner-current-token scanner))))

    (defparameter *spaces*
      (format nil "^([~{~C~}]+)" '(#\space #\newline #\tab)))))


(defmethod gen-scanner-function-name ((target (eql :lisp)) (grammar grammar))
  (intern (format nil "~:@(SCAN-~A~)" (grammar-name grammar))))



(defmethod generate-scanner ((target (eql :lisp)) grammar)
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
  (let* ((an-terminals  (sort (remove-if-not
                               (lambda (item)
                                 (and (stringp item)
                                      (alphanumericp (aref item (1- (length item))))))
                               (grammar-all-terminals grammar))
                              (function >) :key (function length)))
         (nan-terminals (sort (remove-if
                               (lambda (item)
                                 (or (not (stringp item))
                                     (alphanumericp (aref item (1- (length item))))))
                               (grammar-all-terminals grammar))
                              (function >) :key (function length)))
         (nl-terminals (remove-if (function stringp) (grammar-terminals grammar)))
         (lit-an-terminals-regexp
          (format nil "^(~{~A~^|~})([^A-Za-z0-9]|$)"
                  (mapcar (function regexp-quote-extended) an-terminals)))
         (lit-nan-terminals-regexp
          (format nil "^(~{~A~^|~})"
                  (mapcar (function regexp-quote-extended)  nan-terminals))))
    `(defun ,(gen-scanner-function-name target grammar) (scanner)
       (let ((match (string-match *spaces*
                                  (scanner-source scanner)
                                  :start (scanner-position scanner))))
         (when match
           (setf (scanner-position scanner) (match-end 1 match)))
         (setf (scanner-current-position scanner) (scanner-position scanner))
         (cond
           ((scanner-end-of-source scanner)
            (setf (scanner-position scanner)   (length (scanner-source scanner))
                  (scanner-current-text scanner)  "<END OF SOURCE>"
                  (scanner-current-token scanner) nil))
           ,@(when (or an-terminals nan-terminals)
                   `(((or ,@(when an-terminals
                                  `((setf match (string-match ',lit-an-terminals-regexp
                                                             (scanner-source scanner)
                                                             :start (scanner-position scanner)))))
                          ,@(when nan-terminals
                                  `((setf match (string-match ',lit-nan-terminals-regexp
                                                             (scanner-source scanner)
                                                             :start (scanner-position scanner))))))
                      (setf (scanner-position scanner)      (match-end 1 match)
                            (scanner-current-text scanner)  (match-string 1 (scanner-source scanner) match)
                            (scanner-current-token scanner) (scanner-current-text scanner)))))
           ,@(mapcar
              (lambda (terminal)
                `((setf match (string-match
                               ',(format nil "^(~A)" (second terminal))
                               (scanner-source scanner)
                               :start (scanner-position scanner)))
                  (setf (scanner-position scanner)      (match-end 1 match)
                        (scanner-current-text scanner)  (match-string 1 (scanner-source scanner) match)
                        (scanner-current-token scanner) ',(first terminal))))
              nl-terminals)
           (t (error "Invalid character ~C at position: ~D"
                     (aref (scanner-source scanner) (scanner-position scanner))
                     (scanner-position scanner))))))))


(defmethod gen-parse-function-name ((target (eql :lisp)) (grammar grammar) non-terminal)
  (intern (format nil "~:@(~A/PARSE-~A~)" (grammar-name grammar) non-terminal)))

(defmethod gen-in-firsts ((target (eql :lisp)) firsts)
  (if (null (cdr firsts))
      `(word-equal (scanner-current-token scanner) ',(car firsts))
      `(member  (scanner-current-token scanner) ',firsts
                :test (function word-equal))))

(defmethod gen-parsing-statement ((target (eql :lisp)) (grammar grammar) item)
  (if (atom item)
      (if (terminalp grammar item)
          `(accept scanner ',item)
          (let* ((firsts (first-rhs grammar item))
                 (emptyp (member nil firsts)))
            `(,(if emptyp 'when 'if) ,(gen-in-firsts target (remove nil firsts))
               (,(gen-parse-function-name target grammar item) scanner)
               ,@(unless emptyp
                         '((error "Unexpected token ~S"
                            (scanner-current-token scanner)))))))
      (ecase (car item)
        ((seq)
         (destructuring-bind (seq items actions) item
           (declare (ignore seq))
           (let ((index 0))
             `(let ,(mapcar (lambda (item)
                              `(,(intern (format nil "$~D" (incf index)))
                                 ,(gen-parsing-statement target grammar item)))
                            items)
                (let (($0 (list ,@(loop :for i :from 1 :to index
                                     :collect (intern (format nil "$~D" i))))))
                  ,@actions)))))
        ((rep)
         `(loop
             :while ,(gen-in-firsts target (first-rhs grammar (second item)))
             :collect ,(gen-parsing-statement target grammar (second item))))
        ((opt)
         `(when ,(gen-in-firsts target (first-rhs grammar (second item)))
            ,(gen-parsing-statement target grammar (second item))))
        ((alt)
         `(cond
            ,@(mapcar (lambda (item)
                        `(,(gen-in-firsts target (first-rhs grammar item))
                           ,(gen-parsing-statement target grammar item)))
                      (cdr item)))))))


(defmethod generate-nt-parser ((target (eql :lisp)) (grammar grammar) non-terminal)
  `(defun ,(gen-parse-function-name target grammar non-terminal) (scanner)
     ,(gen-parsing-statement target grammar  (find-rule grammar non-terminal))))


(defmethod generate-parser ((target (eql :lisp)) grammar)
  (let ((scanner-function (gen-scanner-function-name target grammar)))
    `(defun ; ,(gen-parse-function-name target grammar (grammar-name grammar))
         ,(intern (format nil "~:@(PARSE-~A~)" (grammar-name grammar)))
         (source)
       (let ((scanner (make-scanner :source source
                                    :function (function ,scanner-function))))
         (,scanner-function scanner)
         (prog1 (,(gen-parse-function-name target grammar (grammar-start grammar))
                  scanner)
           (unless (scanner-end-of-source scanner)
             (error "End of source NOT reached.")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
