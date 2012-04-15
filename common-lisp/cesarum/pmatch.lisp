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
;;;;    2012-02-03 <PJB> Added match-case*.
;;;;    2003-12-17 <PJB> Created.
;;;;BUGS
;;;;    pattern matcher and instantiation won't work with arrays/matrices,
;;;;    structures...
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PMATCH"
  (:use "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export "MATCH-CASE" "MATCH-CASE*" "COLLECT-VARIABLES"
           ":" "?/" "?*" "?+" "??" "?N" "?X" "?C" "?V" "?AX" "?AC" "?AV"
           "MATCH-DICT-MAP" "MATCH-STATE-DICT"
           "MATCH-STATE-FAILED-P" "MATCH")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "WITH-GENSYMS")
  (:documentation
   "Sexp Pattern Matcher

    Copyright Pascal J. Bourguignon 2003 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PMATCH")




(defun make-match-state (&key dict) dict)
(defun match-state-dict (ms) ms)

(defun match-state-fail     (ms raison)
  "PRIVATE"
  (make-match-state :dict  (list* :failed raison (match-state-dict ms))))


(defun match-state-failed-p (ms)
  "
RETURN: Whether the match failed.
"
  (eq :failed (car (match-state-dict ms))) )


(defun match-state-retry    (ms)
  "PRIVATE" 
  (if (match-state-failed-p ms)
      (make-match-state :dict (cdr (match-state-dict ms)))
      ms))


(defun match-dict-map (ms function)
  "
DO:     Calls FUNCTION (lambda (symbol value) ...) with all successive bindings,
        (unless matching state is failed).
RETURN: The list of results of the FUNCTION.
"
  (unless (match-state-failed-p ms)
    (mapcar (lambda (binding) (funcall function (first binding) (second binding)))
            (match-state-dict ms))))


(defun match-dict-add-binding (ms pat exp)
  "PRIVATE" 
  (let* ((var (second (car pat)))
         (val (car exp))
         (ass (assoc var (match-state-dict ms))))
    (cond
      ((null ass)                       ; a new binding:
       (make-match-state :dict (cons (cons var val) (match-state-dict ms))))
      ((equalp (cdr ass) val)           ; already there, same
       ms)
      (t                                ; already there, different
       (match-state-fail ms `(:mismatching-binding ,ass ,val))))))


(defmacro defpattern (name pattern)
  (if (symbolp pattern)
      `(defun ,name (pat) ;; (?ac ...)
         "PRIVATE"
         (and (listp pat) (symbolp (car pat))
              (string= ',pattern (car pat))))
      `(defun ,name (pat) ;; ((?n ...)...)
         "PRIVATE" 
         (and (listp pat) (listp (car pat)) (symbolp (caar pat))
              (string= ',(car pattern) (caar pat))))))


(defpattern pat-anonymous-variable-p   ?av)
(defpattern pat-anonymous-constant-p   ?ac)
(defpattern pat-anonymous-expression-p ?ax)
(defpattern pat-named-p                (?n))
(defpattern pat-variable-p             (?v))
(defpattern pat-constant-p             (?c))
(defpattern pat-expression-p           (?x))
(defpattern pat-optional-p             (??))
(defpattern pat-repeat-p               (?+))
(defpattern pat-optional-repeat-p      (?*))
(defpattern pat-alternative-p          (?/))
(defpattern pat-squeleton-eval-p       |:|)


(defun exp-variable-p (exp)
  "PRIVATE"
  (and (consp exp)
       (symbolp   (car exp))))

(defun exp-constant-p (exp)
  "PRIVATE"
  (and (consp exp)
       (atom (car exp))
       (not (symbolp (car exp)))))


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
  (do ((rest  exp (cdr rest)) ;; what should match after
       (list  '())            ;; what we match (reversed),
       ;;             we reverse only in the final result [F].
       (frame '()))
      ((null rest)
       (push (list list rest) frame)
       frame)
    (push (list list rest) frame)
    (push (car rest) list)))


(defun match (pat exp &optional (ms (make-match-state)))
  "
DO:        A pattern matcher accepting the following syntax:
             ?av        expects a symbol (variable).
             ?ac        expects a constant (non symbol atom).
             ?ax        expects anything (one item).
             (?v n)     expects a symbol (variable)     and bind it.
             (?c n)     expects a constant (non symbol atom)  and bind it.
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
  (cond
    ((match-state-failed-p ms) ms)
    ((atom pat)
     (if (equal pat exp)
         ms
         (match-state-fail ms `(:different ,pat ,exp))))
    ((pat-anonymous-constant-p pat)
     (if (exp-constant-p exp)
         (match (cdr pat) (cdr exp) ms)
         (match-state-fail ms `(:not-constant ,exp))))
    ((pat-anonymous-variable-p pat)
     (if (exp-variable-p exp)
         (match (cdr pat) (cdr exp) ms)
         (match-state-fail ms `(:not-variable ,exp))))
    ((pat-anonymous-expression-p pat)
     (if (null exp)
         (match-state-fail ms `(:missing-expression))
         (match (cdr pat) (cdr exp) ms)))
    ((pat-constant-p pat)
     (if (exp-constant-p exp)
         (match (cdr pat) (cdr exp) (match-dict-add-binding ms pat exp))
         (match-state-fail ms `(:not-constant ,exp))))
    ((pat-variable-p pat)
     (if (exp-variable-p exp)
         (match (cdr pat) (cdr exp) (match-dict-add-binding ms pat exp))
         (match-state-fail ms `(:not-variable ,exp))))
    ((pat-expression-p pat)
     (if (null exp)
         (match-state-fail ms `(:missing-expression))
         (match (cdr pat) (cdr exp) (match-dict-add-binding ms pat exp)) ))
    ((pat-named-p pat)
     (loop
        for (list rest) in (generate-all-follows exp)
        for soe = (match (cdr pat) rest ms)
        for nms = (if (match-state-failed-p soe)
                      soe
                      (let* ((list (reverse list))
                             (nms (match (cddar pat) list soe)))
                        (if (match-state-failed-p nms)
                            nms
                            (match-dict-add-binding nms pat (list list)))))
        while (match-state-failed-p nms)
        finally (return nms)))
    ((and (pat-repeat-p pat) (null exp))
     (match-state-fail ms `(:missing-repeat ,pat)))
    ((or (pat-repeat-p pat) (pat-optional-repeat-p pat) (pat-optional-p pat))
     (loop
        for (list rest) in (generate-all-follows exp)
        for soe = (match (cdr pat) rest ms)
        for nms = (if (match-state-failed-p soe)
                      soe
                      (cond
                        ((pat-repeat-p pat)
                         ;; at least one (...2... already matches)
                         ;; ((?+ ...1...) ...2...)
                         ;; --> (...1... (?* ...1...) ...2...)
                         (match (append (cdar pat) (list (cons '?* (cdar pat))))
                                (reverse list) soe))
                        ((pat-optional-repeat-p pat)
                         ;; zero or more (...2... already matches)
                         ;; ((?* ...1...) ...2...)
                         ;; --> (...1... (?* ...1...) ...2...)
                         ;; --> (...2...)
                         (let ((nms (match (append (cdar pat) (list (car pat)))
                                           (reverse list) soe)))
                           (if (match-state-failed-p nms)
                               (match nil list soe)
                               nms)))
                        ((pat-optional-p pat)
                         ;; zero or one (...2... already matches)
                         ;; ((?? ...1...) ...2...)
                         ;; --> (...1... ...2...)
                         ;; --> (...2...)
                         (let ((nms (match  (cdar pat) (reverse list) soe)))
                           (if (match-state-failed-p nms)
                               (match  nil list soe)
                               nms)))))
        while (match-state-failed-p nms)
        finally (return nms)))
    ((atom exp)
     (match-state-fail ms `(:unexpected-atom ,exp)))
    (t ;; both cars are sublists.
     (match (cdr pat) (cdr exp) (match (car pat) (car exp) ms)))))


(defun evaluate (instance)
  "PRIVATE"
  (cond
    ((atom instance)               instance)
    ((and (atom (car instance)) (string= :|| (car instance)))
     (eval (evaluate (second instance))))
    (t (mapcar (function evaluate) instance))))


(defun dict-value (dict name)
  "PRIVATE"
  (second (assoc name dict :test (function string=))))


(defun dict-boundp (dict name)
  "PRIVATE"
  (and (or (symbolp name) (stringp name))
       (assoc name dict :test (function string=))))
  

(defun subst-bindings (expr dict)
  "PRIVATE"
  (cond
    ((atom expr) (list expr))
    ((and (atom (first expr)) (string= :||  (first expr)))
     (if (and (atom (second expr))
              (dict-boundp dict (second expr)))
         (list (dict-value dict (second expr)))
         (list (mapcan (lambda (subexpr) (subst-bindings subexpr dict)) expr))))
    ((and (atom (first expr)) (string= :|@| (first expr)))
     (copy-seq (dict-value dict (second expr))))
    (t (list (mapcan (lambda (subexpr) (subst-bindings subexpr dict))
                     expr)))))


(defun instanciate (ms skeleton)
  "PRIVATE
PRE:   (not (match-state-failed-p ms))
DO:    Instanciate the skeleton, substituting all occurence of (: var)
       with the value bound to var in the binding dictionary of MS,
       Occurences of (:@ var) are split in line like ,@ in backquotes.
       Then all remaining (: form) are evaluated (with eval) from the
       deepest first.
"
  (assert (not (match-state-failed-p ms)))
  (evaluate (first (subst-bindings skeleton (match-state-dict ms)))))


(defun collect-variables (pat)
  "
PAT:       A symbolic expression with the following syntax:
             (?v v)  expects a symbol (variable).
             (?c c)  expects a constant (non symbol atom).
             (?x x)  expects anything (one item).
             (?+ l)  expects anything (one or more items).
             (?* l)  expects anything (zero or more items).
             other   expects exactly other (can be a sublist).
RETURN:    A list of the symbol used in the various (?. sym) items, 
           in no particular order, but with duplicates deleted.
"
  (delete-duplicates
   (cond
     ((atom pat)
      nil)
     ((and (atom (car pat))
           (member (car pat) '(?v ?c ?x ?+ ?*) :test (function string=)))
      (list (cadr pat)))
     (t
      (nconc (collect-variables (car pat)) (collect-variables (cdr pat)))))))


(defun match-case* (sexp clauses)
  "
SEXP:    A symbolic expression, evaluated.
CLAUSES: A list of (pattern func) or (otherwise ofunc)
         The functions FUNC is called with one BINDING argument.
         The function OFUNC is called with no argument.
DO:      Call the function of the clause whose pattern matches the SEXP,
         or whose pattern is a symbol string-equal to OTHERWISE.
RETURN:  The result of the called function, and the pattern that matched.
EXAMPLE: (match-case* expr
            `(((add       (?x a) to   (?x b)) 
                ,(lambda (bindings) `(+ ,(aget bindings 'a) ,(aget bindings 'b)))
               ((multiply  (?x a) with (?x b))
                ,(lambda (bindings) `(* ,(aget bindings 'a) ,(aget bindings 'b))))
               ((substract (?x a) from (?x a)) 
                ,(constantly 0))
               (otherwise
                ,(lambda () (error \"No matching pattern\"))))))
"
  (loop
     :for (pattern func) :in clauses
     :do (if (and (symbolp pattern) (string-equal "OTHERWISE" pattern))
             (return (values (funcall func) pattern))
             (let ((bindings (match pattern sexp)))
               (unless (match-state-failed-p bindings)
                 (return (values (funcall func bindings) pattern)))))))


(defmacro match-case (sexp &rest clauses)
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
  (with-gensyms (ex ms dc)
    `(let ((,ex ,sexp) (,ms) (,dc))
       (cond
         ,@(mapcar
            (lambda (clause)
              (let ((pat (car clause)) (body (cdr clause)))
                (if (and (symbolp pat) (string-equal "OTHERWISE" pat))
                    `(t ,@body)
                    `((progn (setf ,ms (match ',pat ,ex))
                             (not (match-state-failed-p ,ms)))
                      (setf ,dc (match-state-dict ,ms))
                      (let ( ,@(mapcar
                                (lambda (name) `(,name (cdr (assoc ',name ,dc)))) 
                                (collect-variables pat)) )
                        ,@body))))) clauses)))))


;;;; THE END ;;;;
