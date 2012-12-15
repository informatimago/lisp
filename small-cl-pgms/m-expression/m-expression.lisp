;;;;  -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               m-expression.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Here is a M-expression parser.
;;;;
;;;;    A lot of lisp newbies ask for more conventionnal syntax for lisp.
;;;;    Since day one, lisp was intended to have such a syntax: M-expressions.
;;;;
;;;;    Let's newbies play with them, and realize how impractical they are.
;;;;    Note for example, that we cannot use macros anymore because
;;;;    their syntax would need to be known by the M-expression parser,
;;;;    like it's the case for lambda[[...];...].
;;;;    Macros were added later in lisp history.
;;;;
;;;;
;;;;    Note that S-expressions can still be entered, as literal objects,
;;;;    but using comma instead of space to separate the items in lists.
;;;;
;;;;
;;;;    http://www.informatimago.com/develop/lisp/small-cl-pgms/aim-8/
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-09-28 <PJB> Created.
;;;;BUGS
;;;;
;;;;    Symbols are restricted to alphanumeric characters.
;;;;    This prevents using a lot of Common Lisp symbols.
;;;;    A more modern syntax for M-expressions could be designed,
;;;;    but this wasn't the point of the exercise.
;;;;
;;;;    In my old transcription of AIM-8, I've used two characters to write
;;;;    the arrows: ⎯⟶, but in this parser, the first is not accepted,
;;;;    and arrows must be written either as: ⟶ or as ->.
;;;;    A new version of the transcription only uses ⟶.
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.M-EXPRESSION"
  (:nicknames "M-EXPR")
  (:use "COMMON-LISP")
  (:export "READ-M-EXPRESSION" "PARSE-M-EXPRESSION" "LABEL" "COMBINE"
           "*LOAD-STREAM*" "M-EXPRESSION"
           "DRIVER" "DEFINE-M-FUNCTION"
           "M-REPL"
           "QUIT" "EXIT" "CONTINUE"))
(defpackage "M-LISP-USER"
  (:use "COMMON-LISP")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.M-EXPRESSION"
                "LABEL" "COMBINE" "QUIT" "EXIT" "CONTINUE"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.M-EXPRESSION")



;; To load this utf-8 file, specify the utf-8 external format:
;; (load"m-expression.lisp" :external-format #+clisp charset:utf-8 #+sbcl :utf-8)
;; or convert it first to ASCII...


;;      The S-functions have been described by a class of expres-
;; sions which has been informally introduced.  Let us call these
;; expressions F-expressions.  If we provide a way of translating
;; F-expressions into S-expressions, we can use S-functions to
;; repreent certain functions and predicates of S-expressions.
;;      First we shall describe this translation.
;;      3.1  Representation of S-functions as S-expressions.
;;      The representation is determined by the following rules.
;;      1.  Constant S-expressions can occur as parts of the
;; F-expressions representing S-functions.  An S-expression ℰ is
;; represented by the S-expression.  (QUOTE,ℰ)
;;      2.  Variables and function names which were represented
;; by strings of lower case letters are represented by the cor-
;; responding strings of the corresponding upper case letters.
;; Thus we have FIRST, REST and COMBINE, and we shall use X,Y
;; etc. for variables.
;;      3.  A form is represented by an S-expression whose first
;; term is the name of the main funcntion and whose remaining terms
;; are the argumetns of the function.  Thus combin[first[x];
;; rest[x]] is represented by (COMBINE,(FIRST,X),(REST,X))
;;      4.  The null S-expression ⋀ is named NIL.
;;      5.  The truth values  1  and 0  are denoted by T and F.
;;          The conditional expressoin
;;      write[p₁⎯⟶e₁,p₂⎯⟶e₂,...pk⎯⟶ek]
;; is repersented by
;;           (COND,(p₁,e₁),(p₂,e₂),...(pk,ek))
;;      6.  λ[[x;..;s];ℰ] is represented by (LAMBDA,(X,...,S),ℰ)
;;      7.  label[α;ℰ] is represented by (LABEL,α,ℰ)
;;      8.  x=y is represented by (EQ,X,Y)
;;      With these conventions the substitution function mentioned
;; earlier whose F-expression is
;;      label[subst;λ[[x;y;s];[null[s]⎯⟶⋀;atom[s]⎯⟶
;;           [y=s⎯⟶x;1⎯⟶s];1⎯⟶combine[subst[x;y;first[s]];
;;                subst[x;y;rest[s]]]]]]
;; is represented by the S-expression.
;;           (LABEL,SUBST,(LAMBDA,(X,Y,Z),(COND,((NULL,
;;                Z),NIL),((ATOM,Z),(COND)((EQ,Y,Z),X),(1,Z))),
;;                     (1,(COMBINE,(SUBST,X,Y,(FIRST,Z)),
;;                          (SUBST,X,Y,(REST,Z))))))



(defun read-s-number (stream)
  (let ((sign +1)
        (int   0)
        (ch (read-char stream nil nil)))
    (case ch
      ((#\+) (setf ch (read-char stream nil nil)))
      ((#\-) (setf ch (read-char stream nil nil)) (setf sign -1)))
    (loop
       :while (and ch (digit-char-p ch))
       :do (setf int (+ (* 10 int) (digit-char-p ch))
                 ch  (read-char stream nil nil)))
    (case ch
      ((nil) `(:s-integer ,(* sign int)))
      ((#\.) (let ((frac 0.0)
                   (weight 0.1))
               (loop
                  :initially (setf ch (read-char stream nil nil))
                  :while (and ch (digit-char-p ch))
                  :do (setf frac (+ frac (* weight (digit-char-p ch)))
                            weight (/ weight 10)
                            ch (read-char stream nil nil)))
               (case ch
                 ((nil) `(:s-float ,(* sign (+ int frac))))
                 ((#\E) (let ((exps +1)
                              (expo  0))
                          (setf ch (read-char stream nil nil))
                          (case ch
                            ((#\+)
                             (setf ch (read-char stream nil nil)))
                            ((#\-)
                             (setf ch (read-char stream nil nil))
                             (setf exps -1)))
                          (loop
                             :while (and ch (digit-char-p ch))
                             :do (setf expo (+ (* 10 expo) (digit-char-p ch))
                                       ch  (read-char stream nil nil)))
                          (when ch (unread-char ch stream))
                          `(:s-float ,(* sign
                                         (+ int frac)
                                         (expt 10.0 (* exps expo))))))
                 (otherwise (unread-char ch stream)
                            (if (alpha-char-p ch)
                                (error "Invalid token at ~S"
                                       (read-line stream nil nil))
                                `(:s-float ,(* sign (+ int frac))))))))
      (otherwise
       (unread-char ch stream)
       (if (alpha-char-p ch)
           (error "Invalid token at ~S" (read-line stream nil nil))
           `(:s-integer ,(* sign int)))))))

(defun m-sym-first-char-p (ch)
  (find ch "abcdefghijklmnopqrstuvwxyz" :test (function char=)))
(defun m-sym-follow-char-p (ch)
  (find ch "abcdefghijklmnopqrstuvwxyz0123456789" :test (function char=)))
(defun s-sym-first-char-p (ch)
  (find ch "ABCDEFGHIJKLMNOPQRSTUVWXYZ" :test (function char=)))
(defun s-sym-follow-char-p (ch)
  (find ch "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" :test (function char=)))

(defun read-m-symbol (stream)
  (loop
     :with buffer = (make-array 16 :adjustable t :fill-pointer 0
                                :element-type 'character)
     :for ch = (read-char stream nil nil)
     :while (and ch (m-sym-follow-char-p ch))
     :do (vector-push-extend ch buffer)
     :finally (when ch (unread-char ch stream))
     (return `(:m-symbol ,(intern buffer "M-LISP-USER")))))

(defun read-s-symbol (stream)
  (loop
     :with buffer = (make-array 16 :adjustable t :fill-pointer 0
                                :element-type 'character)
     :for ch = (read-char stream nil nil)
     :while (and ch (s-sym-follow-char-p ch))
     :do (vector-push-extend ch buffer)
     :finally (when ch (unread-char ch stream))
     (return `(:s-symbol ,(intern buffer "M-LISP-USER")))))

(defun skip-spaces (stream)
  (loop
     :with ch = (read-char stream nil nil)
     :while (and ch (find ch #(#\space #\newline #\tab
                               #\return #\linefeed
                               ;; #\vt
                               )))
     :do (setf ch (read-char stream nil nil))
     :finally (when ch (unread-char ch stream))))

(defun get-token (stream)
  (skip-spaces stream)
  (let ((ch (read-char stream nil nil)))
    (case ch
      ((nil)  '(:eof))
      ((#\[)  '(:m-open))
      ((#\])  '(:m-close))
      ((#\⟶)  '(:m-arrow))
      ((#\⋀)  '(:m-symbol |nil|))
      ((#\λ)  '(:m-symbol |lambda|))
      ((#\;)  '(:m-sep))
      ((#\=)  '(:m-equal))
      ((#\()  '(:s-open))
      ((#\))  '(:s-close))
      ((#\,)  '(:s-sep))
      ((#\+)   (read-s-number stream))
      ((#\-)
       (let ((ch (peek-char nil stream nil nil)))
         (case ch
           ((nil) (error "Invalid character '-' at ~S"
                         (read-line stream nil nil)))
           ((#\>)
            (read-char stream)
            '(:m-arrow))
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (unread-char ch stream)
            (read-s-number stream))
           (otherwise (error "Invalid character '-' at ~S"
                             (read-line stream nil nil))))))
      (otherwise
       (unread-char ch stream)
       (cond
         ((digit-char-p ch)
          (read-s-number stream))
         ((m-sym-first-char-p ch) (let ((sym (read-m-symbol stream)))
                                    (case (second sym)
                                      ((m-lisp-user::|nil|)   '(:m-nil))
                                      ((m-lisp-user::|t|)     '(:m-true))
                                      ((m-lisp-user::|f|)     '(:m-false))
                                      (otherwise sym))))
         ((s-sym-first-char-p ch) (read-s-symbol stream))
         (t  (error "Invalid character '~C' at ~S"
                    ch (read-line stream nil nil))))))))

(defvar *test-source* "")

(defun test-scanner ()
  (with-input-from-string (input *test-source*)
    (loop
       :for token = (get-token input)
       :do (print token)
       :until (eq :eof (first token)))))

;;  (test-scanner)

(defstruct parser current-token stream)

(defun advance (parser)
  (setf (parser-current-token parser) (get-token (parser-stream parser))))

(defun token-p (token parser)
    (eql token (first (parser-current-token parser))))

;; m-expr           ::= m-eq | m-term .
;; m-eq             ::= m-term m-equal m-term .
;; m-term           ::= m-var | m-call | m-cond | s-expr | m-lambda-list .
;; m-lambda-list         ::= m-lambda '[' '[' m-pars ']' ';' m-expr ']' .
;; m-pars           ::= | m-pars-items .
;; m-pars-items     ::= m-symbol | m-symbol ';' m-pars-items .
;; m-var            ::= m-symbol .
;; m-function       ::= m-symbol | m-lambda-list .
;; m-call           ::= m-function '[' m-args ']' .
;; m-args           ::= | m-arg-item .
;; m-arg-items      ::= m-expr | m-expr '   ;' m-args .
;; m-cond           ::= '[' m-clauses ']' .
;; m-clauses        ::= | m-clause-items .
;; m-clause-items   ::= m-clause | m-clause ';' m-clauses .
;; m-clause         ::= m-expr m-arrow m-expr .
;; s-expr           ::= s-atom | '(' s-list ')' .
;; s-list           ::= | s-list-items .
;; s-list-items     ::= s-expr | s-expr ',' s-list-items .
;; s-atom           ::= s-symbol | s-integer | s-float | s-string .

;; m-lambda              ::= 'lambda' .
;; m-nil            ::= 'nil' .
;; m-true           ::= 't' .
;; m-false          ::= 'f' .
;; m-equal          ::= '=' .
;; m-arrow          ::= '->' .
;; m-symbol         ::= "[a-z][a-z0-9]*" .
;; s-symbol         ::= "[A-Z][A-Z0-9]*" .
;; s-integer        ::= "[-+]?[0-9]+" .
;; s-float          ::= "[-+]?[0-9]+.[0-9]+(E[-+]?[0-9]+)?" .


(defun parse-m-expr (parser)
  ;; m-expr       ::= m-eq | m-term .
  ;; m-eq         ::= m-term m-equal m-term .
  (if (token-p :eof parser)
      :eof
      (let ((term1 (parse-m-term parser)))
        (if (token-p :m-equal parser)
            (progn (advance parser)
                   (let ((term2 (parse-m-term parser)))
                     `(equal ,term1 ,term2)))
            term1))))

(defun m-to-s-symbol (m-symbol)
  (intern (string-upcase (second m-symbol)) "M-LISP-USER"))

(defun parse-m-args (parser)
  ;; m-args           ::= | m-arg-item .
  ;; m-arg-items      ::= m-expr | m-expr '   ;' m-args .
  (unless (token-p :m-close parser)
    (loop
       :collect (parse-m-expr parser)
       :while (token-p :m-sep parser)
       :do (advance parser))))

(defun parse-m-pars (parser)
  ;; m-pars           ::= | m-pars-items .
  ;; m-pars-items     ::= m-symbol | m-symbol ';' m-pars-items .
  (unless (token-p :m-close parser)
    (loop
       :collect (parse-m-expr parser)
       :while (token-p :m-sep parser)
       :do (advance parser))))

(defun parse-m-clause (parser)
  ;; m-clause     ::= m-expr m-arrow m-expr .
  (let ((antecedent (parse-m-expr parser))
        (consequent (progn
                      (if (token-p :m-arrow parser)
                          (advance parser)
                          (error "Expected an arrow in m-clause, not ~S~% at ~S~
                               (check your brackets)"
                                 (parser-current-token parser)
                                 (read-line (parser-stream parser) nil nil)))
                      (parse-m-expr parser))))
    `(,antecedent ,consequent)))

(defun parse-m-clauses (parser)
  ;; m-clauses    ::= m-clause | m-clause ';' m-clauses .
  (loop
     :collect (parse-m-clause parser)
     :while (token-p :m-sep parser)
     :do (advance parser)))


(defmacro with-parens ((parser open close) &body body)
  (let ((vparser (gensym)) (vopen (gensym)) (vclose (gensym)))
    `(let ((,vparser ,parser)
           (,vopen   ,open)
           (,vclose  ,close))
       (unless (token-p ,vopen ,vparser)
         (error "Expected ~A, not ~S~% at ~S" ,vopen
                (parser-current-token ,vparser)
                (read-line (parser-stream ,vparser) nil nil)))
       (advance ,vparser)
       (prog1 (progn ,@body)
         (if (token-p ,vclose ,vparser)
             (advance ,vparser)
             (error "Expected ~A, not ~S~% at ~S" ,vclose
                    (parser-current-token ,vparser)
                    (read-line (parser-stream ,vparser) nil nil)))))))


(defun parse-m-term (parser)
  ;; m-term       ::= m-var | m-call | m-cond | s-expr | m-lambda-list .
  (cond
    ((token-p :m-open parser)           ; m-cond
     (with-parens (parser :m-open :m-close)
       `(cond ,@(parse-m-clauses parser))))
    ((token-p :s-open    parser)       ; S-expr
     `(quote ,(parse-s-expr parser)))
    ((or (token-p :s-symbol  parser)
         (token-p :s-integer parser)
         (token-p :s-float   parser)
         (token-p :s-string  parser))
     (prog1
         `(quote ,(second (parser-current-token parser)))
       (advance parser)))
    ((or (token-p :m-symbol parser) ; M-expr
         (token-p :m-nil    parser)
         (token-p :m-true   parser)
         (token-p :m-false  parser))     ; m-var or m-call
     (let* ((name (parser-current-token parser))
            (sname  (cond
                      ((or (token-p :m-false parser)
                           (token-p :m-nil parser)) 'nil)
                      ((token-p :m-true parser)       't) 
                      (t             (m-to-s-symbol name)))))
       (advance parser)
       (if (token-p :m-open parser)
           (with-parens (parser :m-open :m-close)
             (if (eql 'lambda sname)
                 `(lambda ,(with-parens (parser :m-open :m-close)
                                   (parse-m-pars parser))
                    ,(progn (unless (token-p :m-sep parser)
                              (error "Expected a semi-colon, not ~S~% at ~S"
                                     (parser-current-token parser)
                                     (read-line (parser-stream parser) nil nil)))
                            (advance parser)
                            (parse-m-expr parser)))
                 `(,sname ,@(parse-m-args parser))))
           sname)))
    (t (error "Unexpected token in m-term: ~S~% at ~S"
              (parser-current-token parser)
              (read-line (parser-stream parser) nil nil)))))

(defun parse-s-list (parser)
  ;; s-list           ::= | s-list-items .
  ;; s-list-items     ::= s-expr | s-expr [','] s-list-items .
  ;; We make comma optional since later m-expression programs (like AIM-16)
  ;; didn't use it...
  (unless (token-p :s-close parser)
    (loop
       :until (token-p :s-close parser)
       :collect (parse-s-expr parser)
       :do (when (token-p :s-sep parser) (advance parser)))))

(defun parse-s-expr (parser)
  ;; s-expr       ::= s-atom | '(' s-list ')' .
  ;; s-atom       ::= s-symbol | s-integer | s-float | s-string .
  (cond
    ((token-p :s-open parser)
     (with-parens (parser :s-open :s-close)
        (parse-s-list parser)))
    ((or (token-p :s-symbol  parser)
         (token-p :s-integer parser)
         (token-p :s-float   parser)
         (token-p :s-string  parser))
     (prog1 (second (parser-current-token parser))
       (advance parser)))
    (t  (error "Unexpected token in a s-expr: ~S~% at ~S"
               (parser-current-token parser)
               (read-line (parser-stream parser) nil nil)))))



(defparameter *test-source* "
     label[subst;λ[[x;y;s];[null[s]->nil;atom[s]⟶
           [y=s->x;1->s];1->combine[subst[x;y;first[s]];
                subst[x;y;rest[s]]]]]]
        =
           (LABEL,SUBST,(LAMBDA,(X,Y,Z),(COND,((NULL,
                Z),NIL),((ATOM,Z),(COND,((EQ,Y,Z),X),(1,Z))),
                     (1,(COMBINE,(SUBST,X,Y,(FIRST,Z)),
                                (SUBST,X,Y,(REST,Z)))))))")


(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition 
         (err) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&")
       (finish-output))
     (condition 
         (err) 
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err)
       (finish-output))))


(defun m-repl (&key ((input *standard-input*) *standard-input*)
               ((output *standard-output*) *standard-output*))
  (let ((parser (make-parser :stream *standard-input*))
        (*package* (find-package "M-LISP-USER")))
    (loop
       :named repl
       :for history :from 1
       :do (progn
             (format t "~%~A[~D]M-REPL> " (package-name *package*) history)
             (finish-output)
             (handling-errors
               (advance parser)
               (setf - (parse-m-expr parser))
               (unless (token-p :m-sep parser)
                 (error "Please terminate your m-expressions with a semi-colon, ~
                        not ~S" (parser-current-token parser)))
               (when (or (eq - :eof) (member - '((quit)(exit)(continue))
                                             :test (function equalp)))
                 (return-from repl))
               (let ((results (multiple-value-list (eval -))))
                 (setf +++ ++   ++ +   + -
                       /// //   // /   / results
                       *** **   ** *   * (first /)))
               (format t "~& --> ~{~S~^ ;~%     ~}~%" /)
               (finish-output))))))


(defun test-parser ()
  (assert (eql :eof (with-input-from-string (src "")
                      (let ((parser (make-parser :stream src)))
                        (advance parser)
                        (parse-m-expr parser)))))
  (with-input-from-string (src *test-source*)
    (let ((parser (make-parser :stream src)))
      (advance parser)
      (parse-m-expr parser))))

(defun read-m-expression (&optional (*standard-input* *standard-input*))
  (let ((parser (make-parser :stream *standard-input*)))
    (advance parser)
    (parse-m-expr parser)))

(defun parse-m-expression (text &key (start 0) (end nil))
  (let ((index nil))
    (values
     (with-input-from-string (src text :index index :start start :end end)
       (read-m-expression src))
     index)))


(defmacro label (name lambda-expression)
  `(defun ,name ,(cadr lambda-expression) ,@(cddr lambda-expression)))

(defun combine (a d) (cons a d))

(defmacro define-m-function (mexp &optional docstring)
  (let ((sexp (parse-m-expression mexp)))
    (if (and (consp sexp)
             (eq  'equal (first sexp)))
        `(defun ,(first (second sexp)) ,(rest (second sexp))
           ,@(when docstring (list docstring))
           ,@(rest (rest sexp)))
        (progn
          (error "M-exp is not a definition: ~%~A~%~S~%" mexp sexp)))))

(defun driver (&optional (*standard-input* *standard-input*))
  (loop
     :for form = (read-m-expression)
     :until (eq :eof form)
     :do (print (eval
                 (if (and (consp form) (eq 'equal (car form)))
                     (if (consp (second form))
                         `(defun ,(first (second form)) ,(rest (second form))
                            ,@(rest (rest form)))
                         `(defparameter ,(second form) ,(third form)))
                     form))))
  (values))

(defvar *load-stream* nil
  "A string of m-expressions, while loaded by M-EXPRESSION.")

(defun m-expression (mexp)
  (with-input-from-string (*load-stream* mexp)
    (driver *load-stream*)))


;; (load "/net/users/pjb/src/public/small-cl-pgms/m-expression/m-expression.lisp" :external-format charset:utf-8) (use-package :com.informatimago.common-lisp.m-expression)
