;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               c-syntax.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines classes to generate C++ syntax.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-12-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2007 - 2007
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
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.LINC")  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EMITTING C CODE
;;; 

(defvar *c-out* t "A stream designator.")
(defvar *same-line* nil)
(defvar *level* 99)
(defvar *indent* 0)
(defvar *naked* t)


(let ((bol t))
  (defun emit (&rest args)
    (loop
       :for arg :in args
       :do (cond
             ((eq :newline arg)
              (terpri *c-out*)
              (setf bol t))
             ((eq :fresh-line arg)
              (unless bol
                (terpri *c-out*)
                (setf bol t)))
             (t
              (if bol
                (format *c-out* "~VA~A" (* *indent* 4) "" arg)
                (princ arg *c-out*))
              (setf bol nil))))))


(defmacro with-indent (&body body)
  `(let ((*indent* (1+ *indent*)))
     ,@body))


(defmacro with-parens (parens &body body)
  `(let ((*level* 99))
     (emit ,(elt parens 0))
     (unwind-protect (with-indent ,@body)
       (emit ,(elt parens 1)))))

(defmacro in-continuation-lines (&body body)
  `(format *c-out* "~{~A~^ \\~%~}"
           (split-sequence:split-sequence
            #\newline (with-output-to-string (*c-out*)
                        (block nil ,@body)
                        (emit :fresh-line)))))






(defclass c-item () ())

(defgeneric generate (item)
  (:method ((self c-item))
    (emit :fresh-line)
    (warn "Cannot generate ~S yet." self)
    (format *c-out* "/* Cannot generate ~S yet. */" self)
    (emit :newline))
  (:method ((self null)))
  (:method ((self t))
    (emit (format nil "/* ~A */" self))))

(defgeneric arguments (item)
  (:method-combination append)
  (:method append ((self c-item)) '()))


(defgeneric c-sexp (item)
  (:method ((self t)) `(quote ,self))
  (:method ((self c-item)) self))


(defgeneric generate-with-indent (item)
  (:method ((self t))
    (with-indent
      (generate (ensure-statement self))))
  (:method ((self null))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defun externalp (symbol)
  (eq (nth-value 1 (find-symbol (symbol-name symbol)
                                (symbol-package symbol)))
      :external))


;;;---------------------------------------------------------------------
;;; C has various namespaces: variables, classes, structs, unions,
;;; types, enums, functions and macros.


(defmacro make-declare (kind &optional docstring)
  "
Defines two functions for each KIND:
  (DECLARE-{KIND} name [:EXTERNAL boolean])
     Declares the name as a KIND.
     When external-p is true, the symbol name is exported from its package.
     When name is a list of symbols, then each of them are declared as a KIND.
  ({KIND}-P name)
     Predicate indicating that name has been declared to be a KIND.
     Note: the suffix is P or -P depending on the presence of #\- in
           the name of KIND.
"
  (let ((fname  (intern (concatenate 'string "DECLARE-" (string kind))
                        (symbol-package kind)))
        (pname  (intern (concatenate 'string (string kind)
                                     (if (find #\- (string kind)) "-P" "P"))
                        (symbol-package kind))))
    `(progn
       (defun ,fname (name &key external)
         ,@(list (format nil "Declare a C ~(~A~).~%~@[~A~]" kind docstring))
         (if (listp name)
             (map nil (function ,fname) name)
             (progn
               (when external (export name (symbol-package name)))
               (setf (get name ',kind) t)))
         name)
       (defun ,pname (name)
         ,@(list (format nil "Predicate whether NAME is a C ~(~A~).~%~@[~A~]" kind docstring))
         (get name ',kind))
       ',kind)))


;;;
;;;---------------------------------------------------------------------


(defun camel-case (name &key capitalize-initial)
  (let ((chunks (split-sequence:split-sequence #\- (string name))))
    (format nil "~{~A~}"
            (cons (funcall (if capitalize-initial
                             (function string-capitalize)
                             (function string-downcase)) (first chunks))
                  (mapcar (function string-capitalize) (rest chunks))))))


(defmethod generate ((self symbol))
  (let* ((packname (package-name (symbol-package self)))
         (symbname (symbol-name self)))
    (when (externalp self)
      (emit (camel-case packname :capitalize-initial t) "_"))
    (emit
     (cond
       ((or (com.informatimago.linc.c::classp    self)
            (com.informatimago.linc.c::structp   self)
            (com.informatimago.linc.c::unionp    self)
            (com.informatimago.linc.c::typep     self)
            (com.informatimago.linc.c::enump     self)
            (com.informatimago.linc.c::functionp self))
        (camel-case symbname :capitalize-initial t))
       ((com.informatimago.linc.c::macrop self)
        (substitute #\_ #\- (string-upcase symbname)))
       (t
        (camel-case symbname :capitalize-initial nil))))))


(defmethod generate ((self string))
  "
BUG: What about the character encoding of C strings?
"
  ;; TODO: Get the C-string writer to generate this.
  (emit (with-output-to-string (out)
          (princ "\"" out)
          (loop
             :for ch :across self
             :do (princ (case ch
                          #+#.(cl:if (cl:char/= #\newline #\return) '(:and) '(:or))
                          ((#\newline)            "\\n")
                          #+#.(cl:if (cl:char/= #\newline #\linefeed) '(:and) '(:or))
                          ((#\linefeed)           "\\l")
                          ((#\return)             "\\r")
                          ((#\tab)                "\\t")
                          ((#\bell)               "\\a")
                          ((#\page)               "\\f")
                          ((#\vt)                 "\\v")
                          ((#\")                  "\\\"")
                          (otherwise
                           (if (< (char-code ch) 32)
                             (format nil "\\~3,'0o" (char-code ch))
                             ch))) out))
          (princ "\"" out))))


(defmethod generate ((self real)) 
 "
BUG: Correct C number syntax!
"
  (emit (format nil "~A" self)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;


(defun at-least-one-p  (list)            list)
(defun at-least-two-p  (list)       (cdr list))
(defun exactly-one-p   (list) (and       list  (not (cdr   list))))
(defun exactly-two-p   (list) (and (cdr  list) (not (cddr  list))))
(defun exactly-three-p (list) (and (cddr list) (not (cdddr list))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EXPRESSIONS
;;;


(defclass expression (c-item)
  ())

(defgeneric c-name (expression)
  (:method ((self expression))
    (error "I don't know the C operation name for an expression of class ~A"
           (class-name (class-of self)))))

(defmethod arguments append ((self expression))
  '()
  #- (and) (error "I don't know the arguments for an expression of class ~A"
                  (class-name (class-of self))))

(defmethod print-object ((self expression) stream)
  (if *print-readably*
    (print-unreadable-object (self stream :identity t :type t)
      (with-slots (c-name priority associativity) self
        (let ((arguments (arguments self)))
          (format stream ":c-name ~S :priority ~A :associativity ~A ~
                               :arguments ~S"
                  c-name priority associativity arguments))))
    (prin1 `(,(class-name (class-of self))
              ,@(mapcar (lambda (arg) (if (typep arg 'c-item) arg `(quote ,arg)))
                        (arguments self))) stream))
  self)

(defmethod c-sexp ((self expression))
  `(,(intern (c-name self) "COM.INFORMATIMAGO.LINC.C")
     ,@(mapcar (function c-sexp) (arguments self))))



(defclass 0-*-arguments ()
  ((arguments :initarg :arguments
              :writer (setf arguments)
              :type list)
   (arity     :initform '0-*
              :reader arity
              :allocation :class)))
(defmethod arguments append ((self 0-*-arguments))
  (when (slot-boundp self 'arguments) (slot-value self 'arguments))) 

(defclass 1-*-arguments ()
  ((arguments :initarg :arguments
              :writer (setf arguments)
              :type list)
   (arity     :initform '1-*
              :reader arity
              :allocation :class)))
(defmethod arguments append ((self 1-*-arguments))
  (when (slot-boundp self 'arguments) (slot-value self 'arguments))) 

(defmethod initialize-instance :after ((self 1-*-arguments)
                                       &key &allow-other-keys)
  (assert (proper-list-p  (arguments self)))
  (assert (at-least-one-p (arguments self)))
  self)


(defclass 2-*-arguments ()
  ((arguments :initarg :arguments
              :writer (setf arguments)
              :type list)
   (arity     :initform '2-*
              :reader arity
              :allocation :class)))
(defmethod arguments append ((self 2-*-arguments))
  (when (slot-boundp self 'arguments) (slot-value self 'arguments))) 

(defmethod initialize-instance :after ((self 2-*-arguments)
                                       &key &allow-other-keys)
  (assert (proper-list-p  (arguments self)))
  (assert (at-least-two-p (arguments self)))
  self)


(defclass 1-argument ()
  ((argument :initarg :argument
             :accessor argument)
   (arity     :initform '1
              :reader arity
              :allocation :class)))
(defmethod initialize-instance :after ((self 1-argument)
                                       &key (arguments nil argumentsp)
                                       (argument nil argumentp)
                                       &allow-other-keys)
  (declare (ignorable argument))
  (assert (and (or argumentsp argumentp) (not (and argumentsp argumentp)))
          () ":argument and :arguments are mutually exclusive, but one of them must be given.")
  (when arguments
    (assert (proper-list-p arguments))
    (assert (exactly-one-p arguments))
    (setf (slot-value self 'argument) (first arguments))))


(defmethod arguments append ((self 1-argument))
  (when (slot-boundp self 'argument) (list (argument self))))
(defmethod (setf arguments) (value (self 1-argument))
  (assert (proper-list-p value))
  (assert (endp (rest value)))
  (setf (argument self) (first value)))


(defclass 2-arguments ()
  ((left-arg  :initarg :left-arg  :initarg :left-argument
              :accessor left-arg  :accessor left-argument)
   (right-arg :initarg :right-arg :initarg :right-argument
              :accessor right-arg :accessor right-argument)
   (arity     :initform '2
              :reader arity
              :allocation :class)))

(defmacro count-true (&rest args)
  (let ((vcount (gensym)))
    `(let ((,vcount 0))
       ,@(mapcar (lambda (arg) `(when ,arg (incf ,vcount))) args)
       ,vcount)))

(defmethod initialize-instance :after ((self 2-arguments)
                                       &key (arguments nil argumentsp)
                                       (left nil leftp)
                                       (right nil rightp)
                                       (left-arg nil left-arg-p)
                                       (right-arg nil right-arg-p)
                                       (left-argument nil left-argument-p)
                                       (right-argument nil right-argument-p)
                                       &allow-other-keys)
  (declare (ignorable left-arg right-arg left-argument right-argument))
  (let ((l (count-true leftp  left-arg-p  left-argument-p))
        (r (count-true rightp right-arg-p right-argument-p)))
    (assert (xor argumentsp (or (plusp l) (plusp r)))
            () ":arguments is mutually exclusive from the other argument initargs.")
    (if argumentsp
      (progn
        (assert (proper-list-p arguments))
        (assert (exactly-two-p arguments))
        (setf (slot-value self 'left-arg)  (first  arguments)
              (slot-value self 'right-arg) (second arguments)))
      (progn
        (assert (= 1 l) ()
                ":left, :left-arg-p and :left-argument-p are mutually ~
exclusive, but one must be given when :arguments is not given.")
        (when leftp (setf (slot-value self 'left-arg) left))
        (assert (= 1 r) ()
                ":right, :right-arg-p and :right-argument-p are mutually ~
exclusive, but one must be given when :arguments is not given.")
        (when rightp (setf (slot-value self 'right-arg) right))))))

(defmethod arguments append ((self 2-arguments))
  (list (when (slot-boundp self 'left-arg)   (left-arg self))
        (when (slot-boundp self 'right-arg)  (right-arg self))))
(defmethod (setf arguments) (value (self 2-arguments))
  (assert (proper-list-p value))
  (assert (exactly-two-p value))
  (setf (left-arg  self) (first  value)
        (right-arg self) (second value))
  value)



(defclass 3-arguments ()
  ((arguments :initarg :arguments
              :writer (setf arguments)
              :type list)
   (arity     :initform '3
              :reader arity
              :allocation :class)))
(defmethod arguments append ((self 3-arguments))
  (when (slot-boundp self 'arguments) (slot-value self 'arguments)))


(defmethod initialize-instance :after ((self 3-arguments)
                                       &key &allow-other-keys)
  (assert (proper-list-p   (arguments self)))
  (assert (exactly-three-p (arguments self)))
  self)






;; (type-pointer  1 ,(lambda (level type)
;;                    ;; (cast var (pointer char)) --> (char*)var; 
;;                    (generate-expression type :level level :naked nil)
;;                    (emit "*")))


;; (com.informatimago.linc.c::literal    1 ,(lambda (level token)
;;                      (declare (ignore level))
;;                      (emit (if (stringp token)
;;                              (lisp->c-string token)
;;                              token))))
;; (com.informatimago.linc.c::identifier 1 ,(lambda (level token)
;;                      (declare (ignore level))
;;                      (generate-identifier token)))


(defun gen-operator (cl-name priority associativity arity
                     c-name-or-generator)
  (let ((c-name (if (stringp c-name-or-generator)
                    c-name-or-generator
                    (string cl-name))))
    (list `(defclass ,cl-name (,(ecase arity
                                       ((0-*) '0-*-arguments)
                                       ((1-*) '1-*-arguments)
                                       ((2-*) '2-*-arguments)
                                       ((1)   '1-argument)
                                       ((2)   '2-arguments)
                                       ((3)   '3-arguments))
                                expression)
             ((priority      :initform ,priority
                             :reader priority
                             :allocation :class)
              (associativity :initform ,associativity
                             :reader associativity
                             :allocation :class)
              (c-name        :initform ,c-name 
                             :reader c-name
                             :allocation :class)))
          `(defun ,cl-name ,(ecase arity
                                   ((0-*) '(&rest arguments))
                                   ((1-*) '(one &rest arguments))
                                   ((2-*) '(one two &rest arguments))
                                   ((1)   '(one))
                                   ((2)   '(one two))
                                   ((3)   '(one two three)))
             (make-instance ',cl-name
                 :arguments ,(ecase arity
                                    ((0-*) 'arguments)
                                    ((1-*) '(cons one arguments))
                                    ((2-*) '(list* one two arguments))
                                    ((1)   '(list one))
                                    ((2)   '(list one two))
                                    ((3)   '(list one two three)))))
          `(setf (symbol-function ',(intern c-name "COM.INFORMATIMAGO.LINC.C"))
                 (symbol-function ',cl-name))
          `(defmethod generate ((self ,cl-name))
             ,(if (stringp c-name-or-generator)
                  (if (eql 1 arity)
                      `(progn
                         (emit ,c-name-or-generator)
                         (generate (argument self)))
                      `(generate-list ,c-name-or-generator
                                      (function generate)
                                      (arguments self)))
                  `(apply ,c-name-or-generator (arguments self)))))))


(defmethod generate :around ((self expression))
  (if (and *naked* (not *level*))
      (let ((*level* -1)
            (*naked* nil))
        (call-next-method))
      (let ((*level* (priority self))
            (*naked* nil))
        (if (< (priority self) *level*)
            ;; need parentheses:
            (with-parens "()" (call-next-method))
            ;; no need for parentheses:
            (call-next-method)))))


(defun generate-list (separator generator list)
  (when list
    (flet ((gen (item) (emit separator) (funcall generator item)))
      (funcall generator (car list))
      (map nil (function gen) (cdr list)))))



(defun make-operators ()
  (loop
    :for priority :from 0
    :for (associativity . operators) :in 
    '((:left
       (expr-seq             1-* ","))
      (:left
       (expr-callargs        0-* ","))
      ;; expr-args above expr-seq to force parens in: fun(arg,(f(a),g(b)),arg);
      (:rigth
       (expr-if              3
        (lambda (condi then else)
          (let ((*level* (1+ *level*))
                (*naked* nil))
            (generate condi)
            (emit "?")
            (generate then))
          (emit ":")
          (generate else)))
       (assign               2-* "=")
       (assign-times         2-* "*=")
       (assign-divided       2-* "/=")
       (assign-modulo        2-* "%=")
       (assign-plus          2-* "+=")
       (assign-minus         2-* "-=")
       (assign-right-shift   2-* ">>=")
       (assign-left-shift    2-* "<<=")
       (assign-bitand        2-* "&=")
       (assign-bitor         2-* "|=")
       (assign-bitxor        2-* "^="))
      (:left
       (expr-logor           2-* "||"))
      (:left
       (expr-logand          2-* "&&"))
      (:left
       (expr-bitor           2-* "|"))
      (:left
       (expr-bitxor          2-* "^"))
      (:left
       (expr-bitand          2-* "&"))
      (:left
       (expr-eq              2   "==")
       (expr-ne              2   "!="))
      (:left
       (expr-lt              2   "<")
       (expr-gt              2   ">")
       (expr-le              2   "<=")
       (expr-ge              2   ">="))
      (:left
       (expr-left-shift      2   "<<")
       (expr-right-shift     2   ">>"))
      (:left
       (expr-plus            2-* "+")
       (expr-minus           2-* "-"))
      (:left
       (expr-times           2-* "*")
       (expr-divided         2-* "/")
       (expr-modulo          2-* "%"))
      (:left
       (expr-memptr-deref    2   ".*")
       (expr-ptrmemptr-deref 2   "->*"))
      (:right
       (expr-cast 2
        (lambda (expression type)
          (with-parens "()" (generate type))
          (let ((*naked* nil)) (generate expression)))))
      (:unary
       (expr-preincr         1   "++")
       (expr-predecr         1   "--")
       (expr-lognot          1   "!")
       (expr-bitnot          1   "~")
       (expr-deref           1   "*")
       (expr-address         1   "&")
       (expr-pos             1   "+")
       (expr-neg             1   "-")
       (expr-sizeof    1
        (lambda (argument)
          (emit "sizeof") (with-parens "()" (generate argument))))
       (expr-new       1
        (lambda (argument)
          (emit "new" " ") (generate argument)))
       (expr-new[]     1
        (lambda (argument)
          (emit "new" "[]" " ") (generate argument)))
       (expr-delete    1
        (lambda (argument)
          (emit "delete" " ") (generate argument)))
       (expr-delete[]  1
        (lambda (argument)
          (emit "delete" "[]" " ") (generate argument)))
       (cpp-stringify        1 "#"))
      (:post
       (expr-postincr         1
        (lambda (expr)
          (let ((*naked* nil)) (generate expr)) (emit "++")))
       (expr-postdecr         1
        (lambda (expr)
          (let ((*naked* nil)) (generate expr)) (emit "--")))
       (expr-field    2-* ".")
       (expr-ptrfield 2-* "->")
       (expr-aref     2-*
        (lambda (&rest expressions)
          (let ((*naked* nil))
            (generate (first expressions))
            (dolist (expr (rest expressions))
              (with-parens "[]" (generate expr))))))
       (expr-call     1-*
        (lambda (&rest expressions)
          (let ((*naked* nil))
            (generate (first expressions))
            (if (and (= 2 (length expressions))
                     (typep (second expressions) 'expr-callargs))
                (with-parens "()"
                  (generate (second expressions)))
                (with-parens "()"
                  (when (rest expressions)
                    (generate
                     (make-instance 'expr-callargs
                         :arguments (rest expressions))))))))))
      (:left
       (absolute-scope    1
        (lambda (name) (emit "::") (generate name))))
      (:left
       (expr-scope        1-*
        (lambda (&rest names)
          (generate-list "::" (function generate) names))))
      (:left
       (cpp-join          2   "##")))
    :nconc (loop
             :for op :in operators
             :nconc (destructuring-bind (cl-name arity c-name-or-generator) op
                      (gen-operator cl-name priority associativity arity
                                    c-name-or-generator)))))

(defmacro gen-operators ()
  `(progn ,@(make-operators)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; STATEMENTS
;;;


(defclass statement (c-item)
  ())

(defclass optional-statement ()
  ((statement :initarg :sub-statement
              :accessor sub-statement
              :initform nil
              :type (or null statement))))

(defmethod arguments append ((self optional-statement))
  (when (sub-statement self) (list (sub-statement self))))


(defclass condition-expression ()
  ((condition :initarg :condition
              :accessor condition-expression
              :type expression)))

(defmethod arguments append ((self condition-expression))
  (when (slot-boundp self 'condition-expression)
    (list (condition-expression self))))


(defun initargs-in-order (class-designator)
  (flet ((initargs (slots)
           (mapcan
            (lambda (x) (copy-seq (closer-mop:SLOT-DEFINITION-INITARGS x)))
            slots))
         (instance-slots (slots)
           (remove ':class slots
                   :key (function closer-mop:SLOT-DEFINITION-ALLOCATION)))
         (class-from-designator (designator)
           (typecase designator
             (class designator)
             (t     (find-class designator)))))
    (let* ((class (class-from-designator class-designator))
           (all-slots (initargs (instance-slots
                                 (closer-mop:COMPUTE-SLOTS class))))
           (dir-slots (initargs (instance-slots
                                 (closer-mop:class-DIRECT-SLOTS class)))))
      (append dir-slots
              (subseq all-slots 0 (mismatch all-slots dir-slots
                                            :from-end t))))))


(defmacro define-statement (cl-name optional-superclasses fields c-keyword
                                    &key print-object c-sexp generate)
  `(progn
     (defclass ,cl-name (,@optional-superclasses statement)
       (,@(mapcar (lambda (field)
                      `(,field :initarg ,(make-keyword field) :accessor ,field))
                  fields)
        (c-keyword :reader c-keyword :allocation :class :initform ,c-keyword)))

     (defmethod arguments append ((self ,cl-name))
       (with-slots ,fields self
         (append
          ,@(mapcar (lambda (field)
                        `(when (slot-boundp self ',field)
                           (list (slot-value self ',field))))
                    fields))))

      (defmethod print-object ((self ,cl-name) stream)
       ,(or print-object
            `(print (cons ',cl-name
                          (mapcar (lambda (arg)
                                      (if (typep arg 'c-item)
                                        arg
                                        `(quote ,arg)))
                                  (arguments self))) stream))
       self)

     (defmethod c-sexp ((self ,cl-name))
       ,(or c-sexp
            `(cons ',(intern c-keyword "COM.INFORMATIMAGO.LINC.C")
                   (mapcar (function c-sexp) (arguments self)))))

     (defmethod generate ((self ,cl-name))
       ,generate)

     (defun ,cl-name (&rest args)
       (apply (function make-instance) ',cl-name
              (loop
                 :for key :in (initargs-in-order ',cl-name)
                 :for val :in args
                 :nconc (list key val))))))


(define-statement stmt-expr () (stmt-expression) "expression"
  :generate (progn
              (emit :fresh-line)
              (generate (stmt-expression self))
              (emit ";" :newline)))


(defgeneric ensure-statement (item)
  (:method ((self t))         (stmt-expr self))
  (:method ((self statement))   self))


(define-statement stmt-label (optional-statement) (identifier) "label"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self)) ,(identifier self)
  ;;    ,@(when (sub-statement self) (list (sub-statement self))))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C") ,(c-sexp (identifier self))
  ;;    ,@(when (sub-statement self) (list (c-sexp (sub-statement self)))))
  ;;                                       ; generate
  :generate (progn                                 
              (unless *same-line* (emit :fresh-line))
              (generate (identifier self)) (emit ":" " ")
              (generate (ensure-statement (sub-statement self)))))


(define-statement stmt-case (optional-statement) (case-value) "case"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self)) ,(case-value self)
  ;;    ,@(when (sub-statement self) (list (sub-statement self))))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C") ,(c-sexp (case-value self))
  ;;    ,@(when (sub-statement self) (list (c-sexp (sub-statement self)))))
  ;;                                       ; generate
  :generate (progn                                 
              (unless *same-line* (emit :fresh-line))
              (emit "case")
              (generate (case-value self))
              (emit ":" " ")
              (generate (ensure-statement (sub-statement self)))))


(define-statement stmt-default (optional-statement) () "default"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self)) 
  ;;    ,@(when (sub-statement self) (list (sub-statement self))))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C")
  ;;    ,@(when (sub-statement self) (list (c-sexp (sub-statement self)))))
  ;;                                       ; generate
  :generate (progn                                 
              (unless *same-line* (emit :fresh-line))
              (emit "default")
              (emit ":" " ")
              (generate (ensure-statement (sub-statement self)))))


(define-statement stmt-block () (block-statements) "block"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self))  ,@(block-statements self))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C")
  ;;    ,@(mapcar (function c-sexp) (block-statements self)))
  ;;                                       ; generate
  :generate (progn
              (with-parens "{}"
                (emit :newline)
                (dolist (item (block-statements self))
                  (generate (ensure-statement item)))
                (emit :fresh-line))))

(defmethod generate-with-indent ((self stmt-block))
  (generate self))


(define-statement stmt-let () (let-statements let-bindings) "let"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self))
  ;;    ,(let-bindings  self) ,@(let-statements self))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C")
  ;;    ,(mapcar (function c-sexp) (let-bindings  self))
  ;;    ,@(mapcar (function c-sexp) (let-statements self)))
  ;;                                       ; generate
  :generate (progn
              (emit :fresh-line "{")
              (dolist (decl (let-bindings self))
                (emit :newline)
                (generate decl)
                (emit ";"))
              (emit :newline)
              (dolist (item (let-statements self))
                  (generate (ensure-statement item)))
              (emit :fresh-line "}")))

(defmethod generate-with-indent ((self stmt-let))
  (generate self))


(define-statement stmt-if (condition-expression) (then else) "if"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self))
  ;;    ,(condition-expression  self)
  ;;    ,(then self)
  ;;    ,@(when (else self) (list (else self))))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C")
  ;;    ,(c-sexp (condition-expression  self))
  ;;    ,(c-sexp (then self))
  ;;    ,@(when (else self) (list (c-sexp (else self)))))
  ;;                                       ; generate
  :generate (progn
              (unless *same-line* (emit :fresh-line))
              (emit "if" "(")
              (generate (condition-expression self))
              (emit ")")
              (generate-with-indent (then self))
              (when (else self)
                (emit "else")
                (generate-with-indent (else self)))))


(define-statement stmt-switch (condition-expression optional-statement) () "switch"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self))
  ;;    ,(condition-expression  self)
  ;;    ,@(when (sub-statement self) (list (sub-statement self))))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C")
  ;;    ,(c-sexp (condition-expression  self))
  ;;    ,@(when (sub-statement self) (list (c-sexp (sub-statement self)))))
  ;;                                       ; generate
  :generate (progn
              (unless *same-line* (emit :fresh-line))
              (emit "switch" "(")
              (generate (condition-expression self))
              (emit ")")
              (generate-with-indent (sub-statement self))))


(define-statement stmt-while (condition-expression optional-statement) () "while"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self))
  ;;    ,(condition-expression  self)
  ;;    ,@(when (sub-statement self) (list (sub-statement self))))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C")
  ;;    ,(c-sexp (condition-expression  self))
  ;;    ,@(when (sub-statement self) (list (c-sexp (sub-statement self)))))
  ;;                                       ; generate
  :generate (progn
              (unless *same-line* (emit :fresh-line))
              (emit "while" "(")
              (generate (condition-expression self))
              (emit ")")
              (generate-with-indent (sub-statement self))))


(define-statement stmt-do (condition-expression optional-statement) () "do"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self))
  ;;    ,(condition-expression  self)
  ;;    ,@(when (sub-statement self) (list (sub-statement self))))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C")
  ;;    ,(c-sexp (condition-expression  self))
  ;;    ,@(when (sub-statement self) (list (c-sexp (sub-statement self)))))
  ;;                                       ; generate
  :generate (progn
              (unless *same-line* (emit :fresh-line))
              (emit "do")
              (if (sub-statement self)
                (generate-with-indent (sub-statement self))
                (emit ";"))
              (emit "while" "(")
              (generate (condition-expression self))
              (emit ")")))



(define-statement stmt-for (optional-statement)
    (for-init-statement go-on-condition step-expression) "for"
  ;;                                       ; print-object
  ;; `(,(class-name (class-of self))
  ;;    ,(for-init-statement  self)
  ;;    ,(go-on-condition     self)
  ;;    ,(step-expression     self)
  ;;    ,@(when (sub-statement self) (list (sub-statement self))))
  ;;                                       ; c-sexp
  ;; `(,(intern (c-keyword self) "COM.INFORMATIMAGO.LINC.C")
  ;;    ,(c-step (for-init-statement  self))
  ;;    ,(c-step (go-on-condition     self))
  ;;    ,(c-step (step-expression     self))
  ;;    ,@(when (sub-statement self) (list (c-sexp (sub-statement self)))))
  ;;                                       ; generate
  :generate (progn
              (unless *same-line* (emit :fresh-line))
              (emit "for" "(")
              (generate (for-init-statement  self))
              (emit ";")
              (generate (go-on-condition     self))
              (emit ";")
              (generate (step-expression     self))
              (emit ")")
              (if (sub-statement self)
                (generate-with-indent (sub-statement self))
                (emit ";"))))


(define-statement stmt-break () () "break"
  :generate (progn
              (unless *same-line* (emit :fresh-line))
              (emit "break" ";" :newline)))

(define-statement stmt-continue () () "continue"
  :generate (progn
              (unless *same-line* (emit :fresh-line))
              (emit "continue" ";" :newline)))

(define-statement stmt-return () (return-result) "return"
  :generate (progn
              (unless *same-line* (emit :fresh-line))
              (emit "return" " ")
              (generate (return-result self))
              (emit ";" :newline)))

(define-statement stmt-goto () (identifier) "goto"
  :generate (progn
              (unless *same-line* (emit :fresh-line))
              (emit "goto" " ")
              (generate (identifier self))
              (emit ";" :newline)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DECLARATIONS
;;;


(defclass declaration (c-item)
  ())


(defmethod ensure-statement ((self declaration)) self)

(defmacro define-declaration (name fields &key generate)
  `(progn
     
     (defclass ,name (declaration)
       (,@(mapcar (lambda (field)
                      `(,field
                        :initarg ,(make-keyword field)
                        :accessor ,field))
                  fields)))
     
     (defmethod arguments append ((self ,name))
       (with-slots ,fields self
         (append
          ,@(mapcar (lambda (field)
                        `(when (slot-boundp self ',field)
                           (list (slot-value self ',field))))
                    fields))))

     (defmethod print-object ((self ,name) stream)
       (print (cons ',name
                    (mapcar (lambda (arg)
                                (if (typep arg 'c-item)
                                  arg
                                  `(quote ,arg)))
                            (arguments self))) stream)
       self)

     (defmethod c-sexp ((self ,name))
       (cons
         ',(intern (substitute #\_ #\- (string-downcase name)) "COM.INFORMATIMAGO.LINC.C")
         (mapcar (function c-sexp) (arguments self))))

     (defmethod generate ((self ,name))
       (with-slots ,fields self
         ,generate))

     (defun ,name (&rest args)
       (apply (function make-instance) ',name
              (loop
                 :for key :in (initargs-in-order ',name)
                 :for val :in args
                 :nconc (list key val))))))


(define-declaration ASM (asm-string)
  :generate (progn
              (emit :fresh-line "asm")
              (with-parens "()" (generate asm-string))
              (emit ";" :newline)))

(define-declaration NAMESPACE
    (namespace-identifier namespace-body)
  :generate (progn
              (emit :fresh-line "namespace")
              (when namespace-identifier
                (emit " ")
                (generate-expression namespace-identifier))
              (with-parens "{}"
                (dolist (declaration namespace-body)
                  (generate declaration)))
              (emit :newline)))

(defmacro with-namespace (ident &body body)
  `(namespace ,ident (list ,@body)))


(define-declaration NAMESPACE-ALIAS
    (namespace-identifier namespace-qualified-specifier)
  :generate (progn
              (emit :fresh-line "namespace")
              (generate namespace-identifier)
              (emit "=")
              (generate namespace-qualified-specifier)
              (emit ";" :newline)))
     
(define-declaration USING-TYPENAME (using-name)
  :generate (progn
              (emit :fresh-line "using" " " "typename" " ")
              (generate using-name)
              (emit ";" :newline)))
     
(define-declaration USING-NAMESPACE (using-name)
  :generate (progn
             (emit :fresh-line "using" " " "namespace" " ")
             (generate using-name)
             (emit ";" :newline)))

(define-declaration USING-SCOPE (using-name)
  :generate (progn
              (emit :fresh-line  "using" " ")
              (generate (absolute-scope using-name))
              (emit ";" :newline)))
     
(define-declaration TEMPLATE (template-parameter-list sub-declaration)
  :generate (progn
              (emit :fresh-line "template" "<")
              (generate-list ","
                             (function generate)
                             template-parameter-list)
              (emit ">")
              (when template-parameter-list (emit :newline))
              (generate sub-declaration)))

(define-declaration export-TEMPLATE
    (template-parameter-list sub-declaration)
  :generate (progn
              (emit :fresh-line "export" "template" "<")
              (generate-list ","
                             (function generate)
                             template-parameter-list)
              (emit ">")
              (when template-parameter-list (emit :newline))
              (generate sub-declaration)))

(define-declaration TEMPLATE1 (sub-declaration)
  :generate (progn
              (emit :fresh-line  "template" " ")
              (generate sub-declaration)))

(define-declaration EXTERN1 (extern-string sub-declaration)
  :generate (progn
              (emit :fresh-line  "extern" " ")
              (generate extern-string)
              (generate sub-declaration)))

(define-declaration EXTERN (extern-string sub-declarations)
  :generate (progn
              (emit :fresh-line  "extern" " ")
              (generate extern-string)
              (with-parens "{}"
                (dolist (sub-declaration sub-declarations)
                  (generate sub-declaration)))
              (emit :newline)))

(defmacro with-extern (extern-name &body sub-declarations)
  `(extern ,extern-name (list ,@sub-declarations)))
    



;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; ptr-operator:
;;   * cv-qualifier-seq
;;                (pointer [const] [volatile])  | pointer
;;   :: nested-name-specifier * cv-qualifier-seq
;;                (member-pointer <class> [const] [volatile])
;;   &
;;                reference

;; TODO: We need to manage some level with declarators. Check if we
;;       can use the same *level* as the expressions.

(defclass declarator (c-item)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defmethod parameters ((self ordinary-lambda-list))
   (append (lambda-list-mandatory-parameters self)
           (lambda-list-optional-parameters self)
           (and (lambda-list-rest-parameter-p self)
                (lambda-list-rest-parameter self))
           (lambda-list-keyword-parameters self))))

(defmacro define-declarator (name lambda-list &key generate)
  (let ((fields
         (let ((ll (parse-lambda-list lambda-list)))
           (mapcar (function parameter-name) (parameters ll)))))
    `(progn
     
       (defclass ,name (declarator)
         (,@(mapcar (lambda (field)
                        `(,field
                          :initarg ,(make-keyword field)
                          :accessor ,field))
                    fields)))
     
       (defmethod arguments append ((self ,name))
         (with-slots ,fields self
           (append
            ,@(mapcar (lambda (field)
                          `(when (slot-boundp self ',field)
                             (list (slot-value self ',field))))
                      fields))))

       (defmethod print-object ((self ,name) stream)
         (print (cons ',name
                      (mapcar (lambda (arg)
                                  (if (typep arg 'c-item)
                                    arg
                                    `(quote ,arg)))
                              (arguments self))) stream)
         self)

       (defmethod c-sexp ((self ,name))
         (cons
          ',(intern (substitute #\_ #\- (string-downcase name)) "COM.INFORMATIMAGO.LINC.C")
          (mapcar (function c-sexp) (arguments self))))

       (defmethod generate ((self ,name))
         (with-slots ,fields self
           ,generate))

       (defun ,name  ,lambda-list
         (make-instance ',name
           ,@(loop :for field :in fields
                :nconc (list (make-keyword field) field))))
    
       ',name)))


(define-declarator reference (sub-declarator)
  :generate (progn (emit "&") (generate sub-declarator)))

(define-declarator pointer (sub-declarator &key (const nil) (volatile nil))
  :generate (progn (emit "*")
                   (when const    (emit " " "const"))
                   (when volatile (emit " " "volatile"))
                   (emit " ")
                   (generate sub-declarator)))

(define-declarator member-pointer (nested-name-specifier
                                   sub-declarator
                                   &key (const nil) (volatile nil))
  :generate (progn (generate nested-name-specifier)
                   (emit "*")
                   (when const    (emit " " "const"))
                   (when volatile (emit " " "volatile"))
                   (emit " ")
                   (generate sub-declarator)))


;;-------------------------------
;; direct-declarator
;;

(define-declarator c-function (sub-declarator
                               parameters
                               &key (const nil) (volatile nil) throw)
  :generate (progn
              (typecase sub-declarator
                ;; or use some *level* and priority
                ((or c-function c-vector        ; direct-declarator
                     expr-scope absolute-scope) ; declarator-id
                 (generate sub-declarator))
                (declarator
                 (with-parens "()"
                   (generate sub-declarator)))
                (c-item
                 (error "A random C-ITEM ~S as C-FUNCTION sub-declarator, really?"
                        sub-declarator))
                (t ;; raw declarator-id
                 (generate sub-declarator)))
              (with-parens "()"
                (generate-list ","
                               (function generate)
                               parameters))
              (when const    (emit " " "const"))
              (when volatile (emit " " "volatile"))
              (when (slot-boundp self 'throw)
                (emit " " "throw")
                (with-parens "()"
                  (generate-list ","
                                 (function generate)
                                 throw)))
              (emit " ")))


(define-declarator c-vector (sub-declarator &optional dimension)
  :generate (progn
              (typecase sub-declarator
                ;; or use some *level* and priority
                ((or c-function c-vector        ; direct-declarator
                     expr-scope absolute-scope) ; declarator-id
                 (generate sub-declarator))
                (declarator
                 (with-parens "()"
                   (generate sub-declarator)))
                (c-item
                 (error "A random C-ITEM ~S as C-VECTOR sub-declarator, really?"
                        sub-declarator))
                (t ;; raw declarator-id
                 (generate sub-declarator)))
              (with-parens "[]"
                (generate dimension))))




;;;; THE END ;;;;


