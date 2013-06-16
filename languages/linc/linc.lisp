;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               linc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    LINC Is Not C, but almost.
;;;;
;;;;    The purpose is to be able to use Common Lisp
;;;;    at the meta-programming level to generate C sources.
;;;;    Linc programs can also be executed and debugged in the
;;;;    Common Lisp environment.
;;;;   
;;;;    A linc file contains normal Common Lisp expressions,
;;;;    and linc expressions.  When compiling the linc file,
;;;;    the Common Lisp expressions are executed, which will  
;;;;    generate a corresponding C source.
;;;;
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-09-02 <PJB> Partial generation of expressions and statements.
;;;;    2005-10-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2007
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

;; file:///home/pjb/library/informatique/protocols_and_standards/ISO-IEC-14882/www.kuzbass.ru:8086/docs/isocpp/index.html

(in-package "COM.INFORMATIMAGO.LINC")  



(defparameter *name-map* (make-hash-table :test (function equal)))


;; (make-instance 'expression
;;   :operator (find-operator 'c::+)
;;   :arguments '(2 3))


;;  (in-package :com.informatimago.linc)
;;  (in-package :com.informatimago.linc.c)

;; c1::c2::m1(a,b,c);
;; ((:: c1 c2 m1) (a b c))

;; (defparameter *operator-map*
;;   (loop
;;      :with opmap = (make-hash-table)
;;      :for priority :from 0
;;      :for level :in *operator-precedence*
;;      :do (loop
;;             :for ops :in (cdr level)
;;             :do (loop
;;                    :for op :in (ensure-list (car ops))
;;                    :do (setf (gethash op opmap) (list* (car level) priority
;;                                                        (cdr  ops)))))
;;      :finally (return opmap)))
;; 
;; 
;; (defun operator (op)
;;   (or (gethash op *operator-map*)
;;       ;; TODO: what about linc macros?
;;       (gethash 'c::call *operator-map*)))

;; (defun operator-associativity (op)  (first  (operator op)))
;; (defun operator-precedence    (op)  (second (operator op)))
;; (defun operator-arity         (op)  (third  (operator op)))
;; (defun operator-generate      (op)  (or (fourth (operator op)) (symbol-name op)))


;; (defun operator-associativity (op)  (associativty  (find-operator op)))
;; (defun operator-precedence    (op)  (priority      (find-operator op)))
;; (defun operator-arity         (op)  (arity         (find-operator op)))
;; (defun operator-generate      (op)  (generator (find-operator op)))
;; 
;; (defun expression-operator   (expr)
;;   (cond
;;     ((symbolp expr)                      'c::identifier)
;;     ((atom expr)                         'c::literal)
;;     ((find-raw-operator (car expr))      (car expr))
;;     (t                                   'c::call)))
;; 
;; (defun expression-arguments  (expr)
;;   (cond
;;     ((atom expr)                         (list expr))
;;     ((find-raw-operator (car expr))      (cdr expr))
;;     (t                                   expr)))
;; 
;; (defun expression-precedence (expr)
;;   (operator-precedence (expression-operator expr)))



;; (maphash (lambda (k v) (print (list k (operator-generate k)))) *operator-map*)
;; (operator-precedence (car '((c::scope c1 c2 m1) (a b c))))
;; (operator-precedence (car '(c::scope c1 c2 m1)))
    
;; (generate-statement '(progn
;;                                (= a (+ (+ (+ (+ a b) c) d) e))
;;                                (= a (+ a (+ b (+ c (+ d e)))))
;;                                (= a (+ a b c d e))))
;; {
;; a=((((a+b)+c)+d)+e); /* left */
;; a=(a+(b+(c+(d+e))));
;; a=(a+b+c+d+e);
;; }
;; 
;; (generate-statement '(progn
;;                                (= (= (= (= a b) c) d) 0); invalid!
;;                                (= a (= b (= c (= d 0))))
;;                                #|(= a b c d)|#))
;; {
;; (((a=b)=c)=d)=0; /* invalid! */
;; a=(b=(c=(d=0))); /* right */
;; }




;; (defun generate-expression (expr &key (level 99 levelp) (naked t))
;;   ;;   (+ a (* b c))    (10 16 (11 16 16))
;;   ;;   a + b*c
;;   ;; 
;;   ;;   (* (+ a b) c)    (11 (10 16 16) 16)
;;   ;;   (a+b) * c
;;   ;; 
;;   ;;   (+ a (+ b c))    (10 16 (10 16 16))
;;   ;;   a + (b+c)
;;   ;; 
;;   ;;   (+ (+ a b) c)    (10 (10 16 16) 16)
;;   ;;   a+b+c
;;   ;; 
;;   ;; 
;;   ;;   (= a (= b c))    (1 16 (1 16 16))
;;   ;;   a = b=c
;;   (when (and naked (not levelp)) (setf level -1))
;;   (let* ((operator (expression-operator expr))
;;          (oplevel  (operator-precedence operator)))
;;     (if (< oplevel level)
;;       ;; need parentheses:
;;       (with-parens "()" (generate-expression expr :level oplevel :naked nil))
;;       ;; no need for parentheses:
;;       (let ((argc (length (expression-arguments  expr)))
;;             (gene (operator-generate operator)))
;;         (unless (ecase (operator-arity operator)
;;                   (3    (=  3 argc))
;;                   (2    (=  2 argc))
;;                   (1    (=  1 argc))
;;                   (2-*  (<= 2 argc))
;;                   (1-*  (<= 1 argc))
;;                   (0-*  t))
;;           (error "Syntax error in: ~S~%~
;;                     Expected ~A arguments, got ~A"
;;                  expr (operator-arity operator) argc))
;;         (etypecase gene
;;           (string
;;            (if (eql 1 (operator-arity operator))
;;              (progn
;;                (emit gene)
;;                (generate-expression (first (expression-arguments expr))
;;                                     :level oplevel :naked nil))
;;              (generate-list
;;               gene
;;               (lambda (item) (generate-expression item :level oplevel :naked nil))
;;               (expression-arguments  expr))))
;;           (function
;;            (apply gene oplevel (expression-arguments  expr))))))))



;; (generate-statement
;;  (%label <identifier> [<statement>])
;;  (%case <constant-expression> [<statement>])
;;  (%default [<statement>])
;;  (%block [<statement>...])
;;  (%if <condition> [<statement>] [<statement>]])
;;  (%switch <condition> [<statement>])
;;  (%while <condition> [<statement>])
;;  (%do [<statement>] <expression>)
;;  (%for (<for-init-statement> [<condition>] [<expression>]) [<statement>])
;;  (%break)
;;  (%continue)
;;  (%return [<expression>])
;;  (%goto <identifier>)
;;  [<expression>])


;; (defun generate-statement (statement &key same-line)
;;   (if (atom statement)
;;     (progn ;; label
;;       (unless same-line (emit :newline))
;;       (emit statement ":"))
;;     (case (first statement)
;;       ((c::block)
;;        (emit "{")
;;        (map nil (function generate-statement)  (rest statement))
;;        (emit :fresh-line "}"))
;;       ((c::let)
;;        (emit :fresh-line "{")
;;        (when (second statement)
;;          (map nil (lambda (decl)
;;                       (emit :newline)
;;                     (generate-parameter decl)
;;                     (emit ";"))
;;               (second  statement))
;;          (emit :newline))
;;        (map nil (function generate-statement) (cddr statement))
;;        (emit :fresh-line "}"))
;;       ((c::if)
;;        (unless same-line (emit :newline))
;;        (case (length statement)
;;          (3
;;           (emit "if" "(")
;;           (generate-expression (second statement))
;;           (emit ")")
;;           (generate-statement (third statement)))
;;          (4
;;           (emit "if" "(")
;;           (generate-expression (second statement))
;;           (emit ")")
;;           (generate-statement (third statement))
;;           (emit "else")
;;           (generate-statement (fourth statement)))
;;          (otherwise
;;           (error "Syntax error in ~S; ~%~
;;               Expected syntax: (IF condition then-statement [else-statement])~%~
;;               Got: ~S" (first statement) statement))))
;;       ((c::case)
;;        (unless same-line (emit :newline))
;;        (when (<= (length statement) 1)
;;          (error "Syntax error in ~S; ~%~
;;              Expected syntax: (CASE expression (constants statement...)...)~%~
;;              Got: ~S" (first statement) statement))
;;        (emit "switch" "(")
;;        (generate-expression (second statement))
;;        (emit ")" "{")
;;        (map nil (lambda (clause)
;;                     (map nil (lambda (constant)
;;                                  (if (eq constant c::otherwise)
;;                                    (emit "default" ":")
;;                                    (progn
;;                                      (emit "case")
;;                                      (generate-expression constant)
;;                                      (emit ":"))))
;;                          (ensure-list (first clause)))
;;                   (map nil (function generate-statement) (rest clause))
;;                   (emit :fresh-line "break" ";"))
;;             (cddr statement))
;;        (emit :fresh-line "}"))
;;       ((c::while)
;;        (unless same-line (emit :newline))
;;        (when (<= (length statement) 1)
;;          (error "Syntax error in ~S; ~%~
;;              Expected syntax: (WHILE condition statement...)~%~
;;              Got: ~S" (first statement) statement))
;;        (emit "while" "(")
;;        (generate-expression (second statement))
;;        (emit ")")
;;        (generate-statement (if (= 1 (length (cddr statement)))
;;                              (third statement)
;;                              `(c::block ,@(cddr statement)))))
;;       ((c::do)
;;        (unless same-line (emit :newline))
;;        (when (or (<= (length statement) 3)
;;                  (not (eq 'c::while (first (last statement 2)))))
;;          (error "Syntax error in ~S; ~%~
;;              Expected syntax: (DO statement ... WHILE condition)~%~
;;              Got: ~S" (first statement) statement))
;;        (emit "do")
;;        (let ((body (butlast (rest statement) 2)))
;;          (generate-statement (if (= 1 (length body))
;;                                body
;;                                `(c::block ,@body))))
;;        (emit "while" "(")
;;        (generate-expression (first (last statement)))
;;        (emit ")"))
;;       ((c::for)
;;        (unless same-line (emit :newline))
;;        (when (< (length statement) 4)
;;          (error "Syntax error in ~S; ~%~
;;              Expected syntax: (FOR init increment stop statement ...)~%~
;;              Got: ~S" (first statement) statement))
;;        (destructuring-bind (for init increm stop . body) statement
;;          ;; (for initial-stat increment-expr stop-expr &body body)
;;          (emit "for" "(")
;;          (if init
;;            (generate-statement init)
;;            (emit ";"))
;;          (generate-expression increm)
;;          (emit ";")
;;          (generate-expression stop)
;;          (emit ")")
;;          (generate-statement (if (= 1 (length body))
;;                                body
;;                                `(c::block ,@body)))))
;;       ((c::break)
;;        (unless same-line (emit :newline))
;;        (when (< 1 (length statement))
;;          (error "Syntax error in ~S; ~%~
;;              Expected syntax: (BREAK)~%~
;;              Got: ~S" (first statement) statement))
;;        (emit "break" ";"))
;;       ((c::continue)
;;        (unless same-line (emit :newline))
;;        (when (< 1 (length statement))
;;          (error "Syntax error in ~S; ~%~
;;              Expected syntax: (CONTINUE)~%~
;;              Got: ~S" (first statement) statement))
;;        (emit"continue" ";"))
;;       ((c::return)
;;        (unless same-line (emit :newline))
;;        (case (length statement)
;;          (1 (emit "return" ";"))
;;          (2 (emit "return" "(")
;;             (generate-expression (second statement))
;;             (emit ")" ";"))
;;          (otherwise
;;           (error "Syntax error in ~S; ~%~
;;               Expected syntax: (RETURN [result])~%~
;;               Got: ~S" (first statement) statement))))
;;       ((c::goto)
;;        (unless same-line (emit :newline))
;;        (when (/= 2 (length statement))
;;          (error "Syntax error in ~S; ~%~
;;              Expected syntax: (GOTO identifier)~%~
;;              Got: ~S" (first statement) statement))
;;        (emit "goto" " ")
;;        (generate-expression (second statement))
;;        (emit ";"))
;;       (otherwise
;;        (unless same-line (emit :newline))
;;        (generate-expression statement)
;;        (emit ";")))))

;; (::)
;; (generate-declaration
;;
;; (vector type  [<constant-expression>])
;; (pointer type [const] [volatile])
;; (reference type)
;; (function (arg-type...) [result-type] [const] [volatile] (throw exception...))
;; (pointer const volatile typename _) ; typename* const volatile   name;
;; (pointer const volatile          _) ; * const volatile name;
;; (reference _)
;; 
;; (declare ((pointer type) name))         ; type* name;
;; (declare ((pointer type) name 0)        ; type* name=0;
;;          ((function (int (pointer const char)) void const (throw (:: std exception)))
;;           fname) ; void fname(int,char const*) throw(std::exception);
;;          ((vector (vector (vector int 4) 5) 6) a) ; int a[6][5][4];
;;          )
;;  
;;  )

;; (generate-statement
;;  (%label <identifier> [<statement>])
;;  (%case <constant-expression> [<statement>])
;;  (%default [<statement>])
;;  (%block [<statement>...])
;;  (%if <condition> [<statement>] [<statement>]])
;;  (%switch <condition> [<statement>])
;;  (%while <condition> [<statement>])
;;  (%do [<statement>] <expression>)
;;  (%for (<for-init-statement> [<condition>] [<expression>]) [<statement>])
;;  (%break)
;;  (%continue)
;;  (%return [<expression>])
;;  (%goto <identifier>)
;;  [<expression>])

;; (%switch state
;;         (%case 1)
;;         (printf "one\n")
;;         (%break)
;;         (%case 2)
;;         (printf "two\n")
;;         (%case 3) (%case 4 (printf "three of four\n")) (%break))

(defun generate-parameter (parm)
  (generate-expression (second parm))
  (emit " ")
  (generate-identifier (first parm))
  (when (cddr parm)
    (emit "=")
    (generate-expression (third parm))))


(defun generate-definition (def)
  (ecase (first def)
    ((com.informatimago.linc.c::defun)
     (destructuring-bind (name arguments result-type &rest body) (rest def)
       (emit :newline)
       (generate-expression result-type)
       (emit " ")
       (generate-identifier name)
       (emit "(")
       (when arguments
         (loop
            :for arg :in (rest arguments)
            :initially (generate-parameter (first arguments))
            :do (emit ",") (generate-parameter arg)))
       (emit ")")
       (generate-statement
        (if (and (null (cdr body))
                 (member (caar body) '(com.informatimago.linc.c::let com.informatimago.linc.c::block)))
            (first body)
            `(com.informatimago.linc.c::block ,@body)))))))


(defmacro com.informatimago.linc.c::when   (condi &body body)
  `(com.informatimago.linc.c::if mcondi          (com.informatimago.linc.c::block ,@body)))

(defmacro com.informatimago.linc.c::unless (condi &body body)
  `(com.informatimago.linc.c::if (com.informatimago.linc.c::not ,condi) (com.informatimago.linc.c::block ,@body)))

(defmacro com.informatimago.linc.c::setf (place expression &rest others)
  (if others
      `(com.informatimago.linc.c::block
         (com.informatimago.linc.c::= ,place ,expression)
         (com.informatimago.linc.c::setf ,@others))
      `(com.informatimago.linc.c::= ,place ,expression)))

(defmacro com.informatimago.linc.c::let* (bindings &body body)
  (if (null bindings)
      `(com.informatimago.linc.c::block ,@body)
      `(com.informatimago.linc.c::let (,(first bindings))
         (com.informatimago.linc.c::let* (rest bindings) ,@body))))

(defmacro com.informatimago.linc.c::comment (&rest items)
  `(progn
     (emit :newline)
     (with-parens ("/*" "*/")
       ,@(mapcar (lambda (item) `(emit :fresh-line ,(format nil "~A" item)))
                 items)
       (emit :newline))))

(defmacro com.informatimago.linc.c::define-function (name arguments result-type &body body)
  (com.informatimago.linc::generate-definition
   `(com.informatimago.linc.c::defun ,name ,arguments ,result-type (com.informatimago.linc.c::block ,@body))))


(defun compile-linc-file (input-file &key verbose print
                          (if-does-not-exist :error)
                          (external-format :default)
                          output-file)
  (with-open-file (input input-file
                         :direction :input
                         :if-does-not-exist if-does-not-exist
                         :external-format external-format)
    (with-open-file (output (or output-file
                                (make-pathname
                                 :type "C" :case :common
                                 :defaults input-file))
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format external-format)
      (with-open-file (header (make-pathname
                               :type "H" :case :common
                               :defaults (or output-file input-file)) 
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format external-format)
        (let ((*c-out* output))
          (load input))))))



(defun repl ()
  (catch 'repl     ; allow for emergency exit with (throw 'com.informatimago.linc::repl)
    (let ((*package* (find-package "C"))
          (*print-pretty* nil)
          (eof *standard-input*)
          (hist 0))
      (loop
         (incf hist)
         (format t "~%~A[~D]> " (package-name *package*) hist)
         (finish-output)
         (handling-errors
          (setf - (read *standard-input* nil eof))
          (cond
            ((or (eq - eof)
                 (and (atom -) (member - '(:quit :exit :continue)
                                       :test (function string-equal))))
             (return-from repl))
            ((and (atom -) (string-equal - :help))
             (format t "~2%==== LINC REPL ====~2%")
             (format t ":QUIT, :EXIT, :CONTINUE    Exit this REPL.~%")
             (format t ":HELP                      Prints this help.~%")
             (format t "Any other S-expression is interpreted tentatively both ~%~
                       as C expression and C statement, and corresponding C ~%~
                       code is printed~%"))
            (t
             (let ((res-expr (multiple-value-bind (val err)
                                 (ignore-errors
                                   (with-output-to-string (*c-out*)
                                     (generate-expression -)))
                               (if err err val)))
                   (res-stat (multiple-value-bind (val err)
                                 (ignore-errors
                                   (with-output-to-string (*c-out*)
                                     (generate-statement -)))
                               (if err err val))))
               (setf +++ ++   ++ +   + -
                     /// //   // /   / (list res-expr res-stat)
                     *** **   ** *   * (first /))
               (format t "~&expression --> ~A~%"  res-expr)
               (format t "~&statement  --> ~A~%"  res-stat))))
          (finish-output t))))))

;; quit
;; (repl)
         
;; (load (compile-file "example.linc"))
;; 
;; CL compiles and CL loads and executes the example.linc program.
;; To execute a LINC program we provide a C semantics layer.
;; 
;; 
;; 
;; (define-module example
;;   (:c-name "example")
;;   (:export simple_addition))
;; (in-module example)
;; (use-module "<string.h>")
;; 
;; (define-type string_t (pointer unsigned-char))
;; 
;; (define-function string_add ((a string_t) (b string_t)) string_t
;;   (let ((av int)
;;         (bv int)
;;         (res string_t (malloc (+ 2 (max (strlen a) (strlen b))))))
;;     (sscanf a "%d" (address av))
;;     (sscanf b "%d" (address bv))
;;     (sprintf res "%d" (+ a b))
;;     (return res)))
;; 
;; (define-function simple_addition
;;     ((a int) (b signed-short) (c unsigned-char) (d float))
;;     int
;;   (return (+ a b c d)))
;; 
;; 
;; int simple_addition (int a,signed short b,unsigned char c,float d){
;;    return(a+b+c+d);
;; }
;; 
;; 
;; (defun string_add (a b)
;;   (assert (c:subtypep (c:type-of a) '(c:pointer c:unsigned-char)))
;;   (assert (c:subtypep (c:type-of b) '(c:pointer c:unsigned-char)))
;;   (let ((av 0) (bv 0) (res (make-string (+ 2 (strlen a) (strlen b)))))))
;; (defun simple_addition (a b c d)
;;   (assert (c:subtypep (c:type-of a) 'c:int))
;;   (assert (c:subtypep (c:type-of b) 'c:signed-short))
;;   (assert (c:subtypep (c:type-of c) 'c:unsigned-char))
;;   (assert (c:subtypep (c:type-of d) 'c:float))
;;   (c:int (+ (c:value-of a) (c:value-of b) (c:value-of c) (c:value-of d))))
;; 
;; 
;; (defun c:+ (arg &rest args)
;;   ())
;; 
;; 
;; (com.informatimago.linc:compile-file "example.linc")
;; 
;; LINC "compiles" the example.linc program, that is, generate C header
;; and source files.
;; 
;; 
;; (com.informatimago.linc:compile-file "example.linc"
;;                    :external-format charset:utf-8
;;                    :verbose t
;;                    :print   t
;;                    :output-file "example.c"
;;                    :ouput-file-type "m"
;;                    :c-compilation-command "make example")
;; 
;; 
;; 
;;     signed-char
;;     unsigned-char
;;     char
;; 
;;     short-int
;;     int
;;     long-int
;;     unsigned-short-int
;;     unsigned-int
;;     unsigned-long-int
;; 
;;     float
;;     double-float
;;     long-float
;; 
;;     void
;; 
;; 
;;     (define-module bcmem
;;         (:c-name "BcMem")
;;       (:export allocate deallocate copy))
;; 
;;     (define-module bcstring
;;         (:c-name "BcString")
;;       (:export id s p set-capacity-copy))
;;     (in-module bcstring)
;;     (use-module "<string.h>")
;;     (use-module bcmem)
;; 
;;     (define-variable  ID
;;         (array (*) (const char))
;;       "$Id: BcString.c,v 1.3 2004/01/21 06:26:09 pjbpjb Exp $")
;; 
;;     (define-type S
;;         (structure
;;          (data       (pointer char))
;;          (dlength    INT32)
;;          (allocation INT32)))
;; 
;;      (define-type P (pointer S))
;; 
;;     (comment "
;;         INVARIANTS:
;;             data#NIL
;;             1<=allocation
;;             0<=dlength<allocation
;;             data[dlength]=(char)0
;;             for all i in [0..dlength-1], data[i]#(char)0
;;     ")
;; 
;;     (define-constant Alloc-Increment 128)
;;     (define-macro Minimum (a b) (if (< a b) a b))
;; 
;;     (define-function Set-Capacity-Copy
;;         ((t t) (nAllocation INT32) (copy BOOLEAN)) T
;;         (let ((this P (cast t P))
;;               (ndata (pointer char))
;;               (nLength INT32))
;;           (if (> nAllocation 1)
;;               (progn
;;                 (setf nData (BcMem:Allocate (* (sizeof char) nAllocation)))
;;                 (if copy
;;                     (progn
;;                       (setf nLength (Minimum (1- nAllocation) (-> this dlength)))
;;                       (BcMem:Copy (-> this data) nData (* nLength (sizeof char))))
;;                     (setf nLength 0)))
;;               (setf nAllocation 1
;;                     nData (BcMem:Allocate (* (sizeof char) nAllocation))
;;                     nLength 0))
;;           (setf (aref nData  nLength) (cast 0 char))
;;           (BcMem:Deallocate (cast (address (-> this data))
;;                                   (pointer (pointer void))))
;;           (setf (-> this data)       nData
;;                 (-> this dlength)    nLength
;;                 (-> this allocation) nAllocation)
;;           (return this)))
;; 
;; 
;; 
;;     (--> (define-variable ?identifier ?type (&optional ?initform))
;;          (if (exported-p ?identifier)
;;              (progn
;;                (in-header "extern" ?type ?identifier ";")
;;                (in-body  ?type ?identifier (when ?initform
;;                                              "=" ?initform) ";"))
;;              (in-body "static" ?type ?identifier (when ?initform
;;                                                    "=" ?initform) ";")))
;; 
;;     (--> (define-type ?identifier ?type)
;;          (if (exported-p ?identifier)
;;              (in-header "typedef" ?type ?identifier)
;;              (in-body   "typedef" ?type ?identifier)))
;; 
;; 
;;     (--> (scope (&optional ?class) ?identifier)
;;          (when ?class ?class) "::" ?identifier)
;; 
;;     (--> (comment ?comment) "/*" ?comment "*/")
;; 
;;     (--> (define-constant ?constant-identifier ?expression)
;;          "#define" ?constant-identifier ?expression)
;; 
;;     (--> (define-macro ?identifier ?arguments ?expression)
;;          "#define" ?identifier ?arguments ?expression)
;; 
;;     (--> (return ?expression)
;;          "return" "(" ?expression ")" ";")
;; 
;; 
;; 
;;     (defparameter *special-operators* (make-hash-table))
;; 
;;     (defun define-special-operator (name generator)
;;       (setf (gethash name *special-operators*) generator))
;; 
;;     (defun spec-gen (name)
;;       (gethash name *special-operators*))
;; 
;; 
;;     (defmacro defspec (name arguments &body body)
;;       (define-special-operator ',name `(lambda ,arguments ,@body)))

(defmacro com.informatimago.linc.c::c (&rest declarations)
  `(cl:block
     ,@(mapcar (function com.informatimago.linc::generate-declaration) declarations)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
 (defun pcond-variable-p (pattern)
   (and (symbolp pattern)
        (<= 1 (length (string pattern)))
        (char= #\? (aref (string pattern) 0))))

 (defun pcond-substitute-literals (pattern)
   "
DO:      Renames any atom found in pattern, collecting them in two
         binding a-list, one for the literals, another for the
         variables.
RETURN:  A destructuring-lambda-list; a literal a-list ; a variable a-list.
"
   (cond
     ((consp pattern)
      (multiple-value-bind (adll asal aval) (pcond-substitute-literals (car pattern))
        (multiple-value-bind (ddll dsal dval) (pcond-substitute-literals (cdr pattern))
          (values (cons adll ddll) (nconc asal dsal) (nconc aval dval)))))
     ((find pattern LAMBDA-LIST-KEYWORDS)
      (values pattern nil))
     ((pcond-variable-p pattern)
      (let ((var (gensym)))
        (values var nil (list (cons var pattern)))))
     ((null pattern)
      (values nil nil nil))
     (t
      (let ((var (gensym)))
        (values var (list (cons var pattern)) nil))))))

(defmacro pcond (expression &rest clauses)
  (let ((vexpression (gensym)))
    `(let ((,vexpression ,expression))
       (cond
         ,@(mapcar
            (lambda (clause)
                (multiple-value-bind (dll sal val)
                    (pcond-substitute-literals (first clause))
                  `((let ,(mapcar (function cdr) val)
                      (when (ignore-errors
                              (destructuring-bind ,dll ,vexpression
                                (when (and
                                       ,@(mapcar
                                          (lambda (binding)
                                              `(equal ,(car binding) ',(cdr binding)))
                                          sal))
                                  (setf ,@(mapcan
                                           (lambda (binding)
                                               (list (cdr binding) (car binding)))
                                           val))
                                  t)))
                        ,@(rest clause))))))
            clauses)))))

;; ;;
;; ;; variable
;; ;; 
;; ;;      int x;
;; (declare x int)
;; ;;      int y=42;
;; (declare y int 42)
;; ;;      char *a=0,*b=0,*c=0;
;; (declare (a b c) (pointer char) 0)
;; ;;      int (*f)(int x);
;; (declare f (pointer (function ((x int)) int)))
;; ;; 
;; ;;      int f(int x);
;; (declare f (function ((x int)) int))
;; ;;
;; ;; function
;; ;;
;; ;;      int f(int x){ /* body */ }
;; (declare f (function ((x int)) int)
;;   (progn ...))
;; ;;


;; (declare colors (enum (blue 1) white red))
;; enum { blue=1, white, red } colors;


(defun generate-type (expression &key name)

  (ecase (first expression)
    
    ((com.informatimago.linc.c::class com.informatimago.linc.c::struct com.informatimago.linc.c::union)
     (emit (format nil "~(~A~)" (first expression)))
     (cond
       ((listp (second expression))
        ;; (class (superclass...) :public|:protected|:private member...)
        ;; superclass ::= (classname [:virtual] [:public|:protected|:private])
        ;; superclass ::= classname
         
        (when name
          (emit " ")
          (generate name))
        (when (second expression)
          (emit ":")
          (generate-list ","
                         (lambda (superclass)
                             (if (listp superclass)
                               (case (length item)
                                 ((1) (generate (first item)))
                                 ((2 3)
                                  (emit (format nil "~(~{~A~^ ~}~)" (rest item)))
                                  (generate (first item)))
                                 (otherwise
                                  (error "Invalid syntax for a superclass: ~S"
                                         superclass)))
                               (generate superclass)))
                         (second expression)))
        (emit :fresh-line)
        (with-parens "{}"
          (dolist (member (rest (rest expression)))
            (if (member member '(:public :protected :private))
              (emit :fresh-line (format nil "~(~A~):" member) :newline)
              (generate member)))))
       (progn
         (emit ))))

    ((com.informatimago.linc.c::enum)
     ;; (enum (blue 1) white red (yellow 10))
     (emit "enum")
     (when name
       (emit " ")
       (generate name))
     (with-parens "{}"
       (dolist (item (rest expression))
         (if (listp item)
           (case (length item)
             ((1)
              (generate (first item)) )
             ((2)
              (generate (first item)) (emit "=") (generate (second item)))
             (otherwise
              (error "Invalid syntax for an enum constant: ~S" item)))
           (generate item))
         (emit "," :newline))))

    
    ))


(defun generate-declaration (?declaration)
  (pcond ?declaration

    ((ASM ?string)
     (emit "asm")
     (with-parens "()" (generate-expression ?string))
     (emit ";" :newline))

    ((NAMESPACE ?identifier &body ?namespace-body)
     (emit "namespace")
     (when ?identifier
       (emit " ")
       (generate-expression ?identifier))
     (with-parens "{}"
       (dolist (?declaration ?namespace-body)
         (generate-declaration ?declaration)))
     (emit :newline))

    ((NAMESPACE-ALIAS ?identifier ?qualified-namespace-specifier)
     (emit "namespace")
     (generate-expression ?identifier)
     (emit "=")
     (generate-expression ?qualified-namespace-specifier)
     (emit ";" :newline))
     
    ((USING (TYPENAME ?name))
     (emit "using" " " "typename" " ")
     (generate-expression ?name)
     (emit ";" :newline))
     
    ((USING (NAMESPACE ?name))
     (emit "using" " " "namespace" " ")
     (generate-expression ?name)
     (emit ";" :newline))

    ((USING (SCOPE ?name))
     (emit "using" " ")
     (generate-expression `(com.informatimago.linc.c::scope ,?name))
     (emit ";" :newline))
     
    ((TEMPLATE (&rest ?template-parameter-list) ?declaration)
     (emit "template" "<")
     (generate-list ","
                    (function generate-expression)
                    ?template-parameter-list)
     (emit ">")
     (when ?template-parameter-list (emit :newline))
     (generate-declaration ?declaration))

    ((TEMPLATE EXPORT (&rest ?template-parameter-list) ?declaration)
     (emit "export" " " "template" "<")
     (generate-list ","
                    (function generate-expression)
                    ?template-parameter-list)
     (emit ">" :newline)
     (generate-declaration ?declaration))

    ((TEMPLATE ?declaration)
     (emit "template" " ")
     (generate-declaration ?declaration))

    ((EXTERN ?string ?declaration)
     (emit "extern" " ")
     (generate-expression ?string)
     (generate-declaration ?declaration))

    ((EXTERN ?string (&rest ?declarations))
     (emit "extern" " ")
     (generate-expression ?string)
     (with-parens "{}"
       (dolist (?declaration ?declartions)
         (generate-declaration ?declaration)))
     (emit :newline))
    
    ((&whole ?everything &rest ?anything)
     (error "Not a declaration: ~S" ?everything))))



(defmethod generate (expression)
  (if (atom expression)
    (generate-expression expression)
    (let ((key (first expression)))
      (ecase key

        ((asm namespace namespace-alias using template extern)
         (generate-declaration expression))

        ((\#cond \#if \#ifdef \#ifndef \#include
                 \#define \#undef \#line \#error \#pragma \#)
         (generate-preprocessor expression))

        ((com.informatimago.linc.c::block
             com.informatimago.linc.c::let
           com.informatimago.linc.c::if
           com.informatimago.linc.c::case
           com.informatimago.linc.c::while
           com.informatimago.linc.c::do
           com.informatimago.linc.c::for
           com.informatimago.linc.c::break
           com.informatimago.linc.c::continue
           com.informatimago.linc.c::return
           com.informatimago.linc.c::goto)
         (generate-statement expression))

        ((com.informatimago.linc.c::progn
           com.informatimago.linc.c::callargs
           com.informatimago.linc.c::?
           com.informatimago.linc.c::=
           com.informatimago.linc.c::*=
           com.informatimago.linc.c::/=
           com.informatimago.linc.c::%=
           com.informatimago.linc.c::+=
           com.informatimago.linc.c::-=
           com.informatimago.linc.c::>>=
           com.informatimago.linc.c::<<=
           com.informatimago.linc.c::&=
           com.informatimago.linc.c::^=
           com.informatimago.linc.c::\|=
           com.informatimago.linc.c::\|\|
           com.informatimago.linc.c::&&
           com.informatimago.linc.c::\|
           com.informatimago.linc.c::^
           com.informatimago.linc.c::&
           com.informatimago.linc.c::==
           com.informatimago.linc.c::!=
           com.informatimago.linc.c::<
           com.informatimago.linc.c::>
           com.informatimago.linc.c::<=
           com.informatimago.linc.c::>=
           com.informatimago.linc.c::<<
           com.informatimago.linc.c::>>
           com.informatimago.linc.c::+
           com.informatimago.linc.c::-
           com.informatimago.linc.c::*
           com.informatimago.linc.c::/
           com.informatimago.linc.c::%
           com.informatimago.linc.c::.*
           com.informatimago.linc.c::->*
           com.informatimago.linc.c::cast
           com.informatimago.linc.c::++
           com.informatimago.linc.c::--
           com.informatimago.linc.c::!
           com.informatimago.linc.c::~
           com.informatimago.linc.c::deref
           com.informatimago.linc.c::pointer
           com.informatimago.linc.c::address
           com.informatimago.linc.c::pos
           com.informatimago.linc.c::neg
           com.informatimago.linc.c::sizeof
           com.informatimago.linc.c::new
           com.informatimago.linc.c::delete
           com.informatimago.linc.c::++post
           com.informatimago.linc.c::--post
           com.informatimago.linc.c::\.
           com.informatimago.linc.c::->
           com.informatimago.linc.c::aref
           com.informatimago.linc.c::call
           com.informatimago.linc.c::scope
           com.informatimago.linc.c::literal
           com.informatimago.linc.c::identifier)
         (generate-expression expression))))))

;; (class (scope Configuration Exception InvalidFieldException))

;;                          (scope c d)              com.informatimago.linc.c::d
;;                 (scope b (scope c d))          b::c::d
;;        (scope a (scope b (scope c d)))      a::b::c::d
;; (scope (scope a (scope b (scope c d))))   ::a::b::c::d
;; 
;; (scope a b c d)           a::b::c::d
;; (scope (scope a b c d)) ::a::b::c::d
;; (scope printf)          ::printf



(defun generate-preprocessor (expression)
  (flet ((gen-progn (expression)
           (if (and (listp expression) (eq '\#progn (first expression)))
             (dolist (expr (rest expression))
               (generate expr))
             (generate expression))))
    (if (atom expression)
      (error "Not a pre-processor expression: ~S" expression)
      (case (first expression)

        ((\#cond)
         (let ((op "#if"))
           (dolist (clause clauses)
             (if (find (first clause) '(t (quote t)) :test (function equal))
               (emit :fresh-line "#else" :newline)
               (progn (emit :fresh-line op " ")
                      (generate-expression (first clauses))
                      (emit :newline)
                      (setf op "#elif")))
             (dolist (item (rest clauses))
               (generate item)))))

        ((\#if \#ifdef \#ifndef)
         (destructuring-bind (\#test ?condition ?then &optional ?else) expression
           (emit (format nil "~(~A~)" \#test) " ")
           (generate-expression ?condition)
           (emit :fresh-line)
           (gen-progn ?then)
           (when ?else
             (emit :fresh-line "#else" :newline)
             (gen-progn ?else))
           (emit :fresh-line "#endif" :newline)))
      
        ((\#define)
         (destructuring-bind (?operator ?name &rest ?arguments)
             (if (listp ?name)
               (in-continuation-lines
                (emit "#define" " " (first ?name))
                (with-parens "()"
                  (generate-list "," (function generate-expression) (rest ?name)))
                (emit :newline)
                (dolist (item ?arguments) (generate item))
                (emit :fresh-line))
               (in-continuation-lines
                (emit "#define" " "  ?name)
                (dolist (item ?arguments) (generate item))
                (emit :fresh-line)))))

        ((\#include \#undef \#line \#error \#pragma \#)
         (destructuring-bind (?operator &rest ?arguments) expression
           (emit :fresh-line (format nil "~(~A~)" ?operator))
           (dolist (arg ?arguments)
             (emit  " ") (generate-expression arg))
           (emit :newline)))
      
        (otherwise
         (error "Not a pre-processor expression: ~S" expression))))))
  

  ;; (#cond
  ;;   (expr
  ;;    dasd
  ;;    dasdas
  ;;    dasda)
  ;;   (expr
  ;;    dasas
  ;;    dasdas
  ;;    dasda))
  ;;
  ;; (#if expr
  ;;   (#progn dasd
  ;;           dasd)
  ;;   (#progn dasd
  ;;           dasd))
  ;; 
  ;; (#ifdef  expr
  ;;          (#progn dasd
  ;;                  dasd)
  ;;          (#progn dasd
  ;;                  dasd))
  ;; (#ifndef expr
  ;;          (#progn dasd
  ;;                  dasd)
  ;;          (#progn dasd
  ;;                  dasd))
  ;; 
  ;; (#include dada...)
  ;; (#define ident ...)
  ;; (#define (ident ...) ...)
  ;; (#undef ident)
  ;; (#line ...)
  ;; (#error ...)
  ;; (#pragma ...)
  ;; (#)


  
#- (and)
  (declaration 
 ::=
 ;; simple-definition
 (  decl-specifier-seq[opt] init-declarator-list[opt] ";"  )
 ;; (  function-definition  )
 (  decl-specifier-seq[opt] declarator ctor-initializer[opt] function-body  )
 (  decl-specifier-seq[opt] declarator function-try-block  )
 (  "asm" "(" string-literal ")" ";"  )
 ;; namespace-alias-definition
 (  "namespace" identifier "=" qualified-namespace-specifier ";"  ) 
 ;; using-declaration
 (  "using" "typename"[opt] "::"[opt] nested-name-specifier unqualified-id ";"  )
 (  "using" "::"  unqualified-id ";"  )
 ;; using-directive
 (  "using"  "namespace"  "::"[opt] nested-name-specifier[opt] namespace-name ";"  )


 ;; (  template-declaration  )
 (  "export"[opt] "template" "<" template-parameter-list ">" declaration  )
 
 ;; (  explicit-instantiation  )
 (  "template" declaration  )

 ;; (  explicit-specialization  )
 (  "template" "<" ">" declaration  )

 ;; (  linkage-specification  )
 (  "extern" string-literal "{" declaration-seq[opt] "}"  )
 (  "extern" string-literal declaration  )

 ;; (  namespace-definition  )
 (  "namespace" identifier[opt] "{" namespace-body "}"  )
 )
;;;; THE END ;;;;
