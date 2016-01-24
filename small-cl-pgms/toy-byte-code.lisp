;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               toy-byte-code.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a toy language byte code interpreter, lap assembler and compiler.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-09-11 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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

(defpackage :com.informatimago.toy-language
  (:use :common-lisp))

(in-package :com.informatimago.toy-language)


(defstruct tl-env
  (variables (make-hash-table)))

(defun tl-var (env identifier)
  (gethash identifier (tl-env-variables env) 0))

(defun (setf tl-var) (new-value env identifier)
  (setf (gethash identifier (tl-env-variables env)) new-value))


(defun tl-eval (env &rest stmt*)
  (dolist (stmt stmt*)
    (ecase (first stmt)
      ((block) (apply (function tl-eval) env (rest stmt)))
      ((if)    (if (tl-expr-assign env (second stmt))
                   (tl-eval env (third stmt))))
      ((while) (loop :while (tl-expr-assign env (second stmt))
                 :do (tl-eval env (third stmt))))
      ((assign) (tl-expr-assign env stmt)))))

(defun tl-expr-assign (env expr-assign)
  (if (atom expr-assign)
      (tl-expr env expr-assign)
      (case (first expr-assign)
        ((assign) (setf (tl-var env (second expr-assign))
                        (tl-expr-assign env (third expr-assign))))
        (otherwise (tl-expr env expr-assign)))))

(defparameter *tl-ops*
  (list (list '&& (lambda (&rest args) (every (function identity) args)))
        (list '|| (lambda (&rest args) (some  (function identity) args)))
        (list '<  (function <))
        (list '>  (function >))
        (list '== (function =))
        (list '<> (function /=))
        (list '+  (function +))
        (list '-  (function -))
        (list '*  (function *))
        (list '/  (function /))))

(defun tl-expr (env expr)
  (cond
    ((symbolp expr) (tl-var env expr))
    ((numberp expr) expr)
    ((atom expr) (error "Invalid atom ~S" expr))
    (t (let ((entry (assoc (first expr) *tl-ops*)))
         (if entry
             (apply (second entry) (mapcar (lambda (expr) (tl-expr env expr)) (rest expr)))
             (error "Invalid operation ~S" (first expr)))))))

    ;;;---


(assert (equalp
         (tl-expr-assign (make-tl-env) '(assign i (* 42 42)))
         1764))

(assert (equalp
         (let ((env  (make-tl-env)))
           (tl-eval env
                    '(block
                      (assign i 42)
                      (assign j 33)
                      (while (<> i j)
                        (block
                            (if (< i j)
                                (assign j (- j i)))
                          (if (< j i)
                              (assign i (- i j)))))))
           (tl-var env 'i))
         3))


(defun hash-table-to-alist (ht)
  (let ((result '()))
    (maphash (lambda (key value) 
               (setq result (acons key value result)))
             ht)
    result))

(assert (equalp
         (let ((env (make-tl-env)))
           (tl-eval env
                    '(block
                      (assign i (* 42 42))
                      (if (< i 0) (assign j -1))
                      (if (> i 0) (assign j +1))
                      (assign k (- i 12))
                      (while (< 0 i)
                        (assign i (- i 12)))))
           (hash-table-to-alist (tl-env-variables env)))
         '((k . 1752) (i . 0) (j . 1))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *instructions* #(and or lt gt eq ne add sub mul div load&push pop&store bfof bba stop))
  (defun codop (instruction) (position instruction *instructions*)))

(defparameter *data-memory-size*    64)
(defparameter *program-memory-size* 128)

(deftype octet () '(unsigned-byte 8))

(defstruct machine
  (memory  (make-array *data-memory-size*    :element-type 'number :initial-element 0))
  (stack  '())
  (program (make-array *program-memory-size* :element-type 'octet :initial-element (codop 'stop)))
  (pc      0)
  (stopped t))

(defun load-program (machine data pgm)
  (replace (machine-memory  machine) data)
  (replace (machine-program machine) pgm)
  (setf (machine-stack   machine) '())
  (setf (machine-pc      machine) 0)
  (setf (machine-stopped machine) nil)
  machine)


(defun machine-step (machine)
  (unless (machine-stopped machine)
    (handler-case
        (symbol-macrolet ((stack   (machine-stack  machine))
                          (data    (machine-memory machine))
                          (program (machine-program machine))
                          (pc      (machine-pc machine)))
          (flet ((get-iword ()
                   (let ((hi (aref program pc))
                         (lo (aref program (incf pc))))
                     (incf pc)
                     (dpb hi (byte 8 8) lo))))
            (let ((code (aref program pc)))
              (incf pc)
              (ecase code
                ((#.(codop 'and)) (push (and (pop stack) (pop stack)) stack))
                ((#.(codop 'or))  (push (or  (pop stack) (pop stack)) stack))
                ((#.(codop 'lt))  (push (<   (pop stack) (pop stack)) stack))
                ((#.(codop 'gt))  (push (>   (pop stack) (pop stack)) stack))
                ((#.(codop 'eq))  (push (=   (pop stack) (pop stack)) stack))
                ((#.(codop 'ne))  (push (/=  (pop stack) (pop stack)) stack))
                ((#.(codop 'add)) (push (+   (pop stack) (pop stack)) stack))
                ((#.(codop 'sub)) (push (-   (pop stack) (pop stack)) stack))
                ((#.(codop 'mul)) (push (*   (pop stack) (pop stack)) stack))
                ((#.(codop 'div)) (push (/   (pop stack) (pop stack)) stack))
                ((#.(codop 'load&push))
                 (let ((address (get-iword)))
                   (push (aref data address) stack)))
                ((#.(codop 'pop&store))
                 (let ((address (get-iword)))
                   (setf (aref data address) (pop stack))))
                ((#.(codop 'bfof))
                 (let ((relative (get-iword)))
                   (if (not (pop stack))
                       (incf pc relative))))
                ((#.(codop 'bba))
                 (let ((relative (get-iword)))
                   (decf pc relative)))
                ((#.(codop 'stop))
                 (setf (machine-stopped machine) t))))))
      (error (err)
        (format *error-output* "~%~A~%" err)
        (setf (machine-stopped machine) t)))))


(defun machine-run (machine)
  (loop
    :until (machine-stopped machine)
    :do (machine-step machine)))


;; Let's write a little assember:

(defun lap* (body)
  "
    body is a list of instructions or labels.
    instructions are: (and) (or) (lt) (gt) (eq) (ne) (add) (sub) (mul) (div) (stop)
                      (load&push address) (pop&store address)
                      (loadi&push value) 
                      (bfof label) (bba label)
    address is a symbol.
    value is a literal value.
    labels are symbols present in the body.
    loadi&push is translated into a load&push with the address where the value is stored.

    RESULT: a byte-code program vector;
            a memory vector;
            a program symbol table;
            a data symbol table.
    "
  (let ((data
         ;; build the data symbol table.
         ;; It's a vector with each variable or literal.
         (coerce
          (delete-duplicates
           (mapcar (function second)
                   (remove-if-not (lambda (instruction)
                                    (and (listp instruction)
                                         (member (first instruction)
                                                 '(load&push pop&store loadi&push))))
                                  body)))
          'vector))
        (program
         ;; build the program symbol table.
         ;; It's an a-list mapping the label to the iaddress.
         (loop
           :with pc = 0
           :with table = '()
           :for instruction :in body
           :do (if (atom instruction)
                   (push (cons instruction pc) table)
                   (case (first instruction)
                     ((load&push pop&store loadi&push bfof bba) (incf pc 3))
                     (otherwise (incf pc))))
           :finally (return table))))
    (values
     ;; generate the program byte code:
     (loop
       :with code = (make-array (length body) :adjustable t :fill-pointer 0
                                :element-type '(unsigned-byte 8))
       :for instruction :in body
       :do (unless (atom instruction)
             (case (first instruction)
               ((loadi&push)
                (let ((address (position (second instruction) data)))
                  (vector-push-extend (codop 'load&push) code)
                  (vector-push-extend (ldb (byte 8 8) address) code)
                  (vector-push-extend (ldb (byte 8 0) address) code)))
               ((load&push pop&store)
                (let ((address (position (second instruction) data)))
                  (vector-push-extend (codop (first instruction)) code)
                  (vector-push-extend (ldb (byte 8 8) address) code)
                  (vector-push-extend (ldb (byte 8 0) address) code)))
               ((bfof)
                (let ((relative (- (cdr (assoc (second instruction) program))
                                   (+ (length code) 3))))
                  (when (minusp relative)
                    (error "~D: (~S ~S) backward~%~S"
                           (length code) (first instruction) (second instruction)
                           program))
                  (vector-push-extend (codop (first instruction)) code)
                  (vector-push-extend (ldb (byte 8 8) relative) code)
                  (vector-push-extend (ldb (byte 8 0) relative) code)))
               ((bba)
                (let ((relative (- (+ (length code) 3)
                                   (cdr (assoc (second instruction) program)))))
                  (when (minusp relative)
                    (error "~D: (~S ~S) forward~%~S"
                           (length code) (first instruction) (second instruction)
                           program))
                  (vector-push-extend (codop (first instruction)) code)
                  (vector-push-extend (ldb (byte 8 8) relative) code)
                  (vector-push-extend (ldb (byte 8 0) relative) code)))
               (otherwise
                (vector-push-extend (codop (first instruction)) code))))
       :finally (return code))
     ;; generate the data vector:
     (map 'vector (lambda (item) (if (symbolp item) 0 item)) data)
     ;; program symbol table:
     program
     ;; data symbol table:
     data)))

(defmacro lap (&body body)
  `(lap* ',body))


;; So we can write little assembler programs for our machine:
;; 
(assert (equalp
         (multiple-value-list (lap
                               (loadi&push 42)
                               (pop&store i)
                               (loadi&push 33)
                               (pop&store j)
                               :while
                               (load&push j)
                               (load&push i)
                               (eq)
                               (bfof :end-while)
                               :if-1
                               (load&push j)
                               (load&push i)
                               (lt)
                               (bfof :end-if-1)
                               (load&push i)
                               (load&push j)
                               (sub)
                               (pop&store j)
                               :end-if-1
                               :if-2
                               (load&push i)
                               (load&push j)
                               (lt)
                               (bfof :end-if-2)
                               (load&push j)
                               (load&push i)
                               (sub)
                               (pop&store i)
                               :end-if-2
                               (bba :while)
                               :end-while
                               (stop)))
         '(#(10 0 0 11 0 3 10 0 1 11 0 2 10 0 2 10 0 3 4 12 0 43 10 0 2 10 0 3 2 12 0 10 10 0 3 10 0 2 7 11 0 2 10 0 3 10 0 2 2 12 0 10 10 0 2 10 0 3 7 11 0 3 13 0 53 14)
           #(42 33 0 0)
           ((:end-while . 65) (:end-if-2 . 62) (:if-2 . 42) (:end-if-1 . 42) (:if-1 . 22) (:while . 12))
           #(42 33 j i))))


;; And we can run programs:
;; 
;;     (setf *print-length* 20)
;;

(assert (equalp
         (let ((machine (make-machine)))
           (multiple-value-bind (program data ptable dtable)
               (lap
                (loadi&push 42)
                (pop&store i)
                (loadi&push 33)
                (pop&store j)
                :while
                (load&push j)
                (load&push i)
                (ne)
                (bfof :end-while)
                :if-1
                (load&push j)
                (load&push i)
                (lt)
                (bfof :end-if-1)
                (load&push i)
                (load&push j)
                (sub)
                (pop&store j)
                :end-if-1
                :if-2
                (load&push j)
                (load&push i)
                (gt)
                (bfof :end-if-2)
                (load&push j)
                (load&push i)
                (sub)
                (pop&store i)
                :end-if-2
                (bba :while)
                :end-while
                (stop))
             (load-program machine data program)
             (machine-run machine)
             machine))
         #S(machine :memory #(42 33 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    :stack nil
                    :program #(10 0 0 11 0 3 10 0 1 11 0 2 10 0 2 10 0 3 5 12 0 43 10 0 2 10 0 3 2 12 0 10 10 0 3 10 0 2 7 11 0 2 10 0 2 10 0 3 3 12 0 10 10 0 2 10 0 3 7 11 0 3 13 0 53 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
                    :pc 66 :stopped t)))



;; There remains now to write a compiler.

(defun tl-compile (&rest stmt*)
      "
    Compile the program (sequence of stmt) STMT*.
    return:  the same as LAP*.
    "
      (lap* (tl-generate-stmts stmt*)))


(defun tl-generate-stmts (stmts)
  (mapcan (lambda (stmt)
            (ecase (first stmt)
              ((block) (tl-generate-stmts (rest stmt)))
              ((if)    (let ((end-if (gensym "END-IF")))
                         (append (tl-generate-expr-assign (second stmt))
                                 `((bfof ,end-if))
                                 (tl-generate-stmts (list (third stmt)))
                                 `(,end-if))))
              ((while) (let ((begin-while (gensym "BEGIN-WHILE"))
                             (end-while   (gensym "END-WHILE")))
                         (append `(,begin-while)
                                 (tl-generate-expr-assign (second stmt))
                                 `((bfof ,end-while))
                                 (tl-generate-stmts (list (third stmt)))
                                 `((bba ,begin-while)
                                   ,end-while))))
              ((assign) (tl-generate-expr-assign stmt))))
          stmts))

(defun tl-generate-expr-assign (expr-assign)
  (if (atom expr-assign)
      (tl-generate-expr expr-assign)
      (case (first expr-assign)
        ((assign) (append (tl-generate-expr-assign (third expr-assign))
                          `((pop&store ,(second expr-assign)))))
        (otherwise (tl-generate-expr expr-assign)))))

(defparameter *tl-op-instructions*
  '((&& . and) (|| . or) (< . lt) (> . gt) (== . eq) (<> . ne)
    (+ . add) (- . sub) (* . mul) (/ . div)))

(defun tl-generate-expr (expr)
  (cond
    ((symbolp expr) `((load&push ,expr)))
    ((numberp expr) `((loadi&push ,expr)))
    ((atom expr) (error "Invalid atom ~S" expr))
    (t (let ((entry (assoc (first expr) *tl-op-instructions*)))
         (if entry
             (if (and (member (first expr) '(- /))
                      (< 2 (length (rest expr))))
                 ;; transforms: (- a b c d) into (- a (+ b c d))
                 (tl-generate-expr `(,(first expr) ,(second expr)
                                      (,(ecase (first expr)
                                               ((-) +)
                                               ((/) *))
                                        ,@(cddr expr))))
                 (append (mapcan (function tl-generate-expr) (reverse (rest expr)))
                         (make-list (1- (length (rest expr)))
                                    :initial-element (list (cdr entry)))))
             (error "Invalid operation ~S" (first expr)))))))

#-(and)
(assert (gensym-unifies-p
         (tl-generate-stmts
          '((block
                (assign i 42)
              (assign j 33)
              (while (<> i j)
                (block
                    (if (< i j)
                        (assign j (- j i)))
                  (if (< j i)
                      (assign i (- i j))))))))
         '((loadi&push 42)
           (pop&store i)
           (loadi&push 33)
           (pop&store j)
           #3=#:begin-while7234
           (load&push j)
           (load&push i)
           (ne)
           (bfof #4=#:end-while7235)
           (load&push j)
           (load&push i)
           (lt)
           (bfof #1=#:end-if7236)
           (load&push i)
           (load&push j)
           (sub)
           (pop&store j)
           #1#
           (load&push i)
           (load&push j)
           (lt)
           (bfof #2=#:end-if7237)
           (load&push j)
           (load&push i)
           (sub)
           (pop&store i)
           #2#
           (bba #3#)
           #4#)))

#-(and)
(assert (gensym-unifies-p
         (multiple-value-list
          (tl-compile '(block
                        (assign i 42)
                        (assign j 33)
                        (while (<> i j)
                          (block
                              (if (< i j)
                                  (assign j (- j i)))
                            (if (< j i)
                                (assign i (- i j))))))))
         '(#(10 0 0 11 0 3 10 0 1 11 0 2 10 0 2 10 0 3 5 12 0 43 10 0 2 10 0 3 2 12 0 10 10 0 3 10 0 2 7 11 0 2 10 0 3 10 0 2 2 12 0 10 10 0 2 10 0 3 7 11 0 3 13 0 53)
           #(42 33 0 0)
           ((#:end-while7243 . 65) (#:end-if7245 . 62) (#:end-if7244 . 42) (#:begin-while7242 . 12))
           #(42 33 j i))))


(assert (equalp
         (let ((machine (make-machine)))
           (multiple-value-bind (program data ptable dtable)
               (tl-compile '(block
                             (assign i 42)
                             (assign j 33)
                             (while (<> i j)
                               (block
                                   (if (< i j)
                                       (assign j (- j i)))
                                 (if (< j i)
                                     (assign i (- i j)))))))
             (load-program machine data program)
             (machine-run machine)
             machine))
         #S(machine :memory #(42 33 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    :stack nil
                    :program #(10 0 0 11 0 3 10 0 1 11 0 2 10 0 2 10 0 3 5 12 0 43 10 0 2 10 0 3 2 12 0 10 10 0 3 10 0 2 7 11 0 2 10 0 3 10 0 2 2 12 0 10 10 0 2 10 0 3 7 11 0 3 13 0 53 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
                    :pc 66 :stopped t)))


#-(and)
(block
    (assign i 42)
  (assign j 33)
  (while (<> i j)
    (block
        (if (< i j)
            (assign j (- j i)))
      (if (< j i)
          (assign i (- i j))))))

;;;; THE END ;;;;
