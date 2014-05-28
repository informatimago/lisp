;;;;****************************************************************************
;;;;FILE:               minlisp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a minimum LISP inspired from AIM-8 LISP, in Common-Lisp.
;;;;    Usage:  (load "minlisp.lisp") 
;;;;            (minlisp:repl)
;;;;    Then at the minlisp prompt, you have LISP, plus:
;;;;       (DEFINE name sexp)     corresponding to =
;;;;       (RELOAD)               to reload minlisp if you edit it.
;;;;       (DUMP-ENVIRONMENT)     to dump the defined symbols.
;;;;       (LOAD "path")          to load an minlisp source. Try "minlisp.minlisp".
;;;;
;;;;     AIM-8 -- 4 MARCH 1959 -- J. MCCARTHY
;;;;     With an addendum dated 23 MARCH 1959
;;;;     ftp://publications.ai.mit.edu/ai-publications/pdf/AIM-008.pdf
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-08-11 <PJB> Created MINLISP out of AIM-8 LISP.
;;;;    2004-10-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2013
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

(defpackage "MINLISP"
  (:use "COMMON-LISP")
  (:export "REPL")
  (:documentation "Implements a minimum lisp called MINLISP. (AGPL3)"))
(in-package "MINLISP")


(defparameter *environment* (make-hash-table :test (function eq)))
(defmacro definition (name)       `(gethash ,name *environment*))
(defun    %boundp    (name) (multiple-value-bind (val bnd) (definition name)
                              (declare (ignore val)) bnd))
(defmacro define  (name value) `(setf (gethash ',name *environment*) ',value))
(defun   fdefine  (name value)  (setf (gethash name *environment*) value))


(defun %subst (x y a)
  (cond ((null a) nil)
        ((atom a) (cond ((eq y a) x) (t a)))
        (t (cons (%subst x y (first a)) (%subst x y (rest a))))))

(defun %subsq (x y z)
  (cond ((null z) nil)
        ((atom z) (cond ((eq y z) x)  (t z)))
        ((eq (first z) 'quote) z)
        (t (cons (%subsq x y (first z)) (%subsq x y (rest z))))))

(defun %evcon (c)
  (cond ((%eval (first (first c))) (%eval (first (rest (first c)))))
        (t (%evcon (rest c)))))

(defun %evlam (vars exp args)
  (cond ((null vars) (%eval exp))
        (t (%evlam (rest vars) (%subsq (first args) (first vars) exp)
                   (rest args)))))

(defun %apply (f args) (%eval (cons f args)))

(defun %eval (e)
  (cond
    ;; begin extensions:
    ((atom e) (cond ((%boundp e) (definition e))
                    (t (error "Undefined: ~A" (first e)))))
    ;; end extensions.
    (t (case (first e)
         ((null)    (null  (%eval (first (rest e)))))
         ((atom)    (atom  (%eval (first (rest e)))))
         ((quote)                 (first (rest e)))
         ((eq)      (eq    (%eval (first (rest e)))
                           (%eval (first (rest (rest e))))))
         ((combine) (cons  (%eval (first (rest e)))
                           (%eval (first (rest (rest e))))))
         ((first)   (first (%eval (first (rest e)))))
         ((rest)    (rest  (%eval (first (rest e)))))
         ((cond)    (%evcon (rest e)))
         ;; begin extensions:
         (otherwise
          (cond
            ((atom (first e))
             (cond ((%boundp (first e)) (%apply (definition (first e)) (rest e)))
                   (t (error "Undefined: ~A" (first e)))))
            ;; end extensions.
            (t (case (first (first e))
                 ((lambda) (%evlam (first (rest (first e)))
                              (first (rest (rest (first e))))
                              (rest e)))
                 ((label) (%eval (cons (%subst (first e)
                                               (first (rest (first e)))
                                               (first (rest (rest (first e)))))
                                       (rest e))))
                 (otherwise (error "Invalid: ~A" (first e)))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nil ())
(define f   ())
(define t   t)
(define and     (lambda (a b) (cond (a (cond (b t) (t nil))) (t nil))))
(define or      (lambda (a b) (cond (a t) (b t) (t nil))))
(define not     (lambda (a)   (cond (a nil) (t t))))
(define maplist 
        (lambda (x f)
          (cond ((null x) nil)
                (t (combine (f x) (maplist (rest x) f))))))
(define subst 
        (lambda (x y a)
          (cond ((null a) nil)
                ((atom a) (cond ((eq y a) x) (t a)))
                (t (combine (subst x y (first a))
                            (subst x y (rest a))))
                )))

(define cons   (function cons))
(define car    (function car))
(define cdr    (function cdr))
(define null   (function null))

(define load   (function load))
(define print  (function print))
(define prin1  (function prin1))
(define terpri (function terpri))
(define read   (function read))


(defun help ()
  (format t "~&You've got:  
    LAMBDA LABEL
    COND AND OR NOT  COMBINE FIRST REST
    NULL ATOM EQ NIL T QUOTE
Extensions:
    DEFINE RELOAD DUMP-ENVIRONMENT LOAD
    QUIT"))


(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition 
      (err) 
      (format *error-output* "~&~A: ~%" (class-name (class-of err)))
      (apply (function format) *error-output*
             (simple-condition-format-control   err)
             (simple-condition-format-arguments err))
      (format *error-output* "~&"))
     (condition 
      (err) 
      (format *error-output* "~&~A: ~%  ~S~%" (class-name (class-of err)) err))))


(defun repl ()
  (let ((*package* (find-package "MINLISP")))
    (help)
    (loop
       (terpri)
       (princ "MINLISP> ")
       (handling-errors
        (let ((sexp (read)))
          (cond
            ((equal sexp '(quit))
             (format t "GOOD BYE") (return-from repl))
            ((equal sexp '(reload))
             (load "minlisp") (repl) (return-from repl))
            ((equal sexp '(dump-environment))
             (format t "~:{~16@A = ~A~%~}" 
                     (let ((res '()))
                       (maphash (lambda (k v) (push (list k v) res)) 
                                *environment*) res)))
            ((and (listp sexp) (eq (first sexp) 'define))
             (fdefine (second sexp) (third sexp))
             (format t "~A" (second sexp)))
            (t 
             (format t "~S" (%eval sexp))))))))
  (terpri)
  (values))


(defpackage "MINLISP-USER"
  (:use)
  (:import-from "MINLISP"
                "DEFINE" "LAMBDA" "LABEL"
                "COND"  "COMBINE" "FIRST" "REST"
                "NULL" "ATOM" "EQ" "NIL" "T" "QUOTE"
                ;; extensions:
                "AND" "OR" "NOT" "MAPLIST" "SUBST"
                "LOAD" "PRINT" "PRIN1" "TERPRI" "READ"
                "CONS" "CAR" "CDR" "NULL"))

;;;; THE END ;;;;
