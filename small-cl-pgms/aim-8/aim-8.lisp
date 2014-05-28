;;;;****************************************************************************
;;;;FILE:               aim-8.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements the LISP described in AIM-8 in Common-Lisp.
;;;;    Usage:  (load "aim-8.lisp") 
;;;;            (aim-8:repl)
;;;;    Then at the aim-8 prompt, you have LISP, plus:
;;;;       (DEFINE name sexp)     corresponding to =
;;;;       (RELOAD)               to reload aim-8 if you edit it.
;;;;       (DUMP-ENVIRONMENT)     to dump the defined symbols.
;;;;       (LOAD "path")          to load an aim-8 source. Try "aim-8.aim-8".
;;;;
;;;;     AIM-8 -- 4 MARCH 1959 -- J. MCCARTHY
;;;;     With an addendum dated 23 MARCH 1959
;;;;     ftp://publications.ai.mit.edu/ai-publications/pdf/AIM-008.pdf
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-10-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2012
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

(defpackage "AIM-8"
  (:use "COMMON-LISP")
  (:export "REPL")
  (:documentation
   "Implements the lisp of AIM-8 -- 4 MARCH 1959 -- J. MCCARTHY
With an addendum dated 23 MARCH 1959
ftp://publications.ai.mit.edu/ai-publications/pdf/AIM-008.pdf"))
(in-package "AIM-8")


(defparameter *environment* (make-hash-table :test (function eq)))
(defmacro def     (name)       `(gethash ,name *environment*))
(defun   %boundp  (name) (multiple-value-bind (val bnd) (def name)
                          (declare (ignore val)) bnd))
(defmacro define  (name value) `(setf (gethash ',name *environment*) ',value))
(defun   fdefine  (name value)  (setf (gethash name *environment*) value))

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
    ((atom e) (cond ((%boundp e) (def e))
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
         ((load)    (load  (%eval (first (rest e)))))
         ((print)   (print (%eval (first (rest e)))))
         ((read)    (read))
         (otherwise
          (cond
            ((atom (first e))
             (cond ((%boundp (first e)) (%apply (def (first e)) (rest e)))
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
  (let ((*package* (find-package "AIM-8")))
    (help)
    (loop
       (terpri)
       (princ "AIM-8> ")
       (handling-errors
        (let ((sexp (read)))
          (cond
            ((equal sexp '(quit))
             (format t "GOOD BYE") (return-from repl))
            ((equal sexp '(reload))
             (load "aim-8") (repl) (return-from repl))
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


(defpackage "AIM-8-USER"
  (:use)
  (:import-from "AIM-8"
                "DEFINE" "LAMBDA" "LABEL"
                "COND"  "COMBINE" "FIRST" "REST"
                "NULL" "ATOM" "EQ" "NIL" "T" "QUOTE"))

;;;; aim-8.lisp                       --                     --          ;;;;
