;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               quine.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Quines are programs that output themselves.
;;;;    Three implementations in Common-Lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-12-29 <PJB> Created.
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************
(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.QUINES"
  (:use "COMMON-LISP")
  (:export #+clisp "QUINE-1"
           "QUINE-2" "QUINE-2S" "QUINE-2E"
           "QUINE-3"
           #+clisp "TRY-QUINE-1"
           "TRY-QUINE-2"
           "TRY-QUINE-3"
           "TRY-LAMBDA-QUINE"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.QUINES")

;; -------------------------------------------------------------------
;; QUINE-1 cheats  a little:  it works only  on clisp and on a
;; non-compiled function, retrieving the lambda-expression stored in
;; the  symbol-function slot of the symbol naming the function itself
;; (similar  to retriving the source of the program from the hard disk).

#+clisp
(defun quine-1 nil
  (let ((lexp (function-lambda-expression (symbol-function 'quine-1))))
    (format t "~S~%"
            `(defun ,(second (fourth lexp)) ,(second lexp)
               ,@(cddr (fourth lexp))))))

;; -------------------------------------------------------------------
;; QUINE-2 is  nicer, but works by  generating a string  and using the
;; FORMAT  interpreter  (with  the  ~S  trick  to  generate  a  quoted
;; string...).

(defun quine-2 nil
  (let ((src "(DEFUN QUINE-2 NIL (LET ((SRC ~S)) (FORMAT T SRC SRC)))"))
     (format t src src)))


;; QUINE-2S is like QUINE-2 but instead of producing its source as a string,
;; it returns it as a s-expression.

(defun quine-2s nil
  (let ((src "(DEFUN QUINE-2S NIL
                (LET ((SRC ~S))
                  (READ-FROM-STRING (FORMAT NIL SRC SRC))))"))
    (read-from-string (format nil src src))))


;; QUINE-2E is like QUINE-2S but instead of producing its source as its result
;; it redefines itself.

(defun quine-2e nil
  (let ((src "(DEFUN QUINE-2E NIL
                (LET ((SRC ~S))
                  (EVAL (READ-FROM-STRING (FORMAT NIL SRC SRC)))))"))
    (eval (read-from-string (format nil src src)))))


;; -------------------------------------------------------------------
;; QUINE-3 generates and returns a new tree equal to the sexp defining
;; QUINE-3 itself.

(defun quine-3 nil
  (labels
    ((find-car
      (token tree)
      (cond
       ((atom tree) nil)
       ((eq token (car tree)) tree)
       (t (or (find-car token (car tree))
              (find-car token (cdr tree)))))))
    (let* ((source '(defun quine-3 nil
                      (labels
                        ((find-car
                          (token tree)
                          (cond
                           ((atom tree) nil)
                           ((eq token (car tree)) tree)
                           (t (or (find-car token (car tree))
                                  (find-car token (cdr tree)))))))
                        (let* ((source ':quine)
                               (quine-3 (copy-tree source)))
                          (setf (car (find-car :quine quine-3)) source)
                          quine-3))))
           (quine-3 (copy-tree source)))
      (setf (car (find-car :quine quine-3)) source)
      quine-3)))


;; -------------------------------------------------------------------
;; QUINE-1 and QUINE-2, since they're outputing a string of character,
;; must be used as follow to effectively loop the quine:

#+clisp (defun try-quine-1 ()
          (read-from-string (with-output-to-string (*standard-output*) (quine-1))))

(defun try-quine-2 ()
 (read-from-string (with-output-to-string (*standard-output*) (quine-2))))

;; while the result of QUINE-2S and QUINE-3 can be evalued directly:
;;  (eval (quine-3))

(defun try-quine-3 ()
  (quine-3))

;; With packages, we have to either go into the package:
#-(and) (in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.QUINES")
;; or bind the *package* to it when printing the quines:
#-(and)
(with-standard-io-syntax
  (let ((*package* (find-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.QUINES")))
    (pprint (COM.INFORMATIMAGO.SMALL-CL-PGMS.QUINES:try-quine-3))))

;; -------------------------------------------------------------------
;; LAMBDA QUINE:

(defun try-lambda-quine ()
  ((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))


;; cmucl: ((LAMBDA (X) `(,X ',X)) '(LAMBDA (X) `(,X ',X)))
;; clisp: ((LAMBDA (X) `(,X ',X)) '(LAMBDA (X) `(,X ',X)))
;; emacs: (#1=(lambda (x) (\` ((\, x) (quote (\, x))))) (quote #1#))
;; sbcl:  ((LAMBDA (X) (SB-IMPL::BACKQ-LIST X (SB-IMPL::BACKQ-LIST (QUOTE QUOTE) X))) (QUOTE (LAMBDA (X) (SB-IMPL::BACKQ-LIST X (SB-IMPL::BACKQ-LIST (QUOTE QUOTE) X)))))

;;;; THE END ;;;;

