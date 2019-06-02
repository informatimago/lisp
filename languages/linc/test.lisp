;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Test forms.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-12-19 <PJB> Extracted from linc.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2007 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(setf *c-out* (open "test.c" :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create))

(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(defpackage "BC-MEM"
  (:documentation "A package corresponding to a C module with prefixed function names.")
  (:use)
  (:export "COPY" "ALLOCATE" "DEALLOCATE"))


(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC.C")

(declare-variable '(n-allocation n-data copy n-length))
(declare-function '(bc-mem:allocate bc-mem:deallocate bc-mem:copy))
(declare-macro    '(minimum))

;; (generate (stmt-block
;;            '(let ((obj1 (pointer abc) (new (abc 1 2 3)))
;;                   (obj2 (pointer abc) (new abc))
;;                   (arr  (pointer (aref (pointer abc) 10)) (new [] (aref (pointer abc) 10))))
;;              (-> obj1 (do-Something 4 5 6))
;;              (delete obj1)
;;              (delete obj2)
;;              (delete [] arr))))


;; (in-package :c)
;; (com.informatimago.languages.linc::generate-expression '(+ 1 2))
;; (com.informatimago.languages.linc::generate-expression
;;  '(* (+ 1 2 3 4) (/ 5 4 3 2) (- 5 4 3 2)
;;    (pos a) (neg b) (deref c) (~ d) (! e)
;;    (pos (+ 1 a)) (neg (+ 2 b)) (deref (+ c 3)) (~ (+ 4 d)) (! (< e 0))))
;; (com.informatimago.languages.linc::generate-expression
;;  '(neg a))

(comment "Here is a little function")

(define-function string_add ((a string_t) (b string_t)) string_t
  (let ((av int)
        (bv int)
        (res string_t (malloc (+ 2 (max (strlen a) (strlen b))))))
    (sscanf a "%d" (address av))
    (sscanf b "%d" (address bv))
    (sprintf res "%d" (+ a b))
    (return res)))

(comment "Here is another function."
              "Slightly bigger this time."
              (* 42 12))

(define-function test () void
 (progn
   (if (> n-Allocation 1)
       (progn
         (= n-Data (Bc-Mem:Allocate (* (sizeof char) n-Allocation)))
         (if copy
             (progn
               (= n-Length (Minimum (- n-Allocation 1) (-> this dlength)))
               (Bc-Mem:Copy (-> this data) n-Data (* n-Length (sizeof char))))
             (= n-Length 0)))
       (progn
         (= n-Allocation 1)
         (= n-Data (Bc-Mem:Allocate (* (sizeof char) n-Allocation)))
         (= n-Length 0)))
   (= (aref n-Data  n-Length) (cast 0 char))
   (Bc-Mem:Deallocate (cast (address (-> this data)) (pointer (pointer void))))
   (= (-> this data)       n-Data)
   (= (-> this dlength)    n-Length)
   (= (-> this allocation) n-Allocation)
   (let ((test double
               (* (+ 1 2 3 4) (/ 5 4 3 2) (- 5 4 3 2)
                  (pos a) (neg b) (deref c) (~ d) (! e)
                  (pos (+ 1 a)) (neg (+ 2 b)) (deref (+ c 3)) (~ (+ 4 d)) (! (< e 0))))))
   (return this)))

#||
(cl:map cl:nil (cl:lambda (x)
                 (com.informatimago.languages.linc::generate-expression x)
                 (cl:terpri))
        '((+ 1 2 3 4)
          (/ 5 4 3 2)
          (- 5 4 3 2)
          (pos a)
          (neg b)
          (pos (neg (pos (neg (pos (neg a))))))
          (+ a (- b (+  c (- d (+ e (- a f))))))
          (deref c)
          (deref (address c))
          (address (deref c))
          (~ d)
          (! e)
          (pos (+ 1 a))
          (neg (+ 2 b))
          (deref (+ c 3))
          (~ (+ 4 d))
          (! (< e 0))
          (& (+ (<< 1 b) (>> c 3)) (\| (<< 1 b) (>> c 3)))
          (? (== a 0) 42 (- a 2))))


(com.informatimago.languages.linc::generate-expression
 '(* (+ 1 2 3 4) (/ 5 4 3 2) (- 5 4 3 2)
   (pos a) (neg b) (deref c) (~ d) (! e)
   (pos (+ 1 a)) (neg (+ 2 b)) (deref (+ c 3)) (~ (+ 4 d)) (! (< e 0))))

;; (in-package :com.informatimago.languages.linc)

(setf *c-out* (open "test.c" :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create))

(cl:progn
  (cl:close       com.informatimago.languages.linc::*c-out*)
  (cl-user::edit (cl:pathname com.informatimago.languages.linc::*c-out*) :wait cl:nil)
  (cl:setf com.informatimago.languages.linc::*c-out* cl:t))

||#


;;;; THE END ;;;;
