;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               sequence-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests sequence.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from sequence.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE.TEST")

(define-test test/replace-subseq ()
  (let ((*standard-output* (make-broadcast-stream)))
    (let ((str (make-array 10 :adjustable t :element-type 'character :initial-contents "abcdefghij")))
      (print str)
      (assert-true (string= "abc123ghij"   (print (replace-subseq "123" str 3 6))))
      (assert-true (string= "abcABCij"     (print (replace-subseq "ABC" str 3 8))))
      (assert-true (string= "abc12345ij"   (print (replace-subseq "12345" str 3 6))))
      (assert-true (string= "78912345ij"   (print (replace-subseq "789" str 0 3))))
      (assert-true (string= "7891234501"   (print (replace-subseq "01" str 8))))
      (assert-true (string= "78912301"     (print (replace-subseq "" str 6 8))))
      (assert-true (string= "7891230123"   (print (replace-subseq "23" str (length str)))))
      (assert-true (string= "567891230123" (print (replace-subseq "56" str 0 0))))
      (assert-true (string= "123"          (print (replace-subseq "123" str 0))))
      (assert-true (string= "hello"        (print (replace-subseq "hello" "" 0))))
      (assert-true (string= ""             (print (replace-subseq "" "hello" 0)))))
    (let ((str (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
      (print str)
      (assert-true (equal '(a b c 1 2 3 g h i j)     (print (replace-subseq '(1 2 3) str 3 6))))
      (assert-true (equal '(a b c A B C i j)         (print (replace-subseq '(a b c) str 3 8))))
      (assert-true (equal '(a b c 1 2 3 4 5 i j)     (print (replace-subseq '(1 2 3 4 5) str 3 6))))
      (assert-true (equal '(7 8 9 1 2 3 4 5 i j)     (print (replace-subseq #(7 8 9) str 0 3))))
      (assert-true (equal '(7 8 9 1 2 3 4 5 0 1)     (print (replace-subseq #(0 1) str 8))))
      (assert-true (equal '(7 8 9 1 2 3 0 1)         (print (replace-subseq "" str 6 8))))
      (assert-true (equal '(7 8 9 1 2 3 0 1 2 3)     (print (replace-subseq #(2 3) str (length str)))))
      (assert-true (equal '(5 6 7 8 9 1 2 3 0 1 2 3) (print (replace-subseq '(5 6) str 0 0))))
      (assert-true (equal '(1 2 3)                   (print (replace-subseq '(1 2 3) str 0))))
      (assert-true (equal (coerce "hello" 'list)     (print (replace-subseq "hello" '() 0))))
      (assert-true (equal '()                        (print (replace-subseq "" '(1 2 3) 0))))))
  (assert-true (nth-value 1 (ignore-errors (replace-subseq "abc" "def" -1 2))))
  (assert-true (nth-value 1 (ignore-errors (replace-subseq "abc" "def" 1 4))))
  (assert-true (nth-value 1 (ignore-errors (replace-subseq "abc" "def" 2 1))))
  (assert-true (nth-value 1 (ignore-errors (replace-subseq "abc" "def" -2 4)))))


(define-test test/group-by ()
  (assert-true (equalp (group-by '() 3) '()))
  (assert-true (equalp (group-by '(1) 3) '((1))))
  (assert-true (equalp (group-by '(1 2) 3) '((1 2))))
  (assert-true (equalp (group-by '(1 2 3) 3) '((1 2 3))))
  (assert-true (equalp (group-by '(1 2 3 4) 3) '((1 2 3) (4))))
  (assert-true (equalp (group-by '(1 2 3 4 5) 3) '((1 2 3) (4 5))))
  (assert-true (equalp (group-by '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6))))
  (assert-true (equalp (group-by '(1 2 3 4 5 6 7) 3) '((1 2 3) (4 5 6) (7))))
  (assert-true (equalp (group-by '(1 2 3 4 5 6 7 8) 3) '((1 2 3) (4 5 6) (7 8))))
  (assert-true (equalp (group-by #() 3) '()))
  (assert-true (equalp (group-by #(1) 3) '(#(1))))
  (assert-true (equalp (group-by #(1 2) 3) '(#(1 2))))
  (assert-true (equalp (group-by #(1 2 3) 3) '(#(1 2 3))))
  (assert-true (equalp (group-by #(1 2 3 4) 3) '(#(1 2 3) #(4))))
  (assert-true (equalp (group-by #(1 2 3 4 5) 3) '(#(1 2 3) #(4 5))))
  (assert-true (equalp (group-by #(1 2 3 4 5 6) 3) '(#(1 2 3) #(4 5 6))))
  (assert-true (equalp (group-by #(1 2 3 4 5 6 7) 3) '(#(1 2 3) #(4 5 6) #(7))))
  (assert-true (equalp (group-by #(1 2 3 4 5 6 7 8) 3) '(#(1 2 3) #(4 5 6) #(7 8)))))

(define-test test/parse-sequence-type ()
  (check equal (multiple-value-list (parse-sequence-type 'list)) '(list t nil))
  (check equal (multiple-value-list (parse-sequence-type 'vector)) '(vector t nil))
  (check equal (multiple-value-list (parse-sequence-type '(vector))) '(vector t nil))
  (check equal (multiple-value-list (parse-sequence-type '(vector *))) '(vector t nil))
  (check equal (multiple-value-list (parse-sequence-type '(vector * 42))) '(vector t 42))
  (check equal (multiple-value-list (parse-sequence-type '(vector integer))) '(vector integer nil))
  (check equal (multiple-value-list (parse-sequence-type '(vector integer 42))) '(vector integer 42))
  (expect-condition 'error  (parse-sequence-type 'array))
  (expect-condition 'error  (parse-sequence-type '(array)))
  (expect-condition 'error  (parse-sequence-type '(array *)))
  (expect-condition 'error  (parse-sequence-type '(array * 42)))
  (expect-condition 'error  (parse-sequence-type '(array * 42)))
  (check equal (multiple-value-list (parse-sequence-type '(array * 1))) '(array t nil))
  (check equal (multiple-value-list (parse-sequence-type '(array integer 1))) '(array integer nil))
  (check equal (multiple-value-list (parse-sequence-type '(array * (42)))) '(array t 42))
  (check equal (multiple-value-list (parse-sequence-type '(array integer (42)))) '(array integer 42))
  (expect-condition 'error  (parse-sequence-type '(array integer (1 2)))))


(define-test test/concatenate-sequences ()
 (equalp (concatenate-sequences 'list   '((1 2 3) (4 5 6) (7 8 9))) '(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences 'list   #((1 2 3) (4 5 6) (7 8 9))) '(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences 'vector '((1 2 3) (4 5 6) (7 8 9))) #(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences 'vector #((1 2 3) (4 5 6) (7 8 9))) #(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences '(array * 1) '((1 2 3) (4 5 6) (7 8 9))) #(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences '(array * 1) #((1 2 3) (4 5 6) (7 8 9))) #(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences 'list   '(#(1 2 3) (4 5 6) #(7 8 9))) '(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences 'list   #(#(1 2 3) (4 5 6) #(7 8 9))) '(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences 'vector '(#(1 2 3) (4 5 6) #(7 8 9))) #(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences 'vector #(#(1 2 3) (4 5 6) #(7 8 9))) #(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences '(array * 1) '(#(1 2 3) (4 5 6) #(7 8 9))) #(1 2 3 4 5 6 7 8 9))
 (equalp (concatenate-sequences '(array * 1) #(#(1 2 3) (4 5 6) #(7 8 9))) #(1 2 3 4 5 6 7 8 9))
 (expect-condition 'error (concatenate-sequences '(array *) #(#(1 2 3) (4 5 6) #(7 8 9)))))


(defun as-vector (sequence)
  (make-array (length sequence) :initial-contents sequence))

(defun as-list (sequence)
  (replace (make-list (length sequence)) sequence))


(define-test test/prefixp ()
  (loop :for pf :in '(identity as-vector as-list)
        :do (loop :for sf  :in '(identity as-vector as-list)
                  :do (loop
                        :for (p s) :in '(("" "")
                                         ("" "HELLO")
                                         ("HELLO" "HELLO")
                                         ("HELLO" "HELLO WORLD"))
                        :do (assert-true (prefixp (funcall pf p) (funcall sf s))))
                      (loop
                        :for (p s) :in '(("HELLO" "HELL")
                                         ("HELLO" "SAY HELLO WORLD")
                                         ("HELLO" "SAY HELLO"))
                        :do (assert-false (prefixp (funcall pf p) (funcall sf s)))))))

(define-test test/suffixp ()
  (loop :for pf :in '(identity as-vector as-list)
        :do (loop :for sf  :in '(identity as-vector as-list)
                  :do (loop
                        :for (p s) :in '(("" "")
                                         ("" "HELLO")
                                         ("HELLO" "HELLO")
                                         ("WORLD" "HELLO WORLD"))
                        :do (assert-true (suffixp (funcall pf p) (funcall sf s))))
                      (loop
                        :for (p s) :in '(("HELLO" "ELLO")
                                         ("HELLO" "SAY HELLO WORLD")
                                         ("HELLO" "SAY WORLD"))
                        :do (assert-false (suffixp (funcall pf p) (funcall sf s)))))))



(define-test test/all ()
  (test/replace-subseq)
  (test/group-by)
  (test/parse-sequence-type)
  (test/concatenate-sequences)
  (test/prefixp)
  (test/suffixp))


;;;; THE END ;;;;
