;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               string-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests string.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from string.lisp.
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING.TEST")


(define-test test/string-designator ()
  (assert-true (typep "toto" 'string-designator))
  (assert-true (typep 'toto  'string-designator))
  (assert-true (typep #\t    'string-designator))
  (assert-true (not (typep 42 'string-designator)))
  (assert-true (not (typep #(#\a #\b) 'string-designator)))
  (assert-true (not (typep '(#\a #\b) 'string-designator)))
  (assert-true (typep "t"    '(string-designator 1)))
  (assert-true (typep 't     '(string-designator 1)))
  (assert-true (typep #\t    '(string-designator 1)))
  (assert-true (typep "toto" '(string-designator 4)))
  (assert-true (typep 'toto  '(string-designator 4)))
  (assert-true (not (typep "toto" '(string-designator 2))))
  (assert-true (not (typep 'toto  '(string-designator 2))))
  (assert-true (not (typep #\t    '(string-designator 2))))
  (assert-true (not (typep 42 '(string-designator 1))))
  (assert-true (not (typep #(#\a #\b) '(string-designator 2))))
  (assert-true (not (typep '(#\a #\b) '(string-designator 2)))))


(define-test test/character-designator ()
  (assert-true (typep "t"    'character-designator))
  (assert-true (typep 't     'character-designator))
  (assert-true (typep #\t    'character-designator))
  (assert-true (not (typep "toto" 'character-designator)))
  (assert-true (not (typep 'toto  'character-designator)))
  (assert-true (not (typep 42     'character-designator)))
  (assert-true (not (typep #(#\a) 'character-designator)))
  (assert-true (not (typep '(#\a) 'character-designator))))


(define-test test/concatenate-strings ()
  (assert-true (equal "" (concatenate-strings '())))
  (assert-true (equal "" (concatenate-strings '(""))))
  (assert-true (equal "" (concatenate-strings '("" "" ""))))
  (assert-true (equal "" (concatenate-strings '(("" 0 0) ("abc" 0 0) ("abc" 1 1) (#\a 0 0)))))
  (assert-true (equal "abc" (concatenate-strings '("abc"))))
  (assert-true (equal "abc" (concatenate-strings '("a" "b" "c"))))
  (assert-true (equal "abc" (concatenate-strings '((#\a #\b) "c"))))
  (assert-true (equal "abc" (concatenate-strings '(#\a #\b #\c))))
  (assert-true (equal "abc" (concatenate-strings '(|a| |b| |c|))))
  (assert-true (equal "abc" (concatenate-strings '(|a| "b" #\c))))
  (assert-true (equal "abcdef" (concatenate-strings '("ab" "cd" "ef"))))
  (assert-true (equal "abcdef" (concatenate-strings '(("abcdef" 0 2) ("abcdef" 2 4) ("abcdef" 4 6)))))
  (assert-true (equal "abcdef" (concatenate-strings '(((#\a #\b #\c #\d #\e #\f) 0 2) ("abcdef" 2 4) ("abcdef" 4 6)))))
  (assert-true (equal "abcdef" (concatenate-strings '(#\a #\b #\c "def")))))


(define-test test/implode-explode ()
  ;; implode-string
  (assert-true (string= "" (implode-string "")))
  (assert-true (string= "" (implode-string #())))
  (assert-true (string= "" (implode-string '())))
  #-sbcl (assert-true (null (ignore-errors (implode-string 42))))
  (assert-true (string= "ABC" (implode-string "ABC")))
  (assert-true (string= "ABC" (implode-string #(#\A #\B #\C))))
  (assert-true (string= "ABC" (implode-string '(#\A #\B #\C))))
  (assert-true (null (ignore-errors (implode-string '(42)))))
  ;; explode-string
  (assert-true (eq      (explode-string "")         nil))
  (assert-true (eq      (explode-string "" 'list)   nil))
  (assert-true (string= (explode-string "" 'string) ""))
  (assert-true (equalp  (explode-string "" 'vector) #()))
  (assert-true (equal  (explode-string "ABC")       '(#\A #\B #\C)))
  (assert-true (equal  (explode-string "ABC" 'list) '(#\A #\B #\C)))
  (assert-true (and  (every 'char= (explode-string "ABC" 'vector) #(#\A #\B #\C))
                     (= (length (explode-string "ABC" 'vector)) (length  #(#\A #\B #\C)))))
  (assert-true (string= (explode-string "ABC" 'string) "ABC"))
  ;; implode a string
  (assert-true (eq      (implode "" 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
                        'com.informatimago.common-lisp.cesarum.string::||))
  (assert-true (eq      (implode "ABC" 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
                        'com.informatimago.common-lisp.cesarum.string::abc))
  (assert-true (eq      (implode "ABC" 'symbol :keyword) ':ABC))
  (assert-true (string= (implode "" 'string) ""))
  (assert-true (string= (implode "ABC" 'string) "ABC"))
  (assert-true (equal   (implode "(1 2 3)" 'list) '(1 2 3))) 
  (assert-true (equal   (implode "NIL" 'list) '()))
  (assert-true (equalp  (implode "#(1 2 3)" 'vector) #(1 2 3))) 
  ;; implode a vector
  (assert-true (eq      (implode #() 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
                        'com.informatimago.common-lisp.cesarum.string::||))
  (assert-true (eq      (implode #(#\A #\B #\C) 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
                        'com.informatimago.common-lisp.cesarum.string::abc))
  (assert-true (eq      (implode #(#\A #\B #\C) 'symbol :keyword) ':ABC))
  (assert-true (string= (implode #() 'string) ""))
  (assert-true (string= (implode #(#\A #\B #\C) 'string) "ABC"))
  (assert-true (equal   (implode #(#\( #\1 #\space #\2  #\space #\3 #\)) 'list) '(1 2 3))) 
  (assert-true (equal   (implode #(#\N #\I #\L) 'list) '()))
  (assert-true (equalp  (implode #(#\# #\( #\1 #\space #\2  #\space #\3 #\)) 'vector) #(1 2 3))) 
  ;; implode a list
  (assert-true (eq      (implode '() 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
                        'com.informatimago.common-lisp.cesarum.string::||))
  (assert-true (eq      (implode '(#\A #\B #\C) 'symbol "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
                        'com.informatimago.common-lisp.cesarum.string::abc))
  (assert-true (eq      (implode '(#\A #\B #\C) 'symbol :keyword) ':ABC))
  (assert-true (string= (implode '() 'string) ""))
  (assert-true (string= (implode '(#\A #\B #\C) 'string) "ABC"))
  (assert-true (equal   (implode '(#\( #\1 #\space #\2  #\space #\3 #\)) 'list) '(1 2 3))) 
  (assert-true (equal   (implode '(#\N #\I #\L) 'list) '()))
  (assert-true (equalp  (implode '(#\# #\( #\1 #\space #\2  #\space #\3 #\)) 'vector) #(1 2 3)))
  ;; explode
  (assert-true (equal  (explode 'hello) '(#\H #\E #\L #\L #\O)))
  (assert-true (equal  (explode 'hello 'list) '(#\H #\E #\L #\L #\O)))
  (assert-true (equalp (explode 'hello 'vector) #(#\H #\E #\L #\L #\O)))
  (assert-true (equalp (explode 'hello 'string) "HELLO"))
  (assert-true (equal  (explode "HELLO") '(#\H #\E #\L #\L #\O)))
  (assert-true (equal  (explode "HELLO" 'list) '(#\H #\E #\L #\L #\O)))
  (assert-true (equalp (explode "HELLO" 'vector) #(#\H #\E #\L #\L #\O)))
  (assert-true (equalp (explode "HELLO" 'string) "HELLO"))
  (assert-true (equalp (explode #(#\H #\E #\L #\L #\O)) '(#\H #\E #\L #\L #\O)))
  (assert-true (equalp (explode #(#\H #\E #\L #\L #\O) 'list) '(#\H #\E #\L #\L #\O)))
  (assert-true (equalp (explode #(#\H #\E #\L #\L #\O) 'vector) #(#\H #\E #\L #\L #\O)))
  (assert-true (equalp (explode #(#\H #\E #\L #\L #\O) 'string) "HELLO")))


(defun as-symbol (string)
  (make-symbol string))

(define-test test/prefixp ()
  (loop :for pf :in '(identity as-symbol)
        :do (loop :for sf  :in '(identity as-symbol)
                  :do (loop
                        :for (p s) :in '(("" #\H)
                                         (#\H #\H)
                                         (#\H "H")
                                         ("H" #\H)
                                         (#\H "HELLO")
                                         ("" "")
                                         ("" "HELLO")
                                         ("HELLO" "HELLO")
                                         ("HELLO" "HELLO WORLD"))
                        :do (assert-true (prefixp (funcall pf p) (funcall sf s))))
                      (loop
                        :for (p s) :in '((#\H "")
                                         (#\H #\A)
                                         (#\A #\H)
                                         ("HELLO" #\H)
                                         ("HELLO" "")
                                         ("HELLO" "HELL")
                                         ("HELLO" "SAY HELLO WORLD")
                                         ("HELLO" "SAY HELLO"))
                        :do (assert-false (prefixp (funcall pf p) (funcall sf s)))))))

(define-test test/suffixp ()
  (loop :for pf :in '(identity as-symbol )
        :do (loop :for sf  :in '(identity as-symbol)
                  :do (loop
                        :for (p s) :in '(("" #\H)
                                         (#\H #\H)
                                         (#\H "H")
                                         ("H" #\H)
                                         (#\O "HELLO")
                                         ("" "")
                                         ("" "HELLO")
                                         ("HELLO" "HELLO")
                                         ("WORLD" "HELLO WORLD"))
                        :do (assert-true (suffixp (funcall pf p) (funcall sf s))))
                      (loop
                        :for (p s) :in '((#\H "")
                                         (#\H #\A)
                                         (#\A #\H)
                                         ("HELLO" #\H)
                                         ("HELLO" "ELLO")
                                         ("HELLO" "SAY HELLO WORLD")
                                         ("HELLO" "SAY WORLD"))
                        :do (assert-false (suffixp (funcall pf p) (funcall sf s)))))))


(define-test test/split-string ()
  (check equalp
         (split-string '(1 2 0 3 4 5 0 6 7 8 0 9) '(0))
         '((1 2) (3 4 5) (6 7 8) (9)))
  (check equalp
         (split-string #(1 2 0 3 4 5 0 6 7 8 0 9) #(0))
         '(#(1 2) #(3 4 5) #(6 7 8) #(9)))
  (check equalp (split-string "1 2 0 3 4 5 0 6 7 8" '(#\space #\0))
         '("1" "2" "" "" "3" "4" "5" "" "" "6" "7" "8"))
  (check equalp (split-string "hello world" "*")
         '("hello world"))
  (check equalp (split-string "hello world*" "*")
         '("hello world" ""))
  (check equalp (split-string "hello world*" "*" :remove-empty)
         '("hello world"))
  (check equalp (split-string "hello world***" "*" :remove-empty)
         '("hello world"))
  (check equalp (split-string "hello world*x" "*")
         '("hello world" "x"))
  (check equalp (split-string "hello*world" "*")
         '("hello" "world"))
  (check equalp (split-string "hello*world*" "*")
         '("hello" "world" ""))
  (check equalp (split-string "hello*world*" "*" :remove-empty)
         '("hello" "world"))
  (check equalp (split-string "hello*world***" "*" :remove-empty)
         '("hello" "world"))
  (check equalp (split-string "hello*world*x" "*")
         '("hello" "world" "x")))




(define-test test/all ()
  (test/string-designator)
  (test/character-designator)
  (test/concatenate-strings)
  (test/implode-explode)
  (test/prefixp)
  (test/suffixp)
  (test/split-string))

;;;; THE END ;;;;
