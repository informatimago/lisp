;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               relative-package-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests the relative-package.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-11-02 <PJB> Adapted from http://franz.com/support/documentation/8.1/doc/packages.htm#relative-2
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE"
                          . #1=("FIND-PACKAGE"
                                "MAKE-PACKAGE" "DELETE-PACKAGE"
                                "FIND-SYMBOL" "IMPORT" "INTERN" "SHADOW" "SHADOWING-IMPORT"
                                "EXPORT" "UNEXPORT" "UNINTERN" "USE-PACKAGE"
                                "UNUSE-PACKAGE" "PACKAGE-NAME" "PACKAGE-NICKNAMES"
                                "PACKAGE-USE-LIST" "PACKAGE-USED-BY-LIST" "PACKAGE-SHADOWING-SYMBOLS"
                                "RENAME-PACKAGE"
                                "WITH-PACKAGE-ITERATOR"
                                "DO-SYMBOLS" "DO-EXTERNAL-SYMBOLS" 
                                "DEFPACKAGE" "IN-PACKAGE"))
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE"
                "SYMBOL-FROM-SPLIT-TOKEN")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
                "SYMBOL-IN-MISSING-PACKAGE-ERROR"
                "SYMBOL-MISSING-IN-PACKAGE-ERROR"
                "UNEXPORTED-SYMBOL-ERROR"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST")

;; There should be no package named
;; :com.informatimago.common-lisp.lisp.relative-package.test.none
;; so that an error is signaled when we try to access it.

(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d.e)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d.f)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.e)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.f)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.d)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.e)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.c)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.d)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.b)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.c)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test.d)

(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test-foo.bar.baz)
(defpackage :com.informatimago.common-lisp.lisp.relative-package.test.none.test-foo.bar.baz.wham)

(define-test test/package-children ()
  (let ((*disable-useless-parent-package-check* nil))
    (check equal
           (sort (mapcar #'package-name
                         (package-children :com.informatimago.common-lisp.lisp.relative-package.test.none.test :recurse nil))
                 #'string<)
           '("COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.B"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.C"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.D"))
    (check equal
           (sort (mapcar #'package-name (package-children :com.informatimago.common-lisp.lisp.relative-package.test.none.test))
                 #'string<)
           '("COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D.E"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D.F"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.E"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.F"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.D"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.E"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.C"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.D"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.B"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.C"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.D"))
    (check equal
           (sort (mapcar #'package-name (package-children :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c))
                 #'string<)
           '("COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D.E"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D.F"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.E"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.F"))
    (check equal
           (sort (mapcar #'package-name
                         (package-children :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c :recurse nil))
                 #'string<)
           '("COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.E"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.F"))
    (check equal
           (sort (mapcar #'package-name (package-children :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d))
                 #'string<)
           '("COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D.E"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D.F"))
    (check equal
           (sort (mapcar #'package-name
                         (package-children :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d :recurse nil))
                 #'string<)
           '("COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D.E"
             "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE.TEST.NONE.TEST.A.B.C.D.F"))
    (check equal
           (package-children :com.informatimago.common-lisp.lisp.relative-package.test.none.test.b)
           '())
    (check equal
           (package-children :com.informatimago.common-lisp.lisp.relative-package.test.none.test.c)
           '())
    (check equal
           (package-children :com.informatimago.common-lisp.lisp.relative-package.test.none.test.d)
           '())))

(define-test test/package-parent ()
  (let ((*disable-useless-parent-package-check* nil))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d.e)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d.f)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.e)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.f)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.d)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.e)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.c)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.d)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.b)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.c)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test))
    (check eql
           (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test.d)
           (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test))
    (expect-condition 'error (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test))
    (expect-condition 'error (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test-foo.bar.baz))
    (expect-condition 'error (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test-foo.bar))
    (expect-condition 'error (package-parent :com.informatimago.common-lisp.lisp.relative-package.test.none.test-foo))))

(define-test test/find-package ()
  (let ((*disable-useless-parent-package-check* nil))
   (dolist (item '((:com.informatimago.common-lisp.lisp.relative-package.test.none.test.a        
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a ".")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test          
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a "..")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.b        
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a "..B")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.c        
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a "..C")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.d        
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a "..D")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b      
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.b "..A.B")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test          
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b "...")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.b        
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b "...B")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d.f
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d "...C.D.F")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test          
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d ".....")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.b        
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d ".....B")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d  
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c.d ".")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c    
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b.c ".")
                   (:com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b      
                    :com.informatimago.common-lisp.lisp.relative-package.test.none.test.a.b ".")))
     (check string=
            (let ((*package* (find-package (second item))) )
              (package-name (find-package (third item))))
            (symbol-name (first item))
            ((second item) (third item))))
    (let ((*package* (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test)))
      (expect-condition 'error (find-package ".."))
      (expect-condition 'error (find-package "..."))
      (expect-condition 'error (find-package "...."))
      (expect-condition 'error (find-package "....FOO")))
    (let ((*package* (find-package :com.informatimago.common-lisp.lisp.relative-package.test.none.test.b)))
      (expect-condition 'error (find-package "...")))))


(define-test test/symbol-from-split-token ()
  (expect-condition 'error (symbol-from-split-token '()))
  (expect-condition 'error (symbol-from-split-token '(1)))
  (expect-condition 'error (symbol-from-split-token '(2)))
  (expect-condition 'error (symbol-from-split-token '("FROOS" 2 "PPO" 2)))
  (expect-condition 'error (symbol-from-split-token '(3 "PPO")))
  (expect-condition 'symbol-in-missing-package-error
                    (symbol-from-split-token '("FROOS" 1 "PPO")))
  (expect-condition 'symbol-in-missing-package-error
                    (symbol-from-split-token '("FROOS" 2 "PPO")))
  (check eq :foo (symbol-from-split-token '(1 "FOO")))
  (expect-condition 'unexported-symbol-error
                    (symbol-from-split-token '("CL-USER" 1 "KAZOO")))
  (check eq 'cl-user::kazoo (symbol-from-split-token '("CL-USER" 2 "KAZOO")))
  (check eq 'cl:sin (symbol-from-split-token '("CL" 1 "SIN")))
  (check eq 'cl:sin (symbol-from-split-token '("CL" 2 "SIN"))))


(define-test test/all ()
  (test/package-children)
  (test/package-parent)
  (test/find-package)
  (test/symbol-from-split-token))

;;;; THE END ;;;;
