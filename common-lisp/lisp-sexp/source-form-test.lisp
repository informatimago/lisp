;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               source-form-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests source-form.lisp
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from source-form.lisp
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM.TEST")

(define-test test/parse-body ()
  (flet ((test/lambda (body expect-error-p docstring declarations forms)
           (handler-case
               (multiple-value-bind (doc dec for) (parse-body :lambda body)
                 (assert-true (not expect-error-p))
                 (assert-true (equalp (first doc) docstring))
                 (assert-true (equalp dec declarations))
                 (assert-true (equalp for forms)))
             (program-error () (assert-true expect-error-p))))
         (test/locally (body expect-error-p declarations forms)
           (handler-case
               (multiple-value-bind (doc dec for) (parse-body :locally body)
                 (assert-true (not expect-error-p))
                 (assert-true (null doc))
                 (assert-true (equalp dec declarations))
                 (assert-true (equalp for forms))
                 (assert-true (equalp (append dec for) body)))
             (program-error () (assert-true expect-error-p))))
         (test/progn (body expect-error-p forms)
           (handler-case
               (multiple-value-bind (doc dec for) (parse-body :progn body)
                 (assert-true (not expect-error-p))
                 (assert-true (null doc))
                 (assert-true (null dec))
                 (assert-true (equalp for forms))
                 (assert-true (equalp for body)))
             (program-error () (assert-true expect-error-p))))
         (test/extract (body rlambda rlocally)
           (destructuring-bind (expect-error-p docstring declarations forms) rlambda
             (handler-case
                 (let ((doc (extract-documentation body))
                       (dec (extract-declarations body t))
                       (for (extract-body body)))
                   (assert-true (not expect-error-p))
                   (assert-true (equalp doc docstring))
                   (assert-true (equalp dec declarations))
                   (assert-true (equalp for forms)))
               (program-error () (assert-true expect-error-p))))
           (destructuring-bind (expect-error-p declarations forms) rlocally
             (declare (ignore forms))
             (handler-case
                 (let ((dec (extract-declarations body nil)))
                   (assert-true (not expect-error-p))
                   (assert-true (equalp dec declarations)))
               (program-error () (assert-true expect-error-p))))))
    (let ((test-cases
           '((()
              (nil nil () ())
              (nil     () ())
              (nil        ()))
             (("Result")
              (nil nil () ("Result"))
              (nil     () ("Result"))
              (nil        ("Result")))
             (("Doc" "Result")
              (nil "Doc" () ("Result"))
              (nil       () ("Doc" "Result"))
              (nil          ("Doc" "Result")))
             (((declare (ignore it)))
              (nil nil ((declare (ignore it))) ())
              (nil     ((declare (ignore it))) ())
              (t                               nil))
             (((declare (ignore it)) "Result")
              (nil nil ((declare (ignore it))) ("Result"))
              (nil     ((declare (ignore it))) ("Result"))
              (t                               nil))
             (((declare (ignore it)) "Doc" "Result")
              (nil "Doc" ((declare (ignore it))) ("Result"))
              (nil       ((declare (ignore it))) ("Doc" "Result"))
              (t                                 nil))
             (((declare (ignore it)) (declare (ignore it)))
              (nil nil ((declare (ignore it)) (declare (ignore it))) ())
              (nil     ((declare (ignore it)) (declare (ignore it))) ())
              (t                                                     nil))
             (((declare (ignore it)) (declare (ignore it)) "Result")
              (nil nil ((declare (ignore it)) (declare (ignore it))) ("Result"))
              (nil     ((declare (ignore it)) (declare (ignore it))) ("Result"))
              (t                                                     nil))
             (((declare (ignore it)) "Doc" (declare (ignore it)))
              (nil "Doc" ((declare (ignore it)) (declare (ignore it))) ())
              (t         nil                                           nil)
              (t                                                       nil))
             (((declare (ignore it)) (declare (ignore it)) "Doc" "Result")
              (nil "Doc" ((declare (ignore it)) (declare (ignore it))) ("Result"))
              (nil       ((declare (ignore it)) (declare (ignore it))) ("Doc" "Result"))
              (t                                                       nil))
             (((declare (ignore it)) "Doc" (declare (ignore it)) "Result")
              (nil "Doc" ((declare (ignore it)) (declare (ignore it))) ("Result"))
              (t         nil                                           nil)
              (t                                                       nil))
             (((declare (ignore it)) "Doc" "Illegal" (declare (ignore it)) "Result")
              (t  nil nil nil)
              (t      nil nil)
              (t          nil))
             (("Doc" (declare (ignore it)) "Result")
              (nil "Doc" ((declare (ignore it))) ("Result"))
              (t         nil                     nil)
              (t                                 nil))
             (("Doc" (declare (ignore it)) (declare (ignore it)))
              (nil "Doc" ((declare (ignore it)) (declare (ignore it))) ())
              (t         nil                                          nil)
              (t                                                      nil))
             (("Doc" (declare (ignore it)) (declare (ignore it)) "Result")
              (nil "Doc" ((declare (ignore it)) (declare (ignore it))) ("Result"))
              (t         nil                                          nil)
              (t                                                      nil))
             (("Doc" (declare (ignore it)) "Illegal" (declare (ignore it)) "Result")
              (t  nil nil nil)
              (t      nil nil)
              (t          nil)))))
      (loop :for (test rlambda rlocally rprogn) :in test-cases :do
        (apply (function test/lambda)  test rlambda)
        (apply (function test/locally) test rlocally)
        (apply (function test/progn)   test rprogn)
        (test/extract test rlambda rlocally)))))


(define-test test/all ()
  (test/parse-body))

;;;; THE END ;;;;
