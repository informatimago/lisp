;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               built-in-macros.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines the builtin cpp macros.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-06-27 <PJB> Created.
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.CPP")


(defun string-literal (string)
  (make-string-literal (format nil "~S" string)
                       (context-column *context*) (context-line *context*) (context-file *context*)))
(defun number-literal (value)
  (make-number (format nil "~A" value)
               (context-column *context*) (context-line *context*) (context-file *context*)))


(defmacro define-built-in-macro (name kind &body lambda-list-and-body)
  (let ((lambda-list (if (eql :function kind)
                         (first lambda-list-and-body)
                         nil))
        (body        (if (eql :function kind)
                         (rest lambda-list-and-body)
                         lambda-list-and-body)))
    `(setf (environment-macro-definition *default-environment* ',name)
           (make-instance
            ',(ecase kind
                (:object   'macro-definition/object/computed)
                (:function 'macro-definition/function/computed))
            :name ',name
            :compute-expansion-function ,(if (eq kind :object)
                                             `(lambda (macro-definition)
                                                (declare (ignorable macro-definition))
                                                ;;DEBUG;;(print ',name)
                                                ,@body)
                                             `(lambda (macro-definition arguments)
                                                (declare (ignorable macro-definition arguments))
                                                (destructuring-bind ,lambda-list arguments
                                                  ;;DEBUG;;(print ',name)
                                                  ,@body)))))))

(define-built-in-macro "__TIMESTAMP__" :object
  (when (option *context* :warn-date-time)
    (cpp-warning *context* "macro ~A might prevent reproducible builds"  "__TIMESTAMP__"))
  (let ((date (or (file-write-date (context-file *context*)) (get-universal-time))))
    (multiple-value-bind (se mi ho da mo ye dow) (decode-universal-time date 0)
      (list (string-literal (format nil "~[Mon~;Tue~;Wed~;Thi~;Fri~;Sat~;Sun~] ~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~2,'0D ~2,'0D:~2,'0D:~2,'0D ~4,'0D"
                                    dow mo da ho mi se ye))))))

(define-built-in-macro "__FILE__" :object
  (list (string-literal (file-namestring (context-file *context*)))))

(define-built-in-macro "__BASE_FILE__" :object
  (list (string-literal (file-namestring (context-base-file *context*)))))

(define-built-in-macro "__INCLUDE_LEVEL__" :object
  (list (number-literal (format nil "~D" (context-include-level *context*)))))

(define-built-in-macro "__STDC__" :object
  (list (number-literal 0 #|are we?|#)))

(define-built-in-macro "__DATE__" :object
  (when (option *context* :warn-date-time)
    (cpp-warning *context* "macro ~A might prevent reproducible builds"  "__DATE__"))
  (let ((date (get-universal-time)))
    (multiple-value-bind (se mi ho da mo ye) (decode-universal-time date 0)
      (declare (ignore se mi ho))
      (list (string-literal (format nil "~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~2,'0D ~4,'0D"
                                    mo da ye))))))

(define-built-in-macro "__TIME__" :object
  (when (option *context* :warn-date-time)
    (cpp-warning *context* "macro ~A might prevent reproducible builds"  "__DATE__"))
  (let ((date (get-universal-time)))
    (multiple-value-bind (se mi ho) (decode-universal-time date 0)
      (list (string-literal (format nil "~2,'0D:~2,'0D:~2,'0D" ho mi se))))))


(define-built-in-macro "__COUNTER__" :object
  (when (option *context* :directives-only)
    (cpp-error *context* "__COUNTER__ expanded inside directive with -fdirectives-only"))
  (list (number-literal (prog1 (context-counter *context*)
                          (incf (context-counter *context*))))))



;;;; THE END ;;;;
