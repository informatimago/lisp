;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               unintern.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-07 <PJB> Extracted from analysis.lisp
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))


(ql:quickload :com.informatimago.common-lisp.lisp-text)

"

com.informatimago.common-lisp.lisp-text.source-text  uses
indifferentiated source-token instances to represents tokens (symbols,
numbers, the dot of pairs).  This is not practical to analyze the sources.

"

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.UNINTERN"
  (:use "COMMON-LISP"
         "COM.INFORMATIMAGO.COMMON-LISP.LISP-TEXT.SOURCE-TEXT")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
                          "COPY-READTABLE"
                          "READTABLE-CASE"
                          "SET-DISPATCH-MACRO-CHARACTER")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
                "DEFPARSER"
                "PARSE-TOKEN"
                "PARSE-DECIMAL-INTEGER-TOKEN"
                "PARSE-INTEGER-TOKEN"
                "PARSE-RATIO-TOKEN"
                "PARSE-FLOAT-1-TOKEN"
                "PARSE-FLOAT-2-TOKEN")
  (:export)
  (:documentation "
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.UNINTERN")

(defvar *system-packages* '("COMMON-LISP" "KEYWORD")
  "List of names of the packages where we intern the symbols with CL:INTERN.")


(defparser parse-symbol-token (token)
  "symbol ::= symbol-name
symbol ::= package-marker symbol-name
symbol ::= package-marker package-marker symbol-name
symbol ::= package-name package-marker symbol-name
symbol ::= package-name package-marker package-marker symbol-name
symbol-name   ::= {alphabetic}+ 
package-name  ::= {alphabetic}+ "
  (let ((colon (position-if
                (lambda (traits) (traitp +ct-package-marker+ traits))
                (token-traits token))))
    (if colon
        (let* ((double-colon (and (< (1+ colon) (token-length token))
                                  (traitp +ct-package-marker+
                                          (token-char-traits token (1+ colon)))))
               (pname (subseq (token-text token) 0 colon))
               (sname (subseq (token-text token)
                              (+ colon (if double-colon 2 1)))))
          (when (position-if
                 (lambda (traits) (traitp +ct-package-marker+ traits))
                 (token-traits token) :start (+ colon (if double-colon 2 1)))
            (reject t "too many package markers in token ~s" (token-text token)))
          (when (zerop colon)
            ;; keywords always exist, so let's intern them before finding them.
            (setf pname "keyword")
            (intern sname pname))
          ;; the following form thanks to andrew philpot <philpot@isi.edu>
          ;; corrects a bug when reading with double-colon uninterned symbols:
          (if (find-package pname)
              (if double-colon
                  (accept 'symbol (intern sname pname))
                  (multiple-value-bind (sym where) (find-symbol sname pname)
                    (if (eq where :external) 
                        (accept 'symbol sym)
                        (accept 'symbol
                                (restart-case (error 'symbol-missing-in-package-error
                                                     :stream *input-stream* :package-name pname :symbol-name sname)
                                  (make-symbol (&rest rest)
                                    :report "make the missing symbol in the specified package"
                                    (declare (ignore rest))
                                    (intern sname pname)))))))
              (accept 'symbol
                      (restart-case (error 'symbol-in-missing-package-error
                                           :stream *input-stream* :package-name pname :symbol-name sname)
                        (intern-here (&rest rest)
                          :report "intern the symbol in the current package, instead"
                          (declare (ignore rest))
                          (intern sname))
                        (return-uninterned (&rest rest)
                          :report "return an uninterned symbol, instead"
                          (declare (ignore rest))
                          (make-symbol sname))))))
        ;; no colon in token, let's just intern the symbol in the current package :
        (accept 'symbol (intern (token-text token) *package*)))))


(defun %parse-token (token)
  "
return:  okp ; the parsed lisp object if okp, or an error message if (not okp)
"
  (let ((message nil))
    (macrolet
        ((rom (&body body)
           "result or message"
           (if (null body)
               'nil
               (let ((vals (gensym)))
                 `(let ((,vals (multiple-value-list ,(car body))))
                    ;; (format *trace-output* "~s --> ~s~%" ',(car body) ,vals)
                    (if (first ,vals)
                        (values-list ,vals)
                        (progn
                          (when (second ,vals)
                            (setf message  (third ,vals)))
                          (rom ,@(cdr body)))))))))
      (multiple-value-bind (ok type object)
          (rom (parse-decimal-integer-token token)
               (parse-integer-token         token)
               (parse-ratio-token           token)
               (parse-float-1-token         token)
               (parse-float-2-token         token)
               ;; (parse-consing-dot-token     token)
               (parse-symbol-token          token))
        (declare (ignorable type))
        ;; (format *trace-output* "ok = ~s ; type = ~s ; object = ~s~%"
        ;;         ok type object)
        (values ok (if ok object message))))))


(defparameter *sources*
  (sort (directory "pw-src/**/*.lisp")
        (function string<) :key (function namestring)))

;; (with-open-file (source (first *sources*))
;;   (cons (pathname source)
;;         (loop
;;           :with eof = (gensym "EOF")
;;           :for sexp = (com.informatimago.common-lisp.lisp-reader.reader:read source nil eof)
;;           :until (eq eof sexp)
;;           :collect sexp)))


(defvar *ccl-source-readtable* (copy-readtable *source-readtable*))
(defvar *ccl-ffi-readtable*    (copy-readtable *source-readtable*))
(setf (readtable-case *ccl-ffi-readtable*) :preserve)


(defclass source-ccl-point (source-object dispatch-macro-character-mixin)
  ((point :initarg :point :accessor source-ccl-point-point)))

(defun reader-dispatch-macro-ccl-point (stream sub-char arg)
  (com.informatimago.common-lisp.lisp-text.source-text::building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-ccl-point
   :point (source-read stream t nil t)))

(set-dispatch-macro-character #\# #\@ (function reader-dispatch-macro-ccl-point)
                              *ccl-source-readtable*)


(defclass source-ccl-ffi (source-object dispatch-macro-character-mixin)
  ((name :initarg :fname :accessor source-ccl-ffi-name)))

(defun ccl-reader-dispatch-macro-ffi (stream sub-char arg)
  (com.informatimago.common-lisp.lisp-text.source-text::building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-ccl-ffi
   :name (let ((*source-readtable* *ccl-ffi-readtable*)) (source-read stream t nil t))))

(set-dispatch-macro-character #\# #\_ (function ccl-reader-dispatch-macro-ffi)
                              *ccl-source-readtable*)


(defparameter *contents*
  (let ((contents '()))
    (dolist (source *sources* contents)
      (with-open-file (stream source)
        (push (cons (pathname source)
                    (loop
                      :for sexp = (source-read stream nil stream)
                      :until (eq sexp stream)
                      :collect sexp))
              contents)))))



;;;; THE END ;;;;
