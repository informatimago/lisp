;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               symbol.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A tool to check duplicate/un-exported/imported symbols.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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

(defpackage "COM.INFORMATIMAGO.TOOLS.SYMBOL"
  (:use "COMMON-LISP")
  (:use "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE")
  (:export "CHECK-DUPLICATE-SYMBOLS"
           "DUPLICATE-SYMBOLS")
  (:documentation "
A tool to check duplicate/un-exported/imported symbols.
"))
(in-package "COM.INFORMATIMAGO.TOOLS.SYMBOL")


(defun report-duplicates (duplicates &optional (*standard-output* *standard-output*))
  (let ((*package* (find-package "KEYWORD")))
    (dolist (symbols (sort duplicates
                           (function string<)
                           :key (lambda (syms) (symbol-name (first syms))))
                     (values))
      (format t "~20A ~{~A~^ ~}~%"
              (symbol-name (first symbols))
              (sort (mapcar (lambda (sym) (package-name (symbol-package sym))) symbols)
                    (function string<))))))

(defun duplicate-symbols (&key (packages (list-all-packages)) (exported nil))
  "Return: a list of list of symbols that have the same name."
  (let ((symbols (make-hash-table :test (function equal))) ; maps names to list of unique symbols
        (duplicates '()))
    (dolist (p packages)
      (dolist (s (list-all-symbols p))
        (pushnew s (gethash (symbol-name s) symbols '()))))
    (maphash (lambda (name symbols)
               (declare (ignore name))
               (when (cdr symbols)
                 (push symbols duplicates)))
             symbols)
    (if exported
        (remove-if-not (lambda (symbols)
                         (some (lambda (symbol)
                                 (eq :external
                                     (nth-value 1 (find-symbol (symbol-name symbol)
                                                               (symbol-package symbol)))))
                               symbols))
                       duplicates)
        duplicates)))

(defun check-duplicate-symbols (&key (packages (list-all-packages)) (exported nil))
  (report-duplicates (duplicate-symbols :packages packages :exported exported)))



;;;; THE END ;;;;
