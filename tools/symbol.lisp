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
;;;;    Copyright Pascal J. Bourguignon 2014 - 2016
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
(defpackage "COM.INFORMATIMAGO.TOOLS.SYMBOL"
  (:use "COMMON-LISP"
        "CL-PPCRE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (:shadow "APROPOS" "APROPOS-LIST")
  (:export "CHECK-DUPLICATE-SYMBOLS"
           "DUPLICATE-SYMBOLS"
           "APROPOS" "APROPOS-LIST"
           "REPORT-PACKAGES" "REPORT-SYMBOL")
  (:documentation "
A tool to check duplicate/un-exported/imported symbols.
"))
(defpackage "COM.INFORMATIMAGO.TOOLS.SYMBOL.EMPTY"
  (:use)
  (:documentation "An empty package, used when printing symbols."))
(in-package "COM.INFORMATIMAGO.TOOLS.SYMBOL")


(defun report-packages (packages)
  (let ((packages (mapcar (function find-package) packages)))
    (dolist (user packages)
      (format t "~%Package ~A ~@[(~{~A~^ ~})~]~%"
              (package-name user)
              (package-nicknames user))
      (dolist (used (package-use-list user))
        (when (member used packages) ; don't display the others
          (format t "    ~:[uses~;USES~] ~A~%"
                  (member used packages)
                  (package-name used)))))))


(defgeneric report-symbol (symbol-designator)
  (:method ((name string))
    (mapc (function report-symbol)
          (let (l)
            (dolist (p (list-all-packages) (remove-duplicates l))
              (multiple-value-bind (sym stat) (find-symbol name p)
                (when stat
                  (push sym l)))))))
  (:method ((symbol symbol))
    (let* ((name         (symbol-name symbol))
           (home-package (symbol-package symbol))
           (all-packages (let (l)
                           (dolist (p (list-all-packages)
                                     (sort l (function string<)
                                           :key (lambda (x) (package-name (cdr x)))))
                             (multiple-value-bind (sym stat) (find-symbol name p)
                               (when (eq symbol sym)
                                 (push (cons stat p) l)))))))
      (format t "~%~72@{-~}" '())
      (format t "~%Symbol ~A~%" name)
      (dolist (p all-packages (values))
        (format t "   ~10A ~:[      ~;(home)~] ~A ~@[(~{~A~^ ~})~]~%"
                (car p)
                (eq (cdr p) home-package)
                (package-name (cdr p))
                (package-nicknames (cdr p))))
      (report-packages (mapcar (function cdr) all-packages)))
    (values)))


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


(defun apropos-list (regexp &optional packages)
  "
Same as CL:APROPOS-LIST, but takes a REGEXP as first argument,
and a list of packages as optional second argument.

Ok, the good thing is that it's a regexp,
the bad thing is that it's a PPCRE regexp. :-(
"
  (let ((packages (if packages
                      (mapcar (function find-package) (ensure-list packages))
                      (list-all-packages)))
        (symbols  '())
        (compre   (create-scanner regexp :case-insensitive-mode t)))
    (dolist (package packages)
      (do-symbols (symbol package)
        (when (scan compre (symbol-name symbol))
          (push symbol symbols))))
    (delete-duplicates symbols)))


(defun apropos (regexp &optional packages)
  "
Same as CL:APROPOS, but takes a REGEXP as first argument,
and a list of packages as optional second argument.

Ok, the good thing is that it's a regexp,
the bad thing is that it's a PPCRE regexp. :-(
"
  (flet ((accessiblep (sym)
           (multiple-value-bind (existing-sym presentp)
               (find-symbol (symbol-name sym) *package*)
             (and presentp (eq existing-sym sym)))))
    (let* ((syms (sort (apropos-list regexp (when packages (ensure-list packages)))
                       (lambda (a b)
                         (let ((pa (package-name (symbol-package a)))
                               (pb (package-name (symbol-package b))))
                           (or (string< pa pb)
                               (and (string= pa pb)
                                    (string< a b)))))))
           (names (let ((*package* (load-time-value (find-package "COM.INFORMATIMAGO.TOOLS.SYMBOL.EMPTY"))))
                    (mapcar (function prin1-to-string) syms)))
           (width (reduce (function max) names :key (function length)
                                               :initial-value 0))
           (colon (reduce (function max) names :key (lambda (nam)
                                                      (or (position #\: nam) 0))
                                               :initial-value 0))
           (after (reduce (function max) names :key (lambda (nam)
                                                      (- (length nam)
                                                         (or (position #\: nam) 0)))
                                               :initial-value 0))
           (*print-right-margin* (or *print-right-margin* 72)))
      (loop :for sym :in syms
            :for nam :in names
            :for col :=  (or (position #\: nam) 0)
            :for pre :=  (- colon col)
            :do (format t "~V,,,' <~>~A~V,,,' <~> ~:[ ~;P~]~:[ ~;V~]~A ~A~%"
                        pre
                        nam
                        (- after (- (length nam) col))
                        (accessiblep sym)
                        (boundp sym)
                        (cond ((not (fboundp sym))      " ")
                              ((special-operator-p sym) "S")
                              ((macro-function sym)     "M")
                              (t                        "F"))
                        (if (boundp sym)
                            (let ((*print-length* 8)
                                  (*print-level*  3))
                              (let ((val (prin1-to-string (symbol-value sym))))
                                (cond ((minusp (- *print-right-margin* 5 width))
                                       "")
                                      ((< (- *print-right-margin* 5 width)
                                          (length val))
                                       (subseq val 0 (- *print-right-margin* 5 width)))
                                   (t
                                     val))))
                            "")))))
  (values))


;;;; THE END ;;;;
