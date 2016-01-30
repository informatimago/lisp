;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package-symbols.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A few package functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-09 <PJB> Created.
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


;; No in-package.

(defun package-internal-symbols (pack)
  (let ((result '()))
    (do-symbols (s pack)
      (when (eq (symbol-package s) pack)
        (push s result)))
    (sort result (function string<)
          :key (function symbol-name))))

(defun package-external-symbols (pack)
  (let ((result '()))
    (do-external-symbols (s pack)
      (push s result))
    (sort result (function string<)
          :key (function symbol-name))))

(defun package-imported-symbols (pack)
  (let ((result '()))
    (do-symbols (s pack)
      (unless (eq (symbol-package s) pack)
        (push s result)))
    (sort result (function string<)
          :key (function symbol-name))))

(defmacro packages-created-by (&body body)
  (let ((vold (gensym)))
    `(let ((,vold (copy-list (list-all-packages))))
       (progn ,@body)
       (set-difference  (list-all-packages) ,vold))))

;;;; THE END ;;;;

