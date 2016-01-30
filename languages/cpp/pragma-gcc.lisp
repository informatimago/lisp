;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pragma-gcc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the #pragma GCC interpreter function.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-06-30 <PJB> Created.
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

;; #pragma GCC dependency path &rest and-stuff
;; #pragma GCC poison &rest identifiers
;; #pragma GCC system_header
;; #pragma GCC warning message
;; #pragma GCC error message
;; // message must be a single string literal.


(defun check-dependency (context tokens)
  (declare (ignore context tokens))
  (values))

(defun pragma-gcc (context tokens)
  ;; silently ignores unknown pragmas.
  (when (and tokens (identifierp (first tokens)))
    (let ((pragmas (context-pragmas context)))
      (scase (token-text (first tokens))
        (("dependency")
         (check-dependency context (rest tokens)))
        (("poison")
         (dolist (token (rest tokens))
           (if (identifierp token)
               (pushnew (token-text token)  (gethash '(:gcc :poison) pragmas '())
                        :test (function string=))
               (cpp-error token "Invalid identifier in #pragma GCC poison: ~S" (token-text token)))))
        (("system_header")
         (setf (gethash '(:gcc :system-header) pragmas) t))
        (("warning")
         (cpp-warning (first tokens) "~{~A~^ ~}" (mapcar (function token-text) (rest tokens))))
        (("error")
         (cpp-error   (first tokens) "~{~A~^ ~}" (mapcar (function token-text) (rest tokens))))))))


(setf (gethash "GCC" *default-pragma-interpreters*) 'pragma-gcc)

;;;; THE END ;;;;
