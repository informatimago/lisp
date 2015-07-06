;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               expression-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests expressions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-06 <PJB> Extracted from expression.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.CPP")

(define-test test/integer-value ()
  (check equal (mapcar (function integer-value)
                       (list (make-number "42"          0 0 "-")
                             (make-number "0x42"        0 0 "-")
                             (make-number "0b101010"    0 0 "-")
                             (make-number "042"         0 0 "-")))
         '(42 66 42 34))
  :success)

(define-test test/character-value ()
  (check equal (mapcar (function character-value)
                       (list (make-character-literal "'A'"      0 0 "-")
                             (make-character-literal "'\\x41'"  0 0 "-")
                             (make-character-literal "'\\n'"    0 0 "-")
                             (make-character-literal "'λ'"      0 0 "-")))
         
         '(65 65 10 955))
  :success)

;;;; THE END ;;;;
