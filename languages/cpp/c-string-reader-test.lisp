;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               c-string-reader-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests c-string-reader.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-06 <PJB> Extracted from c-string-reader.lisp
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.CPP")

(define-test test/read-c-string ()
 (let ((*readtable*
        (let ((rt (copy-readtable nil)))
          (set-macro-character #\" 'read-c-string nil rt)
          (set-macro-character #\' 'read-c-string nil rt)
          rt)))
   (check equal (read-from-string "(\"Hello, bell=\\a, backspace=\\b, page=\\f, newline=\\n, return=\\r, tab=\\t, vt=\\v, \\
\\\"double-quotes\\\", \\'single-quotes\\', question\\?, backslash=\\\\, \\
hexa=\\x3BB, octal=\\101, \\7\\77\\107\\3071\" 'a' '\\xe9' '\\\\' '\\'' '\\n')")
           '("Hello, bell=, backspace=, page=, newline=
, return=, tab=	, vt=, \"double-quotes\", 'single-quotes', question?, backslash=\\, hexa=λ, octal=A, ?GÇ1"
             "a" "é" "\\" "'" "
"))
   :success))

;;;; THE END ;;;;
