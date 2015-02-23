;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.editor.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Editor tools.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-12-23 <PJB> Added system dependencies.
;;;;    2013-12-06 <PJB> Extracted from ~/rc/common.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2015
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

(asdf:defsystem :com.informatimago.editor
  :description "An emacs-like editor."
  :author "Pascal J. Bourguignon"
  :version "1.0.4"
  :license "AGPL3"
  :depends-on ("com.informatimago.common-lisp.cesarum"
               "com.informatimago.common-lisp.lisp-sexp"
               "split-sequence"
               "cl-charms") 
  :components ((:file "package")
               (:file "macros"        :depends-on ("package"))
               (:file "screen"        :depends-on ("package"
                                                   "macros"))
               #+clisp (:file "clisp-screen"  :depends-on ("package"
                                                           "macros" "screen"))
               #+clisp (:file "clisp"         :depends-on ("package"
                                                           "macros" "screen"
                                                           "clisp-screen"))
               (:file "charms-screen" :depends-on ("package"
                                                   "macros" "screen"))
               (:file "editor"        :depends-on ("package"
                                                   "macros" "screen"
                                                   #+clisp "clisp"
                                                   "charms-screen"))))

;;;; THE END ;;;;
