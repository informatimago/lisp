;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.small-cl-pgms.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This system loads the various small-cl-pgms systems.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-12-23 <PJB> Added asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2015
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

(asdf:defsystem :com.informatimago.small-cl-pgms
  :name "com.informatimago.small-cl-pgms"
  :description "Small Common Lisp Programs"
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "GPL3"
  :depends-on ("com.informatimago.small-cl-pgms.brainfuck"
               "com.informatimago.small-cl-pgms.what-implementation"
               "com.informatimago.small-cl-pgms.life"
               "com.informatimago.small-cl-pgms.quine") 
  :components ()
  #+not-yet ((:file "1024")
             (:file "author-signature")
             (:file "cube")
             (:file "douze")
             (:file "example-soft-opcodes")
             (:file "index")
             (:file "init")
             (:file "minlisp")
             (:file "moon")
             (:file "puzzle")
             (:file "solitaire")
             (:file "toy-byte-code")
             (:file "wang-cl")))

;;;; THE END ;;;;
