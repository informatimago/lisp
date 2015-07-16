;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.tools.try-systems.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Symbol tools.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2015
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

#+mocl
(asdf:defsystem "com.informatimago.tools.try-systems"
  :description "Tries to compile systems like in quicklisp validation compilations."
  :long-description "

Compiles all the systems found recursively in a directory,
in an environment similar to what quicklisp uses to check systems,
by forking an sbcl instance per system.

"
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "AGPL3"
  :depends-on () 
  :components ())

#-mocl
(asdf:defsystem "com.informatimago.tools.try-systems"
  :description "Tries to compile systems like in quicklisp validation compilations."
  :long-description "

Compiles all the systems found recursively in a directory,
in an environment similar to what quicklisp uses to check systems,
by forking an sbcl instance per system.

"
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "AGPL3"
  :depends-on ("com.informatimago.common-lisp.cesarum"
               "com.informatimago.tools.source"
               "com.informatimago.tools.script"
               "split-sequence") 
  :components ((:file "try-systems")))


;;;; THE END ;;;;
