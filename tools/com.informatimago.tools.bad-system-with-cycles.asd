;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bad-system-with-cycles.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Check asd file for circular dependencies.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-03-25 <PJB> Created.
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

#+testing
(asdf:defsystem "com.informatimago.tools.bad-system-with-cycles"
    :description "A bad system with cycles, to test check-asdf"
    :author "Pascal J. Bourguignon"
    :version "1.0.3"
    :license "GPL3"
    :depends-on ("com.informatimago.common-lisp.cesarum"
                 "com.informatimago.clext") 
    :components ((:file "a")
                 (:file "b" :depends-on ("a" "c"))
                 (:file "c" :depends-on ("b" "a"))
                 (:file "d" :depends-on ("e"))
                 (:file "e" :depends-on ("f"))
                 (:file "f" :depends-on ("d"))))

;;;; THE END ;;;;
