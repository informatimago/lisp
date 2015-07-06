;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.tools.source.asd
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
(asdf:defsystem "com.informatimago.tools.source"
  :description "Dummy Reads sources and headers to perform some analysis."
  :long-description "

This system would use closer-mop which is not available on MOCL.

"
  :author "Pascal J. Bourguignon"
  :version "1.0.2"
  :license "GPL3"
  :depends-on () 
  :components ())

#-mocl
(asdf:defsystem "com.informatimago.tools.source"
  :description "Reads sources and headers to perform some analysis."
  :author "Pascal J. Bourguignon"
  :version "1.0.3"
  :license "GPL3"
  :depends-on ("com.informatimago.common-lisp.cesarum"
               "com.informatimago.common-lisp.picture"
               "com.informatimago.common-lisp.graphviz" ; used by dependency-cycles
               "com.informatimago.clext"
               "closer-mop"
               "split-sequence") 
  :components ((:file "source")
               (:file "dependency-cycles")
               (:file "asdf-file"  :depends-on ("dependency-cycles" "source"))
               #-(and) (:file "analyse-patchwork")))


;;;; THE END ;;;;
