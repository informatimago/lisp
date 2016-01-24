;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.clext.pipe.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.clext.pipe library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-09-12 <PJB> Created this .asd file.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

#+mocl
(asdf:defsystem "com.informatimago.clext.pipe"
  ;; system attributes:
  :description "Dummy Informatimago Common Lisp Extensions: Pipes."
  :long-description "

This system would use TRIVIAL-GRAY-STREAMS, which is not available on MOCL.

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.2.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Summer 2015")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.clext/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ()
  :components ())

#-mocl
(asdf:defsystem "com.informatimago.clext.pipe"
  ;; system attributes:
  :description "Informatimago Common Lisp Extensions: Pipes."
  :long-description "

This system provides PIPES, a pair of input and output stream
with a synchronized queue in the middle.

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.2.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Summer 2015")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.clext/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("trivial-gray-streams"
               "bordeaux-threads"
               "com.informatimago.clext.closer-weak") ; weak hash-tables are needed for gate.
  :components ((:file "gate")
               (:file "pipe" :depends-on ("gate")))
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op (asdf:test-op "com.informatimago.clext.pipe.test")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)


;;;; THE END ;;;;
 
