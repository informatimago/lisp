;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.clisp.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.clisp library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-10-31 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2014
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

#+clisp (when (find-package "LINUX") (pushnew :linux *features*))

(asdf:defsystem :com.informatimago.clisp

  ;; system attributes:

  :description "Clisp specific packages."

  :long-description "

Various packages using clisp specific features (some of them could or
should be made into implementation independant packages).

"

  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"

  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  
  
  :licence "AGPL3"
  
  ;; component attributes:

  :name "Informatimago Common Lisp Clisp Specific Packages"

  :version "1.2.2"

  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Spring 2014")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.clmisc/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))

  #+asdf-unicode :encoding #+asdf-unicode :utf-8

  :depends-on ("com.informatimago.common-lisp.cesarum")

  :components
  #+clisp ((:file "syslog")
           (:file "disk")
           ;; (:file "objc")
           (:file "string")
           (:file "fifo-stream")
           (:file "iotask")
           (:file "rfc1413" :depends-on ("iotask"))
           ;; :shared-object is not known of asdf, but is defined by clg-tools…
           ;; (:shared-object "libraw-memory"
           ;;                 :components ((:c-source-file "raw-memory-lib"))
           ;;                 :depends-on ())
           ;; (:file "raw-memory" :depends-on ("libraw-memory"))
           #+linux (:file "susv3")
           #+linux (:file "susv3-mc3" :depends-on ("susv3"))
           #+linux (:file "susv3-xsi" :depends-on ("susv3"))
           #+linux (:file "script" :depends-on ("string"))
           #+linux (:file "shell")
           #+linux (:file "xterm" :depends-on ("susv3"))
           #+linux (:file "make-volumes" :depends-on ("susv3")))
  #-clisp ()) 

#+(and clisp (not linux)) (warn "System ~A is incomplete without the LINUX package." :com.informatimago.clisp) 
#-clisp (warn "System ~A is useless on ~A" :com.informatimago.clisp (lisp-implementation-type))

;;;; THE END ;;;;
