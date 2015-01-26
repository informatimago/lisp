;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-all-systems.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A script loading all my systems verbosely, to check they all compile.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal. Bourguignon <pascal.bourguignon@ubudu.com>
;;;;MODIFICATIONS
;;;;    2013-06-20 <PJB> Recently created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal. Bourguignon 2013 - 2015
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
;;;;    
(ql:register-local-projects)

(defparameter *systems*
  '(
    :com.informatimago.xcode
    :com.informatimago.rdp.example
    :com.informatimago.rdp.basic.example
    :com.informatimago.rdp.basic
    :com.informatimago.rdp
    :com.informatimago.lispdoc
    :com.informatimago.linc
    :com.informatimago.languages.lua
    :com.informatimago.languages.cxx
    :com.informatimago.common-lisp.unix
    :com.informatimago.common-lisp.tools.make-depends
    :com.informatimago.common-lisp.telnet
    :com.informatimago.common-lisp.rfc3548
    :com.informatimago.common-lisp.rfc2822
    :com.informatimago.common-lisp.regexp
    :com.informatimago.common-lisp.picture
    :com.informatimago.common-lisp.parser
    :com.informatimago.common-lisp.lisp.stepper
    :com.informatimago.common-lisp.lisp.ibcl
    :com.informatimago.common-lisp.lisp-text
    :com.informatimago.common-lisp.lisp-sexp
    :com.informatimago.common-lisp.lisp-reader
    :com.informatimago.common-lisp.lisp
    :com.informatimago.common-lisp.invoice
    :com.informatimago.common-lisp.interactive
    :com.informatimago.common-lisp.http
    :com.informatimago.common-lisp.html-parser
    :com.informatimago.common-lisp.html-generator
    :com.informatimago.common-lisp.html-base
    :com.informatimago.common-lisp.heap
    :com.informatimago.common-lisp.graphviz
    :com.informatimago.common-lisp.ed
    :com.informatimago.common-lisp.diagram
    :com.informatimago.common-lisp.data-encoding
    :com.informatimago.common-lisp.csv
    :com.informatimago.common-lisp.cesarum
    :com.informatimago.common-lisp.bank
    :com.informatimago.common-lisp.arithmetic
    :com.informatimago.common-lisp.apple-file
    :com.informatimago.common-lisp
    :com.informatimago.clmisc
    #-sbcl               :com.informatimago.clext
    #+clisp              :com.informatimago.susv3
    #+clisp              :com.informatimago.clisp
    #+(and ccl darwin)   :com.informatimago.objcl            ; macosx even.
    #+(and ccl darwin)   :com.informatimago.cocoa-playground ; macosx even.
    ))

(dolist (sys *systems*)
  (handler-case
   (ql:quickload sys :verbose t)
    (error (err)
      (format t "~2%Error while loading system ~A~%~A~%" sys err))))

