;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.cesarum-test.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to test the com.informatimago.common-lisp.cesarum library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-08 <PJB> Created this .asd file.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+clisp
  (unless custom:*ansi*
    (warn "System ~A: clisp should be used with -ansi or (setf custom:*ansi* t) in ~/.clisprc"
          :com.informatimago.common-lisp.cesarum.test)))


(asdf:defsystem "com.informatimago.common-lisp.cesarum.test"
  ;; system attributes:
  :description  "Tests the cesarum library."
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.3.3"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2015")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.cesarum-test/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("com.informatimago.common-lisp.cesarum")
  :components ((:file "a-star-test"         :depends-on ())
               (:file "cache-test"          :depends-on ())
               (:file "date-test"           :depends-on ())
               (:file "dictionary-test"     :depends-on ())
               (:file "file-test"           :depends-on ())
               (:file "list-test"           :depends-on ())
               (:file "llrbtree-test"       :depends-on ())
               (:file "peek-stream-test"    :depends-on ())
               (:file "priority-queue-test" :depends-on ())
               (:file "sequence-test"       :depends-on ())
               (:file "ascii-test"          :depends-on ())
               (:file "string-test"         :depends-on ())
               (:file "set-test"            :depends-on ())
               (:file "index-set-test"      :depends-on ("set-test")))
  :perform (asdf:test-op
            (operation system)
            (dolist (p '("COM.INFORMATIMAGO.COMMON-LISP.CESARUM.A-STAR.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CACHE.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DATE.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DICTIONARY.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LLRBTREE.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PRIORITY-QUEUE.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET.TEST"
                         "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII.TEST"))
              (let ((*package* (find-package p)))
                (uiop:symbol-call p "TEST/ALL")))))


;;;; THE END ;;;;
