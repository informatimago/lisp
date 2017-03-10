;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.pgl.examples.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    ASD file to load all the examples for com.informatimago.pgl library.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-03-10 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2017 - 2017
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


(asdf:defsystem "com.informatimago.pgl.examples"
  ;; system attributes:
  :description "Loads the examples for Portable Graphics Library (Stanford Portable Library)."
  :long-description "

This system loads all the pgl examples.
They can be run with:

    (com.informatimago.pgl.examples:run-all)

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.0.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2017")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("com.informatimago.pgl")
  :components (

               (:file "examples/angles" :depends-on ())
               (:file "examples/ball" :depends-on ())
               (:file "examples/checkerboard" :depends-on ())
               (:file "examples/felt-board" :depends-on ())
               (:file "examples/image" :depends-on ())
               (:file "examples/yarn-pattern" :depends-on ())
               (:file "examples/yin-yang" :depends-on ())


               (:file "examples/all" :depends-on ("examples/angles"
                                                  "examples/ball"
                                                  "examples/checkerboard"
                                                  "examples/felt-board"
                                                  "examples/image"
                                                  "examples/yarn-pattern"
                                                  "examples/yin-yang")))

  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op (asdf:test-op "com.informatimago.pgl.examples.test")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)

;;;; THE END ;;;;

