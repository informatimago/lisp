;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.tools.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.tools libraries.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-08 <PJB> Created this .asd file.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(asdf:defsystem "com.informatimago.tools"
  ;; system attributes:
  :description  "This system aggregates Informatimago Common Lisp Tools."
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.0.8"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Spring 2014")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.tools/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on #-mocl ("com.informatimago.tools.check-asdf"
                      "com.informatimago.tools.make-depends"
                      "com.informatimago.tools.manifest"
                      "com.informatimago.tools.pathname"
                      "com.informatimago.tools.quicklisp"
                      "com.informatimago.tools.reader-macro"
                      "com.informatimago.tools.script"
                      "com.informatimago.tools.source"
                      "com.informatimago.tools.summary"
                      "com.informatimago.tools.symbol"
                      "com.informatimago.tools.thread"
                      "com.informatimago.tools.try-systems"
                      "com.informatimago.tools.undefmethod")


  #+mocl ("com.informatimago.tools.manifest"
          "com.informatimago.tools.pathname"
          "com.informatimago.tools.symbol")
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op (asdf:test-op  "com.informatimago.tools.test"))))



;; (com.informatimago.tools.check-asdf:check-asdf-system-file
;;  #P"/some/system.asd"  :report output-stream)
;; 
;; (com.informatimago.common-lisp.tools.make-depends:generate-asd
;;  system-name sources source-type
;;  &key description (version "0.0.0")
;;  author licence license
;;  (predefined-packages '("COMMON-LISP"))
;;  (implicit-dependencies '())
;;  (depends-on '())
;;  (load-paths (list (make-pathname :directory '(:relative))))
;;  (vanillap nil))
;; 
;; (com.informatimago.common-lisp.tools.make-depends:generate-summary
;;  sources &key (summary-path #p"SUMMARY.HTML")
;;  (character-set "US-ASCII")
;;  (source-type "LISP")
;;  (verbose nil) (repository-url nil))
;; 
;; (com.informatimago.common-lisp.tools.make-depends:make-depends
;;  object-files packages translations load-paths
;;  &key (idf nil) (verbose nil))
;; 
;; (com.informatimago.tools.manifest:distribution)
;; (com.informatimago.tools.manifest:print-manifest system)
;; (com.informatimago.tools.manifest:write-manifest-file program-name system)
;; 
;; (com.informatimago.tools.pathname:make-pathname …)
;; (com.informatimago.tools.pathname:user-homedir-pathname)
;; (com.informatimago.tools.pathname:translate-logical-pathname pathname)
;; 
;; (com.informatimago.tools.quicklisp:quick-*)

#-(and)
(defun use-tools ()
  (use-package :com.informatimago.tools.check-asdf)
  (use-package :com.informatimago.common-lisp.tools.make-depends)
  (use-package :com.informatimago.tools.manifest)
  (shadow '(make-pathname user-homedir-pathname translate-logical-pathname))
  (use-package :com.informatimago.tools.pathname)
  (use-package :com.informatimago.tools.symbol)
  (use-package :com.informatimago.tools.quicklisp)
  (values))


;;;; THE END ;;;;
