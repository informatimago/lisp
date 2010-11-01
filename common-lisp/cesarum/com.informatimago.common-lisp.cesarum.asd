;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.cesarum.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.common-lisp.cesarum library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-10-31 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(asdf:defsystem :com.informatimago.common-lisp.cesarum
    :name "com.informatimago.common-lisp.cesarum"
    :description  "This ASDF system gathers various Common Lisp library packages."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.2.0"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Automn 2010")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.common-lisp.cesarum/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on (:com.informatimago.common-lisp.lisp-sexp)
    :components (
                 ;; Common Lisp addendum:
                 (:file "utility"         :depends-on ())
                 (:file "array"           :depends-on ())
                 (:file "list"            :depends-on ())
                 (:file "string"          :depends-on ("utility" "list" "ecma048"))
                 (:file "package"         :depends-on ())
                 
                 ;; Data structures:
                 (:file "bset"            :depends-on ("utility"))
                 (:file "brelation"       :depends-on ("utility" "bset"))
                 (:file "dictionary"      :depends-on ())
                 (:file "dll"             :depends-on ())
                 (:file "graph"           :depends-on ())
                 (:file "llrbtree"        :depends-on ())
                 (:file "queue"           :depends-on ("utility"))
                 (:file "message-queue"   :depends-on ("queue"))

                 ;; Standards:
                 (:file "ascii"           :depends-on ())
                 (:file "character-sets"  :depends-on ("string"))
                 (:file "ecma048"         :depends-on ("utility"))
                 (:file "iso3166"         :depends-on ())
                 (:file "iso4217"         :depends-on ())
                 (:file "iso639a"         :depends-on ())

                 ;; Algorithms:
                 (:file "pmatch"          :depends-on ("utility"))
                 (:file "combination"     :depends-on ())
                 (:file "raiden"          :depends-on ())
                 (:file "tea"             :depends-on ())

                 ;; Specific stuff:
                 (:file "activity"        :depends-on ())
                 (:file "date"            :depends-on ())
                 (:file "version"         :depends-on ())

                 ))


;; Would require a separate asd file...
;; Tools:
;; (:file "update-iso3166"       :depends-on ())
;; (:file "iana-character-sets"  :depends-on ())

;;;; THE END ;;;;
