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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2012
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

(asdf:defsystem :com.informatimago.common-lisp.cesarum

    ;; system attributes:
    
    :description  "Various general data types, algorithms, utilities and standards."

    :long-description "

This system provides various kinds of packages:

- Common Lisp addendums (utility, array, sequence, list, string,
  package, file, stream);

- Data structures (sets, relations, dictionaries, lists, queues,
  trees, graphs);

- Standards (ascii, ecma048, iso3166, iso4217, iso693a);

- Algorithms (pattern matcher, combinations, constraints, dfa, etc);

all written in 100% conforming Common Lisp.
"
    
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
    
    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    
    :licence "AGPL3"

    ;; component attributes:
    
    :name "com.informatimago.common-lisp.cesarum"

    :version "1.3.2"

    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Autumn 2010")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.cesarum/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    
    :depends-on (:com.informatimago.common-lisp.lisp-sexp)
    
    :components (
                 ;; Common Lisp addendum:
                 (:file "utility"         :depends-on ())
                 (:file "array"           :depends-on ())
                 (:file "sequence"        :depends-on ())
                 (:file "list"            :depends-on ())
                 (:file "string"          :depends-on ("utility" "list" "ecma048"))
                 (:file "package"         :depends-on ())
                 
                 ;; Data structures:
                 (:file "bset"            :depends-on ("utility"))
                 (:file "brelation"       :depends-on ("utility" "bset"))
                 (:file "dictionary"      :depends-on ())
                 (:file "dll"             :depends-on ())
                 (:file "graph"           :depends-on ("utility" "list"))
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
                 (:file "constraints"     :depends-on ("utility" "dictionary"))
                 (:file "raiden"          :depends-on ())
                 (:file "dfa"             :depends-on ())
                 (:file "tea"             :depends-on ())

                 ;; Specific stuff:
                 (:file "activity"        :depends-on ())
                 (:file "date"            :depends-on ())
                 (:file "version"         :depends-on ())

                 ;; Files:
                 (:file "stream"          :depends-on ("string"))
                 (:file "file"            :depends-on ("stream" "ascii"))
                 (:file "peek-stream"     :depends-on ())
                 (:file "cache"           :depends-on ())
                 (:file "float-binio"     :depends-on ())
                 ))


;; Would require a separate asd file...
;; Tools:
;; (:file "update-iso3166"       :depends-on ())
;; (:file "iana-character-sets"  :depends-on ())

;;;; THE END ;;;;
