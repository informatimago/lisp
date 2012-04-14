;;;; -*- mode:lisp -*-

(asdf:defsystem :com.informatimago.rdp.basic
    :name "Recursive Descent Parser Generator - BASIC generator."
    :description   "This package defines methods to generate the parsers in BASIC."
    :author "<PJB> Pascal Bourguignon <pjb@informatimago.com>"
    :version "0.1"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Summer 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.rdp.basic/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on ("com.informatimago.rdp")
    :components ((:file "rdp-basic-gen")))

;;;; THE END ;;;;
