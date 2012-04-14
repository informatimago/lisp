;;;; -*- mode:lisp -*-

(asdf:defsystem :com.informatimago.rdp.basic.example
    :name "An example of parser generated in BASIC with the Recursive Descent Parser Generator."
    :description "An example of parser generated in BASIC with the Recursive Descent Parser Generator."
    :author "<PJB> Pascal Bourguignon <pjb@informatimago.com>"
    :version "0.1"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Summer 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.rdp.basic.example/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on ("com.informatimago.rdp"
                 "com.informatimago.rdp.basic")
    :components ((:file "example-basic")))

;;;; THE END ;;;;
