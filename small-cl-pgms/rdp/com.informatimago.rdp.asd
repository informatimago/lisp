;;;; -*- mode:lisp -*-

(asdf:defsystem :com.informatimago.rdp
    :name "Recursive Descent Parser Generator"
    :description   "This package defines a Recursive Descent Parser generator.
The client may define methods to generate the code of the parser in
different languages than lisp."
    :author "<PJB> Pascal Bourguignon <pjb@informatimago.com>"
    :version "0.1"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Summer 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.rdp/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on ("cl-ppcre")
    :components ((:file "rdp")))

;;;; THE END ;;;;
