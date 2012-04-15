;;;; -*- mode:lisp -*-

(asdf:defsystem :com.informatimago.cocoa-playground
    :name "A playground for Cocoa and ObjCL."
    :description  "This system loads a test program."
    :author "<PJB> Pascal Bourguignon <pjb@informatimago.com>"
    :version "0.7"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.cocoa-playground/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on (:com.informatimago.objcl)
    :components ((:file "layout"             :depends-on ())
                 (:file "cocoa"              :depends-on ("layout"))))

;;;; THE END ;;;;
