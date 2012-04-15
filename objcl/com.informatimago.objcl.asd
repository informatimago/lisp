;;;; -*- mode:lisp -*-

(asdf:defsystem :com.informatimago.objcl
    :name "Reader macros to implement an Objective-CL syntax."
    :description  "This system defines a package exporting reader macros and tools to program with Objective-C object libraries."
    :author "<PJB> Pascal Bourguignon <pjb@informatimago.com>"
    :version "0.9.0"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.objc/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on ()
    :components ((:file "packages")
                 (:file "simple-test"        :depends-on ("packages"))
                 (:file "mac-roman"          :depends-on ("packages"))
                 #+ccl (:file "oclo-ccl"     :depends-on ("packages"))
                 (:file "oclo"               :depends-on ("packages" #+ccl "oclo-ccl"))
                 (:file "objcl"              :depends-on ("packages" "oclo"))
                 (:file "test-objcl"         :depends-on ("packages" "objcl" "simple-test"))))

;;;; THE END ;;;;
