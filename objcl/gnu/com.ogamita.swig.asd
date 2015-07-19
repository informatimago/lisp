;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#+(or (and ccl darwin) (and clisp macos))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (pushnew :macosx *features*))

(asdf:defsystem "com.ogamita.swig"
    :description  "This systems gathers CFFI packages generated by Swig."
    :author "Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "0.0.6"
    :licence "AGPL3"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.ogamita.swig/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on ("cffi")
    :components (#-(and)  (:file "packages")
                 #-(and)  (:file "xcb"              :depends-on ("packages"))
                 #+macosx (:file "coregraphics"     :depends-on ("packages"))
                 #-(and)  (:file "packages-exports" :depends-on (#-(and)  "xcb"
                                                                   #+macosx "coregraphics"))
                 #-(and #|not yet|#) (:file "gnu")
                 #-(and #|not yet|#) (:file "objc" :depends-on ("gnu"))))

;;;; THE END ;;;;
