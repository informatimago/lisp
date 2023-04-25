(asdf:defsystem "make-print-vars"
  ;; system attributes:
  :description "Generate Makefile print-vars rule"
  :long-description "

This program reads a makefile and extract all variable name from
variable definitions and generates a rule to print their value.

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.0.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2023")
               ((#:albert #:output-dir)          . "../documentation/make-print-vars/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on (:com.informatimago.common-lisp.cesarum
               :com.informatimago.common-lisp.tools.make)
  :components ((:file "make-print-vars" :depends-on ()))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)


