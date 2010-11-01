;; -*- mode:lisp; coding:utf-8 -*-

(ASDF:DEFSYSTEM :COM.INFORMATIMAGO.COMMON-LISP
    :DESCRIPTION  "This ASDF system gathers all the COM.INFORMATIMAGO.COMMON-LISP systems."
    :VERSION "1.2.0"
    :AUTHOR "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :LICENCE "GPL"
    :DEPENDS-ON (:com.informatimago.common-lisp.lisp-sexp
                 :com.informatimago.common-lisp.lisp-reader
                 :com.informatimago.common-lisp.lisp-text
                 
                 ;; not yet  :com.informatimago.common-lisp.lisp

                 :com.informatimago.common-lisp.cesarum
                 :com.informatimago.common-lisp.file
                 :com.informatimago.common-lisp.picture
                 :com.informatimago.common-lisp.arithmetic
                 :com.informatimago.common-lisp.data-encoding
                 :com.informatimago.common-lisp.heap
                 
                 :com.informatimago.common-lisp.html-base
                 :com.informatimago.common-lisp.html-generator
                 :com.informatimago.common-lisp.html-parser
                 :com.informatimago.common-lisp.http

                 :com.informatimago.common-lisp.bank
                 :com.informatimago.common-lisp.csv
                 :com.informatimago.common-lisp.cxx
                 :com.informatimago.common-lisp.diagram
                 :com.informatimago.common-lisp.regexp
                 :com.informatimago.common-lisp.ed
                 :com.informatimago.common-lisp.graphviz
                 :com.informatimago.common-lisp.invoice
                 :com.informatimago.common-lisp.interactive
                 :com.informatimago.common-lisp.parser
                 :com.informatimago.common-lisp.rfc2822
                 :com.informatimago.common-lisp.rfc3548
                 :com.informatimago.common-lisp.unix
                 
                 :com.informatimago.common-lisp.make-depends))

;;;; THE END ;;;;

