(ql:register-local-projects)

(defparameter *systems*
  '(
    :scexp
    :common-idioms
    :com.informatimago.xcode
    :com.informatimago.rdp.example
    :com.informatimago.rdp.basic.example
    :com.informatimago.rdp.basic
    :com.informatimago.rdp
    :com.informatimago.objcl
    :com.informatimago.lispdoc
    :com.informatimago.linc
    :com.informatimago.languages.lua
    :com.informatimago.languages.cxx
    :com.informatimago.common-lisp.unix
    :com.informatimago.common-lisp.tools.make-depends
    :com.informatimago.common-lisp.telnet
    :com.informatimago.common-lisp.rfc3548
    :com.informatimago.common-lisp.rfc2822
    :com.informatimago.common-lisp.regexp
    :com.informatimago.common-lisp.picture
    :com.informatimago.common-lisp.parser
    :com.informatimago.common-lisp.lisp.stepper
    :com.informatimago.common-lisp.lisp.ibcl
    :com.informatimago.common-lisp.lisp-text
    :com.informatimago.common-lisp.lisp-sexp
    :com.informatimago.common-lisp.lisp-reader
    :com.informatimago.common-lisp.lisp
    :com.informatimago.common-lisp.invoice
    :com.informatimago.common-lisp.interactive
    :com.informatimago.common-lisp.http
    :com.informatimago.common-lisp.html-parser
    :com.informatimago.common-lisp.html-generator
    :com.informatimago.common-lisp.html-base
    :com.informatimago.common-lisp.heap
    :com.informatimago.common-lisp.graphviz
    :com.informatimago.common-lisp.ed
    :com.informatimago.common-lisp.diagram
    :com.informatimago.common-lisp.data-encoding
    :com.informatimago.common-lisp.csv
    :com.informatimago.common-lisp.cesarum
    :com.informatimago.common-lisp.bank
    :com.informatimago.common-lisp.arithmetic
    :com.informatimago.common-lisp.apple-file
    :com.informatimago.common-lisp
    :com.informatimago.cocoa-playground
    :com.informatimago.clmisc
    #-sbcl :com.informatimago.clext
    #+clisp :com.informatimago.susv3
    #+clisp :com.informatimago.clisp
    ))

(dolist (sys *systems*)
  (ql:quickload sys))

