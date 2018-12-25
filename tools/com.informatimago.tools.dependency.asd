(asdf:defsystem "com.informatimago.tools.dependency"
  :description "Reads sources and headers to perform some analysis."
  :author "Pascal J. Bourguignon"
  :version "1.3.0"
  :license "AGPL3"
  :depends-on ("com.informatimago.common-lisp.cesarum"
               "com.informatimago.common-lisp.picture"
               "com.informatimago.common-lisp.graphviz" ; used by dependency-cycles
               "com.informatimago.clext"
               "closer-mop"
               "split-sequence")
  :components ((:file "dependency-cycles"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)
