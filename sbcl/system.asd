;; -*- mode:lisp -*-

(ASDF:DEFSYSTEM :COM.INFORMATIMAGO.SBCL :DESCRIPTION
                "This ASDF system gathers all the COM.INFORMATIMAGO.SBCL packages."
                :VERSION "1.0.88" :AUTHOR "none" :LICENCE "GPL" :DEPENDS-ON
                (:COM.INFORMATIMAGO.COMMON-LISP) :COMPONENTS
                ((:CL-SOURCE-FILE "posix") (:CL-SOURCE-FILE "readline"))) 
