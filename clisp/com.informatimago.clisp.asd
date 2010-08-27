;; -*- mode:lisp -*-

(ASDF:DEFSYSTEM :COM.INFORMATIMAGO.CLISP :DESCRIPTION
 "This ASDF system gathers all the COM.INFORMATIMAGO.CLISP packages." :VERSION
 "1.0.0" :AUTHOR
 "<PJB> Pascal Bourguignon <pjb@informatimago.com> and <PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
 :LICENCE "GPL" :DEPENDS-ON (:COM.INFORMATIMAGO.COMMON-LISP) :COMPONENTS
 ((:CL-SOURCE-FILE "syslog") (:CL-SOURCE-FILE "disk") (:CL-SOURCE-FILE "objc")
  (:CL-SOURCE-FILE "string") (:CL-SOURCE-FILE "fifo-stream")
  (:CL-SOURCE-FILE "iotask") (:CL-SOURCE-FILE "rfc1413" :DEPENDS-ON ("iotask"))
  (:CL-SOURCE-FILE "raw-memory") (:CL-SOURCE-FILE "susv3")
  (:CL-SOURCE-FILE "susv3-mc3" :DEPENDS-ON ("susv3"))
  (:CL-SOURCE-FILE "susv3-xsi" :DEPENDS-ON ("susv3"))
  (:CL-SOURCE-FILE "script" :DEPENDS-ON ("string")) (:CL-SOURCE-FILE "shell")
  (:CL-SOURCE-FILE "xterm" :DEPENDS-ON ("susv3"))
  (:CL-SOURCE-FILE "make-volumes" :DEPENDS-ON ("susv3")))) 
