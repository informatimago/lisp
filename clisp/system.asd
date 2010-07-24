;; -*- mode:lisp -*-

(ASDF:DEFSYSTEM :COM.INFORMATIMAGO.CLISP :DESCRIPTION
 "This ASDF system gathers all the COM.INFORMATIMAGO.CLISP packages." :VERSION
 "1.0.0" :AUTHOR
 "<PJB> Pascal J. Bourguignon <pjb@informatimago.com> and <PJB> Pascal Bourguignon <pjb@informatimago.com>"
 :LICENCE "GPL" :DEPENDS-ON (:COM.INFORMATIMAGO.COMMON-LISP) :COMPONENTS
 ((:CL-SOURCE-FILE "syslog") (:CL-SOURCE-FILE "disk") (:CL-SOURCE-FILE "objc")
  (:CL-SOURCE-FILE "string") (:CL-SOURCE-FILE "fifo-stream")
  (:CL-SOURCE-FILE "iotask")
  (:CL-SOURCE-FILE "rfc1413" :DEPENDS-ON ("iotask")))) 
