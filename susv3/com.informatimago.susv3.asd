;; -*- mode:lisp -*-

(ASDF:DEFSYSTEM :COM.INFORMATIMAGO.SUSV3 :DESCRIPTION
 "This ASDF system gathers all the COM.INFORMATIMAGO.SUSV3 packages." :VERSION
 "1.0.0" :AUTHOR "<PJB> Pascal Bourguignon <pjb@informatimago.com>" :LICENCE
 "GPL" :DEPENDS-ON (:COM.INFORMATIMAGO.COMMON-LISP :COM.INFORMATIMAGO.CLISP)
 :COMPONENTS
 ((:CL-SOURCE-FILE "tools") (:CL-SOURCE-FILE "dirent" :DEPENDS-ON ("tools"))
  (:CL-SOURCE-FILE "ipc" :DEPENDS-ON ("tools"))
  (:CL-SOURCE-FILE "process" :DEPENDS-ON ("ipc")))) 
