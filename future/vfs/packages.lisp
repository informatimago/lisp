;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               vfs-packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the vfs packages.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-05-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2011 - 2011
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *redefined-symbols*
    '(  
      ;; 19. Filenames
      "PATHNAME" "LOGICAL-PATHNAME"
      "PATHNAME-HOST" "PATHNAME-DEVICE" "PATHNAME-DIRECTORY"
      "PATHNAME-NAME" "PATHNAME-TYPE" "PATHNAME-VERSION"
      "MAKE-PATHNAME" "PATHNAMEP" "LOGICAL-PATHNAME-TRANSLATIONS"
      "LOAD-LOGICAL-PATHNAME-TRANSLATIONS" "LOGICAL-PATHNAME"
      "*DEFAULT-PATHNAME-DEFAULTS*" "PARSE-NAMESTRING"
      "WILD-PATHNAME-P" "PATHNAME-MATCH-P"
      "TRANSLATE-LOGICAL-PATHNAME" "TRANSLATE-PATHNAME" "MERGE-PATHNAMES"
      "NAMESTRING" "FILE-NAMESTRING" "DIRECTORY-NAMESTRING" "HOST-NAMESTRING" "ENOUGH-NAMESTRING"
      ;; 20. Files
      "DIRECTORY" "PROBE-FILE" "ENSURE-DIRECTORIES-EXIST" "TRUENAME"
      "FILE-AUTHOR" "FILE-WRITE-DATE" "RENAME-FILE" "DELETE-FILE"
      ;; 21. Streams.
      "STREAM" "BROADCAST-STREAM" "CONCATENATED-STREAM" "ECHO-STREAM" "FILE-STREAM"
      "STRING-STREAM" "SYNONYM-STREAM" "TWO-WAY-STREAM"
      "INPUT-STREAM-P" "OUTPUT-STREAM-P" "INTERACTIVE-STREAM-P"
      "OPEN-STREAM-P" "STREAM-ELEMENT-TYPE" "STREAMP" "READ-BYTE"
      "WRITE-BYTE" "PEEK-CHAR" "READ-CHAR" "READ-CHAR-NO-HANG" "TERPRI"
      "FRESH-LINE" "UNREAD-CHAR" "WRITE-CHAR" "READ-LINE" "WRITE-STRING"
      "WRITE-LINE" "READ-SEQUENCE" "WRITE-SEQUENCE" "FILE-LENGTH"
      "FILE-POSITION" "FILE-STRING-LENGTH" "OPEN" "STREAM-EXTERNAL-FORMAT"
      "WITH-OPEN-FILE" "CLOSE" "WITH-OPEN-STREAM" "LISTEN" "CLEAR-INPUT"
      "FINISH-OUTPUT" "FORCE-OUTPUT" "CLEAR-OUTPUT" "Y-OR-N-P" "YES-OR-NO-P"
      "MAKE-SYNONYM-STREAM" "SYNONYM-STREAM-SYMBOL"
      "BROADCAST-STREAM-STREAMS" "MAKE-BROADCAST-STREAM"
      "MAKE-TWO-WAY-STREAM" "TWO-WAY-STREAM-INPUT-STREAM"
      "TWO-WAY-STREAM-OUTPUT-STREAM" "ECHO-STREAM-INPUT-STREAM"
      "ECHO-STREAM-OUTPUT-STREAM" "MAKE-ECHO-STREAM"
      "CONCATENATED-STREAM-STREAMS" "MAKE-CONCATENATED-STREAM"
      "GET-OUTPUT-STREAM-STRING" "MAKE-STRING-INPUT-STREAM"
      "MAKE-STRING-OUTPUT-STREAM" "WITH-INPUT-FROM-STRING"
      "WITH-OUTPUT-TO-STRING" "*DEBUG-IO*" "*ERROR-OUTPUT*" "*QUERY-IO*"
      "*STANDARD-INPUT*" "*STANDARD-OUTPUT*" "*TRACE-OUTPUT*"
      "*TERMINAL-IO*" "STREAM-ERROR-STREAM"
      ;; 3. Evaluation and Compilation
      "TYPE")))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM"
  (:nicknames "VFS" "VIRTUAL-FILE-SYSTEM")
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")
  (:shadow . #.*redefined-symbols*)
  (:export "INSTALL-PATHNAME-READER-MACRO" "RESET-READTABLE"
           "DELETE-DIRECTORY"
           "FILE-ELEMENT-TYPE"
           "PURGE-FILE" "DELETE-VERSION"
           . #.*redefined-symbols*))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM-USER"
  (:nicknames "VFS-USER" "VIRTUAL-FILE-SYSTEM-USER")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")
  (:shadowing-import-from
   "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM"
   . #.*redefined-symbols*))


;;;; THE END ;;;;
