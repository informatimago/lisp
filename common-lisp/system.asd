;; -*- mode:lisp -*-

(DEFPACKAGE "COM.INFORMATIMAGO.ASDF" (:USE "COMMON-LISP")) 

(IN-PACKAGE "COM.INFORMATIMAGO.ASDF") 

(UNLESS (HANDLER-CASE (FIND-CLASS 'PJB-CL-SOURCE-FILE) (T NIL NIL))
 (DEFCLASS PJB-CL-SOURCE-FILE (ASDF:CL-SOURCE-FILE) NIL)
 (FLET
  ((OUTPUT-FILES (C)
    (FLET
     ((IMPLEMENTATION-ID NIL
       (FLET
        ((FIRST-WORD (TEXT)
          (LET ((POS (POSITION (CHARACTER " ") TEXT)))
           (REMOVE (CHARACTER ".") (IF POS (SUBSEQ TEXT 0 POS) TEXT)))))
        (FORMAT NIL "~A-~A-~A"
         (COND
          ((STRING-EQUAL "International Allegro CL Enterprise Edition"
            (LISP-IMPLEMENTATION-TYPE))
           "ACL")
          (T (FIRST-WORD (LISP-IMPLEMENTATION-TYPE))))
         (FIRST-WORD (LISP-IMPLEMENTATION-VERSION))
         (FIRST-WORD (MACHINE-TYPE))))))
     (LET*
      ((OBJECT (COMPILE-FILE-PATHNAME (ASDF:COMPONENT-PATHNAME C)))
       (PATH
        (MERGE-PATHNAMES
         (MAKE-PATHNAME :DIRECTORY
          (LIST :RELATIVE (FORMAT NIL "OBJ-~:@(~A~)" (IMPLEMENTATION-ID)))
          :NAME (PATHNAME-NAME OBJECT) :TYPE (PATHNAME-TYPE OBJECT))
         OBJECT)))
      (ENSURE-DIRECTORIES-EXIST PATH) (LIST PATH)))))
  (DEFMETHOD ASDF:OUTPUT-FILES
   ((OPERATION ASDF:COMPILE-OP) (C PJB-CL-SOURCE-FILE)) (OUTPUT-FILES C))
  (DEFMETHOD ASDF:OUTPUT-FILES
   ((OPERATION ASDF:LOAD-OP) (C PJB-CL-SOURCE-FILE)) (OUTPUT-FILES C)))) 

(ASDF:DEFSYSTEM :COM.INFORMATIMAGO.COMMON-LISP :DESCRIPTION
 "This ASDF system gathers all the COM.INFORMATIMAGO.COMMON-LISP packages."
 :VERSION "1.1.333" :AUTHOR
 "<PJB> Pascal J. Bourguignon <pjb@informatimago.com> and <PJB> Pascal Bourguignon <pjb@informatimago.com>"
 :LICENCE "GPL" :DEPENDS-ON NIL :COMPONENTS
 ((:PJB-CL-SOURCE-FILE "package")
  (:PJB-CL-SOURCE-FILE "source-form" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "reader" :DEPENDS-ON ("package" "source-form"))
  (:PJB-CL-SOURCE-FILE "source-text" :DEPENDS-ON ("package" "reader"))
  (:PJB-CL-SOURCE-FILE "utility" :DEPENDS-ON ("package" "source-form"))
  (:PJB-CL-SOURCE-FILE "ascii" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "ecma048" :DEPENDS-ON ("package" "utility"))
  (:PJB-CL-SOURCE-FILE "list" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "dll" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "queue" :DEPENDS-ON ("package" "utility"))
  (:PJB-CL-SOURCE-FILE "array" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "string" :DEPENDS-ON
   ("package" "ecma048" "list" "utility"))
  (:PJB-CL-SOURCE-FILE "stream" :DEPENDS-ON ("package" "string"))
  (:PJB-CL-SOURCE-FILE "file" :DEPENDS-ON ("package" "ascii" "stream"))
  (:PJB-CL-SOURCE-FILE "peek-stream" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "scanner" :DEPENDS-ON ("package" "ecma048" "utility"))
  (:PJB-CL-SOURCE-FILE "parser" :DEPENDS-ON ("package" "scanner" "utility"))
  (:PJB-CL-SOURCE-FILE "llrbtree" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "dictionary" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "bset" :DEPENDS-ON ("package" "utility"))
  (:PJB-CL-SOURCE-FILE "brelation" :DEPENDS-ON ("package" "bset" "utility"))
  (:PJB-CL-SOURCE-FILE "graf" :DEPENDS-ON ("package" "utility" "list"))
  (:PJB-CL-SOURCE-FILE "graph" :DEPENDS-ON ("package" "list" "utility"))
  (:PJB-CL-SOURCE-FILE "graph-dot" :DEPENDS-ON
   ("package" "graph" "string" "list"))
  (:PJB-CL-SOURCE-FILE "graph-diagram" :DEPENDS-ON ("package" "graph" "list"))
  (:PJB-CL-SOURCE-FILE "combination" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "pmatch" :DEPENDS-ON ("package" "utility"))
  (:PJB-CL-SOURCE-FILE "picture" :DEPENDS-ON ("package" "utility" "string"))
  (:PJB-CL-SOURCE-FILE "memory" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "heap" :DEPENDS-ON ("package" "memory" "utility"))
  (:PJB-CL-SOURCE-FILE "activity" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "message-queue" :DEPENDS-ON ("package" "queue"))
  (:PJB-CL-SOURCE-FILE "float-binio" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "data-encoding" :DEPENDS-ON ("package" "utility"))
  (:PJB-CL-SOURCE-FILE "cons-to-ascii" :DEPENDS-ON
   ("package" "picture" "string"))
  (:PJB-CL-SOURCE-FILE "tree-to-ascii" :DEPENDS-ON ("package" "picture"))
  (:PJB-CL-SOURCE-FILE "tree-to-diagram" :DEPENDS-ON ("package" "list"))
  (:PJB-CL-SOURCE-FILE "regexp-posix" :DEPENDS-ON ("package" "utility"))
  (:PJB-CL-SOURCE-FILE "regexp-emacs" :DEPENDS-ON
   ("package" "tree-to-ascii" "string"))
  (:PJB-CL-SOURCE-FILE "rfc2822" :DEPENDS-ON ("package" "ecma048"))
  (:PJB-CL-SOURCE-FILE "rfc3548" :DEPENDS-ON ("package" "stream"))
  (:PJB-CL-SOURCE-FILE "iso639a" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "iso3166" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "iso4217" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "character-sets" :DEPENDS-ON ("package" "string"))
  (:PJB-CL-SOURCE-FILE "html-iso8879-1" :DEPENDS-ON
   ("package" "array" "string"))
  (:PJB-CL-SOURCE-FILE "html" :DEPENDS-ON
   ("package" "list" "character-sets" "string"))
  (:PJB-CL-SOURCE-FILE "hquery" :DEPENDS-ON ("package" "string"))
  (:PJB-CL-SOURCE-FILE "htrans" :DEPENDS-ON
   ("package" "html" "hquery" "string"))
  (:PJB-CL-SOURCE-FILE "database" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "parse-html" :DEPENDS-ON
   ("package" "html-iso8879-1" "peek-stream" "utility" "list" "string"))
  (:PJB-CL-SOURCE-FILE "cache" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "aliases" :DEPENDS-ON
   ("package" "ecma048" "stream" "string" "utility" "list"))
  (:PJB-CL-SOURCE-FILE "passwd" :DEPENDS-ON ("package" "stream" "string"))
  (:PJB-CL-SOURCE-FILE "group" :DEPENDS-ON ("package" "stream" "string"))
  (:PJB-CL-SOURCE-FILE "primes" :DEPENDS-ON ("package" "utility"))
  (:PJB-CL-SOURCE-FILE "tea" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "raiden" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "make-depends" :DEPENDS-ON
   ("package" "html" "string" "character-sets" "list" "utility" "file"))
  (:PJB-CL-SOURCE-FILE "cxx" :DEPENDS-ON ("package" "graph"))
  (:PJB-CL-SOURCE-FILE "csv" :DEPENDS-ON
   ("package" "ecma048" "peek-stream" "utility"))
  (:PJB-CL-SOURCE-FILE "iban" :DEPENDS-ON
   ("package" "iso3166" "list" "string" "utility"))
  (:PJB-CL-SOURCE-FILE "rib" :DEPENDS-ON ("package" "iban"))
  (:PJB-CL-SOURCE-FILE "invoice" :DEPENDS-ON
   ("package" "iso4217" "string" "utility"))
  (:PJB-CL-SOURCE-FILE "browser" :DEPENDS-ON ("package" "string"))
  (:PJB-CL-SOURCE-FILE "ed" :DEPENDS-ON ("package"))
  (:PJB-CL-SOURCE-FILE "interactive" :DEPENDS-ON
   ("package" "browser" "string" "utility")))) 
