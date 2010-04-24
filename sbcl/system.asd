;; -*- mode:lisp -*-

(DEFPACKAGE "COM.INFORMATIMAGO.ASDF"
  (:USE "COMMON-LISP")) 

(IN-PACKAGE "COM.INFORMATIMAGO.ASDF") 

(UNLESS (HANDLER-CASE (FIND-CLASS 'PJB-CL-SOURCE-FILE) (T NIL NIL))
  (DEFCLASS PJB-CL-SOURCE-FILE (ASDF:CL-SOURCE-FILE) NIL)
  (FLET ((OUTPUT-FILES (C)
           (FLET ((IMPLEMENTATION-ID ()
                    (FLET ((FIRST-WORD (TEXT)
                             (LET ((POS (POSITION (CHARACTER " ") TEXT)))
                               (REMOVE (CHARACTER ".")
                                       (IF POS
                                           (SUBSEQ TEXT 0 POS)
                                           TEXT)))))
                      (FORMAT NIL "~A-~A-~A"
                              (COND
                               ((STRING-EQUAL
                                 "International Allegro CL Enterprise Edition"
                                 (LISP-IMPLEMENTATION-TYPE))
                                "ACL")
                               (T (FIRST-WORD (LISP-IMPLEMENTATION-TYPE))))
                              (FIRST-WORD (LISP-IMPLEMENTATION-VERSION))
                              (FIRST-WORD (MACHINE-TYPE))))))
             (LET* ((OBJECT
                     (COMPILE-FILE-PATHNAME (ASDF:COMPONENT-PATHNAME C)))
                    (PATH
                     (MERGE-PATHNAMES
                      (MAKE-PATHNAME :DIRECTORY
                                     (LIST :RELATIVE
                                           (FORMAT NIL "OBJ-~:@(~A~)"
                                                   (IMPLEMENTATION-ID)))
                                     :NAME (PATHNAME-NAME OBJECT) :TYPE
                                     (PATHNAME-TYPE OBJECT))
                      OBJECT)))
               (ENSURE-DIRECTORIES-EXIST PATH)
               (LIST PATH)))))
    (DEFMETHOD ASDF:OUTPUT-FILES
               ((OPERATION ASDF:COMPILE-OP) (C PJB-CL-SOURCE-FILE))
               (OUTPUT-FILES C))
    (DEFMETHOD ASDF:OUTPUT-FILES
               ((OPERATION ASDF:LOAD-OP) (C PJB-CL-SOURCE-FILE))
               (OUTPUT-FILES C)))) 

(ASDF:DEFSYSTEM :COM.INFORMATIMAGO.SBCL
  :DESCRIPTION
  "This ASDF system gathers all the COM.INFORMATIMAGO.SBCL packages."
  :VERSION
  "1.0.76"
  :AUTHOR
  "none"
  :LICENCE
  "GPL"
  :DEPENDS-ON
  (:COM.INFORMATIMAGO.COMMON-LISP)
  :COMPONENTS
  ((:PJB-CL-SOURCE-FILE "posix") (:PJB-CL-SOURCE-FILE "readline"))) 
