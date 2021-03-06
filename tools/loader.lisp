(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

#||

(cd #P"~/works/patchwork/")
(load "loader.lisp")

||#


(pushnew #P"~/works/patchwork/tools/" asdf:*central-registry* :test (function equalp))
(ql:quickload :com.informatimago.check-asdf)
(com.informatimago.check-asdf:check-asdf-system-file #P"~/works/patchwork/patchwork/src/mclgui/mclgui.asd")
(com.informatimago.check-asdf:check-asdf-system-file #P"~/works/patchwork/patchwork/src/patchwork.asd")


#||

(cl:in-package :cl-user)
(cd #P"~/works/patchwork/")
(pushnew  #P"~/works/patchwork/pw-src/"   asdf:*central-registry* :test (function equalp))

(defun delete-pw-packages ()
  (loop :while (some 'identity (mapcar (lambda (p) (ignore-errors (delete-package p)))
                                       '("C-GET-NOTE-SLOTS"
                                         "C-GET-SELECTIONS"
                                         "C-LIST-ITEM" "C-LIST-ITEM-H"
                                         "C-PATCH-ACCUM"
                                         "C-PATCH-BUFFER"
                                         "C-PATCH-CHORD-LINE"
                                         "C-PATCH-FILE-BUFFER"
                                         "C-PATCH-LIST-EDITOR"
                                         "C-PW-MIDI-IN"
                                         "C-PW-SEND-MIDI-NOTE"
                                         "C-PW-TEXT-BOX"
                                         "C-PW-TEXT-INPUT"
                                         "C-TABLE-WINDOW"
                                         "C-TABLE-WINDOW-H" "CLENI"
                                         "CLPF-UTIL"
                                         "COMBINATORIAL-INTERV" "EPW"
                                         "FFI"
                                         "LELISP-MACROS"
                                         "MIDI"
                                         "MIDISHARE" "PATCH-WORK" "PW"
                                         "PW-APPLEEVENT" "PW-MACOSX"
                                         "PW-STYPE" "SCHEDULER"
                                         "QUANTIZING"
                                         "USER-ABSTRACTION"
                                         "USER-COMP-ABSTR"
                                         "USER-SUPPLIED-IN-OUTS")))))


(ql:quickload :patchwork)


||#


#||
 
(cl:in-package :cl-user)
(cd #P"~/works/patchwork/tools/")
(load "dependency-cycles")
(load "asdf-file")
(load "read-sources")
(com.informatimago.read-sources::analyse-patchwork)
(com.informatimago.read-sources::print-objc-class-hierarchy)


(cl:in-package "COM.INFORMATIMAGO.ASDF-FILE")
(progn
  ;; (load-asdf-system #P"~/works/patchwork/patchwork/src/mclgui/mclgui.asd")
  (load-asdf-system #P"~/works/patchwork/pw-src/patchwork.asd")
  (setf *sorted-files*  (topological-sort (hash-table-values *asdf-files*)
                                          (function dependencies)))
  (if (= (length *sorted-files*) (hash-table-count *asdf-files*))
      (format t "~&No cycle among files. ~%")
      (format t "~&The :depends-on relationship between files contains cycles! ~
                 ~%It should be a tree.~%"))
  (report-problems (hash-table-values *asdf-files*)))



(com.informatimago.read-sources::generate-classes-hierarchy)

||#


;; (sort (mapcar (lambda (p) (namestring (make-pathname :type nil :version nil :defaults (enough-namestring p #P"/home/pjb/works/patchwork/pw-src/")))) *sources*)
;;       'string<)



#||

(cl:in-package :cl-user)
(cd #P"~/works/patchwork/")
(pushnew  #P"~/works/patchwork/src/xref/" asdf:*central-registry* :test (function equalp))
(ql:quickload :xref)
(load #P"~/works/patchwork/src/xref/reader-setup.lisp")
(load #P"~/works/patchwork/analyze.lisp")
(cl:in-package  :rp-user)

(eval-when (:compile-toplevel)
  (load (compile-file "package-symbols.lisp")))
(eval-when (:execute)
  (load "package-symbols.lisp"))


;; (list-all-packages)
;; (mapcar (lambda (f) (funcall f :midi))
;;         `(,#'package-symbols
;;           ,#'package-external-symbols
;;           ,(lambda (p)
;;             (set-difference (package-imported-symbols p)
;;                             (package-external-symbols :cl)))))
;;
;; (nil
;;  (#3=MIDI:MIDI-OPEN #2=MIDI:MIDI-WRITE #1=MIDI:MIDI-CLOSE)
;;  (MIDI::*PLAYER* MIDI::*PW-REFNUM* MIDI::MIDI-RESET #1# #2# #3#))

||#



