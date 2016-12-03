;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               analyze.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-30 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :xref))


(defpackage "RP-USER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.PACKAGE"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
        "COM.INFORMATIMAGO.XREF"
        "COM.INFORMATIMAGO.XREF.READER-SETUP")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
                          "READTABLE"
                          "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
                          "READ" "READ-PRESERVING-WHITESPACE"
                          "READ-DELIMITED-LIST"
                          "READ-FROM-STRING"
                          "READTABLE-CASE" "READTABLEP"
                          "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
                          "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
                          "SET-SYNTAX-FROM-CHAR"
                          "WITH-STANDARD-IO-SYNTAX"
                          "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
                          "*READ-SUPPRESS*" "*READTABLE*")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.PACKAGE"
                          "SYMBOL"
                          "SYMBOLP" "MAKE-SYMBOL" "SYMBOL-NAME" "SYMBOL-PACKAGE"
                          "SYMBOL-VALUE" "SYMBOL-FUNCTION" "SYMBOL-PLIST"
                          "BOUNDP" "FBOUNDP"
                          "KEYWORD" "KEYWORDP"
                          "PACKAGE"
                          "PACKAGEP"  "MAKE-PACKAGE" "FIND-PACKAGE" "DELETE-PACKAGE"
                          "FIND-SYMBOL" "IMPORT" "INTERN" "SHADOW" "SHADOWING-IMPORT"
                          "EXPORT" "UNEXPORT" "UNINTERN" "USE-PACKAGE"
                          "UNUSE-PACKAGE" "PACKAGE-NAME" "PACKAGE-NICKNAMES"
                          "PACKAGE-USE-LIST" "PACKAGE-USED-BY-LIST" "PACKAGE-SHADOWING-SYMBOLS"
                          "LIST-ALL-PACKAGES" "FIND-ALL-SYMBOLS" "RENAME-PACKAGE"
                          "*PACKAGE*"
                          "WITH-PACKAGE-ITERATOR"
                          "DO-SYMBOLS" "DO-EXTERNAL-SYMBOLS" "DO-ALL-SYMBOLS"
                          "DEFPACKAGE" "IN-PACKAGE"
                          "PACKAGE-ERROR" "PACKAGE-ERROR-PACKAGE"))
(in-package  "RP-USER")



(defparameter *sources*
  (sort (remove-if-not (function alpha-char-p)
                       (directory "/home/pjb/works/patchwork/pw-src/**/*.lisp")
                       :key (lambda (path) (aref (pathname-name path) 0)))
        (function string<) :key (function namestring)))
(com.informatimago.xref.reader-setup:setup)
(com.informatimago.xref:xref-files *sources*)




#||

;; (progn (mapc 'print *packages-created-automatically*) (terpri) (values))
(progn (mapc 'print (sort (copy-seq (list-all-packages))
                          (function string<) :key (function package-name)))
       (terpri) (values))


(let ((*print-case* :upcase))
  (dolist (s (package-symbols "FFI") (terpri)) (terpri) (princ s)))

(let ((*print-case* :upcase))
  (dolist (db '(:file) (values))
    (print db) (terpri) (display-database db) (terpri)))

(let ((*print-case* :upcase))
  (dolist (db '(:callers :readers :setters) (values))
    (print db) (terpri) (display-database db) (terpri)))

(let ((*print-case* :upcase))
 (print-caller-trees))

(let ((*print-case* :upcase))
 (dolist (mode '(:call-graph :caller-graph) (values))
   (print mode) (terpri) (make-caller-tree mode) (terpri)))

(determine-file-dependencies)
(let ((*print-case* :upcase))
 (print-file-dependencies))

||#

;;; The following functions may be useful for viewing the database and
;;; debugging the calling patterns.
;;;
;;; DISPLAY-DATABASE (&optional database types-to-ignore)         [FUNCTION]
;;;    Prints out the name of each symbol and all its callers. Specify
;;;    database :callers (the default) to get function call references,
;;;    :file to the get files in which the symbol is called, :readers to get
;;;    variable references, and :setters to get variable binding and
;;;    assignments. Ignores functions of types listed in types-to-ignore.
;;;
;;; PRINT-CALLER-TREES (&key (mode *default-graphing-mode*)       [FUNCTION]
;;;                     (types-to-ignore *types-to-ignore*)
;;;                     compact root-nodes)
;;;    Prints the calling trees (which may actually be a full graph and not
;;;    necessarily a DAG) as indented text trees using
;;;    PRINT-INDENTED-TREE. MODE is :call-graph for trees where the children
;;;    of a node are the functions called by the node, or :caller-graph for
;;;    trees where the children of a node are the functions the node calls.
;;;    TYPES-TO-IGNORE is a list of funcall types (as specified in the
;;;    patterns) to ignore in printing out the database. For example,
;;;    '(:lisp) would ignore all calls to common lisp functions. COMPACT is
;;;    a flag to tell the program to try to compact the trees a bit by not
;;;    printing trees if they have already been seen. ROOT-NODES is a list
;;;    of root nodes of trees to display. If ROOT-NODES is nil, tries to
;;;    find all root nodes in the database.
;;;
;;; MAKE-CALLER-TREE (&optional (mode *default-graphing-mode*)    [FUNCTION]
;;;                   (types-to-ignore *types-to-ignore*)
;;;                   compact)
;;;    Outputs list structure of a tree which roughly represents the
;;;    possibly cyclical structure of the caller database.
;;;    If mode is :call-graph, the children of a node are the functions
;;;    it calls. If mode is :caller-graph, the children of a node are the
;;;    functions that call it.
;;;    If compact is T, tries to eliminate the already-seen nodes, so
;;;    that the graph for a node is printed at most once. Otherwise it will
;;;    duplicate the node's tree (except for cycles). This is usefull
;;;    because the call tree is actually a directed graph, so we can either
;;;    duplicate references or display only the first one.
;;;
;;; DETERMINE-FILE-DEPENDENCIES (&optional database)          [FUNCTION]
;;;    Makes a hash table of file dependencies for the references listed in
;;;    DATABASE. This function may be useful for automatically resolving
;;;    file references for automatic creation of a system definition
;;;    (defsystem).
;;;
;;; PRINT-FILE-DEPENDENCIES (&optional database)              [FUNCTION]
;;;    Prints a list of file dependencies for the references listed in
;;;    DATABASE. This function may be useful for automatically computing
;;;    file loading constraints for a system definition tool.
;;;
;;; WRITE-CALLERS-DATABASE-TO-FILE (filename)                     [FUNCTION]
;;;    Saves the contents of the current callers database to a file. This
;;;    file can be loaded to restore the previous contents of the
;;;    database. (For large systems it can take a long time to crunch
;;;    through the code, so this can save some time.)




;;; LIST-CALLERS (symbol)                                         [FUNCTION]
;;;    Lists all functions which call SYMBOL as a function (function
;;;    invocation).
;;;
;;; LIST-READERS (symbol)                                         [FUNCTION]
;;;    Lists all functions which refer to SYMBOL as a variable
;;;    (variable reference).
;;;
;;; LIST-SETTERS (symbol)                                         [FUNCTION]
;;;    Lists all functions which bind/set SYMBOL as a variable
;;;    (variable mutation).
;;;
;;; LIST-USERS (symbol)                                           [FUNCTION]
;;;    Lists all functions which use SYMBOL as a variable or function.
;;;
;;; WHO-CALLS (symbol &optional how)                              [FUNCTION]
;;;    Lists callers of symbol. HOW may be :function, :reader, :setter,
;;;    or :variable."
;;;
;;; WHAT-FILES-CALL (symbol)                                      [FUNCTION]
;;;    Lists names of files that contain uses of SYMBOL
;;;    as a function, variable, or constant.
;;;
;;; SOURCE-FILE (symbol)                                          [FUNCTION]
;;;    Lists the names of files in which SYMBOL is defined/used.
;;;
;;; LIST-CALLEES (symbol)                                         [FUNCTION]
;;;    Lists names of functions and variables called by SYMBOL.
;;;
;;; -----



(defparameter *read-packages*
  '("C-GET-NOTE-SLOTS"
    "C-GET-SELECTIONS"
    "C-PATCH-ACCUM"
    "C-PATCH-BUFFER"
    "C-PATCH-CHORD-LINE"
    "C-PATCH-FILE-BUFFER"
    "C-PATCH-LIST-EDITOR"
    "C-PW-MIDI-IN"
    "C-PW-SEND-MIDI-NOTE"
    "C-PW-TEXT-BOX"
    "C-PW-TEXT-INPUT"
    "CCL"
    "CLENI"
    "CLPF-Util"
    "COMBINATORIAL-INTERV"
    "COMMON-LISP"
    "COMMON-LISP-USER"
    "EPW"
    "FFI"
    "KEYWORD"
    "MIDI"
    "MIDISHARE"
    "PATCH-WORK"
    "PW"
    "SCHEDULER"))


(defparameter *grep-packages*
  '("QUANTIZING"
    "C-PATCH-CHORD-LINE"
    "C-PW-SEND-MIDI-NOTE"
    "C-PW-MIDI-IN"
    "C-GET-NOTE-SLOTS"
    "C-GET-SELECTIONS"
    "CLENI"
    "COMBINATORIAL-INTERV"
    "MIDISHARE"
    "USER-ABSTRACTION"
    "USER-COMP-ABSTR"
    "C-LIST-ITEM-H"
    "C-LIST-ITEM"
    "C-TABLE-WINDOW-H"
    "C-TABLE-WINDOW"
    "C-PATCH-LIST-EDITOR"
    "C-PATCH-BUFFER"
    "C-PATCH-ACCUM"
    "C-PATCH-FILE-BUFFER"
    "C-PW-TEXT-INPUT"
    "C-PW-TEXT-BOX"
    "USER-SUPPLIED-IN-OUTS"
    "LeLisp-macros"
    "PW-STYPE"
    "EPW"
    "CLPF-Util"
    "popUp-menu"
    "MIDI"
    "SCHEDULER"))

(set-difference *read-packages* *grep-packages* :test (function string=))
(set-difference *grep-packages* *read-packages* :test (function string=))

;; ("PW" "PATCH-WORK" "KEYWORD" "FFI" "COMMON-LISP-USER" "COMMON-LISP" "CCL")
;; ("popUp-menu" "PW-STYPE" "LeLisp-macros" "USER-SUPPLIED-IN-OUTS" "C-TABLE-WINDOW" "C-TABLE-WINDOW-H" "C-LIST-ITEM" "C-LIST-ITEM-H" "USER-COMP-ABSTR" "USER-ABSTRACTION" "QUANTIZING")


(defparameter *in-packages*
  '("C-GET-NOTE-SLOTS" "C-GET-SELECTIONS" "C-LIST-ITEM-H"
    "C-PATCH-ACCUM" "C-PATCH-BUFFER" "C-PATCH-CHORD-LINE"
    "C-PATCH-FILE-BUFFER" "C-PATCH-LIST-EDITOR" "C-PW-MIDI-IN"
    "C-PW-SEND-MIDI-NOTE" "C-PW-TEXT-BOX" "C-PW-TEXT-INPUT"
    "C-TABLE-WINDOW-H" "CLENI" "CLPF-Util" "COMBINATORIAL-INTERV"
    "EPW" "LeLisp-macros" "MIDI" "MIDISHARE" "PATCH-WORK" "PW"
    "PW-STYPE" "QUANTIZING" "SCHEDULER"))

(defparameter *defpackages*
  '("C-GET-NOTE-SLOTS" "C-GET-SELECTIONS" "C-LIST-ITEM"
    "C-LIST-ITEM-H" "C-PATCH-ACCUM" "C-PATCH-BUFFER" "C-PATCH-CHORD-LINE"
    "C-PATCH-FILE-BUFFER"  "C-PATCH-LIST-EDITOR" "C-PW-MIDI-IN"
    "C-PW-SEND-MIDI-NOTE" "C-PW-TEXT-BOX"  "C-PW-TEXT-INPUT"
    "C-TABLE-WINDOW" "C-TABLE-WINDOW-H" "CLENI" "CLPF-Util"
    "COMBINATORIAL-INTERV" "EPW" "LeLisp-macros" "MIDI" "MIDISHARE"
    "PW-STYPE" "QUANTIZING" "SCHEDULER" "USER-ABSTRACTION"
    "USER-COMP-ABSTR" "USER-SUPPLIED-IN-OUTS"))

(set-difference *in-packages* *defpackages* :test 'string=)
(set-difference *defpackages* *in-packages* :test 'string=)
;; ("PW" "PATCH-WORK")
;; ("USER-SUPPLIED-IN-OUTS" "USER-COMP-ABSTR" "USER-ABSTRACTION" "C-TABLE-WINDOW" "C-LIST-ITEM")

