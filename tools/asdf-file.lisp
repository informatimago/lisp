;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               asdf-file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Reads ASDF files.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-02 <PJB> Added generate-dot.
;;;;    2013-09-06 <PJB> Updated for publication.
;;;;    2012-04-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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

(defpackage "COM.INFORMATIMAGO.TOOLS.ASDF-FILE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH"
        "COM.INFORMATIMAGO.TOOLS.DEPENDENCY-CYCLES")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.SCRIPT"
                "SHELL")
  (:export "LOAD-SIMPLE-ASD-FILE"

           "ASDF-FILE" "ASDF-FILE-P" "MAKE-ASDF-FILE" "COPY-ASDF-FILE"
           "ASDF-FILE-PATH" "ASDF-FILE-DEPENDS-ON" "ASDF-FILE-REACHABLE"
           
           "ADJACENCY-LIST" "REACHABLE-LIST"
           "DEPENDENCIES"

           "GENERATE-DOT" "DOT")
  (:documentation "

Reads simple .asd files, without instanciating ASDF objects.

  (LOAD-SIMPLE-ASD-FILE path-to-asd-file) --> hashtable mapping file names to ASDF-FILE structures.


NOTE: The current implementation expects the defsystem form to be the
      first and only form in the asd file.

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2012 - 2014
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"))
(in-package "COM.INFORMATIMAGO.TOOLS.ASDF-FILE")

(defstruct asdf-file
  path
  depends-on
  reachable)

(defmethod print-object ((self asdf-file) stream)
  (if *print-readably*
      (format stream "#.(~S ~S ~S)" 'intern-file (asdf-file-path self) '*asdf-files*)
      (print-unreadable-object (self stream :identity t :type t)
        (format stream "~S" (asdf-file-path self))))
  self)


(defun intern-file (path asdf-files)
  (let ((file (gethash path asdf-files)))
    (or file
        (setf (gethash path asdf-files) (make-asdf-file :path path)))))


(defvar *asdf-files* (make-hash-table :test (function equal))
  "For interactive exploring, we keep a reference to the last loaded
asdf files hash-table in this variable.")


(defun load-simple-asd-file (path)
  "
RETURN: A hash-table mapping file paths to ASDF-FILE structures.
"
  (setf *asdf-files*
        (let ((asdf-files (make-hash-table :test (function equal))))
          (let ((system (with-open-file (stream path) (read stream))))
            (dolist (compo (getf (cddr system) :components))
              (when (and (listp compo)
                         (eq :file (first compo)))
                (let ((file (intern-file (second compo) asdf-files)))
                  (dolist (depend (getf (cddr compo) :depends-on))
                    (when (stringp depend)
                      (push (intern-file depend asdf-files) (asdf-file-depends-on file))))))))
          (maphash (lambda (path file)
                     (declare (ignore path))
                     (setf (asdf-file-reachable file)
                           (transitive-closure (function asdf-file-depends-on) (asdf-file-depends-on file))))
                   asdf-files)
          asdf-files)))


(defmethod adjacency-list ((file asdf-file))
  (asdf-file-depends-on file))


(defmethod reachable-list ((file asdf-file))
  (asdf-file-reachable file))

(defun dependencies  (p q) (member q (asdf-file-reachable p)))


(defmethod generate-dot ((file asdf-file))
  (let ((style     "filled")
        (color     "black")
        (fillcolor "LightYellow")
        (label     (pathname-name (asdf-file-path file))))
    (format nil "~S [ style=~A color=~A fillcolor=~A label=\"~A\" ];~%"
            (pathname-name (asdf-file-path file)) style color fillcolor label)))

(defmethod generate-dot ((edge cons))
  (format nil "~S -> ~S [ weight=~D, style=~A, color=~A ];~%"
          (pathname-name (asdf-file-path (car edge)))
          (pathname-name (asdf-file-path (cdr edge)))
          1
          "solid" ; "dotted" "dashed" "bold"
          "black"))

(defmethod generate-dot ((path pathname))
  "
RETURN: A string containing the dot file data for this graph.
"
  (let ((files (load-simple-asd-file path)))
    (with-output-to-string (*standard-output*)
      (format t "digraph ~S~%" (pathname-name path))
      (format t "{~%")
      (format t "rankdir=~A;~%" "TB")
      (format t "concentrate=~:[false~;true~];~%" t)
      (mapc 'write-string '(
                            "// attributes of graph:~%"
                            "// page=8,11.4;    // page size (NeXTprinter:A4).~%"
                            "// size=30,8;     // graph size (please edit to fit).~%"
                            "// rotate=90;    // graph orientation (please edit to fit).~%"
                            "// ratio=fill;  // fill the size (or compress, auto, aspect/ratio).~%"
                            "nodesep=0.3;~%"
                            "ranksep=0.3;~%"
                            "center=1;~%"
                            "// common attributes of NODES:~%"
                            "node [height=0.2 width=0.5 shape=box fontsize=8 fontname=Times] ;~%"))
      (maphash (lambda (key file)
                 (declare (ignore key))
                 (write-string (generate-dot file))) files)
      (format t "// common attributes of edges:~%edge [style=solid];~%")
      (maphash (lambda (key file)
                 (declare (ignore key))
                 (dolist (dependency (asdf-file-depends-on file))
                   (write-string (generate-dot (cons file dependency)))))
               files)
      (format t "}~%"))))

;; (COM.INFORMATIMAGO.TOOLS.ASDF-FILE:generate-dot #P"/Users/pjb/src/public/lisp/tools/com.informatimago.tools.check-asdf.asd")

(defun dot (path)
  (let ((path.dot (make-pathname :defaults path :type "dot"))
        (path.pdf (make-pathname :defaults path :type "pdf")))
    (with-open-file (dot path.dot
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (write-string (generate-dot path) dot))
    (shell "/opt/local/bin/dot -Tpdf -o ~S ~S"
           (#+ccl ccl:native-translated-namestring #-ccl namestring path.pdf)
           (#+ccl ccl:native-translated-namestring #-ccl namestring path.dot))
    (shell "open ~S"
           (#+ccl ccl:native-translated-namestring #-ccl namestring path.pdf))))

;; (dot  #P"/Users/pjb/src/public/lisp/tools/com.informatimago.tools.check-asdf.asd")




;;;; THE END ;;;;
