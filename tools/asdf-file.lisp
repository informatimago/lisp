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
;;;;    2013-09-06 <PJB> Updated for publication.
;;;;    2012-04-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2013
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
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH"
        "COM.INFORMATIMAGO.TOOLS.DEPENDENCY-CYCLES")
  (:export "LOAD-SIMPLE-ASD-FILE"

           "ASDF-FILE" "ASDF-FILE-P" "MAKE-ASDF-FILE" "COPY-ASDF-FILE"
           "ASDF-FILE-PATH" "ASDF-FILE-DEPENDS-ON" "ASDF-FILE-REACHABLE"
           
           "ADJACENCY-LIST" "REACHABLE-LIST"
           "DEPENDENCIES")
  (:documentation "

Reads simple .asd files, without instanciating ASDF objects.

  (LOAD-SIMPLE-ASD-FILE path-to-asd-file) --> hashtable mapping file names to ASDF-FILE structures.


NOTE: The current implementation expects the defsystem form to be the
      first and only form in the asd file.

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2012 - 2013
    
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
                           (compute-closure (function asdf-file-depends-on) (asdf-file-depends-on file))))
                   asdf-files)
          asdf-files)))


(defmethod adjacency-list ((file asdf-file))
  (asdf-file-depends-on file))


(defmethod reachable-list ((file asdf-file))
  (asdf-file-reachable file))

(defun dependencies  (p q) (member q (asdf-file-reachable p)))




;;;; THE END ;;;;
