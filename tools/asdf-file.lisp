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
;;;;    2012-04-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Analysing file dependencies in asdf systems.
;;;


(defpackage "COM.INFORMATIMAGO.ASDF-FILE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH"
        "COM.INFORMATIMAGO.DEPENDENCY-CYCLES")
  (:export "ASDF-FILE" "ASDF-FILE-P" "MAKE-ASDF-FILE" "COPY-ASDF-FILE"
           "ASDF-FILE-PATH" "ASDF-FILE-DEPENDS-ON" "ASDF-FILE-REACHABLE"
           "*ASDF-FILES*" "INTERN-FILE" "LOAD-ASDF-SYSTEM"
           "ADJACENCY-LIST" "REACHABLE-LIST" "DEPENDENCIES"))
(in-package "COM.INFORMATIMAGO.ASDF-FILE")

(defstruct asdf-file
  path
  depends-on
  reachable)

(defmethod print-object ((self asdf-file) stream)
  (if *print-readably*
      (format stream "#.(~S ~S)" 'intern-file (asdf-file-path self))
      (print-unreadable-object (self stream :identity t :type t)
        (format stream "~S" (asdf-file-path self))))
  self)

(defparameter *asdf-files* (make-hash-table :test (function equal)))

(defun intern-file (path)
  (let ((file (gethash path *asdf-files*)))
    (or file
        (setf (gethash path *asdf-files*) (make-asdf-file :path path)))))

(defun load-asdf-system (path)
  (setf *asdf-files* (make-hash-table :test (function equal)))
  (let ((system (with-open-file (stream path) (read stream))))
    (dolist (compo (getf (cddr system) :components))
      (when (and (listp compo)
               (eq :file (first compo)))
        (let ((file (intern-file (second compo))))
          (dolist (depend (getf (cddr compo) :depends-on))
            (when (stringp depend)
              (push (intern-file depend) (asdf-file-depends-on file))))))))
  (maphash (lambda (path file)
             (declare (ignore path))
             (setf (asdf-file-reachable file)
                   (compute-closure (function asdf-file-depends-on) (asdf-file-depends-on file))))
           *asdf-files*)
  (values))


(defmethod adjacency-list ((file asdf-file))
  (asdf-file-depends-on file))


(defmethod reachable-list ((file asdf-file))
  (asdf-file-reachable file))

(defun dependencies  (p q) (member q (asdf-file-reachable p)))




;;;; THE END ;;;;
