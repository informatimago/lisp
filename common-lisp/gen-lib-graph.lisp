;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               gen-lib-graph.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file scans all the .asd files in this library and
;;;;    generates a GrafViz .dot files showing their dependencies.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(in-package "COMMON-LISP-USER")
(require 'asdf)

(setf asdf:*central-registry*
      (append (remove-duplicates
               (mapcar (lambda (path)
                         (make-pathname :name nil :type nil :version nil :defaults path))
                       (directory "**/*.asd"))
               :test (function equalp))
              asdf:*central-registry*))


(asdf:oos 'asdf:load-op :com.informatimago.common-lisp.graphviz)



(defpackage "COM.INFORMATIMAGO.TOOLS.GEN-LIB-GRAPH"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.GRAPHVIZ.GRAPH-DOT"))
(in-package "COM.INFORMATIMAGO.TOOLS.GEN-LIB-GRAPH")


(defparameter *asd-files* (directory "*/*.asd"))


(defun read-asd-file (path)
  (let ((*package* *package*)
        (result '()))
    (labels ((collect (sexp)
               (push sexp result))
             (process (sexp)
               (cond
                 ((atom sexp))
                 ((eql 'eval-when (first sexp))
                  (when (intersection '(:load-toplevel :execute load eval)
                                      (second sexp))
                    (dolist (sexp (cddr sexp))
                      (process sexp))))
                 ((eql 'progn (first sexp))
                  (dolist (sexp (cdr sexp))
                    (process sexp)))
                 ((eql 'in-package (first sexp))
                  (eval sexp))
                 ((eql 'asdf:defsystem (first sexp))
                  (collect sexp)))))
      (with-open-file (asd path)
        (loop
          :for sexp := (ignore-errors (read asd nil asd))
          :until (eql sexp asd)
          :do (process sexp))))
    (nreverse result)))

(defun asd-name (asd)
  (string-downcase (second asd)))

(defun asd-depends-on (asd)
  (mapcar (function string-downcase)
          (getf (cddr asd) :depends-on)))

(defmethod dot-label ((element element-class))
  (getf (properties element) :dot-label))

(defun make-system-graph (asd-files graph-path &key test)
  (let* ((asds  (let ((asds  (mapcan (function read-asd-file) asd-files)))
                  (if test
                      (remove-if-not test asds)
                      asds)))
         (nodes (mapcar (lambda (asd)
                          (format *trace-output* ";; read ~A~%" (asd-name asd))
                          (force-output *trace-output*)
                          (make-instance 'element-class
                                         :ident (asd-name asd)
                                         :properties (list :asd asd
                                                           :dot-label (subseq (string (asd-name asd))
                                                                              (length "com.informatimago.common-lisp.")))))
                        asds))
         (g     (make-instance 'graph-class :edge-class 'directed-edge-class)))
    (add-nodes g nodes)
    (loop
      :for node :in nodes
      :for dependencies = (mapcar (lambda (asd-name)
                                    (find asd-name nodes
                                          :test (function equal)
                                          :key (lambda (node) (asd-name (get-property node :asd)))))
                                  (asd-depends-on (get-property node :asd)))
      :do (loop
            :for dependency :in dependencies
            :when dependency
              :do (format *trace-output* "~A -> ~A~%" (dot-label node) (dot-label dependency))
                  (add-edge-between-nodes g node dependency)))
    (set-property g :dot-rankdir "LR")
    (set-property g :dot-concentrate t)
    (with-open-file (out graph-path
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (princ (generate-dot g) out))))


(make-system-graph *asd-files* "system-graph.dot"
                   :test (lambda (asd) (not (suffixp ".test" (asd-name asd)))))


;;;; THE END ;;;;
