;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               tree.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements a package tree index, for hierarchical package index.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-04 <PJB> Extracted from lispdoc.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
;;;;
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LISPDOC.TREE")


(defvar *hierarchical-package-separator* #\.
  "The character used to separate package 'components' in hierarchical package names.
It's usually a dot, but some may use a different character such as a slash.")


(defun package-path (package)
  "

RETURN: A list of component strings representing the name of the
        PACKAGE, assuming it is structured as a list of components
        joined by the *HIERARCHICAL-PACKAGE-SEPARATOR*.

EXAMPLE: (package-path  \"COM.INFORMATIMAGO.LISPDOC.TREE\")
         --> (\"COM\" \"INFORMATIMAGO\" \"LISPDOC\" \"TREE\")

"
  (split-sequence *hierarchical-package-separator*
                  (etypecase package
                    (string package)
                    (package (package-name package)))))




(defstruct (tree
            (:copier tree-copy))
  "
A node in the package hierarchical naming tree.

PARENT:   a reference to the parent node, or NIL for the root.
NODE:     the string component naming this node.
PACKAGE:  the joined hierarchical package name of the package designated by this node, or NIL if none.
CHILDREN: the subtrees.
"
  parent
  node
  package
  (children '()))


(defmethod print-object ((tree tree) stream)
  (print-unreadable-object (tree stream :identity t :type t)
    (format stream "~S" (list :node (tree-node tree)
                              :package (tree-package tree)
                              :children (length (tree-children tree)))))
  tree)


(defun tree-children-named (tree node)
  "
RETURN: the child in the TREE named by NODE.
"
  (find node (tree-children tree)
        :key (function tree-node)
        :test (function equal)))


(defun tree-add-node-at-path (tree path pname)
  "
DO:     Add a new tree node in the TREE for the package named PNAME at
        the given relative PATH.

RETURN: tree.
"
  (if (endp path)
      (progn
        (setf (tree-package tree) pname)
        tree)
      (let ((child (tree-children-named tree (first path))))
        (if child
            (tree-add-node-at-path child (rest path) pname)
            (let ((new-child (make-tree :parent tree
                                        :node (first path)
                                        :package (when (endp (rest path))
                                                   pname))))
              (appendf (tree-children tree) (list new-child))
              (tree-add-node-at-path new-child (rest path) pname))))))


(defun make-index-tree (package-names)
  "
RETURN: a new tree filled with nodes for all the PACKAGE-NAMES.
"
  (let ((root (make-tree)))
    (dolist (pname package-names root)
     (tree-add-node-at-path root (package-path pname) pname))))


(defun tree-node-at-path (tree path)
  "
RETURN: The tree node found at the given PATH.
"
  (if (endp path)
      tree
      (let ((child (tree-children-named tree (first path))))
        (when child
          (tree-node-at-path child (rest path))))))


(defun tree-path (tree)
  "
RETURN: The path from TREE to the root.
"
  (cons (tree-node tree) (when (and (tree-parent tree)
                                    (tree-node (tree-parent tree)))
                           (tree-path (tree-parent tree)))))


;;;; THE END ;;;;
