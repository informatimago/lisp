;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               a-star.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements the A* algorithm.
;;;;    cf. http://gabrielgambetta.com/path1.html
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-10-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.A-STAR"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export "+INFINITY+" "FIND-PATH")
  (:documentation "The A* algorithm."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.A-STAR")


(defconstant +infinity+ most-positive-long-float)

(defun find-path (successors previous set-previous cost set-cost estimate-distance goalp
                  start-node goal-node)
  "
DO:                 Implement the A* algorithm.

SUCCESSORS:         a function giving for each node, the list of its
                    successors.

PREVIOUS:           a getter giving the current previous node for the
                    given node.

SET-PREVIOUS:       a setter to set the current previous node for the
                    given node. (lambda (new-previous node))

COST:               a getter giving the current cost of the given
                    node. The default should be +INFINITY+.

SET-COST:           a setter setting the current cost of the given
                    node. (lambda (new-cost node))

ESTIMATE-DISTANCE:  a function of two nodes returning a cost estimate
                    of the distance between them.

GOALP:              a predicate indicating whether the node is the
                    GOAL-NODE (or close enough).

START-NODE:         the start node of the searched path.

GOAL-NODE:          the end node of the searched path.

RETURN:             a path, ie. a list of nodes from START-NODE to
                    GOAL-NODE.
"
  (with-functions (successors previous set-previous cost set-cost estimate-distance goalp)
    (flet ((build-path (node)
             (loop
               :with path = '()
               :for current = node :then (previous current)
               :while current
               :do (push current path)
               :finally (return path)))
           (choose-node (reachable)
             (loop
               :with min-cost = +infinity+
               :with best-node = nil
               :for node :in reachable
               :for cost-start-to-node = (cost node)
               :for cost-node-to-goal = (estimate-distance node goal-node)
               :for total-cost = (+ cost-start-to-node cost-node-to-goal)
               :do (print (list node '/ cost-start-to-node '+ cost-node-to-goal '= total-cost '/ min-cost))
               :when (< total-cost min-cost)
                 :do (setf min-cost total-cost
                           best-node node)
               :finally (return best-node))))
      (declare (inline build-path choose-node))
      (set-cost 0 start-node)
      (loop
        :with reachable = (list start-node)
        :with explored = '()
        :while reachable
        :do (let ((node (choose-node reachable)))
              (when (goalp node)
                (return-from find-path (build-path node)))
              (setf reachable (delete node reachable))
              (push node explored)
              (let ((new-reachable (set-difference (successors node) explored)))
                (dolist (adjacent new-reachable)
                  (unless (member adjacent reachable)
                    (push adjacent reachable))
                  (let ((new-cost (1+ (cost node))))
                    (when (< new-cost (cost adjacent))
                      (set-previous node adjacent)
                      (set-cost new-cost adjacent))))))))))

;;;; THE END ;;;;

