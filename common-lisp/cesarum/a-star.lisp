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
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.A-STAR"
  (:use "COMMON-LISP")
  (:export "+INFINITY+" "FIND-PATH")
  (:documentation "The A* algorithm."))
(in-package  "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.A-STAR")


(defmacro with-functions ((&rest fnames) &body body)
  `(flet ,(mapcar (lambda (fname)
                    (if (listp fname)
                        (destructuring-bind (name &rest parameters) fname
                          `(,name ,parameters (funcall ,name ,@parameters)))
                        `(,fname (&rest arguments) (apply ,fname arguments))))
           fnames)
     (declare (inline ,@(mapcar (lambda (fname) (if (listp fname) (first fname) fname)) fnames)))
     ,@body))



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



(defun g1-successors (node)
  "Represents the graph http://gabrielgambetta.com/path1.html"
  (ecase node
    (a '(b f))
    (b '(a c))
    (c '(b))
    (d '())
    (e '(j))
    (f '(a k))
    (g '())
    (h '())
    (j '(e o))
    (k '(f l))
    (l '(k m q))
    (m '(l n))
    (n '(m o))
    (o '(n j t))
    (p '())
    (q '(l v))
    (r '())
    (s '())
    (t '(o y))
    (u '())
    (v '(q w))
    (w '(v x))
    (x '(w y))
    (y '(x t))))

(defun g1-coordinates (node)
  (ecase node
    (a #c(0 0))
    (b #c(1 0))
    (c #c(2 0))
    (d #c(3 0))
    (e #c(4 0))
    (f #c(0 1))
    (g #c(1 1))
    (h #c(2 1))
    (i #c(3 1))
    (j #c(4 1))
    (k #c(0 2))
    (l #c(1 2))
    (m #c(2 2))
    (n #c(3 2))
    (o #c(4 2))
    (p #c(0 3))
    (q #c(1 3))
    (r #c(2 3))
    (s #c(3 3))
    (t #c(4 3))
    (u #c(0 4))
    (v #c(1 4))
    (w #c(2 4))
    (x #c(3 4))
    (y #c(4 4))))

(defun g1-distance (a b)
  (abs (- (g1-coordinates a) (g1-coordinates b))))

(defun test/g1 (&key (start 'a) (goal 't))
  (multiple-value-call (function find-path)
    (function g1-successors)
    (let ((p (make-hash-table)))
      (values (lambda (node) (gethash node p))
              (lambda (new-previous node) (setf (gethash node p) new-previous))))
    (let ((c (make-hash-table)))
      (values (lambda (node) (gethash node c +infinity+))
              (lambda (new-cost node) (setf (gethash node c) new-cost))))
    (function g1-distance)
    (lambda (node) (eql node goal))
    start goal))

(defun test ()
  (assert (equal (test/g1 :start 'a :goal 'x)
                 '(a f k l q v w x)))
  (assert (equal (test/g1 :start 'a :goal 'y)
                 '(a f k l m n o t y)))
  :success)

(test)


;;;; THE END ;;;;

