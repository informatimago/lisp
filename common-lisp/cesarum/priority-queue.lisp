;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               priority-queue.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Priority queues are lists ordered on a key of each element.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-05-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2011 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PRIORITY-QUEUE"
  (:use "COMMON-LISP")
  (:export "PQ" "MAKE-PQ" "PQ-P" "COPY-PQ" "PQ-LESSP" "PQ-EMPTYP"
           "PQ-LENGTH" "PQ-ELEMENTS" "PQ-ELEMENTS" "PQ-FIRST" "PQ-POP"
           "PQ-POP*" "PQ-INSERT" "PQ-INSERT*" "PQ-REMOVE" "PQ-KEY"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PRIORITY-QUEUE")

(defstruct pq
  "Defines a priority queue data structure.
We keep the %queue sorted in a stubbed list.
The pq structure may be initialized with a LESSP function (default is <)
and with a KEY function (default is IDENTITY)."
  (%queue   (list 'head))
  (lessp    (function <))
  (key      (function identity)))


(defun pq-emptyp (pq)
  "Whether the priority queue is empty.  [O(1)]"
  (null (rest (pq-%queue pq))))

(defun pq-length (pq)
  "The number of elements in the priority queue.  [O(length(pq))]"
  (length (rest (pq-%queue pq))))

(defun pq-elements (pq)
  "Returns a list containing the sorted elements in the priority queue. [O(length(pq))]"
  (mapcar (function car) (rest (pq-%queue pq))))

(defun (setf pq-elements) (new-elements pq)
  "Replaces all the elements of PQ by the NEW-ELEMENTS (need not be sorted).
Returns NEW-ELEMENTS."
  (let ((key (pq-key pq)))
    (setf (pq-%queue pq) (cons 'head
                               (sort (map 'list (lambda (x) (cons x (funcall key x))) new-elements)
                                     (pq-lessp pq)
                                     :key (function cdr)))))
  new-elements)

(defun pq-first (pq)
  "Returns the first element of the priority queue."
  (let ((%queue (pq-%queue pq)))
    (if (rest %queue)
        (car (second %queue))
        (error "PQ-FIRST: The priority queue is empty."))))

(defun pq-pop (pq)
  "Removes and returns the first element of the priority queue."
  (let ((%queue (pq-%queue pq)))
    (if (rest %queue)
        (car (pop (rest %queue)))
        (error "PQ-POP: The priority queue is empty."))))

(defun pq-pop* (pq)
  "Removes and returns the first element of the priority queue."
  (let ((%queue (pq-%queue pq)))
    (if (rest %queue)
        (prog1 (rest %queue)
          (setf (rest %queue) (rest (rest %queue))))
        (error "PQ-POP: The priority queue is empty."))))

(defun pq-insert (pq element)
  "Inserts the element in order in the priority queue [O(length(pq))].
Returns the PQ."
  (let ((lessp (pq-lessp pq))
        (key   (pq-key  pq)))
    (loop
       :with ekey = (funcall key element)
       :for current = (pq-%queue pq) :then (rest current)
       :while (and (rest current) (funcall lessp (cdr (second current)) ekey))
       :finally (setf (rest current) (acons element ekey (rest current))))
    pq))

(defun pq-insert* (pq element)
  "Inserts the ((element . key)) in order in the priority queue [O(length(pq))].
Returns the PQ."
  (let ((lessp (pq-lessp pq))
        (key   (pq-key  pq)))
    (loop
       :with ekey = (funcall key (caar element))
       :for current = (pq-%queue pq) :then (rest current)
       :while (and (rest current) (funcall lessp (cdr (second current)) ekey))
       :finally (setf (cdar element) ekey
                      (cdr  element) (rest current)
                      (rest current) element))
    pq))

(defun pq-remove (pq element)
  "Removes the first occurence of the element from the priority queue [O(length(pq))]
O(pq-remove pq (pq-first pq)) = O(1)
Returns the ELEMENT."
  (loop
     :for current = (pq-%queue pq) :then (rest current)
     :while (and (rest current) (not (eql element (car (second current)))))
     :finally (when (rest current)
                (setf (rest current) (rest (rest current)))))
  element)


;;;; THE END ;;;;
