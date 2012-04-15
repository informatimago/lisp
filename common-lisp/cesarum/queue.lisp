;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               queue.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;
;;;;    This module exports a queue type. This is a structure optimized for
;;;;    FIFO operations, keeping a pointer to the head and the tail of a list.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-31 <PJB> Added QUEUE-DELETE
;;;;    2004-02-26 <PJB> Formated for publication.
;;;;    2001-12-31 <PJB> Added pjb-queue-requeue. 
;;;;                     Corrected the return value of some methods.
;;;;    2001-11-12 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2001 - 2005
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE"
  (:use "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export "QUEUE-TEST" "QUEUE-INVARIANT" "QUEUE-DELETE" "QUEUE-REQUEUE"
           "QUEUE-DEQUEUE" "QUEUE-ENQUEUE" "QUEUE-LAST-ELEMENT" "QUEUE-FIRST-ELEMENT"
           "QUEUE-EMPTY-P" "QUEUE-LENGTH" "MAKE-QUEUE" "QUEUE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "WHILE")
  (:documentation
   "This module exports a queue type. This is a structure optimized for
    FIFO operations, keeping a pointer to the head and the tail of a list.
    
    Copyright Pascal J. Bourguignon 2001 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE")




;;; The structure of a queue is as follow:
;;; 
;;;                  queue
;;;                    |
;;;                    V
;;;             +------+------+
;;;             | head | tail |--------------------------+
;;;             +------+------+                          |
;;;                |                                     |
;;;                V                                     V
;;;         +------+------+    +------+------+    +------+------+
;;;         | car  | cdr  |--->| car  | cdr  |--->| car  | cdr  |--->nil
;;;         +------+------+    +------+------+    +------+------+
;;;            |                  |                  |
;;;            V                  V                  V
;;;         +------+           +------+           +------+
;;;         | elem |           | elem |           | elem |
;;;         +------+           +------+           +------+


(defstruct (queue (:constructor %make-queue))
  (head nil :type list)
  (tail nil :type list))


(defun queue-invariant (queue)
  (assert (queue-p queue))
  (assert (or (and (null (queue-head queue))  (null (queue-tail queue)))
              (and (queue-head queue) (queue-tail queue))))
  (when (queue-head queue)
    (assert (list-length (queue-head queue))) ; not a circular list.
    (assert (search (queue-tail queue) (queue-head queue) :test (function eq)))
    (assert (null (cdr (queue-tail queue))))))


(defun make-queue () (%make-queue))


(defun queue-length (queue)
  "
PRE:     (queue-p queue)
RETURN:  The number of elements in the queue.
"
  (assert (queue-p queue))
  (length (queue-head queue))) ;;QUEUE-LENGTH


(defun queue-empty-p (queue)
  "
RETURN:  (= 0 (queue-length queue))
"
  (assert (queue-p queue))
  (null (queue-head queue)))


(defun queue-first-element (queue)
  "
PRE:     (queue-p queue)
RETURN:  The first element of the queue.
"
  (assert (queue-p queue))
  (first (queue-head queue)))


(defun queue-last-element (queue)
  "
PRE:     (queue-p queue)
RETURN:  The last element of the queue.
"
  (assert (queue-p queue))
  (first (queue-tail queue)))


(defun queue-enqueue  (queue element)
  "
PRE:     (queue-p queue)
         l=(queue-length queue)
POST:    (eq (queue-last-element queue) element),
         (queue-p queue),
         l+1=(queue-length queue)
RETURN:  queue
"
  (assert (queue-p queue))
  ;; (car q) = head      (cdr q) = tail
  (if (queue-head queue)
      (progn
        ;; There's already an element, just add to the tail.
        (setf (cdr (queue-tail queue)) (cons element nil))
        (setf (queue-tail queue)       (cdr (queue-tail queue))))
      (progn
        ;; The queue is empty, let's set the head.
        (setf (queue-head queue) (cons element nil))
        (setf (queue-tail queue) (queue-head queue))))
  queue)


(defun queue-delete (queue element &key (test (function eql)))
  "
POST:    (not (member element queue :test test))
RETURN:  queue
"
  (setf (queue-head queue) (delete element  (queue-head queue) :test test)
        (queue-tail queue) (last (queue-head queue)))
  queue)


(defun queue-dequeue (queue)
  "
PRE:     (queue-p queue)
         l=(queue-length queue)
         f=(queue-first-element queue)
POST:    l>0 ==> l-1=(queue-length queue)
         l=0 ==> 0=(queue-length queue)
RETURN:  f
"
  (assert (queue-p queue))
  (prog1 (pop (queue-head queue))
    (when (null (queue-head queue))
      (setf (queue-tail queue) nil))))


(defun queue-requeue (queue element)
  "
DO:      Insert the element at the beginning of the queue.
PRE:     (queue-p queue)
         l=(queue-length queue)
POST:    (eq (queue-first-element queue) element)
         (queue-p queue),
         l+1=(queue-length queue)
RETURN:  queue
"
  (assert (queue-p queue))
  (push element (queue-head queue))
  (when (null (queue-tail queue))
    (setf (queue-tail queue) (queue-head queue)))
  queue)


(defun queue-test ()
  "
DO:     Test the queue data type. Insert test log at the point.
"
  (let (q)
    (flet ((check
               (q)
             (queue-invariant q)
             (if (not (queue-p q))
                 (format t "   NOT A QUEUE !~%~S~%" q)
                 (progn
                   (format t "   Length=~2D~%" (queue-length q))
                   (when (< 0 (queue-length q))
                     (format t "      Head=~S~%      Tail=~S~%" 
                             (queue-first-element q)
                             (queue-last-element q))
                     "")
                   (format t "   Queue=~S~%" q) ))))

      (format t   "Creating a queue~%")
      (setq q (make-queue))
      (check  q)

      (format t   "Dequeuing empty queue~%")
      (format t   "~S~%" (queue-dequeue q))
      (check  q)

      (format t   "Enqueuing...~%")
      (queue-enqueue q '(:first))
      (check  q)

      (format t   "Enqueuing...~%")
      (queue-enqueue q '(:second))
      (check  q)

      (format t   "Enqueuing...~%")
      (queue-enqueue q '(:third))
      (check  q)

      (format t   "Enqueuing...~%")
      (queue-enqueue q '(:fourth))
      (check  q)

      (format t   "Requeuing...~%")
      (queue-requeue q '(:zeroeth))
      (check  q)

      (while (< 0 (queue-length q))
        (format t  "Dequeuing queue~%")
        (format t  "~S~%" (queue-dequeue q))
        (check  q))

      (format t   "Requeuing empty queue...~%")
      (queue-requeue q '(:first))
      (check  q)

      (format t   "Requeuing...~%")
      (queue-requeue q '(:second))
      (check  q)

      (format t   "Enqueuing...~%")
      (queue-enqueue q '(:last))
      (check  q)

      (while (< 0 (queue-length q))
        (format t   "Dequeuing queue~%")
        (format t  "~S~%" (queue-dequeue q))
        (check  q))
      ))) ;;QUEUE-TEST


;;;; queue.lisp                       --                     --          ;;;;


