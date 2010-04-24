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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2001 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.QUEUE"
  (:USE "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
  (:EXPORT "QUEUE-TEST" "QUEUE-INVARIANT" "QUEUE-DELETE" "QUEUE-REQUEUE"
           "QUEUE-DEQUEUE" "QUEUE-ENQUEUE" "QUEUE-LAST-ELEMENT" "QUEUE-FIRST-ELEMENT"
           "QUEUE-EMPTY-P" "QUEUE-LENGTH" "MAKE-QUEUE" "QUEUE")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "WHILE")
  (:DOCUMENTATION
   "This module exports a queue type. This is a structure optimized for
    FIFO operations, keeping a pointer to the head and the tail of a list.
    
    Copyright Pascal J. Bourguignon 2001 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.QUEUE")




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


(DEFSTRUCT (QUEUE (:CONSTRUCTOR %MAKE-QUEUE))
  (HEAD NIL :TYPE LIST)
  (TAIL NIL :TYPE LIST))


(DEFUN QUEUE-INVARIANT (QUEUE)
  (ASSERT (QUEUE-P QUEUE))
  (ASSERT (OR (AND (NULL (QUEUE-HEAD QUEUE))  (NULL (QUEUE-TAIL QUEUE)))
              (AND (QUEUE-HEAD QUEUE) (QUEUE-TAIL QUEUE))))
  (WHEN (QUEUE-HEAD QUEUE)
    (ASSERT (LIST-LENGTH (QUEUE-HEAD QUEUE))) ; not a circular list.
    (ASSERT (SEARCH (QUEUE-TAIL QUEUE) (QUEUE-HEAD QUEUE) :TEST (FUNCTION EQ)))
    (ASSERT (NULL (CDR (QUEUE-TAIL QUEUE))))))


(DEFUN MAKE-QUEUE () (%MAKE-QUEUE))


(DEFUN QUEUE-LENGTH (QUEUE)
  "
PRE:     (queue-p queue)
RETURN:  The number of elements in the queue.
"
  (ASSERT (QUEUE-P QUEUE))
  (LENGTH (QUEUE-HEAD QUEUE))) ;;QUEUE-LENGTH


(DEFUN QUEUE-EMPTY-P (QUEUE)
  "
RETURN:  (= 0 (queue-length queue))
"
  (ASSERT (QUEUE-P QUEUE))
  (NULL (QUEUE-HEAD QUEUE)))


(DEFUN QUEUE-FIRST-ELEMENT (QUEUE)
  "
PRE:     (queue-p queue)
RETURN:  The first element of the queue.
"
  (ASSERT (QUEUE-P QUEUE))
  (FIRST (QUEUE-HEAD QUEUE)))


(DEFUN QUEUE-LAST-ELEMENT (QUEUE)
  "
PRE:     (queue-p queue)
RETURN:  The last element of the queue.
"
  (ASSERT (QUEUE-P QUEUE))
  (FIRST (QUEUE-TAIL QUEUE)))


(DEFUN QUEUE-ENQUEUE  (QUEUE ELEMENT)
  "
PRE:     (queue-p queue)
         l=(queue-length queue)
POST:    (eq (queue-last-element queue) element),
         (queue-p queue),
         l+1=(queue-length queue)
RETURN:  queue
"
  (ASSERT (QUEUE-P QUEUE))
  ;; (car q) = head      (cdr q) = tail
  (IF (QUEUE-HEAD QUEUE)
      (PROGN
        ;; There's already an element, just add to the tail.
        (SETF (CDR (QUEUE-TAIL QUEUE)) (CONS ELEMENT NIL))
        (SETF (QUEUE-TAIL QUEUE)       (CDR (QUEUE-TAIL QUEUE))))
      (PROGN
        ;; The queue is empty, let's set the head.
        (SETF (QUEUE-HEAD QUEUE) (CONS ELEMENT NIL))
        (SETF (QUEUE-TAIL QUEUE) (QUEUE-HEAD QUEUE))))
  QUEUE)


(defun queue-delete (queue element &key (test (function eql)))
  "
POST:    (not (member element queue :test test))
RETURN:  queue
"
  (setf (queue-head queue) (delete element  (queue-head queue) :test test)
        (queue-tail queue) (last (queue-head queue)))
  queue)


(DEFUN QUEUE-DEQUEUE (QUEUE)
  "
PRE:     (queue-p queue)
         l=(queue-length queue)
         f=(queue-first-element queue)
POST:    l>0 ==> l-1=(queue-length queue)
         l=0 ==> 0=(queue-length queue)
RETURN:  f
"
  (ASSERT (QUEUE-P QUEUE))
  (PROG1 (POP (QUEUE-HEAD QUEUE))
    (WHEN (NULL (QUEUE-HEAD QUEUE))
      (SETF (QUEUE-TAIL QUEUE) NIL))))


(DEFUN QUEUE-REQUEUE (QUEUE ELEMENT)
  "
DO:      Insert the element at the beginning of the queue.
PRE:     (queue-p queue)
         l=(queue-length queue)
POST:    (eq (queue-first-element queue) element)
         (queue-p queue),
         l+1=(queue-length queue)
RETURN:  queue
"
  (ASSERT (QUEUE-P QUEUE))
  (PUSH ELEMENT (QUEUE-HEAD QUEUE))
  (WHEN (NULL (QUEUE-TAIL QUEUE))
    (SETF (QUEUE-TAIL QUEUE) (QUEUE-HEAD QUEUE)))
  QUEUE)


(DEFUN QUEUE-TEST ()
  "
DO:     Test the queue data type. Insert test log at the point.
"
  (LET (Q)
    (FLET ((CHECK
               (Q)
             (QUEUE-INVARIANT Q)
             (IF (NOT (QUEUE-P Q))
                 (FORMAT T "   NOT A QUEUE !~%~S~%" Q)
                 (PROGN
                   (FORMAT T "   Length=~2D~%" (QUEUE-LENGTH Q))
                   (WHEN (< 0 (QUEUE-LENGTH Q))
                     (FORMAT T "      Head=~S~%      Tail=~S~%" 
                             (QUEUE-FIRST-ELEMENT Q)
                             (QUEUE-LAST-ELEMENT Q))
                     "")
                   (FORMAT T "   Queue=~S~%" Q) ))))

      (FORMAT T   "Creating a queue~%")
      (SETQ Q (MAKE-QUEUE))
      (CHECK  Q)

      (FORMAT T   "Dequeuing empty queue~%")
      (FORMAT T   "~S~%" (QUEUE-DEQUEUE Q))
      (CHECK  Q)

      (FORMAT T   "Enqueuing...~%")
      (QUEUE-ENQUEUE Q '(:FIRST))
      (CHECK  Q)

      (FORMAT T   "Enqueuing...~%")
      (QUEUE-ENQUEUE Q '(:SECOND))
      (CHECK  Q)

      (FORMAT T   "Enqueuing...~%")
      (QUEUE-ENQUEUE Q '(:THIRD))
      (CHECK  Q)

      (FORMAT T   "Enqueuing...~%")
      (QUEUE-ENQUEUE Q '(:FOURTH))
      (CHECK  Q)

      (FORMAT T   "Requeuing...~%")
      (QUEUE-REQUEUE Q '(:ZEROETH))
      (CHECK  Q)

      (WHILE (< 0 (QUEUE-LENGTH Q))
        (FORMAT T  "Dequeuing queue~%")
        (FORMAT T  "~S~%" (QUEUE-DEQUEUE Q))
        (CHECK  Q))

      (FORMAT T   "Requeuing empty queue...~%")
      (QUEUE-REQUEUE Q '(:FIRST))
      (CHECK  Q)

      (FORMAT T   "Requeuing...~%")
      (QUEUE-REQUEUE Q '(:SECOND))
      (CHECK  Q)

      (FORMAT T   "Enqueuing...~%")
      (QUEUE-ENQUEUE Q '(:LAST))
      (CHECK  Q)

      (WHILE (< 0 (QUEUE-LENGTH Q))
        (FORMAT T   "Dequeuing queue~%")
        (FORMAT T  "~S~%" (QUEUE-DEQUEUE Q))
        (CHECK  Q))
      ))) ;;QUEUE-TEST


;;;; queue.lisp                       --                     --          ;;;;


