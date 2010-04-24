;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               message-queue.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Abstract interface to a message queue.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-01-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2005
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.MESSAGE-QUEUE"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.QUEUE")
  (:EXPORT "RECEIVE" "SEND" "MESSAGE-QUEUE")
  (:DOCUMENTATION "
    Abstract interface to a message queue.

    Copyright Pascal J. Bourguignon 2005 - 2005
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.MESSAGE-QUEUE")


(defclass message-queue ()
  ((messages :initform (make-queue)
             :type queue
             :documentation "The messages enqueued."))
  (:documentation "An abstract message queue."))


(defmethod print-object ((self message-queue) stream)
  (PRINT-UNREADABLE-OBJECT (SELF STREAM :TYPE T :IDENTITY T)
    (FORMAT STREAM ":messages ~S " (slot-value SELF 'messages)))
  self)


(defgeneric send    (mqueue message)
  (:documentation "Enqueues the MESSAGE into the message queue MQUEUE."))


(defgeneric receive (mqueue)
  (:documentation
   "If there's a message available in the queue MQUEUE,
    then dequeues it and returns (values message t)
    else returns (values nil nil)."))


(defmethod send ((self message-queue) message)
  (queue-enqueue (slot-value self 'messages) message)
  (values))


(defmethod receive ((self message-queue))
  (if (queue-empty-p (slot-value self 'messages))
      (values nil nil)
      (values (queue-dequeue (slot-value self 'messages)) t)))


;;;; message-queue.lisp               --                     --          ;;;;
