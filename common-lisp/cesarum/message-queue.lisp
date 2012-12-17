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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2012
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.MESSAGE-QUEUE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE")
  (:export "RECEIVE" "SEND" "MESSAGE-QUEUE")
  (:documentation "

A message queue, implemented using FIFI QUEUEs.


See also: COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2005 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.MESSAGE-QUEUE")


(defclass message-queue ()
  ((messages :initform (make-queue)
             :type queue
             :documentation "The messages enqueued."))
  (:documentation "An abstract message queue."))


(defmethod print-object ((self message-queue) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":messages ~S " (slot-value self 'messages)))
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
