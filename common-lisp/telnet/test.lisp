;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    XXX
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.TELNET")


(defmethod send-binary :before ((self network-virtual-terminal) bytes)
  (format *trace-output* "~S send-binary ~S~%" self bytes)
  (force-output *trace-output*))
(defmethod send-text :before ((self network-virtual-terminal) text)
  (format *trace-output* "~S send-text ~S~%" self text)
  (force-output *trace-output*))
(defmethod send-control :before ((self network-virtual-terminal) control)
  (format *trace-output* "~S send-control ~S~%" self control)
  (force-output *trace-output*))
(defmethod receive :before ((self network-virtual-terminal) bytes &key start end)
  (format *trace-output* "~S receive ~S~%" self (subseq bytes start end))
  (force-output *trace-output*))



(defclass layer ()
  ((name :initform "Anonymous Layer"
         :initarg :name
         :reader layer-name)
   (nvt :accessor layer-nvt)))

(defmethod print-object ((self layer) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~S" (layer-name self)))
  self)



(defclass up-layer (layer)
  ((wanted-options :initform '() :initarg :wanted-options :accessor wanted-options)))

(defmethod want-option-p ((self up-layer) option-name)
  (find option-name (wanted-options self)))

(defmethod receive-option  ((self up-layer) option value)
  (format *trace-output* "~S received option ~S = ~S~%" self option value)
  (force-output *trace-output*)
  (list (option-name option) value))

(defmethod receive-binary  ((self up-layer) bytes &key (start 0) (end (length bytes)))
  (let ((packet (subseq bytes start end)))
    (format *trace-output* "~S received binary: ~S~%" self packet)
    (force-output *trace-output*)
    packet))

(defmethod receive-text    ((self up-layer) text)
  (format *trace-output* "~S received text: ~S~%" self text)
  (force-output *trace-output*)
  text)

(defmethod receive-control ((self up-layer) control)
  (format *trace-output* "~S received control ~S~%" self control)
  (force-output *trace-output*)
  (case control
    (:are-you-there
     (send-text (layer-nvt self) (format nil "~%~A is here.~%" (layer-name self)))))
  #-(and) (:are-you-there :abort-output :interrupt-process :go-ahead
                          :erase-line :erase-character
                          :break :cr :ff :vt :lf :ht :bs :bel :nul
                          :end-of-record)
  control)



(defclass down-layer (layer)
  ((remote :initform nil :accessor layer-remote)))

(defmethod layer-receive ((self down-layer) packet)
  (format *trace-output* "~S receiving ~S~%" self packet)
  (force-output *trace-output*)
  (receive (layer-nvt self) packet :start 0 :end (length packet)))

(defmethod send ((self down-layer) bytes  &key (start 0) (end (length bytes)))
  (let ((packet (subseq bytes start end)))
    (format *trace-output* "~S sending ~S~%" self packet)
    (force-output *trace-output*)
    (when (layer-remote self)
      (layer-receive (layer-remote self) packet))))



(defparameter *wanted-options* '(:transmit-binary :echo :suppress-go-ahead :end-of-record :status))

(defparameter *ulc*    (make-instance 'up-layer   :name "Client UP" :wanted-options *wanted-options*))
(defparameter *uls*    (make-instance 'up-layer   :name "Server UP" :wanted-options *wanted-options*))
(defparameter *dlc*    (make-instance 'down-layer :name "Client DOWN"))
(defparameter *dls*    (make-instance 'down-layer :name "Server DOWN"))
(defparameter *client* (make-instance 'network-virtual-terminal
                                      :name "CLIENT NVT" :client t
                                      :up-sender *ulc* :down-sender  *dlc*))
(defparameter *server* (make-instance 'network-virtual-terminal
                                      :name "SERVER NVT" :client nil
                                      :up-sender *uls* :down-sender  *dls*))

(setf (layer-nvt *ulc*) *client*
      (layer-nvt *dlc*) *client*
      (layer-nvt *uls*) *server*
      (layer-nvt *dls*) *server*
      (layer-remote *dls*) *dlc*
      (layer-remote *dlc*) *dls*)

(print-hashtable (slot-value *client* 'options))

;; (send-binary  (layer-nvt self) #(1 2 3 4))
;; (send-text    (layer-nvt self) "Hello")
;; (send-control (layer-nvt self) :bel)


(send-text    (layer-nvt *ulc*) "Hello")
(send-control (layer-nvt *ulc*) :bel)
(send-binary  (layer-nvt *ulc*) #(1 2 3 4))

(enable-option (layer-nvt *ulc*) :transmit-binary :us)
(loop :while (option-negotiating-p (layer-nvt *ulc*) :transmit-binary))
(send-binary  (layer-nvt *ulc*) #(1 2 3 4))
(send-text    (layer-nvt *ulc*) "World!")
(send-control (layer-nvt *ulc*) :bel)

(send-control (layer-nvt *ulc*) :are-you-there)
(send-control (layer-nvt *ulc*) :interrupt-process)
(send-control (layer-nvt *ulc*) :abort-output)


(enable-option (layer-nvt *ulc*) :status :us)
(loop :while (option-negotiating-p (layer-nvt *ulc*) :status))
(send-status (get-option (layer-nvt *ulc*) :status) (layer-nvt *ulc*))
(send-status (get-option (layer-nvt *ulc*) :status) (layer-nvt *ulc*))

(get-option (layer-nvt *ulc*) :status)



(define-condition interrupt-signal-condition (condition)
  ()
  (:report "interrupt signal"))

(defun test/interrupt ()
  (let* ((iota (bt:make-thread (lambda ()
                                 (unwind-protect
                                      (loop
                                        :for i :from 1
                                        :do (sleep 1)
                                            (princ i) (princ " ")
                                            (finish-output))
                                   (princ "Done") (terpri)
                                   (finish-output)))
                               :name "iota runner")))
    (sleep 10)
    (bt:interrupt-thread iota
                         (function invoke-debugger)
                         (make-condition 'interrupt-signal-condition))
    (princ "Complete.") (terpri) (finish-output)))

