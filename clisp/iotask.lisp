;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               iotask.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Encapsulates clisp socket-status.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-31 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
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

(DEFINE-PACKAGE "COM.INFORMATIMAGO.CLISP.IOTASK"
  (:NICKNAMES "IOTASK")
  (:DOCUMENTATION
   "This package exports a sheduler encapsulating clisp SOCKET:SOCKET-STATUS
    which itself encapsulate select(2)/poll(2).

    Copyright Pascal Bourguignon 2005 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:FROM "COMMON-LISP"                           :IMPORT :ALL)
  (:USE  "COM.INFORMATIMAGO.COMMON-LISP.ECMA048")
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING"  :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.LIST"    :IMPORT :ALL)
  (:EXPORT
   "IOTASK" "IOTASK-ENQUEUE" "IOTASK-ENQUEUE-STREAM" "IOTASK-DEQUEUE"
   "IOTASK-POLL" "IOTASK-SCHEDULE"
   "MAKE-BUFFERED-DISCIPLINE" "MAKE-KEYBOARD-DISCIPLINE"))


(defclass iotask ()
  ((stream        :accessor iotask-stream        :initarg :stream)
   (process-event :accessor iotask-process-event :initarg :process-event)
   (name          :accessor iotask-name          :initarg :name)
   (stdin  :accessor iotask-stdin  :initarg :stdin  :initform *standard-input*)
   (stdout :accessor iotask-stdout :initarg :stdout :initform *standard-output*)
   (stderr :accessor iotask-stderr :initarg :stderr :initform *error-output*)
   (query  :accessor iotask-query  :initarg :query  :initform *query-io*)
   (alarm-time    :accessor iotask-alarm-time    :initarg :alarm-time
                  :documentation
                  "The next run-time an :alarm event should be posted.
                   (in INTERNAL-TIME-UNITS-PER-SECOND units)")
   (alarm-period  :accessor iotask-alarm-period  :initarg :alarm-period
                  :documentation
                  "The period run-time an :alarm event should be posted.
                   (in INTERNAL-TIME-UNITS-PER-SECOND units)")))
(defclass iotask-wait    () ())
(defclass iotask-no-wait () ())


(defmethod initialize-instance ((task iotask) &rest args)
  (declare (ignore args))
  (call-next-method)
  (handler-case (socket:socket-status (iotask-stream task) 0)
    (error     ()                           (change-class task 'iotask-no-wait))
    (:no-error (s n) (declare (ignore s n)) (change-class task 'iotask-wait)))
  task)


(defvar *iotasks*        '()
  "List of IOTASK instances that are scheduled in the pool loop.")
(defvar *iotask-wait*    '()
  "Sublist of *iotask* which can be handled by socket:socket-wait.")
(defvar *iotask-no-wait* '() 
  "Sublist of *iotask* which cannot be handled by socket:socket-wait.")

;; INVARIANT:
;; (assert (null (intersection *iotask-wait* *iotask-no-wait*)))
;; (assert (set-equal *iotasks* (union *iotask-wait* *iotask-no-wait*)))


(defun iotask-enqueue (task)
  (push task *iotasks*)
  (handler-case (socket:socket-status (iotask-stream task) 0)
    (error     ()                           (push task *iotask-no-wait*))
    (:no-error (s n) (declare (ignore s n)) (push task *iotask-wait*))))


(defun iotask-enqueue-stream (stream process-event
                              &key name alarm-time alarm-period)
  (iotask-enqueue (make-instance 'iotask
                    :stream stream
                    :stdin  stream
                    :stdout stream
                    :stderr stream
                    :query  stream
                    :process-event process-event
                    :name          name
                    :alarm-time    alarm-time
                    :alarm-period  alarm-period)))


(defun iotask-dequeue (task)
  (setf *iotasks*        (delete task *iotasks*))
  (setf *iotask-wait*    (delete task *iotask-wait*))
  (setf *iotask-no-wait* (delete task *iotask-no-wait*)))


(defun iotask-poll (&optional (timeout 0.0))
  ;; TODO: implement the :alarm event.
  (map nil 
       (lambda (task status)
         (when status
           (let ((*standard-input*  (iotask-stdin  task))
                 (*standard-output* (iotask-stdout task))
                 (*error-output*    (iotask-stderr task))
                 (*query-io*        (iotask-query  task)))
             (funcall (iotask-process-event task) task status))))
       *iotask-no-wait*
       (mapcar
        (lambda (task)
          (let ((stream (iotask-stream task)))
            (cond
              ((input-stream-p stream)  (cond
                                          ((listen stream)          :input)
                                          ((output-stream-p stream) :output)
                                          (t                        nil)))
              ((output-stream-p stream) :output)
              (t  nil))))
        *iotask-no-wait*))
  (map nil
       (lambda (task status)
         (when status
           (let ((*standard-input*  (iotask-stdin  task))
                 (*standard-output* (iotask-stdout task))
                 (*error-output*    (iotask-stderr task))
                 (*query-io*        (iotask-query  task)))
             (funcall (iotask-process-event task) task status))))
       *iotask-wait*
       (socket:socket-status
        (mapcar (function iotask-stream) *iotask-wait*) timeout)))


(defun iotask-schedule ()
  (loop while *iotasks* do (iotask-poll 1.0)))


(defun make-buffered-discipline (process-input)
  "process-input discipline to be used on buffered input streams."
  (lambda (task event)
    (when (member event '(:input :error))
      (funcall process-input task (read-line (iotask-stream task))))))


(defun make-keyboard-discipline (process-input)
  "process-input discipline to be used on clisp *keyboard-input*:
   buffer up a line before forwarding to process-input."
  (let ((buffer (make-array '(128) :element-type 'character
                            :fill-pointer 0
                            :adjustable t)))
    (lambda (task event)
      (when (eq :input event)
        (let* ((ich (read-char (iotask-stream task)))
               (ch  (system::input-character-char ich)))
          (cond 
            ((null ch))
            ((= (char-code ch) ecma048:CR)
             (terpri)
             (finish-output)
             (funcall process-input 
                      task (subseq buffer 0 (fill-pointer buffer)))
             (setf (fill-pointer buffer) 0))
            ((or (= (char-code ch) ecma048:BS) (= (char-code ch) ecma048::DEL))
             (when (< 0 (fill-pointer buffer))
               (princ (code-char ecma048:BS))
               (princ " ")
               (princ (code-char ecma048:BS))
               (finish-output)
               (decf (fill-pointer buffer))))
            (t
             (princ ch)
             (finish-output)
             (vector-push ch buffer))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;