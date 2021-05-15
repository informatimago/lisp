;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               telnet-repl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a Telnet REPL server.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-13 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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

(in-package "COM.INFORMATIMAGO.CLEXT.TELNET.REPL")




;;;
;;; The REPL:
;;;


;; TODO: Securize the *readtable* and the *package* (cf. something like ibcl)

(defun make-repl-readtable (cn)
  (copy-readtable))

(defun make-repl-package   (cn)
  (mkupack :name (format nil "USER-~D" cn)
           :use '("COMMON-LISP")))

(defun telnet-repl (stream cn must-stop-it)
  (let ((*terminal-io*     stream)
        (*debug-io*        (make-synonym-stream '*terminal-io*))
        (*query-io*        (make-synonym-stream '*terminal-io*))
        (*standard-input*  (stream-input-stream  stream))
        (*standard-output* (stream-output-stream stream))
        (*trace-output*    (stream-output-stream stream))
        (*error-output*    (stream-output-stream stream))
        (package           (make-repl-package   cn))
        (*readtable*       (make-repl-readtable cn))
        (*package*         package)
        (com.informatimago.common-lisp.interactive.interactive::*repl-history*
          (make-array 128 :adjustable t :fill-pointer 0)))
    (catch 'repl
      (unwind-protect
           (let ((+eof+   (gensym))
                 (hist    1))
             (set-macro-character #\! (function repl-history-reader-macro) t)
             (loop
                (handler-case
                    (progn
                      (format *terminal-io* "~%~A[~D]> " (package-name *package*) hist)
                      (finish-output *terminal-io*)
                      (com.informatimago.common-lisp.interactive.interactive::%rep +eof+ hist))
                  (error (err)
                    (format stream "~%Fatal Error: ~A~%" err)
                    (finish-output stream)
                    (throw 'repl)))))
        (delete-package package)))))


;;;
;;; Client
;;;

(defclass repl-client ()
  ((name              :initarg  :name            :reader name)
   (thread            :initarg  :thread          :reader repl-client-thread
                      :initform nil)
   (number            :initarg  :number          :reader repl-client-number)
   (socket            :initarg  :socket          :reader repl-client-socket)
   (banner-function   :initarg  :banner-function :reader banner-function)
   (login-function    :initarg  :login-function  :reader login-function)
   (repl-function     :initarg  :repl-function   :reader repl-function)
   (stop-closure      :initform nil              :reader stop-closure)
   (terminate-closure :initform nil              :reader terminate-closure)))

(defun run-client-loop (client)
  (with-telnet-on-stream (stream (socket-stream (repl-client-socket client)))
    (when (and (not (stop-closure client))
               (banner-function client))
      (funcall (banner-function client) stream (repl-client-number client) (name client)))
    (when (and (not (stop-closure client))
               (or (null    (login-function client))
                   (funcall (login-function client) stream)))
      (funcall (repl-function client) stream (repl-client-number client) (stop-closure client)))))

(defmethod initialize-instance :after ((client repl-client) &key &allow-other-keys)
  (let ((stop nil))
    (setf (slot-value server 'thread)
          (make-thread (lambda ()
                         (unwind-protect (run-client-loop client)
                           (funcall (terminate-closure client))))
                       :name (name client)))))


;;;
;;; Server
;;;

;;; The server listens on one TCP port (one or multiple interfaces).
;;; When receiving a connection it creates a new client thread to handle it.
;;; Once max-clients are active, it waits for clients to stop before
;;; handling a new client connection.


(defclass repl-server ()
  ((name            :initarg  :name            :reader name)
   (thread          :initarg  :thread          :reader repl-server-thread
                    :initform nil)
   (lock            :initform nil              :reader repl-server-lock)
   (more-clients    :initform nil              :reader repl-server-more-clients)
   (stop-closure    :initform nil)
   (banner-function :initarg  :banner-function :reader banner-function)
   (login-function  :initarg  :login-function  :reader login-function)
   (repl-function   :initarg  :repl-function   :reader repl-function)
   (port            :initarg  :port            :reader repl-server-port)
   (interface       :initarg  :interface       :reader repl-server-interface)
   (max-clients     :initarg  :max-clients     :reader repl-server-max-clients)
   (clients         :initform '())))

(defmethod %clean-up ((server repl-server))
  (loop :for slot :in '(thread lock more-clients stop-closure)
        :do (setf (slot-value server slot) nil)))

(defmethod %add-client ((server repl-server) new-client)
  (push new-client (slot-value server 'clients)))

(defmethod remove-client ((server repl-server) old-client)
  (with-lock-held ((repl-server-lock server))
    (socket-close (repl-client-socket old-client))
    (setf (slot-value server 'clients)
          (delete old-client (slot-value server 'clients)))
    (condition-notify (repl-server-more-client server))))

(defmethod wait-for-free-client-slot ((server repl-server))
  (with-lock-held ((repl-server-lock server))
    (loop :while (and (< (repl-server-max-clients server)
                         (length (slot-value server 'clients)))
                      (not (funcall must-stop-p)))
          :do 
          :do (condition-wait (repl-server-mode-clients server)
                              (repl-server-lock server)
                              1 #| check for stop |#))))

(defun run-server-loop (server)
  (with-socket-listener (server-socket (repl-server-interface server)
                                       (repl-server-port server)
                                       :element-type '(unsigned-byte 8)
                                       :timeout 1)
    (loop
      :for cn :from 1
      :for client-socket := (socket-accept server-socket)
      :when client-socket 
        :do (with-lock-held ((repl-server-lock server))
              (let ((client (make-instance
                             'repl-client
                             :name (format nil "~A Client #~D"
                                           (name server) cn)
                             :number cn
                             :socket client-socket
                             :banner-function (banner-function server)
                             :login-function (login-function server)
                             :repl-function  (repl-function server)
                             :stop-closure   (lambda ()
                                               (funcall (slot-value server 'stop-closure)))
                             :terminate-closure (lambda (client)
                                                  (remove-client server client)))))
                (%add-client server client)))
      :do (wait-for-free-client-slot server)
      :until (funcall must-stop-p)
      :finally (loop
                 :while (slot-value server 'clients)
                 :do (wait-for-free-client-slot server))
               (return cn))))

(defmethod initialize-instance :after ((server repl-server) &key &allow-other-keys)
  (let ((stop nil))
    (setf (slot-value server 'stop-closure)
          (lambda (&optional stop-it)
            (when stop-it
              (setf stop t))
            stop)
          
          (slot-value server 'thread)
          (make-thread (lambda () (run-server-loop server))
                       :name (format nil "~A Server" (name server))))))

(defun start-repl-server (&key (name "Telnet REPL")
                            (port 10023) (interface "0.0.0.0")
                            (max-clients 10)
                            (banner-function nil)
                            (login-function nil)
                            (repl-function (function telnet-repl)))
  "Starts a Telnet REPL server thread, listening for incoming
connections on the specified PORT, and on the specified INTERFACE.
At most MAX-CLIENTS at a time are allowed connected.

The clients will start running the BANNER-FUNCTION which takes a
stream, a client number and a client name.

Then the LOGIN-FUNCTION is called with a stream. It should return true
to allow the connection to go on.

If the LOGIN-FUNCTION returns true, then the REPL-FUNCTION is called
with the stream, the client number, and a stop closure that should be
called periodically to know when the REPL should be stopped.


RETURN: The server instance.  Several servers may be run on different
ports (with possibly different functions).
"
  (make-instance 'repl-server
                 :name name
                 :banner-function banner-function
                 :login-function login-function
                 :repl-function repl-function
                 :port port
                 :interface interface
                 :max-clients max-clients))

(defun stop-repl-server (server)
  "Stops the REPL server.  It make take some time top shut down all
the REPL clients, but the REPL server should not accept new
connections right away."
  (when (repl-server-thread server)
    (funcall (repl-server-stop-closure server) t)
    (join-thread (repl-server-thread server))
    (%clean-up server)))

;;;; THE END ;;;;