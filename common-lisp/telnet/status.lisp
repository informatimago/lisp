;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               status.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements the STATUS telnet option.
;;;;    RFC 859 STATUS
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "COM.INFORMATIMAGO.COMMON-LISP.TELNET")


(defclass status (option)
  ()
  (:documentation "

The STATUS Telnet Option.

Usage:

    (defvar nvt (make-instance 'network-virtual-terminal))
    (option-register-class nvt :status 'status)

    (enable-option nvt :status :us)
    (loop :while (option-negotiating-p nvt :status)
          :do (when (received-message buffer)
                (receive nvt buffer)))
    ;; force sending:
    (send-status (get-option nvt :status) nvt)
    ;; the status option sends a status automatically upon request.

or:

    (enable-option nvt :status :him)
    (loop :while (option-negotiating-p nvt :status)
          :do (when (received-message buffer)
                (receive nvt buffer)))
    ;; the request is optional, the remote may send statuses spontaneously.
    (send-status-request (get-option nvt :status) nvt) 
    (loop :do (when (received-message buffer)
                (receive nvt buffer)))
    ;; the RECEIVE-OPTION method of the up-sender is called when a
    ;; status is received.

"))


(defmethod send-status-request ((opt status) nvt)
  "Send a STATUS SEND message."
  (if (option-enabled-p nvt :status :him)
      (send-raw-bytes nvt (vector IAC SB STATUS SEND IAC SE))
      (error 'telnet-option-error
             :nvt nvt
             :option opt
             :format-control "Option DO STATUS not enabled, can't send a STATUS request."
             :format-arguments '())))

(defmethod send-status ((opt status) nvt)
  "Send a STATUS IS message."
  (if (option-enabled-p nvt :status :us)
      (let ((buffer (make-array 8 :fill-pointer 0 :adjustable t :element-type 'ubyte)))
        (flet ((send (&rest bytes)
                 (dolist (byte bytes)
                   (vector-push-extend byte buffer (length buffer)))))
          (send iac)
          (dolist (opt (nvt-options nvt))
            (when (opt-enabled-p opt :us)
              (send WILL (option-code opt))
              (when (= SE (option-code opt))
                (send SE)))
            (when (opt-enabled-p opt :him)
              (send DO (option-code opt))
              (when (= SE (option-code opt))
                (send SE)))
            (when (opt-enabled-p opt)
              (encode-subnegotiation opt buffer)))
          (send SE))
        (send-raw-bytes nvt buffer))
      (error 'telnet-option-error
             :nvt nvt
             :option opt
             :format-control "Option WILL STATUS not enabled, can't send a STATUS."
             :format-arguments '())))


(defmethod receive-status ((opt status) nvt bytes &key (start 0) (end (length bytes)))
  "Receive the STATUS IS message in (subseq bytes start end).
We send a RECEIVE-OPTION message to the UP-SENDER with a value such as:
    (:status (:will optcode) (:do optcode) (:sb optcode …) (:error cmd optcode) …)
"
  (receive-option (up-sender nvt)
                  opt
                  (cons :status
                        (loop
                          :with i = (+ start 4)
                          :while (< i (- end 2))
                          :collect (case (aref bytes i)
                                     (#.DO
                                      (prog1
                                          (list :do (option-name-for-code (aref bytes (incf i))))
                                        (incf i)))
                                     (#.WILL
                                      (prog1
                                          (list :will (option-name-for-code (aref bytes (incf i))))
                                        (incf i)))
                                     (#.SB
                                      (let ((opt (init-option-code nvt (aref bytes (1+ i))))
                                            (end (loop
                                                   :with j = (+ i 2)
                                                   :while (< j (- end 2))
                                                   :do (case (aref bytes j)
                                                         (#.SE (if (= SE (aref bytes (1+ j)))
                                                                   (incf j 2)
                                                                   (return (1+ j))))
                                                         (#.IAC (if (= IAC (aref bytes (1+ j)))
                                                                    (incf j 2)
                                                                    (return j)))
                                                         (otherwise (incf j))))))
                                        (prog1 (decode-subnegotiation opt bytes :start i :end j)
                                          (setf i j))))
                                     (otherwise
                                      (warn 'telnet-option-warning
                                            :nvt nvt
                                            :option opt
                                            :format-control "Invalid subnegotiation command: ~A"
                                            :format-arguments (list (aref bytes i)))
                                      (prog1
                                          ;; assuming there's an option code following…
                                          (list :error (aref bytes i) (aref bytes (incf i)))
                                        (incf i))))))))


(defmethod receive-subnegotiation ((opt status) nvt bytes &key (start 0) (end (length bytes)))
  "Parses the STATUS subnegotiation.
The NVT has already parsed 'IAC SB STATUS' and 'IAC SE'.
IAC SB STATUS SEND IAC SE
IAC SB STATUS IS … IAC SE
"
  (let ((len    (- end start))
        (subcmd (aref bytes (+ start 3))))
    (cond
      ((and (= len 6) (= subcmd TQ-SEND))
       (if (option-enabled-p nvt :status :us)
           (send-status opt nvt)
           (cerror "Ignore the status request."
                   'telnet-option-error
                   :nvt nvt
                   :option opt
                   :format-control "Option DO STATUS not enabled, can't answer to STATUS SEND."
                   :format-arguments '())))
      ((and (< 6 len) (= subcmd TQ-IS))
       (if (option-enabled-p nvt :status :him)
           (receive-status opt nvt bytes :start start :end end)
           (cerror "Ignore the status."
                   'telnet-option-error
                   :nvt nvt
                   :option opt
                   :format-control "Option WILL STATUS not enabled, can't process STATUS IS."
                   :format-arguments '())))
      (t
       #|otherwise ignore it; is it what should be done?|#
       (warn 'telnet-option-warning
             :nvt nvt
             :option opt
             :format-control "Invalid subnegotiation message: ~S"
             :format-arguments (list (subseq bytes start end)))))))


;;;; THE END ;;;;
