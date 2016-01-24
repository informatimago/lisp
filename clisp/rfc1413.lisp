;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               rfc1413.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements an IDENT protocol client.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-31 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2016
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
(declaim (declaration also-use-packages))
(declaim (also-use-packages "SOCKET" "REGEXP" "COM.INFORMATIMAGO.CLISP.IOTASK"))
(defpackage "COM.INFORMATIMAGO.CLISP.RFC1413"
  (:nicknames "COM.INFORMATIMAGO.CLISP.IDENT" "IDENT-CLIENT" "RFC1413")
  (:documentation "Implements a ident protocol client.")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS")
  (:export "CHARSET-MAP" "GET-PORT-FOR-SERVICE" "BINARY-STREAM-LISTEN"
           "IDENT-PARSE-RESPONSE" "IDENT-FORMAT-REQUEST"
           "BATCH-REQUEST-IDENTS" "REQUEST-IDENT"
           "BATCH-REQUEST-IDENTS/IOTASK" "REQUEST-IDENT/IOTASK"))
;; TODO: get-port-for-service should move to a generic socket utility package.
(in-package "COM.INFORMATIMAGO.CLISP.RFC1413")


(defconstant +connection-timeout+    60) ; TODO: check RFC about this timeout.
(defconstant +wait-response-timeout+ 30)


(defun xor (a b)
  (or (and a (not b)) (and (not a) b)))

(defun get-real-time ()
  (/ (coerce (get-internal-real-time) 'double-float)
     internal-time-units-per-second))


;; (defparameter +charset-path+
;;   #P"PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;IANA-CHARACTER-SETS.DATA")
;; (defparameter *charset-map* nil)
;; 
;; 
;; (defun charset-map ()
;;   "Builds and returns the charset-map, which maps character set names
;;    to clisp charsets"
;;   (or *charset-map*
;;       (progn
;;         (setf *charset-map* (make-hash-table :test (function equalp)))
;;         (dolist (cs (read-character-sets +charset-path+))
;;           (let ((charsets
;;                  (mapcan
;;                   (lambda (name)
;;                     ;; map some character set names to charset names.
;;                     (multiple-value-bind (all utf bl)
;;                         (regexp:match "^UTF-\\(.*\\)\\([BL]\\)E$" name)
;;                       (when all
;;                         (setf name (format nil
;;                                      "UNICODE-~A-~:[LITTLE~;BIG~]-ENDIAN"
;;                                      (regexp:match-string name utf)
;;                                      (string-equal
;;                                       (regexp:match-string name bl)
;;                                       "B")))))
;;                     ;; find the character set name in the charset package
;;                     (let ((charset (find-symbol (string-upcase name) :charset)))
;;                       (if charset
;;                           (list charset)
;;                           (loop        ; not found: try with a prefix.
;;                              with u = (string-upcase name)
;;                              for l from (length u) downto 3
;;                              for n = (subseq u 0 l)
;;                              for c = (find-symbol n :charset)
;;                              do (when c (return (list c)))
;;                              finally (return nil)))))
;;                   cs)))
;;             ;; (print `(,cs --> ,charsets))
;;             (cond
;;               ((null charsets))         ; forget it
;;               ((null (cdr charsets))    ; map all to the one
;;                (dolist (name cs)
;;                  (setf (gethash name *charset-map*) (first charsets))))
;;               (t             ; oops!  map each to its own or the first
;;                (warn "One IANA character set maps to more than one ~
;;                          clisp CHARSET: ~S --> ~S" cs charsets)
;;                (dolist (name cs)
;;                  (setf (gethash name *charset-map*)
;;                        (or (find-symbol (string-upcase name) :charset)
;;                            (first charsets))))))))
;;         *charset-map*)))
;; 
;; 
;; (DEFUN get-charset (name)
;;   (gethash name (charset-map)))


(defun get-port-for-service (service)
  #+ #. (cl:if (cl:ignore-errors (cl:find-symbol "SERVICE" "POSIX"))
               '(and) '(or))
  (posix:service-port (posix:service service))
  #- #. (cl:if (cl:ignore-errors (cl:find-symbol "SERVICE" "POSIX"))
               '(and) '(or))
  (multiple-value-bind (name aliases port protocol)
      (socket:socket-service-port service)
    (declare (ignore name aliases protocol))
    port))


(defun binary-stream-listen (stream)
  (case (socket:socket-status stream 0)
    ((:input :io) t)
    (otherwise    nil)))


(defun read-line-in-buffer (stream buffer)
  "Read byte sequence into the buffer, adjusting the fill-pointer.
   When CR-LF is read, return the position of the eoln
   If the buffer becomes full before the CR-LF is read, then signal an error."
  ;; KEEP Synchronized with RECYCLE-LINE-BUFFER
  (loop
     while (binary-stream-listen stream)
     do (vector-push (read-byte stream) buffer))
  (let ((eoln (search #(13 10) buffer)))
    ;; (print `(:eoln ,eoln :buffer ,buffer))
    (cond
      (eoln (decf (fill-pointer buffer) 2))
      ((<= (array-dimension buffer 0) (fill-pointer buffer)) ; no eoln
       (error "ident server sent a line too long."))
      (t nil))))


(defun recycle-line-buffer (buffer)
  "Moves the bytes following the first eoln to the beginning of the buffer."
  ;; KEEP Synchronized with READ-LINE-IN-BUFFER
  ;; The current version doesn't read more than CR LF,
  ;; so we don't have anything to do.
  (setf (fill-pointer buffer) 0))


(defparameter +ch.cr+ (code-char 13))
(defparameter +ch.lf+ (code-char 10))
(defparameter +ch.comma+ (character ","))
(defparameter +comma+ 44 "ASCII CODE for COMMA")
(defparameter +colon+ 58 "ASCII CODE for COLON")


(defun ident-parse-response (buffer)
  (flet ((split-subparts (part process)
           (loop
              with subparts = '()
              for start = 0 then (1+ comma)
              for comma = (position +ch.comma+ part :start start)
              do (push (funcall process
                                (string-trim " " (subseq part start comma)))
                       subparts)
              while comma
              finally (return (nreverse subparts)))))
    (loop
       with parts = '()
       for start = 0 then (1+ colon)
       for colon = (position +colon+ buffer :start start)
       for count from 1
       do (push (cons start colon) parts)
       while (and colon (< count 3))
       finally
       (setf colon (or (and colon (1+ colon)) (length buffer)))
       (let ((str (ext:convert-string-from-bytes buffer charset:ascii
                                                 :end colon)))
         (setf parts (mapcar
                      (lambda (se process)
                        (split-subparts (subseq str (car se) (cdr se))
                                        process))
                      parts
                      (list (function identity)
                            (lambda (name) (intern name :keyword))
                            (function parse-integer))))
         (let ((cs (find-character-set (or (second (first parts)) "ASCII"))))
           (push (if cs
                     (ext:convert-string-from-bytes
                      buffer (cs-lisp-encoding cs) :start colon)
                     (subseq buffer colon))
                 parts))
         (return (nreverse parts))))))

#||
(defparameter ex0 (ext:convert-string-to-bytes
                   "6195, 23 : ERROR : NO-USER"
                   charset:ascii))
(defparameter ex1 (ext:convert-string-to-bytes
                   "6193, 23 : USERID : UNIX :stjohns"
                   charset:ascii))
(defparameter ex2 (ext:convert-string-to-bytes
                   "6193, 23 : USERID : UNIX,ISO-8859-1 :stétienne"
                   charset:iso-8859-1))
(defparameter ex3 (ext:convert-string-to-bytes
                   "6193, 23 : USERID : UNIX,ISO-8859-5 :Распутин"
                   charset:iso-8859-5))

(mapcar (function IDENT-PARSE-RESPONSE) (list ex0 ex1 ex2 ex3))
||#


(defun ident-format-request (request)
  (destructuring-bind (remote-port local-port) request
    (ext:convert-string-to-bytes
     (format nil "~A, ~A~C~C" remote-port local-port +ch.cr+ +ch.lf+)
     charset:ascii)))


(defun batch-request-idents (remote-host port-couples)
  "
RETURN: a list of responses from REMOTE-HOST ident server:
       for each port-couple
           ((remote-port local-port) (:USERID) (opsys [charset]) user-id)
       or: ((remote-port local-port) (:ERROR)  (error-type))
       or: ((remote-port local-port) (:TIMEOUT))
"
  (unless port-couples (return-from batch-request-idents nil))
  (let ((remote (socket:socket-connect (get-port-for-service "ident") remote-host
                                       :element-type '(unsigned-byte 8)
                                       :buffered nil)))
    ;;                No :timeout to get error when connection is refused.
    (unwind-protect
         (loop
            with inbuf  = (make-array '(1024) :element-type '(unsigned-byte 8)
                                      :initial-element 0 :fill-pointer 0)
            with state = :sending
            with start-wait = 0.0
            with results = '()
            with to-send    = port-couples
            with to-receive = port-couples
            do
            #+(or)
            (print `(:state ,state :start-wait ,start-wait :results ,results
                            :to-send ,to-send :to-receive ,to-receive))
            (case (socket:socket-status remote +wait-response-timeout+)
              ((nil)
               (cond
                 ((eq state :sending)
                  (setf state :waiting
                        start-wait (get-real-time)))
                 ((<= +wait-response-timeout+ (- (get-real-time) start-wait))
                  ;; time-out
                  (return-from batch-request-idents
                    (dolist (request to-receive (nreverse results))
                      (push (list (pop to-receive) '(:timeout)) results)))))
               (sleep 0.05))
              ((:input :io)             ; something to read
               (when (read-line-in-buffer remote inbuf)
                 (let ((result (ident-parse-response inbuf)))
                   (unless (equal (car to-receive) (car result))
                     (warn "Desynchronization ~S --> ~S"
                           (car to-receive) result))
                   (pop to-receive)
                   (push result results))
                 (unless to-receive (setf state :waiting))
                 (recycle-line-buffer inbuf)))
              ((:output)
               (cond
                 (to-send
                  (setf state :sending)
                  (write-sequence (ident-format-request (pop to-send))
                                  remote)
                  (finish-output remote))
                 ((and (eq state :waiting)
                       (<= +wait-response-timeout+
                           (- (get-real-time) start-wait)))
                  ;; time-out
                  (return-from batch-request-idents
                    (dolist (request to-receive (nreverse results))
                      (push (list (pop to-receive) '(:timeout)) results))))
                 (to-receive
                  (when (eq state :sending)
                    (setf state :waiting
                          start-wait (get-real-time))))
                 (t (return-from batch-request-idents (nreverse results)))))
              (otherwise                ; eof or error
               (return-from batch-request-idents (nreverse results)))))
      (close remote))))


(defun request-ident (&key remote-host remote-port local-port socket)
  "
NOTE:  Specify either (remote-host remote-port local-port) or socket.
RETURN:
           ((remote-port local-port) (:USERID) (opsys [charset]) user-id)
       or: ((remote-port local-port) (:ERROR)  (error-type))
       or: ((remote-port local-port) (:TIMEOUT))
"
  (assert (xor (and remote-host remote-port local-port)
               socket)
          (remote-host remote-port local-port socket)
          "Either the three remote-host, remote-port and local port, or socket must be specified.")
  (when socket
    (let (local-host)
      (multiple-value-setq (remote-host remote-port)
        (socket:socket-stream-peer socket t))
      (multiple-value-setq (local-host local-port)
        (socket:socket-stream-local socket t))))
  (first (batch-request-idents remote-host
                               (list (list remote-port local-port)))))


#||
(ext:shell "netstat -tnp")
(request-ident :remote-host "62.93.174.78"  :remote-port  22 :local-port 32793)
||#




(defun batch-request-idents/iotask (continuation remote-host port-couples)
  "
DO:     Connects to the remote host and schedules a iotask to process the
        ident protocol, and returns immediately.
        IOTASK-POLL must be called periodically.
        When the responses are received, calls continuation with
        the list of responses from REMOTE-HOST ident server:
        for each port-couple
             ((remote-port local-port) (:USERID) (opsys [charset]) user-id)
         or: ((remote-port local-port) (:ERROR)  (error-type))
         or: ((remote-port local-port) (:TIMEOUT))
"
  (unless port-couples
    (return-from batch-request-idents/iotask (funcall continuation nil)))
  (let ((remote (socket:socket-connect (get-port-for-service "ident") remote-host
                                       :element-type '(unsigned-byte 8)
                                       :buffered nil))
        ;;                No :timeout to get error when connection is refused.
        (inbuf       (make-array '(1024) :element-type '(unsigned-byte 8)
                                 :initial-element 0 :fill-pointer 0))
        (state       :sending)
        (start-wait  0.0)
        (results     '())
        (to-send     port-couples)
        (to-receive  port-couples))
    (flet ((task (task status)
             (flet ((done (result)
                      (close remote)
                      (com.informatimago.clisp.iotask:iotask-dequeue task)
                      (funcall continuation result)
                      (return-from task)))
               ;;(unwind-protect
               #+(or)
               (print `(:state ,state :start-wait ,start-wait :results ,results
                               :to-send ,to-send :to-receive ,to-receive))
               (case status
                 ((:timeout)
                  (done (dolist (request to-receive (nreverse results))
                          (push (list (pop to-receive) '(:timeout)) results))))
                 ((:input :io)          ; something to read
                  (when (read-line-in-buffer remote inbuf)
                    (let ((result (ident-parse-response inbuf)))
                      (unless (equal (car to-receive) (car result))
                        (warn "Desynchronization ~S --> ~S"
                              (car to-receive) result))
                      (pop to-receive)
                      (push result results))
                    (unless to-receive (setf state :waiting))
                    (recycle-line-buffer inbuf)))
                 ((:output)
                  (cond
                    (to-send
                     (setf state :sending)
                     (write-sequence (ident-format-request (pop to-send))
                                     remote)
                     (finish-output remote))
                    ((and (eq state :waiting)
                          (<= +wait-response-timeout+
                              (- (get-real-time) start-wait)))
                     ;; time-out
                     (done
                      (dolist (request to-receive (nreverse results))
                        (push (list (pop to-receive) '(:timeout)) results))))
                    (to-receive
                     (when (eq state :sending)
                       (setf state :waiting
                             start-wait (get-real-time))))
                    (t (done (nreverse results)))))
                 (otherwise             ; eof or error
                  (done (nreverse results)))))))
      (com.informatimago.clisp.iotask:iotask-enqueue-stream
       remote
       (function task)
       :name "BATCH-REQUEST-IDENTS/IOTASK"
       :alarm-time +wait-response-timeout+))))


(defun request-ident/iotask (continuation
                             &key remote-host remote-port local-port socket)
  "
NOTE:   Specify either (remote-host remote-port local-port) or socket.
DO:     Connects to the remote host and schedules a iotask to process the
        ident protocol, and returns immediately.
        IOTASK-POLL must be called periodically.
        When the response is received, calls continuation with
             ((remote-port local-port) (:USERID) (opsys [charset]) user-id)
         or: ((remote-port local-port) (:ERROR)  (error-type))
         or: ((remote-port local-port) (:TIMEOUT))
"
  (assert (xor (and remote-host remote-port local-port)
               socket)
          (remote-host remote-port local-port socket)
          "Either the three remote-host, remote-port and local port, or socket must be specified.")
  (when socket
    (let (local-host)
      (multiple-value-setq (remote-host remote-port)
        (socket:socket-stream-peer socket t))
      (multiple-value-setq (local-host local-port)
        (socket:socket-stream-local socket t))))
  (batch-request-idents/iotask
   remote-host
   (list (list remote-port local-port))
   (lambda (results) (funcall continuation (first results)))))

;;;; THE END ;;;;
