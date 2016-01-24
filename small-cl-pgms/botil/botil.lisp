;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               botil.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     IRC
;;;;DESCRIPTION
;;;;    
;;;;    Botil: an IRC bot monitoring Hacker News.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-17 <PJB> Added commands: help uptime version sources; added restarts.
;;;;    2015-04-27 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL"
  (:use "COMMON-LISP"
        "CL-IRC" "CL-JSON" "DRAKMA"  "SPLIT-SEQUENCE" "BORDEAUX-THREADS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM"
        "COM.INFORMATIMAGO.CLEXT.QUEUE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
                "DATE" "UPTIME")
  (:export "MAIN")
  (:documentation "
Botil is a simple IRC logging and log querying bot.

It is run with:

   (com.informatimago.small-cl-pgms.botil:main)

Copyright Pascal J. Bourguignon 2016 - 2016
Licensed under the AGPL3.
"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL")

(defparameter *version* "1.0.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration:
;;;

(defvar *server*   "irc.freenode.org"
  "The fqdn of the IRC server.")
(defvar *nickname* "botil"
  "The nickname of the botil user.")
(defvar *sources-url*
  "https://gitlab.com/com-informatimago/com-informatimago/tree/master/small-cl-pgms/botil/"
  "The URL where the sources of this ircbot can be found.")
(defvar *connection* nil
  "The current IRC server connection.")
(defvar *botpass*  "1234")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-deadline (timeout)
  (+ (get-internal-real-time)
     (* timeout internal-time-units-per-second)))

;; (defun new-stories ()
;;   "Fetch and return the list of new stories from the HackerNews API
;; server."
;;   (multiple-value-bind (value status)
;;       (http-request "https://hacker-news.firebaseio.com/v0/newstories.json"
;;                     :connection-timeout 10
;;                     #+openmcl :deadline #+openmcl (compute-deadline 3))
;;     (when (= 200 status)
;;       (decode-json-from-string value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct worker
  input-queue
  send
  thread)

(defun send (worker &rest message)
  (apply (worker-send worker) message))

(defmacro make-worker-thread (name message-lambda-list
                              &body body)
  (check-type name symbol)
  (check-type message-lambda-list list)
  (let* ((vmessage     (gensym))
         (vinput-queue (gensym))
         (sname        (string name))
         (ll (parse-lambda-list message-lambda-list :destructuring))
         (pl (make-parameter-list ll)))
    `(let ((,vinput-queue (make-queue ,sname)))
       (make-worker
        :input-queue ,vinput-queue
        :send (lambda (&rest ,vmessage)
                (destructuring-bind ,message-lambda-list ,vmessage
                  (declare (ignorable ,@pl))
                  (enqueue ,vinput-queue ,vmessage)))
        :thread (make-thread
                 (lambda ()
                   (loop
                     :for ,vmessage := (dequeue ,vinput-queue)
                     :do (handler-case
                             (flet ((terminate-worker () (loop-finish)))
                               (block ,name
                                 (destructuring-bind ,message-lambda-list ,vmessage
                                   (declare (ignorable ,@pl))
                                   ,@body)))
                           (error (err)
                             (format *error-output* "~&~A: ~A~%"
                                     ',sname err)))))
                 :name ,sname)))))

(defvar *botil*)
(defvar *dispatch*)
(defvar *command-processor*)
(defvar *query-processor*)
(defvar *sender*)
(defvar *logger*)

(defun sender (recipient text)
  (privmsg *connection* recipient text))

(defun say (&rest args)
  (format t "~&~{~A~^ ~}~%" args)
  (force-output))

(defun answer (recipient format-control &rest format-arguments)
  (let ((text (apply (function format) nil format-control format-arguments)))
    (say text)
    (apply (function send) *sender* recipient format-control format-arguments)))

(defun query-processor (sender query)
  (declare (ignore sender query))
  )


(defun joined-channels ()
  )

(defun join-channel (channel)
  (join *connection* channel))

(defun logged-channels () ;; TIME!
  )

(defun start-logging-channel (channel)
  (unless (member channel (joined-channels) :test (function string=))
    (join-channel channel)))

(defun stop-logging-channel (channel)
  (declare (ignore channel))
  )

(defun logger (message)
  (declare (ignore message))
  )

(defun command-processor (sender command)
  (let ((words (split-sequence #\space command :remove-empty-subseqs t)))
    (scase (first words)
           (("help")
            (answer sender "Available commands: help version uptime sources"))
           (("version")
            (answer sender "Version: ~A" *version*))
           (("uptime")
            (answer sender "~A" (substitute #\space #\newline
                                            (with-output-to-string (*standard-output*)
                                              (date) (uptime)))))
           (("reconnect")
            (if (string= (second words) *botpass*)
                (progn (answer sender "Reconnecting…")
                       (reconnect))
                (answer sender "I'm not in the mood.")))
           (otherwise                   ; ("sources")
            (answer sender "I'm an IRC bot forwarding HackerNews news; ~
                          under AGPL3 license, my sources are available at <~A>."
                    *sources-url*)))))

(defun dispatch (message)
  (with-accessors ((source source)
                   (user user)
                   (host host)
                   (command command)
                   (arguments arguments)) message
    (format t "~&~S~%" (list :source source :user user :host host :command command :arguments arguments))
    (let ((recipient (first arguments)))
      (say arguments)
      (if (string= *nickname* recipient)
          (send *command-processor* user (second arguments))
          (send *logger* message)))))

(defun botil (command)
  (ecase command
    ((reconnect)
     (dolist (channel (logged-channels))
       (start-logging-channel channel)))
    ((quit)
     (exit))))


(defun botil-initialize ()
  (setf *sender*            (make-worker-thread sender (recipient message &rest arguments)
                              (let ((message (format nil "~?" message arguments)))
                                
                                (format *trace-output* "~&~A <- ~A~%" recipient message)
                                (sender recipient message)))
        *query-processor*   (make-worker-thread query-processor (sender message)
                              (format *trace-output* "~&~A -> ~A~%" sender message)
                              (query-processor sender message))
        *logger*            (make-worker-thread logger (message)
                              (format *trace-output* "~&~A -> ~A~%" (user message) message)
                              (logger message))
        *command-processor* (make-worker-thread command-processor (sender message)
                              (format *trace-output* "~&~A -> ~A~%" sender message)
                              (command-processor sender message))
        *dispatch*          (make-worker-thread dispatch (message)
                              (format *trace-output* "~&~A -> ~A~%" (source message) message)
                              (dispatch message))
        *botil*             (make-worker-thread botil (command)
                              (format *trace-output* "~&BOTIL <- ~A~%" command)
                              (botil command))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setf *nickname* "botil-test"  "#botil-test")
(defun configure ()
  (setf *server*   (or (uiop:getenv "BOTIL_SERVER")    *server*)
        *nickname* (or (uiop:getenv "BOTIL_NICKNAME")  *nickname*)
        *botpass*  (or (uiop:getenv "BOTIL_BOTPASS")   *botpass*)))

(defun msg-hook (message)
  "Answers to PRIVMSG sent directly to this bot."
  (send *dispatch* message)
  t)


(defun call-with-retry (delay thunk)
  "Calls THUNK repeatitively, reporting any error signaled,
and calling the DELAY-ing thunk between each."
  (loop
    (handler-case (funcall thunk)
      (error (err) (format *error-output* "~A~%" err)))
    (funcall delay)))

(defmacro with-retry (delay-expression &body body)
  "Evaluates BODY repeatitively, reporting any error signaled,
and evaluating DELAY-EXPRESSIONS between each iteration."
  `(call-with-retry (lambda () ,delay-expression)
                    (lambda () ,@body)))

(defun exit ()
  "Breaks the main loop and exit."
  (throw :gazongues nil))

(defun reconnect ()
  "Disconnect and reconnect to the IRC server."
  (throw :petites-gazongues nil))

(defun main ()
  "The main program of the botil IRC bot.
We connect and reconnect to the *SERVER* under the *NICKNAME*,
log the channels we're instructed to log,
and answer to search queries in those logs."
  (let ((*package* (load-time-value
                    (find-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL"))))
    (configure)
    (botil-initialize)
    (with-simple-restart (quit "Quit")
      (catch :gazongues
        (with-retry (sleep (+ 10 (random 30)))
          (with-simple-restart (reconnect "Reconnect")
            (catch :petites-gazongues
              (unwind-protect
                   (progn
                     (setf *connection* (connect :nickname *nickname* :server *server*))
                     (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
                     (send *botil* 'reconnect)
                     (loop :while (read-message *connection*)
                           #|there's a 10 s timeout in here.|#))
                (when *connection*
                  (quit *connection*)
                  (setf *connection* nil))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THE END ;;;;
