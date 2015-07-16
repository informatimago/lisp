;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               botihn.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     IRC
;;;;DESCRIPTION
;;;;    
;;;;    Botihn: an IRC bot monitoring Hacker News.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
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
(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIHN"
  (:use "COMMON-LISP" "CL-IRC" "CL-JSON" "DRAKMA"  "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
                "DATE" "UPTIME")
  (:export "MAIN")
  (:documentation "
Botihn is a simple IRC bot monitoring Hacker News, and writing to
irc://irc.freenode.org/#hn the title and url of each new news.

It is run with:

   (com.informatimago.small-cl-pgms.botihn:main)

Copyright Pascal J. Bourguignon 2015 - 2015
Licensed under the AGPL3.
"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIHN")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; cf. https://github.com/HackerNews/API

(defvar *period* 10 #|seconds|#
  "The minimum period for fetching new news from Hacker News.")
(defvar *last-story* nil
  "The id of the last story sent to the IRC channel.")

(defun monitor-initialize ()
  "Initialize the Hacker News monitor.
Resets the *LAST-STORY*."
  (unless (find '("application" . "json") *text-content-types*
                :test (function equalp))
    (push '("application" . "json") *text-content-types*))
  (setf *last-story* nil))

(defun new-stories ()
  "Fetch and return the list of new stories from the HackerNews API
server."
  (multiple-value-bind (value status)
      (http-request "https://hacker-news.firebaseio.com/v0/newstories.json")
    (when (= 200 status)
      (decode-json-from-string value))))

(defun story (id)
  "Fetch and return an a-list containing the data of the story
identified by number ID."
  (multiple-value-bind (value status)
      (http-request (format nil "https://hacker-news.firebaseio.com/v0/item/~A.json" id))
    (when (= 200 status)
      (decode-json-from-string value))))

(defun get-new-stories ()
  "Return the list of the new stories IDs that haven't been sent to
the IRC server yet, in chronological order."
  (let ((news (new-stories)))
    (if *last-story*
         (reverse (subseq news 0 (or (position *last-story* news) 1)))
        (list (first news)))))

(defun hn-url (story-id)
  "Return the URL to the HackerNews story page for the given STORY-ID."
  (format nil "https://news.ycombinator.com/item?id=~A" story-id))

(defun format-story (story)
  "Returns a single line message containing the story title and the story URL,
extracted from the give STORY a-list."
  (let ((title  (aget story :title))
        (url    (aget story :url))
        (id     (aget story :id)))
    (when (and title url)
      (format nil "~A <~A>" title (if (zerop (length url))
                                      (hn-url id)
                                      url)))))

(defun monitor-hacker-news (send)
  "Sends the new news message lines by calling the SEND function.
Updates the *LAST-STORY* ID."
  (dolist (story (get-new-stories))
    (let ((message (format-story (story story))))
      (when message
        (funcall send message))
      (setf *last-story* story))))

(declaim (notinline monitor-initialize monitor-hacker-news))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *server*   "irc.freenode.org"
  "The fqdn of the IRC server.")
(defvar *nickname* "botihn"
  "The nickname of the botihn user.")
(defvar *channel*  "#hn"
  "The channel were the HackerNews stories are sent to.")
(defvar *sources-url*
  "https://gitlab.com/com-informatimago/com-informatimago/tree/master/small-cl-pgms/botihn/"
  "The URL where the sources of this ircbot can be found.")
(defvar *connection* nil
  "The current IRC server connection.")
(defvar *botpass*  "1234")

;; (setf *nickname* "botihn-test" *channel* "#botihn-test")

(defun msg-hook (message)
  "Answers to PRIVMSG sent directly to this bot."
  (labels ((say (&rest args)
             (format t "~&~{~A~^ ~}~%" args)
             (force-output))
           (answer (format-control &rest format-arguments)
             (let ((text (apply (function format) nil format-control format-arguments)))
               (say text)
               (privmsg *connection* (source message) text))))
    (let ((arguments  (arguments message)))
      (say arguments)
      (when (string= *nickname* (first arguments))
        (let ((words (split-sequence #\space (second arguments) :remove-empty-subseqs t)))
          (scase (first words)
                 (("help")
                  (answer "Available commands: help uptime sources"))
                 (("uptime")
                  (answer "~A" (substitute #\space #\newline
                                           (with-output-to-string (*standard-output*)
                                             (date) (uptime)))))
                 (("reconnect")
                  (if (string= (second words) *botpass*)
                      (progn (answer "Reconnecting…")
                             (reconnect))
                      (answer "I'm not in the mood.")))
                 (otherwise ; ("sources")
                  (answer "I'm an IRC bot forwarding HackerNews news to ~A; ~
                          under AGPL3 license, my sources are available at <~A>."
                          *channel*
                          *sources-url*)
                  ))))))
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
  "The main program of the botihn IRC bot.
We connect and reconnect to the *SERVER* under the *NICKNAME*,
and join to the *CHANNEL* where HackerNews are published."
  (with-simple-restart (quit "Quit")
    (catch :gazongues
      (with-retry (sleep (+ 10 (random 30)))
        (with-simple-restart (reconnect "Reconnect")
          (catch :petites-gazongues
            (unwind-protect
                 (progn
                   (setf *connection* (connect :nickname *nickname* :server *server*))
                   (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
                   (join *connection* *channel*)
                   (monitor-initialize)
                   (loop
                     :with next-time = (+ *period* (get-universal-time))
                     :for time = (get-universal-time)
                     :do (if (<= next-time time)
                             (progn
                               (monitor-hacker-news (lambda (message) (privmsg *connection* *channel* message)))
                               (incf next-time *period*))
                             (read-message *connection*) #|there's a 10 s timeout in here.|#)))
              (when *connection*
                (quit *connection*)
                (setf *connection* nil)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THE END ;;;;
