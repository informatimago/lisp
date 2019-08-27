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
;;;;    2019-08-27 <PJB> Added blacklist.
;;;;    2015-07-17 <PJB> Added commands: help uptime version sources; added restarts.
;;;;    2015-04-27 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2019
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIHN"
  (:use "COMMON-LISP"
        "CL-IRC" "CL-JSON" "DRAKMA"  "SPLIT-SEQUENCE" "CL-PPCRE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
                "DATE" "UPTIME")
  (:export "MAIN")
  (:documentation "
Botihn is a simple IRC bot monitoring Hacker News, and writing to
irc://irc.freenode.org/#hn the title and url of each new news.

It is run with:

   (com.informatimago.small-cl-pgms.botihn:main)

Copyright Pascal J. Bourguignon 2015 - 2019
Licensed under the AGPL3.
"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIHN")

(defparameter *version* "1.2.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration:
;;;

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

(defvar *blacklist* '()
  "A list of lists (label :url \"regex\") or (label :title \"regexp\") used to match either the url or the title of the HN news.
Matchedp news are not transmitted on irc.
The LABEL is a unique symbol used to identifythe blacklist entry.")

(defvar *blacklist-file* #P"/usr/local/var/botihn/blacklist.sexp"
        "Path of the file where the *blacklist* is saved.")
(defvar *blacklist-log-file* #P"/usr/local/var/botihn/blacklist.log"
        "Path of thef ile where the changes to the blacklist are logged.")

(defvar *requester-nick* nil
  "Nickname of the user who send the current command.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-blacklist ()
  (setf *blacklist* (sexp-file-contents *blacklist-file* :if-does-not-exist '())))

(defun save-blacklist ()
  (setf (sexp-file-contents *blacklist-file*)  *blacklist*))

(defun find-blacklist-entry (label)
  (find label *blacklist* :key (function first) :test (function string=)))

(defun delete-blacklist-entry (label)
  (setf *blacklist* (delete label *blacklist* :key (function first) :test (function string=)))
  (save-blacklist))

(defun add-blacklist-entry (label kind regexp)
  (let ((entry (find-blacklist-entry label)))
    (if entry
        (setf (second entry) kind
              (third entry)  regexp)
        (push (list label kind regexp) *blacklist*))
    (save-blacklist)))

(defun blacklistedp (story)
  (loop
    :for (label kind regexp) :in *blacklist*
      :thereis (scan regex
                     (ecase kind
                       (:title (story-title story))
                       (:url   (story-url   story))))))

(defun log-blacklist-change (nick operation entry)
  (with-open-file (log *blacklist-log-file*
                       :if-does-not-exist :create
                       :if-exists :append
                       :direction :output)
    (format log "~A ~S ~S ~S~%" (get-universal-time) nick operation entry)))

(defun process-blacklist-command (words)
  ;; blacklist help
  ;; blacklist add label url|title regexp
  ;; blacklist list
  ;; blacklist delete label
  (scase (second words)
         (("help")
          (answer "blacklist list")
          (answer "blacklist add <label> url|title <regexp>")
          (answer "blacklist delete <label>"))
         (("add")
          (let* ((label        (third words))
                 (kind-string  (fourth words))
                 (regexp       (fifth words))
                 (kind         (scase kind-string
                                      (("url") :url)
                                      (("title") :title))))
            (unless (and (stringp label)
                         (member kind '(:url :title))
                         (stringp regexp))
              (answer "Invalid command."))
            (log-blacklist-change *requester-nick* :add (list label kind regexp))
            (add-blacklist-entry label kind regexp)))
         (("delete")
          (let ((label        (third words)))
            (unless (stringp label)
              (answer "Invalid command."))
            (let ((entry (find-blacklist-entry label)))
              (if entry
                  (progn
                    (log-blacklist-change *requester-nick* :delete entry)
                    (delete-blacklist-entry label))
                  (answer "No such entry ~S" label)))))
         (("list")
          (loop :for (label kind regexp) :in *blacklist*
                :do (answer "~12A ~8A ~A~%" label kind regexp)))
         (otherwise
          (answer "Invalid command."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; cf. https://github.com/HackerNews/API

(defvar *period* 10 #|seconds|#
  "The minimum period for fetching new news from Hacker News.")
(defvar *last-story* nil
  "The id of the last story sent to the IRC channel.")

(declaim (notinline monitor-initialize monitor-hacker-news))

(defun monitor-initialize ()
  "Initialize the Hacker News monitor.
Resets the *LAST-STORY*."
  (unless (find '("application" . "json") *text-content-types*
                :test (function equalp))
    (push '("application" . "json") *text-content-types*))
  (setf *last-story* nil))

(defun compute-deadline (timeout)
  (+ (get-internal-real-time)
     (* timeout internal-time-units-per-second)))

(defun new-stories ()
  "Fetch and return the list of new stories from the HackerNews API
server."
  (multiple-value-bind (value status)
      (http-request "https://hacker-news.firebaseio.com/v0/newstories.json"
                    :connection-timeout 10
                    #+openmcl :deadline #+openmcl (compute-deadline 3))
    (when (= 200 status)
      (decode-json-from-string value))))

(defun story (id)
  "Fetch and return an a-list containing the data of the story
identified by number ID."
  (multiple-value-bind (value status)
      (http-request (format nil "https://hacker-news.firebaseio.com/v0/item/~A.json" id)
                    :connection-timeout 10
                    #+openmcl :deadline #+openmcl (compute-deadline 3))
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

(defun story-id    (story) (aget story :id))
(defun story-title (story) (aget story :title))
(defun story-url   (story)
  (let ((url (aget story :url)))
    (when url
      (if (zerop (length url))
          (hn-url id)
          url))))

(defun format-story (story)
  "Returns a single line message containing the story title and the story URL,
extracted from the give STORY a-list."
  (let ((title  (story-title story))
        (url    (story-url   story))
        (id     (story-id    story)))
    (when (and title url)
      (format nil "~A <~A>" title url))))

(defun monitor-hacker-news (send)
  "Sends the new news message lines by calling the SEND function.
Updates the *LAST-STORY* ID."
  (dolist (story (get-new-stories))
    (unless (blacklistedp story)
      (let ((message (format-story (story story))))
        (when message
          (funcall send message))
        (setf *last-story* story)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setf *nickname* "botihn-test" *channel* "#botihn-test")
(defun configure ()
  (setf *server*   (or (uiop:getenv "BOTIHN_SERVER")    *server*)
        *nickname* (or (uiop:getenv "BOTIHN_NICKNAME")  *nickname*)
        *channel*  (or (uiop:getenv "BOTIHN_CHANNEL")   *channel*)
        *botpass*  (or (uiop:getenv "BOTIHN_BOTPASS")   *botpass*)))

(defun say (&rest args)
  (format t "~&~{~A~^ ~}~%" args)
  (force-output))

(defun answer (format-control &rest format-arguments)
  (let ((text (apply (function format) nil format-control format-arguments)))
    (say text)
    (privmsg *connection* *requester-nick* text)))

(defun eliza (words)
  (dolist (line (split-sequence #\newline (uiop:run-program "fortune" :output :string)
                                :remove-empty-subseqs t))
    (answer line)))

(defun msg-hook (message)
  "Answers to PRIVMSG sent directly to this bot."
  (let ((arguments        (arguments message))
        (*requester-nick* (source message)))
    (say arguments)
    (when (string= *nickname* (first arguments))
      (let ((words (split-sequence #\space (second arguments) :remove-empty-subseqs t)))
        (scase (first words)
               (("help")
                (answer "Available commands: help version uptime sources blacklist")
                (answer "Type:  blacklist help   for help on blacklist command."))
               (("version")
                (answer "Version: ~A" *version*))
               (("uptime")
                (answer "~A" (substitute #\space #\newline
                                         (with-output-to-string (*standard-output*)
                                           (date) (uptime)))))
               (("reconnect")
                (if (string= (second words) *botpass*)
                    (progn (answer "Reconnecting…")
                           (reconnect))
                    (answer "I'm not in the mood.")))

               (("blacklist")
                (process-blacklist-command words))
               (("sources")
                (answer "I'm an IRC bot forwarding HackerNews news to ~A; ~
                          under AGPL3 license, my sources are available at <~A>."
                        *channel*
                        *sources-url*))
               (otherwise
                (eliza words))))))
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

(defparameter *allow-print-backtrace* t)
(defun print-backtrace (&optional (output *error-output*))
  #+ccl (when *allow-print-backtrace*
          (let ((*allow-print-backtrace* nil))
            (format output "~&~80,,,'-<~>~&~{~A~%~}~80,,,'-<~>~&"
                    (ccl::backtrace-as-list)))))

(defun main ()
  "The main program of the botihn IRC bot.
We connect and reconnect to the *SERVER* under the *NICKNAME*,
and join to the *CHANNEL* where HackerNews are published."
  (let ((*package* (load-time-value (find-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIHN"))))
    (configure)
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
                                 (handler-case
                                     (monitor-hacker-news (lambda (message) (privmsg *connection* *channel* message)))
                                   (error (err)
                                     (print-backtrace)
                                     (format *error-output* "~%~A~%" err)))
                                 (incf next-time *period*))
                               (read-message *connection*) #|there's a 10 s timeout in here.|#)))
                (when *connection*
                  (quit *connection*)
                  (setf *connection* nil))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THE END ;;;;
