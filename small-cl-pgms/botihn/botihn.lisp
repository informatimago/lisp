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
  (:use "COMMON-LISP" "CL-IRC" "CL-JSON" "DRAKMA" 
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (:export "MAIN"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIHN")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; cf. https://github.com/HackerNews/API

(defvar *period* 10 #|seconds|#)
(defvar *last-story* nil)

(defun monitor-initialize ()
  (unless (find '("application" . "json") *text-content-types*
                :test (function equalp))
    (push '("application" . "json") *text-content-types*))
  (setf *last-story* nil))

(defun new-stories ()
  (multiple-value-bind (value status) (http-request "https://hacker-news.firebaseio.com/v0/newstories.json")
    (when (= 200 status)
      (decode-json-from-string value))))

(defun story (id)
  (multiple-value-bind (value status) (http-request (format nil "https://hacker-news.firebaseio.com/v0/item/~A.json" id))
    (when (= 200 status)
      (decode-json-from-string value))))

(defun get-new-stories ()
  (let ((news (new-stories)))
    (if *last-story*
         (reverse (subseq news 0 (or (position *last-story* news) 1)))
        (list (first news)))))

(defun hn-url (story)
  (format nil "https://news.ycombinator.com/item?id=~A" story))

(defun format-story (story)
  (let ((title  (aget story :title))
        (url    (aget story :url))
        (id     (aget story :id)))
    (when (and title url)
      (format nil "~A <~A>" title (if (zerop (length url))
                                      (hn-url id)
                                      url)))))

(defun monitor-hacker-news (send)
  (dolist (story (get-new-stories))
    (let ((message (format-story (story story))))
      (when message
        (funcall send message))
      (setf *last-story* story))))

(declaim (notinline monitor-initialize monitor-hacker-news))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *server*   "irc.freenode.org")
(defvar *nickname* "botihn")
(defvar *channel*  "#hn")
(defvar *sources-url* "https://gitlab.com/com-informatimago/com-informatimago/tree/master/small-cl-pgms/botihn/")
(defvar *connection* nil)

(defun msg-hook (message)
  (when (string= *nickname* (first (arguments message)))
    (privmsg *connection* (source message)
             (format nil "I'm an IRC bot forwarding HackerNews news to ~A; ~
                          under AGPL3 license, my sources are available at <~A>."
                     *channel*
                     *sources-url*))))


(defun call-with-retry (delay thunk)
  (loop
    (handler-case (funcall thunk)
      (error (err) (format *error-output* "~A~%" err)))
    (funcall delay)))

(defmacro with-retry (delay-expression &body body)
  `(call-with-retry (lambda () ,delay-expression)
                    (lambda () ,@body)))

(defun main ()
  (catch :gazongues
    (with-retry (sleep (+ 10 (random 30)))
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
          (setf *connection* nil))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THE END ;;;;
