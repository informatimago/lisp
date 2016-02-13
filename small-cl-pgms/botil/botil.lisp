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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL"
  (:use "COMMON-LISP"
        "CL-IRC" "CL-JSON" "DRAKMA"  "SPLIT-SEQUENCE" "BORDEAUX-THREADS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM"
        "COM.INFORMATIMAGO.CLEXT.QUEUE")
  (:shadow "LOG")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
                "DATE" "UPTIME")
  (:export
   "MAIN"

   ;; -- admin commands
   "*INITIAL-SERVER*" "INITIALIZE-DATABASE")
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

(defvar *external-format* :utf-8
  "External format used for all files (database files, log files).")

(defvar *initial-server* "irc.freenode.org"
  "Only used when creating the database the first time.")

(defvar *default-nickname* "botil"
  "The nickname of the botil user.")

(defvar *default-password* "1234"
  "The nickname of the botil user.")

(defvar *database*
  (find-if (lambda (dir)
             (probe-file (merge-pathnames #P"botil.database" dir nil)))
           #(#P"/data/databases/irc/"
             #P"/mnt/data/databases/irc/"
             #P"/Users/pjb/Documents/irc/"))
  "The pathname to the botil database.")

(defvar *sources-url*
  "https://gitlab.com/com-informatimago/com-informatimago/tree/master/small-cl-pgms/botil/"
  "The URL where the sources of this ircbot can be found.")




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

(defclass server ()
  ((hostname   :initarg  :hostname
               :initform (error "A server needs a :hostname")
               :reader   hostname
               :type     string)
   (botnick    :initarg  :botnick
               :initform *default-nickname* 
               :accessor botnick
               :type     string
               :documentation "The nick of the bot on this server.")
   (botpass    :initarg  :botpass
               :initform *default-password* 
               :accessor botpass
               :type     (or null string)
               :documentation "The password of the bot on this server.")
   (channels   :initarg  :channels
               :initform '()
               :accessor channels
               :type     list)
   (connection :initarg  :connection
               :initform nil
               :accessor connection
               :type (or null connection))))

(defclass channel ()
  ((server     :initarg  :server
               :initform (error "A channel needs a :server")
               :reader   server
               :type     server)
   (name       :initarg  :name
               :initform (error "A channel needs a :name")
               :reader   name
               :type     string)
   (log-stream :initform nil
               :accessor log-stream
               :type     (or null file-stream))
   (log-month  :initform nil
               :accessor log-month
               :type     (or null integer))))

(defmethod initialize-instance :after ((channel channel) &key &allow-other-keys)
  (push channel (channels (server channel))))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :identity nil :type t)
    (format stream "~S :nick ~S :channels ~S"
            (hostname server)
            (botnick server)
            (channels server)))
  server)

(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :identity nil :type t)
    (format stream "~A" (name channel)))
  channel)


(defclass botil-user ()
  ((server   :initarg  :server
             :initform (error "A channel needs a :server")
             :reader   server
             :type     server)
   (name     :initarg  :name
             :initarg  :nickname
             :initform (error "A user needs a :name")
             :reader   name
             :type     string)
   (password :initarg  :password
             :initform nil
             :accessor password
             :type     (or null string))
   (email    :initarg  :email
             :initform nil
             :accessor email
             :type     (or null email))))

(defmethod print-object ((user botil-user) stream)
  (print-unreadable-object (user stream :identity nil :type t)
    (format stream "~a@~a" (name user) (hostname (server user))))
  user)


(defclass message ()
  ((source             :accessor source
                       :initarg  :source
                       :type     (or null string))
   (user               :accessor user
                       :initarg  :user
                       :type     (or null string))
   (host               :accessor host
                       :initarg  :host
                       :type     (or null string))
   (command            :accessor command
                       :initarg  :command
                       :type     (or null string))
   (arguments          :accessor arguments
                       :initarg  :arguments
                       :type     list)
   (received-time      :accessor received-time
                       :initarg  :received-time)
   (raw-message-string :accessor raw-message-string
                       :initarg  :raw-message-string
                       :type     string)))

(defclass request ()
  ((channel    :initarg  :channel
               :initform (error "A log needs a :channel")
               :reader   channel
               :type     channel)
   (owner      :initarg  :owner
               :initform (error "A request-request needs a :owner")
               :reader   owner
               :type     botil-user)
   (start-date :initarg  :start-date
               :initform (get-universal-time)
               :reader   start-date
               :type     integer)
   (end-date   :initarg  :end-date
               :initform nil
               :reader   end-date
               :type     (or null integer))))



(defclass query ()
  ((owner      :initarg  :owner
               :initform (error "A query needs a :owner")
               :reader   owner
               :type     botil-user
               :documentation "Who made the request and will receive the results.")
   (sender     :initarg  :sender
               :initform nil
               :reader   sender
               :type     (or null string))
   (channel    :initarg  :channel
               :initform nil
               :reader   channel
               :type     (or null channel))
   (start-date :initarg  :start-date
               :initform nil
               :reader   start-date
               :type     (or null integer))
   (end-date   :initarg  :end-date
               :initform nil
               :reader   end-date
               :type     (or null integer))
   (criteria   :initarg  :criteria
               :initform nil
               :accessor criteria)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *servers* '())
;; (setf *servers* nil)
;;
;; /data/databases/irc/
;;        |-- botil.database
;;        `-- servers
;;            `-- irc.freenode.org
;;                |-- server.plist
;;                |-- channels
;;                |   `-- #lisp
;;                |       `-- 201601.log
;;                `-- users
;;                    `-- pjb.sexp

;;; --------

(defgeneric server-pathname (object)
  (:method ((server-name string))
    (merge-pathnames (make-pathname :directory (list :relative "servers" server-name)
                                    :name nil :type nil :version nil
                                    :defaults *database*)
                     *database*))
  (:method ((server server))
    (server-pathname (hostname server))))

(defgeneric server-datafile-pathname (object)
  (:method ((server-name string))
    (make-pathname :name "server" :type "plist" :version nil
                   :defaults (server-pathname server-name)))
  (:method ((server server))
    (server-datafile-pathname server)))

(defgeneric user-pathname (object &optional server)
  (:method ((name t) &optional server)
    (check-type name (or string (member :wild)))
    (assert server (server) "SERVER is needed when calling USER-PATHNAME with a string.")
    (let ((serverdir (server-pathname server)))
      (merge-pathnames (make-pathname :directory '(:relative "users")
                                      :name name :type "sexp" :version nil
                                      :defaults serverdir)
                       serverdir nil)))
  (:method ((user user) &optional server)
    (when server
      (assert (eq server (server user))))
    (user-pathname (name user) (server user))))

(defgeneric channel-pathname (channel &optional server)
  (:method ((channel t) &optional server)
    (check-type channel (or string (member :wild)))
    (assert server (server) "SERVER is needed when calling USER-PATHNAME with a string.")
    (let ((serverdir (server-pathname server)))
      (merge-pathnames (make-pathname :directory (list :relative "channels" channel)
                                      :name nil :type nil :version nil
                                      :defaults serverdir)
                       serverdir)))
  (:method ((channel channel) &optional server)
    (when server
      (assert (eq server (server channel))))
    (channel-pathname (name channel) (server channel))))

(defgeneric log-file-pathname (channel)
  (:method ((channel channel))
    (make-pathname :name (format nil "~6,'0D" (log-month channel)) :type "log" :version nil
                   :defaults (channel-pathname channel))))

;;; --------

(defgeneric save (object)
  (:method (object) object))


(defun server-data (pathname)
  (sexp-file-contents (server-datafile-pathname pathname)
                      :external-format *external-format*
                      :if-does-not-exist nil))

(defmethod save ((server server))
  (setf (sexp-file-contents (server-datafile-pathname (hostname server))
                            :external-format *external-format*
                            :if-does-not-exist :create
                            :if-exists :supersede)
        (list :botnick (botnick server) :botpassword (botpass server))))

(defun load-servers ()
  (mapcar (lambda (pathname)
            (let* ((hostname (first (last (pathname-directory pathname))))
                   (data (server-data hostname)))
              (make-instance 'server
                             :hostname hostname
                             :botnick  (getf data :botnick)
                             :botpass  (getf data :botpass))))
          (directory (server-pathname :wild))))


                      
(defun load-channels-of-server (server)
  (dolist (pathname (directory (channel-pathname :wild server)))
    (make-instance 'channel
                   :server server
                   :name (first (last (pathname-directory pathname))))))

(defun load-server-database ()
  (setf *servers* (load-servers))
  (dolist (server *servers*)
    (load-channels-of-server server)))


(defun create-server (hostname &optional (nick "botil") (password nil))
  (check-type hostname string)
  (ensure-directories-exist
   (make-pathname :name "dummy" :type "dummy" :version nil
                  :defaults (server-pathname hostname)))
  (let ((server (make-instance 'server
                               :hostname hostname
                               :botnick nick
                               :botpass password)))
    (save server)
    (push server *servers*)
    server))


(defmethod create-channel ((server server) (name string))
  (assert (char= #\# (aref name 0)))
  (ensure-directories-exist
   (make-pathname :name "dummy" :type "dummy" :version nil
                  :defaults (channel-pathname name server)))
  (make-instance 'channel :server server :name name))

(defmethod save ((user botil-user))
  (let ((pathname (user-pathname user)))
    (with-open-file (out pathname
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :external-format *external-format*)
      (print (list :name (name user) :password (password user)) out)
      (terpri out)))
  user)

(defmethod create-user ((server server) (name string))
  (assert (char/= #\# (aref name 0)))
  (let ((pathname (user-pathname name server)))
    (ensure-directories-exist pathname)
    (let ((user (make-instance 'botil-user :server server :name name)))
      (save user))))

(defmethod ensure-log-stream ((channel channel) time)
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time time 0)
    (declare (ignore se mi ho da))
    (let ((month (+ (* ye 100) mo)))
      (when (/= month (log-month channel))
        (when (log-stream channel)
          (close (log-stream channel))
          (setf (log-stream channel) nil))
        (setf (log-month channel) month))
      (unless (log-stream channel)
        (setf (log-stream channel) (open (log-file-pathname channel)
                                         :external-format *external-format*
                                         :direction :output
                                         :if-does-not-exist :create
                                         :if-exists :append)))))
  (log-stream channel))


(defmethod write-message ((channel channel) (message message))
  (ensure-log-stream channel (received-time message))
  (prin1 (list :source (source message)
               :user (user message)
               :host (host message)
               :command (command message)
               :arguments (arguments message)
               :received-time (received-time message)
               :raw-message-string (raw-message-string message))
         (log-stream channel))
  (terpri (log-stream channel))
  message)

(defmethod create-message ((channel channel) (ircmsg irc-message))
  (write-message channel
                 (make-instance 'message
                                :source (source ircmsg)
                                :user (user ircmsg)
                                :host (host ircmsg)
                                :command (command ircmsg)
                                :arguments (arguments ircmsg)
                                :received-time (received-time ircmsg)
                                :raw-message-string (raw-message-string ircmsg))))

;; (setf (log-month  (first (channels (first *servers*)))) 201601)
;; (log-file-pathname (first (channels (first *servers*))))

(defun initialize-database (dbdir-pathname)
  "This initialize the empty directory at pathname DBDIR-PATHNAME as a
botil database, and creates the initial server named
*INITIAL-SERVER*.

This should be used only once to create a virgin empty database, and
configure an initial IRC server to which to connect to receive
commands."
  (let ((dbfile (merge-pathnames #P"botil.database" dbdir-pathname nil)))
    (ensure-directories-exist dbfile)
    (when (probe-file dbfile)
      (error "A botil database already exists at ~S" dbdir-pathname))
    (setf *database* dbdir-pathname)
    (with-open-file (out dbfile
                         :if-does-not-exist :create                         
                         :external-format *external-format*)
      (write-line ";; -*- mode:lisp -*-" out)
      (write-line ";; This is the local botil database." out))
    (create-server *initial-server* *default-nickname* *default-password*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct worker
  input-queue
  send
  thread)

(defun send-worker (worker &rest message)
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
                                 (destructuring-bind ,message-lambda-list ,vmessage
                                   (declare (ignorable ,@pl))
                                   ,@body)))
                           (error (err)
                             (format *error-output* "~&~A: ~A~%"
                                     ',sname err)))))
                 :name ,sname)))))


;;; workers:
(defvar *botil*)
(defvar *command-processor*)
(defvar *query-processor*)
(defvar *sender*)
(defvar *logger*)

(defun send-irc (recipient text)
  (privmsg (connection (server recipient)) recipient text))

(defun say (&rest args)
  (format t "~&~{~A~^ ~}~%" args)
  (force-output))

(defun answer (recipient format-control &rest format-arguments)
  (let ((text (apply (function format) nil format-control format-arguments)))
    (say text)
    (apply (function send-irc) *sender* recipient text)))

(defun query-processor (sender query)
  (declare (ignore sender query))
  )


(defun joined-channels ()
  )

(defun join-channel (channel)
  (join (connection (server channel)) channel))

(defun logged-channels (server) ;; TIME!
  (declare (ignore server))
  )

(defun start-logging-channel (channel)
  (unless (member channel (joined-channels) :test (function string=))
    (join-channel channel)))

(defun stop-logging-channel (channel)
  (declare (ignore channel))
  )

(defun logger (message)
  (format t "~&Logged: ~A~%" message))

#-(and) ((rights
          
          (query criterial)
          (create-log server "#name")
          (create-server hostname)
          )
         (commands

          (register password email) ; -> (create-user server name password email)
          (identify password)       ; -> (validate-user user password)
          (send-irc-pass [nickname]) ; -> (send-irc-password-change-key user) ; NY
          (set-pass nickname key password) ; -> (set-password user key password) ; NY
          (list-logs)                      ; -> (list-logs user)

          (query criteria)              
          (create-log "#name" [start-date [end-date]])  ; -> (create-channel server "#name") (create-log-request channel user start-date end-date)
          (create-server hostname [botil-nick]) ; (create-server hostname botil-nick) --> actual-botil-nick

          ))
(defun command-processor (sender command)
  (let ((words (split-sequence #\space command :remove-empty-subseqs t))
        (commands '(help version uptime sources)))
    (scase (first words)
           (("help")
            (answer sender "Available commands: ~(~{~A~^, ~}~)." commands))
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
            (answer sender "I'm an IRC log bot, under AGPL3 license, my sources are available at <~A>."
                    *sources-url*)))))

(defun botil (command server)
  (ecase command
    ((reconnect)
     (dolist (channel (logged-channels server))
       (start-logging-channel channel)))
    ((quit)
     (exit))))


(defun botil-initialize ()
  (setf *sender*            (make-worker-thread sender (recipient message &rest arguments)
                              (let ((message (format nil "~?" message arguments)))
                                
                                (format *trace-output* "~&~A <- ~A~%" recipient message)
                                (send-irc recipient message)))
        *query-processor*   (make-worker-thread query-processor (sender message)
                              (format *trace-output* "~&~A -> ~A~%" sender message)
                              (query-processor sender message))
        *logger*            (make-worker-thread logger (message)
                              (format *trace-output* "~&~A -> ~A~%" (user message) message)
                              (logger message))
        *command-processor* (make-worker-thread command-processor (sender message)
                              (format *trace-output* "~&~A -> ~A~%" sender message)
                              (command-processor sender message))
        *botil*             (make-worker-thread botil (command server)
                              (format *trace-output* "~&BOTIL <- ~A~%" command)
                              (botil command server)))
  (load-server-database)
  (mapcar (lambda (server)
            (hostname server)
            (botnick server)
            (botpass server))
          *servers*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; source = "nickname" or "#channel"
;; user = "~t" "identified-user@host" etc
;; host = fqdn of user's host
;; command = "PRIVMSG"
;; arguments = ("command" "arguments"); for /msg botil hello world --> ("botil" "hello world")

;; #test-botil <test-botil> /msg botil hello how do you do?
;; (:sender "test-botil" :recipient "botil"       :arguments ("botil" "hello how do you do?"))
;; #test-botil <test-botil> How do you do? 
;; (:sender "test-botil" :recipient "#test-botil" :arguments ("#test-botil" "How do you do?"))


(defun msg-hook (message)
  "Answers to PRIVMSG."
  (with-accessors ((sender source)
                   (arguments arguments)) message
    (let ((recipient (first arguments)))
      (format t "~&msg-hook message = ~S~%" (list :sender sender
                                                  :recipient recipient
                                                  :arguments arguments))
      (say arguments)
      (if (string= *nickname* recipient)
          (send-worker *command-processor* sender (second arguments))
          (send-worker *logger* message))))
  t)


(defun svc-hook (message)
  "Answers to service messages."
  (with-accessors ((sender source)
                   (arguments arguments)) message
    (let ((recipient (first arguments)))
      (format t "~&svc-hook message = ~S~%" (list :sender sender
                                                  :recipient recipient
                                                  :command (command message)
                                                  :arguments arguments))))
  (send-worker *logger* message)
  t)

;; (join *connection* "#test-botil")


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


(defmethod connect-to-server ((server server))
  (setf (connection server) (connect :nickname (botnick server)
                                     :server (hostname server)))
  (add-hook (connection server) 'irc-privmsg-message 'msg-hook)
  (mapc (lambda (class) (add-hook (connection server) class 'svc-hook)) 
        '(irc-notice-message 
          irc-topic-message irc-error-message
          irc-mode-message
          ;; -
          irc-nick-message irc-join-message
          irc-part-message irc-quit-message
          irc-kill-message irc-kick-message
          irc-invite-message))
  (send-worker *botil* 'reconnect server))

(defun main ()
  "The main program of the botil IRC bot.
We connect and reconnect to the *SERVER* under the *NICKNAME*,
log the channels we're instructed to log,
and answer to search queries in those logs."
  (let ((*package* (load-time-value
                    (find-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL"))))
    (botil-initialize)
    (with-simple-restart (quit "Quit")
      (catch :gazongues
        (loop
          :for server :in *servers*
          :for connection := (connection server)
          :if connection
            :do (with-simple-restart (reconnect "Reconnect")
                  (catch :petites-gazongues
                    (read-message connection)))
          :else
            :do (connect-to-server server))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
