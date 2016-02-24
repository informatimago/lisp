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
(in-package "COMMON-LISP-USER")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(declaim (declaration also-use-packages))
(declaim (also-use-packages "CL-IRC" "UIOP"))
(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL"
  (:use "COMMON-LISP"
        "CL-JSON" "DRAKMA"  "SPLIT-SEQUENCE" "BORDEAUX-THREADS"
        "CL-DATE-TIME-PARSER" "CL-PPCRE" "CL-SMTP"
        "COM.INFORMATIMAGO.RDP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
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

(defvar *smtp-server* "hubble.informatimago.com")
(defvar *smtp-port*   2526)

(defvar *botil-email* "botil@informatimago.com")

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

;; Taken from swank:
(defun format-iso8601-time (time-value &optional include-timezone-p)
  "Formats a universal time TIME-VALUE in ISO 8601 format, with
    the time zone included if INCLUDE-TIMEZONE-P is non-NIL"
  ;; Taken from http://www.pvv.ntnu.no/~nsaa/ISO8601.html
  ;; Thanks, Nikolai Sandved and Thomas Russ!
  (flet ((format-iso8601-timezone (zone)
           (if (zerop zone)
               "Z"
               (multiple-value-bind (h m) (truncate (abs zone) 1.0)
                 ;; Tricky.  Sign of time zone is reversed in ISO 8601
                 ;; relative to Common Lisp convention!
                 (format nil "~:[+~;-~]~2,'0D:~2,'0D"
                         (> zone 0) h (round (* 60 m)))))))
    (multiple-value-bind (second minute hour day month year dow dst zone)
        (decode-universal-time time-value)
      (declare (ignore dow))
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~:[~*~;~A~]"
              year month day hour minute second
              include-timezone-p (format-iso8601-timezone
                                  (if dst (+ zone 1) zone))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass server ()
  ((hostname      :initarg  :hostname
                  :initform (error "A server needs a :hostname")
                  :reader   hostname
                  :type     string)
   (botnick       :initarg  :botnick
                  :initform *default-nickname* 
                  :accessor botnick
                  :type     string
                  :documentation "The nick of the bot on this server.")
   (botpass       :initarg  :botpass
                  :initform *default-password* 
                  :accessor botpass
                  :type     (or null string)
                  :documentation "The password of the bot on this server.")
   (enabled       :type     boolean
                  :initarg  :enabled
                  :initform t
                  :accessor enabled
                  :documentation "Set to false to prevent this server to connect.")
   (request-id    :initform 0
                  :initarg :request-id
                  :type integer)

   (channels      :initarg  :channels
                  :initform '()
                  :accessor channels
                  :type     list)
   (users         :initarg  :users
                  :initform '()
                  :accessor users
                  :type     list)
   
   (request-index :initform (make-request-index)
                  :reader request-index)
   (last-update   :initform (get-universal-time)
                  :accessor last-update
                  :type integer)
   (connection    :initarg  :connection
                  :initform nil
                  :accessor connection
                  :type (or null connection))))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :identity nil :type t)
    (format stream "~S :enabled ~S :nick ~S :channels ~S :requests-count ~A"
            (hostname server)
            (enabled server)
            (botnick server)
            (mapcar (function name) (channels server))
            (length (requests server))))
  server)

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
   (log-stream-write-time :initform (get-universal-time)
                          :accessor log-stream-write-time
                          :type integer)
   (log-month  :initform nil
               :accessor log-month
               :type     (or null integer))))

(defmethod initialize-instance :after ((channel channel) &key &allow-other-keys)
  (pushnew channel (channels (server channel))
           :key (function name)
           :test (function string-equal))) ; channel names are case insensitive.

(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :identity nil :type t) 
    (format stream "~A" (name channel)))
  channel)


(defclass user ()
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
             :type     (or null email))
   (verified :initarg :verified
             :initform nil
             :accessor verified
             :type boolean)
   (key      :initarg :key
             :initform nil
             :accessor key
             :type (or null string)
             :documentation "Verification key when verified is false;
password setting token when verified is true.")
   (identified :initarg :identified
               :initform nil
               :accessor identified
               :type boolean)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :identity nil :type t)
    (format stream "~a@~a~@[ ~A~]~:[~; verified~]"
            (name user) (hostname (server user))
            (email user) (verified user)))
  user)

(defclass request ()
  ((id         :initform 0
               :initarg  :id
               :reader   id
               :type     integer
               :documentation "A persistent ID for the request to let the users refer to them easily.
cf. command: cancel log $ID")
   (channel    :initarg  :channel
               :initform (error "A log needs a :channel")
               :reader   channel
               :type     channel)
   (owner      :initarg  :owner
               :initform (error "A request-request needs a :owner")
               :reader   owner
               :type     user)
   (start-date :initarg  :start-date
               :initform (get-universal-time)
               :reader   start-date
               :type     integer)
   (end-date   :initarg  :end-date
               :initform nil
               :reader   end-date
               :type     (or null integer))))

(defmethod print-object ((request request) stream)
  (print-unreadable-object (request stream :identity nil :type t)
    (format stream ":channel ~S :owner ~S :start-date ~A~@[ :end-date ~A~]"
            (name (channel request))
            (name (owner request))
            (format-iso8601-time (start-date request) t)
            (and (end-date request) (format-iso8601-time (end-date request) t))))
  request)



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


(defclass query ()
  ((owner      :initarg  :owner
               :initform (error "A query needs a :owner")
               :reader   owner
               :type     user
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


;; /Users/pjb/Documents/irc/
;; |-- botil.database
;; `-- servers
;;     `-- irc.freenode.org
;;         |-- channels
;;         |   `-- #lisp
;;         |       |-- logs
;;         |-- server.sexp
;;         |-- requests.sexp
;;         `-- users
;;             |-- ogam.sexp
;;             `-- tbot`.sexp

;;; --------


(defgeneric server-pathname (object)
  (:method ((server-name string))
    (merge-pathnames (make-pathname :directory (list :relative "servers" server-name)
                                    :name nil :type nil :version nil
                                    :defaults *database*)
                     *database*))
  (:method ((server-name (eql :wild)))
    (merge-pathnames (make-pathname :directory (list :relative "servers" :wild)
                                    :name nil :type nil :version nil
                                    :defaults *database*)
                     *database*))
  (:method ((server server))
    (server-pathname (hostname server))))

(defgeneric server-datafile-pathname (object)
  (:method ((server-name string))
    (make-pathname :name "server" :type "sexp" :version nil
                   :defaults (server-pathname server-name)))
  (:method ((server server))
    (server-datafile-pathname (hostname server))))

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

(defgeneric log-file-pathname (channel &optional month)
  (:method ((channel channel) &optional (month (log-month channel)))
    (let ((channel-pathname  (channel-pathname channel)))
      (merge-pathnames (make-pathname :directory '(:relative "logs")
                                      :name (format nil "~6,'0D" month) :type "log" :version nil
                                      :defaults channel-pathname)
                       channel-pathname nil))))

(defun requests-pathname (server)
  (let ((server-pathname (server-pathname server)))
    (merge-pathnames (make-pathname :name "requests" :type "sexp" :version nil
                                    :defaults server-pathname)
                     server-pathname nil)))

;;; --------------------

(defgeneric save (object)
  (:method (object) object))


;;; --------------------
;;; server
;;; --------------------

(defun server-data (pathname)
  (sexp-file-contents (server-datafile-pathname pathname)
                      :external-format *external-format*
                      :if-does-not-exist nil))

(defmethod save ((server server))
  (let ((pathname (server-datafile-pathname (hostname server))))
    (ensure-directories-exist pathname)
    (setf (sexp-file-contents pathname
                              :external-format *external-format*
                              :if-does-not-exist :create
                              :if-exists :supersede)
          (list :botnick     (botnick server)
                :botpassword (botpass server)
                :enabled     (enabled server)
                :request-id  (slot-value server 'request-id)
                :hostname    (hostname server)))
    (save-requests server)
    server))

(defun create-server (hostname &optional (nick "botil") (password nil))
  (check-type hostname string)
  (check-type nick     string)
  (check-type password (or null string))
  (let ((server (make-instance 'server
                               :hostname hostname
                               :botnick nick
                               :botpass password
                               :enabled nil)))
    (push server *servers*)
    (save server)))


(defgeneric load-channels (server)
  (:method ((server server))
    (dolist (pathname (directory (channel-pathname :wild server)))
      (make-instance 'channel
                     :server server
                     :name (first (last (pathname-directory pathname)))))))

(defgeneric save-requests (server)
  (:method ((server server))
    (setf (sexp-file-contents (requests-pathname server)
                              :external-format *external-format*)
          (mapcar (lambda (request)
                    (list :id         (id request)
                          :channel    (name (channel request))
                          :owner      (name (owner request))
                          :start-date (start-date request)
                          :end-date   (end-date request)
                          :server     (hostname (server (channel request)))))
                  (requests server)))))

(defgeneric load-requests (server)
  (:method ((server server))
    (mapcar (lambda (data)
              (request-index-add
               (make-instance
                'request
                :id         (getf data :id)
                :channel    (ensure-channel server
                                            (or (getf data :channel)
                                                (error "Missing channel in request data.")))
                :owner      (ensure-user server
                                         (or (getf data :owner)
                                             (error "Missing owner in request data.")))
                :start-date (getf data :start-date (get-universal-time))
                :end-date   (getf data :end-date nil))))
            (sexp-file-contents (requests-pathname server)
                                :if-does-not-exist nil
                                :external-format *external-format*))))

(defun load-server (hostname)
  (let ((server (or (find-server hostname)
                    (let ((data  (server-data hostname)))
                      (make-instance 'server
                                     :hostname   hostname
                                     :botnick    (getf data :botnick)
                                     :botpass    (getf data :botpass)
                                     :enabled    (getf data :enabled)
                                     :request-id (getf data :request-id))))))
    (load-channels server)
    (load-requests server)
    server))

(defun load-server-database ()
  (setf *servers*
        (mapcar (lambda (pathname)
                  (load-server (first (last (pathname-directory pathname)))))
                (directory (server-pathname :wild)))))


(defgeneric find-server (name))
(defmethod find-server ((name string))
  (find name *servers* :key (function hostname)
                       :test (function string-equal)))

(defmethod next-request-id ((server server))
  (prog1 (incf (slot-value server 'request-id))
    (save server)))


;;; --------------------
;;; channel
;;; --------------------

(defgeneric find-channel (server name)
  (:method ((hostname string) (name string))
    (find-channel (find-server hostname) name))
  (:method ((server server) (name string))
    (find name (channels server)
          :key (function name)
          :test (function string-equal))))

(defgeneric ensure-channel (server name)
  (:method ((hostname string) (name string))
    (ensure-channel (find-server hostname) name))
  (:method   ((server server) (name string))
    (assert (char= #\# (aref name 0)))
    (or (find-channel server name)
        (let ((channel (make-instance 'channel :server server :name name)))
          (ensure-directories-exist (log-file-pathname channel 201601))
          channel))))

(defmethod ensure-log-stream ((channel channel) time)
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time time 0)
    (declare (ignore se mi ho da))
    (let ((month (+ (* ye 100) mo)))
      (unless (eql month (log-month channel))
        (when (log-stream channel)
          (close (log-stream channel))
          (setf (log-stream channel) nil))
        (setf (log-month channel) month))
      (unless (log-stream channel)
        ;; TODO: add an open file counter, and when too many, close least recent log file written.
        (setf (log-stream channel) (open (log-file-pathname channel)
                                         :external-format *external-format*
                                         :direction :output
                                         :if-does-not-exist :create
                                         :if-exists :append)
              (log-stream-write-time channel) (get-universal-time)))))
  (log-stream channel))

(defmethod create-message ((channel channel) (ircmsg irc:irc-message))
  (ensure-log-stream channel (irc:received-time ircmsg))
  (let ((stream (log-stream channel)))
    (prin1 (print (list :source (irc:source ircmsg)
                        :user (irc:user ircmsg)
                        :host (irc:host ircmsg)
                        :command (irc:command ircmsg)
                        :arguments (irc:arguments ircmsg)
                        :received-time (irc:received-time ircmsg)
                        :raw-message-string (irc:raw-message-string ircmsg)))
           stream)
    (terpri stream)
    (finish-output stream))
  (setf (log-stream-write-time channel) (get-universal-time)))


;; (setf (log-month  (first (channels (first *servers*)))) 201601)
;; (log-file-pathname (first (channels (first *servers*))))
;; (values (first (channels (first *servers*)))
;;         (log-month (first (channels (first *servers*))))
;;         (log-stream (first (channels (first *servers*))))
;;         (ensure-log-stream (first (channels (first *servers*)))
;;                            (get-universal-time)))





;;; --------------------
;;; user
;;; --------------------

(defmethod save ((user user))
  (let ((pathname (user-pathname user)))
    (ensure-directories-exist pathname)
    (setf (sexp-file-contents pathname
                              :if-does-not-exist :create
                              :if-exists :supersede
                              :external-format *external-format*)
          (list :name     (name user)
                :password (password user)
                :email    (email user)
                :key      (key user)
                :verified (verified user))))
  user)

(defgeneric find-user (server name)
  (:method ((hostname string) (name string))
    (find-user (find-server hostname) name))
  (:method ((server server) (name string))
    (or (find name (users server)
              :key (function name)
              :test (function string-equal))
        (let ((data (sexp-file-contents (user-pathname name server)
                                        :if-does-not-exist nil
                                        :external-format *external-format*)))
          (when data
            (let ((user (apply (function make-instance) 'user
                               :server server :name name
                               data)))
              (push user (users server))
              user))))))

(defgeneric ensure-user (server name)
  (:method ((hostname string) (name string))
    (ensure-user (find-server hostname) name))
  (:method ((server server) (name string))
    (assert (and (plusp (length name)) (char/= #\# (aref name 0))))
    (or (find-user server name)
        (let ((user  (apply (function make-instance) 'user
                            :server server :name name 
                            (sexp-file-contents (user-pathname name server)
                                                :if-does-not-exist nil
                                                :external-format *external-format*))))
          (push user (users server))
          (save user)))))


;;; --------------------
;;; requests
;;; --------------------

;; [channel]-1------*-[request]-*--------1-[user]

(defclass request-index ()
  ((all-requests  :initform '() :accessor all-requests)
   (channel-index :initform (make-hash-table :test (function equalp)) :reader channel-index)
   (owner-index   :initform (make-hash-table :test (function equalp)) :reader owner-index)))

(defun make-request-index ()
  (make-instance 'request-index))

(defun request-index-add (request)
  (let ((index (request-index (server (channel request)))))
    (push request (all-requests index))
    (let* ((name   (name (channel request)))
           (index  (channel-index index))
           (serie  (or (gethash name index) (setf (gethash name index) (list nil)))))
      (push request (cdr serie)))
    (let* ((name   (name (owner request)))
           (index  (owner-index index))
           (serie  (or (gethash name index) (setf (gethash name index) (list nil)))))
      (push request (cdr serie)))
    request))

(defun request-index-remove (request)
  (let ((index (request-index (server (channel request)))))
    (deletef (all-requests index) request)
    (deletef (cdr (gethash (name (channel request)) (channel-index index) (list nil))) request)
    (deletef (cdr (gethash (name (owner   request)) (owner-index   index) (list nil))) request) 
    request))

(defmethod create-request ((channel channel) (owner user) &optional start-date end-date)
  (let ((request (make-instance 'request :id (next-request-id (server channel))
                                         :channel channel
                                         :owner owner
                                         :start-date (or start-date (get-universal-time))
                                         :end-date end-date)))
    (request-index-add request)
    (save-requests (server (channel request)))
    request))

(defmethod delete-request ((request request))
  (request-index-remove request)
  (save-requests (server (channel request)))
  request)

(defun sorted-serie (serie)
  (if (car serie)
      (cdr serie)
      (setf (car serie) t
            (cdr serie) (sort (cdr serie)
                              (function <)
                              :key (function start-date)))))

(defgeneric requests (object)
  (:method ((server server))
    (all-requests (request-index server)))
  (:method ((channel channel))
    (sorted-serie (gethash (name channel) (channel-index (request-index (server channel))) (list t))))
  (:method ((owner user))
    (sorted-serie (gethash (name owner) (owner-index (request-index (server owner))) (list t)))))


(defun channel-active-p (channel &optional (date (get-universal-time)))
  (check-type channel channel)
  (loop :for r :in (requests channel)
        :while (and (end-date r) (< (end-date r) date))
        :finally (return (and r (<= (start-date r) date)))))

(defun channel-next-start-log-date (channel &optional (date (get-universal-time)))
  (check-type channel channel)
  (loop :for r :in (requests channel)
        :do (cond
              ((and (end-date r) (< (end-date r) date)))
              ((and (<= (start-date r) date)
                    (null (end-date r)))
               (return t))
              ((<= date (start-date r))
               (return  (start-date r))))
        :finally (return nil)))

(defun channel-next-stop-log-date (channel &optional (date (get-universal-time)))
  (check-type channel channel)
  (loop :for r :in (requests channel)
        :do (if (<= (start-date r) date)
                (if (end-date r)
                    (when (<= date (end-date r))
                      (return (end-date r)))
                    (return t))
                (return nil))
        :finally (return nil)))

;;; --------------------
;;; channel display
;;; --------------------

(defgeneric joined-channels (server)
  (:method  ((server server))
    (mapcar (function irc:normalized-name)
            (hash-table-values (irc:channels (connection server))))))

(defgeneric wanted-channels (server)
  (:method  ((server server))
    (mapcar (function name)
            (remove-if-not (function channel-active-p) (channels server)))))

(defvar *update-period* 20)
(defvar *max-chunk*      5)
(defun at-most (n seq)
  (subseq seq 0 (min n (length seq))))

(defun update-channels (server)
  (when (< (+ *update-period* (last-update server)) (get-universal-time))
    (setf (last-update server) (get-universal-time))
    (let* ((wanted (wanted-channels server))
           (joined (joined-channels server))
           (to-join (at-most *max-chunk* (set-difference wanted joined :test (function string-equal))))
           (to-part (at-most *max-chunk* (set-difference joined wanted :test (function string-equal)))))
      (when to-join (join server to-join))
      (when to-part (part server to-part)))))

;;; --------------------
;;; initialization
;;; --------------------

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
      (write-line ";; This directory holds the local botil database." out))
    (create-server *initial-server* *default-nickname* *default-password*)))


;;;-----------------------------------------------------------------------------
;;;
;;; Workers
;;;

(defstruct worker
  input-queue
  send
  thread)

(defun send-worker (worker &rest message)
  (funcall (worker-send worker) message))

(defun kill-worker (worker)
  (funcall (worker-send worker) 'die))

(defmacro make-worker-thread (name message-lambda-list &body body)
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
        :send (lambda (,vmessage)
                (destructuring-bind ,message-lambda-list ,vmessage
                  (declare (ignorable ,@pl))
                  (enqueue ,vinput-queue ,vmessage)))
        :thread (make-thread
                 (lambda ()
                   (loop
                     :for ,vmessage := (dequeue ,vinput-queue)
                     :until (eql ,vmessage 'die)
                     :do (handler-bind
                             ((error (lambda (err)
                                       (format *error-output* "~&~A: ~A~%"
                                               ',sname err)
                                       #+ccl
                                       (format *error-output*
                                               "~&~80,,,'-<~>~&~{~A~%~}~80,,,'-<~>~&"
                                               (ccl::backtrace-as-list))
                                       nil)))
                           (flet ((terminate-worker () (loop-finish)))
                             (block ,name
                               (destructuring-bind ,message-lambda-list ,vmessage
                                 (declare (ignorable ,@pl))
                                 ,@body))))

                     #-(and) (handler-case
                                 (flet ((terminate-worker () (loop-finish)))
                                   (block ,name
                                     (destructuring-bind ,message-lambda-list ,vmessage
                                       (declare (ignorable ,@pl))
                                       ,@body)))
                               (error (err)
                                 (format *error-output* "~&~A: ~A~%"
                                         ',sname err)))))
                 :name ,sname)))))


;;;-----------------------------------------------------------------------------
;;; Workers
;;;

(defvar *botil*)
(defvar *command-processor*)
(defvar *query-processor*)
(defvar *sender*)
(defvar *logger*)

(defun todo (what)
  (format *trace-output* "~&TODO: ~S~%" what))


;;;-----------------------------------------------------------------------------
;;; Sender
;;;-----------------------------------------------------------------------------

(defun send-irc (recipient text)
  (check-type recipient (or user channel))
  (check-type text string)
  (let ((connection (connection (server recipient))))
    (when connection
      (irc:privmsg connection (name recipient) text))))

(defun send (server recipient message &rest arguments)
  (format *trace-output* "~&SENDER :server    ~S~%" server)
  (format *trace-output* "~&SENDER :recipient ~S~%" recipient)
  (format *trace-output* "~&SENDER :message   ~S~%" message)
  (format *trace-output* "~&SENDER :arguments ~S~%" arguments)
  ;; TODO: we may want to do better throttling.
  (etypecase message
    (list
     (ecase (first message)
       ((disconnect)
        (sleep 0.5)
        (irc:quit (connection server)
                  (format nil "~A ordered me to quit." (name recipient))))
       ((part)
        (dolist (channel (second message))
          (sleep 0.5)
          (irc:part (connection server) channel)))
       ((join)
        (dolist (channel (second message))
          (sleep 0.5)
          (irc:join (connection server) channel)))))
    (string
     (sleep 0.5)
     (let ((message (format nil "~?" message arguments)))
       (format *trace-output* "~&SENDER ~A <- ~A~%" recipient message)
       (send-irc recipient message)))))

;; TODO: change answer to queue the message thru the *sender* worker.
(defun answer (recipient format-control &rest format-arguments)
  (apply (function send-worker) *sender* (server recipient)
         recipient format-control format-arguments))

(defun disconnect (server sender)
  ;; TODO: queue a message for the (server sender) thru the *sender*
  ;; worker, to disconnect the server when it processes it.
  (send-worker *sender* server sender (list 'disconnect)))

(defun join (server channels)
  (send-worker *sender* server nil (list 'join channels)))

(defun part (server channels)
  (send-worker *sender* server nil (list 'part channels)))

;;;-----------------------------------------------------------------------------
;;; Query processor
;;;-----------------------------------------------------------------------------

(defun query-processor (server sender query)
  (check-type server server)
  (check-type sender user)
  (check-type query string)
  (todo 'query-processor))


;;;-----------------------------------------------------------------------------
;;; Logger
;;;-----------------------------------------------------------------------------
(defun logger (server message)
  (check-type server server)
  (check-type message irc:irc-message)
  (typecase message
    ((or irc:irc-privmsg-message
         irc:irc-join-message
         irc:irc-part-message)
     ;; irc:irc-privmsg-message
     (create-message (find-channel server (first (irc:arguments message))) message))
    (t
     (format t "~&Will log: ~A~%" (irc:raw-message-string message))
     (todo 'logger))))

;;;-----------------------------------------------------------------------------
;;; Command processor
;;;-----------------------------------------------------------------------------

;; nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
;; letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
;; digit      =  %x30-39                 ; 0-9
;; hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
;; special    =  %x5B-60 / %x7B-7D ; "[", "]", "\", "`", "_", "^", "{", "|", "}" ;
;; 
;; channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring [ ":" chanstring ]
;; chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
;; chanstring =/ %x2D-39 / %x3B-FF ; any octet except NUL, BELL, CR, LF, " ", "," and ":" ;
;; channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )

;; servername =  hostname
;; hostname   =  shortname *( "." shortname )
;; shortname  =  ( letter / digit ) *( letter / digit / "-" ) *( letter / digit ) ; as specified in RFC 1123 [HNAME] ;


(defun nicknamep (word)
  (and (stringp word)
       (scan "^[][\\`_^{|}A-Za-z][][\\`_^{|}A-Za-z0-9-]{0,8}$" word)))

(defun channel-name-p (word)
  (and (stringp word)
       (scan "^([#+&][!-+--9;-~]+|![0-9A-Z]{5})(:[!-+--9;-~]+)?$" word)))

(defun server-name-p (word)
  (and (stringp word)
       (scan "^([A-Za-z0-9][-A-Za-z0-9]*)(\\.([A-Za-z0-9][-A-Za-z0-9]*))*$" word)))

(defun emailp (word)
  (and (stringp word)
       (position #\@ word)
       (< 0 (position #\@ word) (1- (length word)))))



(defmethod word-equal ((a string) (b token))
  (word-equal b a))
(defmethod word-equal ((a token) (b string))
  (string= (token-text a) b))

;; TODO: We need a command to create a new server before connecting it:
;;       create server hostname nick botil password secret
;;       (create-server hostname [botil-nick])
;;
;; TODO: A command to manage users right.
;; - server creation
;; - server enable/disable
;; - user banishment

(defun rhs-bnf (rhs)
  (cond
    ((null rhs)    "")
    ((symbolp rhs) (format nil "<~A>" rhs))
    ((atom rhs)    rhs)
    (t (case (first rhs)
         ((alt)
          (if (null (cddr rhs))
              (rhs-bnf (second rhs))
              (format nil "(~{~A~^|~})" (mapcar (function rhs-bnf) (rest rhs)))))
         ((opt)
          (format nil "[~{~A~^ ~}]" (mapcar (function rhs-bnf) (subseq rhs 1 (position :action rhs)))))
         ((seq)
          (format nil "~{~A~^ ~}"   (mapcar (function rhs-bnf) (subseq rhs 1 (position :action rhs)))))
         ((rep)
          (format nil "{~{~A~^ ~}}" (mapcar (function rhs-bnf) (subseq rhs 1 (position :action rhs)))))
         (otherwise
          (rhs-bnf `(seq ,@rhs)))))))

(defgrammar botil
  :terminals ((word     "[^ ]+")
              (string   "\"[^\"]+\"|'[^']+'"))
  :skip-spaces t
  :start command
  :eof-symbol eol
  :rules #1=(
             (--> command
                  (alt query
                       connect
                       log-request
                       cancel-log
                       list
                       set
                       server-commands
                       register identify reset
                       help version uptime sources)
                  :action $1)
             
             (--> server   word :action `(server   ,(second word)))
             (--> email    word :action `(email    ,(second word)))
             (--> channel  word :action `(channel  ,(second word)))
             (--> nick     word :action `(nick     ,(second word)))
             (--> password word :action `(password ,(second word)))
             (--> id       word :action `(id       ,(second word)))
             (--> key      word :action `(key      ,(second word)))
             (--> date     word :action `(date     ,(second word)))
             
             (--> criteria
                  disjunction
                  :action `(criteria ,disjunction))
             
             (--> disjunction
                  conjunction (opt "or" disjunction :action disjunction)
                  :action (if $2
                              `(or ,conjunction ,$2)
                              conjunction))
             
             (--> conjunction
                  term (opt "and" conjunction :action conjunction)
                  :action (if $2
                              `(and ,term ,$2)
                              term))
             
             (--> term
                  (alt (seq "not" term :action `(not ,$2))
                       (seq selection :action selection)))

             (--> user (alt "nick" "nickname" "user" "mr" "mrs" "miss" "dr" "pr"))
             
             (--> selection
                  (alt
                   (seq "(" disjunction ")" :action disjunction)
                   (seq (rep string) :action `(keywords ,@$1))
                   (seq (opt "channel") channel :action channel)
                   (seq user nick :action nick)))
             
             (--> query
                  "query" criteria
                  :action `(query ,criteria))

             (--> connect
                  "connect" (opt "to") (opt "server") server
                  (opt (opt "as") user nick :action nick)
                  (opt (opt "with") "password" password :action password)
                  :action `(connect ,server ,$5 ,$6))

             (--> server-commands
                  (alt (seq "enable"    :action 'enable)
                       (seq "disable"   :action 'disable))
                  (opt "server") server
                  :action `(,$1 ,server))

             (--> register
                  "register" email password
                  :action `(register ,email ,password))
             (--> identify
                  "identify" password
                  :action `(identify ,password))
             (--> reset
                  "reset" (opt (opt user) nick :action nick)
                  :action `(reset ,$2))
             (--> set
                  "set"
                  (alt
                   (seq "password" (opt user) nick (opt "key") key (opt "password") password
                        :action `(set-password ,nick ,key ,password)))
                  :action $2)


             (--> requests 
                  (alt (seq (alt "log" "logs") (opt (alt "request" "requests")) )
                       "request" "requests"))

             (--> of-nick
                  "of"  (opt user)      nick
                  :action nick)
             (--> for-channel
                  "for" (opt "channel") channel
                  :action channel)
             (--> my-log-requests
                  "my" requests (opt for-channel :action for-channel)
                  :action `(nil ,$3 nil))
             (--> request-selection
                  (alt  my-log-requests
                        (seq "all" (alt my-log-requests
                                        (seq requests
                                             (opt of-nick     :action $1)
                                             (opt for-channel :action $1)
                                             (opt "on" (opt "server") server :action server)
                                             :action (list t $2 $3 $4)))
                             :action $2)))
             (--> list
                  "list" (alt (seq request-selection
                                   :action `(list-log-requests ,@request-selection))
                              (seq (alt "user" "users")
                                   (opt "on" (opt "server") server :action server)
                                   :action `(list-users ,$2))
                              (seq (alt "server" "servers")
                                   :action `(list-servers)))
                  :action $2)
             
             (--> cancel-log
                  "cancel" (alt
                            (seq request-selection
                                 :action `(cancel-log-requests ,@request-selection))   
                            (seq "log" id
                                 :action `(cancel-log-id ,id)))
                  :action $2)

             ;; list     my log requests                                (requests sender)
             ;; list     my log requests for channel #lisp              (of-channel channel (requests sender))
             ;; list all my log requests                                (requests sender)
             
             ;; list all    log requests of mr foo                      (requests user)
             ;; list all    log requests of mr foo for channel #lisp    (of-channel channel (requests user))
             ;; list all    log requests           for channel #lisp    (requests channel)
             ;; list all    log requests                                (requests server)
             
             (--> log-request
                  "log" (opt "channel") channel
                  (opt "on" (opt "server") server :action server)
                  (opt (alt (seq "from" date (opt "to" date :action date)
                                 :action `(,date ,$3))
                            (seq "to" date :action `(nil ,date)))
                       :action $1)
                  :action `(log-request ,channel ,$4 ,@$5))

             (--> help
                  "help" (opt (alt "query" "connect" "reconnect" "enable" "disable" "register" "identify"
                                   "reset" "set" "list" "start" "stop" "log" "cancel" "help" "version" "uptime" "sources"
                                   word)
                              :action (second $1))
                  :action `(help ,$2))
             (--> version "version"                :action '(version))
             (--> uptime  "uptime"                 :action '(uptime))
             (--> sources (alt "sources" "source") :action '(sources))))


(defparameter *help*
  '("query <criteria>"
    "connect [to] [server] <server> [[as] nick <nick>] [[with] password <password>]"
    "enable [server] <server>"
    "disable [server] <server>"
    "register <email> <password>"
    "identify <password>"
    "reset [<nick>]"
    "set password [nick] <nick> [key] <key> [password] <password>"
    "list (server|servers)"
    "list (user|users) [on [server] <server>]"
    "log [channel] <channel> (from <date> [to <date>])|(to <date>)"
    "list [all] my [log] requests [for [channel] <channel>]"
    "list all [log] requests [of [nick] <nick>] [for [channel] <channel>] [on [server] <server>]"
    "cancel all [log] requests [of [nick] <nick>] [for [channel] <channel>] [on [server] <server>]"
    "cancel log <id>"
    "help"
    "version"
    "uptime"
    "sources"))

(defun reconnect (server)
  (irc:quit (connection server) "Will reconnect...")
  (setf (connection server) nil))

;;;-----------------------------------------------------------------------------
;;; Miscellaenous commands
;;;

(defparameter *disses*
  #(
    "You’ve baked a really lovely cake, but then you’ve used dog sh!t for frosting."
    "You make some of the best products in the world — but you also make a lot of crap. Get rid of the crappy stuff."
    "This company is in shambles, and I don’t have time to wet-nurse the board. So I need all of you to resign."
    "Do you want to spend the rest of your life selling sugared water or do you want a chance to change the world?"
    "Being the richest man in the cemetery doesn’t matter to me…"
    "Be a yardstick of quality.  Some people aren’t used to an environment where excellence is expected."
    "The products suck!  There's no sex in them anymore!"
    "I’ve never known an HR person who had anything but a mediocre mentality."
    "Everything you have done in your life is shit!  So why don't you come work for me?"
    "My job is to say when something sucks rather than sugarcoat it."
    "Look, I don’t know who you are, but no one around here is too important to fire besides me. And you’re fired!"
    "You think I'm an arrogant ass -- who thinks he's above the law, and I think you're a slime bucket who gets most of his facts wrong."
    "That's the kind of Porsche that dentists drive."
    "Sometimes when you innovate, you make mistakes. It is best to admit them quickly, and get on with improving your other innovations."))

(defun one-of (s) (elt s (random (length s))))
(defun diss (sender)
  (answer sender (one-of *disses*)))

(defun help (sender what)
  (let ((lines (mapcar (lambda (command)
                         (list (subseq command 0 (position #\space command)) command))
                       *help*)))
    (if what
        (loop :for (command line) :in lines
              :when (string= what command)
                :do (answer sender "~A" line))
        (answer sender "Available commands: ~{~A~^ ~}"
                (remove-duplicates (mapcar (function first) lines)
                                   :test (function string=))))))

;; (help (find-user "irc.freenode.org" "pjb") "everything")

(defun version (sender)
  (answer sender "Version: ~A" *version*))

(defun uptime-cmd (sender)
  (answer sender "~A" (substitute #\space #\newline
                                  (with-output-to-string (*standard-output*)
                                    (date) (uptime)))))

(defun sources (sender)
  (answer sender "I'm an IRC log bot, under AGPL3 license, ~
                                            my sources are available at <~A>."
          *sources-url*))


;;; -----------------------------------------------------------------------------
(defmacro when-null ((var default) &body otherwise)
  `(if (null ,var)
       ,default
       (progn ,@otherwise)))

(defmacro when-unwrap ((var tag &optional filter-expression) &body body)
  `(cond
     ((and (consp ,var)
           (consp (cdr ,var))
           (null (cddr ,var))
           (eq ',tag (car ,var)))
      (let ((,var (cadr ,var)))
        ,(if filter-expression
             `(if ,filter-expression
                  (progn ,@body)
                  (error "Invalid ~(~A~) argument: ~S" ',tag ,var))
             `(progn ,@body))))
     (t (error "Invalid ~(~A~) argument: ~S" ',tag ,var))))

(defun argument-server (sender server)
  (when-null (server (server sender))
    (when-unwrap (server server (server-name-p server))
      (or (find-server server)
          (error "Unknown server: ~S" (cadr server))))))

(defun argument-channel (sender channel &optional server)
  (when-unwrap (channel channel (channel-name-p channel))
    (ensure-channel (or server (server sender)) channel)))

(defun argument-nickname (nick)
  "Just a string for a new nickname."
  (when-null (nick nil)
    (when-unwrap (nick nick (nicknamep nick))
      nick)))

(defun argument-new-user (sender nick server)
  (when-null (nick sender)
    (when-unwrap (nick nick (nicknamep nick))
      (ensure-user server nick))))

(defun argument-old-user (sender nick server)
  (when-null (nick sender)
    (when-unwrap (nick nick (nicknamep nick))
      (or (find-user server nick)
          (error "I have no user nicknamed ~A on ~A" nick (hostname server))))))

(defun argument-key (key)
  (when-unwrap (key key (stringp key))
    key))

(defun argument-id (id)
  (when-unwrap (id id (stringp id))
    (parse-integer id)))

(defun argument-email (email)
  (when-unwrap (email email (emailp email))
    email))

(defun argument-password (password)
  (when-unwrap (password password (stringp password))
    password))

(defun argument-date (date)
  (when-unwrap (date date (stringp date))
    (parse-date-time date)))

;;; -----------------------------------------------------------------------------
;;; credentials
;;;

(defvar *administrators* '("pjb" "tbot"))

(defun administratorp (user)
  (member (name user) *administrators* :test (function string=)))

(defun has-server-credential (sender server)
  ;; TODO:
  (declare (ignore server))
  (administratorp sender))

(defun has-user-credentials (sender user) 
  ;; TODO:
  (and (or (administratorp sender)
           (eql sender user))
       (or (identified sender)
           (answer sender "You're not identified.")
           nil)))



;;; -----------------------------------------------------------------------------
;;; server commands
;;;

(defmacro with-server ((sender server) &body body)
  `(let ((,server (argument-server ,sender ,server)))
     (declare (ignorable ,server))
     ,@body))

(defun connect   (sender server nickname password)
  ;; (connect (server "irc.freenode.org") (nick "botnick") (password "botpassword"))
  (if (has-server-credential sender nil)
      (with-server (sender server)
        (if (find-server server)
            (answer sender "The server ~A is already configured." server)
            (let ((nick (or (argument-nickname nickname) "botil"))
                  (password (argument-password password)))
              (create-server server nick password)
              (answer sender "The server ~A has been created. enable it to connect."))))
      (answer sender "You are not authorized to order me to connect to new servers.")))

;; (enable (find-user (first *servers*) "pjb") '(server "irc.freenode.org"))


(defun enable    (sender server)
  ;; (enable (server "irc.freenode.org"))
  (with-server (sender server)
    (if (has-server-credential sender server)
        (progn
          (setf (enabled server) t)
          (answer sender "~A is enabled, will connect." (hostname server)))
        (answer sender "You are not authorized to enable the server ~A." (hostname server)))))

(defun disable   (sender server)
  ;; (disable (server "irc.freenode.org"))
  (with-server (sender server)
    (if (has-server-credential sender server)
        (if (= 1 (count-if (function enabled) *servers*))
            (answer sender "Cannot disable the last server.")
            (progn
              (setf (enabled server) nil)
              (if (eq server (server sender))
                  (answer sender "This server is disabled, will disconnect.")
                  (answer sender "~A is disabled, will disconnect." (hostname server)))
              (disconnect server sender)))
        (answer sender "You are not authorized to disable the server ~A." (hostname server)))))

;;; -----------------------------------------------------------------------------
;;; list commands
;;;

(defun list-servers  (sender)
  (dolist (server *servers*)
    (answer sender "~A (~A) ~:[disabled~;~:[~;connected~]~] ~:[~;(this server)~]"
            (hostname server) (botnick server)
            (enabled server) (connection server)
            (eq server (server sender)))))

(defmacro with-user ((sender nick server) &body body)
  `(let ((,nick (argument-old-user ,sender ,nick ,server)))
     ,@body))

;; list     my log requests                                (requests sender)
;; list     my log requests for channel #lisp              (of-channel channel (requests sender))
;; list all my log requests                                (requests sender)
;;
;; list all    log requests of mr foo                      (requests user)
;; list all    log requests of mr foo for channel #lisp    (of-channel channel (requests user))
;; list all    log requests           for channel #lisp    (requests channel)
;; list all    log requests                                (requests server)

(defun of-channel (channel requests)
  (remove channel requests :test-not (function eql) :key (function channel)))

(defun select-log-requests (sender allp user channel server)
  (let* ((channel  (when channel (argument-channel  sender channel)))
         (user     (when user    (argument-old-user sender user    server))))
    (if allp
        (if user
            (if (has-user-credentials sender user)
                (values (if channel
                            (of-channel channel (requests user))
                            (requests user))
                        user)
                (progn
                  (answer sender "You are not authorized to access the requests from the user ~A." (name user))
                  nil))
            (if channel
                (if (has-server-credential sender server)
                    (values (requests channel))
                    (values (of-channel channel (requests sender)) sender))
                (if (has-server-credential sender server)
                    (values (requests (or server (server sender))))
                    (values (requests sender) sender))))
        (if channel
            (values (of-channel channel (requests sender)) sender)
            (values (requests sender))))))

(defun list-log-requests (sender allp user channel &optional server)
  ;; (list-log-requests (server "irc.freenode.org") (nick "pjb"))
  ;; (list-log-requests nil nil)
  (with-server (sender server)
    (multiple-value-bind (requests owner) (select-log-requests sender allp user channel server)
      (if requests
          (dolist (request requests
                           (answer sender "~R request~:*~p." (length requests)))
            (answer sender "~3D) ~9A requested ~15A from ~A~@[ to ~A~]"
                    (id request)
                    (name (owner request))
                    (name (channel request))
                    (format-iso8601-time (start-date request))
                    (when (end-date request)
                      (format-iso8601-time (end-date request)))))
          (answer sender "~:[No log requests.~;~A has requested no logs.~]" owner (name owner))))))

(defun user-names (server)
  (mapcar (function pathname-name)
          (directory (user-pathname :wild server))))

(defun list-users (sender server)
  ;; (list-users (server "irc.freenode.org")) 
  ;; (list-users nil)
  (with-server (sender server)
    (dolist (user 
             (if (has-server-credential sender server)
                 (user-names server)
                 (progn (answer sender "You are not authorized to list the users other than you.")
                        (list (name sender)))))
      (let ((user (find-user server user)))
        (answer sender "~16A ~:[ ~;I~]~:[ ~;V~] ~@[~A~]"
                (name user) (identified user) (verified user) (email user))))))

;;; -----------------------------------------------------------------------------
;;; User identification commands
;;;

(defun sendmail (to subject message)
  (send-email *smtp-server* *botil-email* to subject message :port *smtp-port*))

(defun generate-key ()
  (substitute #\I #\1
              (substitute-if #\O (lambda (ch) (find ch "0Q"))
                             (with-output-to-string (out)
                               (loop :repeat 4
                                     :for sep = "" :then "-"
                                     :do (format out "~A~36,4R" sep (random (expt 36 4))))))))

(defun send-verification-email (sender user)
  (if (email user)
      (progn
        (setf (verified user) nil
              (key user) (generate-key))
        (save user)
        (sendmail (email user)
                  "Verify your email with botil."
                  (format nil "Please let botil verify your email ~%~
                                                 by giving the following command ~%~
                                                 to irc://~A@~A :~%~
                                                 set password ~A ~A ~A~%"
                          (botnick (server sender))
                          (hostname (server sender))
                          (name user) (key user) "{your-new-password}")))
      (answer sender "User has no email yet. Use the register command!")))

(defun send-password-change-email (sender user)
  (if (email user)
      (progn
        (setf (verified user) nil
              (key user) (generate-key))
        (save user)
        (sendmail (email user)
                  "Password change with botil."
                  (format nil "You may change your password ~%~
                                                 by giving the following command ~%~
                                                 to irc://~A@~A :~%~
                                                 set password ~A ~A ~A~%"
                          (botnick (server sender))
                          (hostname (server sender))
                          (name user) (key user) "{your-new-password}")))
      (answer sender "User has no email yet. Use the register command!")))

(defun identify (sender password)
  ;; (identify (password "secret-password"))
  (let ((password (argument-password password)))
    (if (and (password sender)
             (string= (password sender) password))
        (progn
          (setf (identified sender) t)
          (answer sender "You're identified."))
        (answer sender "Failed."))))

(defun register (sender email password)
  ;; TODO: register doesn't need the password.
  ;; (register (email "pjb@informatimago.com") (password "secret-password"))
  (let ((email    (argument-email email))
        (password (argument-password password)))
    (if (and (null (email sender))
             (null (password sender)))
        (progn
          (setf (email sender) email
                (password sender) password
                (verified sender) nil)
          (send-verification-email sender sender)
          (answer sender "You're registered."))
        (answer sender "You are already registered.  Use reset and set password to change your password."))))

(defun reset (sender nick)
  ;; (reset (nick "pjb")) 
  ;; (reset nil)
  (let ((user (argument-old-user sender nick (server sender))))
    (if (has-user-credentials sender user)
        (progn (send-password-change-email sender user)
               (answer sender "Password change email sent to ~A" (email user)))
        (answer sender "You are not authorized to ask for a password reset for ~A." (name user)))))

(defun set-password (sender nick key password)
  ;; (set-password (nick "pjb") (key "321545623f") (password "new-secret-password"))
  (let ((user     (argument-old-user sender nick (server sender)))
        (key      (argument-key key))
        (password (argument-password password)))
    (if (has-user-credentials sender user)
        (cond
          ((string/= key (key user))
           (answer sender "Invalid key, use the reset command and try with the new key."))
          (t (setf (password user)   password
                   (verified user)   t
                   (key user)        nil
                   (identified user) nil)
             (save user)
             (answer sender "Password set. Identify again!")))
        (answer sender "You are not authorized to set the password of ~A." (name user)))))

;;; -----------------------------------------------------------------------------
;;; Log commands
;;;

(defun log-request (sender channel server
                    &optional (start-date (get-universal-time)) end-date)
  ;; (log-request (channel "#lisp") (server "irc.freenode.org") (date "2016-03-01") (date "2016-03-31")) 
  ;; (log-request (channel "#lisp") (server "irc.freenode.org") (date "2016-06-01") nil) 
  ;; (log-request (channel "#lisp") (server "irc.freenode.org") nil (date "2016-02-28")) 
  ;; (log-request (channel "#lisp") nil (date "20160301T000000") (date "2016-03-31T00:00:00")) 
  ;; (log-request (channel "#lisp") nil (date "20160601T000000") nil) 
  ;; (log-request (channel "#lisp") nil nil (date "2016-02-28T00:00:00"))
  ;; TODO: mixup user from one server asking logs on another server.
  (with-server (sender server)
    (let ((channel    (argument-channel sender channel))
          (start-date (if (integerp start-date)
                          start-date
                          (argument-date (or start-date (get-universal-time)))))
          (end-date   (typecase end-date
                        (integer end-date)
                        (null    nil)
                        (t       (argument-date end-date)))))
      (if (and end-date (<= end-date start-date))
          (answer sender "End date must be after start date.")
          (answer sender "Added ~A" (create-request channel sender start-date end-date))))))

(defun cancel-log-requests (sender allp user channel &optional server)
  ;; (cancel-log-requests (channel "#lisp") (server "irc.freenode.org")) 
  ;; (cancel-log-requests (channel "#lisp") nil)
  (with-server (sender server)
    (let ((to-be-canceled (select-log-requests sender allp user channel server)))
      (if to-be-canceled
          (dolist (request to-be-canceled
                           (answer sender "~R request~:*~p deleted." (length to-be-canceled)))
            (answer sender "Deleted ~A" (delete-request request)))
          (answer sender "Found no request to be deleted.")))))

(defun cancel-log-id (sender id)
  ;; (cancel-log-id (id "42"))
  (let ((id (argument-id id)))
    (let* ((requests (requests sender))
           (the-one  (find id requests :key (function id))))
      (if the-one
          (answer sender "Deleted ~A" (delete-request the-one))
          (answer sender "Found no request ID ~A" id)))))


;;; -----------------------------------------------------------------------------
;;; log querying
;;;

(defun query (sender criteria)
  (declare (ignore criteria))
  ;; (query (criteria (and (channel "#lisp") (and (nick "pjb") (keywords "cl-all"))))) 
  ;; (query (criteria (and (keywords "\"cl\"") (and (or (keywords "all") (or (keywords "some") (keywords "any"))) (channel "#lisp"))))) 
  (answer sender "Queries are not implemented yet, sorry."))

;;; -----------------------------------------------------------------------------
;;; Command interpreter.

(defun interpret (sender expression)
  (ecase (first expression)
    ((help)                    (help                    sender (second expression)))
    ((version)                 (version                 sender))
    ((uptime)                  (uptime-cmd              sender))
    ((sources)                 (sources                 sender))
    ((query)                   (query                   sender (second expression)))
    ((connect)                 (connect                 sender (second expression) (third expression) (fourth expression)))
    ((disable)                 (disable                 sender (second expression)))
    ((enable)                  (enable                  sender (second expression)))
    ((list-servers)            (list-servers            sender))
    ((list-users)              (list-users              sender (second expression)))
    ((list-log-requests)       (apply (function list-log-requests)   sender (rest expression)))
    ((cancel-log-requests)     (apply (function cancel-log-requests) sender (rest expression)))
    ((log-request)             (apply (function log-request)         sender (rest expression)))
    ((cancel-log-id)           (cancel-log-id           sender (second expression)))
    ((identify)                (identify                sender (second expression)))
    ((register)                (register                sender (second expression) (third expression)))
    ((reset)                   (reset                   sender (second expression)))
    ((set-password)            (set-password            sender (second expression) (third expression) (fourth expression)))))



(defun command-processor (server sender command)
  (check-type server server)
  (check-type sender user)
  (check-type command string)
  (format *trace-output* "~&~S~%  ~S~%  ~S~%  ~S~%"
          'command-processor server sender command)
  (let ((expression (handler-case
                        (parse-botil command)
                      (error ()
                        (let ((command (subseq command 0 (position #\space command))))
                          (answer sender "Syntax error.")
                          (help sender command)
                          (return-from command-processor))))))
    (handler-case 
        (interpret sender expression)
      (error (err)
        (answer sender "~A" (substitute #\space #\newline (princ-to-string err)))))))


(defun botil (server command)
  (ecase command
    ((reconnect)
     ;; we've connected to server, let's rejoin the channels and go on logging.
     (update-channels server))
    ((quit)
     (exit))))


(defvar *botil-workers* '())
(defun botil-initialize ()
  (setf *sender*            (make-worker-thread botil-sender (server recipient message &rest arguments)
                              (apply (function send) server recipient message arguments))
        *query-processor*   (make-worker-thread botil-query-processor (server sender message)
                              (format *trace-output* "~&QUERY ~A -> ~A~%" sender message)
                              (query-processor server sender message))
        *logger*            (make-worker-thread botil-logger (server message)
                              (format *trace-output* "~&LOGGER ~A -> ~A~%" (irc:source message) message)
                              (logger server message))
        *command-processor* (make-worker-thread botil-command-processor (server sender message)
                              (format *trace-output* "~&COMMAND ~A -> ~A~%" sender message)
                              (command-processor server sender message))
        *botil*             (make-worker-thread botil-main (server command)
                              (format *trace-output* "~&BOTIL <- ~A~%" command)
                              (botil server command)))
  (push *sender*            *botil-workers*)
  (push *query-processor*   *botil-workers*)
  (push *logger*            *botil-workers*)
  (push *command-processor* *botil-workers*)
  (push *botil*             *botil-workers*)
  (load-server-database))



;;;-----------------------------------------------------------------------------

;; source = "nickname" or "#channel"
;; user = "~t" "identified-user@host" etc
;; host = fqdn of user's host
;; command = "PRIVMSG"
;; arguments = ("command" "arguments"); for /msg botil hello world --> ("botil" "hello world")

;; #test-botil <test-botil> /msg botil hello how do you do?
;; (:sender "test-botil" :recipient "botil"       :arguments ("botil" "hello how do you do?"))
;; #test-botil <test-botil> How do you do? 
;; (:sender "test-botil" :recipient "#test-botil" :arguments ("#test-botil" "How do you do?"))


(defun msg-hook (server message)
  (with-accessors ((sender irc:source)
                   (arguments irc:arguments)) message
    (let ((recipient (first arguments)))
      (format *trace-output* "~&msg-hook message = ~S~%"
              (list :server server
                    :sender sender
                    :recipient recipient
                    :arguments arguments))
      (if (string= (botnick server) recipient)
          (let ((sender (ensure-user server sender)))
            (send-worker *command-processor* server sender (second arguments)))
          (send-worker *logger* server message)))))

(defun make-msg-hook (server)
  (lambda (message)
    "Answers to PRIVMSG."
    (msg-hook server message)
    t))

;; Note: we can track nick and quit only of users on the same
;; channels as we are on, so it's useless in general: we may
;; have to require passwords for all critical commands.

(defun svc-hook (server message)
  (send-worker *logger* server message)
  (format t "~&svc-hook message type = ~S~%" (type-of message))
  (with-accessors ((sender irc:source)
                   (arguments irc:arguments)) message
    (let ((recipient (first arguments)))

      ;; irc:irc-nick-message
      ;; irc:irc-quit-message
      (format t "~&svc-hook message = ~S~%"
              (list :server server
                    :sender sender
                    :recipient recipient
                    :command (irc:command message)
                    :arguments arguments)))))

(defun make-svc-hook (server)
  (lambda (message)
    "Answers to service messages."
    (svc-hook server message)
    t))


(defmethod connect-to-server ((server server))
  (setf (connection server) (irc:connect :server   (hostname server)
                                         :nickname (botnick server)
                                         :username (botnick server)
                                         :password (botpass server)))
  (let ((msg-hook (make-msg-hook server))
        (svc-hook (make-svc-hook server)))
    (irc:add-hook (connection server) 'irc:irc-privmsg-message msg-hook)
    (mapc (lambda (class) (irc:add-hook (connection server) class svc-hook)) 
          '(irc:irc-notice-message 
            irc:irc-topic-message
            irc:irc-error-message
            irc:irc-mode-message
            ;; -
            irc:irc-nick-message
            irc:irc-quit-message
            irc:irc-join-message
            irc:irc-part-message 
            irc:irc-kill-message
            irc:irc-kick-message
            irc:irc-invite-message))
    (send-worker *botil* server 'reconnect)))


;;;-----------------------------------------------------------------------------
;;;
;;; The main loop.
;;;

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
        (unwind-protect
             (loop
               (dolist (server *servers*)
                 (let ((connection (connection server)))
                   (if connection
                       (with-simple-restart (reconnect "Reconnect")
                         (catch :petites-gazongues
                           (irc:read-message connection) #|  goes to msg-hook or svc-hook.|#
                           (update-channels server)))
                       (when (enabled server)
                         (connect-to-server server))))))
          (mapc (function kill-worker) *botil-workers*)
          (setf *botil-workers* '()))))))

#-(and)
(progn
  
  (send-worker *command-processor* (find-server "irc.freenode.org")
               (ensure-user "irc.freenode.org" "pjb")
               "list servers")
  (send-worker *command-processor* (find-server "irc.freenode.org")
               (ensure-user "irc.freenode.org" "pjb")
               "enable irc.freenode.org")

  *servers*
  (setf (enabled (first *servers*)) t)
  (save  (first *servers*))


  )

;;;;
;;;;
;;;;  Tests
;;;;
;;;;

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL.TEST"

  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST")

  (:import-from "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL"

                "*BOTIL-EMAIL*" "*BOTIL-WORKERS*" "*DATABASE*"
                "*DEFAULT-NICKNAME*" "*DEFAULT-PASSWORD*" "*DISSES*"
                "*EXTERNAL-FORMAT*" "*INITIAL-SERVER*" 
                "*SERVERS*" "*SOURCES-URL*" "*VERSION*")

  (:import-from "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL"

                "CHANNEL-ACTIVE-P"
                "CHANNEL-NEXT-START-LOG-DATE"
                "CHANNEL-NEXT-STOP-LOG-DATE"
                "AND" "ANSWER" "ARGUMENT-EMAIL" "ARGUMENT-KEY"
                "ARGUMENT-PASSWORD" "ARGUMENT-SERVER" "ARGUMENT-OLD-USER" "ARGUMENT-NEW-USER"
                "ARGUMENTS" "BOTIL" "BOTIL-INITIALIZE" "BOTNICK"
                "BOTPASS" "CALL-WITH-RETRY" "CANCEL-LOG-REQUESTS"
                "CANCEL-LOG-ID" "CHANNEL" "CHANNEL-PATHNAME"
                "CHANNELS" "COMMAND" "COMMAND-PROCESSOR"
                "COMPUTE-DEADLINE" "CONNECT" "CONNECT-TO-SERVER"
                "CONNECTION" "COPY-WORKER" "CREATE-MESSAGE"
                "CREATE-REQUEST" "CREATE-SERVER" 
                "CRITERIA" "DATE" "LOG-REQUEST" "DISABLE"
                "DISCONNECT" "DISS" "EMAIL" "ENABLE" "ENABLED"
                "END-DATE" "ENSURE-CHANNEL" "ENSURE-LOG-STREAM"
                "ENSURE-USER" "EXIT" "FIND-CHANNEL" "FIND-SERVER"
                "FIND-USER" "GENERATE-KEY" "HELP" "HOST" "HOSTNAME"
                "ID" "IDENTIFIED" "IDENTIFY" "INITIALIZE-DATABASE"
                "INTERPRET"  "KEY"
                "KEYWORDS" "KILL-WORKER" "LIST-LOG-REQUESTS"
                "LIST-SERVERS" "LIST-USERS" 
                "LOAD-REQUESTS" "LOAD-SERVER-DATABASE" 
                "LOG-FILE-PATHNAME" "LOG-MONTH" "LOG-STREAM"
                "LOGGER" "MAIN" "MAKE-MSG-HOOK"
                "MAKE-SVC-HOOK" "MAKE-WORKER" "MAKE-WORKER-THREAD"
                "NAME" "NICK" "ONE-OF" "OR" "OWNER" "PARSE-BOTIL"
                "PASSWORD" "QUERY" "QUERY-PROCESSOR"
                "RAW-MESSAGE-STRING" "RECEIVED-TIME" "RECONNECT"
                "REGISTER" "REQUESTS" "REQUESTS-PATHNAME" "RESET"
                "SAVE" "SAVE-REQUESTS"  "SEND-IRC"
                "SEND-PASSWORD-CHANGE-EMAIL" "SEND-VERIFICATION-EMAIL"
                "SEND-WORKER" "SENDER" "SERVER" "SERVER-DATA"
                "SERVER-DATAFILE-PATHNAME" "SERVER-PATHNAME"
                "SET-PASSWORD" "SOURCE" "SOURCES" "START-DATE"
                "TODO" "UPTIME-CMD" "USER"
                "USER-PATHNAME" "VERIFIED" "VERSION" "WITH-RETRY"
                "WITH-SERVER" "WORKER-INPUT-QUEUE" "WORKER-P"
                "WORKER-SEND" "WORKER-THREAD" 
                "SERVER-NAME-P" "CHANNEL-NAME-P" "NICKNAMEP")
  
  (:export "TEST/ALL")
  (:documentation "
Tests the botil program.
Copyright Pascal J. Bourguignon 2016 - 2016
Licensed under the AGPL3.
"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL.TEST")


(defmacro with-temporary-database (&body body)
  `(let ((*database* (pathname (format nil "/tmp/irc-~8,'0X/" (random (expt 2 32)))))
         (*servers*  '()))
     (unwind-protect
          (progn ,@body)
       (uiop:run-program (format nil "rm -rf ~S" (namestring *database*))))))

(define-test test/botil-grammar ()
  (map nil (lambda (test)
             (destructuring-bind (sentence expected) test
               (check equal (parse-botil sentence) expected
                      (sentence))))
    '(
      ("query channel #lisp and nick pjb and \"cl-all\""
       (query (criteria (and  (channel "#lisp") (and (nick "pjb") (keywords "cl-all"))))))
      ("query \"cl\" and ( \"all\" or \"some\" or \"any\" ) and #lisp"
       (query (criteria (and (keywords "\"cl\"")
                         (and (or (keywords "all") (or (keywords "some") (keywords "any")))
                          (channel "#lisp"))))))
      ("connect irc.freenode.org"
       (connect (server "irc.freenode.org") nil nil))
      ("connect to irc.freenode.org"
       (connect (server "irc.freenode.org") nil nil))
      ("connect server irc.freenode.org"
       (connect (server "irc.freenode.org") nil nil))
      ("connect to server irc.freenode.org"
       (connect (server "irc.freenode.org") nil nil))
      ("disable irc.freenode.org"
       (disable (server "irc.freenode.org")))
      ("enable irc.freenode.org"
       (enable (server "irc.freenode.org")))
      ("register pjb@informatimago.com secret-password"
       (register (email "pjb@informatimago.com") (password "secret-password")))
      ("identify secret-password"
       (identify (password "secret-password")))
      ("reset"
       (reset nil))
      ("reset pjb"
       (reset (nick "pjb")))
      ("set password pjb 321545623f new-secret-password"
       (set-password (nick "pjb") (key "321545623f") (password "new-secret-password")))
      
      ("list users"
       (list-users nil))
      ("list users on irc.freenode.org"
       (list-users (server "irc.freenode.org")))
      ("list servers"
       (list-servers))

      ;;
      ("list     my log requests"
       (list-log-requests nil nil nil nil))                                
      ("list     my log requests for channel #lisp"
       (list-log-requests nil nil (channel "#lisp") nil))              
      ("list all my log requests"
       (list-log-requests nil nil nil nil))                                

      ("list all    log requests of mr foo"
       (list-log-requests t (nick "foo") (channel "#lisp") nil))                      
      ("list all    log requests of mr foo for channel #lisp"
       (list-log-requests t (nick "foo") (channel "#lisp") nil))    
      ("list all    log requests           for channel #lisp"
       (list-log-requests t nil (channel "#lisp") nil))    
      ("list all    log requests"
       (list-log-requests t nil (channel "#lisp") nil))                                

      ("list all    log requests of mr foo  on server irc.freenode.org"
       (list-log-requests t (nick "foo") (channel "#lisp") (server "irc.freenode.org")))                      
      ("list all    log requests of mr foo for channel #lisp  on server irc.freenode.org"
       (list-log-requests t (nick "foo") (channel "#lisp") (server "irc.freenode.org")))    
      ("list all    log requests           for channel #lisp  on server irc.freenode.org"
       (list-log-requests t nil (channel "#lisp") (server "irc.freenode.org")))    
      ("list all    log requests on server irc.freenode.org"
       (list-log-requests t nil (channel "#lisp") (server "irc.freenode.org")))                                
      
      ("list all channel logs on server irc.freenode.org"
       (list-log-requests t nil nil (server "irc.freenode.org")))
      ("list all channels"
       (list-log-requests t nil nil nil))
      ("list all logs on irc.freenode.org"
       (list-log-requests t nil nil (server "irc.freenode.org")))
      ("list all logs"
       (list-log-requests t nil nil nil))
      
      ;;
      ("cancel     my log requests"
       (cancel-log-requests nil nil nil nil))                                
      ("cancel     my log requests for channel #lisp"
       (cancel-log-requests nil nil (channel "#lisp") nil))              
      ("cancel all my log requests"
       (cancel-log-requests nil nil nil nil))                                

      ("cancel all    log requests of mr foo"
       (cancel-log-requests t (nick "foo") (channel "#lisp") nil))                      
      ("cancel all    log requests of mr foo for channel #lisp"
       (cancel-log-requests t (nick "foo") (channel "#lisp") nil))    
      ("cancel all    log requests           for channel #lisp"
       (cancel-log-requests t nil (channel "#lisp") nil))    
      ("cancel all    log requests"
       (cancel-log-requests t nil (channel "#lisp") nil))                                

      ("cancel all    log requests of mr foo  on server irc.freenode.org"
       (cancel-log-requests t (nick "foo") (channel "#lisp") (server "irc.freenode.org")))                      
      ("cancel all    log requests of mr foo for channel #lisp  on server irc.freenode.org"
       (cancel-log-requests t (nick "foo") (channel "#lisp") (server "irc.freenode.org")))    
      ("cancel all    log requests           for channel #lisp  on server irc.freenode.org"
       (cancel-log-requests t nil (channel "#lisp") (server "irc.freenode.org")))    
      ("cancel all    log requests on server irc.freenode.org"
       (cancel-log-requests t nil (channel "#lisp") (server "irc.freenode.org")))                                
      
      ("cancel all channel logs on server irc.freenode.org"
       (cancel-log-requests t nil nil (server "irc.freenode.org")))
      ("cancel all channels"
       (cancel-log-requests t nil nil nil))
      ("cancel all logs on irc.freenode.org"
       (cancel-log-requests t nil nil (server "irc.freenode.org")))
      ("cancel all logs"
       (cancel-log-requests t nil nil nil))
      ;;
      ("cancel all log request on channel #lisp on server irc.freenode.org"
       (cancel-log-requests t nil (channel "#lisp") (server "irc.freenode.org")))
      ("cancel all log request on channel #lisp"
       (cancel-log-requests t nil (channel "#lisp") nil))
      ("cancel all #lisp on irc.freenode.org"
       (cancel-log-requests t nil (channel "#lisp") (server "irc.freenode.org")))
      ("cancel all #lisp"
       (cancel-log-requests t nil (channel "#lisp") nil))
      ;;
      ("cancel log 42"
       (cancel-log-id (id "42")))
      ;;
      ("log channel #lisp on server irc.freenode.org                 to 2016-02-28"
       (log-request (channel "#lisp") (server "irc.freenode.org") nil (date "2016-02-28")))
      ("log channel #lisp on server irc.freenode.org from 2016-03-01 to 2016-03-31"
       (log-request (channel "#lisp") (server "irc.freenode.org") (date "2016-03-01") (date "2016-03-31")))
      ("log channel #lisp on server irc.freenode.org from 2016-06-01"
       (log-request (channel "#lisp") (server "irc.freenode.org") (date "2016-06-01") nil))
      ("log #lisp                      to 2016-02-28T00:00:00"
       (log-request (channel "#lisp") nil nil (date "2016-02-28T00:00:00")))
      ("log #lisp from 20160301T000000 to 2016-03-31T00:00:00"
       (log-request (channel "#lisp") nil (date "20160301T000000") (date "2016-03-31T00:00:00")))
      ("log #lisp from 20160601T000000"
       (log-request (channel "#lisp") nil (date "20160601T000000") nil))
      )))


(define-test test/pathnames ()
  (with-temporary-database
    (let* ((server     (create-server "irc.freenode.org"))
           (user       (ensure-user    server "pjb"))
           (channel    (ensure-channel server "#lisp")))
      (declare (ignorable server user channel))
      
      (check equal (server-pathname "irc.freenode.org")
             (merge-pathnames #P"servers/irc.freenode.org/" *database*))

      (check equal (server-pathname (find-server "irc.freenode.org"))
             (merge-pathnames #P"servers/irc.freenode.org/" *database*))

      (check equal (server-pathname "irc.freenode.org")
             (server-pathname (find-server "irc.freenode.org")))

      (check equal (server-pathname :wild)
             (merge-pathnames #P"servers/*/" *database*))

      (check equal (server-datafile-pathname "irc.freenode.org")
             (merge-pathnames #P"servers/irc.freenode.org/server.sexp" *database*))
      
      (check equal (server-datafile-pathname (find-server "irc.freenode.org"))
             (merge-pathnames #P"servers/irc.freenode.org/server.sexp" *database*))
      
      (check equal (server-datafile-pathname "irc.freenode.org")
             (server-datafile-pathname (find-server "irc.freenode.org")))


      (expect-condition 'error (user-pathname "pjb"))
      (check equal (user-pathname "pjb" "irc.freenode.org")
             (merge-pathnames #P"servers/irc.freenode.org/users/pjb.sexp" *database*))
      (check equal (user-pathname "pjb" (find-server "irc.freenode.org"))
             (merge-pathnames #P"servers/irc.freenode.org/users/pjb.sexp" *database*))
      (check equal (user-pathname (find-user (find-server "irc.freenode.org") "pjb"))
             (merge-pathnames #P"servers/irc.freenode.org/users/pjb.sexp" *database*))
      (check equal (user-pathname (find-user "irc.freenode.org" "pjb"))
             (merge-pathnames #P"servers/irc.freenode.org/users/pjb.sexp" *database*))
      (check equal (user-pathname :wild "irc.freenode.org")
             (merge-pathnames #P"servers/irc.freenode.org/users/*.sexp" *database*))

      (expect-condition 'error (channel-pathname "#lisp"))
      (check equal (channel-pathname "#lisp" "irc.freenode.org")
             (merge-pathnames #P"servers/irc.freenode.org/channels/#lisp/" *database*))
      (check equal (channel-pathname "#lisp" (find-server "irc.freenode.org"))
             (merge-pathnames #P"servers/irc.freenode.org/channels/#lisp/" *database*))
      (check equal (channel-pathname (find-channel (find-server "irc.freenode.org") "#lisp"))
             (merge-pathnames #P"servers/irc.freenode.org/channels/#lisp/" *database*))
      (check equal (channel-pathname (find-channel "irc.freenode.org" "#lisp"))
             (merge-pathnames #P"servers/irc.freenode.org/channels/#lisp/" *database*))
      (check equal (channel-pathname :wild "irc.freenode.org")
             (merge-pathnames #P"servers/irc.freenode.org/channels/*/" *database*))

      (check equal (log-file-pathname (ensure-channel "irc.freenode.org" "#lisp") 201602)
             (merge-pathnames #P"servers/irc.freenode.org/channels/#lisp/logs/201602.log" *database*))

      (check equal (requests-pathname "irc.freenode.org")
             (merge-pathnames #P"servers/irc.freenode.org/requests.sexp" *database*)) 
      (check equal (requests-pathname server)
             (merge-pathnames #P"servers/irc.freenode.org/requests.sexp" *database*)) 

      )))


(define-test test/users ()
  (with-temporary-database
    (let* ((server     (create-server "irc.freenode.org"))
           (user       (ensure-user    server "pjb"))
           (channel    (ensure-channel server "#lisp")))
      (declare (ignorable server user channel))

      (check eq (find-user server "pjb") (ensure-user server "pjb"))
      
      )))

(define-test test/requests ()
  (with-temporary-database
    (let ((server (create-server "irc.freenode.org")))

      (create-request (ensure-channel server "#lisp")    (ensure-user server "pjb"))
      (create-request (ensure-channel server "#clnoobs") (ensure-user server "pjb"))
      (create-request (ensure-channel server "#lisp")    (ensure-user server "foo"))
      (let ((all     (requests server))
            (lisp    (requests (ensure-channel server "#lisp")))
            (clnoobs (requests (ensure-channel server "#clnoobs")))
            (foo     (requests (ensure-user server "foo")))
            (pjb     (requests (ensure-user server "pjb"))))
        (check = (length all)     3)
        (check = (length lisp)    2)
        (check = (length clnoobs) 1)
        (check = (length pjb)     2)
        (check = (length foo)     1)
        (assert-true (subsetp lisp    all))
        (assert-true (subsetp clnoobs all))
        (assert-true (subsetp foo     all))
        (assert-true (subsetp pjb     all)))
      
      (create-request (ensure-channel server "#time") (ensure-user server "foo") 12000 15500)
      (create-request (ensure-channel server "#time") (ensure-user server "pjb") 10000 12500)
      (create-request (ensure-channel server "#time") (ensure-user server "pjb") 11000 13500)
      (create-request (ensure-channel server "#time") (ensure-user server "bar") 13000 14500)
      (let ((all (requests (ensure-channel server "#time"))))
        (check = (length all) 4)
        (loop
          :for (a b) :on all
          :while b
          :do (assert-true (<= (start-date a) (start-date b)))))
      (let ((channel (ensure-channel server "#time")))
        (assert-false (channel-active-p channel  9600))
        (assert-true  (channel-active-p channel 10600))
        (assert-true  (channel-active-p channel 11600))
        (assert-true  (channel-active-p channel 12600))
        (assert-true  (channel-active-p channel 13600))
        (assert-true  (channel-active-p channel 14600))
        (assert-true  (channel-active-p channel 15600))
        (assert-false (channel-active-p channel 16600))
        (loop :for time :from 8600 :to 17600 :by 1000
              :for results :in '((10000 nil) 
                                 (10000 nil) 
                                 (11000 12500) 
                                 (12000 12500) 
                                 (13000 13500) 
                                 (nil 15500) 
                                 (nil 15500) 
                                 (nil nil) 
                                 (nil nil))
              :do (check eql (channel-next-start-log-date channel time) (first results))
                  (check eql (channel-next-stop-log-date channel time) (second results)))
        (create-request channel (ensure-user server "pjb") 9000)
        (check eql (channel-next-start-log-date channel 8600) 9000)
        (check eql (channel-next-stop-log-date  channel 8600) nil)
        (loop :for time :from 9600 :to 17600 :by 1000
              :do (check eql (channel-next-start-log-date channel time) t)
                  (check eql (channel-next-stop-log-date  channel time) t))
        (loop :for time :from 9600 :to 17600 :by 1000
              :do (assert-true (channel-active-p channel time)
                               (time)))))))

(define-test test/predicates ()
  (dolist (nick '("pjb" "[pjb]" "pjb`"))
    (assert-true (nicknamep nick)))
  (dolist (nick '("hello world" "#lisp" "!foo" "&bar" "+baz"))
    (assert-false (nicknamep nick)))
  (dolist (channel '("#lisp" "##lisp" "&lisp" "+lisp"
                     "#foo~" "##b^ar" "&!quux!" "+~-/*+" 
                     "!ABCDE" "!01234" "!56789"
                     "#lisp:foo" "##lisp:bar" "&lisp:baz" "+lisp:quux"
                     "#foo~:1234" "##b^ar:/div" "&!quux!:hey" "+~-/*+:--" 
                     "!ABCDE:;-)" "!01234:\\" "!56789:yep"))
    (assert-true (channel-name-p channel)))
  (dolist (channel '("!foo" "!1234567" "!ZORRO" "pjb"))
    (assert-false (channel-name-p channel)))
  (dolist (server '("irc.freenode.org" "01.irc.example.net" "localhost"))
    (assert-true (server-name-p server)))
  (dolist (server '("#lisp" "foo.%a6t%a7.com"))
    (assert-false (server-name-p server))))

(define-test test/all ()
  (test/botil-grammar)
  (test/predicates)
  (test/users)
  (test/pathnames)
  (test/requests))




#-(and)
(progn
  
  (let (s)
    (com.informatimago.common-lisp.cesarum.list:maptree
     (lambda (node) (if (symbolp node) (push node s)))
     sexp)
    (mapcar (function symbol-name) (sort (remove-duplicates s) (function string<))) )

  (mapcar (function symbol-name)
          (sort (remove-duplicates
                 (let (ss
                       (p (find-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL")))
                   (do-symbols (s p ss)
                     (when (and (fboundp s)
                                (eql (symbol-package s) p)
                                (not (com.informatimago.common-lisp.cesarum.sequence:prefixp "BOTIL/PARSE-" (symbol-name s))))
                       (push s ss)))))
                (function string<)))

  (mapcar (function symbol-name)
          (sort (remove-duplicates
                 (let (ss
                       (p (find-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL")))
                   (do-symbols (s p ss)
                     (when (and (boundp s)
                                (eql (symbol-package s) p))
                       (push s ss)))))
                (function string<)))


  (progn
    (create-request (ensure-channel (first *servers*) "#lisp")
                    (ensure-user (first *servers*) "pjb"))
    (create-request (ensure-channel (first *servers*) "#clnoobs")
                    (ensure-user (first *servers*) "pjb"))
    (create-request (ensure-channel (first *servers*) "#lisp")
                    (ensure-user (first *servers*) "foo")))

  (values
   (requests (first *servers*))
   (requests (ensure-channel (first *servers*) "#lisp"))
   (requests (ensure-channel (first *servers*) "#clnoobs"))
   (requests (ensure-user (first *servers*) "foo"))
   (requests (ensure-user (first *servers*) "pjb")))
  )





;; (test/all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
;; (untrace ensure-user)
;; (reconnect (first *servers*))
;; (ql:quickload :com.informatimago.small-cl-pgms.botil)
;; (find-user (first *servers*) "ogam")
;; (mapcar (lambda (f) (funcall f (find-user (first *servers*) "ogam"))) '(name email password key verified))
;; (reset  (find-user (first *servers*) "ogam") nil)

