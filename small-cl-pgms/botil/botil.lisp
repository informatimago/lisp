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
(declaim (also-use-packages "CL-IRC"))
(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTIL"
  (:use "COMMON-LISP"
        "CL-JSON" "DRAKMA"  "SPLIT-SEQUENCE" "BORDEAUX-THREADS"
        "COM.INFORMATIMAGO.RDP"
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
             :type     (or null email))))

(defmethod print-object ((user user) stream)
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
               :type     user)
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
  (:method ((server-name (eql :wild)))
    (merge-pathnames (make-pathname :directory (list :relative "servers" :wild)
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
          (list :botnick (botnick server) :botpassword (botpass server)))
    server))

(defun create-server (hostname &optional (nick "botil") (password nil))
  (check-type hostname string)
  (check-type nick     string)
  (check-type password (or null string))
  (let ((server (make-instance 'server
                               :hostname hostname
                               :botnick nick
                               :botpass password)))
    (push server *servers*)
    (save server)))

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



;;; --------------------
;;; channel
;;; --------------------

(defmethod create-channel ((server server) (name string))
  (assert (char= #\# (aref name 0)))
  (ensure-directories-exist
   (make-pathname :name "dummy" :type "dummy" :version nil
                  :defaults (channel-pathname name server)))
  (make-instance 'channel :server server :name name))


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

(defmethod create-message ((channel channel) (ircmsg irc:irc-message))
  (write-message channel
                 (make-instance 'message
                                :source (irc:source ircmsg)
                                :user (irc:user ircmsg)
                                :host (irc:host ircmsg)
                                :command (irc:command ircmsg)
                                :arguments (irc:arguments ircmsg)
                                :received-time (irc:received-time ircmsg)
                                :raw-message-string (irc:raw-message-string ircmsg))))

;; (setf (log-month  (first (channels (first *servers*)))) 201601)
;; (log-file-pathname (first (channels (first *servers*))))


;;; --------------------
;;; user
;;; --------------------

(defmethod save ((user user))
  (let ((pathname (user-pathname user)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :external-format *external-format*)
      (print (list :name (name user) :password (password user)) out)
      (terpri out)))
  user)

(defmethod find-user ((server server) (name string))
  (with-open-file (out (user-pathname name server)
                       :direction :input
                       :if-does-not-exist nil
                       :external-format *external-format*)
    (when out
      (apply (function make-instance) 'user
             :server server (read out)))))

(defmethod create-user ((server server) (name string))
  (assert (and (plusp (length name)) (char/= #\# (aref name 0))))
  (save (make-instance 'user :server server :name name)))

(defmethod ensure-user ((server server) (name string))
  (or (find-user server name)
      (create-user server name)))


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
      (write-line ";; This is the local botil database." out))
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
  (apply (worker-send worker) message))

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

(defun send-irc (recipient text)
  (check-type recipient (or user channel))
  (check-type text string)
  (irc:privmsg (connection (server recipient)) (name recipient) text))

(defun answer (recipient format-control &rest format-arguments)
  (let ((text (apply (function format) nil format-control format-arguments)))
    (send-irc recipient text)))

(defun query-processor (server sender query)
  (check-type server server)
  (check-type sender user)
  (check-type query string)
  (todo 'query-processor))


(defun joined-channels ()
  )

(defmethod join-channel ((channel channel))
  (irc:join (connection (server channel)) (name channel)))


(defun logged-channels (server) ;; TIME!
  (declare (ignore server))
  )

(defun start-logging-channel (channel)
  (unless (member channel (joined-channels) :test (function string=))
    (join-channel channel)))

(defun stop-logging-channel (channel)
  (declare (ignore channel))
  )

(defun logger (server message)
  (check-type server server)
  (check-type message irc:irc-message)
  (format t "~&Logged: ~A~%" message)
  ;; (make-instance 'message
  ;;                :source (irc:source ircmsg)
  ;;                :user (irc:user ircmsg)
  ;;                :host (irc:host ircmsg)
  ;;                :command (irc:command ircmsg)
  ;;                :arguments (irc:arguments ircmsg)
  ;;                :received-time (irc:received-time ircmsg)
  ;;                :raw-message-string (irc:raw-message-string ircmsg))
  ;; 
  ;; (with-accessors ((sender irc:source)
  ;;                  (arguments irc:arguments)) message)
  (todo 'logger))


#-(and) ((rights
          
          (query criteria)
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


(defmethod word-equal ((a string) (b token))
  (word-equal b a))
(defmethod word-equal ((a token) (b string))
  (string= (token-text a) b))

#|

    message    =  [ ":" prefix SPACE ] command [ params ] crlf
    prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
    command    =  1*letter / 3digit
    params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
               =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

    nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
                    ; any octet except NUL, CR, LF, " " and ":"
    middle     =  nospcrlfcl *( ":" / nospcrlfcl )
    trailing   =  *( ":" / " " / nospcrlfcl )

    SPACE      =  %x20        ; space character
    crlf       =  %x0D %x0A   ; "carriage return" "linefeed"




Kalt                         Informational                      [Page 6]

 
RFC 2812          Internet Relay Chat: Client Protocol        April 2000


   NOTES:
      1) After extracting the parameter list, all parameters are equal
         whether matched by <middle> or <trailing>. <trailing> is just a
         syntactic trick to allow SPACE within the parameter.

      2) The NUL (%x00) character is not special in message framing, and
         basically could end up inside a parameter, but it would cause
         extra complexities in normal C string handling. Therefore, NUL
         is not allowed within messages.

   Most protocol messages specify additional semantics and syntax for
   the extracted parameter strings dictated by their position in the
   list.  For example, many server commands will assume that the first
   parameter after the command is the list of targets, which can be
   described with:

  target     =  nickname / server
  msgtarget  =  msgto *( "," msgto )
  msgto      =  channel / ( user [ "%" host ] "@" servername )
  msgto      =/ ( user "%" host ) / targetmask
  msgto      =/ nickname / ( nickname "!" user "@" host )
  channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring
                [ ":" chanstring ]
  servername =  hostname
  host       =  hostname / hostaddr
  hostname   =  shortname *( "." shortname )
  shortname  =  ( letter / digit ) *( letter / digit / "-" )
                *( letter / digit )
                  ; as specified in RFC 1123 [HNAME]
  hostaddr   =  ip4addr / ip6addr
  ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
  ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
  ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
  nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
  targetmask =  ( "$" / "#" ) mask
                  ; see details on allowed masks in section 3.3.1
  chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
  chanstring =/ %x2D-39 / %x3B-FF
                  ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
  channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )

  Other parameter syntaxes are:

  user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
                  ; any octet except NUL, CR, LF, " " and "@"
  key        =  1*23( %x01-05 / %x07-08 / %x0C / %x0E-1F / %x21-7F )
                  ; any 7-bit US_ASCII character,
                  ; except NUL, CR, LF, FF, h/v TABs, and " "
  letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
  digit      =  %x30-39                 ; 0-9
  hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
  special    =  %x5B-60 / %x7B-7D
                   ; "[", "]", "\", "`", "_", "^", "{", "|", "}"

  NOTES:
      1) The <hostaddr> syntax is given here for the sole purpose of
         indicating the format to follow for IP addresses.  This
         reflects the fact that the only available implementations of
         this protocol uses TCP/IP as underlying network protocol but is
         not meant to prevent other protocols to be used.

      2) <hostname> has a maximum length of 63 characters.  This is a
         limitation of the protocol as internet hostnames (in
         particular) can be longer.  Such restriction is necessary
         because IRC messages are limited to 512 characters in length.
         Clients connecting from a host which name is longer than 63
         characters are registered using the host (numeric) address
         instead of the host name.

      3) Some parameters used in the following sections of this
         documents are not defined here as there is nothing specific
         about them besides the name that is used for convenience.
         These parameters follow the general syntax defined for
         <params>.
|#

#|

   Each user is distinguished from other users by a unique nickname
   having a maximum length of nine (9) characters.  See the protocol
   grammar rules (section 2.3.1) for what may and may not be used in a
   nickname.

   While the maximum length is limited to nine characters, clients
   SHOULD accept longer strings as they may become used in future
   evolutions of the protocol.



   Channels names are strings (beginning with a '&', '#', '+' or '!'
   character) of length up to fifty (50) characters.  Apart from the
   requirement that the first character is either '&', '#', '+' or '!',
   the only restriction on a channel name is that it SHALL NOT contain
   any spaces (' '), a control G (^G or ASCII 7), a comma (',').  Space
   is used as parameter separator and command is used as a list item
   separator by the protocol).  A colon (':') can also be used as a
   delimiter for the channel mask.  Channel names are case insensitive.

(coerce (loop for i from 32 to 126 collect (code-char i)) 'string)


"$%"
"'()*"
"-./0123456789"
";<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

(id       "[0-9]+")
(key      "[0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z](-[0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z])+")
(date     "(20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]|[0-2][0-9]:[0-5][0-9]:[0-5][0-9]|20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]T[0-2][0-9]:[0-5][0-9]:[0-5][0-9]|20[0-9][0-9][0-1][0-9][0-3][0-9]T[0-2][0-9][0-5][0-9][0-5][0-9])")

|#


(defgrammar botil
  :terminals ((word     "[^ ]+")
              (string   "\"[^\"]+\"|'[^']+'"))
  :skip-spaces t
  :start command
  :eof-symbol eol
  :rules ((--> command
               (alt query
                    connect
                    start-log
                    stop-log
                    delimited-log
                    cancel-log
                    list
                    set
                    server-commands
                    register identify reset)
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
          
          (--> selection
               (alt
                (seq "(" disjunction ")" :action disjunction)
                (seq (rep string) :action `(keywords ,@$1))
                (seq (opt "channel") channel :action channel)
                (seq (alt "nick" "user" "mr" "mrs" "miss" "dr" "pr") nick :action nick)))
         
          (--> query
               "query" criteria
               :action `(query ,criteria))

          (--> connect
               "connect" (opt "to") (opt "server") server
               :action `(connect ,server))

          (--> server-commands
               (alt (seq "reconnect" (opt "to") :action 'reconnect)
                    (seq "enable"    :action 'enable)
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
               "reset" (opt nick :action nick)
               :action `(reset ,$2))
          (--> set
               "set"
               (alt
                (seq "password" nick key password
                     :action `(set-password ,nick ,key ,password)))
               :action $2)
          
          (--> list
               "list"
               (alt (seq (alt (seq (alt "channel" "channels") (opt (alt "log" "logs")))
                              (alt "log" "logs"))
                         (opt "on" (opt "server") server :action server)
                         :action `(list-channels ,$2))
                    (seq (alt "user" "users")
                         (opt "on" (opt "server") server :action server)
                         :action `(list-users ,$2))
                    (seq (alt "server" "servers")
                         :action `(list-servers)))
               :action $2)

          (--> start-log
               "start" (alt "log" "logging") (opt "channel") channel
               (opt "on" (opt "server") server :action server)
               :action `(start-log ,channel ,$5))

          (--> stop-log
               "stop" (alt "log" "logging") (opt "channel") channel
               (opt "on" (opt "server") server :action server)
               :action `(stop-log ,channel ,$5))

          (--> delimited-log
               "log" (opt "channel") channel
               (opt "on" (opt "server") server :action server)
               (alt (seq "from" date (opt "to" date :action date)
                         :action `(,date ,$3))
                    (seq "to" date :action `(nil ,date)))
               :action `(delimited-log ,channel ,$4 ,@$5))
          
          (--> cancel-log
               "cancel" (alt
                         (seq "all" (opt "log") (opt "on") (opt "channel") channel
                              (opt "on" (opt "server") server :action server)
                              :action `(cancel-log-channel ,channel ,$6))   
                         (seq "log" id
                              :action `(cancel-log-id ,id)))
               :action $2)


          (--> help    "help"                   :action '(help))
          (--> version "version"                :action '(version))
          (--> uptime  "uptime"                 :action '(uptime))
          (--> sources (alt "sources" "source") :action '(sources))))


(defun test/botil-grammar ()
  (map nil (lambda (test)
             (destructuring-bind (sentence expected) test
               (let ((result (parse-botil sentence)))
                 (unless (equal result expected)
                   (format t "Parsing:    ~S~%~
                            I get:      ~S~%~
                            instead of: ~S~%"
                           sentence result expected)))))
    '(
      ("query channel #lisp and nick pjb and \"cl-all\""
       (query (criteria (and  (channel "#lisp") (and (nick "pjb") (keywords "cl-all"))))))
      ("query \"cl\" and ( \"all\" or \"some\" or \"any\" ) and #lisp"
       (query (criteria (and (keywords "\"cl\"")
                         (and (or (keywords "all") (or (keywords "some") (keywords "any")))
                          (channel "#lisp"))))))
      ("connect irc.freenode.org"
       (connect (server "irc.freenode.org")))
      ("connect to irc.freenode.org"
       (connect (server "irc.freenode.org")))
      ("connect server irc.freenode.org"
       (connect (server "irc.freenode.org")))
      ("connect to server irc.freenode.org"
       (connect (server "irc.freenode.org")))
      ("reconnect irc.freenode.org"
       (reconnect (server "irc.freenode.org")))
      ("reconnect to irc.freenode.org"
       (reconnect (server "irc.freenode.org")))
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
      ("list channel logs on server irc.freenode.org"
       (list-channels (server "irc.freenode.org")))
      ("list channels"
       (list-channels nil))
      ("list logs"
       (list-channels nil))
      ("list logs on irc.freenode.org"
       (list-channels (server "irc.freenode.org")))
      ("list users"
       (list-users nil))
      ("list users on irc.freenode.org"
       (list-users (server "irc.freenode.org")))
      ("list servers"
       (list-servers))
      ("start logging channel #lisp"
       (start-log (channel "#lisp") nil))
      ("start logging channel #lisp on server irc.freenode.org"
       (start-log (channel "#lisp") (server "irc.freenode.org")))
      ("start log #lisp on irc.freenode.org"
       (start-log (channel "#lisp") (server "irc.freenode.org")))
      ("stop logging channel #lisp"
       (stop-log (channel "#lisp") nil))
      ("stop logging channel #lisp on server irc.freenode.org"
       (stop-log (channel "#lisp") (server "irc.freenode.org")))
      ("stop log #lisp on irc.freenode.org"
       (stop-log (channel "#lisp") (server "irc.freenode.org")))
      ("log channel #lisp on server irc.freenode.org                 to 2016-02-28"
       (delimited-log (channel "#lisp") (server "irc.freenode.org") nil (date "2016-02-28")))
      ("log channel #lisp on server irc.freenode.org from 2016-03-01 to 2016-03-31"
       (delimited-log (channel "#lisp") (server "irc.freenode.org") (date "2016-03-01") (date "2016-03-31")))
      ("log channel #lisp on server irc.freenode.org from 2016-06-01"
       (delimited-log (channel "#lisp") (server "irc.freenode.org") (date "2016-06-01") nil))
      ("log #lisp                      to 2016-02-28T00:00:00"
       (delimited-log (channel "#lisp") nil nil (date "2016-02-28T00:00:00")))
      ("log #lisp from 20160301T000000 to 2016-03-31T00:00:00"
       (delimited-log (channel "#lisp") nil (date "20160301T000000") (date "2016-03-31T00:00:00")))
      ("log #lisp from 20160601T000000"
       (delimited-log (channel "#lisp") nil (date "20160601T000000") nil))
      ("cancel all log on channel #lisp on server irc.freenode.org"
       (cancel-log-channel (channel "#lisp") (server "irc.freenode.org")))
      ("cancel all log on channel #lisp"
       (cancel-log-channel (channel "#lisp") nil))
      ("cancel all #lisp on irc.freenode.org"
       (cancel-log-channel (channel "#lisp") (server "irc.freenode.org")))
      ("cancel all #lisp"
       (cancel-log-channel (channel "#lisp") nil))
      ("cancel log 42"
       (cancel-log-id (id "42"))))))

(test/botil-grammar)


(defun reconnect (server)
  (irc:quit (connection server) "Will reconnect...")
  (setf (connection server) nil))

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
  (declare (ignore what))
  (let ((commands '(help version uptime sources)))
    (answer sender "Available commands: ~(~{~A~^, ~}~)." commands)))

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

(defun query (sender criteria)
  (answer sender "Queries are not implemented yet, sorry."))

(defun connect   (sender server))
(defun reconnect (sender server))
(defun enable    (sender server))
(defun disable   (sender server))
(defun list-servers  (sender))
(defun list-channels (sender server))
(defun list-users    (sender server))
(defun identify (sender password))
(defun register (sender email password))
(defun reset (sender nick))
(defun set-password (sender nick key password))
(defun start-log (sender channel server))
(defun stop-log (sender channel server))
(defun cancel-log-channel (sender channel server))
(defun cancel-log-id (sender id))
(defun delimited-log (sender channel server from-date to-date))

(defun interpret (server sender expression)
  (ecase (first expression)
    ((help)    (help       sender (rest expression)))
    ((version) (version    sender))
    ((uptime)  (uptime-cmd sender))
    ((sources) (source     sender))
    ((query)
     ;; (query (criteria (and (channel "#lisp") (and (nick "pjb") (keywords "cl-all"))))) 
     ;; (query (criteria (and (keywords "\"cl\"") (and (or (keywords "all") (or (keywords "some") (keywords "any"))) (channel "#lisp"))))) 
     )
    ((connect) 
     ;; (connect (server "irc.freenode.org"))
     (connect sender (second expression)))
    ((reconnect)
     ;; (reconnect (server "irc.freenode.org"))
     (reconnect sender (second expression)))
    ((disable)
     ;; (disable (server "irc.freenode.org"))
     (disable sender (second expression)))
    ((enable)
     ;; (enable (server "irc.freenode.org"))
     (enable sender (second expression)))
    ((list-servers)
     (list-servers sender))
    ((list-channels)
     ;; (list-channels (server "irc.freenode.org")) 
     ;; (list-channels nil)
     (list-channels sender (second expression)))
    ((list-users)
     ;; (list-users (server "irc.freenode.org")) 
     ;; (list-users nil)
     (list-users sender (second expression)))
    ((identify)
     ;; (identify (password "secret-password"))
     (identify sender (second expression)))
    ((register)
     ;; (register (email "pjb@informatimago.com") (password "secret-password"))
     (register sender (second expression) (third expression)))
    ((reset)
     ;; (reset (nick "pjb")) 
     ;; (reset nil) 
     (reset sender (second expression)))
    ((set-password)
     ;; (set-password (nick "pjb") (key "321545623f") (password "new-secret-password"))
     (set-password sender (second expression)
                   (third expression)
                   (fourth expression)))
    ((start-log)
     ;; (start-log (channel "#lisp") (server "irc.freenode.org")) 
     ;; (start-log (channel "#lisp") nil)
     (start-log sender (second expression) (third expression)))
    ((stop-log)
     ;; (stop-log (channel "#lisp") (server "irc.freenode.org")) 
     ;; (stop-log (channel "#lisp") nil) 
     (stop-log sender (second expression) (third expression)))
    ((cancel-log-channel)
     ;; (cancel-log-channel (channel "#lisp") (server "irc.freenode.org")) 
     ;; (cancel-log-channel (channel "#lisp") nil)
     (cancel-log-channel sender (second expression) (third expression)))
    ((cancel-log-id)
     ;; (cancel-log-id (id "42"))
     (cancel-log-id sender (second expression)))
    ((delimited-log)
     ;; (delimited-log (channel "#lisp") (server "irc.freenode.org") (date "2016-03-01") (date "2016-03-31")) 
     ;; (delimited-log (channel "#lisp") (server "irc.freenode.org") (date "2016-06-01") nil) 
     ;; (delimited-log (channel "#lisp") (server "irc.freenode.org") nil (date "2016-02-28")) 
     ;; (delimited-log (channel "#lisp") nil (date "20160301T000000") (date "2016-03-31T00:00:00")) 
     ;; (delimited-log (channel "#lisp") nil (date "20160601T000000") nil) 
     ;; (delimited-log (channel "#lisp") nil nil (date "2016-02-28T00:00:00")) 
     (delimited-log sender (second expression)
                    (third expression)
                    (fourth expression)
                    (fifth expression)))))


(defun command-processor (server sender command)
  (check-type server server)
  (check-type sender user)
  (check-type command string)
  (format *trace-output* "~&~S~%  ~S~%  ~S~%  ~S~%"
          'command-processor server sender command)
  (handler-case
      (let ((expression (parse-botil command)))
        (interpret server sender expression))
    (error (err)
      (answer sender "~A" (princ-to-string err)))))



(defun botil (server command)
  (ecase command
    ((reconnect) ;; we've connected to server, let's rejoin the channels and go on logging.
     (dolist (channel (logged-channels server))
       (start-logging-channel channel)))
    ((quit)
     (exit))))


(defun botil-initialize ()
  (setf *sender*            (make-worker-thread sender (server recipient message &rest arguments)
                              (let ((message (format nil "~?" message arguments)))
                                (format *trace-output* "~&~A <- ~A~%" recipient message)
                                (send-irc recipient message)))
        *query-processor*   (make-worker-thread query-processor (server sender message)
                              (format *trace-output* "~&~A -> ~A~%" sender message)
                              (query-processor server sender message))
        *logger*            (make-worker-thread logger (server message)
                              (format *trace-output* "~&~A -> ~A~%" (user message) message)
                              (logger server message))
        *command-processor* (make-worker-thread command-processor (server sender message)
                              (format *trace-output* "~&~A -> ~A~%" sender message)
                              (command-processor server sender message))
        *botil*             (make-worker-thread botil (server command)
                              (format *trace-output* "~&BOTIL <- ~A~%" command)
                              (botil server command)))
  (load-server-database))



;; Connect and 

;; source = "nickname" or "#channel"
;; user = "~t" "identified-user@host" etc
;; host = fqdn of user's host
;; command = "PRIVMSG"
;; arguments = ("command" "arguments"); for /msg botil hello world --> ("botil" "hello world")

;; #test-botil <test-botil> /msg botil hello how do you do?
;; (:sender "test-botil" :recipient "botil"       :arguments ("botil" "hello how do you do?"))
;; #test-botil <test-botil> How do you do? 
;; (:sender "test-botil" :recipient "#test-botil" :arguments ("#test-botil" "How do you do?"))


(defun make-msg-hook (server)
  (lambda (message)
    "Answers to PRIVMSG."
    (block msg-hook
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
              (send-worker *logger* server message))))
      t)))


(defun make-svc-hook (server)
  (lambda (message)
    "Answers to service messages."
    (block svc-hook
      (with-accessors ((sender irc:source)
                       (arguments irc:arguments)) message
        (let ((recipient (first arguments)))
          (format *trace-output*  "~&svc-hook message = ~S~%"
                  (list :server server
                        :sender sender
                        :recipient recipient
                        :command (irc:command message)
                        :arguments arguments))))
      (send-worker *logger* server message)
      t)))

  
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
            irc:irc-topic-message irc:irc-error-message
            irc:irc-mode-message
            ;; -
            irc:irc-nick-message irc:irc-join-message
            irc:irc-part-message irc:irc-quit-message
            irc:irc-kill-message irc:irc-kick-message
            irc:irc-invite-message))
    (send-worker *botil* 'reconnect server)))


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
        (loop
          (loop
            :for server :in *servers*
            :for connection := (connection server)
            :if connection
              :do (with-simple-restart (reconnect "Reconnect")
                    (catch :petites-gazongues
                      (irc:read-message connection) #|  goes to msg-hook or svc-hook.|#))
            :else
              :do (connect-to-server server)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
;; (untrace ensure-user)
;; (reconnect (first *servers*))
;; (ql:quickload :com.informatimago.small-cl-pgms.botil)
