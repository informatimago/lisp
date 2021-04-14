;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               botvot.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     IRC
;;;;DESCRIPTION
;;;;
;;;;    Botvot: an IRC bot monitoring Hacker News.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-04-14 <PJB> Reconverted into botvot.
;;;;    2019-08-27 <PJB> Added blacklist.
;;;;    2015-07-17 <PJB> Added commands: help uptime version sources; added restarts.
;;;;    2015-04-27 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2021
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

(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTVOT"
  (:use "COMMON-LISP"
        "CL-IRC" "SPLIT-SEQUENCE" "CL-PPCRE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
                "DATE" "UPTIME")
  (:export "MAIN" "BALLOT-REPL")
  (:documentation "
Botvot is a simple IRC bot to manage ballots.

It is run with:

   (com.informatimago.small-cl-pgms.botvot:main)

Copyright Pascal J. Bourguignon 2015 - 2021
Licensed under the AGPL3.
"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTVOT")

(defparameter *version* "1.0")


;;;
;;; Configuration:
;;;

(defvar *server*   "irc.freenode.org"
  "The fqdn of the IRC server.")
(defvar *nickname* "botvot"
  "The nickname of the botvot user.")
(defvar *sources-url*
  "https://gitlab.com/com-informatimago/com-informatimago/tree/master/small-cl-pgms/botvot/"
  "The URL where the sources of this ircbot can be found.")
(defvar *connection* nil
  "The current IRC server connection.")
(defvar *botpass*  "1234")

(defvar *ballots* '()
  "A list of ballots.")

(defvar *ballot-file* #P"/usr/local/var/botvot/ballot.sexp"
        "Path of the file where the *ballot* is saved.")

;; All the data is serialized to the *ballot-file* after each mutation,
;; and when zombie ballots are garbage collected.

(defvar *ballot-zombie-life* (days 15)
  "Delete closed or cancelled ballots older than that.")

(defvar *requester-nick* nil
  "Nickname of the user who send the current command.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun seconds (x) x)
  (defun minutes (x) (* x 60))
  (defun hours   (x) (* x 60 60))
  (defun days    (x) (* x 24 60 60)))

(defconstant +minimum-relative-deadline+ (minutes 30))

;;;
;;; Types and classes:
;;;

(deftype octet () `(unsigned-byte 8))
(deftype universal-time () `integer)
(deftype ballot-state () `(member :editing :open :closed :cancelled))

(defclass choice ()
  ((ballot-id :reader   ballot-id :type string :initarg :ballot-id)
   (id        :reader   choice-id :type string :initarg :choice-id)
   (title     :accessor title     :type string :initarg :title)))

(defclass vote ()
  ((ballot-id      :reader ballot-id      :type string :initarg :ballot-id)
   (user-id-hash   :reader user-id-hash   :type string :initarg :user-id-hash)
   (choice-id      :reader choice-id      :type string :initarg :choice-id)
   (choice-id-hash :reader choice-id-hash :type string :initarg :choice-id-hash)))

(defclass ballot ()
  ((id               :reader   ballot-id               :type string                   :initarg %id)
   (title            :accessor title                   :type string                   :initarg :title)
   (deadline         :reader   ballot-deadline         :type (or universal-time list) :initarg :deadline)
   (remind-period    :reader   ballot-remind-period    :type integer                  :initarg :remind-period)
   (owner            :reader   ballot-owner            :type string                   :initarg :owner)
   (password-hash    :reader   ballot-password-hash    :type string                   :initarg %password-hash)
   (secret-seed      :reader   ballot-secret-seed      :type (vector octet)           :initarg %secret-seed)
   (state            :reader   ballot-state            :type ballot-state             :initarg %state            :initform :editing)
   (last-publication :reader   ballot-last-publication :type universal-time           :initarg %last-publication :initform 0) 
   (channels         :reader   ballot-channels         :type list                     :initarg %channels         :initform '())
   (choices          :reader   ballot-choices          :type list                     :initarg %choices          :initform '())
   (votes            :reader   ballot-votes            :type list                     :initarg %votes            :initform '())))


#|

The BALLOT-ID are unique IDs amongst *ballots*. They could be reused
only when a ballot is deleted from the system.

The TITLE of a ballot can be changed at will by the ballot owner while
the ballot is in state :EDITING.

The DEADLINE is either a UNIVERSAL-TIME specifying an absolute
deadline, or a list (:relative offset), which specifies a deadline
relatively to the open datetime.  When the ballot goes to the :OPEN
state, the relative deadline is replaced by an absolute deadline
computed from the current universal-time.

The REMIND-PERIOD is a period (in seconds) for publishing reminders to
the ballot channels, before the deadline.  The time of reminder
publishing are computed from the deadline:

    (- deadline (* n remind-period))

The LAST-PUBLICATION universal-time is the date of the last
publication message.  A new publication can only occur after:

    (+ last-publication remind-period).

The OWNER of the ballot is the user-id of the user who created the
ballot and who is the sole user allowed to edit it and to open or
cancel it.  The owner specifies a password for critical operations
(open and cancel).

When the ballot is created, a random SECRET-SEED value is computed, to
be used to compute the other hashes.

The ballot STATES are: :EDITING :OPEN :CANCELED :CLOSED

- when created, the ballot is in the :EDITING state which
  allows modification of some parameters, such as title, and the choices.

- once ready the ballot can be put in the :OPEN state, which allows
  the users to vote, and reminders to be published to the channels.
  The relative deadlines are computed from the time the ballot goes to
  the :OPEN state.

- the owner can move a ballot to the :CANCELED state, which stops any
  operation for the ballot.

- when the deadline passes without the ballot being cancelled, the
  ballot goes to the :CLOSED state, and the results of the vote are
  then published, and available for query.

The CHANNELS is a list of IRC channel names (strings starting with
"#"), where the reminders and ballot results are sent.

The CHOICES is a list of ballot choices, that can be selected by the
voters.

The VOTES is a list of votes, at most one per user, selecting one choice.
When the ballot is closed those votes are tallied by choice.

The VOTE instances contain a user-id-hash to ensure that only one vote
per user is present, and a choice-id-hash to validate the vote of each
user.

    user-id-hash = (sha256-hash user-id)
    choice-id-hash = (sha256-hash (concat user-id-hash ballot-id choice-id))

|#



;;;
;;; Conditions
;;;

(define-condition deadline-syntax-error (error)
  ((words :initarg :words :reader deadline-syntax-error-words))
  (:report  (lambda (condition stream)
              (format stream "Invalid deadline specification syntax: ~{~A~^ ~}. Try: in X days|hours|minutes|seconds "
                      (deadline-syntax-error-words condition)))))

(define-condition deadline-too-close (error)
  ((specification    :initarg :specification             :reader deadline-too-close-deadline-specification)
   (minimum-relative :initarg :minimum-relative-deadline :reader deadline-too-close-minimum-relative-deadline))
  (:report (lambda (condition stream)
             (format stream "Deadline ~S too close.  Must be at least ~A hours from now."
                     (deadline-too-close-deadline-specification condition)
                     (deadline-too-close-minimum-relative-deadline condition)))))

(define-condition invalid-vote-data (error)
  ((vote           :initarg :vote           :reader invalid-vote-data-vote)
   (choice-id-hash :initarg :choice-id-hash :reader invalid-vote-data-choice-id-hash))
  (:report (lambda (condition stream)
             (format stream "Invalid vote choice-id-hash for ~S: ~S"
                     (invalid-vote-data-vote condition)
                     (invalid-vote-data-choice-id-hash condition)))))

(define-condition owner-error (error)
  ((ballot  :initarg :ballot  :reader owner-error-ballot)
   (user-id :initarg :user-id :reader owner-error-user-id))
  (:report (lambda (condition stream)
              (format stream "User ~S is not owner of ballot ~S"
                      (owner-error-user-id condition)
                      (owner-error-ballot condition)))))

(define-condition password-error (error)
  ((ballot  :initarg :ballot  :reader password-error-ballot))
  (:report (lambda (condition stream)
              (format stream "Bad password for ballot ~S"
                      (password-error-ballot condition)))))

(define-condition ballot-state-error (error)
  ((ballot    :initarg :ballot         :reader ballot-state-error-ballot)
   (state     :initarg :expected-state :reader ballot-state-error-expected-state)
   (operation :initarg :operation      :reader ballot-state-error-operation))
  (:report (lambda (condition stream)
             (format stream "Ballot ~S is not in the right state ~S for the operation ~A"
                     (ballot-state-error-ballot condition)
                     (ballot-state-error-expected-state condition)
                     (ballot-state-error-operation condition)))))

(define-condition no-ballot-error (error)
  ((id :initarg :id :reader no-ballot-error-ballot-id))
  (:report (lambda (condition stream)
             (format stream "No ballot with ID ~S; try: list ballots"
                     (no-ballot-error-ballot-id condition)))))

(define-condition already-voted-error (error)
  ((user-id   :initarg :user-id   :reader already-voted-error-user-id)
   (ballot-id :initarg :ballot-id :reader already-voted-error-ballot-id))
  (:report (lambda (condition stream)
             (format stream "User ~S has already voted to ballot ~S"
                     (already-voted-error-user-id condition)
                     (already-voted-error-ballot-id condition)))))

(define-condition syntax-error (error)
  ((command :initarg :command :reader syntax-error-command)
   (token   :initarg :token :reader syntax-error-token))
  (:report (lambda (condition stream)
             (format stream "Syntax error in command ~S; invalid token: ~S"
                     (syntax-error-command condition)
                     (syntax-error-token condition)))))

(define-condition double-quote-syntax-error (syntax-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Syntax error in command ~S; Missing closing double-quote"
                     (syntax-error-command condition)))))

(define-condition no-binding-error (error) ; internal-error ?
  ((variable :initarg :variable :reader no-binding-error-variable)
   (command  :initarg :command  :reader no-binding-error-command)
   (call     :initarg :call     :reader no-binding-error-call))
  (:report (lambda (condition stream)
             (format stream "No binding for variable ~S used in call ~S for command ~S"
                     (no-binding-error-variable condition)
                     (no-binding-error-command condition)
                     (no-binding-error-call condition)))))

(define-condition bad-channel-error (error)
  ((channel :initarg :channel :reader bad-channel-error-channel))
  (:report (lambda (condition stream)
             (format stream "Bad channel: ~A"
                     ( bad-channel-error-channel condition)))))
;;;
;;; Hash
;;;

(defun sha256-hash (text)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :sha256 (babel:string-to-octets text :encoding :utf-8))))

(defun sha256-hash-bytes (text)
  (ironclad:digest-sequence :sha256 (babel:string-to-octets text :encoding :utf-8)))

(defun sha256-hmac (secret text)
  (check-type secret (vector (unsigned-byte 8)))
  (check-type text string)
  (let ((hmac (ironclad:make-hmac secret :sha256)))
    (ironclad:update-hmac hmac (babel:string-to-octets text :encoding :utf-8))
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))


;;;
;;; Initialization, serialization:
;;;

(defun choice (ballot-id choice-id title)
  (make-instance 'choice
                 :ballot-id ballot-id
                 :choice-id choice-id
                 :title title))

(defmethod print-object ((choice choice) stream)
  (print-unreadable-object (choice stream :identity t :type t)
    (format stream "~S ~S ~S"
            (ballot-id choice)
            (choice-id choice)
            (title choice)))
  choice)

(defmethod serialize ((choice choice) stream)
  (format stream "(choice ~S ~S ~S)"
          (ballot-id choice)
          (choice-id choice)
          (title choice)))



(defun vote (ballot-id user-id-hash choice-id choice-id-hash)
  (make-instance 'vote
                 :ballot-id ballot-id
                 :choice-id choice-id
                 :user-id-hash user-id-hash
                 :choice-id-hash choice-id-hash))

(defmethod print-object ((vote vote) stream)
  (print-unreadable-object (vote stream :identity t :type t)
    (format stream "~S ~S ~S"
            (ballot-id vote)
            (user-id-hash vote)
            (choice-id vote)))
  vote)

(defmethod initialize-instance :after ((vote vote) &key (user-id nil user-id-p) &allow-other-keys)
  (if user-id-p
      ;; compute the new choice-id-hash:
      (let* ((user-id-hash   (sha256-hash user-id))
             (choice-id-hash (sha256-hash (format nil "~A|~A|~A"
                                                  user-id-hash
                                                  (ballot-id vote)
                                                  (choice-id vote)))))
        (setf (slot-value vote 'user-id-hash) user-id-hash
              (slot-value vote 'choice-id-hash) choice-id-hash))
      ;; validate the choice-id-hash:
      (let ((choice-id-hash (sha256-hash (format nil "~A|~A|~A"
                                                 (user-id-hash vote)
                                                 (ballot-id vote)
                                                 (choice-id vote)))))
        (unless (string-equal choice-id-hash (slot-value vote 'choice-id-hash))
          (error 'invalid-vote-data
                 :vote vote
                 :choice-id-hash choice-id-hash)))))

(defmethod serialize ((vote vote) stream)
  (format stream "(vote ~S ~S ~S ~S)"
          (ballot-id vote)
          (user-id-hash vote)
          (choice-id vote)
          (choice-id-hash vote)))


(defun compute-new-ballot-id (title ballots)
  (let ((stem (map 'string (lambda (word) (char-downcase (aref word 0)))
                   (split-sequence #\space title :remove-empty-subseqs t))))
    (loop
      :with id := stem
      :for i :from 1
      :while (find id ballots :key (function ballot-id) :test (function string-equal))
      :do (setf id (format nil "~A~D" stem i))
      :finally (return id))))

(defun compute-secret-seed (ballot-id ballot-owner)
  (sha256-hash-bytes (format nil "~A|~A|~36R"
                             ballot-id
                             ballot-owner
                             (random #.(expt 36 8)))))

(defun compute-password-hash (secret-seed password)
  (sha256-hmac secret-seed password))

(defmethod initialize-instance :after ((ballot ballot) &key (password nil passwordp) &allow-other-keys)
  (let* ((ballot-id      (compute-new-ballot-id (title ballot) *ballots*))
         (secret-seed    (when passwordp (compute-secret-seed ballot-id  (ballot-owner ballot))))
         (password-hash  (when passwordp (compute-password-hash secret-seed password))))
    (setf (slot-value ballot 'id) ballot-id)
    (when passwordp
      (setf (slot-value ballot 'secret-seed) secret-seed
            (slot-value ballot 'password-hash) password-hash))))

(defmethod print-object ((ballot ballot) stream)
  (print-unreadable-object (ballot stream :identity t :type t)
    (format stream "~S ~S (~{~S~^ ~}) (~D vote~:*~P) ~{~A~^ ~}"
            (ballot-id ballot)
            (title ballot)
            (mapcar (function choice-id) (ballot-choices ballot))
            (length (ballot-votes ballot))
            (ballot-channels ballot)))
  ballot)

(defun ballot (ballot-id title deadline remind-period owner password-hash secret-seed state last-publication channels choices votes)
  (make-instance 'ballot
                 '%id ballot-id
                 :title title
                 :deadline deadline
                 :remind-period remind-period
                 :owner owner
                 '%password-hash password-hash
                 '%secret-seed (coerce secret-seed '(vector octet))
                 '%state state
                 '%last-publication last-publication
                 '%channels channels
                 '%choices choices
                 '%votes votes))

(defmethod serialize ((ballot ballot) stream)
  (format stream "(ballot ~S ~S ~S ~S ~S ~S ~S ~S ~S "
          (ballot-id ballot)
          (title ballot)
          `(quote ,(ballot-deadline ballot))
          `(quote ,(ballot-remind-period ballot))
          (ballot-owner ballot)
          (ballot-password-hash ballot)
          (ballot-secret-seed ballot)
          (ballot-state ballot)
          (ballot-last-publication ballot))
  (format stream "(list ~{~S~^ ~})" (ballot-channels ballot))
  (terpri stream)
  (format stream "(list")
  (dolist (choice (ballot-choices ballot))
    (terpri stream)
    (serialize choice stream))
  (format stream ")")
  (terpri stream)
  (format stream "(list")
  (dolist (vote (ballot-votes ballot))
    (terpri stream)
    (serialize vote stream))
  (format stream ")")
  (format stream ")"))

;;;
;;; Commands
;;;

;; | new ballot $title $password                      | $ballot-id             | Creates a new ballot and issues a ballot ID for reference.
(defun new-ballot (owner password title &key
                                          (deadline '(:relative #.(days 1)))
                                          (remind-period #.(hours 1)))
  (let ((deadline
          (typecase deadline
            (integer
             (if (< (+ (get-universal-time) +minimum-relative-deadline+)
                    deadline)
                 deadline
                 (error 'deadline-too-close
                        :specification deadline
                        :minimum-relative-deadline (/ +minimum-relative-deadline+ (hours 1.0)))))
            (list
             (ecase (first deadline)
               (:relative
                (if (< +minimum-relative-deadline+ (second deadline))
                    deadline
                    (error 'deadline-too-close
                           :specification deadline
                           :minimum-relative-deadline (/ +minimum-relative-deadline+ (hours 1.0))))))))))
    (let ((ballot (make-instance 'ballot
                                 :title title
                                 :owner owner
                                 :password password
                                 :deadline deadline
                                 :remind-period remind-period)))
      (setf *ballots* (sort (cons ballot *ballots*)
                            (function string<)
                            :key (function ballot-id)))
      (format t "New ballot ID: ~S~%" (ballot-id ballot))))
  (save-ballots))

(defmethod user-is-owner ((ballot ballot) (user-id string))
  (string= user-id (ballot-owner ballot)))

(defun check-channel (channel)
  (unless (and (stringp channel)
               (< 1 (length channel))
               (char= #\# (aref channel 0))
               ;; find-channel finds only joined channels (channels in *connection*).
               ;; (if *connection*
               ;;     (find-channel *connection* channel)
               ;;     (char= #\# (aref channel 0)))
               )
    (error 'bad-channel-error :channel channel)))

(defmethod check-ballot-owner ((ballot ballot) (user-id string))
  (unless (user-is-owner ballot user-id)
    (error 'owner-error
           :ballot ballot
           :user-id user-id)))

(defmethod check-ballot-password ((ballot ballot) (password string))
  (let ((password-hash  (compute-password-hash (ballot-secret-seed ballot) password)))
    (unless (string-equal password-hash (ballot-password-hash ballot))
      (error 'password-error
             :ballot ballot))))

(defmethod check-ballot-state ((ballot ballot) (state symbol) operation)
  (unless (eql (ballot-state ballot) state)
    (error 'ballot-state-error
           :ballot ballot
           :expected-state state
           :operation operation)))

(defmethod check-ballot-state ((ballot ballot) (state cons) operation)
  (unless (member (ballot-state ballot) state)
    (error 'ballot-state-error
           :ballot ballot
           :expected-state state
           :operation operation)))

(defun find-ballot-id (ballot-id)
  (let ((ballot (find ballot-id *ballots*
                      :key (function ballot-id)
                      :test (function string-equal))))
    (unless ballot
      (error 'no-ballot-error :id ballot-id))
    ballot))

(defmethod find-vote ((ballot ballot) (user-id string))
  (let ((user-id-hash (sha256-hash user-id)))
    (find user-id-hash (ballot-votes ballot)
          :key (function user-id-hash)
          :test (function string-equal))))

;; | set [ballot] title         $ballot-id $title     |                        | Sets the title of a ballot.    Only in :editing state.
(defun set-ballot-title (user-id ballot-id title)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot :editing 'set-ballot-title)
    ;; TODO: Add some validation of title?
    (setf (slot-value ballot 'title) title))
  (save-ballots))

;; | set [ballot] dead[-]line   $ballot-id $dead-line |                        | Sets the deadline of a ballot. Only in :editing state.
(defun set-ballot-deadline (user-id ballot-id deadline)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot :editing 'set-ballot-deadline)
    ;; TODO: Add some validation of deadline
    (setf (slot-value ballot 'deadline) deadline))
  (save-ballots))

;; | set [ballot] remind-period $ballot-id $period    |                        | Sets the remind period of a ballot.
(defun set-ballot-remind-period (user-id ballot-id remind-period)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot :editing 'set-ballot-remind-period)
    ;; TODO: Add some validation of remind-period
    (setf (slot-value ballot 'remind-period) remind-period))
  (save-ballots))

(defun cl-user::fmt-datetime (stream date colon at &rest parameters)
  (declare (ignore colon at parameters))
  (format stream "~{~5*~4,'0D-~2:*~2,'0D-~2:*~2,'0D ~2:*~2,'0D:~2:*~2,'0D:~2:*~2,'0D~8*~}"
          (multiple-value-list (decode-universal-time date))))

(defmethod ballot-tally ((ballot ballot))
  (let ((tally (make-hash-table :test (function equal))))
    (dolist (vote (ballot-votes ballot))
      (incf (gethash (choice-id vote) tally 0)))
    (let ((votes '()))
      (maphash (lambda (choice-id count)
                 (push (list choice-id count) votes))
               tally)
      (sort votes (function >) :key (function second)))))

(defmethod format-ballot-tally ((ballot ballot))
  (let ((tally  (ballot-tally ballot))
        (nvotes (length (ballot-votes ballot))))
    (with-output-to-string (*standard-output*)
      (loop :for (choice-id count) :in tally
            :for percent = (/ (* 100.0 count) nvotes)
            :do (format t "~A ~A (~,1F%); " choice-id count percent)))))

(defmethod list-ballot ((ballot ballot))
  (format t "~8A ~9A ~S ~:[~A after open~*~;~*at ~/fmt-datetime/~] (~8A) ~A~%"
          (ballot-id ballot)
          (ballot-state ballot)
          (title ballot)
          (typep (ballot-deadline ballot) 'universal-time)
          (unless (typep (ballot-deadline ballot) 'universal-time)
            (d-dms (/ (second (ballot-deadline ballot)) 3600.0d0)))
          (when (typep (ballot-deadline ballot) 'universal-time)
            (ballot-deadline ballot))
          (ballot-owner ballot)
          (case (ballot-state ballot)
            (:editing   "")
            (:open      (format nil "~D vote~:*~P in." (length (ballot-votes ballot))))
            (:closed    (format-ballot-tally ballot))
            (:cancelled ""))))

;; | list [ballot[s]]                                 | list of ballots        | Lists all known ballots with their state and deadline or results.
(defun list-ballots ()
  (if *ballots*
   (dolist (ballot *ballots*)
     (list-ballot ballot))
   (format t "No ballots yet.  Use the new ballot command.~%")))

;; | show [ballot] $ballot-id                         | display info of ballot | Displays all the public information about the ballot.
(defun show-ballot (ballot-id)
  (let ((ballot (find-ballot-id ballot-id)))
    (list-ballot ballot)))


;; | add choice $ballot-id $choice-id $choice-title   |                        | Adds a new choice to the ballot.  Only in :editing state.
(defun add-choice (user-id ballot-id choice-id choice-title)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot :editing 'add-choice)
    (setf (slot-value ballot 'choices)
          (sort (cons (make-instance 'choice
                                     :ballot-id (ballot-id ballot)
                                     :choice-id choice-id
                                     :title choice-title)
                      (slot-value ballot 'choices))
                (function string<)
                :key (function choice-id)))
    (format t "There are ~D choice~:*~P in ballot ~A.~%"
            (length (ballot-choices ballot))
            (ballot-id ballot)))
  (save-ballots))

;; | delete choice $ballot-id $choice-id              |                        | Remove a choice from a ballot.    Only in :editing state.
(defun delete-choice (user-id ballot-id choice-id)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot :editing 'delete-choice)
    (setf (slot-value ballot 'choices)
          (delete choice-id (slot-value ballot 'choices)
                  :key (function choice-id)
                  :test (function string-equal)))
    (format t "There remain ~D choice~:*~P in ballot ~A.~%"
            (length (ballot-choices ballot))
            (ballot-id ballot)))
  (save-ballots))

;; | list choice[s] $ballot-id                        | list of choices        | Lists all the choices of a ballot.
;; | ballot [choice[s]] $ballot-id                    | list of choices        | Lists all the choices of a ballot. If the user has already voted, this will be indicated.
(defun list-choices (user-id ballot-id)
  (let ((ballot (find-ballot-id ballot-id)))
    (dolist (choice (ballot-choices ballot))
      (let ((user-has-voted-for-choice
              (and user-id
                   (let ((vote   (find-vote ballot user-id)))
                     (when vote
                       (string-equal (choice-id choice)
                                     (choice-id vote)))))))
        (format t "Ballot ~A: choice ~8A ~S~:[~; *~]~%"
                ballot-id (choice-id choice) (title choice)
                user-has-voted-for-choice)))))


(defun open-ballots ()
  (remove :open *ballots* :test-not (function eql) :key (function ballot-state)))

(defun open-channels ()
  (remove-duplicates (loop
                       :for ballot :in (open-ballots)
                       :append (ballot-channels ballot))
                     :test (function string-equal)))

(defun update-channels ()
  (when *connection*
    (let* ((myself           (find-user *connection* *nickname*))
           (open-channels    (open-channels))
           (current-channels (mapcar (function name) (channels myself)))
           (new-channels     (set-difference current-channels open-channels
                                             :test (function string-equal)))
           (old-channels     (set-difference open-channels current-channels
                                             :test (function string-equal))))
      (dolist (channel old-channels)
        (part *connection* channel))
      (dolist (channel new-channels)
        (join *connection* channel)))))


;; | add channel[s] $ballot-id #channel…              |                        | Add a channel to the ballot.      Only in :editing state.
(defun add-channels (user-id ballot-id channels)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot '(:editing :open) 'add-channels)
    (dolist (channel channels)
      (check-channel channel))
    (setf (slot-value ballot 'channels)
          (sort (remove-duplicates
                 (append channels (slot-value ballot 'channels))
                 :test (function string=))
                (function string<)))
    (update-channels)
    (list-ballot-channels ballot-id)
    (save-ballots)))

;; | remove channel[s] $ballot-id #channel               |                        | Remove channels from the ballot. Only in :editing state.
(defun remove-channels (user-id ballot-id channels)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot '(:editing :open) 'remove-channels)
    (dolist (channel channels)
      (check-channel channel))
    (setf (slot-value ballot 'channels)
          (set-difference (slot-value ballot 'channels)
                          channels
                          :test (function string=)))
    (update-channels)
    (list-ballot-channels ballot-id)
    (save-ballots)))

;; | list [ballot] channel[s] $ballot-id              | list of channels       | List the channels of the ballot.
(defun list-ballot-channels (ballot-id)
  (let ((ballot (find-ballot-id ballot-id)))
    (format t "Ballot ~A ~:[will be~;has been~] published in ~D channel~:*~P: ~{~A~^ ~}.~%"
            (ballot-id ballot)
            (member (ballot-state ballot) '(:editing :open))
            (length (ballot-channels ballot))
            (ballot-channels ballot))))

(defun absolute-deadline (deadline)
  (cond ((and (listp deadline) (eql ':relative (first deadline)))
         (+ (get-universal-time) (second deadline)))
        ((typep deadline 'universal-time)
         deadline)
        (t
         (error 'type-error
                :datum deadline
                :expected-type 'universal-time))))


(defmethod publish-ballot ((ballot ballot))
  (let ((message (case (ballot-state ballot)
                   (:open     (format nil "Ballot ~A ~S is open till ~/fmt-datetime/.  ~
                          Cast your vote with:  /msg ~A ballot ~A   ~
                          and:  ~2:*/msg ~A vote ~A $choice-id"
                                      (ballot-id ballot) (title ballot)
                                      (ballot-deadline ballot)
                                      *nickname* (ballot-id ballot)))
                   (:canceled (format nil "Ballot ~A ~S is canceled."
                                      (ballot-id ballot) (title ballot)))
                   (:closed   (format nil "Ballot ~A ~S is closed.  Results are: ~A"
                                      (ballot-id ballot) (title ballot)
                                      (format-ballot-tally ballot)))
                   (otherwise nil))))
    (when message
      (answer "~A" message)
      (setf (slot-value ballot 'last-publication) (get-universal-time))
      (when *connection*
        (dolist (channel (ballot-channels ballot))
          (privmsg *connection* channel message))))))


;; | open ballot $ballot-id $password                 |                        | Opens the ballot. From the :editing state.
(defun open-ballot (user-id password ballot-id)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot :editing 'open-ballot)
    (check-ballot-password ballot password)
    (setf (slot-value ballot 'state)    :open
          (slot-value ballot 'deadline) (absolute-deadline (ballot-deadline ballot)))
    (publish-ballot ballot)
    (save-ballots)))

;; | cancel ballot $ballot-id $password               |                        | Cancel a ballot.  We can cancel a ballot from the :editing or the :open state.
(defun cancel-ballot (user-id password ballot-id)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot '(:editing :open) 'cancel-ballot)
    (check-ballot-password ballot password)
    (setf (slot-value ballot 'state) :cancelled)
    (publish-ballot ballot)
    (save-ballots)))

;; | close ballot $ballot-id $password                 |                        | Close the ballot before the deadline. From the :opoen state.
(defun close-ballot (user-id password ballot-id)
  (let ((ballot (find-ballot-id ballot-id)))
    (check-ballot-owner ballot user-id)
    (check-ballot-state ballot :open 'close-ballot)
    (check-ballot-password ballot password)
    (setf (slot-value ballot 'state) :closed)
    (publish-ballot ballot)
    (save-ballots)))


;; | vote $ballot-id [choice] $choice-id              |                        | Cast or change a vote.  Only in :open state.
;; |                                                  |                        | If the same user votes several times, only the last vote is taken into account.
(defun vote-choice (user-id ballot-id choice-id)
  (let* ((ballot (find-ballot-id ballot-id))
         (vote   (find-vote ballot user-id)))
    (check-ballot-state ballot :open 'vote-choice)
    (if vote
        (error 'already-voted :user-id user-id :ballot-id ballot-id)
        ;; cast a new vote
        (progn
          (push (make-instance 'vote
                              :ballot-id ballot-id
                              :user-id user-id
                              :choice-id choice-id)
                (slot-value ballot 'votes))
          (save-ballots)))))

;; | [ballot] results [of] [ballot] $ballot-id        | list of vote results   | in :editing state, we only display the ballot info (same as show ballot $ballot-id);
;; |                                                  |                        | in :open state, we only display the number of casted votes;
;; |                                                  |                        | in :closed state, we display the vote results.
(defun show-ballot-results (user-id ballot-id)
  (let ((ballot (find-ballot-id ballot-id)))
    (ecase (ballot-state ballot)
      (:editing
       (list-ballot ballot))
      (:open
       (format t "~D vote~:*~P have been casted in ballot ~A~%"
               (length (ballot-votes ballot))
               (ballot-id ballot))
       (when (user-is-owner ballot user-id)
         (ballot-tally ballot)))
      (:closed
       (ballot-tally ballot))
      (:cancelled
       (format t "Ballot ~A has been cancelled.~%"
               (ballot-id ballot))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-ballots ()
  (setf *ballots* (eval (sexp-file-contents *ballot-file* :if-does-not-exist '())))
  (update-channels)
  (values))

(defun save-ballots ()
  (setf (text-file-contents *ballot-file*)
        (with-output-to-string (out)
          (princ "(list " out)
          (terpri out)
          (dolist (ballot *ballots*)
            (serialize ballot out)
            (terpri out))
          (princ ")" out)
          (terpri out)))
  (values))

(defun garbage-collect-ballots ()
  (let ((live-ballots (remove-if (lambda (ballot)
                                   (and (member (ballot-state ballot) '(:canceled :closed))
                                        (< (+ (ballot-last-publication ballot)
                                              *ballot-zombie-life*)
                                           (get-universal-time))))
                                 *ballots*)))
    (when (/= (length live-ballots)
              (length *ballots*))
      (setf *ballots* live-ballots)
      (save-ballots))))

(defun publish-ballots ()
  ;; Recently published, do nothing:
  ;; | deadline-(n+1)*remind  | last    | now   | last+remind            | deadline-n*remind             |    …    | deadline-remind    | deadline
  ;;
  ;; Recently published, do nothing:
  ;; | deadline-(n+1)*remind  | last            | last+remind   | now    | deadline-n*remind             |    …    | deadline-remind    | deadline
  ;;
  ;; Publish again:
  ;; | deadline-(n+1)*remind  | last            | last+remind            | deadline-n*remind    | now    |    …    | deadline-remind    | deadline
  (let ((now (get-universal-time)))
    (dolist (ballot (remove :open *ballots* :test-not (function eql) :key (function ballot-state)))
      (when (or (zerop (ballot-last-publication ballot))
                (and (< (+ (ballot-last-publication ballot) (ballot-remind-period ballot)) now)
                     (< (let ((n (truncate (- (ballot-deadline ballot) (ballot-last-publication ballot))
                                           (ballot-remind-period ballot))))
                          (- (ballot-deadline ballot) (* n (ballot-remind-period ballot))))
                        now)))
        (publish-ballot ballot)))))


;;;
;;; Syntax of Commands
;;;

#|

The *commands* list specifies the syntax of all botvot commands.

Each sublist in *commands* is of the form: (syntax docstring --> call)

SYNTAX is a list specifying the syntax of the command.
Each element in the list can be:

- a string specifying a terminal;

- a symbol specifying a parameter word or string; the word in the
  command is then bound to that symbol, which can be refered to in the
  CALL sexp.

- a list starting with the symbol OPT specifying a sequence of
  optional tokens.

TODO: parsing OPT sequence is not implemented yet, only a single token is allowed in the OPT element.

- a list starting with the symbol ALT specifying alternate tokens.

- a list starting with the symbol OPT-ALT specifying optional
  alternate tokens.

The list can be a dotted-list, in which case the last element must be
a symbol that will be bound to the remaining words in the command.

The DOCSTRING is displayed in the help for the command.

The symbol --> is required and ignored.  It's for esthetics.

The CALL sexp is a list containing the name of a function, followed by
arguments.  The arguments may be:

- the symbol USER-ID which is then substituted by the value of the
  *REQUESTER-NICK*;

- the symbol NIL which is then passed to the function;

- a list which is processed recursively as the CALL sexp, the result
  of the function call used as argument;

- any other symbol is substituted by the word bound to it from parsing
  the command.

|#

(defparameter *commands*
  '((("new" "ballot" title password)
     "Creates a new ballot and issues a ballot ID for reference."
     --> (new-ballot user-id password title))
    (("set" (opt "ballot") "title" ballot-id title)
     "Sets the title of a ballot.    Only in :editing state."
     --> (set-ballot-title user-id ballot-id))
    (("set" (opt "ballot") (alt "dead-line" "deadline") ballot-id . deadline)
     "Sets the deadline of a ballot. Only in :editing state. "
     --> (set-ballot-deadline user-id ballot-id (parse-deadline deadline)))
    (("set" (opt "ballot") "remind-period" ballot-id . period)
     "Sets the remind period of a ballot."
     --> (set-ballot-remind-period user-id ballot-id (parse-period period)))
    (("list" (opt-alt "ballot" "ballots"))
     "Lists all known ballots with their state and deadline or results."
     --> (list-ballots))
    (("show" (opt "ballot") ballot-id)
     "Displays all the public information about the ballot."
     --> (show-ballot ballot-id))
    (("add"    "choice" ballot-id choice-id choice-title)
     "Adds a new choice to the ballot.  Only in :editing state."
     --> (add-choice user-id ballot-id choice-id choice-title))
    (("delete" "choice" ballot-id choice-id)
     "Remove a choice from a ballot.    Only in :editing state."
     --> (delete-choice user-id ballot-id choice-id))
    (("list"   (alt "choice" "choices") ballot-id)
     "Lists all the choices of a ballot."
     --> (list-choices nil ballot-id))
    (("ballot" (opt-alt "choice" "choices") ballot-id)
     "Lists all the choices of a ballot. If the user has already voted, this will be indicated."
     --> (list-choices user-id ballot-id))
    (("add"    (alt "channel" "channels") ballot-id . channels)
     "Add a channel to the ballot.      Only in :editing state. "
     --> (add-channels user-id ballot-id channels))
    (("remove" (alt "channel" "channels") ballot-id . channels)
     "Remove channels from the ballot.  Only in :editing state."
     --> (remove-channels user-id ballot-id channels))
    (("list" (opt "ballot") (alt "channel" "channels") ballot-id)
     "List the channels of the ballot."
     --> (list-ballot-channels ballot-id))
    (("open"   (opt "ballot") ballot-id password)
     "Opens the ballot. From the :editing state."
     --> (open-ballot user-id password ballot-id))
    (("cancel" (opt "ballot") ballot-id password)
     "Cancel a ballot.  We can cancel a ballot from the :editing or the :open state."
     --> (cancel-ballot user-id password ballot-id))
    (("close" (opt "ballot") ballot-id password)
     "Closes the ballot before the deadline. From the :open state."
     --> (close-ballot user-id password ballot-id))
    (("vote" ballot-id (opt "choice") choice-id)
     "Cast or change a vote.  Only in :open state."
     --> (vote-choice user-id ballot-id choice-id))
    (((alt "results" "result") (opt "of") (opt "ballot") ballot-id)
     "Display vote results (tally only in the :closed state)"
     --> (show-ballot-results user-id ballot-id))
    ;; --------------------
    (("help" . command)
     "Display this help."
     --> (help command))
    (("version")
     "Display the version of this bot."
     --> (display-version))
    (("uptime")
     "Display the uptime of this bot."
     --> (display-uptime))
    (("sources")
     "Display information about the sources and license of this bot."
     --> (display-sources))
    (("reconnect" botpass)
     "Instructs the bot to close the connection to the IRC server and reconnect."
     --> (reconnect-command botpass))))


;;;
;;; Parsing and processing commands:
;;;

(defun parse-hms (word)
  (* 3600 (dms-d word)))

(defun parse-time (words)
  ;; ("HH:MM")
  ;; ("HH:MM:SS")
  ;; ("HH" "hour|hours|oclock|o'clock")
  (cond
    ((and (= 1 (length words))
          (member (count #\: (first words)) '(1 2)))
     (parse-hms (first words)))
    ((and (= 2 (length words))
          (member (second words)
                  '("hour" "hours" "oclock" "o'clock")
                  :test (function string-equal)))
     (ignore-errors (hours (parse-integer (first words)))))
    (t nil)))

(defun today (&optional time)
  (if time
      (let ((tz 0)
            (time (round time)))
        (multiple-value-bind (se mi ho da mo ye dow dst tz) (decode-universal-time (get-universal-time) tz)
          (declare (ignore dst dow))
          (declare (ignore se mi ho))
          (multiple-value-bind (se2 mi2 ho2) (decode-universal-time time 0)
            (encode-universal-time se2 mi2 ho2 da mo ye tz))))
      (get-universal-time)))

(defun next-dow (next-dow time)
  (let ((tz 0))
    (multiple-value-bind (se mi ho da mo ye dow dst tz) (decode-universal-time (get-universal-time) tz)
      (declare (ignore se mi ho dst))
      (multiple-value-bind (se2 mi2 ho2) (decode-universal-time time 0)
        (if (< dow next-dow)
            (encode-universal-time se2 mi2 ho2 (+ da (- next-dow dow)) mo ye tz)
            (encode-universal-time se2 mi2 ho2 (+ da 7 (- next-dow dow)) mo ye tz))))))

(defun next-monday    (time) (next-dow 0 time))
(defun next-tuesday   (time) (next-dow 1 time))
(defun next-wednesday (time) (next-dow 2 time))
(defun next-thirsday  (time) (next-dow 3 time))
(defun next-friday    (time) (next-dow 4 time))
(defun next-saturday  (time) (next-dow 5 time))
(defun next-sunday    (time) (next-dow 6 time))

(defun parse-deadline (words)
  ;; in $x days|minutes|seconds                    
  ;; on mon|tue|wed|thi|fri|sat|sun at $h [hours|o'clock]         
  ;; on mon|tue|wed|thi|fri|sat|sun at $HH:MM         
  ;; at $h [hours|o'clock]                         
  ;; at $HH:MM                                     
  (let ((deadline
         (cond ((string= "in" (first words))
                (let ((n (ignore-errors (parse-integer (second words)))))
                  (when n
                    (let ((unit (first (find (third words)
                                             '((days    "day" "days" "d")
                                               (hours   "hour" "hours" "h")
                                               (minutes "minutes" "minute" "min" "m")
                                               (seconds "seconds" "second" "sec" "s"))
                                             :test (lambda (w u)
                                                     (member w u :test (function string-equal)))))))
                      (when unit
                        `(:relative ,(funcall unit n)))))))
               ((and (string= "on" (first words))
                     (string= "at" (third words)))
                (let ((dow (first (find (second words)
                                        '((next-monday "monday" "mon")
                                          (next-tuesday "tuesday" "tue")
                                          (next-wednesday "wednesday" "wed")
                                          (next-thirsday "thirsday" "thi")
                                          (next-friday "friday" "fri")
                                          (next-saturday "saturday" "sat")
                                          (next-sunday "sunday" "sun"))
                                        :test (lambda (w u)
                                                (member w u :test (function string-equal)))))))
                  (when dow
                    (let ((time (parse-time (subseq words 3))))
                      (when time 
                        (funcall dow time))))))
               ((string= "at" (first words))
                (let ((time (parse-time (subseq words 1))))
                  (when time
                    (today time)))))))
    (or deadline
        (error 'deadline-syntax-error :words words))))

(defun parse-period (words)
  (error "Not implemented yet."))

(defun format-command-syntax (command-syntax)
  (with-output-to-string (*standard-output*)
    (labels ((format-word (syntax)
               (cond
                 ((null syntax)
                  nil)
                 ((atom syntax)
                  ;; dotted list
                  (if (symbolp syntax)
                      (format t "$~(~A~)…" syntax)
                      (format t "~(~A~)…" syntax))
                  nil)
                 ((symbolp (first syntax))
                  (format t "$~(~A~)" (pop syntax))
                  syntax)
                 ((stringp (first syntax))
                  (format t "~(~A~)" (pop syntax))
                  syntax)
                 ((listp (first syntax))
                  (case (first (first syntax))
                    (opt
                     (format t "[~A]"
                             (format-command-syntax (rest (first syntax))))
                     (rest syntax))
                    (alt
                     (format t "~{~A~^|~}"
                             (mapcar (lambda (token)
                                       (format-command-syntax (list token)))
                                     (rest (first syntax))))
                     (rest syntax))
                    (opt-alt
                     (format t "[~{~A~^|~}]"
                             (mapcar (lambda (token)
                                       (format-command-syntax (list token)))
                                     (rest (first syntax))))
                     (rest syntax))
                    (t
                     (error 'syntax-error :command command-syntax :token syntax))))
                 (t
                  (error 'syntax-error :command command-syntax :token syntax)))))
      (loop
        :for sep := "" :then " "
        :for rest-command-syntax := (progn (princ sep)
                                           (format-word command-syntax))
        :do (setf command-syntax rest-command-syntax)
        :while command-syntax))))

(defun command-matches-approximatively (command-syntax words)
  (let ((flat-syntax
          (labels ((flatten-syntax (syntax)
                     (cond ((atom syntax) nil)
                           ((stringp (first syntax))
                            (cons (first syntax)
                                  (flatten-syntax (rest syntax))))
                           ((symbolp (first syntax))
                            (flatten-syntax (rest syntax)))
                           ((listp (first syntax))
                            (case (first (first syntax))
                              ((opt alt opt-alt)
                               (nconc (flatten-syntax (rest (first syntax)))
                                      (flatten-syntax (rest syntax))))
                              (otherwise
                               (error 'syntax-error :command command-syntax :token syntax))))
                           (t
                            (error 'syntax-error :command command-syntax :token syntax)))))
            (flatten-syntax command-syntax))))
    (loop :for word := (pop words)
          :for m := (member word flat-syntax :test (function string-equal))
            :then (member word m :test (function string-equal))
          :while (and m words)
          :finally (return (and m (null words))))))

(defun command-matches-exactly (command-syntax words)
  (loop
    :with bindings := '()
    :do (cond
          ((null command-syntax)
           (return-from command-matches-exactly
             (values (null words) bindings)))
          ((symbolp command-syntax) ;; dotted-list
           (push (cons command-syntax words) bindings)
           (return-from command-matches-exactly
             (values t bindings)))
          ((atom command-syntax)
           (error 'syntax-error :command command-syntax :token command-syntax))
          (t (let ((syntax-element (pop command-syntax)))
               (cond ((symbolp syntax-element)
                      (if words
                          (push (cons syntax-element (pop words)) bindings)
                          (return-from command-matches-exactly
                            (values nil bindings))))
                     ((stringp syntax-element)
                      (if (and words (string-equal (first words) syntax-element))
                          (pop words)
                          (return-from command-matches-exactly
                            (values nil bindings))))
                     ((atom syntax-element)
                      (error 'syntax-error :command command-syntax :token syntax-element))
                     (t
                      (case (first syntax-element)
                        ((opt-alt opt)  ; 0 or 1
                         ;; TODO: parse correctly (opt tok1 tok2 … tokn) vs. (opt-alt tok1 tok2 … tokn)
                         (if (member (first words) (rest syntax-element)
                                     :test (function string-equal))
                             (pop words)))
                        (alt            ; need 1
                         (if (member (first words) (rest syntax-element)
                                     :test (function string-equal))
                             (pop words)
                             (return-from command-matches-exactly
                               (values nil bindings))))
                        (otherwise
                         (error 'syntax-error :command command-syntax :token syntax-element))))))))))

;; | ("set" (opt "ballot") "title" ballot-id title)                   | exact | ¬exact
;; |------------------------------------------------------------------+-------+--------+
;; | ("set")                                                          | nil   | t
;; | ("ballot")                                                       | nil   | t
;; | ("title")                                                        | nil   | t
;; | ("ballot" "title")                                               | nil   | t
;; | ("set" "ballot" "title" "pg1" "Presentation Garbage Collection") | t     | t
;; | ("set" "ballot" "title" "pg1")                                   | t     | t
;; | ("set" "title" "pg1" "Presentation Garbage Collection")          | t     | t
;; | ("set" "title" "pg1")                                            | t     | t

(defun match-command (words commands &key (exact t))
  (if exact
      (loop :for (command docstring nil call) :in commands
            :do (multiple-value-bind (matches bindings) (command-matches-exactly command words)
                  (when matches
                    (return-from match-command (values (list command)
                                                       docstring
                                                       bindings
                                                       call)))))
      (loop :for command :in commands
            :when (command-matches-approximatively (first command) words)
              :collect command)))

(defun print-command-help (command)
  (destructuring-bind (syntax docstring --> call) command
    (declare (ignore --> call))
    (answer "~A -- ~A" (format-command-syntax syntax) docstring)))

(defun list-commands (commands)
  (loop :for i :from 0
        :for command :in commands
          :initially (answer "List of commands:")
        :do (answer "~A" (format-command-syntax (first command)))
            (if (zerop (mod i 10))
                (sleep 2)
                (sleep 0.5))
        :finally (answer "Done")))

(defun help (command)
  (if command
      (let ((matched-commands (match-command command *commands* :exact nil)))
        (if (rest matched-commands)
            (list-commands matched-commands)
            (print-command-help (first matched-commands))))
      (list-commands *commands*)))

(defun display-version ()
  (answer "Version: ~A" *version*))

(defun display-uptime ()
  (answer "~A" (substitute #\space #\newline
                           (with-output-to-string (*standard-output*)
                             (date) (uptime)))))

(defun display-sources ()
  (answer "I'm an IRC bot helping to define ballots and cast votes on them; ~
                          under AGPL3 license, my sources are available at <~A>."
          *sources-url*))

(defun reconnect-command (botpass)
  (if (string= botpass *botpass*)
      (progn (answer "Reconnecting…")
             (reconnect))
      (answer "I'm not in the mood.")))

(defun parse-words (command)
  "We parse the irc message splitting words on spaces,
but with double-quotes escaping them."
  (loop
    :with words := '()
    :with word := '()
    :with state := :out
    :for ch :across command
    :do (case state
          (:out (case ch
                  (#\"
                   (setf state :in
                         word '()))
                  (#\space
                   (when word
                     (push (coerce (nreverse word) 'string) words)
                     (setf word '())))
                  (otherwise
                   (push ch word))))
          (:in  (case ch
                  (#\"
                   (push (coerce (nreverse word) 'string) words)
                   (setf state :out
                         word '()))
                  (#\\
                   (setf state :escape))
                  (otherwise
                   (push ch word))))
          (:escape
           (push ch word)
           (setf state :in)))
    :finally (case state
               (:out
                (when word
                  (push (coerce (nreverse word) 'string) words)
                  (setf word '()))
                (return (nreverse words)))
               (otherwise
                (error 'double-quote-syntax-error
                       :command command
                       :token (coerce (nreverse word) 'string))))))

(defun call-command (command call bindings)
  (let ((fun  (first call))
        (args (mapcar (lambda (arg)
                        (cond
                          ((stringp arg) arg)
                          ((null arg) nil)
                          ((eql 'user-id arg) *requester-nick*)
                          ((consp arg)
                           (call-command command arg bindings))
                          (t
                           (let ((binding (assoc arg bindings)))
                             (if binding
                                 (cdr binding)
                                 (error 'no-binding-error
                                        :variable arg
                                        :command command
                                        :call call))))))
                      (rest call))))
    (apply fun args)))

(defun process-command (words)
  (handler-bind ((error (lambda (err)
                          (print-backtrace)
                          (format *error-output* "~%~A~%" err)
                          (return-from process-command))))
    (unwind-protect
         (multiple-value-bind (matches docstring bindings call)
             (match-command words *commands* :exact t)
           (declare (ignore docstring))
           (if matches
               (call-command matches call bindings)
               (answer "Invalid command: ~{~S~^ ~}" words)))
      (garbage-collect-ballots))))

;;;
;;; A little ballot REPL to try out the command processing from the
;;; lisp REPL instead of thru a IRC connection: 
;;;

(defun ballot-repl ()
  (unwind-protect
       (let ((*requester-nick* (first (last (pathname-directory (user-homedir-pathname))))))
         (load-ballots)
         (loop
           :for command := (progn (format t "~&> ") (finish-output) (read-line))
           :when (string-equal "quit" (string-trim #(#\space #\tab) command))
             :do (loop-finish)
           :do (with-simple-restart (continue "Continue REPL")
                 (handler-bind ((error (function invoke-debugger)))
                   (process-command (parse-words command))))))
    (save-ballots)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The IRC agent:
;;;

(defun configure ()
  (setf *server*   (or (uiop:getenv "BOTVOT_SERVER")    *server*)
        *nickname* (or (uiop:getenv "BOTVOT_NICKNAME")  *nickname*)
        *botpass*  (or (uiop:getenv "BOTVOT_BOTPASS")   *botpass*)))

(defun say (&rest args)
  (format *trace-output* "~{~A~^ ~}~%" args)
  (force-output *trace-output*))

(defun answer (format-control &rest format-arguments)
  (let ((text (apply (function format) nil format-control format-arguments)))
    (say text)
    (when (and *connection* *requester-nick*)
      (privmsg *connection* *requester-nick* text))))

(defun msg-hook (message)
  "Answers to PRIVMSG sent directly to this bot."
  #-(and)
  (with-slots (source user host command arguments connection received-time raw-message-string)
      message
    (format t "~20A = ~S~%" 'source source) 
    (format t "~20A = ~S~%" 'user user) 
    (format t "~20A = ~S~%" 'host host) 
    (format t "~20A = ~S~%" 'command command) 
    (format t "~20A = ~S~%" 'arguments arguments) 
    (format t "~20A = ~S~%" 'connection connection) 
    (format t "~20A = ~S~%" 'received-time received-time) 
    (format t "~20A = ~S~%" 'raw-message-string
            (map 'string (lambda (ch)
                           (let ((code (char-code ch)))
                             (if (< code 32)
                                 (code-char (+ code 9216))
                                 ;; (aref "␀␁␂␃␄␅␆␇␈␉␊␋␌␍␎␏␐␑␒␓␔␕␖␗␘␙␚␛␜␝␞␟␠␡" code)
                                 ch)))
                 raw-message-string))
    (finish-output))
  (let ((arguments        (arguments message))
        (*requester-nick* (source message)))
    (when (string= *nickname* (first arguments))
      (dolist (line (split-sequence
                     #\newline
                     (with-output-to-string (*standard-output*)
                       (process-command (parse-words (second arguments))))
                     :remove-empty-subseqs t))
        (answer "~A" line))))
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
  "The main program of the botvot IRC bot.
We connect and reconnect to the *SERVER* under the *NICKNAME*,
and join to the *CHANNEL* where HackerNews are published."
  (let ((*package* (load-time-value (find-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTVOT"))))
    (configure)
    (load-ballots)
    (catch :gazongues
      (with-simple-restart (quit "Quit")
        (with-retry (sleep (+ 10 (random 30)))
          (catch :petites-gazongues
            (with-simple-restart (reconnect "Reconnect")
              (unwind-protect
                   (progn
                     (setf *connection* (connect :nickname *nickname* :server *server*))
                     (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
                     (load-ballots)
                     (loop
                       :with next-time = (+ *period* (get-universal-time))
                       :for time = (get-universal-time)
                       :do (if (<= next-time time)
                               (progn
                                 (handler-bind
                                     ((error (lambda (err)
                                               (print-backtrace)
                                               (format *error-output* "~%~A~%" err))))
                                   (publish-ballots))
                                 (incf next-time *period*))
                               (read-message *connection*) #|there's a 10 s timeout in here.|#)))
                (save-ballots)
                (when *connection*
                  (quit *connection*)
                  (setf *connection* nil))))))))))


;; (join *connection* *channel*)
;; (monitor-initialize)
;; (monitor-hacker-news (lambda (message) (privmsg *connection* *channel* message)))




#-(and) (progn
          
          (setf *nickname* "botvot-test"
                *ballot-file* #P"/tmp/ballot.sexp"
                *ballot-zombie-life* (minutes 10))

          (bt:make-thread (function main) :name "botvol ircbot")

          (*requester-user* (format nil "~A!~A@~A"
                                    (nickname user)
                                    (username user)
                                    (hostname user)))

          (find-user *connection* *requester-nick*)
          
          (channels (find-user *connection* *nickname*))

          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THE END ;;;;
