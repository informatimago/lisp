;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               nickserv.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Letting an IRC bot register its own nick with NickServ.
;;;;
;;;;    An unregistered bot is nagged by NickServ at every connection and
;;;;    can have its nick taken while it is down; but registering it by
;;;;    hand means stopping the bot, connecting in its place with its nick,
;;;;    and registering from a human client.  This lets the bot do it
;;;;    itself, on the order of an administrator, so that the confirmation
;;;;    mail reaches a human who never has to impersonate the bot.
;;;;
;;;;    The exchange, on Libera.Chat (Atheme services), is:
;;;;
;;;;        bot -> NickServ : REGISTER <password> <email>
;;;;        NickServ -> bot : an e-mail was sent to <email>
;;;;        (the human reads the mail, which carries the key)
;;;;        bot -> NickServ : VERIFY REGISTER <nick> <key>
;;;;        NickServ -> bot : <nick> has now been verified
;;;;
;;;;    and afterwards, at every connection:
;;;;
;;;;        bot -> NickServ : IDENTIFY <nick> <password>
;;;;
;;;;    The password is generated here -- no human ever needs to know it --
;;;;    and stored in a file readable by the bot only.  Storing it is what
;;;;    makes the whole thing worth doing: it is what lets the bot identify
;;;;    itself on every later connection.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2026-08-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2026 - 2026
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

(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.NICKSERV"
  (:use "COMMON-LISP")
  (:nicknames "NICKSERV")
  (:documentation "Self-registration of an IRC bot nick with NickServ.")
  (:export "*NICKSERV*"
           "GENERATE-PASSWORD"
           "REGISTER-MESSAGE" "VERIFY-MESSAGE" "IDENTIFY-MESSAGE"
           "PARSE-SELF-REGISTRY" "SELF-REGISTRY-USAGE"
           "LOAD-PASSWORD" "SAVE-PASSWORD" "PASSWORD-PRESENT-P" "FORGET-PASSWORD"
           "VALID-EMAIL-P"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.NICKSERV")


(defparameter *nickserv* "NickServ"
  "The nick of the nickname service.")

(defparameter *password-characters*
  "abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ23456789"
  "The alphabet passwords are drawn from: alphanumeric, minus the
characters that are easily confused with one another when a human has to
read the password out of a file (0/O, 1/l/I).")

(defparameter *password-length* 24)

(defun random-bytes (count)
  "COUNT bytes from /dev/urandom, or NIL when it cannot be read."
  (ignore-errors
   (with-open-file (input #P"/dev/urandom"
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist nil)
     (when input
       (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
         (when (= count (read-sequence bytes input))
           bytes))))))

(defun generate-password (&optional (length *password-length*))
  "A fresh random password.  Drawn from /dev/urandom when available; the
fallback on RANDOM is there so that this never signals, but it is not
meant to be relied upon."
  (let* ((alphabet (length *password-characters*))
         (bytes (random-bytes length))
         (state (unless bytes (make-random-state t))))
    (map 'string
         (lambda (index)
           (char *password-characters*
                 (if bytes
                     ;; The modulo bias is negligible here: 256 mod 56 with
                     ;; a 24-character password.
                     (mod (aref bytes index) alphabet)
                     (random alphabet state))))
         (let ((indices (make-array length)))
           (dotimes (i length indices)
             (setf (aref indices i) i))))))

(defun valid-email-p (string)
  "A deliberately loose check: enough to catch a mistyped command, not to
validate an address."
  (let ((at (position #\@ string)))
    (and at
         (plusp at)
         (< (1+ at) (length string))
         (find #\. string :start at)
         (not (find #\Space string))
         (not (find #\, string)))))


;;; The three messages sent to NickServ.

(defun register-message (password email)
  (format nil "REGISTER ~A ~A" password email))

(defun verify-message (nick key)
  (format nil "VERIFY REGISTER ~A ~A" nick key))

(defun identify-message (nick password)
  (format nil "IDENTIFY ~A ~A" nick password))


;;; Where the password is kept.

(defun save-password (pathname password)
  "Writes PASSWORD to PATHNAME, readable by its owner only.  Returns PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (output pathname
                          :direction :output
                          :external-format :utf-8
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-line password output))
  ;; WITH-OPEN-FILE cannot set the mode; do it now, before anyone looks.
  (ignore-errors
   (uiop:run-program (list "chmod" "600" (namestring (truename pathname)))
                     :ignore-error-status t))
  pathname)

(defun load-password (pathname)
  "The password stored in PATHNAME, or NIL when there is none."
  (let ((pathname (probe-file pathname)))
    (when pathname
      (with-open-file (input pathname :external-format :utf-8 :if-does-not-exist nil)
        (when input
          (let ((line (read-line input nil nil)))
            (when (and line (plusp (length (string-trim " " line))))
              (string-trim " " line))))))))

(defun password-present-p (pathname)
  (not (null (load-password pathname))))

(defun forget-password (pathname)
  "Removes the stored password.  Returns T when there was one.

The password is written before REGISTER is sent -- losing a password we
had already sent would leave the nick registered and the bot unable to
identify -- so a REGISTER the service refused (a bad address, or too many
accounts on it) leaves a password behind for a registration that never
happened.  This is how one gets rid of it and tries again."
  (let ((pathname (probe-file pathname)))
    (when pathname
      (delete-file pathname)
      t)))


;;; The "self-registry" command.

(defparameter *self-registry-usage*
  "Usage: self-registry <email> | verify <key> | status | forget")

(defun self-registry-usage () *self-registry-usage*)

(defun split-words (string)
  (let ((words '())
        (start nil))
    (dotimes (index (length string))
      (let ((space (member (char string index) '(#\Space #\Tab))))
        (cond ((and space start) (push (subseq string start index) words)
                                 (setf start nil))
              ((not (or space start)) (setf start index)))))
    (when start (push (subseq string start) words))
    (nreverse words)))

(defun parse-self-registry (arguments)
  "Parses the arguments of the self-registry command, given as a string or
as a list of words (the command word itself excluded).

Returns two values: the action -- :REGISTER, :VERIFY, :STATUS, :FORGET or
NIL when the arguments make no sense -- and its argument (the e-mail
address, the verification key, or NIL)."
  (let ((words (if (listp arguments) arguments (split-words arguments))))
    (cond
      ((null words) (values nil nil))
      ((string-equal "status" (first words)) (values :status nil))
      ((string-equal "forget" (first words)) (values :forget nil))
      ((string-equal "verify" (first words))
       (if (second words)
           (values :verify (second words))
           (values nil nil)))
      ((and (null (rest words)) (valid-email-p (first words)))
       (values :register (first words)))
      (t (values nil nil)))))

;;;; THE END ;;;;
