;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               passwd.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This package exports a function to read unix passwd files.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-19 <PJB> Renamed ENTRY -> USER.
;;;;    2004-03-31 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2004 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.UNIX.PASSWD"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM")
  (:export "USER-SHELL" "USER-HOME" "USER-GECOS" "USER-GID"
           "USER-UID" "USER-PASSWD" "USER-LOGIN" "USER" "READ-PASSWD")
  (:documentation
   "
This package exports a function to read unix passwd files.


License:

    AGPL3

    Copyright Pascal J. Bourguignon 2004 - 2015

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.UNIX.PASSWD")


;; pwent ::= login ':' passwd ':' uid ':' gid ':' gecos ':' home ':' shell


(defstruct user
  "A unix /etc/passwd USER."
  (login  "" :type string)
  (passwd "" :type string)
  (uid    0  :type integer)
  (gid    0  :type integer)
  (gecos  () :type list)
  (home   "" :type string)
  (shell  "" :type string))

(setf (documentation 'user-login 'function)
      "The login of the user (string)."
      (documentation 'user-passwd 'function)
      "The password of the user (string)."
      (documentation 'user-uid 'function)
      "The user ID (integer)."
      (documentation 'user-gid 'function)
      "The user Group ID (integer)."
      (documentation 'user-gecos 'function)
      "The user GECOS field (a list of strings)."
      (documentation 'user-home 'function)
      "The user home directory (string)."
      (documentation 'user-shell 'function)
      "The user shell (string).")



(defun parse-passwd (line)
  (let ((fields (split-escaped-string line "\\" ":")))
    (if (= (length fields) 7)
        (let ((uid   (parse-integer (third fields) :junk-allowed nil))
              (gid   (parse-integer (fourth fields) :junk-allowed nil))
              (gecos (split-escaped-string (fifth fields) "\\" ",")))
          (make-user :login (first fields)
                      :passwd (second fields)
                      :uid uid
                      :gid gid
                      :gecos gecos
                      :home (sixth fields)
                      :shell (seventh fields)))
        (warn "Invalid passwd line ~S~%" line))))


(defun read-passwd (&optional (passwd-file-path "/etc/passwd"))
  "
DO:                 Read a passwd file.
PASSWD-FILE-PATH:   The pathname of the passwd file. Default: \"/etc/passwd\".
RETURN:             A list of passwd USER structures.
"
  (mapcar (function parse-passwd)
          (with-open-file (in passwd-file-path
                              :direction :input
                              :if-does-not-exist :error)
            (stream-to-string-list in))))


;;;; THE END ;;;;
