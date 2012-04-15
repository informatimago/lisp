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
;;;;    2004-03-31 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2004
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.UNIX.PASSWD"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM")
  (:export "ENTRY-NAME" "ENTRY-SHELL" "ENTRY-HOME" "ENTRY-GECOS" "ENTRY-GID"
           "ENTRY-UID" "ENTRY-PASSWD" "ENTRY-LOGIN" "ENTRY" "READ-PASSWD")
  (:documentation
   "This package exports a function to read unix passwd files.

    Copyright Pascal J. Bourguignon 2004 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.UNIX.PASSWD")


;; pwent ::= login ':' passwd ':' uid ':' gid ':' gecos ':' home ':' shell


(defstruct entry
  (login  "" :type string)
  (passwd "" :type string)
  (uid    0  :type integer)
  (gid    0  :type integer)
  (gecos  () :type list)
  (home   "" :type string)
  (shell  "" :type string))

(defmacro entry-name (entry)
  (warn "ENTRY-NAME is deprecated, use ENTRY-LOGIN")
  `(entry-login ,entry))


(defun parse-passwd (line)
  (let ((fields (split-escaped-string line "\\" ":")))
    (if (= (length fields) 7)
        (let ((uid   (parse-integer (third fields) :junk-allowed nil))
              (gid   (parse-integer (fourth fields) :junk-allowed nil))
              (gecos (split-escaped-string (fifth fields) "\\" ",")))
          (make-entry :login (first fields)
                      :passwd (second fields)
                      :uid uid
                      :gid gid
                      :gecos gecos
                      :home (sixth fields)
                      :shell (seventh fields)))
        (warn "Invalid passwd line ~S~%" line))))
    

(defun read-passwd (&optional (passwd-file-path "/etc/passwd"))
  "
RETURN:  A list of COM.INFORMATIMAGO.COMMON-LISP.UNIX.PASSWD:ENTRY structures.
"
  (mapcar (function parse-passwd)
          (with-open-file (in passwd-file-path
                              :direction :input
                              :if-does-not-exist :error)
            (stream-to-string-list in))))


;;;; THE END ;;;;
