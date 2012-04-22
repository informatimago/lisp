;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               group.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a function to read unix group files.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2012
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.UNIX.GROUP"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM")
  (:export "ENTRY-SHELL" "ENTRY-HOME" "ENTRY-GECOS" "ENTRY-GID" "ENTRY-UID"
           "ENTRY-GROUP" "ENTRY-NAME" "ENTRY" "READ-GROUP")
  (:documentation
   "This package exports a function to read unix group files.

    Copyright Pascal J. Bourguignon 2004 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.UNIX.GROUP")


;;  group_name:passwd:GID:user_list

(defstruct entry
  (name   "" :type string)
  (passwd "" :type string)
  (gid    0  :type integer)
  (users  () :type list)) ;;of string: the user names


(defun parse-group (line)
  (let ((fields (split-escaped-string line "\\" ":")))
    (if (= (length fields) 4)
        (let ((gid   (parse-integer (third fields) :junk-allowed nil))
              (users (split-escaped-string (fourth fields) "\\" ",")))
          (make-entry :name (first fields)
                      :passwd (second fields)
                      :gid gid
                      :users users))
        (warn "Invalid group line ~S~%" line))))
    

(defun read-group (&optional (group-file-path "/etc/group"))
  "
RETURN:  A list of COM.INFORMATIMAGO.COMMON-LISP.UNIX.GROUP:ENTRY structures.
"
  (mapcar (function parse-group)
          (with-open-file (in group-file-path
                              :direction :input
                              :if-does-not-exist :error)
            (stream-to-string-list in))))


;;;; THE END ;;;
