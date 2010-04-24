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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2004
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GROUP"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING" "COMMON-LISP")
  (:EXPORT "ENTRY-SHELL" "ENTRY-HOME" "ENTRY-GECOS" "ENTRY-GID" "ENTRY-UID"
           "ENTRY-GROUP" "ENTRY-NAME" "ENTRY" "READ-GROUP")
  (:DOCUMENTATION
   "This package exports a function to read unix group files.

    Copyright Pascal J. Bourguignon 2004 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GROUP")


;;  group_name:passwd:GID:user_list

(defstruct entry
  (name   "" :type string)
  (passwd "" :type string)
  (gid    0  :type integer)
  (users  () :type list) ;;of string: the user names
  )                      ;;entry


(DEFUN PARSE-GROUP (LINE)
  (let ((fields (split-escaped-string line "\\" ":")))
    (if (= (length fields) 4)
        (let ((gid   (parse-integer (third fields) :junk-allowed nil))
              (users (split-escaped-string (fourth fields) "\\" ",")))
          (make-entry :name (first fields)
                      :passwd (second fields)
                      :gid gid
                      :users users))
        (warn "Invalid group line ~S~%" line)))) ;;PARSE-GROUP
    

(DEFUN READ-GROUP (&OPTIONAL (GROUP-FILE-PATH "/etc/group"))
  "
RETURN:  A list of COM.INFORMATIMAGO.COMMON-LISP.GROUP:ENTRY structures.
"
  (MAPCAR (FUNCTION PARSE-group)
          (WITH-OPEN-FILE (IN group-FILE-PATH
                              :DIRECTION :INPUT
                              :IF-DOES-NOT-EXIST :ERROR)
            (STREAM-TO-STRING-LIST IN)))
  ) ;;READ-GROUP


;;;; group.lisp                       --                     --          ;;;;
