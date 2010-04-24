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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PASSWD"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING" "COMMON-LISP")
  (:EXPORT "ENTRY-NAME" "ENTRY-SHELL" "ENTRY-HOME" "ENTRY-GECOS" "ENTRY-GID"
           "ENTRY-UID" "ENTRY-PASSWD" "ENTRY-LOGIN" "ENTRY" "READ-PASSWD")
  (:DOCUMENTATION
   "This package exports a function to read unix passwd files.

    Copyright Pascal J. Bourguignon 2004 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PASSWD")


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


(DEFUN PARSE-PASSWD (LINE)
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
    

(DEFUN READ-PASSWD (&OPTIONAL (PASSWD-FILE-PATH "/etc/passwd"))
  "
RETURN:  A list of COM.INFORMATIMAGO.COMMON-LISP.PASSWD:ENTRY structures.
"
  (MAPCAR (FUNCTION PARSE-passwd)
          (WITH-OPEN-FILE (IN passwd-FILE-PATH
                              :DIRECTION :INPUT
                              :IF-DOES-NOT-EXIST :ERROR)
            (STREAM-TO-STRING-LIST IN))))


;;;; passwd.lisp                      --                     --          ;;;;
