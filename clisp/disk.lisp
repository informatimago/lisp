;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               disk.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             clisp
;;;;USER-INTERFACE:     clisp
;;;;DESCRIPTION
;;;;
;;;;    This package exports disk management functions,
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-06-27 <PJB> Removed string constant +TAB+.
;;;;    2003-10-21 <PJB> Added du.
;;;;    2002-01-09 <PJB> Moved in df from tar-backup.
;;;;    2002-10-?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2010
;;;;
;;;;    This file is part of PJB Clisp Utilities.
;;;;
;;;;    This  program is  free software;  you can  redistribute  it and/or
;;;;    modify it  under the  terms of the  GNU General Public  License as
;;;;    published by the Free Software Foundation; either version 2 of the
;;;;    License, or (at your option) any later version.
;;;;
;;;;    This program  is distributed in the  hope that it  will be useful,
;;;;    but  WITHOUT ANY WARRANTY;  without even  the implied  warranty of
;;;;    MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a  copy of the GNU General Public License
;;;;    along with  this program; see the  file COPYING; if  not, write to
;;;;    the Free  Software Foundation, Inc.,  59 Temple Place,  Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(DEFINE-PACKAGE "COM.INFORMATIMAGO.CLISP.DISK"
  (:NICKNAMES "PJB-DISK")
  (:DOCUMENTATION
   "This package exports disk management functions.

    Copyright Pascal J. Bourguignon 2002 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:FROM "COMMON-LISP" :IMPORT :ALL)
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.STREAM")
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING"  :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" :IMPORT :ALL)
  (:EXPORT "VOLINFO" "VOLINFO-PATH" "VOLINFO-FS-TYPE"  "VOLINFO-SIZE"
           "VOLINFO-USED" "VOLINFO-AVAILABLE" "VOLINFO-MOUNT-POINT"
           "DF" "DU" )) ;;COM.INFORMATIMAGO.CLISP.DISK


(DEFUN OLD-DF ()
  "
RETURN:  A list of sublists each containing:
           (device size used available mount-point)
"
  (MAPCAR
   (LAMBDA (LINE) (SPLIT-STRING LINE " "))
   (com.informatimago.common-lisp.stream:STREAM-TO-STRING-LIST
    (EXT:RUN-SHELL-COMMAND
     "/bin/df -T|postodax|/bin/awk '/Available/{next;}{print $1,$2,$3,$4,$5,$7;}'"
     :OUTPUT :STREAM))))


(DEFSTRUCT VOLINFO
  (PATH        "" :TYPE STRING)
  (FS-TYPE     "" :TYPE STRING)
  (SIZE        0  :TYPE NUMBER)
  (USED        0  :TYPE NUMBER)
  (AVAILABLE   0  :TYPE NUMBER)
  (MOUNT-POINT "" :TYPE STRING)
  ) ;;VOLINFO


(DEFUN DF (&OPTIONAL PATH)
  "
RETURN:  A list of volinfo structures.
"
  (UNLESS PATH (SETQ PATH ""))
  (MAPCAR
   (LAMBDA (LINE)
     (LET ((DATA (SPLIT-STRING LINE " \\+")))
       (MAKE-VOLINFO :PATH (NTH 0 DATA)
                     :FS-TYPE (NTH 1 DATA)
                     :SIZE (* 1024 (READ-FROM-STRING (NTH 2 DATA)))
                     :USED (* 1024 (READ-FROM-STRING (NTH 3 DATA)))
                     :AVAILABLE (* 1024 (READ-FROM-STRING (NTH 4 DATA)))
                     :MOUNT-POINT (NTH 5 DATA))))
   (com.informatimago.common-lisp.stream:STREAM-TO-STRING-LIST
    (EXT:RUN-SHELL-COMMAND
     (FORMAT
         NIL "df -k -T ~A|postodax|/bin/awk '/Available/{next;}{print $1,$2,$3,$4,$5,$7;}'"
         PATH)
     :OUTPUT :STREAM))))


(DEFUN DU (&OPTIONAL (PATH (EXT:CD)))
  "
RETURN:  The Disk Usage of the given PATH (or the current directory).
         It's given as the underlying du command gives it, either in
         blocks of 1KB or of 512 B.
"
  (VALUES
   (READ-FROM-STRING
    (CAAR
     (MAPCAR (LAMBDA (LINE) (SPLIT-STRING LINE #.(string (CODE-CHAR 9))))
             (com.informatimago.common-lisp.stream:STREAM-TO-STRING-LIST
              (EXT:RUN-PROGRAM "du"
                :ARGUMENTS (LIST "-s" PATH) :OUTPUT :STREAM)))))))


;;;; THE END ;;;;
