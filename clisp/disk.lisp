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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2002 - 2012
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

(defpackage "COM.INFORMATIMAGO.CLISP.DISK"
  (:documentation
   "This package exports disk management functions.

    Copyright Pascal J. Bourguignon 2002 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export "VOLINFO" "VOLINFO-PATH" "VOLINFO-FS-TYPE"  "VOLINFO-SIZE"
           "VOLINFO-USED" "VOLINFO-AVAILABLE" "VOLINFO-MOUNT-POINT"
           "DF" "DU" ))
(in-package  "COM.INFORMATIMAGO.CLISP.DISK")


(defun old-df ()
  "
RETURN:  A list of sublists each containing:
           (device size used available mount-point)
"
  (mapcar
   (lambda (line) (split-string line " "))
   (stream-to-string-list
    (ext:run-shell-command
     "/bin/df -T|postodax|/bin/awk '/Available/{next;}{print $1,$2,$3,$4,$5,$7;}'"
     :output :stream))))


(defstruct volinfo
  (path        "" :type string)
  (fs-type     "" :type string)
  (size        0  :type number)
  (used        0  :type number)
  (available   0  :type number)
  (mount-point "" :type string)
  ) ;;VOLINFO


(defun df (&optional path)
  "
RETURN:  A list of volinfo structures.
"
  (unless path (setq path ""))
  (mapcar
   (lambda (line)
     (let ((data (split-string line " \\+")))
       (make-volinfo :path (nth 0 data)
                     :fs-type (nth 1 data)
                     :size (* 1024 (read-from-string (nth 2 data)))
                     :used (* 1024 (read-from-string (nth 3 data)))
                     :available (* 1024 (read-from-string (nth 4 data)))
                     :mount-point (nth 5 data))))
   (stream-to-string-list
    (ext:run-shell-command
     (format
      nil "df -k -T ~A|postodax|/bin/awk '/Available/{next;}{print $1,$2,$3,$4,$5,$7;}'"
      path)
     :output :stream))))


(defun du (&optional (path (ext:cd)))
  "
RETURN:  The Disk Usage of the given PATH (or the current directory).
         It's given as the underlying du command gives it, either in
         blocks of 1KB or of 512 B.
"
  (values
   (read-from-string
    (caar
     (mapcar (lambda (line) (split-string line #.(string (code-char 9))))
             (stream-to-string-list
              (ext:run-program "du"
                               :arguments (list "-s" path) :output :stream)))))))


;;;; THE END ;;;;
