;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               iana-character-sets.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Converts the IANA character sets assignments to a lisp package.
;;;;    <a href=http://www.iana.org/assignments/character-sets>IANA character set</a>
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-31 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.IANA-CHARACTER-SETS"
  (:use "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "COMMON-LISP")
  (:export "CHARACTER-SETS-MAP" "READ-CHARACTER-SETS")
  (:documentation
   "Converts the IANA character set assignments to a lisp package.
    http://www.iana.org/assignments/character-sets"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.IANA-CHARACTER-SETS")




(defun second-word (line)
  (let* ((space  (position (character " ") line))
         (start  (position (character " ") line
                           :start space
                           :test  (complement (function eql))))
         (end    (position (character " ") line :start start)))
    (subseq line start end)))


(defun read-character-sets (path)
  (with-open-file (in path)
    (loop
       for line = (read-line in nil nil)
       with name = nil
       with aliases = '()
       with results = '()
       while line
       do ;; scan Name: and Alias: ; generate on empty line
         (cond
         ((prefixp "Name: "  line )  (setf name (second-word line)))
         ((prefixp "Alias: " line)
          (let ((alias (second-word line)))
            (unless (string-equal "None" alias) (push alias aliases))))
         ((zerop (length (string-trim " " line)))
          (when name (push  (cons name aliases) results))
          (setf name nil aliases '())))
       finally (return results))))


(defun character-sets-map (path)
  (let ((hash (make-hash-table :test (function equalp))))
    (dolist (cs (read-character-sets path))
      (dolist (name cs) (setf (gethash name hash) (first cs))))
    hash))
  


