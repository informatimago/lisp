;;;; -*- coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:               isbn.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Compute ISBN check digit.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-28 <PJB> Created.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;***************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.ISBN"
  (:use "COMMON-LISP")
  (:export "COMPUTE-ISBN-CHECK")
  (:documentation
   "
    Compute ISBN check digit.

    Copyright Pascal J. Bourguignon 2004 - 2004
   
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
    "))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.ISBN")



(defun compute-isbn-check (isbn)
  (when (or (not (stringp isbn))
            (/= 9 (length isbn))
            (not (every (function digit-char-p) isbn)))
    (error "An ISBN must be a string of 9 digits."))
  (aref "0123456789X"
        (- 11 (mod (reduce (function +)
                           (mapcar (function *) 
                                   (map 'list 
                                        (lambda (ch) (parse-integer (string ch)))
                                        isbn)
                                   '(10 9 8 7 6 5 4 3 2))) 
                   11))));;COMPUTE-ISBN-CHECK


(defun test ()
  (assert (char= #\4 (compute-isbn-check "052135938"))))

;;;; isbn.lisp                        --                     --          ;;;;
