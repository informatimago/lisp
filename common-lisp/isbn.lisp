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
;;;;***************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ISBN"
  (:USE "COMMON-LISP")
  (:EXPORT "COMPUTE-ISBN-CHECK")
  (:DOCUMENTATION
   "
    Compute ISBN check digit.

    Copyright Pascal J. Bourguignon 2004 - 2004
   
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
    "))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ISBN")



(DEFUN COMPUTE-ISBN-CHECK (ISBN)
  (WHEN (OR (NOT (STRINGP ISBN))
            (/= 9 (LENGTH ISBN))
            (NOT (EVERY (FUNCTION DIGIT-CHAR-P) ISBN)))
    (ERROR "An ISBN must be a string of 9 digits."))
  (AREF "0123456789X"
        (- 11 (MOD (REDUCE (FUNCTION +)
                           (MAPCAR (FUNCTION *) 
                                   (MAP 'LIST 
                                        (LAMBDA (CH) (PARSE-INTEGER (STRING CH)))
                                        ISBN)
                                   '(10 9 8 7 6 5 4 3 2))) 
                   11))));;COMPUTE-ISBN-CHECK


(DEFUN TEST ()
  (ASSERT (CHAR= #\4 (COMPUTE-ISBN-CHECK "052135938"))))

;;;; isbn.lisp                        --                     --          ;;;;
