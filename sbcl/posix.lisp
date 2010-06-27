;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               posix.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             SBCL
;;;;USER-INTERFACE:     NONE
;;;;NOWEB:              T
;;;;DESCRIPTION
;;;;    
;;;;    This packages exports POSIX functions.
;;;;    This is the SBCL specific implementation of the POSIX API.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2003-05-13 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2003
;;;;    mailto:pjb@informatimago.com
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
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "SB-EXT"))
(defpackage "COM.INFORMATIMAGO.SBCL.POSIX"
  #+sbcl (:NICKNAMES "POSIX")
  (:DOCUMENTATION "This packages exports POSIX functions.
    This is the CLISP specific implementation of the POSIX API.")
  (:USE "COMMON-LISP")
  (:EXPORT "GETENV"))


(DEFUN GETENV (NAME)
  "
URL:        http://www.opengroup.org/onlinepubs/007904975/functions/getenv.html
RETURN:     NIL or the value of the environment variable named NAME.
"
  (DECLARE (STRING NAME))
  #+sbcl (SB-EXT:POSIX-GETENV NAME)
  #-sbcl (error "~S not implemented yet." 'getenv))

;;;; THE END ;;;;
