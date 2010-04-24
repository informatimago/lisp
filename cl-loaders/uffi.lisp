;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               uffi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    UFFI loader.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-06-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2003
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

(UNLESS (FIND-PACKAGE "ASDF")
  (LOAD "LOADER:CCLAN.LISP"))

(PUSH (DIRECTORY-NAMESTRING (TRANSLATE-LOGICAL-PATHNAME "UFFI:UFFI.ASD"))
      ASDF:*CENTRAL-REGISTRY*)
(ASDF:OPERATE 'ASDF:LOAD-OP :UFFI)  


;;;; uffi.lisp                        -- 2003-06-05 21:06:08 -- pascal   ;;;;
