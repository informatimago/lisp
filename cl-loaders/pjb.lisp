;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loader for my packages.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-06-15 <PJB> Created.
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


(DOLIST
    (PACKAGE
     '( COM.INFORMATIMAGO.COMMON-LISP.COMPATIBILITY
        COM.INFORMATIMAGO.COMMON-LISP.DFA
        COM.INFORMATIMAGO.COMMON-LISP.DICTIONARY
        ;; COM.INFORMATIMAGO.COMMON-LISP.GRAPH-DIAGRAM
        ;; COM.INFORMATIMAGO.COMMON-LISP.GRAPH-DOT
        COM.INFORMATIMAGO.COMMON-LISP.GRAPH
        ;; collision with MAP: COM.INFORMATIMAGO.COMMON-LISP.HTML
        COM.INFORMATIMAGO.COMMON-LISP.LIST
        COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS
        COM.INFORMATIMAGO.COMMON-LISP.PICTURE
        COM.INFORMATIMAGO.COMMON-LISP.STRING
        COM.INFORMATIMAGO.COMMON-LISP.TREE-TO-DIAGRAM
        COM.INFORMATIMAGO.COMMON-LISP.TREE-TO-ASCII
        COM.INFORMATIMAGO.COMMON-LISP.UTILITY
        ))
  (IN-PACKAGE "COMMON-LISP-USER")
  (COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:LOAD-PACKAGE PACKAGE)
  (IN-PACKAGE "COMMON-LISP-USER")
;;  (USE-PACKAGE PACKAGE)
  )




;;;; pjb.lisp                         -- 2003-06-15 21:55:45 -- pascal   ;;;;
