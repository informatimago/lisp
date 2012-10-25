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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2012
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
;;;;****************************************************************************


(dolist
    (package
     '( com.informatimago.common-lisp.compatibility
        com.informatimago.common-lisp.dfa
        com.informatimago.common-lisp.dictionary
        ;; COM.INFORMATIMAGO.COMMON-LISP.GRAPH-DIAGRAM
        ;; COM.INFORMATIMAGO.COMMON-LISP.GRAPH-DOT
        com.informatimago.common-lisp.graph
        ;; collision with MAP: COM.INFORMATIMAGO.COMMON-LISP.HTML
        com.informatimago.common-lisp.list
        com.informatimago.common-lisp.make-depends
        com.informatimago.common-lisp.picture
        com.informatimago.common-lisp.string
        com.informatimago.common-lisp.tree-to-diagram
        com.informatimago.common-lisp.tree-to-ascii
        com.informatimago.common-lisp.utility
        ))
  (in-package "COMMON-LISP-USER")
  (com.informatimago.common-lisp.package:load-package package)
  (in-package "COMMON-LISP-USER")
;;  (USE-PACKAGE PACKAGE)
  )




;;;; pjb.lisp                         -- 2003-06-15 21:55:45 -- pascal   ;;;;
