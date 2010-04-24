;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               portableaserve.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loader for portableaserve.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-05-30 <PJB> Created.
;;
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

(LOAD "package:portableaserve;logical-hostnames.lisp")
(LOAD "package:portableaserve;acl-compat;acl-compat-clisp.system")
(DEF-LP-TRANS "ACL-COMPAT"  +SHARE-LISP+ "portableaserve/acl-compat/")
(EXT:WITHOUT-PACKAGE-LOCK ()
  (MK:OOS "ACL-COMPAT" :LOAD :COMPILE-DURING-LOAD T))
(LOAD "aserve:aserve.system")
(MK:OOS "ASERVE" :LOAD :COMPILE-DURING-LOAD T)

(defpackage :aserve-example)
(defun aserve-example (&optional (port 2001))
  (load "aserve:example.cl")
  ;; This option enables extended debug message output
  (net.aserve::debug-on :info)
  ;; This option enables to enter the debugger if an error
  ;; occurs. (instead of simply logging and ignoring it)
  (net.aserve::debug-on :notrap)
  ;; Start example server on port 2001
  (aserve-example::start-simple-server :port port)
  );;aserve-example

;;;; portableaserve.lisp              -- 2003-05-31 04:02:00 -- pascal   ;;;;
