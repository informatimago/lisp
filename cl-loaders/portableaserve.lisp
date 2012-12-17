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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************

(load "package:portableaserve;logical-hostnames.lisp")
(load "package:portableaserve;acl-compat;acl-compat-clisp.system")
(def-lp-trans "ACL-COMPAT"  +share-lisp+ "portableaserve/acl-compat/")
(ext:without-package-lock ()
  (mk:oos "ACL-COMPAT" :load :compile-during-load t))
(load "aserve:aserve.system")
(mk:oos "ASERVE" :load :compile-during-load t)

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
