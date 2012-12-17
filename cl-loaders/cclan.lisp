;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               cclan.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loads CCLAN.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-06-05 <PJB> Created.
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

(load "CCLAN:ASDF;ASDF.LISP")
(load "CCLAN:CCLAN-GET;PACKAGE.LISP")
(load "CCLAN:CCLAN-GET;CCLAN-GET.ASD")
(asdf:operate 'asdf:load-op :cclan-get)  
(setf cclan-get::*cclan-tarball-directory* "SHARE-LISP:CCLAN;TARBALL;"
      cclan-get::*cclan-source-directory*  "SHARE-LISP:CCLAN;SOURCE;"
      cclan-get::*cclan-asdf-registry*     "SHARE-LISP:CCLAN;REGISTRY;")
(push cclan-get::*cclan-asdf-registry* asdf:*central-registry*)

(format t  "Push on ASDF:*CENTRAL-REGISTRY* directories where .asd files are ~
            to be found.~%")

;;;; cclan.lisp                       --                     --          ;;;;
