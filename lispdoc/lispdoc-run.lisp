;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               lispdoc-run.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Generates the com.informatimago documentation.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-28 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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
;;;;**************************************************************************
;;;;    

(in-package :cl-user)

(defun doc () 
  (let ((*print-length* 10)
        (*print-level* 4))
    (mapc 'delete-file (directory #P"/home/pjb/public_html/sites/com.informatimago.www/develop/lisp/doc/*.html"))
    (com.informatimago.lispdoc:lispdoc-html
     "/home/pjb/public_html/sites/com.informatimago.www/develop/lisp/doc/"
     (remove-if-not (lambda (p)
                      (and (search "COM.INFORMATIMAGO" (package-name p))
                           (not (search "COM.INFORMATIMAGO.PJB" (package-name p)))))
                    (list-all-packages))
     :title  "Informatimago Common Lisp Packages Documentation")))


;;;; THE END ;;;;
