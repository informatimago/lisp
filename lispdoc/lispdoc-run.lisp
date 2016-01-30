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
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LISPDOC.RUN")

(defun doc (&key
              (target-class 'html-documentation)
              (doc-directory (merge-pathnames #P"public_html/sites/com.informatimago.www/develop/lisp/doc/"
                                              (user-homedir-pathname)))) 
  (let ((*print-length* 10)
        (*print-level* 4)
        (*print-right-margin* 80))
    (mapc 'delete-file (directory (merge-pathnames #P"*.html" doc-directory)))
    (com.informatimago.lispdoc.generate:generate-lispdoc
     target-class
     doc-directory
     (remove-if-not (lambda (p)
                      (and (search "COM.INFORMATIMAGO" (package-name p))
                           (not (search "COM.INFORMATIMAGO.PJB" (package-name p)))))
                    (list-all-packages))
     :title "Informatimago CL Software" ; "Informatimago Common Lisp Packages Documentation"
     :copyright "Copyright Pascal J. Bourguignon 2012 - 2015"
     :author "Pascal J. Bourguignon"
     :email "pjb@informatimago.com"
     :keywords "Informatimago, Common Lisp, Lisp, Library")))


;;;; THE END ;;;;
