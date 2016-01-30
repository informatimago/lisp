;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Save the botihn executable.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-04-27 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(in-package "COMMON-LISP-USER")  
(progn (format t "~%;;; Loading quicklisp.~%") (finish-output) (values))
(load #P"~/quicklisp/setup.lisp")

(progn (format t "~%;;; Loading botihn.~%") (finish-output) (values))
(push (make-pathname :name nil :type nil :version nil
                     :defaults *load-truename*) asdf:*central-registry*)

(ql:quickload :com.informatimago.small-cl-pgms.botihn)

#+ccl (pushnew 'com.informatimago.common-lisp.interactive.interactive:initialize
               ccl:*restore-lisp-functions*)

(progn (format t "~%;;; Saving hotihn.~%") (finish-output) (values))
;; This doesn't return.
#+ccl (ccl::save-application
       "botihn"
       :mode #o755 :prepend-kernel t
       :toplevel-function (function com.informatimago.small-cl-pgms.botihn:main)
       :init-file  nil ;; "~/.botihn.lisp"
       :error-handler :quit)
