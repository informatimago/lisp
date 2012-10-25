;;;;**************************************************************************
;;;;FILE:               ibcl-bootstrap.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This script generates an executable saved image environment using 
;;;;    IMAGE-BASED-COMMON-LISP instead of COMMON-LISP.
;;;;    
;;;;    IBCL Bootstrap
;;;;   
;;;;    To create the executable image:
;;;;   
;;;;      clisp -ansi -norc                -i     ibcl-bootstrap.lisp -p "IBCL"
;;;;      sbcl        --userinit /dev/null --load ibcl-bootstrap.lisp
;;;;   
;;;;    Then, to launch it, use: 
;;;;   
;;;;      ./ibcl
;;;;   
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-07-01 <PJB> Added support to SBCL.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2012
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
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")
(load (merge-pathnames #p"ibcl.lisp" *load-pathname*))
(in-package "COMMON-LISP")
(rename-package (find-package "COMMON-LISP-USER") "OLD-CL-USER")
(defpackage "COMMON-LISP-USER"
  (:nicknames "CL-USER")
  (:use "IBCL"))
(in-package "IMAGE-BASED-COMMON-LISP-USER")


#+clisp (ext:saveinitmem          "ibcl" :executable t)
#+sbcl  (sb-ext:save-lisp-and-die
         "ibcl" :executable t
         :toplevel (lambda ()
                     (setf *package* (find-package "COMMON-LISP-USER"))
                     (delete-package (find-package "OLD-CL-USER"))
                     (sb-posix:putenv
                      (format nil "SBCL_HOME=~A"
                              (namestring (user-homedir-pathname))))
                     (sb-impl::toplevel-repl nil)
                     (sb-ext:quit 0)))
#-(or clisp sbcl) (error "How do we save an image in ~A" 
                         (lisp-implementation-type))


#+clisp (ext:quit)
#+sbcl  (sb-ext:quit)
#-(or clisp sbcl) (error "How do we quit from ~A" (lisp-implementation-type))
