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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
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
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")
(load (merge-pathnames #P"ibcl.lisp" *load-pathname*))
(in-package "COMMON-LISP")
(rename-package (find-package "COMMON-LISP-USER") "OLD-CL-USER")
(defpackage "COMMON-LISP-USER"
  (:nicknames "CL-USER")
  (:use "IBCL"))
(in-package "IMAGE-BASED-COMMON-LISP-USER")


#+clisp (ext:saveinitmem          "ibcl" :executable t)
#+sbcl  (sb-ext:SAVE-LISP-AND-DIE
         "ibcl" :executable t
         :toplevel (lambda ()
                     (setf *package* (find-package "COMMON-LISP-USER"))
                     (delete-package (find-package "OLD-CL-USER"))
                     (SB-POSIX:PUTENV
                      (format nil "SBCL_HOME=~A"
                              (namestring (user-homedir-pathname))))
                     (SB-IMPL::TOPLEVEL-REPL nil)
                     (sb-ext:quit 0)))
#-(or clisp sbcl) (error "How do we save an image in ~A" 
                         (lisp-implementation-type))


#+clisp (ext:quit)
#+sbcl  (sb-ext:quit)
#-(or clisp sbcl) (error "How do we quit from ~A" (lisp-implementation-type))
