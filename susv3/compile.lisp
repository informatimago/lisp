;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               compile.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Replaces the Makefile.
;;;;    
;;;;    Usage:   (load "compile.lisp")
;;;;
;;;;    will compile all outdated files.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-07-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2004 - 2004
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

;; (defpackage "COM.INFORMATIMAGO.COMMON-LISP.COMPILE"
;;   (:use "COMMON-LISP")
;;   (:export "MAIN"))
;; (in-package "COM.INFORMATIMAGO.COMMON-LISP.COMPILE")


;; Not used yet.
(defvar PREFIX "/usr/local/")
(defvar MODULE "common-lisp")
(defvar PACKAGE-PATH "com/informatimago/common-lisp")

(defun logger (ctrl &rest args)
  (format *trace-output* "~&;;;;~%;;;; ~?~%;;;;~%" ctrl args))
(logger "*** COMPILING COM.INFORMATIMAGO.SYSV3 ***")

(load "init.lisp")
;; package.lisp is loaded by init.lisp.
(LOAD "PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF.LISP")
(push (function package:PACKAGE-SYSTEM-DEFINITION)
      ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*)
;;(asdf:operate 'asdf:load-op :com.informatimago.common-lisp)
(asdf:operate 'asdf:load-op :com.informatimago.clisp)


(defparameter *SOURCES*
  (append
   '(
     #+ffi tools
     )
   (when (find-package "LINUX")
     '(
       #+ffi dirent
       #+ffi ipc
       #+ffi process))))
  

(defparameter *source-type* "lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate the asdf system file, loading the sources.

(logger "GENERATING THE ASDF SYSTEM FILE")

(com.informatimago.common-lisp.make-depends:generate-asd
 :com.informatimago.susv3 *sources* *source-type*
 :VERSION "1.0.0"
 :predefined-packages '("COMMON-LISP" "FFI" "EXT" "LINUX" "REGEXP" "GRAY" "SYS")
 :depends-on '(:com.informatimago.common-lisp :com.informatimago.clisp)
 :vanillap t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now, we generate a summary.html page.
;;;

(logger "GENERATING THE SUMMARY.HTML")
(COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS:generate-summary
 *sources*
 :source-type  *source-type*
 :summary-path "summary.html"
 :repository-url (lambda (pp)
                   (format nil ;; "http://darcs.informatimago.com~
                               ;;  /darcs/public/lisp/~(~A/~A~).lisp"
                           "com/informatimago/~(~A/~A~).lisp"
                           (car (last (pathname-directory pp)))
                           (pathname-name pp)))
 #-clisp :comment-start #-clisp ";;;;")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finally, we compile and load the system
;;;

(logger "COMPILING THE ASDF SYSTEM")
(SETF ASDF:*COMPILE-FILE-WARNINGS-BEHAVIOUR* :IGNORE)
(let ((*LOAD-VERBOSE* t)
      (*COMPILE-VERBOSE* t)
      (asdf::*verbose-out* t))
  (asdf:operate 'asdf:load-op :com.informatimago.susv3))



;;;; compile.lisp                     --                     --          ;;;;
