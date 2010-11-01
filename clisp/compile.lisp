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
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-13 <PJB> Added generation of ASD file and use of ASDF.
;;;;    2004-07-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
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


;;; Not used yet
(defvar PREFIX "/usr/local/")
(defvar MODULE "common-lisp")
(defvar PACKAGE-PATH "com/informatimago/common-lisp")
;;; ----

(defun logger (ctrl &rest args)
  (format *trace-output* "~&;;;;~%;;;; ~?~%;;;;~%" ctrl args))
(logger "*** COMPILING COM.INFORMATIMAGO.CLISP ***")


(load "init.lisp")
;; package.lisp is loaded by init.lisp.
#+(or allegro ccl ecl) (load (compile-file #P"PACKAGES:net;sourceforge;cclan;asdf;asdf.lisp"))
#-(or allegro ccl ecl) (load (compile-file #P"PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF.LISP"))
(push (function package:PACKAGE-SYSTEM-DEFINITION)
      ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*)
(asdf:operate 'asdf:load-op :com.informatimago.common-lisp)

(defparameter *SOURCES*
  (append '(
            syslog
            ;;  #+FFI uffi
            ;;uffi-test
            disk
            objc
            string
            fifo-stream

            iotask
            rfc1413
            )
          (when (find-package "LINUX")
            '(
              raw-memory
              susv3
              susv3-mc3
              susv3-xsi
              script                    ; use linux
              shell                     ; use linux
              xterm                     ; use susv3
              make-volumes              ; use susv3
              ))))


(defparameter *source-type* "lisp")

(com.informatimago.common-lisp.make-depends:generate-asd
 :com.informatimago.clisp *sources* *source-type*
 :VERSION "1.0.0"
 :predefined-packages '("COMMON-LISP" "FFI" "EXT" "LINUX" "REGEXP" "SOCKET"
                        "GRAY" "SYS" "CLOS")
 :depends-on '(:com.informatimago.common-lisp)
 :vanillap t)


(COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS:generate-summary
 *sources*
 :source-type  *source-type*
 :summary-path "summary.html"
 :repository-url (lambda (pp)
                   (format nil
                           ;; "http://darcs.informatimago.com~
                           ;;  /darcs/public/lisp/~(~A/~A~).lisp"
                           ;; "com/informatimago/~(~A/~A~).lisp"
                           "~(~*~A~).lisp"
                           (car (last (pathname-directory pp)))
                           (pathname-name pp)))
 #-clisp :comment-start #-clisp ";;;;")


(asdf:operate 'asdf:load-op :com.informatimago.clisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
