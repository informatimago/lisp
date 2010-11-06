;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               .openmcl-init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             OpenMCL
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    The OpenMCL init file.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-12-18 <PJB> Adapted from .clisprc to .openmcl-init.
;;;;    2003-05-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2003
;;;;    mailto:pjb@informatimago.com
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

;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------

(SETQ *LOAD-VERBOSE* NIL)
(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :NAME ".common" :TYPE "lisp") (user-homedir-pathname)))


(in-package "COM.INFORMATIMAGO.PJB")
(export '(EDIT QUIT))


;; ---------------------------------------------------------------------
;; clocc defsystem is erroneous for clisp --
;; -----------------------------------------

;;; (format t "~&tr=~S~%" (logical-pathname-translations "DEFSYSTEM"))
;;; (format t "~&dir=~S~%" (directory "DEFSYSTEM:*.*"))
;;; (LOAD "DEFSYSTEM:DEFSYSTEM.LISP")
;;; (SETQ MK::*FILENAME-EXTENSIONS* '("lisp" . "fas"))
;;; (MK::DEFINE-LANGUAGE :LISP
;;;                      :COMPILER #'COMPILE-FILE
;;;                      :LOADER   #'LOAD
;;;                      :SOURCE-EXTENSION (CAR MK::*FILENAME-EXTENSIONS*)
;;;                      :BINARY-EXTENSION (CDR MK::*FILENAME-EXTENSIONS*))


;;; (UNLESS (FIND-PACKAGE "MK") (DEFPACKAGE "MK"))
;;; (DEFUN LOAD-MK ()
;;;   (LOAD "DEFSYSTEM:DEFSYSTEM.LISP")
;;;   (LET ((FE '("lisp" . "fas")))
;;;     (DEFVAR MK::*FILENAME-EXTENSIONS* FE)
;;;     (MK::DEFINE-LANGUAGE :LISP
;;;                          :COMPILER #'COMPILE-FILE
;;;                          :LOADER   #'LOAD
;;;                          :SOURCE-EXTENSION (CAR FE)
;;;                          :BINARY-EXTENSION (CDR FE)))
;;;   );;LOAD-MK



;;----------------------------------------------------------------------
;; Setting environment -- OpenMCL specific --
;; ------------------------------------------

;; We'll use clocc xlib\clx\clue
;; (WHEN (FIND-PACKAGE "XLIB") (DELETE-PACKAGE (FIND-PACKAGE "XLIB")))

(def-lp-trans "CCL"      "/usr/local/lib/" "ccl/")
(def-lp-trans "COCOA"    "/usr/local/lib/" "ccl/examples/")
(def-lp-trans "EXAMPLES" "/usr/local/lib/" "ccl/examples/")

(setf COMMON-LISP-USER::*default-bundle-path* "CCL:OPENMCL.APP;")
;;  ccl::*module-search-path*  ;; paths used by REQUIRE.


#|
From: John DeSoi <jd@icx.net>
Note: Do not put any semicolon comments before this line.
The next forms define a semicolon reader for both #\linefeed
and #\return line endings so that OpenMCL can read MCL input
files (Mac line endings). Should also work with with MCL for unix files.
We could try (setf CCL::*LINEFEED-EQUALS-NEWLINE* t)
|#  
(defun semicolon-reader (stream char)
   (declare (ignore char))
   (do ((c #\null)) ((or (char= c #\linefeed) (char= c #\return)))
     (setf c (read-char stream nil #\newline t)))
   (values))
(set-macro-character #\; #'semicolon-reader)


;;----------------------------------------------------------------------
;; EDIT --
;; -------

;; editor-name is redefined in config.lisp to be:
;; (defun editor-name () (or (getenv "EDITOR") *editor*))

(DEFUN GET-FIRST-WORD (STRING)
  "
RETURN:     The first word of the string, or the empty string.
"
  (DO ((I 0)
       (J 0)
       (FOUND NIL)
       (DONE NIL))
      (DONE (IF FOUND (SUBSEQ STRING I  J) ""))
    (IF  (<= (LENGTH STRING) I)
      (SETQ DONE T FOUND NIL)
      (IF (<= J I)
        (IF (ALPHA-CHAR-P (CHAR STRING I))
          (SETQ J (1+ I))
          (INCF I))
        (IF (<= (LENGTH STRING) J)
          (SETQ DONE T FOUND T)
          (IF (ALPHA-CHAR-P (CHAR STRING J))
            (INCF J)
            (SETQ DONE T FOUND T))))))
  );;GET-FIRST-WORD


(defun edit (&optional (x nil x-p))
  (declare (ignore x x-p))
  (format *error-output* "~&Not implemented yet.~%"))
(defun quit ()                      (ccl:quit))

;;----------------------------------------------------------------------
;; (format *trace-output* "~&.openmcl-init.lisp loaded~%")
;;----------------------------------------------------------------------


(in-package "COMMON-LISP-USER")
(use-package "COM.INFORMATIMAGO.PJB")

;;;; openmcl-init.lisp                --                     --          ;;;;
