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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2012
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

;;----------------------------------------------------------------------
;; Setting environment -- COMMON-LISP part --
;; ------------------------------------------

(setq *load-verbose* nil)
(load (merge-pathnames
       (make-pathname :name ".common" :type "lisp") (user-homedir-pathname)))


(in-package "COM.INFORMATIMAGO.PJB")
(export '(edit quit))


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

(setf common-lisp-user::*default-bundle-path* "CCL:OPENMCL.APP;")
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

(defun get-first-word (string)
  "
RETURN:     The first word of the string, or the empty string.
"
  (do ((i 0)
       (j 0)
       (found nil)
       (done nil))
      (done (if found (subseq string i  j) ""))
    (if  (<= (length string) i)
      (setq done t found nil)
      (if (<= j i)
        (if (alpha-char-p (char string i))
          (setq j (1+ i))
          (incf i))
        (if (<= (length string) j)
          (setq done t found t)
          (if (alpha-char-p (char string j))
            (incf j)
            (setq done t found t))))))
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
