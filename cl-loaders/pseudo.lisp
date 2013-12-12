;;;; -*- coding:utf-8 -*-

;; LOADER:PSEUDO.LISP 
;; ----------------------------------------------------------------------
;; -- PSEUDO -- Pseudo-Scheme --
;; -----------------------------

(in-package :common-lisp-user)
(defvar *pseudoscheme-directory* *default-pathname-defaults*)
(defvar *use-scheme-read*  nil)
(defvar *use-scheme-write* t)
(pushnew :unix *features*)

(progn
  (load (com.informatimago.tools.pathname:translate-logical-pathname #P"PACKAGES:EDU;MIT;AI;PSEUDO;LOADIT"))
  (load-pseudoscheme (com.informatimago.tools.pathname:translate-logical-pathname #P"PACKAGES:EDU;MIT;AI;PSEUDO;")))

(defun scheme () (ps:scheme))
(format t "~2%Use: (scheme)~2%")

;;;; THE END ;;;;
