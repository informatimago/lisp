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

(if (equal (lisp-implementation-type) "Emacs Common Lisp")
    (progn
      (LOAD "/usr/local/share/lisp/packages/edu/mit/ai/pseudo/loadit.lisp")
      (LOAD-PSEUDOSCHEME  "/usr/local/share/lisp/packages/edu/mit/ai/pseudo/"))
    (progn
      (LOAD "PACKAGES:EDU;MIT;AI;PSEUDO;LOADIT")
      (LOAD-PSEUDOSCHEME "PACKAGES:EDU;MIT;AI;PSEUDO;")))
(DEFMACRO SCHEME () `(PS:SCHEME))
(format t "~2%Use: (scheme)~2%")
;;;; pseudo.lisp                      --                     --          ;;;;
