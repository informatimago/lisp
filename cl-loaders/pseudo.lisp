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
      (load "/usr/local/share/lisp/packages/edu/mit/ai/pseudo/loadit.lisp")
      (load-pseudoscheme  "/usr/local/share/lisp/packages/edu/mit/ai/pseudo/"))
    (progn
      (load "PACKAGES:EDU;MIT;AI;PSEUDO;LOADIT")
      (load-pseudoscheme "PACKAGES:EDU;MIT;AI;PSEUDO;")))
(defmacro scheme () `(ps:scheme))
(format t "~2%Use: (scheme)~2%")
;;;; pseudo.lisp                      --                     --          ;;;;
