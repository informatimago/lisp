;;;; -*- coding:utf-8 -*-


(in-package "COMMON-LISP-USER")

#+sbcl (progn
         (load "db-sockets")
         (pushnew :db-sockets *features*) )


#+clisp (defparameter saved-wofpc  custom:*warn-on-floating-point-contagion*)
#+clisp (setq custom:*warn-on-floating-point-contagion* nil)



(let ((home (or #+sbcl  (sb-ext:posix-getenv "HOME")
                #+clisp (ext:getenv "HOME")
                "/home/pascal")))
  (setf (logical-pathname-translations "HOME")
        (list
         (list "**;*.*"  (concatenate 'string home "/**/*.*"))
         (list ";**;*.*" (concatenate 'string home "/**/*.*")))))


(pushnew :clx-debugging *features*)

(defparameter *clx-sources*
  '(
    ;; First load port:
    "clocc:clocc;src;port;ext"
    "clocc:clocc;src;port;gray"
    "clocc:clocc;src;port;path"
    "clocc:clocc;src;port;sys"
    "clocc:clocc;src;port;net"
    "clocc:clocc;src;port;proc"
    ;; Then split-sequence
    "clocc:clocc;src;defsystem-4;src;utilities;split-sequence"
    ;; Then load the true system:
    "clocc:clocc;src;gui;clx;package"
    "clocc:clocc;src;gui;clx;depdefs"
    "clocc:clocc;src;gui;clx;clx"
    "clocc:clocc;src;gui;clx;dependent"
    "clocc:clocc;src;gui;clx;macros"          ; these are just macros
    "clocc:clocc;src;gui;clx;bufmac"          ; these are just macros
    "clocc:clocc;src;gui;clx;buffer"
    "clocc:clocc;src;gui;clx;display"
    "clocc:clocc;src;gui;clx;gcontext"
    "clocc:clocc;src;gui;clx;input"
    "clocc:clocc;src;gui;clx;requests"
    "clocc:clocc;src;gui;clx;fonts"
    "clocc:clocc;src;gui;clx;graphics"
    "clocc:clocc;src;gui;clx;text"
    "clocc:clocc;src;gui;clx;attributes"
    "clocc:clocc;src;gui;clx;translate"
    "clocc:clocc;src;gui;clx;keysyms"
    "clocc:clocc;src;gui;clx;manager"
    "clocc:clocc;src;gui;clx;image"
    "clocc:clocc;src;gui;clx;resource"
    "clocc:clocc;src;gui;clx;shape"
    ;; Then hello world!
    "clocc:clocc;src;gui;clx;demo;hello"
    ))


(dolist (file *clx-sources*) (load file))

#+clisp (setq custom:*warn-on-floating-point-contagion*  saved-wofpc)


(defun compile-clx ()
  (dolist (file *clx-sources*)
    (format t "Compiling ~A~%" file)
    (load (compile-file file))))


(defun test-clx ()
  (xlib::hello-world ""))


(load "PACKAGE:CLX-DEMOS;QIX.LISP")
(load "PACKAGE:CLX-DEMOS;SOKOBAN.LISP")
;; note: sokoban only works with clisp clx for it needs xpm extension.



;; With common-lisp-controller: (please note that the patches included in
;; the clocc-port subdirectory have not yet been send upstream, so the
;; cvs and cclan version won't do)

;; Put the source in for example ~/common-lisp/src/clx and add the
;; following to your startup script ( ~/.sbclrc or ~/.cmucl-init.lisp)

;; |;;; -*- Mode: Lisp; Package: USER; Base: 10; Syntax: Common-Lisp -*-
;; |
;; |(load "/etc/sbclrc")
;; |
;; |(format t "Hello Peter!~%")
;; |  
;; |(common-lisp-controller:add-project-directory
;; | #p"/home/pvaneynd/common-lisp/src/"
;; | #p"/home/pvaneynd/common-lisp/fasl-sbcl/"
;; | '("CLX")
;; | "/home/pvaneynd/common-lisp/systems/")

;; then you can do: 

;; * (require :db-sockets)
;; * (pushnew :db-sockets *features*)
;; * (require :clocc-port)
;; * (mk:oos :clx :compile)
;; * (mk:oos :clx :load)


;;;; clx.lisp                         -- 2003-05-13 21:58:49 -- pascal   ;;;;
