;;;; -*- coding:utf-8 -*-

;; ----------------------------------------------------------------------
;; CLOCC -- Compiling the CLOCC --
;; -------------------------------

(DEFPARAMETER *CLOCC-ROOT*
  (namestring
   (translate-logical-pathname "PACKAGES:NET;SOURCEFORGE;CLOCC;CLOCC;")))
(LOAD "CLOCC:CLOCC")
(LOAD "CLOCC:SRC;DEFSYSTEM;DEFSYSTEM")
(LOAD "CLOCC:SRC;CLLIB;BASE")

(DEFUN COMPILE-CLOCC ()
  (LOAD (compile-file "CLOCC:CLOCC"))
  (LOAD (compile-file "CLOCC:SRC;DEFSYSTEM;DEFSYSTEM"))
  ;; * compile some systems
   (SETQ MK::*CENTRAL-REGISTRY*
        (NCONC
         ;; select the systems you want to use
         (MAPCAR (LAMBDA (SYSSUB)
                   (TRANSLATE-LOGICAL-PATHNAME
                    (CONCATENATE 'STRING "CLOCC:SRC;" SYSSUB)))
                 '("CLLIB;"
                   "EXT;QUEUES;"
                   "EXT;UNION-FIND;"
                   "GUI;CLX;"
                   "GUI;CLUE;"
                   "F2CL;PACKAGES;" "F2CL;"
                   "PORT;"
                   "PORT;CONFIGURATION;"
                   "PORT;ENVIRONMENT;"
                   "PORT;SHELL;"
                   "TOOLS;METERING;"
                   "TOOLS;CLUNIT;"
                   "SCREAMER;"
                   "SCREAMER;"
                   ))
         MK::*CENTRAL-REGISTRY*))
  (MK:OOS "cllib" :COMPILE)
  (MK:OOS "f2cl" :COMPILE));;COMPILE-CLOCC

;; ----------------------------------------------------------------------
;; -- CLOCC -- FORTRAN to LISP translator --
;; -----------------------------------------

(DEFVAR *EXT*
  #+CLISP   ".fas"
  #+CMUCL   ".x86f"
  #+SBCL    ".fasl"
  #+ALLEGRO ".fasl"
  #+VMS     ".fas"
  #+LUCID   ".sbin")
(DEFVAR *F2CL_DIR* "CLOCC:CLOCC;SRC;F2CL;SRC;")
(DEFUN LOAD-F2CL-MODULE (X) 
  (LOAD (CONCATENATE 'STRING *F2CL_DIR* X *EXT*) :PRINT NIL :VERBOSE NIL)) ;;LOAD-F2CL-MODULE
(DEFUN LOAD-F2CL ()
  (LOAD-F2CL-MODULE "f2cl0" )
  (LOAD-F2CL-MODULE "f2cl1" )
  (LOAD-F2CL-MODULE "f2cl2" )
  (LOAD-F2CL-MODULE "f2cl3" )
  (LOAD-F2CL-MODULE "f2cl4" )
  (LOAD-F2CL-MODULE "f2cl5" )
  (LOAD-F2CL-MODULE "f2cl6" )
  (LOAD-F2CL-MODULE "f2cl7" )
  (LOAD-F2CL-MODULE "macros" )
  (FORMAT T "~&The f2cl software has been loaded.~%"));;LOAD-F2CL

(format t  "Push on MK::*CENTRAL-REGISTRY* directories where .system files are ~
            to be found.~%")

(push "clocc:src;port;" MK::*CENTRAL-REGISTRY*)

;;;; clocc.lisp                       --                     --          ;;;;
