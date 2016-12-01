;;;; -*- coding:utf-8 -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

;; ----------------------------------------------------------------------
;; CLOCC -- Compiling the CLOCC --
;; -------------------------------

(defparameter *clocc-root*
  (namestring
   (translate-logical-pathname "PACKAGES:NET;SOURCEFORGE;CLOCC;CLOCC;")))
(load "CLOCC:CLOCC")
(load "CLOCC:SRC;DEFSYSTEM;DEFSYSTEM")
(load "CLOCC:SRC;CLLIB;BASE")

(defun compile-clocc ()
  (load (compile-file "CLOCC:CLOCC"))
  (load (compile-file "CLOCC:SRC;DEFSYSTEM;DEFSYSTEM"))
  ;; * compile some systems
   (setq mk::*central-registry*
        (nconc
         ;; select the systems you want to use
         (mapcar (lambda (syssub)
                   (translate-logical-pathname
                    (concatenate 'string "CLOCC:SRC;" syssub)))
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
         mk::*central-registry*))
  (mk:oos "cllib" :compile)
  (mk:oos "f2cl" :compile));;COMPILE-CLOCC

;; ----------------------------------------------------------------------
;; -- CLOCC -- FORTRAN to LISP translator --
;; -----------------------------------------

(defvar *ext*
  #+clisp   ".fas"
  #+cmucl   ".x86f"
  #+sbcl    ".fasl"
  #+allegro ".fasl"
  #+vms     ".fas"
  #+lucid   ".sbin")
(defvar *f2cl_dir* "CLOCC:CLOCC;SRC;F2CL;SRC;")
(defun load-f2cl-module (x)
  (load (concatenate 'string *f2cl_dir* x *ext*) :print nil :verbose nil)) ;;LOAD-F2CL-MODULE
(defun load-f2cl ()
  (load-f2cl-module "f2cl0" )
  (load-f2cl-module "f2cl1" )
  (load-f2cl-module "f2cl2" )
  (load-f2cl-module "f2cl3" )
  (load-f2cl-module "f2cl4" )
  (load-f2cl-module "f2cl5" )
  (load-f2cl-module "f2cl6" )
  (load-f2cl-module "f2cl7" )
  (load-f2cl-module "macros" )
  (format t "~&The f2cl software has been loaded.~%"));;LOAD-F2CL

(format t  "Push on MK::*CENTRAL-REGISTRY* directories where .system files are ~
            to be found.~%")

(push "clocc:src;port;" mk::*central-registry*)

;;;; clocc.lisp                       --                     --          ;;;;
