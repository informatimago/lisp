;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               tools.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Utilities for implementing SUSV3 API.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-04-04 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2016
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

(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages)
         (also-use-packages "FFI"))
(defpackage "COM.INFORMATIMAGO.SUSV3.TOOLS"
  (:documentation 
   "Utilities for implementing SUSV3 API.")
  (:use "COMMON-LISP")
  (:export 
   "DEFINE-FFI-COPIERS"))
(in-package  "COM.INFORMATIMAGO.SUSV3.TOOLS")

(defmacro define-ffi-copiers (&rest arguments)
  "
ARGUMENTS:
   (define-ffi-copiers (lisp-structure ffi-struct to-name from-name) slot...)
   (define-ffi-copiers lisp-structure  ffi-struct slot...)
SLOT:
   (lisp-slot ffi-slot)                                 ; for simple types.
   ((lisp-slot lisp-structure) (ffi-slot ffi-structure) ; for structure types.
DO:     Generates two macros, named TO-NAME and FROM-NAME,
        or {LIST-STRUCTURE}->{FFI-STRUCT} and  {FFI-STRUCT}->{LIST-STRUCTURE}
        to copy between LISP and FFI structures.
        For slots that are simple, SETF is used.
        For slots that are structures, the names {LIST-STRUCTURE}->{FFI-STRUCT} 
        and {FFI-STRUCT}->{LIST-STRUCTURE} built from the types given with
        the slot are used.
NOTE:   It's advised to keep the l->f and f->l format for TO-NAME and FROM-NAME,
        to be able to use l and f as slot type...
"
  (let (lisp-structure ffi-struct slots to-name from-name)
    (if (listp (car arguments))
        (setf lisp-structure (first  (car arguments))
              ffi-struct     (second (car arguments))
              to-name        (third  (car arguments))
              from-name      (fourth (car arguments))
              slots          (cdr arguments))
        (progn
          (setf lisp-structure (car  arguments)
                ffi-struct     (cadr arguments)
                slots          (cddr arguments))
          (setf to-name   (intern (with-standard-io-syntax
                                    (format nil "~A->~A" 
                                            lisp-structure ffi-struct))
                                  (symbol-package lisp-structure))
                from-name (intern (with-standard-io-syntax
                                    (format nil "~A->~A" 
                                            ffi-struct lisp-structure))
                                  (symbol-package lisp-structure)))))
    (let ((complex-slots (remove-if (lambda (slot) (atom (first slot))) slots))
          (simple-slots  (remove-if (lambda (slot) (not (atom (first slot)))) 
                                    slots)))
      `(progn 
         (defmacro ,to-name (src dst)
           `(progn
              ,,@(mapcar
                  (lambda (slot)
                    (list
                     'list
                     (list 'quote (intern (with-standard-io-syntax 
                                            (format nil "~A->~A" 
                                                    (second (first  slot))
                                                    (second (second slot))))
                                          (symbol-package (second (first slot)))))
                     (list 'list (list 'quote (first (first slot))) 'src)
                     (list 'list ''ffi:slot 'dst 
                           (list 'quote (list 'quote (first (second slot)))))))
                  complex-slots)
              (setf ,,@(mapcan
                        (lambda (slot) 
                          (list
                           (list 'list ''ffi:slot 'dst 
                                 (list 'quote (list 'quote (second slot))))
                           (list 'list (list 'quote (first slot)) 'src))) 
                        simple-slots))
              ,dst))
         (defmacro ,from-name (src dst)
           `(progn
              ,,@(mapcar
                  (lambda (slot)
                    (list
                     'list
                     (list 'quote  (intern (with-standard-io-syntax 
                                             (format nil "~A->~A" 
                                                     (second (second slot))
                                                     (second (first  slot))))
                                           (symbol-package (second (first slot)))))
                     (list 'list ''ffi:slot 'src
                           (list 'quote (list 'quote (first (second slot)))))
                     (list 'list (list 'quote (first (first slot))) 'dst)))
                  complex-slots)
              (setf ,,@(mapcan 
                        (lambda (slot)
                          (list
                           (list 'list (list 'quote (first slot)) 'dst)
                           (list 'list ''ffi:slot 'src
                                 (list 'quote (list 'quote (second slot)))))) 
                        simple-slots))
              ,dst))))))

;;;; THE END ;;;;
