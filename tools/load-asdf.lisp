;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               load-asdf.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file requires or loads ASDF, and ASDF-BINARY-LOCATIONS.
;;;;    (With ASDF2, it only configures ASDF with binary locations,
;;;;    without the use of an external system ASDF-BINARY-LOCATIONS).
;;;;
;;;;    Parameters:
;;;;
;;;;      cl-user::*asdf-source*
;;;;
;;;;           Should be bound to the path of the asdf.lisp source file.
;;;;
;;;;
;;;;      cl-user::*asdf-binary-locations-directory*
;;;;
;;;;           Should be bound to the path of the directory of the
;;;;           asdf-binary-location system (where the
;;;;           asdf-binary-location.asd file lies).
;;;;
;;;;           This is not used if ASDF2 is available.
;;;;
;;;;    Define these two variables, and load this file.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-06 <PJB> Extracted from compile-with-asdf.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2015
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
;;;;**************************************************************************

(in-package :cl-user)

(defvar *asdf-source*
  #p"/data/lisp/packages/net/common-lisp/projects/asdf/asdf/asdf.lisp")

(defvar *asdf-binary-locations-directory*
  #p"/data/lisp/packages/net/common-lisp/projects/asdf-binary-locations/asdf-binary-locations/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ASDF
;;;

(unless (find-package :asdf)
  (handler-case (require :asdf)
    (error ()   (load (compile-file *asdf-source*)))))

(defun push-asdf-repository (path)
  (pushnew path asdf:*central-registry* :test #'equal))

(defun asdf-load (&rest systems)
  (mapcar (lambda (system) (asdf:operate 'asdf:load-op system))
          systems))

(defun asdf-delete-system (&rest systems)
  (mapc (lambda (system) (remhash (string-downcase system) asdf::*defined-systems*))
        systems)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ASDF-BINARY-LOCATIONS
;;;

(defun hostname ()
  (let ((outpath (format nil "/tmp/hostname-~8,'0X.txt" (random #x100000000))))
    (asdf:run-shell-command
     "( hostname --fqdn 2>/dev/null || hostname --long 2>/dev/null || hostname ) > ~A"
     outpath)
    (prog1 (with-open-file (hostname outpath)
             (read-line hostname))
      (delete-file outpath))))

(let ((sym (find-symbol "ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY" "ASDF")))
  (when (and sym (fboundp sym))
    (push :has-asdf-enable-asdf-binary-locations-compatibility *features*)))

#+has-asdf-enable-asdf-binary-locations-compatibility
(progn
  (format *trace-output* "enable-asdf-binary-locations-compatibility ~%")
  (asdf:enable-asdf-binary-locations-compatibility
   :centralize-lisp-binaries     t
   :default-toplevel-directory   (merge-pathnames (format nil ".cache/common-lisp/~A/" (hostname))
                                                  (user-homedir-pathname) nil)
   :include-per-user-information nil
   :map-all-source-files t
   :source-to-target-mappings    nil))

#-has-asdf-enable-asdf-binary-locations-compatibility
(progn
 (push-asdf-repository *asdf-binary-locations-directory*)
 (asdf-load :asdf-binary-locations))

#-has-asdf-enable-asdf-binary-locations-compatibility
(progn
  (format *trace-output* "enable-asdf-binary-locations-compatibility ~%")
  (setf asdf:*centralize-lisp-binaries*     t
        asdf:*include-per-user-information* nil
        asdf:*default-toplevel-directory*
        (merge-pathnames (format nil ".cache/common-lisp/~A/" (hostname))
                         (user-homedir-pathname) nil)
        asdf:*source-to-target-mappings* '()))


;;;; THE END ;;;;

