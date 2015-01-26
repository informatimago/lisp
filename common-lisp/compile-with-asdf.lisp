;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               compile-with-asdf.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Compile the com.informatimago.common-lisp libraries with ASDF.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-01 <PJB> Created.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling com.informatimago.common-lisp
;;;

(setf asdf:*central-registry*
      (append (remove-duplicates
               (mapcar (lambda (path)
                         (make-pathname :name nil :type nil :version nil :defaults path))
                       (directory "**/*.asd"))
               :test (function equalp))
              asdf:*central-registry*))

(asdf-load  :com.informatimago.common-lisp)


;;;; THE END ;;;;



