;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               asdf-tools.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    ASDF tools.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-06 <PJB> Extracted from rc/common.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2013 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.TOOLS.ASDF"
  (:use "COMMON-LISP"
        "ASDF"
        "QUICKLISP")
  (:shadowing-import-from
   "COM.INFORMATIMAGO.TOOLS.PATHNAME"
   "USER-HOMEDIR-PATHNAME" "MAKE-PATHNAME" "TRANSLATE-LOGICAL-PATHNAME")
  (:export "ASDF-LOAD"
           "ASDF-LOAD-SOURCE"
           "ASDF-INSTALL"
           "ASDF-DELETE-SYSTEM"
           "FIND-ASDF-SUBDIRECTORIES"
           "UPDATE-ASDF-REGISTRY")
  (:documentation "ASDF tools -- somewhat deprecated by quicklisp."))
(in-package "COM.INFORMATIMAGO.TOOLS.ASDF")


(defun asdf-load (&rest systems)
  "Load the ASDF systems.  See also (QL:QUICKLOAD system) to install them."
  (dolist (system systems systems)
    #+quicklisp (ql:quickload system)
    #-quicklisp (asdf:operate 'asdf:load-op system)))


(defun asdf-load-source (&rest systems)
  "Load the sources of the ASDF systems.  See also (QL:QUICKLOAD system) to install them."
  (dolist (system systems systems)
    (asdf:operate 'asdf:load-source-op system)))

(defun asdf-install (&rest systems)
  "Download and install a system.  Now, uses quicklisp."
  (dolist (system systems systems)
    (declare (ignorable system))
    #+quicklisp (ql:quickload system)
    #-quicklisp (error "Please install and use quicklisp!")))

(defun asdf-delete-system (system)
  "Clear the system from ASDF, to force reloading them on next ASDF-LOAD."
  ;;(remhash (string-downcase system) asdf::*defined-systems*)
  (asdf:clear-system system)
  (values))


;;;------------------------------------------------------------------------
;;;


(defparameter *asdf-interval-between-rescan* (* 7 24 60 60)
  "Force rescan at leastr once this amount of seconds.")


(defparameter *asdf-registry-file*
  (merge-pathnames (user-homedir-pathname)
                   (make-pathname :name "ASDF-CENTRAL-REGISTRY" :type "DATA" :version :newest :case :common
                                  :defaults (user-homedir-pathname))
                   nil)
  "Cache file.")

(defparameter *original-asdf-registry* asdf:*central-registry*)
(defvar *asdf-install-location* #P"")
(defun find-asdf-subdirectories (&optional (directories (list *asdf-install-location*)))
  "Return a list of all the subdirectories of DIRECTORIES that contain .asd files.
It is sorted in ascending namestring length."
  (format *trace-output* "~&;; Scanning ASDF packages...~%")
  (prog1
      (sort
       (delete-duplicates
        (mapcar
         (lambda (p) (make-pathname :name nil :type nil :version nil :defaults p))
         (mapcan (lambda (dir)
                   (directory (merge-pathnames
                               (make-pathname :directory (if (pathname-directory dir)
                                                             '(:relative :wild-inferiors)
                                                             '(:absolute :wild-inferiors))
                                              :name :wild
                                              :type "ASD"
                                              :version :newest
                                              :case :common
                                              :defaults dir)
                               dir nil)))
                 directories))
        :test (function equal))
       (lambda (a b) (if (= (length a) (length b))
                         (string< a b)
                         (< (length a) (length b))))
       :key (function namestring))
    (format *trace-output* "~&;; Done.~%")))


(defun update-asdf-registry (&key (force-scan nil) (directories nil directoriesp))
  "Update asdf:*central-registry* with the subdirectories of DIRECTORIES containing ASD files,
either scanned, or from the cache."
  (length
   (setf asdf:*central-registry*
         (nconc (if (and (not force-scan)
                         (probe-file *asdf-registry-file*)
                         (let ((fdate (file-write-date *asdf-registry-file*)))
                           (and fdate (< (get-universal-time)
                                         (+ fdate *asdf-interval-between-rescan*)))))
                    ;; Get it from the cache.
                    (with-open-file (in *asdf-registry-file*)
                      (format *trace-output* "~&;; Reading ASDF packages from ~A...~%"
                              *asdf-registry-file*)
                      (let ((*read-eval* nil))
                        (read in nil nil)))
                    ;; Scan it anew.
                    (let ((scan (apply (function find-asdf-subdirectories)
                                       (when directoriesp
                                         (list directories)))))
                      (unless force-scan ; we save only when not :force-scan t
                        (format *trace-output* "~&;; Writing ASDF packages to ~A...~%"
                                *asdf-registry-file*)
                        (with-open-file (out *asdf-registry-file*
                                             :direction :output
                                             :if-does-not-exist :create
                                             :if-exists :supersede)
                          (print scan out)))
                      scan))
                *original-asdf-registry*))))

;;;; THE END ;;;;
