;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               quicklisp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Quicklisp tools.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-06 <PJB> Extracted from rc/common.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2015
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
(in-package "COMMON-LISP-USER")


(defpackage "COM.INFORMATIMAGO.TOOLS.QUICKLISP"
  (:use "COMMON-LISP"
        "QUICKLISP"
        "ASDF")
  (:export "PRINT-SYSTEMS"
           "QUICK-INSTALLED-SYSTEMS"
           "QUICK-LIST-SYSTEMS"
           "QUICK-LIST-PROJECTS"
           "QUICK-APROPOS"
           "QUICK-UPDATE"
           "QUICK-CLEAN"
           "QUICK-INSTALL-ALL"
           "QUICK-UNINSTALL"
           "QUICK-WHERE-IS"
           "QUICK-WHERE"
           "QUICK-DELETE"
           "QUICK-RELOAD"
           "QUICK-LOCAL-PROJECTS"
           "QUICK-RESET")
  (:documentation "Quicklisp quick commands."))
(in-package "COM.INFORMATIMAGO.TOOLS.QUICKLISP")

(defun print-systems (systems pattern &key sort)
  (flet ((name (thing)
           (cond ((ignore-errors (slot-boundp thing  'ql-dist:name))
                  (slot-value thing 'ql-dist:name))
                 ((ignore-errors (slot-boundp thing 'ql-dist:project-name))
                  (slot-value thing 'ql-dist:project-name)))))
   (let ((sorted-systems (make-array (length systems) :fill-pointer 0)))
     (if pattern
         (let ((spattern (string pattern)))
           (dolist (system systems)
             (when (search spattern (name system) :test (function char-equal))
               (vector-push system sorted-systems))))
         (progn
           (setf (fill-pointer sorted-systems) (length systems))
           (replace sorted-systems systems)))
     (map nil (function print)
       (if sort
           (sort sorted-systems (function string<) :key (function name))
           sorted-systems))
     (terpri)))
   (values))


(defun quick-installed-systems (&optional pattern)
  "Print the system installed by quicklisp."
  (print-systems (ql-dist:installed-releases (ql-dist:dist "quicklisp"))
                 pattern :sort t))

(defun quick-list-systems (&optional pattern)
  "List the quicklisp systems.  If the string designator PATTERN is
given, then only the systems containing it in their name are listed."
  (print-systems (ql-dist:provided-systems t)
                 pattern :sort t))

(defun quick-list-projects (&optional pattern)
  "List the quicklisp projects (releases).  If the string designator
PATTERN is given, then only the projects containing it in their name
are listed."
  (print-systems (ql-dist:provided-releases t)
                 pattern :sort t))


(defun quick-apropos (pattern)
  "Search the quicklisp system matching the pattern and print them."
  ;; For now, we just list the systems:
  (print-systems (ql-dist:provided-systems t) pattern :sort t))


(defun quick-update ()
  "Updates the quicklisp client, and all the system distributions."
  (ql:update-client)
  (ql:update-all-dists)) 

(defun quick-clean ()
  "Clean the quicklisp system distributions."
  #+#.(cl:if (cl:find-symbol "CLEAN" "QL-DIST") '(:and) '(:or))
  (map nil 'ql-dist:clean (ql-dist:enabled-dists))
  #-#.(cl:if (cl:find-symbol "CLEAN" "QL-DIST") '(:and) '(:or))
  (error "QL-DIST:CLEAN is not available."))

(defun quick-install-all (&key verbose)
  "Installs all the quicklisp systems, skipping over the errors."
  (map nil (lambda (system)
             (handler-case
                 (progn
                   (when verbose
                     (format *trace-output* "~&~A~%" system))
                   (ql-dist:ensure-installed system))
               (error (err)
                 (format *trace-output* "~&~A ~A~%" system err))))
       (ql-dist:provided-systems t)))

(defun quick-uninstall (system &rest systems)
  "Uninstall the given systems releases from the quicklisp installation."
  (map 'list (lambda (system)
               (ql-dist:uninstall (ql-dist:release (string-downcase system))))
       (cons system systems)))


(defun quick-where-is (system &rest systems)
  "Says where the given systems are."
  #+#.(cl:if (cl:find-symbol "WHERE-IS-SYSTEM" "QUICKLISP-CLIENT") '(:and) '(:or))
  (map 'list (lambda (system) (ql:where-is-system (string-downcase system)))
       (cons system systems))
  #-#.(cl:if (cl:find-symbol "WHERE-IS-SYSTEM" "QUICKLISP-CLIENT") '(:and) '(:or))
  (error "QUICKLISP-CLIENT:WHERE-IS-SYSTEM is not available."))

(defun quick-where (system &rest systems)
  "Says where the given systems are."
  (apply (function quick-where-is) (cons system systems)))


(defun quick-delete (system &rest systems)
  "Delete the ASDF systems so they'll be reloaded."
  (map 'list (lambda (system) (asdf:clear-system system)) (cons system systems)))

(defun quick-reload (system &rest systems)
  "Delete and reload the ASDF systems."
  (map 'list (lambda (system)
               ;; (asdf-delete-system system)
               (format *trace-output* "~&See also M-x slime-load-system RET~%")
               (force-output  *trace-output*)
               (asdf:load-system system)
               (ql:quickload system))
       (cons system systems)))

(defun quick-local-projects ()
  "Rebuilds the local projects system index."
  (ql:register-local-projects))

(defun quick-reset ()
  "Rebuilds the local projects system index."
  (quick-local-projects))


;;;; THE END ;;;;
