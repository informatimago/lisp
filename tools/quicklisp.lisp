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
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
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
           "QUICK-LOCAL-PROJECTS")
  (:documentation "Quicklisp quick commands."))
(in-package "COM.INFORMATIMAGO.TOOLS.QUICKLISP")

(defun print-systems (systems pattern)
  (if pattern
      (let ((spattern (string pattern)))
        (dolist (system systems)
          (when (search spattern (slot-value system 'ql-dist:name)
                        :test (function char-equal))
            (print system))))
      (dolist (system systems)
        (print system)))
   (values))


(defun quick-installed-systems (&optional pattern)
  "Print the system installed by quicklisp."
  (print-systems (ql-dist:installed-releases (ql-dist:dist "quicklisp"))
                 pattern))

(defun quick-list-systems (&optional pattern)
  "List the quicklisp systems.  If the string designator PATTERN is
given, then only the systems containing it in their name are listed."
  (print-systems (ql-dist:provided-systems t)
                 pattern))

(defun quick-list-projects (&optional pattern)
  "List the quicklisp projects (releases).  If the string designator
PATTERN is given, then only the projects containing it in their name
are listed."
  (print-systems (ql-dist:provided-releases t)
                 pattern))


(defun quick-apropos (pattern)
  "Search the quicklisp system matching the pattern and print them."
  ;; For now, we just list the systems:
  (print-systems (ql-dist:provided-systems t) pattern))


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

(defun quick-uninstall (&rest systems)
  "Uninstall the given systems releases from the quicklisp installation."
  (map 'list (lambda (system)
               (ql-dist:uninstall (ql-dist:release (string-downcase system))))
       systems))


(defun quick-where-is (&rest systems)
  "Says where the given systems are."
  #+#.(cl:if (cl:find-symbol "WHERE-IS-SYSTEM" "QUICKLISP-CLIENT") '(:and) '(:or))
  (map 'list (lambda (system) (ql:where-is-system (string-downcase system)))
       systems)
  #-#.(cl:if (cl:find-symbol "WHERE-IS-SYSTEM" "QUICKLISP-CLIENT") '(:and) '(:or))
  (error "QUICKLISP-CLIENT:WHERE-IS-SYSTEM is not available."))

(defun quick-where (&rest systems)
  "Says where the given systems are."
  (apply (function quick-where-is) systems))


(defun quick-delete (&rest systems)
  "Delete the ASDF systems so they'll be reloaded."
  (map 'list (lambda (system) (asdf:clear-system system)) systems))

(defun quick-reload (&rest systems)
  "Delete and reload the ASDF systems."
  (map 'list (lambda (system)
               ;; (asdf-delete-system system)
               (format *trace-output* "~&See also M-x slime-load-system RET~%")
               (force-output  *trace-output*)
               (asdf:load-system system)
               (ql:quickload system))
       systems))

(defun quick-local-projects ()
  "Rebuilds the local projects system index."
  (ql:register-local-projects))


;;;; THE END ;;;;
