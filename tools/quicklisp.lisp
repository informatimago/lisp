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
;;;;    2021-05-13 <PJB> Added QUICK-WHERE-FROM and associated functions.
;;;;    2013-12-06 <PJB> Extracted from rc/common.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2013 - 2021
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
(defpackage "COM.INFORMATIMAGO.TOOLS.QUICKLISP"
  (:use "COMMON-LISP"
        "QUICKLISP"
        "ASDF"
        "COM.INFORMATIMAGO.TOOLS.ASDF"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "PRINT-SYSTEMS"
           "QUICK-INSTALLED-SYSTEMS"
           "QUICK-LIST-SYSTEMS"
           "QUICK-LIST-PROJECTS"
           "QUICK-APROPOS"
           "QUICK-UPDATE"
           "QUICK-CLEAN"
           "QUICK-INSTALL-ALL"
           "QUICK-LOAD-ALL"
           "QUICK-UNINSTALL"
           "QUICK-WHERE"
           "QUICK-WHERE-IS"   "SYSTEM-WHERE-IS"
           "QUICK-WHERE-FROM" "SYSTEM-WHERE-FROM" "PROJECT-WHERE-FROM"
           "QUICK-DELETE"
           "QUICK-RELOAD"
           "QUICK-LOCAL-PROJECTS"
           "QUICK-RESET"
           "QUICKLOAD/NEW-PACKAGES")
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
  (ql:update-all-dists)
  (update-project-dir :force t))

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


(defun quick-load-all (&key verbose)
  "Loads all the quicklisp systems, skipping over the errors."
  (map nil (lambda (system)
             (handler-case
                 (ql:quickload (ql-dist:name system) :verbose verbose)
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
  (apply (function quick-where-is) system systems))


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

(defun quickload/new-packages (systems &rest args &key &allow-other-keys)
  "Performs quickload, and return the list of the new packages."
  (let ((old-ps (mapcar (function package-name) (list-all-packages))))
    (apply (function ql:quickload) systems args)
    (let ((new-ps (mapcar (function package-name) (list-all-packages))))
      (sort (set-difference new-ps old-ps :test (function string=))
            (function string<)))))


(defconstant +one-month+ (* 30 24 60 60 ))
(defvar *projects-dir* nil)

(defun update-project-dir (&key force)
  (symbol-macrolet ((timestamp (sexp-file-contents (merge-pathnames "timestamp" *projects-dir*)
                                                   :if-does-not-exist 0)))
    (macrolet ((run-command-reporting-error (label command)
                 (let ((vout (gensym)) (verr (gensym)) (vstat (gensym)))
                   `(multiple-value-bind (,vout ,verr ,vstat)
                        (uiop:run-program ,command
                                          :ignore-error-status t :force-shell t
                                          :output 'string :error-output 'string)
                      (unless (zerop ,vstat)
                        (error "~A exited with status ~D:~%~A~%~A~%"
                               ,label ,vstat ,vout ,verr))))))
      (let* ((cache-dir   (merge-pathnames ".cache/" (user-homedir-pathname) nil))
             (project-dir (merge-pathnames "quicklisp-projects/" cache-dir nil))
             (probe       (merge-pathnames "README.md" *projects-dir* nil)))
        (setf *projects-dir* project-dir)
        (unless (probe-file probe)
          (ensure-directories-exist probe)
          (run-command-reporting-error
           "git cloning quicklisp-project"
           (format nil "cd ~S && git clone git@github.com:quicklisp/quicklisp-projects.git" (namestring cache-dir)))
          (setf timestamp (get-universal-time))))
      (when (or force (< timestamp (- (get-universal-time) +one-month+)))
        (run-command-reporting-error
         "git pulling quicklisp-project"
         (format nil "cd ~S && git pull" (namestring *projects-dir*)))
        (setf timestamp (get-universal-time))))))

(defun project-where-from (pname)
  "Return the contents of the source.txt file of the project PNAME in quicklisp-projects."
  (update-project-dir)
  (split-string (string-trim #(#\newline)
                             (text-file-contents (merge-pathnames
                                                  (make-pathname :directory (list :relative "projects" pname)
                                                                 :name "source" :type "txt" :version nil)
                                                  *projects-dir*)
                                                 :if-does-not-exist nil))
                " " t))

(defun system-where-is (system)
  "Return the path where the SYSTEM is stored (where the asd file is found)."
  #+#.(cl:if (cl:find-symbol "WHERE-IS-SYSTEM" "QUICKLISP-CLIENT") '(:and) '(:or))
  (ql:where-is-system system)
  #-#.(cl:if (cl:find-symbol "WHERE-IS-SYSTEM" "QUICKLISP-CLIENT") '(:and) '(:or))
  nil)

(defun system-where-from (system)
  "Return a list indicating where the project in the release that provided the SYSTEM originated from.
This is the contents of the source.txt file of the project in quicklisp-projects."
  (let* ((system       (ql-dist:find-system system))
         (release      (ql-dist:release system))
         (distribution (ql-dist:dist    system))
         (dname        (and distribution
                            (ql-dist:name distribution)))
         (pname        (and release
                            (ql-dist:project-name release))))
    (cond
      ((null pname)
       '())
      ((equal dname "quicklisp")
       (project-where-from pname))
      (t
       '()))))

(defun quick-where-from (system &rest systems)
  "Says where the systems are from."
  (let ((local-systems (ql:list-local-systems)))
    (dolist (sys (cons system systems))
      (let ((sname (asdf-system-name (asdf:find-system sys))))
        (if (member sname local-systems :test (function string=))
            (print (list :system sname
                         :distribution :local
                         :directory (system-where-is sname)
                         :from nil #|TODO: we could look in the directory if there's a .git and show-remotes |#))
            (let* ((system       (ql-dist:find-system sname))
                   (release      (ql-dist:release system))
                   (distribution (ql-dist:dist    system))
                   (dname        (ql-dist:name distribution))
                   (pname        (and release
                                      (ql-dist:project-name release)))
                   (wfrom        (cond
                                   ((null pname)
                                    '())
                                   ((string= dname "quicklisp")
                                    (project-where-from pname))
                                   (t
                                    '()))))
              (print (list :system sname
                           :distribution dname
                           :directory (system-where-is sname)
                           :where-from wfrom))))))))

;;;; THE END ;;;;

