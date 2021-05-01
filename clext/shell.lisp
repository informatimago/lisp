;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               shell.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Exports shell functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2020-11-04 <PJB> Extracted from tools.manifest.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2020 - 2020
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
(defpackage "COM.INFORMATIMAGO.CLEXT.SHELL"
  (:use "COMMON-LISP")
  (:export "SHELL-COMMAND-TO-STRING"
           "MKTEMP-PATH"))
(in-package "COM.INFORMATIMAGO.CLEXT.SHELL")

(defun mktemp-path (&key
                      (kind :file)
                      (stem "TEMP")
                      (type "TXT")
                      base-directory)
  (check-type kind (member :file :directory))
  (check-type stem string)
  (check-type type (or null string))
  (check-type base-directory (or null string pathname))
  (let ((name (format nil "~:@(~A~36,8,'0R~)"
                      stem (random (expt 2 32))))
        (type (when type (string-upcase type))))
    (namestring
     (translate-logical-pathname
      (ecase kind
        (:file
         (cond
           (base-directory
            (merge-pathnames
             (make-pathname :name name :type type :version nil
                            :case :common)
             base-directory))
           ((ignore-errors (logical-pathname-translations "TMP"))
            (make-pathname :host "TMP" :directory '(:absolute)
                           :name name :type type :version nil
                           :case :common))
           (t
            (merge-pathnames
             (make-pathname :directory '(:relative)
                            :name name :type type :version nil
                            :case :common)
             (user-homedir-pathname)))))
        (:directory
         (cond
           (base-directory
            (merge-pathnames
             (make-pathname :directory (list :relative name)
                            :name nil :type nil :version nil
                            :case :common)
             base-directory))
           ((ignore-errors (logical-pathname-translations "TMP"))
            (make-pathname :host "TMP" :directory (list :absolute name)
                           :name nil :type nil :version nil
                           :case :common))
           (t
            (merge-pathnames
             (make-pathname :directory (list :relative name)
                            :name nil :type nil :version nil
                            :case :common)
             (user-homedir-pathname))))))))))

#-(and)
(list
 (mktemp-path)
 (mktemp-path :kind :file)
 (mktemp-path :kind :file :stem "foo" :type "bar")
 (mktemp-path :kind :file :base-directory "/var/tmp/")
 (mktemp-path :kind :directory)
 (mktemp-path :kind :directory  :stem "foo" :type "bar")
 (mktemp-path :kind :directory :base-directory "/var/tmp/"))


(defun shell-command-to-string (command &rest arguments)
  "Execute the COMMAND with asdf:run-shell-command and returns its
stdout in a string (going thru a file)."
  (let* ((*default-pathname-defaults* #P"")
         (path (mktemp-path :stem "OUT-")))
    (unwind-protect
         (when (zerop (asdf:run-shell-command
                       (format nil "~? > ~S" command arguments path)))
           (with-output-to-string (out)
             (with-open-file (file path)
               (loop
                 :for line = (read-line file nil nil)
                 :while line :do (write-line line out)))))
      (ignore-errors (delete-file path)))))

;;;; THE END ;;;;
