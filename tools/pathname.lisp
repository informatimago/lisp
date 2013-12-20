;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pathname.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Pathname utilities.
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
(defpackage "COM.INFORMATIMAGO.TOOLS.PATHNAME"
  (:use "COMMON-LISP")
  (:shadow "MAKE-PATHNAME"
           "USER-HOMEDIR-PATHNAME"
           "TRANSLATE-LOGICAL-PATHNAME")
  (:export "MAKE-PATHNAME"
           "USER-HOMEDIR-PATHNAME"
           "TRANSLATE-LOGICAL-PATHNAME")
  (:documentation "Pathname tools."))
(in-package "COM.INFORMATIMAGO.TOOLS.PATHNAME")

(defparameter *case-common-is-not-downcased-on-posix-systems*
  #+(or allegro ccl emacs-cl) t
  #-(or allegro ccl emacs-cl) nil)


(defun user-homedir-pathname ()
  "On CCL on MS-Windows, it's not the USER-HOMEDIR-PATHNAME."
  #+(and ccl windows-target)
  (let ((home (ccl::getenv "HOME")))
    (if home
        (pathname (format nil "~A\\" home))
        #P"C:\\cygwin\\home\\pjb\\"))
  #-(and ccl windows-target)
  (cl:user-homedir-pathname))


(defun make-pathname (&key (host nil hostp) (device nil devicep) (directory nil directoryp)
                        (name nil namep) (type nil typep) (version nil versionp)
                        (defaults nil defaultsp) (case :local casep))
  (declare (ignorable casep))

  (if *case-common-is-not-downcased-on-posix-systems*
      (labels ((localize (object)
                 (typecase object
                   (list   (mapcar (function localize) object))
                   (string (string-downcase object))
                   (t      object)))
               (parameter (indicator key value)
                 (when indicator
                   (list key (if (eql case :common)
                                 (localize value)
                                 value)))))
        (apply (function cl:make-pathname)
               (append (parameter hostp      :host      host)
                       (parameter devicep    :device    device)
                       (parameter directoryp :directory directory)
                       (parameter namep      :name      name)
                       (parameter typep      :type      type)
                       (parameter versionp   :version   version)
                       (parameter defaultsp  :defaults  defaults)
                       (list :case :local))))
      (apply (function cl:make-pathname)
             (append
              (when hostp      (list :host      host))
              (when devicep    (list :device    device))
              (when directoryp (list :directory directory))
              (when namep      (list :name      name))
              (when typep      (list :type      type))
              (when versionp   (list :version   version))
              (when defaultsp  (list :defaults  defaults))
              (when casep      (list :case      case))))))


(defun translate-logical-pathname (pathname)
  (cl:translate-logical-pathname
   (etypecase pathname
     (string             (translate-logical-pathname (pathname pathname)))
     (logical-pathname   (make-pathname :host      (pathname-host pathname)
                                        :device    (pathname-device pathname)
                                        :directory (pathname-directory pathname)
                                        :name      (pathname-name pathname)
                                        :type      (pathname-type pathname)
                                        :version   (pathname-version pathname)
                                        :defaults  pathname
                                        :case      :common))
     (pathname           pathname))))


;;;; THE END ;;;;
