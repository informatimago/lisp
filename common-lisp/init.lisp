;;;; -*- coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:               init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Initialization for common-lisp packages.
;;;;
;;;;    This files remove some specificities from the lisp environment
;;;;    (to make it more Common-Lisp),
;;;;    loads the package COM.INFORMATIMAGO.COMMON-LISP.PACKAGE,
;;;;    and add logical pathname translations to help find then other packages.
;;;;
;;;;    Since we're generating an image, it should be useful only
;;;;    at compilation-time, so any path present here should not be needed
;;;;    at run-time. (But we don't clear them from the translations...).
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-01-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2004 - 2016
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
;;;;***************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(setq *load-verbose* t)
#+clisp (setq custom:*load-echo* nil)

;; clean the imported packages:
(mapc (lambda (used) (unuse-package used "COMMON-LISP-USER"))
      (remove (find-package "COMMON-LISP")
              (copy-seq (package-use-list "COMMON-LISP-USER"))))

(progn
  (defvar *directories*  '())
  (defun get-directory (key &optional (subpath ""))
    (unless *directories*
      (with-open-file (dirs (make-pathname :name "DIRECTORIES" :type "TXT"
                                           :version nil :case :common
                                           :defaults (user-homedir-pathname)))
        (loop
           :for k = (read dirs nil dirs)
           :until (eq k dirs)
           :do (push (string-trim " " (read-line dirs)) *directories*)
           :do (push (intern (substitute #\- #\_ (string k))
                             "KEYWORD") *directories*))))
    (unless (getf *directories* key)
      (error "~S: No directory keyed ~S" 'get-directory key))
    (merge-pathnames subpath (getf *directories* key) nil)))


#+clisp
(when (string= (lisp-implementation-version)  "2.33.83"
               :end1 (min (length (lisp-implementation-version)) 7))
  (ext:without-package-lock ("COMMON-LISP")
    (let ((oldload (function cl:load)))
      (fmakunbound 'cl:load)
      (defun cl:load (filespec &key (verbose *load-verbose*)
                      (print *load-print*)
                      (if-does-not-exist t)
                      (external-format :default))
        (handler-case (funcall oldload filespec :verbose verbose
                               :print print :if-does-not-exist if-does-not-exist
                               :external-format external-format)
          (system::simple-parse-error
              ()
            (funcall oldload (translate-logical-pathname filespec)
                     :verbose verbose
                     :print print :if-does-not-exist if-does-not-exist
                     :external-format external-format)))))))

;; (DEFUN SCONC (&REST ARGS)
;;   (apply (function CONCATENATE)
;;          'string
;;          (mapcar (lambda (item) (typecase item
;;                              (pathname   (namestring item))
;;                              (otherwise  (string item)))) ARGS)));;SCONC


;; COM.INFORMATIMAGO.COMMON-LISP packages depends only on themselves,
;; from the current directory.

;; Load COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:
(handler-case (load "package")
  (t ()       (load "package.lisp")))

;; Import DEFINE-PACKAGE, and add translations:
(import 'package:define-package)
(setf (logical-pathname-translations "PACKAGES")
      (handler-case (logical-pathname-translations "PACKAGES")
        (error nil)))
(package:add-translations
 (list (make-pathname
        :host "PACKAGES"
        :directory '(:absolute
                     "COM" "INFORMATIMAGO" #-cmu"COMMON-LISP"
                     :wild-inferiors)
        :name :wild :type :wild :version :wild)
       (merge-pathnames
        (make-pathname
         :directory '(:relative :wild-inferiors)
         :name :wild :type :wild :version :wild)
        #-cmu *load-pathname*
        #+(and cmu unix)
        (let ((dir (pathname-directory *load-pathname*)))
          (if (and dir (eq :absolute (first dir)))
              *load-pathname*
              (merge-pathnames
               *load-pathname*
               (nth-value 1 (unix:unix-current-directory)) nil)))
        #+(and cmu (not unix)) (error "Cannot compile here.")
         nil))
 (list (make-pathname
        :host "PACKAGES"
        :directory '(:absolute :wild-inferiors)
        :name :wild :type :wild :version :wild)
       (get-directory :share-lisp "packages/**/*.*")))


;; #+sbcl (setf (logical-pathname-translations "PACKAGES")
;;              (sort (copy-seq (logical-pathname-translations "PACKAGES"))
;;                    (lambda (a b) (< (length (second a)) (length (second b))))))

;;;; init.lisp                        --                     --          ;;;;


