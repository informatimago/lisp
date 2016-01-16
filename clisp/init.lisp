;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Initialization for clisp packages.
;;;;
;;;;    This files remove some specificities from the lisp environment
;;;;    (to make it more Common-Lisp),
;;;;    loads the package COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE,
;;;;    and add logical pathname translations to help find the other packages.
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
;;;;****************************************************************************

(setq *load-verbose* nil)

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


;; COM.INFORMATIMAGO.CLISP packages depend on themselves, from 
;; the current directory, and on COM.INFORMATIMAGO.COMMON-LISP
;; packages from the repository.


;; (SETF (logical-pathname-translations "PACKAGES") nil)
;; 
;; (HANDLER-CASE (LOAD (get-directory :share-lisp "packages/com/informatimago/common-lisp/package"))
;;   (T ()       (LOAD (get-directory :share-lisp "packages/com/informatimago/common-lisp/package.lisp"))))

(setf (logical-pathname-translations "PACKAGES") nil
      (logical-pathname-translations "PACKAGES")
      (list
       (list "PACKAGES:**;*"
             (get-directory :share-lisp "packages/**/*"))
       (list "PACKAGES:**;*.*"
             (get-directory :share-lisp "packages/**/*.*"))
       (list "PACKAGES:**;*.*.*"
             (get-directory :share-lisp "packages/**/*.*.*"))))


(handler-case (load "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE")
  (t ()       (load "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE.LISP")))

;; Import DEFINE-PACKAGE, and add translations:
(import 'package:define-package)

(package:add-translations
 (list "PACKAGES:COM;INFORMATIMAGO;CLISP;**;*.*.*"  "**/*.*.*"))

;;        (make-pathname
;;         :directory (append (pathname-directory *DEFAULT-PATHNAME-DEFAULTS*)
;;                            '(:wild-inferiors))
;;         :name :wild :type :wild :version :wild)))


;;;; init.lisp                        --                     --          ;;;;
