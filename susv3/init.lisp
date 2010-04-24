;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Initialization for susv3 packages.
;;;;
;;;;    This files remove some specificities from the lisp environment
;;;;    (to make it more Common-Lisp),
;;;;    loads the package COM.INFORMATIMAGO.COMMON-LISP.PACKAGE,
;;;;    and add logical pathname translations to help find the other packages.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-01-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2004
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************
(SETQ *LOAD-VERBOSE* T)

;; clean the imported packages:
(MAPC (LAMBDA (USED) (UNUSE-PACKAGE USED "COMMON-LISP-USER"))
      (REMOVE (FIND-PACKAGE "COMMON-LISP") 
              (COPY-SEQ (PACKAGE-USE-LIST "COMMON-LISP-USER"))))

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
(when (string= (LISP-IMPLEMENTATION-VERSION)  "2.33.83"
               :end1 (min (length (LISP-IMPLEMENTATION-VERSION)) 7))
  (EXT:WITHOUT-PACKAGE-LOCK ("COMMON-LISP")
    (let ((oldload (function cl:load)))
      (fmakunbound 'cl:load)
      (defun cl:load (filespec &key (verbose *load-verbose*)
                      (print *load-print*)
                      (if-does-not-exist t)
                      (external-format :default))
        (handler-case (funcall oldload filespec :verbose verbose
                               :print print :if-does-not-exist if-does-not-exist
                               :external-format external-format)
          (SYSTEM::SIMPLE-PARSE-ERROR
              ()
            (funcall oldload (translate-logical-pathname filespec)
                     :verbose verbose
                     :print print :if-does-not-exist if-does-not-exist
                     :external-format external-format)))))))


;; COM.INFORMATIMAGO.CLISP packages depend on themselves, from 
;; the current directory, and on COM.INFORMATIMAGO.COMMON-LISP
;; packages from the repository.

(SETF (LOGICAL-PATHNAME-TRANSLATIONS "PACKAGES")
      (list (LIST "PACKAGES:**;*.*.*"
                  (get-directory :share-lisp "packages/**/*.*.*"))))

(HANDLER-CASE (LOAD "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE")
  (T ()       (LOAD "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE.LISP")))

;; Import DEFINE-PACKAGE, and add translations:
(IMPORT 'PACKAGE:DEFINE-PACKAGE)

;;----------------------------------------------------------------------


(PACKAGE:ADD-TRANSLATIONS
 (LIST "PACKAGES:COM;INFORMATIMAGO;SUSV3;**;*.*.*"  "**/*.*.*"))

;;;; init.lisp                        --                     --          ;;;;
