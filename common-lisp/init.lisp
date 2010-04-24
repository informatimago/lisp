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
;;;;***************************************************************************

(SETQ *LOAD-VERBOSE* T)
#+clisp (SETQ custom:*LOAD-echo* nil)

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

;; (DEFUN SCONC (&REST ARGS) 
;;   (apply (function CONCATENATE)
;;          'string
;;          (mapcar (lambda (item) (typecase item
;;                              (pathname   (namestring item))
;;                              (otherwise  (string item)))) ARGS)));;SCONC


;; COM.INFORMATIMAGO.COMMON-LISP packages depends only on themselves, 
;; from the current directory.

;; Load COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:
(HANDLER-CASE (LOAD "package")
  (T ()       (LOAD "package.lisp")))

;; Import DEFINE-PACKAGE, and add translations:
(IMPORT 'PACKAGE:DEFINE-PACKAGE)
(SETF (LOGICAL-PATHNAME-TRANSLATIONS "PACKAGES")
      (HANDLER-CASE (LOGICAL-PATHNAME-TRANSLATIONS "PACKAGES") 
        (ERROR NIL)))
(PACKAGE:ADD-TRANSLATIONS
 (LIST (make-pathname
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
               (nth-value 1 (UNIX:UNIX-CURRENT-DIRECTORY)) nil)))
        #+(and cmu (not unix)) (error "Cannot compile here.")
         nil))
 (LIST (make-pathname
        :host "PACKAGES"
        :directory '(:absolute :wild-inferiors)
        :name :wild :type :wild :version :wild)
       (get-directory :share-lisp "packages/**/*.*")))


;; #+sbcl (setf (logical-pathname-translations "PACKAGES")
;;              (sort (copy-seq (logical-pathname-translations "PACKAGES"))
;;                    (lambda (a b) (< (length (second a)) (length (second b))))))

;;;; init.lisp                        --                     --          ;;;;


