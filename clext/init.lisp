;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Initialization for clext packages.
;;;;
;;;;    This files remove some specificities from the lisp environment
;;;;    (to make it more Common Lisp),
;;;;    initialize the environment 
;;;;    and add logical pathname translations to help find the other packages.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-06-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2006 - 2016
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



(defparameter *default-version*
  #+clisp nil
  #+sbcl  nil
  #-(or clisp sbcl) (progn (warn "What default version to use in ~A?"
                                 (lisp-implementation-type))
                           :newest))

(defparameter *project-directory*
  (truename
   (merge-pathnames
    (make-pathname :directory '(:relative)) 
    (make-pathname :name nil :type nil :version nil
                   :defaults *load-truename*) *default-version*))
  "The directory of this project.")



(defun make-translations (host logical-dir physical-dir)
  (mapcar
   (lambda (item)
     (destructuring-bind (logical-tail physical-tail) item
       (list (apply (function make-pathname)
                    :host host
                    :directory `(:absolute ,@logical-dir :wild-inferiors)
                    logical-tail)
             (format nil "~A**/~A" physical-dir physical-tail))))
   #+clisp
   '(((:name :wild :type :wild :version nil)   "*.*")
     ((:name :wild :type nil   :version nil)   "*"))
   #+sbcl
   '(((:name :wild :type :wild :version :wild)  "*.*"))
   #-(or clisp sbcl)
   '(((:name :wild :type nil   :version nil)   "*")
     ((:name :wild :type :wild :version nil)   "*.*")
     ((:name :wild :type :wild :version :wild) "*.*"))))


(setf (logical-pathname-translations "PACKAGES") nil
      (logical-pathname-translations "PACKAGES")
      (append
       (make-translations "PACKAGES" '("COM" "INFORMATIMAGO" "CLEXT")
                          *project-directory*)
       ;; clext packages dont depend on com.informatimago.common-lisp (yet)
       ;; but compile.lisp uses com.informatimago.common-lisp.make-depends.make-depends
       (make-translations "PACKAGES" '() (get-directory :share-lisp "packages/"))))

(handler-case (load "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE")
  (t ()       (load "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE.LISP")))

(import 'package:define-package)


;;;; init.lisp                        --                     --          ;;;;
