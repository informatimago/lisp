;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Initialization for sbcl packages.
;;;;
;;;;    This files remove some specificities from the lisp environment
;;;;    (to make it more Common-Lisp),
;;;;    loads the package COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE,
;;;;    and add logical pathname translations to help find the other packages.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-01-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2004 - 2004
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




;; Load COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE:


(SETF (logical-pathname-translations "PACKAGES") nil
      (LOGICAL-PATHNAME-TRANSLATIONS "PACKAGES")
      (list
       (LIST "PACKAGES:**;*"
             (get-directory :share-lisp "packages/**/*"))
       (LIST "PACKAGES:**;*.*"
             (get-directory :share-lisp "packages/**/*.*"))
       (LIST "PACKAGES:**;*.*.*"
             (get-directory :share-lisp "packages/**/*.*.*"))))

(HANDLER-CASE (LOAD (translate-logical-pathname
                     "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE"))
  (T ()       (LOAD (translate-logical-pathname
                     "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE.LISP"))))


;; (SETF (logical-pathname-translations "PACKAGES") nil)
;; (HANDLER-CASE (LOAD (get-directory :share-lisp "packages/com/informatimago/common-lisp/package"))
;;   (T ()       (LOAD (get-directory :share-lisp "packages/com/informatimago/common-lisp/package.lisp"))))



;; Import DEFINE-PACKAGE, and add translations:
(IMPORT 'PACKAGE:DEFINE-PACKAGE)
(PACKAGE:ADD-TRANSLATIONS
 (LIST "COM;INFORMATIMAGO;SBCL;*"     (get-directory :lisp-sources "sbcl/*"))  
 (LIST "COM;INFORMATIMAGO;SBCL;*.*"   (get-directory :lisp-sources "sbcl/*.*"))
 (LIST "COM;INFORMATIMAGO;SBCL;*.*.*" (get-directory :lisp-sources "sbcl/*.*"))
 (LIST "**;*"              (get-directory :share-lisp "packages/**/*"))
 (LIST "**;*.*"            (get-directory :share-lisp "packages/**/*.*"))
 (LIST "**;*.*.*"          (get-directory :share-lisp "packages/**/*.*")))



;; Note: We may also be loaded by non-SBCL, eg. to generate system.asd or summary.html.

#+sbcl
(defun logical-pathname-namestring (logical-pathname)
  (format nil "~A:~{~A;~}~:[~;~:*~A~:[~;.~:*~A~:[~;.~:*~A~]~]~]"
          (SB-IMPL::LOGICAL-HOST-NAME (pathname-host logical-pathname))
          (mapcar
           (lambda (item)
             (cond
               ((eq :wild-inferiors item) "**")
               ((eq :wild item) "*")
               (t item)))
           (if (eq :absolute (first (pathname-directory logical-pathname)))
               (rest (pathname-directory logical-pathname))
               (cons "" (rest (pathname-directory logical-pathname)))))
          (if (eq :wild (pathname-name logical-pathname))
              "*"
              (pathname-name logical-pathname))
          (if (eq :wild (pathname-type logical-pathname))
              "*"
              (pathname-type logical-pathname))
          (if (eq :wild (pathname-version logical-pathname))
              "*"
              (pathname-version logical-pathname))))

#+sbcl
(defun post-process-logical-pathname-translations (host)
  (flet ((pstring (x)
           (typecase x
             (logical-pathname (logical-pathname-namestring x))
             (pathname         (namestring x))
             (t                (string x)))))
    (setf (logical-pathname-translations host)
          (sort (mapcar
                 (lambda (trans)
                   (let ((p (second trans)))
                     (list (first trans)
                           (make-pathname :host      (pathname-host      p)
                                          :device    (pathname-device    p)
                                          :directory (pathname-directory p)
                                          :name      (pathname-name      p)
                                          :type      (pathname-type      p)
                                          :version   nil
                                          :defaults  #P""))))
                 (logical-pathname-translations host))
                (function >)
                :key (lambda (x) (length (pstring (first x))))))))

#+sbcl
(post-process-logical-pathname-translations "PACKAGES") 


;;;; THE END ;;;;
