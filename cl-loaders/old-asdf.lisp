;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               asdf.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loads ASDF.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-01-21 <PJB> Added scanning of PACKAGES: for ASD files.
;;;;    2003-06-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(in-package "COMMON-LISP-USER")
(load "CCLAN:ASDF;ASDF.LISP")
(load "CCLAN:CCLAN-GET;PACKAGE.LISP")
(load "CCLAN:CCLAN-GET;CCLAN-GET.ASD")
(handler-case (asdf:operate 'asdf:load-op :cclan-get)
  (error ()  (asdf:operate 'asdf:compile-op :cclan-get)))

(in-package "COM.INFORMATIMAGO.PJB")

(defparameter *original-asdf-registry* asdf:*central-registry*)

(setf cclan-get::*cclan-tarball-directory* "SHARE-LISP:CCLAN;TARBALL;"
      cclan-get::*cclan-source-directory*  "SHARE-LISP:CCLAN;SOURCE;"
      cclan-get::*cclan-asdf-registry*     "SHARE-LISP:CCLAN;REGISTRY;")

(defun asdf-rescan-packages ()
  (format *trace-output* "~&;; Scanning ASDF packages...~%")
  (prog1
      (sort 
       (delete-duplicates 
        (mapcar
         (lambda (p) (make-pathname :name nil :type nil :version nil :defaults p))
         (directory "PACKAGES:**;*.ASD")) 
        :test (function equal))
       (lambda (a b) (if (= (length a) (length b))
                  (string< a b)
                  (< (length a) (length b))))
       :key (function namestring))
    (format *trace-output* "~&;; Done.~%")))


(defun update-asdf-registry ()
  (setf asdf:*central-registry*
        (nconc (asdf-rescan-packages)
                (list cclan-get::*cclan-asdf-registry*)
                *original-asdf-registry*)))

(export 'update-asdf-registry)
(update-asdf-registry)

(in-package "COMMON-LISP-USER")
(asdf:operate 'asdf:load-op :asdf-install)

(format t
  "~2%Push on ASDF:*CENTRAL-REGISTRY* directories where .asd files are ~
      to be found.~@
      Use: (asdf:operate 'asdf:load-op ,system-name)~@
    ~&     (asdf-install:install       ,system-name)~@
   ~2%")


;;;; asdf.lisp                        --                     --          ;;;;
