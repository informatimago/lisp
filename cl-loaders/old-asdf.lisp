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
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-01-21 <PJB> Added scanning of PACKAGES: for ASD files.
;;;;    2003-06-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2005.
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

(in-package "COMMON-LISP-USER")
(LOAD "CCLAN:ASDF;ASDF.LISP")
(LOAD "CCLAN:CCLAN-GET;PACKAGE.LISP")
(LOAD "CCLAN:CCLAN-GET;CCLAN-GET.ASD")
(handler-case (ASDF:OPERATE 'ASDF:load-OP :CCLAN-GET)
  (error ()  (ASDF:OPERATE 'ASDF:compile-OP :CCLAN-GET)))

(in-package "COM.INFORMATIMAGO.PJB")

(defparameter *original-asdf-registry* ASDF:*CENTRAL-REGISTRY*)

(SETF CCLAN-GET::*CCLAN-TARBALL-DIRECTORY* "SHARE-LISP:CCLAN;TARBALL;"
      CCLAN-GET::*CCLAN-SOURCE-DIRECTORY*  "SHARE-LISP:CCLAN;SOURCE;"
      CCLAN-GET::*CCLAN-ASDF-REGISTRY*     "SHARE-LISP:CCLAN;REGISTRY;")

(defun asdf-rescan-packages ()
  (format *trace-output* "~&;; Scanning ASDF packages...~%")
  (prog1
      (SORT 
       (DELETE-DUPLICATES 
        (MAPCAR
         (LAMBDA (P) (MAKE-PATHNAME :NAME NIL :TYPE NIL :VERSION NIL :DEFAULTS P))
         (DIRECTORY "PACKAGES:**;*.ASD")) 
        :test (function equal))
       (LAMBDA (A B) (if (= (length a) (length b))
                  (string< a b)
                  (< (length a) (length b))))
       :key (function namestring))
    (format *trace-output* "~&;; Done.~%")))


(defun update-asdf-registry ()
  (setf ASDF:*CENTRAL-REGISTRY*
        (nconc (asdf-rescan-packages)
                (list CCLAN-GET::*CCLAN-ASDF-REGISTRY*)
                *original-asdf-registry*)))

(EXPORT 'UPDATE-ASDF-REGISTRY)
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
