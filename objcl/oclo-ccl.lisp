;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               oclo-ccl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines implementation specific additions to the
;;;;    oclo package.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-12-17 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.OBJECTIVE-C.LOWER")

(defun objc-to-lisp-classname-p (str &optional (package *package*))
  (let ((name (gethash str ccl::*lisp-classname-table*)))
    (typecase name
      (string (setf (gethash str ccl::*lisp-classname-table*)
                    (or (find-symbol name "NS")
                        (intern name package))))
      (t      name))))


(defun lisp-to-objc-classname-p (sym)
   (gethash sym ccl::*objc-classname-table*))



(defparameter *null* ccl:+null-ptr+
  "A NULL pointer.
Don't compare to it to check for a null pointer, but use NULLP instead.")


(defun nullp (object)
  "
RETURN: Whether OBJECT is a null pointer.
"
  (ccl::%null-ptr-p object))


(defun selector (name)
  "
NAME:   A selector name (string). Example: \"initWithFrame:\"
RETURN: The Objective-C selector named NAME.
"
  (ccl::%get-selector (ccl::ensure-objc-selector name)))


;;;; THE END ;;;;
