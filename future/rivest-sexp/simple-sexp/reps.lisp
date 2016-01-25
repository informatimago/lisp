;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               reps.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines representations for the read.lisp
;;;;    simple lisp reader/printer.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-10-26 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2007 - 2016
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
;;;;**************************************************************************

;;;--------------------
;;; Representations
;;;--------------------

;;; Strings:
(defun %make-string  (contents) (assert (stringp contents)) contents)
(defun %stringp (object) (stringp object))
(defun %string-length (object) (assert (%stringp object)) (length object))
(defun %string-ref (object i) (assert (%stringp object)) (char object i))

;;; Symbols:
;;;   These symbol name contains the package and colons in addition to
;;;   the name. 
(defstruct sym name)
(defun %make-symbol  (name) (make-sym :name name))
(defun %symbolp      (object) (typep object 'sym))
(defun %symbol-eql   (a b) (string= (sym-name a) (sym-name b)))
(defun %symbol-name  (s) (sym-name s))


;;; Integers:
(defun %make-integer (contents)
  (assert (stringp contents))
   (multiple-value-bind (ok strongp value) (parse-integer-token contents)
     (cond
       (ok      (assert (integerp value)) value)
       (strongp (error value))
       (t       nil))))
(defun %integerp (object) (integerp object))

;;; Floating points:
(defun %make-float (contents)
  (assert (stringp contents))
  (multiple-value-bind (ok strongp value) (parse-float-token contents)
    (cond
      (ok      (assert (floatp value)) value)
      (strongp (error value))
      (t       nil))))
(defun %floatp (object) (floatp object))


;;; Conses:
(defun %cons (car cdr) (cons car cdr))
(defun %consp (object) (consp object))
(defun %car (cons)     (car cons))
(defun %cdr (cons)     (cdr cons))
(defun %nil () '())
(defun %null (object) (eq (%nil) object))
(defun %nreverse (list) (nreverse list))
(defun %length (list) (length list))
(defun %nreconc (list tail) (nreconc list tail))
(defmacro %push (object list) `(push ,object ,list))
(defun %list (&rest args)
  (do ((sgra (reverse args) (cdr sgra))
       (list (%nil) (%cons (car sgra) list)))
      ((null sgra) list)))

;;; Hashes:
(defstruct hash   options data)
(defun %make-hash-table (options data)
  "
OPTIONS: an %alist of (%cons key value) options.
DATA:    an %alist of (%cons key value) data.
"
  (make-hash :options options :data data))
(defun %hash-table-p (object) (typep object 'hash))
(defun %hash-table-options (object)
  (assert (%hash-table-p object))
  (hash-options object))
(defun %hash-table-data (object)
  (assert (%hash-table-p object))
  (hash-data object))


;;; Structures:
(defstruct struct type data)
(defun %make-struct (type data) (make-struct :type type :data data))
(defun %structp (object) (typep object 'struct))
(defun %struct-type (object) (assert (%structp object)) (struct-type object))
(defun %struct-data (object) (assert (%structp object)) (struct-data object))

;;; Arrays:
(defun %make-array (dimensions contents)
  (if contents
      (make-array dimensions :initial-contents contents)
      (make-array dimensions)))
(defun %arrayp (object) (arrayp object))
(defun %array-ref (array &rest indices)
  (apply (function aref) array indices))
(defun %array-dimensions (array)
  (array-dimensions array))
(defun %array-collect-contents (array)
  "
RETURN: A %list of the array contents.
"
  (labels ((collect (indices dims)
             (if (null dims)
                 (apply (function %array-ref) array indices)
                 (let ((row (%nil)))
                   (dotimes (i (first dims) (%nreverse row))
                     (%push (collect (append indices (%cons i (%nil)))
                                     (%cdr dims)) row))))))
    (collect (%nil) (%array-dimensions array))))


;;; Dots:
(defstruct dots contents)
(defun %make-dots    (contents) (make-dots :contents contents))
(defun %dotsp (object)  (typep object 'dots))
(defun %dots-contents (object) (assert (%dots)))


;;;; THE END ;;;;
