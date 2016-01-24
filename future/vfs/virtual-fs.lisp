;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               virtual-fs.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file implements a RAM based virtual file system.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-04-26 <PJB> Created.  Quick-and-Dirty.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2016
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

(in-package "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")

;; (do-external-symbols (symbol "COMMON-LISP") 
;;   (export (intern (string symbol) *package*)))


(defvar *maximum-file-size* (* 1024 1024)
  "Maximum virtual file size.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals: Memory based file systems.

(deftype file-type        () '(or string null))
(deftype pathname-type    () '(or file-type (member :wild :unspecific)))
(deftype file-version     () 'integer)
(deftype pathname-version () '(or file-version
                               (member nil :wild :unspecific :newest)))

(defvar *dump-indent* "    :")

(defgeneric dump (object &OPTIONAL STREAM LEVEL)
  (:documentation "Dumps the OBJECT to *standard-output*."))


;;;---------------------------------------------------------------------
;;; FS-ITEM
;;;---------------------------------------------------------------------

(defclass fs-item ()
  ((parent :accessor parent
           :initarg :parent 
           :type (or null fs-directory)))
  (:documentation "Either a file, a directory or a whole file system."))


(defmethod dump ((self fs-item)
                 &optional (stream *standard-output*) (level ""))
  (format stream "~A--> [ITM] ~S ### CANNOT DUMP SUCH AN ITEM~%" level self))


;;;---------------------------------------------------------------------
;;; FS-DIRECTORY
;;;---------------------------------------------------------------------

(defclass fs-directory (fs-item)
  ((name            :accessor name
                    :initarg :name
                    :type     string)
   (entries         :accessor entries
                    :initarg :entries
                    :initform (make-hash-table :test (function equal))
                    :type     hash-table))
  (:documentation "A directory, mapping item names to subdirectories or files."))

(defmethod pathname ((self fs-directory))
  (let ((path (pathname (parent self))))
    (setf (%pathname-directory path) (nconc (%pathname-directory path)
                                            (list (name self))))
    path))

(defmethod dump ((self fs-directory)
                 &optional (stream *standard-output*) (level ""))
  (format t "~A--> [DIR] ~A~%" level (name self))
  (maphash (lambda (k v)
             (declare (ignore k))
             (dump v stream (concatenate 'string level *dump-indent*)))
           (entries self)))

(defgeneric select-entries (self predicate))
(defmethod select-entries ((self t) predicate)
  (declare (ignorable self predicate))
  '())

(defmethod select-entries ((self fs-directory) predicate)
  (declare (ignorable predicate))
  (let ((result '()))
    (maphash (lambda (k v) 
               (declare (ignore k))
               (when (funcall predicate v) (push v result))) (entries self))
    result))

(defgeneric entry-name (self))
(defmethod entry-name ((self fs-directory)) 
  (name self))

(defgeneric entry-named (self name))
(defmethod entry-named ((self t) (name string))
  (error "~A is not a directory" (pathname self)))

(defmethod entry-named ((self fs-directory) (name string))
  (gethash name (entries self)))


(defgeneric entry-at-path (self path))
(defmethod entry-at-path ((self t) path)
  (declare (ignorable path))
  (error "~S is not a directory" self))

(defmethod entry-at-path ((self fs-directory) path)
  (if (null path)
      self
      (let ((entry (entry-named self (car path))))
        (if (null entry)
            (error "There's no directory ~A~A;" (pathname self) (car path))
            (entry-at-path entry (cdr path))))))


(defgeneric add-entry (self entry))
(defmethod add-entry ((self fs-directory) (entry fs-item))
  (let ((name (entry-name entry)))
    (if (entry-named self name)
        (error "An entry named ~S already exists in ~S" name self)
        (setf (parent entry)                self
              (gethash name (entries self)) entry))))

(defgeneric remove-entry-named (self name))
(defmethod remove-entry-named ((self fs-directory) (name string))
  (if (entry-named self name)
      (remhash name (entries self))
      (error "No entry named ~S exists in ~S" name self)))


;;;---------------------------------------------------------------------
;;; FS-FILE
;;;---------------------------------------------------------------------

(defclass fs-file (fs-item)
  ((name            :accessor name
                    :initarg :name
                    :type     string)
   (type            :accessor type
                    :initarg :type
                    :type     file-type)
   (versions        :accessor versions
                    :initarg :versions
                    :initform (make-hash-table :test (function eql))
                    :type     hash-table)
   (newest          :accessor newest
                    :initarg :newest
                    :initform nil
                    :type    (or null file-contents)))
  (:documentation "A file, with all its versions."))

(defmethod pathname ((self fs-file))
  (let ((path (pathname (parent self))))
    (setf (%pathname-name path)  (name self)
          (%pathname-type path)  (type self))
    path))

(defmethod dump ((self fs-file)
                 &optional (stream *standard-output*) (level ""))
  (format t "~A--> [FIL] ~A.~A~%" level (name self) (type self))
  (dolist (v (let ((versions '()))
               (maphash (lambda (k v)
                          (declare (ignore k))
                          (push v versions))
                        (versions self))
               (sort versions (function <) :key (function version))))
    (dump v stream (concatenate 'string level *dump-indent*)))
  self)

(defun pathname-entry-name (path)
  (format nil "~A.~A" (pathname-name path) (pathname-type path)))

(defmethod entry-name ((self fs-file)) 
  (format nil "~A.~A" (name self) (type self)))

(defgeneric author (self))
(defgeneric write-date (self))
(defgeneric element-type (self))

(defmethod author       ((self fs-file)) (author       (newest self)))
(defmethod write-date   ((self fs-file)) (write-date   (newest self)))
(defmethod element-type ((self fs-file)) (element-type (newest self)))

(defgeneric select-versions (self predicate))
(defmethod select-versions ((self fs-file) predicate)
  (let ((result '()))
    (maphash (lambda (k v) 
               (declare (ignore k))
               (when (funcall predicate v) (push v result))) (versions self))
    result))


(defun purge-file (file)
  "
DO: Delete all the versions of the file but the newest.
"
  (let ((entry (file-entry file)))
    (if entry
        (let* ((file   (file entry))
               (newest (newest file)))
          (when newest
            (let ((newtab (make-hash-table :test (function eql))))
              (setf (gethash (version newest) newtab) newest
                    (versions file) newtab))))
        (error "There's no file ~A" file)))
  file)


(defun delete-version (file)
  "
DO: Delete only the specified version.
"
  (let ((entry (file-entry file)))
    (if entry
        (remove-version (file entry) (version entry))
        (error "There's no file ~A" file))))


;;;---------------------------------------------------------------------
;;; FILE-CONTENTS
;;;---------------------------------------------------------------------

(defclass file-contents ()
  ((file            :accessor file
                    :initarg :file
                    :type  fs-file)
   (version         :accessor version
                    :initarg :version
                    :type     file-version)
   (author          :accessor author
                    :initarg :author
                    :initform nil
                    :type    (or null string))
   (write-date      :accessor write-date
                    :initarg :write-date
                    :initform (get-universal-time)
                    :type    (or null (integer 0)))
   (element-type    :accessor element-type
                    :initarg :element-type
                    :initform 'character)
   (contents        :accessor contents
                    :initarg :contents
                    :type     vector))
  (:documentation "A versionned file contents."))

(defmethod pathname ((self file-contents))
  (let ((path (pathname (file self))))
    (setf (%pathname-version path) (version self))
    path))

(defmethod dump ((self file-contents)
                 &optional (stream *standard-output*) (level ""))
  (format stream "~A--> [VER] ~A ~((:AUTHOR ~S :WRITE-DATE ~S :ELEMENT-TYPE ~S :SIZE ~A)~)~%"
          level (version self) (author self) (write-date self)
          (element-type self)
          (length (contents self))))


;;;---------------------------------------------------------------------
;;; fs-file


(defvar *author* nil
  "The name or identification of the user.")


(defgeneric create-new-version (self &key element-type))
(defmethod create-new-version ((self fs-file) &key (element-type 'character))
  "
DO:     Add a new version to the file.
RETURN: The FS-FILE.
"
  (setf (newest self)
        (make-instance 'file-contents
                       :version (1+ (if (null (newest self)) 0 (version (newest self))))
                       :author *author*
                       :write-date (get-universal-time)
                       :element-type element-type
                       :contents (make-array 0 :fill-pointer 0 :adjustable t
                                               :element-type element-type)
                       :file self))
  (setf (gethash (version (newest self)) (versions self)) (newest self))
  self)



;;;---------------------------------------------------------------------
;;; FILE SYSTEM 
;;;---------------------------------------------------------------------
;;;
;;; We initialize three file systems ROOT:, SYS: and HOME:.
;;;

(defclass file-system (fs-directory)
  ()
  (:documentation "A file system."))

(defmethod pathname ((self file-system))
  (make-pathname :host (name self) :directory (list :absolute)))


(defparameter *file-systems* (make-hash-table :test (function equal)))


(defun file-system-register (fs)
  (setf (gethash (name fs) *file-systems*) fs))

(defun file-system-named (name)
  (gethash name *file-systems*))


(defparameter *default-file-system* 
  (file-system-register (make-instance 'file-system :name "ROOT")))

(file-system-register (make-instance 'file-system :name "SYS"))
(file-system-register (make-instance 'file-system :name "HOME"))

(defparameter *default-pathname-defaults* 
  (make-pathname :host (name *default-file-system*)
                 :directory '(:absolute)
                 :defaults nil))



(defun resolve-pathspec (pathspec)
  (translate-logical-pathname (pathname pathspec)))


(defun directory-entry (pathspec)
  (let* ((fspath (resolve-pathspec pathspec))
         (fs  (if (pathname-host fspath)
                  (file-system-named (pathname-host fspath))
                  *default-file-system*)))
    (if fs
        (entry-at-path fs (cdr (pathname-directory fspath)))
        (error "There's no file system named ~S" (pathname-host fspath)))))


(defgeneric create-directories-at-path (self path &optional created))
(defmethod create-directories-at-path ((self fs-directory) path
                                       &optional created)
  (if (null path)
      created
      (let ((entry (entry-named self (car path))))
        (unless entry
          (setf entry (make-instance 'fs-directory :name (car path))
                created t)
          (add-entry self entry))
        (if (typep entry 'fs-directory)
            (create-directories-at-path entry (cdr path) created)
            (error "~A~A; already exists and is not a directory."
                   (pathname self) (car path))))))


(defun file-entry (pathspec)
  "
RETURN: The FILE-CONTENTS specified by PATHSPEC (if no version is specified, NEWEST is returned).
"
  (let* ((file     (resolve-pathspec pathspec))
         (dir      (directory-entry file))
         (entry    (entry-named dir (pathname-entry-name file))))
    (when entry
      (case (pathname-version file)
        ((nil :newest) (newest entry))
        (otherwise     (gethash (pathname-version file) (versions entry)))))))


(defun create-file-at-path (pathspec &key (create-version-p t) (element-type 'character))
  "
RETURN: The FS-FILE created.
NOTE:   If a FS-FILE existed at the given PATHSPEC, then it is returned,
        a new version being created if CREATE-VERSION-P is true.
"
  (let* ((file     (resolve-pathspec pathspec))
         (dir      (directory-entry file))
         (entry    (entry-named dir (pathname-entry-name file))))
    (unless entry
      (setf entry (make-instance 'fs-file
                      :name (pathname-name file) :type (pathname-type file)))
      (add-entry dir entry))
    (typecase entry
      (fs-file (if create-version-p 
                   (create-new-version entry :element-type element-type)
                   entry))
      (t (error "~A already exist and is not a file" (pathname entry))))))



;;;; the END ;;;;
