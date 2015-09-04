;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               files.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    THis file defines the files operators.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-01-15 <PJB> Extracted from 'virtual-fs.lisp'.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 20. Files
;;; http://www.lispworks.com/documentation/HyperSpec/Body/20_.htm

(defun collapse-sequences-of-wild-inferiors (list)
  (if (search '(:wild-inferiors :wild-inferiors) list)
      (labels ((collapse (list)
                 (cond ((null list) list)
                       ((and (eq :wild-inferiors (first  list))
                             (eq :wild-inferiors (second list)))
                        (collapse (rest list)))
                       (t (cons (first list) (collapse (rest list)))))))
        (collapse list))
      list))


(defun collect (current dpath fspath)
  (cond
    ((null dpath)
     (if (pathname-name fspath)
         (let ((entries
                (select-entries
                 current
                 (lambda (item)
                   (and (typep item 'fs-file)
                        (match-item-p (name item) (pathname-name fspath) t)
                        (match-item-p (type item) (pathname-type fspath) t))))))
           (if  (pathname-version fspath)
                (mapcan (lambda (item)
                          (select-versions 
                           item
                           (lambda (version) 
                             (match-item-p (version version)
                                           (pathname-version fspath) nil))))
                        entries)
                entries))
         (list current)))
    ((eq :wild-inferiors (car dpath))
     (nconc (mapcan (lambda (item) (collect item dpath fspath))
                    (select-entries current (constantly t)))
            (mapcan (lambda (item) (collect item (rest dpath) fspath))
                    (select-entries current (constantly t)))))
    (t
     (mapcan 
      (lambda (item) (collect item (rest dpath) fspath))
      (select-entries
       current 
       (lambda (item) (and (typep item 'fs-directory)
                           (match-item-p (name item) (car dpath) t))))))))


(defun directory (pathspec &key)
  (let* ((fspath (resolve-pathspec pathspec))
         (fs  (if (pathname-host fspath)
                  (file-system-named (pathname-host fspath))
                  *default-file-system*)))
    (if fs
        (let ((d (cdr (pathname-directory fspath))))
          (mapcar (function pathname)
                  (collect fs (collapse-sequences-of-wild-inferiors d) fspath)))
        (error "Invalid host ~S"  (pathname-host fspath)))))


(defun ensure-directories-exist (pathspec &key verbose)
  (declare (ignore verbose))
  (let* ((fspath (resolve-pathspec pathspec))
         (fs  (if (pathname-host fspath)
                  (file-system-named (pathname-host fspath))
                  *default-file-system*))
         (dir (if (pathname-name fspath)
                  (pathname-directory fspath)
                  (butlast (pathname-directory fspath)))))
    (if fs
        (values pathspec (create-directories-at-path fs (cdr dir)))
        (error "There's no file system named ~S" (pathname-host fspath)))))


(defun truename (filespec)
  "
RETURN:      The truename of the filespec.
URL:         http://www.lispworks.com/documentation/HyperSpec/Body/f_tn.htm
COMMON-LISP: truename tries to find the file indicated by filespec and
             returns its truename.  If the filespec designator is an
             open stream, its associated file is used.  If filespec is
             a stream, truename can be used whether the stream is open
             or closed.  It is permissible for truename to return more
             specific information after the stream is closed than when
             the stream was open.  If filespec is a pathname it
             represents the name used to open the file.  This may be,
             but is not required to be, the actual name of the file.
"
  (let ((filespec (resolve-pathspec filespec)))
    (if (wild-pathname-p filespec)
        (error (make-condition 'simple-file-error
                               :pathname filespec
                               :format-control "~A: Filespec ~S is a wild pathname. "
                               :format-arguments (list 'truename filespec)))
        (let ((entry (file-entry filespec)))
          (if entry
              (pathname entry)
              (error (make-condition 'simple-file-error
                                     :pathname filespec
                                     :format-control "~A: File ~S does not exist. "
                                     :format-arguments (list 'truename filespec))))))))


(defun probe-file (pathspec)
  "
RETURN:      the truename of the file or NIL.
URL:         http://www.lispworks.com/documentation/HyperSpec/Body/f_probe_.htm
COMMON-LISP: probe-file tests whether a file exists.

             probe-file returns false if there is no file named
             pathspec, and otherwise returns the truename of
             pathspec.

             If the pathspec designator is an open stream, then
             probe-file produces the truename of its associated
             file. If pathspec is a stream, whether open or closed, it
             is coerced to a pathname as if by the  function pathname.
"
  (values (ignore-errors (truename pathspec))))



(defun file-author       (path) (author       (file-entry (truename path))))
(defun file-write-date   (path) (write-date   (file-entry (truename path))))
(defun file-element-type (path) (element-type (file-entry (truename path))))

(defmethod rename-entry ((self fs-file) newpath)
  ;; rename the whole file
  (when (ignore-errors (probe-file newpath))
    (delete-file newpath))
  (delete-entry self)
  (setf (name self) (pathname-name newpath)
        (type self) (pathname-type newpath))
  (add-entry newpath self)
  self)

(defmethod rename-entry ((self file-contents) newpath)
  ;; rename the version
  (let ((file (if (ignore-errors (probe-file newpath))
                  (file-at-path newpath)
                  (create-file-at-path newpath :create-version-p nil))))
    (remove-version (file self) (version self))
    (setf (version self) (if (newest file)
                             (max (version self) (1+ (version (newest file))))
                             (version self))
          (file self)   file
          (gethash (version self) (versions file)) self)
    self))

(defmethod delete-entry ((self fs-file))
  ;; delete the whole file
  (remove-entry-named (parent self) (pathname-entry-name self)))

(defmethod remove-version ((self fs-file) version)
  (remhash version (versions self))
  (when (= version (version (newest self)))
    (let ((maxk -1) (maxv))
      (maphash (lambda (k v) (when (< maxk k) (setf maxk k maxv v))) (versions self))
      (if maxv
          (setf (newest self) maxv)
          ;; otherwise, we've deleted the last version, let's delete the file:
          (delete-entry self)))))

(defmethod delete-entry ((self file-contents))
  ;; delete the version ( careful with (newest (file self)) ).
  (remove-version (file self) (version self)))

(defun rename-file (filespec new-name)
  (let* ((defaulted (merge-pathnames new-name filespec))
         (old-truename (truename filespec))
         (new-truename (resolve-pathspec defaulted)))
    (print (list defaulted old-truename new-truename))
    (when (wild-pathname-p defaulted)
      (error (make-condition
              'simple-file-error
              :pathname defaulted
              :format-control "~A: source path ~A contains wildcards"
              :format-arguments (list 'rename-file defaulted))))
    (when (wild-pathname-p new-truename)
      (error (make-condition
              'simple-file-error
              :pathname new-truename
              :format-control "~A: target path ~A contains wildcards"
              :format-arguments (list 'rename-file new-truename))))
    (let* ((newpath (make-pathname :version nil :defaults new-truename))
           (newdir  (directory-entry newpath)))
      (unless newdir
        (error (make-condition
                'simple-file-error
                :pathname newpath
                :format-control "~A: target directory ~A doesn't exist"
                :format-arguments (list 'rename-file newpath))))
      (rename-entry (file (file-entry old-truename)) newpath))
    (values defaulted old-truename new-truename)))

(defun delete-file (filespec)
  (delete-entry (file (file-entry (truename filespec))))
  t)

(defun delete-directory (pathspec)
  (let ((dir (directory-entry pathspec)))
    (when dir
      (when (plusp (hash-table-count (entries dir)))
        (error (make-condition
                'simple-file-error
                :pathname pathspec
                :format-control "~A: directory ~A is not empty"
                :format-arguments (list 'delete-directory pathspec))))
      (delete-entry dir)))
  t)

;;;; THE END ;;;;
