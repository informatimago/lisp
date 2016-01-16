;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mxarc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tool to extract multics archives.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-03-28 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(defpackage "COM.INFORMATIMAGO.MULTICS.MXARC"
  (:use "COMMON-LISP")
  (:export "MXARC"))
(in-package "COM.INFORMATIMAGO.MULTICS.MXARC")

(defconstant +sep+ 15)
(defconstant +eof+  0)

(defparameter *spaces* #(#\space #\tab #\newline #\return))
(defun spacep (ch)
  (find ch *spaces*))

(defun skip-to-next-record (stream)
  (loop
    :for byte = (read-byte stream)
    :until (eql +sep+ byte)))

(defun read-record (stream)
  (map 'string (function code-char)
    (loop
      :for byte = (read-byte stream)
      :until (eql +sep+ byte)
      :collect byte)))

(defun read-blob (stream)
  (loop
    :with data = (make-array 4096 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)
    :for byte = (loop
                  :for byte = (read-byte stream)
                  :while (eql +sep+ byte)
                  :finally (return byte))
      :then (read-byte stream nil stream)
    :until (or (eql +eof+ byte) (eql stream byte))
    :do (vector-push-extend byte data 4096)
    :finally (return data)))


(defun parse-date (string)
  (let ((mo (parse-integer string :start  0 :end  2))
        (da (parse-integer string :start  3 :end  5))
        (ye (parse-integer string :start  6 :end  8))
        (ho (parse-integer string :start 10 :end 12))
        (mi (parse-integer string :start 12 :end 14))
        (s  (parse-integer string :start 15 :end 16)))
    (encode-universal-time (* 60 (/ s 10)) mi ho da mo (+ ye (if (< ye 50) 2000 1900)) 0)))

(defun parse-header (header)
  (let* ((start (position-if-not (function spacep) header))
         (name  (string-trim " " (subseq header start (+ start 32))))
         (creation-date     (parse-date (subseq header (+ start 32) (+ start 48))))
         (modification-date (parse-date (subseq header (+ start 52) (+ start 68))))
         (access-rights     (subseq header (+ start 48) (+ start 51)))
         (size              (parse-integer (subseq header (+ start 68)))))
    (values name creation-date access-rights modification-date size)))

(defun save (path data &key (access-rights "r w")
                         (creation-date     (get-universal-time))
                         (modification-date (get-universal-time)))
  (declare (ignore access-rights creation-date modification-date))
  (with-open-file (out path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-does-not-exist :create
                       :if-exists :rename)
    (write-sequence data out)))

(defun extract (path &key (output-directory #P"") (verbose nil))
  (ensure-directories-exist path)
  (with-open-file (arc path
                       :direction :input
                       :element-type '(unsigned-byte 8))
    (handler-case
        (loop
          (skip-to-next-record arc)
          (let ((header (read-record arc)))
            (when verbose (write-line (string-trim *spaces* header)))
            (multiple-value-bind (name creation-date access-rights modification-date size)
                (parse-header header)
              (declare (ignore size))
              (let ((data (read-blob arc)))
                (save (merge-pathnames name output-directory nil) data
                      :access-rights access-rights
                      :creation-date creation-date
                      :modification-date modification-date)))))
      (end-of-file ()
        (return-from extract)))))

;; (extract #P"~/Downloads/bound_multics_emacs_.s.archive")

#-(and) (progn

          
         (let ((archive-dir  #P"~/Downloads/")
               (out-dir-root #P"/tmp/multics/"))
           (dolist (archive '("bound_emacs_ctls_.s.archive" 
                              "bound_emacs_full_.s.archive" 
                              "bound_emacs_macros_.s.archive" 
                              "bound_emacs_packages_.s.archive" 
                              "bound_emacs_rmail_.s.archive" 
                              "bound_multics_emacs_.s.archive"))
             (let* ((name (subseq archive (length "bound_") (1- (position #\. archive))))
                    (outdir (merge-pathnames (make-pathname :directory (list :relative name))
                                             out-dir-root)))
               (extract (merge-pathnames archive archive-dir)
                        :output-directory outdir)
               #-(and) (map nil 'print (directory (merge-pathnames "*.*" outdir))))))


         
         (extract #P"~/Downloads/bound_multics_emacs_.s.archive"
                  :output-directory #P"/tmp/me/" :verbose t)
         (directory #P"/tmp/me/*.*")
         
         )



;;;; THE END ;;;;
