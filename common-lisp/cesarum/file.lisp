;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports some file utility functions.
;;;;    
;;;;    binary-file-contents, sexp-file-contents, text-file-contents, and
;;;;    string-list-text-file-contents are accessors.
;;;;    They can be used with setf to store data into the file.
;;;;    (push 'hi (sexp-file-contents file :if-does-not-exist '()))
;;;;    (incf (sexp-file-contents version-file :if-does-not-exist 0))
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-10-22 <PJB> Factorized out file reader functions, handle if-does-not-exist non-nil values.
;;;;    2009-07-27 <PJB> Renamed TEXT-FILE-TO-STRING-LIST to STRING-LIST-TEXT-FILE-CONTENTS,
;;;;                     Added missing setf functions.
;;;;    2007-07-07 <PJB> Made use of new CONTENTS-FROM-STREAM function.
;;;;    2006-08-05 <PJB> Added SAFE-TEXT-FILE-TO-STRING-LIST.
;;;;    2005-03-17 <PJB> Added REMOVE-FIRST-LINES.
;;;;    2005-02-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM")
    #+mocl (:shadowing-import-from "COM.INFORMATIMAGO.MOCL.KLUDGES.MISSING"
                                 "*TRACE-OUTPUT*"
                                 "ARRAY-DISPLACEMENT"
                                 "CHANGE-CLASS"
                                 "COMPILE"
                                 "COMPLEX"
                                 "ENSURE-DIRECTORIES-EXIST"
                                 "FILE-WRITE-DATE"
                                 "INVOKE-DEBUGGER" "*DEBUGGER-HOOK*"
                                 "LOAD"
                                 "LOGICAL-PATHNAME-TRANSLATIONS"
                                 "MACHINE-INSTANCE"
                                 "MACHINE-VERSION"
                                 "NSET-DIFFERENCE"
                                 "RENAME-FILE"
                                 "SUBSTITUTE-IF"
                                 "TRANSLATE-LOGICAL-PATHNAME")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
                "CONTENTS-FROM-STREAM"
                "STREAM-TO-STRING-LIST" "COPY-STREAM" "COPY-OVER")
  (:export "REMOVE-FIRST-LINES" "BINARY-FILE-CONTENTS"
           "SAFE-TEXT-FILE-TO-STRING-LIST"
           "STRING-LIST-TEXT-FILE-CONTENTS" "TEXT-FILE-CONTENTS"
           "SEXP-FILE-CONTENTS" "SEXP-LIST-FILE-CONTENTS"
           "COPY-FILE" "COPY-DIRECTORY")
  (:documentation
   "

This package exports file utility functions.

BINARY-FILE-CONTENTS, SEXP-FILE-CONTENTS, TEXT-FILE-CONTENTS, and
STRING-LIST-TEXT-FILE-CONTENTS are accessors.
They can be used with setf to store data into the file.

Examples:

    (setf (sexp-file-contents list-file) (cons 'hi (sexp-file-contents file :if-does-not-exist '())))
    (incf (sexp-file-contents version-file :if-does-not-exist 0))


See also: COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2005 - 2014
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")


(defun copy-file (src dst &key (if-exists :error) (external-format :default) (element-type '(unsigned-byte 8)))
  "
DO:     Copy the contents of the file at path SRC to the file at path DST.
"
  (with-open-file (inp src
                       :direction :input
                       :if-does-not-exist :error
                       :external-format external-format
                       :element-type element-type)
    (with-open-file (out dst
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists if-exists
                         :external-format external-format
                         :element-type element-type)
      (copy-stream inp out))))


(defun copy-directory (src dst &key (recursively t) (verbose nil) (on-error :error)
                                 (if-exists :error) (external-format :default) (element-type '(unsigned-byte 8)))
  "
DO:             Copy files from the directory SRC to the directory
                DST, using the specified key parameters.

RECURSIVELY:    When NIL, only the files directly under SRC are
                copied.  Otherwise, subdirectories and all their files
                are also copied.

IF-ERROR:       Can be :ERROR, then the error is signaled,
                :CONTINUE, then the error is ignored and copying continues,
                :ABORT, then the error is ignored, and copying is aborted.

RETURN:         list-of-destination-files-copied ; list-of-source-files-with-errors

NOTE:           Files are scanned with CL:DIRECTORY, so only those
                that are accessible from the CL implementation are
                copied.

NOTE:           Empty subdirectories are not copied.
"
  (let* ((src       (truename src))
         (src-files (mapcar (lambda (path) (cons path (enough-namestring path src)))
                            (remove-if (lambda (path) (not (or (pathname-name path) (pathname-type path))))
                                       (remove-duplicates
                                        (append (directory (merge-pathnames (if recursively "**/*.*" "*.*") src))
                                                (directory (merge-pathnames (if recursively "**/*"   "*")   src)))
                                        :test (function equalp)))))
         (copied-files '())
         (error-files  '()))
    (dolist (src-file src-files (values copied-files error-files))
      (let ((dst-file (merge-pathnames (cdr src-file) dst)))
        (when verbose (format *trace-output* "~&;; ~S -> ~S~%" (car src-file) dst-file))
        (ensure-directories-exist dst-file)
        (block :copy
          (handler-bind ((error (lambda (err)
                                  (push (cons (car src-file) err) error-files)
                                  (case on-error
                                    ((:error)
                                     nil)
                                    ((:continue)
                                      (when verbose (format *error-output* "~&ERROR: ~A~%" err))
                                      (return-from :copy))
                                    ((:abort)
                                     (when verbose (format *error-output* "~&ERROR: ~A~%" err))
                                     (return-from copy-directory (values copied-files error-files)))))))
            (copy-file (car src-file) dst-file :if-exists if-exists :external-format external-format :element-type element-type)
            (push dst-file copied-files)))))))




(defun call-with-input-file (path element-type if-does-not-exist external-format thunk)
  (let ((if-does-not-exist-action (if (member if-does-not-exist '(:error :create nil))
                                      if-does-not-exist
                                      nil))
        (if-does-not-exist-value  (if (member if-does-not-exist '(:error :create nil))
                                      nil
                                      if-does-not-exist)))
    (with-open-file (in path :direction :input
                             :if-does-not-exist if-does-not-exist-action
                             :element-type element-type
                             :external-format external-format)
      (typecase in
        (null if-does-not-exist-value)
        (stream (funcall thunk in))))))


(defmacro with-input-file ((streamvar path element-type if-does-not-exist external-format) &body body)
  `(call-with-input-file ,path ,element-type ,if-does-not-exist ,external-format (lambda (,streamvar) ,@body)))


(defun sexp-file-contents (path &key (if-does-not-exist :error)
                                  (external-format :default))
  "
IF-DOES-NOT-EXIST:  Can be :error, :create, nil, or another value that
                    is returned instead of the content of the file if
                    it doesn't exist.

RETURN:             The first SEXP of the file at PATH, or the value
                    of IF-DOES-NOT-EXIST when not :ERROR or :CREATE
                    and the file doesn't exist.
"
  (with-input-file (in path 'character if-does-not-exist external-format)
    (let ((*read-base* 10.))
      (read in nil in))))


(defun (setf sexp-file-contents) (new-contents path
                                  &key (if-does-not-exist :create)
                                  (if-exists :supersede)
                                  (external-format :default))
  "
DO:     Writes the NEW-CONTENTS SEXP readably into the file at PATH.  By default,
        that file is created or superseded; this can be changed with
        the keyword IF-DOES-NOT-EXIST or IF-EXISTS.
RETURN: The NEW-CONTENTS, or if-exists or if-does-not-exist in case of error.
"
  (with-open-file (out path :direction :output
                       :if-does-not-exist if-does-not-exist
                       :if-exists if-exists
                       :external-format external-format)
    (if (and (streamp out) (not (or (eq out if-exists)  (eq out if-does-not-exist))))
        (write new-contents :stream out
               :array t :base 10. :case :upcase :circle t
               :escape t :gensym t :length nil :level nil :lines nil
               :miser-width nil  :pretty nil
               :radix t :readably t :right-margin nil)
        out)))


(defun sexp-list-file-contents (path &key (if-does-not-exist :error)
                                       (external-format :default))
  "
IF-DOES-NOT-EXIST:  Can be :error, :create, nil, or another value that
                    is returned instead of the content of the file if
                    it doesn't exist.

RETURN:             All the SEXPs of the file at PATH gathered in a
                    list, or the value of IF-DOES-NOT-EXIST when not
                    :ERROR or :CREATE and the file doesn't exist.
"
  (with-input-file (in path 'character if-does-not-exist external-format)
    (let ((*read-base* 10.))
      (loop
        :for form = (read in nil in)
        :until (eq form in)
        :collect form))))

(defun (setf sexp-list-file-contents) (new-contents path
                                       &key (if-does-not-exist :create)
                                         (if-exists :supersede)
                                         (external-format :default))
  "
NEW-CONTENTS:   A list of sexps.

DO:             Writes the NEW-CONTENTS SEXPs readably into the file
                at PATH.  By default, that file is created or
                superseded; this can be changed with the keyword
                IF-DOES-NOT-EXIST or IF-EXISTS.

RETURN:         The NEW-CONTENTS, or if-exists or if-does-not-exist in
                case of error.
"
  (with-open-file (out path :direction :output
                       :if-does-not-exist if-does-not-exist
                       :if-exists if-exists
                       :external-format external-format)
    (if (and (streamp out) (not (or (eq out if-exists)  (eq out if-does-not-exist))))
        (let ((*read-base* 10.))
          (loop
            :for form :in new-contents
            :do (write form :stream out
                       :array t :base 10. :case :upcase :circle t
                       :escape t :gensym t :length nil :level nil :lines nil
                       :miser-width nil  :pretty nil
                       :radix t :readably t :right-margin nil)
            :do (terpri out)))
        out)))



(defun string-list-text-file-contents (path &key (if-does-not-exist :error)
                                              (external-format :default))
  "
IF-DOES-NOT-EXIST:  Can be :error, :create, nil, or another value that
                    is returned instead of the content of the file if
                    it doesn't exist.

RETURN:             The list of lines collected from the file, or the
                    value of IF-DOES-NOT-EXIST when not :ERROR or
                    :CREATE and the file doesn't exist.
"
  (with-input-file (in path 'character if-does-not-exist external-format)
     (stream-to-string-list in)))


(defun (setf string-list-text-file-contents) (new-contents path
                                  &key (if-does-not-exist :create)
                                  (if-exists :supersede)
                                  (external-format :default))
  "
DO:             Store the NEW-CONTENTS, into the file at PATH, each string on a line.
                By default, that file is created or superseded; this can be changed with
                the keyword IF-DOES-NOT-EXIST or IF-EXISTS.
NEW-CONTENT:    A sequence of strings, none of them should contain #\newline,
                otherwise the mapping between strings and file lines won't be
                conserved.
RETURN:         The NEW-CONTENTS or if-exists or if-does-not-exist in case of error.
"
  (with-open-file (out path :direction :output
                       :if-does-not-exist if-does-not-exist
                       :if-exists if-exists
                       :external-format external-format)
    (if (and (streamp out) (not (or (eq out if-exists)  (eq out if-does-not-exist))))
        (progn (map nil (lambda (line) (write-line line out)) new-contents)
               new-contents)
        out)))


(defun text-file-contents (path &key (if-does-not-exist :error)
                                  (external-format :default))
  "
IF-DOES-NOT-EXIST:  Can be :error, :create, nil, or another value that
                    is returned instead of the content of the file if
                    it doesn't exist.

RETURN:             The contents of the file at PATH as a LIST of
                    STRING lines, or the value of IF-DOES-NOT-EXIST
                    when not :ERROR or :CREATE and the file doesn't
                    exist.
"
  (with-input-file (in path 'character if-does-not-exist external-format)
    (contents-from-stream in :min-size 16384)))


(defun (setf text-file-contents) (new-contents path
                                  &key (if-does-not-exist :create)
                                  (if-exists :supersede)
                                  (external-format :default))
  "
RETURN: The NEW-CONTENTS, or if-exists or if-does-not-exist in case of error.
DO:     Store the NEW-CONTENTS into the file at PATH.  By default,
        that file is created or superseded; this can be changed with
        the keyword IF-DOES-NOT-EXIST or IF-EXISTS.
"
  (with-open-file (out path :direction :output
                       :if-does-not-exist if-does-not-exist
                       :if-exists if-exists
                       :external-format external-format)
    (if (and (streamp out) (not (or (eq out if-exists)  (eq out if-does-not-exist))))
        (write-sequence new-contents out)
        out)))


(defun binary-file-contents (path &key (if-does-not-exist :error)
                                    (element-type '(unsigned-byte 8))
                                    (external-format :default))
  "
IF-DOES-NOT-EXIST:  Can be :error, :create, nil, or another value that
                    is returned instead of the content of the file if
                    it doesn't exist.

RETURN:             The contents of the file at PATH as a VECTOR of
                    (UNSIGNED-BYTE 8), or the value of
                    IF-DOES-NOT-EXIST when not :ERROR or :CREATE and
                    the file doesn't exist.
"
  (with-input-file (in path element-type if-does-not-exist external-format)
    (contents-from-stream in :min-size 16384)))


(defun (setf binary-file-contents) (new-contents path
                                    &key (if-does-not-exist :create)
                                    (if-exists :supersede)
                                    (element-type '(unsigned-byte 8))
                                    (external-format :default))
  "
RETURN: The NEW-CONTENTS, or if-exists or if-does-not-exist in case of error.
DO:     Store the NEW-CONTENTS into the file at PATH.  By default,
        that file is created or superseded; this can be changed with
        the keyword IF-DOES-NOT-EXIST or IF-EXISTS.
NEW-CONTENT:  A sequence of ELEMENT-TYPE.
"
  (with-open-file (out path :direction :output
                       :if-does-not-exist if-does-not-exist
                       :if-exists if-exists
                       :element-type element-type
                       :external-format external-format)
    (if (and (streamp out) (not (or (eq out if-exists)  (eq out if-does-not-exist))))
        (write-sequence new-contents out)
        out)))





(defun safe-text-file-to-string-list (path &key (if-does-not-exist :error))
  "
DO:     - Read the file at PATH as a binary file,
        - Remove all null bytes (handle UTF-16, UCS-2, etc),
        - Split 'lines' on CR, CR+LF or LF,
        - Replace all bytes less than 32 or greater than 126 by #\?,
        - Convert the remaining bytes as ASCII codes into the CL standard set.
RETURN: The contents of the file as a list of base-string lines.
"
  (loop
     :with data   = (delete 0 (binary-file-contents
                               path :if-does-not-exist if-does-not-exist))
     :with cursor = (make-array 1 :element-type '(unsigned-byte 8)
                                :adjustable t
                                :displaced-to data :displaced-index-offset 0)
     :for bol = 0 :then eol
     :for eol = (or (position-if (lambda (ch) (or (= 10 ch) (= 13 ch)))
                                 data :start bol)
                    (length data))
     :collect (map 'string
                   (lambda (code)
                     (if (<= sp code 126)
                         (aref *ascii-characters* (- code sp))
                         #\?))
                    (adjust-array cursor (- eol bol)
                                        :displaced-to data
                                        :displaced-index-offset bol))
     :do (cond
           ((<= (length data) eol))
           ((and (= 13 (aref data eol))
                 (< (1+ eol) (length data))
                 (= 10 (aref data (1+ eol))))
            (incf eol 2))
           (t (incf eol)))
     :while (< eol (length data))))


(defun remove-first-lines (file-name line-count &key (element-type 'character))
  "
DO:         Modifies the file at path FILE-NAME, 
            removing the LINE-COUNT first lines.
WARNING:    There's no backup: if the COPY-OVER fails, the file will be left
            in an unspecified state.
"
  (with-open-file (file file-name :direction :io 
                        :element-type element-type
                        :if-exists :overwrite
                        :if-does-not-exist :error)
    ;; skip over the LINE-COUNT first lines:
    (dotimes (i line-count)
      (unless(print (read-line file nil nil))
        (error "Less than ~A lines in the file ~A." line-count file-name)))
    ;; copy over the rest of the file to the start:
    (copy-over file (file-position file) 0)))



;;;; THE END ;;;;
