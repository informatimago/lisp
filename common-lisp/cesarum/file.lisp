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
;;;;    2009-07-27 <PJB> Renamed TEXT-FILE-TO-STRING-LIST to STRING-LIST-TEXT-FILE-CONTENTS,
;;;;                     Added missing setf functions.
;;;;    2007-07-07 <PJB> Made use of new CONTENTS-FROM-STREAM function.
;;;;    2006-08-05 <PJB> Added SAFE-TEXT-FILE-TO-STRING-LIST.
;;;;    2005-03-17 <PJB> Added REMOVE-FIRST-LINES.
;;;;    2005-02-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2009
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
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
  (:documentation
   "This package exports file utility functions.

    binary-file-contents, sexp-file-contents, text-file-contents, and
    string-list-text-file-contents are accessors.
    They can be used with setf to store data into the file.
    (setf (sexp-file-contents list-file) (cons 'hi (sexp-file-contents file :if-does-not-exist '())))
    (incf (sexp-file-contents version-file :if-does-not-exist 0))

    Copyright Pascal J. Bourguignon 2005 - 2009
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
                "CONTENTS-FROM-STREAM"
                "STREAM-TO-STRING-LIST" "COPY-STREAM" "COPY-OVER")
  (:export "REMOVE-FIRST-LINES" "BINARY-FILE-CONTENTS"
           "SAFE-TEXT-FILE-TO-STRING-LIST"
           "STRING-LIST-TEXT-FILE-CONTENTS"
           "TEXT-FILE-CONTENTS" "SEXP-FILE-CONTENTS" "COPY-FILE"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")


(defun copy-file (src dst &key (if-exists :error) (external-format :default)
                  (element-type 'character))
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


(defun sexp-file-contents (path &key (if-does-not-exist :error)
                           (external-format :default))
  "
RETURN: The first SEXP of the file at PATH,
        or what is specified by IF-DOES-NOT-EXIST if it doesn't exist.
"
  (with-open-file (in path :direction :input
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format)
    (if (and (streamp in) (not (eq in if-does-not-exist)))
        (read in nil in)
        in)))


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


(defun string-list-text-file-contents (path &key (if-does-not-exist :error)
                                      (external-format :default))
  "
RETURN:  the list of lines collected from the file.
"
  (with-open-file (in path :direction :input
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format)
    (stream-to-string-list  in)))


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
RETURN: The contents of the file at PATH as a LIST of STRING lines.
        or what is specified by IF-DOES-NOT-EXIST if it doesn't exist.
"
  (with-open-file (in path :direction :input
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format)
    (if (and (streamp in) (not (eq in if-does-not-exist)))
        (contents-from-stream in :min-size 16384)
        in)))


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
RETURN: The contents of the file at PATH as a VECTOR of (UNSIGNED-BYTE 8),
        or what is specified by IF-DOES-NOT-EXIST if it doesn't exist.
"
  (with-open-file (in path :direction :input
                      :if-does-not-exist if-does-not-exist
                      :element-type element-type
                      :external-format external-format)
    (if (and (streamp in) (not (eq in if-does-not-exist)))
        (contents-from-stream in :min-size 16384)
        in)))


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
                     (if (<= COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII:sp code 126)
                         (aref COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII:*ascii-characters*
                               (- code COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII:sp))
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


(defun test/accessors (path)
  (let ((data '(a b c 1 2 3 #(a 1))))
    (setf (sexp-file-contents path) data)
    (assert (equalp data (sexp-file-contents path)))
    (push 0 (sexp-file-contents path))
    (assert (equalp (cons 0 data) (sexp-file-contents path))))
  (delete-file path)
  (let ((data   "
DO:         Modifies the file at path FILE-NAME, 
            removing the LINE-COUNT first lines.
WARNING:    There's no backup: if the COPY-OVER fails, the file will be left
            in an unspecified state.
"))
    (setf (text-file-contents path) data)
    (assert (string= data (text-file-contents path)))
    (setf (text-file-contents path :if-exists :append) "A new line")
    (assert (string= (concatenate 'string data "A new line") (text-file-contents path))))
  (delete-file path)
  (let ((data   '(""
                  "DO:         Modifies the file at path FILE-NAME, "
                  "            removing the LINE-COUNT first lines."
                  "WARNING:    There's no backup: if the COPY-OVER fails, the file will be left"
                  "            in an unspecified state."
                  "")))
    (setf (string-list-text-file-contents path) data)
    (assert (equal data (string-list-text-file-contents path)))
    (setf (string-list-text-file-contents path :if-exists :append)
          '("A new line" "And another one"))
    (assert (equal (append data '("A new line" "And another one"))
                   (string-list-text-file-contents path))))
  (delete-file path)
  (let ((data #(1 2 3 4 5 6 9 0 11 12 13)))
    (setf (binary-file-contents path) data)
    (assert (equalp data (binary-file-contents path)))
    (setf (binary-file-contents path :if-exists :append) data)
    (assert (equalp (concatenate 'vector data data) (binary-file-contents path))))
  (delete-file path)
  :success)

;;;; THE END ;;;;
