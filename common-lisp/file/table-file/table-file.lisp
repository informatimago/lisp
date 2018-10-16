;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               table-file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This package implements a binary file access method,
;;;;    with fixed-size records, indexed by row and column.
;;;;    The record size, and the numbers of rows and columns are fixed
;;;;    and determined at file creation time.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-10-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.FILE.TABLE-FILE"
  (:use "COMMON-LISP")
  (:export "CREATE-TABLE-FILE"
           "OPEN-TABLE-FILE"
           "TABLE-FILE-REF"
           "CLOSE-TABLE-FILE"
           "TABLE-FILE"
           "TABLE-FILE-P"
           "TABLE-FILE-VERSION"
           "TABLE-FILE-ROWS"
           "TABLE-FILE-COLS"
           "TABLE-FILE-RECORD-SIZE"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.FILE.TABLE-FILE")

(define-condition table-file-error (stream-error)
  ((table-file       :initarg :table-file       :reader table-file-error-table-file)
   (format-string    :initarg :format-string    :reader table-file-error-format-string)
   (format-arguments :initarg :format-arguments :reader table-file-error-format-arguments)
   (stream-error     :initarg :stream-error     :reader table-file-error-stream-error)))

(define-condition table-file-header-too-big-error (table-file-error)
  ())


(deftype octet () '(unsigned-byte 8))

(defconstant +header-size+ 1024)
(defconstant +filler+ (char-code #\newline) "Default byte to fill buffers with.")

(defun write-header (file rows cols record-size)
  (let* ((header (make-array +header-size+
                             :element-type 'octet
                             :initial-element +filler+))
         (data    (map 'vector 'char-code
                       (with-output-to-string (out)
                         (prin1 (list :file :table
                                      :version 1
                                      :rows rows
                                      :cols cols
                                      :record-size record-size)
                                out)
                         (terpri out)))))
    (when (< (length header) (length data))
      (error 'table-file-header-too-big-error
             :table-file file
             :format-string "Data header for the ~S at pathname ~S is too big (more than ~D bytes)."
             :format-arguments 'table-file (pathname file) +header-size+))
    (replace header data)
    (file-position file 0)
    (write-sequence header file)))

(defun create-table-file (pathname rows cols record-size)
  (with-open-file (file pathname :element-type 'octet
                                 :direction :io
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
    (write-header file rows cols record-size)
    (let ((row (make-array (* cols record-size)
                           :element-type 'octet
                           :initial-element +filler+)))
      (file-position file +header-size+)
      (loop :repeat rows
            :do (write-sequence row file)))))

(defstruct table-file
  stream
  version
  rows
  cols
  record-size
  serializer
  deserializer)

(defun read-header (stream)
  (file-position stream 0)
  (let ((header (make-array +header-size+ :element-type 'octet)))
    (read-sequence header stream)
    (let ((header (read-from-string (map 'string (function code-char) header))))
      (destructuring-bind (&key file version rows cols record-size) header
        (assert (and (eql file :table)
                     (eql version 1)
                     (typep rows '(integer 1))
                     (typep cols '(integer 1))
                     (typep record-size '(integer 1)))
                () "Bad header ~S for file ~S" header (pathname stream))
        (make-table-file :stream stream
                         :version version
                         :rows rows
                         :cols cols
                         :record-size record-size)))))

(defun open-table-file (pathname &key (direction :input) serializer deserializer)
  (assert (member direction '(:input :io)))
  (let* ((stream (open pathname :direction direction
                                :if-does-not-exist :error
                                :if-exists :append ; with :io
                                :element-type 'octet))
         (file (read-header stream)))
    (setf (table-file-serializer file) serializer
          (table-file-deserializer file) deserializer)
    file))

(defun check-arguments (file row col record)
  (assert (< -1 row (table-file-rows file))
          (row) "row ~A out of bounds 0 .. ~A" row (1-  (table-file-rows file)))
  (assert (< -1 col (table-file-cols file))
          (col) "col ~A out of bounds 0 .. ~A" col (1-  (table-file-cols file)))
  (when record
    (assert (and (typep record 'vector)
                 (subtypep 'octet (array-element-type record))
                 (<= (table-file-record-size file) (array-dimension record 0)))
            (record)
            "Improper record type ~S for the ~S at pathname ~S"
            (type-of record) 'table-file (pathname (table-file-stream file)))))

(defun table-file-ref (file row col &optional record)
  (check-arguments file row col record)
  (let ((pos (+ +header-size+
                (* (table-file-record-size file)
                   (+ col (* row (table-file-cols file))))))
        (record (or record
                    (make-array (table-file-record-size file)
                                :element-type 'octet)))
        (stream (table-file-stream file)))
    (file-position stream pos)
    (read-sequence record stream)
    (if (table-file-deserializer file)
        (funcall (table-file-deserializer file) record)
        record)))

(defun (setf table-file-ref) (new-value file row col &optional record)
  (check-arguments file row col record)
  (let ((pos (+ +header-size+
                (* (table-file-record-size file)
                   (+ col (* row (table-file-cols file))))))
        (record (or record  (make-array (table-file-record-size file)
                                        :element-type 'octet
                                        :initial-element +filler+)))
        (stream (table-file-stream file)))
    (replace record (if (table-file-serializer file)
                        (funcall (table-file-serializer file) new-value)
                        new-value))
    (file-position stream pos)
    (write-sequence record stream)
    new-value))

(defun close-table-file (file)
  (close (table-file-stream file)))


