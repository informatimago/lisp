;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               filter-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Filter streams are stream wrappers with a function to process
;;;;    the data being input or output.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-04-16 <PJB> Close is also defered to the filter function.
;;;;                     Now all operations can be performed by the filter
;;;;                     function and the filter-stream stream can be any
;;;;                     object, not necessarily a stream.
;;;;    2016-12-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2016 - 2016
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.CLEXT.FILTER-STREAM"
  (:use "COMMON-LISP"
        "TRIVIAL-GRAY-STREAMS")
  (:export "MAKE-INPUT-FILTER-STREAM"
           "MAKE-OUTPUT-FILTER-STREAM"
           "FILTER-STREAM-STREAM"
           "FILTER-STREAM-FUNCTION"
           "FILTER-ELEMENT-TYPE"
           "NO-FILTER"))
(in-package "COM.INFORMATIMAGO.CLEXT.FILTER-STREAM")


(defclass filter ()
  ((function     :initarg :function     :accessor filter-stream-function)
   (stream       :initarg :stream       :accessor filter-stream-stream)
   (element-type :initarg :element-type :reader   filter-stream-element-type)))

(defclass filter-character-input-stream (filter fundamental-character-input-stream)
  ((column :initform 0 :accessor column)))

(defclass filter-character-output-stream (filter fundamental-character-output-stream)
  ((column :initform 0 :accessor column)))

(defclass filter-binary-input-stream (filter fundamental-binary-input-stream)
  ())

(defclass filter-binary-output-stream (filter fundamental-binary-output-stream)
  ())

(defun make-input-filter-stream (stream filter-function &key element-type)
  (let ((element-type (or element-type (stream-element-type stream))))
    (make-instance (if (subtypep element-type 'character)
                       'filter-character-input-stream
                       'filter-binary-input-stream)
                   :function filter-function
                   :stream stream)))

(defun make-output-filter-stream (stream filter-function &key element-type)
  (let ((element-type (or element-type (stream-element-type stream))))
    (make-instance (if (subtypep element-type 'character)
                       'filter-character-output-stream
                       'filter-binary-output-stream)
                   :function filter-function
                   :stream stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; stream methods
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-stream-open (stream where)
  (unless (open-stream-p stream)
    (error "~S cannot deal with closed stream ~S"
           where stream)))

;;; character input


(declaim (inline update-column))
(defun update-column (stream ch)
  (when (characterp ch)
    (if (char= ch #\newline)
        (setf (column stream) 0)
        (incf (column stream))))
  ch)


(defmethod stream-read-char ((stream filter-character-input-stream))
  (check-stream-open stream 'stream-read-char)
  (funcall (filter-stream-function stream)
           'read-char
           (filter-stream-stream stream)))

(defmethod stream-read-char-no-hang ((stream filter-character-input-stream))
  (check-stream-open stream 'stream-read-char-no-hang)
  (funcall (filter-stream-function stream)
           'read-char-no-hang
           (filter-stream-stream stream)))

 (defmethod stream-peek-char ((stream filter-character-input-stream))
   (check-stream-open stream 'stream-peek-char)
   (funcall (filter-stream-function stream)
            'peek-char
            (filter-stream-stream stream)))

(defmethod stream-read-line ((stream filter-character-input-stream))
  (check-stream-open stream 'stream-read-line)
  (funcall (filter-stream-function stream)
            'read-line
            (filter-stream-stream stream)))

(defmethod stream-listen ((stream filter-character-input-stream))
  (check-stream-open stream 'stream-listen)
  (funcall (filter-stream-function stream)
            'listen
            (filter-stream-stream stream)))

(defmethod stream-unread-char ((stream filter-character-input-stream) ch)
  (check-stream-open stream 'stream-unread-char)
  (funcall (filter-stream-function stream)
            'unread-char
            (filter-stream-stream stream)
            ch)
  ch)


;;; character output

(defmethod stream-write-char ((stream filter-character-output-stream) ch)
  (check-stream-open stream 'stream-write-char)
  (funcall (filter-stream-function stream)
           'write-char
           (filter-stream-stream stream)
           ch)
  (if (char= #\newline ch)
      (setf (column stream) 0)
      (incf (column stream)))
  ch)

(defmethod stream-terpri ((stream filter-character-output-stream))
  (check-stream-open stream 'stream-terpri)
  (stream-write-char stream #\newline)
  nil)

(defmethod stream-write-string ((stream filter-character-output-stream) string &optional (start 0) end)
  (check-stream-open stream 'stream-write-string)
  (let* ((end  (or end (length string)))
         (nlp  (position #\newline string :start start :end end :from-end t)))
    (funcall (filter-stream-function stream)
             'write-string
             (filter-stream-stream stream)
             string start (or end (length string)))
    (if nlp
        (setf (column stream) (- end nlp))
        (incf (column stream) (- end start))))
  string)

(defmethod stream-line-column ((stream filter-character-output-stream))
  (column stream))

(defmethod stream-start-line-p ((stream filter-character-output-stream))
  (zerop (column stream)))

(defmethod stream-advance-to-column ((stream filter-character-output-stream) column)
  (check-stream-open stream 'stream-advance-to-column)
  (let ((delta (- column (column stream))))
    (when (plusp delta)
      (stream-write-string stream (make-string delta :initial-element #\space))
      delta)))



(defmethod close ((stream filter-character-output-stream) &key abort)
  (funcall (filter-stream-function stream)
           'close
           (filter-stream-stream stream)
           abort))

(defmethod close ((stream filter-binary-output-stream) &key abort)
  (funcall (filter-stream-function stream)
           'close
           (filter-stream-stream stream)
           abort))

(defmethod close ((stream filter-character-input-stream) &key abort)
  (funcall (filter-stream-function stream)
           'close
           (filter-stream-stream stream)
           abort))

(defmethod close ((stream filter-binary-input-stream) &key abort)
  (funcall (filter-stream-function stream)
           'close
           (filter-stream-stream stream)
           abort))


;; binary input

(defmethod stream-read-byte ((stream filter-binary-input-stream))
  (check-stream-open stream 'stream-read-byte)
  (funcall (filter-stream-function stream)
           'read-byte
           (filter-stream-stream stream)))

;; binary output

(defmethod stream-write-byte ((stream filter-binary-output-stream) byte)
  (check-stream-open stream 'stream-write-byte)
  (funcall (filter-stream-function stream)
           'write-byte
           (filter-stream-stream stream)
           byte)
  byte)



;;; sequence I/O

(defun check-sequence-arguments (direction stream sequence start end)
  (assert (<= 0 start end (length sequence))
          (sequence start end)
          "START = ~D or END = ~D are not sequence bounding indexes for a ~S of length ~D"
          start end (type-of sequence) (length sequence))
  (ecase direction
    (:read
     (assert (or (listp sequence)
                 (and (vectorp sequence)
                      (subtypep (filter-stream-element-type stream) (array-element-type sequence))))
             (sequence)
             "For reading, the sequence element type ~S should be a supertype of the stream element type ~S"
             (array-element-type sequence) (filter-stream-element-type stream)))
    (:write
     (assert (or (listp sequence)
                 (and (vectorp sequence)
                      (subtypep (array-element-type sequence) (filter-stream-element-type stream))))
             (sequence)
             "For writing, the sequence element type ~S should be a subtype of the stream element type ~S"
             (array-element-type sequence) (filter-stream-element-type stream)))))

(defun %stream-read-sequence (stream sequence start end)
  (check-sequence-arguments :read stream sequence start end)
  (funcall (filter-stream-function stream)
           'read-sequence
           (filter-stream-stream stream)
           sequence start end))

(defun %stream-write-sequence (stream sequence start end)
  (check-sequence-arguments :write stream sequence start end)
  (funcall (filter-stream-function stream)
           'write-sequence
           (filter-stream-stream stream)
           sequence start end)
  sequence)

(defmethod stream-read-sequence ((stream filter-character-input-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-read-sequence)
  (%stream-read-sequence stream sequence start end))

(defmethod stream-read-sequence ((stream filter-binary-input-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-read-sequence)
  (%stream-read-sequence stream sequence start end))

(defmethod stream-write-sequence ((stream filter-character-output-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-write-sequence)
  (%stream-write-sequence stream sequence start end))

(defmethod stream-write-sequence ((stream filter-binary-output-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-write-sequence)
  (%stream-write-sequence stream sequence start end))


(defun no-filter (operation stream &rest arguments)
  (ecase operation
    ;; character
    (read-char            (read-char         stream nil :eof))
    (read-char-no-hang    (read-char-no-hang stream nil :eof))
    (peek-char            (peek-char         nil stream nil :eof))
    (read-line            (read-line         stream nil :eof))
    (listen               (listen            stream))
    (unread-char          (unread-char       (first arguments) stream))
    (write-char           (write-char        (first arguments) stream))
    (write-string         (write-string      (first arguments) stream :start (second arguments) :end (third arguments)))
    ;; binary:
    (read-byte            (read-byte         stream nil :eof))
    (write-byte           (write-byte        (first arguments) stream))
    ;; both:
    (read-sequence        (read-sequence     (first arguments) stream :start (second arguments) :end (third arguments)))
    (write-sequence       (write-sequence    (first arguments) stream :start (second arguments) :end (third arguments)))
    (close                (close stream :abort (first arguments)))))

;;;; THE END ;;;;
