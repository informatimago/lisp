;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports some stream utility functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-07-07 <PJB> Added CONTENTS-FROM-STREAM.
;;;;    2005-03-17 <PJB> Added COPY-OVER.
;;;;    2004-09-12 <PJB> Removed use of GRAY streams,
;;;;                     exported BVSTREAM-READ-BYTE and BVSTREAM-WRITE-BYTE.
;;;;    2004-08-18 <PJB> Added WITH-OUTPUT-TO-BYTE-VECTOR
;;;;                       and WITH-INPUT-FROM-BYTE-VECTOR.
;;;;    2004-02-28 <PJB> Extracted from utility to avoid circle
;;;;                     in package dependencies.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
  (:use "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "BVSTREAM-READ-BYTE" "BVSTREAM-WRITE-BYTE" "BVSTREAM-POSITION"
           "WITH-INPUT-FROM-BYTE-VECTOR" "WITH-OUTPUT-TO-BYTE-VECTOR"
           "CONTENTS-FROM-STREAM"
           "COPY-OVER" "COPY-STREAM" "STREAM-TO-STRING-LIST"
           "STREAM-INPUT-STREAM" "STREAM-OUTPUT-STREAM"
           "BARE-STREAM")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING" "SPLIT-STRING")
  (:documentation
   "This package exports utility functions about streams.

    Copyright Pascal J. Bourguignon 2003 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM")


  

(defun stream-to-string-list (stream)
  "
RETURN:  the list of lines collected from stream.
"
  (typecase stream
    (stream    (loop
                  :for line = (read-line stream nil nil)
                  :while line :collect line))
    (string    (split-string stream (format nil "~C" #\newline)))
    (otherwise nil)))


(defun copy-stream (from to)
  "Copy into TO from FROM until end of the input file.  Do not
translate or otherwise maul anything.
AUTHORS: Daniel Barlow, Xarch"
  (let ((buf (make-array 4096 :element-type (stream-element-type from))))
    (do ((pos (read-sequence buf from) (read-sequence buf from)))
        ((= 0 pos) nil)
      (write-sequence buf to :end pos))))



(defun contents-from-stream (stream &key length (min-size 256) max-extend)
  "
STREAM:     May be a binary or character, file or non-file stream.
LENGTH:     NIL, or the number of stream elements to read.
MIN-SIZE:   Minimum pre-allocated buffer size. If LENGTH is given, or STREAM
            has a FILE-LENGTH, then the MIN-SIZE is ignored.
MAX-EXTEND: NIL ==> double the buffer size, or double the buffer size until
            it's greater than MAX-EXTEND, and then increment by MAX-EXTEND.
RETURN:     A vector containing the elements read from the STREAM.
"
  (let* ((busize (or length (ignore-errors (file-length stream)) min-size))
         (eltype (stream-element-type stream))
         (initel (if (subtypep eltype 'integer) 0 #\space))
         (buffer (make-array busize 
                             :element-type eltype
                             :initial-element initel
                             :adjustable t :fill-pointer t))
         (start 0))
    (loop
       (let ((end (read-sequence buffer stream :start start)))
         (when (or (< end busize) (and length (= length end)))
           ;; we got eof, or have read enough
           (setf (fill-pointer buffer) end)
           (return-from contents-from-stream buffer))
         ;; no eof; extend the buffer
         (setf busize
               (if (or (null max-extend) (<= (* 2 busize) max-extend))
                   (* 2 busize)
                   (+ busize max-extend))
               start end))
       (adjust-array buffer busize :initial-element initel :fill-pointer t))))
  


(defun copy-over (stream from-pos to-pos &key (element-type 'character))
  "
DO:         Copies elements from the FROM-POS to the end of the STREAM
            to the TO-POS.
POST:       (file-position stream) == (+ to-pos (- eof-pos from-ops))
NOTE:       The file is not truncated.
"
  (assert (< to-pos from-pos))
  (do ((buffer (make-array '(1048576) :element-type element-type))
       (eof nil)
       (length))
      (eof)
    (file-position stream from-pos)
    (setf length (read-sequence buffer stream))
    (setf from-pos (file-position stream))
    (if (= length 0)
        (setf eof t)
        (progn
          (file-position stream to-pos)
          (write-sequence buffer stream :start 0 :end length)
          (setf to-pos (file-position stream))))))




(defgeneric stream-input-stream (stream)
  (:documentation "RETURN: A simple INPUT-STREAM.")
  (:method ((stream stream))
    stream)
  (:method ((stream concatenated-stream))
    (stream-input-stream (first (concatenated-stream-streams stream))))
  (:method ((stream echo-stream))
    (stream-input-stream (echo-stream-input-stream stream)))
  (:method ((stream synonym-stream))
    (stream-input-stream (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream))
    (stream-input-stream (two-way-stream-input-stream stream))))


(defgeneric stream-output-stream (stream)
  (:documentation "RETURN: A simple OUTPUT-STREAM.")
  (:method ((stream stream))
    stream)
  (:method ((stream broadcast-stream))
    (stream-output-stream (first (broadcast-stream-streams stream))))
  (:method ((stream echo-stream))
    (stream-input-stream (echo-stream-output-stream stream)))
  (:method ((stream synonym-stream))
    (stream-input-stream (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream))
    (stream-input-stream (two-way-stream-output-stream stream))))


;; (defun stream-input-stream (stream)
;;   "
;; RETURN: An input-stream.
;; "
;;   (etypecase stream
;;     (two-way-stream      (two-way-stream-input-stream stream))
;;     (echo-stream         (echo-stream-input-stream  stream))
;;     (synonym-stream      (stream-input-stream
;;                           (symbol-value (synonym-stream-symbol stream))))
;;     (concatenated-stream (stream-input-stream
;;                           (first (concatenated-stream-streams stream))))
;;     (t
;;      (if (input-stream-p stream)
;;          stream
;;          (error "Stream ~S is not an input-stream." stream)))))
;; 
;; 
;; (defun stream-output-stream (stream)
;;   "
;; RETURN: An output-stream.
;; "
;;   (etypecase stream
;;     (two-way-stream  (two-way-stream-output-stream stream))
;;     (echo-stream     (echo-stream-output-stream  stream))
;;     (synonym-stream  (stream-output-stream
;;                       (symbol-value (synonym-stream-symbol stream))))
;;     (t
;;      (if (output-stream-p stream)
;;          stream
;;          (error "Stream ~S is not an output-stream." stream)))))


(defun bare-stream (stream &key (direction :output))
  "
RETURN: A stream or a list of streams that are not compound streams
        (and therefore usable by #+clisp SOCKET:SOCKET-STATUS).
"
  (etypecase stream
    (two-way-stream
     (ecase direction
       (:output (bare-stream (two-way-stream-output-stream stream)
                             :direction direction))
       (:input  (bare-stream (two-way-stream-input-stream stream)
                             :direction direction))))
    (echo-stream
     (ecase direction
       (:output (bare-stream (echo-stream-output-stream stream)
                             :direction direction))
       (:input  (bare-stream (echo-stream-input-stream  stream)
                             :direction direction))))
    (synonym-stream
     (bare-stream (symbol-value (synonym-stream-symbol stream))
                             :direction direction))
    (broadcast-stream
     (remove-if-not
      (lambda (stream)
        (ecase direction
          (:output (output-stream-p stream))
          (:input  (input-stream-p  stream))))
      (mapcar (lambda (stream) (bare-stream stream :direction direction))
              (broadcast-stream-streams stream))))
    (stream stream)))



;;----------------------------------------------------------------------

(defgeneric bvstream-position (self position))
(defgeneric bvstream-write-byte (self byte))
(defgeneric bvstream-read-byte (self))


(defclass bvstream-out ()
  ((bytes :reader get-bytes
          :writer set-bytes
          :accessor byte-vector
          :initform (make-array '(1024)
                                :element-type '(unsigned-byte 8)
                                :adjustable t
                                :fill-pointer 0)
          :initarg :bytes)))



(defmethod bvstream-position ((self bvstream-out) position)
  (if position
      (setf (fill-pointer (byte-vector self))
            (min (array-dimension (byte-vector self) 0) (max 0 position)))
      (fill-pointer (byte-vector self))))


(defmethod bvstream-write-byte ((self bvstream-out) (byte integer))
  (vector-push-extend (ldb (byte 8 0) byte) 
                      (byte-vector self)
                      (array-dimension (byte-vector self) 0)))


(defmacro with-output-to-byte-vector ((var &optional byte-vector-form 
                                           &key element-type) &body body)
  (declare (ignore element-type)) ;; TODO: Remove this parameter!
  `(let ((,var (make-instance 'bvstream-out
                 ,@(when byte-vector-form `(:bytes ,byte-vector-form)))))
     (let ((,var ,var)) ,@body)
     (get-bytes ,var)))


(defclass bvstream-in ()
  ((bytes :reader get-bytes :writer set-bytes
          :accessor byte-vector
          :initarg :bytes)
   (position :reader get-position
             :accessor bis-position 
             :initarg :position :initform 0)
   (end :initarg :end :initform nil)))



(defmethod initialize-instance ((self bvstream-in) &rest args)
  (declare (ignore args))
  (call-next-method)
  (let ((len (length (byte-vector self))))
    (setf (slot-value self 'end) (if (slot-value self 'end)
                                     (min (slot-value self 'end) len) len)
          (bis-position self) (max 0 (min (bis-position self) len))))
  self)
                                                

(defmethod bvstream-position ((self bvstream-in) position)
  (if position
      (setf (bis-position self) (min (bis-position self) (max 0 position)))
      (bis-position self)))


(defmethod bvstream-read-byte ((self bvstream-in))
  (if (< (bis-position self) (slot-value self 'end))
      (prog1 (aref (get-bytes self) (bis-position self))
        (incf (bis-position self)))
      :eof))


(defmacro with-input-from-byte-vector ((var byte-vector &key index start end)
                                       &body body)
  `(let ((,var (make-instance 'bvstream-in :bytes ,byte-vector
                              ,@(when start `((:position ,start)))
                              ,@(when end   `((:end ,end))))))
     (let ((,var ,var)) ,@body)
     ,(when index `(setf ,index (get-position ,var)))
     (get-position ,var)))


;;;; stream.lisp                      --                     --          ;;;;
