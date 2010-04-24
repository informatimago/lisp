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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
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

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.STREAM"
  (:USE "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.STRING")
  (:EXPORT "BVSTREAM-READ-BYTE" "BVSTREAM-WRITE-BYTE" "BVSTREAM-POSITION"
           "WITH-INPUT-FROM-BYTE-VECTOR" "WITH-OUTPUT-TO-BYTE-VECTOR"
           "CONTENTS-FROM-STREAM"
           "COPY-OVER" "COPY-STREAM" "STREAM-TO-STRING-LIST"
           "BARE-STREAM")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING" "SPLIT-STRING")
  (:DOCUMENTATION
   "This package exports utility functions about streams.

    Copyright Pascal J. Bourguignon 2003 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.STREAM")


  

(DEFUN STREAM-TO-STRING-LIST (STREAM)
  "
RETURN:  the list of lines collected from stream.
"
  (typecase stream
    (STREAM    (LOOP
                  :for LINE = (READ-LINE STREAM NIL NIL)
                  :WHILE LINE :COLLECT LINE))
    (string    (SPLIT-STRING STREAM (FORMAT NIL "~C" #\newline)))
    (otherwise NIL)))


(DEFUN COPY-STREAM (FROM TO)
  "Copy into TO from FROM until end of the input file.  Do not
translate or otherwise maul anything.
AUTHORS: Daniel Barlow, Xarch"
  (LET ((BUF (MAKE-ARRAY 4096 :ELEMENT-TYPE (STREAM-ELEMENT-TYPE FROM))))
    (DO ((POS (READ-SEQUENCE BUF FROM) (READ-SEQUENCE BUF FROM)))
        ((= 0 POS) NIL)
      (WRITE-SEQUENCE BUF TO :END POS))))



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
         (eltype (stream-ELEMENT-TYPE stream))
         (initel (if (subtypep eltype 'integer) 0 #\Space))
         (buffer (make-ARRAY busize 
                             :ELEMENT-TYPE eltype
                             :INITIAL-ELEMENT initel
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
  


(DEFUN COPY-OVER (STREAM FROM-POS TO-POS &key (element-type 'character))
  "
DO:         Copies elements from the FROM-POS to the end of the STREAM
            to the TO-POS.
POST:       (file-position stream) == (+ to-pos (- eof-pos from-ops))
NOTE:       The file is not truncated.
"
  (ASSERT (< TO-POS FROM-POS))
  (DO ((BUFFER (MAKE-ARRAY '(1048576) :ELEMENT-TYPE element-type))
       (EOF NIL)
       (LENGTH))
      (EOF)
    (FILE-POSITION STREAM FROM-POS)
    (SETF LENGTH (READ-SEQUENCE BUFFER STREAM))
    (SETF FROM-POS (FILE-POSITION STREAM))
    (IF (= LENGTH 0)
        (SETF EOF T)
        (PROGN
          (FILE-POSITION STREAM TO-POS)
          (WRITE-SEQUENCE BUFFER STREAM :START 0 :END LENGTH)
          (SETF TO-POS (FILE-POSITION STREAM))))))
  


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

(DEFGENERIC BVSTREAM-POSITION (SELF POSITION))
(DEFGENERIC BVSTREAM-WRITE-BYTE (SELF BYTE))
(DEFGENERIC BVSTREAM-READ-BYTE (SELF))


(DEFCLASS BVSTREAM-OUT ()
  ((BYTES :READER GET-BYTES
          :WRITER SET-BYTES
          :ACCESSOR BYTE-VECTOR
          :INITFORM (MAKE-ARRAY '(1024)
                                :ELEMENT-TYPE '(UNSIGNED-BYTE 8)
                                :ADJUSTABLE T
                                :FILL-POINTER 0)
          :INITARG :BYTES)))



(DEFMETHOD BVSTREAM-POSITION ((SELF BVSTREAM-OUT) POSITION)
  (IF POSITION
      (SETF (FILL-POINTER (BYTE-VECTOR SELF))
            (MIN (ARRAY-DIMENSION (BYTE-VECTOR SELF) 0) (MAX 0 POSITION)))
      (FILL-POINTER (BYTE-VECTOR SELF))))


(DEFMETHOD BVSTREAM-WRITE-BYTE ((SELF BVSTREAM-OUT) (BYTE INTEGER))
  (VECTOR-PUSH-EXTEND (LDB (BYTE 8 0) BYTE) 
                      (BYTE-VECTOR SELF)
                      (ARRAY-DIMENSION (BYTE-VECTOR SELF) 0)))


(DEFMACRO WITH-OUTPUT-TO-BYTE-VECTOR ((VAR &OPTIONAL BYTE-VECTOR-FORM 
                                           &KEY ELEMENT-TYPE) &BODY BODY)
  (declare (ignore element-type)) ;; TODO: Remove this parameter!
  `(LET ((,VAR (MAKE-INSTANCE 'BVSTREAM-OUT
                 ,@(WHEN BYTE-VECTOR-FORM `(:BYTES ,BYTE-VECTOR-FORM)))))
     (LET ((,VAR ,VAR)) ,@BODY)
     (GET-BYTES ,VAR)))


(DEFCLASS BVSTREAM-IN ()
  ((BYTES :READER GET-BYTES :WRITER SET-BYTES
          :ACCESSOR BYTE-VECTOR
          :INITARG :BYTES)
   (POSITION :READER GET-POSITION
             :ACCESSOR BIS-POSITION 
             :INITARG :POSITION :INITFORM 0)
   (END :INITARG :END :INITFORM NIL)))



(DEFMETHOD INITIALIZE-INSTANCE ((SELF BVSTREAM-IN) &REST ARGS)
  (DECLARE (IGNORE ARGS))
  (CALL-NEXT-METHOD)
  (LET ((LEN (LENGTH (BYTE-VECTOR SELF))))
    (SETF (SLOT-VALUE SELF 'END) (IF (SLOT-VALUE SELF 'END)
                                     (MIN (SLOT-VALUE SELF 'END) LEN) LEN)
          (BIS-POSITION SELF) (MAX 0 (MIN (BIS-POSITION SELF) LEN))))
  SELF)
                                                

(DEFMETHOD BVSTREAM-POSITION ((SELF BVSTREAM-IN) POSITION)
  (IF POSITION
      (SETF (BIS-POSITION SELF) (MIN (BIS-POSITION SELF) (MAX 0 POSITION)))
      (BIS-POSITION SELF)))


(DEFMETHOD BVSTREAM-READ-BYTE ((SELF BVSTREAM-IN))
  (IF (< (BIS-POSITION SELF) (SLOT-VALUE SELF 'END))
      (PROG1 (AREF (GET-BYTES SELF) (BIS-POSITION SELF))
        (INCF (BIS-POSITION SELF)))
      :EOF))


(DEFMACRO WITH-INPUT-FROM-BYTE-VECTOR ((VAR BYTE-VECTOR &KEY INDEX START END)
                                       &BODY BODY)
  `(LET ((,VAR (MAKE-INSTANCE 'BVSTREAM-IN :BYTES ,BYTE-VECTOR
                              ,@(WHEN START `((:POSITION ,START)))
                              ,@(WHEN END   `((:END ,END))))))
     (LET ((,VAR ,VAR)) ,@BODY)
     ,(WHEN INDEX `(SETF ,INDEX (GET-POSITION ,VAR)))
     (GET-POSITION ,VAR)))


;;;; stream.lisp                      --                     --          ;;;;
