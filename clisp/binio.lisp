;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               binio.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports clisp specific binary stream functions, including:
;;;;    - reading and writing encoded text from/to binary streams.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-09-03 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.CLISP.BINIO"
  ;;(:NICKNAMES "BINIO")
  (:documentation "
   This package exports clisp specific, binary I/O functions, including:
   - reading and writing encoded text from/to binary streams.
   - reading and writing byte from/to text streams.

    Copyright Pascal J. Bourguignon 2005 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:use "COMMON-LISP"
        "EXT" "SOCKET"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048")
  (:shadow
   "READ-BYTE"  "WRITE-BYTE" "PEEK-CHAR" "READ-CHAR" "READ-CHAR-NO-HANG"
   "TERPRI" "UNREAD-CHAR" "WRITE-CHAR" "READ-LINE" "WRITE-STRING"
   "READ-SEQUENCE" "WRITE-SEQUENCE" "LISTEN"  "Y-OR-N-P" "YES-OR-NO-P"
   "FORMAT")
  (:export
   "READ-BYTE"  "WRITE-BYTE" "PEEK-CHAR" "READ-CHAR" "READ-CHAR-NO-HANG"
   "TERPRI" "UNREAD-CHAR" "WRITE-CHAR" "READ-LINE" "WRITE-STRING"
   "READ-SEQUENCE" "WRITE-SEQUENCE" "LISTEN"  "Y-OR-N-P" "YES-OR-NO-P"
   "FORMAT"))
(in-package  "COM.INFORMATIMAGO.CLISP.BINIO")



;; (defun convert-string-to-bytes (string encoding)
;;   (loop with result = '()
;;         with start = 0
;;         for cr = (position (code-char 13) string :start start)
;;         for lf = (position (code-char 10) string :start start)
;;         while (or cr lf)
;;         do (let ((end (if (and cr lf) (min cr lf) (or cr lf))))
;;               (print (list start cr lf end))
;;              (push (ext:convert-string-to-bytes
;;                     string encoding :start start :end end) result)
;;              (push (list (char-code (aref string end))) result)
;;              (setf start (1+ end)))
;;         finally (return (apply (function concatenate)
;;                                'vector (nreverse result)))))


(defun eoln-bytes (stream)
(ecase (ext:encoding-line-terminator
        (stream-external-format stream))
  (:unix #.(vector lf))
  (:mac  #.(vector cr))
  (:dos  #.(vector cr lf))))

;;----------------------------------------------------------------------
;;

(defun listen (&optional (stream *input-stream*))
  ;; TODO: write a better version comparison function
#+#.(cl:when (cl:string<=
              (cl:lisp-implementation-version)  "2.35"
              :end1 (cl:min (cl:length (cl:lisp-implementation-version)) 4))
      :clisp)
(if (socket:socketp stream)
    (case (socket:socket-status stream 0)
      ((:input :io) t)
      (otherwise    nil))
    (common-lisp:listen stream))
#-#.(cl:when (cl:string<=
              (cl:lisp-implementation-version)  "2.35"
              :end1 (cl:min (cl:length (cl:lisp-implementation-version)) 4))
      :clisp)
(common-lisp:listen stream))



;;----------------------------------------------------------------------
;; Binary I/O: we convert to character when it's a character stream.

(defun read-byte (stream &optional (eof-error-p t) (eof-value nil))
(if (subtypep (stream-element-type stream) 'character)
    (progn
      )
    (common-lisp:read-byte stream eof-error-p eof-value)))


(defun write-byte (byte stream)
(if (subtypep (stream-element-type stream) 'character)
    (progn
      )
    (common-lisp:write-byte byte stream)))


(defun read-sequence (sequence stream &key (start 0) (end nil))
(if (subtypep (stream-element-type stream) 'character)
    (progn
      )
    (common-lisp:read-sequence sequence stream :start start :end end)))


(defun write-sequence (sequence stream &key (start 0) (end nil))
(if (subtypep (stream-element-type stream) 'character)
    (progn
      )
    (common-lisp:write-sequence sequence stream :start start :end end)))


;;----------------------------------------------------------------------
;; Text I/O: we convert to bytes when it's a binary stream.

(defvar *peek-bytes* '()
"A list of weak mapping streams to peek bytes")

(defun clean-peek-bytes ()
"Flush the *peek-bytes* list of dead associations."
(setf *peek-bytes* (delete-if (complement (function ext:weak-mapping-pair))
                              *peek-bytes*)))

(defun get-peek-byte (stream)
"Return the byte associated to the stream, or NIL."
(loop initialy (clean-peek-bytes)
   for wmap in *peek-bytes*
   do (multiple-value-bind (k v p) (ext:weak-mapping-pair wmap)
        (when (and p (eq k stream))
          (setf *peek-bytes* (delete wmap *peek-bytes*))
          (return-from get-peek-byte v)))
   finally (return nil)))

(defun set-peek-byte (stream byte)
"Associate the byte to the stream, and return the byte."
(loop initialy (clean-peek-bytes)
   for wmap in *peek-bytes*
   do (multiple-value-bind (k v p) (ext:weak-mapping-pair wmap)
        (when (and p (eq k stream))
          (setf (ext:weak-mapping-value wmap) byte)
          (return-from get-peek-byte byte)))
   finally (progn (push (ext:weak-mapping stream byte) *peek-bytes*)
                  (return byte))))


(defun peek-char (&optional (peek-type nil) (input-stream *standard-input*)
                  (eof-error-p t) (eof-value nil) (recursive-p nil))
  (if (subtypep (stream-element-type stream) 'character)
      (common-lisp:peek-char peek-type input-stream
                             eof-error-p eof-value recursive-p)
      (or (get-peek-byte stream)
          (progn
            ))))

(defun read-char ()
  )

(defun read-char-no-hang ()
  )

(defun unread-char ()
  )



(defun terpri (&optional (stream *standard-output*))
  (if (subtypep (stream-element-type stream) 'character)
      (common-lisp:terpri stream)
      (progn (write-sequence (eoln-bytes stream) stream)
             nil)))

(defun fresh-line (&optional (stream *standard-output*))
  (if (subtypep (stream-element-type stream) 'character)
      (common-lisp:fresh-line stream)
      (progn (write-sequence (eoln-bytes stream) stream)
             t)))


(defun read-line ()
  )

(defun read-line-in-buffer (stream buffer)
  "Read byte sequence into the buffer, adjusting the fill-pointer.
   When CR-LF is read, return the position of the eoln
   If the buffer becomes full before the CR-LF is read, then signal an error."
  ;; KEEP Synchronized with RECYCLE-LINE-BUFFER
  (loop
     while (binary-stream-listen stream)
     do (vector-push (read-byte stream) buffer))
  (let ((eoln (search #(13 10) buffer)))
    ;; (print `(:eoln ,eoln :buffer ,buffer))
    (cond
      (eoln (decf (fill-pointer buffer) 2))
      ((<= (array-dimension buffer 0) (fill-pointer buffer)) ; no eoln
       (error "ident server sent a line too long."))
      (t nil))))



(defun read-line (&optional (stream *standard-input*)
                  (eof-error-p t) (eof-value nil) (recursive-p nil))
  (if (subtypep (stream-element-type stream) 'character)
      (read-line stream eof-error-p eof-value recursive)
      (let ()

        )))

(defun read-line (&optional (input-stream *standard-input*) (eof-error-p t)
                  (eof-value nil) (recursive-p nil))
  "
NEWLINE:  nil   <=> accepts any CR, LF, or CRLF as a new-line.
          :CR   <=> accepts only CR as new-line.
          :LF   <=> accepts only LF as new-line.
          :CRLF <=> accepts only CRLF as new-line.
"
  ;; => line, missing-newline-p
  (setf input-stream (or input-stream *standard-input*))
  (assert (member newline '(nil :cr :crlf :lf)))
  (if (and input-stream (streamp input-stream)
           (equal (stream-element-type input-stream) '(unsigned-byte 8)))
      (flet ((result
                 (buffer missing-newline-p)
               (cond
                 ((and missing-newline-p eof-error-p)
                  (error (make-condition
                          'system::simple-end-of-file
                          :stream input-stream
                          :format-control "~S: input stream ~S has reached its end"
                          :format-arguments (list 'read input-stream))))
                 ((and missing-newline-p (zerop (length buffer)))
                  (values eof-value missing-newline-p))
                 (t
                  (values (ext:convert-string-from-bytes
                           buffer custom:*default-file-encoding*)
                          missing-newline-p)))))
        (loop with buffer = (make-array '(4094) :element-type '(unsigned-byte 8)
                                        :initial-element 0
                                        :adjustable t
                                        :fill-pointer 0)
           with eof = (gensym)
           for byte = (read-byte input-stream nil eof)
           do (if (eq eof byte)
                  (return (result buffer t))
                  (case newline
                    ((:cr)
                     ;; BUG: we cannot peek a binary stream, so we take CR-LF as
                     ;;      a newline followed by a LF. We would have to buffer
                     ;;      the stream.
                     (if (= 13 byte)
                         (return (result buffer nil))
                         (vector-push-extend byte buffer)))
                    ((:crlf)
                     (if (= 13 byte)
                         (let ((byte (read-byte input-stream nil eof)))
                           (cond
                             ((eq eof byte)
                              (return (result buffer t)))
                             ((= 10 byte)
                              (return (result buffer nil)))
                             (t
                              (vector-push-extend 13 buffer)
                              (vector-push-extend byte buffer))))
                         (vector-push-extend byte buffer)))
                    ((:lf)
                     (if (= 10 byte)
                         (return (result buffer nil))
                         (vector-push-extend byte buffer)))
                    ((nil)
                     ;; BUG: we cannot peek a binary stream, so we take CR-LF as
                     ;;      a newline followed by a LF. We would have to buffer
                     ;;      the stream.
                     (if (or (= 10 byte) (= 13 byte))
                         (return (result buffer nil))
                         (vector-push-extend byte buffer)))))))
      (common-lisp:read-line input-stream eof-error-p eof-value recursive-p)))





(defun write-char ()
  )

(defun write-string (string &optional (output-stream *standard-output*)
                     &key (start 0) (end nil))
  )

(defun write-line (string &optional (output-stream *standard-output*)
                   &key (start 0) (end nil))
  )


(defun y-or-n-p (&optional control &rest arguments)
  )

(defun yes-or-no-p (&optional control &rest arguments)
  )

(defun write (object &key array base case circle escape gensym length level lines miser-width pprint-dispatch pretty radix readably right-margin stream)
  )

(defun prin1  (object &optional (output-stream *standard-output*))
  )

(defun princ  (object &optional (output-stream *standard-output*))
  )

(defun print  (object &optional (output-stream *standard-output*))
  )

(defun pprint (object &optional (output-stream *standard-output*))
  )
       

(defun format (dest ctrl &rest args)
  (when (eq t dest)
    (setf dest *standard-output*))
  (if (or (null dest)
          (not (streamp dest))
          (subtypep (stream-element-type dest) 'character))
      (apply (function common-lisp:format) dest ctrl args)
      (prog1 nil
        (write-sequence (ext:convert-string-to-bytes
                         (apply (function common-lisp:format) nil ctrl args)
                         (stream-external-format dest))
                        dest))))



(in-package "COMMON-LISP-USER")

(setf custom:*default-file-encoding* (ext:make-encoding
                                      :charset charset:iso-8859-1
                                      :line-terminator :unix))

(let ((cr   (list (code-char 13)))
      (lf   (list (code-char 10)))
      (crlf (list (code-char 13)  (code-char 10))))
  (dolist (nl (list cr lf crlf))
    (let ((p (format nil "(~D) = ~{~C~} x" (length nl) nl)))
      ;; (print (list (length nl) (length p)))
      (print (ext:convert-string-to-bytes p custom:*default-file-encoding*))))
  (dolist (test (list (list cr lf crlf)
                      (list cr crlf lf)
                      (list lf crlf cr)))
    (print (ext:convert-string-to-bytes
            (apply (function format) nil
                   "line1:field1~{~C~}field2~{~C~}field3~{~C~}line1:field1~{~C~}field2~{~C~}field3~{~C~}"
                   (append test test)) custom:*default-file-encoding* ))))



(defparameter +ascii+
  #(
    nul soh stx etx eot enq ack bel bs tab lf vt ff cr so si 
    dle dc1 dc2 dc3 dc4 nak syn etb can em sub esc fs gs rs us 
    sp nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil del 
    pad hop bph nbh ind nel ssa esa hts htj vts pld plu ri ss2 ss3 
    dcs pu1 pu2 sts cch mw spa epa sos sgci sci csi st osc pm apc))

(defun show-all (string &optional (out t))
  (loop for ch across string
     do (show-char ch out)))

(defun show-char (ch &optional (out t))
  (if (or (<= (length +ascii+) (char-code ch))
          (null (aref +ascii+  (char-code ch))))
      (princ ch out)
      (format out "<~A>"  (aref +ascii+  (char-code ch)))))


(let ((cr   (list (code-char 13)))
      (lf   (list (code-char 10)))
      (crlf (list (code-char 13)  (code-char 10))))
  (dolist (test (list (list cr lf crlf)
                      (list lf cr crlf)
                      (list cr crlf lf)
                      (list lf crlf cr)
                      (list crlf cr lf)
                      (list crlf lf cr)))
    (format t "~2%")
    (with-open-file (out "test.txt" :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-does-not-exist :create :if-exists :supersede)
      (apply (function byteio:format) out "line1:field1~{~C~}field2~{~C~}field3~{~C~}line1:field1~{~C~}field2~{~C~}field3~{~C~}"  (append test test)))
    (with-open-file (in "test.txt" :direction :input 
                        :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte in nil nil)
         while byte do (show-char (code-char byte))
         finally (format t "~%")))
    (dolist (newline '(nil :cr :lf :crlf))
      (with-open-file (in "test.txt" :direction :input
                          :element-type '(unsigned-byte 8))
        (format t "reading with ~6A as newline: ~%" newline)
        (loop for line =  (byteio:read-line in nil nil nil newline)
           for num from 1
           while line do
           (format t "  line #~2D :  ~3D byte~:[ ~;s~]: ~A~%"
                   num
                   (length line)
                   (< 1 (length line))
                   (with-output-to-string (s) (show-all line s))))))))


(with-open-file (out "test.bin" :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :create :if-exists :supersede)
  (byteio:format out "-~C-~C-~C~C-" #\return #\linefeed #\return #\linefeed))

(with-open-file (out "test.txt" :direction :output
                     :element-type 'character
                     :external-format (ext:make-encoding
                                       :charset charset:iso-8859-1
                                       :line-terminator :dos)
                     :if-does-not-exist :create :if-exists :supersede)
  (format out "-~C-~C-~C~C-" #\return #\linefeed #\return #\linefeed))


(defun dump-chars (file)
  (with-open-file (in file :direction :input
                      :element-type '(unsigned-byte 8))
    (let ((buffer (make-array '(4000) :fill-pointer t
                              :element-type  '(unsigned-byte 8)
                              :initial-element 0)))
      (setf (fill-pointer buffer) (read-sequence buffer in))
      (show-all (map 'string (function code-char) buffer)))))
(terpri)(dump-chars "test.txt")
(terpri)(dump-chars "test.bin")
(terpri)


#||
(load"clisp-bin-read.lisp")
;; Loading file clisp-bin-read.lisp ...
#(40 49 41 32 61 32 13 32 120) 
#(40 49 41 32 61 32 10 32 120) 
#(40 50 41 32 61 32 13 10 32 120) 
#(108 105 110 101 49 58 102 105 101 108 100 49 13 102 105 101 108 100 50 10 102
  105 101 108 100 51 13 10 108 105 110 101 49 58 102 105 101 108 100 49 13 102
  105 101 108 100 50 10 102 105 101 108 100 51 13 10) 
#(108 105 110 101 49 58 102 105 101 108 100 49 13 102 105 101 108 100 50 13 10
  102 105 101 108 100 51 10 108 105 110 101 49 58 102 105 101 108 100 49 13 102
  105 101 108 100 50 13 10 102 105 101 108 100 51 10) 
#(108 105 110 101 49 58 102 105 101 108 100 49 10 102 105 101 108 100 50 13 10
  102 105 101 108 100 51 13 108 105 110 101 49 58 102 105 101 108 100 49 10 102
  105 101 108 100 50 13 10 102 105 101 108 100 51 13) 

line1:field1<CR>field2<LF>field3<CR><LF>line1:field1<CR>field2<LF>field3<CR><LF>
reading with NIL    as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :    6 bytes: field2
line # 3 :    6 bytes: field3
line # 4 :    0 byte : 
line # 5 :   12 bytes: line1:field1
line # 6 :    6 bytes: field2
line # 7 :    6 bytes: field3
line # 8 :    0 byte : 
reading with CR     as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :   13 bytes: field2<LF>field3
line # 3 :   13 bytes: <LF>line1:field1
line # 4 :   13 bytes: field2<LF>field3
line # 5 :    1 byte : <LF>
reading with LF     as newline: 
line # 1 :   19 bytes: line1:field1<CR>field2
line # 2 :    7 bytes: field3<CR>
line # 3 :   19 bytes: line1:field1<CR>field2
line # 4 :    7 bytes: field3<CR>
reading with CRLF   as newline: 
line # 1 :   26 bytes: line1:field1<CR>field2<LF>field3
line # 2 :   26 bytes: line1:field1<CR>field2<LF>field3


line1:field1<LF>field2<CR>field3<CR><LF>line1:field1<LF>field2<CR>field3<CR><LF>
reading with NIL    as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :    6 bytes: field2
line # 3 :    6 bytes: field3
line # 4 :    0 byte : 
line # 5 :   12 bytes: line1:field1
line # 6 :    6 bytes: field2
line # 7 :    6 bytes: field3
line # 8 :    0 byte : 
reading with CR     as newline: 
line # 1 :   19 bytes: line1:field1<LF>field2
line # 2 :    6 bytes: field3
line # 3 :   20 bytes: <LF>line1:field1<LF>field2
line # 4 :    6 bytes: field3
line # 5 :    1 byte : <LF>
reading with LF     as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :   14 bytes: field2<CR>field3<CR>
line # 3 :   12 bytes: line1:field1
line # 4 :   14 bytes: field2<CR>field3<CR>
reading with CRLF   as newline: 
line # 1 :   26 bytes: line1:field1<LF>field2<CR>field3
line # 2 :   26 bytes: line1:field1<LF>field2<CR>field3


line1:field1<CR>field2<CR><LF>field3<LF>line1:field1<CR>field2<CR><LF>field3<LF>
reading with NIL    as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :    6 bytes: field2
line # 3 :    0 byte : 
line # 4 :    6 bytes: field3
line # 5 :   12 bytes: line1:field1
line # 6 :    6 bytes: field2
line # 7 :    0 byte : 
line # 8 :    6 bytes: field3
reading with CR     as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :    6 bytes: field2
line # 3 :   20 bytes: <LF>field3<LF>line1:field1
line # 4 :    6 bytes: field2
line # 5 :    8 bytes: <LF>field3<LF>
reading with LF     as newline: 
line # 1 :   20 bytes: line1:field1<CR>field2<CR>
line # 2 :    6 bytes: field3
line # 3 :   20 bytes: line1:field1<CR>field2<CR>
line # 4 :    6 bytes: field3
reading with CRLF   as newline: 
line # 1 :   19 bytes: line1:field1<CR>field2
line # 2 :   26 bytes: field3<LF>line1:field1<CR>field2
line # 3 :    7 bytes: field3<LF>


line1:field1<LF>field2<CR><LF>field3<CR>line1:field1<LF>field2<CR><LF>field3<CR>
reading with NIL    as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :    6 bytes: field2
line # 3 :    0 byte : 
line # 4 :    6 bytes: field3
line # 5 :   12 bytes: line1:field1
line # 6 :    6 bytes: field2
line # 7 :    0 byte : 
line # 8 :    6 bytes: field3
reading with CR     as newline: 
line # 1 :   19 bytes: line1:field1<LF>field2
line # 2 :    7 bytes: <LF>field3
line # 3 :   19 bytes: line1:field1<LF>field2
line # 4 :    7 bytes: <LF>field3
reading with LF     as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :    7 bytes: field2<CR>
line # 3 :   19 bytes: field3<CR>line1:field1
line # 4 :    7 bytes: field2<CR>
line # 5 :    7 bytes: field3<CR>
reading with CRLF   as newline: 
line # 1 :   19 bytes: line1:field1<LF>field2
line # 2 :   26 bytes: field3<CR>line1:field1<LF>field2
line # 3 :    6 bytes: field3


line1:field1<CR><LF>field2<CR>field3<LF>line1:field1<CR><LF>field2<CR>field3<LF>
reading with NIL    as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :    0 byte : 
line # 3 :    6 bytes: field2
line # 4 :    6 bytes: field3
line # 5 :   12 bytes: line1:field1
line # 6 :    0 byte : 
line # 7 :    6 bytes: field2
line # 8 :    6 bytes: field3
reading with CR     as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :    7 bytes: <LF>field2
line # 3 :   19 bytes: field3<LF>line1:field1
line # 4 :    7 bytes: <LF>field2
line # 5 :    7 bytes: field3<LF>
reading with LF     as newline: 
line # 1 :   13 bytes: line1:field1<CR>
line # 2 :   13 bytes: field2<CR>field3
line # 3 :   13 bytes: line1:field1<CR>
line # 4 :   13 bytes: field2<CR>field3
reading with CRLF   as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :   26 bytes: field2<CR>field3<LF>line1:field1
line # 3 :   14 bytes: field2<CR>field3<LF>


line1:field1<CR><LF>field2<LF>field3<CR>line1:field1<CR><LF>field2<LF>field3<CR>
reading with NIL    as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :    0 byte : 
line # 3 :    6 bytes: field2
line # 4 :    6 bytes: field3
line # 5 :   12 bytes: line1:field1
line # 6 :    0 byte : 
line # 7 :    6 bytes: field2
line # 8 :    6 bytes: field3
reading with CR     as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :   14 bytes: <LF>field2<LF>field3
line # 3 :   12 bytes: line1:field1
line # 4 :   14 bytes: <LF>field2<LF>field3
reading with LF     as newline: 
line # 1 :   13 bytes: line1:field1<CR>
line # 2 :    6 bytes: field2
line # 3 :   20 bytes: field3<CR>line1:field1<CR>
line # 4 :    6 bytes: field2
line # 5 :    7 bytes: field3<CR>
reading with CRLF   as newline: 
line # 1 :   12 bytes: line1:field1
line # 2 :   26 bytes: field2<LF>field3<CR>line1:field1
line # 3 :   13 bytes: field2<LF>field3

-<CR>-<CR><LF>-<CR><CR><LF>-
-<CR>-<LF>-<CR><LF>-
;; Loaded file clisp-bin-read.lisp
T
[299]>
||#

;;;; THE END ;;;;
