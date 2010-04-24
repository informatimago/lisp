;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               fifo-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A fifo stream: all input is buffered and restituted as output.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-09-11 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
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

(defpackage  "COM.INFORMATIMAGO.CLISP.FIFO-STREAM"
  (:use "COMMON-LISP" "GRAY")
  (:export "FIFO-STREAM")
  (:DOCUMENTATION
   "
    A fifo stream: all input is buffered and restituted as output.
    
    Copyright Pascal J. Bourguignon 2003 - 2003
"))
(IN-PACKAGE "COM.INFORMATIMAGO.CLISP.FIFO-STREAM")


(DEFUN MAKE-FIFO ()
  ;; (VALUES INPUT-STREAM OUTPUT-STREAM)
  (values nil nil))



(DEFCONSTANT +CHUNK-SIZE+ 4096 "Size of one buffer string.")

(DEFCLASS FIFO-STREAM (FUNDAMENTAL-INPUT-STREAM
                       FUNDAMENTAL-OUTPUT-STREAM
                       FUNDAMENTAL-CHARACTER-STREAM)
  ((OUTPUT
    :INITform '()
    :initarg :OUTPUT
    :accessor output
    :TYPE     LIST
    :DOCUMENTATION "The head of the FIFO buffer list.")
   (INPUT
     :initform '()
     :initarg :INPUT
     :accessor input
     :TYPE     LIST
     :DOCUMENTATION "The tail of the FIFO buffer list.")
   )
  (:DOCUMENTATION
   "A fifo stream: all input is buffered and restituted as output.")
  )


;;; (string start end closed)
;;; 
;;; (and (<= 0 output)
;;;      (<= output input)
;;;      (<= 0 input)
;;;      (<= input (length string)))
;;; 
;;; closed <=> don't add input, see next buffer
;;;
;;; We could use the fill-pointer for the input.

(DEFUN MAKE-BUFFER ()
  (LIST (MAKE-STRING +CHUNK-SIZE+) 0 0 NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;; general generic functions defined on streams
;;; 

(DEFMETHOD CLOSE ((STREAM FIFO-STREAM) &KEY ABORT)
  "
Closes the stream and flushes any associated buffers.
"
  (declare (ignore abort))
  ;; When you define a primary method on this function, do not forget to
  ;; CALL-NEXT-METHOD.
  ;; TODO: (SETF (BUFFERS STREAM) 'NIL)
  (CALL-NEXT-METHOD))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; generic functions for character input
;;;


(defmethod STREAM-READ-CHAR ((STREAM FIFO-STREAM))
  "
If a character was pushed back using DEFUN STREAM-UNREAD-CHAR, returns
and consumes it. Otherwise returns and consumes the next character
from the stream. Returns :EOF if the end-of-stream is reached.
"
  (LET ((BUFFER (CAR (OUTPUT STREAM))))
    (IF BUFFER
        (LET* ((STR (POP BUFFER))
               (OUT (POP BUFFER))
               (INP (POP BUFFER))
               (CLOSED (POP BUFFER)))
          (COND
            ((< OUT INP)
             (INCF (SECOND BUFFER))
             (CHAR STR OUT))
            (CLOSED
             :EOF)
            (T
             (POP (OUTPUT STREAM))
             (STREAM-READ-CHAR STREAM))))
        :EOF)))


(defmethod STREAM-UNREAD-CHAR ((STREAM FIFO-STREAM) (CHAR CHARACTER))
  "
Pushes char, which must be the last character read from the stream,
back onto the front of the stream.
"
  (SETF (OUTPUT STREAM) (CONS (LIST (MAKE-STRING 1 :INITIAL-ELEMENT CHAR) 0 1 T)
                              (OUTPUT STREAM)))
  (UNLESS (INPUT STREAM) (SETF (INPUT STREAM) (OUTPUT STREAM)))
  NIL)


(defmethod STREAM-READ-CHAR-NO-HANG ((STREAM FIFO-STREAM))
  "
Returns a character or :EOF, like DEFUN STREAM-READ-CHAR, if that
would return immediately. If DEFUN STREAM-READ-CHAR's value is not
available immediately, returns NIL instead of waiting.

The default method simply calls DEFUN STREAM-READ-CHAR; this is
sufficient for streams whose DEFUN STREAM-READ-CHAR method never
blocks.
"
  )


(defmethod STREAM-PEEK-CHAR ((STREAM FIFO-STREAM))
  "
If a character was pushed back using DEFUN STREAM-UNREAD-CHAR, returns
it. Otherwise returns the next character from the stream, avoiding any
side effects DEFUN STREAM-READ-CHAR would do. Returns :EOF if the
end-of-stream is reached.

The default method calls DEFUN STREAM-READ-CHAR and DEFUN
STREAM-UNREAD-CHAR; this is sufficient for streams whose DEFUN
STREAM-READ-CHAR method has no side-effects.
"
  )


(defmethod STREAM-LISTEN ((STREAM FIFO-STREAM))
  "
If a character was pushed back using DEFUN STREAM-UNREAD-CHAR, returns it. Otherwise returns the next character from the stream, if already available. If no character is available immediately, or if end-of-stream is reached, returns NIL.

The default method calls DEFUN STREAM-READ-CHAR-NO-HANG and DEFUN STREAM-UNREAD-CHAR; this is sufficient for streams whose DEFUN STREAM-READ-CHAR method has no side-effects.
"
  )


(defmethod STREAM-READ-CHAR-WILL-HANG-P ((STREAM FIFO-STREAM))
  "
Returns NIL if DEFUN STREAM-READ-CHAR will return immediately. Otherwise it returns true.

The default method calls DEFUN STREAM-READ-CHAR-NO-HANG and DEFUN STREAM-UNREAD-CHAR; this is sufficient for streams whose DEFUN STREAM-READ-CHAR method has no side-effects.

This function is a CLISP extension (see EXT:READ-CHAR-WILL-HANG-P).
"
  )


(defmethod STREAM-READ-CHAR-SEQUENCE ((STREAM FIFO-STREAM)
                                      SEQUENCE &OPTIONAL (START 0) (END NIL))
  "
Fills the subsequence of sequence specified by :START and :END with
characters consecutively read from stream. Returns the index of the
first element of sequence that was not updated (= end or < end if the
stream reached its end).

sequence is an array of characters, i.e. a string. start is a
nonnegative integer and default to 0. end is a nonnegative integer or
NIL and defaults to NIL, which stands for (LENGTH sequence).

The default method repeatedly calls DEFUN STREAM-READ-CHAR; this is
always sufficient if speed does not matter.

This function is a CLISP extension (see EXT:READ-CHAR-SEQUENCE)
"
  (declare (ignore stream sequence start end))
  0)


(DEFMETHOD STREAM-READ-LINE ((STREAM FIFO-STREAM))
  "
Reads a line of characters, and return two values: the line (a string, without the terminating #\Newline character), and a boolean value which is true if the line was terminated by end-of-stream instead of #\Newline.

The default method repeatedly calls DEFUN STREAM-READ-CHAR; this is always sufficient.
"

  )


(DEFMETHOD STREAM-CLEAR-INPUT ((STREAM FIFO-STREAM))
  "
Clears all pending interactive input from the stream, and returns true if some pending input was removed.

The default method does nothing and returns NIL; this is sufficient for non-interactive streams.
"
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; generic functions for character output
;;;

(DEFMETHOD STREAM-WRITE-CHAR ((STREAM FIFO-STREAM) (CHAR CHARACTER))
  "
Writes char.

You must define a method for this function.
"
  )


(DEFMETHOD STREAM-LINE-COLUMN ((STREAM FIFO-STREAM))
  "
Returns the column number where the next character would be written (0 stands for the first column), or NIL if that is not meaningful for this stream.

You must define a method for this function.
"
  )

#|||
(DEFMETHOD STREAM-START-LINE-P STREAM)

RETURNS TRUE IF THE NEXT CHARACTER WOULD BE WRITTEN AT THE START OF A NEW LINE.

THE DEFAULT METHOD CALLS DEFUN STREAM-LINE-COLUMN AND COMPARES ITS RESULT WITH 0; this is sufficient for streams whose DEFUN STREAM-LINE-COLUMN never returns NIL.
(DEFMETHOD STREAM-WRITE-CHAR-SEQUENCE STREAM SEQUENCE &OPTIONAL [START [END]])

OUTPUTS THE SUBSEQUENCE OF SEQUENCE SPECIFIED BY :START AND :END TO STREAM.

SEQUENCE IS AN ARRAY OF CHARACTERS, I.E. A STRING. START IS A NONNEGATIVE INTEGER AND DEFAULT TO 0. END IS A NONNEGATIVE INTEGER OR NIL AND DEFAULTS TO NIL, WHICH STANDS FOR (LENGTH SEQUENCE).

THE DEFAULT METHOD REPEATEDLY CALLS DEFUN STREAM-WRITE-CHAR; this is always sufficient if speed does not matter.

THIS FUNCTION IS A CLISP EXTENSION (SEE EXT:WRITE-CHAR-SEQUENCE)
(DEFMETHOD STREAM-WRITE-STRING STREAM STRING &OPTIONAL [START [END]])

OUTPUTS THE SUBSEQUENCE OF STRING SPECIFIED BY :START AND :END TO STREAM. RETURNS STRING.

STRING IS A STRING. START IS A NONNEGATIVE INTEGER AND DEFAULT TO 0. END IS A NONNEGATIVE INTEGER OR NIL AND DEFAULTS TO NIL, WHICH STANDS FOR (LENGTH STRING).

THE DEFAULT METHOD CALLS DEFUN STREAM-WRITE-CHAR-SEQUENCE; this is always sufficient.
(DEFMETHOD STREAM-TERPRI STREAM)

OUTPUTS A #\NEWLINE CHARACTER.

THE DEFAULT METHOD CALLS DEFUN STREAM-WRITE-CHAR; this is always sufficient.
(DEFMETHOD STREAM-FRESH-LINE STREAM)

POSSIBLY OUTPUTS A #\NEWLINE CHARACTER, SO AS TO ENSURE THAT THE NEXT CHARACTER WOULD BE WRITTEN AT THE START OF A NEW LINE. RETURNS TRUE IF IT DID OUTPUT A #\NEWLINE CHARACTER.

THE DEFAULT METHOD CALLS DEFUN STREAM-START-LINE-P AND THEN DEFUN STREAM-TERPRI IF NECESSARY ; this is always sufficient.
(DEFMETHOD STREAM-FINISH-OUTPUT STREAM)

ENSURES THAT ANY BUFFERED OUTPUT HAS REACHED ITS DESTINATION, AND THEN RETURNS.

THE DEFAULT METHOD DOES NOTHING.
(DEFMETHOD STREAM-FORCE-OUTPUT STREAM)

BRINGS ANY BUFFERED OUTPUT ON ITS WAY TOWARDS ITS DESTINATION, AND RETURNS WITHOUT WAITING UNTIL IT HAS REACHED ITS DESTINATION.

THE DEFAULT METHOD DOES NOTHING.
(DEFMETHOD STREAM-CLEAR-OUTPUT STREAM)

ATTEMPTS TO DISCARD ANY BUFFERED OUTPUT WHICH HAS NOT YET REACHED ITS DESTINATION.

THE DEFAULT METHOD DOES NOTHING.
(DEFMETHOD STREAM-ADVANCE-TO-COLUMN STREAM COLUMN)

ENSURES THAT THE NEXT CHARACTER WILL BE WRITTEN AT COLUMN AT LEAST.

THE DEFAULT METHOD OUTPUTS AN APPROPRIATE AMOUNT OF SPACE CHARACTERS ; this is sufficient for non-proportional output.


|||#

;;;; fifo-stream.lisp                 --                     --          ;;;;
