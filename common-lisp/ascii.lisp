;;;;**************************************************************************
;;;;FILE:               ascii.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Some ASCII code utilities.
;;;;
;;;;    Examples:
;;;;
;;;;     (bytes= buffer #.(ascii-bytes "HELO ") :end1 (min 5 (length buffer)))
;;;;     (bytes= buffer #"HELO "                :end1 (min 5 (length buffer)))
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-07-05 <PJB> Moved to public/lisp/common-lisp
;;;;    2006-10-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2007
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

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ASCII"
  (:nicknames "ASCII")
  (:USE "COMMON-LISP")
  (:EXPORT
   "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL" "BS" "HT" "LF" "VT"
   "FF" "CR" "SO" "SI" "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB"
   "CAN" "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "DEL" "SP"
   "*NEWLINE*" "*ASCII-CHARACTERS*"  "*HEXADECIMAL-DIGITS*"
   "ASCII-CODE"   "CODE-ASCII"   "CODE-ASCII-DIGIT-P"
   "ASCII-STRING" "ASCII-BYTES"  "ASCII-DISPATCH-MACRO"
   "READ-ASCII-LINE" "ASCII-FORMAT"
   "BYTES=" "BYTES/=" "BYTES<" "BYTES<=" "BYTES>=" "BYTES>")
  (:DOCUMENTATION "
    Some ASCII code utilities.

    Copyright Pascal Bourguignon 2006 - 2007
    
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
   "))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ASCII")

;;; http://en.wikipedia.org/wiki/Ascii

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Control codes:
  (defconstant NUL       #x00  "^@  ASCII Control Code Null character ")
  (defconstant SOH       #x01  "^A  ASCII Control Code Start of Header")
  (defconstant STX       #x02  "^B  ASCII Control Code Start of Text")
  (defconstant ETX       #x03  "^C  ASCII Control Code End of Text")
  (defconstant EOT       #x04  "^D  ASCII Control Code End of Transmission")
  (defconstant ENQ       #x05  "^E  ASCII Control Code Enquiry")
  (defconstant ACK       #x06  "^F  ASCII Control Code Acknowledgement")
  (defconstant BEL       #x07  "^G  ASCII Control Code Bell")
  (defconstant BS        #x08  "^H  ASCII Control Code Backspace")
  (defconstant HT        #x09  "^I  ASCII Control Code Horizontal Tab")
  (defconstant LF        #x0A  "^J  ASCII Control Code Line feed")
  (defconstant VT        #x0B  "^K  ASCII Control Code Vectical Tab")
  (defconstant FF        #x0C  "^L  ASCII Control Code Form feed")
  (defconstant CR        #x0D  "^M  ASCII Control Code Carriage return")
  (defconstant SO        #x0E  "^N  ASCII Control Code Shift Out")
  (defconstant SI        #x0F  "^O  ASCII Control Code Shift In")
  (defconstant DLE       #x10  "^P  ASCII Control Code Data Link Escape")
  (defconstant DC1       #x11  "^Q  ASCII Control Code Device Control 1 (X-ON)")
  (defconstant DC2       #x12  "^R  ASCII Control Code Device Control 2")
  (defconstant DC3       #x13  "^S  ASCII Control Code Device Control 3 (X-OFF)")
  (defconstant DC4       #x14  "^T  ASCII Control Code Device Control 4")
  (defconstant NAK       #x15  "^U  ASCII Control Code Negative Acknowledge")
  (defconstant SYN       #x16  "^V  ASCII Control Code Synchronous Idle")
  (defconstant ETB       #x17  "^W  ASCII Control Code End of Transmision Block")
  (defconstant CAN       #x18  "^X  ASCII Control Code Cancel")
  (defconstant EM        #x19  "^Y  ASCII Control Code End of Medium")
  (defconstant SUB       #x1A  "^Z  ASCII Control Code Substitute")
  (defconstant ESC       #x1B  "^[  ASCII Control Code Escape")
  (defconstant FS        #x1C  "^\  ASCII Control Code File Separator")
  (defconstant GS        #x1D  "^]  ASCII Control Code Group Separator")
  (defconstant RS        #x1E  "^^  ASCII Control Code Record Separator")
  (defconstant US        #x1F  "^_  ASCII Control Code Unit Separator")
  (defconstant DEL       #x7F  "^?  ASCII Control Code Delete ")
  ;; Printable character:
  (defconstant SP        #x20 "     Code of ASCII Character SPACE") 



  (defparameter *ascii-characters*  #.(concatenate 'string
                                        " !\"#$%&'()*+,-./0123456789:;<=>?"
                                        "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                                        "`abcdefghijklmnopqrstuvwxyz{|}~"))

  (defparameter *hexadecimal-digits* "0123456789abcdef")

  (declaim (inline ascii-code))
  (defun ascii-code  (ch)
    "
RETURN:  The ASCII code of the character ch, or raise an error if the character
         has no ascii code.  
         Only printable characters are accepted. No control code.
" 
    (let ((code (position ch *ascii-characters*)))
      (if code
          (+ SP code)
          (error "Character ~C cannot be encoded in ASCII" ch)))))


(defparameter *newline* :crlf
  "(OR (MEMBER :CRLF :CR :LF)
     (CONS (MEMBER :CRLF :CR :LF) (MEMBER :CRLF :CR :LF :ANY))
The encoding used for #\newline for output and for input.
If it's a keyword, it's used for both output and input.
If it's a CONS cell, the CAR specifies the newline encoding for output
and the CDR specifies the newline encoding for input (it may be :ANY to
accept any of CR-LF, CR or LF; LF-CR would read as two newlines).")
(declaim (inline input-newline output-newline))
(defun input-newline  (newline) (if (consp newline) (cdr newline) newline))
(defun output-newline (newline) (if (consp newline) (car newline) newline))


(declaim (inline ascii-error))
(defun ascii-error (code)
   (error "ASCII control codes cannot be converted to characters; ~
           got ~D (#x~:*~2,'0X)" code))


(declaim (inline code-ascii))
(defun code-ascii (code)
  "
RETURN:  The character corresponding to the given ASCII code.
         Only codes for printable characters are accepted, 
         and both CR and LF are mapped to #\newline.
"
  (cond
    ((<= SP code 126)             (aref *ascii-characters* (- code SP)))
    ((or (= code CR) (= code LF)) #\newline)
    (t                            (ascii-error code))))


(declaim (inline code-ascii-digit-p))
(defun code-ascii-digit-p (code)
  "
RETURN:  The decimal digit value of the character encoded by the ASCII CODE,
         or NIL if CODE is not the ASCII code of a digit character.
"
  (and (<= #.(ascii-code #\0) code #.(ascii-code #\9))
       (- code #.(ascii-code #\0))))



(defun ascii-string (bytes &key (newline *newline*))
  "
DO:       Converts the ASCII bytes to a string. If there are control codes,
          an error is signaled.
RETURN:   A string containing the characters encoded in the ASCII bytes.
NEWLINE:  (member :crlf :cr :lf :any) ; the default is *NEWLINE*.
          If lone CR or LF are present, then an error is signaled.
"
  (setf newline (input-newline newline))
  (loop
     :with len = (length bytes)
     :with result = (make-array len :element-type 'character
                                :adjustable t :fill-pointer 0)
     :with i = 0
     :while (< i len)
     :do (let ((code (aref bytes i)))
           (if (<= SP code 126)
               (vector-push (aref *ascii-characters* (- code SP)) result)
               (case code
                 ((#.CR)
                  (ecase newline
                    ((:crlf) (if (and (< (1+ i) len) (= LF (aref bytes (1+ i))))
                                 (progn (incf i)
                                        (vector-push #\newline result))
                                 (ascii-error code)))
                    ((:any)  (if (and (< (1+ i) len) (= LF (aref bytes (1+ i))))
                                 (incf i)
                                 (vector-push #\newline result)))
                    ((:cr)   (vector-push #\newline result))
                    ((:lf)   (ascii-error code))))
                 ((#.LF)
                  (ecase newline
                    ((:any  :lf) (vector-push #\newline result))
                    ((:crlf :cr) (ascii-error code))))
                 (otherwise (ascii-error code))))
           (incf i))
     :finally (return result)))


(defun ascii-bytes (string &key (newline *newline*))
  "
RETURN:   A byte vector containing the ASCII codes of the characters in
          the string.
          Only printable character and #\newline are accepted in the string.
          #\newline is translated to either CR+LF, CR, or LF according to the 
          NEWLINE parameter.
NEWLINE:  (member :crlf :cr :lf) ; the default is *NEWLINE*.
"
  (setf newline (output-newline newline))
  (loop
     :with bytes = (make-array
                    (+ (length string)
                       (if (eq newline :crlf) (count #\newline string) 0))
                    :element-type '(unsigned-byte 8))
     :with b = -1
     :for ch :across string
     :do (if (char= ch #\newline)
             (ecase newline
               ((:crlf) (setf (aref bytes (incf b)) CR
                              (aref bytes (incf b)) LF))
               ((:cr)   (setf (aref bytes (incf b)) CR))
               ((:lf)   (setf (aref bytes (incf b)) LF)))
             (setf (aref bytes (incf b)) (ascii-code ch)))
     :finally (return bytes)))


(defun ascii-dispatch-macro (stream sub-char argument)
  "
DO:       Read a string and converts it to an ASCII byte sequence.
ARGUMENT: If NIL, #\newline are encoded according to *NEWLINE*.
          If 0, #\newline are encoded to CR-LF.
          If 1, #\newline are encoded to LF.
          If 2, #\newline are encoded to CR.
          Otherwise an error is issued.
SUB-CHAR: If it is #\" then it's considered the start of the string,
          otherwise we expect to read a full string next.
          This allow to set dispatch macro characters for: #\"abc\" or #Y\"abc\"
EXAMPLES:
  (set-dispatch-macro-character #\# #\Y (function ascii-dispatch-macro) *readtable*)
  (set-dispatch-macro-character #\# #\" (function ascii-dispatch-macro) *readtable*)
"
  (when (char= #\" sub-char)
      (unread-char sub-char stream))
  (let ((string (read stream t nil t)))
    (assert (stringp string) (string) "~S expects to read a string, not ~S"
            'ascii-dispatch-macro string)
    (ascii-bytes string :newline (ecase argument
                                   ((nil)   *NEWLINE*)
                                   ((0)     :crlf)
                                   ((1)     :lf)
                                   ((2)     :cr)))))



(defun read-ascii-line (stream &optional (eof-error t) (eof-value nil)
                        (newline *newline*))
  "
NEWLINE:  (member :crlf :cr :lf) ; the defaultl is :CRLF since that's what's
          used in Internet binary protocols using ASCII.
"
  (setf newline (input-newline newline))
  (loop
     :with buffer = (make-array 80 :element-type (stream-element-type stream)
                                :adjustable t :fill-pointer 0)
     :for item = (read-byte stream nil)
     :while item
     :do (vector-push-extend item buffer (array-dimension buffer 0))
     :do (ecase newline
           ((:crlf) (when (and (< 2 (length buffer))
                               (= CR (aref buffer (- (length buffer) 2)))
                               (= LF (aref buffer (- (length buffer) 1))))
                      (decf (fill-pointer buffer) 2)
                      (return-from read-ascii-line buffer)))
           ((:cr)   (when (and (< 1 (length buffer))
                               (= CR (aref buffer (- (length buffer) 2))))
                      (decf (fill-pointer buffer))
                      (return-from read-ascii-line buffer)))
           ((:lf)   (when (and (< 1 (length buffer))
                               (= LF (aref buffer (- (length buffer) 2))))
                      (decf (fill-pointer buffer))
                      (return-from read-ascii-line buffer))))
     :finally (cond
                ((plusp (length buffer)) (return-from read-ascii-line buffer))
                (eof-error                (error 'end-of-file :stream stream))
                (t                 (return-from read-ascii-line eof-value)))))


(defun ascii-format (destination ctrl-string &rest arguments)
  (let ((bytes (ascii-bytes
                (apply (function format) nil ctrl-string arguments))))
    (case destination
      ((t)       (write-sequence bytes *standard-output*) nil)
      ((nil)     bytes)
      (otherwise (write-sequence bytes destination)       nil))))



(defun bytes= (v1 v2 &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "
Like STRING=, but for byte vectors.
"
  (if (and (zerop start1)
           (zerop start2)
           (or (null end1) (= end1 (length v1)))
           (or (null end2) (= end2 (length v2))))
      (equalp v1 v2)
      (and (= (- (or end1 (length v1)) start1)
              (- (or end2 (length v2)) start2))
           (loop
              :for i :from start1 :below (or end1 (length v1))
              :for j :from start2 :below (or end2 (length v2))
              :always (= (aref v1 i) (aref v2 j))))))


(defun bytes/= (v1 v2 &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "
Like STRING/=, but for byte vectors.
"
  (not (bytes= v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


(defun bytes< (v1 v2 &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "
Like STRING<, but for byte vectors.
"
  (loop
     :with i = start1 :with mi = (or end1 (length v1))
     :with j = start2 :with mj = (or end2 (length v2))
     :while (and (< i mi) (< j mj)  (= (aref v1 i) (aref v2 j)))
     :do (incf i) (incf j)
     :finally (return (if (< i mi)
                          (if (< j mj)
                              (< (aref v1 i) (aref v2 j)) ; "abc" "axc"
                              nil)      ; "abcd" "abc"
                          (< j mj))))) ; "abc" "abcd" ; "abc" "abc"


(defun bytes<= (v1 v2 &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "
Like STRING<=, but for byte vectors.
"
  (or (bytes= v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)
      (bytes< v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


(defun bytes>= (v1 v2 &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "
Like STRING>=, but for byte vectors.
"
  (not (bytes< v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


(defun bytes> (v1 v2 &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "
Like STRING<, but for byte vectors.
"
  (not (bytes<= v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


(defun test ()
  "
DO:     Test the ASCII package; signal an error if something is wrong.
RETURN: :SUCCESS
"
  (loop
     :for ch :across *ascii-characters*
     :for code :from sp
     :do (assert (= code (ascii-code ch)))
     :do (assert (char= ch (code-ascii code))))
  (loop
     :for code :from (ascii-code #\0) :to (ascii-code #\9)
     :for n :from 0
     :do (assert (eql n (code-ascii-digit-p code))))
  (string= (ascii-string #(65 66 67 68)) "ABCD")
  (bytes=  #(65 66 67 68)  (ascii-bytes "ABCD"))
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\Y (function ascii-dispatch-macro)
                                  *readtable*)
    (set-dispatch-macro-character #\# #\" (function ascii-dispatch-macro)
                                  *readtable*)
    (assert (bytes= #(65 66 67 68) (read-from-string "#\"ABCD\"")))
    (assert (bytes= #(65 66 67 68) (read-from-string "#Y\"ABCD\""))))
  ;; TODO: Added more testing of bytes comparisons.
  :success)


;;;; THE END ;;;;
