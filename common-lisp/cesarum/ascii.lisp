;;;;**************************************************************************
;;;;FILE:               ascii.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Some ASCII code utilities, to process sequences of ASCII code
;;;;    bytes as easily as strings.
;;;;
;;;;    Examples:
;;;;
;;;;     (bytes= buffer #.(ascii-bytes "HELO ") :end1 (min 5 (length buffer)))
;;;;     (bytes= (read-ascii-line) #"HELO "     :end1 (min 5 (length buffer)))
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-20 <PJB> Added conditions;
;;;;                     Added :start and :stop to ASCII-STRING and ASCII-BYTES.
;;;;                     Added ASCII-CONTROL-CODE-P and ASCII-PRINTABLE-CODE-P.
;;;;                     Completed the TEST function.
;;;;    2007-07-05 <PJB> Moved to public/lisp/common-lisp
;;;;    2006-10-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2012
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

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
  (:use "COMMON-LISP")
  (:export
   "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL" "BS" "HT" "LF" "VT"
   "FF" "CR" "SO" "SI" "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB"
   "CAN" "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "DEL" "SP"
   "*NEWLINE*" "*ASCII-CHARACTERS*"  "*HEXADECIMAL-DIGITS*"
   "ENCODING-ERROR" "ENCODING-ERROR-CHARACTER"
   "ENCODING-ERROR-CODING-SYSTEM" "ENCODING-ERROR-MESSAGE"
   "DECODING-ERROR" "DECODING-ERROR-CODE"
   "DECODING-ERROR-CODING-SYSTEM" "DECODING-ERROR-MESSAGE"
   "ASCII-CODE"   "CODE-ASCII"   "CODE-ASCII-DIGIT-P"
   "ASCII-CONTROL-CODE-P"    "ASCII-PRINTABLE-CODE-P"
   "ASCII-STRING" "ASCII-BYTES"  "ASCII-DISPATCH-MACRO"
   "READ-ASCII-LINE" "ASCII-FORMAT"
   "BYTES=" "BYTES/=" "BYTES<" "BYTES<=" "BYTES>=" "BYTES>")
  (:documentation "
    Some ASCII code utilities.

    Copyright Pascal Bourguignon 2006 - 2012
    
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
   "))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII")

;;; http://en.wikipedia.org/wiki/Ascii

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Control codes:
  (defconstant nul       #x00  "^@  ASCII Control Code Null character ")
  (defconstant soh       #x01  "^A  ASCII Control Code Start of Header")
  (defconstant stx       #x02  "^B  ASCII Control Code Start of Text")
  (defconstant etx       #x03  "^C  ASCII Control Code End of Text")
  (defconstant eot       #x04  "^D  ASCII Control Code End of Transmission")
  (defconstant enq       #x05  "^E  ASCII Control Code Enquiry")
  (defconstant ack       #x06  "^F  ASCII Control Code Acknowledgement")
  (defconstant bel       #x07  "^G  ASCII Control Code Bell")
  (defconstant bs        #x08  "^H  ASCII Control Code Backspace")
  (defconstant ht        #x09  "^I  ASCII Control Code Horizontal Tab")
  (defconstant lf        #x0a  "^J  ASCII Control Code Line feed")
  (defconstant vt        #x0b  "^K  ASCII Control Code Vectical Tab")
  (defconstant ff        #x0c  "^L  ASCII Control Code Form feed")
  (defconstant cr        #x0d  "^M  ASCII Control Code Carriage return")
  (defconstant so        #x0e  "^N  ASCII Control Code Shift Out")
  (defconstant si        #x0f  "^O  ASCII Control Code Shift In")
  (defconstant dle       #x10  "^P  ASCII Control Code Data Link Escape")
  (defconstant dc1       #x11  "^Q  ASCII Control Code Device Control 1 (X-ON)")
  (defconstant dc2       #x12  "^R  ASCII Control Code Device Control 2")
  (defconstant dc3       #x13  "^S  ASCII Control Code Device Control 3 (X-OFF)")
  (defconstant dc4       #x14  "^T  ASCII Control Code Device Control 4")
  (defconstant nak       #x15  "^U  ASCII Control Code Negative Acknowledge")
  (defconstant syn       #x16  "^V  ASCII Control Code Synchronous Idle")
  (defconstant etb       #x17  "^W  ASCII Control Code End of Transmision Block")
  (defconstant can       #x18  "^X  ASCII Control Code Cancel")
  (defconstant em        #x19  "^Y  ASCII Control Code End of Medium")
  (defconstant sub       #x1a  "^Z  ASCII Control Code Substitute")
  (defconstant esc       #x1b  "^[  ASCII Control Code Escape")
  (defconstant fs        #x1c  "^\  ASCII Control Code File Separator")
  (defconstant gs        #x1d  "^]  ASCII Control Code Group Separator")
  (defconstant rs        #x1e  "^^  ASCII Control Code Record Separator")
  (defconstant us        #x1f  "^_  ASCII Control Code Unit Separator")
  (defconstant del       #x7f  "^?  ASCII Control Code Delete ")
  ;; Printable character:
  (defconstant sp        #x20 "     Code of ASCII Character SPACE") 


  (define-condition encoding-error (error)
    ((character     :initarg :character     :reader encoding-error-character)
     (coding-system :initarg :coding-system :reader encoding-error-coding-system)
     (message       :initarg :message       :reader encoding-error-message))
    (:report (lambda (condition stream)
               (format stream "The character ~C (native code ~D) cannot be encoded in ~A: ~A"
                       (encoding-error-character condition)
                       (char-code (encoding-error-character condition))
                       (encoding-error-coding-system condition)
                       (encoding-error-message condition)))))

  
  (define-condition decoding-error (error)
    ((code          :initarg :code           :reader decoding-error-code)
     (coding-system :initarg :coding-system  :reader decoding-error-coding-system)
     (message       :initarg :message        :reader decoding-error-message))
    (:report (lambda (condition stream)
               (format stream "The code ~D (hexa ~:*~2,'0X) cannot be decoded in ~A: ~A"
                       (decoding-error-code condition)
                       (decoding-error-coding-system condition)
                       (decoding-error-message condition)))))
  

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
          (+ sp code)
          (error 'encoding-error
                 :character ch
                 :coding-system :us-ascii
                 :message "This character cannot be encoded in ASCII")))))


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
  (error 'decoding-error
         :code code
         :coding-system :us-ascii
         :message (cond
                    ((or (< code sp) (= code del))
                     "ASCII control codes cannot be converted to characters.")
                    ((< del code)
                     "Codes greater than 127 are not ASCII codes.")
                    (t
                     "[SHOULD NOT OCCUR]"))))


(declaim (inline code-ascii))
(defun code-ascii (code)
  "
RETURN:  The character corresponding to the given ASCII code.
         Only codes for printable characters are accepted, 
         and both CR and LF are mapped to #\newline.
"
  (cond
    ((<= sp code  (1- del))       (aref *ascii-characters* (- code sp)))
    ((or (= code cr) (= code lf)) #\newline)
    (t                            (ascii-error code))))


(declaim (inline ascii-printable-code-p))
(defun ascii-printable-code-p (code)
  "RETURN:  Whether CODE is the code of an ASCII printable character."
  (<= sp code (1- del)))


(declaim (inline ascii-control-code-p))
(defun ascii-control-code-p (code)
  "RETURN:  Whether CODE is an ASCII control code."
  (or (<= nul code (1- sp)) (= del code)))



(declaim (inline code-ascii-digit-p))
(defun code-ascii-digit-p (code)
  "
RETURN:  The decimal digit value of the character encoded by the ASCII CODE,
         or NIL if CODE is not the ASCII code of a digit character.
"
  (and (<= #.(ascii-code #\0) code #.(ascii-code #\9))
       (- code #.(ascii-code #\0))))



(defun ascii-string (bytes &key (newline *newline*) (start 0) (end (length bytes)))
  "
DO:       Converts the ASCII bytes to a string. If there are control codes,
          an error is signaled.
RETURN:   A string containing the characters encoded in the ASCII bytes.
NEWLINE:  (member :crlf :cr :lf :any) ; the default is *NEWLINE*.
          If lone CR or LF are present, then an error is signaled.
START:    index of the first byte to be converted.
END:      index beyond the last byte to be converted.
"
  (setf newline (input-newline newline))
  (loop
     :with len = (- end start)
     :with result = (make-array len :element-type 'character
                                :adjustable t :fill-pointer 0)
     :with i = start
     :while (< i end)
     :do (let ((code (aref bytes i)))
           (if (<= sp code 126)
               (vector-push (aref *ascii-characters* (- code sp)) result)
               (case code
                 ((#.cr)
                  (ecase newline
                    ((:crlf) (if (and (< (1+ i) len) (= lf (aref bytes (1+ i))))
                                 (progn (incf i)
                                        (vector-push #\newline result))
                                 (ascii-error code)))
                    ((:any)  (if (and (< (1+ i) len) (= lf (aref bytes (1+ i))))
                                 (incf i)
                                 (vector-push #\newline result)))
                    ((:cr)   (vector-push #\newline result))
                    ((:lf)   (ascii-error code))))
                 ((#.lf)
                  (ecase newline
                    ((:any  :lf) (vector-push #\newline result))
                    ((:crlf :cr) (ascii-error code))))
                 (otherwise (ascii-error code))))
           (incf i))
     :finally (return result)))


(defun ascii-bytes (string &key (newline *newline*) (start 0) (end (length string)))
  "
RETURN:   A byte vector containing the ASCII codes of the characters in
          the string.
          Only printable character and #\newline are accepted in the string.
          #\newline is translated to either CR+LF, CR, or LF according to the 
          NEWLINE parameter.
NEWLINE:  (member :crlf :cr :lf) ; the default is *NEWLINE*.
"
  (loop
    :with newline = (output-newline newline)
    :with bytes = (make-array
                   (+ (- end start)
                      (if (eq newline :crlf)
                          (count #\newline string :start start :end end)
                          0))
                   :element-type '(unsigned-byte 8))
    :with b = -1
    :for i :from start :below end
    :for ch = (aref string i)
    :do (if (char= ch #\newline)
            (ecase newline
              ((:crlf) (setf (aref bytes (incf b)) cr
                             (aref bytes (incf b)) lf))
              ((:cr)   (setf (aref bytes (incf b)) cr))
              ((:lf)   (setf (aref bytes (incf b)) lf)))
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
    (assert (stringp string) (string) "~s expects to read a string, not ~s"
            'ascii-dispatch-macro string)
    (ascii-bytes string :newline (ecase argument
                                   ((nil)   *newline*)
                                   ((0)     :crlf)
                                   ((1)     :lf)
                                   ((2)     :cr)))))



(defun read-ascii-line (stream &optional (eof-error t) (eof-value nil)
                        (newline *newline*))
  "
newline:  (member :crlf :cr :lf) ; the defaultl is :CRLF since that's what's
          used in internet binary protocols using ascii.
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
                               (= cr (aref buffer (- (length buffer) 2)))
                               (= lf (aref buffer (- (length buffer) 1))))
                      (decf (fill-pointer buffer) 2)
                      (return-from read-ascii-line buffer)))
           ((:cr)   (when (and (< 1 (length buffer))
                               (= cr (aref buffer (- (length buffer) 2))))
                      (decf (fill-pointer buffer))
                      (return-from read-ascii-line buffer)))
           ((:lf)   (when (and (< 1 (length buffer))
                               (= lf (aref buffer (- (length buffer) 2))))
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
like string=, but for byte vectors.
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
like string/=, but for byte vectors.
"
  (not (bytes= v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


(defun bytes< (v1 v2 &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "
like string<, but for byte vectors.
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
like string<=, but for byte vectors.
"
  (or (bytes= v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)
      (bytes< v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


(defun bytes>= (v1 v2 &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "
like string>=, but for byte vectors.
"
  (not (bytes< v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


(defun bytes> (v1 v2 &key (start1 0) (start2 0) (end1 nil) (end2 nil))
  "
like string<, but for byte vectors.
"
  (not (bytes<= v1 v2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))


(defun test ()
  "
DO:     test the ascii package; signal an error if something is wrong.
RETURN: :success
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
  (assert (typep (nth-value 1 (ignore-errors (ascii-string #(65 66 8 67 69)))) 'decoding-error))
  (assert (typep (nth-value 1 (ignore-errors (ascii-bytes "En été, il fait chaud."))) 'encoding-error))
  (assert (string= "ABCD" (ascii-string #(65 66 67 68))))
  (assert (string= "ABCD" (ascii-string #(0 0 65 66 67 68 0 0 0 0) :start 2 :end 6)))
  (assert (bytes=  #(65 66 67 68)  (ascii-bytes "ABCD")))
  (assert (bytes=  #(65 66 67 68)  (ascii-bytes "00ABCD0000" :start 2 :end 6)))
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\Y (function ascii-dispatch-macro)
                                  *readtable*)
    (set-dispatch-macro-character #\# #\" (function ascii-dispatch-macro)
                                  *readtable*)
    (assert (bytes= #(65 66 67 68) (read-from-string "#\"ABCD\"")))
    (assert (bytes= #(65 66 67 68) (read-from-string "#Y\"ABCD\""))))
  ;; TODO: Added more testing of bytes comparisons.
  :success)


(test)


;;;; THE END ;;;;
