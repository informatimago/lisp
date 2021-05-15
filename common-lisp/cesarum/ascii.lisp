;;;;**************************************************************************
;;;;FILE:               ascii.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-14 <PJB> Added REPLACE-ASCII-BYTES and REPLACE-ASCII-CHARACTERS.
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
;;;;    Copyright Pascal J. Bourguignon 2006 - 2021
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
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048")
  (:shadow "ED")
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
   "BYTES=" "BYTES/=" "BYTES<" "BYTES<=" "BYTES>=" "BYTES>"
   "REPLACE-ASCII-BYTES" "REPLACE-ASCII-CHARACTERS")
  (:documentation "

Some ASCII code utilities, to process sequences of ASCII code bytes as
easily as strings.

Examples:

    (bytes= buffer #.(ascii-bytes \"HELO \") :end1 (min 5 (length buffer)))
    (bytes= (read-ascii-line) #\"HELO \"     :end1 (min 5 (length buffer)))

License:

    AGPL3

    Copyright Pascal J. Bourguignon 2006 - 2015

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII")

;;; http://en.wikipedia.org/wiki/Ascii



#-(and) (eval-when (:compile-toplevel :load-toplevel :execute) ;; defined in ecma048
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
          (defconstant del       #x7f  "^?  ASCII Control Code Delete "))

;; NUL Null: The all-zeros character which may serve to accomplish time
;;     fill and media fill.
;;
;; SOH Start of Heading: A communication control character used at the
;;     beginning of a sequence of characters which constitute a
;;     machine-sensible address or routing information. Such a sequence
;;     is referred to as the "heading." An STX character has the effect
;;     of terminating a heading.
;;
;; STX Start of Text: A communication control character which precedes a
;;     sequence of characters that is to be treated as an entity and
;;     entirely transmitted through to the ultimate destination. Such a
;;     sequence is referred to as "text." STX may be used to terminate a
;;     sequence of characters started by SOH.
;;
;; ETX End of Text: A communication control character used to terminate a
;;     sequence of characters started with STX and transmitted as an
;;     entity.
;;
;; EOT End of Transmission: A communication control character used to
;;     indicate the conclusion of a transmission, which may have
;;     contained one or more texts and any associated headings.
;;
;; ENQ Enquiry: A communication control character used in data
;;     communication systems as a request for a response from a remote
;;     station. It may be used as a "Who Are You" (WRU) to obtain
;;     identification, or may be used to obtain station status, or both.
;;
;; ACK Acknowledge: A communication control character transmitted by a
;;     receiver as an affirmative response to a sender.
;;
;; BEL Bell: A character for use when there is a need to call for human
;;     attention. It may control alarm or attention devices.
;;
;; BS  Backspace: A format effector which controls the movement of the
;;     printing position one printing space backward on the same printing
;;     line. (Applicable also to display devices.)
;;
;; HT  Horizontal Tabulation: A format effector which controls the
;;     movement of the printing position to the next in a series of
;;     predetermined positions along the printing line. (Applicable also
;;     to display devices and the skip function on punched cards.)
;;
;; LF  Line Feed: A format effector which controls the movement of the
;;     printing position to the next printing line. (Applicable also to
;;     display devices.) Where appropriate, this character may have the
;;     meaning "New Line" (NL), a format effector which controls the
;;     movement of the printing point to the first printing position on
;;     the next printing line. Use of this convention requires agreement
;;     between sender and recipient of data.
;;
;; VT  Vertical Tabulation: A format effector which controls the movement
;;     of the printing position to the next in a series of predetermined
;;     printing lines. (Applicable also to display devices.)
;;
;; FF  Form Feed: A format effector which controls the movement of the
;;     printing position to the first pre-determined printing line on the
;;     next form or page. (Applicable also to display devices.)
;;
;; CR  Carriage Return: A format effector which controls the movement of
;;     the printing position to the first printing position on the same
;;     printing line. (Applicable also to display devices.)
;;
;; SO  Shift Out: A control character indicating that the code
;;     combinations which follow shall be interpreted as outside of the
;;     character set of the standard code table until a Shift In
;;     character is reached.
;;
;; SI  Shift In: A control character indicating that the code
;;     combinations which follow shall be interpreted according to the
;;     standard code table.
;;
;; DLE Data Link Escape: A communication control character which will
;;     change the meaning of a limited number of contiguously following
;;     characters. It is used exclusively to provide supplementary
;;     controls in data communication networks.
;;
;; DC1, DC2, DC3, DC4 Device Controls: Characters for the control of
;;     ancillary devices associated with data processing or
;;     telecommunication systems, more especially switching devices "on"
;;     or "off." (If a single "stop" control is required to interrupt or
;;     turn off ancillary devices, DC4 is the preferred assignment.)
;;
;; NAK Negative Acknowledge: A communication control character
;;     transmitted by a receiver as a negative response to the sender.
;;
;; SYN Synchronous Idle: A communication control character used by a
;;     synchronous transmission system in the absence of any other
;;     character to provide a signal from which synchronism may be
;;     achieved or retained.
;;
;; ETB End of Transmission Block: A communication control character used
;;     to indicate the end of a block of data for communication
;;     purposes. ETB is used for blocking data where the block structure
;;     is not necessarily related to the processing format.
;;
;; CAN Cancel: A control character used to indicate that the data with
;;     which it is sent is in error or is to be disregarded.
;;
;; EM  End of Medium: A control character associated with the sent data
;;     which may be used to identify the physical end of the medium, or
;;     the end of the used, or wanted, portion of information recorded on
;;     a medium. (The position of this character does not necessarily
;;     correspond to the physical end of the medium.)
;;
;; SUB Substitute: A character that may be substituted for a character
;;     which is determined to be invalid or in error.
;;
;; ESC Escape: A control character intended to provide code extension
;;     (supplementary characters) in general information interchange. The
;;     Escape character itself is a prefix affecting the interpretation
;;     of a limited number of contiguously following characters.
;;
;; FS, GS, RS, US  File Separator, Group Separator, Record Separator, and
;;     Unit Separator: These information separators may be used within
;;     data in optional fashion, except that their hierarchical
;;     relationship shall be: FS is the most inclusive, then GS, then RS,
;;     and US is least inclusive. (The content and length of a File,
;;     Group, Record, or Unit are not specified.)
;;
;; DEL Delete: This character is used primarily to "erase" or
;;     "obliterate" erroneous or unwanted characters in perforated
;;     tape. (In the strict sense, DEL is not a control character.)




(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Printable character:
  (defconstant sp        #x20 "     Code of ASCII Character SPACE")


  (defgeneric encoding-error-character (err)
    (:documentation "The character that cannot be encoded."))
  (defgeneric encoding-error-coding-system (err)
    (:documentation "A keyword denoting the coding system which cannot encode the character."))
  (defgeneric encoding-error-message (err)
    (:documentation "The error message."))

  (define-condition encoding-error (error)
    ((character     :initarg :character     :reader encoding-error-character)
     (coding-system :initarg :coding-system :reader encoding-error-coding-system)
     (message       :initarg :message       :reader encoding-error-message))
    (:documentation "The condition denoting an encoding error.")
    (:report (lambda (condition stream)
               (format stream "The character ~C (native code ~D) cannot be encoded in ~A: ~A"
                       (encoding-error-character condition)
                       (char-code (encoding-error-character condition))
                       (encoding-error-coding-system condition)
                       (encoding-error-message condition)))))


  (defgeneric decoding-error-code (err)
    (:documentation "The code that corresponds to no character."))
  (defgeneric decoding-error-coding-system (err)
    (:documentation "A keyword denoting the coding system which cannot decode the code."))
  (defgeneric decoding-error-message (err)
    (:documentation "The error message."))

  (define-condition decoding-error (error)
    ((code          :initarg :code           :reader decoding-error-code)
     (coding-system :initarg :coding-system  :reader decoding-error-coding-system)
     (message       :initarg :message        :reader decoding-error-message))
    (:documentation "The condition denoting a decoding error.")
    (:report (lambda (condition stream)
               (format stream "The code ~D (hexa ~:*~2,'0X) cannot be decoded in ~A: ~A"
                       (decoding-error-code condition)
                       (decoding-error-coding-system condition)
                       (decoding-error-message condition)))))


  (defparameter *ascii-characters*
    #.(concatenate 'string
                   " !\"#$%&'()*+,-./0123456789:;<=>?"
                   "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                   "`abcdefghijklmnopqrstuvwxyz{|}~")
    "A string containing all the ASCII characters in lexical order.")

  (defparameter *hexadecimal-digits* "0123456789abcdef"
    "A string contaiing the hexadecimal digits (lower case letters) in order.")

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


(defun replace-ascii-characters (string bytes &key (newline *newline*) (start1 0) end1 (start2 0) (end2 (length bytes)))
  (loop
    :with newline := (input-newline newline)
    :with endd := (or end1 (length string))
    :with len := (- end2 start2)
    :with j := start2
    :for code := (aref bytes j)
    :for i :from start1 :below endd
    :while (< j end2)
    :do (if (<= sp code 126)
            (setf (aref string i) (aref *ascii-characters* (- code sp)))
            (case code
              ((#.cr)
               (ecase newline
                 ((:crlf) (if (and (< (1+ j) len) (= lf (aref bytes (1+ j))))
                              (progn (incf j)
                                     (setf (aref string i) #\newline))
                              (ascii-error code)))
                 ((:any)  (if (and (< (1+ j) len) (= lf (aref bytes (1+ j))))
                              (progn (incf j)
                                     (setf (aref string i) #\newline))
                              (setf (aref string i) #\newline)))
                 ((:cr)   (setf (aref string i) #\newline))
                 ((:lf)   (ascii-error code))))
              ((#.lf)
               (ecase newline
                 ((:any  :lf) (setf (aref string i) #\newline))
                 ((:crlf :cr) (ascii-error code))))
              (otherwise (ascii-error code))))
    :finally (return string)))


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
  (replace-ascii-characters (make-array (- end start)
                                        :element-type 'character
                                        :adjustable t :fill-pointer 0)
                            bytes :newline newline :start2 start :end2 end))


(defun replace-ascii-bytes (bytes string &key (newline *newline*) (start1 0) end1 (start2 0) (end2 (length string)))
  "
RETURN:   BYTES, and the position beyond the last byte stored.
BYTES:    A byte vector containing the ASCII codes of the characters in
          the string.
          Only printable character and #\newline are accepted in the string.
          #\newline is translated to either CR+LF, CR, or LF according to the
          NEWLINE parameter.
NEWLINE:  (member :crlf :cr :lf) ; the default is *NEWLINE*.
"
  (loop
    :with newline = (output-newline newline)
    :with endd := (or end1 (length bytes))
    :with i := (- start1 1)
    :for j :from start2 :below end2
    :for ch := (aref string j)
    :while (< i endd)
    :do (if (char= ch #\newline)
            (ecase newline
              ((:crlf) (setf (aref bytes (incf i)) cr
                             (aref bytes (incf i)) lf))
              ((:cr)   (setf (aref bytes (incf i)) cr))
              ((:lf)   (setf (aref bytes (incf i)) lf)))
            (setf (aref bytes (incf i)) (ascii-code ch)))
    :finally (return (values bytes j))))


(defun ascii-bytes (string &key (newline *newline*) (start 0) (end (length string)))
  "
RETURN:   A byte vector containing the ASCII codes of the characters in
          the string.
          Only printable character and #\newline are accepted in the string.
          #\newline is translated to either CR+LF, CR, or LF according to the
          NEWLINE parameter.
NEWLINE:  (member :crlf :cr :lf) ; the default is *NEWLINE*.
"
  (replace-ascii-bytes (make-array
                        (+ (- end start)
                           (if (eq newline :crlf)
                               (count #\newline string :start start :end end)
                               0))
                        :element-type '(unsigned-byte 8))
                       string :newline newline :start2 start :end2 end))


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
         (ecase newline
           ((:crlf) (when (and (<= 2 (length buffer))
                               (= cr (aref buffer (- (length buffer) 2)))
                               (= lf (aref buffer (- (length buffer) 1))))
                      (decf (fill-pointer buffer) 2)
                      (return-from read-ascii-line buffer)))
           ((:cr)   (when (and (<= 1 (length buffer))
                               (= cr (aref buffer (- (length buffer) 1))))
                      (decf (fill-pointer buffer))
                      (return-from read-ascii-line buffer)))
           ((:lf)   (when (and (<= 1 (length buffer))
                               (= lf (aref buffer (- (length buffer) 1))))
                      (decf (fill-pointer buffer))
                      (return-from read-ascii-line buffer))))
     :finally (cond
                ((plusp (length buffer)) (return-from read-ascii-line buffer))
                (eof-error                (error 'end-of-file :stream stream))
                (t                 (return-from read-ascii-line eof-value)))))


(defun ascii-format (destination ctrl-string &rest arguments)
  "

DO:             Format the CTRL-STRING and the ARGUMENTS with FORMAT,
                convert the resulting string to a vector of ASCII
                bytes and send it to the DESTINATION.

RETURN:         NIL if DESTINATION is not NIL, a byte vector otherwise.

DESTINATION:    T      -> *STANDARD-OUTPUT*
                NIL    -> A vector of bytes is returned.
                STREAM -> An output binary stream to which the bytes are written.

CTRL-STRING:    A FORMAT control string (contaiing only characters in
                the ASCII character set).

ARGUMENTS:      Arguments to be formated with FORMAT.

SEE ALSO:       FORMAT
"
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


;;;; THE END ;;;;
