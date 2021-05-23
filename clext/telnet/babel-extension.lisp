;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               babel-extension.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A function to test for code sequences.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-14 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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

(in-package "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION")

#|
We would want to know when we have accumulated in a buffer enough bytes to decode a character, depending on the current encodng…
babel doesn't provide a convenient (efficient) API to test that, but I hoped to be able to use OCTETS-TO-STRING for that.
Unfortunately, handling of incomplete code sequences by the different encoding is not consistent.


```
cl-user> (babel:OCTETS-TO-STRING (coerce #(194 182) '(vector (unsigned-byte 8))) :start 0 :end 2 :errorp nil :encoding :utf-8)
"¶"
cl-user> (babel:OCTETS-TO-STRING (coerce #(194 182) '(vector (unsigned-byte 8))) :start 0 :end 1 :errorp nil :encoding :utf-8)
"�"
cl-user> (babel:OCTETS-TO-STRING (coerce #(194 182) '(vector (unsigned-byte 8))) :start 0 :end 2 :errorp nil :encoding :utf-16)
"슶"
cl-user> (babel:OCTETS-TO-STRING (coerce #(194 182) '(vector (unsigned-byte 8))) :start 0 :end 1 :errorp nil :encoding :utf-16)
> Debug: Failed assertion: (= babel-encodings::i babel-encodings::end)
> While executing: (:internal swank::invoke-default-debugger), in process new-repl-thread(1481).
> Type cmd-/ to continue, cmd-. to abort, cmd-\ for a list of available restarts.
> If continued: test the assertion again.
> Type :? for other options.
1 > :q
; Evaluation aborted on #<simple-error #x302006CBABDD>.
cl-user> (babel:octets-to-string (babel:string-to-octets "こんにちは 世界" :encoding :eucjp) :start 0 :end 2 :encoding :eucjp)
"こ"
cl-user> (babel:octets-to-string (babel:string-to-octets "こんにちは 世界" :encoding :eucjp) :start 0 :end 1 :encoding :eucjp)
> Debug: Illegal :eucjp character starting at position 0.
> While executing: (:internal swank::invoke-default-debugger), in process repl-thread(3921).
> Type cmd-. to abort, cmd-\ for a list of available restarts.
> Type :? for other options.
1 > :q
; Evaluation aborted on #<babel-encodings:end-of-input-in-character #x302006CA4EAD>.
cl-user>
```


I would suggest to add a keyword parameter to specify what to do in such a case:

| :on-invalid-code substitution-character | would insert the given substitution-character in place of the code. |
| :on-invalid-code :ignore                | would ignore the code and go on.                                    |
| :on-invalid-code :error                 | would signal a babel-encodings:character-decoding-error condition.  |



I would propose also, to provide an efficient function to query the length of a code sequence for the next character:
```
(babel:decode-character bytes &key start end encoding)
--> character ;
    sequence-valid-p ;
    length
```

- If a character can be decoded, then it is returned as primary value, otherwise NIL.

- If the code sequence is definitely invalid then NIL, else T. Notably if it is just too short, but could be a valid code sequence if completed, T should be returned.

- If the character is decoded and returned, then the length of the decoded code sequence is returned; if sequence-valid-p then a minimal code sequence length with the given prefix is returned; otherwise a minimum code sequence length.


| character | sequence-valid-p | length                                                         |
|-----------+------------------+----------------------------------------------------------------|
| ch        | T                | length of the decoded sequence                                 |
| ch        | NIL              | --impossible--                                                 |
| NIL       | T                | minimal length of a valid code sequence with the given prefix. |
| NIL       | NIL              | minimal length of a valid code sequence.                       |

For example, in the case NIL T len, if len <= (- end start), then it means the given code sequence is valid, but the decoded code is not the code of a character.  eg. ```#(#xED #xA0 #x80)``` is UTF-8 for 55296, but ```(code-char 55296) --> nil```.


```
(babel:decode-character (coerce #(65 32 66) '(vector (unsigned-byte 8)))
                         :start 0 :end 3 :encoding :utf-8)
--> #\A
    T
    1

(babel:decode-character (coerce #(195 128 32 80 97 114 105 115) '(vector (unsigned-byte 8)))
                        :start 0 :end 3 :encoding :utf-8)
--> #\À
    T
    2

(babel:decode-character (coerce #(195 128 32 80 97 114 105 115) '(vector (unsigned-byte 8)))
                        :start 0 :end 1 :encoding :utf-8)
--> NIL
    T
    2

(babel:decode-character (coerce #(195 195 32 80 97 114 105 115) '(vector (unsigned-byte 8)))
                        :start 0 :end 1 :encoding :utf-8)
--> NIL
    T
    2

(babel:decode-character (coerce #(195 195 32 80 97 114 105 115) '(vector (unsigned-byte 8)))
                        :start 0 :end 2 :encoding :utf-8)
--> NIL
    NIL
    1

(babel:decode-character (coerce #(#xED #xA0 #x80) '(vector (unsigned-byte 8)))
                        :start 0 :end 3 :encoding :utf-8)
--> NIL
    T
    3
```

|#


;; (defparameter *replacement-character*
;;   (if (<= 65535 char-code-limit)        ; does it really mean that the
;;                                         ; implementation uses unicode?
;;
;;       (code-char 65533)                 ; #\Replacement_Character
;;
;;       ;; Let's assume ASCII:
;;       (code-char 26)                    ; #\Sub
;;       ;; SUB is #x3f  in EBCDIC
;;       )
;;
;;   "A replacement character.")


(defparameter *replacement-character* (code-char 65533)
  ;; TODO: Is it always the same for all encodings?
  "The replacement character used by babel.")


(defun decode-character (octets &key (start 0) end (encoding :utf-8))
  ;; we'll optimize :us-ascii, :iso-8895-1 and :utf-8 cases.
  (let ((end (or end (length octets))))
    (case encoding
      ((:us-ascii :csascii :cp637 :ibm637 :us :iso646-us :ascii :iso-ir-6)
       (if (<= end start)
           (values nil t 1)
           (let ((code (aref octets start)))
             (if (<= 0 code 127)
                 (values (code-char code) t 1)
                 (values nil nil 1)))))
      ((:iso-8859-1 :iso_8859-1 :latin1 :l1 :ibm819 :cp819 :csisolatin1)
       (if (<= end start)
           (values nil t 1)
           (let ((code (aref octets start)))
             (if (<= 0 code 255)
                 (values (code-char code) t 1)
                 (values nil nil 1)))))
      ((:utf-8)
       (if (<= end start)
           (values nil t 1)
           (let ((byte (aref octets start))
                 (code 0))
             (cond
               ((<= 0 byte 127)         ; 1 byte
                (values (code-char byte) t 1))
               ((<= #b11000000 byte #b11011111) ; 2 bytes
                (setf code (ash (ldb (byte 5 0) byte) 6))
                (incf start)
                (if (< start end)
                    (let ((byte (aref octets start)))
                      (if (<= #b10000000 byte #b10111111)
                          (progn
                            (setf code (dpb (ldb (byte 6 0) byte) (byte 6 0) code))
                            (values (code-char code) t 2))
                          (values nil nil 2)))
                    (values nil t 2)))
               ((<= #b11100000 byte #b11101111) ; 3 bytes
                (setf code (ash (ldb (byte 4 0) byte) 12))
                (incf start)
                (if (< start end)
                    (let ((byte (aref octets start)))
                      (if (<= #b10000000 byte #b10111111)
                          (progn
                            (setf code (dpb (ldb (byte 6 0) byte) (byte 6 6) code))
                            (incf start)
                            (if (< start end)
                                (let ((byte (aref octets start)))
                                  (if (<= #b10000000 byte #b10111111)
                                      (progn
                                        (setf code (dpb (ldb (byte 6 0) byte) (byte 6 0) code))
                                        (values (code-char code) t 3))
                                      (values nil nil 3)))
                                (values nil t 3)))
                          (values nil nil 3)))
                    (values nil t 3)))
               ((<= #b11110000 byte #b11110111) ; 4 bytes
                (setf code (ash (ldb (byte 3 0) byte) 18))
                (incf start)
                (if (< start end)
                    (let ((byte (aref octets start)))
                      (if (<= #b10000000 byte #b10111111)
                          (progn
                            (setf code (dpb (ldb (byte 6 0) byte) (byte 6 12) code))
                            (incf start)
                            (if (< start end)
                                (let ((byte (aref octets start)))
                                  (if (<= #b10000000 byte #b10111111)
                                      (progn
                                        (setf code (dpb (ldb (byte 6 0) byte) (byte 6 6) code))
                                        (incf start)
                                        (if (< start end)
                                            (let ((byte (aref octets start)))
                                              (if (<= #b10000000 byte #b10111111)
                                                  (progn
                                                    (setf code (dpb (ldb (byte 6 0) byte) (byte 6 0) code))
                                                    (values (code-char code) t 4))
                                                  (values nil nil 4)))
                                            (values nil t 4)))
                                      (values nil nil 4)))
                                (values nil t 4)))
                          (values nil nil 4)))
                    (values nil t 4)))
               (t
                (values nil nil 1))))))
      (otherwise
       (handler-case
           (octets-to-string octets :start start :end end :errorp nil :encoding encoding)
         (:no-error (string)
           (if (= 1 (length string))
               (if (char= (aref string 0) *replacement-character*)
                   (values nil t 1)     ; ???
                   (values (aref string 0) t (length (string-to-octets string :encoding encoding))))
               (values (aref string 0) t (length (string-to-octets string :end 1 :encoding encoding)))))
         (end-of-input-in-character ()
           (values nil t 1))            ; ???
         (character-out-of-range ()
           (values nil t 1))            ; ???
         (character-decoding-error ()
           (values nil nil 1) #|???|#))))))


(defparameter babel::*string-vector-mappings*
  (babel::instantiate-concrete-mappings
   ;; :optimize ((speed 3) (safety 0) (debug 0) (compilation-speed 0))
   :octet-seq-setter babel::ub-set
   :octet-seq-getter babel::ub-get
   :octet-seq-type (vector (unsigned-byte 8) *)
   :code-point-seq-setter babel::string-set
   :code-point-seq-getter babel::string-get
   :code-point-seq-type babel::unicode-string))


(defun replace-octets-by-string (octets string &key (encoding *default-character-encoding*)
                                                 (use-bom :default)
                                                 (start1 0) end1 ; for octets
                                                 (start2 0) end2 ; for string
                                                 (errorp (not babel::*suppress-character-coding-errors*))
                                                 (error-on-out-of-space-p t))
  (declare (optimize (speed 3) (safety 2)))
  (let* ((babel::*suppress-character-coding-errors* (not errorp))
         (end1 (or end1 (length octets)))
         (end2 (or end2 (length string)))
         (mapping (babel::lookup-mapping babel::*string-vector-mappings* encoding))
         (bom (babel::bom-vector encoding use-bom))
         (bom-length (length bom))
         (octet-count (funcall (the function (babel::octet-counter mapping))
                               string start2 end2 -1)))
    (if (< (- end1 start1) (+ bom-length octet-count))
        (if error-on-out-of-space-p
            (error "Not enough space in destination octets vector; needed ~D bytes, available ~D bytes."
                   (+ bom-length octet-count)
                   (- end1 start1))
            (values nil (+ start1 bom-length octet-count)))
        (progn
          (replace octets bom :start1 start1)
          (funcall (the function (babel::encoder mapping))
                   string start2 end2 octets (+ start1 bom-length))
          (values octets (+ start1 bom-length octet-count))))))


;;;; THE END ;;;;
