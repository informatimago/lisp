;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               c-string-reader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A C string reader, implememting C string back-slash escapes.
;;;;    Also includes a writer to print strings with C back-slash escapes.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-05-22 <PJB> Added character-code-reader-macro, factorized
;;;;                     out c-escaped-character-map.
;;;;                     Published as http://paste.lisp.org/display/137262
;;;;    2011-05-21 <PJB> Updated from http://paste.lisp.org/display/69905 (lost).
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2013 - 2016
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.CPP")


(defun c-escaped-character-map (escaped-character)
  (case escaped-character
    ((#\' #\" #\? #\\) escaped-character)
    ((#\newline) -1)
    ((#\a)        7)
    ((#\b)        8)
    ((#\t)        9)
    ((#\n)       10)
    ((#\v)       11)
    ((#\f)       12)
    ((#\r)       13)
    ((#\x)       :hexa)
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) :octal)
    (otherwise   :default)))


(defun char-unicode (character)
  (let ((bytes (babel:string-to-octets (string character)
                                       :encoding :utf-32be
                                       :use-bom nil)))
    (+ (* 256 (+ (* 256 (+ (* 256 (aref bytes 0))
                           (aref bytes 1)))
                 (aref bytes 2)))
       (aref bytes 3))))


(defun character-code-reader-macro (stream quotation-mark)
  "Reads the emacs syntax for character
 ?a -> 64
 ?\\10 -> 8
 ?\\x10 -> 16
#\\? must have been already read.
"
  (declare (ignore quotation-mark))
  (flet ((encode (ch) (char-unicode ch)))
    (let ((ch (read-char stream)))
      (if (char/= #\\ ch)
          (encode ch)
          (let* ((ch (read-char stream))
                 (code (c-escaped-character-map ch)))
            (flet ((read-code (*read-base* base-name)
                     (let ((code (read stream)))
                       (if (and (integerp code) (<= 0 code (1- char-code-limit)))
                           code
                           (cpp-error "Invalid ~A character code: ~A" base-name code)))))
              (case code
                (:hexa  (read-code 16 "hexadecimal"))
                (:octal (unread-char ch stream) (read-code  8 "octal"))
                (:default ;; In emacs ?\x = ?x
                 (encode ch))
                (otherwise
                 (if (characterp code)
                     (encode code)
                     code)))))))))

;; (set-macro-character #\? 'character-code-reader-macro t)

(defun read-c-string (stream delimiter)
  "Read a C string or a C char from the STREAM, depending on delimiter = #\\\" or #\\'.
The initial delimited must have been read already."
  ;; TODO: "\xE9" and "é" won't issue the same bytes depending on the target encoding "\xE9" is fixed char[]{233,0}.
  (let ((buffer (make-array 80 :element-type 'character
                            :adjustable t :fill-pointer 0))
        (state :in-string)
        (start  0))
    (flet ((process-token (ch)
             (ecase state
               ((:in-string)
                (setf state (cond
                              ((char= delimiter ch) :out)
                              ((char= #\\       ch) :escape)
                              (t                    (vector-push-extend ch buffer)
                                                    :in-string)))
                nil)
               ((:escape)
                (setf state :in-string)
                (let ((code (c-escaped-character-map ch)))
                  (case code
                    (:hexa
                     (setf state :in-hexa
                           start (fill-pointer buffer)))
                    (:octal
                     (setf state :in-octal
                           start (fill-pointer buffer))
                     (vector-push-extend ch buffer))
                    (:default
                     (cpp-error "Invalid escape character \\~C at position ~D"
                            ch (fill-pointer buffer)))
                    (otherwise
                     (cond
                       ((characterp code) (vector-push-extend code buffer))
                       ((eql -1 code) #|remove it|#)
                       (t (vector-push-extend (aref #(- - - - - - -
                                                      #\bell #\backspace #\tab
                                                      #\linefeed #\vt #\page
                                                      #\return)
                                                    code)
                                              buffer))))))
                nil)
               ((:in-octal)
                (flet ((insert-octal ()
                         (setf (aref buffer start) (code-char (parse-integer buffer :start start :radix 8))
                               (fill-pointer buffer) (1+ start)
                               state :in-string)))
                 (case ch
                   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                    (vector-push-extend ch buffer)
                    (when (<= 3 (- (fill-pointer buffer) start))
                      (insert-octal))
                    nil)
                   (otherwise
                    (insert-octal)
                    :again))))
               ((:in-hexa)
                (case ch
                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                        #\a #\b #\c #\d #\e #\f
                        #\A #\B #\C #\D #\E #\F)
                   (vector-push-extend ch buffer)
                   nil)
                  (otherwise
                   (if (< start (fill-pointer buffer))
                       (setf (aref buffer start) (code-char (parse-integer buffer :start start :radix 16))
                             (fill-pointer buffer) (1+ start))
                       (cpp-error "Invalid hexadecimal digit at position ~A" (fill-pointer buffer)))
                   (setf state :in-string)
                   :again))))))
      (loop
         :for ch = (read-char stream)
         :do (loop :while (process-token ch))
         :until (eq state :out)
         :finally (return buffer)))))


(defun write-c-string (string &optional (stream *standard-output*))
  "Prints the string as a C string, with C escape sequences."
  (loop
     :for ch :across string
     :initially (princ "\"" stream)
     :do (princ (case ch
                  ((#\bell)               "\\a")
                  ((#\backspace)          "\\b")
                  ((#\page)               "\\f")
                  ((#\newline
                    #-#.(cl:if (cl:char= #\newline #\linefeed) '(:and) '(:or))
                    #\linefeed) "\\n")
                  ((#\return)             "\\r")
                  ((#\tab)                "\\t")
                  ((#\vt)                 "\\v")
                  ((#\")                  "\\\"")
                  ((#\\)                  "\\\\")
                  (otherwise
                   (if (< (char-code ch) 32)
                       (format nil "\\~3,'0o" (char-code ch))
                       ch))) stream)
     :finally (princ "\"" stream)))


;;;; THE END ;;;;
