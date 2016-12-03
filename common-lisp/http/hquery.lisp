;;;;**************************************************************************
;;;;FILE:               hquery.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This packages exports utility routines for web applications.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-05-30 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2007 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.HTTP.HQUERY"
  (:use "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "QUERY-ESCAPE" "QUERY-UNESCAPE" "QUERY-PARSE" "QUERY-ARGUMENT"
           "BUILD-QUERY")
  (:documentation
   "
This packages exports utility routines for web applications.


License:

    AGPL3

    Copyright Pascal J. Bourguignon 2007 - 2012

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.HTTP.HQUERY")


(defun query-escape (string)
  "
RETURN:     A string where the spaces are replaced by '+'
            and other unsavory characters are replaced by %HL sequences.
"
  (do ((result (make-string (* 3 (length string))))
       (i 0)
       (j 0))
      ((<= (length string) i) (subseq result 0 j))
    (cond
      ((char= (character " ") (char string i))
       (setf (char result j) (character "+")) (incf j) (incf i))
      ((position
        (char string i)
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
       (setf (char result j) (char string i)) (incf j) (incf i))
      (t
       (let ((escape (format nil "%~16,2R" (char-code (char string i)))))
         (setf (char result j) (char escape 0)) (incf j)
         (setf (char result j) (char escape 1)) (incf j)
         (setf (char result j) (char escape 2)) (incf j)
         (incf i))))))


(defun query-unescape (qpart)
  "
RETURN:     A string where the + are replaced by spaces
            and the %HL are replaced by the caracter whose code is HL
            in hexadecimal.
"
  (do ((result (make-string (length qpart)))
       (i 0)
       (j 0))
      ((<= (length qpart) i) (subseq result 0 j))
    (cond
      ((char= (character "+") (char qpart i))
       (setf (char result j) (character " "))
       (incf j)
       (incf i))
      ((and (char= (character "%") (char qpart i))
            (< (+ i 2) (length qpart)))
       (let ((code (parse-integer qpart :start (+ i 1) :end (+ i 3) :radix 16)))
         (cond
           ((null code)
            (setf (char result j) (char qpart i)) (incf j) (incf i))
           ((= 13 code)
            (incf i 3))
           (t
            (setf (char result j) (code-char code)) (incf j) (incf i 3))) ))
      (t
       (setf (char result j) (char qpart i)) (incf j) (incf i)))))


(defun query-parse (query-string)
  "
RETURN:  the QUERY-ARGUMENTS, a list of lists (variable . value)
         found in the HTML CGI QUERY-STRING.
"
  (mapcar
   (lambda (ass)
     (let* ((posep (position (character "=") ass))
            (var (if posep (subseq ass 0 posep) ass))
            (val (if posep (subseq ass (1+ posep)) "")))
       (cons (query-unescape var) (query-unescape val))))
   (split-string query-string "&")))


(defun query-argument (name query-arguments)
  "
RETURN:  The value of the QUERY-STRING argument named NAME.
"
  (cdr (assoc name query-arguments :test (function string-equal))))


(defun build-query (&rest field-value)
  "
RETURN:     A formated string containing the field=value&field=value…
EXAMPLE:    (build-query :first-name \"john\" :surname \"doe\")
            --> \"first%2Dname=john&surname=doe\"
"
  (format nil "~{~A=~A~^&~}"
          (mapcar (lambda (item) (query-escape (if (stringp item)
                                                   item
                                                   (format nil "~A" item))))
                  field-value)))


;;;; THE END ;;;;
