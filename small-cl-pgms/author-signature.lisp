;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               author-signature.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     Common-Lisp
;;;;DESCRIPTION
;;;;
;;;;    This program compute an "author signature" from a text.
;;;;    See: http://unix.dsu.edu/~johnsone/comp.html
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2003-03-13 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2003 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.AUTHOR-SIGNATURE"
  (:documentation
   "This program compute an \"author signature\" from a text.
    See: <http://unix.dsu.edu/~johnsone/comp.html>

    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:use "COMMON-LISP")
  (:export compare-texts tally-compare
           tally-word-lengths tally-small-words
           tally-percent  split-words )
  );;COM.INFORMATIMAGO.COMMON-LISP.AUTHOR-SIGNATURE
(in-package "COM.INFORMATIMAGO.COMMON-LISP.AUTHOR-SIGNATURE")


(defun stream-as-string (stream)
  "
RETURN:  A string containing all the character read from the stream.
"
  (loop with result = ""
        with eoln = (format nil "~%")
        for line = (read-line stream nil nil)
        while line
        do (setq result (concatenate 'string result line eoln))
        finally (return result))
  );;STREAM-AS-STRING


(defun remove-ponctuation (text)
  "
RETURN: A copy of the text string where all character not alphanumeric is
        replaced by a space.
"
  (setq text (copy-seq text))
  (loop for i from 0 below (length text)
        for ch = (char text i)
        do (unless (alphanumericp ch) (setf (char text i) #\SPACE)))
  text
  );;REMOVE-PONCTUATION


(defun split-words (text)
  "
RETURN: A list of words read from the text.
"
  (with-input-from-string
   (in (remove-ponctuation text))
   (let ((result  ())
         (ch (read-char in nil nil)))
     (loop while ch do
           (loop while (and ch (eql #\SPACE ch)) ;;skip spaces
                 do (setq ch (read-char in nil nil)))
           (loop while (and ch (not (eql #\SPACE ch)))
                 collect ch into word
                 do (setq ch (read-char in nil nil))
                 finally (when (< 0 (length word))
                           (push (make-array (list (length word))
                                             :element-type 'character
                                             :initial-contents word) result)))
           )
     (nreverse result)))
  ) ;;SPLIT-WORDS


(defun tally-word-lengths (word-list)
  "
RETURN: An array containing the number of words of each length (in
        slot 0 is stored the number of words greater than (length result),
        and (length word-list).
"
  ;; max word length in French: 36.
  (let* ((max-len 36)
         (tally (make-array (list (1+ max-len))
                            :element-type 'fixnum
                            :initial-element 0))
         )
    (loop for word in word-list
          for len = (length word)
          for count = 0 then (1+ count)
          do
          (if (< max-len len)
            (incf (aref tally 0))
            (incf (aref tally len)))
          finally (return (values tally count))))
  );;TALLY-WORD-LENGTHS


(defun tally-small-words (word-list)
  "
RETURN: An array containing the number of occurences of a list of
        small words returned as third value.
        The second value is (length word-list).
"
  (let* ((small-words '("A" "BUT" "IN" "NO" "OUR" "THE" "US"
                        "WE" "WHICH" "WITH"))
         (max-len (length small-words))
         (tally (make-array (list (1+ max-len))
                            :element-type 'fixnum
                            :initial-element 0))
         )
    (loop for word in word-list
          for count = 0 then (1+ count)
          for pos = (position word small-words :test (function string-equal))
          do
          (if pos
            (incf (aref tally (1+ pos)))
            (incf (aref tally 0)))
          finally (return (values tally count small-words))))
  );;TALLY-SMALL-WORDS


;; (TALLY-SMALL-WORDS (SPLIT-WORDS (WITH-OPEN-FILE (IN "~/tmp/misc/test.txt" :DIRECTION :INPUT) (STREAM-AS-STRING IN))))


(defun tally-percent (tally count)
  (let ((result  (make-array (list (length tally))
                             :element-type 'float
                             :initial-element 0.0)))
    (do ((i 0 (1+ i)))
        ((<= (length tally) i) result)
      (setf (aref result i) (coerce (/ (aref tally i) count) 'float))))
  );;TALLY-PERCENT


(defun module (vector)
  "
RETURN:  The module of the vector. [ sqrt(x^2+y^2+z^2) ]
"
  (sqrt (apply (function +)
               (map 'list (function (lambda (x) (* x x))) vector)))
  );;MODULE


(defun tally-compare (tally-1 tally-2)
  "
RETURN:  The module and the vector of percentages of differences
         between vectors tally-1 and tally-2.
"
  (assert (= (length tally-1) (length tally-2)))
  (let ((differences (make-array (list (length tally-1))
                                 :element-type 'float
                                 :initial-element 0.0)))
    (do* ((i 0 (1+ i))
          (d) (m))
        ((<= (length differences) i))
      (setq d (abs (- (aref tally-1 i) (aref tally-2 i)))
            m (max (aref tally-1 i) (aref tally-2 i)))
      (setf (aref differences i) (if (= 0.0 m) m (coerce (/ d m) 'float))) )
    (values (module differences) differences))
  );;TALLY-COMPARE


(defun compare-texts (path-list tally-function)
  (let ((tallies ()))
    (mapc
     (lambda (path)
       (with-open-file (input path  :direction :input)
         (push (cons (namestring path)
                     (multiple-value-bind (tally c)
                         (funcall tally-function
                          (split-words (stream-as-string input)))
                       (tally-percent tally c))) tallies)))
     path-list)
    (do* ((t1 tallies (cdr t1))
          (n-tally-1 (car t1) (car t1))
          (tally-1 (cdr n-tally-1) (cdr n-tally-1)) )
        ((null t1))

      (do* ((t2 (cdr t1) (cdr t2))
            (n-tally-2 (car t2) (car t2))
            (tally-2 (cdr n-tally-2) (cdr n-tally-2)) )
          ((null t2))

          (multiple-value-bind
           (m d) (tally-compare tally-1 tally-2)
           (format t "~20A ~20A ~8A~%   ~A~%~%"
                   (car n-tally-1) (car n-tally-2) m d))
        ))
    tallies)
  );;COMPARE-TEXTS


;; (COMPARE-TEXTS (DIRECTORY "i-*.txt") (FUNCTION TALLY-WORD-LENGTHS))
;; (COMPARE-TEXTS (DIRECTORY "i-*.txt") (FUNCTION TALLY-SMALL-WORDS))

;; (TALLY-COMPARE
;;  (MULTIPLE-VALUE-BIND (TALLY C)
;;      (TALLY-WORD-LENGTHS (SPLIT-WORDS STR))
;;    (TALLY-PERCENT TALLY C))
;;  (MULTIPLE-VALUE-BIND (TALLY C)
;;      (TALLY-WORD-LENGTHS (SPLIT-WORDS STR2))
;;    (TALLY-PERCENT TALLY C)))



;;;; author-signature.lisp            -- 2004-03-14 01:32:40 -- pascal   ;;;;
