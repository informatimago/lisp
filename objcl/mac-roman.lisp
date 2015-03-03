;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mac-roman.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a function to check if a character is in the
;;;;    Mac-Roman repertoire.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-04-07 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2011 - 2015
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

(in-package "COM.INFORMATIMAGO.OBJCL.MAC-ROMAN")
;; or we could depend on "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS"


#+(and developing clisp)
(defvar *mac-roman-codes*
 (map 'list 'char-code
      (sort (ext:convert-string-from-bytes
             (coerce
              (set-difference (com.informatimago.common-lisp.cesarum.list:iota 256)
                              '(#xf0)) 'vector)
             charset:mac-roman)
            (function char<))))

#+developing
(defun segments (list)
  (loop
     :while list
     :collect (loop
                 :with start = (pop list)
                 :with end = start
                 :while (and list (= (1+ end) (car list)))
                 :do (setf end (pop list))
                 :finally (return (if (= start end)
                                      start
                                      (cons start end))))))



(defun dichotomy-search (vector value compare &key
                         (start 0) (end (length vector))
                         (key (function identity)))
  ;; From com.informatimago.common-lisp.cesarum.utility
  "
PRE:	entry is the element to be searched in the table.
        (<= start end)
RETURN: (values found index order)
POST:	(<= start index end)
        +-------------------+----------+-------+----------+----------------+
        | Case              |  found   | index |  order   |     Error      |
        +-------------------+----------+-------+----------+----------------+
        | x < a[min]        |   FALSE  |  min  |  less    |      0         |
        | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |      0         |
        | x = a[i]          |   TRUE   |   i   |  equal   |      0         |
        | a[max] < x        |   FALSE  |  max  |  greater |      0         |
        +-------------------+----------+-------+----------+----------------+
"
  (let* ((curmin start)
         (curmax end)
         (index    (truncate (+ curmin curmax) 2))
         (order  (funcall compare value (funcall key (aref vector index)))) )
    (loop :while (and (/= 0 order) (/= curmin index)) :do
       ;; (FORMAT T "~&min=~S  cur=~S  max=~S   key=~S <~S> [cur]=~S ~%" CURMIN INDEX CURMAX VALUE (FUNCALL COMPARE VALUE (FUNCALL KEY (AREF VECTOR INDEX))) (AREF VECTOR INDEX))
       (if (< order 0)
           (setf curmax index)
           (setf curmin index))
       (setf index (truncate (+ curmin curmax) 2))
       (setf order  (funcall compare value (funcall key (aref vector index)))))
    (when (and (< start index) (< order 0))
      (setf order 1)
      (decf index))
    #+testing
    (assert
     (or (< (funcall compare value (funcall key (aref vector index))) 0)
         (and (> (funcall compare value (funcall key (aref vector index))) 0)
              (or (>= (1+ index) end)
                  (< (funcall compare value
                              (funcall key (aref vector (1+  index)))) 0)))
         (= (funcall compare value (funcall key (aref vector index))) 0)))
    (values (= order 0) index order)))



(defun mac-roman-char-p (ch)
  "Whether the CH is a Mac-Roman characters."
  (let ((code (char-code ch)))
    (or (<= 0 code 127) ; an optimization, 95% of the characters are ASCII.
        (dichotomy-search
         ;; Computed as (coerce (segments *mac-roman-codes*))
         ;; This vector represents the set of unicodes in the MacRoman character
         ;; set.  It is a sorted vector of segments (min . max) or single codes.
         #((0 . 127) (160 . 165) (167 . 172) (174 . 177) (180 . 184)
           (186 . 187) (191 . 207) (209 . 214) (216 . 220) (223 . 239)
           (241 . 252) 255 305 (338 . 339) 376 402 (710 . 711)
           (728 . 733) 960 (8211 . 8212) (8216 . 8218) (8220 . 8222)
           (8224 . 8226) 8230 8240 (8249 . 8250) 8260 8482 8486 8706
           8710 8719 8721 8730 8734 8747 8776 8800 (8804 . 8805) 9674
           (64257 . 64258))
         code
         (lambda (code segment)
           (if (consp segment)
               (cond ((< code (car segment)) -1)
                     ((< (cdr segment) code) +1)
                     (t                       0))
               (cond ((< code segment)       -1)
                     ((< segment code)       +1)
                     (t                       0))))))))


(defun mac-roman-string-p (string)
  "Whether the STRING contains only Mac-Roman characters."
  (every (function mac-roman-char-p) string))


;;;; THE END ;;;;
