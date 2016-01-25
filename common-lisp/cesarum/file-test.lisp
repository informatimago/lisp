;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               file-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests file.list.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from file.list.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE.TEST")


(define-test test/accessors (path)
  (let ((data '(a b c 1 2 3 #(a 1))))
    (setf (sexp-file-contents path) data)
    (assert-true (equalp data (sexp-file-contents path)))
    (push 0 (sexp-file-contents path))
    (assert-true (equalp (cons 0 data) (sexp-file-contents path))))
  (delete-file path)
  (let ((data   "
DO:         Modifies the file at path FILE-NAME, 
            removing the LINE-COUNT first lines.
WARNING:    There's no backup: if the COPY-OVER fails, the file will be left
            in an unspecified state.
"))
    (setf (text-file-contents path) data)
    (assert-true (string= data (text-file-contents path)))
    (setf (text-file-contents path :if-exists :append) "A new line")
    (assert-true (string= (concatenate 'string data "A new line") (text-file-contents path))))
  (delete-file path)
  (let ((data   '(""
                  "DO:         Modifies the file at path FILE-NAME, "
                  "            removing the LINE-COUNT first lines."
                  "WARNING:    There's no backup: if the COPY-OVER fails, the file will be left"
                  "            in an unspecified state."
                  "")))
    (setf (string-list-text-file-contents path) data)
    (assert-true (equal data (string-list-text-file-contents path)))
    (setf (string-list-text-file-contents path :if-exists :append)
          '("A new line" "And another one"))
    (assert-true (equal (append data '("A new line" "And another one"))
                        (string-list-text-file-contents path))))
  (delete-file path)
  (let ((data #(1 2 3 4 5 6 9 0 11 12 13)))
    (setf (binary-file-contents path) data)
    (assert-true (equalp data (binary-file-contents path)))
    (setf (binary-file-contents path :if-exists :append) data)
    (assert-true (equalp (concatenate 'vector data data) (binary-file-contents path))))
  (delete-file path))


(define-test test/all ()
  (test/accessors #P"/tmp/file-test.data"))


;;;; THE END ;;;;

