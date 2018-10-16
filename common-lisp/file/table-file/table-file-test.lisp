;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               table-file-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests the table-file file access method.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-10-16 <PJB> Extracted from table-file.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
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

(in-package "COM.INFORMATIMAGO.COMMON-LISP.FILE.TABLE-FILE")

(defun test/table-file ()
  (let ((path  "/tmp/test-txt.table")
        (path2 "/tmp/test-bin.table")
        (rows   7)
        (cols   5)
        (rsize 64))

    (create-table-file path rows cols rsize)

    (let ((table (open-table-file path :direction :io))
          (record (make-array rsize :element-type 'octet)))
      (loop :for i :from 1 :below rows
            :do (loop :for j :from 1 :below cols
                      :for value :=  (* i j)
                      :do (replace record (map 'vector (function char-code) (format nil "~D~%" value)))
                          (setf (table-file-ref table i j) record)))
      (close-table-file table))

    (let ((table (open-table-file path :direction :io))
          (record (make-array rsize :element-type 'octet)))
      (loop :for i :from 1 :below rows
            :do  (replace record (map 'vector (function char-code) (format nil "~D~%" i)))
                 (setf (table-file-ref table i 0) record))
      (loop :for j :from 1 :below cols
            :do  (replace record (map 'vector (function char-code) (format nil "~D~%" j)))
                 (setf (table-file-ref table 0 j) record))
      (loop :for i :from 1 :below rows
            :do (loop :for j :from 1 :below cols
                      :for value :=  (* i j)
                      :do (assert (eql value (read-from-string
                                              (map 'string (function code-char)
                                                   (table-file-ref table i j record)))))))
      (close-table-file table))

    (flet ((dump-table (path deserializer)
             (let* ((table (open-table-file path :direction :input :deserializer deserializer))
                    (record (make-array (table-file-record-size table) :element-type 'octet)))
               (loop :for i :from 1 :below (table-file-rows table)
                     :do (loop :for j :from 1 :below (table-file-cols table)
                               :do (format t "~4D " (table-file-ref table i j record)))
                         (terpri))
               (close-table-file table))))

      (format t "~%Stored as text:~%")
      (dump-table path (lambda (record)
                         (read-from-string (map 'string (function code-char) record))))


      (let ((rsize 4))
        (flet ((serializer (integer)
                 (loop
                   :with record := (make-array rsize :element-type 'octet)
                   :for i :below (length record)
                   :do (setf (aref record i) (ldb (byte 8 (* 8 i)) integer))
                   :finally (return record)))
               (deserializer  (record)
                 (loop
                   :with integer := 0
                   :for i :below (length record)
                   :do (setf integer (dpb (aref record i) (byte 8 (* 8 i)) integer))
                   :finally (return integer))))
          (create-table-file path2 rows cols rsize)
          (let ((table (open-table-file path2
                                        :direction :io
                                        :serializer (function serializer)
                                        :deserializer (function deserializer)))
                (record (make-array rsize :element-type 'octet)))
            (loop :for i :from 1 :below rows
                  :do (setf (table-file-ref table i 0) i))
            (loop :for j :from 1 :below cols
                  :do  (setf (table-file-ref table 0 j) j))
            (loop :for i :from 1 :below rows
                  :do (loop :for j :from 1 :below cols
                            :do (setf (table-file-ref table i j record) (* i j))))
            (loop :for i :from 1 :below rows
                  :do (loop :for j :from 1 :below cols
                            :do (assert (= (table-file-ref table i j record) (* i j)))))
            (close-table-file table)
            (format t "~%Stored as binary:~%")
            (dump-table path2 (function deserializer))))))))

#|
cl-user> (test/table-file)
Stored as text:
   1    2    3    4 
   2    4    6    8 
   3    6    9   12 
   4    8   12   16 
   5   10   15   20 
   6   12   18   24 

Stored as binary:
   1    2    3    4 
   2    4    6    8 
   3    6    9   12 
   4    8   12   16 
   5   10   15   20 
   6   12   18   24 
t
cl-user>
|#

(defun test/all ()
  (test/table-file)
  :success)

;;;; THE END ;;;;
