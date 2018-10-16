(defpackage "COM.INFORMATIMAGO.COMMON-LISP.TABLE-FILE"
  (:use "COMMON-LISP")
  (:export "CREATE-TABLE-FILE"
           "OPEN-TABLE-FILE"
           "TABLE-FILE-REF"
           "CLOSE-TABLE-FILE"
           "TABLE-FILE"
           "TABLE-FILE-P"
           "TABLE-FILE-VERSION"
           "TABLE-FILE-ROWS"
           "TABLE-FILE-COLS"
           "TABLE-FILE-RECORD-SIZE"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.TABLE-FILE")

(deftype octet () '(unsigned-byte 8))

(defconstant +header-size+ 1024)

(defun write-header (file rows cols record-size)
  (let ((header (make-array +header-size+ 
                           :element-type 'octet
                           :initial-element (char-code #\newline))))
    (replace header (map 'vector 'char-code (prin1-to-string (list :file :table
                                                                   :version 1
                                                                   :rows rows
                                                                   :cols cols
                                                                   :record-size record-size))))
    (file-position file 0)
    (write-sequence header file)))

(defun create-table-file (pathname rows cols record-size)
  (with-open-file (file pathname :element-type 'octet
                                 :direction :io
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
    (write-header file rows cols record-size)
    (let ((row (make-array (* cols record-size)
                           :element-type 'octet
                           :initial-element (char-code #\newline))))
      (file-position file +header-size+)
      (loop :repeat rows
            :do (write-sequence row file)))))

(defstruct table-file
  stream
  version
  rows
  cols
  record-size)

(defun read-header (stream)
  (file-position stream 0)
  (let ((header (make-array +header-size+ :element-type 'octet)))
    (read-sequence header stream)
    (let ((header (read-from-string (map 'string (function code-char) header))))
      (destructuring-bind (&key file version rows cols record-size) header
        (assert (and (eql file :table)
                     (eql version 1)
                     (typep rows '(integer 1))
                     (typep cols '(integer 1))
                     (typep record-size '(integer 1)))
                () "Bad header ~S for file ~S" header (pathname stream))
        (make-table-file :stream stream
                         :version version
                         :rows rows
                         :cols cols
                         :record-size record-size)))))

#|
|--------------------+--------------------|
| :direction         | :input             | 
|                    | :output            |
|                    | :io                |
|--------------------+--------------------|
| :if-does-not-exist | :create            |
|                    | :error             |
|                    | nil                |
|--------------------+--------------------|
| :if-exists         | :error             |
|                    | :new-version       |
|                    | :rename            |
|                    | :rename-and-delete |
|                    | :overwrite         |
|                    | :append            |
|                    | :supersede         |
|                    | nil.               |
|--------------------+--------------------|
|#

(defun open-table-file (pathname &key (direction :input))
  (assert (member direction '(:input :io)))
  (let* ((stream (open pathname :direction direction
                                :if-does-not-exist :error
                                :if-exists :append ; with :io
                                :element-type 'octet)))
     (read-header stream)))

(defun table-file-ref (file row col &optional record)
  (assert (< -1 row (table-file-rows file))
          (row) "row ~A out of bounds 0 .. ~A" row (1-  (table-file-rows file)))
  (assert (< -1 col (table-file-cols file))
          (col) "col ~A out of bounds 0 .. ~A" col (1-  (table-file-cols file)))
  (let ((pos (+ +header-size+
                (* (table-file-record-size file)
                   (+ col (* row (table-file-cols file))))))
        (record (or record
                    (make-array (table-file-record-size file)
                                :element-type 'octet)))
        (stream (table-file-stream file)))
    (file-position stream pos)
    (read-sequence record stream)
    record))

(defun (setf table-file-ref) (new-record file row col)
  (assert (< -1 row (table-file-rows file))
          (row) "row ~A out of bounds 0 .. ~A" row (1-  (table-file-rows file)))
  (assert (< -1 col (table-file-cols file))
          (col) "col ~A out of bounds 0 .. ~A" col (1-  (table-file-cols file)))
  (let ((pos (+ +header-size+
                (* (table-file-record-size file)
                   (+ col (* row (table-file-cols file))))))
        (record (make-array (table-file-record-size file)
                            :element-type 'octet
                            :initial-element (char-code #\newline)))
        (stream (table-file-stream file)))
    (replace record new-record)
    (file-position stream pos)
    (write-sequence record stream)
    record))

(defun close-table-file (file)
  (close (table-file-stream file)))


(defun test/table-file ()
  (let ((path  "/tmp/test.table")
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

    (let ((table (open-table-file path :direction :io))
          (record (make-array rsize :element-type 'octet)))
      (loop :for i :from 1 :below rows
            :do (loop :for j :from 1 :below cols
                      :do (format t "~4D " (read-from-string
                                            (map 'string (function code-char)
                                                 (table-file-ref table i j record)))))
                (terpri))
      (close-table-file table))))

#|
cl-user> (test/table-file)
   1    2    3    4 
   2    4    6    8 
   3    6    9   12 
   4    8   12   16 
   5   10   15   20 
   6   12   18   24 
t
cl-user> 
|#
