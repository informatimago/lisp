;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               array.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    See defpackage documentation string.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-09 <PJB> Added ARRAY-EQUAL-P.
;;;;    2011-08-03 <PJB> Added POSITIONS and VECTOR-DELETE.
;;;;    2006-10-20 <PJB> Added nudge-displaced-vector.
;;;;                     Moved in displaced-vector from
;;;;                     com.informatimago.common-lisp.cesarum.string
;;;;    2005-05-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2012
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


(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
  (:use "COMMON-LISP")
  (:export
   "POSITIONS" ; should go to a sequence package...

   "VECTOR-EMPTYP" "VECTOR-FIRST" "VECTOR-LAST" "VECTOR-REST"
   "VECTOR-BUTLAST" "VECTOR-DELETE"
   "NUDGE-DISPLACED-VECTOR" "DISPLACED-VECTOR"
   
   "ARRAY-TO-LIST" "COPY-ARRAY"
   "ARRAY-EQUAL-P")
  (:documentation
   "
This package exports a few array utility functions.
    

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2005 - 2012
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY")



(defun positions (item vector &key (from-end nil) (test 'eql) (test-not nil) (start 0) (end nil) (count nil) (key 'identity))
  "
RETURN:     A list of indices of the occurences of ITEM in the VECTOR.
            The occurences are defined by the ITEM, TEST, TEST-NOT,
            KEY, START, END, FROM-END and COUNT parameters as in
            DELETE.

EXAMPLE:    (positions 'a #(a door a window a big hole and a bucket) :start 1)
            ==> (2 4 8)
"
  (let ((args (list :key key
                    (if test-not :test-not :test)
                    (if test-not  test-not  test))))
    (if from-end
        (loop
          :with result = '()
          :for cur-end = end :then p
          :for p = (apply (function position) item vector
                          :from-end t
                          :start start :end cur-end
                          args)
          :while (and p (or (null count) (<= 0 (decf count))))
          :do (push p result) 
          :finally (return result))
        (loop
          :for cur-start = start :then (1+ p)
          :for p = (apply (function position) item vector
                          :start cur-start :end end
                          args)
          :while (and p (or (null count) (<= 0 (decf count))))
          :collect p))))



(defun vector-emptyp (vector)
  "
RETURN:  Whether the vector is empty.
"
  (zerop (length vector)))


(defun vector-first (vector)
  "
RETURN: The first element of the vector, or 0 values if empty.
"
  (if (plusp (length vector))
      (aref vector 0)
      (values)))


(defun vector-last (vector)
  "
RETURN: The last element of the vector, or 0 values if empty.
"
  (if (plusp (length vector))
      (aref vector (1- (length vector)))
      (values)))


(defun vector-rest (vector)
  "
RETURN: A displaced, adjustable array, with fill-pointer,  covering all the elements of the VECTOR but the first.
"
  (let* ((emptyp (vector-emptyp vector))
         (size   (if emptyp 0 (1- (length vector)))))
    (make-array size
                :element-type (array-element-type vector)
                :displaced-to vector
                :displaced-index-offset (if emptyp 0 1)
                :adjustable t
                :fill-pointer size)))


(defun vector-butlast (vector)
  "
RETURN: A displaced, adjustable array, with fill-pointer, covering all the elements of the VECTOR but the last.
"
  (let* ((emptyp (vector-emptyp vector))
         (size   (if emptyp 0 (1- (length vector)))))
    (make-array size
                :element-type (array-element-type vector)
                :displaced-to vector
                :displaced-index-offset 0
                :adjustable t
                :fill-pointer size)))


(defun vector-delete (item vector &rest keys &key (from-end nil) (test 'eql) (test-not nil) (start 0) (end nil) (count nil) (key 'identity))
  "
DO:         Delete occurences of ITEM from the VECTOR.  The occurences
            are defined by the ITEM, TEST, TEST-NOT, KEY, START, END,
            FROM-END and COUNT parameters as in DELETE.

            The deletion is performed by moving the elements down, and
            updating the fill-pointer of the VECTOR.  If the VECTOR
            doesn't have a fill-pointer, then DELETE is called
            instead.

RETURN:     VECTOR if it has a fill-pointer, or the result of DELETE
            otherwise.

EXAMPLE:    (let ((v (make-array 10
                            :initial-contents #(a door a window a big hole and a bucket)
                            :fill-pointer t)))
                       (list v (vector-delete 'a v :count 3) (fill-pointer v)))
            --> (#1=#(door window big hole and a bucket) #1# 7)
"
  (declare (ignorable from-end test test-not start end count key))
  (if (array-has-fill-pointer-p vector)
      (let ((positions (apply (function positions) item vector keys)))
        (if positions
            (loop
              :with dst-start = (pop positions)
              :for  src-start = (1+ dst-start) :then (1+ src-end)
              :for  src-end   = (pop positions)
              :for  src-end2  = (or src-end (length vector))
              ;; :do (print (list :dst-start dst-start
              ;;                  :src-start src-start
              ;;                  :src-end src-end
              ;;                  :src-end2 src-end2))
              :do (when (< src-start src-end2)
                    ;; (print `(replace vector vector
                    ;;                  :start1 ,dst-start
                    ;;                  :start2 ,src-start :end2 ,src-end2))
                    (replace vector vector
                             :start1 dst-start
                             :start2 src-start :end2 src-end2)
                    (incf dst-start (- src-end2 src-start))
                    (setf src-start (1+ src-end2)))
              :while src-end
              :finally (progn
                         (setf (fill-pointer vector) dst-start)
                         (return vector)))
            vector))
      ;; No fill pointer, fall back to CL:DELETE:
      (apply (function delete) item vector keys)))



(defun copy-array (array &key copy-fill-pointer copy-adjustable
                   copy-displacement)
  "
RETURN:             A copy of the ARRAY.
ARRAY:              An array.
COPY-FILL-POINTER:  Indicate whether the copy must have the same
                    FILL-POINTER as the ARRAY.
COPY-ADJUSTABLE:    Indicate whether the copy must be an adjustable
                    array when the ARRAY is adjustable.
COPY-DISPLACEMENT:  Indicate whether the copy must be an array
                    displaced to the same array as the ARRAY.
"
  (when copy-displacement
    (multiple-value-bind (disto disoff) (array-displacement array)
      (when disto
        (return-from copy-array
          (make-array (array-dimensions array)
                      :element-type (array-element-type array)
                      :displaced-to disto
                      :displaced-index-offset disoff
                      :adjustable (when copy-adjustable 
                                    (adjustable-array-p array))
                      :fill-pointer (when copy-fill-pointer
                                      (fill-pointer array)))))))
  (let ((copy (make-array (array-dimensions array)
                          :adjustable (when copy-adjustable 
                                        (adjustable-array-p array))
                          :fill-pointer (when copy-fill-pointer
                                          (fill-pointer array))
                          :element-type (array-element-type array))))
    (dotimes (i (array-total-size copy))
      (setf (row-major-aref copy i) (row-major-aref array i)))
    copy))


(defun array-to-list (array)
  "
RETURN:     A list (of list)* containing the elements of the array.

EXAMPLE:    (array-to-list #3A(((1 2 3) (4 5 6)) ((a b c) (d e f))))
            --> (((1 2 3) (4 5 6)) ((a b c) (d e f)))
"
  (if (= 1 (length (array-dimensions array)))
      (coerce array 'list)
      (loop
        :with subarray = (make-array (rest (array-dimensions array))
                                     :adjustable t
                                     :element-type (array-element-type array)
                                     :displaced-to array
                                     :displaced-index-offset 0)
        :for i :from 0 :below (array-dimension array 0)
        :collect (progn
                   (adjust-array subarray (array-dimensions subarray)
                                 :displaced-to array
                                 :displaced-index-offset 
                                 (apply (function *) i
                                        (rest (array-dimensions array))))
                   (array-to-list subarray)))))


(defun displaced-vector (array &optional start end fill-pointer)
  "
DO:         When array is a vector, same as SUBSEQ but with a displaced array.

            When the rank of array is different from 1, then creates
            a displaced vector onto ARRAY.   In that case the optional
            arguments are ignored.

RETURN:     A new displaced vector.
SEE ALSO:   COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY:NSUBSEQ
"
  (if (= 1 (array-rank array))
      (progn
        (setf end (or end  (length array)))
        (assert (<= 0 start (length array)) (start)
                "START should be an integer between 0 and ~D, not ~D" 
                (length array) start)
        (assert (<= start end (length array)) (end)
                "END should be an integer between ~D and ~D, not ~D"
                start (length array) end)
        (make-array  (list (- end start)) 
                     :element-type (array-element-type array)
                     :adjustable t
                     :fill-pointer fill-pointer
                     :displaced-to array 
                     :displaced-index-offset start))
      (make-array (array-total-size array)
                  :element-type (array-element-type array)
                  :displaced-to array)))


(defun nudge-displaced-vector (displaced-vector
                               &key
                               (start   nil startp)
                               (start+  nil start+p)
                               (start-  nil start-p)
                               (end     nil endp)
                               (end+    nil end+p)
                               (end-    nil end-p)
                               (length  nil lengthp)
                               (length+ nil length+p)
                               (length- nil length-p)
                               (fill-pointer nil))
  "
DO:             Changes the displacement of the DISPLACED-VECTOR.
START:          Indicates the new absolute displacement offset.
START+:         Indicates an increment of the displacement offset.
START-:         Indicates a decrement of the displacement offset.
END:            Indicates the new absolute end of the DISPLACED-VECTOR.
END+:           Indicates an increment of the DISPLACED-VECTOR end.
END-:           Indicates a decrement of the DISPLACED-VECTOR end.
LENGTH:         Indicates the new absolute length of the DISPLACED-VECTOR.
LENGTH+:        Indicates an increment of the length of the DISPLACED-VECTOR.
LENGTH-:        Indicates a decrement of the length of the DISPLACED-VECTOR.
FILL-POINTER:   Indicates the new fill pointer for the DISPLACED-VECTOR.
NOTES:          START, START+ and START- are mutually exclusive.
                END, END+, END-, LENGTH, LENGTH+ and LENGTH- are mutually exclusive.
                START and END are expressed as indices in the displaced-to array.
RETURN:         The adjusted array.

EXAMPLE:        (let* ((s #(a door a window a big hole and a bucket))
                            (v (displaced-vector s 0 3 t)))
                       (show v)
                       (show (nudge-displaced-vector v :end+   1))
                       (show (nudge-displaced-vector v :fill-pointer 2))
                       (show (nudge-displaced-vector v :start+ 3 :end+ 3))
                       (show (nudge-displaced-vector v :start- 1 :end- 1))
                       (show (nudge-displaced-vector v :fill-pointer 1)))
                prints:
                    v = #(a door a)
                    (nudge-displaced-vector v :end+ 1) = #(a door a)
                    (nudge-displaced-vector v :fill-pointer 2) = #(a door)
                    (nudge-displaced-vector v :start+ 3 :end+ 3) = #(window a)
                    (nudge-displaced-vector v :start- 1 :end- 1) = #(a window)
                    (nudge-displaced-vector v :fill-pointer 1) = #(a)
                    #(a)
"
  (assert (<= (count-if (function identity) (list startp start+p start-p)) 1))
  (assert (<= (count-if (function identity) (list endp end+p end-p
                                                  lengthp length+p length-p)) 1))
  (multiple-value-bind (vector old-start) (array-displacement displaced-vector)
    (let* ((old-length (array-dimension displaced-vector 0))
           (old-end    (+ old-start old-length))
           (new-start (cond (startp     start)
                            (start+p    (+ old-start start+))
                            (start-p    (- old-start start-))
                            (t          old-start)))
           (new-length (cond (endp      (- end new-start))
                             (end+p     (- (+ old-end end+) new-start))
                             (end-p     (- (- old-end end-) new-start))
                             (lengthp   length)
                             (length+p  (+ old-length length+))
                             (length-p  (- old-length length-))
                             (t         (- old-end new-start))))
           (fill-pointer (if (eq 't fill-pointer)
                             new-length
                             fill-pointer)))
      (adjust-array
       displaced-vector
       (list new-length)
       :element-type (array-element-type vector)
       :fill-pointer (and (array-has-fill-pointer-p displaced-vector)
                          fill-pointer)
       :displaced-to vector
       :displaced-index-offset new-start))))




(defun array-equal-p (a1 a2)
  "RETURN: A1 and A2 have the same dimensions (or the same length in
case of vectors) and their elements in the same position are = if
numbers, or equal otherwise."
  (if (and (vectorp a1) (vectorp a2))
      (and (= (length a1) (length a2))
           (flet ((same (i)
                    (let ((x1 (aref a1 i))
                          (x2 (aref a2 i)))
                      (if (and (numberp x1) (numberp x2))
                          (= x1 x2)
                          (equal x1 x2)))))
             (declare (inline same))
             (loop
               :for i :below (length a1)
               :always (same i))))
      (and (equal (array-dimensions a1) (array-dimensions a2))
           (flet ((same (i)
                    (let ((x1 (row-major-aref a1 i))
                          (x2 (row-major-aref a2 i)))
                      (if (and (numberp x1) (numberp x2))
                          (= x1 x2)
                          (equal x1 x2)))))
             (loop
               :for i :below (array-total-size a1)
               :always (same i))))))

;;;; THE END ;;;;
