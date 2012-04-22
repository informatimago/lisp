;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               array.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports some array utility functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;****************************************************************************


(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
  (:use "COMMON-LISP")
  (:export
   "POSITIONS" ; should go to a sequence package...
   "VECTOR-DELETE"
   "NUDGE-DISPLACED-VECTOR" "DISPLACED-VECTOR"
   "ARRAY-TO-LIST" "COPY-ARRAY")
  (:documentation
   "Array functions.
    
    Copyright Pascal J. Bourguignon 2005 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY")



(defun positions (item vector &key (from-end nil) (test 'eql) (test-not nil) (start 0) (end nil) (count nil) (key 'identity))
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


(defun vector-delete (item vector &rest keys &key (from-end nil) (test 'eql) (test-not nil) (start 0) (end nil) (count nil) (key 'identity))
  (declare (ignorable from-end test test-not start end count key))
  (if (array-has-fill-pointer-p vector)
      (let ((positions (apply (function positions) item vector keys)))
        (if (print positions)
            (loop
               :with dst-start = (pop positions)
               :for  src-start = (1+ dst-start) :then (1+ src-end)
               :for  src-end   = (pop positions)
               :for  src-end2  = (or src-end (length vector))
               :do (print (list :dst-start dst-start
                                :src-start src-start
                                :src-end src-end
                                :src-end2 src-end2))
               :do (when (< src-start src-end2)
                     (print `(replace vector vector
                                      :start1 ,dst-start
                                      :start2 ,src-start :end2 ,src-end2))
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
  "RETURN: a list (of list)* containing the elements of the array."
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


(defun displaced-vector (vector start &optional end &key (fill-pointer nil))
  "Same as SUBSEQ but with a displaced array."
  (setf end (or end  (length vector)))
  (assert (<= 0 start (length vector)) (start)
          "START should be an integer between 0 and ~D, not ~D" 
          (length vector) start)
  (assert (<= start end (length vector)) (end)
          "END should be an integer between ~D and ~D, not ~D"
          start (length vector) end)
  (make-array  (list (- end start)) 
               :element-type (array-element-type vector)
               :adjustable t
               :fill-pointer fill-pointer
               :displaced-to vector 
               :displaced-index-offset start))

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
  (assert (<= (count-if (function identity) (list startp start+p start-p)) 1))
  (assert (<= (count-if (function identity) (list endp end+p end-p
                                                  lengthp length+p length-p)) 1))
  (multiple-value-bind (vector old-start) (array-displacement displaced-vector)
    (let* ((old-length (length displaced-vector))
           (old-end    (+ old-start old-length))
           (new-start (cond (startp     start)
                            (start+p    (+ old-start start+))
                            (start-p    (+ old-start start-))
                            (t          old-start)))
           (new-length (cond (endp      (- end new-start))
                             (end+p     (- (+ old-end end+) new-start))
                             (end-p     (- (- old-end end-) new-start))
                             (lengthp   length)
                             (length+p  (+ old-length length+))
                             (length-p  (- old-length length-))
                             (t         (- old-end new-start)))))
      (adjust-array
       displaced-vector
       (list new-length)
       :element-type (array-element-type vector)
       :fill-pointer (and (array-has-fill-pointer-p displaced-vector)
                          fill-pointer)
       :displaced-to vector
       :displaced-index-offset new-start))))



;;;; array.lisp                       --                     --          ;;;;
