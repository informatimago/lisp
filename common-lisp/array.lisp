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
;;;;    2006-10-20 <PJB> Added nudge-displaced-vector.
;;;;                     Moved in displaced-vector from
;;;;                     com.informatimago.common-lisp.string
;;;;    2005-05-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************


(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ARRAY"
  (:USE "COMMON-LISP")
  (:EXPORT "NUDGE-DISPLACED-VECTOR" "DISPLACED-VECTOR" "ARRAY-TO-LIST"
           "COPY-ARRAY")
  (:DOCUMENTATION
   "Array functions.
    
    Copyright Pascal J. Bourguignon 2005 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ARRAY")




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


(DEFUN DISPLACED-VECTOR (VECTOR START &OPTIONAL END &key (fill-pointer nil))
  "Same as SUBSEQ but with a displaced array."
  (SETF END (OR END  (LENGTH VECTOR)))
  (ASSERT (<= 0 START (LENGTH VECTOR)) (START)
          "START should be an integer between 0 and ~D, not ~D" 
          (LENGTH VECTOR) START)
  (ASSERT (<= START END (LENGTH VECTOR)) (END)
          "END should be an integer between ~D and ~D, not ~D"
          START (LENGTH VECTOR) END)
  (MAKE-ARRAY  (LIST (- END START)) 
               :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE VECTOR)
               :adjustable t
               :fill-pointer fill-pointer
               :DISPLACED-TO VECTOR 
               :DISPLACED-INDEX-OFFSET START))

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
