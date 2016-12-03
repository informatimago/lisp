;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               sparse-vector.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implement a sparse vector abstraction, allowing for big
;;;;    vectors, optimizing storage from empty to full vector.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-03-23 <PJB> Created.
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SPARSE-VECTOR"
  (:use "COMMON-LISP")
  (:export
   ;; Conditions:
   "OUT-OF-BOUNDS" "OUT-OF-BOUNDS-VECTOR" "OUT-OF-BOUNDS-INDEX"
   ;; Generic interface:
   "VECTOR-LENGTH" "VECTOR-ELEMENT-TYPE" "VECTOR-REF" "VECTOR-ELEMENTS"
   "VECTOR-COUNT" "VECTOR-STORAGE-SIZE"
   ;; Vector-tree:
   "MAKE-VECTOR-TREE" "VECTOR-TREE" "VECTOR-TREE-P"
   ;; Sparse-vector:
   "MAKE-SPARSE-VECTOR"
   "SPARSE-VECTOR"
   "SPARSE-VECTOR-P"
   "SPARSE-VECTOR-COUNT"
   "SPARSE-VECTOR-LENGTH"
   "SPARSE-VECTOR-MINIMUM-INDEX"
   "SPARSE-VECTOR-MAXIMUM-INDEX"
   "SPARSE-VECTOR-REF"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SPARSE-VECTOR")


;;;
;;; Conditions
;;;

(define-condition out-of-bounds (error)
  ((vector :initarg :vector :reader out-of-bounds-vector)
   (index  :initarg :index  :reader out-of-bounds-index))
  (:report (lambda (stream condition)
             (let ((vector (out-of-bounds-vector condition)))
               (format stream "Index ~D is out of bounds for the ~S of size ~D"
                       (out-of-bounds-index condition)
                       (let ((type (type-of vector)))
                         (if (listp type)
                             (first type)
                             type))
                       (vector-length vector))))))


;;;
;;; Generic interface:
;;;

(defgeneric vector-length (vector)
  (:documentation "RETURN: the length of the vector.")
  (:method ((v vector))
    (length v)))


(defgeneric vector-count (vector)
  (:documentation "RETURN: the number of elements in the vector that are not the default element.")
  (:method ((v vector))
    (length v)))


(defgeneric vector-storage-size (vector)
  (:documentation "RETURN: a number proportional to the storage used by the vector.")
  (:method ((v vector))
    (length v)))


(defgeneric vector-element-type (vector)
  (:documentation "RETURN: the element-type of the vector.")
  (:method ((v vector))
    (array-element-type v)))


(defgeneric vector-default-element (vector)
  (:documentation "
RETURN: the default element of the vector.

NOTE:   It is the INITIAL-ELEMENT.
"))


(defgeneric vector-ref (vector index)
  (:documentation "
RETURN: the element in the VECTOR at the given INDEX.

SIGNAL: OUT-OF-BOUNDS error if INDEX is negative or not less than the
        vector length.
")
  (:method :before (v index)
    (when (or (minusp index) (<= (vector-tree-length v)))
      (error 'out-of-bounds :vector v :index index)))
  (:method ((v vector) index)
    (aref v index)))


(defgeneric (setf vector-ref) (new-value vector index)
  (:documentation "
DO:     Sets the VECTOR element at INDEX to be the NEW-VALUE.

RETURN: NEW-VALUE

SIGNAL: OUT-OF-BOUNDS error if INDEX is negative or not less than the
        vector length.

        TYPE-ERROR if NEW-VALUE is not of the vector element-type.
")
  (:method :before (new-value v index)
    (when (or (minusp index) (<=  (vector-length v) index))
      (error 'out-of-bounds :vector v :index index))
    (unless (typep new-value (vector-element-type v))
      (error 'type-error :datum new-value :expected-type (vector-element-type v))))
  (:method (new-value (v vector) index)
    (setf (aref v index) new-value)))


(defgeneric vector-elements (vector)
  (:documentation "
RETURN: a CL:VECTOR of same element-type and same length
        containing all the elements of the vector.
")
  (:method ((v vector))
    v))




;;;
;;; Vector tree
;;;


(defvar %none% '#:none "Internal value used in place of unallocated nodes in vector trees.")
(defstruct (vector-tree
            (:constructor %make-vector-tree)
            (:copier nil))
  length
  element-type
  default-element
  node-size
  root-subnode-width
  root)


(defun make-vector-tree (node-size length &key (element-type t) (initial-element nil))
  "
RETURN: A new VECTOR-TREE

NOTE:   The new vector-tree is empty, which means that all slot have
        as value the initial-element (saved in vector-tree-default-element),
        and only the root node (a vector of size NODE-SIZE) is allocated.
"
  (%make-vector-tree :node-size node-size
                     :length length
                     :element-type element-type
                     :default-element initial-element
                     :root-subnode-width (expt node-size (1- (ceiling (log length node-size))))
                     :root (make-array node-size :initial-element %none%)))

(defmethod vector-length ((v vector-tree))
  (vector-tree-length v))


(defmethod vector-element-type ((v vector-tree))
  (vector-tree-element-type v))


(defmethod vector-default-element ((v vector-tree))
  (vector-tree-default-element v))


(defmethod vector-count ((v vector-tree))
  (let ((node-size (vector-tree-node-size v))
        (default   (vector-default-element v)))
    (labels ((count-elements (node subnode-size)
               (cond
                 ((eql node %none%)  0)
                 ((= subnode-size 1) (count default node :test-not (function eql)))
                 (t (let ((subnode-size (/ subnode-size node-size)))
                      (loop :for subnode :across node
                            :sum (count-elements subnode subnode-size)))))))
      (count-elements (vector-tree-root v)
                      (vector-tree-root-subnode-width v)))))


(defmethod vector-storage-size ((v vector-tree))
  (let ((node-size (vector-tree-node-size v)))
    (labels ((storage-size (node subnode-size)
               (cond
                 ((eql node %none%)  1)
                 ((= subnode-size 1) (length node))
                 (t (let ((subnode-size (/ subnode-size node-size)))
                      (+ (length node)
                         (loop :for subnode :across node
                               :sum (storage-size subnode subnode-size))))))))
      (storage-size (vector-tree-root v)
                    (vector-tree-root-subnode-width v)))))


(defmethod vector-ref ((v vector-tree) index)
  (let ((node-size  (vector-tree-node-size v)))
    (labels ((node-ref (node subnode-width index)
               (let* ((subindex (truncate index subnode-width))
                      (val      (aref node subindex)))
                 (cond ((eql val %none%)    (vector-default-element v))
                       ((= 1 subnode-width) val)
                       (t (node-ref val
                                    (/ subnode-width node-size)
                                    (- index (* subindex subnode-width))))))))
      (node-ref (vector-tree-root v) (vector-tree-root-subnode-width v) index))))


(defmethod (setf vector-ref) (new-value (v vector-tree) index)
  (let ((node-size  (vector-tree-node-size v))
        (default    (vector-default-element v)))
    (labels ((node-set (node subnode-width index)
               (if (= 1 subnode-width)
                   (progn
                     (setf (aref node index) new-value)
                     (if (and (eql new-value default)
                              (every (lambda (element) (eql element default)) node))
                         %none%
                         new-value))
                   (let* ((subindex (truncate index subnode-width))
                          (subnodes (aref node subindex)))
                     (when (eql subnodes %none%) ; so far
                       (setf subnodes
                             (setf (aref node subindex)
                                   (if (= node-size subnode-width)
                                       (make-array node-size
                                                   :element-type (vector-tree-element-type v)
                                                   :initial-element default)
                                       (make-array node-size
                                                   :element-type t
                                                   :initial-element %none%)))))
                     (let ((result (node-set subnodes
                                             (/ subnode-width node-size)
                                             (- index (* subindex subnode-width)))))
                       (if (eql result %none%)
                           (progn
                             (setf (aref node subindex) %none%)
                             (if (every (lambda (element) (eql element default)) node)
                                 %none%
                                 new-value))
                           result))))))
      (node-set (vector-tree-root v) (vector-tree-root-subnode-width v) index))))


(defmethod vector-elements ((v vector-tree))
  (let* ((node-size (vector-tree-node-size v))
         (length    (vector-length v))
         (result    (make-array length
                                :element-type (vector-element-type v)
                                :initial-element (vector-default-element v)))
         (pos       0)
         (default   (vector-default-element v)))
    (labels ((node-copy (node subnode-width)
               (if (= 1 subnode-width)
                   (progn
                     (replace result node :start1 pos)
                     (incf pos node-size))
                   (loop
                     :for subnode :across node
                     :while (< pos length)
                     :do (if (eql subnode %none%)
                             (progn
                               (fill result default :start pos :end (min length (+ pos subnode-width)))
                               (incf pos subnode-width))
                             (node-copy subnode (/ subnode-width node-size)))))))
      (node-copy (vector-tree-root v) (vector-tree-root-subnode-width v))
      result)))


;;;
;;; Sparse vector
;;;

;; 1114112

(defstruct (sparse-vector
            (:constructor %make-sparse-vector))
  data
  length
  count
  default-element
  minimum-index
  maximum-index)

(setf (documentation 'sparse-vector-p 'function)
      "RETURN: whether the object is a SPARSE-VECTOR."
      (documentation 'sparse-vector-length 'function)
      "RETURN: The size of the sparse vector."
      (documentation 'sparse-vector-count 'function)
      "RETURN: The number of non-default-element elements in the sparse vector."
      (documentation 'sparse-vector-default-element 'function)
      "RETURN: The default-element of the sparse vector."
      (documentation 'sparse-vector-minimum-index 'function)
      "RETURN: The index of the first non-default-element in the sparse vector."
      (documentation 'sparse-vector-maximum-index 'function)
      "RETURN: The index of the last non-default-element in the sparse vector.")

(defmethod vector-length ((v sparse-vector))
  (sparse-vector-length v))

(defun make-sparse-vector (size &key initial-element)
  "
RETURN: a new sparse vector, able to contain up to SIZE elements,
        filled with the INITIAL-ELEMENT value as default sparse value.
"
  (%make-sparse-vector :data nil
                       :length size
                       :count 0
                       :default-element initial-element
                       :minimum-index -1
                       :maximum-index -1))

#|

While count is less than 5-35 we use an alist.

node size overhead     collection size overhead
vector:           1/1                        3
llrbtree:         4/5                        3
a-list:           1/2                        0
hashtable:        1/2 (ccl)                 80 (ccl (default))
n-tree:           1/n                        n

|#

(defun make-k-tree-size-function (k)
  (lambda (count fill-ratio)
    (declare (ignore fill-ratio))
    (let ((h (log count k)))
      (values (* k (truncate (1- (expt k h))
                             (1- k)))
              h))))


(defparameter *structures*
  ;;                node size        collection size
  ;;                overhead                overhead
  ;;                       access time fn
  ;;                       number of collections fn
  `((vector           ,(lambda (count fill-ratio)
                         (declare (ignore fill-ratio))
                         (values (+ 3 count) 1)))
    (llrbtree         ,(lambda (count fill-ratio)
                         (values (+ 3 (* 5 (* count fill-ratio))) (log count 2))))
    (a-list           ,(lambda (count fill-ratio)
                         (values (* 2 (* count fill-ratio)) count)))
    (hashtable        #+ccl ,(lambda (count fill-ratio)
                               (values (+ 80 (* 2 (* count fill-ratio))) 1))
                      #-ccl ,(lambda (count fill-ratio)
                               (values (+ 100 (* 2 count)) 1)))
    (2-tree           ,(make-k-tree-size-function  2))
    (4-tree           ,(make-k-tree-size-function  4))
    (8-tree           ,(make-k-tree-size-function  8))
    (16-tree          ,(make-k-tree-size-function 16))
    (32-tree          ,(make-k-tree-size-function 32))
    (64-tree          ,(make-k-tree-size-function 64))))


(defun best-structure (block-size fill-ratio)
  (let ((nodes (* fill-ratio block-size)))
    (declare (ignore nodes))
    (sort (map 'vector (lambda (struct)
                         (destructuring-bind (name size-fn) struct
                           (cons name
                                 (multiple-value-list (funcall size-fn block-size fill-ratio)))))
            *structures*)
          (function <)
          :key (function second))))

#-(and)
(map nil 'print (best-structure 1000000 0.3))



(defun local-density (vector page-size default-element)
  (let ((density (make-array (ceiling (length vector) page-size) :initial-element 0)))
    (loop
      :for i :below (length density)
      :do (setf (aref density i)
                (/ (loop :for j :from (* i page-size) :below (min (length vector) (* (1+ i) page-size))
                         :sum (if (eql default-element (aref vector j)) 0 1)) page-size)))
    density))


#-(and)
(map 'vector (lambda (x) (if (< 1/6 x) 1 0))
  (local-density #(0 0 1 1               ; 2/4
                   0 0 0 0
                   0 0 1 2               ; 2/4
                   0 0 0 0
                   1 3 0 4               ; 3/4
                   0 0 0 0
                   0 0 0 0
                   0 0 0 5               ; 1/4
                   0 0 0 0
                   0 0 0 0)
                 6 0))



(defun get-cell (sparse-vector index)
  (unless (<= 0 index (1- (sparse-vector-length sparse-vector)))
    (error 'out-of-bounds :vector sparse-vector :index index))

  (cond

    ()
    ((not (<= (sparse-vector-minimum-index sparse-vector)
              index
              (sparse-vector-maximum-index sparse-vector)))
     (error "~S: out of bound index ~D length: ~D"
            'sparse-vector-ref index (sparse-vector-length sparse-vector)))))






(defun sparse-vector-ref (sparse-vector index)
  "RETURN: the value at the given index; whether it was not a default value.
SIGNALS: out"
  (let ((cell (get-cell sparse-vector index)))
    (if cell
        (values (cell-value cell) t)
        (values (sparse-vector-default-element sparse-vector) nil))))

(defun (setf sparse-vector-ref) (new-value sparse-vector index)
  (if (eql new-value (sparse-vector-default-element sparse-vector))
      (remove-cell sparse-vector index)
      (let ((cell (get-cell-create sparse-vector index)))
        (setf (cell-value cell) new-value))))

;;;; THE END ;;;;
