;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               set.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines an abstract SET class API.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-05-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:shadow "MERGE" "INTERSECTION" "UNION")
  (:export
   "CONTAINS" "CARDINAL" "EMPTYP" "MINIMUM" "MAXIMUM"
   "COLLECTING-RESULT" "MAKE-COLLECTOR" "MAP-ELEMENTS" "THEREIS"
   "THEREIS1" "ALWAYS" "SET-EQUAL" "IS-SUBSET" "IS-STRICT-SUBSET"
   "INTENSION" "COPY" "UNION" "INTERSECTION" "DIFFERENCE"
   "SYMETRIC-DIFFERENCE" "INCLUDE" "EXCLUDE" "ASSIGN-EMPTY"
   "ASSIGN-SINGLETON" "ASSIGN" "MERGE" "INTERSECT" "SUBTRACT")
  (:export "LIST-SET" "ELEMENTS")
  (:documentation
   "

This package defines an abstract set class API.

The minimum implementation should define methods for: INCLUDE,
EXCLUDE, CONTAINS, CARDINAL, SELECT, MINIMUM, MAXIMUM, MAP-ELEMENTS
and MAKE-COLLECTOR.

But an efficient implementation will have to implement specializations
for the other generic functions too.

Methods of MAKE-COLLECTOR specify which RESULT-TYPE sets are
available.  Methods are defined for NIL, LIST and VECTOR,  to make
null collector (ignoring the collected elements), a list collector or
a vector collector.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2013 - 2013
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET")


(defgeneric contains              (set element)
  (:documentation "
RETURN: Whether the SET contains the ELEMENT.
")
  (:method ((set sequence) element)
    (find element set)))


(defgeneric cardinal              (set)
  (:documentation "
RETURN: The number of elements in the SET.
NOTE:   We only consider finite sets.
")
  (:method ((set sequence))
    (length set)))


(defgeneric emptyp              (set)
  (:documentation "
RETURN: (zerop (cardinal set))
NOTE:   Implementations of EMPTYP may be more efficient than CARDINAL.
")
  (:method (set)
    (zerop (cardinal set)))
  (:method ((set null))
    t)
  (:method ((set cons))
    nil))


(defgeneric select                (set)
  (:documentation "
PRE:    (not (emptyp SET))
RETURN: one element from the SET.
"))


;; When the elements are ordered:

(defgeneric minimum               (set)
  (:documentation "
PRE:    (not (emptyp SET))
RETURN: the smallest element of the SET.
"))

(defgeneric maximum               (set)
  (:documentation "
PRE:    (not (emptyp SET))
RETURN: the biggest element of the SET.
"))



;; result-type:
;;   empty-result: --> set
;;   include: set element -> set


(defgeneric make-collector        (result-type)
  (:documentation "
RETURN: A collector for the RESULT-TYPE.

        A collector is a function that takes optionnaly two arguments,
        a set and an element.

        When called with no argument, it should return a fresh empty
        set object.

        When called with a set and an element argument, it should
        include the element into the set, and return the (possibly
        new) set.
")
  (:method ((result-type (eql 'nil)))
    (declare (ignore result-type))
    (lambda (&optional set element)
      (declare (ignore set element))
      (values)))
  (:method ((result-type (eql 'list)))
    (declare (ignorable result-type))
    (lambda (&optional set (element nil add-element-p))
      (if add-element-p
          (if (member element set)
              set
              (cons element set))
          '())))
  (:method ((result-type (eql 'vector)))
    (declare (ignorable result-type))
    (lambda (&optional set (element nil add-element-p))
      (if add-element-p
          (progn
            (unless (find element set)
              (vector-push-extend element set (length set)))
            set)
          (make-array 2 :adjustable t :fill-pointer 0)))))


(defmacro collecting-result ((collect-operator-name result-type) &body body)
  "
DO:     Evaluate BODY in an environment where a function named by
        COLLECT-OPERATOR-NAME is defined to take one argument and to
        add it to a set of type RESULT-TYPE.

RETURN: The collected set of elements.
"
  (let ((collector (gensym))
        (result    (gensym)))
    `(let* ((,collector (make-collector ,result-type))
            (,result (funcall ,collector)))
       (flet ((,collect-operator-name (element)
                (setf ,result (funcall ,collector ,result element))))
         ,@body)
       ,result)))


(defgeneric map-elements           (result-type mapper set)
  (:documentation "
DO:             Calls MAPPER on each element of the SET in turn (no
                specified order), collecting the results in a set of
                type RESULT-TYPE.

RESULT-TYPE:    A symbol denoting a set class, or LIST or VECTOR.

MAPPER:         A function taking an element of SET as argument, and
                returning an element for the set of type RESULT-TYPE.

SET:            A set.

RETURN:         A set of type RESULT-TYPE containing the elements
                returned by MAPPER.
")
  (:method (result-type mapper (elements sequence))
    (collecting-result (collect result-type)
      (map nil
           (lambda (element)
             (collect (funcall mapper element)))
           elements))))


(defgeneric thereis               (predicate set)
  (:documentation "
RETURN:         Whether there is an element in the SET for which the
                PREDICATE is true.
")
  (:method (predicate set)
    (map-elements nil (lambda (element)
                        (when (funcall predicate element)
                          (return-from thereis t)))
                  set)
    nil))


(defgeneric thereis1              (predicate set)
  (:documentation "
RETURN:         Whether there is exactly one element in the SET for
                which the PREDICATE is true.
")
  (:method (predicate set)
    (let ((seen-one nil))
      (map-elements nil (lambda (element)
                          (when (funcall predicate element)
                            (if seen-one
                                (return-from thereis1)
                                (setf seen-one t))))
                    set)
      seen-one)))


(defgeneric always                (predicate set)
  (:documentation "
RETURN:         Whether the PREDICATE is true for all the elements of
                the SET.
")  
  (:method (predicate set)
    (map-elements nil (lambda (element)
                        (unless (funcall predicate element)
                          (return-from always nil)))
                  set)
    t))


(defgeneric set-equal             (set1 set2)
  (:documentation "
RETURN:         Whether the two sets contains the same elements.
")
  (:method ((set1 list) (set2 list))
    (and (subsetp set1 set2)
         (subsetp set2 set1)))
  (:method (set1 set2)
    (and (is-subset set1 set2)
         (is-subset set2 set1))))




(defgeneric is-subset             (subset set)
  (:documentation "
RETURN:         Whether SUBSET is a subset of SET.
")
  (:method (subset set)
    (and (<= (cardinal subset) (cardinal set))
         (always (curry (function contains) set) subset))))



(defgeneric is-strict-subset      (subset set)
  (:documentation "
RETURN:         Whether SUBSET is a strict subset of SET.
")
  (:method (subset set)
    (and (< (cardinal subset) (cardinal set))
         (always (curry (function contains) set) subset))))



(defgeneric intension             (result-type predicate set)
  (:documentation "
RETURN:         A new set containing only the elements of SET that
                have PREDICATE true.
")
  (:method (result-type predicate set)
    (collecting-result (collect result-type)
      (map-elements nil (lambda (element)
                          (when (funcall predicate element)
                            (collect element)))
                    set))))


(defgeneric copy                  (set)
  (:documentation "
RETURN:         A new set of same class as SET, containing the same
                elements as SET.
")
  (:method (set)
    (assign (make-instance (class-of set)) set)))

(defgeneric union                 (result-type set1 set2)
  (:documentation "
RETURN:         A new set of type RESULT-TYPE containing the union of
                the two sets.
")
  (:method (result-type set1 set2)
    (collecting-result (collect result-type)
      (map-elements nil (function collect) set1)
      (map-elements nil (function collect) set2))))


(defgeneric intersection          (result-type set1 set2)
  (:documentation "
RETURN:         A new set of type RESULT-TYPE containing the
                intersection of the two sets.
")
  (:method (result-type set1 set2)
    (let* ((smallest-is-1  (< (cardinal set1) (cardinal set2)))
           (smallest (if smallest-is-1
                         set1
                         set2))
           (biggest (if smallest-is-1
                        set2
                        set1)))
      (intension result-type (curry (function contains) biggest) smallest))))


(defgeneric difference            (result-type set1 set2)
  (:documentation "
RETURN:         A new set of type RESULT-TYPE containing the
                difference between set1 and set2.
")
  (:method (result-type set1 set2)
    (intension result-type (complement (curry (function contains) set2)) set1)))


(defgeneric symetric-difference   (result-type set1 set2)
  (:documentation "
RETURN:         A new set of type RESULT-TYPE containing the
                symetric difference between the two sets.
")
  (:method (result-type set1 set2)
    (union result-type
           (difference (class-of set1) set1 set2)
           (difference (class-of set2) set2 set1))))


;;; Mutation

(defgeneric include               (destination-set element)
  (:documentation "
POST:   (contains DESTINATION-SET ELEMENT)
RETURN: DESTINATION-SET
"))

(defgeneric exclude               (destination-set element)
  (:documentation "
POST:   (not (contains DESTINATION-SET ELEMENT))
RETURN: DESTINATION-SET
"))

(defgeneric assign-empty          (destination-set)
  (:documentation "
POST:   (emptyp DESTINATION-SET))
RETURN: DESTINATION-SET
")
  (:method (destination-set)
    (loop
      :until (emptyp destination-set)
      :do (exclude destination-set (select destination-set)))
    destination-set))

(defgeneric assign-singleton      (destination-set element)
  (:documentation "
POST:   (and (= 1 (cardinal DESTINATION-SET)) (contains DESTINATION-SET ELEMENT))
RETURN: DESTINATION-SET
")
  (:method (destination-set element)
    (assign-empty destination-set)
    (include destination-set element)
    destination-set))

(defgeneric assign                (destination-set source-set)
  (:documentation "
POST:   (and (set-equal DESTINATION-SET  SOURCE-SET)
             (set-equal (old SOURCE-SET) SOURCE-SET))
RETURN: DESTINATION-SET
")
  (:method (destination-set source-set)
    (assign-empty destination-set)
    (map-elements nil (lambda (element) (include destination-set element)) source-set)
    destination-set))

(defgeneric merge                 (destination-set source-set)
  (:documentation "
POST:   (and (is-subset SOURCE-SET DESTINATION-SET)
             (set-equal (old SOURCE-SET) SOURCE-SET))
RETURN: DESTINATION-SET
")
  (:method (destination-set source-set)
    (map-elements nil (curry (function include) destination-set) source-set)
    destination-set))

(defgeneric intersect             (destination-set source-set)
  (:documentation "
POST:   (and (set-equal DESTINATION-SET (intersection (old DESTINATION-SET) SOURCE-SET))
             (set-equal (old SOURCE-SET) SOURCE-SET))
RETURN: DESTINATION-SET
")
  (:method (destination-set source-set)
    (map-elements nil (lambda (element)
                        (unless (contains source-set element)
                          (exclude destination-set element)))
                  destination-set)
    destination-set))

(defgeneric subtract              (destination-set source-set)
  (:documentation "
POST:   (and (set-equal DESTINATION-SET (difference (old DESTINATION-SET) SOURCE-SET))
             (set-equal (old SOURCE-SET) SOURCE-SET))
RETURN: DESTINATION-SET
")
  (:method (destination-set source-set)
    (map-elements nil (curry (function exclude) destination-set) source-set)
    destination-set))



;;; I/O


;; Note: different set could be serialized differently.

(defgeneric read-set (set stream)
  (:documentation "
DO:      Accumulate in SET the elements read from the stream as a list.
RETURN:  SET.
")
  (:method (set stream)
    (assign-empty set)
    (when (peek-char (character "(") stream nil nil)
      (read-char stream)
      (do ()
          ((char= (peek-char t stream nil (character ")")) (character ")")))
        (include set (read stream))))
    (read-char stream)
    set))


(defgeneric write-set (set stream)
  (:documentation "
DO:     Writes to the stream the elements in SET as a list of elements.
RETURN: SET.
")
  (:method (set stream)
    (princ "(" stream)
    (let ((separator ""))
      (map-elements nil
                    (lambda (element)
                      (princ separator stream)
                      (princ element stream)
                      (setf separator " "))
                    set))
    (princ ")" stream)
    set))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIST-SET CLASS
;;;
;;; A simple implementation to test the default methods.
;;;

(defclass list-set ()
  ((elements :initform '() :initarg :elements :reader elements)))

(defmethod print-object ((set list-set) stream)
  (print-unreadable-object (set stream :type t :identity t)
    (prin1 (slot-value set 'elements) stream))
  set)

(defmethod include               ((destination-set list-set) element)
  (pushnew element (slot-value destination-set 'elements))
  destination-set)

(defmethod exclude               ((destination-set list-set) element)
  (setf  (slot-value destination-set 'elements) (delete element (slot-value destination-set 'elements)))
  destination-set)

(defmethod contains               ((set list-set) element)
  (not (not (member element (slot-value set 'elements)))))

(defmethod cardinal               ((set list-set))
  (length  (slot-value set 'elements)))

(defmethod select               ((set list-set))
  (if (slot-value set 'elements)
      (first (slot-value set 'elements))
      (values)))

(defmethod map-elements           (result-type mapper (set list-set))
  (collecting-result (collect result-type)
    (map nil
         (lambda (element)
           (collect (funcall mapper element)))
         (slot-value set 'elements))))

(defmethod make-collector        ((result-type (eql 'list-set)))
  (declare (ignorable result-type))
  (lambda (&optional set (element nil add-element-p))
    (if add-element-p
        (progn
          (pushnew element (slot-value set 'elements))
          set)
        (make-instance 'list-set))))

(defmethod minimum               ((set list-set))
  (when (every (function realp) (slot-value set 'elements))
    (reduce (function min) (slot-value set 'elements))))

(defmethod maximum               ((set list-set))
  (when (every (function realp) (slot-value set 'elements))
    (reduce (function max) (slot-value set 'elements))))


;;;; THE END ;;;;
