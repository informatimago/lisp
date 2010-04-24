;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               combination.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports three classes to generate lazily combinations,
;;;;    and arrangement with and without repeatition (permutations).
;;;;
;;;;    (Sorry about the comments that are untranslated from C++ to Lisp).
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-04-24 <PJB> Converted from BpCombi C++ classes.
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.COMBINATION"
  (:USE "COMMON-LISP")
  (:EXPORT "DONE-P" "GET-NEXT-ELEMENT" "GET-CURRENT-ELEMENT" "RESET"
           "AT-BEGINNING-P" "ELEMENT-SIZE" "BASE-CARDINAL" "INDEX" "CARDINAL"
           "ARRANGEMENT" "COMBINATION" "ARRANGEMENT-SANS-REPEAT"
           "ARRANGEMENT-WITH-REPEAT")
  (:SHADOW "STEP")
  (:DOCUMENTATION
   "This package exports three classes to generate lazily combinations,
    and arrangement with and without repeatition (permutations).

    Copyright Pascal J. Bourguignon 2005 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.COMBINATION")





(deftype cardinal () '(integer 0))

(defun copy-vector (vector &key (start 0) (end (length vector))
                    (element-type nil))
  (let ((result (apply (function make-array) (list (- end start))
                       (when element-type (list :element-type element-type)))))
    (replace result vector :start2 start :end2 end)
    result))


(defgeneric compute-cardinal (self)
  (:documentation "
NOTE:       It must be overriden by subclasses to compute the
            _cardinal from the _baseCardinal and the _elementSize
            attributes.
"))


(defgeneric initialize (self)
  (:documentation "
NOTE:       It must be overriden by subclasses to initialize the 
            enumeration. It must compute the _cardinal, and set 
            the_index either to 0 or to _cardinal. If
            _index != _cardinal then the choice array must be 
            set to the first enumerated element.
"))


(defgeneric next (self)
  (:documentation "
NOTE:       It must be overriden by subclasses to step to the next 
            element of the enumeration. If _index<_cardinal, then 
            it must increment _index ; if _index¹_cardinal then the 
            choice array must be set to the first enumerated 
            element.
"))


(defgeneric cardinal (self)
  (:documentation "
PRE:        !atBegining()..
RETURN:     the number of elements enumerated by this object.
"))


(defgeneric index (self)
  (:documentation "
PRE:        !atBegining()..
RETURN:     the index of the current element enumerated by 
            this object.
"))


(defgeneric element-size (self)
  (:documentation "
RETURN:     the size of each element returned by getCurrentElement 
            and getNextElement in the choice arrays.
"))


(defgeneric reset (self)
  (:documentation "
POST:       atBegining().
DO:         resets the enumeration.
"))


(defgeneric at-beginning-p (self)
  (:documentation  "
RETURN:     whether the reset() method has been called and 
            getNextElement() (or getCurrentElement()) has not 
            already been called. 
"))


(defgeneric get-Current-Element (self)
  (:documentation  "
PRE:        cardinal()>0.
POST:       !atBegining(),
RETURN:     A vector of cardinal: choice.
DO:         Sets the choice array to the current enumerated 
            element. (ie. the last element retrived with the 
            getNextElement method). The choice array must contain 
            at least elementSize() integers.
"))

(defgeneric get-Next-Element (self)
  (:documentation  "
PRE:        cardinal()>0, !done-p(), atBegining()=b, 
            (!b => index()=a).
POST:       !atBegining(), (!b => index()=a+1),
RETURN:     A vector of cardinal: choice; done-p.
DO:         Computes the next element to be enumerated and sets the
            choice array to it. It returns TRUE when the last 
            element is retrived, ie. all elements have been 
            enumerated. The choice array must contain at least 
            elementSize() integers.
"))

(defgeneric done-p (self)
  (:documentation  "
RETURN:     !atBegining() 
            && ((cardinal()=0) || (index()=cardinal())).
"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Functor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass set-functor ()
  ((base-cardinal  :type cardinal :initform 0 :reader base-cardinal
                   :documentation "The cardinal of the base set.")
   (cardinal       :type cardinal :initform 0 :reader cardinal
                   :documentation "The cardinal of the functor set.")
   (index          :type cardinal :initform 0 :reader index)
   (element-size   :type cardinal :initform 0 :reader element-size)
   (choice         :type vector   :initform nil)
   (at-beginning-p :type boolean  :initform nil :reader at-beginning-p)))


(defgeneric set-base-cardinal (functor cardinal))
(defgeneric set-element-size (functor size))
(defgeneric step (functor index))

(defmethod set-base-cardinal ((self set-functor) card)
  "
PRE:        c=card.
POST:       baseCardinal()=c.
DO:         informs SELF about the number of elements in the set of 
            which this is function.
"
  (when (/= (base-cardinal self) card)
    (setf (slot-value self 'base-cardinal) card
          (slot-value self 'cardinal)      0))
  self)


(defmethod set-element-size ((self set-functor) size)
  "
PRE:        s=size.
POST:       elementSize()=s. 
DO:         Sets the elementSize() parameter.
"
  (when (/= (element-size self) size)
    (setf (slot-value self 'element-size) size
          (slot-value self 'cardinal)     0))
  self)


(defsetf base-cardinal set-base-cardinal)
(defsetf element-size  set-element-size)


(defmethod reset ((self set-functor))
  (compute-cardinal self)
  (setf (slot-value self 'choice) (make-array (list (cardinal self))
                                              :element-type 'cardinal))
  (initialize self)
  (setf (slot-value self 'at-beginning-p) t)
  self)


(defmethod get-current-element ((self set-functor))
  (setf (slot-value self 'at-beginning-p) nil)
  (copy-vector (slot-value self 'choice) :end (element-size self)))


(defmethod get-next-element ((self set-functor))
  (if (and (plusp (cardinal self)) (not (done-p self)))
      (progn
        (if (at-beginning-p self)
            (setf (slot-value self 'at-beginning-p) nil)
            (next self))
        (values (get-current-element self) (done-p self)))
      (values nil nil)))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arrangement Sans Repeat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
(defclass arrangement-sans-repeat (set-functor)
  ((done  :type boolean)
   (taken :type (vector boolean))))


(defmethod done-p ((self arrangement-sans-repeat))
  (= (aref (slot-value self 'choice) 0) (base-cardinal self)))
    

(defun arrangement (n m)
  (loop with r = 1
     for i from (- m n -1) to m
     do (setf r (* r i))
     finally (return r)))


(defmethod compute-cardinal ((self arrangement-sans-repeat))
  (setf (slot-value self 'cardinal)
        (arrangement (element-size self) (base-cardinal self))))


(defmethod initialize ((self arrangement-sans-repeat))
  (setf (slot-value self 'taken) (make-array (base-cardinal self)
                                             :element-type 'boolean
                                             :initial-element nil)
        (slot-value self 'done) nil)
  (step self 0))


(defmethod step ((self arrangement-sans-repeat) k)
  (macrolet ((transition (guard action state)
               `(when ,guard ,action (go ,state))))
    (let ((choice (slot-value self 'choice))
          (taken  (slot-value self 'taken))
          (element-size  (element-size self))
          (base-cardinal (base-cardinal self)))
      (tagbody
       :state-0
         (transition (< k element-size)       (setf (aref choice k) 0)   :state-1)
         (transition (not (< k element-size)) (setf k (1- element-size)) :state-5)
       :state-1
         (transition (aref taken (aref choice k))        nil             :state-2)
         (transition (not (aref taken (aref choice k)))  nil             :state-3)
       :state-2
         (incf (aref (slot-value self 'choice) k))
         (transition (< (aref choice k) base-cardinal)   nil             :state-1)
         (transition (and (not (< (aref choice k) base-cardinal)) (zerop k))
                     (setf (slot-value self 'done) t)
                     :terminate)
         (transition (and (not (< (aref choice k) base-cardinal)) (not (zerop k)))
                     (decf k)
                     :state-5)
       :state-3
         (setf (aref taken (aref choice k)) t)
         (transition (< k (1- element-size)) (incf k) :state-0)
         (transition (not (< k (1- element-size))) nil :terminate)
       :state-5
         (setf (aref taken (aref choice k)) nil)
         (transition t nil :state-2)
       :terminate))))


(defmethod next ((self arrangement-sans-repeat))
  (step self (element-size self)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arrangement With Repeat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass arrangement-with-repeat (set-functor)
  ())


(defmethod done-p ((self arrangement-with-repeat))
  (loop with choice = (slot-value self 'choice)
     for i from (1- (element-size self)) downto 1
     do (unless (zerop (aref choice i)) (return nil))
     finally (return (= (aref choice 0) (base-cardinal self)))))


(defmethod compute-cardinal ((self arrangement-with-repeat))
  (setf (slot-value self 'cardinal)
        (expt (base-cardinal self) (element-size self))))


(defmethod initialize ((self arrangement-with-repeat))
  (loop with choice = (slot-value self 'choice)
     for i from 0 below (element-size self)
     do (setf (aref choice i) 0))
  self)


(defmethod next ((self arrangement-with-repeat))
  (loop with k = (1- (element-size self))
     with choice = (slot-value self 'choice)
     with base-cardinal = (base-cardinal self)
     initially (incf (aref choice k))
     while (and (> k 0) (>= (aref choice k) base-cardinal))
     do (progn
          (setf (aref choice k) 0)
          (decf k)
          (incf (aref choice k))))
  self)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combinations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass combination (set-functor)
  ())


(defmethod done-p ((self combination))
  (if (slot-value self 'choice)
      (= (aref (slot-value self 'choice) 0)
         (- (base-cardinal self) (element-size self) -1))
      t))


(defun combination (n m)
  (let ((r 1)
        a b)
    (if (> n (- m n))
        (setf a n
              b (- m n))
        (setf a (- m n)
              b n))
    ;; a>=b
    ;; N!/(a!b!) = Pi(i=a+1,N,i)/b!
    (loop for i from (1+ a) to m do (setf r (* r i)))
    (loop for i from 2      to b do (setf r (/ r i)))
    r))


(defmethod compute-cardinal ((self combination))
  (setf (slot-value self 'cardinal)
        (combination (element-size self) (base-cardinal self))))


(defmethod initialize ((self combination))
  (loop with choice = (slot-value self 'choice)
     for i from 0 below (element-size self)
     do (setf (aref choice i) i))
  self)
    

(defun next-step (choice limit i)
  (incf (aref choice i))
  (if (>= (aref choice i) (+ limit i))
      (when (> i 0)
        (next-step choice limit (1- i))
        (setf (aref choice i) (1+ (aref choice (1- i))))))
  (values))


(defmethod next ((self combination))
  (next-step (slot-value self 'choice)
             (- (base-cardinal self) (1- (element-size self)))
             (1- (element-size self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test ()
  (terpri)(terpri)
  (let ((awr (make-instance 'arrangement-with-repeat)))
    (setf (base-cardinal awr) 5
          (element-size  awr) 3)
    (reset awr)
    (princ "Take 3 of 5. ")
    (princ (cardinal awr))
    (loop named awr do
         (multiple-value-bind (arrangement done) (get-next-element awr)
           (when done (loop-finish))
           (print arrangement)))
    (terpri))
  (let ((asr (make-instance 'arrangement-sans-repeat)))
    (setf (base-cardinal asr) 5
          (element-size  asr) 3)
    (reset asr)
    (princ "Take 3 of 5 distinct. ")
    (princ (cardinal asr))
    (loop named asr do
         (multiple-value-bind (arrangement done) (get-next-element asr)
           (when done (loop-finish))
           (print arrangement)))
    (terpri))
  (let ((com (make-instance 'combination)))
    (setf (base-cardinal com) 5
          (element-size  com) 3)
    (reset com)
    (princ "Combination of 3 from 5. ")
    (princ (cardinal com))
    (loop named com do
         (multiple-value-bind (arrangement done) (get-next-element com)
           (when done (loop-finish))
           (print arrangement)))
    (terpri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;