;;;;  -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;; FILE:               metamodel-macros.lisp
;;;; LANGUAGE:           Common-Lisp
;;;; SYSTEM:             Common-Lisp
;;;; USER-INTERFACE:     NONE
;;;; DESCRIPTION
;;;;     
;;;;     Macros definitions for the objecteering metamodel.
;;;;     
;;;; AUTHORS
;;;;     <PJB> Pascal J. Bourguignon <pjb@anevia.com>
;;;; MODIFICATIONS
;;;;     2009-05-20 <PJB> Adapted these macros for the objecteering metamodel.
;;;;     2009-01-09 <PJB> Added this comment.
;;;; BUGS
;;;; LEGAL
;;;;     GPL
;;;;     
;;;;     Copyright 
;;;;     
;;;;     This program is free software; you can redistribute it and/or
;;;;     modify it under the terms of the GNU General Public License
;;;;     as published by the Free Software Foundation; either version
;;;;     2 of the License, or (at your option) any later version.
;;;;     
;;;;     This program is distributed in the hope that it will be
;;;;     useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;     PURPOSE.  See the GNU General Public License for more details.
;;;;     
;;;;     You should have received a copy of the GNU General Public
;;;;     License along with this program; if not, write to the Free
;;;;     Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;     Boston, MA 02111-1307 USA
;;;; *************************************************************************

(defpackage "COM.INFORMATIMAGO.CLEXT.ASSOCIATION"
  (:use "COMMON-LISP" "CLOSER-MOP")
  (:shadowing-import-from "CLOSER-MOP"
                          "STANDARD-CLASS" "STANDARD-GENERIC-FUNCTION" "STANDARD-METHOD"
                          "DEFMETHOD" "DEFGENERIC")
  (:export "DEFINE-CLASS" "DEFINE-ASSOCIATION" "CHECK-OBJECT" "CHECK-CHAIN"
           "ATTACH" "DETACH" "ASSOCIATEDP"  "DID-LINK" "WILL-UNLINK"))
(in-package "COM.INFORMATIMAGO.CLEXT.ASSOCIATION")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASSES
;;;

(defmacro define-class (class-name superclasses &key slots documentation)
  "
DO:     Define a class, with a slightly different syntax.
        Since there are a lot of classes with no additionnal slots,
        to make the slots optional, we introduce them with a :slots keyword.
        The initarg and accessor are automatically generated with the same
        name as the slot by default.
        The initform is automatically set to nil by default.
"
  `(progn
     (defclass ,class-name ,superclasses
      ,(mapcar
        (lambda (slot)
          (if (atom slot)
              `(,slot
                :initarg ,(intern (string slot) "KEYWORD")
                :initform 'nil
                :accessor ,slot)
              (destructuring-bind (slot-name &key initarg initform type accessor documentation) slot
                `(,slot-name
                  :initarg ,(or initarg
                                (intern (string slot-name) "KEYWORD"))
                  :initform ,(or initform 'nil)
                  :accessor ,(or accessor slot-name)
                  ,@(when documentation (list :documentation documentation))
                  ,@(when type (list :type type))))))
        slots)
      ,@(when documentation `((:documentation ,documentation))))
     ',class-name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ASSOCIATIONS
;;;
;;; This kind of association will modify the class of the objects they
;;; associate, adding the needed slots.
;;;


(defun add-new-element (list element &key (key (function identity)) lessp test)
  "
DO:     Modify the list, adding the element if it is not already in the list.
        LESSP and TEST are mutually exclusive.
        When LESSP is provided, the element is inserted in the middle of the list.
        When TEST is provided, the element is inserted at the end.
        If LIST is NIL, then a new list is returned.
KEY:   a key function. Default IDENTITY.
LESSP: a lessp function. Default NIL.
TEST:  an equal function. Default EQL.
"
  (assert (or (null lessp) (null test)))
  (cond
   ((null list) (list element))
   ((null lessp)
    (let ((test (or test (function eql))))
      (loop
       :with element-key = (funcall key element)
       :for cell :on (cons nil list)
       :while (and (cdr cell)
                   (not (funcall test
                                 element-key
                                 (funcall key (cadr cell)))))
       :finally (progn
                  (unless (cdr cell)
                    (setf (cdr cell) (list element)))
                  (return list)))))
   (t
    (loop
     :with element-key = (funcall key element)
     :with result = (cons nil list)
     :for cell :on result
     :while (and (cdr cell)
                 (funcall lessp
                          (funcall key (cadr cell))
                          element-key))
     :finally (progn
                (cond
                 ((null (cdr cell))
                  (setf (cdr cell) (list element)))
                 ((funcall lessp
                           element-key
                           (funcall key (cadr cell)))
                  (push element (cdr cell))))
                (return (cdr result)))))))

(defun test/add-new-element ()
  (assert (equal '(x) (add-new-element nil 'x)))
  (assert (equal '(x) (add-new-element nil 'x :test (function equal))))
  (assert (equal '(x) (add-new-element nil 'x :lessp (function string<))))
  (assert (equal '(x) (add-new-element (list 'x) 'x)))
  (assert (equal '(x) (add-new-element (list 'x) 'x :test (function equal))))
  (assert (equal '(x) (add-new-element (list 'x) 'x :lessp (function string<))))
  (progn (let* ((list (list 1 2 3))
               (result (add-new-element list 0 :test #'=)))
           (assert (equal result '(1 2 3 0)))
           (assert (eql result list)))
         (let* ((list (list 1 2 3)) (result (add-new-element list 0)))
           (assert (equal result '(1 2 3 0)))
           (assert (eql result list)))
         (let* ((list (list 1 2 3))
               (result (add-new-element list 4 :lessp #'<)))
           (assert (equal result '(1 2 3 4)))
           (assert (eql result list)))
         (let* ((list (list 1 2 3))
               (result (add-new-element list 1 :lessp #'<)))
           (assert (equal result '(1 2 3)))
           (assert (eql result list)))
         (let* ((list (list 1 2 3)) (result (add-new-element list 1)))
           (assert (equal result '(1 2 3)))
           (assert (eql result list)))
         (let* ((list (list 1 2 3))
               (result (add-new-element list 1.0 :test #'=)))
           (assert (equal result '(1 2 3)))
           (assert (eql result list)))
         (let* ((list (list 1 2 3)) (result (add-new-element list 2)))
           (assert (equal result '(1 2 3)))
           (assert (eql result list)))
         (let* ((list (list 1 2 3))
               (result (add-new-element list 2.0 :test #'=)))
           (assert (equal result '(1 2 3)))
           (assert (eql result list)))
         (let* ((list (list 1 2 3)) (result (add-new-element list 3)))
           (assert (equal result '(1 2 3)))
           (assert (eql result list)))
         (let* ((list (list 1 2 3))
               (result (add-new-element list 3.0 :test #'=)))
           (assert (equal result '(1 2 3)))
           (assert (eql result list)))
         (let* ((list (list 1 2 4))
               (result (add-new-element list 3 :lessp #'<)))
           (assert (equal result '(1 2 3 4)))
           (assert (eql result list)))
         (let* ((list (list 1 2 4))
               (result (add-new-element list 2 :lessp #'<)))
           (assert (equal result '(1 2 4)))
           (assert (eql result list)))
         (let* ((list (list 1 3 4))
               (result (add-new-element list 2 :lessp #'<)))
           (assert (equal result '(1 2 3 4)))
           (assert (eql result list)))
         (let* ((list (list 1 3 4))
               (result (add-new-element list 3 :lessp #'<)))
           (assert (equal result '(1 3 4)))
           (assert (eql result list))))  
  (let ((list (list 2 3 4))) (assert (equal '(1 2 3 4) (add-new-element list 1 :lessp (function <)))))
  (let ((list (list 2 3 4))) (assert (equal '(1 2 3 4) (add-new-element list 1 :lessp (function <)))))
  :success)
(test/add-new-element)


(eval-when (:load-toplevel :compile-toplevel :execute)

  (defun variations (item list)
    (if (null list)
        (list (list item))
        (cons (cons item list)
              (mapcar (lambda (rest) (cons (car list) rest))
                      (variations item (cdr list))))))

  (defun permutations (elements)
    (cond
      ((null elements) (list elements))
      ((null (cdr elements)) (list elements))
      (t (mapcan (lambda (subperm) (variations (car elements) subperm))
                 (permutations (cdr elements))))))


  (defun multiplicity (multiplicity)
    "
DO:            Decodes the multiplicity.
MULTIPLICITY:  may be either an integer,  or a string designator
               the form  \"*\" or \"MIN-MAX\" or \"MIN..MAX\".
               (beware that the token 0..1 is a 'potential number').
RETURN:        MIN; MAX"
    (multiple-value-bind (min max)
        (if (integerp multiplicity)
            (values multiplicity multiplicity)
            (let* ((smul   (string multiplicity))
                   (dash   (position #\- smul))
                   (dotdot (search ".." smul))
                   (*read-eval* nil)
                   (*read-base* 10.))
              (cond
                (dash
                 (values  (read-from-string smul t nil :end dash)
                          (read-from-string smul t nil :start (1+ dash))))
                (dotdot
                 (values  (read-from-string smul t nil :end dotdot)
                          (read-from-string smul t nil :start (+ 2 dotdot))))
                (t
                 (let ((star (read-from-string smul)))
                   (if (eq '* star)
                       (values 0 '*)
                       (error "Missing a '-' or '..' in the multiplicity: ~A"
                              multiplicity)))))))
      ;; (print (list min max  (and (integerp min)  (not (minusp min))
      ;;                            (or (eq max '*)
      ;;                                (and (integerp max) (<= min max))))))
      (assert (and (integerp min)  (not (minusp min))
                   (or (eq max '*)
                       (and (integerp max) (<= min max))))
              (min max) "Invalid multiplicity ~A" multiplicity)
      (values min max)))

  
  (defun test/multiplicity ()
    (assert (equal (mapcar (lambda (test) (multiple-value-list (multiplicity test)))
                           '(0    1    2    3
                             0-1  1-1  0-4  2-4
                             *    0-*  1-*  4-* ; 34-2
                             0..1 1..1 0..4 2..4
                             *    0..* 1..* 4..* ; 34..2
                             ))
                   '((0 0) (1 1) (2 2) (3 3)
                     (0 1) (1 1) (0 4) (2 4)
                     (0 *) (0 *) (1 *) (4 *) ; (34 2)
                     (0 1) (1 1) (0 4) (2 4)
                     (0 *) (0 *) (1 *) (4 *) ; (34 2)
                     ))))
  

  (defun xor   (a b) (if a (not b) b))
  (defun imply (p q) (or (not p) q))


  (defun generate-link-parameters (endpoints)
    (mapcar (function first) endpoints))

  (defun generate-link-arguments (endpoints)
    (let ((keyword (find-package "KEYWORD")))
      (mapcan (lambda (endpoint)
                (destructuring-bind (role &key &allow-other-keys) endpoint
                  (list (intern (string role) keyword) role)))
              endpoints)))

  (defun generate-attach-parameters (endpoints)
    (mapcar (lambda (endpoint)
              (destructuring-bind (role &key type accessor slot
                                        &allow-other-keys) endpoint
                (assert (not (and accessor slot)) (accessor slot)
                        "ACCESSOR and SLOT are mutually exclusive.")
                (list role type)))
            endpoints))


  ;; 0
  ;; 1
  ;; n
  ;; 0..1
  ;; 0..n
  ;; 1..1
  ;; n..m
  ;; 0..* 
  ;; 1..*
  ;; n..*
  ;; 
  ;;                                        n-1     n-m,1<m
  ;; set             o           (k o)       
  ;; add             o           (k o)
  ;; remove          o              o
  ;; contains        o              o
  ;; get             x             x
  ;; size            x             x
  ;; clear           x             x 
  ;; remove-key                   k
  ;; contains-key                 k
  ;;
  ;; (a . role . set b)            (asso-link a b)           (b . role . set a)
  ;; (a . role . add b)            (asso-link a b)           (b . role . add a)
  ;; (a . role . remove b)         (asso-unlink a b)         (b . role . remove a)
  ;; (a . role . contains b)       (asso-contains-p a b)     (b . role . contains a)
  ;; (a . role . get) -> b                =/=                (b . role . get) -> a
  ;; (a . role . size) -> n1   =/= (asso-size)           =/= (b . role . size) -> n2  
  ;; (a . role . clear)        =/= (asso-clear)          =/= (b . role . clear)       
  ;; (a . role . remove-key k1)           =/=                (b . role . remove-key k2)
  ;; (a . role . contains-key k1)         =/=                (b . role . contains-key k2)
  ;; (a . role . add  k1 b)    =/= (asso-link k2 a k1 b) =/= (b . role . add k2 b)
  ;; (a . role . add  k1 b)    =/= (asso-link k2 a k1 b) =/= (b . role . add k2 b)
  ;;
  ;; Currently implemented:
  ;; ASSO-LINK, ASSO-UNLINK, ASSO-CONTAINS-P
  ;; GET and SIZE are implemented by using directly the accessor for the role

  (defun generate-single-setter (accessor slot copier object value)
    (if accessor
        `(setf (,accessor ,object)         (,copier ,value))
        `(setf (slot-value ,object ',slot) (,copier ,value))))

  (defun generate-multi-adder (accessor slot copier test object value)
    `(pushnew (funcall ,copier ,value)
              (if accessor
                  (,accessor ,object)
                  (slot-value ,object ',slot))
              :test ,test))

  (defun generate-getter (accessor slot copier object value)
    (declare (ignore value))
    (if (eq copier 'identity)
        (if accessor
            `(,accessor ,object)
            `(slot-value ,object ',slot))
        `(,copier ,(if accessor
                       `(,accessor ,object)
                       `(slot-value ,object ',slot)))))


  (defgeneric did-link    (association-name left right)
    (:documentation
     "Hook called after a new link for the association is created between LEFT and RIGHT.")
    (:method (association-name (left t) (right t))
      (declare (ignorable association-name))
      (values)))

  (defgeneric will-unlink (association-name left right)
    (:documentation
     "Hook called before an old link for the association is removed between LEFT and RIGHT.")
    (:method (association-name (left t) (right t))
      (declare (ignorable association-name))
      (values)))


  (defun generate-addset (association-name value object this)
    (destructuring-bind (this-role &key
                                   ((:slot this-slot))
                                   ((:accessor this-accessor))
                                   ((:multiplicity this-multiplicity))
                                   ((:implementation this-implementation))
                                   ((:ordered this-ordered))
                                   ((:test this-test) '(function eql))
                                   ((:lessp this-lessp) nil this-lessp-givenp)
                                   ((:key this-key) '(function identity))
                                   ((:copy this-copy) '(function identity))
                                   &allow-other-keys) this
      (multiple-value-bind (this-min this-max) (multiplicity this-multiplicity)
        (declare (ignore this-min))
        (let ((this-implementation (or this-implementation
                                       (if (equal 1 this-max) 'reference 'list)))
              (this-test (if this-lessp-givenp
                           (let ((a (gensym))
                                 (b (gensym))
                                 (lessp (gensym)))
                            `(lambda (,a ,b)
                               (let ((,lessp ,this-lessp))
                                 (and (not (funcall ,lessp ,a ,b))
                                      (not (funcall ,lessp ,b ,a))))))
                           this-test)))
          (assert (member this-implementation  '(list reference))
                  (this-implementation)
                  "IMPLEMENTATION other than REFERENCE, LIST are ~
                   not implemented yet.")
          (assert (imply (eq this-implementation 'reference) (equal 1 this-max))
                  (this-implementation this-max)
                  "THIS-IMPLEMENTATION must be LIST when THIS-MAX is not 1")
          (flet ((slot  () (if this-accessor
                               `(,this-accessor ,object)
                               `(slot-value ,object ',this-slot)))
                 (value () (if (or (equal '(function identity) this-copy)
                                   (eq 'identity this-copy))
                               value
                               `(,this-copy  ,value))))
            ;; 0-1   link          reference   (setf as (copy o))
            ;; 1-1   link          reference   (setf as (copy o))
            ;;
            ;; n-m   link          list        (if (and (< (length as) m) (not (containsp o as)))  (push o as) (error "full"))
            ;; 0-*   link          list        (pushnew o as :test test)
            ;; 1-*   link          list        (pushnew o as :test test)
            ;; n-*   link          list        (pushnew o as :test test)
            ;; 0-1   link          list        (setf as (list (copy o)))
            ;; 1-1   link          list        (setf as (list (copy o)))
            (ecase this-implementation
              ((reference)
               `(progn (assert (null ,(slot)))
                       (setf ,(slot) ,(value))))
              ((list)
               (cond
                 ((eql 1 this-max)
                  `(progn (assert null ,(slot))
                          (setf ,(slot) (list ,(value)))))
                 ((eql '* this-max)
                  (if this-ordered
                    `(progn (assert (not (find ,value ,(slot) :test ,this-test :key ,this-key)))
                           (setf ,(slot) (add-new-element ,(slot) ,(value)
                                                          ,@(if this-lessp-givenp
                                                              `(:lessp ,this-lessp) 
                                                              `(:test ,this-test))
                                                          :key ,this-key)))
                   `(progn (assert (not (find ,value ,(slot) :test ,this-test :key ,this-key)))
                           (pushnew ,(value) ,(slot) :test ,this-test :key ,this-key))))
                 (t
                  (let ((vendpoint (gensym)))
                    `(let ((,vendpoint  ,(slot)))
                       (if (and (<  (length ,vendpoint) ,this-max)
                                (not (find ,value ,vendpoint :test ,this-test :key ,this-key)))
                         ,(if this-ordered
                            `(setf ,(slot) (add-new-element ,(slot) ,(value)
                                                            ,@(if this-lessp-givenp
                                                                `(:lessp ,this-lessp) 
                                                                `(:test ,this-test))
                                                            :key ,this-key))
                            `(push ,(value) ,(slot)))
                           (cerror "Endpoint ~A of association ~A is full, maximum multiplicity is ~A is reached."
                                   ',this-role ',association-name ',this-max)))))))))))))

  
  (defun generate-remove (association-name value object this)
    (destructuring-bind (this-role &key
                                   ((:slot this-slot))
                                   ((:accessor this-accessor))
                                   ((:multiplicity this-multiplicity))
                                   ((:implementation this-implementation))
                                   ;; ((:ordered this-ordered))
                                   ((:test this-test) '(function eql))
                                   ((:lessp this-lessp) nil this-lessp-givenp)
                                   ((:key this-key) '(function identity))
                                   ((:copy this-copy) '(function identity))
                                   &allow-other-keys) this
      (multiple-value-bind (this-min this-max) (multiplicity this-multiplicity)
        (let ((this-implementation (or this-implementation
                                       (if (equal 1 this-max) 'reference 'list)))
              (this-test (if this-lessp-givenp
                           (let ((a (gensym))
                                 (b (gensym))
                                 (lessp (gensym)))
                             `(lambda (,a ,b)
                                (let ((,lessp ,this-lessp))
                                  (and (not (funcall ,lessp ,a ,b))
                                       (not (funcall ,lessp ,b ,a))))))
                           this-test)))
          (assert (member this-implementation  '(list reference))
                  (this-implementation)
                  "IMPLEMENTATION other than REFERENCE or LIST ~
                   are not implemented yet.")
          (assert (imply (eq this-implementation  'reference) (equal 1 this-max))
                  (this-implementation this-max)
                  "THIS-IMPLEMENTATION must be LIST when THIS-MAX is not 1")
          (flet ((slot  () (if this-accessor
                               `(,this-accessor ,object)
                               `(slot-value ,object ',this-slot)))
                 (value () (if (or (equal '(function identity) this-copy)
                                   (eq 'identity this-copy))
                               value
                               `(,this-copy  ,value))))
            ;; 1-1   unlink        reference   (error)    
            ;; 0-1   unlink        reference   (setf as nil)
            ;;
            ;; 1-*   unlink        list        (if (< 1 (length as)) (setf as (delete o as :test test)) (error))
            ;; 1-1   unlink        list        (if (< 1 (length as)) (setf as (delete o as :test test)) (error))
            ;; n-*   unlink        list        (if (< n (length as)) (setf as (delete o as :test test)) (error))
            ;; n-m   unlink        list        (if (< n (length as)) (setf as (delete o as :test test)) (error))
            ;; 0-*   unlink        list        (setf as (delete o as :test test))
            ;; 0-1   unlink        list        (setf as (delete o as :test test))
            (ecase this-implementation
              ((reference)
               `(when (funcall ,this-test ,value ,(slot))
                  ,(if (eql 1 this-min)
                       `(error "Cannot remove the only ~A from the ~
                                association ~A of minimum multiplicity ~A."
                               ',this-role ',association-name ',this-min)
                       `(setf ,(slot) nil))))
              ((list)
               (let ((vendpoint (gensym)))
                 `(let ((,vendpoint ,(slot)))
                    (when (find ,value ,vendpoint :test ,this-test :key ,this-key)
                      ,(if (zerop this-min)
                           `(setf ,(slot) (delete ,value ,vendpoint
                                                  :test ,this-test :key ,this-key :count 1))
                           `(if  (< ,this-min (length ,vendpoint))
                                 (setf ,(slot) (delete ,value ,vendpoint
                                                       :test ,this-test :key ,this-key :count 1))
                                 (error "The role ~A of the association ~A ~
                                         has reached its minimum multiplicity ~A."
                                        ',this-role ',association-name
                                        ',this-min)))))))))))))

  
  (defun generate-contains-p (association-name value object this)
    (declare (ignore association-name))
    (destructuring-bind (this-role &key
                                   ((:slot this-slot))
                                   ((:accessor this-accessor))
                                   ((:multiplicity this-multiplicity))
                                   ((:implementation this-implementation))
                                   ((:test this-test) '(function eql))
                                   ((:copy this-copy) '(function identity))
                                   &allow-other-keys) this
      (declare (ignore this-role))
      (multiple-value-bind (this-min this-max) (multiplicity this-multiplicity)
        (declare (ignore this-min))
        (let ((this-implementation (or this-implementation
                                       (if (equal 1 this-max) 'reference 'list))))
          (assert (member this-implementation  '(list reference))
                  (this-implementation)
                  "IMPLEMENTATION other than REFERENCE or LIST ~
                   are not implemented yet.")
          (assert (imply (eq this-implementation  'reference) (equal 1 this-max))
                  (this-implementation this-max)
                  "THIS-IMPLEMENTATION must be LIST when THIS-MAX is not 1")
          (flet ((slot  () (if this-accessor
                               `(,this-accessor ,object)
                               `(slot-value ,object ',this-slot)))
                 (value () (if (or (equal '(function identity) this-copy)
                                   (eq 'identity this-copy))
                               value
                               `(,this-copy  ,value))))
            ;; 0-1   containsp     reference   (test as o)
            ;; 1-1   containsp     reference   (test as o)
            ;;
            ;; 0-*   containsp     list        (find o as :test test)
            ;; 0-1   containsp     list        (find o as :test test)
            ;; 1-*   containsp     list        (find o as :test test)
            ;; 1-1   containsp     list        (find o as :test test)
            ;; n-*   containsp     list        (find o as :test test)
            ;; n-m   containsp     list        (find o as :test test)
            (ecase this-implementation
              ((reference) `(funcall ,this-test ,value ,(slot)))
              ((list)      `(member ,value ,(slot) :test ,this-test))))))))

  '#:eval-when/functions-for-macro)


(eval-when (:execute)
  (test/multiplicity)
  'tests) ;; eval-when


(defun convert-to-direct-slot-definition (class canonicalized-slot)
  (apply (function make-instance)
         (apply (function closer-mop:direct-slot-definition-class) class canonicalized-slot)
         canonicalized-slot))


(defun canonicalize-slot-definition (slotdef)
  (list :name         (closer-mop:slot-definition-name         slotdef)
        :readers      (closer-mop:slot-definition-readers      slotdef)
        :writers      (closer-mop:slot-definition-writers      slotdef)
        :type         (closer-mop:slot-definition-type         slotdef)
        :allocation   (closer-mop:slot-definition-allocation   slotdef)
        :initargs     (closer-mop:slot-definition-initargs     slotdef)
        :initform     (closer-mop:slot-definition-initform     slotdef)
        :initfunction (closer-mop:slot-definition-initfunction slotdef)))


(defun ensure-class-slot (class-name slot-name)
  (let ((class (find-class class-name)))
    (when class
      ;; finalize it before calling CLOSER-MOP:CLASS-SLOTS
      (make-instance class-name)
      (unless (find slot-name (closer-mop:class-slots class)
                    :key (function closer-mop:slot-definition-name))
        (closer-mop:ensure-class
         class-name
         :direct-slots
         (append (mapcar (function canonicalize-slot-definition) (closer-mop:class-direct-slots class))
                 (list (list  :name slot-name
                              :initform 'nil
                              :initfunction (constantly nil)
                              :initargs (list (intern (string slot-name) "KEYWORD"))
                              :readers  (list slot-name)
                              :writers  (list `(setf ,slot-name))
                              :documentation  "Generated by define-association"))))))
    class))


(defun scat (&rest string-designators)
  (intern (apply (function concatenate) 'string
                 (mapcar (function string) string-designators))))


;; (defmacro define-association (name ((role &key type slot accessor
;;                                           multiplicity implementation
;;                                           multiple ordered qualifier
;;                                           test lessp key copy
;;                                           &allow-other-keys)
;;                                     &rest other-endpoints)
;;                                    &key documentation)
;;   "
;; SLOT xor ACCESSOR
;; QUALIFIER
;; "
;;   )
;; 
;; (defclass employer ()
;;   ())
;; 
;; (defclass employee ()
;;   ())
;; 
;; (define-association employs
;;   (employers :type employer
;;              :multiplicity 0-*
;;              :multiple nil)
;;   (employees :type employee
;;              :qualifier emp-number
;;              :multiplicity 1
;;              :implementation hash-table))
;; 
;; (defclass employer ()
;;   ((employees :initform (make-hash-table))))
;; 
;; (defclass employee ()
;;   ((employers :initform '())))


(defmacro define-association (name endpoints &rest options)
  "
Define functions to manage the association:

    (name-LINK       a b ...)
    (name-UNLINK     a b ...)
    (name-CONTAINS-P a b ...) --> BOOLEAN
    (name-SIZE)  --> INTEGER
    (name-CLEAR) --> INTEGER

taking &KEY arguments named for the ROLE names.

There may be more than two endpoints, in case of ternary, etc associations.

ENDPOINTS      a list of (ROLE &KEY TYPE ACCESSOR SLOT MULTIPLICITY MULTIPLE
                          IMPLEMENTATION COPY TEST ORDERED).


TYPE           needed for ATTACH and DETACH.
               If all the types are present and different, then ATTACH and
               DETACH methods are created for the arguments in any order.

    Note: we should review this macro for TYPE vs. CLASS.
          Slots may be accessed only in instances of standard-class classes.
          Accessors may be used with any type.

ACCESSOR and SLOT are optional, and mutually exclusive.

   --------  ---------  ----------  -------------  -------  ------  --------
   ACCESSOR    SLOT     Slot        Accessor       CreSlot  CreAcc  Use
   --------  ---------  ----------  -------------  -------  ------  --------
    absent    absent    Role name   Role Name       Yes      Yes     slot
                        When both :accessor and :slot are absent, the role
                        name is used to create a slot with an accessor in
                        the associated class. 
                        Note: In this case, :type must be given a class.
   --------  ---------  ----------  -------------  -------  ------  --------
    absent    present   Given slot     N/A           No       No     slot

                        The associated class is not changed.  The given slot
                        is directly used.
   --------  ---------  ----------  -------------  -------  ------  --------
   present    absent        N/A     Given Accessor   No       No    accessor

                        The associated class is not changed.  The given
                        accessor is used.
   --------  ---------  ----------  -------------  -------  ------  --------
   present    present   ...................FORBIDDEN........................
   --------  ---------  ----------  -------------  -------  ------  --------

MULTIPLICITY   may be either an integer, or a string designator the form \"MIN-MAX\"

MIN, MAX       an integer or * representing infinity; PRE: (< MIN MAX)

MULTIPLE       boolean default NIL indicates whether the same objects may be
               in relation together several times.

COPY           if not NIL, a function used to copy the objects before storing
               or returning them.

LESSP          default is NIL.  A function used to compare the objects
               put into the relation.

TEST           default is (FUNCTION EQL), the function used to compare
               the objects put into the relation.

   Note: If you set COPY, you will probably want to set TEST or LESSP too.
         TEST and LESSP are mutually exclusive.

         For strings, you may want to set TEST to EQUAL or EQUALP or
         LESSP to STRING< or STRING-LESSP

         For numbers, you may want to set TEST to =, or LESSP to <.

         COPY, TEST and LESSP are evaluated, so you can pass 'fun,
         (function fun) or (lambda (x) (fun x)).

ORDERED        (only for REFERENCE, LIST, VECTOR and REIFIED).

               NIL:  the objects are not ordered in the containers.

               T:    If LESSP is not given, then the objects are kept
                     in the order of association in the containers.
                     The KEY of the objects are compared with the TEST
                     function.

                     If LESSP is given, then the objects are kept in
                     the order specified by LESSP applied on the KEY
                     of the objects.

IMPLEMENTATION is (OR (MEMBER REFERENCE LIST VECTOR HASH-TABLE A-LIST P-LIST REIFIED)
                      (CONS (HASH-TABLE A-LIST P-LIST)
                            (CONS (MEMBER REFERENCE LIST VECTOR) NIL)))
               indicates the kind of slot used to implement the role.
    REFERENCE  only when (= 1 MAX) : the related object is stored in the slot.
    LIST       the related objects are stored in a list.
    VECTOR     the related objects are stored in a vector.
               If MAX is finite, then the size of the vector must be = MAX
               else the VECTOR must be adjustable and may have a fill-pointer.
    A-LIST     the related keys and objects are stored in an a-list.
               For qualified roles.
    P-LIST     the related keys and objects are stored in a p-list.
               For qualified roles.
    HASH-TABLE the related keys and objects are stored in a HASH-TABLE.
               For qualified roles.
    REIFIED    the association is reified and nothing is stored in the
               related objects.

    For qualified roles, the multiplicity is per key.
       (persons :multiplicity 0-* :implementation hash-table)
       gives several persons per key (name -> homonyms).
    In case of qualified roles and (< 1 MAX), the IMPLEMENTATION can be given
    as a list of two items, the first giving the implementation of the role,
    and the second the implementation of the values. (HASH-TABLE VECTOR) maps
    with an hash-table keys to vectors of associated objects.

    Currently implemented:  REFERENCE and LIST.
    MULTIPLE is not implemented yet.

OPTIONS        a list of (:keyword ...) options.
   (:DOCUMENTATION string)

BUGS:    If there is an error in handling one association end, after
         handling the other end, the state becomes invalid. No transaction :-(
"
  (declare (ignore options)) ; for now
  (when (endp (rest endpoints))
    (error "The association ~A needs at least two endpoints." name))
  (assert (= 2 (length endpoints)) ()
          "Sorry, associations with more than two endpoints such ~
            as ~A are not implemented yet." name)
  (let* ((endpoints (mapcar (lambda (endpoint)
                              (destructuring-bind (role &rest others
                                                        &key slot accessor type
                                                        &allow-other-keys) endpoint
                                (unless (or slot accessor)
                                  (assert type (type)
                                          "A :TYPE for the association end must be given ~
                                                when there's no :ACCESSOR or :SLOT.")
                                  (unless slot
                                    (setf slot role)))
                                (list* role :slot slot :accessor accessor :type type others)))
                            endpoints))
         (link-parameters (generate-link-parameters endpoints))
         ;; (link-arguments  (generate-link-arguments  endpoints))
         (types           (loop :for endpoint :in endpoints
                             :for type = (getf (rest endpoint) :type)
                             :when type :collect type))
         (attachp         (= (length endpoints)
                             (length (remove-duplicates types))))
         (attach-args-permutations
          (and attachp (permutations (generate-attach-parameters endpoints))))
         (link            (scat name '-link))
         (unlink          (scat name '-unlink))
         (contains-p      (scat name '-contains-p)))

    `(progn
       ,@(let ((troles (mapcar (lambda (endpoint)
                                 (destructuring-bind (role &key type &allow-other-keys) endpoint
                                   (list type role)))
                               endpoints)))
              (append
               (when (first (second troles))
                 (list `(ensure-class-slot ',(first (first troles)) ',(second (second troles)))))
               (when (first (first troles))
                 (list `(ensure-class-slot ',(first (second troles)) ',(second (first troles))))))) 
       (defun ,link (&key ,@link-parameters)
         ,(generate-addset name
                           (first link-parameters) (second link-parameters)
                           (first endpoints))
         ,(generate-addset name
                           (second link-parameters) (first link-parameters)
                           (second endpoints))
         (did-link ',name ,(first link-parameters) ,(second link-parameters)))
       (defun ,unlink (&key ,@link-parameters)
         (multiple-value-prog1 (will-unlink  ',name
                                             ,(first link-parameters)
                                             ,(second link-parameters))
           ,(generate-remove name
                             (first link-parameters) (second link-parameters)
                             (first endpoints))
           ,(generate-remove name
                             (second link-parameters) (first link-parameters)
                             (second endpoints))))
       (defun ,contains-p (&key ,@link-parameters)
         (and ,(generate-contains-p name
                                    (first link-parameters) (second link-parameters)
                                    (first endpoints))
              ,(generate-contains-p name
                                    (second link-parameters) (first link-parameters)
                                    (second endpoints))
              t))
       ,@ (when attachp
            (let ((link-arguments
                   (generate-link-arguments endpoints ;; (first attach-args-permutations)
                                            )))
              (mapcar (lambda (arguments)
                        (let ((arguments (cons `(asso (eql ',name)) arguments)))
                          `(progn
                             (defmethod attach      ,arguments
                               (declare (ignore asso))
                               (,link       ,@link-arguments))
                             (defmethod detach      ,arguments
                               (declare (ignore asso))
                               (,unlink     ,@link-arguments))
                             (defmethod associatedp ,arguments
                               (declare (ignore asso))
                               (,contains-p ,@link-arguments)))))
                      attach-args-permutations)))
       ',name)))



#-(and)
(let* ((class-name type))
  
  (let* ((class  (find-class class-name))
         (slots  (mop:compute-slots class)))
    (unless (find role slots :key (function mop:slot-description-name))
      (mop:ensure-class
       class-name
       :direct-default-initargs (mop:class-direct-default-initargs class)
       :direct-slots            (mop:class-direct-slots            class)
       :direct-superclasses     (mop:class-direct-superclasses     class)
       :name                    class-name
       :metaclass               (class-of class)))))






(defmacro check-object (expression)
  "Evaluates the expression and reports an error if it's NIL."
  `(or ,expression (error  "~S returned NIL" ',expression)))


(defmacro check-chain (expression)
  (flet ((chain-expression-p (expression)
           "An expression that is a function call of a single other
            chain-expression or a simple-expression."
           ;; Actually, we only check the toplevel...
           (and (listp expression)
                (= 2 (length expression))
                (consp (second expression)))))
    (let ((vvalue (gensym)))
      (if (chain-expression-p expression)
          `(let ((,vvalue (check-chain ,(second expression))))
             (or (,(first expression) ,vvalue)
                 (error "~S returned NIL" ',expression)))
          `(check-object ,expression)))))



;;;; THE END ;;;;

