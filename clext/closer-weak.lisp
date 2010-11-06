;;;; -*- coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               closer-weak.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Closer to Weak objects.
;;;;    Implements the specification: http://clisp.cons.org/impnotes/weak.html
;;;;    for Common Lisp implementations that have weak-pointers.
;;;;
;;;;    WEAK-OR-RELATION is a primitive that cannot be implemented propertly
;;;;    without implementation support.
;;;;
;;;;    Currently work on:
;;;;           clisp      full support
;;;;           cmucl      partial support (missing WEAK-OR-RELATION)
;;;;           sbcl       partial support (missing WEAK-OR-RELATION)
;;;;
;;;;    Read-time Features:
;;;;
;;;;        :WEAK-TEST    When testing, call the garbage collector
;;;;                      to break weak pointers sooner.
;;;;
;;;;        :DEBUG-WEAK   When compiling on clisp, use this implementation
;;;;                      instead of the native one.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-05-19 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
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
;;;;**************************************************************************

;;; (push :debug-weak *features*) in clisp to have it on clisp for debugging...

(in-package "COMMON-LISP-USER")

(defpackage "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK"
  (:documentation "
   Closer to Weak objects.
   Implements the specification: http://clisp.cons.org/impnotes/weak.html
   for Common Lisp implementations that have weak-pointers.

   WEAK-OR-RELATION is a primitive that cannot be implemented propertly
   without implementation support.

   Currently work on:
          clisp      full support
          cmucl      partial support (missing WEAK-OR-RELATION)
          sbcl       partial support (missing WEAK-OR-RELATION)

   Copyright Pascal Bourguignon 2006 - 2006
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.
   ")
  (:USE "COMMON-LISP")
  #-(and clisp (not debug-weak))
  (:SHADOW "HASH-TABLE" "MAKE-HASH-TABLE"
           "HASH-TABLE-P" "HASH-TABLE-COUNT" "HASH-TABLE-REHASH-SIZE"
           "HASH-TABLE-REHASH-THRESHOLD" "HASH-TABLE-SIZE" "HASH-TABLE-TEST"
           "GETHASH" "REMHASH" "MAPHASH" "WITH-HASH-TABLE-ITERATOR" "CLRHASH")
  #+(and clisp debug-weak)
  (:import-from
   "EXT"
   "WEAK-POINTER"           "MAKE-WEAK-POINTER"      "WEAK-POINTER-VALUE")
  #+(and clisp (not debug-weak))
  (:import-from
   "EXT"
   "WEAK-POINTER"
   "MAKE-WEAK-POINTER"      "WEAK-POINTER-P"      "WEAK-POINTER-VALUE"
   "MAKE-WEAK-LIST"         "WEAK-LIST-P"         "WEAK-LIST-LIST"
   "WEAK-AND-RELATION"
   "MAKE-WEAK-AND-RELATION" "WEAK-AND-RELATION-P" "WEAK-AND-RELATION-LIST"
   "WEAK-OR-RELATION"
   "MAKE-WEAK-OR-RELATION"  "WEAK-OR-RELATION-P"  "WEAK-OR-RELATION-LIST"
   "MAKE-WEAK-MAPPING"      "WEAK-MAPPING-P"      "WEAK-MAPPING-PAIR"
   "WEAK-MAPPING-VALUE"
   "MAKE-WEAK-AND-MAPPING"  "WEAK-AND-MAPPING-P"  "WEAK-AND-MAPPING-PAIR"
   "WEAK-AND-MAPPING-VALUE"
   "WEAK-OR-MAPPING"
   "MAKE-WEAK-OR-MAPPING"   "WEAK-OR-MAPPING-P"   "WEAK-OR-MAPPING-PAIR"
   "WEAK-OR-MAPPING-VALUE"
   "MAKE-WEAK-ALIST"        "WEAK-ALIST-P"        "WEAK-ALIST-TYPE"
   "WEAK-ALIST-CONTENTS"    "WEAK-ALIST-ASSOC"    "WEAK-ALIST-RASSOC"
   "WEAK-ALIST-VALUE"
   "WEAK-MAPPING"           "HASH-TABLE-WEAK-P")
  #+cmu
  (:import-from "EXTENSIONS"
                "WEAK-POINTER" "MAKE-WEAK-POINTER" "WEAK-POINTER-VALUE")
  #+sbcl
  (:import-from "SB-EXT"
                "WEAK-POINTER" "MAKE-WEAK-POINTER" "WEAK-POINTER-VALUE")
  (:export
   "WEAK-POINTER"
   "MAKE-WEAK-POINTER"      "WEAK-POINTER-P"      "WEAK-POINTER-VALUE"
   "MAKE-WEAK-LIST"         "WEAK-LIST-P"         "WEAK-LIST-LIST"
   "WEAK-AND-RELATION"
   "MAKE-WEAK-AND-RELATION" "WEAK-AND-RELATION-P" "WEAK-AND-RELATION-LIST"
   "WEAK-OR-RELATION"
   "MAKE-WEAK-OR-RELATION"  "WEAK-OR-RELATION-P"  "WEAK-OR-RELATION-LIST"
   "MAKE-WEAK-MAPPING"      "WEAK-MAPPING-P"      "WEAK-MAPPING-PAIR"
   "WEAK-MAPPING-VALUE"
   "MAKE-WEAK-AND-MAPPING"  "WEAK-AND-MAPPING-P"  "WEAK-AND-MAPPING-PAIR"
   "WEAK-AND-MAPPING-VALUE"
   "WEAK-OR-MAPPING"
   "MAKE-WEAK-OR-MAPPING"   "WEAK-OR-MAPPING-P"   "WEAK-OR-MAPPING-PAIR"
   "WEAK-OR-MAPPING-VALUE"
   "MAKE-WEAK-ALIST"        "WEAK-ALIST-P"        "WEAK-ALIST-TYPE"
   "WEAK-ALIST-CONTENTS"    "WEAK-ALIST-ASSOC"    "WEAK-ALIST-RASSOC"
   "WEAK-ALIST-VALUE"
   "WEAK-MAPPING"           "HASH-TABLE-WEAK-P"
   "HASH-TABLE" "MAKE-HASH-TABLE"
   "HASH-TABLE-P" "HASH-TABLE-COUNT"
   "HASH-TABLE-REHASH-SIZE"
   "HASH-TABLE-REHASH-THRESHOLD" "HASH-TABLE-SIZE"
   "HASH-TABLE-TEST" "GETHASH" "REMHASH"
   "MAPHASH" "WITH-HASH-TABLE-ITERATOR" "CLRHASH"))

(defpackage "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK-USER"
  (:nicknames "CLOSER-WEAK-USER" "C2WEAK-USER")
  (:USE "COMMON-LISP" "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK")
  (:SHADOWING-IMPORT-FROM
   "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK"
   "HASH-TABLE" "MAKE-HASH-TABLE"
   "HASH-TABLE-P" "HASH-TABLE-COUNT" "HASH-TABLE-REHASH-SIZE"
   "HASH-TABLE-REHASH-THRESHOLD" "HASH-TABLE-SIZE" "HASH-TABLE-TEST"
   "GETHASH" "REMHASH" "MAPHASH" "WITH-HASH-TABLE-ITERATOR" "CLRHASH"))

(in-package "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK")

;; When testing, we call the garbage collector soon to be more precisely weak.
#+(and weak-test clisp)(import '(ext:gc))
#+(and weak-test sbcl) (defun gc () (sb-ext:GC :full t))
#+(and weak-test cmu)  (import '(EXTENSIONS:GC))




;;;---------------------------------------------------------------------
;;; Weak pointers

#+cmu
(defun weak-pointer-p (object)
  "Returns true if the object is of type WEAK-POINTER."
  (typep object 'EXTENSIONS:WEAK-POINTER))

#+sbcl
(defun weak-pointer-p (object)
  "Returns true if the object is of type WEAK-POINTER."
  (typep object 'SB-EXT:WEAK-POINTER))

#+(and clisp debug-weak)
(defun weak-pointer-p (object)
  "Returns true if the object is of type WEAK-POINTER."
  (EXT:WEAK-POINTER-P object))


#-(or clisp sbcl cmu)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "~A: Please implement WEAK-POINTER and WEAK-OR-RELATION for ~A"
         'closer-weak (lisp-implementation-type)))

;;;---------------------------------------------------------------------
;;; Weak lists

#-(and clisp (not debug-weak))
(defstruct (weak-list (:constructor %make-weak-list))
  "A WEAK-LIST is an ordered collection of references to objects that
does not keep the objects from being garbage-collected. It is
semantically equivalent to a list of WEAK-POINTERs, however with a
more efficient in-memory representation than a plain list of
WEAK-POINTERs would be."
  head)

#-(and clisp (not debug-weak))
(setf (documentation 'weak-list-p 'function)
      "Returns true if the object is of type WEAK-LIST.")

#-(and clisp (not debug-weak))
(defun make-weak-list (list)
  "Creates a WEAK-LIST pointing to each of the elements in the given list."
  (%make-weak-list :head (map 'list (function make-weak-pointer) list)))

#-(and clisp (not debug-weak))
(defun weak-list-list (weak-list)
  "Returns a LIST of those objects from the weak-list that are still alive."
  (let ((alive (delete-if (lambda (item)
                            (multiple-value-bind (object livep) 
                                (weak-pointer-value item)
                              (declare (ignore object))
                              (not livep))) (weak-list-head weak-list))))
    (setf (weak-list-head weak-list) alive)
    (mapcar (function weak-pointer-value) alive)))


#-(and clisp (not debug-weak))
(defun (setf weak-list-list) (value weak-list)
  "Replaces the list of objects stored by the weak-list."
  (setf (weak-list-head weak-list)
        (map 'list (function make-weak-pointer) value))
  value)


;;;---------------------------------------------------------------------
;;; Weak AND relations


#-(and clisp (not debug-weak))
(defstruct (weak-and-relation (:constructor %make-weak-and-relation)
                              (:conc-name %weak-and-relation-))
  "A weak 'and' relation is an ordered collection of references to
objects, that does not keep the objects from being garbage-collected,
and which allows access to all the objects as long as all of them are
still alive. As soon as one of them is garbage-collected, the entire
collection of objects becomes empty."
  objects)

#-(and clisp (not debug-weak))
(setf (documentation 'weak-and-relation-p 'function)
      "Returns true if the object is of type WEAK-AND-RELATION.")

#-(and clisp (not debug-weak))
(defun make-weak-and-relation (list)
  "Creates a WEAK-AND-RELATION between the objects in the given list."
  (%make-weak-and-relation
   :objects (map 'vector (function make-weak-pointer) list)))

#-(and clisp (not debug-weak))
(defun weak-and-relation-list (weak-and-relation)
  "Returns the list of objects stored in the weak-and-relation. 
The returned list must not be destructively modified."
  (loop
     :named :loop
     :with res = '()
     :for weak-pointer :across (%weak-and-relation-objects weak-and-relation)
     :do (multiple-value-bind (object presp) (weak-pointer-value weak-pointer)
           (if presp
               (push object res)
               (return-from :loop '())))
     :finally (return-from :loop (nreverse res))))


;; (defun warp (war) (map 'list (lambda (x) (multiple-value-bind (o p) (weak-pointer-value x) (declare (ignore o)) p)) (%weak-and-relation-objects war)))

;;;---------------------------------------------------------------------
;;; Weak OR relations

;;; Unfortunately, it looks like this is a primitive operation that
;;; cannot be implemented with just weak pointers.  CMUCL & SBCL are weak.

#+(or cmu sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (format *error-output*
    "~2%WARNING: ~A: WEAK-OR-RELATION should be implemented ~
     as primitive in ~A~2%" 'closer-weak (lisp-implementation-type)))

#-(and clisp (not debug-weak))
(defstruct (weak-or-relation (:constructor %make-weak-or-relation)
                             (:conc-name %weak-or-relation-))
  "A weak 'or' relation is an ordered collection of references to
objects, that keeps all objects from being garbage-collected as long
as one of them is still alive. In other words, each of them keeps all
others among them from being garbage-collected. When all of them are
unreferenced, the collection of objects becomes empty."
  objects)

#-(and clisp (not debug-weak))
(setf (documentation 'weak-or-relation-p 'function)
      "Returns true if the object is of type WEAK-OR-RELATION.")

#-(and clisp (not debug-weak))
(defun make-weak-or-relation (list)
  "Creates a WEAK-OR-RELATION between the objects in the given list."
  (%make-weak-or-relation
   :objects (map 'vector (if (and list (null (cdr list)))
                             (function make-weak-pointer)
                             (function identity))  list)))


#-(and clisp (not debug-weak))
(defun weak-or-relation-list (weak-or-relation)
  "Returns the list of objects stored in the WEAK-OR-RELATION. 
The returned list must not be destructively modified."
  (if (= 1 (length (%weak-or-relation-objects weak-or-relation)))
      (multiple-value-bind (object alivep) 
          (weak-pointer-value (aref (%weak-or-relation-objects 
                                     weak-or-relation) 0))
        (if alivep
            (list object)
            '()))
      (map 'list (function identity) 
           (%weak-or-relation-objects weak-or-relation))))


;;;---------------------------------------------------------------------

(defgeneric alivep            (object)
  (:documentation "Returns whether the object is alive."))
(defgeneric pair-key          (object)
  (:documentation "Returns the key in the pair object."))
(defgeneric pair-value        (object)
  (:documentation "Returns the value in the pair object."))
(defgeneric (setf pair-value) (value object)
  (:documentation "Changes the value in the pair object."))


(defmacro define-pair-methods (op alive-fun)
  "Generates the methods for WEAK-AND-RELATIONs and WEAK-OR-RELATIONs."
  (flet ((c (args) (intern (format nil "~{~A~}" args))))
    `(progn
       (defmethod initialize-instance ((self ,(c `(weak- ,op -relation)))
                                       &key key value)
         ,(format nil  "Initialize a WEAK-~A-RELATION object." op)
         (setf (,(c `(%weak- ,op -relation-objects)) self)
               (map 'vector (function make-weak-pointer) (list key value))))
       (defmethod alivep ((self ,(c `(weak- ,op -relation))))
         "Returns whether the object is alive."
         ,(if (eq op 'or)
              ;; our pseudo weak-or-relations are just 
              ;; vectors of normal object references.
              ;; Unless there is only one element in it.
              `(if (= 1 (length (%weak-or-relation-objects self)))
                   (,alive-fun (lambda (wo) (nth-value 1 (weak-pointer-value wo)))
                               (,(c `(%weak- ,op -relation-objects)) self))
                   t)
              `(,alive-fun (lambda (wo) (nth-value 1 (weak-pointer-value wo)))
                           (,(c `(%weak- ,op -relation-objects)) self))))
       (defmethod pair-key ((self ,(c `(weak- ,op -relation))))
         "Returns the key in the pair object."
         ,(if (eq op 'or)
              ;; our pseudo weak-or-relations are just 
              ;; vectors of normal object references.
              ;; Unless there is only one element in it. (There's 2 in pairs).
              `(aref (,(c `(%weak- ,op -relation-objects)) self) 0)
              `(weak-pointer-value
                (aref (,(c `(%weak- ,op -relation-objects)) self) 0))))
       (defmethod pair-value ((self ,(c `(weak- ,op -relation))))
         "Returns the value in the pair object."
         ,(if (eq op 'or)
              ;; our pseudo weak-or-relations are just 
              ;; vectors of normal object references.
              ;; Unless there is only one element in it. (There's 2 in pairs).
              `(aref (,(c `(%weak- ,op -relation-objects)) self) 1)
              `(weak-pointer-value
                (aref (,(c `(%weak- ,op -relation-objects)) self) 1))))
       (defmethod (setf pair-value) (value (self ,(c `(weak- ,op -relation))))
         "Changes the value in the pair object."
         (setf (aref (,(c `(%weak- ,op -relation-objects)) self) 1) 
               ,(if (eq op 'or)
                    ;; our pseudo weak-or-relations are just 
                    ;; vectors of normal object references. 
                    ;; Unless there is only one element in it. 
                    ;; (There's 2 in pairs).
                    `value
                    `(make-weak-pointer value)))))))

#-(and clisp (not debug-weak)) (define-pair-methods and every)
#-(and clisp (not debug-weak)) (define-pair-methods or  some)

;;;---------------------------------------------------------------------
;;; Weak Associations

#-(and clisp (not debug-weak))
(defstruct (weak-mapping
             (:constructor %make-weak-mapping)
             (:conc-name %weak-mapping-))
  "A weak association is a mapping from an object called key to an
object called value, that exists as long as the key is alive. In other
words, as long as the key is alive, it keeps the value from being
garbage-collected."
  key value)

#-(and clisp (not debug-weak))
(setf (documentation 'weak-mapping-p 'function)
      "Returns true if the object is of type WEAK-MAPPING.")

#-(and clisp (not debug-weak))
(defun make-weak-mapping (key value)
  "Creates a WEAK-MAPPING." 
  (%make-weak-mapping :key (make-weak-pointer key) :value value))

#-(and clisp (not debug-weak))
(defmethod initialize-instance ((self weak-mapping) &key key value)
  "Initialize a WEAK-MAPPING object."
  (setf (%weak-mapping-key   self) (make-weak-pointer key) 
        (%weak-mapping-value self) value))

#-(and clisp (not debug-weak))
(defun weak-mapping-pair (weak-mapping)
  "Returns true if the object is of type WEAK-MAPPING."
  (multiple-value-bind (key kpresp)
      (weak-pointer-value (%weak-mapping-key weak-mapping))
    (if kpresp
        (values key (%weak-mapping-value weak-mapping) t)
        (progn (setf (%weak-mapping-value weak-mapping) nil)
               #+weak-test (gc)
               (values nil nil nil)))))

#-(and clisp (not debug-weak))
(defun %weak-mapping-pair (weak-mapping)
  "Returns true if the object is of type WEAK-MAPPING."
  (multiple-value-bind (key kpresp)
      (weak-pointer-value (%weak-mapping-key weak-mapping))
    (if kpresp
        (values key (%weak-mapping-value weak-mapping) t)
        (values nil nil nil))))

#-(and clisp (not debug-weak))
(defun weak-mapping-value (weak-mapping)
  "Returns three values: the original key, the original value, and T, 
if the key has not yet been garbage-collected, else NIL, NIL, NIL."
  (if (nth-value 1 (weak-pointer-value (%weak-mapping-key weak-mapping)))
      (%weak-mapping-value weak-mapping)
      (progn
        (setf (%weak-mapping-value weak-mapping) nil)
        #+weak-test (gc)
        nil)))

#-(and clisp (not debug-weak))
(defun (setf weak-mapping-value) (value weak-mapping)
  "Replaces the value stored in the weak-mapping. 
It has no effect when the key has already been garbage-collected."
  (if (nth-value 1 (weak-pointer-value (%weak-mapping-key weak-mapping)))
      (setf (%weak-mapping-value weak-mapping) value) 
      (progn (setf (%weak-mapping-value weak-mapping) nil)
             #+weak-test (ext:gc)
             value)))



#-(and clisp (not debug-weak))
(defmethod alivep ((self weak-mapping))
  "Returns whether the object is alive."
  (nth-value 2 (%weak-mapping-pair self)))
#-(and clisp (not debug-weak))
(defmethod pair-key ((self weak-mapping))
  "Returns the key in the pair object."
  (nth-value 0 (%weak-mapping-pair self)))
#-(and clisp (not debug-weak))
(defmethod pair-value ((self weak-mapping))
  "Returns the value in the pair object."
  (nth-value 1 (%weak-mapping-pair self)))
#-(and clisp (not debug-weak))
(defmethod (setf pair-value) (value (self weak-mapping))
  "Changes the value in the pair object."
  (if (alivep self)
      (setf (%weak-mapping-value self) value)
      (progn 
        ;; TODO: check this...
        (setf (%weak-mapping-value self) nil)
        #+weak-test (gc)
        value)))

;;;---------------------------------------------------------------------
;;; Weak And/Or Mappings

(defmacro define-mapping (op)
  "Generates the functions for WEAK-AND/OR-MAPPINGS."
  (flet ((c (args) (intern (format nil "~{~A~}" args))))
    `(progn
       (defstruct (,(c `(weak- ,op -mapping) )
                    (:constructor ,(c `(%make-weak- ,op -mapping)))
                    (:conc-name ,(c `(%weak- ,op -mapping-))))
         ,(if (eq op 'or)
              "A weak 'or' mapping is a mapping from a tuple of
objects called keys to an object called value, that keeps all keys and
the value from being garbage-collected as long as one of the keys is
still alive. In other words, each of the keys keeps all others among
them and the value from being garbage-collected. When all of them are
unreferenced, the entire mapping goes away."
              "A weak 'and' mapping is a mapping from a tuple of
objects called keys to an object called value, that does not keep the
keys from being garbage-collected and that exists as long as all keys
are alive. As soon as one of the keys is garbage-collected, the entire
mapping goes away.")
         keys value)
       (setf (documentation ',(c `(weak- ,op -mapping-p)) 'function)
             ,(format nil
                      "Returns true if the object is of type WEAK-~A-MAPPING."
                      op))
       (defun ,(c `(make-weak- ,op -mapping)) (keys value)
         ,(format nil "Creates a WEAK-~A-MAPPING between the keys  objects in
 the given list and the given value. The keys list must be non-empty." op) 
         (assert (etypecase keys
                   (list   (not (null keys)))
                   (vector (plusp (length keys))))
                 (keys) "Keys must be a non null list, not ~S" keys)
         (,(c `(%make-weak- ,op -mapping))
           :keys (,(c `(make-weak- ,op -relation)) keys)
           :value value))
       (defun ,(c `(weak- ,op -mapping-pair)) (mapping)
         ,(if (eq op 'or)
              "Returns three values: the list of keys, the value, and
T, if the keys have not yet been garbage-collected, else NIL, NIL,
NIL. The returned keys list must not be destructively modified."
              "Returns three values: the list of keys, the value, and
T, if none of the keys have been garbage-collected, else NIL, NIL,
NIL. The returned keys list must not be destructively modified.")
         (let ((keys (,(c `(%weak- ,op -mapping-keys)) mapping)))
           (if (alivep keys) 
               (values (,(c `(weak- ,op -relation-list)) keys)
                       (,(c `(%weak- ,op -mapping-value)) mapping)
                       t)
               (progn
                 (setf (,(c `(%weak- ,op -mapping-value)) mapping) nil)
                 #+weak-test (gc)
                 (values nil nil nil)))))
       (defun ,(c `( weak- ,op -mapping-value)) (mapping)
         ,(if (eq op 'or)
              "Returns the value, if the keys have not yet been
garbage-collected, else NIL."
              "Returns the value, if none of the keys have been 
garbage-collected, else NIL.")
         (if (alivep (,(c `(%weak- ,op -mapping-keys)) mapping))
             (,(c `(%weak- ,op -mapping-value)) mapping)
             (progn
               (setf (,(c `(%weak- ,op -mapping-value)) mapping) nil)
               #+weak-test (gc)
               nil)))
       (defun (setf ,(c `( weak- ,op -mapping-value))) (value mapping)
         ,(if (eq op 'or)
              "Replaces the value stored in the WEAK-OR-MAPPING. 
It has no effect when the keys have already been garbage-collected."
              "Replaces the value stored in the WEAK-AND-MAPPING. 
It has no effect when some key has already been garbage-collected.")
         (if (alivep (,(c `(%weak- ,op -mapping-keys)) mapping))
             (setf (,(c `(%weak- ,op -mapping-value)) mapping) value) 
             (progn (setf (,(c `(%weak- ,op -mapping-value)) mapping) nil)
                    #+weak-test (gc)
                    value))))))

#-(and clisp (not debug-weak)) (define-mapping and)
#-(and clisp (not debug-weak)) (define-mapping or)

;;;---------------------------------------------------------------------
;;; Weak Association Lists

#-(and clisp (not debug-weak))
(defstruct (inverse-weak-mapping (:include weak-mapping)))
#-(and clisp (not debug-weak))
(defmethod initialize-instance ((self inverse-weak-mapping) &key key value)
  (call-next-method self :key value :value key))
#-(and clisp (not debug-weak))
(defmethod alivep ((self inverse-weak-mapping))
  (nth-value 2 (%weak-mapping-pair self)))
#-(and clisp (not debug-weak))
(defmethod pair-key ((self inverse-weak-mapping))
  (%weak-mapping-value self))
#-(and clisp (not debug-weak))
(defmethod pair-value ((self inverse-weak-mapping))
  (weak-pointer-value (%weak-mapping-key   self)))
#-(and clisp (not debug-weak))
(defmethod (setf pair-value) (value (self inverse-weak-mapping))
  (setf (%weak-mapping-key self) (make-weak-pointer value)))



#-(and clisp (not debug-weak))
(defclass weak-alist ()
  ((contents :accessor wal-contents
             :initarg :contents)))

#-(and clisp (not debug-weak)) (defclass wal-key           (weak-alist) ())
#-(and clisp (not debug-weak)) (defclass wal-value         (weak-alist) ())
#-(and clisp (not debug-weak)) (defclass wal-key-and-value (weak-alist) ())
#-(and clisp (not debug-weak)) (defclass wal-key-or-value  (weak-alist) ())

#-(and clisp (not debug-weak)) 
(defgeneric wal-pair-class (wal)
  (:method ((wal weak-alist))        nil)
  (:method ((wal wal-key))           'weak-mapping)
  (:method ((wal wal-value))         'inverse-weak-mapping)
  (:method ((wal wal-key-and-value)) 'weak-and-relation)
  (:method ((wal wal-key-or-value))  'weak-or-relation))


#-(and clisp (not debug-weak))
(defgeneric weak-alist-p (wal)
  (:method ((wal t))                 nil)
  (:method ((wal wal-key))           t)
  (:method ((wal wal-value))         t)
  (:method ((wal wal-key-and-value)) t)
  (:method ((wal wal-key-or-value))  t))


#-(and clisp (not debug-weak)) 
(defgeneric weak-alist-type (wal)
  (:method ((wal weak-alist))        nil)
  (:method ((wal wal-key))           :key)
  (:method ((wal wal-value))         :value)
  (:method ((wal wal-key-and-value)) :key-and-value)
  (:method ((wal wal-key-or-value))  :key-or-value))


#-(and clisp (not debug-weak))
(defun make-weak-alist (&key (type :key) (initial-contents '()))
  (let ((wal (make-instance (ecase type
                              (:key           'wal-key)
                              (:value         'wal-value)
                              (:key-and-value 'wal-key-and-value)
                              (:key-or-value  'wal-key-or-value)))))
    (setf (weak-alist-contents wal) initial-contents)
    wal))

#-(and clisp (not debug-weak)) 
(defgeneric weak-alist-contents (wal)
  (:method ((wal weak-alist))
    (let ((alive
           #+weak-test
            (loop 
               :for count = (length (wal-contents wal))
               :then        (length alive)
               :for alive = (delete-if-not (function alivep) (wal-contents wal))
               :then        (delete-if-not (function alivep) alive)
               :while (< (length alive) count)
               :do (gc)
               :finally (return alive))
            #-weak-test
            (delete-if-not (function alivep) (wal-contents wal))))
      (setf (wal-contents wal) alive)
      (mapcar (lambda (pair) (cons (pair-key pair) (pair-value pair))) alive))))


#-(and clisp (not debug-weak)) 
(defgeneric (setf weak-alist-contents) (value wal)
  (:method ((value t) (wal weak-alist))
    (let ((pair-class (wal-pair-class wal)))
      (setf (wal-contents wal) (mapcar (lambda (pair) (make-instance pair-class
                                                        :key   (car pair)
                                                        :value (cdr pair)))
                                       value)))
    value))

#-(and clisp (not debug-weak))
(defgeneric weak-alist-assoc (item self &key test test-not key))
#-(and clisp (not debug-weak))
(defmethod weak-alist-assoc (item (self weak-alist) &key (test (function eql))
                             (test-not nil) (key (function identity)))
  (if test-not
      (weak-alist-assoc  item self :test (complement test-not) :key key)
      (loop
         :named :assoc
         :for pair :in (wal-contents self)
         :do (when (and (alivep pair)
                        (funcall test item (funcall key (pair-key pair))))
               (return-from :assoc (cons (pair-key pair)
                                         (pair-value pair))))
         :finally (return-from :assoc nil))))

#-(and clisp (not debug-weak))
(defmethod weak-alist-remove-assoc (item (self weak-alist)
                                    &key (test (function eql))
                                    (test-not nil) (key (function identity)))
  (if test-not
      (weak-alist-remove-assoc  item self :test (complement test-not) :key key)
      (loop
         :named :assoc
         :for previous = nil :then current
         :for current = (wal-contents self) :then (cdr current)
         :do (when (and (alivep (car current))
                        (funcall test item
                                 (funcall key (pair-key (car current)))))
               (if previous
                   (setf (cdr previous) (cdr current))
                   (setf (wal-contents self) (cdr current)))
               (return-from :assoc t))
         :finally (return-from :assoc nil))))

#-(and clisp (not debug-weak))
(defmethod weak-alist-rassoc (item (self weak-alist) &key (test (function eql))
                              (test-not nil) (key (function identity)))
  (if test-not
      (weak-alist-rassoc  item self :test (complement test-not) :key key)
      (loop
         :named :assoc
         :for pair :in (wal-contents self)
         :do (when (and (alivep pair)
                        (funcall test item (funcall key (pair-value pair))))
               (return-from :assoc (cons (pair-key pair)
                                         (pair-value pair))))

         :finally (return-from :assoc nil))))

#-(and clisp (not debug-weak))
(defmethod weak-alist-value (item (self weak-alist)
                             &key (test (function eql)) (test-not nil))
  (cdr (weak-alist-assoc item self :test test :test-not test-not)))

#-(and clisp (not debug-weak))
(defmethod (setf weak-alist-value) (value item (self weak-alist)
                                    &key (test (function eql)) (test-not nil))
  (if test-not
      (setf (weak-alist-value item self :test (complement test-not)) value) 
      (loop
         :named :assoc
         :for pair :in (wal-contents self)
         :do (when (and (alivep pair) (funcall test item (pair-key pair)))
               (return-from :assoc (setf (pair-value pair) value)))
         :finally (progn
                    (push (make-instance (wal-pair-class self)
                            :key item :value value) (wal-contents self))
                    (return-from :assoc value)))))

;;;---------------------------------------------------------------------
;;; Weak Hash Tables

#-(and clisp (not debug-weak))
(defclass weak-hash-table ()
  ((rehash-size      :accessor %hash-table-rehash-size
                     :initarg :rehash-size
                     :initform 2.0)
   (rehash-threshold :accessor %hash-table-rehash-threshold
                     :initarg :rehahs-threshold
                     :initform 0.90)
   (test             :accessor %hash-table-test
                     :initarg :test
                     :initform (function eql))
   (pair-type        :reader wht-pair-type
                     :initarg :weak
                     :initform :key)
   (buckets          :accessor wht-buckets)))


(defmacro define-forward (name)
  (let ((method-name (intern (format nil "%~A" name)))
        (cl-name     (intern (string name) "COMMON-LISP")))
    `(progn
       (defgeneric ,method-name (self) (:method ((self t)) (,cl-name self)))
       (defun ,name (hash-table) (,method-name hash-table)))))

#-(and clisp (not debug-weak)) (define-forward hash-table-count)
#-(and clisp (not debug-weak)) (define-forward hash-table-rehash-size)
#-(and clisp (not debug-weak)) (define-forward hash-table-rehash-threshold)
#-(and clisp (not debug-weak)) (define-forward hash-table-size)
#-(and clisp (not debug-weak)) (define-forward hash-table-test)

#-(and clisp (not debug-weak))
(defmethod %hash-table-count ((self weak-hash-table))
  (let ((count 0))
    (maphash (lambda (k v) (declare (ignore k v)) (incf count)) self)
    count))


#-(and clisp (not debug-weak))
(defmethod dump-wht ((self weak-hash-table) &optional (out *standard-output*))
  (format out "~A~%" (class-name (class-of self)))
  ((lambda (items) 
     (format out (format nil "~~:{  ~~~DA = ~~A~~%~~}"
                         (reduce
                          (function max) items 
                          :key (lambda (item) (length (string (first item))))))
             items))
   (list (list 'count            (%hash-table-count self))
         (list 'rehash-size      (%hash-table-rehash-size self))
         (list 'rehash-threshold (%hash-table-rehash-threshold self))
         (list 'test             (%hash-table-test self))
         (list 'pair-type        (wht-pair-type self))
         (list '|(LENGTH WHT-BUCKETS)|  (length (wht-buckets self)))))
  (dotimes (i (length (wht-buckets self)))
    (let* ((bucket (aref (wht-buckets self)i))
           (pairs  (cond ((null bucket)         nil)
                         ((weak-alist-p bucket) (weak-alist-contents bucket))
                         ((alivep bucket)       (acons (pair-key bucket)
                                                       (pair-value bucket)
                                                       '()))
                         (t                     nil))))
      (when pairs 
        (format out "  ~4D : (~S . ~S)~%" 
                i (car (first pairs)) (cdr (first pairs)))
        (dolist (pair (rest pairs))
          (format out "         (~S . ~S)~%" (car pair) (cdr pair))))))
  self)

#-(and clisp (not debug-weak))
(defun HASH-TABLE-WEAK-P (object)
  "http://clisp.cons.org/impnotes/hash-dict.html#make-hash"
  (and (typep object 'weak-hash-table) (wht-pair-type object)))

#-(and clisp (not debug-weak))
(defun equiv (a b) (or (and a b) (and (not a) (not b))))

#-(and clisp (not debug-weak))
(defun (setf hash-table-weak-p) (value object)
  (unless (equiv (hash-table-weak-p object) value)
    (error "Changing a weak hash table to a normal hash table or ~
            vice-versa while preserving the table identity is not ~
            supported in this ~A" (lisp-implementation-type)))
  (unless (eq (hash-table-weak-p object) value)
    (error "Changing the weak type of a weak hash table ~
            is not implemented yet."))
  value)


#-(and clisp (not debug-weak))
(defmethod initialize-instance ((self weak-hash-table)
                                &KEY (TEST (function eql) testp)
                                (SIZE 37)
                                (rehash-size 2.0)
                                (REHASH-THRESHOLD 0.90)
                                (weak :key)
                                (initial-contents nil)
                                &allow-other-keys)
  (check-type weak (member :key :value :key-and-value :key-or-value))
  (call-next-method)
  (setf (wht-buckets self) (make-array size :initial-element nil))
  (dolist (pair initial-contents)
    (setf (%gethash (car pair) self) (cdr pair)))
  self)

#-(and clisp (not debug-weak))
(defmethod %hash-table-size ((self weak-hash-table))
  (array-dimension (wht-buckets self) 0))

#-(and clisp (not debug-weak))
(defmethod wht-iterator ((self weak-hash-table))
  (let ((current-bucket 0)
        (current-pairs  nil))
    (lambda ()
      (loop
         :named :next
         :while (and (null current-pairs)
                     (< current-bucket (length (wht-buckets self))))
         :do (let ((bucket (aref (wht-buckets self) current-bucket)))
               (incf current-bucket)
               (setf current-pairs
                     (cond ((null bucket)         nil)
                           ((weak-alist-p bucket) (weak-alist-contents bucket))
                           ((alivep bucket)       (acons (pair-key bucket)
                                                         (pair-value bucket)
                                                         '()))
                           (t                     nil))))
         :finally (return-from :next
                    (if current-pairs
                        (let ((pair (pop current-pairs)))
                          (values t (car pair) (cdr pair)))
                        (values nil nil nil)))))))

#-(and clisp (not debug-weak))
(defmethod %rehash-table ((self weak-hash-table) new-size)
  (let ((new (make-hash-table
              :weak             (wht-pair-type self)
              :test             (hash-table-test self)
              :size             (round new-size)
              :rehash-size      (hash-table-rehash-size self)
              :rehash-threshold (hash-table-rehash-threshold self))))
    (maphash (lambda (k v) (setf (gethash k new) v)) self)
    (setf (wht-buckets self) (wht-buckets new))
    self))

#-(and clisp (not debug-weak))
(defmethod check-reduce-size ((self weak-hash-table) result-values)
  (let ((threshold-count (truncate (hash-table-size self)
                                   (hash-table-rehash-threshold self))))
    (when (< (%hash-table-count self) threshold-count)
      (%rehash-table
       self
       (if (integerp (hash-table-rehash-size self))
           (- (hash-table-size self) (hash-table-rehash-size self))
           (/ (hash-table-size self) (hash-table-rehash-size self))))))
  (values-list result-values))

#-(and clisp (not debug-weak))
(defmethod check-increase-size ((self weak-hash-table) result-values)
  (let ((threshold-count  (* (hash-table-size self)
                             (hash-table-rehash-threshold self))))
    (when (< threshold-count (%hash-table-count self))
      (%rehash-table
       self
       (if (integerp (hash-table-rehash-size self))
           (+ (hash-table-size self) (hash-table-rehash-size self))
           (* (hash-table-size self) (hash-table-rehash-size self))))))
  (values-list result-values))

(defun %sxhash (object)
  #+clisp (if (typep object 'fixnum)
              object
              (sxhash object))
  #-clisp (sxhash object))

#-(and clisp (not debug-weak))
(defmethod %gethash (key (self weak-hash-table) &optional default)
  (declare (ignore default))
  (let* ((h (mod (%sxhash key) (length (wht-buckets self))))
         (bucket (aref (wht-buckets self) h)))
    (cond
      ((null bucket)  (values nil nil)) 
      ((weak-alist-p bucket)
       (let ((pair (weak-alist-assoc key bucket
                                     :test (%hash-table-test self))))
         (if pair
             (values (cdr pair) t)
             (values nil nil))))
      (t (if (and (alivep bucket)
                  (funcall (%hash-table-test self) key (pair-key bucket)))
             (values (pair-value bucket) t)
             (progn
               (setf (aref (wht-buckets self) h) nil)
               #+weak-test (gc)
               (check-reduce-size self (list nil nil))))))))

      
;;; We probably need to use defsetf or define-setf-expander
;;; to process correctly the default value:

#-(and clisp (not debug-weak))
(defmethod (setf %gethash) (value key (self weak-hash-table) &optional default)
  ;; Should check when the new count goes above a threshold and increase
  ;; hash size.
  (let* ((h (mod (%sxhash key) (length (wht-buckets self))))
         (bucket (aref (wht-buckets self) h)))
    (cond
      ((null bucket)
       (setf (aref (wht-buckets self) h)
             (make-instance 
                 (ecase (wht-pair-type self)
                   (:key           'weak-mapping)
                   (:value         'inverse-weak-mapping)
                   (:key-and-value 'weak-and-relation)
                   (:key-or-value  'weak-or-relation))
               :key key :value value)))
      ((weak-alist-p bucket)
       (let ((pair (weak-alist-assoc key bucket
                                     :test (%hash-table-test self))))
         (if pair
             (setf (pair-value pair) value)
             (progn
               (setf (weak-alist-value key bucket :test (%hash-table-test self))
                     value)))))
      (t (if (alivep bucket)
             ;; We cannot shortcut the above test,
             ;; because the key itself could be broken.
             (if (funcall (%hash-table-test self) key (pair-key bucket))
                 ;; change the value.
                 (setf (pair-value bucket) value) 
                 ;; add a new value to the bucket, making it an walist.
                 (progn
                   (setf (aref (wht-buckets self) h)
                         (make-weak-alist
                          :type (wht-pair-type self)
                          :initial-contents (acons key value
                                                   (acons (pair-key   bucket)
                                                          (pair-value bucket)
                                                          '()))))
                   (check-increase-size self (list value))))
             ;; replace the dead single bucket.
             (setf (aref (wht-buckets self) h)
                   (make-instance 
                       (ecase (wht-pair-type self)
                         (:key           'weak-mapping)
                         (:value         'inverse-weak-mapping)
                         (:key-and-value 'weak-and-relation)
                         (:key-or-value  'weak-or-relation))
                     :key key :value value)))))
    value))

#-(and clisp (not debug-weak))
(defmethod %remhash (key (self weak-hash-table))
  ;; Should check when the new count goes below a threshold and reduce
  ;; hash size.
  (let* ((h (mod (%sxhash key) (length (wht-buckets self))))
         (bucket (aref (wht-buckets self) h)))
    (cond
      ((null bucket)  nil)
      ((weak-alist-p bucket)
       (and (weak-alist-remove-assoc key self :test (%hash-table-test self))
            (check-reduce-size self '(t))))
      
      (t
       (when (funcall (%hash-table-test self) key (pair-key bucket))
         (setf (aref (wht-buckets self) h) nil)
         #+weak-test (gc)
         (check-reduce-size self '(t)))))))

#-(and clisp (not debug-weak)) 
(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (let ((vh (gensym)) (iterator (gensym)))
    `(let ((,vh ,hash-table))
       (if (typep ,vh 'weak-hash-table)
           (let ((,iterator (wht-iterator ,vh)))
             (macrolet ((,name () (list 'funcall ',iterator)))
               ,@body))
           (common-lisp:with-hash-table-iterator (,name ,vh) ,@body)))))


#-(and clisp (not debug-weak))
(defmethod %maphash (function (self weak-hash-table))
  (with-hash-table-iterator (next self)
    (loop
       :for (gotit key value) = (multiple-value-list (next))
       :while gotit
       :do (funcall function key value))))

#-(and clisp (not debug-weak))
(defmethod %clrhash ((self weak-hash-table))
  (with-slots ((buckets buckets)) self
    (loop :for i :from 0 :below (length buckets)
       :do (setf (aref buckets i) nil)))
  #+weak-test (gc)
  self)

#-(and clisp (not debug-weak))
(deftype hash-table () '(or common-lisp:hash-table weak-hash-table))

#-(and clisp (not debug-weak))
(defun hash-table-p (object)
  (or (common-lisp:hash-table-p object) (typep object 'weak-hash-table)))


#-(and clisp (not debug-weak)) 
(defgeneric %gethash (key self &optional default)
  (:method (key (self t) &optional default)
    (common-lisp:gethash key self default)))
#-(and clisp (not debug-weak)) 
(defun gethash (key hash-table &optional default)
  (%gethash key hash-table default))

#-(and clisp (not debug-weak)) 
(defgeneric (setf %gethash) (value key self &optional default)
  (:method (value key (self t) &optional default)
    (setf (common-lisp:gethash key self default) value)))
#-(and clisp (not debug-weak)) 
(defun (setf gethash) (value key hash-table &optional default)
  (setf  (%gethash key hash-table default) value))


#-(and clisp (not debug-weak)) 
(defgeneric %remhash (key self)
  (:method (key (self t)) (common-lisp:remhash key self)))
#-(and clisp (not debug-weak)) 
(defun remhash (key hash-table) (%remhash key hash-table))

#-(and clisp (not debug-weak)) 
(defgeneric %maphash (function self)
  (:method (function (self t)) (common-lisp:maphash function self)))
#-(and clisp (not debug-weak)) 
(defun maphash (function hash-table) (%maphash function hash-table))


  
#-(and clisp (not debug-weak)) 
(defgeneric %clrhash (self)
  (:method ((self t)) (common-lisp:clrhash self)))
#-(and clisp (not debug-weak)) 
(defun clrhash (hash-table) (%clrhash hash-table))


#-(and clisp (not debug-weak))
(defun MAKE-HASH-TABLE (&rest other-keys
                        &KEY (TEST (function eql) testp)
                        (SIZE nil sizep)
                        (rehash-size nil rehash-size-p)
                        (REHASH-THRESHOLD nil REHASH-THRESHOLD-p)
                        (weak nil #|:KEY :VALUE :KEY-AND-VALUE :KEY-OR-VALUE|#) 
                        ;; implementation dependant:
                        &allow-other-keys)
  (apply (if weak
             (function make-instance)
             (function common-lisp:make-hash-table))
         (append
          (when weak (list 'weak-hash-table :weak weak))
          (when testp              (list :test             test))
          (when sizep              (list :size             size))
          (when rehash-size-p      (list :rehash-size      rehash-size))
          (when rehash-threshold-p (list :rehash-threshold rehash-threshold))
          other-keys)))

  
;;;; THE END ;;;;

