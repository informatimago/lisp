;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              list.lisp
;;;;LANGUAGE:          common-lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This module exports some list utility functions.
;;;;USAGE
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2008-06-24 <PJB> Added ENSURE-CIRCULAR, MAKE-CIRCULAR-LIST, CIRCULAR-LENGTH.
;;;;    2007-01-05 <PJB> Added REPLACE-TREE (should move to a new package later).
;;;;    2005-09-02 <PJB> Moved EQUIVALENCE-CLASSES in from ECMA048.
;;;;    2005-08-10 <PJB> Moved TRIM-LIST in from make-depends.
;;;;    2004-10-15 <PJB> Added IOTA.
;;;;    2004-08-24 <PJB> Added TRANSPOSE, HASHED-REMOVE-DUPLICATE.
;;;;    2003-06-10 <PJB> Added NSPLIT-LIST
;;;;    2002-12-03 <PJB> Common-Lisp'ized.
;;;;    2001-11-30 <PJB> Added list-remove-elements.
;;;;    199?-??-?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2008
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.LIST"
  (:USE "COMMON-LISP")
  (:EXPORT "DLL-NEXT" "DLL-PREVIOUS" "DLL-NODE" "LIST-TO-DOUBLE-LINKED-LIST"
           "EQUIVALENCE-CLASSES" "SUBSETS" "COMBINE" "IOTA"
           "MAKE-LIST-OF-RANDOM-NUMBERS" "LIST-INSERT-SEPARATOR"
           "NSPLIT-LIST-ON-INDICATOR" "NSPLIT-LIST" "DEEPEST-REC" "DEEPEST" "DEPTH"
           "FLATTEN" "LIST-TRIM" "TRANSPOSE" "AGET" "MEMQ" "PLIST-REMOVE" "PLIST-GET"
           "PLIST-PUT" "HASHED-INTERSECTION" "HASHED-REMOVE-DUPLICATES"
           "ENSURE-LIST" "PROPER-LIST-P"
           "ENSURE-CIRCULAR" "MAKE-CIRCULAR-LIST" "CIRCULAR-LENGTH"
           "TREE-DIFFERENCE" "REPLACE-TREE" "MAPTREE")
  (:DOCUMENTATION
   "This package exports list processing functions.
    
    Copyright Pascal J. Bourguignon 2003 - 2008
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.LIST")


(DEFUN ENSURE-LIST (ITEM)
  "
RETURN: item if it's a list or (list item) otherwise.
"
  (IF (LISTP ITEM) ITEM (LIST ITEM)))


(defun proper-list-p (object)
  "
RETURN: whether object is a proper list
NOTE:   terminates with any kind of list, dotted, circular, etc.
"
  (labels ((proper (current slow)
             (cond ((null current)       t)
                   ((atom current)       nil)
                   ((null (cdr current)) t)
                   ((atom (cdr current)) nil)
                   ((eq current slow)    nil)
                   (t                    (proper (cddr current) (cdr slow))))))
    (and (listp object) (proper object (cons nil object)))))


(defun ensure-circular (list)
  "
If list is not a circular list, then modify it to make it circular.
RETURN: LIST
"
  (if (proper-list-p list)
      (setf (cdr (last list)) list)
      list))

(defun make-circular-list (size &key initial-element)
  "
RETURN: a new circular list of length SIZE.
POST: (circular-length (make-circular-list size)) == (values size 0 size)
"
  (let ((list (make-list size :initial-element initial-element)))
    (setf (cdr (last list)) list)
    list))


(defun circular-length (list)
  "LIST must be either a proper-list or a circular-list, not a dotted-list.
RETURN: the total length ; the length of the stem ; the length of the circle.
"
  (let ((indexes (make-hash-table)))
    (loop
       :for i :from 0
       :for current :on list
       :do (let ((index (gethash current indexes)))
             (if index
                 ;; found loop
                 (return (values i index (- i index)))
                 (setf (gethash current indexes) i)))
       :finally (return (values i)))))



(DEFUN HASHED-SET-REMOVE-DUPLICATES (SEQUENCE &KEY (TEST (FUNCTION EQL))
                                     (KEY (FUNCTION IDENTITY)))
  (LET ((TABLE (MAKE-HASH-TABLE :TEST TEST :SIZE (LENGTH SEQUENCE)))
        (RESULT '()))
    (MAP NIL (LAMBDA (ITEM) (SETF (GETHASH (FUNCALL KEY ITEM) TABLE) ITEM)) SEQUENCE)
    (MAPHASH (LAMBDA (KEY VALUE) (DECLARE (IGNORE KEY)) (PUSH VALUE RESULT)) TABLE)
    RESULT))


(DEFUN HASHED-REMOVE-DUPLICATES (SEQUENCE &KEY (TEST (FUNCTION EQL))
                                 TEST-NOT
                                 (START 0) (END (LENGTH SEQUENCE))
                                 (KEY (FUNCTION IDENTITY))
                                 (FROM-END NIL))
  (WHEN TEST-NOT
    (WARN ":TEST-NOT is deprecated.")
    (SETF TEST (COMPLEMENT TEST-NOT)))
  (LET ((TABLE (MAKE-HASH-TABLE :TEST TEST :SIZE (- END START))))
    (MAP NIL (IF FROM-END 
                 (LAMBDA (ITEM)
                   (LET ((ITEM-KEY (FUNCALL KEY ITEM)))
                     (MULTIPLE-VALUE-BIND (VAL PRE) (GETHASH ITEM-KEY TABLE)
                       (DECLARE (IGNORE VAL))
                       (UNLESS PRE (SETF (GETHASH ITEM-KEY TABLE) ITEM)))))
                 (LAMBDA (ITEM) (SETF (GETHASH (FUNCALL KEY ITEM) TABLE) ITEM)))
         (IF (OR (/= START 0) (/= END (LENGTH SEQUENCE)))
             (SUBSEQ SEQUENCE START END) SEQUENCE))
    (IF (EQ (TYPE-OF SEQUENCE) 'CONS)
        (LET ((RESULT '()))
          (MAPHASH (LAMBDA (KEY VALUE) (DECLARE (IGNORE KEY)) (PUSH VALUE RESULT)) 
                   TABLE)
          (IF (OR (/= START 0) (/= END (LENGTH SEQUENCE)))
              (NCONC (SUBSEQ SEQUENCE 0 START) RESULT (SUBSEQ SEQUENCE END))
              RESULT))
        (IF (OR (/= START 0) (/= END (LENGTH SEQUENCE)))
            (LET ((RESULT (MAKE-SEQUENCE (TYPE-OF SEQUENCE)
                                         (+ START (HASH-TABLE-COUNT TABLE)
                                            (- (LENGTH SEQUENCE) END))))
                  (I START))
              (REPLACE RESULT SEQUENCE :END2 START)
              (MAPHASH (LAMBDA (KEY VALUE) (DECLARE (IGNORE KEY)) 
                               (SETF (AREF RESULT I) VALUE) (INCF I)) TABLE)
              (REPLACE RESULT SEQUENCE :START2 END :START1 I)
              RESULT)
            (LET ((RESULT (MAKE-SEQUENCE (TYPE-OF SEQUENCE) 
                                         (HASH-TABLE-COUNT TABLE)))
                  (I 0))
              (MAPHASH (LAMBDA (KEY VALUE) (DECLARE (IGNORE KEY)) 
                               (SETF (AREF RESULT I) VALUE) (INCF I)) TABLE)
              RESULT)))))


(DEFUN HASHED-DELETE-DUPLICATES (SEQUENCE &KEY (TEST (FUNCTION EQL))
                                 TEST-NOT
                                 (START 0) (END (LENGTH SEQUENCE))
                                 (KEY (FUNCTION IDENTITY))
                                 (FROM-END NIL))
  (HASHED-REMOVE-DUPLICATES 
   SEQUENCE :TEST TEST :TEST-NOT TEST-NOT :START START :END END
   :KEY KEY :FROM-END FROM-END))


(DEFUN HASHED-INTERSECTION (SET1 SET2)
  "
AUTHORS: Paul F. Dietz <dietz@dls.net>
         Thomas A. Russ <tar@sevak.isi.edu>
"
  (DECLARE (OPTIMIZE SPEED (SAFETY 0) (DEBUG 0))
           (LIST SET1 SET2))
  (LET ((TABLE (MAKE-HASH-TABLE :SIZE (LENGTH SET2)))
        (RESULT NIL))
    (DOLIST (E SET2) (SETF (GETHASH E TABLE) T))
    (DOLIST (E SET1) (WHEN (GETHASH E TABLE)
                       (PUSH E RESULT)
                       (SETF (GETHASH E TABLE) NIL)))
    RESULT))


(DEFUN PLIST-PUT (PLIST PROP VALUE)
  "
 Change value in PLIST of PROP to VALUE.
 PLIST is a property list, which is a list of the form
 (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VALUE is any object.
 If PROP is already a property on the list, its value is set to VALUE,
 otherwise the new PROP VALUE pair is added.  The new plist is returned;
 use `(setq x (plist-put x prop val))' to be sure to use the new value.
 The PLIST is modified by side effects.
"
  (SETF (GETF PLIST PROP) VALUE)
  PLIST)


(DEFUN PLIST-GET (PLIST PROP)
  "
 Extract a value from a property list.
 PLIST is a property list, which is a list of the form
 (PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
 corresponding to the given PROP, or nil if PROP is not
 one of the properties on the list.
"
  (GETF PLIST PROP))


(DEFUN PLIST-REMOVE (PLIST PROP)
  "
DO:      (REMF PLIST PROP)
RETURN:  The modified PLIST.
"
  (REMF PLIST PROP)
  PLIST)


(DEFUN MEMQ (ITEM LIST)
  "
RETURN:   (MEMBER ITEM LIST :TEST (FUNCTION EQ))
"
  (MEMBER ITEM LIST :TEST (FUNCTION EQ)))

(DECLAIM (INLINE PLIST-PUT PLIST-GET PLIST-REMOVE MEMQ))


(DEFUN TRANSPOSE (TREE)
  "
RETURN: A tree where all the CAR and CDR are exchanged.
"
  (IF (ATOM TREE)
      TREE
      (CONS (TRANSPOSE (CDR TREE)) (TRANSPOSE (CAR TREE)))))



(DEFUN LIST-TRIM (BAG LIST
                  &KEY (TEST (FUNCTION EQL)) (KEY (FUNCTION IDENTITY)))
  (DO ((LIST (REVERSE LIST) (CDR LIST)))
      ((OR (NULL LIST) (NOT (MEMBER (CAR LIST) BAG :TEST TEST :KEY KEY)))
       (DO ((LIST (NREVERSE LIST) (CDR LIST)))
           ((OR (NULL LIST) (NOT (MEMBER (CAR LIST) BAG :TEST TEST :KEY KEY)))
            LIST)))))


(DEFUN LIST-TRIM-TEST ()
  (EVERY
   (LAMBDA (X) (EQUALP '(D E F) X))
   (LIST
    (LIST-TRIM '(A B C) '( A B C D E F A B C C C ))
    (LIST-TRIM '((A 1)(B 2)(C 3)) '( A B C D E F A B C C ) :KEY (FUNCTION CAR))
    (LIST-TRIM '(:A :B :C) '( A B C D E F A B C C ) :TEST (FUNCTION STRING=))
    (LIST-TRIM '(A B C) '( A B C D E F))
    (LIST-TRIM '(A B C) '( D E F A B C C C )))))


(DEFUN MAPTREE (FUN &REST TREES)
  (COND ((NULL TREES) NIL)
        ((EVERY (FUNCTION NULL)  TREES) NIL)
        ((EVERY (FUNCTION ATOM)  TREES) (APPLY FUN TREES))
        ((EVERY (FUNCTION CONSP) TREES)
         (CONS (APPLY (FUNCTION MAPTREE) FUN (MAPCAR (FUNCTION CAR) TREES))
               (APPLY (FUNCTION MAPTREE) FUN (MAPCAR (FUNCTION CDR) TREES))))
        (T NIL)))


(DEFUN FLATTEN (TREE)
  "
RETURN: A list containing all the elements of the `tree'.
"
  (LOOP WITH RESULT = NIL
     WITH STACK = NIL
     WHILE (OR TREE STACK)
     DO (COND
          ((NULL TREE)
           (SETQ TREE (POP STACK)))
          ((ATOM TREE)
           (PUSH TREE RESULT)
           (SETQ TREE (POP STACK)))
          ((LISTP (CAR TREE))
           (PUSH (CDR TREE) STACK)
           (SETQ TREE (CAR TREE)))
          (T
           (PUSH (CAR TREE) RESULT)
           (SETQ TREE (CDR TREE))))
     FINALLY (RETURN (NREVERSE RESULT))))


(DEFUN DEPTH (TREE)
  "
RETURN:     The depth of the tree.
"
  (IF (ATOM TREE)
      0
      (1+ (APPLY (FUNCTION MAX) 
                 0
                 (DO ((TREE TREE (CDR TREE))
                      (RESULTS '()))
                     ((ATOM TREE) RESULTS)
                   (IF (LISTP (CAR TREE)) (PUSH (DEPTH (CAR TREE)) RESULTS)))))))


(DEFUN DEEPEST-REC (TREE)
  "
RETURN:     The deepest list in the tree.
NOTE:       Recursive algorithm.
SEE-ALSO:   deepest-iti
"
  (LET ((SUBTREE (DELETE-IF (FUNCTION ATOM) TREE)))
    (COND
      ((NULL SUBTREE)    TREE)
      ((EVERY (LAMBDA (ITEM) (EVERY (FUNCTION ATOM) ITEM)) SUBTREE)
       (CAR SUBTREE))
      (T
       (DEEPEST-REC (APPLY 'CONCATENATE 'LIST SUBTREE))))))


(DEFUN DEEPEST (TREE)
  "
RETURN:     The deepest list in the tree.
NOTE:       Iterative algorithm.
SEE-ALSO:   deepest-rec
"
  (DO* ((TREE TREE (APPLY 'CONCATENATE 'LIST SUBTREE))
        (SUBTREE (DELETE-IF (FUNCTION ATOM) TREE)
                 (DELETE-IF (FUNCTION ATOM) TREE)))
       ((OR (NULL SUBTREE)
            (EVERY (LAMBDA (ITEM) (EVERY (FUNCTION ATOM) ITEM)) SUBTREE))
        (IF (NULL SUBTREE) TREE (CAR SUBTREE)))))


(DEFUN NSPLIT-LIST (LIST POSITION &KEY (FROM-END NIL))
  "
PRE:            0<=POSITION<=(LENGTH LIST)
DO:             SPLIT THE LIST IN TWO AT THE GIVEN POSITION.
                (NSPLIT-LIST (LIST 'A 'B 'C) 0) --> NIL ; (A B C)
                (NSPLIT-LIST (LIST 'A 'B 'C) 1) --> (A) ; (B C)
                (NSPLIT-LIST (LIST 'A 'B 'C) 2) --> (A B) ; (C)
                (NSPLIT-LIST (LIST 'A 'B 'C) 3) --> (A B C) ; NIL
POSITION:       POSITION OF THE SPLIT; 
                WHEN FROM-START AND 0<=POSITION<=(LENGTH LIST),
                THAT'S THE LENGTH OF THE FIRST RESULT
FROM-START:     THE DEFAULT, SPLIT COUNTING FROM THE START.
FROM-END:       WHEN SET, COUNT FROM THE END OF THE LIST.
                 (NSPLIT-LIST L P :FROM-END T)
                 === (NSPLIT-LIST L (- (LENGTH L) P))
RETURN:         THE FIRST PART ; THE LAST PART
"
  (IF FROM-END
      (NSPLIT-LIST LIST (- (LENGTH LIST) POSITION))
      (DO* ((PREV NIL  REST)
            (REST LIST (CDR REST)))
           ((OR (NULL REST) (ZEROP POSITION))
            (PROGN
              (IF PREV
                  (SETF (CDR PREV) NIL)
                  (SETF LIST NIL))
              (VALUES LIST REST)))
        (DECF POSITION))))


(DEFUN NSPLIT-LIST-ON-INDICATOR (LIST INDICATOR)
  "
RETURN: a list of sublists of list (the conses from list are reused),
        the list is splited between items a and b for which (indicator a b).
"
  (DECLARE (TYPE (FUNCTION (T T) T) INDICATOR))
  (LET* ((RESULT NIL)
         (SUBLIST LIST)
         (CURRENT LIST)
         (NEXT    (CDR CURRENT)))
    (LOOP :WHILE NEXT :DO
         (IF (FUNCALL INDICATOR (CAR CURRENT) (CAR NEXT))
             (PROGN ;; split
               (SETF (CDR CURRENT) NIL)
               (PUSH SUBLIST RESULT)
               (SETQ CURRENT NEXT)
               (SETQ NEXT (CDR CURRENT))
               (SETQ SUBLIST CURRENT))
             (PROGN ;; keep
               (SETQ CURRENT NEXT)
               (SETQ NEXT (CDR CURRENT)))))
    (PUSH SUBLIST RESULT)
    (NREVERSE RESULT)))


(DEFUN LIST-INSERT-SEPARATOR (LIST SEPARATOR)
  "
RETURN:  A list composed of all the elements in `list'
         with `separator' in-between.
EXAMPLE: (list-insert-separator '(a b (d e f)  c) 'x)
         ==> (a x b x (d e f) x c)
"
  (COND
    ((NULL LIST)       '())
    ((NULL (CDR LIST)) (LIST (CAR LIST)))
    (T  (DO ((RESULT '())
             (ITEMS LIST (CDR ITEMS)))
            ((ENDP ITEMS) (NREVERSE (CDR RESULT)))
          (PUSH (CAR ITEMS) RESULT)
          (PUSH SEPARATOR RESULT)))))



(DEFUN IOTA (COUNT &OPTIONAL (START 0)(STEP 1))
  "
RETURN:   A list containing the elements 
          (start start+step ... start+(count-1)*step)
          The start and step parameters default to 0 and 1, respectively. 
          This procedure takes its name from the APL primitive.
EXAMPLE:  (iota 5) => (0 1 2 3 4)
          (iota 5 0 -0.1) => (0 -0.1 -0.2 -0.3 -0.4)
"
  (WHEN (< 0 COUNT)
    (DO ((RESULT '())
         (ITEM (+ START (* STEP (1- COUNT))) (- ITEM STEP)))
        ((< ITEM START) RESULT)
      (PUSH ITEM RESULT)))) ;;iota


(DEFUN MAKE-LIST-OF-RANDOM-NUMBERS (LENGTH &KEY (MODULO MOST-POSITIVE-FIXNUM))
  "
RETURN:  A list of length `length' filled with random numbers
MODULO:  The argument to RANDOM.
"
  (LOOP WHILE (< 0 LENGTH)
     COLLECT (RANDOM MODULO) INTO RESULT
     DO (SETQ LENGTH (1- LENGTH))
     FINALLY (RETURN RESULT)))


(DEFUN COMBINE (&REST ARGS)
  "
RETURN:  (elt args 0) x (elt args 1) x ... x (elt args (1- (length args)))
         = the set of tuples built taking one item in order from each list
           in args.
EXAMPLE: (COMBINE '(WWW FTP) '(EXA) '(COM ORG))) 
           --> ((WWW EXA COM) (WWW EXA ORG) (FTP EXA COM) (FTP EXA ORG))
"
  (COND
    ((NULL ARGS)        '(NIL))
    ((NULL  (CAR ARGS)) (APPLY (FUNCTION COMBINE) (CDR ARGS)))
    ((CONSP (CAR ARGS)) (MAPCAN (LAMBDA (ITEM)
                                  (APPLY (FUNCTION COMBINE) ITEM (CDR ARGS)))
                                (CAR ARGS)))
    (T                  (MAPCAN (LAMBDA (REST) (LIST (CONS (CAR ARGS) REST)))
                                (APPLY (FUNCTION COMBINE) (CDR ARGS))))))

;; Sets:


(DEFUN SUBSETS (SET)
  "
RETURN: The set of all subsets of the strict SET.
"
  (LOOP
     :WITH CARD = (LENGTH SET)
     :FOR INDICATOR :FROM 0 :BELOW (EXPT 2 CARD)
     :COLLECT (LOOP
                 :FOR INDEX :FROM 0 :BELOW CARD
                 :FOR ITEM :IN SET
                 :NCONC (IF (LOGBITP INDEX INDICATOR) (LIST ITEM) NIL) 
                 :INTO RESULT 
                 :FINALLY (RETURN RESULT)) :INTO RESULT
     :FINALLY (RETURN RESULT)))


(DEFUN EQUIVALENCE-CLASSES (SET &KEY (TEST (FUNCTION EQL))
                            (KEY (FUNCTION IDENTITY)))
  "
RETURN: The equivalence classes of SET, via KEY, modulo TEST.
"
  (LOOP
     :WITH CLASSES = '()
     :FOR ITEM :IN SET
     :FOR ITEM-KEY = (FUNCALL KEY ITEM)
     :FOR CLASS = (CAR (MEMBER ITEM-KEY CLASSES
                               :TEST TEST :KEY (FUNCTION SECOND)))
     :DO (IF CLASS
             (PUSH ITEM (CDDR CLASS))
             (PUSH (LIST :CLASS ITEM-KEY ITEM ) CLASSES))
     :FINALLY (RETURN (MAPCAR (FUNCTION CDDR) CLASSES))))



;; A-lists:

(DEFUN AGET (PLACE INDICATOR &OPTIONAL DEFAULT)
  "
RETURN:   The value of the entry INDICATOR of the a-list PLACE, or DEFAULT.
"
  (LET ((A (ASSOC INDICATOR PLACE)))
    (IF A (CDR A) DEFAULT)))


;; (DEFSETF AGET (PLACE INDICATOR &OPTIONAL DEFAULT) (VALUE)
;;   "
;; DO:       Set or add a new entry INDICATOR in the a-list at PLACE.
;; "
;;   (DECLARE (IGNORE DEFAULT))
;;   (ERROR "THIS DOES NOT WORK. DEALING WITH SETF EXPANSION IS NEEDED HERE!")
;;   (LET ((ACS (GENSYM "AC")))
;;     `(LET* ((,ACS (ASSOC ,INDICATOR ,PLACE)))
;;        (IF ,ACS
;;            (SETF (CDR ,ACS) ,VALUE)
;;            (SETF ,PLACE (ACONS ,INDICATOR ,VALUE ,PLACE)))
;;        ,VALUE)))


(define-setf-expander aget (place indicator &optional default &environment env)
  (declare (ignore default))
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let* ((vindicator (gensym "INDICATOR"))
           (vvalue     (gensym "VALUE"))
           (vstore     (first store-vars))
           (acs        (gensym "PAIR")))
      (values (list* vindicator vars)
              (list* indicator  vals)
              (list  vvalue)
              `(LET* ((,ACS (ASSOC ,vindicator ,reader-form)))
                 (IF ,ACS
                     (SETF (CDR ,ACS) ,vvalue)
                     (let ((,vstore (ACONS ,vindicator ,vvalue ,reader-form)))
                        ,writer-form))
                 ,vvalue)
              `(assoc ,vindicator ,reader-form)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Double-Linked Lists

(DEFUN LIST-TO-DOUBLE-LINKED-LIST (SINGLE)
  "
RETURN:  A double-linked-list.
NOTE:    Use dll-node, dll-next and dll-previous to walk the double-linked-list.
EXAMPLE: (setq d (list-to-double-linked-list '( a b c)))
         ==> (a nil b #0 c (b #0 c #1))
         (dll-node d)
         ==> a
         (dll-next d)
         ==> (b (a nil b #1 c #0) c #0)
         (dll-previous (dll-next d))
         ==> (a nil b #0 c (b #0 c #1))
"
  (LOOP WITH HEAD = NIL
     FOR PREVIOUS = NIL THEN CURRENT
     FOR ELEMENT IN SINGLE
     FOR CURRENT = (LIST ELEMENT PREVIOUS)
     UNLESS HEAD DO (SETQ HEAD CURRENT)
     WHEN PREVIOUS DO (SETF (CDR (CDR PREVIOUS))  CURRENT)
     FINALLY (RETURN HEAD)))


(DEFUN DLL-NODE     (DLL-CONS)
  "
RETURN:  The node in the `dll-cons' double-linked-list node.
"
  (CAR  DLL-CONS))


(DEFUN DLL-PREVIOUS (DLL-CONS)
  "
RETURN:  The previous dll-cons in the `dll-cons' double-linked-list node.
"
  (CADR DLL-CONS))


(DEFUN DLL-NEXT     (DLL-CONS)
  "
RETURN:  The next dll-cons in the `dll-cons' double-linked-list node.
"
  (CDDR DLL-CONS))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tree-difference (a b &key (test (function eql)))
  (cond
    ((funcall test a b)     '=)
    ((or (atom a) (atom b)) `(/= ,a ,b))
    (t (cons (tree-difference (car a) (car b) :test test)
             (tree-difference (cdr a) (cdr b) :test test)))))


(defun replace-tree (dst src)
  "
DO:     Copies the elements of the src tree into the dst tree.
        If dst is missing cons cells, structure sharing occurs.
RETURN: dst
"
  (cond ((atom dst)  src)
        ((atom src) nil)
        (t (if (or (atom (car dst)) (atom (car src)))
               (setf (car dst) (car src))
               (replace-tree (car dst) (car src)))
           (if (or (atom (cdr dst)) (atom (cdr src)))
               (setf (cdr dst) (cdr src))
               (replace-tree (cdr dst) (cdr src)))
           dst)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Sets


;;; (DEFUN CONS-LESSP (A B)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (DO* ( (AP A (CDR AP))
;;;          (AI (CAR AP) (CAR AP))
;;;          (BP B (CDR BP))
;;;          (BI (CAR BP) (CAR BP)) )
;;;       ( (NOT (AND AI BI (EQ AI BI)))
;;;         (ANY-LESSP AI BI) )
;;;     )
;;;   ) ;;cons-lessp


;;; (DEFUN FORMATED-LESSP (A B)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (STRING-LESSP (FORMAT NIL "~S" A) (FORMAT NIL "~S" B))
;;;   );;formated-lessp


;;; (DEFUN SYMBOL-LESSP (A B)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (STRING-LESSP (SYMBOL-NAME A) (SYMBOL-NAME B))
;;;   );;symbol-lessp


;;; (DEFUN VECTOR-LESSP (A B)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (IF (= (LENGTH A) (LENGTH B))
;;;       (LOOP FOR I FROM 0 BELOW (LENGTH A)
;;;             FOR AI = (AREF A I)
;;;             FOR BI = (AREF B I)
;;;             WHILE (EQ AI BI)
;;;             ;;do (show ai bi)
;;;             ;;finally (show ai bi) (show (or bi (not ai)))
;;;             FINALLY (RETURN (ANY-LESSP AI BI)))
;;;     (< (LENGTH A) (LENGTH B)))
;;;   );;vector-lessp


;;; (DEFUN ANY-LESSP (A B)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (IF (EQ (TYPE-OF A) (TYPE-OF B))
;;;       (FUNCALL
;;;        (CDR (ASSOC
;;;              (TYPE-OF A)
;;;              '((BOOL-VECTOR . VECTOR-LESSP)
;;;                (BUFFER . FORMATED-LESSP)
;;;                (CHAR-TABLE . VECTOR-LESSP)
;;;                (COMPILED-FUNCTION . VECTOR-LESSP)
;;;                (CONS . CONS-LESSP)
;;;                (FLOAT . <=)
;;;                (FRAME . FORMATED-LESSP)
;;;                (INTEGER . <=)
;;;                (MARKER . <=)
;;;                (OVERLAY . FORMATED-LESSP)
;;;                (PROCESS . FORMATED-LESSP)
;;;                (STRING . STRING-LESSP)
;;;                (SUBR . FORMATED-LESSP)
;;;                (SYMBOL . SYMBOL-LESSP)
;;;                (VECTOR . VECTOR-LESSP)
;;;                (WINDOW . FORMATED-LESSP)
;;;                (WINDOW-CONFIGURATION . FORMATED-LESSP)
;;;                ))) A B)
;;;     (STRING-LESSP (SYMBOL-NAME (TYPE-OF A))
;;;                   (SYMBOL-NAME (TYPE-OF B))))
;;;   );;any-lessp


;;; (DEFUN LIST-TO-SET-SORTED (LIST)
;;;   "
;;; RETURN: A set, that is a list where duplicate elements from `list' are removed.
;;; NOTE:   This implementation first sorts the list, so its complexity should be
;;;         of the order of O(N*(1+log(N))) [N==(length list)]
;;;         BUT, it's still slower than list-to-set
;;; "
;;;   (IF (NULL LIST)
;;;       NIL
;;;     (LET* ((SORTED-LIST (SORT LIST 'ANY-LESSP))
;;;            (FIRST (CAR SORTED-LIST))
;;;            (REST  (CDR SORTED-LIST))
;;;            (SET NIL))
;;;       (LOOP WHILE REST DO
;;;         (IF (EQ FIRST (CAR REST))
;;;             (SETQ REST (CDR REST))
;;;           (PROGN
;;;             (PUSH FIRST SET)
;;;             (SETQ FIRST (CAR REST)
;;;                   REST  (CDR REST)))))
;;;       SET)));;list-to-set-sorted

;;; (loop for size = 10 then (* 10 size)
;;;       for l1 = (make-list-of-random-numbers size)
;;;       for l2 = (copy-seq l1)
;;;       do
;;;       (format t "~%-----------~%list-to-set        (~s)~%-----------" size)
;;;       (finish-output)
;;;       (time (setf l1 (list-to-set l1)))
;;;       (format t "~%-----------~%list-to-set-sorted (~s)~%-----------" size)
;;;       (finish-output)
;;;       (time (setf l2 (list-to-set l2))))
;; (array->list array) --> (coerce array 'list)

;;;; list.lisp                        --                     --          ;;;;
