;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               llrbtree.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implementation of Left Leaning Red Black Trees.
;;;;    Robert Sedgewick's algorithms.
;;;;    http://www.cs.princeton.edu/~rs
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2009-09-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2009 - 2015
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
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LLRBTREE"
  (:nicknames "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LEFT-LEANING-RED-BLACK-TREE")
  (:use "COMMON-LISP")
  (:export "MAKE-TREE" "TREEP" "TREE-EMPTY-P"
           "TREE-COUNT" "TREE-LESSP"
           "TREE-GET" ; (setf tree-get)
           "TREE-DELETE" "TREE-DELETE-MIN" "TREE-DELETE-MAX"
           "MAP-TREE" "WITH-TREE-ITERATOR"
           "TREE-CLEAR")
  (:documentation "
Implementation of Left Leaning Red Black Trees.
Robert Sedgewick's algorithms.
http://www.cs.princeton.edu/~rs



License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2009 - 2015
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LLRBTREE")


(defstruct node
  (color  :red   :type (member :red :black)) ; color of this node
  (left   nil    :type (or null node))       ; the left child
  (right  nil    :type (or null node))       ; the right child
  (key    nil    :type t)          ; the key of the object of the node
  (value  nil    :type t))         ; the value of the node

(defun node-red-p (node)
  (and node (eq (node-color node) :red)))

(defstruct (tree (:predicate treep)
                 (:constructor %make-tree)
                 (:copier %copy-tree))
  (root   nil    :type (or null node))  ; the root node
  (count  0      :type (integer 0))   ; the number of nodes in the tree
  (lessp  (function <)                ; the key order
          :type t; (function (t t) t)
          :read-only t))

(setf (documentation 'tree-root  'function) "The root node."
      (documentation 'tree-count 'function) "The number of nodes in the tree."
      (documentation 'tree-lessp 'function) "The key order."
      (documentation 'treep      'function) "Whether the object is a left leaning red-black tree.")



(defun walk-nodes-infix (node fun)
  (when node
    (walk-nodes-infix (node-left  node) fun)
    (funcall fun node)
    (walk-nodes-infix (node-right node) fun)))


(defun walk-nodes-infix/from-end (node fun)
  (when node
    (walk-nodes-infix/from-end (node-right node) fun)
    (funcall fun node)
    (walk-nodes-infix/from-end (node-left  node) fun)))


(defun walk-nodes-suffix (node fun)
  (when node
    (walk-nodes-infix (node-left  node) fun)
    (walk-nodes-infix (node-right node) fun)
    (funcall fun node)))


(declaim (inline  count-nodes))

(defun count-nodes (node)
  (let ((count 0))
    (walk-nodes-infix node (lambda (node) (declare (ignore node)) (incf count)))
    count))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Left-Leaning Red-Black Tree Invariant.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline equiv))
  (defun equiv (a b) (or (and a b) (and (not a) (not b))))
  (defmacro imply (p q) `(or (not ,p) ,q)))


(defun invariant (tree)
  (check-type tree tree)
  (assert (equiv (zerop (tree-count tree))
                 (tree-empty-p tree)))
  (assert (equiv (null  (tree-root  tree))
                 (tree-empty-p tree)))
  (assert (imply (tree-root tree)
                 (eq (node-color (tree-root tree)) :black))
          (tree) "The root link must be black.")
  (let ((count  (count-nodes (tree-root tree))))
    (assert (= (tree-count tree) count)
            (tree)
            "There must be TREE-COUNT = ~A nodes in the tree.  ~
             Found ~A nodes by walking."
            (tree-count tree) count))
  
  (let ((nodes '()))
    ;; check the nodes are ordered.
    (and (tree-root tree)
         (walk-nodes-infix/from-end (tree-root tree) (lambda (node) (push node nodes))))
    (assert (= (tree-count tree) (length nodes)))
    (loop
       :for (a b) :on nodes
       :while b
       :always (funcall (tree-lessp tree) (node-key a) (node-key b)))
    (assert (= (tree-count tree) (length nodes))
            (tree)
            "There must be TREE-COUNT = ~A nodes in the tree.  Found ~A nodes by closure."
            (tree-count tree) (length nodes))
    (loop
       :for node :in nodes
       :do (assert (member (node-color node) '(:black :red))
                   (node) "Node color must be :black or :red ; found ~A"
                   (node-color node)))
     
    (loop                 ; LLRBT have different constraints than RBT.
       :named llrbt-constraints
       :for node :in nodes
       :do (assert
            (imply (node-red-p (node-left node))
                   (and (not (node-red-p (node-right node)))
                        (or (null (node-left (node-left node)))
                            (not (node-red-p (node-left (node-left (node-left node))))))))
            (node))))
  
  (let ((h (make-hash-table)))
    ;; check that all paths to leaves have the same number of black links
    (setf (gethash nil h) 1)
    (and (tree-root tree)
         (walk-nodes-suffix (tree-root tree)
                            (lambda (node)
                              (assert (= (gethash (node-left  node) h)
                                         (gethash (node-right node) h))
                                      (node)
                                      "Children have different black link counts: ~A and ~A"
                                      (gethash (node-left  node) h)
                                      (gethash (node-right node) h))
                              (setf (gethash node h)
                                    (+ (gethash (node-left  node) h)
                                       (if (eq (node-color node) :black)
                                           1
                                           0)))))))

  :success)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;



(defun make-tree (&key (lessp (function <)))
  "
RETURN: a new empty TREE.
"
  (%make-tree :lessp lessp))


(defun tree-empty-p (tree)
  "
RETURN: Whether the TREE contains no element.
"
  (and (treep tree) (null (tree-root tree))))


(defun tree-clear (tree)
  "
DO:     Remove all the entries in the TREE.
RETURN: TREE
"
  (check-type tree tree)
  (setf (tree-root  tree) nil
        (tree-count tree) 0)
  tree)


(defun map-tree (fun tree &key (start nil startp) (end nil endp) from-end)
  "
DO:          Calls (funcall FUN key value) for the entries in the TREE,
             by increasing, when (NOT FROM-END),  or decreasing when FROM-END,
             key order.
FROM-END:    Whether the we start from the end.
START, END:  Limiting values for the key.  Only entries whose key k is
             such as START <= k < END are passed to FUN.
RETURN:      Nothing.
"
  (check-type tree tree)
  (let ((lessp (tree-lessp tree))
        (root  (tree-root  tree)))
    (when root
     (if from-end
         (funcall (function walk-nodes-infix/from-end)
                  root
                  (let ((doit (not end)))
                    (lambda (node)
                      (if doit
                          (if startp
                              (if (funcall lessp
                                           (node-key node) start)
                                  (setf doit nil)
                                  (funcall fun (node-key node) (node-value node)))
                              (funcall fun (node-key node) (node-value node)))
                          (when endp
                            (unless (funcall lessp endp (node-key node))
                              (setf doit t)
                              (funcall fun (node-key node) (node-value node))))))))
         (funcall (function walk-nodes-infix)
                  root
                  (let ((doit (not startp)))
                    (lambda (node)
                      (if doit
                          (if endp
                              (if (funcall lessp
                                           (node-key node) end)
                                  (funcall fun (node-key node) (node-value node))
                                  (setf doit nil))
                              (funcall fun (node-key node) (node-value node)))
                          (when startp
                            (unless (funcall lessp (node-key node) start)
                              (setf doit t)
                              (funcall fun (node-key node) (node-value node)))))))))))
  (values))


(defun make-iterator (tree &rest args
                      &key ;; (start nil startp) (end nil endp) ; not yet
                      from-end)
  "
RETURN: An iterator to walk the nodes of the TREE.
        This iterator is a function that called repeatitively will
        return successively each key value pairs in the tree (as three
        values, T; key; value), and when done, returns NIL.
NOTE:   The data is collected before iterating, so you can modify the
        tree at will during iteration.
"
  ;; (declare (ignore start end)) ; not yet
  (check-type tree tree)
  (let ((data '()))
    (apply (if from-end              ; notice we push a reversed list.
               (function walk-nodes-infix)
               (function walk-nodes-infix/from-end))
           (tree-root tree)
           (lambda (node) (push (cons (node-key node) (node-value node)) data))
           args)
    (lambda ()
      (if data
          (let ((pair (pop data)))
            (values t (car pair) (cdr pair)))
          (values nil nil nil)))))


(defmacro with-tree-iterator ((name tree &rest args
                                    &key ;; start end ; not yet
                                    from-end) &body body)
  "
Within the lexical scope of the BODY, NAME is defined via macrolet
such that successive invocations of (NAME) return the items, one by
one, from the LLRBL:TREE that is obtained by evaluating TREE
only once.

An invocation (NAME) returns three values as follows:

1. A generalized boolean that is true if an entry is returned.
2. The key from the tree entry.
3. The value from the tree entry.
   
After all entries have been returned by successive invocations
of (NAME), then only one value is returned, namely nil.

It is unspecified what happens if any of the implicit interior state
of an iteration is returned outside the dynamic extent of the
WITH-TREE-ITERATOR form such as by returning some closure over
the invocation form.

Any number of invocations of WITH-TREE-ITERATOR can be nested,
and the BODY of the innermost one can invoke all of the locally
established macros, provided all of those macros have distinct NAMEs.
"
  (declare (ignore from-end))
  ;; (declare (ignore start end from-end))
  (let ((iter (gensym)))
    `(let ((,iter (make-iterator ,tree ,@args)))
       (declare (ignorable ,iter))
       (macrolet ((,name () (list 'funcall ',iter)))
         (locally ,@body)))))


;;;;;;;;;;;;;;;;;;;;;;
;; Search: tree-get
;;

(defun search-node-for-key (node key lessp)
  "
DO:     Search the node with the key,
        or the parent where a node with key would be inserted.
NODE:   The root node of the tree.
KEY:    The key of the object to search.
RETURN: found-node ; foundp
NODE:   (not foundp) => NIL ; foundp => the node such as
        (and (not (funcall lessp key (node-key node))
             (not (funcall lessp (node-key node) key))
FOUNDP: Whether a node with the KEY was found from NODE.
"
  (cond
    ((null node)
     (values nil nil))
    ((funcall lessp key (node-key node))
     (search-node-for-key (node-left  node) key lessp))
    ((funcall lessp (node-key node) key)
     (search-node-for-key (node-right node) key lessp))
    (t
     (values node t))))


(defun tree-get (key tree &optional default)
  "
RETURN: the object in TREE whose key is the same as KEY under the
        tree's equivalence test (implied by TREE-LESSP).  If there is
        no such entry, DEFAULT is returned ;
        PRESENT-P is true if an entry is found; otherwise, it is false.

NOTE:   SETF may be used with TREE-GET to modify the value associated
        with a given key, or to add a new entry.  When a TREE-GET form
        is used as a SETF place, any default which is supplied is
        evaluated according to normal left-to-right evaluation rules,
        but its value is ignored.
"
  (check-type tree tree)
  (multiple-value-bind (node foundp)
      (search-node-for-key (tree-root tree) key (tree-lessp tree))
    (if foundp            ; node exists, found it.
        (values (node-value node) t)
        (values default           nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;

(declaim (inline rotate-left rotate-right lean-left lean-right
                 split-four-node move-red-left move-red-right))

(defun rotate-left (h)
  (assert (and h (node-red-p (node-right h))))
  (let ((x (node-right h)))
    (setf (node-right h) (node-left x)
          (node-left x) h)
    x))


(defun lean-left (h)
  "Same as rotate-left, plus update colors."
  (let ((x (node-right h)))
    (setf (node-right h) (node-left x)
          (node-left x) h
          (node-color x) (node-color h)
          (node-color h) :red)
    x))


(defun rotate-right (h)
  (assert (and h (node-red-p (node-left h))))
  (let ((x (node-left h)))
    (setf (node-left h) (node-right x)
          (node-right x) h)
    x))


(defun lean-right (h)
  "Same as rotate-right, plus update colors."
  (let ((x (node-left h)))
    (setf (node-left h) (node-right x)
          (node-right x) h
          (node-color x) (node-color h)
          (node-color h) :red)
    x))


(defun split-four-node (h)
  (setf h (rotate-right h)
        (node-color (node-left h)) :black)
  h)


(defun move-red-left (h)
  (setf (node-color h)             :black
        (node-color (node-left h)) :red)
  (if (node-red-p (node-left (node-right h)))
      (setf (node-right h) (rotate-right (node-right h))
            h (rotate-left h))
      (setf (node-color (node-right h)) :red))
  h)


(defun move-red-right (h)
  (setf (node-color h)              :black
        (node-color (node-right h)) :red)
  (if (node-red-p (node-left (node-left h)))
      (setf h (rotate-right h)
            (node-color h)             :red
            (node-color (node-left h)) :black)
      (setf (node-color (node-left h)) :red))
  h)


(defun left-most-node (h)
  (if (null (node-left h))
      h
      (left-most-node (node-left h))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert: (setf tree-get)
;;

(defun insert-node (h key value tree lessp)
  (if (null h)
      (progn
        ;; insert at the bottom.
        (incf (tree-count tree))
        (make-node :key key :value value :color :red))
      (progn
        
        (when (and (node-red-p (node-left h))
                   (node-red-p (node-left (node-left h))))
          (setf h (split-four-node h)))

        (cond
          ((funcall lessp key (node-key h))
           (setf (node-left  h) (insert-node (node-left  h) key value tree lessp)))
          ((funcall lessp (node-key h) key)
           (setf (node-right h) (insert-node (node-right h) key value tree lessp)))
          (t
           (setf (node-value h) value)))
        
        (when (node-red-p (node-right h))
          (setf h (lean-left h)))
        h)))


(defun (setf tree-get) (new-value key tree &optional default)
  "
DO:     SETF may be used with TREE-GET to modify the value associated
        with a given key, or to add a new entry.  When a TREE-GET form
        is used as a SETF place, any default which is supplied is
        evaluated according to normal left-to-right evaluation rules,
        but its value is ignored.
RETURN: NEW-VALUE.
"
  (declare (ignore default))
  (check-type tree tree)
  (setf (tree-root tree) (insert-node (tree-root tree)
                                      key new-value
                                      tree (tree-lessp tree))
        (node-color (tree-root tree)) :black)
  new-value)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete: tree-delete
;;


(defun delete-min (h)
  (when (node-left h)
    (when (and (not (node-red-p (node-left h)))
               (not (node-red-p (node-left (node-left h)))))
      (setf h (move-red-left h)))
    (setf (node-left h) (delete-min (node-left h))) 
    (when (node-red-p (node-right h))
      (setf h (lean-right h)))
    h)) 


(defun tree-delete-min (tree)
  "
DO:      Delete the minimum entry of the TREE.
RETURN:  Whether an entry has been deleted.
"
  (when (tree-root tree)
    (setf (tree-root tree) (delete-min (tree-root tree)))
    (when (tree-root tree)
      (setf (node-color (tree-root tree)) :black))
    (decf (tree-count tree))
    t))



(defun delete-max (h)
  (if (node-right h)
      (progn
        (when (node-red-p (node-left h))
          (setf h (lean-right h)))
        (when (and (not (node-red-p (node-right h)))
                   (not (node-red-p (node-left (node-right h)))))
          (setf h (move-red-right h)))
        (setf (node-right h) (delete-max (node-right h)))
        (when (node-red-p (node-right h))
          (setf h (lean-left h)))
        h)
      (progn
        (when (node-left h)
          (setf (node-color (node-left h)) :black))
        (node-left h)))) 


(defun tree-delete-max (tree)
  "
DO:      Delete the maximum entry of the TREE.
RETURN:  Whether an entry has been deleted.
"
  (when (tree-root tree)
    (setf (tree-root tree) (delete-max (tree-root tree)))
    (when (tree-root tree)
      (setf (node-color (tree-root tree)) :black))
    (decf (tree-count tree))
    t))


(defun delete-node (h key tree lessp)
  (when h
    (if (funcall lessp key (node-key h))
        (progn                          ; LEFT
          (when (and (not (node-red-p (node-left h)))
                     (not (node-red-p (node-left (node-left h)))))
            (setf h (move-red-left h)))
          (setf (node-left  h) (delete-node (node-left  h) key tree lessp))
          (when (node-red-p (node-right h))
            (setf h (lean-left h)))
          h) 
        (progn
          (when (node-red-p (node-left h))
            (setf h  (lean-right h)))
          (if (and (not (funcall lessp key (node-key h)))
                   (not (funcall lessp (node-key h) key))
                   (null (node-right h))) 
              (progn                     ; EQUAL (at bottom)
                (decf (tree-count tree)) ; delete node
                nil)
              (progn 
                (when (and (not (node-red-p (node-right h)))
                           (not (node-red-p (node-left (node-right h)))))
                  (setf h (move-red-right h)))
                (if (and (not (funcall lessp key (node-key h)))
                         (not (funcall lessp (node-key h) key)))
                    (let ((successor  (left-most-node (node-right h))))
                                        ; EQUAL (not at bottom)
                      (decf (tree-count tree))
                      (setf (node-key h)   (node-key   successor)
                            (node-value h) (node-value successor)
                            (node-right h) (delete-min (node-right h))))
                                        ; RIGHT
                    (setf (node-right h) (delete-node (node-right h) key tree lessp)))
                (when (node-red-p (node-right h))
                  (setf h (lean-left h)))
                h)))))) 


(defun tree-delete (key tree)
  "
DO:     Removes the entry for KEY in TREE, if any.
RETURN: true if there was such an entry, or false otherwise.
"
  (check-type tree tree)
  (let ((old-count (tree-count tree)))
    (setf (tree-root tree) (delete-node (tree-root tree) key tree (tree-lessp tree)))
    (when (tree-root tree)
      (setf (node-color (tree-root tree)) :black))
    (/= old-count (tree-count tree))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOT File Generator
;;;

(defparameter *dot-counter* 0)
(defgeneric generate-dot (graph))
(defmethod generate-dot ((self tree))
  (let ((id (incf *dot-counter*)))
    (with-open-file (dot (format nil "tree-~5,'0D.dot" id)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (format dot "digraph tree~D {~%" id)
      (format dot " rankdir=TD;~%")
      (let ((node-indices (make-hash-table)))
        (loop
           :for node :in (let ((nodes '()))
                           (and (tree-root self)
                                (walk-nodes-infix/from-end
                                 (tree-root self)
                                 (lambda (node) (push node nodes))))
                           nodes)
           :for index :from 0
           :do (setf (gethash node node-indices) index))
        (format dot "\"ROOT\" -> \"N~D\";~%"
                (gethash (tree-root self) node-indices))
        (maphash (lambda (node index)
                   (format dot "N~D[shape=\"record\",style=\"bold\",color=\"~(~A~)\",~
                                label=\"<left> |<parent> ~A : ~A |<right> \"];~%"
                           index
                           (node-color node)
                           (node-key node)
                           (node-value node)))
                 node-indices)
        (maphash (lambda (node index)
                   (when (node-left node)
                     (format dot "\"N~D\":left -> \"N~D\":parent;~%"
                             index (gethash (node-left node) node-indices)))
                   (when (node-right node)
                     (format dot "\"N~D\":right -> \"N~D\":parent;~%"
                             index (gethash (node-right node) node-indices))))
                 node-indices))
      (format dot "}~%"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASCII Tree Dump
;;;


(defun indentation (level leftp)
  (format nil "~{~A~}" (loop :repeat level :collect (if leftp "|    " "     "))))

(defun concat (&rest args)
  (apply (function concatenate) 'string args))

(defun string-butlast (str)
  (if (plusp (length str))
      (subseq str 0 (1- (length str)))
      str))

(defparameter *red*
  (map 'string (function code-char)
       #(#x1b #x5b #x33 #x30 #x6d #x1b #x5b #x34 #x31 #x6d)))

(defparameter *black*
  (map 'string (function code-char)
       #(#x1b #x5b #x33 #x37 #x6d #x1b #x5b #x34 #x30 #x6d)))

(defparameter *normal*
  (map 'string (function code-char)
       #(#x1b #x5b #x30 #x6d)))

(defgeneric dump (object &optional indentation bar))

(defmethod dump ((self null) &optional (indentation "") (bar " "))
  (declare (ignorable self))
  (format t "~A~A~A+---- NIL~A~%" indentation bar *black* *normal*))

(defmethod dump ((self node) &optional (indentation "") (bar " "))
  (dump (node-left  self) (concat indentation bar "    ") "|")
  (format t "~A~A    |~%" indentation bar)
  (format t "~A~A~A+---- ~A: ~A~A~%" indentation bar
          (if (node-red-p self) *red* *black*) 
          (node-key self) (node-value self)
          *normal*)
  (format t "~A~A    |~%" indentation bar)
  (dump (node-right self) (concat indentation bar "    ") "|"))


(defmethod dump ((self tree) &optional (indentation "") (bar " "))
  (format t "Tree: ~%")
  (dump (tree-root self) indentation bar))


(defmethod print-object ((self node) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~S --> ~S ~%~
                    ~:[black~;red~], ~A left subnode~:*~P, ~A right subnode~:*~P"
            (node-key self) (node-value self)
            (node-red-p self)
            (count-nodes (node-left  self))
            (count-nodes (node-right self)))
    #||
    (let ((*standard-output* stream))
        (terpri)
        (dump self))
    ||#)
  self)


(defmethod print-object ((self tree) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A nodes" (tree-count self)))
  self)



#||

(trace move-red-right delete-node delete-min
       insert
       (tree-get
        :pre  (progn (print :pre)  (invariant (second EXT:*TRACE-ARGS*))
                     (generate-dot (second EXT:*TRACE-ARGS*)) (print *dot-counter*))
        :post (progn (print :post) (invariant (second EXT:*TRACE-ARGS*))
                     (generate-dot (second EXT:*TRACE-ARGS*)) (print *dot-counter*)))
       ((setf tree-get)
        :pre  (progn (print :pre)  (invariant (third  EXT:*TRACE-ARGS*))
                     (generate-dot (third  EXT:*TRACE-ARGS*)) (print *dot-counter*))
        :post (progn (print :post) (invariant (third  EXT:*TRACE-ARGS*))
                     (generate-dot (third  EXT:*TRACE-ARGS*)) (print *dot-counter*)))
       (tree-delete
        :pre  (progn (print :pre)  (invariant (second EXT:*TRACE-ARGS*))
                     (generate-dot (second EXT:*TRACE-ARGS*)) (print *dot-counter*))
        :post (progn (print :post) (invariant (second EXT:*TRACE-ARGS*))
                     (generate-dot (second EXT:*TRACE-ARGS*)) (print *dot-counter*)))
       (tree-clear
        :pre  (progn (print :pre)  (invariant (first  EXT:*TRACE-ARGS*))
                     (generate-dot (first  EXT:*TRACE-ARGS*)) (print *dot-counter*))
        :post (progn (print :post) (invariant (first  EXT:*TRACE-ARGS*))
                     (generate-dot (first  EXT:*TRACE-ARGS*)) (print *dot-counter*))))

||#

;;;; THE END ;;;;
