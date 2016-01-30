;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               sedit.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A simple sexp editor.
;;;;
;;;;    It is invoked as (sedit sexp), and returns the modified sexp.
;;;;    (The sexp is modified destructively).
;;;;
;;;;     (sedit (copy-tree '(an example)))
;;;;
;;;;    At each interaction loop, it prints the whole sexp, showing the selected
;;;;    sub-sexp, and query a command. The list of commands are:
;;;;
;;;;     q quit                to return the modified sexp from sedit.
;;;;     i in                  to enter inside the selected list.
;;;;     o out                 to select the list containing the selection.
;;;;     f forward n next      to select the sexp following the selection (or out).
;;;;     b backward p previous to select the sexp preceding the selection (or out).
;;;;     s insert              to insert a new sexp before the selection.
;;;;     r replace             to replace the selection with a new sexp.
;;;;     a add                 to add a new sexp after the selection.
;;;;     x cut                 to cut the selection into a *clipboard*.
;;;;     c copy                to copy the selection into a *clipboard*.
;;;;     y paste               to paste the *clipboard* replacing the selection.
;;;;
;;;;    Notice: it uses the unicode characters LEFT_BLACK_LENTICULAR_BRACKET
;;;;    and RIGHT_BLACK_LENTICULAR_BRACKET to show the selected sub-sexp.
;;;;    This could be changed easily modifying SEDIT-PRINT-SELECTION.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-09-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.SEDIT.2"
  (:use "COMMON-LISP")
  (:export "SEDIT"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.SEDIT.2")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (= char-code-limit 1114112)
    (pushnew :unicode *features*)))


(defstruct selection
  parent-list
  sexp)

(defmethod print-object ((selection selection) stream)
  (format stream #+unicode " 【~S】 "
                 #-unicode " [~S] "
                 (selection-sexp selection))
  selection)



(defvar *clipboard* nil)


(defun find-cell (object list)
  (cond
    ((eq (car list) object)       list)
    ((null (cdr list))            nil)
    ((and (atom (cdr list))
          (eq (cdr list) object)) list)
    (t                            (find-cell object (cdr list)))))


(defun unselect (selection)
  (let ((cell (find-cell selection (selection-parent-list selection))))
    (when cell
      (if (eq (car cell) selection)
          (setf (car cell) (selection-sexp selection))
          (setf (cdr cell) (selection-sexp selection))))))

(defun nth-cdr (n list)
  (cond
    ((not (plusp n))   list)
    ((atom list)       list)
    (t (nth-cdr (1- n) (cdr list)))))

(defun select (selection parent index)
  (cond
    ((atom parent)
     (error "Cannot select from an atom."))
    (t
     (let ((cell (nth-cdr index parent)))
       (cond
         ((null cell) (error "cannot select so far"))
         ((atom cell) (setf (selection-parent-list selection) parent
                            (selection-sexp selection) cell
                            (cdr (nth-cdr (1- index) parent)) selection))
         (t           (setf (selection-parent-list selection) parent
                            (selection-sexp selection) (car cell)
                            (car cell) selection)))))))


(defun sedit-find-object (sexp object)
  "Return the cons cell of SEXP where the OBJECT is."
  (cond
    ((atom sexp)      nil)
    ((eq sexp object) nil)
    ((or (eq (car sexp) object)
         (eq (cdr sexp) object)) sexp)
    (t (or (sedit-find-object (car sexp) object)
           (sedit-find-object (cdr sexp) object)))))


(defun sedit-in (root selection)
  "When a non-empty list is selected, change the selection to the first element of the list."
  (declare (ignore root))
  (if (atom (selection-sexp selection))
      (progn (princ "Cannot enter an atom.") (terpri))
      (progn
        (unselect selection)
        (select selection (selection-sexp selection) 0))))

(defun sedit-out (root selection)
  "Change the selection to the parent list of the current selection."
  (let ((gparent (sedit-find-object root (selection-parent-list selection))))
    (when gparent
     (unselect selection)
     (select selection gparent 0))))

(defun sedit-forward (root selection)
  "Change the selection to the element following the current selection, or the parent if it's the last."
  (let ((index (position selection (selection-parent-list selection))))
    (if (or (null index)
            (<= (length (selection-parent-list selection))  (1+ index)))
        (sedit-out root selection)
        (progn (unselect selection)
               (select selection (selection-parent-list selection) (1+ index))))))

(defun sedit-backward (root selection)
  "Change the selection to the element preceeding the current selection, or the parent if it's the first."
  (let ((index (position selection (selection-parent-list selection))))
    (if (or (null index) (<= index 0))
        (sedit-out root selection)
        (progn (unselect selection)
               (select selection (selection-parent-list selection) (1- index))))))


(defun sedit-enlist (root selection)
  "Put the selection in a new list."
  (declare (ignore root))
  (setf (selection-sexp selection) (list (selection-sexp selection))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun sedit-splice (root selection)
  "Splice the elements of the selection in the parent."
  (let* ((parent   (selection-parent-list selection))
         (selected (selection-sexp selection))
         (index    (position selection parent)))
    (sedit-out root selection)
    (setf (selection-sexp selection)
          (append (subseq parent 0 index)
                  (ensure-list selected)
                  (subseq parent (1+ index))))))

(defun sedit-slurp (root selection)
  "Append to the selection the element following it."
  (declare (ignore root))
  (let ((index (position selection (selection-parent-list selection))))
    (when (and index
               (< (1+ index) (length (selection-parent-list selection))))
      (setf (selection-sexp selection)
            (append (ensure-list (selection-sexp selection))
                    (list (pop (cdr (nth-cdr index (selection-parent-list selection))))))))))

(defun sedit-barf (root selection)
  "Split out the last element of the selection."
  (declare (ignore root))
  (let ((index (position selection (selection-parent-list selection))))
    (when (and index (consp (selection-sexp selection)))
      (let ((barfed (first (last (selection-sexp selection)))))
        (setf (selection-sexp selection) (butlast (selection-sexp selection)))
        (insert barfed (selection-parent-list selection) :after selection)))))

(defun sedit-cut (root selection)
  "Save the selection to the *CLIPBOARD*, and remove it."
  (setf *clipboard* (selection-sexp selection))
  (let ((gparent (sedit-find-object root (selection-parent-list selection))))
    (if (eq (car gparent) (selection-parent-list selection))
        (setf (car gparent) (delete selection (selection-parent-list selection)))
        (setf (cdr gparent) (delete selection (selection-parent-list selection))))
    (select selection gparent 0)))

(defun sedit-copy (root selection)
  "Save the selection to the *CLIPBOARD*."
  (declare (ignore root))
  (setf *clipboard* (copy-tree (selection-sexp selection))))

(defun sedit-paste (root selection)
  "Replace the selection by the contents of teh *CLIPBOARD*."
  (declare (ignore root))
  (setf (selection-sexp selection) (copy-tree *clipboard*)))

(defun sedit-replace (root selection)
  "Replace the selection by the expression input by the user."
  (declare (ignore root))
  (princ "replacement sexp: " *query-io*)
  (setf (selection-sexp selection) (read *query-io*)))

(defun insert (object list where reference)
  (ecase where
    ((:before)
     (cond
       ((null list) (error "Cannot insert in an empty list."))
       ((eq (car list) reference)
        (setf (cdr list) (cons (car list) (cdr list))
              (car list) object))
       (t (insert object (cdr list) where reference))))
    ((:after)
     (cond
       ((null list) (error "Cannot insert in an empty list."))
       ((eq (car list) reference)
        (push object (cdr list)))
       (t (insert object (cdr list) where reference))))))

(defun sedit-insert (root selection)
  "Insert the expression input by the user before the selection."
  (princ "sexp to be inserted before: " *query-io*)
  (let ((new-sexp (read *query-io*)))
    (if (eq (first (selection-parent-list selection)) selection)
        (let ((gparent (sedit-find-object root (selection-parent-list selection))))
          (setf (selection-parent-list selection)
                (if (eq (car gparent) (selection-parent-list selection))
                    (setf (car gparent) (cons new-sexp (selection-parent-list selection)))
                    (setf (cdr gparent) (cons new-sexp (selection-parent-list selection))))))
        (insert new-sexp (selection-parent-list selection) :before selection))))

(defun sedit-add (root selection)
  "Insert the expression input by the user after the selection."
  (declare (ignore root))
  (princ "sexp to be inserted after: " *query-io*)
  (let ((new-sexp (read *query-io*)))
    (insert new-sexp (selection-parent-list selection) :after selection)))



(defun sedit (sexp)
  "Edit the SEXP; return the new sexp."
  (terpri)
  (princ "Sexp Editor:")
  (terpri)
  (let* ((root (list sexp))
         (selection (make-selection)))
    (select selection root 0)
    (setf (first root) selection)
    (unwind-protect
         (loop
           (pprint root)
           (terpri) (princ "> " *query-io*)
           (let ((command (read *query-io*)))
             (case command
               ((q quit)      (return))
               ((i in)        (sedit-in      root selection))
               ((o out)       (sedit-out     root selection))
               ((f forward  n next)     (sedit-forward  root selection))
               ((b backward p previous) (sedit-backward root selection))
               ((s insert)    (sedit-insert  root selection)) ; before
               ((r replace)   (sedit-replace root selection)) ; in place
               ((a add)       (sedit-add     root selection)) ; after
               ((x cut)       (sedit-cut     root selection))
               ((c copy)      (sedit-copy    root selection))
               ((y paste)     (sedit-paste   root selection))
               ((l enlist)    (sedit-enlist  root selection))
               ((e splice)    (sedit-splice  root selection))
               ((u slurp)     (sedit-slurp   root selection))
               ((v barf)      (sedit-barf    root selection))
               (otherwise
                (princ "Please use one of these commands:")
                (terpri)
                (princ "quit, in, out, forward, backward, insert, replace, add, cut, copy, paste, slurp, barf.")
                (terpri)))))
      (unselect selection))
    (first root)))

;;;; THE END ;;;;
