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
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
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

(defstruct selection
  parent-list
  sexp)

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
                            (selection-sexp selection) (cdr cell)
                            (cdr cell) selection))
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

(declaim (ftype (function (t t)) sedit-print))

(defun sedit-print-selection (selection)
  (princ "【")
  (sedit-print (selection-sexp selection) selection)
  (princ "】"))

(defun sedit-print-list (list selection)
  (sedit-print (car list) selection)
  (unless (null (cdr list))
    (princ " ")
    (sedit-print-list (cdr list) selection)))

(defun sedit-print (sexp selection)
  (cond
    ((eq sexp selection) (sedit-print-selection selection))
    ((atom sexp) (prin1 sexp))
    (t (princ "(")
       (unless (null sexp)
         (sedit-print-list sexp selection))
       (princ ")"))))

(defun sedit-in (root selection)
  (declare (ignore root))
  (if (atom (selection-sexp selection))
      (progn (princ "Cannot enter an atom.") (terpri))
      (progn
        (unselect selection)
        (select selection (selection-sexp selection) 0))))

(defun sedit-out (root selection)
  (let ((gparent (sedit-find-object root (selection-parent-list selection))))
    (when gparent
     (unselect selection)
     (select selection gparent 0))))

(defun sedit-forward (root selection)
  (let ((index (position selection (selection-parent-list selection))))
    (if (or (null index)
            (<= (length (selection-parent-list selection))  (1+ index)))
        (sedit-out root selection)
        (progn (unselect selection)
               (select selection (selection-parent-list selection) (1+ index))))))

(defun sedit-backward (root selection)
  (let ((index (position selection (selection-parent-list selection))))
    (if (or (null index) (<= index 0))
        (sedit-out root selection)
        (progn (unselect selection)
               (select selection (selection-parent-list selection) (1- index))))))

(defun sedit-cut (root selection)
  (setf *clipboard* (selection-sexp selection))
  (let ((gparent (sedit-find-object root (selection-parent-list selection))))
    (if (eq (car gparent) (selection-parent-list selection))
        (setf (car gparent) (delete selection (selection-parent-list selection)))
        (setf (cdr gparent) (delete selection (selection-parent-list selection))))
    (select selection gparent 0)))

(defun sedit-copy (root selection)
  (declare (ignore root))
  (setf *clipboard* (copy-tree (selection-sexp selection))))

(defun sedit-paste (root selection)
  (declare (ignore root))
  (setf (selection-sexp selection) (copy-tree *clipboard*)))

(defun sedit-replace (root selection)
  (declare (ignore root))
  (princ "replacement sexp: " *query-io*)
  (setf (selection-sexp selection) (read *query-io*)))

(defun insert (object list where reference)
  (ecase where
    ((:before)
     (cond
       ((null list) (error "Cannot insert in an empty list."))
       ((eq (cadr list) reference)
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
  (declare (ignore root))
  (princ "sexp to be inserted after: " *query-io*)
  (let ((new-sexp (read *query-io*)))
    (insert new-sexp (selection-parent-list selection) :after selection)))



(defun sedit (sexp)
  (terpri)
  (princ "Sexp Editor:")
  (terpri)
  (let* ((root (list sexp))
         (selection (make-selection)))
    (select selection root 0)
    (setf (first root) selection)
    (unwind-protect
         (loop :do
            (sedit-print-list root selection)
            (terpri) (princ "> " *query-io*)
            (let ((command (read *query-io*)))
              (case command
                ((q quit)      (loop-finish))
                ((i in)        (sedit-in root selection))
                ((o out)       (sedit-out root selection))
                ((f forward n next)   (sedit-forward root selection))
                ((b backward p previous)  (sedit-backward root selection))
                ((s insert)    (sedit-insert root selection)) ; before
                ((r replace)   (sedit-replace root selection)) ; in place
                ((a add)       (sedit-add root selection)) ; after
                ((x cut)       (sedit-cut root selection))
                ((c copy)      (sedit-copy root selection))
                ((y paste)     (sedit-paste root selection))
                (otherwise
                 (princ "Please use one of these commands:")
                 (terpri)
                 (princ "quit, in, out, forward, backward, insert, replace, add, cut, copy, paste.")
                 (terpri)))))
      (unselect selection))
    (first root)))

;;;; THE END ;;;;
