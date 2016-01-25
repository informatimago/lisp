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
;;;;    At each interaction loop, it prints the whole sexp, showing
;;;;    the selected sub-sexp, and query a command. Use the help
;;;;    command to get the list of commands.
;;;;
;;;;    Notice: it uses the unicode characters LEFT_BLACK_LENTICULAR_BRACKET
;;;;    and RIGHT_BLACK_LENTICULAR_BRACKET to show the selected sub-sexp.
;;;;    This could be changed easily modifying SEDIT-PRINT-SELECTION.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-10-15 <PJB> Abstracted away BUFFER.  Added bindings, loading and saving.
;;;;    2010-09-08 <PJB> Created.
;;;;BUGS
;;;;
;;;;    Command table and bindings is implemented quick & dirty.
;;;;
;;;;    There's some new code in sedit2.lisp; a diff3 should be done
;;;;    with the previous version and new commands merged in.
;;;;
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

(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.SEDIT"
  (:use "COMMON-LISP")
  (:export "SEDIT"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.SEDIT")


#|

The buffer is represented by two objects: a root cell, and a selection
structure.

The root cell is a list containing the edited sexp as single element.

The selection structure is present inside the root sexp, and contain a
reference to the list where it is present in, and the sub-sexp that is
selected.  (Note when the whole sexp is selected, then the selection
structure is the element of the root cell).

|#


(defstruct selection
  parent-list
  sexp)

(defstruct (buffer (:constructor %make-buffer))
  root selection)

(defvar *clipboard* nil)

(defun make-buffer (contents)
  (let* ((root      (list contents))
         (selection (make-selection)))
    (select selection root 0)
    (setf (first root) selection)
    (%make-buffer :root root
                  :selection selection)))

(defmacro with-buffer ((buffer rootvar selectionvar) &body body)
  `(with-accessors ((,rootvar buffer-root)
                    (,selectionvar buffer-selection))
       ,buffer
     ,@body))


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

(defmethod print-object ((selection selection) stream)
  (princ "【" stream)
  (prin1 (selection-sexp selection) stream)
  (princ "】" stream)
  selection)

(defun sedit-print (buffer)
  (pprint (first (buffer-root buffer)))
  (finish-output))


(defun unselected-sexp (list selection)
  "Return a copy of the lisp sexp with the selection removed."
  (cond
    ((consp list)
     (cons (unselected-sexp (car list) selection)
           (unselected-sexp (cdr list) selection)))
    ((eq list selection)
     (unselected-sexp (selection-sexp selection) selection))
    (t
     list)))

(defmacro reporting-errors (&body body)
  `(handler-case
       (progn ,@body)
     (simple-condition  (err) 
       (format *error-output* "~&~A:~%~?~&"
               (class-name (class-of err))
               (simple-condition-format-control   err)
               (simple-condition-format-arguments err))
       (finish-output *error-output*))
     (condition (err) 
       (format *error-output* "~&~A:~%~A~%"
               (class-name (class-of err))
               err)
       (finish-output *error-output*))))

(defun sedit-eval (buffer)
  (with-buffer (buffer root selection)
    (let ((sexp (selection-sexp selection)))
      (reporting-errors
        (format *query-io* "~& --> ~{~S~^ ;~%     ~}~%"
                (let ((*package*   *package*)
                      (*readtable* *readtable*))
                  (multiple-value-list (eval sexp)))))
      (finish-output *query-io*)
      (finish-output))))

(defun sedit-down (buffer)
  (with-buffer (buffer root selection)  
    (if (atom (selection-sexp selection))
        (progn (princ "Cannot enter an atom.") (terpri))
        (progn
          (unselect selection)
          (select selection (selection-sexp selection) 0)))))

(defun sedit-up (buffer)
  (with-buffer (buffer root selection)  
    (let ((gparent (sedit-find-object root (selection-parent-list selection))))
      (when gparent
        (unselect selection)
        (select selection gparent 0)))))

(defun sedit-forward (buffer)
  (with-buffer (buffer root selection)
    (let ((index (position selection (selection-parent-list selection))))
      (if (or (null index)
              (<= (length (selection-parent-list selection))  (1+ index)))
          (sedit-up buffer)
          (progn (unselect selection)
                 (select selection (selection-parent-list selection) (1+ index)))))))

(defun sedit-backward (buffer)
  (with-buffer (buffer root selection)
    (let ((index (position selection (selection-parent-list selection))))
      (if (or (null index) (<= index 0))
          (sedit-up buffer)
          (progn (unselect selection)
                 (select selection (selection-parent-list selection) (1- index)))))))

(defun sedit-cut (buffer)
  (with-buffer (buffer root selection)
    (setf *clipboard* (selection-sexp selection))
    (let ((gparent (sedit-find-object root (selection-parent-list selection))))
      (if (eq (car gparent) (selection-parent-list selection))
          (setf (car gparent) (delete selection (selection-parent-list selection)))
          (setf (cdr gparent) (delete selection (selection-parent-list selection))))
      (select selection gparent 0))))

(defun sedit-copy (buffer)
  (with-buffer (buffer root selection)
    (declare (ignore root))
    (setf *clipboard* (copy-tree (selection-sexp selection)))))

(defun sedit-paste (buffer)
  (with-buffer (buffer root selection)
    (declare (ignore root))
    (setf (selection-sexp selection) (copy-tree *clipboard*))))

(defun sedit-replace (buffer)
  (with-buffer (buffer root selection)
    (declare (ignore root))
    (princ "replacement sexp: " *query-io*)
    (finish-output *query-io*)
    (setf (selection-sexp selection) (read *query-io*))))

(defun insert (object list where reference)
  (ecase where
    ((:before)
     (cond
       ((null list) (error "Cannot insert in an empty list."))
       ((eq reference (car list))
        (setf (cdr list) (cons (car list) (cdr list))
              (car list) object))
       (t (insert object (cdr list) where reference))))
    ((:after)
     (cond
       ((null list) (error "Cannot insert in an empty list."))
       ((eq (car list) reference)
        (push object (cdr list)))
       (t (insert object (cdr list) where reference))))))

(defun sedit-insert (buffer)
  (with-buffer (buffer root selection)
    (princ "sexp to be inserted before: " *query-io*)
    (finish-output *query-io*)
    (let ((new-sexp (read *query-io*)))
      (cond
        ((eq selection (first root))
         ;; The whole expression is selected. To insert before it we
         ;; need to wrap it in a new list.
         (setf (car root) (list new-sexp selection)
               (selection-parent-list selection) (car root)))
        ((eq selection (first (selection-parent-list selection)))
         (let ((gparent (sedit-find-object root (selection-parent-list selection))))
           (setf (selection-parent-list selection)
                 (if (eq (car gparent) (selection-parent-list selection))
                     (setf (car gparent) (cons new-sexp (selection-parent-list selection)))
                     (setf (cdr gparent) (cons new-sexp (selection-parent-list selection)))))))
        (t
         (insert new-sexp (selection-parent-list selection) :before selection))))))

(defun sedit-add (buffer)
  (with-buffer (buffer root selection)
    (princ "sexp to be inserted after: " *query-io*)
    (finish-output *query-io*)
    (let ((new-sexp (read *query-io*)))
      (if (eq selection (first root))
          ;; The whole expression is selected. To insert after it we
          ;; need to wrap it in a new list.
          (setf (car root) (list selection new-sexp)
                (selection-parent-list selection) (car root))
          (insert new-sexp (selection-parent-list selection) :after selection)))))

(defun sedit-replace (buffer)
  (with-buffer (buffer root selection)
    (declare (ignore root))
    (princ "replacement sexp: " *query-io*)
    (finish-output *query-io*)
    (setf (selection-sexp selection) (read *query-io*))))


(defun sedit-save (buffer)
  (with-buffer (buffer root selection) 
    (princ "Save buffer to file: " *query-io*)
    (finish-output *query-io*)
    (let ((path (read-line *query-io*)))
      (with-open-file (out path :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede)
        (pprint (unselected-sexp (first root) selection) out)
        (terpri out)))))

(defun sedit-load (buffer)
  (with-buffer (buffer root selection) 
    (princ "Load file: " *query-io*)
    (finish-output *query-io*)
    (let* ((path (read-line *query-io*))
           (new (make-buffer (with-open-file (inp path :direction :input)
                               (read inp)))))
      (setf root (buffer-root new)
            selection (buffer-selection new)))))

(defun sedit-quit (buffer)
  (throw 'gazongue buffer))


;;; Quick and dirty command table and bindings:
;;; TODO: make it better.

(defparameter *command-map*
  '((q quit sedit-quit "return the modified sexp from sedit.")
    (d down sedit-down "enter inside the selected list.")
    (u up sedit-up "select the list containing the selection.")
    (f forward sedit-forward "select the sexp following the selection (or up).")
    (n next sedit-forward "select the sexp following the selection (or up).")
    (b backward sedit-backward "select the sexp preceding the selection (or up).")
    (p previous sedit-backward "select the sexp preceding the selection (or up).")
    (i insert sedit-insert "insert a new sexp before the selection.")
    (r replace sedit-replace "replace the selection with a new sexp.")
    (a add sedit-add "add a new sexp after the selection.")
    (x cut sedit-cut "cut the selection into a *clipboard*.")
    (c copy sedit-copy "copy the selection into a *clipboard*.")
    (y paste sedit-paste "paste the *clipboard* replacing the selection.")
    (e eval sedit-eval "evaluate the selection")
    (s save sedit-save "save the buffer to a file.")
    (l load sedit-load "load a file into the buffer.")
    (h help sedit-help "print this help.")))

(defvar *bindings* (make-hash-table))

(defun bind (command function)
  (setf (gethash command *bindings*) function))

(defun unbind (command)
  (remhash command *bindings*))

(defun binding (command)
  (gethash command *bindings*))

(defun add-command (short long function help)
  (setf *command-map* (append *command-map*
                              (list (list short long function help))))
  (bind short function)
  (bind long function))

(defun sedit-help (buffer)
  (declare (ignore buffer))
  (format t "~:{~A) ~10A ~*~A~%~}" *command-map*))

(defun initialize-bindings ()
  (loop :for (short long function) :in *command-map*
        :do (bind short function)
            (bind long  function)))


;;; The core:

(declaim (notinline sedit-core))
(defun sedit-core (buffer)
  (with-buffer (buffer root selection)
    (sedit-print buffer)
    (terpri *query-io*)
    (princ "> " *query-io*)
    (finish-output *query-io*)
    (let* ((command (let ((*package*
                            (load-time-value
                             (find-package
                              "COM.INFORMATIMAGO.SMALL-CL-PGMS.SEDIT"))))
                      (read *query-io*)))
           (function (binding command)))
      (if function
          (funcall function buffer)
          (format *query-io* "~%Please use one of these commands:~%~
                                    ~{~<~%~1,40:;~*~A (~2:*~A~*)~>~^, ~}.~2%"
                  *command-map*)))))

(defun sedit (&optional sexp)
  (terpri)
  (princ "Sexp Editor:")
  (terpri)
  (initialize-bindings)
  (let ((buffer (make-buffer sexp)))
    (unwind-protect
         (catch 'gazongue (loop (reporting-errors
                                  (sedit-core buffer))))
      (unselect (buffer-selection buffer)))
    (first (buffer-root buffer))))


;;;; THE END ;;;;
