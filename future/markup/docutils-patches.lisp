;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               docutils-patches.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Patches to docutils.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-13 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package :docutils)



;; (defmethod print-object((node field-name) stream)
;;   (format stream "~A " (as-text node))
;;   (print-unreadable-object (node stream :type t :identity t)
;;     (format stream "~S ~@[line ~D ~]"  (as-text node) (line node)))
;;   node)
;; 
;; (defmethod print-object((node field) stream)
;;   (print-unreadable-object (node stream :type t :identity t)
;;         (format stream "~S ~@[line ~D ~]"
;;                 (when (child node 0) (as-text (child node 0)))
;;                 (line node)))
;;   node)


(defparameter *print-object-indent-bar*  "|")
(defparameter *print-object-indent-stem* "+")
(setf *print-circle* nil
      *print-pretty* nil)

(defmethod print-object ((node text) stream)
  (format stream "~A-- ~A ~@[line ~D, ~] ~S~%"
          *print-object-indent-stem*
          (class-name (class-of node))
          (line node)
          (if (slot-boundp node 'text)
              (let* ((text    (slot-value node 'text))
                     (extract (subseq text 0 (min 20 (or (position #\newline text)
                                                         (length text))))))
                (if (< (length extract) (length text))
                    (concatenate 'string extract "…")
                    extract))
              "#<UNBOUND>"))
  node)

(defmethod print-object ((node element) stream)
  (setf *print-circle* nil)
  (format stream "~A--+-- ~A ~@[~S ~]~@[line ~D, ~]~:[~;~D child~:[~;ren~]~]~%"
          *print-object-indent-stem*
          (class-name (class-of node))
          (attribute node :name)
          (line node)
          (> (number-children node) 0)
          (number-children node)
          (< 1 (number-children node)))
  (format stream "~A  ~:[ ~;|~]   ~:[()~;~:*~S~]~%"
          *print-object-indent-bar*
          (slot-value node 'children)
          (slot-value node 'attributes))
  (when (slot-value node 'children)
    (let ((*print-object-indent-bar*  (concatenate 'string *print-object-indent-bar* "  |"))
          (*print-object-indent-stem* (concatenate 'string *print-object-indent-bar* "  +")))
      (format stream "~{~A~}" (butlast (slot-value node 'children))))
    (let ((*print-object-indent-bar*  (concatenate 'string *print-object-indent-bar* "   "))
          (*print-object-indent-stem* (concatenate 'string *print-object-indent-bar* "  +")))
      (format stream "~A" (car (last (slot-value node 'children)))))
    (format stream "~A~%" *print-object-indent-bar*))
  node)

(defmethod print-object ((node evaluateable) stream)
  (format stream "~A-- ~A ~@[~S ~]~@[line ~D, ~] ~S ~S ~S ~S~%"
          *print-object-indent-stem*
          (class-name (class-of node))
          (attribute node :name)
          (line node)
          (evaluation-language node)
          (output-format node)
          (expression node)
          (if (slot-boundp node 'result)
              (slot-value node 'result)
              '<not-computed-yet>))
  node)





;;;; THE END ;;;;
