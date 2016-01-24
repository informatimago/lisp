;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               gentext.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Generates a single-file plain text documentation.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-04 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
;;;;    
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;    
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;    
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;    
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(in-package "COM.INFORMATIMAGO.LISPDOC.GENERATE.TEXT")


(defclass text-documentation (documentation-generator)
  ((file :initform nil :accessor documentation-file)))


(defmethod generate-begin ((target text-documentation))
  (setf (documentation-file target)
        (open (format nil "~(~A~).txt" (substitute #\- #\space (documentation-title target)))
              :direction :output
              :if-does-not-exist :create
              :if-exists :supersede)))


(defmethod generate-end ((target text-documentation))
  (when (documentation-file target)
    (close (documentation-file target))
    (setf (documentation-file target) nil)))

(defvar *line-width* 80)

(defmacro with-doc-output (target &body body)
  `(let ((*standard-output* (or (documentation-file ,target)
                                *standard-output*))
         ;; TODO: add a line-width slot to text-documentation.
         (*line-width*      *print-right-margin*))
     ,@body))

(defun line (ch)
  (format t "~V,,,V<~>~%" *line-width* ch))

(defun title (ch title)
  (terpri)
  (line ch)
  (write-line title)
  (line ch)
  (terpri))

(defvar *h1* #\#)
(defvar *h2* #\+)
(defvar *h3* #\=)
(defvar *h4* #\-)

(defmethod generate-introduction ((target text-documentation))
  (with-doc-output target
    (title *h1* (documentation-title target))
    (when (author    target) (format t "Author: ~A~%" (author target)))
    (when (email     target) (format t "Email: ~A~%" (email target)))
    (when (copyright target) (format t "~A~%" (copyright target)))
    (when (keywords  target) (format t "Key words: ~A~%" (keywords  target)))
    (format t "~2%")))


(defmethod generate-navigation-menu ((target text-documentation) &optional (entries nil entriesp))
  (declare (ignore entries entriesp))
  target)


;;;---------------------------------------------------------------------
;;;
;;; HTML rendering
;;;

(defmethod generate-package-documentation-pages ((target text-documentation))
  (with-doc-output target
    (title *h2* "Packages")
    (call-next-method)))

(defun doc-title (name arglist kind)
  (let* ((kwidth (length "generic-function"))
         (swidth (- *line-width* kwidth 1))
         (skind  (princ-to-string kind)))
    (line *h4*)
    (format t "~VA~V<~>~A~%"
            swidth (with-output-to-string (*standard-output*)
                     (cond
                       ((not (member kind '(:function :generic-function :macro)))
                        (format t "~A" (right-case name)))
                       ((and (consp name) (eq (car name) 'setf))
                        (format t "(setf (")
                        (format t "~A" (right-case (second name)))
                        (format t "~{ ~A~}) ~A)" (right-case (rest arglist))
                                (right-case (first arglist))))
                       (t
                        (format t "(")
                        (format t "~A"  (right-case name))
                        (format t "~{ ~A~}" (right-case arglist))
                        (format t ")"))))
            (1+ (max 0 (- kwidth (length skind))))
            skind)
    (line *h4*)))


(defun pjb-docstring (docstring)
  (format t "~%~A~%" docstring))


(defmethod render ((doc doc) (target text-documentation))
  (ecase (doc-kind doc)
    (:type
     (doc-title (doc-symbol doc) nil (doc-kind doc))
     (pjb-docstring (doc-string doc)))
    (:skip
     (format *trace-output* "~&;; warning: lispdoc skipping ~s~%" (doc-symbol doc)))
    (:undocumented
     (doc-title (doc-symbol doc) nil "undocumented"))))


(defmethod render ((doc packdoc) (target text-documentation))
  (let ((title (format nil "Package ~A" (doc-name doc))))
    (title *h3* title)
    (when (packdoc-nicknames doc)
      (format t "Nicknames: ~{ ~A~^           ~%~}~%"  (packdoc-nicknames doc)))
    (terpri)
    (pjb-docstring (doc-string doc))
    (terpri)
    (mapc (lambda (doc)
            (render doc target)
            (terpri))
          (packdoc-external-symbol-docs doc))))


(defmethod render ((doc vardoc) (target text-documentation))
  (doc-title (doc-symbol doc) nil (doc-kind doc))
  (pjb-docstring (doc-string doc))
  (if (eq (vardoc-initial-value doc) :unbound)
      (format t "    Initially unbound.~%")
      (format t "    Initial value: ~A~%" (vardoc-initial-value doc))))


(defmethod render ((doc fundoc) (target text-documentation))
  (doc-title (doc-symbol doc) (fundoc-lambda-list doc) (doc-kind doc))
  (pjb-docstring (doc-string doc)))


(defmethod render ((doc classdoc) (target text-documentation))
  (doc-title (doc-symbol doc) nil (doc-kind doc))
  (pjb-docstring (doc-string doc))
  (when (classdoc-precedence-list doc)
    (format t "Class precedence list: ~{~A~^, ~}~%" (classdoc-precedence-list doc)))
  (when (classdoc-initargs doc)
    (format t "Class init args: ~{ ~A~}~%" (classdoc-initargs doc))))


;;;---------------------------------------------------------------------


(defmethod generate-hierarchical-package-index ((target text-documentation) tree &optional (filename "hierindex"))
  (declare (ignore tree filename))
  target)


(defmethod generate-flat-package-index ((target text-documentation) pages &optional (filename "flatindex"))
  (declare (ignore filename))
  (with-doc-output target
    (title *h2* "Flat Package List")
    (format t "~{    ~A~%~}~%" pages)))


(defmethod generate-flat-symbol-index ((target text-documentation) syms &optional (filename "flatsymindex"))
  "
RETURN: NIL
"
  (declare (ignore filename))
  (with-doc-output target
    (title *h2* "Alphabetical Symbol Index")
    (let ((groups (build-flat-symbol-index-groups syms)))
      ;; Generate each group index:
      (dolist (group groups)
        (let* ((group   (sort group
                              (function string-lessp)
                              :key (lambda (x) (symbol-name (doc-name x)))))
               (width    (reduce (function max) group
                                 :key (lambda (x) (length (princ-to-string (doc-symbol x)))))))
          (terpri)
          (dolist (sym group)
            (let ((packname (package-name
                             (symbol-package (doc-name sym)))))
              (format t "~A~V<~>~A~%"
                      (doc-symbol sym)
                      (- width -4 (length (princ-to-string (doc-symbol sym))))
                      packname))))))))


(defmethod generate-permuted-symbol-index ((target text-documentation) syms &optional (filename "permsymindex"))
  "
RETURN: NIL
"
  (declare (ignore filename))
  (with-doc-output target
    (title *h2* "Permuted Symbol Index")
    (let ((groups  (build-permuted-symbol-index-groups syms)))
      ;; Generate each group index:
      (dolist (group groups)
        (let ((first-letter (pop group)))
          (labels ((compute-offset (name index)
                     (if (equalp first-letter (aref name index))
                         index
                         (loop
                           :for previous :from index
                           :for i :from (1+ index) :below (length name)
                           :while (not (and (not (alpha-char-p (aref name previous)))
                                            (equalp first-letter (aref name i))))
                           :finally (return (if (< i (length name)) i nil)))))
                   (offset (doc)
                     (if (consp (doc-symbol doc))
                         (compute-offset (princ-to-string (doc-symbol doc)) (length "(setf "))
                         (compute-offset (symbol-name (doc-name doc)) 0))))
            (let* ((group    (sort  group
                                    (function string-lessp)
                                    :key (lambda (doc)
                                           (let ((name   (princ-to-string (doc-symbol doc)))
                                                 (offset (offset doc)))
                                             (concatenate 'string
                                                          (subseq name offset) (subseq name 0 offset))))))
                   (indent   (reduce (lambda (a b)
                                       (cond
                                         ((null a) b)
                                         ((null b) a)
                                         (t (max a b))))
                                     group :key (function offset)))
                   (width    (reduce (function max) group
                                     :key (lambda (x) (length (princ-to-string (doc-symbol x)))))))
              (terpri)
              (dolist (sym group)
                (let ((packname (package-name (symbol-package (doc-name sym))))
                      (offset (offset sym)))
                  (when offset
                    (format t "~V<~>~A~V<~>~A~%"
                            (- indent (offset sym))
                            (doc-symbol sym)
                            (- (+ width 4)
                               (- (length (princ-to-string (doc-symbol sym)))
                                  offset))
                            packname)))))))))))


(defmethod generate-symbol-index ((target text-documentation) flat-indices permuted-indices symbol-count &optional (filename "symindex"))
  (declare (ignore flat-indices permuted-indices symbol-count filename))
  target)


;;;; THE END ;;;;
