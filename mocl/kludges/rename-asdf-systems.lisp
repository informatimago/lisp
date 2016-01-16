;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rename-asdf-systems.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Kludge for MoCL: rename asdf systems substituting exclaims for dots.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-11-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2016
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

(defpackage "COM.INFORMATIMAGO.MOCL.KLUDGES.RENAME-ASDF-SYSTEMS"
  (:use "COMMON-LISP")
  (:export "GENERATE-RENAMED-SYSTEMS-FROM-DIRECTORY"))
(in-package "COM.INFORMATIMAGO.MOCL.KLUDGES.RENAME-ASDF-SYSTEMS")

(defconstant +replacement+ #\!)

(defun rename-system-name (name)
  (etypecase name
    (string (substitute +replacement+ #\. name))
    (symbol (intern (substitute +replacement+ #\. (string name))
                    (load-time-value (find-package "KEYWORD"))))))

(defun process-component (component)
  (assert (listp component))
  (destructuring-bind (type name &rest options) component
    (case type
      ((:module)
       (list* type (rename-system-name name)
              (process-options options)))
      (otherwise
       component))))

(defun process-options (options)
  (loop
    :for (key value) :on options :by (function cddr)
    :collect key
    :collect (case key 
               ((:defsystem-depends-on)
                (mapcar (function rename-system-name) value))
               ((:weakly-depends-on)
                (mapcar (function rename-system-name) value))
               ((:depends-on)
                (mapcar (lambda (component)
                          (cond
                            ((atom component)
                             (rename-system-name component))
                            ((eq :version (first component))
                             (list (first component)
                                   (rename-system-name (second component))
                                   (third component)))
                            (t
                             component)))
                        value))
               ((:in-order-to)
                (mapcar (lambda (dependency)
                          (assert (listp dependency))
                          (cons (first dependency)
                                (mapcar (lambda (requirement)
                                          (assert (listp requirement))
                                          (cond
                                            ((eq :feature (first requirement))
                                             requirement)
                                            (t
                                             (cons (first requirement)
                                                   (mapcar (function process-component)
                                                           (rest requirement))))))
                                        (rest dependency))))
                        value))
               ((:components)
                (mapcar (function process-component) value))
               (otherwise value))))

(defun rename-systems (defsystem-form)
  (assert (and (listp defsystem-form)
               (eq 'asdf:defsystem (first defsystem-form))))
  (destructuring-bind (defsystem system &rest options) defsystem-form
    `(,defsystem ,(rename-system-name system)
         ,@(process-options options))))


(defun replace-asdf/defsystem (sexp-text)
  (let* ((target  "asdf/defsystem")
         (pos (search target sexp-text)))
    (concatenate 'string (subseq sexp-text 0 pos)
                 "asdf"
                 (subseq sexp-text (+ pos (length target))))))

(defun process-asd-file (path)
  (let ((name (pathname-name path)))
    (when (position #\. name)
      (with-standard-io-syntax
        (let* ((new-name (substitute +replacement+ #\. name))
               (new-path (make-pathname :name new-name :defaults path))
               (*package* (find-package "COMMON-LISP-USER"))
               (*features* (remove :asdf-unicode *features*))
               (*print-case* :downcase))
          (with-open-file (out new-path
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede
                               :external-format :iso-8859-1)
            (with-open-file (inp path
                                 :direction :input
                                 :if-does-not-exist :error
                                 :external-format :iso-8859-1)
              (loop
                :for sexp = (read inp nil inp)
                :until (eq sexp inp)
                :do (cond
                      ((and (listp sexp)
                            (eql 'cl:in-package (first sexp)))
                       (setf *package* (find-package (second sexp)))
                       (print sexp out))
                      ((and (listp sexp)
                            (eql 'asdf:defsystem (first sexp)))
                       (terpri out)
                       (princ (replace-asdf/defsystem (prin1-to-string (rename-systems sexp))) out))
                      (t
                       (print sexp out)))))))))))

(defun generate-renamed-systems-from-directory (directory)
  (map nil (function process-asd-file)
       (directory (merge-pathnames "**/*.asd" directory nil))))


;; (com.informatimago.mocl.kludges.rename-asdf-systems:generate-renamed-systems-from-directory #P"~/src/public/lisp/")
;;;; THE END ;;;;
