;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cpp-macro.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the cpp macros.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-06-28 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LANGUAGES.CPP")


(defgeneric environment-macro-definedp (environment macro-name))
(defgeneric environment-macro-undefine (environment macro-name))
(defgeneric environment-macro-definition (environment macro-name))
(defgeneric (setf environment-macro-definition) (definition environment macro-name))


(defmethod environment-macro-definedp ((environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (nth-value 1 (gethash macro-name environment)))

(defmethod environment-macro-undefine ((environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (remhash macro-name environment))

(defmethod environment-macro-definition ((environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (gethash macro-name environment))

(defmethod (setf environment-macro-definition) (definition (environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (setf (gethash macro-name environment) definition))



(defclass macro-definition ()
  ((name :initarg :name :accessor macro-definition-name)))

(defgeneric expand-macro-definition (macro-definition &optional arguments))


(defclass macro-definition/object (macro-definition)
  ((expansion :initarg :expansion :accessor macro-definition-expansion)))

(defmethod expand-macro-definition ((macro-definition macro-definition/object) &optional (arguments '() argumentsp))
  (when argumentsp
    (error "~S cannot take arguments for object-like macro ~A" 'expand-macro-definition  (macro-definition-name macro-definition)))
  (macro-definition-expansion macro-definition))


(defclass macro-definition/function (macro-definition)
  ((parameters :initarg :parameters :initform '() :accessor macro-definition-parameters)
   (expansion :initarg :expansion :accessor macro-definition-expansion)))

(defmethod expand-macro-definition ((macro-definition macro-definition/function) &optional (arguments '() argumentsp))
  (unless argumentsp
    (error "~S needs arguments for function-like macro ~A()" 'expand-macro-definition (macro-definition-name macro-definition)))
  (expand-macro-call macro-definition arguments))


(defclass macro-definition/computed-mixin ()
  ((compute-expansion-function :initarg :compute-expansion-function
                               :accessor macro-definition-compute-expansion-function)))

(defclass macro-definition/object/computed (macro-definition/object macro-definition/computed-mixin)
  ())

(defmethod expand-macro-definition ((macro-definition macro-definition/object/computed) &optional (arguments '() argumentsp))
  (when argumentsp
    (error "~S cannot take arguments for object-like macro ~A" 'expand-macro-definition (macro-definition-name macro-definition)))
  (funcall (macro-definition-compute-expansion-function macro-definition) macro-definition))


(defclass macro-definition/function/computed (macro-definition/function macro-definition/computed-mixin)
  ())

(defmethod expand-macro-definition ((macro-definition macro-definition/object/computed) &optional (arguments '() argumentsp))
  (unless argumentsp
    (error "~S needs arguments for function-like macro ~A()" 'expand-macro-definition (macro-definition-name macro-definition)))
  (funcall (macro-definition-compute-expansion-function macro-definition) macro-definition arguments))




(deftype option-key ()
  `(member :warn-date-time
           :directives-only
           :substitute-trigraphs
           :warn-on-trigraph 
           :warn-spaces-in-continued-lines 
           :single-line-comments
           :accept-unicode-escapes 
           :dollar-is-punctuation
           
           :include-disable-current-directory
           ;; When true, files are not searched in the current directory.
           ;; NOTE: current directory is defined as:
           ;;        (or *load-truename* *compile-file-truename*
           ;;            *default-pathname-defaults*)

           :include-quote-directories
           ;; Directories where #include \"\" files are searched.

           :include-bracket-directories
           ;; Directories where #include <> files are searched.
           ;; May contain keywords indexing search functions in the following a-list:

           :include-search-functions
           ;; An a-list mapping keywords to search functions (lambda (path kind directive) …)
           ;; kind (member :quote :bracket), directive (member :include :import)
           ;; RETURN: NIL if include-file is not found,
           ;;         T   if include-file is already included, or
           ;;         a pathname to the include-file to be loaded.

           ))

(defparameter *default-options*
  '((:warn-date-time . t)
    (:directives-only . nil)
    (:substitute-trigraphs . t)
    (:warn-on-trigraph . t) 
    (:warn-spaces-in-continued-lines . t) 
    (:single-line-comments . t)
    (:accept-unicode-escapes . t) 
    (:dollar-is-punctuation . nil)
    (:include-disable-current-directory . nil)
    (:include-quote-directories . ())
    (:include-bracket-directories . ())
    (:include-search-functions . ())
    (:external-format . :default)))

(defvar *default-environment* (make-hash-table :test 'equal))

(defclass context ()
  ((base-file      :initarg :base-file     :initform "-"                                     :accessor context-base-file)
   (file           :initarg :file          :initform "-"                                     :accessor context-file)
   (line           :initarg :line          :initform 1                                       :accessor context-line)
   (column         :initarg :column        :initform 1                                       :accessor context-column)
   (token          :initarg :token         :initform nil                                     :accessor context-token)
   (counter        :initarg :counter       :initform 0                                       :accessor context-counter)
   (include-level  :initarg :include-level :initform 0                                       :accessor context-include-level)
   (if-level       :initarg :if-level      :initform 0                                       :accessor context-if-level)
   (options        :initarg :options       :initform (copy-tree *default-options*)           :accessor context-options)
   (environment    :initarg :environment   :initform (copy-hash-table *default-environment*) :accessor context-environment)))
   

(defvar *context* nil)

(defun updated-context (&key
                          (token         nil tokenp)
                          (line          nil linep)
                          (column        nil columnp)
                          (file          nil filep)
                          (include-level nil include-level-p))
  (when tokenp          (setf (context-token         *context*) token))
  (when linep           (setf (context-line          *context*) line))
  (when columnp         (setf (context-column        *context*) column))
  (when filep           (setf (context-file          *context*) file))
  (when include-level-p (setf (context-include-level *context*) include-level))
  *context*)

(defun option (context option)
  (cdr (assoc option (context-options context))))

;;;; THE END ;;;;
