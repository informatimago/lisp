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


(defun make-environment ()
   (make-hash-table :test 'equal))

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


;; Only one level of embedded lexical scope, for function-like macro parameters is possible.

(defstruct lexical-environment
  global
  bindings)

(defmethod environment-macro-definedp ((environment lexical-environment) (macro-name string))
  (or (assoc macro-name (lexical-environment-bindings environment)
             :test (function string=)
             :key (function token-text))
      (environment-macro-definedp (lexical-environment-global environment) macro-name)))

(defmethod environment-macro-definition ((environment lexical-environment) (macro-name string))
  (let ((entry (assoc macro-name (lexical-environment-bindings environment)
                      :test (function string=)
                      :key (function token-text))))
    (if entry
        (cdr entry)
        (environment-macro-definition (lexical-environment-global environment) macro-name))))



(defclass macro-definition ()
  ((name :initarg :name :accessor macro-definition-name)))

(defgeneric expand-macro-definition (macro-definition &optional arguments))


(defclass macro-definition/object (macro-definition)
  ((expansion :initarg :expansion :accessor macro-definition-expansion)))

(defmethod print-object ((macro macro-definition/object) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (macro stream :type t :identity t)
      (format stream ":name ~S :expansion ~S"
              (macro-definition-name macro)
              (macro-definition-expansion macro)))
    macro))

(defmethod expand-macro-definition ((macro-definition macro-definition/object) &optional (arguments '() argumentsp))
  (declare (ignore arguments))
  (when argumentsp
    (error "~S cannot take arguments for object-like macro ~A" 'expand-macro-definition  (macro-definition-name macro-definition)))
  (substitute-concatenates (macro-definition-expansion macro-definition)))

(defclass macro-definition/function (macro-definition)
  ((parameters :initarg :parameters :initform '() :accessor macro-definition-parameters)
   (expansion :initarg :expansion :accessor macro-definition-expansion)))

(defmethod print-object ((macro macro-definition/function) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (macro stream :type t :identity t)
      (format stream ":name ~S :parameters ~S :expansion ~S"
              (macro-definition-name macro)
              (macro-definition-parameters macro)
              (macro-definition-expansion macro)))
    macro))

(defun ellipsis-parameter-p (parameter)
  (and (consp parameter)
       (eq :ellipsis (first parameter))))



(defstruct argument
  tokens
  expanded
  %stringified)

(defun stringify (tokens)
  (let ((first-token (find-if-not (function null) tokens)))
    (apply (function make-string-literal) (format nil "~S" (mapconcat (function token-text) tokens ""))
           (if first-token
               (list (token-column first-token)
                     (token-line first-token)
                     (token-file first-token))
               (list (context-column *context*)
                     (context-line *context*)
                     (context-file *context*))))))

(defmethod argument-stringified ((argument argument))
  (or (argument-%stringified argument)
      (setf (argument-%stringified argument) (stringify (argument-tokens argument)))))

(defmethod argument-stringified ((argument list))
  (let ((strings     (mapcar (lambda (arg)
                               (mapconcat (function token-text) (argument-tokens arg) ""))
                             argument))
        (first-token (loop
                       :with args = argument
                       :while (and args (null (argument-tokens (first args))))
                       :do (pop args)
                       :finally (return (when args
                                          (find-if-not (function null) (argument-tokens (first args))))))))
    (apply (function make-string-literal)
           (format nil "~S" (mapconcat (function identity) strings ","))
           (if first-token
               (list (token-column first-token)
                     (token-line first-token)
                     (token-file first-token))
               (list (context-column *context*)
                     (context-line *context*)
                     (context-file *context*))))))

(defun concatenate-tokens (tokens)
  (let* ((first-token (find-if-not (function null) tokens))
         (concatenated
           (tokenize-line (cons (mapconcat (function token-text) (remove nil tokens) "")
                                (if first-token
                                    (list (token-line first-token)
                                          (token-file first-token))
                                    (list (context-line *context*)
                                          (context-file *context*))))
                         :accept-unicode-escapes (option *context* :accept-unicode-escapes)
                         :dollar-is-punctuation  (option *context* :dollar-is-punctuation))))
    
    (when (rest concatenated)
      (cpp-error (first tokens)
                 "Pasting ~{~A~^ ~} does not give a valid preprocessing token" (mapcar (function token-text) tokens)))
    concatenated))

(defun substitute-concatenates (line)
  (when line
   (if (sharpsharpp (first line))
       (progn
         (cpp-error (first line) "'##' cannot appear at either end of a macro expansion")
         (substitute-concatenates (rest line)))
       (loop
         :with result = ()
         :while line
         :do (let ((curr (pop line)))
               (if (sharpsharpp (first line))
                   (let ((file (token-file (first line)))
                         (lino (token-line (first line))))
                     (loop
                       :with concat = (list curr)
                       :while (sharpsharpp (first line))
                       :do (pop line)
                           (unless line
                             (cpp-error (pseudo-token file lino) "'##' cannot appear at either end of a macro expansion"))
                           (unless (sharpsharpp (first line)) 
                             (push (pop line) concat))
                       :finally (setf result (nreconc (concatenate-tokens (nreverse concat)) result))))
                   (push curr result)))
         :finally (return (nreverse result))))))

(defun macro-bind (name parameters arguments)
  (loop
    :with bindings := '()
    :with no-parameters = (null parameters)
    :while parameters
    :do (let ((par (pop parameters)))
          (cond
            ((ellipsis-parameter-p par)
             (push (list* (second par) :ellipsis arguments) bindings)
             (setf arguments nil))
            ((null arguments)
             (cpp-error *context* "Missing argument to function-like macro call ~S" (token-text name))
             (return :error))
            (t
             (let ((arg (pop arguments)))
               (push (cons par arg) bindings)))))
    :finally (when (and arguments
                        (not (and no-parameters
                                  (null (cdr arguments))
                                  (null (car arguments)))))
               (cpp-error *context* "Too many arguments for function-like macro call ~S" (token-text name))
               (return :error))
             (return bindings)))

(defun macro-bindings-expand-arguments (bindings)
  (flet ((marg (tokens)
           (make-argument
            :tokens tokens
            :expanded (reduce (function nconc) (macro-expand-macros tokens '() '()
                                                                    (context-macros-being-expanded *context*)
                                                                    (context-environment *context*))))))
    (dolist (binding bindings bindings)
      (if (and (listp (cdr binding))
               (eq :ellipsis (second binding)))
          (setf (cddr binding) (mapcar (function marg) (cddr binding)))
          (setf (cdr  binding) (marg (cdr  binding)))))))

(defun substitute-parameters (definition bindings) 
  (flet ((get-entry (ident)
           (assoc (token-text ident) bindings
                  :test (function string=)
                  :key (function token-text))))
    (loop :with result := '()
          :while definition
          :do (let ((item (pop definition)))
                (if (atom item)
                    (cond
                      ((and (commap item) ;; , ## __VA_ARGS__
                            (cdr definition)
                            (sharpsharpp (first definition))
                            (identifierp (second definition))
                            (listp (cdr (get-entry (second definition))))
                            (eq :ellipsis (cadr (get-entry (second definition)))))
                       ;;DEBUG;; (print (list ", ##" item (first definition) (second definition) '/ (get-entry (second definition))))
                       (if (cddr (get-entry (second definition)))
                           (progn (push item result)
                                  (pop definition)) ; pop ##
                           (setf definition (cddr definition))))
                      ((identifierp item)
                       (let ((entry (get-entry item)))
                         (if entry
                             (if (and (listp (cdr entry))
                                      (eq :ellipsis (cadr entry)))
                                 (setf result (revappend (rest (loop
                                                                 :with comma = (make-punctuation "," 0 0 "-")
                                                                 :for item :in (cddr entry)
                                                                 :collect comma
                                                                 :append (argument-expanded item)))
                                                         result))
                                 (setf result (revappend (or (argument-expanded (cdr entry)) '(())) result)))
                             (push item result))))
                      (t
                       (push item result)))
                    (ecase (first item)
                      (:stringify (let ((entry (get-entry (second item))))
                                    (if entry
                                        (push (argument-stringified (if (and (listp (cdr entry))
                                                                             (eq :ellipsis (cadr entry)))
                                                                        (cddr entry)
                                                                        (cdr entry))) result)
                                        (cpp-error (second item) "'#' is not followed by a macro parameter")))))))
          :finally (return (nreverse result)))))

(defmethod expand-macro-definition ((macro-definition macro-definition/function) &optional (arguments '() argumentsp))
  (unless argumentsp
    (error "~S needs arguments for function-like macro ~A()" 'expand-macro-definition (macro-definition-name macro-definition)))
  (let* ((name          (macro-definition-name macro-definition))
         (parameters    (macro-definition-parameters macro-definition))
         (definition    (macro-definition-expansion macro-definition))
         ;; bind arguments to parameters, checking variadic parameters:
         (bindings      (macro-bind name parameters arguments)))
    (when (eq :error bindings)
      (return-from expand-macro-definition '()))
    ;; macroexpand arguments
    ;; we do that after the bindings loop so we don't macro-expand-macros if there's an argcount error.
    ;;DEBUG;; (print (list :before :bindings bindings))
    (setf bindings (macro-bindings-expand-arguments bindings))
    ;; stringify arguments will be done lazily by argument-stringified.
    ;;DEBUG;; (print (list :name name :bindings bindings))
    ;; substitute parameters in definition.
    (remove nil (substitute-concatenates (substitute-parameters definition bindings)))))


(defclass macro-definition/computed-mixin ()
  ((compute-expansion-function :initarg :compute-expansion-function
                               :accessor macro-definition-compute-expansion-function)))

(defclass macro-definition/object/computed (macro-definition/object macro-definition/computed-mixin)
  ())

(defmethod print-object ((macro macro-definition/object/computed) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (macro stream :type t :identity t)
      (format stream ":name ~S"
              (macro-definition-name macro)))
    macro))

(defmethod expand-macro-definition ((macro-definition macro-definition/object/computed) &optional (arguments '() argumentsp))
  (declare (ignore arguments))
  (when argumentsp
    (error "~S cannot take arguments for object-like macro ~A" 'expand-macro-definition (macro-definition-name macro-definition)))
  (funcall (macro-definition-compute-expansion-function macro-definition) macro-definition))


(defclass macro-definition/function/computed (macro-definition/function macro-definition/computed-mixin)
  ())

(defmethod print-object ((macro macro-definition/function/computed) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (macro stream :type t :identity t)
      (format stream ":name ~S :parameters ~S"
              (macro-definition-name macro)
              (macro-definition-parameters macro)))
    macro))

(defmethod expand-macro-definition ((macro-definition macro-definition/function/computed) &optional (arguments '() argumentsp))
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

(defvar *default-environment* (make-environment))

(defclass context ()
  ((base-file             :initarg :base-file             :initform "-"                                     :accessor context-base-file)
   (file                  :initarg :file                  :initform "-"                                     :accessor context-file)
   (line                  :initarg :line                  :initform 1                                       :accessor context-line)
   (column                :initarg :column                :initform 1                                       :accessor context-column)
   (token                 :initarg :token                 :initform nil                                     :accessor context-token)
   (counter               :initarg :counter               :initform 0                                       :accessor context-counter)
   (include-level         :initarg :include-level         :initform 0                                       :accessor context-include-level)
   (if-level              :initarg :if-level              :initform 0                                       :accessor context-if-level)
   (macros-being-expanded :initarg :macros-being-expanded :initform '()                                     :accessor context-macros-being-expanded)
   (options               :initarg :options               :initform (copy-tree       *default-options*)     :accessor context-options)
   (environment           :initarg :environment           :initform (copy-hash-table *default-environment*) :accessor context-environment)))
   

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
