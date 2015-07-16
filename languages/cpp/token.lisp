;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               token.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines cpp tokens.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-06-28 <PJB> 
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.CPP")

;;; ------------------------------------------------------------

(defstruct (numbered-line
            (:type list)
            (:conc-name line-))
  (text "")
  (lino 1)
  (file "-"))

(defun number-lines (lines file-name &key (start 1))
  (loop
    :for lino :from start
    :for line :in lines
    :collect (make-numbered-line :text line :lino lino :file file-name)))

(defmethod token-file ((numbered-line cons))
  (line-file numbered-line))
(defmethod token-line ((numbered-line cons))
  (line-lino numbered-line))
(defmethod token-column ((numbered-line cons))
  1)
(defmethod token-text ((numbered-line cons))
  (line-text numbered-line))

;;; ------------------------------------------------------------

(defclass cpp-token (token)
  ((file   :initform "-" :initarg :file   :accessor token-file)))

(defmacro define-token-class (name &optional slots)
  (let ((class-name (intern (concatenate 'string (string name) (string '-token)))))
    `(progn
       (defclass ,class-name (cpp-token) ,slots)
       (defun ,(intern (concatenate 'string (string name) (string '-p))) (object)
         (typep object ',class-name))
       (defmethod print-object ((self ,class-name) stream)
         (print-unreadable-object (self stream :identity nil :type t)
           (let ((*print-circle* nil))
             (format stream "~A:~A:~A: ~A ~S"
                     (token-file self) (token-line self) (token-column self)
                     (token-kind self) (token-text self))))
         self)
       (defun ,(intern (concatenate 'string (string 'make-) (string name))) (text &optional (column 0) (line 0) (file "-"))
         (make-instance ',class-name :text text :column column :line line :file file)))))

(defgeneric token-symbol (token)
  (:method ((token token))
    (token-kind token)))

(define-token-class identifier
    ((symbol :initarg :symbol
             :initform nil
             :accessor token-symbol)))
(define-token-class number)
(define-token-class string-literal)
(define-token-class character-literal)
(define-token-class punctuation)
(define-token-class other)

(defvar *identifier-package* (load-time-value (find-package #.(package-name *package*)))
  "Package where to intern token-symbol of identifiers.")

(defmethod initialize-instance :after ((token identifier-token) &key &allow-other-keys)
  (setf (slot-value token 'kind)   'identifier
        (slot-value token 'symbol) (intern (slot-value token 'text) *identifier-package*)
        (slot-value token 'text) nil))
(defmethod token-text ((token identifier-token))
  (symbol-name (token-symbol token)))

(defmethod initialize-instance :after ((token punctuation-token) &key &allow-other-keys)
  (setf (slot-value token 'kind) 'punctuation))
(defmethod initialize-instance :after ((token number-token) &key &allow-other-keys)
  (setf (slot-value token 'kind) 'number))
(defmethod initialize-instance :after ((token string-literal-token) &key &allow-other-keys)
  (setf (slot-value token 'kind) 'string-literal))
(defmethod initialize-instance :after ((token character-literal-token) &key &allow-other-keys)
  (setf (slot-value token 'kind) 'character-literal))

(defun identifierp (token)
  (typep token 'identifier-token))

(defun number-token-p (token)
  (typep token 'number-token))


(defun pseudo-token (file lino)
  (make-other "" 0 lino file))

(defun token-predicate-label (predicate-name)
  (case predicate-name
    (sharpp          "« # »")
    (sharpsharpp     "« ## »")
    (spacep          "a space")
    (openp           "« ( »") 
    (closep          "« ) »")
    (open-bracket-p  "« < »")
    (close-bracket-p "« > »")
    (commap          "« , »")
    (ellipsisp       "« ... »")
    (identifierp     "an identifier")
    (number-token-p  "a number")
    (otherwise (format nil "a ~(~A~)"  predicate-name))))

(defmacro define-punctuation-predicate (name value)
  `(defun ,name (token)
     (and (typep token 'punctuation-token)
          (or (string= ,value  (token-text token))))))

(defparameter *whitespaces* #(#\space #\tab #\vt #\page #\nul #\newline #\return #\linefeed))
(defun  whitespacep (character)
  (find character *whitespaces*))
(defun spacep (token)
  (and (typep token 'punctuation-token)
       (= 1 (length (token-text token)))
       (whitespacep (aref (token-text token) 0))))

(define-punctuation-predicate sharpp           "#")
(define-punctuation-predicate sharpsharpp      "##")
(define-punctuation-predicate openp            "(")
(define-punctuation-predicate closep           ")") 
(define-punctuation-predicate open-bracket-p   "<")
(define-punctuation-predicate close-bracket-p  ">")
(define-punctuation-predicate commap           ",")
(define-punctuation-predicate ellipsisp        "...")

(define-punctuation-predicate op-plus-p        "+")
(define-punctuation-predicate op-minus-p       "-")
(define-punctuation-predicate op-lognot-p      "!")
(define-punctuation-predicate op-bitnot-p      "~")
(define-punctuation-predicate op-times-p       "*")
(define-punctuation-predicate op-divides-p     "/")
(define-punctuation-predicate op-remainder-p   "%")
(define-punctuation-predicate op-left-shift-p  "<<")
(define-punctuation-predicate op-right-shift-p ">>")
(define-punctuation-predicate op-lt-p          "<")
(define-punctuation-predicate op-le-p          "<=")
(define-punctuation-predicate op-gt-p          ">")
(define-punctuation-predicate op-ge-p          ">=")
(define-punctuation-predicate op-eq-p          "==")
(define-punctuation-predicate op-ne-p          "!=")
(define-punctuation-predicate op-bitand-p      "&")
(define-punctuation-predicate op-bitior-p      "|")
(define-punctuation-predicate op-bitxor-p      "^")
(define-punctuation-predicate op-logand-p      "&&")
(define-punctuation-predicate op-logior-p      "||")
(define-punctuation-predicate op-question-p    "?")
(define-punctuation-predicate op-colon-p       ":")



(define-condition cpp-error (simple-error)
  ())

(define-condition cpp-warning (simple-warning)
  ())

(defgeneric cpp-error (token format-control &rest format-arguments)
  (:method ((context context) format-control &rest format-arguments)
    (cerror "Continue" 'cpp-error
            :format-control "~A:~A: error: ~?"
            :format-arguments (list (context-file *context*)
                                    (context-line *context*)
                                    format-control format-arguments)))
  (:method ((token token) format-control &rest format-arguments)
    (apply (function cpp-error)
           (update-context *context* :token token
                                     :line (token-line token)
                                     :column (token-column token)
                                     :file (token-file token))
           format-control format-arguments))
  (:method ((line cons) format-control &rest format-arguments)
    (apply (function cpp-error) (first line) format-control format-arguments))
  (:method ((line null) format-control &rest format-arguments)
    (error 'cpp-error
           :format-control format-control
           :format-arguments  format-arguments)))


(defgeneric cpp-warning (token format-control &rest format-arguments)
  (:method ((context context) format-control &rest format-arguments)
    (warn 'cpp-warning
          :format-control "~A:~A: warning: ~?"
          :format-arguments (list (context-file *context*)
                                  (context-line *context*)
                                  format-control format-arguments)))
  (:method ((token token) format-control &rest format-arguments)
    (apply (function cpp-warning)
           (update-context *context* :token token
                                     :line (token-line token)
                                     :column (token-column token)
                                     :file (token-file token))
           format-control format-arguments))
  (:method ((line cons) format-control &rest format-arguments)
    (apply (function cpp-warning) (first line) format-control format-arguments))
  (:method ((line null) format-control &rest format-arguments)
    (warn 'cpp-warning
          :format-control format-control
          :format-arguments  format-arguments)))

;;;; THE END ;;;;
