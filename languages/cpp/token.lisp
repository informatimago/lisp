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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LANGUAGES.CPP")


(defclass token ()
  ((line   :initform 0   :initarg :line   :accessor token-line)
   (column :initform 0   :initarg :column :accessor token-column)
   (file   :initform "-" :initarg :file   :accessor token-file)
   (text                 :initarg :text   :accessor token-text)))



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



(defmacro define-token-class (name)
  (let ((class-name (intern (concatenate 'string (string name) (string '-token)))))
    `(progn
       (defclass ,class-name   (token) ())
       (defun ,(intern (concatenate 'string (string name) (string '-p))) (object)
         (typep object ',class-name))
       (defmethod print-object ((self ,class-name) stream)
         (print-unreadable-object (self stream :identity nil :type t)
           (format stream "~A:~A:~A: ~S"
                   (token-file self) (token-line self) (token-column self) (token-text self)))
         self)
       (defun ,(intern (concatenate 'string (string 'make-) (string name))) (text column line file)
         (make-instance ',class-name :text text :column column :line line :file file)))))

(define-token-class identifier)
(define-token-class number)
(define-token-class string-literal)
(define-token-class character-literal)
(define-token-class punctuation)
(define-token-class other)

(defun pseudo-token (file lino)
  (make-other "" 0 lino file))


(defun sharpp (token)
  (and (typep token 'punctuation-token)
       (or (string= "#"  (token-text token)))))

(defun sharpsharpp (token)
  (and (typep token 'punctuation-token)
       (or (string= "##"  (token-text token)))))

(defun spacep (token)
  (and (typep token 'punctuation-token)
       (or (string= " "  (token-text token)))))

(defun openp (token)
  (and (typep token 'punctuation-token)
       (or (string= "("  (token-text token)))))

(defun closep (token)
  (and (typep token 'punctuation-token)
       (or (string= ")"  (token-text token)))))

(defun open-bracket-p (token)
  (and (typep token 'punctuation-token)
       (or (string= "<"  (token-text token)))))

(defun close-bracket-p (token)
  (and (typep token 'punctuation-token)
       (or (string= ">"  (token-text token)))))

(defun commap (token)
  (and (typep token 'punctuation-token)
       (or (string= ","  (token-text token)))))

(defun ellipsisp (token)
  (and (typep token 'punctuation-token)
       (or (string= "..."  (token-text token)))))


(defun identifierp (token)
  (typep token 'identifier-token))

(defun number-token-p (token)
  (typep token 'number-token))



(defun cpp-error (token format-control &rest format-arguments)
  (let ((*context* (if (typep token 'context)
                       token
                       (updated-context :token token
                                        :line (token-line token)
                                        :column (token-column token)
                                        :file (token-file token)))))
    (cerror "Continue" "~A:~A: ~?"
            (context-file *context*)
            (context-line *context*)
            format-control format-arguments)))

(defun cpp-warning (token format-control &rest format-arguments)
  (let ((*context* (if (typep token 'context)
                       token
                       (updated-context :token token
                                        :line (token-line token)
                                        :column (token-column token)
                                        :file (token-file token)))))
    (warn "~A:~A: ~?"
          (context-file *context*)
          (context-line *context*)
          format-control format-arguments)))





;;;; THE END ;;;;
