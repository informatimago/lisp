;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rdp-lisp-boilerplate.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The lisp parser boilerplate.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-06 <PJB> Extracted from rdp.lisp.
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.RDP")

(declaim (declaration stepper))

(defvar *non-terminal-stack* '()
  "For error reporting.")

(define-condition parser-error (error)
  ((line    :initarg :line    :initform 1   :reader parser-error-line)
   (column  :initarg :column  :initform 0   :reader parser-error-column)
   (grammar :initarg :grammar :initform nil :reader parser-error-grammar)
   (scanner :initarg :scanner :initform nil :reader parser-error-scanner)
   (non-terminal-stack :initarg :non-terminal-stack
                       :initform '()
                       :reader parser-error-non-terminal-stack)
   (format-control     :initarg :format-control
                       :initform ""
                       :reader parser-error-format-control)
   (format-arguments   :initarg :format-arguments
                       :initform '()
                       :reader parser-error-format-arguments))
  (:report print-parser-error))

(defmethod print-parser-error ((err parser-error) stream)
  (declare (stepper disable))
  (format stream
          "~&~@[~A:~]~D:~D: ~?~%"
          (let ((source (scanner-source (parser-error-scanner err))))
            (unless (stringp source) (ignore-errors (pathname source))))
          (parser-error-line err)
          (parser-error-column err)
          (parser-error-format-control err)
          (parser-error-format-arguments err)))

(define-condition parser-end-of-source-not-reached (parser-error)
  ())



(define-condition unexpected-token-error (scanner-error)
  ((expected-token     :initarg :expected-token
                       :initform nil
                       :reader unexpected-token-error-expected-token)
   (non-terminal-stack :initarg :non-terminal-stack
                       :initform '()
                       :reader unexpected-token-error-non-terminal-stack))
  (:report print-scanner-error))

(defmethod print-scanner-error ((err scanner-error) stream)
  (declare (stepper disable))
  (format stream
          "~&~@[~A:~]~D:~D: ~?~%"
          (let ((source (scanner-source (scanner-error-scanner err))))
            (unless (stringp source) (ignore-errors (pathname source))))
          (scanner-error-line err)
          (scanner-error-column err)
          (scanner-error-format-control err)
          (scanner-error-format-arguments err)))

(defmethod print-object ((err scanner-error) stream)
  (print-scanner-error err stream)
  err)


(defclass rdp-scanner (scanner)
  ((buffer       :accessor scanner-buffer
                 :type     (or null string)
                 :initform nil)
   (current-text :accessor scanner-current-text
                 :initform ""))
  (:default-initargs :line 0))

(defmethod scanner-current-token ((scanner rdp-scanner))
  (token-kind (call-next-method)))

(defmethod scanner-end-of-line-p ((scanner rdp-scanner))
  (or (null (scanner-buffer scanner))
      ;; column is 1-based:
      (< (length (scanner-buffer scanner))
         (scanner-column scanner))))

(defmethod scanner-end-of-source-p ((scanner rdp-scanner))
  (and (scanner-end-of-line-p scanner)
       (let ((ps  (slot-value scanner 'stream)))
         (not (ungetchar ps (getchar ps))))))

(defmethod advance-line ((scanner rdp-scanner))
  "RETURN: The new current token = old next token"
  (cond
    ((scanner-end-of-source-p scanner)
     #|End of File -- don't move.|#
     (scanner-current-token scanner))
    ((setf (scanner-buffer scanner) (readline (slot-value scanner 'stream)))
     ;; We must skip the empty lines.
     (incf (scanner-line          scanner))
     (setf (scanner-column        scanner) 1
           (scanner-current-text  scanner) ""
           (scanner-current-token scanner) nil)
     ;; (loop :do (incf (scanner-line   scanner))
     ;;   :while (and (zerop (length (scanner-buffer scanner)))
     ;;               (setf (scanner-buffer scanner) (readline (slot-value scanner 'stream)))))
     ;; got a line -- advance a token.
     (scan-next-token scanner))
    (t
     ;; Just got EOF
     (setf (scanner-current-text  scanner) "<END OF FILE>"
           (scanner-current-token scanner) '|<END OF FILE>|))))


(defmethod accept ((scanner rdp-scanner) token)
  (if (word-equal token (scanner-current-token scanner))
      (prog1 (list (token-kind (scanner-current-token scanner))
                   (scanner-current-text scanner)
                   (scanner-column scanner))
        (scan-next-token scanner))
      (error 'unexpected-token-error
             :line   (scanner-line   scanner)
             :column (scanner-column scanner)
             :state  (scanner-state  scanner)
             :current-token (scanner-current-token scanner)
             :scanner scanner
             :non-terminal-stack (copy-list *non-terminal-stack*)
             :expected-token token
             :format-control "Expected ~S, not ~A (~S)~%~S~%" ;; "~{~A --> ~S~}"
             :format-arguments (list
                                token
                                (scanner-current-token scanner)
                                (scanner-current-text scanner)
                                *non-terminal-stack*
                                ;; (assoc (first *non-terminal-stack*)
                                ;;        ',(grammar-rules grammar))
                                ))))

(defparameter *spaces*
  (format nil "^([~{~C~}]+)" '(#\space #\newline #\tab)))

;;;; THE END ;;;;
