;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-scanner.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Generates a scanner class with its SCAN-NEXT-TOKEN method.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-02 <PJB> Extracted from rdp.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")
(declaim (declaration stepper))

(defgeneric print-parser-error  (error stream))
(defgeneric print-scanner-error (error stream))
(defgeneric scanner-end-of-line-p   (scanner))
(defgeneric scanner-end-of-source-p (scanner))
(defgeneric advance-line            (scanner))
(defgeneric accept                  (scanner token))


(defmethod print-scanner-error ((err scanner-error) stream)
  (declare (stepper disable))
  (format stream "~&~@[~A:~]~D:~D: ~?~%"
          (let ((source (scanner-source (scanner-error-scanner err))))
            (typecase source
              (file-stream  (or (ignore-errors (pathname source))
                                (scanner-error-file err)))
              (peek-stream  (or (ignore-errors (pathname (peek-stream-stream source)))
                                (scanner-error-file err)))
              (otherwise    (scanner-error-file err))))
          (scanner-error-line err)
          (scanner-error-column err)
          (scanner-error-format-control err)
          (scanner-error-format-arguments err))
  err)


(define-condition unexpected-token-error (scanner-error)
  ((expected-token     :initarg :expected-token
                       :initform nil
                       :reader unexpected-token-error-expected-token)
   (non-terminal-stack :initarg :non-terminal-stack
                       :initform '()
                       :reader unexpected-token-error-non-terminal-stack))
  (:report print-scanner-error))



(defclass buffered-scanner (scanner)
  ((buffer       :accessor scanner-buffer
                 :type     (or null string)
                 :initform nil)
   (current-text :accessor scanner-current-text
                 :initform ""))
  (:default-initargs :line 0))

(defmethod slots-for-print append ((self token))
  (extract-slots self '(buffer current-text)))

(defmethod scanner-end-of-line-p ((scanner buffered-scanner))
  (or (null (scanner-buffer scanner))
      ;; column is 1-based:
      (< (length (scanner-buffer scanner))
         (scanner-column scanner))))

(defmethod scanner-end-of-source-p ((scanner buffered-scanner))
  (and (scanner-end-of-line-p scanner)
       (let ((ps  (slot-value scanner 'stream)))
         (not (ungetchar ps (getchar ps))))))

(defmethod advance-line ((scanner buffered-scanner))
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


(defgeneric word-equal (a b)
  (:method ((a t) (b t))           (eql a b))
  (:method ((a string) (b string)) (string= a b))
  (:method ((a symbol) (b string)) (string= a b))
  (:method ((a string) (b symbol)) (string= a b))
  (:method ((token token) (kind symbol)) (eql (token-kind token) kind))
  (:method ((kind symbol) (token token)) (eql (token-kind token) kind)))

(defmethod accept ((scanner buffered-scanner) token)
  (if (word-equal token (scanner-current-token scanner))
      (prog1 (list (token-kind (scanner-current-token scanner))
                   (scanner-current-text scanner)                   
                   (scanner-column scanner))
        (scan-next-token scanner))
      (error 'unexpected-token-error
             :file   (scanner-file scanner)
             :line   (scanner-line scanner)
             :column (scanner-column scanner)
             :scanner scanner
             :expected-token token
             :format-control "Expected ~S, not ~A (~S)"
             :format-arguments (list token
                                     (scanner-current-token scanner)
                                     (scanner-current-text scanner)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun generate-scanner (name superclass terminals skip-spaces alphanumerics spaces)
    ;;
    ;; an-terminals  = literal terminals (given as string in rules), ending with an alphanumeric.
    ;; nan-terminals = literal terminals ending with something else than an alphanumeric.
    ;; nl-terminals  = non-literal terminals (specified in :terminals clauses).
    ;;
    ;; an-terminals are scanned by excluding alphanumerics directly after them.
    ;; "while" --> "(while)([^A-Za-z0-9]|$)"  so that "while whilemin" scans as <while> <identifier>.
    ;;
    ;; nl-terminals are processed in the order they're given in the :terminals clauses.
    ;;
    
    (let* (
           ;; Literal Alpha Numeric Terminals
           (an-terminals  (sort (remove-if-not
                                 (lambda (item)
                                   (and (stringp item)
                                        (find (aref item (1- (length item))) alphanumerics)))
                                 terminals)
                                (function >) :key (function length)))
           ;; Literal Non Alpha Numeric Terminals
           (nan-terminals (sort (remove-if-not
                                 (lambda (item)
                                   (and (stringp item)
                                        (not (find (aref item (1- (length item))) alphanumerics))))
                                 terminals)
                                (function >) :key (function length)))
           ;; Non Literal Terminals
           (nl-terminals (remove-if (function stringp) terminals))
           ;; Regexps for all the Literal Alpha Numeric Terminals
           (lit-an-terminals-regexp
             (format nil "^(~{~A~^|~})([^~A]|$)"
                     (mapcar (function regexp-quote-extended) an-terminals)
                     alphanumerics))
           ;; Regexps for all the Literal Non Alpha Numeric Terminals
           (lit-nan-terminals-regexp
             (format nil "^(~{~A~^|~})"
                     (mapcar (function regexp-quote-extended)  nan-terminals)))
           (kind-pname   (package-name *package*))
           (symbol-pname (package-name *package*)))

      `(progn

         (defclass ,name  (,superclass)
           ()
           (:default-initargs :spaces ,spaces
                              :token-kind-package   (load-time-value (find-package ,kind-pname))
                              :token-symbol-package (load-time-value (find-package ,symbol-pname))))

         (defmethod scan-next-token ((scanner ,name) &optional parser-data)
           "RETURN: (scanner-current-token scanner)" 
           (declare (ignore parser-data))
           (let (match) 
             ,@(when skip-spaces
                 `((setf match (string-match (format nil "^([~A]+)" (coerce (scanner-spaces scanner) 'string))
                                             (scanner-buffer scanner)
                                             :start (1- (scanner-column scanner))))
                   (when match
                     (setf (scanner-column scanner) (1+ (match-end 1 match))))))
             (let ((pos (1- (scanner-column scanner))))
               (cond
                 ;; end of source
                 ((scanner-end-of-source-p scanner)
                  (setf (scanner-column scanner)   (1+ (length (scanner-buffer scanner)))
                        (scanner-current-text scanner)   "<END OF SOURCE>"
                        (scanner-current-token scanner) '|<END OF SOURCE>|))
                 ;; end of line
                 ((scanner-end-of-line-p scanner)
                  (advance-line scanner))
                 ;; Literal Alpha Numeric and Non Alpha Numeric Terminals:
                 ,@(when (or an-terminals nan-terminals)
                     ;; (print (list an-terminals nan-terminals))
                     `(((or ,@(when an-terminals
                                `((setf match (string-match ',lit-an-terminals-regexp
                                                            (scanner-buffer scanner)
                                                            :start pos))))
                            ,@(when nan-terminals
                                `((setf match (string-match ',lit-nan-terminals-regexp
                                                            (scanner-buffer scanner)
                                                            :start pos)))))
                        (let ((text (match-string 1 (scanner-buffer scanner) match)))
                          (setf (scanner-column scanner)        (1+ (match-end 1 match))
                                (scanner-current-text scanner)  text
                                (scanner-current-token scanner) text))))) 
                 ;; Non Literal Terminals: we have a regexp for each terminal.
                 ,@(mapcar
                    (lambda (terminal)
                      ;; (print terminal)
                      `(,(if (= 4 (length terminal))
                             ;; (terminal-name match-regexp / exclude-regexp)
                             `(and (setf match (string-match
                                                ',(format nil "^(~A)" (second terminal))
                                                (scanner-buffer scanner)
                                                :start pos))
                                   (not (string-match ,(format nil "^(~A)" (fourth terminal))
                                                      (scanner-buffer scanner)
                                                      :start (match-end 1 match))))
                             ;; (terminal-name match-regexp)
                             `(setf match (string-match
                                           ',(format nil "^(~A)" (second terminal))
                                           (scanner-buffer scanner)
                                           :start pos)))
                        (setf (scanner-column scanner)        (1+ (match-end 1 match))
                              (scanner-current-text scanner)  (match-string 1 (scanner-buffer scanner) match)
                              (scanner-current-token scanner) ',(first terminal))))
                    nl-terminals)
                 ;; Else we have an error:
                 (t
                  (error 'scanner-error-invalid-character
                         :file   (scanner-file   scanner)
                         :line   (scanner-line   scanner)
                         :column (scanner-column scanner)
                         :state  (scanner-state  scanner)
                         :current-token (scanner-current-token scanner)
                         :scanner scanner
                         :invalid-character (aref (scanner-buffer scanner) pos)
                         :format-control "Invalid character ~S at position: ~D~%"
                         :format-arguments (list (aref (scanner-buffer scanner) pos)
                                                 (scanner-column scanner)))))))
           (setf (scanner-current-token scanner) (make-current-token scanner)))))))


(defmacro define-scanner (name &key
                                 (superclass 'buffered-scanner)
                                 terminals
                                 (alphanumerics "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
                                 (spaces #(#\space #\newline))
                                 (skip-spaces t))
  "

DO:             This macro generates a simple scanner.  This defines
                subclass of BUFFERED-SCANNER named `NAME'.

RETURN:         NAME

SUPERCLASS:     The name of the superclass of the class NAME. Should
                be a subclass of SCANNER.

TOKEN-CLASS-NAME:
                The name of the class of tokens. Should TOKEN or a
                subclass of TOKEN.

TERMINALS:      A list of couples (name-of-terminals
                regexp-of-terminal) or strings containing the literal terminal..

ALPHANUMERICS:  Characters that may be present in identifiers and keywords,
                and as such, shall be eaten greedily when scanning for keywords.

SKIP-SPACES:    When NIL, the spaces are not eaten between token, but
                they must be dealt with by definiting a terminal
                covering them.

"
  (generate-scanner name superclass terminals skip-spaces alphanumerics spaces))

;;;; THE END ;;;;
