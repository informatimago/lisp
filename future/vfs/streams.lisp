;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               streams.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file exports streams.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-01-15 <PJB> Extracted from 'virtual-fs.lisp'.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 21. Streams
;;; http://www.lispworks.com/documentation/HyperSpec/Body/21_.htm

(define-condition simple-stream-error (stream-error simple-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~?"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))))

;;;---------------------------------------------------------------------
;;; STREAM
;;;---------------------------------------------------------------------

(defclass stream ()
  ((open-p          :accessor %open-stream-p
                    :initarg  :open-p
                    :initform nil)
   (element-type    :accessor %stream-element-type
                    :initarg  :element-type
                    :initform 'character)
   (external-format :accessor %stream-external-format
                    :initarg  :external-format
                    :initform :default)
   (input-p         :accessor %input-stream-p
                    :initarg  :input-p
                    :initform nil)
   (output-p        :accessor %output-stream-p
                    :initarg  :output-p
                    :initform nil)))

(defmethod print-object-fields ((self stream) stream)
  (format stream " ~:[CLOSED~;OPEN~] ~[PROBE~;INPUT~;OUTPUT~;I/O~] :ELEMENT-TYPE ~S :EXTERNAL-FORMAT ~S"
          (%open-stream-p self)
          (if (%input-stream-p self)
              (if (%output-stream-p self)
                  3
                  1)
              (if (%output-stream-p self)
                  2
                  0))
          (%stream-element-type self)
          (%stream-external-format self)))


(defmethod print-object ((self stream) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (print-object-fields self stream))
  self)


(defclass string-stream (stream)
  ())





;; (define-forward name arguments
;;   [ documentation-string ]
;;   { declarations }
;;   { forward-method-description })
;;
;; (declare (stream-arguments stream)
;;          (stream-designnator (istream :input)) ; default
;;          (stream-designator  (ostream :output))
;;          (check-stream-type file-stream)
;;          (cl-forward t))
;; 
;; (declare (stream-arguments stream))
;; 
;; (declare (check-stream-type file-stream))
;;
;; method-description ::= (:method class [[declaration* | documentation]] form*)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun make-method-lambda-list (lambda-list self-name self-type)
    (let* ((got-it nil)
           (mand (mapcar (lambda (par) 
                           (let ((name (parameter-name par)))
                             (if (eq name self-name)
                                 (progn (setf got-it t)
                                        (list name self-type))
                                 (list name 't))))
                         (lambda-list-mandatory-parameters lambda-list)))
           (opti (let ((optionals  (lambda-list-optional-parameters lambda-list)))
                   (cond
                     ((null optionals) nil)
                     (got-it (cons '&optional 
                                   (mapcar (function parameter-specifier)
                                           optionals)))
                     (t (let ((pos  (position self-name optionals
                                              :key (function parameter-name))))
                          (if pos
                              (append
                               (mapcar (lambda (par) (list (parameter-name par) 't))
                                       (subseq optionals 0 pos))
                               (list
                                (list (parameter-name (nth pos optionals))
                                      self-type))
                               (when (< (1+ pos) (length optionals))
                                 (cons '&optional 
                                       (mapcar (function parameter-specifier)
                                               (subseq optionals (1+ pos))))))
                              (cons '&optional 
                                    (mapcar (function parameter-specifier)
                                            optionals))))))))
           (keys (mapcar (function parameter-specifier)
                         (lambda-list-keyword-parameters lambda-list)))
           (rest (and (lambda-list-rest-p lambda-list)
                      (mapcar (function parameter-specifier)
                              (lambda-list-rest-parameter lambda-list)))))
      (append mand opti
              (when keys (cons '&key keys))
              (when rest (list '&rest rest))))))


(defun stream-designator (stream direction)
  "DIRECTION is either *standard-input* or *standard-output*"
  (case stream 
    ((t)       *terminal-io*)
    ((nil)     direction)
    (otherwise stream)))

(defun signal-type-error (object type)
  (error (make-condition 'type-error :datum object :expected-type type)))





(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *stream-methods* (make-hash-table)
    "Keep the information about methods defined with DEFINE-FORWARD,
for use by DEFINE-STREAM-METHODS"))


(defun check-open (method stream)
  (unless (%open-stream-p stream)
    (error (make-condition 'simple-stream-error
                           :stream stream
                           :format-control "~S on ~S is illegal: the stream must be open. "
                           :format-arguments (list method stream)))))


(defclass cl-stream (stream) ()) ; forward declaration for define-forward...

#+emacs (put 'define-forward 'lisp-indent-function 2)
(defmacro define-forward (name arguments &body body)
  "
DO:     Specifies the name and parameter list of methods.
        The BODY contains declarations and method clauses.

        Specific pseudo-declarations are:

        (stream-argument   stream-parameter)
        (stream-designator (stream-parameter [:input|:output]))

            Specify the stream parameter.  In the case of
            stream-designator, the stream can be *standard-input* or
            *standard-output* by default, as indicated by the keyword.
        
        (check-stream-type stream-parameter)

            When given, the stream type is checked in the default method.
            (overriding methods should (call-next-method)).

        (check-open-p      stream-parameter)

            When given, the methods generated by DEFINE-STREAM-METHODS
            will test for an open stream.

        (cl-forward        booolean)

             When the boolean is true, a method is defined for CL-STREAM
             that forwards the call to the corresponding CL function.

        The method clauses in the body are of the form:

        (:method class . body)

             For each of these clause, method is defined for the given
             stream class.
       
"
  (let* ((documentation     (extract-documentation body))
         (declarations      (declarations-hash-table (extract-declarations  body)))
         (body              (extract-body          body))
         (stream-argument   (caar  (gethash 'stream-argument   declarations)))
         (stream-designator (caar  (gethash 'stream-designator declarations)))
         (stream-name       (or stream-argument  
                                (if (consp stream-designator)
                                    (first stream-designator)
                                    stream-designator)))
         (check-stream-type (caar  (gethash 'check-stream-type declarations)))
         (cl-forward        (caar  (gethash 'cl-forward        declarations)))
         (check-open-p      (caar  (gethash 'check-open-p      declarations)))
         (lambda-list       (parse-lambda-list arguments :ordinary))
         (m-name            (intern (format nil "%~A" (string name))))
         (cl-name           (intern (string name) "COMMON-LISP")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *stream-methods*) 
               (list ',m-name (parse-lambda-list ',arguments :ordinary)
                     ',stream-name ',check-open-p)))
       (defun ,name ,arguments
         ,@(when documentation (list documentation))
         ,@(when stream-designator
             `((setf ,stream-name (stream-designator 
                                   ,stream-name
                                   ,(if (listp stream-designator)
                                        (ecase (second stream-designator)
                                          ((:input)  '*standard-input*)
                                          ((:output) '*standard-output*))
                                        '*standard-input*)))))
         ,(if (lambda-list-rest-p lambda-list)
              `(apply (function ,m-name) ,@(make-argument-list lambda-list))
              `(,m-name         ,@(butlast (make-argument-list lambda-list)))))
       ,@(when cl-forward
           ;; TODO: review the generation of generic function lambda list:
           (let ((method-lambda-list (make-method-lambda-list lambda-list stream-name 'cl-stream)))
             `((defgeneric ,m-name ,(mapcar (lambda (parameter)
                                              (if (listp parameter)
                                                  (first parameter)
                                                  parameter))
                                     method-lambda-list))
               (defmethod ,m-name ,method-lambda-list
                 ,(let ((arguments (mapcar
                                    (lambda (arg)
                                      (if (eq arg stream-name)
                                          `(cl-stream-stream ,stream-name)
                                          arg))
                                    (make-argument-list lambda-list))))
                    (if (lambda-list-rest-p lambda-list)
                        `(apply (function ,cl-name) ,@arguments)
                        `(,cl-name ,@(butlast arguments)))))
               ;; We don't want to allow access to CL:STREAM from a sandbox.
               ;; (defmethod ,m-name 
               ;;     ,(make-method-lambda-list lambda-list stream-name 'cl:stream)
               ;;   ,(let ((arguments (make-argument-list lambda-list)))
               ;;         (if (lambda-list-rest-p lambda-list)
               ;;             `(apply (function ,cl-name) ,@arguments)
               ;;             `(,cl-name ,@(butlast arguments)))))
               )))
       ,@(when check-stream-type
           `((defmethod ,m-name ,(make-method-lambda-list lambda-list stream-name 't)
               (signal-type-error ,stream-name ',check-stream-type))))
       ,@(mapcar
          (lambda (method)
            (when (and (listp method) (eq :method (car method)))
              (destructuring-bind (method class-name &body body) method
                (declare (ignore method))
                `(defmethod ,m-name
                     ,(make-method-lambda-list lambda-list stream-name class-name)
                   ,@body))))
          body))))


#+emacs (put 'define-stream-methods 'lisp-indent-function 1)
(defmacro define-stream-methods (class-name &body methods)
  "
DO:     Expands to a bunch of defmethod forms, with the parameter
        defined with DEFINE-FORWARD, and the body provided in the
        METHODS clauses.
"
  `(progn
     ,@(mapcar (lambda (method)
                 (let ((minfo (gethash (first method) *stream-methods*)))
                   (unless minfo
                     (error "Unknown method ~S; please use DEFINE-FORWARD first"
                            (first method)))
                   (destructuring-bind (name lambda-list stream-name check-open-p)
                       minfo
                     `(defmethod ,name 
                          ,(make-method-lambda-list lambda-list stream-name class-name)
                        ,@(when check-open-p `((check-open ',name ,stream-name)))
                        ,@(rest method)))))
               methods)))



(define-forward input-stream-p       (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t)))

(define-forward output-stream-p      (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t)))

(define-forward interactive-stream-p (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t))
  (:method stream nil))

(define-forward open-stream-p        (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t)))

(define-forward stream-element-type  (stream) 
  (declare (stream-argument stream)
           (check-stream-type stream)
           (cl-forward t)))

(defun streamp (object) (typep object 'stream))

(defun eof-stream (stream eof-error-p eof-value)
  (if eof-error-p
      (error (make-condition 'eof-error :stream stream))
      eof-value))


(define-forward read-byte (stream &optional (eof-error-p t) (eof-value nil))
  (declare (stream-argument stream)
           (check-stream-type stream) 
           (cl-forward t)
           (check-open-p t)))

(define-forward write-byte (byte stream)
  (declare (stream-argument stream)
           (check-stream-type stream) 
           (cl-forward t)
           (check-open-p t)))

(define-forward peek-char (&optional (peek-type nil) (stream *standard-input*)
                                     (eof-error-p t) (eof-value nil)
                                     (recursive-p nil))
  (declare (stream-designator (stream :input))
           (cl-forward t)
           (check-open-p t)))


(define-forward read-char (&optional (input-stream *standard-input*) 
                                     (eof-error-p t) (eof-value nil)
                                     (recursive-p nil))
  (declare (stream-designator (input-stream :input))
           (cl-forward t)
           (check-open-p t)))


(define-forward read-char-no-hang (&optional (input-stream *standard-input*) 
                                             (eof-error-p t) (eof-value nil)
                                             (recursive-p nil))
  (declare (stream-designator (input-stream :input))
           (cl-forward t)
           (check-open-p t)))


(define-forward terpri (&optional (output-stream *standard-output*))
  (declare (stream-designator (output-stream :output))
           (cl-forward t)
           (check-open-p t)))


(define-forward fresh-line (&optional (output-stream *standard-output*))
  (declare (stream-designator (output-stream :output))
           (cl-forward t)
           (check-open-p t)))


(define-forward unread-char (character &optional (input-stream *standard-input*))
  (declare (stream-designator (input-stream :input))
           (cl-forward t)
           (check-open-p t)))


(define-forward write-char (character
                            &optional (output-stream *standard-output*))
  (declare (stream-designator (output-stream :output))
           (cl-forward t)
           (check-open-p t)))


(define-forward read-line (&optional (input-stream *standard-input*)
                                     (eof-error-p t) (eof-value nil)
                                     (recursive-p nil))
  (declare (stream-designator (input-stream :input))
           (cl-forward t)
           (check-open-p t)))


(define-forward write-string (string
                              &optional (output-stream *standard-output*)
                              &key (start 0) (end nil))
  (declare (stream-designator (output-stream :output))
           (cl-forward t)
           (check-open-p t)))


(define-forward write-line (string
                            &optional (output-stream *standard-output*)
                            &key (start 0) (end nil))
  (declare (stream-designator (output-stream :output))
           (cl-forward t)
           (check-open-p t)))


(define-forward read-sequence (sequence stream &key (start 0) (end nil))
  (declare (stream-argument stream)
           (cl-forward t)
           (check-open-p t)))


(define-forward write-sequence (sequence stream &key (start 0) (end nil))
  (declare (stream-argument stream)
           (cl-forward t)
           (check-open-p t)))


(define-forward file-length (stream)
  (declare (stream-argument stream)
           (check-stream-type file-stream) 
           (cl-forward t)))


(define-forward file-position (stream &optional (position-spec nil))
  (declare (stream-argument stream)
           (cl-forward t)
           (check-open-p t)))


(define-forward file-string-length (stream object)
  (declare (stream-argument stream)
           (check-stream-type file-stream) 
           (cl-forward t)
           (check-open-p t)))


(define-forward stream-external-format (stream)
  (declare (stream-argument stream)
           (check-stream-type stream) 
           (cl-forward t)))


(define-forward close (stream &key (abort nil))
  (declare (stream-argument stream)
           (check-stream-type stream) 
           (cl-forward t)))


(define-forward listen (&optional input-stream)
  (declare (stream-designator input-stream)
           (cl-forward t) (check-open-p t)))


(define-forward clear-input (&optional input-stream)
  (declare (stream-designator (input-stream :input))
           (cl-forward t) (check-open-p t)))


(define-forward clear-output (&optional output-stream)
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))


(define-forward force-output (&optional output-stream)
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))


(define-forward finish-output (&optional output-stream)
  (declare (stream-designator (output-stream :output))
           (cl-forward t) (check-open-p t)))


;;;; THE END ;;;;
