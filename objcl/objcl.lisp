;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objcl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Objective-CL: Objective-C­like syntax
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-12-17 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2012
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
(in-package "COM.INFORMATIMAGO.OBJECTIVE-CL")

(define-condition read-error (stream-error)
  ((control-string :initarg :control-string
                   :initform "Read error"
                   :accessor read-error-control-string)
   (arguments      :initarg :arguments
                   :initform '()
                   :accessor read-error-arguments))
  (:report (lambda (condition stream)
             (format stream "In stream ~S: ~?"
                     (stream-error-stream condition)
                     (read-error-control-string condition)
                     (read-error-arguments condition)))))

(defun read-error (stream control-string &rest arguments)
  (error 'read-error
         :stream stream
         :control-string control-string
         :arguments arguments))

;; (PROGN (SETF *READTABLE* (COPY-READTABLE NIL NIL)) (SETF (READTABLE-CASE *READTABLE*) :UPCASE))

;; To reset the reader macro:
;; (if old-definition
;;      (set-macro-character char old-definition old-non-term)
;;      (set-syntax-from-char char char))

(defparameter *default-readtable*  (copy-readtable nil nil)
  "The default readtable.  Used to read @symbols.")

(defparameter *lisp-readtable*
  (let ((rt (copy-readtable *readtable* nil)))
    (set-syntax-from-char #\[ #\( rt)
    (set-syntax-from-char #\] #\) rt)
    (set-macro-character #\[ 'objcl-expression-reader-macro nil rt)
    (set-macro-character #\@ 'objcl-definition-reader-macro t   rt)
    rt)
  "The lisp readtable, used to read lisp expressions (possibly
including Objective-CL forms starting with #\\[ or #\\@).")

(defparameter *objc-readtable*
  (let ((rt (copy-readtable *readtable* nil)))
    (set-syntax-from-char #\[ #\( rt)
    (set-syntax-from-char #\] #\) rt)
    (set-macro-character #\[ 'objcl-expression-reader-macro nil rt)
    (set-macro-character #\@ 'objcl-definition-reader-macro t   rt)
    (setf (readtable-case rt) :preserve)
    rt)
  "The Objective-CL readtable, used to read Objective-CL expressions
\(possibly including Objective-CL forms starting with #\\[ or #\\@).
Basically the same as *lisp-readtable*, but with readtable-case set to :preserve.")


(defun skip-spaces (stream)
  (peek-char t stream nil nil t))

(defun read-identifier (stream)
  (let ((buffer (loop
                   :for ch = (peek-char nil stream nil nil t)
                   :while (and ch (or (alphanumericp ch)
                                      (find ch "-_<>")))
                   :collect (read-char stream nil nil t))))
    (if buffer
        (coerce buffer 'string)
        (error "Expected an identifier, instead got character '~C'"
               (peek-char nil stream nil nil t)))))


(defun split-string (string &optional (separators " "))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (let ((chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)))
    (loop :while (< position strlen) :do
       (loop
          :while (and (< nextpos strlen)
                      (not (find (aref string nextpos) separators)))
          :do (incf nextpos))
       (push (subseq string position nextpos) chunks)
       (setf position (incf nextpos)))
    (nreverse chunks)))


(defun objc-to-lisp-classname (identifier &optional (*package* *package*))
  (let ((classname (oclo:objc-to-lisp-classname identifier *package*)))
    (etypecase classname
      (string (intern classname *package*))
      (symbol classname))))

(defun objc-to-lisp-identifier (identifier)
  (or (oclo:objc-to-lisp-classname-p identifier)
      (let ((*readtable* *lisp-readtable*))
        (read-from-string identifier))))

(defun objc-to-lisp-message (selector)
  (mapcar (lambda (name)
            (if (zerop (length name))
                :||
                (first (oclo:objc-to-lisp-message (concatenate 'string name ":")))))
          (split-string selector ":")))


(defun read-type-specifier (stream)
  (assert (eql #\( (skip-spaces stream)))
  (let* ((*package*      (find-package "KEYWORD"))
         (*readtable*    *lisp-readtable*)
         (type-specifier (read stream t nil t)))
    (assert (listp type-specifier))
    (assert (= 1 (length type-specifier)))
    (assert (keywordp (first type-specifier)))
    (first type-specifier)))


(defun read-final-signature (stream)
  (let ((*readtable* *lisp-readtable*))
   (let ((rest      (read stream t nil t))
         (parameter (read stream t nil t)))
     (assert (eql '&rest rest))
     parameter)))


(defun read-method-signature (stream)
  "
RETURN: a list containing the selector, a list of parameters, and the rest parameter.
"
  (let ((simple-selector (and (skip-spaces stream)
                              (read-identifier stream)))
        (next-char       (peek-char nil stream nil nil t))
        (selector        '())
        (parameters      '()))
    (if (eql #\: next-char)
        (loop
           :named compound-selector
           :initially (push simple-selector selector)
           :while (eql #\: next-char)
           :do
           (read-char stream t nil t)
           ;; (type-identifier) objcl-identifier
           (let ((next-char (skip-spaces stream)))
             (cond
               ((or (null next-char) (eql #\) next-char))
                (error "Missing argument after selector part ~A:"
                       (car (last selector 2))))
               ((eql #\( next-char)
                (let ((type-identifier (read-type-specifier stream))
                      (parameter       (let ((*readtable*  *lisp-readtable*))
                                         (read stream t nil t))))
                  (assert (symbolp parameter)
                          (parameter)
                          "The parameter name should be a symbol.")
                  (push (list type-identifier parameter) parameters)))
               (t
                (error "Expecpted a type specifier in parentheses instead of '~C'"
                       next-char))))
           ;; objcl-identifier ':' 
           (setf next-char (skip-spaces stream))
           (cond
             ((or (null next-char) (char= #\) next-char))
              (return-from compound-selector
                (list (format nil "~{~A:~}" (nreverse  selector))
                      (nreverse parameters)
                      nil)))
             ((char= #\& next-char)
              (return-from compound-selector
                (list (format nil "~{~A:~}" (nreverse  selector))
                      (nreverse parameters)
                      (read-final-signature stream))))
             ((char= #\: next-char) ; empty selector-part
              (push "" selector))
             (t
              (push (and (skip-spaces stream)
                         (read-identifier stream)) selector)
              (setf next-char (peek-char nil stream nil nil t))))
           :finally (error "~@[Invalid character '~C'. ~]Expected a colon after identifier '~A' in selector '~{~A:~}'."
                           next-char (first selector) (reverse selector)))
        (list simple-selector nil nil))))


(defun read-final-arguments (stream)
  (loop
     :while (eql #\( (skip-spaces stream))
     :collect (list (read-type-specifier stream)
                    (let ((*readtable* *lisp-readtable*)) (read stream t nil t)))))

(defun read-message (stream)
  (let ((selector-part (and (skip-spaces stream)
                              (read-identifier stream)))
        (next-char       (peek-char nil stream nil nil t))
        (selector        '())
        (arguments       '()))
    (if (eql #\: next-char)
        (loop
           :named compound-selector
           :initially (push selector-part selector)
           :while (eql #\: next-char)
           :do
           (read-char stream t nil t)
           ;; objcl-expression
           (let ((next-char (skip-spaces stream)))
             (when (or (null next-char) (eql #\] next-char))
               (error "Missing argument after selector part ~A:" (car (last selector 2))))
             (push (let ((*readtable* *lisp-readtable*)) (read stream t nil t))
                   arguments))
           ;; objcl-identifier ':' 
           (setf next-char (skip-spaces stream))
           (cond
             ((or (null next-char) (char= #\] next-char))
              (return-from compound-selector
                (list (format nil "~{~A:~}" (nreverse  selector))
                      (nreverse arguments)
                      '())))
             ((char= #\( next-char)
              (return-from compound-selector
                (list (format nil "~{~A:~}" (nreverse  selector))
                      (nreverse arguments)
                      (read-final-arguments stream))))
             ((char= #\: next-char) ; empty selector-part
              (push "" selector))
             (t
              (push (and (skip-spaces stream)
                         (read-identifier stream)) selector)
              (setf next-char (peek-char nil stream nil nil t))))
           :finally (error "~@[Invalid character '~C'. ~]Expected a colon after identifier '~A' in selector '~{~A:~}'."
                           next-char (first selector) (reverse selector)))
        (list selector-part arguments '()))))


(defparameter *pseudo-selector-parts*
  '(("subClass"    . objcl-identifier)
    ("slots"       . list)
    ("method"      . signature)
    ("classMethod" . signature)
    ("resultType"  . type-specifier)
    ("body"        . body)))


(defun read-pseudo-message (stream)
  "
Read a pseudo-message.  The following selector parts with the
indicated following arguments are expected:

    subClass:    objcl-identifier
    slots:       list (sexp)
    method:      '(' signature ')'
    classMethod: '(' signature ')'
    resultType:  '(' type-identifier ')'
    body:        list...

Return a list containing:
   - the compound selector,
   - a list of arguments but those of body:, and
   - a list of the body arguments.
"
  (let ((selector-part (and (skip-spaces stream)
                            (read-identifier stream)))
        (next-char       (peek-char nil stream nil nil t))
        (selector        '())
        (arguments       '())
        (body            '()))
    (if (eql #\: next-char)
        (loop
           :named compound-selector
           :initially (push selector-part selector)
           :while (eql #\: next-char)
           :do
           (read-char stream t nil t) ; read the colon
           ;; read argument
           (let ((next-char (skip-spaces stream)))
             (when (or (null next-char) (eql #\] next-char))
               (error "Missing argument after selector part ~A:" (car (last selector 2))))
             (case (cdr (assoc selector-part *pseudo-selector-parts* :test (function string=)))
               ((objcl-identifier)
                (push (read-identifier stream) arguments))
               ((type-specifier)
                (push (read-type-specifier stream) arguments))
               ((signature)
                (push (progn
                        (assert (eql #\( (skip-spaces           stream)))
                        (read-char stream t nil t)
                        (prog1
                            (read-method-signature stream)
                          (assert (eql #\) (skip-spaces           stream)))
                          (read-char stream t nil t))) arguments))
               ((body)
                (loop
                   :do (push (let ((*readtable* *lisp-readtable*))
                               (read stream t nil t)) body)
                   :until (or (null (setf next-char (skip-spaces stream))) (eql #\] next-char))))
               (otherwise ; same as list
                (push (let ((*readtable* *lisp-readtable*))
                        (read stream t nil t))
                      arguments))))
           ;; read selector-part
           (setf next-char (skip-spaces stream))
           (cond
             ((or (null next-char) (char= #\] next-char))
              (return-from compound-selector
                (list (format nil "~{~A:~}" (nreverse  selector))
                      (nreverse arguments)
                      (nreverse body))))
             ((char= #\( next-char)
              (error "Syntax error in pseudo-message send, expected a selector part, but got an opening parenthesis after reading ~{~A:~}."
                     (reverse selector)))
             ((char= #\: next-char) ; empty selector-part
              (push "" selector))
             (t
              (push (setf selector-part (and (skip-spaces stream)
                                             (read-identifier stream))) selector)
              (setf next-char (peek-char nil stream nil nil t))))
           :finally (error "~@[Invalid character '~C'. ~]Expected a colon after identifier '~A' in selector '~{~A:~}'."
                           next-char (first selector) (reverse selector)))
        (list selector-part arguments (nreverse body)))))


(defun read-message-send (stream recipient read-message)
  (let ((next-char (skip-spaces stream)))
    (cond
      ((alpha-char-p next-char)
       (destructuring-bind (selector arguments final-arguments) (funcall read-message stream)
         (cond
           ((char= #\] (skip-spaces stream)) (read-char stream))
           (t (read-error stream "Syntax error, missing closing bracket after ~A" selector)))
         (list recipient selector arguments final-arguments)))
      ((char= #\] next-char)
       (read-error stream "Syntax error, missing a selector in the brackets, after ~S"
                   recipient))
      (t
       (read-error stream "Lexical error,  unexpected character: ~C, in the brackets, after ~S"
                   next-char recipient)))))


#-(and)
(defun read-class-definition (stream)
  (let ((class-name       (progn (skip-spaces     stream)
                                 (read-identifier stream)))
        (super-class-name (progn (skip-spaces     stream)
                                 (read-identifier stream)))
        (slots            (let ((*readtable* *lisp-readtable*))
                            (loop
                               :until (char= #\] (skip-spaces stream))
                               :collect (read stream t nil t)
                               :finally (read-char stream t nil t)))))
    (list (oclo:objc-to-lisp-classname class-name)
          (oclo:objc-to-lisp-classname super-class-name)
          slots)))

#-(and)
(defun read-method-definition (stream)
  (let ((class-name       (progn (skip-spaces           stream)
                                 (read-identifier       stream)))
        (type-specifier   (progn (skip-spaces           stream)
                                 (read-type-specifier   stream)))
        (signature        (progn
                            (assert (eql #\( (skip-spaces           stream)))
                            (read-char stream)
                            (prog1
                                (read-method-signature stream)
                              (assert (eql #\) (skip-spaces           stream)))
                              (read-char stream))))
        (body             (let ((*readtable* *lisp-readtable*))
                            (loop
                               :until (char= #\] (skip-spaces stream))
                               :collect (read stream t nil t)
                               :finally (read-char stream t nil t)))))
    (list (oclo:objc-to-lisp-classname class-name)
          type-specifier signature body)))



(defun generate-message-send (recipient selector arguments final-arguments)
  ;; `(oclo:slet ((,result
  ;;               ))
  ;;             ,result)
  `(,@(if (and (symbolp recipient) (string-equal "super" recipient))
          `(oclo:objc-message-send-super ;; ,(objc-to-lisp-identifier "self")
                                         )
          `(oclo:send ,recipient))
      ,@(if arguments
            (mapcan (function list)
                    (objc-to-lisp-message selector)
                    arguments)
            (list `',(intern (symbol-name (first (objc-to-lisp-message selector))))))
      ,@(when final-arguments
              (list (mapcan (function copy-list) final-arguments)))))


(defun generate-class-definition (class-name super-class-name slots)
  `(defclass ,class-name (,super-class-name)
      ,slots
      (:metaclass ns:+ns-object))
  ;; (progn
  ;;    (eval-when (:execute :compile-toplevel :load-toplevel)
  ;;      (oclo:define-classname-translation ,() ,class-name))
  ;;    )
  )


(defun generate-method-definition (level class-name result-type signature body)
  "
LEVEL is :+ or :-  for class methods or instance methods.
"
  ;; (:ID "multipleArg:complexSelector:" ((:INT A) (:INT B)) NIL)
  (destructuring-bind (selector fixed-parameters rest-parameter) signature
    `(,(ecase level
              ((:-) 'oclo:define-objc-method)
              ((:+) 'oclo:define-objc-class-method))
       ((,result-type
         ,@(if fixed-parameters
               (mapcan (function list)
                       (objc-to-lisp-message selector)
                       fixed-parameters)
               (list `',(intern (symbol-name (first (objc-to-lisp-message selector))))))
         ,@(when rest-parameter (list rest-parameter)))
        ,class-name)
       ,@body)))



(defun message-send (stream recipient)
  "
Reads and parse a message sending (recipient is already read),
and generate a message sending form.
"
  (apply (function generate-message-send) (read-message-send stream recipient (function read-message))))


;; (defun class-definition (stream)
;;   (apply (function generate-class-definition) (read-class-definition stream)))
;; 
;; 
;; (defun class-method-definition (stream)
;;   (apply (function generate-method-definition) :+ (read-method-definition stream)))
;; 
;; 
;; (defun instance-method-definition (stream)
;;   (apply (function generate-method-definition) :- (read-method-definition stream)))



(defun objcl-definition (stream recipient)
  "
Reads and parse a pseudo-message sending (recipient is already read),
and generate an Objective-CL definition form.
"
  (destructuring-bind (recipient selector arguments body)
      (read-message-send stream recipient (function read-pseudo-message))
    ;; (print `(:recipient ,recipient :selector ,selector :arguments ,arguments :body ,body))
    (cond
      ((string= selector "subClass:slots:")
       (generate-class-definition (objc-to-lisp-classname (first arguments))
                                  (objc-to-lisp-classname recipient)
                                  (second arguments)))
      ((string= selector "classMethod:resultType:body:")
       (generate-method-definition :+ (objc-to-lisp-classname recipient)
                                   (second arguments) (first arguments) body))
      ((string= selector "method:resultType:body:")
       (generate-method-definition :- (objc-to-lisp-classname recipient)
                                   (second arguments) (first arguments) body))
      (t
       (error "Unknown Objective-CL definition selector ~A" selector)))))



(defun read-objcl-expression (stream)
  "Reads a message sending expressions.
The opening bracket has already been read.
Returns a message send form."
  (let ((*readtable* *objc-readtable*)
        (next-char (skip-spaces stream)))
    (if (alpha-char-p next-char)
        (let ((identifier (read-identifier stream)))
          (cond
            ((string= identifier "super")        (message-send stream :super))
            (t                                   (message-send stream (objc-to-lisp-identifier identifier)))))
        (message-send stream (let ((*readtable* *lisp-readtable*)) (read stream t nil t))))))


(defun read-objcl-definition (stream)
  "Reads an Objective-CL definition.
This can be either a subclass definition, an instance method or a class method definition.
In either case, the first token is a class name, followed by a pseudo-message send syntax.
The opening bracket has already been read.
Returns a defclass, a define-objc-method or a define-objc-class-method form.
"
  (let ((*readtable* *objc-readtable*)
        (next-char (skip-spaces stream)))
    (if (alpha-char-p next-char)
        (let ((identifier (read-identifier stream)))
          (cond
            ((string= identifier "super")
             (error "Cannot send Objective-CL definition message to 'super' in @[...]."))
            (t
             (objcl-definition stream identifier))))
        ;; (objcl-definition stream (let ((*readtable* *lisp-readtable*)) (read stream t nil t)))
        ;; This is strange, because low-level Objective-C API allow it without problem, but as documented in ccl:
        (error "We cannot ensure the right semantics for dynamic Objective-C class and method creation with the current bridge. Please, only use class identifier as recipient to Objective-CL definition messages in @[...]."))))


(defun read-suppressed-objcl-expression (stream)
  "
Reads and parses the following syntax:

    suppressed-objc-expression := '[' { suppressed-expression } ']' .
    suppressed-expression      := sexp | suppressed-objcl-expression .

Returns NIL.
Assumes the '[' has been read already.
"
  (loop
     :until (eql #\] (skip-spaces stream))
     :do (read stream t nil t)
     :finally (read-char stream))
  nil)



(defun objcl-expression-reader-macro (stream ch)
  (assert (eql #\[ ch))
  (if *read-suppress*
      (read-suppressed-objcl-expression stream)
      (read-objcl-expression stream)))


;;;
;;; Constants strings in objc-runtime don't support unicode characters.
;;; so we need to redo it here.
;;; http://www.cocoabuilder.com/archive/cocoa/131727-how-to-code-nsstring-literal-with-utf8.html
;; (objc:make-nsstring  "été")
;; #<NS-MUTABLE-STRING "été" (#x16038F80)>
;; objc:lisp-string-from-nsstring


(defun make-utf8-cstring (lstring)
  (let* ((llen  (length lstring))
         (clen  (ccl::utf-8-octets-in-string lstring 0 llen)))
    (declare (fixnum llen clen))
    (let* ((cstring (ccl::malloc (the fixnum (1+ clen)))))
      (ccl::utf-8-memory-encode lstring cstring 0 0 llen)
      (setf (ccl::%get-byte cstring clen) 0)
      #+testing
      (print (loop
                for str = cstring
                for i from 0 
                for ch = (ccl:%get-unsigned-byte str i)
                while (plusp ch)
                collect ch))
      (values cstring clen))))

(defun make-macroman-cstring (lstring)
  ;; We assume lstring contains only Mac-Roman characters.
  (let* ((llen  (length lstring)))
    (declare (fixnum llen))
    (let* ((cstring (ccl::malloc (the fixnum (1+ llen)))))
      (ccl::encode-string-to-memory (load-time-value (ccl:lookup-character-encoding :macosroman))
                                    cstring 0 lstring 0 llen)
      (setf (ccl::%get-byte cstring llen) 0)
      #+testing
      (print (loop
                for str = cstring
                for i from 0 
                for ch = (ccl:%get-unsigned-byte str i)
                while (plusp ch)
                collect ch))
      (values cstring llen))))


(defun %make-constant-nsstring (string)
  "Make a persistent (heap-allocated) NSConstantString from the
argument lisp string."
  #+apple-objc
  (if (mac-roman-string-p string)
      (ccl::make-record :<nsc>onstant<s>tring
                        :isa ccl::*nsconstantstring-class*
                        :bytes (make-macroman-cstring string)
                        :num<b>ytes (length string))
      (multiple-value-bind (bytes bytelen) (make-utf8-cstring string)
        (objc:send ns:ns-string
                   :string-with-c-string bytes
                   :encoding #$|NSUTF8StringEncoding|)))
  #+cocotron-objc
  (warn "Check the encoding used for NSConstantString in cocotron.")
  #+cocotron-objc
  (make-record :<nsc>onstant<s>tring
               :isa *nsconstantstring-class*
               :_bytes (make-cstring string)
               :_length (length string))

  #+gnu-objc
  (warn "Check the encoding used for NSConstantString in GNUstep on this system.")
  #+gnu-objc
  (make-record :<nxc>onstant<s>tring
               :isa *nsconstantstring-class*
               :c_string (make-cstring string)
               :len (length string)))


(defvar *objc-constant-strings* (make-hash-table :test #'equal))

(eval-when (:execute :compile-toplevel :load-toplevel)

  (defstruct objc-constant-string
    string 
    nsstringptr)

  #-(and)
  (defclass objc-constant-string ()
   ((string      :initarg :string
                 :accessor objc-constant-string-string
                 :initform nil)
    (nsstringptr :initarg :nsstringptr
                 :accessor objc-constant-string-nsstringptr
                 :initform nil)))

  );;eval-when

(defun ns-constant-string (string)
  (or (gethash string *objc-constant-strings*)
      (setf (gethash string *objc-constant-strings*)
            (make-objc-constant-string :string string
                                       :nsstringptr (%make-constant-nsstring string)))))

(defun ns-mutable-string (string)
  (multiple-value-bind (bytes bytelen) (make-utf8-cstring string)
    (declare (ignore bytelen))
    (objc:send ns:ns-string
               :string-with-c-string bytes
               :encoding #$|NSUTF8StringEncoding|)))


(defun lisp-string (an-objc-string)
  (typecase an-objc-string
    (ns:ns-constant-string     (objc:lisp-string-from-nsstring an-objc-string))
    (ns:ns-mutable-string      (objc:lisp-string-from-nsstring an-objc-string))
    (ccl::objc-constant-string (ccl::objc-constant-string-string an-objc-string))
    (objc-constant-string      (objc-constant-string-string an-objc-string))
    (t                         (ccl::%get-utf-8-cstring
                                (objc:send an-objc-string 'utf8-string)))))
#+testing
(print (loop
          for str = (objc:send a-nsstring 'utf8-string)
          for i from 0
          for ch = (ccl:%get-unsigned-byte str i)
          while (plusp ch)
          collect ch))

(eval-when (:execute :compile-toplevel :load-toplevel)

 (defmethod make-load-form ((s ns:ns-constant-string) &optional env)
   (declare (ignore env))
   `(ns-constant-string ,(lisp-string s)))

 (defmethod make-load-form ((s ns:ns-mutable-string) &optional env)
   (declare (ignore env))
   `(ns-mutable-string ,(lisp-string s)))

 (defmethod make-load-form ((s objc-constant-string) &optional env)
   (declare (ignore env))
   `(ns-constant-string ,(objc-constant-string-string s))))


(defmacro \@ (string)
  `(objc-constant-string-nsstringptr ,(ns-constant-string string)))



(defun objcl-definition-reader-macro (stream ch)
  (assert (eql #\@ ch))
  (if *read-suppress*
      (read stream t nil t)
      (let ((next-char (peek-char nil stream t nil t)))
        (case next-char
          ((#\[) (read-char stream t nil t) (read-objcl-definition stream))
          ((#\") (objc-constant-string-nsstringptr (ns-constant-string (read stream t nil t))))
          (otherwise (unread-char ch stream)
                     (let ((*readtable* *default-readtable*))
                       (read stream t nil t)))))))


(defparameter *objective-cl-readtable* *lisp-readtable*)

(defmacro set-objective-cl-syntax ()
  "Sets the *READTABLE* to *OBJECTIVE-CL-READTABLE*.
Must be a macro to be taken into account when compiling and loading."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *readtable* *objective-cl-readtable*)))

;;;; THE END ;;;;
