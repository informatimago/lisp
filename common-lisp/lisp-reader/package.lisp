;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements the Common Lisp package system.
;;;;    
;;;;    <Xach> The basic idea of that file is that the semantics of the CL
;;;;    package system can be implemented by an object with three special
;;;;    kinds of tables (present-table, shadowing-table, external-table)
;;;;    and two lists (used-packs, used-by-packs). The rest is
;;;;    implementation.
;;;;
;;;;AUTHORS
;;;;    <XACH> Zachary Beane <xach@xach.com>,
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-03 <PJB> Completed corrections to pass package ansi-tests.
;;;;    2012-03-30 <PJB> Added checks, made the API conforming to CL.
;;;;    2012-03-30 <PJB> Added this header; Removed "Z" prefix to CL
;;;;                     symbol names; shadowed and exported them.
;;;;BUGS
;;;;
;;;;    make-load-form for packages should probably return two forms, since
;;;;    packages can have circular dependencies.
;;;;
;;;;    Are missing some standard restarts to correct
;;;;    conflicts. (choosing one or the other symbol, doing the same
;;;;    for all conflicts, etc).
;;;;
;;;;LEGAL
;;;;    Copyright (c) 2012 Zachary Beane <xach@xach.com>, All Rights Reserved
;;;;    Copyright (c) 2012 Pascal Bourguignon <pjb@informatimago.com>, All Rights Reserved
;;;;
;;;;    Redistribution and use in source and binary forms, with or without
;;;;    modification, are permitted provided that the following conditions
;;;;    are met:
;;;;
;;;;      * Redistributions of source code must retain the above copyright
;;;;        notice, this list of conditions and the following disclaimer.
;;;;
;;;;      * Redistributions in binary form must reproduce the above
;;;;        copyright notice, this list of conditions and the following
;;;;        disclaimer in the documentation and/or other materials
;;;;        provided with the distribution.
;;;;
;;;;    THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;;    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;;    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;;    ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;;    DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;;    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;;    GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;;    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;;    WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;;    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;;    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;**************************************************************************


(pushnew :test-zpack *features*)

(cl:defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.PACKAGE"
  (:use "COMMON-LISP")
  (:nicknames "ZPACK")
  (:shadow "SIMPLE-TYPE-ERROR")
  (:shadow . #1=("SYMBOL"
                 "SYMBOLP" "MAKE-SYMBOL" "SYMBOL-NAME" "SYMBOL-PACKAGE" 
                 "SYMBOL-VALUE" "SYMBOL-FUNCTION" "SYMBOL-PLIST"
                 "BOUNDP" "FBOUNDP"
                 "KEYWORD" "KEYWORDP"
                 "PACKAGE"
                 "PACKAGEP"  "MAKE-PACKAGE" "FIND-PACKAGE" "DELETE-PACKAGE"
                 "FIND-SYMBOL" "IMPORT" "INTERN" "SHADOW" "SHADOWING-IMPORT"
                 "EXPORT" "UNEXPORT" "UNINTERN" "USE-PACKAGE"
                 "UNUSE-PACKAGE" "PACKAGE-NAME" "PACKAGE-NICKNAMES"
                 "PACKAGE-USE-LIST" "PACKAGE-USED-BY-LIST" "PACKAGE-SHADOWING-SYMBOLS"
                 "LIST-ALL-PACKAGES" "FIND-ALL-SYMBOLS" "RENAME-PACKAGE"
                 "*PACKAGE*"
                 "WITH-PACKAGE-ITERATOR"
                 "DO-SYMBOLS" "DO-EXTERNAL-SYMBOLS" "DO-ALL-SYMBOLS"
                 "DEFPACKAGE" "IN-PACKAGE" 
                 "PACKAGE-ERROR" "PACKAGE-ERROR-PACKAGE"))
  (:export . #1#)
  ;; Additionnal conditions:
  (:export "PACKAGE-EXISTS-ERROR"
           "PACKAGE-DOES-NOT-EXIST-ERROR"
           "SYMBOL-CONFLICT-ERROR"
           "SYMBOL-CONFLICT-EXISTING-SYMBOL"
           "SYMBOL-CONFLICT-IMPORTED-SYMBOL"
           "PACKAGE-DOCUMENTATION")
  (:documentation "
This package implements the Common Lisp package system.
Author: Zach Beane.
Modified by Pascal Bourguignon.
"))
(cl:in-package "ZPACK")

;;; Symbol internal management

(defgeneric sym-pack (sym))
(defgeneric (setf sym-pack) (pack sym))

(defgeneric make-constant (symbol value))

;;; Sym tables

(defgeneric make-sym-table ())
(defgeneric tget (sym-name table))
(defgeneric tput (sym table))
(defgeneric tremove (sym table))
(defgeneric tmember (sym table))
(defgeneric tmap-syms (fun table))
(defgeneric tmembers (table))

;;; Pack management

(defgeneric present-table (pack))
(defgeneric shadowing-table (pack))
(defgeneric external-table (pack))

(defgeneric accessiblep (sym pack))
(defgeneric externalp (sym pack))
(defgeneric shadowingp (sym pack))
(defgeneric presentp (sym pack))

(defgeneric check-import-conflict (sym pack))
(defgeneric check-inherit-conflict (used-pack using-pack))
(defgeneric check-export-conflict (sym pack))
(defgeneric check-unintern-conflict (sym-name pack))

(defgeneric zimport-without-checks (sym pack))
(defgeneric zunintern-without-checks (sym pack))

(defgeneric (setf used-packs) (used-packs pack))
(defgeneric (setf used-by-packs) (used-by-packs pack))

;;; Clone of the CL symbol/package interface

(defgeneric make-symbol (sym-name))
(defgeneric symbol-name (sym))
(defgeneric symbol-package (sym))

(defgeneric make-package (pack-name &key nicknames use))
(defgeneric find-package (pack-name))
(defgeneric delete-package (pack-name))

(defgeneric find-symbol (sym-name &optional pack))
(defgeneric import (sym &optional pack))
(defgeneric intern (sym-name &optional pack))
(defgeneric shadow (sym-name &optional pack))
(defgeneric shadowing-import (sym &optional pack))
(defgeneric export (sym &optional pack))

(defgeneric unexport (sym &optional pack))
(defgeneric unintern (sym &optional pack))

(defgeneric use-package (pack &optional using-pack))
(defgeneric unuse-package (pack &optional using-pack))

(defgeneric package-name (pack))
(defgeneric package-use-list (pack))
(defgeneric package-used-by-list (pack))
(defgeneric package-shadowing-symbols (pack))

(defgeneric find-all-symbols (name))
(defgeneric rename-package (package new-name &optional new-nicknames))



;;; Variables

(defparameter *keyword-package*           nil) 
(defparameter *common-lisp-package*       nil)
(defparameter *common-lisp-user-package*  nil)
(defvar *package*)



;;; Conditions

(define-condition simple-error-mixin (condition)
  ((format-control   :initarg :format-control   :reader format-control
                     :initform "Simple error.")
   (format-arguments :initarg :format-arguments :reader format-arguments
                     :initform '()))
  (:report (lambda (condition stream)
             (format stream "~?"
                     (format-control condition)
                     (format-arguments condition)))))

(define-condition simple-program-error (simple-error-mixin program-error)
  ())

(define-condition simple-type-error (simple-error-mixin type-error)
  ())



(define-condition package-error (error)
  ((package :initarg :package :reader package-error-package))
  (:report (lambda (condition stream)
             (format stream "Package error with ~A" (package-error-package condition)))))


(define-condition simple-package-error (package-error simple-error-mixin)
  ())

(define-condition package-exists-error (simple-package-error)
  ())

(define-condition package-does-not-exist-error (simple-package-error)
  ())

(define-condition symbol-conflict-error (simple-package-error)
  ((existing-symbol :initarg :existing-symbol :reader symbol-conflict-existing-symbol)
   (imported-symbol :initarg :imported-symbol :reader symbol-conflict-imported-symbol))
  (:report (lambda (condition stream)
             (format stream "The would-be imported symbol ~S conflicts with the existing symbol ~S in the package ~S"
                     (symbol-conflict-imported-symbol condition)
                     (symbol-conflict-existing-symbol condition)
                     (package-name (package-error-package condition))))))

(define-condition symbol-does-not-exist-error (simple-package-error)
  ((symbol-name :initarg :symbol-name :reader symbol-does-not-exist-symbol-name))
  (:report (lambda (condition stream)
             (format stream "There exists no symbol named ~S in the package ~S"
                     (symbol-does-not-exist-symbol-name condition)
                     (package-name (package-error-package condition))))))

(define-condition symbol-inaccessible-error (simple-package-error)
  ((symbol :initarg :symbol :reader symbol-inaccessible-symbol))
  (:report (lambda (condition stream)
             (format stream "~S is not accessible in ~S"
                     (symbol-inaccessible-symbol condition)
                     (package-name (package-error-package condition))))))



(defun query-string ()
  (format *query-io* "Enter a new string: ")
  (finish-output *query-io*)
  (list (read-line *query-io*)))

(defun query-symbol ()
  (list (loop
          :for sym = (progn
                       (format *query-io* "Enter a new symbol (current package is ~A): "
                               (package-name *package*))
                       (finish-output *query-io*)
                       (read *query-io*))
          :until (symbolp sym)
          :finally (return sym))))

(defun query-package-name ()
  (format *query-io* "Enter a package name (string or symbol): ")
  (finish-output *query-io*)
  (list (read *query-io*)))

(defun query-package-nickname ()
  (format *query-io* "Enter a package nickname (string or symbol): ")
  (finish-output *query-io*)
  (list (read *query-io*)))



(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))


;;; Implementation of syms

(defclass symbol ()
  ((name
    :initarg :name
    :reader symbol-name)
   (pack
    :initarg :pack
    :reader symbol-package
    :accessor sym-pack)
   (plist
    :initarg :plisp
    :initform nil
    :accessor symbol-plist)
   (value
    :initarg :value
    :accessor symbol-value)
   (function
    :initarg :function
    :accessor symbol-function)
   (constantp
    :initarg :constantp
    :initform nil
    :accessor symbol-constantp))
  (:default-initargs
   :pack nil))

(defmethod symbolp ((object t))      nil)
(defmethod symbolp ((object symbol)) t)

(defmethod boundp ((object t))
  (error 'type-error :datum object :expected-type 'symbol))
(defmethod boundp ((object symbol))
  (slot-boundp object 'value))

(defmethod fboundp ((object t))
  (error 'type-error :datum object :expected-type 'symbol))
(defmethod fboundp ((object symbol))
  (slot-boundp object 'function))


(defclass keyword (symbol)
  ())

(defmethod keywordp ((object t))       nil)
(defmethod keywordp ((object keyword)) t)


(defmethod make-symbol (sym-name)
  (make-instance 'symbol :name (copy-seq sym-name)))

(defmethod make-load-form ((sym symbol) &optional environment)
  (declare (ignore environment))
  `(intern ,(symbol-name sym) ,(package-name (symbol-package sym))))


(defun constituentp (ch first-character-p &optional (readtable *readtable*))
  (multiple-value-bind (macro-character-p non-terminating-p) (get-macro-character ch readtable)
    (or (not macro-character-p)
        (and (not first-character-p)
             non-terminating-p))))

(defun specialp (ch &optional (readtable *readtable*))
  (declare (ignore readtable))
  (find ch #(#\Space #\: #\| #\\
             #\Newline #\Tab #\Linefeed #\Return #\Page)))

(defun parses-as-a-number-p (string &key (start 0) (end nil) (base *read-base*))
  ;; integer  ::= [sign] digit+
  ;; integer  ::= [sign] decimal-digit+ decimal-point 
  ;; ratio    ::= [sign] {decimal-digit}+ '/' {decimal-digit}+
  ;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ exponent
  ;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ 
  ;; float    ::= [sign] {decimal-digit}+ exponent
  ;; float    ::= [sign] {decimal-digit}+ decimal-point {decimal-digit}* exponent
  ;; exponent ::=  exponent-marker [sign] {digit}+
  ;; We may ignore ratio starting with #\# since that's excluded by constituentp.
  ;; ratio    ::= [#b|#o|#x|#{decimal-digit}+r] [sign] digit+ '/' digit+
  (loop
    :with end =  (or end (length string))
    :with i = start
    :with state = :opt-sign
    :for ch = (and (< i end) (aref string i))
    :while (< i end)
    :do (ecase state
          (:opt-sign (case ch ((#\+ #\-) (incf i)))
                     (setf state :unknown0))
          (:unknown0  (if (<= base 10)
                          (cond
                            ((digit-char-p ch base) (incf i) (setf state :unknown1))
                            ((digit-char-p ch 10)   (incf i) (setf state :decimal))
                            (t (case ch
                                 ((#\.) (incf i) (setf state :float0))
                                 (otherwise (return nil)))))
                          (cond
                            ((digit-char-p ch 10)   (incf i) (setf state :unknown1))
                            ((digit-char-p ch base) (incf i) (setf state :integer))
                            (t (case ch
                                 ((#\.) (incf i) (setf state :float0))
                                 (otherwise (return nil)))))))
          (:unknown1  (if (<= base 10)
                          (cond
                            ((digit-char-p ch base) (incf i) (setf state :unknown1))
                            ((digit-char-p ch 10)   (incf i) (setf state :decimal))
                            (t (case ch
                                 ((#\/) (incf i) (setf state :ratio0))
                                 ((#\.) (incf i) (setf state :dot))
                                 ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                                  (incf i) (setf state :float-expo/opt-sign))
                                 (otherwise (return nil)))))
                          (cond
                            ((digit-char-p ch 10)   (incf i) (setf state :unknown1))
                            ((digit-char-p ch base) (incf i) (setf state :integer))
                            (t (case ch
                                 ((#\/) (incf i) (setf state :ratio0))
                                 ((#\.) (incf i) (setf state :dot))
                                 ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                                  (incf i) (setf state :float-expo/opt-sign))
                                 (otherwise (return nil)))))))
          (:integer   (if (digit-char-p ch base)
                          (incf i)
                          (return nil)))
          (:decimal   (if (digit-char-p ch 10)
                          (incf i)
                          (case ch
                            ((#\/) (incf i) (setf state :ratio0))
                            ((#\.) (incf i) (setf state :dot))
                            ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                             (incf i) (setf state :float-expo/opt-sign))
                            (otherwise (return nil)))))
          (:dot      (if (digit-char-p ch 10)
                         (progn (incf i) (setf state :float))
                         (case ch
                           ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                            (incf i) (setf state :float-expo/opt-sign))
                           (otherwise (return nil)))))
          (:ratio0   (if (digit-char-p ch 10)
                         (progn (incf i) (setf state :ratio))
                         (return nil)))
          (:ratio    (if (digit-char-p ch 10)
                         (incf i)
                         (return nil)))
          (:float0   (if (digit-char-p ch 10)
                         (progn (incf i) (setf state :float))
                         (return nil)))
          (:float    (if (digit-char-p ch 10)
                         (incf i)
                         (case ch
                           ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                            (incf i) (setf state :float-expo/opt-sign))
                           (otherwise (return nil)))))
          (:float-expo/opt-sign (case ch ((#\+ #\-) (incf i)))
                                (setf state :float-expo0))
          (:float-expo0 (if (digit-char-p ch 10)
                            (progn (incf i) (setf state :float-expo))
                            (return nil)))
          (:float-expo  (if (digit-char-p ch 10)
                            (incf i)
                            (return nil))))
    :finally (return (case state
                       ((:unknown1 :integer :dot :ratio :float :float-expo) t)
                       (otherwise nil)))))


(defun needs-escape-p (symbol-name)
  "Whether the symbol name needs to be escaped."
  (cond
    ((string= "" symbol-name) t)
    ((or *print-readably* *print-escape*)
     (or (notevery (let ((first-character-p t))
                     (lambda (ch)
                       (prog1 (and (not (specialp ch))
                                   (constituentp ch first-character-p))
                         (setf first-character-p nil))))
                   symbol-name)
         ;; Parses as a number integer, decimal, ratio or float.
         (parses-as-a-number-p symbol-name :base *print-base*)))
    (t
     nil)))

(defun mixed-case-p (string)
  "Whether the string contains both lower case and upper case letters."
  (and (some (lambda (ch) (and (alpha-char-p ch) (upper-case-p ch))) string)
       (some (lambda (ch) (and (alpha-char-p ch) (lower-case-p ch))) string)))

(defun prepare-symbol-name (sname)
  (cond
    ((needs-escape-p sname)
     (with-output-to-string (*standard-output*)
       (loop
         :for ch :across sname
         :initially (princ "|")
         :do (if (char= #\| ch) (princ "\\|") (princ ch))
         :finally (princ "|"))))
    (t
     (let ((transform 
            (if *print-escape*
                (ecase (readtable-case *readtable*)
                  (:upcase     (lambda (ch)
                                 (if (both-case-p ch)
                                     (if (lower-case-p ch)
                                         (format nil "\\~C" ch)
                                         ch)
                                     ch)))
                  (:downcase   (lambda (ch)
                                 (if (both-case-p ch)
                                     (if (upper-case-p ch)
                                         (format nil "\\~C" ch)
                                         ch))))
                  (:preserve   (function identity))
                  (:invert     (function identity)))   
                (ecase (readtable-case *readtable*)
                  (:upcase     (let ((start-word t))
                                 (lambda (ch)
                                   (prog1 (if (both-case-p ch)
                                              (if (upper-case-p ch)
                                                  (ecase *print-case*
                                                    (:upcase     ch)
                                                    (:downcase   (char-downcase ch))
                                                    (:capitalize (if start-word
                                                                     (char-upcase ch)
                                                                     (char-downcase ch))))
                                                  ch)
                                              ch)
                                     (if (alphanumericp ch)
                                         (setf start-word nil)
                                         (setf start-word t))))))
                  (:downcase   (let ((start-word t))
                                 (lambda (ch)
                                   (prog1 (if (both-case-p ch)
                                              (if (lower-case-p ch)
                                                  (ecase *print-case*
                                                    (:upcase     (char-upcase ch))
                                                    (:downcase   ch)
                                                    (:capitalize (if start-word
                                                                     (char-upcase ch)
                                                                     (char-downcase ch))))
                                                  ch)
                                              ch)
                                     (if (alphanumericp ch)
                                         (setf start-word nil)
                                         (setf start-word t))))))
                  (:preserve   (function identity))
                  (:invert     (if (mixed-case-p sname)
                                   (function identity)
                                   (lambda (ch)
                                     (cond
                                       ((not (both-case-p ch)) ch)
                                       ((upper-case-p ch)      (char-downcase ch))
                                       ((lower-case-p ch)      (char-upcase ch))
                                       (t                      ch)))))))))
       (with-output-to-string (*standard-output*)
         (loop
           :for ch :across sname
           :do (princ (funcall transform ch))))))))


(defmethod print-object ((sym symbol) stream)
  (let ((*print-readably* t))
    (flet ((print-it ()
             (let ((pack (symbol-package sym)))
               (cond ((null pack)
                      (format stream "~:[~;#:~]~A"
                              (or *print-readably* (and *print-escape* *print-gensym*))
                              (prepare-symbol-name (symbol-name sym))))
                     ((eql pack *keyword-package*)
                      (format stream ":~A"
                              (prepare-symbol-name (symbol-name sym))))
                     ((or (eq pack *package*)
                          (eq sym (find-symbol (symbol-name sym) *package*)))
                      (format stream "~A" (prepare-symbol-name (symbol-name sym))))
                     (t
                      (format stream "~A~:[::~;:~]~A"
                              (prepare-symbol-name (package-name pack))
                              (externalp sym pack)
                              (prepare-symbol-name (symbol-name sym))))))))
      (if *print-readably*
          (print-it)
          (progn
            (format stream "#<~S " 'symbol)
            (print-it)
            (format stream ">")))))
  sym)


(defmethod make-constant (symbol value)
  (declare (ignorable value))
  (setf (symbol-value symbol) value
        (symbol-constantp symbol) t)
  symbol)



;;; Implementation of sym-tables

(defclass sym-table ()
  ((name-table
    :initarg :name-table
    :reader name-table))
  (:default-initargs
   :name-table (make-hash-table :test 'equal)))

(defmethod make-sym-table ()
  (make-instance 'sym-table))

(defmethod tget (sym-name table)
  (values (gethash sym-name (name-table table))))

(defmethod tmember (sym table)
  (let ((entry (tget (symbol-name sym) table)))
    (eq entry sym)))

(defmethod tput (sym table)
  (setf (gethash (symbol-name sym) (name-table table)) sym))

(defmethod tremove (sym table)
  (remhash (symbol-name sym) (name-table table)))

(defmethod tmap-syms (fun table)
  (maphash (lambda (sym-name sym)
             (declare (ignore sym-name))
             (funcall fun sym))
           (name-table table)))

(defmethod tmembers (table)
  (let ((members '()))
    (tmap-syms (lambda (sym)
                 (push sym members))
               table)
    members))


;;; Implementation of packs & CL clone interface

(defparameter *packs* (make-hash-table :test 'equal))


(defun list-all-packages ()
  (let ((packages '()))
    (maphash (lambda (k v) (declare (ignore k)) (pushnew v packages)) *packs*)
    packages))


(defclass package ()
  ((name
    :initarg :name
    :reader package-name
    :writer (setf name))
   (external-table
    :initarg :external-table
    :reader external-table)
   (present-table
    :initarg :present-table
    :reader present-table)
   (shadowing-table
    :initarg :shadowing-table
    :reader shadowing-table)
   (used-packs
    :initarg :used-packs
    :reader package-use-list
    :writer (setf used-packs))
   (used-by-packs
    :initarg :used-by-packs
    :reader package-used-by-list
    :writer (setf used-by-packs))
   (nicknames
    :initarg :nicknames
    :reader package-nicknames
    :writer (setf nicknames))
   (documentation
    :initarg :documentation
    :initform nil
    :accessor package-documentation))
  (:default-initargs
   :name (error "A package name is required")
    :external-table (make-sym-table)
    :present-table (make-sym-table)
    :shadowing-table (make-sym-table)
    :used-packs nil
    :used-by-packs nil))

(defmacro define-normalize-package-methods (name &key (if-package-does-not-exist :replace) (type-error nil))
  `(progn
     ,@ (when type-error
          `((defmethod ,name ((name t))  (error 'simple-type-error
                                                :datum name
                                                :expected-type 'package-designator
                                                :format-control "~S called with a non ~S: ~S"
                                                :format-arguments (list ',name 'package-designator name)))))
     (defmethod ,name ((name string))    (,name (normalize-package-designator name :if-package-does-not-exist ,if-package-does-not-exist)))
     (defmethod ,name ((name character)) (,name (normalize-package-designator name :if-package-does-not-exist ,if-package-does-not-exist)))
     (defmethod ,name ((name cl:symbol)) (,name (normalize-package-designator name :if-package-does-not-exist ,if-package-does-not-exist)))
     (defmethod ,name ((name symbol))    (,name (normalize-package-designator (symbol-name name) :if-package-does-not-exist ,if-package-does-not-exist)))))

(define-normalize-package-methods package-name            :type-error t)
(define-normalize-package-methods package-use-list        :type-error t)
(define-normalize-package-methods package-used-by-list    :type-error t)
(define-normalize-package-methods package-nicknames       :type-error t)
(define-normalize-package-methods package-shadowing-symbols)



(defgeneric packagep (package)
  (:method ((object t)) nil)
  (:method ((package package)) t))


(defmethod print-object ((pack package) stream)
  (if *print-readably*
      (error 'print-not-readable :object pack)
      (format stream "#<~S ~S>" 'package (package-name pack)))
  pack)

(defmethod package-shadowing-symbols (pack)
  (tmembers (shadowing-table pack)))


(defmethod accessiblep (sym pack)
  (let ((existing-sym (find-symbol (symbol-name sym) pack)))
    (eq existing-sym sym)))

(defmethod externalp (sym pack)
  (tmember sym (external-table pack)))

(defmethod shadowingp (sym pack)
  (tmember sym (shadowing-table pack)))

(defmethod presentp (sym pack)
  (tmember sym (present-table pack)))








(deftype string-designator ()
  '(or string character symbol cl:symbol))


(defun normalize-string-designator (object &key (if-not-a-string-designator :error))
  (check-type if-not-a-string-designator (member nil :error :ignore :replace :ignore-or-replace))
  (typecase object
    (string     object)
    (character  (string object))
    (cl:symbol  (string object))
    (symbol     (symbol-name object))
    (otherwise
     (case if-not-a-string-designator
       ((:error) (error 'type-error
                        :datum object
                        :expected-type 'string-designator))
       ((nil)    nil)
       ((:ignore :replace :ignore-or-replace)
        (restart-case (error 'type-error
                             :datum object
                             :expected-type 'string-designator)
          (ignore ()
            :test (lambda (condition)
                    (declare (ignore condition))
                    (member if-not-a-string-designator '(:ignore :ignore-or-replace)))
            :report "Ignore it."           
            nil)
          (read-a-new-string-designator (new-string)
            :test (lambda (condition)
                    (declare (ignore condition))
                    (member if-not-a-string-designator '(:replace :ignore-or-replace)))
            :interactive query-string
            :report "Enter a string"           
            (normalize-string-designator
             new-string
             :if-not-a-string-designator if-not-a-string-designator))))))))


(defun normalize-weak-designator-of-list-of-string-designator (object)
  (mapcan (lambda (nickname)
            (ensure-list (normalize-string-designator
                          nickname
                          :if-not-a-string-designator :ignore-or-replace)))
          (ensure-list object)))



(deftype package-designator ()
  '(or package string-designator))


(defun normalize-package-designator (object &key
                                            (if-package-does-not-exist :string)
                                            (if-package-exists :package)
                                            (if-not-a-package-designator :error))
  "
Normalize the given PACKAGE-DESIGNATOR.  Objects of type
PACKAGE-DESIGNATOR are either PACKAGE or objects of type
STRING-DESIGNATOR.

RETURN: either NIL, a STRING designating a non-existent package, or an
        existing PACKAGE.


IF-NOT-A-PACKAGE-DESIGNATOR The default is :ERROR.

    NIL                     If the OBJECT is not a PACKAGE-DESIGNATOR
                            then return NIL.

    :ERROR                  If the OBJECT is not a PACKAGE-DESIGNATOR
                            then signal a TYPE-ERROR.

    :IGNORE                 If the OBJECT is not a PACKAGE-DESIGNATOR
                            then signal a TYPE-ERROR, with an IGNORE
                            restart that when chosen returns NIL.

    :REPLACE                If the OBJECT is not a PACKAGE-DESIGNATOR
                            then signal a TYPE-ERROR, with a replace
                            restart that when chosen let the user
                            input another PACKAGE-DESIGNATOR.

    :IGNORE-OR-REPLACE      If the OBJECT is not a PACKAGE-DESIGNATOR
                            then signal a TYPE-ERROR, with the two
                            previous restarts.

If the object is a PACKAGE-DESIGNATOR, then the results depends on the
following parameters and whether the designated package exists or not.


IF-PACKAGE-DOES-NOT-EXIST   The default is :STRING

    NIL                     If the OBJECT designates a PACKAGE that
                            doesn't exist then return NIL.

    :STRING                 If the OBJECT designates a PACKAGE that
                            doesn't exist then (it would be a
                            STRING-DESIGNATOR) return the designated
                            STRING.

    :ERROR                  If the OBJECT designates a PACKAGE that
                            doesn't exist then signal a
                            PACKAGE-DOES-NOT-EXIST-ERROR.

    :IGNORE                 If the OBJECT designates a PACKAGE that
                            doesn't exist then signal a
                            PACKAGE-DOES-NOT-EXIST-ERROR with an
                            IGNORE restart that when chosen returns
                            NIL.

    :REPLACE                If the OBJECT designates a PACKAGE that
                            doesn't exist then signal a
                            PACKAGE-DOES-NOT-EXIST-ERROR with a
                            replace restart that when chosen let the
                            user input another PACKAGE-DESIGNATOR.

    :IGNORE-OR-REPLACE      If the OBJECT designates a PACKAGE that
                            doesn't exist then signal a
                            PACKAGE-DOES-NOT-EXIST-ERROR with the two
                            previous restarts.


IF-PACKAGE-EXISTS           The default is :PACKAGE

    :PACKAGE                If the OBJECT designates a PACKAGE that
                            does exist then return the designated
                            PACKAGE.

    :STRING                 If the OBJECT designates a PACKAGE that
                            does exist then return the designated
                            package name.

    :ERROR                  If the OBJECT designates a PACKAGE that
                            does exist then signal a
                            PACKAGE-EXISTS-ERROR.

    :IGNORE                 If the OBJECT designates a PACKAGE that
                            does exist then signal a
                            PACKAGE-EXISTS-ERROR with an IGNORE
                            restart that when chosen returns NIL.

    :REPLACE                If the OBJECT designates a PACKAGE that
                            does exist then signal a
                            PACKAGE-EXISTS-ERROR with a replace
                            restart that when chosen let the user
                            input another PACKAGE-DESIGNATOR.

    :IGNORE-OR-REPLACE      If the OBJECT designates a PACKAGE that
                            does exist then signal a
                            PACKAGE-EXISTS-ERROR with the two previous
                            restarts.

"
  (check-type if-not-a-package-designator (member :error :ignore :replace :ignore-or-replace         nil))
  (check-type if-package-does-not-exist   (member :error :ignore :replace :ignore-or-replace :string nil))
  (check-type if-package-exists           (member :error :ignore :replace :ignore-or-replace :string :package))

  (flet ((retry-string-designator (restarts condition &rest arguments)
           (check-type restarts (member :ignore :replace :ignore-or-replace))
           (restart-case (apply (function error) condition arguments)
             (ignore ()
               :test (lambda (condition)
                       (declare (ignore condition))
                       (member restarts '(:ignore :ignore-or-replace)))
               :report "Ignore it."           
               nil)
             (read-a-new-package-designator (new-package)
               :test (lambda (condition)
                       (declare (ignore condition))
                       (member restarts '(:replace :ignore-or-replace)))
               :interactive query-package-name
               :report "Enter a package name"           
               (normalize-package-designator
                new-package
                :if-not-a-package-designator if-not-a-package-designator
                :if-package-does-not-exist if-package-does-not-exist
                :if-package-exists if-package-exists))))
         (retry-package-designator (restarts condition &rest arguments)
           (check-type restarts (member :ignore :replace :ignore-or-replace))
           (restart-case (apply (function error) condition arguments)
             (ignore ()
               :test (lambda (condition)
                       (declare (ignore condition))
                       (member restarts '(:ignore :ignore-or-replace)))
               :report "Ignore it."           
               nil)
             (read-a-new-package-designator (new-package)
               :test (lambda (condition)
                       (declare (ignore condition))
                       (member restarts '(:replace :ignore-or-replace)))
               :interactive query-package-name
               :report "Enter a package name"           
               (normalize-package-designator
                new-package
                :if-not-a-package-designator if-not-a-package-designator
                :if-package-does-not-exist if-package-does-not-exist
                :if-package-exists if-package-exists)))))
    
    (typecase object

      (string-designator
       (let* ((normalized  (normalize-string-designator object))
              (package     (find-package normalized)))
         (if package
             (normalize-package-designator package :if-package-exists if-package-exists)
             (case if-package-does-not-exist
               ((nil)         nil)
               ((:string)     normalized)
               ((:error)      (error
                               'package-does-not-exist-error
                               :package normalized
                               :format-control "There is no package named ~S"
                               :format-arguments (list normalized)))
               ((:ignore :replace :ignore-or-replace)
                (retry-package-designator if-package-does-not-exist
                                          'package-does-not-exist-error
                                          :package normalized
                                          :format-control "There is no package named ~S"
                                          :format-arguments (list normalized)))))))
      
      (package
       (case if-package-exists
         ((:package) object)
         ((:string)  (package-name object))
         ((:error)   (error
                      'package-exists-error
                      :package object
                      :format-control "There is already a package named ~S"
                      :format-arguments (list (package-name object))))
         ((:ignore :replace :ignore-or-replace)
          (retry-package-designator if-package-exists
                                    'package-exists-error
                                    :package object
                                    :format-control "There is already a package named ~S"
                                    :format-arguments (list (package-name object))))))
      
      (otherwise
       (case if-not-a-package-designator
         ((nil)     nil)
         ((:error)  (error 'type-error
                           :datum object
                           :expected-type 'package-designator))
         ((:ignore :replace :ignore-or-replace)
          (retry-string-designator if-not-a-package-designator
                                   'type-error
                                   :datum object
                                   :expected-type 'package-designator)))))))




(defun make-package-iterator (packages symbol-types)
  (let ((packages (mapcan (lambda (package-designator)
                            (list (normalize-package-designator
                                   package-designator :if-package-does-not-exist :ignore-or-replace)))
                          (ensure-list packages)))
        (package  nil)
        (stypes   nil)
        (stype    nil)
        (symbols  '()))
    (labels ((iterator ()
               (cond
                 (symbols    (let ((sym (pop symbols)))
                               (values t
                                       sym
                                       (cond
                                         ((externalp sym package) :external)
                                         ((eq stype :inherited)   stype)
                                         (t                       :internal))
                                       package)))
                 (stypes     (setf stype (pop stypes))
                             (ecase stype
                               ((:internal)
                                (tmap-syms (lambda (sym)
                                             (unless (externalp sym package)
                                               (push sym symbols)))
                                           (present-table package)))
                               ((:external)
                                (tmap-syms (lambda (sym) (push sym symbols))
                                           (external-table package)))
                               ((:inherited)
                                (dolist (pack (package-use-list package))
                                  (tmap-syms (lambda (sym)
                                               (let ((shadow (find-symbol (symbol-name sym) package)))
                                                (unless (and shadow
                                                             (shadowingp shadow package)
                                                             (not (eq sym shadow)))
                                                  (push sym symbols))))
                                             (external-table (find-package pack)))))
                               ((:present)
                                (tmap-syms (lambda (sym) (push sym symbols))
                                           (present-table package)))
                               ((:shadowing)
                                (tmap-syms (lambda (sym) (push sym symbols))
                                           (shadowing-table package))))
                             (iterator))
                 (packages   (setf package (pop packages)
                                   stypes  symbol-types)
                             (iterator))
                 (t          nil))))
      (function iterator))))


(defmacro with-package-iterator ((name package-list-form &rest symbol-types)
                                 &body declarations-body)
  (flet ((valid-symbol-type-p (object)
           (member object '(:internal :external :inherited
                            ;; extensions:
                            :present :shadowing))))
    (cond
      ((null symbol-types) (error 'simple-program-error
                                  :format-control "Missing at least one symbol-type"))
      ((every (function valid-symbol-type-p) symbol-types))
      (t (error 'simple-program-error
                :format-control "Invalid symbol-type: ~S"
                :format-arguments (list (find-if-not (function valid-symbol-type-p) symbol-types))))))
  (let ((viterator (gensym "ITERATOR")))
    `(let ((,viterator (make-package-iterator ,package-list-form ',symbol-types)))
       (macrolet ((,name () '(funcall ,viterator)))
         ,@declarations-body))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun declarations (body)
    (loop
      :for item :in body
      :while (and (listp item) (eql 'declare (car item)))
      :collect item))
  
  (defun body (body)
    (loop
      :for items :on body
      :for item = (car items)
      :while (and (listp item) (eql 'declare (car item)))
      :finally (return items)))

  (assert (equal (mapcar (lambda (body) (list (declarations body) (body body)))
                         '(()
                           ((declare (ignore x)))
                           ((declare (ignore x)) (declare (ignore y)))
                           ((print w) (print z))
                           ((declare (ignore x)) (print w) (print z))
                           ((declare (ignore x)) (declare (ignore y)) (print w) (print z))))
                 '((nil nil)
                   (((declare (ignore x))) nil)
                   (((declare (ignore x)) (declare (ignore y))) nil)
                   (nil ((print w) (print z)))
                   (((declare (ignore x))) ((print w) (print z)))
                   (((declare (ignore x)) (declare (ignore y))) ((print w) (print z))))))

  
  (defun generate-do-symbols-loop (var package result-form body symbol-types)
    (let ((iter   (gensym "ITERATOR"))
          (got-it (gensym "GOT-IT"))
          (symbol (gensym "SYMBOL"))
          (vpack  (gensym "PACKAGE")))
      `(let ((,vpack (or ,package *package*)))
         (with-package-iterator (,iter ,vpack ,@symbol-types)
           (let (,var)
             ,@(declarations body)
             (loop
               (multiple-value-bind (,got-it ,symbol) (,iter)
                 (if ,got-it
                     (tagbody
                        (setf ,var ,symbol)
                        ,@(body body))
                     (progn
                       (setf ,var nil)
                       (return ,result-form)))))))))))



(defmacro do-symbols         ((var &optional package result-form) &body body)
  (generate-do-symbols-loop var package result-form body '(:internal :external :inherited)))


(defmacro do-external-symbols ((var &optional package result-form) &body body)
  (generate-do-symbols-loop var package result-form body '(:external)))


(defmacro do-all-symbols      ((var &optional result-form) &body body)
  (generate-do-symbols-loop var '(list-all-packages) result-form body '(:internal :external :inherited)))



(defmethod check-import-conflict (sym pack)
  (let ((existing-sym (find-symbol (symbol-name sym) pack)))
    (if (and existing-sym (not (eq existing-sym sym)))
        (restart-case (error 'symbol-conflict-error
                             :package pack
                             :format-control "Conflict: importing ~A into ~A conflicts with ~A"
                             :format-arguments (list sym pack existing-sym)
                             :existing-symbol existing-sym
                             :imported-symbol sym)
          (enter-new-name (new-symbol)
            :interactive query-symbol
            :report "Enter a new symbol, instead" 
            (check-import-conflict new-symbol pack))
          (ignore-symbol ()
            :report (lambda (stream) (format stream "Ignore the symbol ~S" sym))
            (values nil nil)))
        (values sym t))))


(defmethod check-inherit-conflict (used-pack using-pack)
  (do-external-symbols (inherited-sym used-pack)
    (let ((existing-sym (find-symbol (symbol-name inherited-sym)
                                     using-pack)))
      (when (and existing-sym
                 (not (eq inherited-sym existing-sym))
                 (not (shadowingp existing-sym using-pack)))
        (error "Conflict: Inheriting ~A from ~A conflicts with ~A in ~A"
               inherited-sym
               used-pack
               existing-sym
               using-pack)))))

(defmethod check-export-conflict (sym pack)
  (let ((sym-name (symbol-name sym)))
    (dolist (using-pack (package-used-by-list pack))
      (let ((existing-sym (find-symbol sym-name using-pack)))
        (when (and existing-sym
                   (not (member existing-sym (package-shadowing-symbols using-pack))))
          (unless (eq existing-sym sym)
            (error "Conflict: exporting ~A conflicts with ~A in ~A"
                   sym existing-sym using-pack)))))))

(defmethod check-unintern-conflict (sym pack)
  (let ((sym-name (symbol-name sym))
        (first-existing-sym nil))
    (dolist (used-pack (package-use-list pack))
      (let ((existing-sym (find-symbol sym-name used-pack)))
        (when existing-sym
         (if first-existing-sym
             (unless (eq existing-sym first-existing-sym)
               (error "Conflict: uninterning ~A would lead to conflict ~
                      between ~A and ~A"
                      sym first-existing-sym existing-sym))
             (setf first-existing-sym existing-sym)))))))


(defmethod zimport-without-checks (sym pack)
  (tput sym (present-table pack))
  (unless (symbol-package sym)
    (setf (sym-pack sym) pack)))

(defmethod zunintern-without-checks (sym pack)
  (tremove sym (external-table pack))
  (tremove sym (shadowing-table pack))
  (tremove sym (present-table pack))
  (when (eq (symbol-package sym) pack)
    (setf (sym-pack sym) nil)))



(defun check-new-names (pack-name nicknames &key renaming-package)
  (loop
    :with result = '()
    :for name :in (cons pack-name nicknames)
    :do (loop
          :for pack = (find-package name)
          :while (if renaming-package
                     (and pack (not (eq pack renaming-package)))
                     pack)
          :do (restart-case (error 'package-exists-error
                                   :package name
                                   :format-control "A package named ~S already exists"
                                   :format-arguments (list name))
                (enter-new-name (new-name)
                  :test (lambda (condition) (declare (ignore condition)) (eq name pack-name))
                  :interactive query-package-name
                  :report "Enter a new package name, instead"           
                  (setf name new-name))
                (enter-new-name (new-name)
                  :test  (lambda (condition) (declare (ignore condition)) (not (eq name pack-name)))
                  :report "Enter a new package nickname, instead"           
                  :interactive query-package-nickname
                  (setf name new-name))
                (ignore-nickname ()
                  :test (lambda (condition) (declare (ignore condition)) (not (eq name pack-name)))
                  :report (lambda (stream) (format stream "Ignore the nickname ~S" name))
                  (return)))
          :finally (push name result))
    :finally (let ((result (nreverse result)))
               (return (values (car result) (cdr result))))))


(defmethod make-package (pack-name &key (nicknames '()) (use '()))
  (let ((pack-name (normalize-string-designator pack-name :if-not-a-string-designator :replace))
        (nicknames (normalize-weak-designator-of-list-of-string-designator nicknames))
        (use       (mapcan (lambda (package-designator)
                             (list (normalize-package-designator
                                    package-designator :if-package-does-not-exist :ignore-or-replace)))
                           use)))
    (multiple-value-setq (pack-name nicknames) (check-new-names pack-name nicknames))
    (let ((package (make-instance 'package
                       :name (copy-seq pack-name)
                       :nicknames (mapcar (function copy-seq) nicknames))))
      (dolist (upack use)
        (use-package upack package))
      (dolist (name (cons pack-name nicknames) package)
        (setf (gethash name *packs*) package)))))


(defmethod find-package (pack-name)
  (etypecase pack-name
    (string-designator
     (values (gethash (normalize-string-designator pack-name) *packs*)))
    (package pack-name)))


(defmethod delete-package (pack)
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :replace)))
    (when (and pack (package-name pack))
      (dolist (used (package-used-by-list pack))
        (unuse-package pack used))
      (dolist (puse (package-use-list pack))
        (unuse-package puse pack))
      (do-symbols (sym pack)
        (when (eq (symbol-package sym) pack)
          (zunintern-without-checks sym pack)))
      (dolist (name (cons (package-name pack) (package-nicknames pack)))
        (remhash name *packs*))
      (setf (name pack) nil)
      pack)))



(defmethod find-symbol (sym-name &optional (pack *package*))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :replace))
        sym)
    (cond ((setf sym (tget sym-name (external-table pack)))
           (values sym :external))
          ((setf sym (tget sym-name (shadowing-table pack)))
           (values sym :internal))
          ((setf sym (some (lambda (used-pack)
                             (tget sym-name (external-table used-pack)))
                           (package-use-list pack)))
           (values sym :inherited))
          ((setf sym (tget sym-name (present-table pack)))
           (values sym :internal))
          (t
           (values nil nil)))))



(defmethod import (symbols &optional (pack *package*))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-import (sym)
             (check-type sym symbol)
             (multiple-value-bind (sym good) (check-import-conflict sym pack)
               (when (and good (not (presentp sym pack)))
                 (if (and (null (symbol-package sym))
                          (eql pack *keyword-package*))
                     (progn
                       (zimport-without-checks sym pack)
                       (change-class sym 'keyword)
                       (make-constant sym sym)
                       (export sym pack))
                     (zimport-without-checks sym pack))))))
      (mapc (function do-import) (ensure-list symbols)))
    t))


(defmethod intern (sym-name &optional (pack *package*))
  (check-type sym-name string)
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (multiple-value-bind (sym status) (find-symbol sym-name pack)
      (if status
          (values sym status)
          (values (let ((sym (make-symbol sym-name)))
                    (import sym pack)
                    (when (eql pack *keyword-package*)
                      (change-class sym 'keyword)
                      (make-constant sym sym)
                      (export sym pack))
                    sym)
                  nil)))))


(defmethod shadow (symbol-names &optional (pack *package*))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-shadow (sym-name)
             (let ((sym (tget sym-name (present-table pack))))
               (unless sym
                 (setf sym (make-symbol sym-name))
                 (zimport-without-checks sym pack))
               (tput sym (shadowing-table pack)))))
      (mapc (function do-shadow)
            (normalize-weak-designator-of-list-of-string-designator symbol-names)))
    t))


(defmethod shadowing-import (symbols &optional (pack *package*))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-shadowing-import (sym)
             (check-type sym symbol)
             (let ((sym-name (symbol-name sym)))
               (multiple-value-bind (existing-sym type) (find-symbol sym-name pack)
                 (case type
                   ((nil :inherited)
                    (zimport-without-checks sym pack))
                   ((:external :internal)
                    (unless (eq existing-sym sym)
                      (zunintern-without-checks existing-sym pack)
                      (import sym pack))))
                 (tput sym (shadowing-table pack))))))
      (mapc (function do-shadowing-import) (ensure-list symbols))
      t)))


(defmethod export (symbols &optional (pack *package*))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-export (sym)
             (check-type sym symbol)
             (unless (accessiblep sym pack)
               (error 'symbol-inaccessible-error :package pack :symbol sym))
             (check-export-conflict sym pack)
             (unless (presentp sym pack)
               (import sym pack))
             (tput sym (external-table pack))))
      (mapc (function do-export) (ensure-list symbols))
      t)))


(defmethod unexport (symbols &optional (pack *package*))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-unexport (sym)
             (check-type sym symbol)
             (unless (accessiblep sym pack)
               (error 'symbol-inaccessible-error :package pack :symbol sym))
             (tremove sym (external-table pack))))
      (mapc (function do-unexport) (ensure-list symbols))
      t)))


(defmethod unintern (sym &optional (pack *package*))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (when (accessiblep sym pack)
      (check-unintern-conflict sym pack)
      (zunintern-without-checks sym pack)
      t)))


(defmethod use-package (packs &optional (using-pack *package*))
  (dolist (pack (ensure-list packs) t)
    (let* ((pack       (normalize-package-designator pack :if-package-does-not-exist :error))
           (using-pack (normalize-package-designator using-pack :if-package-does-not-exist :error))
           (use-list   (package-use-list using-pack)))
      (unless (member pack use-list)
        (check-inherit-conflict pack using-pack)
        (setf (used-packs using-pack) (cons pack use-list))
        (setf (used-by-packs    pack) (cons using-pack (package-used-by-list pack)))))))

(defmethod unuse-package (packs &optional (using-pack *package*))
  (dolist (pack (ensure-list packs) t)
    (let ((pack       (normalize-package-designator pack :if-package-does-not-exist :error))
          (using-pack (normalize-package-designator using-pack :if-package-does-not-exist :error)))
      (setf (used-packs using-pack) (remove pack (package-use-list using-pack)))
      (setf (used-by-packs pack)    (remove using-pack (package-used-by-list pack))))))


(defmethod find-all-symbols (name)
  (let ((name (normalize-string-designator name))
        (symbols '()))
    (dolist (pack (list-all-packages) (delete-duplicates symbols))
      (multiple-value-bind (sym found) (find-symbol name pack)
        (when found
          (push sym symbols))))))


(defmethod rename-package (package new-name &optional new-nicknames)
  (let ((package       (normalize-package-designator
                        package :if-package-does-not-exist :error))
        (new-name      (normalize-string-designator new-name))
        (new-nicknames (normalize-weak-designator-of-list-of-string-designator new-nicknames)))
    (multiple-value-setq (new-name new-nicknames) (check-new-names new-name new-nicknames
                                                                   :renaming-package package))
    ;; remove old names:
    (dolist (name (cons (package-name package) (package-nicknames package)))
      (remhash name *packs*))
    ;; set new names:
    (setf (name package) (copy-seq new-name)
          (nicknames package) (mapcar (function copy-seq) new-nicknames))
    (dolist (name (cons new-name new-nicknames) package)
      (setf (gethash name *packs*) package))))







(defun check-disjoints (shadows shadowing-import-froms import-froms
                        interns exports)
  (loop
    :for sets :in (list (append (list shadows interns)
                                (mapcar (function second) import-froms)
                                (mapcar (function second) shadowing-import-froms))
                        (list interns exports))
    :do (loop
          :for lefts :on sets
          :for left = (first lefts)
          :while (rest lefts)
          :do (loop
                :for rights :on (rest lefts)
                :for right = (first rights)
                :for inter = (intersection left right :test (function string=))
                :do (when inter
                      (flet ((set-name (set)
                               (let ((name (cdr (assoc set (list (cons shadows :shadow)
                                                                 (cons interns :intern)
                                                                 (cons exports :export))))))
                                 (or name
                                     (let ((name (first (find set shadowing-import-froms :key (function rest)))))
                                       (when name (list :shadowing-import-from name)))
                                     (let ((name (first (find set import-froms :key (function rest)))))
                                       (when name (list :import-from name)))))))
                        (error 'simple-program-error
                               :format-control "Symbol names in common between ~S and ~S: ~S"
                               :format-arguments (list (set-name left) (set-name right) inter)))))))
  nil)


(assert (null (check-disjoints (list "S1" "S2" "S3")
                               (list (list "P1" (list "P1A" "P1B" "P1C"))
                                     (list "P2" (list "P2A" "P2B" "P2C")))
                               (list (list "P3" (list "I1A" "I1B" "I1C"))
                                     (list "P4" (list "I2A" "I2B" "I2C")))
                               (list "I1" "I2" "I3")
                               (list "E1" "E2" "E3"))))

(assert (null (check-disjoints (list "S1" "S2" "S3")
                               '()
                               (list (list "P3" (list "I1A" "I1B" "I1C"))
                                     (list "P4" (list "I2A" "I2B" "I2C")))
                               '()
                               (list "E1" "E2" "E3"))))

(assert (nth-value 1 (ignore-errors (check-disjoints (list "S1" "S2" "S3")
                                                     (list (list "P1" (list "P1A" "P1B" "P1C"))
                                                           (list "P2" (list "P2A" "P2B" "P2C" "S3")))
                                                     (list (list "P3" (list "I1A" "I1B" "I1C"))
                                                           (list "P4" (list "I2A" "I2B" "I2C")))
                                                     (list "I1" "I2" "I3")
                                                     (list "E1" "E2" "E3")))))

(assert (null (check-disjoints (list "S1" "S2" "S3")
                               (list (list "P1" (list "P1A" "P1B" "P1C"))
                                     (list "P2" (list "P2A" "P2B" "P2C")))
                               (list (list "P3" (list "I1A" "I1B" "I1C"))
                                     (list "P4" (list "I2A" "I2B" "I2C")))
                               (list "I1" "I2" "I3")
                               (list "E1" "E2" "E3" "S2"))))




(defun %define-package (name shadows shadowing-imports
                        uses imports interns exports
                        documentation nicknames)
  (flet ((find-symbols (import-package names option)
           (mapcan (lambda (name)
                     (multiple-value-bind (symbol status) (find-symbol name import-package)
                       (if (null status)
                           (progn
                             (cerror (format nil "Ignore (~S  ~~*~~S ~~*~~S)" option)
                                     'symbol-does-not-exist-error
                                     :package import-package
                                     :symbol-name name)
                             '())
                           (list symbol))))
                   names)))
    (let ((package (find-package name)))
      (if package
          (let ((unuse-list (set-difference (package-use-list package) uses)))
            (rename-package package name nicknames)
            (unuse-package unuse-list package))
          (setf package (make-package name :nicknames nicknames :use '())))
      (setf (package-documentation package) documentation)
      ;; 1. :shadow and :shadowing-import-from.
      (shadow shadows package)
      (loop
        :for (import-package symbols) :in shadowing-imports
        :do (shadowing-import (find-symbols import-package symbols
                                            :shadowing-import-from)
                              package))
      ;; 2. :use.
      (dolist (upack uses)
        (use-package upack package))
      ;; 3. :import-from and :intern.
      (loop
        :for (import-package symbols) :in imports
        :do (import (find-symbols import-package symbols
                                  :import-from)
                    package))
      (dolist (name interns)
        (intern name package))
      ;; 4. :export.
      (export (mapcar (lambda (name) (intern name package)) exports) package)
      package)))



(define-modify-macro appendf (&rest args) append "Append onto list")


(defmacro defpackage (defined-package-name &rest options)
  ;; option::= (:nicknames nickname*)* |  
  ;;           (:documentation string) |  
  ;;           (:use package-name*)* |  
  ;;           (:shadow {symbol-name}*)* |  
  ;;           (:shadowing-import-from package-name {symbol-name}*)* |  
  ;;           (:import-from package-name {symbol-name}*)* |  
  ;;           (:export {symbol-name}*)* |  
  ;;           (:intern {symbol-name}*)* |  
  ;;           (:size integer)
  (dolist (option options)
    (unless (typep option 'list)
      (error 'simple-type-error
             :datum option
             :expected-type 'list
             :format-control "This implementation doesn't support any non-standard option such as ~S"
             :format-arguments (list option)))
    (unless (typep (car option) '(member :nicknames :documentation :use
                                  :shadow :shadowing-import-from
                                  :import-from :export :intern :size))
      (error 'simple-type-error
             :datum (car option)
             :expected-type '(member :nicknames :documentation :use
                              :shadow :shadowing-import-from
                              :import-from :export :intern :size)
             :format-control "This implementation doesn't support any non-standard option such as ~S"
             :format-arguments (list option))))
  (dolist (key '(:documentation :size))
   (unless (<= (count key options :key (function first)) 1)
     (cerror "Ignore all but the first" 'simple-program-error
             :format-control "Too many ~S options given: ~S"
             :format-arguments (list key (remove key options :test-not (function eql) :key (function first))))))
  (labels ((extract-strings (key)
             (delete-duplicates
              (normalize-weak-designator-of-list-of-string-designator
               (reduce (function append)
                       (mapcar (function rest)
                               (remove key options
                                       :key (function first)
                                       :test-not (function eql)))))))
           (extract-packages (key)
             (delete-duplicates
              (mapcan (lambda (package)
                        (list (normalize-package-designator
                               package
                               :if-package-does-not-exist :ignore-or-replace
                               :if-package-exists :string)))
                      (reduce (function append)
                              (mapcar (function rest)
                                      (remove key options
                                              :key (function first)
                                              :test-not (function eql)))))))           
           (extract-from (key)
             (let ((table (make-hash-table))
                   (result '()))
               (dolist (entry  (remove key options
                                       :key (function first)
                                       :test-not (function eql)))
                 (let ((entry (rest entry)))
                   (appendf (gethash (normalize-package-designator
                                      (first entry) :if-package-does-not-exist :error)
                                     table)
                            (normalize-weak-designator-of-list-of-string-designator (rest entry)))))
               ;; should do the same as in classify-per-package below.
               (maphash (lambda (k v) (push (list k v) result))
                        table)
               result))
           (check-string (object)
             (check-type object string)
             object)
           (extract-one-string (key)
             (let ((entries (remove key options
                                    :key (function first)
                                    :test-not (function eql))))
               (let ((entry (first entries)))
                 (when (rest entry)
                   (assert (null (cddr entry))
                           () "Invalid :DOCUMENTATION option: it should contain only one string.")
                   (check-string (second entry)))))))
    (let* ((shadows           (extract-strings    :shadow))
           (shadowing-imports (extract-from       :shadowing-import-from))
           (import-froms      (extract-from       :import-from))
           (interns           (extract-strings    :intern))
           (exports           (extract-strings    :export)))
      (check-disjoints shadows shadowing-imports import-froms interns exports)
      `(eval-when (:execute :compile-toplevel :load-toplevel)
         (%define-package ',(normalize-string-designator defined-package-name :if-not-a-string-designator :replace)
                          ',shadows
                          ',shadowing-imports
                          ',(extract-packages   :use)
                          ',import-froms
                          ',interns
                          ',exports
                          ',(extract-one-string :documentation)
                          ',(extract-strings    :nicknames))))))



(defun classify-per-package (symbols)
  (let ((table (make-hash-table))
        (result '()))
    (dolist (sym symbols)
      (push (symbol-name sym) (gethash (symbol-package sym) table '())))
    ;; should do the same as defpackage/extract-from above.
    (maphash (lambda (k v) (push (list k v) result)) table)
    result))

;; (set-equal (package-shadowing-symbols p)
;;            (reduce 'union (cons (package-shadow-list p)
;;                                 (mapcar 'rest (package-shadowing-import-list p)))))

(defmethod package-shadow-list (package)
  "Return the list of shadowed symbols (but not shadowing-imported ones)"
  (remove package (package-shadowing-symbols package)
          :test-not (function eql)
          :key (function symbol-package)))

(defmethod package-shadowing-import-list (package)
  "Return a list of lists of shadowing-imports.
Each sublist contains the package followed by its imported symbols."
  (classify-per-package  (remove package (package-shadowing-symbols package)
                                 :key (function symbol-package))))


;; NOTE: we don't know where the imported symbols were taken from, we
;;       only know their home package.  If they were imported from a
;;       package that used them, or that imported them, then we won't
;;       remember it, and will import them directly from their home.
;;       This is probably not good.

(defmethod package-import-from-list (package)
  (let ((symbols '()))
    (with-package-iterator (it package :present)
      (loop
        (multiple-value-bind (got-it symbol kind home) (it)
          (declare (ignore kind))
          (if got-it
              (unless (eq home package)  (push symbol symbols))
              (return (classify-per-package symbols))))))))

(defmethod package-symbols (package)
  (let ((result '()))
    (with-package-iterator (it package :present)
      (loop
        (multiple-value-bind (got-it symbol kind home) (it)
          (declare (ignore kind))
          (if got-it
              (when (eq home package) (push symbol result))
              (return result)))))))

(defmethod package-export-list (package)
  (let ((result '()))
    (with-package-iterator (it package :external)
      (loop
        (multiple-value-bind (got-it symbol kind home) (it)
          (declare (ignore kind home))
          (if got-it
              (push symbol result)
              (return result)))))))




(defmethod make-load-form ((pack package) &optional environment)
  (declare (ignore environment))
  `(%define-package ',(package-name pack)
                    ',(mapcar (function symbol-name) (package-shadow-list pack))
                    ',(package-shadowing-import-list pack)
                    ',(mapcar (function package-name) (package-use-list pack))
                    ',(package-import-from-list pack)
                    ',(mapcar (function symbol-name) (package-symbols pack))
                    ',(mapcar (function symbol-name) (package-export-list pack))
                    ',(package-documentation pack)
                    ',(package-nicknames pack)))


(defmacro in-package (name)
  (let ((name (normalize-string-designator name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((new-package (normalize-package-designator
                           ,name :if-package-does-not-exist :ignore-or-replace)))
         (when new-package
           (setf *package* new-package))))))




(defpackage "KEYWORD"
  (:use)
  (:documentation "The KEYWORD package."))

(defpackage "COMMON-LISP"
  (:use)
  (:nicknames "CL")
  (:export "*" "**" "***" "*BREAK-ON-SIGNALS*"
           "*COMPILE-FILE-PATHNAME*" "*COMPILE-FILE-TRUENAME*"
           "*COMPILE-PRINT*" "*COMPILE-VERBOSE*" "*DEBUG-IO*"
           "*DEBUGGER-HOOK*" "*DEFAULT-PATHNAME-DEFAULTS*"
           "*ERROR-OUTPUT*" "*FEATURES*" "*GENSYM-COUNTER*"
           "*LOAD-PATHNAME*" "*LOAD-PRINT*" "*LOAD-TRUENAME*"
           "*LOAD-VERBOSE*" "*MACROEXPAND-HOOK*" "*MODULES*"
           "*PACKAGE*" "*PRINT-ARRAY*" "*PRINT-BASE*"
           "*PRINT-CASE*" "*PRINT-CIRCLE*" "*PRINT-ESCAPE*"
           "*PRINT-GENSYM*" "*PRINT-LENGTH*" "*PRINT-LEVEL*"
           "*PRINT-LINES*" "*PRINT-MISER-WIDTH*"
           "*PRINT-PPRINT-DISPATCH*" "*PRINT-PRETTY*"
           "*PRINT-RADIX*" "*PRINT-READABLY*"
           "*PRINT-RIGHT-MARGIN*" "*QUERY-IO*" "*RANDOM-STATE*"
           "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*"
           "*READ-EVAL*" "*READ-SUPPRESS*" "*READTABLE*"
           "*STANDARD-INPUT*" "*STANDARD-OUTPUT*"
           "*TERMINAL-IO*" "*TRACE-OUTPUT*" "+" "++" "+++" "-"
           "/" "//" "///" "/=" "1+" "1-" "<" "<=" "=" ">" ">="
           "ABORT" "ABS" "ACONS" "ACOS" "ACOSH" "ADD-METHOD"
           "ADJOIN" "ADJUST-ARRAY" "ADJUSTABLE-ARRAY-P"
           "ALLOCATE-INSTANCE" "ALPHA-CHAR-P" "ALPHANUMERICP"
           "AND" "APPEND" "APPLY" "APROPOS" "APROPOS-LIST"
           "AREF" "ARITHMETIC-ERROR"
           "ARITHMETIC-ERROR-OPERANDS"
           "ARITHMETIC-ERROR-OPERATION" "ARRAY"
           "ARRAY-DIMENSION" "ARRAY-DIMENSION-LIMIT"
           "ARRAY-DIMENSIONS" "ARRAY-DISPLACEMENT"
           "ARRAY-ELEMENT-TYPE" "ARRAY-HAS-FILL-POINTER-P"
           "ARRAY-IN-BOUNDS-P" "ARRAY-RANK" "ARRAY-RANK-LIMIT"
           "ARRAY-ROW-MAJOR-INDEX" "ARRAY-TOTAL-SIZE"
           "ARRAY-TOTAL-SIZE-LIMIT" "ARRAYP" "ASH" "ASIN"
           "ASINH" "ASSERT" "ASSOC" "ASSOC-IF" "ASSOC-IF-NOT"
           "ATAN" "ATANH" "ATOM" "BASE-CHAR" "BASE-STRING"
           "BIGNUM" "BIT" "BIT-AND" "BIT-ANDC1" "BIT-ANDC2"
           "BIT-EQV" "BIT-IOR" "BIT-NAND" "BIT-NOR" "BIT-NOT"
           "BIT-ORC1" "BIT-ORC2" "BIT-VECTOR" "BIT-VECTOR-P"
           "BIT-XOR" "BLOCK" "BOOLE" "BOOLE-1" "BOOLE-2"
           "BOOLE-AND" "BOOLE-ANDC1" "BOOLE-ANDC2" "BOOLE-C1"
           "BOOLE-C2" "BOOLE-CLR" "BOOLE-EQV" "BOOLE-IOR"
           "BOOLE-NAND" "BOOLE-NOR" "BOOLE-ORC1" "BOOLE-ORC2"
           "BOOLE-SET" "BOOLE-XOR" "BOOLEAN" "BOTH-CASE-P"
           "BOUNDP" "BREAK" "BROADCAST-STREAM"
           "BROADCAST-STREAM-STREAMS" "BUILT-IN-CLASS"
           "BUTLAST" "BYTE" "BYTE-POSITION" "BYTE-SIZE"
           "CAAAAR" "CAAADR" "CAAAR" "CAADAR" "CAADDR" "CAADR"
           "CAAR" "CADAAR" "CADADR" "CADAR" "CADDAR" "CADDDR"
           "CADDR" "CADR" "CALL-ARGUMENTS-LIMIT" "CALL-METHOD"
           "CALL-NEXT-METHOD" "CAR" "CASE" "CATCH" "CCASE"
           "CDAAAR" "CDAADR" "CDAAR" "CDADAR" "CDADDR" "CDADR"
           "CDAR" "CDDAAR" "CDDADR" "CDDAR" "CDDDAR" "CDDDDR"
           "CDDDR" "CDDR" "CDR" "CEILING" "CELL-ERROR"
           "CELL-ERROR-NAME" "CERROR" "CHANGE-CLASS" "CHAR"
           "CHAR-CODE" "CHAR-CODE-LIMIT" "CHAR-DOWNCASE"
           "CHAR-EQUAL" "CHAR-GREATERP" "CHAR-INT" "CHAR-LESSP"
           "CHAR-NAME" "CHAR-NOT-EQUAL" "CHAR-NOT-GREATERP"
           "CHAR-NOT-LESSP" "CHAR-UPCASE" "CHAR/=" "CHAR<"
           "CHAR<=" "CHAR=" "CHAR>" "CHAR>=" "CHARACTER"
           "CHARACTERP" "CHECK-TYPE" "CIS" "CLASS" "CLASS-NAME"
           "CLASS-OF" "CLEAR-INPUT" "CLEAR-OUTPUT" "CLOSE"
           "CLRHASH" "CODE-CHAR" "COERCE" "COMPILE"
           "COMPILE-FILE" "COMPILE-FILE-PATHNAME"
           "COMPILED-FUNCTION" "COMPILED-FUNCTION-P"
           "COMPILER-MACRO-FUNCTION" "COMPLEMENT" "COMPLEX"
           "COMPLEXP" "COMPUTE-APPLICABLE-METHODS"
           "COMPUTE-RESTARTS" "CONCATENATE"
           "CONCATENATED-STREAM" "CONCATENATED-STREAM-STREAMS"
           "COND" "CONDITION" "CONJUGATE" "CONS" "CONSP"
           "CONSTANTLY" "CONSTANTP" "CONTINUE" "CONTROL-ERROR"
           "COPY-ALIST" "COPY-LIST" "COPY-PPRINT-DISPATCH"
           "COPY-READTABLE" "COPY-SEQ" "COPY-STRUCTURE"
           "COPY-SYMBOL" "COPY-TREE" "COS" "COSH" "COUNT"
           "COUNT-IF" "COUNT-IF-NOT" "CTYPECASE" "DECF"
           "DECLAIM" "DECLARATION" "DECLARE" "DECODE-FLOAT"
           "DECODE-UNIVERSAL-TIME" "DEFCLASS" "DEFCONSTANT"
           "DEFGENERIC" "DEFINE-COMPILER-MACRO"
           "DEFINE-CONDITION" "DEFINE-METHOD-COMBINATION"
           "DEFINE-MODIFY-MACRO" "DEFINE-SETF-EXPANDER"
           "DEFINE-SYMBOL-MACRO" "DEFMACRO" "DEFMETHOD"
           "DEFPACKAGE" "DEFPARAMETER" "DEFSETF" "DEFSTRUCT"
           "DEFTYPE" "DEFUN" "DEFVAR" "DELETE"
           "DELETE-DUPLICATES" "DELETE-FILE" "DELETE-IF"
           "DELETE-IF-NOT" "DELETE-PACKAGE" "DENOMINATOR"
           "DEPOSIT-FIELD" "DESCRIBE" "DESCRIBE-OBJECT"
           "DESTRUCTURING-BIND" "DIGIT-CHAR" "DIGIT-CHAR-P"
           "DIRECTORY" "DIRECTORY-NAMESTRING" "DISASSEMBLE"
           "DIVISION-BY-ZERO" "DO" "DO*" "DO-ALL-SYMBOLS"
           "DO-EXTERNAL-SYMBOLS" "DO-SYMBOLS" "DOCUMENTATION"
           "DOLIST" "DOTIMES" "DOUBLE-FLOAT"
           "DOUBLE-FLOAT-EPSILON"
           "DOUBLE-FLOAT-NEGATIVE-EPSILON" "DPB" "DRIBBLE"
           "DYNAMIC-EXTENT" "ECASE" "ECHO-STREAM"
           "ECHO-STREAM-INPUT-STREAM"
           "ECHO-STREAM-OUTPUT-STREAM" "ED" "EIGHTH" "ELT"
           "END-OF-FILE" "ENDP" "ENOUGH-NAMESTRING"
           "ENSURE-DIRECTORIES-EXIST" "ENSURE-GENERIC-FUNCTION"
           "EQ" "EQL" "EQUAL" "EQUALP" "ERROR" "ETYPECASE"
           "EVAL" "EVAL-WHEN" "EVENP" "EVERY" "EXP" "EXPORT"
           "EXPT" "EXTENDED-CHAR" "FBOUNDP" "FCEILING"
           "FDEFINITION" "FFLOOR" "FIFTH" "FILE-AUTHOR"
           "FILE-ERROR" "FILE-ERROR-PATHNAME" "FILE-LENGTH"
           "FILE-NAMESTRING" "FILE-POSITION" "FILE-STREAM"
           "FILE-STRING-LENGTH" "FILE-WRITE-DATE" "FILL"
           "FILL-POINTER" "FIND" "FIND-ALL-SYMBOLS"
           "FIND-CLASS" "FIND-IF" "FIND-IF-NOT" "FIND-METHOD"
           "FIND-PACKAGE" "FIND-RESTART" "FIND-SYMBOL"
           "FINISH-OUTPUT" "FIRST" "FIXNUM" "FLET" "FLOAT"
           "FLOAT-DIGITS" "FLOAT-PRECISION" "FLOAT-RADIX"
           "FLOAT-SIGN" "FLOATING-POINT-INEXACT"
           "FLOATING-POINT-INVALID-OPERATION"
           "FLOATING-POINT-OVERFLOW" "FLOATING-POINT-UNDERFLOW"
           "FLOATP" "FLOOR" "FMAKUNBOUND" "FORCE-OUTPUT"
           "FORMAT" "FORMATTER" "FOURTH" "FRESH-LINE" "FROUND"
           "FTRUNCATE" "FTYPE" "FUNCALL" "FUNCTION"
           "FUNCTION-KEYWORDS" "FUNCTION-LAMBDA-EXPRESSION"
           "FUNCTIONP" "GCD" "GENERIC-FUNCTION" "GENSYM"
           "GENTEMP" "GET" "GET-DECODED-TIME"
           "GET-DISPATCH-MACRO-CHARACTER"
           "GET-INTERNAL-REAL-TIME" "GET-INTERNAL-RUN-TIME"
           "GET-MACRO-CHARACTER" "GET-OUTPUT-STREAM-STRING"
           "GET-PROPERTIES" "GET-SETF-EXPANSION"
           "GET-UNIVERSAL-TIME" "GETF" "GETHASH" "GO"
           "GRAPHIC-CHAR-P" "HANDLER-BIND" "HANDLER-CASE"
           "HASH-TABLE" "HASH-TABLE-COUNT" "HASH-TABLE-P"
           "HASH-TABLE-REHASH-SIZE"
           "HASH-TABLE-REHASH-THRESHOLD" "HASH-TABLE-SIZE"
           "HASH-TABLE-TEST" "HOST-NAMESTRING" "IDENTITY" "IF"
           "IGNORABLE" "IGNORE" "IGNORE-ERRORS" "IMAGPART"
           "IMPORT" "IN-PACKAGE" "INCF" "INITIALIZE-INSTANCE"
           "INLINE" "INPUT-STREAM-P" "INSPECT" "INTEGER"
           "INTEGER-DECODE-FLOAT" "INTEGER-LENGTH" "INTEGERP"
           "INTERACTIVE-STREAM-P" "INTERN"
           "INTERNAL-TIME-UNITS-PER-SECOND" "INTERSECTION"
           "INVALID-METHOD-ERROR" "INVOKE-DEBUGGER"
           "INVOKE-RESTART" "INVOKE-RESTART-INTERACTIVELY"
           "ISQRT" "KEYWORD" "KEYWORDP" "LABELS" "LAMBDA"
           "LAMBDA-LIST-KEYWORDS" "LAMBDA-PARAMETERS-LIMIT"
           "LAST" "LCM" "LDB" "LDB-TEST" "LDIFF"
           "LEAST-NEGATIVE-DOUBLE-FLOAT"
           "LEAST-NEGATIVE-LONG-FLOAT"
           "LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT"
           "LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT"
           "LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT"
           "LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT"
           "LEAST-NEGATIVE-SHORT-FLOAT"
           "LEAST-NEGATIVE-SINGLE-FLOAT"
           "LEAST-POSITIVE-DOUBLE-FLOAT"
           "LEAST-POSITIVE-LONG-FLOAT"
           "LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT"
           "LEAST-POSITIVE-NORMALIZED-LONG-FLOAT"
           "LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT"
           "LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT"
           "LEAST-POSITIVE-SHORT-FLOAT"
           "LEAST-POSITIVE-SINGLE-FLOAT" "LENGTH" "LET" "LET*"
           "LISP-IMPLEMENTATION-TYPE"
           "LISP-IMPLEMENTATION-VERSION" "LIST" "LIST*"
           "LIST-ALL-PACKAGES" "LIST-LENGTH" "LISTEN" "LISTP"
           "LOAD" "LOAD-LOGICAL-PATHNAME-TRANSLATIONS"
           "LOAD-TIME-VALUE" "LOCALLY" "LOG" "LOGAND"
           "LOGANDC1" "LOGANDC2" "LOGBITP" "LOGCOUNT" "LOGEQV"
           "LOGICAL-PATHNAME" "LOGICAL-PATHNAME-TRANSLATIONS"
           "LOGIOR" "LOGNAND" "LOGNOR" "LOGNOT" "LOGORC1"
           "LOGORC2" "LOGTEST" "LOGXOR" "LONG-FLOAT"
           "LONG-FLOAT-EPSILON" "LONG-FLOAT-NEGATIVE-EPSILON"
           "LONG-SITE-NAME" "LOOP" "LOOP-FINISH" "LOWER-CASE-P"
           "MACHINE-INSTANCE" "MACHINE-TYPE" "MACHINE-VERSION"
           "MACRO-FUNCTION" "MACROEXPAND" "MACROEXPAND-1"
           "MACROLET" "MAKE-ARRAY" "MAKE-BROADCAST-STREAM"
           "MAKE-CONCATENATED-STREAM" "MAKE-CONDITION"
           "MAKE-DISPATCH-MACRO-CHARACTER" "MAKE-ECHO-STREAM"
           "MAKE-HASH-TABLE" "MAKE-INSTANCE"
           "MAKE-INSTANCES-OBSOLETE" "MAKE-LIST"
           "MAKE-LOAD-FORM" "MAKE-LOAD-FORM-SAVING-SLOTS"
           "MAKE-METHOD" "MAKE-PACKAGE" "MAKE-PATHNAME"
           "MAKE-RANDOM-STATE" "MAKE-SEQUENCE" "MAKE-STRING"
           "MAKE-STRING-INPUT-STREAM"
           "MAKE-STRING-OUTPUT-STREAM" "MAKE-SYMBOL"
           "MAKE-SYNONYM-STREAM" "MAKE-TWO-WAY-STREAM"
           "MAKUNBOUND" "MAP" "MAP-INTO" "MAPC" "MAPCAN"
           "MAPCAR" "MAPCON" "MAPHASH" "MAPL" "MAPLIST"
           "MASK-FIELD" "MAX" "MEMBER" "MEMBER-IF"
           "MEMBER-IF-NOT" "MERGE" "MERGE-PATHNAMES" "METHOD"
           "METHOD-COMBINATION" "METHOD-COMBINATION-ERROR"
           "METHOD-QUALIFIERS" "MIN" "MINUSP" "MISMATCH" "MOD"
           "MOST-NEGATIVE-DOUBLE-FLOAT" "MOST-NEGATIVE-FIXNUM"
           "MOST-NEGATIVE-LONG-FLOAT"
           "MOST-NEGATIVE-SHORT-FLOAT"
           "MOST-NEGATIVE-SINGLE-FLOAT"
           "MOST-POSITIVE-DOUBLE-FLOAT" "MOST-POSITIVE-FIXNUM"
           "MOST-POSITIVE-LONG-FLOAT"
           "MOST-POSITIVE-SHORT-FLOAT"
           "MOST-POSITIVE-SINGLE-FLOAT" "MUFFLE-WARNING"
           "MULTIPLE-VALUE-BIND" "MULTIPLE-VALUE-CALL"
           "MULTIPLE-VALUE-LIST" "MULTIPLE-VALUE-PROG1"
           "MULTIPLE-VALUE-SETQ" "MULTIPLE-VALUES-LIMIT"
           "NAME-CHAR" "NAMESTRING" "NBUTLAST" "NCONC"
           "NEXT-METHOD-P" "NIL" "NINTERSECTION" "NINTH"
           "NO-APPLICABLE-METHOD" "NO-NEXT-METHOD" "NOT"
           "NOTANY" "NOTEVERY" "NOTINLINE" "NRECONC" "NREVERSE"
           "NSET-DIFFERENCE" "NSET-EXCLUSIVE-OR"
           "NSTRING-CAPITALIZE" "NSTRING-DOWNCASE"
           "NSTRING-UPCASE" "NSUBLIS" "NSUBST" "NSUBST-IF"
           "NSUBST-IF-NOT" "NSUBSTITUTE" "NSUBSTITUTE-IF"
           "NSUBSTITUTE-IF-NOT" "NTH" "NTH-VALUE" "NTHCDR"
           "NULL" "NUMBER" "NUMBERP" "NUMERATOR" "NUNION"
           "ODDP" "OPEN" "OPEN-STREAM-P" "OPTIMIZE" "OR"
           "OUTPUT-STREAM-P" "PACKAGE" "PACKAGE-ERROR"
           "PACKAGE-ERROR-PACKAGE" "PACKAGE-NAME"
           "PACKAGE-NICKNAMES" "PACKAGE-SHADOWING-SYMBOLS"
           "PACKAGE-USE-LIST" "PACKAGE-USED-BY-LIST" "PACKAGEP"
           "PAIRLIS" "PARSE-ERROR" "PARSE-INTEGER"
           "PARSE-NAMESTRING" "PATHNAME" "PATHNAME-DEVICE"
           "PATHNAME-DIRECTORY" "PATHNAME-HOST"
           "PATHNAME-MATCH-P" "PATHNAME-NAME" "PATHNAME-TYPE"
           "PATHNAME-VERSION" "PATHNAMEP" "PEEK-CHAR" "PHASE"
           "PI" "PLUSP" "POP" "POSITION" "POSITION-IF"
           "POSITION-IF-NOT" "PPRINT" "PPRINT-DISPATCH"
           "PPRINT-EXIT-IF-LIST-EXHAUSTED" "PPRINT-FILL"
           "PPRINT-INDENT" "PPRINT-LINEAR"
           "PPRINT-LOGICAL-BLOCK" "PPRINT-NEWLINE" "PPRINT-POP"
           "PPRINT-TAB" "PPRINT-TABULAR" "PRIN1"
           "PRIN1-TO-STRING" "PRINC" "PRINC-TO-STRING" "PRINT"
           "PRINT-NOT-READABLE" "PRINT-NOT-READABLE-OBJECT"
           "PRINT-OBJECT" "PRINT-UNREADABLE-OBJECT"
           "PROBE-FILE" "PROCLAIM" "PROG" "PROG*" "PROG1"
           "PROG2" "PROGN" "PROGRAM-ERROR" "PROGV" "PROVIDE"
           "PSETF" "PSETQ" "PUSH" "PUSHNEW" "QUOTE" "RANDOM"
           "RANDOM-STATE" "RANDOM-STATE-P" "RASSOC" "RASSOC-IF"
           "RASSOC-IF-NOT" "RATIO" "RATIONAL" "RATIONALIZE"
           "RATIONALP" "READ" "READ-BYTE" "READ-CHAR"
           "READ-CHAR-NO-HANG" "READ-DELIMITED-LIST"
           "READ-FROM-STRING" "READ-LINE"
           "READ-PRESERVING-WHITESPACE" "READ-SEQUENCE"
           "READER-ERROR" "READTABLE" "READTABLE-CASE"
           "READTABLEP" "REAL" "REALP" "REALPART" "REDUCE"
           "REINITIALIZE-INSTANCE" "REM" "REMF" "REMHASH"
           "REMOVE" "REMOVE-DUPLICATES" "REMOVE-IF"
           "REMOVE-IF-NOT" "REMOVE-METHOD" "REMPROP"
           "RENAME-FILE" "RENAME-PACKAGE" "REPLACE" "REQUIRE"
           "REST" "RESTART" "RESTART-BIND" "RESTART-CASE"
           "RESTART-NAME" "RETURN" "RETURN-FROM" "REVAPPEND"
           "REVERSE" "ROOM" "ROTATEF" "ROUND" "ROW-MAJOR-AREF"
           "RPLACA" "RPLACD" "SATISFIES" "SBIT" "SCALE-FLOAT"
           "SCHAR" "SEARCH" "SECOND" "SEQUENCE"
           "SERIOUS-CONDITION" "SET" "SET-DIFFERENCE"
           "SET-DISPATCH-MACRO-CHARACTER" "SET-EXCLUSIVE-OR"
           "SET-MACRO-CHARACTER" "SET-PPRINT-DISPATCH"
           "SET-SYNTAX-FROM-CHAR" "SETF" "SETQ" "SEVENTH"
           "SHADOW" "SHADOWING-IMPORT" "SHARED-INITIALIZE"
           "SHIFTF" "SHORT-FLOAT" "SHORT-FLOAT-EPSILON"
           "SHORT-FLOAT-NEGATIVE-EPSILON" "SHORT-SITE-NAME"
           "SIGNAL" "SIGNED-BYTE" "SIGNUM" "SIMPLE-ARRAY"
           "SIMPLE-BASE-STRING" "SIMPLE-BIT-VECTOR"
           "SIMPLE-BIT-VECTOR-P" "SIMPLE-CONDITION"
           "SIMPLE-CONDITION-FORMAT-ARGUMENTS"
           "SIMPLE-CONDITION-FORMAT-CONTROL" "SIMPLE-ERROR"
           "SIMPLE-STRING" "SIMPLE-STRING-P"
           "SIMPLE-TYPE-ERROR" "SIMPLE-VECTOR"
           "SIMPLE-VECTOR-P" "SIMPLE-WARNING" "SIN"
           "SINGLE-FLOAT" "SINGLE-FLOAT-EPSILON"
           "SINGLE-FLOAT-NEGATIVE-EPSILON" "SINH" "SIXTH"
           "SLEEP" "SLOT-BOUNDP" "SLOT-EXISTS-P"
           "SLOT-MAKUNBOUND" "SLOT-MISSING" "SLOT-UNBOUND"
           "SLOT-VALUE" "SOFTWARE-TYPE" "SOFTWARE-VERSION"
           "SOME" "SORT" "SPECIAL" "SPECIAL-OPERATOR-P" "SQRT"
           "STABLE-SORT" "STANDARD-CHAR" "STANDARD-CHAR-P"
           "STANDARD-CLASS" "STANDARD-GENERIC-FUNCTION"
           "STANDARD-METHOD" "STANDARD-OBJECT" "STEP"
           "STORAGE-CONDITION" "STORE-VALUE" "STREAM"
           "STREAM-ELEMENT-TYPE" "STREAM-ERROR"
           "STREAM-ERROR-STREAM" "STREAM-EXTERNAL-FORMAT"
           "STREAMP" "STRING" "STRING-CAPITALIZE"
           "STRING-DOWNCASE" "STRING-EQUAL" "STRING-GREATERP"
           "STRING-LEFT-TRIM" "STRING-LESSP" "STRING-NOT-EQUAL"
           "STRING-NOT-GREATERP" "STRING-NOT-LESSP"
           "STRING-RIGHT-TRIM" "STRING-STREAM" "STRING-TRIM"
           "STRING-UPCASE" "STRING/=" "STRING<" "STRING<="
           "STRING=" "STRING>" "STRING>=" "STRINGP"
           "STRUCTURE-CLASS" "STRUCTURE-OBJECT" "STYLE-WARNING"
           "SUBLIS" "SUBSEQ" "SUBSETP" "SUBST" "SUBST-IF"
           "SUBST-IF-NOT" "SUBSTITUTE" "SUBSTITUTE-IF"
           "SUBSTITUTE-IF-NOT" "SUBTYPEP" "SVREF" "SXHASH"
           "SYMBOL" "SYMBOL-FUNCTION" "SYMBOL-MACROLET"
           "SYMBOL-NAME" "SYMBOL-PACKAGE" "SYMBOL-PLIST"
           "SYMBOL-VALUE" "SYMBOLP" "SYNONYM-STREAM"
           "SYNONYM-STREAM-SYMBOL" "T" "TAGBODY" "TAILP" "TAN"
           "TANH" "TENTH" "TERPRI" "THE" "THIRD" "THROW" "TIME"
           "TRACE" "TRANSLATE-LOGICAL-PATHNAME"
           "TRANSLATE-PATHNAME" "TREE-EQUAL" "TRUENAME"
           "TRUNCATE" "TWO-WAY-STREAM"
           "TWO-WAY-STREAM-INPUT-STREAM"
           "TWO-WAY-STREAM-OUTPUT-STREAM" "TYPE" "TYPE-ERROR"
           "TYPE-ERROR-DATUM" "TYPE-ERROR-EXPECTED-TYPE"
           "TYPE-OF" "TYPECASE" "TYPEP" "UNBOUND-SLOT"
           "UNBOUND-SLOT-INSTANCE" "UNBOUND-VARIABLE"
           "UNDEFINED-FUNCTION" "UNEXPORT" "UNINTERN" "UNION"
           "UNLESS" "UNREAD-CHAR" "UNSIGNED-BYTE" "UNTRACE"
           "UNUSE-PACKAGE" "UNWIND-PROTECT"
           "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS"
           "UPDATE-INSTANCE-FOR-REDEFINED-CLASS"
           "UPGRADED-ARRAY-ELEMENT-TYPE"
           "UPGRADED-COMPLEX-PART-TYPE" "UPPER-CASE-P"
           "USE-PACKAGE" "USE-VALUE" "USER-HOMEDIR-PATHNAME"
           "VALUES" "VALUES-LIST" "VECTOR" "VECTOR-POP"
           "VECTOR-PUSH" "VECTOR-PUSH-EXTEND" "VECTORP" "WARN"
           "WARNING" "WHEN" "WILD-PATHNAME-P" "WITH-ACCESSORS"
           "WITH-COMPILATION-UNIT" "WITH-CONDITION-RESTARTS"
           "WITH-HASH-TABLE-ITERATOR" "WITH-INPUT-FROM-STRING"
           "WITH-OPEN-STREAM" "WITH-OUTPUT-TO-STRING"
           "WITH-PACKAGE-ITERATOR" "WITH-SIMPLE-RESTART"
           "WITH-SLOTS" "WITH-STANDARD-IO-SYNTAX" "WRITE"
           "WRITE-BYTE" "WRITE-CHAR" "WRITE-LINE"
           "WRITE-SEQUENCE" "WRITE-STRING" "WRITE-TO-STRING"
           "Y-OR-N-P" "YES-OR-NO-P" "ZEROP")
  (:documentation "The COMMON-LISP package."))

(defpackage "COMMON-LISP-USER"
  (:use "COMMON-LISP")
  (:nicknames "CL-USER")
  (:documentation "The COMMON-LISP-USER package."))


(setf *keyword-package* (find-package "KEYWORD")
      *common-lisp-package* (find-package "COMMON-LISP")
      *common-lisp-user-package* (find-package "COMMON-LISP-USER")
      *package* *common-lisp-user-package*)


;; (ignore-errors (delete-package "TEST-SHADOWING-IMPORT"))
;; (ignore-errors (delete-package "TEST-LISP"))
;; (defpackage "TEST-LISP"
;;   (:use)
;;   (:export "CONS" "CAR" "CDR" "NULL" "NIL" "T"))
;; 
;; (defpackage "TEST-SHADOWING-IMPORT"
;;   (:use "COMMON-LISP")
;;   (:shadowing-import-from "TEST-LISP"
;;                           "CONS" "CAR" "CDR" "NULL" "NIL" "T")
;;   (:shadow "IF" "COND")
;;   (:export "IF" "COND"))
;; 
;; 
;; (progn (unintern (find-symbol "CONS" "TEST-SHADOWING-IMPORT") "TEST-SHADOWING-IMPORT")
;;        (shadowing-import (find-symbol "CONS" "TEST-LISP") "TEST-SHADOWING-IMPORT")
;;        (assert (eql (symbol-package (find-symbol "CONS" "TEST-SHADOWING-IMPORT"))
;;                     (find-package "TEST-LISP"))))
;; 
;; (progn (unintern (find-symbol "CONS" "TEST-SHADOWING-IMPORT") "TEST-SHADOWING-IMPORT")
;;        (intern  "CONS" "TEST-SHADOWING-IMPORT")
;;        (shadowing-import (find-symbol "CONS" "TEST-LISP") "TEST-SHADOWING-IMPORT")
;;        (assert (eql (symbol-package (find-symbol "CONS" "TEST-SHADOWING-IMPORT"))
;;                     (find-package "TEST-LISP"))))



;; (list (package-shadowing-symbols "TEST-SHADOWING-IMPORT")
;;       (package-shadow-list (find-package "TEST-SHADOWING-IMPORT"))
;;       (package-shadowing-import-list (find-package "TEST-SHADOWING-IMPORT")))


;;;; THE END ;;;;
