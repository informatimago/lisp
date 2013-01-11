;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
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



(cl:in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.PACKAGE")

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

(defgeneric make-symbol (sym-name)
  (:documentation "
DO:     Make a new symbol
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_sym.htm>
"))

(defgeneric symbol-name (sym)
  (:documentation "
RETURN: the name of the symbol.     
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_2.htm>
"))

(defgeneric symbol-package (sym)
  (:documentation "
RETURN: the home package of the symbol.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_3.htm>
"))

(defgeneric make-package (pack-name &key nicknames use)
  (:documentation "
DO:     Make a new package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_pkg.htm>
"))

(defgeneric find-package (pack-name)
  (:documentation "
RETURN: The package designated by PACK-NAME, or NIL if none.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_find_p.htm>
"))

(defgeneric delete-package (pack-name)
  (:documentation "
DO:     Delete the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_del_pk.htm>
"))

(defgeneric find-symbol (sym-name &optional pack)
  (:documentation "
RETURN: the symbol named SYM-NAME in the package PACK, if found and a status keyword.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_find_s.htm>
"))

(defgeneric import (symbols &optional pack)
  (:documentation "
DO:     Import the symbols into the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_import.htm>
"))

(defgeneric intern (sym-name &optional pack)
  (:documentation "
DO:     Intern the symbol name in the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_intern.htm>
"))

(defgeneric shadow (symbol-names &optional pack)
  (:documentation "
DO:     Shadow the designated symbols.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_shadow.htm>
"))

(defgeneric shadowing-import (symbols &optional pack)
  (:documentation "
DO:     Shadow and import the designated symbols.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_shdw_i.htm>
"))

(defgeneric export (sym &optional pack)
  (:documentation "
DO:     Export the designated symbols from the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_export.htm>
"))

(defgeneric unexport (sym &optional pack)
  (:documentation "
DO:     Unexport the designated symbols from the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_unexpo.htm>
"))

(defgeneric unintern (sym &optional pack)
  (:documentation "
DO:     Unintern the designated symbols from the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_uninte.htm>
"))

(defgeneric use-package (pack &optional using-pack)
  (:documentation "
DO:     Make the USING-PACK use the package PACK.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_use_pk.htm>
"))

(defgeneric unuse-package (pack &optional using-pack)
  (:documentation "
DO:     Make the USING-PACK unuse the package PACK 
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_unuse_.htm>
"))

(defgeneric package-name (pack)
  (:documentation "
RETURN: The package name.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_na.htm>
"))

(defgeneric package-use-list (pack)
  (:documentation "
RETURN: The list of packages used by PACK.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_us.htm>
"))

(defgeneric package-used-by-list (pack)
  (:documentation "
RETURN: The list of packages that use PACK.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg__1.htm>
"))

(defgeneric package-shadowing-symbols (pack)
  (:documentation "
RETURN: The list of shadowing symbols of the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_sh.htm>
"))

(defgeneric find-all-symbols (name)
  (:documentation "
RETURN: The list of all symbols named NAME in all packages.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_find_a.htm>
"))

(defgeneric rename-package (package new-name &optional new-nicknames)
  (:documentation "
DO:     Rename the package giving it the NEW-NAME and NEW-NICKNAMES.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_rn_pkg.htm>
"))



;;; Variables

(defparameter *keyword-package*           nil) 
(defparameter *common-lisp-package*       nil)
(defparameter *common-lisp-user-package*  nil)
(defvar *package* nil
  "
The current package.

URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm>
")



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


(defgeneric package-error-package (package-error)
  (:documentation "
RETURN: The package in error.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_er.htm>
"))

(define-condition package-error (error)
  ((package :initarg :package :reader package-error-package))
  (:report (lambda (condition stream)
             (format stream "Package error with ~A" (package-error-package condition))))
(:documentation "
The type package-error consists of error conditions related to operations on packages. 
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/e_pkg_er.htm>
"))


(define-condition simple-package-error (package-error simple-error-mixin)
  ())

(define-condition package-exists-error (simple-package-error)
  ()
  (:documentation "The error condition signaling that a package with the same name already exists."))

(define-condition package-does-not-exist-error (simple-package-error)
  ()
  (:documentation "The error condition signaling that no package with that name exists."))

(defgeneric symbol-conflict-existing-symbol (error)
  (:documentation "RETURN: The existing symbol in conflict."))

(defgeneric symbol-conflict-imported-symbol (error)
  (:documentation "RETURN: The imported symbol in conflict."))

(define-condition symbol-conflict-error (simple-package-error)
  ((existing-symbol :initarg :existing-symbol :reader symbol-conflict-existing-symbol)
   (imported-symbol :initarg :imported-symbol :reader symbol-conflict-imported-symbol))
  (:report (lambda (condition stream)
             (format stream "The would-be imported symbol ~S conflicts with the existing symbol ~S in the package ~S"
                     (symbol-conflict-imported-symbol condition)
                     (symbol-conflict-existing-symbol condition)
                     (package-name (package-error-package condition)))))
(:documentation "The error condition signaling a symbol conflict."))

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






;;; Implementation of syms

(defgeneric symbol-plist (symbol)
(:documentation "
RETURN: The plist of the symbol.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_4.htm>
"))

(defgeneric symbol-value (symbol)
(:documentation "
RETURN: The value of the symbol.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_5.htm>
"))

(defgeneric symbol-function (symbol)
(:documentation "
RETURN: The function of the symbol.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_1.htm>
"))

(defclass symbol ()
  ((name
    :initarg :name
    :reader symbol-name)
   (pack
    :initarg :pack
    :reader symbol-package
    :accessor sym-pack)
   (plist
    :initarg :plist
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
   :pack nil)
  (:documentation "
The symbol class.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm>
"))

(defgeneric symbolp (object)
  (:method ((object t))      nil)
  (:method ((object symbol)) t)
  (:documentation "
RETURN: Whether the object is a symbol.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_symbol.htm>
"))


(defgeneric boundp (object)
  (:method ((object t))
    (error 'type-error :datum object :expected-type 'symbol))
  (:method ((object symbol))
    (slot-boundp object 'value))
  (:documentation "
RETURN: Whether the symbol is bound to a value.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_boundp.htm>
"))

(defgeneric fboundp (object)
  (:method ((object t))
    (error 'type-error :datum object :expected-type 'symbol))
  (:method ((object symbol))
    (slot-boundp object 'function))
  (:documentation "
RETURN: Whether the symbol is fbound to a function.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_fbound.htm>
"))


(defclass keyword (symbol)
  ()
  (:documentation "
The keyword class.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/t_kwd.htm>
"))


(defgeneric keywordp (object)
  (:method ((object t))       nil)
  (:method ((object keyword)) t)
  (:documentation "
RETURN: Whether the object is a keyword.     
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_kwdp.htm>
"))


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
  "
RETURN: A fresh list of all registered packages.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_list_a.htm>
"
  (let ((packages '()))
    (maphash (lambda (k v) (declare (ignore k)) (pushnew v packages)) *packs*)
    packages))

(defgeneric package-documentation (package)
  (:documentation "RETURN: The documentation string of the package."))
(defgeneric package-nicknames (package)
  (:documentation "RETURN: The list of nicknames of the package."))

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
    :used-by-packs nil)
  (:documentation "
The package class.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm>
"))

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
  (:method ((package package)) t)
  (:documentation "
RETURN: Whether the object is a package.     
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_pkgp.htm>
"))


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








(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))

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
                                           (present-table package)))
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


(defmacro zdo-external-symbols ((var pack) &body body)
  `(tmap-syms (lambda (,var)
                ,@body)
              (external-table ,pack)))

(defmethod check-inherit-conflict (used-pack using-pack)
  (zdo-external-symbols (inherited-sym used-pack)
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
  "
DO:     Sets the current *package* to the package designated by NAME.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_in_pkg.htm>
"
  (let ((name (normalize-string-designator name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((new-package (normalize-package-designator
                           ,name :if-package-does-not-exist :ignore-or-replace)))
         (when new-package
           (setf *package* new-package))))))

;; To test:
;; (cl-user::cd #P"~/src/lisp/implementations/ansi-tests/") (mapc 'delete-file (directory "*.lx*")) (load "zpack-load.lsp")

;;;; THE END ;;;;
