;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               relative-package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements Allegro CL-like relative packages.
;;;;    http://franz.com/support/documentation/8.1/doc/packages.htm#relative-2
;;;;
;;;;    Note: |..foo| won't be read as a relative package name.
;;;;          .|.foo| will be read as the relative package name ..|foo|.
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-11-01 <PJB> Created.
;;;;BUGS
;;;;
;;;;    Doesn't handle escapes in dotted package names!
;;;;    .|.foo|:x ..f\o\o:x ..f\:\o:x ..|f:o|:x ..f\ \o:x ..|f o|:x are broken.
;;;;
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE"

  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")

  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
                "READTABLE-PARSE-TOKEN"
                "SYMBOL-IN-MISSING-PACKAGE-ERROR"
                "SYMBOL-MISSING-IN-PACKAGE-ERROR"
                "UNEXPORTED-SYMBOL-ERROR"
                ;; temporarily: (will have to export our own restart symbols).
                "INTERN-HERE" "RETURN-UNINTERNED"

                "INVALID-SYMBOL-COMPONENT-LIST" "INTERNAL-SYMBOL"
                "MISSING-SYMBOL" "MISSING-PACKAGE"
                "MAKE-SYMBOL-PARSER-FUNCTION" "MAKE-TOKEN-PARSER")

  (:shadow . #1=("FIND-PACKAGE"
                 "MAKE-PACKAGE" "DELETE-PACKAGE"
                 "FIND-SYMBOL" "IMPORT" "INTERN" "SHADOW" "SHADOWING-IMPORT"
                 "EXPORT" "UNEXPORT" "UNINTERN" "USE-PACKAGE"
                 "UNUSE-PACKAGE" "PACKAGE-NAME" "PACKAGE-NICKNAMES"
                 "PACKAGE-USE-LIST" "PACKAGE-USED-BY-LIST" "PACKAGE-SHADOWING-SYMBOLS"
                 "RENAME-PACKAGE"
                 "WITH-PACKAGE-ITERATOR"
                 "DO-SYMBOLS" "DO-EXTERNAL-SYMBOLS"
                 "DEFPACKAGE" "IN-PACKAGE"))

  (:export "ENABLE-RELATIVE-PACKAGE-NAMES"
           "DISABLE-RELATIVE-PACKAGE-NAMES"
           "PACKAGE-DESIGNATOR" "PACKAGE-PARENT" "PACKAGE-CHILDREN"
           "*DISABLE-USELESS-PARENT-PACKAGE-CHECK*"
           . #1#))

(defpackage "COMMON-LISP-WITH-RELATIVE-PACKAGE"
  (:nicknames "CL-RP")
  (:documentation "This is a package like COMMON-LISP, but with relative package names.")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE"
                          . #1=("FIND-PACKAGE"
                                "MAKE-PACKAGE" "DELETE-PACKAGE"
                                "FIND-SYMBOL" "IMPORT" "INTERN" "SHADOW" "SHADOWING-IMPORT"
                                "EXPORT" "UNEXPORT" "UNINTERN" "USE-PACKAGE"
                                "UNUSE-PACKAGE" "PACKAGE-NAME" "PACKAGE-NICKNAMES"
                                "PACKAGE-USE-LIST" "PACKAGE-USED-BY-LIST" "PACKAGE-SHADOWING-SYMBOLS"
                                "RENAME-PACKAGE"
                                "WITH-PACKAGE-ITERATOR"
                                "DO-SYMBOLS" "DO-EXTERNAL-SYMBOLS"
                                "DEFPACKAGE" "IN-PACKAGE"))
  (:export . #.(let ((names '())) (do-external-symbols (s "COMMON-LISP" names)
                                    (push (symbol-name s) names))))
  (:export "ENABLE-RELATIVE-PACKAGE-NAMES"
           "DISABLE-RELATIVE-PACKAGE-NAMES"
           "PACKAGE-DESIGNATOR" "PACKAGE-PARENT" "PACKAGE-CHILDREN"
           "*DISABLE-USELESS-PARENT-PACKAGE-CHECK*"))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.RELATIVE-PACKAGE")

#|

Notes
==================================================

Package structure vs. name structure
--------------------------------------------------

cf. `<http://franz.com/support/documentation/8.1/doc/packages.htm#relative-2>`_

The hierarchial packages as implemented by Allegro CL, introduce an
inconsistency in the children-parent relationship between PACKAGEs.

On one one hand, RELATIVE-PACKAGE-NAME-TO-PACKAGE and PACKAGE-PARENT
enforce that the parent of a package be an existing package, and
therefore when we have two packages: "R.A1.A2.A3.X" and
"R.B1.B2.B3.Y", then cannot refer one to the other using the relative
package NAME ....B1.B2.B3.Y or ....A1.A2.A3.X if there are no package
named "R.A1.A2.A3" "R.A1.A2" "R.A1"  or "R.B1.B2.B3" "R.B1.B2" "R.B1".

On the other hand, PACKAGE-CHILDREN :RECURSE T will gladly return in
its result, packages selected on the only basis of their package NAME
having a given prefix, regardless whether THEY have a parent.
So with only the two packages "R.A1.A2.A3.X" and "R.B1.B2.B3.Y",
(package-children "R")
will return (#<package "R.A1.A2.A3.X"> #<package "R.B1.B2.B3.Y">),
but (mapcar 'package-parent (package-children "R")) will signal an
error.

Furthermore, if packages where "hierarchical", and really "have"
children, shouldn't DELETE-PACKAGE also delete the children of a
package bar leaving them parentless?

A parent-children relationship would be a run-time entity, while
refering to another package using a relative NAME would be a read-time
operation.  Do we need the former?



This package implements "bug-for-bug" Allegro's "hierarchical"
packages, but IMO, it would be better to base the operations on
package NAMES rather than on an implied or effective parent-children
relationship between PACKAGES.

For example, in Allegro's reference implementation it's indicated that
relative-package-name-to-package should be fast because used at
read-time.  Well, it would be faster if we didn't tested for the
existence of all the intermediary parent packages!

Another advantage of basing a design of relative package names only on
NAMES, is that it would be a smaller extension on the CL standard, and
therefore risking fewer unseen consequences (such as DELETE-PACKAGE
having to delete the children packages).


On the other hand, one advantage on insisting on the existence of
intermediary packages, is that it allows to create a border around
relative package pathnames, to effectively prevent refering packages
outside of a sub-hierarchy (cf. in relative-package-test.lisp how it's
done by avoiding the creating the ".TEST.NONE" package).

Relative package names are insufficient
--------------------------------------------------

Relative packages are useful to provide short names to packages that
are related.  However, when using library packages with structured
names, they are not useful, since we are actually crossing to other
package name trees: ::

    (in-package :com.ogamita.nasium-lse.os)
    (com.informatimago.common-lisp.cesarum.string:prefixp "Insu" "Insufficient!")

For this, we need local nicknames.

Local nicknames can be compiled with relative package names to imply
local nicknames for all children and grand children of the local
nicknamed packages. ::

    (in-package :com.ogamita.nasium-lse.os)
    (add-local-nickname :com.informatimago.common-lisp.cesarum :cesarum)
    (cesarum.string:prefixp "Su" "Sufficient!")

|#

(deftype package-designator () '(or package string-designator))

(defun package-name-to-package (name)
  (cl:find-package name))

(defconstant +package-separator+   #\:)
(defconstant +relative-prefix+     #\.)
(defconstant +component-separator+ #\.)
(defparameter *relative-prefix*     (string +relative-prefix+))
(defparameter *component-separator* (string +component-separator+))

(defvar *disable-useless-parent-package-check* nil)
;; (setf  *disable-useless-parent-package-check* t)

(defun resolve-relative-package-name (name &optional (*package* *package*))
  (if (and (plusp (length name))
           (char= (aref name 0) +relative-prefix+))
      (let ((base (nreverse (split-string (package-name *package*)
                                          *component-separator*))))
        (loop :for i :from 1 :below (length name)
              :while (char= (aref name i) +relative-prefix+)
              :do (pop base)
              :do (unless *disable-useless-parent-package-check*
                    ;; bug-for-bug simile of Allegro's.
                    (let ((parent (unsplit-string (reverse base) +component-separator+)))
                      (unless (package-name-to-package parent)
                        (error "The parent package ~S does not exist." parent))))
              :finally (return
                         (let ((parent (unsplit-string (nreverse base) +component-separator+)))
                           (if (< i (length name))
                               (concatenate 'string parent *component-separator* (subseq name i))
                               parent)))))
      name))

(defun relative-package-name-to-package (name &optional (*package* *package*))
  (when (and (plusp (length name))
             (char= (aref name 0) +relative-prefix+))
    (package-name-to-package (resolve-relative-package-name name *package*))))

(defgeneric find-package (package-designator)
  (:documentation "

PACKAGE-DESIGNATOR: a package designator.

RETURN:     the designated package, or NIL.

DO:         When a string designator is given,

            then if there is a package with the same *name* or
            *nickname*, it's designated,

            else if there is a package *named* by the combination of
            the designator and the current *PACKAGE* *name*, it's
            designated.

            Otherwise NIL is returned.

")
  (:method ((designator t))
    (check-type designator package-designator
                "A package designator is expected")
    (find-package designator))
  (:method ((package package))      package)
  (:method ((designator symbol))    (find-package (symbol-name designator)))
  (:method ((designator character)) (find-package (string designator)))
  (:method ((name string))
    (or (package-name-to-package name)
        (relative-package-name-to-package name))))


(defgeneric package-parent (package-designator)
  (:documentation "

SIGNAL: an ERROR if there's no direct parent package.

RETURN: the parent package of the package designated by PACKAGE-DESIGNATOR.

NOTE:   if *DISABLE-USELESS-PARENT-PACKAGE-CHECK* is true  then return
        the name of the missing parent package instead of signaling an
        error.

")
  (:method ((designator t))
    (check-type designator package-designator
                "A package designator is expected")
    (package-parent designator))
  (:method ((package package))      (package-parent (package-name package)))
  (:method ((designator symbol))    (package-parent (symbol-name designator)))
  (:method ((designator character)) (package-parent (string designator)))
  (:method ((name string))
    (let ((pos (position +component-separator+ name :from-end t)))
      (if pos
          (let ((parent (subseq name 0 pos)))
            (or (package-name-to-package parent)
                (if *disable-useless-parent-package-check*
                    parent
                    (error "The parent of ~a does not exist." name))))
          (error "There is no parent of ~a." name)))))

(defgeneric package-children (package-specifier &key recurse)
  (:documentation "

RETURN: A list of all the child packages of the package designated by
        PACKAGE-DESIGNATOR.  If RECURSE is NIL, then only the direct
        children are listed.

NOTE:   The current implementation uses a prefix filter on the name of
        packages, so with RECURSE set, we return grandchildren even if
        there's no intermediary package.

")
  (:method ((designator t)         &key (recurse t))
    (check-type designator package-designator
                "A package designator is expected")
    (package-children designator :recurse recurse))
  (:method ((package package)      &key (recurse t))
    (package-children (package-name package) :recurse recurse))
  (:method ((designator symbol)    &key (recurse t))
    (package-children (symbol-name designator) :recurse recurse))
  (:method ((designator character) &key (recurse t))
    (package-children (string designator) :recurse recurse))
  (:method ((name string)          &key (recurse t))
    (let ((prefix (concatenate 'string name *component-separator*)))
      (remove-if-not (lambda (package)
                       (let ((pname (package-name package)))
                         (and (prefixp prefix pname)
                              (or recurse
                                  (not (position +component-separator+ pname
                                                 :start (length prefix)))))))
                     (list-all-packages)))))

(defun normalize-designator (designator)
  (if (typep designator 'string-designator)
      (resolve-relative-package-name (string designator))
      designator))

(defmacro define-normalize-and-forward-package-methods (name &key (type-error nil))
  (let ((cl-name (cl:intern (string name) (load-time-value (cl:find-package "COMMON-LISP")))))
    `(progn
       (defgeneric ,name (name))
       ,@ (if type-error
              `((defmethod ,name ((name t))  (error 'simple-type-error
                                                    :datum name
                                                    :expected-type 'package-designator
                                                    :format-control "~S called with a non ~S: ~S"
                                                    :format-arguments (list ',name 'package-designator name)))
                (defmethod ,name ((name package)) (,cl-name name)))
              `((defmethod ,name (name)           (,cl-name name))))
          (defmethod ,name ((name character)) (,name (string name)))
          (defmethod ,name ((name symbol))    (,name (string name)))
          (defmethod ,name ((name string))
            ;; We don't have here the same sophisticated normalize
            ;; function as in
            ;; "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.PACKAGE" so we
            ;; defer handling wrong package names to the CL function.
            (,cl-name (normalize-designator name))))))

(define-normalize-and-forward-package-methods delete-package)
(define-normalize-and-forward-package-methods package-name)
(define-normalize-and-forward-package-methods package-nicknames)
(define-normalize-and-forward-package-methods package-use-list)
(define-normalize-and-forward-package-methods package-used-by-list)
(define-normalize-and-forward-package-methods package-shadowing-symbols)


(defgeneric make-package (pack-name &key nicknames use)
  (:method ((pack-name character) &key (nicknames '()) (use '()))
    (make-package (string pack-name) :nicknames nicknames :use use))
  (:method ((pack-name symbol) &key (nicknames '()) (use '()))
    (make-package (string pack-name) :nicknames nicknames :use use))
  (:method ((pack-name string) &key (nicknames '()) (use '()))
    (cl:make-package (resolve-relative-package-name pack-name)
                     :nicknames nicknames
                     :use (mapcar (function normalize-designator) use))))

(defgeneric rename-package (package new-name &optional new-nicknames)
  (:method (package new-name &optional new-nicknames)
    (cl:rename-package package new-name new-nicknames))
  (:method ((pack-name character) new-name &optional new-nicknames)
    (rename-package (string pack-name) new-name new-nicknames))
  (:method ((pack-name symbol) new-name &optional new-nicknames)
    (rename-package (string pack-name) new-name new-nicknames))
  (:method ((pack-name string) new-name &optional new-nicknames)
    (cl:rename-package (normalize-designator pack-name) new-name new-nicknames)))



;; Using normalize-designator in the following functions defer
;; handling the undefined packages to the CL functions:

(defgeneric find-symbol (sym-name &optional pack)
  (:method (sym-name &optional (pack *package*))
    (cl:find-symbol sym-name (normalize-designator pack))))

(defgeneric intern (sym-name &optional pack)
  (:method (sym-name &optional (pack *package*))
    (cl:intern sym-name (normalize-designator pack))))

(defgeneric unintern (symbol &optional pack)
  (:method (symbol &optional (pack *package*))
    (cl:unintern symbol (normalize-designator pack))))

(defgeneric import (symbols &optional pack)
  (:method (symbols &optional (pack *package*))
    (cl:import symbols (normalize-designator pack))))

(defgeneric export (symbols &optional pack)
  (:method (symbols &optional (pack *package*))
    (cl:export symbols (normalize-designator pack))))

(defgeneric unexport (symbols &optional pack)
  (:method (symbols &optional (pack *package*))
    (cl:unexport symbols (normalize-designator pack))))

(defgeneric shadow (symbols &optional pack)
  (:method (symbols &optional (pack *package*))
    (cl:shadow symbols (normalize-designator pack))))

(defgeneric shadowing-import (symbols &optional pack)
  (:method (symbols &optional (pack *package*))
    (cl:shadowing-import symbols (normalize-designator pack))))

(defgeneric use-package (packs &optional using-pack)
  (:method (packs &optional (using-pack *package*))
    (cl:use-package (mapcar (function normalize-designator) packs)
                    (normalize-designator using-pack))))

(defgeneric unuse-package (packs &optional using-pack)
  (:method (packs &optional (using-pack *package*))
    (cl:unuse-package (mapcar (function normalize-designator) packs)
                      (normalize-designator using-pack))))


;;; Macros:

(defmacro in-package (name)
  (if (typep name 'string-designator)
      `(cl:in-package ,(normalize-designator name))
      `(cl:in-package ,name)))

(defmacro with-package-iterator ((name package-list-form &rest symbol-types)
                                 &body declarations-body)
  `(cl:with-package-iterator (,name (mapcar (function normalize-designator)
                                            ,package-list-form)
                                    ,@symbol-types)
     ,@declarations-body))

(defmacro do-symbols ((var &optional package result-form) &body body)
  `(cl:do-symbols (,var ,(if package
                             `((normalize-designator ,package))
                             '*package*)
                    ,result-form)
     ,@body))

(defmacro do-external-symbols ((var &optional package result-form) &body body)
  `(cl:do-external-symbols (,var ,(if package
                                      `((normalize-designator ,package))
                                      '*package*)
                             ,result-form)
     ,@body))

(defmacro defpackage (defined-package-name &rest options)
"
DO:     Like CL:DEFPACKAGE, but pre-processes the package names in
        those clauses:

            (:use package-name*)* |
            (:shadowing-import-from package-name {symbol-name}*)* |
            (:import-from package-name {symbol-name}*)* |

NOTE:   Since relative package names in those closes are resolved at
        macro-expansion time, the refered relative packages and all
        the parents in their path must exist at macro-expansion-time.
"
  `(cl:defpackage ,defined-package-name
     ,@(mapcar (lambda (option)
                 (if (atom option)
                     option
                     (case (first option)
                       (:use
                        `(:use ,@(mapcar (function normalize-designator)
                                         (rest option))))
                       ((:shadowing-import-from :import-from)
                        `(,(first option) ,(normalize-designator (second option))
                          ,@(cddr option)))
                       (otherwise option))))
               options)))

;;; Dot reader macro.


(defun symbol-from-split-token (components)
  "
DO:          Same as .READER:SYMBOL-FROM-SPLIT-TOKEN, but use relative
             package functions instead of CL ones.

COMPONENTS:  a list of strings separated by integers specifying the
             number of colons.

EXAMPLES:    X         (\"X\")
             :Y        (1 \"Y\")
             X:Y       (\"X\" 1 \"Y\")
             X::Y      (\"X\" 2 \"Y\")
             X:::Y     (\"X\" 3 \"Y\")
             X::       (\"X\" 2)
             X:Y:Z     (\"X\" 1 \"Y\" 1 \"Z\")

RETURN:      A symbol designated by the components,
             or signal an error.

NOTE:        This function implements the standard semantics,
             where only one occurence of : or :: is allowed,
             and depending on : or ::, an exported symbol is expected
             or not.

"
  (values
   (case (length components)
     (1
      (if (stringp (first components))
          (intern (first components) *package*)
          (invalid-symbol-component-list components)))
     (2 (case (first components)
          ((1 2)
           (intern (second components)
                   (load-time-value (find-package "KEYWORD"))))
          (otherwise
           (invalid-symbol-component-list components))))
     (3 (destructuring-bind (pname colons sname) components
          (assert (stringp pname) (pname) "Symbol component was expected to be a string.")
          (assert (stringp sname) (sname) "Symbol component was expected to be a string.")
          (let ((package (find-package pname))) ; *** this is the critical call for relative packages.
            (if package
                (case colons
                  (1 (multiple-value-bind (sym where) (find-symbol sname package)
                       (case where
                         ((nil)       (missing-symbol  package sname))
                         ((:internal) (internal-symbol package sname sym))
                         ((:external) sym))))
                  (2 (intern sname package))
                  (otherwise
                   (invalid-symbol-component-list components)))
                (missing-package pname sname)))))
     (otherwise
      (invalid-symbol-component-list components)))))

(defparameter *dot-reader-readtable*
  (let ((rt (com.informatimago.common-lisp.lisp-reader.reader:copy-readtable nil)))
    (setf (readtable-parse-token rt)
          (make-token-parser :parse-symbol-token (make-symbol-parser-function
                                                  (function symbol-from-split-token))))
    rt)
  "
Note: a COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:READTABLE,
not a CL:READTABLE.
")



(declaim (inline terminating-macro-character-p whitespacep))

(defun terminating-macro-character-p (char &optional (*readtable* *readtable*))
  (multiple-value-bind (fun non-terminating-p) (get-macro-character char *readtable*)
    (and fun (not non-terminating-p))))

(defun whitespacep (char &optional (*readtable* *readtable*))
  ;; Hard to cache since the whitespace[2] status of a character can
  ;; be changed at any time in a readtable with SET-SYNTAX-FROM-CHAR.
  (and (not (get-macro-character char *readtable*))
       (equal '(:x :x)
              (ignore-errors
               (let ((*package* (load-time-value (find-package "KEYWORD"))))
                 (read-from-string (format nil "(X~CX)" char)
                                   nil nil))))))

#-(and)
(loop for c below char-code-limit
      for ch = (code-char c)
      when (and ch (whitespacep ch))
        collect ch)


(defun dot-reader-macro (stream dot)
  (let ((buffer (make-array 64 :element-type 'character
                               :fill-pointer 0
                               :adjustable t)))
    ;; .     -> ?
    ;; ..    -> Illegal symbol syntax in "..".
    ;; .x    -> symbol in *package*
    ;; .x::y -> symbol in relative package
    ;; ...:y -> symbol in relative package
    ;; .42   -> float
    ;; .42:y -> symbol in relative package

    ;; TODO: we'd have to implement the whole lisp reader algorithm,
    ;;       notably to handle escape sequences.  Perhaps we could
    ;;       export from our lisp reader the core returning a token,
    ;;       before parsing it.  Then we could do simply:
    ;;          (funcall (..reader:readtable-parse-token rt)
    ;;                   (..reader:read-token stream rt))
    ;;
    ;;       Alternatively, see if we couldn't transfer the reader macros
    ;;       from the host readtable to our reader readtable, so that we
    ;;       may just use (..reader:read stream).
    ;;
    ;;       Also broken: single dot in dotted lists cannot be read
    ;;       anymore, once we have a reader macro on #\. We deal with
    ;;       it in LIST-READER-MACRO, but this could be avoided if we
    ;;       didn't have to write a reader macro for #\.

    (vector-push-extend dot buffer)
    (loop
      :for ch := (read-char stream nil nil)
      :until (or (null ch)
                 (whitespacep ch)
                 (terminating-macro-character-p ch))
      :do (vector-push-extend ch buffer)
      :finally (unread-char ch stream))
    (let ((com.informatimago.common-lisp.lisp-reader.reader:*readtable* *dot-reader-readtable*))
      (values (com.informatimago.common-lisp.lisp-reader.reader:read-from-string buffer)))))

#-(and)
(progn

  (let ((token "...string:prefixp"))
    (check eql (with-input-from-string (in token)
                 (dot-reader-macro in (read-char in)))
           'prefixp))


  (mapcar (lambda (token)
            (with-input-from-string (in token)
              (dot-reader-macro in (read-char in))))
          '("...string:prefixp"
            ".X" ".TEST::HOO"
            "..:ADD-NICKNAME"
            ".42"
            "..relative:enable-relative-package-names"))


  (dolist (token '(" " "..."))
    (expect-condition 'error (with-input-from-string (in token)
                               (dot-reader-macro in (read-char in))))))


(defun list-reader-macro (stream open)
  (declare (ignore open))
  (loop
    :with result := (cons nil nil)
    :with tail   := result
    :for ch := (peek-char t stream t nil t) ; skip spaces.
    :do (case ch

          ((#\))
           (read-char stream t nil t)
           (return-from list-reader-macro (cdr result)))

          ((#\.)
           (read-char stream t nil t)
           (if (let ((next (peek-char nil stream nil nil t)))
                 (or (null next)
                     (whitespacep next)
                     (terminating-macro-character-p next)))
               (progn                   ; dot
                 (setf (cdr tail) (read stream t nil t))
                 (let ((final (peek-char t stream t nil t)))
                   (unless (eql final #\))
                     (error "Invalid syntax after dot in dotted list ~S; found ~C"
                            (cdr result) final))
                   (read-char stream t nil t))
                 (return-from list-reader-macro (cdr result)))
               (progn
                 (unread-char ch stream)
                 (setf (cdr tail) (cons (read stream t nil t) nil)
                       tail (cdr tail)))))

          (otherwise
           (let ((mac (get-macro-character ch)))
             (if mac
                 ;; When we have a reader macro, we must call it ourselves,
                 ;; to check whether it reads an object or not, so we may
                 ;; try again.
                 (let ((item (multiple-value-list (funcall mac stream (read-char stream t nil t)))))
                   (unless (null item)
                     (setf (cdr tail) (cons (first item) nil)
                           tail (cdr tail))))
                 ;; if not a reader macro, we can use read to proceed:
                 (setf (cdr tail) (cons (read stream t nil t) nil)
                       tail (cdr tail))))))))

(defmacro enable-relative-package-names ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-macro-character +relative-prefix+ (function dot-reader-macro) t *readtable*)
     (when (char= +relative-prefix+ #\. )
       (set-macro-character #\( (function list-reader-macro) nil *readtable*))))

(defmacro disable-relative-package-names ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-macro-character +relative-prefix+ nil t *readtable*)))

;;;; THE END ;;;;

#|

1- The aforementioned package-inferred-system extension to ASDF, which
is standard since ASDF 3.1 (from May 2014, now included with all
modern maintained CL implementations, which includes ABCL, Allegro,
CCL, CMUCL, ECL, LispWorks, MKCL, SBCL, but not the also supported
CLISP, CormanCL, GCL, Genera, MCL, MOCL, SCL, XCL).
https://common-lisp.net/project/asdf/asdf/The-package_002dinferred_002dsystem-extension.html#The-package_002dinferred_002dsystem-extension
It works well, is actually used in production by several systems, and
does actually provide a more maintainable package discipline than the
traditional "everything in one big package".

2- My experimental system package-renaming, which allows you to
portably rename packages around some files, though it comes with some
practical limitations.
https://git.common-lisp.net/frideau/package-renaming
I haven't actually used it, but it can portably implement local
package nicknames, if you really want them.

|#
