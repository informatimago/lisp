;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package-mac.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file contains the macros.
;;;;
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
;;;;    Copyright (c) 2012 Pascal J. Bourguignon <pjb@informatimago.com>, All Rights Reserved
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

(define-modify-macro appendf (&rest args) append "Append onto list")


(defmacro with-package-iterator ((name package-list-form &rest symbol-types)
                                 &body declarations-body)
  "
DO:     Within the lexical scope of the body forms, the name is
        defined via macrolet such that successive invocations of
        (name) will return the symbols, one by one, from the packages
        in package-list.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_w_pkg_.htm>
"
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
                       (return ,result-form))))))))))

  );;eval-when


(defmacro do-symbols         ((var &optional package result-form) &body body)
  "
DO:     Iterate over all the symbols of the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm>
"
  (generate-do-symbols-loop var package result-form body '(:internal :external :inherited)))


(defmacro do-external-symbols ((var &optional package result-form) &body body)
  "
DO:     Iterate over all the external symbols of the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm>
"
  (generate-do-symbols-loop var package result-form body '(:external)))


(defmacro do-all-symbols      ((var &optional result-form) &body body)
  "
DO:     Iterate over all the symbols of all the packages.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm>
"
  (generate-do-symbols-loop var '(list-all-packages) result-form body '(:internal :external :inherited)))


(defmacro defpackage (defined-package-name &rest options)
  "
DO:     Define a new package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm>
"
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
      `(eval-when (:execute :load-toplevel #-mocl :compile-toplevel)
         (%define-package ',(normalize-string-designator defined-package-name :if-not-a-string-designator :replace)
                          ',shadows
                          ',shadowing-imports
                          ',(extract-packages   :use)
                          ',import-froms
                          ',interns
                          ',exports
                          ',(extract-one-string :documentation)
                          ',(extract-strings    :nicknames))))))


;;;; THE END ;;;;
