;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               analyse-patchwork.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Read and analyse the patchwork sources.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.TREE-TO-ASCII"))
(defpackage "COM.INFORMATIMAGO.TOOLS.ANALYSE-PATCHWORK"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH"
        "COM.INFORMATIMAGO.TOOLS.DEPENDENCY-CYCLES"
        "COM.INFORMATIMAGO.TOOLS.ASDF-FILE")
  (:export "READ-SOURCES" "*CONTENTS*")
  (:shadow "CLASS" "CLASS-NAME" "FIND-CLASS"))
(in-package "COM.INFORMATIMAGO.TOOLS.ANALYSE-PATCHWORK")

(defun safe-find-package (designator)
  (or (cl:find-package designator)
      (error "No such package ~S" designator)))


(defvar *readtable-preserve*)

(defun sharp-underline-dispatch-reader-macro (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((*readtable* *readtable-preserve*)
        (*package* (safe-find-package "FFI")))
    ;; actual would return the name of a FFI function, variable or #define,
    ;; we just return the preserved symbol, read in the FFI package..
    (values (read stream))))

(defun sharp-at-dispatch-reader-macro (stream subchar arg)
  "#@(x y) reads a Point.  We just read a quoted list."
  (declare (ignore subchar arg))
  (destructuring-bind (h v) (read stream)
    (values (dpb (ldb (byte 16 0) (round v)) (byte 16 16) (ldb (byte 16 0) (round h))))))

(defun sharp-dot-dispatch-reader-macro (stream subchar arg)
  "#. we read the expression in the host CL."
  (declare (ignore subchar arg))
  (let ((form (cl:read stream)))
    (if *read-eval*
        (values (eval form))
        (error "Cannot evaluate #.~S when ~S is ~S"
               form '*read-eval* *read-eval*))))

(defun sharp-i-dispatch-reader-macro (stream char count)
  (declare (ignore char count))
  `(prefix-expr ',(read stream t nil t)))

(defun setup ()
  "
Configure a com.informatimago.common-lisp.lisp-reader.reader:readtable
for reading lisp sources interning packages and symbols in
com.informatimago.common-lisp.lisp-reader.package:package and
com.informatimago.common-lisp.lisp-reader.package:symbol instead of
common-lisp:package and common-lisp:symbol.
"
  (setf *read-eval* t)
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character  #\# #\_ (function sharp-underline-dispatch-reader-macro) rt)
    (set-dispatch-macro-character  #\# #\$ (function sharp-underline-dispatch-reader-macro) rt)
    (set-dispatch-macro-character  #\# #\@ (function sharp-at-dispatch-reader-macro)        rt)
    (set-dispatch-macro-character  #\# #\. (function sharp-dot-dispatch-reader-macro)       rt)
    (set-dispatch-macro-character  #\# #\i (function sharp-i-dispatch-reader-macro)         rt)
    (setf *readtable-preserve* (copy-readtable rt))
    (setf (readtable-case  *readtable-preserve*) :preserve)
    (setf *readtable* rt)))



(defparameter *sorted-files* '())
(defparameter *contents*     '())
(defvar *asdf-files* (make-hash-table))

(defun read-sources (&key (system-file #P"patchwork.asd")
                     (base #P"~/works/patchwork/patchwork/src/"))
  (setup)
  (LOAD-SIMPLE-ASD-FILE (merge-pathnames system-file base))
  (setf *sorted-files* (reverse (topological-sort (hash-table-values *asdf-files*)
                                           (function dependencies))))
  ;; (defparameter *sources*
  ;;   (sort (directory "/home/pjb/works/patchwork/pw-src/**/*.lisp")
  ;;         (function string<) :key (function namestring)))
  ;; (setf *sources*
  ;;       (let* ((files '())
  ;;              (path (merge-pathnames #P"patchwork.asd" base))
  ;;              (system (with-open-file (stream path) (read stream))))
  ;;         (dolist (compo (getf (cddr system) :components) (reverse files))
  ;;           (when (and (listp compo)
  ;;                      (eq :file (first compo)))
  ;;             (push (second compo) files)))))
  (setf  *contents* '())
  (handler-case
      (dolist (file  *sorted-files*)
        (let ((*package* *package*)
              (path (asdf-file-path file)))
          (push (cons path
                      (with-open-file (stream (merge-pathnames (make-pathname :type "LISP" :case :common)
                                                               (merge-pathnames path base)))
                        (princ (namestring (truename (pathname stream)))) (terpri) (finish-output)
                        (loop
                          :for sexp = (read stream nil stream)
                          :until (eql stream sexp)
                          :collect sexp
                          :do (when (listp sexp)
                                (case (first sexp)
                                  ((defpackage in-package import export use-package)
                                   (eval sexp)))))))
                *contents*)))
    (error (err)
      (terpri) (princ err) (terpri)))
  (values))


(defun find-forms (operator sexp)
  (cond
    ((atom sexp)
     '())
    ((eq operator (first sexp))
     (list sexp))
    (t
     '())))

(defun find-all-forms (operator)
  (mapcan (lambda (file)
            (let ((forms (mapcan (lambda (sexp) (find-forms operator sexp))
                                 (cdr file))))
              (if forms
                  (list (cons (car file) forms))
                  '())))
          *contents*))

;; (read-sources)
;; (find-all-forms 'eval-when)




(defstruct class
  name
  file
  superclasses
  all-superclasses
  subclasses)


;; (apropos "class-direct-subclasses")


(defparameter *classes* (make-hash-table))
(defun find-class (name) (gethash name *classes*))
(defun intern-class (name file superclasses &optional subclasses)
  (or (gethash name *classes*)
      (setf (gethash name *classes*)
            (make-class :name name
                        :file file
                        :superclasses superclasses
                        :subclasses subclasses))))

(defmethod adjacency-list ((class class))
  (mapcar (function find-class) (class-superclasses class)))
(defmethod reachable-list ((class class))
  (or (class-all-superclasses class)
      (setf (class-all-superclasses class)
            (compute-closure (function class-superclasses) (class-superclasses class)))))
(defun dependencies  (p q) (member q (reachable-list p)))

(defvar *defclasses* nil)
(defvar *classes* nil)

(defun classify-classes ()
  (maphash (lambda (name class)
               (declare (ignore name))
             (setf (class-superclasses class)
                   (mapcan (lambda (name)
                               (let ((superclass (find-class name)))
                                 (if superclass
                                   (list superclass)
                                   (format t "~A inherits from ~A that has no defclass form.~%"
                                           (class-name class) name))))
                           (class-superclasses class)))
             (dolist (super (class-superclasses class))
               (push class (class-subclasses super))))
           *classes*))


(defun analyse-patchwork ()
  (read-sources :system-file #P"patchwork.asd"
                :base #P"~/works/patchwork/patchwork/src/")
  (setf *defclasses* (cons '("root" (defclass root () ()))
                           (find-all-forms 'defclass)))
  (loop
    :for (file . classes) :in *defclasses*
    :initially (setf *classes* (make-hash-table))
    :do (loop
          :for (nil name superclasses) :in classes
          :for cls = (find-class name)
          :do (if cls
                  (format t "~A names two classes, one in ~S and one in ~S~%"
                          name file (class-file cls))
                  (intern-class name file (or superclasses
                                              (if (eq 'root name)
                                                  '()
                                                  '(root))))))
    :finally (classify-classes))
  (report-problems (hash-table-values *classes*)))

(defvar *tlee* nil) ; for debugging.

(defun print-class-hierarchy ()
  (let ((tlee  (let ((nodes (make-hash-table)))
                 (labels ((tlee (class)
                            (setf (gethash (class-name class) nodes)
                                  (cons (class-name class)
                                        (mapcar (function tlee) (class-subclasses class))))))
                   (dolist (class (topological-sort (hash-table-values *classes*)
                                                    (function dependencies)))
                     (tlee class)))
                 (or (gethash 'root nodes)
                     (when (cl:find-package "OBJC")
                       (gethash (find-symbol "OBJC-OBJECT" (cl:find-package "OBJC")) nodes))
                     (when (cl:find-package "NS")
                       (gethash (find-symbol "NS-OBJECT" (cl:find-package "NS")) nodes)))))
        (*package* (or (cl:find-package :pw)
                       (safe-find-package :cl-user))))
    (setf *tlee* tlee)
    (princ
     (com.informatimago.common-lisp.picture.tree-to-ascii:tree-to-ascii
      tlee
      :boxed t
      ;; :format-fun
      ;; :background
      ;; :to-length
      ;; :from-length
      ))))


(defun print-class-parents ()
  (dolist (leaf (remove-if (function class-subclasses) (hash-table-values *classes*)) (terpri))
    (terpri)
    (let ((tlee (let ((nodes (make-hash-table)))
                  (labels ((tlee (class)
                             (setf (gethash (class-name class) nodes)
                                   (cons (class-name class)
                                         (remove '(root) (mapcar (function tlee) (class-superclasses class))
                                                 :test (function equal))))))
                    (tlee leaf))
                  (gethash (class-name leaf) nodes)))
          (*package* (or (cl:find-package :pw)
                         (safe-find-package :cl-user))))
      (princ
       (com.informatimago.common-lisp.picture.tree-to-ascii:tree-to-ascii
        tlee
        :boxed t
        ;; :format-fun
        ;; :background
        ;; :to-length
        ;; :from-length
        )))))

(defun convert-class-tree (clos-class)
  (when clos-class
    (let* ((clos-class   (if (symbolp clos-class)
                           (cl:find-class clos-class)
                           clos-class))
           (superclasses (closer-mop:class-direct-superclasses clos-class))
           (subclasses   (closer-mop:class-direct-subclasses   clos-class)))
      (intern-class (cl:class-name clos-class)
                    nil
                    (mapcar (function cl:class-name) superclasses)
                    ;; (mapcar (function cl:class-name) subclasses)
                    )
      (dolist (subclass subclasses)
        (convert-class-tree subclass)))))


(defun print-objc-class-hierarchy ()
  (setf *classes* (make-hash-table))
  #+#.(cl:if (cl:find-package "NS") '(:and) '(:or))
  (convert-class-tree 'ns:ns-object)
  (classify-classes)
  (print-class-hierarchy))

(defparameter *utf-8* #-clisp :utf-8 #+clisp charset:utf-8)

(defun generate-classes-hierarchy ()
  (with-open-file (*standard-output* "classes-hierarchy.txt"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format *utf-8*)
    (write-line "-*- mode:view; coding:utf-8 -*-")
    (terpri)
    (print-class-hierarchy))

  (with-open-file (*standard-output* "classes-parents.txt"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format *utf-8*)
    (write-line "-*- mode:view; coding:utf-8 -*-")
    (terpri)
    (print-class-parents))
  (values))




;; (last (topological-sort (hash-table-values *classes*) (function dependencies)))
;; (#1=#S(class :name root :file "root" :superclasses #2=(#1#) :all-superclasses #2#))



;; (setf *sorted-files* (reverse (topological-sort (hash-table-values *asdf-files*)
;;                                            (function dependencies))))

;; (first  *defclasses*)
;; ("pw-music/boxes/edit/mn-editor-polif" (defclass patch-work::c-patch-polifmn (patch-work::c-patch-application) ((patch-work::chord-line-list :initform nil :initarg :chord-line-list :accessor patch-work::chord-line-list))))



;; (loop
;;   :with externals = '()
;;   :for (file . defpacks) :in (find-all-forms 'defpackage)
;;   :for importing-defpacks = (remove-if-not
;;                              (lambda (defpack)
;;                                (let ((entry (assoc :import-from (cddr defpack))))
;;                                  (and entry
;;                                       (or (string= "PATCH-WORK" (second entry))
;;                                           (string= "PW "        (second entry))))))
;;                              defpacks)
;;   :when importing-defpacks
;;   :do (progn
;;         (print file)
;;         (dolist (entry (mapcar (lambda (defpack)
;;                                  (assoc :import-from (cddr defpack)))
;;                                importing-defpacks))
;;           (alexandria:appendf externals (cddr entry))))
;;   :finally (print `(:export ,@(sort (remove-duplicates externals :test (function string=))
;;                                     (function string<)))))

;; (read-sources)
;; (find-all-forms 'export)
;; nil
;; (find-all-forms 'import)
;; (("pw-lib/epw-1.0b/import" (import '(patch-work:new-menu patch-work:pw-addmenu))))
;; nil

;;;; THE END ;;;;
