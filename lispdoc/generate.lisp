;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the generator API and the main generation routine.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-04 <PJB> Extracted from lispdoc.
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
;;;;    
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;    
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;    
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;    
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(in-package "COM.INFORMATIMAGO.LISPDOC.GENERATE")


#|

Some targets (like RST-DOCUMENTATION) will generate all the documentation in a single file,
with few navigation features (it may still include Table of Contents,
and cross section links), while some other targets (like HTML-DOCUMENTATION) will generate the
documentation by spreading it over several files.  

|#

(defgeneric render (doc target)
  (:documentation "Generate the representation of the DOC structure on the TARGET."))


(defgeneric generate-begin (target)
  (:method (target) target)
  (:documentation "Called before generation for the TARGET.
Can be used to open files and other initialization."))

(defgeneric generate-end   (target)
  (:method (target) target)
  (:documentation "Called before generation for the TARGET.
Can be used to close files and other terminations."))

(defgeneric generate-introduction (target)
  (:documentation "Generate the introduction of the documentation.
It may include a menu to the various indices."))


(defgeneric package-navigation-menu (target current-page &optional navigation-menu)
  (:documentation "

RETURN:     The list concatenation of the NAVIGATION-MENU and entries
            required to navigate from the CURRENT-PAGE.

NAVIGATION-MENU:
            The current navigation menu; If not provided then
            (NAVIGATION TARGET) is used instead.

")
  (:method (target current-page &optional (entries nil entriesp))
    (flet ((shorten (pname)
             (subseq pname (or (position #\. pname
                                         :end (or (position #\. pname :from-end t) 1)
                                         :from-end t)
                               0))))
      (loop
        :with target-pages := (pages target)
        :for prev = nil :then curr
        :for curr = (first target-pages) :then next
        :for pages = (rest target-pages) :then (rest pages)
        :for next = (first pages)
        :while curr
        :do (when (equalp curr current-page)
              (let* ((parent-path (butlast (package-path curr)))
                     (parent      (format nil "~{~A~^.~}" parent-path)))
                (return
                  (append (if entriesp entries (navigation target))
                          (when prev
                            (list (list prev   (format nil "Previous: ~A" (shorten prev)))))
                          (when next
                            (list (list next   (format nil "Next: ~A" (shorten next)))))
                          (when parent-path
                            (list (list parent (format nil "Up: ~A" (shorten parent)))))))))
        :finally (return (if entriesp entries (navigation target)))))))


(defgeneric generate-navigation-menu (target &optional entries)
  (:documentation "

DO:         Optionally generate a navigation menu to navigate to the
            various pages in the ENTRIES list.  If ENTRIES is not
            provided, then (NAVIGATION TARGET) is generated.

ENTRIES:    A list of lists (filename title) used to build the
            navigation menu.

")
  (:method (target &optional entries)
    (declare (ignore entries))
    #|no menu|#
    target))

(defgeneric generate-hierarchical-package-index (target tree &optional filename)
  (:documentation "

DO:         Optionally generate a hierarchical package index, by
            listing for each non-trivial node in the TREE, all the
            packages and subsystems.  The packages are links to the
            package documentation page, and the subsystems are links
            to recursively generated hierarchical package index for
            the node.

TARGET:     the generation document instance.

TREE:       the index-tree of the target or one of its subnodes.

FILENAME:   the name of the generated file.

NOTE:       'system' doesn't mean asdf system here; it's just a node
            in the package index tree.

"))


(defgeneric generate-flat-package-index (target pages &optional filename)
  (:documentation "

DO:         Optionally generate a flat list of packages linking to
            each package documentation page, sorted alphabetically.

TARGET:     the generation document instance.

PAGES:      the flat list of packages pages 

FILENAME:   the name of the generated file.

"))


(defgeneric generate-flat-symbol-index (target syms &optional filename)
  (:documentation "

DO:         Optionally generate a flat symbol index, sorted
            alphabetically; the symbols are grouped according to the
            initial letter, and for each group an index file is
            generated listing the symbols in the group alphabetically
            with links to the packages they are defined in.

TARGET:     the generation document instance.

SYM:        A list of the symbol doc structures.

FILENAME:   the stem of the name of the generated files.

RETURN:     a sorted list of lists containing the unique initial
            letters of all symbols, or :OTHER for non-alphabetical
            symbols, and the filename of that group index file).

"))


(defun build-flat-symbol-index-groups (syms)
  "
RETURN:     a list of groups of syms by first letter.
"
  (com.informatimago.common-lisp.cesarum.iso639a::split-groups
   (sort (copy-list syms) (function string-lessp) :key (function doc-name))
   (lambda (a b) (not (equalp (first-letter a) (first-letter b))))))


(defgeneric generate-permuted-symbol-index (target syms &optional filename)
  (:documentation "

DO:         Optionally generate a permuted symbol index, sorted
            alphabetically on the first letter of each component of
            the symbol name, components being the substrings of the
            symbol name separated by dashes.  The symbols are grouped
            according to the initial letter of each of its component,
            and for each group, an index file is generated listing the
            symbols in the group, alphabetically, and aligned on the
            initial letter of the component considered for that group.

TARGET:     the generation document instance.

SYM:        A list of the symbol doc structures.

FILENAME:   the stem of the name of the generated files.

RETURN:     a sorted list of lists containing the unique initial
            letters of all components, or :OTHER for non-alphabetical
            symbols, and the filename of that group index file).

"))


(defun build-permuted-symbol-index-groups (syms)
  "
RETURN: a list of groups of syms by first letter of components.
"
  (let ((table   (make-hash-table :test (function equalp)))
        (groups  '()))
    (dolist (sym syms)
      (let ((words (split-sequence-if (complement (function alpha-char-p))
                                      (symbol-name (doc-name sym))
                                      :remove-empty-subseqs t)))
        (dolist (word words)
          (push sym (gethash (first-letter word) table '())))))
    (maphash (lambda (k v)
               (push (cons k (sort (copy-list v)
                                   (function string<)
                                   :key (function doc-name)))
                     groups))
             table)
    (sort groups (function char-lessp) :key (function car))))


(defgeneric generate-symbol-index (target flat-indices permuted-indices symbol-count &optional filename)
  (:documentation "

DO:         Optionally generate a symbol index page, with links to the
            the letters in the FLAT-INDICES to the corresponding  flat
            symbol index file, and the letters in the PERMUTED-INDICES
            to the corresponding permuted symbol index file.

TARGET:     the generation document instance.

FLAT-INDICES:
            A list of flat symbol index indices (list of first letter and index file name).

PERMUTED-INDICES:
            A list of permuted symbol index indices (list of first letter and index file name).

SYMBOL-COUNT:
            Total number of symbols indexed (informative: it's just mentionned in the generated page).

FILENAME:   the name of the generated files.

"))


(defun collect-all-symbols (packdocs)
  "
RETURN: a list of the docs of all the exported symbols from the
        packages documented in the PACKDOCS list.
"
  (let ((syms '()))
    (dolist (pack packdocs (remove-duplicates syms
                                              :test (function equal)
                                              :key (function doc-symbol)))
      (appendf syms (packdoc-external-symbol-docs pack)))))


(defun first-letter (object)
  "
RETURN: the initial letter of the name of the object, upcased, or
        the keyword :OTHER if it is not an alphabetic character.
"
  (etypecase object
    (doc       (first-letter (doc-name object)))
    (symbol    (first-letter (symbol-name object)))
    (string    (first-letter (aref object 0)))
    (character (if (alpha-char-p object)
                   (char-upcase object)
                   :other))))


(defun right-case (sexp)
  (cond ((null sexp)    nil)
        ((symbolp sexp) (if (and (null (symbol-package sexp))
                                 (char= #\G (char (symbol-name sexp) 0)))
                            "x"
                            (right-case (symbol-name sexp))))
        ((stringp sexp) (if (notany (function lower-case-p) sexp)
                            (string-downcase sexp)
                            (format nil "|~A|" sexp)))
        ((numberp sexp) (princ-to-string sexp))
        ((consp sexp) (cons (right-case (car sexp)) (right-case (cdr sexp))))
        (t (warn "Unexpected atom in right-cased sexp: ~S of type ~S"
                  sexp (type-of sexp))
           (format nil "|~A|" (string-replace (format nil "~S" sexp) "|" "\\|")))))


(defgeneric generate-package-documentation-pages (target)
  (:documentation "

DO:         Generate the documentation page for each package in the TARGET packdocs.

TARGET:     the generation document instance.

NOTE:       The default method on the abstract superclass DOCUMENTATION-GENERATOR
            just calls RENDER on each packdoc of the TARGET.

"))


(defclass documentation-generator ()
  ((title      :initform "Documentation" :accessor documentation-title   :initarg :title
               :documentation "The global title of the documentation.")
   (copyright  :initform ""              :accessor copyright  :initarg :copyright
               :documentation "A copyright mention for the documentation.")
   (author     :initform nil             :accessor author     :initarg :author
               :documentation "The author's name, for insertion in the html head meta.")
   (email      :initform nil             :accessor email      :initarg :email
               :documentation "The email, for insertion in the html head meta.")
   (keywords   :initform nil             :accessor keywords   :initarg :keywords
               :documentation "The keywords, for insertion in the html head meta.")
   (packdocs   :initform '()             :accessor packdocs   :initarg :packdocs
               :documentation "A list of PACKDOC objects. This is the root of the documentation.")
   (pages      :initform '()             :accessor pages
               :documentation "A list of pages in the documentation, used for navigation purpose.
We generate one page per package, and pages are designated by their package name.")
   (index-tree :initform nil             :accessor index-tree
               :documentation "The index tree for hierarchical packages.")
   (navigation :initform '()             :accessor navigation
               :documentation "A menu in the form of a list of (relative-urls title).")))

(defmethod initialize-instance :after ((target documentation-generator) &key packdocs &allow-other-keys)
  (setf (pages      target) (mapcar (function doc-name) packdocs)
        (index-tree target) (make-index-tree (pages target))))


(defmethod generate-package-documentation-pages ((target documentation-generator))
  (dolist (doc (packdocs target))
    (render doc target)))


(defun generate-lispdoc (target-class directory packages &rest keys &key &allow-other-keys)
  "

DO:         Generate documentation for the exported symbols of each
            package, into the TARGET format, storing one or more files
            in DIRECTORY.

TARGET-CLASS:
            a class designator for a subclass of DOCUMENTATION-GENERATOR.

"
  (let* ((*default-pathname-defaults* (pathname directory))
         (packdocs     (lispdoc (sort (mapcar (lambda (package)
                                                (if (packagep package) 
                                                    package 
                                                    (find-package package)))
                                              packages)
                                      (function string<) :key (function package-name))))
         (all-symbols  (collect-all-symbols packdocs))
         (target       (apply (function make-instance) target-class :packdocs packdocs keys)))
    ;; ---
    (unwind-protect
         (progn
           (generate-begin target)
           (generate-introduction target)
           (generate-hierarchical-package-index target (index-tree target) "hierarchical-package-index")
           (generate-flat-package-index         target (pages      target) "flat-package-index")
           (generate-symbol-index
            target 
            (generate-flat-symbol-index         target all-symbols  "alphabetic-symbol-index")
            (generate-permuted-symbol-index     target all-symbols  "permuted-symbol-index")
            (length all-symbols)
            "symbol-index")
           (generate-package-documentation-pages target))
      (generate-end target))
    *default-pathname-defaults*))


;;;; THE END ;;;;


