;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               genhtml.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    HTML generator for the documentation.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-03 <PJB> Extracted from lispdoc.
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(in-package "COM.INFORMATIMAGO.LISPDOC.GENERATE.HTML")


;;;----------------------------------------------------------------------
;;;
;;; processing pjb docstrings.
;;;


(defun replace-urls (text)
  "
Search all the urls in the text, and replace them with an A tag.
"
  (let ((start 0))
    (loop
      (multiple-value-bind (wbegin wend begins ends)
          (scan '(:sequence #\< (:register uri) #\>) text :start start)
        (declare (ignore wbegin wend))
        (if begins
            (let ((begin (aref begins 0))
                  (end   (aref ends   0)))
              (pcdata "~A" (subseq text start begin))
              (let ((url (subseq text begin end)))
                (a (:href url) (pcdata "~A" url)))
              (setf start end))
            (progn
              (pcdata "~A" (subseq text start (length text)))
              (return)))))))

(defun pjb-docstring (docstring)
  (if (eq :undocumented docstring)
      (p (:class "undocumented")
         (i - (pcdata "undocumented")))
      (pre (:class "docstring")
           (replace-urls docstring))))


;;;----------------------------------------------------------------------
;;;
;;; Documentation HTML generation
;;;

(defun report-file (path)
  (format *trace-output* "~&;; Writing file ~A~%" path)
  path)

(defun style-sheet ()
  (link (:rel "stylesheet" :type "text/css" :href "style.css")))

(defun create-style-sheet ()
  (with-open-file (css (report-file "style.css")
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :external-format :utf-8)
    (format css "
body {
  margin: 10px;
  padding: 10px;
}

pre.docstring {
  margin: 20px;
}

div.symbol {
  padding:2px;
  background-color:#ddeeee;
}

div.kind {
  padding:2px;
  background-color:#ddeeee;
}

.undocumented {
  foreground-color:#ff0000;
}

.menu a {
  background-color:#80a0a0;
  border:1px solid #308080;
  padding:2px;
}

.menu input {
  color:#000000;
  background-color:#80a0a0;
  border:1px solid #308080;
  padding:2px;
}

.menu a:link    {color:#000000;}  /* unvisited link */
.menu a:visited {color:#004444;}  /* visited link */
.menu a:hover   {color:#FFFFFF;}  /* mouse over link */
.menu a:active  {color:#00FFFF;}  /* selected link */

.header p { font-size:80%; }
.footer p { font-size:80%; }
")))



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


(defun doc-title (name arglist kind)
  (a (:name (if (atom name)
                (format nil "~A" (symbol-name name))
                (format nil "(SETF ~A)" (symbol-name (second name))))))
  (table (:border "0" :width "100%")
         (tr -
             (td (:valign "top" :align "left")
                 (div (:class "symbol")
                     (cond
                       ((not (member kind '(:function :generic-function :macro)))
                        (b - (pcdata "~A" (right-case name))))
                       ((and (consp name) (eq (car name) 'setf))
                        (pcdata "(setf (")
                        (b - (pcdata "~A" (right-case (second name))))
                        (pcdata "~{ ~A~}) ~A)" (right-case (rest arglist))
                                (right-case (first arglist))))
                       (t
                        (pcdata "(")
                        (b - (pcdata "~A"  (right-case name)))
                        (pcdata "~{ ~A~}" (right-case arglist))
                        (pcdata ")")))))
             (td (:valign "top" :align "right" :width "200px")
                 (div (:class "kind")
                     (i - (pcdata "~(~A~)" kind)))))))


(defun generate-head (target &optional (title nil titlep))
  (head ()
    (title () (pcdata "~A" (if titlep title (documentation-title target))))
    (link (:rel "shortcut icon" :href "/favicon.ico"))
    (link (:rel "icon"          :href "/favicon.ico" :type "image/vnd.microsoft.icon"))
    (link (:rel "icon"          :href "/favicon.png" :type "image/png"))
    (meta (:http-equiv "Content-Type"   :content "text/html; charset=utf-8"))
    (when (author   target) (meta (:name "Author"    :content (author   target))))
    (when (email    target) (meta (:name "Reply-To"  :content (email    target))))
    (when (keywords target) (meta (:name "Keywords"  :content (keywords target))))
    (style-sheet)))


;;;---------------------------------------------------------------------
;;;
;;; HTML Documentation render
;;;


(defclass html-documentation (documentation-generator)
  ())

(defmethod initialize-instance :after ((target html-documentation) &key &allow-other-keys)
  (setf (navigation target) `(("../index"                   ,(documentation-title target))
                              ("index"                      "Documentation Index")
                              ("hierarchical-package-index" "Hierarchical Package Index")
                              ("flat-package-index"         "Flat Package Index")
                              ("symbol-index"               "Symbol Indices"))))


(defgeneric header (target page-name))
(defgeneric footer (target page-name))


(defun make-url (page)
  (format nil "~(~A.html~)" page))

(defmethod generate-introduction ((target html-documentation))
  (create-style-sheet)
  (let ((page-name "index"))
    (with-open-file (html (report-file (make-url page-name))
                      :direction :output
                      :if-does-not-exist :create
                      :if-exists :supersede
                      :external-format :utf-8)
      (with-html-output (html :encoding :utf-8)
        (doctype :transitional
          (html ()
            (generate-head target)
            (body ()
              (header target page-name)
              (h1 () (pcdata "~A" (documentation-title target)))
              (ul -
                (loop
                  :for (fn text) :in (navigation target)
                  :unless (equalp fn page-name)
                    :do (li -  (a (:href (make-url fn)) (pcdata "~A" text)))))
              (footer target page-name))))))))


(defvar *navigation* nil
  "The current navigation menu.")

(defmethod generate-navigation-menu ((target html-documentation) &optional (entries nil entriesp))
  "
ENTRIES: A list of (list url text). Usually, some variant of (navigation target).
"
  (div (:class "menu")
    (p - (loop
           :for (filename text) :in (if entriesp
                                        entries
                                        (navigation target))
           :do (pcdata "   ")
               ;; (form (:action (make-url filename) :method "GET")
               ;;       (input (:type "submit" :value text)))
               (a (:href (make-url filename)) (pcdata "~A" text))))))


(defmethod header ((target html-documentation) page-name)
  (div (:class "header")
    (generate-navigation-menu target (remove page-name (navigation target)
                                    :test (function string=)
                                    :key (function first))))
  (hr) (br))


(defmethod footer ((target html-documentation) page-name)
  (br) (hr)
  (div (:class "footer")
    (generate-navigation-menu target (remove page-name (navigation target)
                                    :test (function string=)
                                    :key (function first)))
    (p - (pcdata (copyright target)))))


;;;---------------------------------------------------------------------
;;;
;;; HTML rendering
;;;


(defmethod render ((doc doc) (target html-documentation))
  (ecase (doc-kind doc)
    (:type
     (doc-title (doc-symbol doc) nil (doc-kind doc))
     (pjb-docstring (doc-string doc)))
    (:skip
     (format t "~&;; warning: lispdoc skipping ~s~%" (doc-symbol doc)))
    (:undocumented
     (p -
        (b - (pcdata "~A" (doc-symbol doc)))
        "    "
        (i (:class "undocumented") "undocumented")))))


(defmethod render ((doc packdoc) (target html-documentation))
  (let ((page-name (doc-name doc)))
   (with-open-file (html (report-file (make-url page-name))
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :external-format :utf-8)
     (let ((title  (format nil "Package ~A" (doc-name doc)))
           (*navigation* (package-navigation-menu target page-name (or *navigation* (navigation target)))))
       (with-html-output (html :encoding :utf-8)
         (doctype :transitional
                  (html ()
                        (generate-head target)
                        (body ()
                              (header target page-name)
                              (h1 () (pcdata "~A" title))
                              (when (packdoc-nicknames doc)
                                (blockquote -
                                            (pcdata "Nicknames: ")
                                            (tt - (pcdata "~{ ~A~}" (packdoc-nicknames doc)))))
                              (pjb-docstring (doc-string doc))
                              (mapc (lambda (doc) (render doc target)) (packdoc-external-symbol-docs doc))
                              (footer target page-name))))))
     (pathname html))))


(defmethod render ((doc vardoc) (target html-documentation))
  (doc-title (doc-symbol doc) nil (doc-kind doc))
  (pjb-docstring (doc-string doc))
  (if (eq (vardoc-initial-value doc) :unbound)
      (blockquote - (pcdata "Initially unbound"))
      (blockquote - (pcdata "Initial value: ") (tt - (pcdata "~A" (vardoc-initial-value doc))))))


(defmethod render ((doc fundoc) (target html-documentation))
  (doc-title (doc-symbol doc) (fundoc-lambda-list doc) (doc-kind doc))
  (pjb-docstring (doc-string doc)))


(defmethod render ((doc classdoc) (target html-documentation))
  (doc-title (doc-symbol doc) nil (doc-kind doc))
  (pjb-docstring (doc-string doc))
  (when (classdoc-precedence-list doc)
    (blockquote -
                (pcdata "Class precedence list: ")
                (tt - (pcdata "~{ ~A~}" (classdoc-precedence-list doc)))))
  (when (classdoc-initargs doc)
    (blockquote -
                (pcdata "Class init args: ")
                (tt - (pcdata "~{ ~A~}" (classdoc-initargs doc))))))


;;;---------------------------------------------------------------------


(defmethod generate-hierarchical-package-index ((target html-documentation) tree &optional (filename "hierindex"))
  (loop
    ;; find the first node from the root that has more than one child or designate an actual package.
    :while (and (= 1 (length (tree-children tree)))
                (null (tree-package tree))
                (null (tree-package (first (tree-children tree)))))
    :do (setf tree (first (tree-children tree))))
  (flet ((filename (path)
           (format nil "~{~A~^.~}" (reverse path))))
    (let ((title (filename (tree-path tree))))
      (with-open-file (html (report-file (make-url filename))
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede
                        :external-format :utf-8)
        (with-html-output (html :encoding :utf-8)
          (doctype :transitional
            (html ()
              (generate-head target title)
              (body ()
                (header target filename)
                (h1 () (pcdata "~A" title))
                (ul -
                  (dolist (child (tree-children tree))
                    (let ((childfile (filename (tree-path child))))
                      (li - (a (:href (make-url childfile))
                              (if (tree-package child)
                                  (pcdata "Package ~A" (tree-package child))
                                  (progn
                                    (pcdata "System ~A" childfile)
                                    (generate-hierarchical-package-index target child childfile))))))))
                (footer target filename)))))))))


(defmethod generate-flat-package-index ((target html-documentation) pages &optional (filename "flatindex"))
  (let ((title "Flat Package Index"))
    (with-open-file (html (report-file (make-url filename))
                      :direction :output
                      :if-does-not-exist :create
                      :if-exists :supersede
                      :external-format :utf-8)
      (with-html-output (html :encoding :utf-8)
        (doctype :transitional
          (html ()
            (generate-head target title)
            (body ()
              (header target filename)
              (h1 () (pcdata "~A" title))
              (ul -
                (dolist (page pages)
                  (li - (a (:href (make-url page))
                          (pcdata "~A" page)))))
              (footer target filename))))))))



(defmethod generate-flat-symbol-index ((target html-documentation) syms &optional (filename "flatsymindex"))
  "
RETURN: A list of (first-letter filename)
"
  (let ((groups (build-flat-symbol-index-groups syms))
        (indices '()))
    ;; Generate each group index:
    (dolist (group groups (nreverse indices))
      (let* ((group   (sort group
                            (function string-lessp)
                            :key (lambda (x) (symbol-name (doc-name x)))))
             (first-letter  (first-letter (first group)))
             (filename (format nil "~A-~A" filename first-letter))
             (title    (format nil "Alphabetical Symbol Index -- ~A" first-letter))
             (width    (reduce (function max) group
                               :key (lambda (x) (length (princ-to-string (doc-symbol x)))))))
        (push (list first-letter filename) indices)
        (with-open-file (html (report-file (make-url filename))
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :external-format :utf-8)
          (with-html-output (html :encoding :utf-8)
            (doctype :transitional
              (html ()
                (generate-head target title)
                (body ()
                  (header target filename)
                  (h1 () (pcdata "~A" title))
                  (pre -
                    (dolist (sym group)
                      (let ((packname (package-name
                                       (symbol-package (doc-name sym)))))
                        (a (:href
                            (with-standard-io-syntax
                              (format nil "~A#~A"
                                      (make-url packname)
                                      (doc-symbol sym))))
                          (pcdata "~A" (doc-symbol sym)))
                        (pcdata "~V<~>" (- width -4 (length (princ-to-string (doc-symbol sym)))))
                        (a (:href (make-url packname))
                          (pcdata "~(~A~)" packname))
                        (pcdata "~%"))))
                  (footer target filename))))))))))


(defmethod generate-permuted-symbol-index ((target html-documentation) syms &optional (filename "permsymindex"))
  "
RETURN: A list of (first-letter filename)
"
  (let ((groups  (build-permuted-symbol-index-groups syms))
        (indices '()))
    ;; Generate each group index:
    (dolist (group groups (nreverse indices))
      (let ((first-letter (pop group)))
        (labels ((compute-offset (name index)
                   (if (equalp first-letter (aref name index))
                       index
                       (loop
                         :for previous :from index
                         :for i :from (1+ index) :below (length name)
                         :while (not (and (not (alpha-char-p (aref name previous)))
                                          (equalp first-letter (aref name i))))
                         :finally (return (if (< i (length name)) i nil)))))
                 (offset (doc)
                   (if (consp (doc-symbol doc))
                       (compute-offset (princ-to-string (doc-symbol doc)) (length "(setf "))
                       (compute-offset (symbol-name (doc-name doc)) 0))))
          (let* ((group    (sort  group
                                  (function string-lessp)
                                  :key (lambda (doc)
                                         (let ((name   (princ-to-string (doc-symbol doc)))
                                               (offset (offset doc)))
                                           (concatenate 'string
                                                        (subseq name offset) (subseq name 0 offset))))))
                 (filename (format nil "~A-~A" filename first-letter))
                 (title    (format nil "Permuted Symbol Index -- ~A" first-letter))
                 (indent   (reduce (lambda (a b)
                                     (cond
                                       ((null a) b)
                                       ((null b) a)
                                       (t (max a b))))
                                   group :key (function offset)))
                 (width    (reduce (function max) group
                                   :key (lambda (x) (length (princ-to-string (doc-symbol x)))))))
            (push (list first-letter filename) indices)
            (with-open-file (html (report-file (make-url filename))
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede
                              :external-format :utf-8)
              (with-html-output (html :encoding :utf-8)
                (doctype :transitional
                  (html ()
                    (generate-head target title)
                    (body ()
                      (header target filename)
                      (h1 () (pcdata "~A" title))
                      (pre -
                        (dolist (sym group)
                          (let ((packname (package-name (symbol-package (doc-name sym))))
                                (offset (offset sym)))
                            (when offset
                              (pcdata (make-string (- indent (offset sym)) :initial-element #\space))
                              (a (:href (with-standard-io-syntax
                                          (format nil "~A#~A"
                                                  (make-url packname)
                                                  (doc-symbol sym))))
                                (pcdata "~A" (doc-symbol sym)))
                              (pcdata "~V<~>" (- (+ width 4)
                                                 (- (length (princ-to-string (doc-symbol sym)))
                                                    offset)))
                              (a (:href (make-url packname))
                                (pcdata "~(~A~)" packname))
                              (pcdata "~%")))))
                      (footer target filename))))))))))))


(defmethod generate-symbol-index ((target html-documentation) flat-indices permuted-indices symbol-count &optional (filename "symindex"))
  (flet ((gen-index (indices)
           (div (:class "menu")           
             (loop
               :for sep = "" :then "   "
               :for (first-letter initial-filename) :in indices
               :do (progn (pcdata sep)
                          (a (:href (make-url initial-filename))
                            (pcdata "~A" (if (eq :other first-letter)
                                             "Non-Alphabebtic"
                                             first-letter))))))))
    (let ((title "Symbol Indices"))
      (with-open-file (html (report-file (make-url filename))
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede
                        :external-format :utf-8)
        (with-html-output (html :encoding :utf-8)
          (doctype :transitional
            (html ()
              (generate-head target title)
              (body ()
                (header target filename)
                (h1 - (pcdata "Alphabetical Symbol Index"))
                (p - (pcdata "There are ~A symbols exported from the Informatimago Common Lisp packages."
                             symbol-count))
                (gen-index flat-indices)
                (p - (a (:href "")
                       (pcdata "Click here to see all the symbols on one page, alphabetically.")))
                (h1 - (pcdata "Permuted Symbol Index"))
                (p -
                  (pcdata "A permuted index includes each ") (i - (pcdata "n"))
                  (pcdata "-word entry up to ") (i - (pcdata "n"))
                  (pcdata " times, at points corresponding to the use of each word in the entry")
                  (pcdata " as the sort key.  For example, a symbol ") (tt - (pcdata "FOO-BAR"))
                  (pcdata " would occur twice, once under ") (tt - (pcdata "FOO"))
                  (pcdata " and ") (tt - (pcdata "BAR")) (pcdata ". This allows you to use any")
                  (pcdata " word in th symbol's name to search for that symbol."))
                (gen-index permuted-indices)
                (footer target filename)))))))))



#-(and)
(setf *index-tree*
      (make-index-tree (mapcar (function doc-name)
                          (lispdoc (sort (mapcar (lambda (package)
                                                   (if (packagep package) 
                                                       package 
                                                       (find-package package)))
                                                 (remove-if-not (lambda (p)
                                                                  (and (search "COM.INFORMATIMAGO" (package-name p))
                                                                       (not (search "COM.INFORMATIMAGO.PJB" (package-name p)))))
                                                                (list-all-packages)))
                                         (function string<) :key (function package-name))))))




;;;; THE END ;;;;
