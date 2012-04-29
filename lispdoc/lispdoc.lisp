;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;
;;;; $Id: lispdoc.lisp,v 1.8 2004/01/13 14:03:41 sven Exp $
;;;;


(defpackage :com.informatimago.lispdoc
  (:use :common-lisp
        :split-sequence
        :cl-ppcre
        :com.informatimago.common-lisp.cesarum.string
        :com.informatimago.common-lisp.html-generator.html)
  (:shadowing-import-from :common-lisp #:map)
  (:export #:lispdoc #:render-html #:lispdoc-html)
  (:documentation "

Automatically generate documentation for properly documented symbols
exported from packages.

This is tool automatically generates documentation for Common Lisp code
based on symbols that exported from packages and properly documented.
This code was written for OpenMCL (http://openmcl.clozure.com)


License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2012
    Copyright (C) 2003 Sven Van Caekenberghe.

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    (http://opensource.franz.com/preamble.html), also known as the LLGPL.
"))

(in-package :com.informatimago.lispdoc)




;;;----------------------------------------------------------------------
;;;
;;; processing pjb docstrings.
;;;

;;; URLs:

(define-parse-tree-synonym num
    (:char-class (:range #\0 #\9)))
(define-parse-tree-synonym alpha
    (:char-class (:range #\A #\Z) (:range #\a #\z)))
(define-parse-tree-synonym alphanum
    (:char-class (:range #\A #\Z) (:range #\a #\z) (:range #\0 #\9)))
(define-parse-tree-synonym alpha-num
    (:char-class #\- (:range #\A #\Z) (:range #\a #\z) (:range #\0 #\9)))


(define-parse-tree-synonym http-scheme
    (:alternation "http" "https" "ftp" "telnet" "irc" "file"
                  "aaa" "aaas" "acap" "cap" "crid" "icap"
                  "dav" "dict"  "dns"  "go" "gopher" "imap" "info"
                  "ipp" "iris" "iris.beep" "iris.xpc" "iris.xpcs" "iris.lws"
                  "ldap" "msrp" "msrps" "mtqp" "mupdate" "nfs" "nntp"
                  "opaquelocktoken" "pop" "prospero" "rsync" "rtsp"
                  "service" "shttp" "sieve" "snmp" "tftp" "thismessage" "tip" "tv"
                  "soap.beep" "soap.beeps" "vmmi" "z39.50r" "z39.50s"
                  ;; and so on
                  ))

(define-parse-tree-synonym mail-scheme
    (:alternation "mailto" "phoneto" "faxto" "fax"
                  "cid" "data" "geo" "h323" "iax" "im" "lsid" "mid" "modem"
                  "news" "pres" "sip" "sms" "tag" "tel" "urn" "uuid"
                  "ws" "wss" "xmpp"
                  ;; and so on
                  ))

(define-parse-tree-synonym phonenumber
    (:sequence (:greedy-repetition 0 1 "+")
               (:greedy-repetition 1 nil num)))
(define-parse-tree-synonym username
    (:greedy-repetition 1 nil (:inverted-char-class #\space #\newline #\@ #\:)))
(define-parse-tree-synonym password
    (:greedy-repetition 1 nil (:inverted-char-class #\space #\newline #\@)))
(define-parse-tree-synonym port
    (:greedy-repetition 1 nil num))
(define-parse-tree-synonym name
    (:greedy-repetition 1 nil alpha-num))
(define-parse-tree-synonym host
    (:sequence name
               (:greedy-repetition 1 nil (:sequence "." name))
               (:greedy-repetition 0 1 ".")))
(define-parse-tree-synonym path
    (:greedy-repetition 1 nil (:inverted-char-class #\space #\newline #\? #\#)))
(define-parse-tree-synonym query
    (:greedy-repetition 1 nil (:inverted-char-class #\space #\newline #\#)))
(define-parse-tree-synonym fragment
    (:greedy-repetition 1 nil (:inverted-char-class #\space #\newline)))


(define-parse-tree-synonym url
    (:alternation
     (:sequence http-scheme "://"
                (:greedy-repetition 0 1 (:sequence
                                         (:greedy-repetition 0 1 (:sequence username
                                                                            (:greedy-repetition 0 1 ":" password)
                                                                            "@"))
                                         host
                                         (:greedy-repetition 0 1 (:sequence ":" port))))
                (:greedy-repetition 0 1 (:sequence path))
                (:greedy-repetition 0 1 (:sequence "?" query))
                (:greedy-repetition 0 1 (:sequence "#" fragment))
                )
     (:sequence mail-scheme ":"
                (:alternation (:sequence username "@" host
                                         (:greedy-repetition 0 1 query))
                              (:sequence phonenumber)))))


(defun replace-urls (text)
  "
Search all the urls in the text, and replace them with an A tag.
"
  (let ((start 0))
    (loop
      (multiple-value-bind (begin end) (scan '(:group url) text :start start)
        (unless begin
          (pcdata "~A" (subseq text start (length text)))
          (return))
        (pcdata "~A" (subseq text start begin))
        (let ((url (subseq text begin end)))
          (a (:href url) (pcdata "~A" url)))
        (setf start end)))))


;;; pjb docstrings:

(defun pjb-docstring (docstring)
  (if (eq :undocumented docstring)
      (p (:class "undocumented")
         (i - (pcdata "undocumented")))
      (pre (:class "docstring")
           (replace-urls docstring))))








;;;----------------------------------------------------------------------
;;;
;;; doc structures
;;;


(defstruct doc
  symbol
  kind
  string)

(defstruct (packdoc (:include doc))
  nicknames
  external-symbol-docs)

(defstruct (vardoc (:include doc))
  initial-value)

(defstruct (fundoc (:include doc))
  lambda-list)

(defstruct (classdoc (:include doc))
  precedence-list
  initargs)


(defun doc-name (doc)
  (let ((name (doc-symbol doc)))
    (etypecase name
      (package (package-name name))
      (symbol  name)
      (cons    (second name)))))


;;;----------------------------------------------------------------------
;;;
;;; documentation portability layer
;;;



(defun function-lambda-list (funame)
  "
FUNAME:  A function name.
RETURN:  The function lambda list.
"
  (let ((le (function-lambda-expression (if (consp funame)
                                            (fdefinition funame)
                                            (or (macro-function funame)
                                                (symbol-function funame))))))
    (if le
        (second le)
        (or
         #+openmcl   (ccl:arglist funame)
         #+lispworks (lw:function-lambda-list funame)
         '()))))


(defun class-precedence-list (class-name)
  "
CLASS-NAME: A class name.
RETURN:     The class precedence list.
"
  (closer-mop:class-precedence-list (find-class class-name)))


(defun class-slot-initargs (class-name)
  "
CLASS-NAME: A class name.
RETURN:     The initargs of the class slots.
"
  (let ((class (find-class class-name)))
    (mapcan (lambda (slot) (copy-seq (closer-mop:slot-definition-initargs slot)))
            (closer-mop:class-slots class))))




(defun has-meaning (symbol)
  (or (boundp symbol)
      (fboundp symbol)
      (ignore-errors (fdefinition `(setf ,symbol)))
      (ignore-errors (find-class symbol))))




;;;----------------------------------------------------------------------
;;;
;;; lispdoc
;;;

(defun lispdoc-symbol (symbol)
  "
RETURN: A list of doc structures for the SYMBOL.
"
  (let ((doc '()))
    
    (when (documentation symbol 'variable)
      (push (make-vardoc :kind          (if (constantp symbol)
                                            :constant
                                            :variable)
                         :symbol         symbol
                         :string        (documentation symbol 'variable)
                         :initial-value (if (boundp symbol)
                                            (symbol-value symbol)
                                            :unbound))
            doc))
    
    (let ((spec `(setf ,symbol)))
      (when (and (documentation spec 'function)
                 (fboundp spec))
        (push (make-fundoc :kind        (cond
                                          ((typep (fdefinition spec) 'standard-generic-function)
                                           :generic-function)
                                          (t
                                           :function))
                           :symbol      `(setf ,symbol)
                           :string      (documentation spec 'function)
                           :lambda-list (function-lambda-list spec))
              doc)))
    
    (when (and (documentation symbol 'function)
               (fboundp symbol))
      (push (make-fundoc :kind        (cond
                                        ((macro-function symbol)
                                         :macro)
                                        ((typep (fdefinition symbol) 'standard-generic-function)
                                         :generic-function)
                                        (t
                                         :function))
                         :symbol      symbol
                         :string      (documentation symbol 'function)
                         :lambda-list (function-lambda-list symbol))
            doc))
    
    (when (documentation symbol 'type)
      (cond
        ((not (ignore-errors (find-class symbol)))
         (push (make-doc :kind :type
                         :symbol symbol
                         :string (documentation symbol 'type))
               doc))
        ((subtypep (find-class symbol) (find-class 'structure-object))
         (push  (make-classdoc :kind :structure
                               :symbol symbol
                               :string (documentation symbol 'type))
                doc))
        (t
         (block :ignore
           (push (make-classdoc :kind            (cond
                                                   ((subtypep (find-class symbol) (find-class 'condition))
                                                    :condition)
                                                   ((subtypep (find-class symbol) (find-class 'standard-object))
                                                    :class)
                                                   (t (return-from :ignore)))
                                :symbol          symbol
                                :string          (documentation symbol 'type)
                                :precedence-list (mapcar #'class-name (class-precedence-list symbol))
                                :initargs        (class-slot-initargs symbol))
                 doc)))))
    
    (unless  doc
      (push (make-doc :kind (if (has-meaning symbol)
                                :undocumented
                                :skip)
                      :symbol symbol)
            doc))
    
    doc))


(defun lispdoc-package (package)
  "
RETURN:  packdoc structure for the package.
"
  (make-packdoc :kind :package
                :symbol package
                :string (or (documentation package t) :undocumented)
                :nicknames (package-nicknames package)
                :external-symbol-docs
                (mapcan #'lispdoc-symbol
                        (let ((symbols '()))
                          (do-external-symbols (x package) (push x symbols))
                          (sort symbols #'string-lessp)))))



(defun lispdoc (packages)
  "Generate a lispdoc sexp documenting the exported symbols of each package"
  (mapcar (lambda (package)
            (lispdoc-package (if (packagep package)
                                 package
                                 (find-package package))))
          packages))


;;;----------------------------------------------------------------------
;;;
;;; HTML generation tools
;;;


(defun package-path (package)
  (split-sequence #\. (etypecase package
                        (string package)
                        (package (package-name package)))))



(define-modify-macro appendf (&rest args) 
    append "Append onto list")



(defstruct (tree
             (:copier tree-copy))
  parent
  node
  package
  (children '()))

(defmethod print-object ((tree tree) stream)
  (print-unreadable-object (tree stream :identity t :type t)
    (format stream "~S" (list :node (tree-node tree)
                              :package (tree-package tree)
                              :children (length (tree-children tree)))))
  tree)


(defun tree-children-named (tree node)
  (find node (tree-children tree)
        :key (function tree-node)
        :test (function equal)))

(defun tree-add-node-at-path (tree path pname)
  (if (endp path)
      (progn
        (setf (tree-package tree) pname)
        tree)
      (let ((child (tree-children-named tree (first path))))
        (if child
            (tree-add-node-at-path child (rest path) pname)
            (let ((new-child (make-tree :parent tree
                                        :node (first path)
                                        :package (when (endp (rest path))
                                                   pname))))
              (appendf (tree-children tree) (list new-child))
              (tree-add-node-at-path new-child (rest path) pname))))))

(defun make-index-tree (package-names)
  (let ((root (make-tree)))
    (dolist (pname package-names root)
     (tree-add-node-at-path root (package-path pname) pname))))


(defun tree-node-at-path (tree path)
  (if (endp path)
      tree
      (let ((child (tree-children-named tree (first path))))
        (when child
          (tree-node-at-path child (rest path))))))

(defun tree-path (tree)
  "RETURN: The path from TREE to the root."
  (cons (tree-node tree) (when (and (tree-parent tree)
                                    (tree-node (tree-parent tree)))
                           (tree-path (tree-parent tree)))))


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
        (t (error "Unexpected atom in right-cased sexp: ~S of type ~S"
                  sexp (type-of sexp)))))


(defun doc-title (name arglist kind)
  (a (:name (if (atom name)
                (format nil "~A" (symbol-name name))
                (format nil "(SETF ~A)" (symbol-name (second name))))))
  (table (:border "0" :width "100%")
         (tr -
             (td (:valing "top" :align "left")
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


(defun pjb-head (title)
  (head ()
        (title () (pcdata "~A" title))
        (link (:rel "shortcut icon" :href "/favicon.ico"))
        (link (:rel "icon"          :href "/favicon.ico" :type "image/vnd.microsoft.icon"))
        (link (:rel "icon"          :href "/favicon.png" :type "image/png"))
        (meta (:http-equiv "Content-Type"   :content "text/html; charset=utf-8"))
        (meta (:name "author"               :content "Pascal J. Bourguignon"))
        (meta (:name "Reply-To"             :content "pjb@informatimago.com"))
        (meta (:name "Keywords"             :content "Informatimago, Common Lisp, Lisp, Library"))        
        (style-sheet)))


(defvar *navigation* '())

(defun navigation-menu (&optional (entries *navigation*))
  "
ENTRIES: A list of (list url text).
"
  (div (:class "menu")
      (p - (loop
             :for (filename text) :in entries
             :do (pcdata "   ") (a (:href (make-url filename)) (pcdata "~A" text))
             ;; (pcdata "   ")
             ;; (form (:action (make-url filename) :method "GET")
             ;;       (input (:type "submit" :value text)))
             ))))

(defun header (filename)
  (div (:class "header")
      (navigation-menu (remove  filename *navigation*
                               :test (function string=)
                               :key (function first))))
  (hr) (br))


(defun footer (filename)
  (br) (hr)
  (div (:class "footer")
      (navigation-menu (remove filename *navigation*
                               :test (function string=)
                               :key (function first)))
    (p - (pcdata "Copyright Pascal J. Bourguignon 2012 - 2012"))))


(defvar *pages* '())
(defvar *index-tree* nil)

(defun make-url (page)
  (format nil "~(~A.html~)" page))

(defun package-navigation-menu (current-page &optional (*navigation* *navigation*))
  (flet ((shorten (pname)
           (subseq pname (or (position #\. pname :from-end t :end (or (position #\. pname :from-end t) 1))
                             0))))
    (loop
      :for prev = nil :then curr
      :for curr = (first *pages*) :then next
      :for pages = (rest *pages*) :then (rest pages)
      :for next = (first pages)
      :while curr
      :do (when (equalp curr current-page)
            (let* (;; (node        (tree-node-at-path *index-tree* (package-path curr)))
                   (parent-path (butlast (package-path curr)))
                   (parent      (format nil "~{~A~^.~}" parent-path)))
             (return
               (append *navigation*
                       (when prev
                         (list (list prev   (format nil "Previous: ~A" (shorten prev)))))
                       (when next
                         (list (list next   (format nil "Next: ~A" (shorten next)))))
                       (when parent
                         (list (list parent (format nil "Up: ~A" (shorten parent)))))))))
      :finally (return *navigation*))))


(defgeneric render-html (doc)
  (:documentation "Generate the HTML representation of the DOC structure."))


(defmethod render-html ((doc doc))
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


(defmethod render-html ((doc packdoc))
  (let ((filename (doc-name doc)))
   (with-open-file (html (report-file (make-url filename))
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :external-format :utf-8)
     (let ((title  (format nil "Package ~A" (doc-name doc)))
           (*navigation* (package-navigation-menu  (doc-name doc) *navigation*)))
       (with-html-output (html :encoding :utf-8)
         (doctype :transitional
                  (html ()
                        (pjb-head title)
                        (body ()
                              (header filename)
                              (h1 () (pcdata "~A" title))
                              (when (packdoc-nicknames doc)
                                (blockquote -
                                            (pcdata "Nicknames: ")
                                            (tt - (pcdata "~{ ~A~}" (packdoc-nicknames doc)))))
                              (pjb-docstring (doc-string doc))
                              (mapc (function render-html) (packdoc-external-symbol-docs doc))
                              (footer filename))))))
     (pathname html))))


(defmethod render-html ((doc vardoc))
  (doc-title (doc-symbol doc) nil (doc-kind doc))
  (pjb-docstring (doc-string doc))
  (if (eq (vardoc-initial-value doc) :unbound)
      (blockquote - (pcdata "Initially unbound"))
      (blockquote - (pcdata "Initial value: ") (tt - (pcdata "~A" (vardoc-initial-value doc))))))


(defmethod render-html ((doc fundoc))
  (doc-title (doc-symbol doc) (fundoc-lambda-list doc) (doc-kind doc))
  (pjb-docstring (doc-string doc)))


(defmethod render-html ((doc classdoc))
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




(defun generate-hierarchical-package-index (tree &optional (filename "hierindex"))
  (loop
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
                        (pjb-head title)
                        (body ()
                               (header filename)
                               (h1 () (pcdata "~A" title))
                               (ul -
                                   (dolist (child (tree-children tree))
                                     (let ((childfile (filename (tree-path child))))
                                       (li - (a (:href (make-url childfile))
                                                (if (tree-package child)
                                                    (pcdata "Package ~A" (tree-package child))
                                                    (progn
                                                      (pcdata "System ~A" childfile)
                                                      (generate-hierarchical-package-index child childfile))))))))
                               (footer filename)))))))))


(defun generate-flat-package-index (pages &optional (filename "flatindex"))
  (let ((title "Flat Package Index"))
    (with-open-file (html (report-file (make-url filename))
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :external-format :utf-8)
      (with-html-output (html :encoding :utf-8)
        (doctype :transitional
                 (html ()
                       (pjb-head title)
                       (body ()
                             (header filename)
                             (h1 () (pcdata "~A" title))
                             (ul -
                                 (dolist (page pages)
                                   (li - (a (:href (make-url page))
                                            (pcdata "~A" page)))))
                             (footer filename))))))))

(defun collect-all-symbols (packdocs)
  (let ((syms '()))
    (dolist (pack packdocs (remove-duplicates syms
                                              :test (function equal)
                                              :key (function doc-symbol)))
      (appendf syms (packdoc-external-symbol-docs pack)))))


(defun initial (object)
  (etypecase object
    (doc       (initial (doc-name object)))
    (symbol    (initial (symbol-name object)))
    (string    (initial (aref object 0)))
    (character (if (alpha-char-p object)
                   (char-upcase object)
                   :other))))


(defun generate-flat-symbol-index (syms &optional (filename "flatsymindex"))
  "
RETURN: A list of (initial filename)
"
  (let* ((syms   (sort (copy-list syms) (function string-lessp) :key (function doc-name)))
         (groups (com.informatimago.common-lisp.cesarum.iso639a::split-groups
                  syms (lambda (a b)
                         (not (equalp (initial a) (initial b))))))
         (indices '()))
    ;; Generate each group index:
    (dolist (group groups)
      (let* ((group   (sort group
                            (function string-lessp)
                            :key (lambda (x) (symbol-name (doc-name x)))))
             (initial  (initial (first group)))
             (filename (format nil "~A-~A" filename initial))
             (title    (format nil "Alphabetical Symbol Index -- ~A" initial))
             (width    (reduce (function max) group
                               :key (lambda (x) (length (princ-to-string (doc-symbol x)))))))
        (push (list initial filename) indices)
        (with-open-file (html (report-file (make-url filename))
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede
                              :external-format :utf-8)
          (with-html-output (html :encoding :utf-8)
            (doctype :transitional
                     (html ()
                           (pjb-head title)
                           (body ()
                                 (header filename)
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
                                 (footer filename))))))))
    (reverse indices)))


(defun generate-permuted-symbol-index (syms &optional (filename "permsymindex"))
  "
RETURN: A list of (initial filename)
"
  (let ((table   (make-hash-table :test (function equalp)))
        (groups  '())
        (indices '()))
    (dolist (sym syms)
      (let ((words (split-sequence-if (complement (function alpha-char-p))
                                      (symbol-name (doc-name sym))
                                      :remove-empty-subseqs t)))
        (dolist (word words)
          (push sym (gethash (initial word) table '())))))
    (maphash (lambda (k v)
               (push (cons k (sort (copy-list v)
                                   (function string<)
                                   :key (function doc-name)))
                     groups))
             table)
    (setf groups (sort groups (function char-lessp) :key (function car)))
    ;; Generate each group index:
    (dolist (group groups)
      (let ((initial (pop group)))
        (labels ((compute-offset (name index)
                   (if (equalp initial (aref name index))
                       index
                       (loop
                         :for previous :from index
                         :for i :from (1+ index) :below (length name)
                         :while (not (and (not (alpha-char-p (aref name previous)))
                                          (equalp initial (aref name i))))
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
                 (filename (format nil "~A-~A" filename initial))
                 (title    (format nil "Permuted Symbol Index -- ~A" initial))
                 (indent   (reduce (lambda (a b)
                                     (cond
                                       ((null a) b)
                                       ((null b) a)
                                       (t (max a b))))
                                   group :key (function offset)))
                 (width    (reduce (function max) group
                                   :key (lambda (x) (length (princ-to-string (doc-symbol x)))))))
            (push (list initial filename) indices)
            (with-open-file (html (report-file (make-url filename))
                                  :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede
                                  :external-format :utf-8)
              (with-html-output (html :encoding :utf-8)
                (doctype :transitional
                         (html ()
                               (pjb-head title)
                               (body ()
                                     (header filename)
                                     (h1 () (pcdata "~A" title))
                                     (pre -
                                          (dolist (sym group)
                                            (let ((packname (package-name (symbol-package (doc-name sym))))
                                                  (offset (offset sym)))
                                              (when offset
                                                (pcdata (make-string (- indent (offset sym))
                                                                     :initial-element #\space))
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
                                     (footer filename))))))))))
    (reverse indices)))


(defun generate-symbol-index (flat-indices permuted-indices symbol-count &optional (filename "symindex"))
  (flet ((gen-index (indices)
           (div (:class "menu")           
               (loop
                 :for sep = "" :then "   "
                 :for (initial initial-filename) :in indices
                 :do (progn (pcdata sep)
                            (a (:href (make-url initial-filename))
                               (pcdata "~A" (if (eq :other initial)
                                                "Non-Alphabebtic"
                                                initial))))))))
    (let ((title "Symbol Indices"))
     (with-open-file (html (report-file (make-url filename))
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede
                           :external-format :utf-8)
       (with-html-output (html :encoding :utf-8)
         (doctype :transitional
                  (html ()
                        (pjb-head title)
                        (body ()
                              (header filename)
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
                              (footer filename)))))))))


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





(defun lispdoc-html (directory packages &key (title "Packages"))
  "Generate HTML documentation in a file per package in directory for the exported symbols of each package"
  (let* ((*default-pathname-defaults* (pathname directory))
         (packdocs     (lispdoc (sort (mapcar (lambda (package)
                                                (if (packagep package) 
                                                    package 
                                                    (find-package package)))
                                              packages)
                                      (function string<) :key (function package-name))))
         (*pages*      (mapcar (function doc-name) packdocs))
         (*index-tree* (setf *index-tree* (make-index-tree *pages*)))
         (*navigation* '(("../index"                   "Informatimago CL Software")
                         ("index"                      "Documentation Index")
                         ("hierarchical-package-index" "Hierarchical Package Index")
                         ("flat-package-index"         "Flat Package Index")
                         ("symbol-index"               "Symbol Indices")))
         (all-symbols  (collect-all-symbols packdocs)))
    ;; ---
    (generate-hierarchical-package-index *index-tree* "hierarchical-package-index")
    (generate-flat-package-index         *pages*      "flat-package-index")
    (generate-symbol-index
     (generate-flat-symbol-index         all-symbols  "alphabetic-symbol-index")
     (generate-permuted-symbol-index     all-symbols  "permuted-symbol-index")
     (length all-symbols)
     "symbol-index")
    ;; ---
    (create-style-sheet)
    (let ((filename "index"))
      (with-open-file (html (report-file (make-url filename))
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede
                            :external-format :utf-8)
        (with-html-output (html :encoding :utf-8)
          (doctype :transitional
                   (html ()
                         (pjb-head title)
                         (body ()
                               (header filename)
                               (h1 () (pcdata "~A" title))
                               (ul -
                                   (loop
                                     :for (fn text) :in *navigation*
                                     :unless (equalp fn filename)
                                     :do (li -  (a (:href (make-url fn)) (pcdata "~A" text)))))
                               (footer filename)))))))
    ;; ---
    (dolist (doc packdocs)
      (render-html doc))
    *default-pathname-defaults*))

;;;; THE END ;;;;


