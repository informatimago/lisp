;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               parse-html.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A Simple HTML parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-10-20 <PJB> Added PARSE-HTML-STREAM.
;;;;    2012-03-13 <PJB> Renamed package to match its position in the hierarchy.
;;;;    2005-02-22 <PJB> Optimized WALK for HTML-SEQ.
;;;;    2003-11-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.HTML-PARSER.PARSE-HTML"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-ENTITIES"
        "COM.INFORMATIMAGO.COMMON-LISP.HTML-BASE.ML-SEXP")
  #+mocl (:shadowing-import-from "COM.INFORMATIMAGO.MOCL.KLUDGES.MISSING"
                                 "*TRACE-OUTPUT*"
                                 "*LOAD-VERBOSE*"
                                 "*LOAD-PRINT*"
                                 "ARRAY-DISPLACEMENT"
                                 "CHANGE-CLASS"
                                 "COMPILE"
                                 "COMPLEX"
                                 "ENSURE-DIRECTORIES-EXIST"
                                 "FILE-WRITE-DATE"
                                 "INVOKE-DEBUGGER" "*DEBUGGER-HOOK*"
                                 "LOAD"
                                 "LOGICAL-PATHNAME-TRANSLATIONS"
                                 "MACHINE-INSTANCE"
                                 "MACHINE-VERSION"
                                 "NSET-DIFFERENCE"
                                 "RENAME-FILE"
                                 "SUBSTITUTE-IF"
                                 "TRANSLATE-LOGICAL-PATHNAME"
                                 "PRINT-NOT-READABLE"
                                 "PRINT-NOT-READABLE-OBJECT")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING" "UNSPLIT-STRING"
                "SPLIT-STRING" "STRING-REPLACE")
  (:export "UNPARSE-HTML" "WRITE-HTML-TEXT"
           "PARSE-HTML-STREAM" "PARSE-HTML-STRING" "PARSE-HTML-FILE")
  (:documentation "

This package implements a simple HTML parser.

Example:

        (parse-html-string \"&lt;html&gt;&lt;head&gt;&lt;title&gt;Test&lt;/title&gt;&lt;/head&gt;
        &lt;body&gt;&lt;h1&gt;Little Test&lt;/h1&gt;
        &lt;p&gt;How dy? &lt;a href=\\\"/check.html\\\"&gt;Check this&lt;/a&gt;&lt;/p&gt;
        &lt;ul&gt;&lt;li&gt;one&lt;li&gt;two&lt;li&gt;three&lt;/ul&gt;&lt;/body&gt;&lt;/html&gt;\")
        --> ((:html nil (:head nil (:title nil \"Test\")) \"
            \" (:body nil (:h1 nil \"Little Test\") \"
            \" (:p nil \"How dy? \" (:a (:href \"/check.html\") \"Check this\")) \"
            \" (:ul nil (:li nil \"one\" (:li nil \"two\" (:li nil \"three\")))))))

Sexp html format:

    element    ::=  (tag (&rest attributes) &rest contents) .
    tag        ::= (or symbol string) . -- usually a keyword
    attributes ::= list of (name value) .
    contents   ::= list of element | string .
    name       ::= (or symbol string) . -- usually a keyword.
    value      ::= string .

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2003 - 2015
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.HTML-PARSER.PARSE-HTML")



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tag-package* (load-time-value (find-package "KEYWORD"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scanner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (html-scanner (:constructor %make-html-scanner))
  (state      :normal           :type symbol)
  (next-state nil               :type symbol)
  (source (make-instance 'peek-stream :stream *standard-input*)
   :type peek-stream))

(defun make-html-scanner (&key (source *standard-input*) (state :normal))
  (%make-html-scanner :source (make-instance 'peek-stream :stream source)
                      :state state))

(defmacro defcharset (name characters &key complement)
  (let ((characters (eval characters))
        (table (make-array '(256) :element-type 'bit
                                  :initial-element (if complement 1 0))))
    (dotimes (i (length characters))
      (setf (aref table (char-code (aref characters i))) (if complement 0 1)))
    `(let ((table ,table))
       (defun ,name (ch)
         (let ((code (char-code ch)))
           (if (<= 0 code 255)
               (/= 0 (aref table code))
               ,complement))))))

(defcharset cs-space-p
    (format nil "~{~C~}" (mapcar (function code-char) '(32 10 13 9 11 12))))

(defcharset cs-crlf-p
    (format nil "~{~C~}" (mapcar (function code-char) '(10 13))))

(defcharset cs-string-d-char-p
    (format nil "\"~{~C~}" (mapcar (function code-char) '(10 13)))
  :complement t)

(defcharset cs-string-s-char-p
    (format nil "'~{~C~}" (mapcar (function code-char) '(10 13)))
  :complement t)

(defcharset cs-string-n-char-p
    (format nil "~{~C~}\"'=>" (mapcar (function code-char) '(32 10 13 9 11 12)))
  :complement t)
;; "-+,./0123456789:?ABCDEFGHIJKLMNOPQRSTUVWXYZZ_abcdefghijklmnopqrstuvwxyzz"

(defcharset cs-alpha-char-p
  "ABCDEFGHIJKLMNOPQRSTUVWXYZZabcdefghijklmnopqrstuvwxyz")

(defcharset cs-ident-char-p
  "0123456789-ABCDEFGHIJKLMNOPQRSTUVWXYZZ_abcdefghijklmnopqrstuvwxyz:")

;; xmlns:rdf
(defparameter +crlf+ (format nil "~C~C" (code-char 10) (code-char 13)))

;;  <td width=\"180\" align=\"rig valign=bottom nowrap><object classid=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\" codebase=\"http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=6,0,29,0\" width=\"140\" height=\"29\"><param name=\"movie\" value=\"/images/pest_tiendas.swf\"><param name=\"quality\" value=\"high\">


;; <input type="hidden" name="dire" value="Pol. Industrial, 2 "E"">
;; :: if in tag and close-" 
;; :: then look ahead while closed and not '>' and not '=' and not '/' for xml '/>'.

(defun heuristic-quote-in-string (string start end-of-string)
  "( *[a-z]+ *= *{string})/?>"
  (let* ((next-quote (position end-of-string   string :start (1+ start)))
         (equal-sign (position (character "=") string :start (1+ start)
                                                      :end next-quote)))
    ;; (format t "next-quote=~S equal-sign=~S~%" next-quote equal-sign)
    (if next-quote
        (if (and equal-sign (< equal-sign next-quote))
            (values start nil)          ; sss"xxx=xxx"
            (values (1+ next-quote) t)) ; ssss"ssss"xx
        (values start nil))))           ; sss"xxx=xxx>


(defun get-token (scanner)
  (let ((value (make-array '(16) :fill-pointer 0 :adjustable t 
                                 :element-type 'character)))
    (labels
        ((get-char-and-keep ()
           "Get the next character from the source, advance, and append it to value."
           (let ((ch (getchar (html-scanner-source scanner))))
             (when ch
               (vector-push-extend ch value))
             ch))
         (eat-char ()
           "Get the next character from the source, and advance."
           (getchar (html-scanner-source scanner)))
         (unget-char (ch)
           "Remove the character ch from the value, and put it back to the source."
           (ungetchar (html-scanner-source scanner) ch)
           (vector-pop value))
         (next-char ()
           "Peek the next character from the source.
           (eql (get-char-and-keep) (next-char)) --> true
           (eql (next-char) (get-char-and-keep)) --> true or false!
           Same with eat-char."
           (nextchar (html-scanner-source scanner)))
         ;; (has-char ()
         ;;   "Whether value is not empty."
         ;;   (plusp (fill-pointer value)))
         ;; (last-char ()
         ;;   "Return the last character in value."
         ;;   (aref value (1- (fill-pointer value))))
         )
      (declare (inline get-char-and-keep eat-char unget-char next-char))
      (case (html-scanner-state scanner)
        ((:normal)
         (let ((ch (get-char-and-keep)))
           (cond
             ;; eof
             ((null ch) (return-from get-token (values :eof nil)))
             ;; open a tag
             ((char= ch (character "<"))
              (setf ch (get-char-and-keep))
              (cond
                ;; eof
                ((null ch) (return-from get-token (values :pcdata value)))
                ;; <?
                ((char= ch (character "?"))
                 (loop
                   :named :foreign
                   :with state = :foreign
                   :for ch = (get-char-and-keep)
                   :while ch
                   :do (ecase state
                         ((:foreign) (when (char= ch (character "?"))
                                       (setf state :end)))
                         ((:end)     (if (char= ch (character ">"))
                                         (return-from :foreign
                                           (values :foreign value))
                                         (setf state :foreign))))))
                ;; <!
                ((char= ch (character "!"))
                 (setf ch (get-char-and-keep))
                 (cond
                   ;; eof
                   ((null ch) (return-from get-token (values :pcdata value)))
                   ;; <!- comment
                   ((char= ch (character "-"))
                    (loop named :comment
                          with state = :comment
                          for ch = (get-char-and-keep)
                          while ch 
                          do (case state
                               ((:comment) (when (char= ch (character "-"))
                                             (setf state :dash)))
                               ((:dash) (if (char= ch (character "-"))
                                            (setf state :dash-dash)
                                            (setf state :comment)))
                               ((:dash-dash) (cond
                                               ((char= ch (character " ")))
                                               ((char= ch (character ">"))
                                                (return-from :comment
                                                  (values :comment value)))
                                               (t (setf state :comment)))))))
                   ;; <! open definition
                   (t (unget-char ch)
                      (setf (html-scanner-state scanner) :tag)
                      (values :open-def value))))
                ;; </ open close tag
                ((char= ch (character "/"))
                 (setf (html-scanner-state scanner) :tag)
                 (values :close-tag value))
                ;; < open open tag
                (t
                 (unget-char ch) 
                 (setf (html-scanner-state scanner) :tag-ident)
                 (values :open-tag value))))
             ;; outside of a tag: pcdata
             (t (loop for ch = (get-char-and-keep)
                      while (and ch (char/= ch (character "<")))
                      finally (when ch (unget-char ch)))
                (values :pcdata value)))))
        ((:script :style)
         (setf (html-scanner-state scanner) :normal)
         (values :cdata (loop
                          :for ch = (get-char-and-keep)
                          :for nc = (next-char)
                          :while (and ch nc
                                      (or (char/= ch (character "<"))
                                          (char/= nc (character "/"))))
                          :finally (progn (unget-char ch) (return value)))))
        ((:tag-ident :tag)
         (let ((ch (loop
                     :for ch = (eat-char)
                     :while (and ch (cs-space-p ch))
                     :finally (progn (when ch (vector-push-extend ch value))
                                     (return ch)))))
           (cond
             ;; eof
             ((null ch) (return-from get-token (values :eof nil)))
             ;; /> close close tag
             ((and (char= ch (character "/"))
                   (char= (next-char) (character ">")))
              (get-char-and-keep)
              (setf (html-scanner-state scanner) :normal)
              (values :close-end-tag value))
             ;; > close close tag
             ((char= ch (character ">"))
              (setf (html-scanner-state scanner)
                    (if (html-scanner-next-state scanner)
                        (prog1  (html-scanner-next-state scanner)
                          (setf (html-scanner-next-state scanner) nil))
                        :normal))
              (values :end-tag value))
             ;; identifier
             ((cs-alpha-char-p ch) ;;ident
              (loop for ch = (get-char-and-keep)
                    while (and ch (cs-ident-char-p ch))
                    finally (when ch (unget-char ch)))
              (when (eq (html-scanner-state scanner) :tag-ident)
                (setf (html-scanner-state scanner) :tag)
                (cond
                  ((string-equal "script" value)
                   (setf (html-scanner-next-state scanner) :script))
                  ((string-equal "style" value)
                   (setf (html-scanner-next-state scanner) :style))))
              (values :identifier value))
             ;; "string" or 'string'
             ((or (char= ch (character "\"")) ;; string-d
                  (char= ch (character "'"))) ;; string-n
              (let ((char-set (if (char= ch (character "\""))
                                  (function cs-string-d-char-p)
                                  (function cs-string-s-char-p)))
                    (end-of-string ch))
                (vector-pop value) ; remove the string openining character
                (tagbody
                 :go-on
                   (loop
                     :for ch = (get-char-and-keep)
                     :while (and ch (or (funcall char-set ch)
                                        (find ch +crlf+)))
                     :when (find ch +crlf+)
                       :do  (warn "Newline inside a string: ~S" value)
                     :finally (if (or (null ch) (char/= ch end-of-string))
                                  (error "Newline inside a string: ~S" value)
                                  (vector-pop value))) ; remove the string closing character
                   ;; There are two common syntax error found in web pages
                   ;; (both hand-written and generated automatically):
                   ;; 1- A closing attribute double quote is duplicated at
                   ;;    the end of the tag:   <tag attrib="xyz"">
                   ;; 2- Double-quotes are used inside the attribute to quote:
                   ;;    <tag attrib="Jojo "The Beef" Steack">
                   ;; Notice that the end of the tag could be ">" or " />"
                   
                   ;; (cond
                   ;;   ((char= (next-char) end-of-string)
                   ;;    ;; let's handle a syntax error: <tag attrib="xyz"">
                   ;;    (let ((ch (eat-char)))
                   ;;      (when (find (next-char) "/>")
                   ;;        (unget-char ch))))
                   ;; 
                   ;;   ((and (has-char) (char= (last-char) #\space)
                   ;;         (not (find (next-char) " />")))
                   ;;    ;; let's handle a syntax error: 
                   ;;    ;; <tag attrib="Jojo "The Beef" Steack">
                   ;;    (let ((start (length value)))
                   ;;      (vector-push-extend end-of-string value) 
                   ;;      (loop ; collect all chars till the next #\>
                   ;;         :for ch = (get-char-and-keep)
                   ;;         :while (and ch (not (find ch ">"))))
                   ;;      (multiple-value-bind (end go-on)
                   ;;          (heuristic-quote-in-string value start end-of-string)
                   ;;        (loop ; put back characters after the string
                   ;;           :while (< end (length value))
                   ;;           :do (unget-char (last-char)))
                   ;;        (when go-on (go :go-on))))))


                   ))
              (values :string value))
             ;; unquoted attribute: cdata
             ((cs-string-n-char-p ch) ;; string-n
              (loop for ch = (get-char-and-keep)
                    while (and ch (cs-string-n-char-p ch))
                    finally (unget-char ch))
              (values :string value))
             ((char= ch (character "="))
              (values :equal value))
             (t (error "Invalid character '~C' in tag." ch)))))
        (otherwise (error "Invalid state ~S" 
                          (html-scanner-state scanner)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Keep these structures disjoint types for the walk methods below.
(defstruct html-seq first rest)
(defstruct comment data)
(defstruct foreign data)
(defstruct pattribute name value)
(defstruct definition name attributes)
(defstruct open-tag name attributes closed)
(defstruct close-tag name attributes)


(defun normalize-tag (tag)
  (intern (string-upcase tag) *tag-package*))

(defstruct html-parser  
  scanner
  token value
  next-token next-value)

(defun advance (parser)
  (multiple-value-bind (tok val) (get-token (html-parser-scanner parser))
    (setf (html-parser-token parser)      (html-parser-next-token parser)
          (html-parser-value parser)      (html-parser-next-value parser) 
          (html-parser-next-token parser) tok
          (html-parser-next-value parser) val)))

;; Productions

(defun report-error (parser message)
  (error "~A; (~S ~S) (~S ~S)" message
         (html-parser-token parser)
         (html-parser-value parser)
         (html-parser-next-token parser)
         (html-parser-next-value parser)))

(defun parse-file (parser)
  ;; file --> tag        { html-seq: [(first tag)  (rest nil) ] }
  ;;        | data       { html-seq: [(first data) (rest nil) ] }
  ;;        | tag  file  { html-seq: [(first tag)  (rest file)] }
  ;;        | data file  { html-seq: [(first data) (rest file)] } ;
  (loop
    :for synthetic = (case (html-parser-token parser)
                       ((:eof)       nil)
                       ((:pcdata)    (prog1 (html-parser-value parser) 
                                       (advance parser)))
                       ((:cdata)     (prog1 (html-parser-value parser) 
                                       (advance parser)))
                       ((:open-def)  (parse-definition parser))
                       ((:foreign)   (prog1 (make-foreign
                                             :data (html-parser-value parser))
                                       (advance parser)))
                       ((:comment)   (prog1 (make-comment 
                                             :data (let ((text (html-parser-value parser)))
                                                     (subseq text 4 (- (length text) 3))))
                                       (advance parser)))
                       ((:open-tag)  (parse-open-tag parser))
                       ((:close-tag) (parse-close-tag parser))
                       (otherwise   (report-error parser "Unexpected token")))
    :while synthetic
    :collect synthetic))

(defun parse-definition (parser)
  ;; tag -->   "<!" ident ">"
  ;;            { definition: [(name ident) ] }
  ;;         | "<!" ident aivs ">"
  ;;            { definition: [(name ident) (pattributes aivs)] }
  (advance parser)
  (unless (eq :identifier (html-parser-token parser))
    (report-error parser "Expected an identifier"))
  (let ((ident (normalize-tag (html-parser-value parser))))
    (advance parser)
    (if (eq :end-tag (html-parser-token parser))
        (progn (advance parser)
               (make-definition :name ident :attributes nil))
        (let ((pattributes (loop :while (member (html-parser-token parser) '(:string :identifier))
                                :collect (html-parser-value parser)
                                :do (advance parser))))
          (if (eq :end-tag (html-parser-token parser))
              (advance parser)
              (report-error parser "Expected a \">\""))
          (make-definition :name ident :attributes pattributes)))))

(defun parse-open-tag (parser)
  ;;         | "<" ident ">"
  ;;            { open-tag: [(name ident) ] }
  ;;         | "<" ident pattributes ">"
  ;;            { open-tag: [(name ident) (pattributes pattributes)] }
  (advance parser)
  (unless (eq :identifier (html-parser-token parser))
    (report-error parser "Expected a tag identifier"))
  (let ((tag (prog1 (make-open-tag 
                     :name (prog1 (normalize-tag (html-parser-value parser))
                             (advance parser))
                     :attributes (parse-attributes parser)
                     :closed (eq (html-parser-token parser) :close-end-tag))
               (unless (member (html-parser-token parser)
                               '(:end-tag :close-end-tag))
                 (report-error parser "Expected a \">\""))
               (advance parser))))
    tag))

(defun parse-close-tag (parser)
  ;; Same as open-tag, but for the make-close-tag.
  (advance parser)
  (unless (eq :identifier (html-parser-token parser))
    (report-error parser "Expected a tag identifier")) 
  (prog1 (make-close-tag 
          :name (prog1 (normalize-tag (html-parser-value parser))
                  (advance parser))
          :attributes (parse-attributes parser))
    (unless (eq :end-tag (html-parser-token parser))
      (report-error parser "Expected a \">\""))
    (advance parser)))

(defun parse-attributes (parser)
  ;; attributes -->   attribute
  ;;                  { html-seq: [(first attribute) (rest nil) ] }
  ;;                | attribute attributes 
  ;;                  { html-seq: [(first attribute) (rest attributes)] };
  (loop
    :while (member (html-parser-token parser) '(:identifier :string))
    :collect (parse-attribute parser)))

(defun parse-attribute (parser)
  ;; attribute -->   ident
  ;;                  { attribute: [(name ident) (value nil)  ] }
  ;;               | ident "=" value 
  ;;                  { attribute: [(name ident) (value value)] } ;
  (unless (member (html-parser-token parser) '(:identifier :string))
    (report-error parser "Expected an attribute identifier or a string."))
  (make-pattribute 
   :name (prog1 (normalize-tag (html-parser-value parser))
           (advance parser))
   :value (clean-attribute (if (eq :equal (html-parser-token parser))
                               (progn (advance parser)
                                      (unless (member (html-parser-token parser)
                                                      '(:string :identifier))
                                        (report-error parser "Expected an attribute value"))
                                      (prog1 (string (html-parser-value parser))
                                        (advance parser)))
                               (html-parser-value parser)))))

(defun clean-attribute (attr)
  "If the attribute name is quoted or double-quoted, then remove those quotes."
  (cond ((not (stringp attr)) attr)
        ((or (and (<= 2 (length attr))
                  (char= (character "'") (char attr 0))
                  (char= (character "'") (char attr (1- (length attr)))))
             (and (<= 2 (length attr))
                  (char= (character "\"") (char attr 0))
                  (char= (character "\"") (char attr (1- (length attr))))))
         (subseq attr 1 (- (length attr) 1)))
        (t    attr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defstruct comment data)
;; (defstruct foreign data)
;; (defstruct pattribute name value)
;; (defstruct definition name pattributes)
;; (defstruct open-tag name pattributes closed)
;; (defstruct close-tag name pattributes)

(defstruct pelement name attributes contents closed)
(defparameter *newline* (string #\newline) "A string containing only a newline.")

(defgeneric unwrap-pattribute (pattribute)
  (:method ((pattribute pattribute))
    (make-attribute (pattribute-name pattribute) (pattribute-value pattribute))))

(defgeneric unwrap-element (element)
  (:method ((element t))
    element)
  (:method ((element foreign))
    (make-element :foreign '() (list (foreign-data element))))
  (:method ((element comment))
    (make-element :comment '() (list (comment-data element))))
  (:method ((element definition))
    (make-element  :definition '() (definition-attributes element)))
  (:method ((element pelement))
    (make-element (pelement-name element)
                  (mapcan (function unwrap-pattribute) (pelement-attributes element))
                  (remove *newline* (mapcar (function unwrap-element) (pelement-contents element))
                          :test (function equal)))))


(defun encase (tok-list)
  "
Transform a list of tokens and content strings, into a :document sexp tree
structured according to the OPEN-TAG and (optional) CLOSE-TAG tokens.
"
  (loop
    :with stack := (list (make-pelement :name :document))
    :for tok :in tok-list
    :do (etypecase tok
          ((or string comment foreign definition)
           (push tok (pelement-contents (car stack))))
          (open-tag
           (let ((pelement (make-pelement :name (open-tag-name tok)
                                          :attributes (open-tag-attributes tok)
                                          :closed (open-tag-closed tok))))
             (if (pelement-closed pelement)
                 (push pelement (pelement-contents (car stack)))
                 (push pelement stack))))
          (close-tag
           (if (position (close-tag-name tok) stack :key (function pelement-name))
               (loop
                 :for top := (pop stack)
                 :do (setf (pelement-closed top) t
                           (pelement-contents top) (nreverse (pelement-contents top)))
                     (push top (pelement-contents (car stack)))
                 :until (eq (close-tag-name tok) (pelement-name top)))
               #|else ignore the unbalanced close tag|#)))
    :finally (return (loop
                       :for top := (pop stack)
                       :do (setf (pelement-closed top) t
                                 (pelement-contents top) (nreverse (pelement-contents top)))
                       :while stack
                       :do (push top (pelement-contents (car stack)))
                       :finally (return top)))))



(defun parse-html-stream (stream &key (verbose nil))
  "
DO:                 Parse the HTML stream STREAM.
VERBOSE:            When true, writes some information in the *TRACE-OUTPUT*.
RETURN:             A list of html elements.
SEE ALSO:           ELEMENT-TAG, ELEMENT-ATTRIBUTES, ATTRIBUTE-NAMED, ELEMENT-CHILDREN.
"
  (let ((name (or (ignore-errors (namestring stream))
                  (princ-to-string stream)))
        synthetic encased)
    (when verbose
      (format *trace-output* "~&starting parsing of file ~S~%" name))
    (setf synthetic (let ((parser (make-html-parser :scanner (make-html-scanner :source stream))))
                      (advance parser)
                      (advance parser)
                      (parse-file parser)))
    (when verbose (format *trace-output* "~&file ~S parsed~%" name))
    (setf encased (unwrap-element (encase  synthetic)))
    (when verbose (format *trace-output* "~&file ~S encased -- done.~%" name))
    encased))


(defun parse-html-file (pathname &key (verbose nil) (external-format :default))
  "
DO:                 Parse the HTML file PATHNAME.
VERBOSE:            When true, writes some information in the *TRACE-OUTPUT*.
EXTERNAL-FORMAT:    The external-format to use to open the HTML file.
RETURN:             A list of html elements.
SEE ALSO:           ELEMENT-TAG, ELEMENT-ATTRIBUTES, ATTRIBUTE-NAMED, ELEMENT-CHILDREN.
"
  (with-open-file (src pathname :direction :input 
                                :if-does-not-exist :error
                                :external-format external-format)
    (parse-html-stream src :verbose verbose)))


(defun parse-html-string (string &key (start 0) (end (length string)) (verbose nil))
  "
DO:                 Parse the HTML in the STRING (between START and END)
VERBOSE:            When true, writes some information in the *TRACE-OUTPUT*.
RETURN:             A list of html elements.
SEE ALSO:           ELEMENT-TAG, ELEMENT-ATTRIBUTES, ATTRIBUTE-NAMED, ELEMENT-CHILDREN.
" 
  (when verbose
    (format *trace-output* "~&starting string parsing from ~D~%" start))
  (with-input-from-string (src string :start start :end end)
    (parse-html-stream src :verbose verbose)))




(defparameter *nl* (make-hash-table)
  "
This hash-table maps tag symbols (interned in *TAG-PACKAGE*)
to a list of two elements:

- a list of keywords indicating the newlines that should be written
  around the element when writing HTML:
       :bo  before open tag.
       :ao  after open tag.
       :bc  before close tag.
       :ac  after open tag.

- a function taking the element as parameted (named SELF), used
  to format the element as (reStructured)text.
")

(defun element-key (html)
  (intern (string (element-tag html)) *tag-package*))

(defun must-new-line (html where)
  (member where (first (gethash (element-key html) *nl*))))


(defun write-text (element)
  (typecase element
    (string (princ (melt-entities element)))
    (atom   (princ element))
    (otherwise
     (flet ((write-it ()
              (let ((entry (gethash (element-tag element) *nl*)))
                (if (second entry)
                    (funcall (second entry) element)
                    (progn
                      (when (intersection '(:bo :ao) (first entry))
                        (terpri))
                      (print element *trace-output*)
                      (princ element)
                      (when (intersection '(:bc :ac) (first entry))
                        (terpri)))))))
       (cond ((member (element-tag element) '(:foreign :definition :comment)
                      :test (function string-equal))
              #|ignore|#)
             ((member (element-tag element) '(:pre :quote :address)
                      :test (function string-equal))
              (let ((*pre* t))
                (write-it)))
             (t
              (write-it)))))))

(defun write-children-text (self)
  (dolist (child (element-children self))
    (write-text child)))

(defun write-nothing (self)
  (declare (ignore self))
  (values))

(defun write-title (self line-char &optional abovep)
  (let* ((title (remove #\newline
                        (with-output-to-string (*standard-output*)
                          (write-children-text self))))
         (line  (make-string (length title) :initial-element line-char)))
    (terpri) (terpri)
    (when abovep
      (princ line) (terpri))
    (princ title) (terpri)
    (princ line) (terpri) (terpri)))

(defun write-indented-children (self)
  (dolist (line (split-string (with-output-to-string (*standard-output*)
                                (write-children-text self))
                              #(#\newline)))
    (princ "   ") (princ line) (terpri)))

(defun write-parenthesized-children (self left right)
  (princ left)
  (write-children-text self)
  (princ right))

(defmacro define-element-writer (tag nls &body body)
  `(progn
     (setf (gethash (intern (string-upcase ,(symbol-name tag)) *tag-package*) *nl*)
           (list ',nls
                 ,(case (first body)
                    ((:children) `(function write-children-text))
                    ((:skip)     `(function write-nothing))
                    (otherwise   `(lambda (self)
                                    (block ,tag
                                      ,@body))))))
     ',tag))

;; ^  <a>^  x^  </a>^
;; :bo   :ao :bc    :ac

(define-element-writer a                ()                 :children)
(define-element-writer abbr             ()                 :children)
(define-element-writer acronym          ()                 :children)
(define-element-writer address          (:bo :ac)          :children)
(define-element-writer applet           (:bo :ao :bc :ac)  :skip)
(define-element-writer area             (:bo :ac)          :children)
(define-element-writer b                ()                 (write-parenthesized-children self "**" "**"))
(define-element-writer base             ()                 :children)
(define-element-writer basefont         ()                 :children)
(define-element-writer bdo              ()                 :children)
(define-element-writer big              ()                 :children)
(define-element-writer blockquote       (:bo :ao :bc :ac)  (write-indented-children self))
(define-element-writer body             (:bo :ao :bc :ac)  :children)
(define-element-writer br               (:bo :ac)
  (terpri)
  (write-children-text self))
(define-element-writer button           (:bo :ac)          :children)
(define-element-writer center           (:bo :ac)          :children)
(define-element-writer cite             ()                 :children)
(define-element-writer code             ()                 (write-parenthesized-children self "`" "`"))
(define-element-writer del              ()                 :children)
(define-element-writer dfn              ()                 :children)
(define-element-writer dir              ()                 :children)
(define-element-writer div              (:bo :ac)          :children)
(define-element-writer em               ()                 (write-parenthesized-children self "*" "*"))
(define-element-writer fieldset         ()                 :children)
(define-element-writer font             ()                 :children)
(define-element-writer form             (:bo :ao :bc :ac)  :children)
(define-element-writer frame            (:bo :ao :bc :ac)  :children)
(define-element-writer frameset         (:bo :ao :bc :ac)  :children)
(define-element-writer h1               (:bo :ac)          (write-title self #\#))
(define-element-writer h2               (:bo :ac)          (write-title self #\*))
(define-element-writer h3               (:bo :ac)          (write-title self #\=))
(define-element-writer h4               (:bo :ac)          (write-title self #\-))
(define-element-writer h5               (:bo :ac)          (write-title self #\^))
(define-element-writer h6               (:bo :ac)          (write-title self #\.))
(define-element-writer head             (:bo :ao :bc :ac)
  :children)
(define-element-writer hr               (:bo :ac)
  (terpri)
  (princ (make-string 78 :initial-element #\-))
  (terpri)
  (write-children-text self))
(define-element-writer html             (:bo :ao :bc :ac)  :children)
(define-element-writer i                ()                 (write-parenthesized-children self "/" "/"))
(define-element-writer iframe           (:bo :ao :bc :ac)  :children)
(define-element-writer img              (:bo :ac)
  (let ((alt (attribute-named self :alt)))
    (when alt (princ alt))))
(define-element-writer input            (:bo :ac)          :children)
(define-element-writer ins              ()                 :children)
(define-element-writer isindex          ()                 :children)
(define-element-writer kbd              ()                 (write-parenthesized-children self "[" "]"))
(define-element-writer label            ()                 :children)
(define-element-writer legend           ()                 :children)
(define-element-writer link             (:bo :ac)          :children)
(define-element-writer map              (:bo :ao :bc :ac)  :children)
(define-element-writer menu             (:bo :ao :bc :ac)  :children)
(define-element-writer meta             (:bo :ac)          :children)
(define-element-writer noframes         (:bo :ao :bc :ac)  :children)
(define-element-writer noscript         (:bo :ao :bc :ac)  :children)
(define-element-writer object           (:bo :ao :bc :ac)  :children)
(define-element-writer optgroup         (:bo :ao :bc :ac)  :children)
(define-element-writer option           (:bo :ac)          :children)
(define-element-writer p                (:bo :ac)
  (terpri) (terpri)
  (write-children-text self))
(define-element-writer param            (:bo :ac)          :children)
(define-element-writer pre              (:bo :ac)
  (terpri)
  (princ "::")
  (terpri) (terpri)
  (write-indented-children self)
  (terpri) (terpri))
(define-element-writer q                ()                 :children)
(define-element-writer s                ()                 :children)
(define-element-writer samp             ()                 :children) 
(define-element-writer script           (:bo :ao :bc :ac)  :skip)
(define-element-writer select           (:bo :ao :bc :ac)  :children)
(define-element-writer small            ()                 :children)
(define-element-writer span             ()                 :children)
(define-element-writer strike           ()                 :children)
(define-element-writer strong           ()                 (write-parenthesized-children self "**" "**"))
(define-element-writer style            (:bo :ac)          :children)
(define-element-writer sub              ()                 :children)
(define-element-writer sup              ()                 :children)
(define-element-writer textarea         (:bo :ac)          :children)
(define-element-writer title            (:bo :ac)          (write-title self #\# t))
(define-element-writer tt               ()                 (write-parenthesized-children self "`" "`"))
(define-element-writer u                ()                 :children)
(define-element-writer var              ()                 :children)


(defvar *ol-index* nil)
(defvar *ol-stack* '())

(define-element-writer dl               (:bo :ac)
  (push *ol-index* *ol-stack*)
  (setf *ol-index* nil)
  (terpri) (write-children-text self) )
(define-element-writer dt               (:bo)
  (terpri)
  (write-children-text self))
(define-element-writer dd               (:bo)
  (terpri)
  (write-indented-children self))

(define-element-writer ol               (:bo :ao :bc :ac)
  (push *ol-index* *ol-stack*)
  (setf *ol-index* 0)
  (terpri) (write-children-text self) )
(define-element-writer ul               (:bo :ao :bc :ac)
  (push *ol-index* *ol-stack*)
  (setf *ol-index* nil)
  (terpri) (write-children-text self))
(define-element-writer li               (:bo :ac)
  (if (integerp *ol-index*)
      (format t "~%~D. " (incf *ol-index*))
      (format t "~%- "))
  (write-indented-children self))



(defvar *row-kind* :body)
(defstruct row   kind tag attributes cells)
(defstruct cell           attributes lines)


(defun collect-table-cells (element)
  (when (listp element)
    (case (element-tag element)
      ((:table)  (let ((*row-kind* :body)
                       (rows       '()))
                   (dolist (child (element-children element) rows)
                     (when (listp child)
                       (case (element-tag child)
                         ((:thead :tbody) (appendf rows (collect-table-cells child)))
                         ((:th :tr)       (appendf rows (list (collect-table-cells child))))
                         ((:caption :col :colgroup) #| ignore for now |#)
                         (otherwise       (warn "Stray element in table: ~S" element)))))))
      ((:thead)  (let ((*row-kind* :head))
                   (collect-table-cells element)))
      ((:tbody)  (let ((*row-kind* :body))
                   (collect-table-cells element)))
      ((:th :tr) (make-row
                  :kind       *row-kind*
                  :tag        (element-tag element)
                  :attributes (element-attributes element)
                  :cells      (mapcar (function collect-table-cells)
                                      (remove-if-not (lambda (element)
                                                       (and (listp element)
                                                            (eql :td (element-tag element))))
                                                     (element-children element)))))
      ((:td)     (make-cell :attributes (element-attributes element)
                            :lines      (split-string (with-output-to-string (*standard-output*)
                                                        (write-children-text element))
                                                      #(#\newline))))
      (otherwise (warn "Stray element in table: ~S" element)
       nil))))

(defun compute-max-widths (rows)
  (reduce
   (lambda (widths cells)
     (loop
       :for width :in widths
       :for current-cells = cells :then (cdr current-cells)
       :for cell = (car current-cells)
       :collect (if cell
                    (reduce (function max)
                            (mapcar (function length) (cell-lines cell))
                            :initial-value width)
                    width)))
   rows
   :key (function row-cells)
   :initial-value (make-list (reduce (function max) (mapcar (compose length row-cells) rows))
                             :initial-element 0)))

(defun generate-line (widths)
  (with-output-to-string (*standard-output*)
    (princ "+")
    (dolist (width widths)
      (princ (make-string (+ 2 width) :initial-element #\-))
      (princ "+"))
    (terpri)))

(defun generate-control-string (widths)
  (format nil "~~{|~:{ ~~:[~A~~;~~:*~~~DA~~] |~}~~%~~}"
          (mapcar (lambda (width) (list (make-string width :initial-element #\space) width))
                  widths)))

(define-element-writer table            (:bo :ao :bc :ac)
  ;; TODO: deal with colspan, rowspan.
  (let* ((*row-kind* :table)
         (rows       (collect-table-cells self))
         (widths     (compute-max-widths rows))
         (line       (generate-line widths))
         (cstr       (generate-control-string widths)))
    (terpri)
    (princ line)
    (loop
      :for cells :in (mapcar (lambda (row) (mapcar (function cell-lines) (row-cells row))) rows)
      :unless (null cells)
        :do (loop
              :for data = (loop
                            :for current :on cells
                            :collect (pop (car current)))
              :do (format t cstr data)
              :while (some (function identity) cells)
              :finally (princ line)))))
(define-element-writer caption          ()                 :children)
(define-element-writer colgroup         ()                 :children)
(define-element-writer col              ()                 :children)
(define-element-writer thead            (:bo :ao :bc :ac)  :children)
(define-element-writer tfoot            (:bo :ao :bc :ac)  :children)
(define-element-writer tbody            (:bo :ao :bc :ac)  :children)
(define-element-writer tr               (:bo :ac)          :children)
(define-element-writer th               (:bo :ac)          :children)
(define-element-writer td               (:bo :ac)          :children)

(defun write-html-text (html &optional (stream *standard-output*))
  "Writes on STREAM a textual rendering of the HTML.
Some reStructuredText formating is used.
Simple tables are rendered, but colspan and rowspan are ignored.
"
  (let ((*standard-output* stream))
    (if (string-equal (element-tag html) :document)
        (dolist (child (element-children html))
          (write-text child))
        (write-text html))))


(defun tag-case (tag)
  (let ((stag (string tag)))
    (if (every (lambda (ch)
                 (if (alpha-char-p ch)
                     (upper-case-p ch)
                     t))
               stag)
        (string-downcase stag)
        stag)))

(defun unparse-html (html &optional (stream *standard-output*))
  "Writes back on STREAM the reconstituted HTML source."
  (let ((package *package*))
    (with-standard-io-syntax
      (let ((*package* package))
        (cond
          ((atom html)
           (format stream "~A" html))
          ((string-equal (element-tag html) :document)
           ;; (:document nil …)
           (dolist (child (element-children html))
             (unparse-html child stream)))
          ((string-equal (element-tag html) :foreign)
           ;; (:foreign nil "<?xml version=\"1.0\" encoding=\"utf-8\" ?>")
           (format stream "~&~{~A~}~%" (element-children html)))
          ((string-equal (element-tag html) :comment)
           ;; (:foreign nil "<?xml version=\"1.0\" encoding=\"utf-8\" ?>")
           (format stream "~&<!--~{~A~}-->~%" (element-children html)))
          ((string-equal (element-tag html) :definition)
           ;; (:definition () :doctype "html" "PUBLIC" "-//W3C//DTD XHTML 1.0 Transitional//EN"
           ;;                        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd")
           (format stream "~&<!~{~A~^ ~}>~%" (cddr html)))
          (t
           (let ((nl (first (gethash (element-key html) *nl*))))
             (format stream "~:[~;~&~]<~A~{ ~A=~S~}>~:[~;~&~]" 
                     (member :bo nl)
                     (tag-case (element-tag html))
                     (loop :for (attr val) :on (element-attributes html) :by (function cddr)
                           :nconc (list (tag-case attr) val))
                     (member :ao nl))
             (dolist (child (element-children html))
               (unparse-html child stream))
             (format stream "~:[~;~&~]</~A>~:[~;~&~]"
                     (member :bc nl)
                     (tag-case (element-tag html))
                     (member :ac nl)))))))))

;;;; THE END ;;;;

