;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               html.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Generating HTML pages.
;;;;
;;;;    See also:
;;;;    <a href=http://www.cliki.net/HTML-from-sexpr>cliki HTML from sexpr</a>
;;;;
;;;;
;;;;
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-05-17 <PJB> New implementation.
;;;;    2004-08-23 <PJB> Added BUILD-QUERY.
;;;;    2003-05-16 <PJB> Extracted from vacation.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2003 - 2007
;;;;    mailto:pjb@informatimago.com
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTML"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CHARACTER-SETS")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING"
                "SPLIT-STRING" "STRING-REPLACE")
  (:SHADOW "MAP")
  (:EXPORT

   "*HTML-OUTPUT-STREAM*" "*HTML-CHARACTER-SET*"
   "*DOCTYPE*" "*HTML-VERSION*"
   "DOCTYPE*" "DOCTYPE"

   ;; ELEMENT objects:
   "HTML-STRING*" "COMMENT*" "CDATA*" "PCDATA*"
   "HTML-STRING"  "COMMENT"  "CDATA"  "PCDATA"
   "ELEMENT" "ELEMENT-WITH-TAG"
   "ELEMENT-WITH-BODY" "ELEMENT-WITHOUT-END"
   "ELEMENT-TAG" "ELEMENT-ATRIBUTES" "ELEMENT-BODY" "WRITE-ELEMENT"
   "HTML-STRING-TEXT" "CDATA-DATA" "PCDATA-DATA"
   "WRITE-HTML" "WITH-HTML-OUTPUT" "COLLECT-ELEMENT"

   ;; Low level stuff:
   "*HTML-XHTML-MODE-P*"  "WRITE-ESCAPING" "NORMALIZE-ATTRIBUTE-NAME"

   ;; HTML 4.01 tags functions:
   "A*" "ABBR*" "ACRONYM*" "ADDRESS*" "APPLET*" "AREA*" "B*" "BASE*"
   "BASEFONT*" "BDO*" "BIG*" "BLOCKQUOTE*" "BODY*" "BR*" "BUTTON*"
   "CAPTION*"  "CENTER*" "CITE*" "CODE*" "COL*" "COLGROUP*" "DD*"
   "DEL*" "DFN*" "DIR*"  "DIV*" "DL*" "DT*" "EM*" "FIELDSET*" "FONT*"
   "FORM*" "FRAME*" "FRAMESET*"  "H1*" "H2*" "H3*" "H4*" "H5*" "H6*"
   "HEAD*" "HR*" "HTML*" "I*" "IFRAME*" "IMG*"  "INPUT*" "INS*"
   "ISINDEX*" "KBD*" "LABEL*" "LEGEND*" "LI*" "LINK*" "MAP*"  "MENU*"
   "META*" "NOFRAMES*" "NOSCRIPT*" "OBJECT*" "OL*" "OPTGROUP*"
   "OPTION*" "P*" "PARAM*" "PRE*" "Q*" "S*" "SAMP*" "SCRIPT*"
   "SELECT*" "SMALL*"  "SPAN*" "STRIKE*" "STRONG*" "STYLE*" "SUB*"
   "SUP*" "TABLE*" "TBODY*" "TD*"  "TEXTAREA*" "TFOOT*" "TH*" "THEAD*"
   "TITLE*" "TR*" "TT*" "U*" "UL*" "VAR*"

   ;; HTML 4.01 tags macros:
   "A" "ABBR" "ACRONYM" "ADDRESS" "APPLET" "AREA" "B" "BASE"
   "BASEFONT" "BDO" "BIG" "BLOCKQUOTE" "BODY" "BR" "BUTTON" "CAPTION"
   "CENTER" "CITE" "CODE" "COL" "COLGROUP" "DD" "DEL" "DFN" "DIR"
   "DIV" "DL" "DT" "EM" "FIELDSET" "FONT" "FORM" "FRAME" "FRAMESET"
   "H1" "H2" "H3" "H4" "H5" "H6" "HEAD" "HR" "HTML" "I" "IFRAME" "IMG"
   "INPUT" "INS" "ISINDEX" "KBD" "LABEL" "LEGEND" "LI" "LINK" "MAP"
   "MENU" "META" "NOFRAMES" "NOSCRIPT" "OBJECT" "OL" "OPTGROUP"
   "OPTION" "P" "PARAM" "PRE" "Q" "S" "SAMP" "SCRIPT" "SELECT" "SMALL"
   "SPAN" "STRIKE" "STRONG" "STYLE" "SUB" "SUP" "TABLE" "TBODY" "TD"
   "TEXTAREA" "TFOOT" "TH" "THEAD" "TITLE" "TR" "TT" "U" "UL" "VAR")

  (:DOCUMENTATION
   "This package exports functions to generate HTML pages.

    Copyright Pascal J. Bourguignon 2003 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTML")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *html-output-stream* *standard-output*
    "The stream used by WITH-HTML-OUTPUT and WRITE-HTML by default.")


  (defvar *html-output-elements* '()
    "The HTML macros collect issued elements into this list.")


  (DEFVAR *DOCTYPE*      :STRICT
    "The DOCTYPE of HTML document being generated.
   May be a token: :STRICT, :TRANSITIONAL, :LOOSE or :FRAMESET.")


  (DEFVAR *HTML-VERSION* "4.01"
    "The HTML version of the document being generated.")

  (defvar *html-xhtml-mode-p* nil
    "Set to true when the elements must be written in XHTML 1.0 variant.")


  (defvar *html-character-set*  (find-character-set "US-ASCII")
    "The encoding used to write the HTML code. By default it's ASCII,
even if ISO-8859-1 is the default for HTML, since ASCII is the intersection with
the Common Lisp standard character set and HTML default character set.")


  (defun collect-element (element)
    (push element *html-output-elements*)
    element)


  (defun doctype* (kind)
    (UNLESS (MEMBER KIND '(:STRICT :TRANSITIONAL :LOOSE :FRAMESET))
      (ERROR "Unexpected DOCTYPE kind. Please choose :STRICT, ~
             :TRANSITIONAL, :LOOSE or :FRAMESET."))
    (html-string
     (eCASE KIND
       ((:STRICT)
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" ~
                \"http://www.w3.org/TR/html4/strict.dtd\">~%")
       ((:TRANSITIONAL :LOOSE)
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" ~
                 \"http://www.w3.org/TR/html4/loose.dtd\">~%")
       ((:FRAMESET)
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" ~
                 \"http://www.w3.org/TR/html4/frameset.dtd\">~%"))))


  (DEFMACRO DOCTYPE (KIND &BODY BODY)
    (let ((vkind (gensym)))
      `(let* ((,vkind        ,kind)
              (*DOCTYPE*     ,vkind)
              (*HTML-VERSION* "4.01"))
         (doctype* ,vkind)
         ,@BODY)))





  ;; We consider only two kinds of elements:
  ;; <a>x</a>   elements with contents who get always an end tag
  ;; <br> or <br />    elements without contents, who never get an end tag.
  ;; We don't write <br></br>: either we generate HTML 4.01 as: <br>
  ;; or we generate XHTML 1.0 as: <br />



  (defgeneric element-tag (element))
  (defgeneric element-attributes (element))
  (defgeneric element-body (element))
  (defgeneric write-element (element stream))



  (defclass element ()
    ())
  (defmethod write-element ((self element) stream)
    (declare (ignore stream))
    self)



  (defun write-escaping (escapes string stream)
    (if (cs-ranges *html-character-set*)
        (loop
           :for ch :across string
           :for as = (assoc ch escapes)
           :do (cond
                 (as
                  (princ (cdr as) stream))
                 ((character-in-character-set-p ch *html-character-set*)
                  (princ ch stream))
                 (t
                  (format stream "&#~D;" (char-code ch)))))
        (loop
           :for ch :across string
           :for as = (assoc ch escapes)
           :do (cond
                 (as
                  (princ (cdr as) stream))
                 ((<= (char-code ch) 127)
                  (princ ch stream))
                 (t
                  (format stream "&#~D;" (char-code ch)))))))






  (defclass html-string (element)
    ((text :initarg :text :reader html-string-text :type string)))
  (defmethod write-element ((self html-string) stream)
    (write-string (html-string-text self) stream)
    self)


  (defun html-string* (control &rest arguments)
    "
RETURN:  An element storing the result of formating the CONTROL string
         with the ARGUMENTS containing HTML code.
EXAMPLE: (HTML-STRING \"<P>Some paragraph</P>\") --> #<element>
"
    (make-instance 'html-string
        :text (APPLY (FUNCTION FORMAT) NIL CONTROL ARGUMENTS)))


  (defun html-string (control &rest arguments)
    (collect-element (apply (function html-string*) control arguments)))



  (DEFUN COMMENT* (CONTROL &REST ARGUMENTS)
    "
RETURN:  An element storing the result of formating the CONTROL string
         with the ARGUMENTS as HTML comment.
"
    (html-string "~&<!-- ~A -->~%"
                 (STRING-REPLACE (APPLY (FUNCTION FORMAT) NIL CONTROL ARGUMENTS)
                                 "--"  "==")))

  (defun comment (CONTROL &REST ARGUMENTS)
    (collect-element (apply (function comment*) control arguments)))



  (defclass cdata (element)
    ((data :initarg :data :reader cdata-data :type string)))
  (defmethod initialize-instance :after ((self cdata) &rest args)
    (declare (ignore args))
    (setf (slot-value self 'data) (string-trim " " (slot-value self 'data)))
    self)
  (defmethod write-element ((self cdata) stream)
    (write-escaping '((#\& . "&amp;") (#\" . "&quot;"))
                    (cdata-data self) stream)
    self)

  (DEFUN CDATA* (CONTROL &REST ARGUMENTS)
    "
RETURN:  An element storing the result of formating the CONTROL string
         with the ARGUMENTS as CDATA (ie. post-processed to quote special
         HTML characters (&,\").
"
    (make-instance 'cdata :data (APPLY (FUNCTION FORMAT) NIL CONTROL ARGUMENTS)))

  (defun cdata (control &rest arguments)
    (collect-element (apply (function cdata*) control arguments)))


  (defclass pcdata (element)
    ((data :initarg :data :reader pcdata-data :type string)))
  (defmethod write-element ((self pcdata) stream)
    (write-escaping '((#\& . "&amp;") (#\< . "&lt;") (#\> . "&gt;"))
                    (pcdata-data self) stream)
    self)


  (DEFUN PCDATA* (CONTROL &REST ARGUMENTS)
    "
RETURN:  An element storing the result of formating the CONTROL string
         with the ARGUMENTS as CDATA (ie. post-processed to quote special
         HTML characters (<,>,&,\").
"
    (make-instance 'pcdata :data (APPLY (FUNCTION FORMAT) NIL CONTROL ARGUMENTS)))

  (defun pcdata (control &rest arguments)
    (collect-element (apply (function pcdata*) control arguments)))


  (defclass element-with-tag (element)
    ((tag        :initarg :tag
                 :reader element-tag
                 :type (or string nil))
     (attributes :initarg :attributes
                 :reader element-attributes
                 :type list
                 :initform '())))
  (defmethod print-object ((self element-with-tag) stream)
    (print-unreadable-object (self stream :identity t :type t)
      (format stream "~A" (element-tag self)))
    self)
  (defmethod write-element ((self element-with-tag) stream)
    (loop
       :for (k v) :on (element-attributes self) :by (function cddr)
       :initially (format stream "~&<~A" (element-tag self))
       :do (format stream " ~A=\"" k)
       :do (write-element v stream)
       :do (princ "\"" stream)
       :finally (princ ">" stream)))


  (defclass element-without-end (element-with-tag)
    ())



  (defclass element-with-body (element-with-tag)
    ((body :initarg :body :reader element-body :type list)))
  (defmethod write-element ((self element-with-body) stream)
    (call-next-method)
    (unwind-protect
         (dolist (element (element-body self))
           (write-element element stream))
      (format stream "</~A>" (element-tag self)))
    self)




  (defparameter *letters* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    "Only ASCII letters.")
  (defparameter *digits* "0123456789"
    "Only ASCII digits.")

  (defun token-id-p (token)
    (and (<= 1 (length token))
         (position (aref token 0) *letters*)
         (every (lambda (ch) (or  (position ch *letters*)
                                  (position ch *digits*)
                                  (position ch "-_:.")))
                token)))

  (defun token-idrefs-p (token)
    (and (<= 1 (length token))
         (every (function token-id-p) (split-string token))))

  (defun token-number-p (token)
    (and (<= 1 (length token))
         (every (lambda (ch) (position ch *digits*)) token)))



  (defun WRITE-HTML (element &optional (stream *html-output-stream*))
    "
DO:    Write the HTML encoded in the ELEMENT to the output STREAM.
"
    (write-element element stream))



  (defmacro with-html-output ((&optional (stream *html-output-stream*)
                                         &key (kind :html kindp)
                                         (encoding "US-ASCII" encodingp))
                              &body body)
    "
DO:       Execute body (collecting *HTML-TAGS*), and finally writes to
          the STREAM the HTML collected.

KIND:     indicates which kind of is used: :HTML, :XHTML or :XML.
          (tags may be generated differently in HTML 4.01 than in
          XHTML 1.0 or XML).

ENCODING: indicates which character encoding is used to write the
          document.  CDATA and PCDATA may be escaped differently
          depending on the encoding.  (The default is ASCII because
          it's the intersection between the lisp standard character
          set and the default HTML character set (ISO-8859-1).
"
    (let ((vstream (gensym)))
      `(let ((,vstream ,stream)
             (*html-output-elements* '())
             (*html-xhtml-mode-p*  ,(if kindp
                                        `(not (eq ,kind :html))
                                        `*html-xhtml-mode-p*))
             (*html-character-set* ,(if encodingp
                                        (let ((venc (gensym)))
                                          `(let ((,venc ,encoding))
                                             (if (typep ,venc 'character-set)
                                                 ,venc
                                                 (find-character-set
                                                  (string ,venc)))))
                                        `*html-character-set*)))
         (multiple-value-prog1 (progn ,@body)
           (dolist (tag (nreverse *html-output-elements*))
             (write-html tag ,vstream))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loading the DTD.
;;;
;;;  Eventually, we should parse and load the DTD sources.
;;;  For now, we have in HTML401.lisp the DTD in lisp form, and we
;;;  can interpret it with the defelement and defattribute macros.
;;;
;;;  To generate the functions and macros to build the document elements
;;;  we need both the element and attribute loaded, so we generate them
;;;  afterwards.
;;;



  ;; (eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct (dtd-element (:conc-name el-))
    name options documentation attributes)


  (defstruct (dtd-attribute (:conc-name at-))
    name elements type default options documentation)


  (defparameter *ELEMENTS*   (make-hash-table :test (function equal))
    "Maps element names to elements.")


  (defparameter *ATTRIBUTES* (make-hash-table :test (function equal))
    "Maps attribute names to attributes.")


  (DEFMACRO DEFELEMENT (NAME OPTIONS &OPTIONAL (DOCUMENTATION "A HTML element."))
    "
DO:         Defines an HTML element macro.
NAME:       A symbol that will be used to define a macro.
OPTIONS:    A list of keywords: :START-OPTIONAL :END-FORBIDDEN :EMPTY
                                :DEPRECATED :LOOSE-DTD or :FRAMESET-DTD.
            :END-FORBIDDEN  -> the close tag is not generated.
            :DEPRECATED     -> warning when the macro is used.
            :EMPTY          -> the macro won't take a BODY.
            :START-OPTIONAL -> ignored.
            :LOOSE-DTD      -> error when *DOCTYPE* isn't :LOOSE.
            :FRAMESET-DTD   -> error when *DOCTYPE* isn't :FRAMESET.
NOTE: All HTML 4.01 elements have EMPTY <=> END-FORBIDDEN.
DOCUMENTATION:  A string used as documentation string for the macro NAME.
"
    (let ((elem (make-dtd-element :name (string-downcase name)
                                  :options options
                                  :documentation documentation)))
      (setf (gethash (el-name elem) *elements*) elem))
    `',name)




  (DEFMACRO DEFATTRIBUTE (ATTR-NAME ELEMENTS TYPE DEFAULT OPTIONS DOCUMENTATION)
    "
DO:       Defines an HTML attribute.
"
    (let ((attr (make-dtd-attribute :name (normalize-attribute-name attr-name)
                                    :elements elements
                                    :type type
                                    :default default
                                    :options options
                                    :documentation documentation)))
      (setf (gethash (at-name attr) *attributes*) attr)
      (if (eq (car elements) :all-elements-but)
          (maphash (lambda (name element)
                     (unless (member name (cdr elements)
                                     :test (function string-equal))
                       (pushnew (normalize-attribute-name attr-name)
                                (el-attributes element)
                                :test (function string=))))
                   *elements*)
          (mapc (lambda (name)
                  (unless (gethash (string-downcase name) *elements*)
                    (error "~A doesn't name an element (please use ~
                                     DEFELEMENT before DEFATTRIBUTE)"
                           name))
                  (pushnew (normalize-attribute-name attr-name)
                           (el-attributes
                            (gethash (string-downcase name) *elements*))
                           :test (function string=)))
                elements)))
    (list 'quote attr-name))


  (defun check-loose (name)
    (UNLESS (EQ :LOOSE *DOCTYPE*)
      (ERROR "The element ~A can be used only with loose DTD." NAME)))


  (defun check-frameset (name)
    (UNLESS (EQ :FRAMESET *DOCTYPE*)
      (ERROR "The element ~A can be used only with frameset DTD." NAME)))




  (defun normalize-attribute-name (name)
    (etypecase name
      (string name)
      (symbol (let ((name (string name)))
                (if (every (function upper-case-p) name)
                    (string-downcase name)
                    name)))))

  (defun generate-element-macro-body (fname vattr vbody)
    (let ((vresults (gensym)))
      `(let ((,vresults))
         (push (,fname ,(cond
                         ((or (null vattr) (eq '- vattr)) '())
                         ((or (keywordp (first vattr))
                              (not (symbolp (first vattr)))
                              (and (evenp (length vattr))
                                   (loop
                                      :for (k v) :on vattr :by (function cddr)
                                      :always (keywordp k))))
                          `(list ,@vattr))
                         (t vattr))
                       (let ((*html-output-elements* '()))
                         (setf ,vresults (multiple-value-list
                                          (progn ,@vbody)))
                         (nreverse *html-output-elements*)))
               *html-output-elements*)
         (apply (function values) ,vresults))))


  (defun generate-elements (elements)
    (let ((forms '()))
      (maphash
       (lambda (name element)
         (let ((vattr (gensym))
               (vbody (gensym))
               (mname (intern (string-upcase name)))
               (fname (intern (format nil "~:@(~A*~)" name))))
           (push
            `(defmacro ,mname (&optional ,vattr &body ,vbody)
               (generate-element-macro-body ',fname ,vattr ,vbody)) forms)
           (push
            `(defun ,fname (&optional ,vattr ,vbody)
               ,@(when (member :deprecated (el-options element))
                       `((warn ,(format nil "HTML element ~A is deprecated."
                                        name))))
               ,@(COND
                  ((MEMBER :LOOSE-DTD    (el-OPTIONS element))
                   `((check-loose    ',name)))
                  ((MEMBER :FRAMESET-DTD (el-OPTIONS element))
                   `((check-frameset ',name))))
               ,@(when (member :empty (el-options element))
                       `((when ,vbody
                           (error "HTML element ~A doesn't take ~
                                   any content; ~S was given"
                                  ',name ,vbody))))
               ;; html-string :text
               ;; cdata :data
               ;; pcdata :data
               ;; element-without-end :tag :attributes
               ;; element-with-body :tag :attributes :body
               ,(if (member :empty (el-options element))
                    `(make-instance 'element-without-end
                         :tag ',(el-name element)
                         :attributes
                         (loop
                            :for (k v) :on ,vattr :by (function cddr)
                            :nconc (list
                                    (normalize-attribute-name k)
                                    (if (typep v 'cdata)
                                        v
                                        (make-instance 'cdata
                                            :data (format nil "~A" v))))))
                    `(make-instance 'element-with-body
                         :tag ',(el-name element)
                         :attributes
                         (loop
                            :for (k v) :on ,vattr :by (function cddr)
                            :nconc (list
                                    (normalize-attribute-name k)
                                    (if (typep v 'cdata)
                                        v
                                        (make-instance 'cdata
                                            :data (format nil "~A" v)))))
                         :body (mapcar (lambda (item)
                                         (if (stringp item)
                                             (make-instance 'pcdata :data item)
                                             item))
                                       ,vbody))))
            forms)))
       elements)
      (cons 'progn (nreverse forms))))



  (defmacro generate ()
    (append (generate-elements *elements*)
            '('done)))

  );;eval-when

(eval-when (:compile-toplevel :load-toplevel :execute)
  (LOAD "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;HTML401.LISP")
  (generate))



;; (DEFUN ATTRIBUTES-TO-STRING (ELEMENT ATTRIBUTES)
;;   "
;; ELEMENT: The element into which the attributes will be inserted.
;;          (To check if the required attributes are effectively present).
;; NOTE:    Not implemente yet the fine processing of defined attributes
;;          with DEFATTRIBUTE...
;; NOTE:    The attribute names and the attribute values are quoted with
;;          CDATA-QUOTE-STRING (\" and &).
;; RETURN:  A string containing the attributes formated for inclusion in
;;          a HTML tag.
;; "
;;   (DECLARE (IGNORE ELEMENT)) ;; CHECKING NOT IMPLEMENTED YET.
;;   (DO* ((ATTRIBUTES ATTRIBUTES        (CDDR ATTRIBUTES))
;;         (KEY        (CAR ATTRIBUTES)  (CAR ATTRIBUTES))
;;         (VALUE      (CADR ATTRIBUTES) (CADR ATTRIBUTES))
;;         (STRING     ()))
;;        ((NULL ATTRIBUTES)
;;         (APPLY (FUNCTION CONCATENATE) 'STRING (NREVERSE STRING)))
;;     ;; CHECK IF ATTRIBUTE BELONGS TO ELEMENT
;;     ;; CHECK ATTRIBUTE BELONG TO THIS *DOCTYPE*
;;     ;; CHECK TYPE OF ATTRIBUTE VALUE
;;     (IF (NULL VALUE) ;; is this a boolean ATTRIBUTE?
;;         (WHEN KEY (PUSH (FORMAT NIL " ~A"
;;                                 (CDATA-QUOTE-STRING (STRING KEY))) STRING))
;;         (PUSH (FORMAT NIL " ~A=\"~A\""
;;                       (CDATA-QUOTE-STRING (STRING KEY))
;;                       (CDATA-QUOTE-STRING (FORMAT NIL "~A" VALUE))) STRING)))
;;   ;; CHECK IF ALL REQUIRED ATTRIBUTES OF ELEMENT ARE PRESENT.
;;   )
;;


(DEFUN OBFUSCATE (ADDRESS)
  "
DO:         Generate an email address as an HTML string with the characters
            written as entities.
"
  (DO  ((I (1- (LENGTH ADDRESS)) (1- I))
        (RES '()))
       ((< I 0) (APPLY (FUNCTION CONCATENATE) 'STRING RES))
    (PUSH (FORMAT NIL "&#~D;" (CHAR-CODE (AREF ADDRESS I))) RES)))


;;;; THE END ;;;;
