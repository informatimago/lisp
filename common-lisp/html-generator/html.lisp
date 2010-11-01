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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
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
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML")



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
                                      :for (k) :on vattr :by (function cddr)
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




;; (eval-when (:compile-toplevel :load-toplevel)
;;   
;;   #+(or allegro ccl ecl) (LOAD #P"PACKAGES:com;informatimago;common-lisp;html401.lisp")
;;   #-(or allegro ccl ecl) (LOAD (merge-pathnames
;;                                 (make-pathname :case :common
;;                                                :directory '(:relative :up "HTML-BASE")
;;                                                :name "HTML401"
;;                                                :type "LISP"
;;                                                :defaults (or *compile-file-pathname*
;;                                                              *load-pathname*))
;;                                 (print (or *compile-file-pathname*
;;                                            *load-pathname*))
;;                                 nil))
;;   
;;   (generate))


;;;;---------------------------------------------------------------------------

;;;; -*- coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:               html401.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    HTML 4.01 DTD
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-11-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
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
;;;;***************************************************************************


;;
;; (defelement name options [documentation-string])
;; options: ([:start-optional] [:end-forbidden] [:empty] [:deprecated]
;;           [:loose-dtd] [:frameset-dtd])
;;

(DEFELEMENT A          ()  "anchor")
(DEFELEMENT ABBR       ()  "abbreviated form (e.g., WWW, HTTP, etc.)")
(DEFELEMENT ACRONYM    ())
(DEFELEMENT ADDRESS    ()                        "information on author")
(DEFELEMENT APPLET     (:DEPRECATED :LOOSE-DTD)  "Java applet")
(DEFELEMENT AREA       (:END-FORBIDDEN :EMPTY)   "client-side image map area")
(DEFELEMENT B          ()                        "bold text style")
(DEFELEMENT BASE       (:END-FORBIDDEN :EMPTY)   "document base URI")
(DEFELEMENT BASEFONT   (:END-FORBIDDEN :EMPTY :DEPRECATED :LOOSE-DTD)
  "base font size")
(DEFELEMENT BDO        ()                        "I18N BiDi over-ride")
(DEFELEMENT BIG        ()                        "large text style")
(DEFELEMENT BLOCKQUOTE ()                        "long quotation")
(DEFELEMENT BODY       (:START-OPTIONAL :END-OPTIONAL)
  "document body")
(DEFELEMENT BR         (:END-FORBIDDEN :EMPTY)   "forced line break")
(DEFELEMENT BUTTON     ()                        "push button")
(DEFELEMENT CAPTION    ()                        "table caption")
(DEFELEMENT CENTER     (:DEPRECATED :LOOSE-DTD)
  "shorthand for DIV align=center")
(DEFELEMENT CITE       ()                        "citation")
(DEFELEMENT CODE       ()                        "computer code fragment")
(DEFELEMENT COL        (:END-FORBIDDEN :EMPTY)   "table column")
(DEFELEMENT COLGROUP   (:END-OPTIONAL)           "table column group")
(DEFELEMENT DD         (:END-OPTIONAL)  "defelementinition description")
(DEFELEMENT DEL        ()                        "deleted text")
(DEFELEMENT DFN        ()                        "instance defelementinition")
(DEFELEMENT DIR        (:DEPRECATED :LOOSE-DTD)  "directory list")
(DEFELEMENT DIV        ()  "generic language/style container")
(DEFELEMENT DL         ()  "defelementinition list")
(DEFELEMENT DT         (:END-OPTIONAL)           "defelementinition term")
(DEFELEMENT EM         ()                        "emphasis")
(DEFELEMENT FIELDSET   ()                        "form control group")
(DEFELEMENT FONT       (:DEPRECATED :LOOSE-DTD)  "local change to font")
(DEFELEMENT FORM       ()  "interactive form")
(DEFELEMENT FRAME      (:END-FORBIDDEN :EMPTY :FRAMESET-DTD)  "subwindow")
(DEFELEMENT FRAMESET   (:FRAMESET-DTD)           "window subdivision")
(DEFELEMENT H1         ()                        "Heading")
(DEFELEMENT H2         ()                        "Heading")
(DEFELEMENT H3         ()                        "Heading")
(DEFELEMENT H4         ()                        "Heading")
(DEFELEMENT H5         ()                        "Heading")
(DEFELEMENT H6         ()                        "Heading")
(DEFELEMENT HEAD       (:START-OPTIONAL :END-OPTIONAL)  "document head")
(DEFELEMENT HR         (:END-FORBIDDEN :EMPTY)   "horizontal rule")
(DEFELEMENT HTML       (:START-OPTIONAL :END-OPTIONAL)
  "document root element")
(DEFELEMENT I          ()                        "italic text style")
(DEFELEMENT IFRAME     (:LOOSE-DTD)              "inline subwindow")
(DEFELEMENT IMG        (:END-FORBIDDEN :EMPTY)   "embedded image")
(DEFELEMENT INPUT      (:END-FORBIDDEN :EMPTY)   "form control")
(DEFELEMENT INS        ()                        "inserted text")
(DEFELEMENT ISINDEX    (:END-FORBIDDEN :EMPTY :DEPRECATED :LOOSE-DTD)
  "single line prompt")
(DEFELEMENT KBD        ()  "text to be entered by the user")
(DEFELEMENT LABEL      ()                        "form field label text")
(DEFELEMENT LEGEND     ()                        "fieldset legend")
(DEFELEMENT LI         (:END-OPTIONAL)           "list item")
(DEFELEMENT LINK       (:END-FORBIDDEN :EMPTY)   "a media-independent link")
(DEFELEMENT MAP        ()                        "client-side image map")
(DEFELEMENT MENU       (:DEPRECATED :LOOSE-DTD)  "menu list")
(DEFELEMENT META       (:END-FORBIDDEN :EMPTY)   "generic metainformation")
(DEFELEMENT NOFRAMES   (:FRAMESET-DTD)
  "alternate content container for non frame-based rendering")
(DEFELEMENT NOSCRIPT   ()
  "alternate content container for non script-based rendering")
(DEFELEMENT OBJECT     ()               "generic embedded object")
(DEFELEMENT OL         ()               "ordered list")
(DEFELEMENT OPTGROUP   ()               "option group")
(DEFELEMENT OPTION     (:END-OPTIONAL)  "selectable choice")
(DEFELEMENT P          (:END-OPTIONAL)  "paragraph")
(DEFELEMENT PARAM      (:END-FORBIDDEN :EMPTY)  "named property value")
(DEFELEMENT PRE        ()               "preformatted text")
(DEFELEMENT Q          ()               "short inline quotation")
(DEFELEMENT S          (:DEPRECATED :LOOSE-DTD)
  "strike-through text style")
(DEFELEMENT SAMP       ()               "sample program output, scripts, etc.")
(DEFELEMENT SCRIPT     ()               "script statements")
(DEFELEMENT SELECT     ()               "option selector")
(DEFELEMENT SMALL      ()               "small text style")
(DEFELEMENT SPAN       ()               "generic language/style container")
(DEFELEMENT STRIKE     (:DEPRECATED :LOOSE-DTD)  "strike-through text")
(DEFELEMENT STRONG     ()               "strong emphasis")
(DEFELEMENT STYLE      ()               "style info")
(DEFELEMENT SUB        ()               "subscript")
(DEFELEMENT SUP        ()               "superscript")
(DEFELEMENT TABLE      ())
(DEFELEMENT TBODY      (:START-OPTIONAL :END-OPTIONAL)  "table body")
(DEFELEMENT TD         (:END-OPTIONAL)  "table data cell")
(DEFELEMENT TEXTAREA   ()               "multi-line text field")
(DEFELEMENT TFOOT      (:END-OPTIONAL)  "table footer")
(DEFELEMENT TH         (:END-OPTIONAL)  "table header cell")
(DEFELEMENT THEAD      (:END-OPTIONAL)  "table header")
(DEFELEMENT TITLE      ()               "document title")
(DEFELEMENT TR         (:END-OPTIONAL)  "table row")
(DEFELEMENT TT         ()  "teletype or monospaced text style")
(DEFELEMENT U          (:DEPRECATED :LOOSE-DTD)  "underlined text style")
(DEFELEMENT UL         ()  "unordered list")
(DEFELEMENT VAR        ()  "instance of a variable or program argument")



(DEFATTRIBUTE ABBR 
  (TD TH)
  (%TEXT)  :IMPLIED
  ()  "abbreviation for header cell")

(DEFATTRIBUTE ACCEPT-CHARSET
  (FORM)
  (%CHARSETS)  :IMPLIED
  ()  "list of supported charsets")

(DEFATTRIBUTE ACCEPT 
  (FORM INPUT)
  (%CONTENTTYPES)  :IMPLIED
  ()  "list of MIME types for file upload")

(DEFATTRIBUTE ACCESSKEY
  (A AREA BUTTON INPUT LABEL LEGEND TEXTAREA)
  (%CHARACTER)  :IMPLIED
  ()  "accessibility key character")

(DEFATTRIBUTE ACTION 
  (FORM)
  (%URI)  :REQUIRED
  ()  "server-side form handler")

;;
;; (DEFATTRIBUTE ATTR-NAME ELEMENTS TYPE DEFAULT OPTIONS DOCUMENTATION)
;;

(DEFATTRIBUTE ALIGN
  (CAPTION)
  (%CALIGN)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "relative to table")

(DEFATTRIBUTE ALIGN 
  (APPLET IFRAME IMG INPUT OBJECT)
  (%IALIGN)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "vertical or horizontal alignment")

(DEFATTRIBUTE ALIGN
  (LEGEND)
  (%LALIGN)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "relative to fieldset")

(DEFATTRIBUTE ALIGN
  (TABLE)
  (%TALIGN)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "table position relative to window")

(DEFATTRIBUTE ALIGN
  (HR)
  (OR  "LEFT" "CENTER" "RIGHT")  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "")

(DEFATTRIBUTE ALIGN 
  (DIV H1 H2 H3 H4 H5 H6 P)
  (OR  "LEFT" "CENTER" "RIGHT" "JUSTIFY")  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "align, text alignment")

(DEFATTRIBUTE ALIGN 
  (COL COLGROUP TBODY TD TFOOT TH THEAD TR)
  (OR  "LEFT" "CENTER" "RIGHT" "JUSTIFY" "CHAR")  :IMPLIED
  ()  "")

(DEFATTRIBUTE ALINK 
  (BODY)
  (%COLOR)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "color of selected links")

(DEFATTRIBUTE ALT 
  (APPLET)
  (%TEXT)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "short description")

(DEFATTRIBUTE ALT 
  (AREA IMG)
  (%TEXT)  :REQUIRED
  ()  "short description")

(DEFATTRIBUTE ALT 
  (INPUT)
  (CDATA)  :IMPLIED
  ()  "short description")

(DEFATTRIBUTE ARCHIVE
  (APPLET)
  (CDATA)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "comma-separated archive list")

(DEFATTRIBUTE ARCHIVE
  (OBJECT)
  (CDATA)  :IMPLIED
  ()  "space-separated list of URIs")

(DEFATTRIBUTE AXIS 
  (TD TH)
  (CDATA)  :IMPLIED
  ()  "comma-separated list of related headers")

(DEFATTRIBUTE BACKGROUND
  (BODY)
  (%URI)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "texture tile for document background")

(DEFATTRIBUTE BGCOLOR
  (TABLE)
  (%COLOR)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "background color for cells")

(DEFATTRIBUTE BGCOLOR
  (TR)
  (%COLOR)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "background color for row")

(DEFATTRIBUTE BGCOLOR
  (TD TH)
  (%COLOR)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "cell background color")

(DEFATTRIBUTE BGCOLOR
  (BODY)
  (%COLOR)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "document background color")

(DEFATTRIBUTE BORDER
  (TABLE)
  (%PIXELS)  :IMPLIED
  ()  "controls frame width around table")

(DEFATTRIBUTE BORDER
  (IMG OBJECT)
  (%PIXELS)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "link border width")

(DEFATTRIBUTE CELLPADDING
  (TABLE)
  (%LENGTH)  :IMPLIED
  ()  "spacing within cells")

(DEFATTRIBUTE CELLSPACING
  (TABLE)
  (%LENGTH)  :IMPLIED
  ()  "spacing between cells")

(DEFATTRIBUTE CHAR 
  (COL COLGROUP TBODY TD TFOOT TH THEAD TR)
  (%CHARACTER)  :IMPLIED
  ()  "alignment char, e.g. char=':'")

(DEFATTRIBUTE CHAROFF 
  (COL COLGROUP TBODY TD TFOOT TH THEAD TR)
  (%LENGTH)  :IMPLIED
  ()  "offset for alignment char")

(DEFATTRIBUTE CHARSET 
  (A LINK SCRIPT)
  (%CHARSET)  :IMPLIED
  ()  "char encoding of linked resource")

(DEFATTRIBUTE CHECKED 
  (INPUT)
  (CHECKED)  :IMPLIED
  ()  "for radio buttons and check boxes")

(DEFATTRIBUTE CITE 
  (BLOCKQUOTE Q)
  (%URI)  :IMPLIED
  ()  "URI for source document or msg")

(DEFATTRIBUTE CITE 
  (DEL INS)
  (%URI)  :IMPLIED
  ()  "info on reason for change")

(DEFATTRIBUTE CLASS 
  (:ALL-ELEMENTS-BUT BASE BASEFONT HEAD HTML META PARAM SCRIPT STYLE TITLE)
  (CDATA)  :IMPLIED
  ()  "space-separated list of classes")

(DEFATTRIBUTE CLASSID 
  (OBJECT)
  (%URI)  :IMPLIED
  ()  "identifies an implementation")

(DEFATTRIBUTE CLEAR 
  (BR)
  (OR  "LEFT" "ALL" "RIGHT" "NONE")  "NONE"
  (:DEPRECATED  :LOOSE-DTD)  "control of text flow")

(DEFATTRIBUTE CODE 
  (APPLET)
  (CDATA)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "applet class file")

(DEFATTRIBUTE CODEBASE
  (OBJECT)
  (%URI)  :IMPLIED
  ()  "base URI for classid, data, archive")

(DEFATTRIBUTE CODEBASE
  (APPLET)
  (%URI)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "optional base URI for applet")

(DEFATTRIBUTE CODETYPE
  (OBJECT)
  (%CONTENTTYPE)  :IMPLIED
  ()  "content type for code")

(DEFATTRIBUTE COLOR
  (BASEFONT FONT)
  (%COLOR)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "text color")

(DEFATTRIBUTE COLS
  (FRAMESET)
  (%MULTILENGTHS)  :IMPLIED
  (:FRAMESET-DTD)  "list of lengths, default: 100% (1 col)")

(DEFATTRIBUTE COLS
  (TEXTAREA)
  (NUMBER)  :REQUIRED
  ()  "")

(DEFATTRIBUTE COLSPAN 
  (TD TH)
  (NUMBER) "1"
  ()  "number of cols spanned by cell")

(DEFATTRIBUTE COMPACT 
  (DIR DL MENU OL UL)
  (COMPACT)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "reduced interitem spacing")

(DEFATTRIBUTE CONTENT 
  (META)
  (CDATA)  :REQUIRED
  ()  "associated information")

(DEFATTRIBUTE COORDS 
  (AREA)
  (%COORDS)  :IMPLIED
  ()  "comma-separated list of lengths")

(DEFATTRIBUTE COORDS 
  (A)
  (%COORDS)  :IMPLIED
  ()  "for use with client-side image maps")

(DEFATTRIBUTE DATA 
  (OBJECT)
  (%URI)  :IMPLIED
  ()  "reference to object's data")

(DEFATTRIBUTE DATETIME 
  (DEL INS)
  (%DATETIME)  :IMPLIED
  ()  "date and time of change")

(DEFATTRIBUTE DECLARE 
  (OBJECT)
  (DECLARE)  :IMPLIED
  ()  "declare but don't instantiate flag")

(DEFATTRIBUTE DEFER 
  (SCRIPT)
  (DEFER)  :IMPLIED
  ()  "UA may defer execution of script")

(DEFATTRIBUTE DIR 
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FRAME FRAMESET IFRAME PARAM SCRIPT)
  (OR  "LTR" "RTL")  :IMPLIED
  ()  "direction for weak/neutral text")

(DEFATTRIBUTE DIR 
  (BDO)
  (OR  "LTR" "RTL")  :REQUIRED
  ()  "directionality")

(DEFATTRIBUTE DISABLED
  (BUTTON INPUT OPTGROUP OPTION SELECT TEXTAREA)
  (DISABLED)  :IMPLIED
  ()  "unavailable in this context")

(DEFATTRIBUTE ENCTYPE 
  (FORM)
  (%CONTENTTYPE)
  "application/x-www-form-urlencoded"
  ()  "")

(DEFATTRIBUTE FACE
  (BASEFONT FONT)
  (CDATA)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "comma-separated list of font names")

(DEFATTRIBUTE FOR 
  (LABEL)
  (IDREF)  :IMPLIED
  ()  "matches field ID value")

(DEFATTRIBUTE FRAME 
  (TABLE)
  (%TFRAME)  :IMPLIED
  ()  "which parts of frame to render")

(DEFATTRIBUTE FRAMEBORDER
  (FRAME IFRAME)
  (OR  "1" "0")  "1"

  :FRAMESET-DTD
  "request frame borders?")

(DEFATTRIBUTE HEADERS 
  (TD TH)
  (IDREFS)  :IMPLIED
  ()  "list of id's for header cells")

(DEFATTRIBUTE HEIGHT
  (IFRAME)
  (%LENGTH)  :IMPLIED
  (:LOOSE-DTD)  "frame height")

(DEFATTRIBUTE HEIGHT 
  (TD TH)
  (%LENGTH)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "height for cell")

(DEFATTRIBUTE HEIGHT
  (IMG OBJECT)
  (%LENGTH)  :IMPLIED
  ()  "override height")

(DEFATTRIBUTE HEIGHT
  (APPLET)
  (%LENGTH)  :REQUIRED
  (:DEPRECATED  :LOOSE-DTD)  "initial height")

(DEFATTRIBUTE HREF 
  (A AREA LINK)
  (%URI)  :IMPLIED
  ()  "URI for linked resource")

(DEFATTRIBUTE HREF 
  (BASE)
  (%URI)  :IMPLIED
  ()  "URI that acts as base URI")

(DEFATTRIBUTE HREFLANG 
  (A LINK)
  (%LANGUAGECODE)  :IMPLIED
  ()  "language code")

(DEFATTRIBUTE HSPACE 
  (APPLET IMG OBJECT)
  (%PIXELS)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "horizontal gutter")

(DEFATTRIBUTE HTTP-EQUIV
  (META)
  (NAME)  :IMPLIED
  ()  "HTTP response header name")

(DEFATTRIBUTE ID 
  (:ALL-ELEMENTS-BUT BASE HEAD HTML META SCRIPT STYLE TITLE)
  (ID)  :IMPLIED
  ()  "document-wide unique id")

(DEFATTRIBUTE ISMAP 
  (IMG INPUT)
  (ISMAP)  :IMPLIED
  ()  "use server-side image map")

(DEFATTRIBUTE LABEL
  (OPTION)
  (%TEXT)  :IMPLIED
  ()  "for use in hierarchical menus")

(DEFATTRIBUTE LABEL
  (OPTGROUP)
  (%TEXT)  :REQUIRED
  ()  "for use in hierarchical menus")

(DEFATTRIBUTE LANG 
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BR FRAME FRAMESET IFRAME PARAM SCRIPT)
  (%LANGUAGECODE)  :IMPLIED
  ()  "language code")

(DEFATTRIBUTE LANGUAGE
  (SCRIPT)
  (CDATA)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "predefined script language name")

(DEFATTRIBUTE LINK 
  (BODY)
  (%COLOR)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "color of links")

(DEFATTRIBUTE LONGDESC
  (IMG)
  (%URI)  :IMPLIED
  ()  "link to long description (complements alt)")

(DEFATTRIBUTE LONGDESC
  (FRAME IFRAME)
  (%URI)  :IMPLIED
  (:FRAMESET-DTD)  "link to long description (complements title)")

(DEFATTRIBUTE MARGINHEIGHT
  (FRAME IFRAME)
  (%PIXELS)  :IMPLIED
  (:FRAMESET-DTD)  "margin height in pixels")

(DEFATTRIBUTE MARGINWIDTH
  (FRAME IFRAME)
  (%PIXELS)  :IMPLIED
  (:FRAMESET-DTD)  "margin widths in pixels")

(DEFATTRIBUTE MAXLENGTH
  (INPUT)
  (NUMBER)  :IMPLIED
  ()  "max chars for text fields")

(DEFATTRIBUTE MEDIA 
  (STYLE)
  (%MEDIADESC)  :IMPLIED
  ()  "designed for use with these media")

(DEFATTRIBUTE MEDIA 
  (LINK)
  (%MEDIADESC)  :IMPLIED
  ()  "for rendering on these media")

(DEFATTRIBUTE METHOD 
  (FORM)
  (OR  "GET" "POST")  "GET"
  ()  "HTTP method used to submit the form")

(DEFATTRIBUTE MULTIPLE
  (SELECT)
  (MULTIPLE)  :IMPLIED
  ()  "default is single selection")

(DEFATTRIBUTE NAME
  (BUTTON TEXTAREA)
  (CDATA)  :IMPLIED
  ()  "")

(DEFATTRIBUTE NAME
  (APPLET)
  (CDATA)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "allows applets to find each other")

(DEFATTRIBUTE NAME
  (SELECT)
  (CDATA)  :IMPLIED
  ()  "field name")

(DEFATTRIBUTE NAME 
  (FORM)
  (CDATA)  :IMPLIED
  ()  "name of form for scripting")

(DEFATTRIBUTE NAME 
  (FRAME IFRAME)
  (CDATA)  :IMPLIED
  (:FRAMESET-DTD)  "name of frame for targetting")

(DEFATTRIBUTE NAME 
  (IMG)
  (CDATA)  :IMPLIED
  ()  "name of image for scripting")

(DEFATTRIBUTE NAME 
  (A)
  (CDATA)  :IMPLIED
  ()  "named link end")

(DEFATTRIBUTE NAME 
  (INPUT OBJECT)
  (CDATA)  :IMPLIED
  ()  "submit as part of form")

(DEFATTRIBUTE NAME 
  (MAP)
  (CDATA)  :REQUIRED
  ()  "for reference by usemap")

(DEFATTRIBUTE NAME 
  (PARAM)
  (CDATA)  :REQUIRED
  ()  "property name")

(DEFATTRIBUTE NAME 
  (META)
  (NAME)  :IMPLIED
  ()  "metainformation name")

(DEFATTRIBUTE NOHREF 
  (AREA)
  (NOHREF)  :IMPLIED
  ()  "this region has no action")

(DEFATTRIBUTE NORESIZE
  (FRAME)
  (NORESIZE)  :IMPLIED
  (:FRAMESET-DTD)  "allow users to resize frames?")

(DEFATTRIBUTE NOSHADE
  (HR)
  (NOSHADE)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "")

(DEFATTRIBUTE NOWRAP 
  (TD TH)
  (NOWRAP)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "suppress word wrap")

(DEFATTRIBUTE OBJECT 
  (APPLET)
  (CDATA)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "serialized applet file")

(DEFATTRIBUTE ONBLUR 
  (A AREA BUTTON INPUT LABEL SELECT TEXTAREA)
  (%SCRIPT)  :IMPLIED
  ()  "the element lost the focus")

(DEFATTRIBUTE ONCHANGE
  (INPUT SELECT TEXTAREA)
  (%SCRIPT)  :IMPLIED
  ()  "the element value was changed")

(DEFATTRIBUTE ONCLICK
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a pointer button was clicked")

(DEFATTRIBUTE ONDBLCLICK
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a pointer button was double clicked")

(DEFATTRIBUTE ONFOCUS
  (A AREA BUTTON INPUT LABEL SELECT TEXTAREA)
  (%SCRIPT)  :IMPLIED
  ()  "the element got the focus")

(DEFATTRIBUTE ONKEYDOWN
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a key was pressed down")

(DEFATTRIBUTE ONKEYPRESS
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a key was pressed and released")

(DEFATTRIBUTE ONKEYUP
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a key was released")

(DEFATTRIBUTE ONLOAD 
  (FRAMESET)
  (%SCRIPT)  :IMPLIED
  (:FRAMESET-DTD)  "all the frames have been loaded")

(DEFATTRIBUTE ONLOAD 
  (BODY)
  (%SCRIPT)  :IMPLIED
  ()  "the document has been loaded")

(DEFATTRIBUTE ONMOUSEDOWN
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a pointer button was pressed down")

(DEFATTRIBUTE ONMOUSEMOVE
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a pointer was moved within")

(DEFATTRIBUTE ONMOUSEOUT
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a pointer was moved away")

(DEFATTRIBUTE ONMOUSEOVER
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a pointer was moved onto")

(DEFATTRIBUTE ONMOUSEUP
  (:ALL-ELEMENTS-BUT APPLET BASE BASEFONT BDO BR FONT FRAME FRAMESET HEAD HTML IFRAME ISINDEX META PARAM SCRIPT STYLE TITLE)
  (%SCRIPT)  :IMPLIED
  ()  "a pointer button was released")

(DEFATTRIBUTE ONRESET
  (FORM)
  (%SCRIPT)  :IMPLIED
  ()  "the form was reset")

(DEFATTRIBUTE ONSELECT
  (INPUT TEXTAREA)
  (%SCRIPT)  :IMPLIED
  ()  "some text was selected")

(DEFATTRIBUTE ONSUBMIT
  (FORM)
  (%SCRIPT)  :IMPLIED
  ()  "the form was submitted")

(DEFATTRIBUTE ONUNLOAD
  (FRAMESET)
  (%SCRIPT)  :IMPLIED
  (:FRAMESET-DTD)  "all the frames have been removed")

(DEFATTRIBUTE ONUNLOAD
  (BODY)
  (%SCRIPT)  :IMPLIED
  ()  "the document has been removed")

(DEFATTRIBUTE PROFILE 
  (HEAD)
  (%URI)  :IMPLIED
  ()  "named dictionary of meta info")

(DEFATTRIBUTE PROMPT 
  (ISINDEX)
  (%TEXT)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "prompt message")

(DEFATTRIBUTE READONLY
  (TEXTAREA)
  (READONLY)  :IMPLIED
  ()  "")

(DEFATTRIBUTE READONLY
  (INPUT)
  (READONLY)  :IMPLIED
  ()  "for text and passwd")

(DEFATTRIBUTE REL 
  (A LINK)
  (%LINKTYPES)  :IMPLIED
  ()  "forward link types")

(DEFATTRIBUTE REV 
  (A LINK)
  (%LINKTYPES)  :IMPLIED
  ()  "reverse link types")

(DEFATTRIBUTE ROWS
  (FRAMESET)
  (%MULTILENGTHS)  :IMPLIED
  (:FRAMESET-DTD)  "list of lengths, default: 100% (1 row)")

(DEFATTRIBUTE ROWS
  (TEXTAREA)
  (NUMBER)  :REQUIRED
  ()  "")

(DEFATTRIBUTE ROWSPAN 
  (TD TH)
  (NUMBER) "1"
  ()  "number of rows spanned by cell")

(DEFATTRIBUTE RULES 
  (TABLE)
  (%TRULES)  :IMPLIED
  ()  "rulings between rows and cols")

(DEFATTRIBUTE SCHEME 
  (META)
  (CDATA)  :IMPLIED
  ()  "select form of content")

(DEFATTRIBUTE SCOPE 
  (TD TH)
  (%SCOPE)  :IMPLIED
  ()  "scope covered by header cells")

(DEFATTRIBUTE SCROLLING
  (FRAME IFRAME)
  (OR  "YES" "NO" "AUTO")  "AUTO"
  (:FRAMESET-DTD)  "scrollbar or none")

(DEFATTRIBUTE SELECTED
  (OPTION)
  (SELECTED)  :IMPLIED
  ()  "")

(DEFATTRIBUTE SHAPE 
  (AREA)
  (%SHAPE)
  "rect"
  ()  "controls interpretation of coords")

(DEFATTRIBUTE SHAPE 
  (A)
  (%SHAPE) "RECT"
  ()  "for use with client-side image maps")

(DEFATTRIBUTE SIZE 
  (HR)
  (%PIXELS)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "")

(DEFATTRIBUTE SIZE
  (FONT)
  (CDATA)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "[+ -]nn e.g. size=\"+1\", size=\"4\"")

(DEFATTRIBUTE SIZE 
  (INPUT)
  (CDATA)  :IMPLIED
  ()  "specific to each type of field")

(DEFATTRIBUTE SIZE
  (BASEFONT)
  (CDATA)  :REQUIRED
  (:DEPRECATED  :LOOSE-DTD)  "base font size for FONT elements")

(DEFATTRIBUTE SIZE
  (SELECT)
  (NUMBER)  :IMPLIED
  ()  "rows visible")

(DEFATTRIBUTE SPAN 
  (COL)
  (NUMBER) "1"
  ()  "COL attributes affect N columns")

(DEFATTRIBUTE SPAN
  (COLGROUP)
  (NUMBER) "1"
  ()  "default number of columns in group")

(DEFATTRIBUTE SRC
  (SCRIPT)
  (%URI)  :IMPLIED
  ()  "URI for an external script")

(DEFATTRIBUTE SRC 
  (INPUT)
  (%URI)  :IMPLIED
  ()  "for fields with images")

(DEFATTRIBUTE SRC 
  (FRAME IFRAME)
  (%URI)  :IMPLIED
  (:FRAMESET-DTD)  "source of frame content")

(DEFATTRIBUTE SRC 
  (IMG)
  (%URI)  :REQUIRED
  ()  "URI of image to embed")

(DEFATTRIBUTE STANDBY 
  (OBJECT)
  (%TEXT)  :IMPLIED
  ()  "message to show while loading")

(DEFATTRIBUTE START 
  (OL)
  (NUMBER)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "starting sequence number")

(DEFATTRIBUTE STYLE 
  (:ALL-ELEMENTS-BUT BASE BASEFONT HEAD HTML META PARAM SCRIPT STYLE TITLE)
  (%STYLESHEET)  :IMPLIED
  ()  "associated style info")

(DEFATTRIBUTE SUMMARY 
  (TABLE)
  (%TEXT)  :IMPLIED
  ()  "purpose/structure for speech output")

(DEFATTRIBUTE TABINDEX
  (A AREA BUTTON INPUT OBJECT SELECT TEXTAREA)
  (NUMBER)  :IMPLIED
  ()  "position in tabbing order")

(DEFATTRIBUTE TARGET 
  (A AREA BASE FORM LINK)
  (%FRAMETARGET)  :IMPLIED
  (:LOOSE-DTD)  "render in this frame")

(DEFATTRIBUTE TEXT 
  (BODY)
  (%COLOR)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "document text color")

(DEFATTRIBUTE TITLE 
  (:ALL-ELEMENTS-BUT BASE BASEFONT HEAD HTML META PARAM SCRIPT TITLE)
  (%TEXT)  :IMPLIED
  ()  "advisory title")

(DEFATTRIBUTE TYPE 
  (A LINK)
  (%CONTENTTYPE)  :IMPLIED
  ()  "advisory content type")

(DEFATTRIBUTE TYPE
  (OBJECT)
  (%CONTENTTYPE)  :IMPLIED
  ()  "content type for data")

(DEFATTRIBUTE TYPE 
  (PARAM)
  (%CONTENTTYPE)  :IMPLIED
  ()  "content type for value when valuetype=ref")

(DEFATTRIBUTE TYPE
  (SCRIPT)
  (%CONTENTTYPE)  :REQUIRED
  ()  "content type of script language")

(DEFATTRIBUTE TYPE 
  (STYLE)
  (%CONTENTTYPE)  :REQUIRED
  ()  "content type of style language")

(DEFATTRIBUTE TYPE 
  (INPUT)
  (%INPUTTYPE) "TEXT"
  ()  "what kind of widget is needed")

(DEFATTRIBUTE TYPE 
  (LI)
  (%LISTYLE)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "list item style")

(DEFATTRIBUTE TYPE 
  (OL)
  (%OLSTYLE)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "numbering style")

(DEFATTRIBUTE TYPE 
  (UL)
  (%ULSTYLE)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "bullet style")

(DEFATTRIBUTE TYPE
  (BUTTON)
  (OR  "BUTTON" "SUBMIT" "RESET")  "SUBMIT"
  ()  "for use as form button")

(DEFATTRIBUTE USEMAP 
  (IMG INPUT OBJECT)
  (%URI)  :IMPLIED
  ()  "use client-side image map")

(DEFATTRIBUTE VALIGN 
  (COL COLGROUP TBODY TD TFOOT TH THEAD TR)
  (OR  "TOP" "MIDDLE" "BOTTOM" "BASELINE")  :IMPLIED
  ()  "vertical alignment in cells")

(DEFATTRIBUTE VALUE
  (INPUT)
  (CDATA)  :IMPLIED
  ()  "Specify for radio buttons and checkboxes")

(DEFATTRIBUTE VALUE
  (OPTION)
  (CDATA)  :IMPLIED
  ()  "defaults to element content")

(DEFATTRIBUTE VALUE
  (PARAM)
  (CDATA)  :IMPLIED
  ()  "property value")

(DEFATTRIBUTE VALUE
  (BUTTON)
  (CDATA)  :IMPLIED
  ()  "sent to server when submitted")

(DEFATTRIBUTE VALUE 
  (LI)
  (NUMBER)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "reset sequence number")

(DEFATTRIBUTE VALUETYPE
  (PARAM)
  (OR  "DATA" "REF" "OBJECT")  "DATA"
  ()  "How to interpret value")

(DEFATTRIBUTE VERSION 
  (HTML)
  (CDATA) :%HTML.VERSION
  (:DEPRECATED  :LOOSE-DTD)  "Constant")

(DEFATTRIBUTE VLINK 
  (BODY)
  (%COLOR)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "color of visited links")

(DEFATTRIBUTE VSPACE 
  (APPLET IMG OBJECT)
  (%PIXELS)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "vertical gutter")

(DEFATTRIBUTE WIDTH
  (HR)
  (%LENGTH)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "")

(DEFATTRIBUTE WIDTH
  (IFRAME)
  (%LENGTH)  :IMPLIED
  (:LOOSE-DTD)  "frame width")

(DEFATTRIBUTE WIDTH 
  (IMG OBJECT)
  (%LENGTH)  :IMPLIED
  ()  "override width")

(DEFATTRIBUTE WIDTH
  (TABLE)
  (%LENGTH)  :IMPLIED
  ()  "table width")

(DEFATTRIBUTE WIDTH 
  (TD TH)
  (%LENGTH)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "width for cell")

(DEFATTRIBUTE WIDTH
  (APPLET)
  (%LENGTH)  :REQUIRED
  (:DEPRECATED  :LOOSE-DTD)  "initial width")

(DEFATTRIBUTE WIDTH 
  (COL)
  (%MULTILENGTH)  :IMPLIED
  ()  "column width specification")

(DEFATTRIBUTE WIDTH
  (COLGROUP)
  (%MULTILENGTH)  :IMPLIED
  ()  "default width for enclosed COLs")

(DEFATTRIBUTE WIDTH 
  (PRE)
  (NUMBER)  :IMPLIED
  (:DEPRECATED  :LOOSE-DTD)  "")


;;;; THE END ;;;;
;;;;---------------------------------------------------------------------------

(generate)


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
