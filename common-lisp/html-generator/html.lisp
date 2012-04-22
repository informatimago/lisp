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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2012
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
                "SPLIT-STRING" "STRING-REPLACE")
  (:shadow "MAP")
  (:export

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

  (:documentation
   "This package exports functions to generate HTML pages.

    Copyright Pascal J. Bourguignon 2003 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *html-output-stream* *standard-output*
    "The stream used by WITH-HTML-OUTPUT and WRITE-HTML by default.")


  (defvar *html-output-elements* '()
    "The HTML macros collect issued elements into this list.")


  (defvar *doctype*      :strict
    "The DOCTYPE of HTML document being generated.
   May be a token: :STRICT, :TRANSITIONAL, :LOOSE or :FRAMESET.")


  (defvar *html-version* "4.01"
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
    (unless (member kind '(:strict :transitional :loose :frameset))
      (error "Unexpected DOCTYPE kind. Please choose :STRICT, ~
             :TRANSITIONAL, :LOOSE or :FRAMESET."))
    (html-string
     (ecase kind
       ((:strict)
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" ~
                \"http://www.w3.org/TR/html4/strict.dtd\">~%")
       ((:transitional :loose)
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" ~
                 \"http://www.w3.org/TR/html4/loose.dtd\">~%")
       ((:frameset)
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" ~
                 \"http://www.w3.org/TR/html4/frameset.dtd\">~%"))))


  (defmacro doctype (kind &body body)
    (let ((vkind (gensym)))
      `(let* ((,vkind        ,kind)
              (*doctype*     ,vkind)
              (*html-version* "4.01"))
         (doctype* ,vkind)
         ,@body)))





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
        :text (apply (function format) nil control arguments)))


  (defun html-string (control &rest arguments)
    (collect-element (apply (function html-string*) control arguments)))



  (defun comment* (control &rest arguments)
    "
RETURN:  An element storing the result of formating the CONTROL string
         with the ARGUMENTS as HTML comment.
"
    (html-string "~&<!-- ~A -->~%"
                 (string-replace (apply (function format) nil control arguments)
                                 "--"  "==")))

  (defun comment (control &rest arguments)
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

  (defun cdata* (control &rest arguments)
    "
RETURN:  An element storing the result of formating the CONTROL string
         with the ARGUMENTS as CDATA (ie. post-processed to quote special
         HTML characters (&,\").
"
    (make-instance 'cdata :data (apply (function format) nil control arguments)))

  (defun cdata (control &rest arguments)
    (collect-element (apply (function cdata*) control arguments)))


  (defclass pcdata (element)
    ((data :initarg :data :reader pcdata-data :type string)))
  (defmethod write-element ((self pcdata) stream)
    (write-escaping '((#\& . "&amp;") (#\< . "&lt;") (#\> . "&gt;"))
                    (pcdata-data self) stream)
    self)


  (defun pcdata* (control &rest arguments)
    "
RETURN:  An element storing the result of formating the CONTROL string
         with the ARGUMENTS as CDATA (ie. post-processed to quote special
         HTML characters (<,>,&,\").
"
    (make-instance 'pcdata :data (apply (function format) nil control arguments)))

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



  (defun write-html (element &optional (stream *html-output-stream*))
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


  (defparameter *elements*   (make-hash-table :test (function equal))
    "Maps element names to elements.")


  (defparameter *attributes* (make-hash-table :test (function equal))
    "Maps attribute names to attributes.")


  (defmacro defelement (name options &optional (documentation "A HTML element."))
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




  (defmacro defattribute (attr-name elements type default options documentation)
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
    (unless (eq :loose *doctype*)
      (error "The element ~A can be used only with loose DTD." name)))


  (defun check-frameset (name)
    (unless (eq :frameset *doctype*)
      (error "The element ~A can be used only with frameset DTD." name)))




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
               (fname (intern (with-standard-io-syntax (format nil "~:@(~A*~)" name)))))
           (push
            `(defmacro ,mname (&optional ,vattr &body ,vbody)
               (generate-element-macro-body ',fname ,vattr ,vbody)) forms)
           (push
            `(defun ,fname (&optional ,vattr ,vbody)
               ,@(when (member :deprecated (el-options element))
                       `((warn ,(format nil "HTML element ~A is deprecated."
                                        name))))
               ,@(cond
                  ((member :loose-dtd    (el-options element))
                   `((check-loose    ',name)))
                  ((member :frameset-dtd (el-options element))
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
;;;;    Copyright Pascal J. Bourguignon 2003 - 2012
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

(defelement a          ()  "anchor")
(defelement abbr       ()  "abbreviated form (e.g., WWW, HTTP, etc.)")
(defelement acronym    ())
(defelement address    ()                        "information on author")
(defelement applet     (:deprecated :loose-dtd)  "Java applet")
(defelement area       (:end-forbidden :empty)   "client-side image map area")
(defelement b          ()                        "bold text style")
(defelement base       (:end-forbidden :empty)   "document base URI")
(defelement basefont   (:end-forbidden :empty :deprecated :loose-dtd)
  "base font size")
(defelement bdo        ()                        "I18N BiDi over-ride")
(defelement big        ()                        "large text style")
(defelement blockquote ()                        "long quotation")
(defelement body       (:start-optional :end-optional)
  "document body")
(defelement br         (:end-forbidden :empty)   "forced line break")
(defelement button     ()                        "push button")
(defelement caption    ()                        "table caption")
(defelement center     (:deprecated :loose-dtd)
  "shorthand for DIV align=center")
(defelement cite       ()                        "citation")
(defelement code       ()                        "computer code fragment")
(defelement col        (:end-forbidden :empty)   "table column")
(defelement colgroup   (:end-optional)           "table column group")
(defelement dd         (:end-optional)  "defelementinition description")
(defelement del        ()                        "deleted text")
(defelement dfn        ()                        "instance defelementinition")
(defelement dir        (:deprecated :loose-dtd)  "directory list")
(defelement div        ()  "generic language/style container")
(defelement dl         ()  "defelementinition list")
(defelement dt         (:end-optional)           "defelementinition term")
(defelement em         ()                        "emphasis")
(defelement fieldset   ()                        "form control group")
(defelement font       (:deprecated :loose-dtd)  "local change to font")
(defelement form       ()  "interactive form")
(defelement frame      (:end-forbidden :empty :frameset-dtd)  "subwindow")
(defelement frameset   (:frameset-dtd)           "window subdivision")
(defelement h1         ()                        "Heading")
(defelement h2         ()                        "Heading")
(defelement h3         ()                        "Heading")
(defelement h4         ()                        "Heading")
(defelement h5         ()                        "Heading")
(defelement h6         ()                        "Heading")
(defelement head       (:start-optional :end-optional)  "document head")
(defelement hr         (:end-forbidden :empty)   "horizontal rule")
(defelement html       (:start-optional :end-optional)
  "document root element")
(defelement i          ()                        "italic text style")
(defelement iframe     (:loose-dtd)              "inline subwindow")
(defelement img        (:end-forbidden :empty)   "embedded image")
(defelement input      (:end-forbidden :empty)   "form control")
(defelement ins        ()                        "inserted text")
(defelement isindex    (:end-forbidden :empty :deprecated :loose-dtd)
  "single line prompt")
(defelement kbd        ()  "text to be entered by the user")
(defelement label      ()                        "form field label text")
(defelement legend     ()                        "fieldset legend")
(defelement li         (:end-optional)           "list item")
(defelement link       (:end-forbidden :empty)   "a media-independent link")
(defelement map        ()                        "client-side image map")
(defelement menu       (:deprecated :loose-dtd)  "menu list")
(defelement meta       (:end-forbidden :empty)   "generic metainformation")
(defelement noframes   (:frameset-dtd)
  "alternate content container for non frame-based rendering")
(defelement noscript   ()
  "alternate content container for non script-based rendering")
(defelement object     ()               "generic embedded object")
(defelement ol         ()               "ordered list")
(defelement optgroup   ()               "option group")
(defelement option     (:end-optional)  "selectable choice")
(defelement p          (:end-optional)  "paragraph")
(defelement param      (:end-forbidden :empty)  "named property value")
(defelement pre        ()               "preformatted text")
(defelement q          ()               "short inline quotation")
(defelement s          (:deprecated :loose-dtd)
  "strike-through text style")
(defelement samp       ()               "sample program output, scripts, etc.")
(defelement script     ()               "script statements")
(defelement select     ()               "option selector")
(defelement small      ()               "small text style")
(defelement span       ()               "generic language/style container")
(defelement strike     (:deprecated :loose-dtd)  "strike-through text")
(defelement strong     ()               "strong emphasis")
(defelement style      ()               "style info")
(defelement sub        ()               "subscript")
(defelement sup        ()               "superscript")
(defelement table      ())
(defelement tbody      (:start-optional :end-optional)  "table body")
(defelement td         (:end-optional)  "table data cell")
(defelement textarea   ()               "multi-line text field")
(defelement tfoot      (:end-optional)  "table footer")
(defelement th         (:end-optional)  "table header cell")
(defelement thead      (:end-optional)  "table header")
(defelement title      ()               "document title")
(defelement tr         (:end-optional)  "table row")
(defelement tt         ()  "teletype or monospaced text style")
(defelement u          (:deprecated :loose-dtd)  "underlined text style")
(defelement ul         ()  "unordered list")
(defelement var        ()  "instance of a variable or program argument")



(defattribute abbr 
  (td th)
  (%text)  :implied
  ()  "abbreviation for header cell")

(defattribute accept-charset
  (form)
  (%charsets)  :implied
  ()  "list of supported charsets")

(defattribute accept 
  (form input)
  (%contenttypes)  :implied
  ()  "list of MIME types for file upload")

(defattribute accesskey
  (a area button input label legend textarea)
  (%character)  :implied
  ()  "accessibility key character")

(defattribute action 
  (form)
  (%uri)  :required
  ()  "server-side form handler")

;;
;; (DEFATTRIBUTE ATTR-NAME ELEMENTS TYPE DEFAULT OPTIONS DOCUMENTATION)
;;

(defattribute align
  (caption)
  (%calign)  :implied
  (:deprecated  :loose-dtd)  "relative to table")

(defattribute align 
  (applet iframe img input object)
  (%ialign)  :implied
  (:deprecated  :loose-dtd)  "vertical or horizontal alignment")

(defattribute align
  (legend)
  (%lalign)  :implied
  (:deprecated  :loose-dtd)  "relative to fieldset")

(defattribute align
  (table)
  (%talign)  :implied
  (:deprecated  :loose-dtd)  "table position relative to window")

(defattribute align
  (hr)
  (or  "LEFT" "CENTER" "RIGHT")  :implied
  (:deprecated  :loose-dtd)  "")

(defattribute align 
  (div h1 h2 h3 h4 h5 h6 p)
  (or  "LEFT" "CENTER" "RIGHT" "JUSTIFY")  :implied
  (:deprecated  :loose-dtd)  "align, text alignment")

(defattribute align 
  (col colgroup tbody td tfoot th thead tr)
  (or  "LEFT" "CENTER" "RIGHT" "JUSTIFY" "CHAR")  :implied
  ()  "")

(defattribute alink 
  (body)
  (%color)  :implied
  (:deprecated  :loose-dtd)  "color of selected links")

(defattribute alt 
  (applet)
  (%text)  :implied
  (:deprecated  :loose-dtd)  "short description")

(defattribute alt 
  (area img)
  (%text)  :required
  ()  "short description")

(defattribute alt 
  (input)
  (cdata)  :implied
  ()  "short description")

(defattribute archive
  (applet)
  (cdata)  :implied
  (:deprecated  :loose-dtd)  "comma-separated archive list")

(defattribute archive
  (object)
  (cdata)  :implied
  ()  "space-separated list of URIs")

(defattribute axis 
  (td th)
  (cdata)  :implied
  ()  "comma-separated list of related headers")

(defattribute background
  (body)
  (%uri)  :implied
  (:deprecated  :loose-dtd)  "texture tile for document background")

(defattribute bgcolor
  (table)
  (%color)  :implied
  (:deprecated  :loose-dtd)  "background color for cells")

(defattribute bgcolor
  (tr)
  (%color)  :implied
  (:deprecated  :loose-dtd)  "background color for row")

(defattribute bgcolor
  (td th)
  (%color)  :implied
  (:deprecated  :loose-dtd)  "cell background color")

(defattribute bgcolor
  (body)
  (%color)  :implied
  (:deprecated  :loose-dtd)  "document background color")

(defattribute border
  (table)
  (%pixels)  :implied
  ()  "controls frame width around table")

(defattribute border
  (img object)
  (%pixels)  :implied
  (:deprecated  :loose-dtd)  "link border width")

(defattribute cellpadding
  (table)
  (%length)  :implied
  ()  "spacing within cells")

(defattribute cellspacing
  (table)
  (%length)  :implied
  ()  "spacing between cells")

(defattribute char 
  (col colgroup tbody td tfoot th thead tr)
  (%character)  :implied
  ()  "alignment char, e.g. char=':'")

(defattribute charoff 
  (col colgroup tbody td tfoot th thead tr)
  (%length)  :implied
  ()  "offset for alignment char")

(defattribute charset 
  (a link script)
  (%charset)  :implied
  ()  "char encoding of linked resource")

(defattribute checked 
  (input)
  (checked)  :implied
  ()  "for radio buttons and check boxes")

(defattribute cite 
  (blockquote q)
  (%uri)  :implied
  ()  "URI for source document or msg")

(defattribute cite 
  (del ins)
  (%uri)  :implied
  ()  "info on reason for change")

(defattribute class 
  (:all-elements-but base basefont head html meta param script style title)
  (cdata)  :implied
  ()  "space-separated list of classes")

(defattribute classid 
  (object)
  (%uri)  :implied
  ()  "identifies an implementation")

(defattribute clear 
  (br)
  (or  "LEFT" "ALL" "RIGHT" "NONE")  "NONE"
  (:deprecated  :loose-dtd)  "control of text flow")

(defattribute code 
  (applet)
  (cdata)  :implied
  (:deprecated  :loose-dtd)  "applet class file")

(defattribute codebase
  (object)
  (%uri)  :implied
  ()  "base URI for classid, data, archive")

(defattribute codebase
  (applet)
  (%uri)  :implied
  (:deprecated  :loose-dtd)  "optional base URI for applet")

(defattribute codetype
  (object)
  (%contenttype)  :implied
  ()  "content type for code")

(defattribute color
  (basefont font)
  (%color)  :implied
  (:deprecated  :loose-dtd)  "text color")

(defattribute cols
  (frameset)
  (%multilengths)  :implied
  (:frameset-dtd)  "list of lengths, default: 100% (1 col)")

(defattribute cols
  (textarea)
  (number)  :required
  ()  "")

(defattribute colspan 
  (td th)
  (number) "1"
  ()  "number of cols spanned by cell")

(defattribute compact 
  (dir dl menu ol ul)
  (compact)  :implied
  (:deprecated  :loose-dtd)  "reduced interitem spacing")

(defattribute content 
  (meta)
  (cdata)  :required
  ()  "associated information")

(defattribute coords 
  (area)
  (%coords)  :implied
  ()  "comma-separated list of lengths")

(defattribute coords 
  (a)
  (%coords)  :implied
  ()  "for use with client-side image maps")

(defattribute data 
  (object)
  (%uri)  :implied
  ()  "reference to object's data")

(defattribute datetime 
  (del ins)
  (%datetime)  :implied
  ()  "date and time of change")

(defattribute declare 
  (object)
  (declare)  :implied
  ()  "declare but don't instantiate flag")

(defattribute defer 
  (script)
  (defer)  :implied
  ()  "UA may defer execution of script")

(defattribute dir 
  (:all-elements-but applet base basefont bdo br frame frameset iframe param script)
  (or  "LTR" "RTL")  :implied
  ()  "direction for weak/neutral text")

(defattribute dir 
  (bdo)
  (or  "LTR" "RTL")  :required
  ()  "directionality")

(defattribute disabled
  (button input optgroup option select textarea)
  (disabled)  :implied
  ()  "unavailable in this context")

(defattribute enctype 
  (form)
  (%contenttype)
  "application/x-www-form-urlencoded"
  ()  "")

(defattribute face
  (basefont font)
  (cdata)  :implied
  (:deprecated  :loose-dtd)  "comma-separated list of font names")

(defattribute for 
  (label)
  (idref)  :implied
  ()  "matches field ID value")

(defattribute frame 
  (table)
  (%tframe)  :implied
  ()  "which parts of frame to render")

(defattribute frameborder
  (frame iframe)
  (or  "1" "0")  "1"

  :frameset-dtd
  "request frame borders?")

(defattribute headers 
  (td th)
  (idrefs)  :implied
  ()  "list of id's for header cells")

(defattribute height
  (iframe)
  (%length)  :implied
  (:loose-dtd)  "frame height")

(defattribute height 
  (td th)
  (%length)  :implied
  (:deprecated  :loose-dtd)  "height for cell")

(defattribute height
  (img object)
  (%length)  :implied
  ()  "override height")

(defattribute height
  (applet)
  (%length)  :required
  (:deprecated  :loose-dtd)  "initial height")

(defattribute href 
  (a area link)
  (%uri)  :implied
  ()  "URI for linked resource")

(defattribute href 
  (base)
  (%uri)  :implied
  ()  "URI that acts as base URI")

(defattribute hreflang 
  (a link)
  (%languagecode)  :implied
  ()  "language code")

(defattribute hspace 
  (applet img object)
  (%pixels)  :implied
  (:deprecated  :loose-dtd)  "horizontal gutter")

(defattribute http-equiv
  (meta)
  (name)  :implied
  ()  "HTTP response header name")

(defattribute id 
  (:all-elements-but base head html meta script style title)
  (id)  :implied
  ()  "document-wide unique id")

(defattribute ismap 
  (img input)
  (ismap)  :implied
  ()  "use server-side image map")

(defattribute label
  (option)
  (%text)  :implied
  ()  "for use in hierarchical menus")

(defattribute label
  (optgroup)
  (%text)  :required
  ()  "for use in hierarchical menus")

(defattribute lang 
  (:all-elements-but applet base basefont br frame frameset iframe param script)
  (%languagecode)  :implied
  ()  "language code")

(defattribute language
  (script)
  (cdata)  :implied
  (:deprecated  :loose-dtd)  "predefined script language name")

(defattribute link 
  (body)
  (%color)  :implied
  (:deprecated  :loose-dtd)  "color of links")

(defattribute longdesc
  (img)
  (%uri)  :implied
  ()  "link to long description (complements alt)")

(defattribute longdesc
  (frame iframe)
  (%uri)  :implied
  (:frameset-dtd)  "link to long description (complements title)")

(defattribute marginheight
  (frame iframe)
  (%pixels)  :implied
  (:frameset-dtd)  "margin height in pixels")

(defattribute marginwidth
  (frame iframe)
  (%pixels)  :implied
  (:frameset-dtd)  "margin widths in pixels")

(defattribute maxlength
  (input)
  (number)  :implied
  ()  "max chars for text fields")

(defattribute media 
  (style)
  (%mediadesc)  :implied
  ()  "designed for use with these media")

(defattribute media 
  (link)
  (%mediadesc)  :implied
  ()  "for rendering on these media")

(defattribute method 
  (form)
  (or  "GET" "POST")  "GET"
  ()  "HTTP method used to submit the form")

(defattribute multiple
  (select)
  (multiple)  :implied
  ()  "default is single selection")

(defattribute name
  (button textarea)
  (cdata)  :implied
  ()  "")

(defattribute name
  (applet)
  (cdata)  :implied
  (:deprecated  :loose-dtd)  "allows applets to find each other")

(defattribute name
  (select)
  (cdata)  :implied
  ()  "field name")

(defattribute name 
  (form)
  (cdata)  :implied
  ()  "name of form for scripting")

(defattribute name 
  (frame iframe)
  (cdata)  :implied
  (:frameset-dtd)  "name of frame for targetting")

(defattribute name 
  (img)
  (cdata)  :implied
  ()  "name of image for scripting")

(defattribute name 
  (a)
  (cdata)  :implied
  ()  "named link end")

(defattribute name 
  (input object)
  (cdata)  :implied
  ()  "submit as part of form")

(defattribute name 
  (map)
  (cdata)  :required
  ()  "for reference by usemap")

(defattribute name 
  (param)
  (cdata)  :required
  ()  "property name")

(defattribute name 
  (meta)
  (name)  :implied
  ()  "metainformation name")

(defattribute nohref 
  (area)
  (nohref)  :implied
  ()  "this region has no action")

(defattribute noresize
  (frame)
  (noresize)  :implied
  (:frameset-dtd)  "allow users to resize frames?")

(defattribute noshade
  (hr)
  (noshade)  :implied
  (:deprecated  :loose-dtd)  "")

(defattribute nowrap 
  (td th)
  (nowrap)  :implied
  (:deprecated  :loose-dtd)  "suppress word wrap")

(defattribute object 
  (applet)
  (cdata)  :implied
  (:deprecated  :loose-dtd)  "serialized applet file")

(defattribute onblur 
  (a area button input label select textarea)
  (%script)  :implied
  ()  "the element lost the focus")

(defattribute onchange
  (input select textarea)
  (%script)  :implied
  ()  "the element value was changed")

(defattribute onclick
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a pointer button was clicked")

(defattribute ondblclick
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a pointer button was double clicked")

(defattribute onfocus
  (a area button input label select textarea)
  (%script)  :implied
  ()  "the element got the focus")

(defattribute onkeydown
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a key was pressed down")

(defattribute onkeypress
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a key was pressed and released")

(defattribute onkeyup
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a key was released")

(defattribute onload 
  (frameset)
  (%script)  :implied
  (:frameset-dtd)  "all the frames have been loaded")

(defattribute onload 
  (body)
  (%script)  :implied
  ()  "the document has been loaded")

(defattribute onmousedown
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a pointer button was pressed down")

(defattribute onmousemove
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a pointer was moved within")

(defattribute onmouseout
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a pointer was moved away")

(defattribute onmouseover
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a pointer was moved onto")

(defattribute onmouseup
  (:all-elements-but applet base basefont bdo br font frame frameset head html iframe isindex meta param script style title)
  (%script)  :implied
  ()  "a pointer button was released")

(defattribute onreset
  (form)
  (%script)  :implied
  ()  "the form was reset")

(defattribute onselect
  (input textarea)
  (%script)  :implied
  ()  "some text was selected")

(defattribute onsubmit
  (form)
  (%script)  :implied
  ()  "the form was submitted")

(defattribute onunload
  (frameset)
  (%script)  :implied
  (:frameset-dtd)  "all the frames have been removed")

(defattribute onunload
  (body)
  (%script)  :implied
  ()  "the document has been removed")

(defattribute profile 
  (head)
  (%uri)  :implied
  ()  "named dictionary of meta info")

(defattribute prompt 
  (isindex)
  (%text)  :implied
  (:deprecated  :loose-dtd)  "prompt message")

(defattribute readonly
  (textarea)
  (readonly)  :implied
  ()  "")

(defattribute readonly
  (input)
  (readonly)  :implied
  ()  "for text and passwd")

(defattribute rel 
  (a link)
  (%linktypes)  :implied
  ()  "forward link types")

(defattribute rev 
  (a link)
  (%linktypes)  :implied
  ()  "reverse link types")

(defattribute rows
  (frameset)
  (%multilengths)  :implied
  (:frameset-dtd)  "list of lengths, default: 100% (1 row)")

(defattribute rows
  (textarea)
  (number)  :required
  ()  "")

(defattribute rowspan 
  (td th)
  (number) "1"
  ()  "number of rows spanned by cell")

(defattribute rules 
  (table)
  (%trules)  :implied
  ()  "rulings between rows and cols")

(defattribute scheme 
  (meta)
  (cdata)  :implied
  ()  "select form of content")

(defattribute scope 
  (td th)
  (%scope)  :implied
  ()  "scope covered by header cells")

(defattribute scrolling
  (frame iframe)
  (or  "YES" "NO" "AUTO")  "AUTO"
  (:frameset-dtd)  "scrollbar or none")

(defattribute selected
  (option)
  (selected)  :implied
  ()  "")

(defattribute shape 
  (area)
  (%shape)
  "rect"
  ()  "controls interpretation of coords")

(defattribute shape 
  (a)
  (%shape) "RECT"
  ()  "for use with client-side image maps")

(defattribute size 
  (hr)
  (%pixels)  :implied
  (:deprecated  :loose-dtd)  "")

(defattribute size
  (font)
  (cdata)  :implied
  (:deprecated  :loose-dtd)  "[+ -]nn e.g. size=\"+1\", size=\"4\"")

(defattribute size 
  (input)
  (cdata)  :implied
  ()  "specific to each type of field")

(defattribute size
  (basefont)
  (cdata)  :required
  (:deprecated  :loose-dtd)  "base font size for FONT elements")

(defattribute size
  (select)
  (number)  :implied
  ()  "rows visible")

(defattribute span 
  (col)
  (number) "1"
  ()  "COL attributes affect N columns")

(defattribute span
  (colgroup)
  (number) "1"
  ()  "default number of columns in group")

(defattribute src
  (script)
  (%uri)  :implied
  ()  "URI for an external script")

(defattribute src 
  (input)
  (%uri)  :implied
  ()  "for fields with images")

(defattribute src 
  (frame iframe)
  (%uri)  :implied
  (:frameset-dtd)  "source of frame content")

(defattribute src 
  (img)
  (%uri)  :required
  ()  "URI of image to embed")

(defattribute standby 
  (object)
  (%text)  :implied
  ()  "message to show while loading")

(defattribute start 
  (ol)
  (number)  :implied
  (:deprecated  :loose-dtd)  "starting sequence number")

(defattribute style 
  (:all-elements-but base basefont head html meta param script style title)
  (%stylesheet)  :implied
  ()  "associated style info")

(defattribute summary 
  (table)
  (%text)  :implied
  ()  "purpose/structure for speech output")

(defattribute tabindex
  (a area button input object select textarea)
  (number)  :implied
  ()  "position in tabbing order")

(defattribute target 
  (a area base form link)
  (%frametarget)  :implied
  (:loose-dtd)  "render in this frame")

(defattribute text 
  (body)
  (%color)  :implied
  (:deprecated  :loose-dtd)  "document text color")

(defattribute title 
  (:all-elements-but base basefont head html meta param script title)
  (%text)  :implied
  ()  "advisory title")

(defattribute type 
  (a link)
  (%contenttype)  :implied
  ()  "advisory content type")

(defattribute type
  (object)
  (%contenttype)  :implied
  ()  "content type for data")

(defattribute type 
  (param)
  (%contenttype)  :implied
  ()  "content type for value when valuetype=ref")

(defattribute type
  (script)
  (%contenttype)  :required
  ()  "content type of script language")

(defattribute type 
  (style)
  (%contenttype)  :required
  ()  "content type of style language")

(defattribute type 
  (input)
  (%inputtype) "TEXT"
  ()  "what kind of widget is needed")

(defattribute type 
  (li)
  (%listyle)  :implied
  (:deprecated  :loose-dtd)  "list item style")

(defattribute type 
  (ol)
  (%olstyle)  :implied
  (:deprecated  :loose-dtd)  "numbering style")

(defattribute type 
  (ul)
  (%ulstyle)  :implied
  (:deprecated  :loose-dtd)  "bullet style")

(defattribute type
  (button)
  (or  "BUTTON" "SUBMIT" "RESET")  "SUBMIT"
  ()  "for use as form button")

(defattribute usemap 
  (img input object)
  (%uri)  :implied
  ()  "use client-side image map")

(defattribute valign 
  (col colgroup tbody td tfoot th thead tr)
  (or  "TOP" "MIDDLE" "BOTTOM" "BASELINE")  :implied
  ()  "vertical alignment in cells")

(defattribute value
  (input)
  (cdata)  :implied
  ()  "Specify for radio buttons and checkboxes")

(defattribute value
  (option)
  (cdata)  :implied
  ()  "defaults to element content")

(defattribute value
  (param)
  (cdata)  :implied
  ()  "property value")

(defattribute value
  (button)
  (cdata)  :implied
  ()  "sent to server when submitted")

(defattribute value 
  (li)
  (number)  :implied
  (:deprecated  :loose-dtd)  "reset sequence number")

(defattribute valuetype
  (param)
  (or  "DATA" "REF" "OBJECT")  "DATA"
  ()  "How to interpret value")

(defattribute version 
  (html)
  (cdata) :%html.version
  (:deprecated  :loose-dtd)  "Constant")

(defattribute vlink 
  (body)
  (%color)  :implied
  (:deprecated  :loose-dtd)  "color of visited links")

(defattribute vspace 
  (applet img object)
  (%pixels)  :implied
  (:deprecated  :loose-dtd)  "vertical gutter")

(defattribute width
  (hr)
  (%length)  :implied
  (:deprecated  :loose-dtd)  "")

(defattribute width
  (iframe)
  (%length)  :implied
  (:loose-dtd)  "frame width")

(defattribute width 
  (img object)
  (%length)  :implied
  ()  "override width")

(defattribute width
  (table)
  (%length)  :implied
  ()  "table width")

(defattribute width 
  (td th)
  (%length)  :implied
  (:deprecated  :loose-dtd)  "width for cell")

(defattribute width
  (applet)
  (%length)  :required
  (:deprecated  :loose-dtd)  "initial width")

(defattribute width 
  (col)
  (%multilength)  :implied
  ()  "column width specification")

(defattribute width
  (colgroup)
  (%multilength)  :implied
  ()  "default width for enclosed COLs")

(defattribute width 
  (pre)
  (number)  :implied
  (:deprecated  :loose-dtd)  "")


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


(defun obfuscate (address)
  "
DO:         Generate an email address as an HTML string with the characters
            written as entities.
"
  (do  ((i (1- (length address)) (1- i))
        (res '()))
       ((< i 0) (apply (function concatenate) 'string res))
    (push (format nil "&#~D;" (char-code (aref address i))) res)))


;;;; THE END ;;;;
