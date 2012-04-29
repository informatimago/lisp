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
;;;;    2012-03-13 <PJB> Renamed package to match its position in the hierarchy.
;;;;    2005-02-22 <PJB> Optimized WALK for HTML-SEQ.
;;;;    2003-11-12 <PJB> Created.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.HTML-PARSER.PARSE-HTML"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-ENTITIES")
  (:export "HTML-ATTRIBUTE" "HTML-CONTENTS" "HTML-ATTRIBUTES" "HTML-TAG"
           "UNPARSE-HTML" "WRITE-HTML-TEXT"
           "PARSE-HTML-STRING" "PARSE-HTML-FILE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING" "UNSPLIT-STRING"
                "SPLIT-STRING" "STRING-REPLACE")
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

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2003 - 2012
    
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
    If not, see http://www.gnu.org/licenses/

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.HTML-PARSER.PARSE-HTML")


;; ------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +tag-package+ (find-package "KEYWORD"))
  (defvar *attributes* () "List of symbols of all attributes defined.")
  (defvar *elements*   () "List of symbols of all elements defined."))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro defattribute (attr-name elements type default options documentation)
    "
DO:       Defines an HTML attribute.
"
    (declare (ignore attr-name elements type default options documentation))
    ;; NOP
    (values))
  );;eval-when

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct element
    name options documentation))


(defun find-element (element-name) (getf *elements* element-name))


(defun element-empty-p (element-name)
  (and (find-element element-name)
       (member :empty (element-options (find-element element-name))
               :test (function eq))))


(defun element-start-optional-p (element-name)
  (or (not (find-element element-name))
      (member :start-optional (element-options (find-element element-name))
              :test (function eq))))


(defun element-end-optional-p (element-name)
  (or (not (find-element element-name))
      (member :end-optional (element-options (find-element element-name))
              :test (function eq))))


(defun element-end-forbidden-p (element-name)
  (and (find-element element-name)
       (member :end-forbidden (element-options (find-element element-name))
               :test (function eq))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defelement (name options &optional (documentation "A HTML element."))
    "
DO:         Defines a HTML element macro.
NAME:       A symbol that will be used to define a macro.
OPTIONS:    A list of keywords: :START-OPTIONAL :END-FORBIDDEN :EMPTY
                                :DEPRECATED :LOOSE-DTD or :FRAMESET-DTD.
            :END-FORBIDDEN  -> the close tag is not generated.
            :DEPRECATED     -> warning when the macro is used.
            :EMPTY          -> the macro won't take a BODY.
            :START-OPTIONAL -> ignored.
            :LOOSE-DTD      -> error when *DOCTYPE* isn't :LOOSE.
            :FRAMESET-DTD   -> error when *DOCTYPE* isn't :FRAMESET.
DOCUMENTATION:  A string used as documentation string for the macro NAME.
"
    (with-gensyms (gname)
      `(let ((,gname (intern ,(string name) +tag-package+)))
            (push (make-element :name ,gname
                                :options ',options
                                :documentation ',documentation) *elements*)
            (push ,gname *elements*))))
  );;eval-when

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (LOAD
;;    ( #+allegro (lambda (designator)
;;                  (if (stringp designator)
;;                      (let ((colon (position #\: designator)))
;;                        (format nil "~:@(~A~)~(~A~)"
;;                                (subseq designator 0 colon)
;;                                (subseq designator colon)))
;;                      designator))
;;      #-allegro identity
;;      
;;      "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;HTML401.LISP")))
;;;;----------------------------------------------------------------------------
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

;;;;----------------------------------------------------------------------------



;;; (defun set-tag-package (package)
;;;   "
;;; DO:    Set the package where the tags will be interned.
;;; "
;;;   (setf +tag-package+ package)
;;;   (setf *elements* nil)
;;;   (LOAD "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;HTML401.LISP")
;;;   );;set-tag-package



;; ------------------------------------------------------------------------
;; HTML Parser

;; (:name "html"
;;  :package "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-PARSER.PARSE-HTML"
;;  :identifier-start-chars
;;  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
;;  :identifier-continue-chars
;;  "-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
;;  :intern-identifier t
;;  :string-delimiter #.(code-char 0)
;;  :symbol-delimiter #.(code-char 0)
;;  :lex-cats (
;;             (ident      "[A-Za-z][-_A-Za-z0-9]*")
;;             (string-s   #.(format nil "'\\([^'~C~C]*\\)'" 
;;                            (code-char 10) (code-char 13)))
;;             (string-d   #.(format nil "\"\\([^\"~C~C]*\\)\""
;;                            (code-char 10) (code-char 13)))
;;             (string-n   "\\([-+,./0-9:?A-Z_a-z]+\\)")
;;             (comment-k  "[^-]+")
;;             (data       "[^<]+")
;;             )
;;  :grammar  "zebu-mg"
;;  :case-sensitive nil
;;  )
;; 
;; ;; Domain definition
;; 
;; html-seq   := [(first) (rest)];
;; comment    := [(data)];
;; attribute  := [(name ident) (value)];
;; definition := [(name ident) (attributes)];
;; open-tag   := [(name ident) (attributes)];
;; close-tag   := [(name ident)];
;; 
;; 
;; 
;; ;; Productions
;; 
;; file --> tag        { html-seq: [(first tag)  (rest nil) ] }
;;        | data       { html-seq: [(first data) (rest nil) ] }
;;        | tag  file  { html-seq: [(first tag)  (rest file)] }
;;        | data file  { html-seq: [(first data) (rest file)] } ;
;; 
;; tag -->   "<!" ident ">"
;;            { definition: [(name ident) ] }
;;         | "<!" ident aivs ">"
;;            { definition: [(name ident) (attributes aivs)] }
;;         | "<!-" comments "--" ">" 
;;            { comment: [(data comments)] }
;;         | "<!--" comments "--" ">"
;;            { comment: [(data comments)] }
;;         | "<" ident ( "/>" | ">" )
;;            { open-tag: [(name ident) ] }
;;         | "<" ident attributes ( "/>" | ">" )
;;            { open-tag: [(name ident) (attributes attributes)] }
;;         | "</" ident ">" 
;;            { close-tag: [(name ident)] } ;
;; 
;; comments --> comment-k              
;;              {html-seq: [(first comment-k) (rest nil)]}
;;            | comment-k     comments 
;;              {html-seq: [(first comment-k) (rest comments)]}
;;            | comment-k "-" comments 
;;              {html-seq: [(first comment-k) (rest comments)]};
;; 
;; aivs -->   aiv      { html-seq: [(first aiv) (rest nil) ] }
;;          | aiv aivs { html-seq: [(first aiv) (rest aivs)] } ;
;; aiv  --> attribute | ident | value ;
;; 
;; attributes -->   attribute
;;                  { html-seq: [(first attribute) (rest nil) ] }
;;                | attribute attributes 
;;                  { html-seq: [(first attribute) (rest attributes)] };
;; 
;; attribute -->   ident
;;                  { attribute: [(name ident) (value nil)  ] }
;;               | value
;;                  { attribute: [(name value) (value nil)  ] }
;;               | ident "=" value 
;;                  { attribute: [(name ident) (value value)] } ;
;; 
;; value -->  string-s  
;;          | string-d 
;;          | string-n ;


;; ------------------------------------------------------------------------
;; ZEBU Scanner/Parser
;; ------------------------------------------------------------------------

;; (DEFTYPE STRING-D   () 'STRING)
;; (DEFTYPE STRING-S   () 'STRING)
;; (DEFTYPE STRING-N   () 'STRING)
;; (DEFTYPE DATA       () 'STRING)
;; (DEFTYPE COMMENT-K  () 'STRING)
;; (DEFTYPE IDENT      () 'STRING)
;; 
;; (DEFUN STRING-D-P  (OBJECT) (STRINGP OBJECT))
;; (DEFUN STRING-S-P  (OBJECT) (STRINGP OBJECT))
;; (DEFUN STRING-N-P  (OBJECT) (STRINGP OBJECT))
;; (DEFUN DATA-P      (OBJECT) (STRINGP OBJECT))
;; (DEFUN COMMENT-K-P (OBJECT) (STRINGP OBJECT))
;; (DEFUN IDENT-P     (OBJECT) (STRINGP OBJECT))
;; 
;; 
;; 
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (load "html-domain.lisp")
;;   (defparameter gr-html (zebu:zebu-load-file "html-grammar.tab")))
;; 
;; 
;; (defun parse-file (pathname)
;;   (car (zebu:file-parser pathname
;;                          :grammar (zebu:find-grammar "html")
;;                          :print-parse-errors t
;;                          :verbose nil))
;;   );;parse-file
;; 
;; 
;; (defun parse-string (string &key (junk-allowed nil)
;;                             (print-parse-error t)
;;                             (error-fn (function error))
;;                             (start 0))
;;   (zebu:read-parser string
;;                     :grammar (zebu:find-grammar "html")
;;                     :junk-allowed junk-allowed
;;                     :print-parse-errors print-parse-error
;;                     :error-fn error-fn
;;                     :start start));;parse-string



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


#||

    (defun f (p goon)
      (terpri)
      (princ #\") (princ string)
      (list (subseq string 0 p) (subseq string p) goon))

    (multiple-value-call #'f (heuristic-quote-in-string (setf string "Pol. Industrial, 2 \"E\"\">")  
                                                        (position #\" string)  #\"))
    (multiple-value-call #'f (heuristic-quote-in-string (setf string"test \"name=\"titi\">" ) 
                                                        (position #\" string)  #\"))
    (multiple-value-call #'f (heuristic-quote-in-string (setf string"test \"name=\", all.\">")  
                                                        (position #\" string)  #\"))
    (multiple-value-call #'f (heuristic-quote-in-string (setf string"test \"name\", all.\">")  
                                                        (position #\" string)  #\"))
    (multiple-value-call #'f (heuristic-quote-in-string (setf string"test \"name\", all")  
                                                        (position #\" string)  #\"))
||#

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
         (values :cdata (loop for ch = (get-char-and-keep)
                           for nc = (next-char)
                           while (and ch nc
                                      (or (char/= ch (character "<"))
                                          (char/= nc (character "/"))))
                           finally (progn (unget-char ch) (return value)))))
        ((:tag-ident :tag)
         (let ((ch (loop for ch = (eat-char)
                      while (and ch (cs-space-p ch))
                      finally (progn (when ch (vector-push-extend ch value))
                                     (return ch)))))
           (cond
             ;; eof
             ((null ch) (return-from get-token (values :eof nil)))
             ;; /> close close tag
             ((and (char= ch (character "/")) (char= (next-char) (character ">")))
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


;; (with-input-from-string (source "<li id=\"ca-history\" class=\"collapsible \"><a href=\"http://en.wikipedia.org/w/index.php?title=List_of_The_Office_(U.S._TV_series)_episodes&amp;action=history\"  title=\"Past versions of this page [h]\" accesskey=\"h\"><span>View history</span></a></li>")
;;   (loop
;;      :with scanner = (make-html-scanner :source source)
;;      :for tok = (multiple-value-list (get-token scanner))
;;      :do (print tok)
;;      :until (eql :eof (first tok))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Keep these structures disjoint types for the walk methods below.
(defstruct html-seq first rest)
(defstruct comment data)
(defstruct foreign data)
(defstruct attribute name value)
(defstruct definition name attributes)
(defstruct open-tag name attributes closed)
(defstruct close-tag name attributes)


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
     :with items = '()
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
                                              :data (html-parser-value parser))
                                        (advance parser)))
                        ((:open-tag)  (parse-open-tag parser))
                        ((:close-tag) (parse-close-tag parser))
                        (otherwise   (report-error parser
                                                   "Unexpected token")))
     :while synthetic :do (push synthetic items)
     :finally (return (when items
                        (let ((result '()))
                          (dolist (item items result)
                            (setf result (make-html-seq
                                          :first item
                                          :rest result))))))))


(defun parse-definition (parser)
  ;; tag -->   "<!" ident ">"
  ;;            { definition: [(name ident) ] }
  ;;         | "<!" ident aivs ">"
  ;;            { definition: [(name ident) (attributes aivs)] }
  (advance parser)
  (unless (eq :identifier (html-parser-token parser))
    (report-error parser "Expected an identifier"))
  (let ((ident (html-parser-value parser)))
    (advance parser)
    (if (eq :end-tag (html-parser-token parser))
        (progn (advance parser) (make-definition :name ident :attributes nil))
        (let ((attributes (parse-aivs parser)))
          (if (eq :end-tag (html-parser-token parser))
              (advance parser)
              (report-error parser "Expected a \">\""))
          (make-definition :name ident :attributes attributes)))))


(defun parse-aivs (parser)
  ;; aivs -->   aiv      { html-seq: [(first aiv) (rest nil) ] }
  ;;          | aiv aivs { html-seq: [(first aiv) (rest aivs)] } ;
  ;; aiv  --> attribute | ident | value ;
  (make-html-seq :first (if (eq :string (html-parser-token parser))
                            (prog1 (html-parser-value parser) (advance parser))
                            (parse-attribute parser))
                 :rest  (when (member (html-parser-token parser)
                                      '(:string :identifier))
                          (parse-aivs parser))))

(defun parse-open-tag (parser)
  ;;         | "<" ident ">"
  ;;            { open-tag: [(name ident) ] }
  ;;         | "<" ident attributes ">"
  ;;            { open-tag: [(name ident) (attributes attributes)] }
  (advance parser)
  (unless (eq :identifier (html-parser-token parser))
    (report-error parser "Expected a tag identifier"))
  (let ((tag (prog1 (make-open-tag 
                     :name (prog1 (html-parser-value parser) (advance parser))
                     :attributes (unless (member (html-parser-token parser) 
                                                 '(:end-tag :close-end-tag))
                                   (parse-attributes parser))
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
          :name (prog1 (html-parser-value parser) (advance parser))
          :attributes (unless (eq :end-tag (html-parser-token parser))
                        (parse-attributes parser)))
    (unless (eq :end-tag (html-parser-token parser))
      (report-error parser "Expected a \">\""))
    (advance parser)))

  
(defun parse-attributes (parser)
  ;; attributes -->   attribute
  ;;                  { html-seq: [(first attribute) (rest nil) ] }
  ;;                | attribute attributes 
  ;;                  { html-seq: [(first attribute) (rest attributes)] };
  (make-html-seq :first (parse-attribute parser)
                 :rest  (when (eq (html-parser-token parser) :identifier)
                          (parse-attributes parser))))


(defun parse-attribute (parser)
  ;; attribute -->   ident
  ;;                  { attribute: [(name ident) (value nil)  ] }
  ;;               | ident "=" value 
  ;;                  { attribute: [(name ident) (value value)] } ;
  (unless (member (html-parser-token parser) '(eq :identifier :string))
    (report-error parser "Expected an attribute identifier or a string."))
  (make-attribute 
   :name (prog1 (html-parser-value parser) (advance parser))
   :value (if (eq :equal (html-parser-token parser))
              (progn (advance parser)
                     (unless (member (html-parser-token parser)
                                     '(:string :identifier))
                       (report-error parser "Expected an attribute value"))
                     (prog1 (html-parser-value parser) (advance parser)))
              nil)))
             

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric walk (html))

(defmethod walk ((self html-seq))
  (do ((current self (html-seq-rest current))
       (result '()))
      ((not (typep current 'html-seq))
       (assert (null current))
       (nreverse result))
    (push (walk (html-seq-first current)) result)))


(defmethod walk ((self comment))
  (list :comment
        '() (walk (comment-data self))))


(defmethod walk ((self foreign))
  (list :foreign
        '() (walk (foreign-data self))))


(defmethod walk ((self attribute))
  (list (intern (string-upcase (attribute-name self)) +tag-package+)
        (or (attribute-value self) t)))


(defmethod walk ((self open-tag))
  (list :open
        (intern (string-upcase (open-tag-name self)) +tag-package+)      
        (walk (open-tag-attributes self))))


(defmethod walk ((self close-tag))
  (list :close
        (intern (string-upcase (close-tag-name self)) +tag-package+)
        (walk (close-tag-attributes self))))


(defmethod walk ((self definition))
  (list :definition
        (intern (string-upcase (definition-name self)) +tag-package+)      
        (walk (definition-attributes self))))


(defmethod walk ((self t))  self)


(defun clean-attribute (attr)
  (cond ((not (stringp attr)) attr)
        ((or (and (<= 2 (length attr))
                  (char= (character "'") (char attr 0))
                  (char= (character "'") (char attr (1- (length attr)))))
             (and (<= 2 (length attr))
                  (char= (character "\"") (char attr 0))
                  (char= (character "\"") (char attr (1- (length attr))))))
         (subseq attr 1 (- (length attr) 1)))
        (t    attr)))


(defun encase (tag-list)
  ;;     content-list-reversed
  ;;     ( (tag (attributes)) content-list-reversed)
  ;;     ( (tag (attributes)) content-list-reversed)
  ;;     ( (tag (attributes)) content-list-reversed)
  ;;     ( (tag (attributes)) )
  (do* ((stack    (list nil))
        (tag-list tag-list       (cdr tag-list))
        (tag      (car tag-list) (car tag-list)))
       ((null tag-list)      (nreverse (pop stack)))
    (cond
      ((or (atom tag) (eq (car tag) :comment))
       (push tag (car stack)))
      ((eq (car tag) :definition)
       ;; ignore
       )
      ((eq (car tag) :open)
       (push (cons (second tag)
                   (list
                    (mapcan (lambda (kv)
                              (list (first kv)
                                    (clean-attribute (second kv))))
                            (third tag)))) (car stack))
       (unless (element-end-forbidden-p (second tag))
         (push nil stack)))
      ((and (eq (car tag) :close)
            (position (cadr tag) stack :key (lambda (item)  (and (consp item) 
                                                                 (consp (car item)) 
                                                                 (caar item)))))
       (until (and (consp (caar stack)) (eq (cadr tag) (caaar stack)))
              (let ((attributes (nreverse (pop stack))))
                (setf (caar stack) (append (caar stack) attributes))))))))
;; (eq (car tag) :close) and no corresponding open )))


(defun parse-html-file (pathname &key (verbose nil) (external-format :default))
  "
DO:                 Parse the HTML file PATHNAME.
VERBOSE:            When true, writes some information in the *TRACE-OUTPUT*.
EXTERNAL-FORMAT:    The external-format to use to open the HTML file.
RETURN:             A list of html elements.
SEE ALSO:           HTML-TAG, HTML-ATTRIBUTES, HTML-ATTRIBUTE, HTML-CONTENTS.
"
  (let ((name (namestring pathname))
        synthetic walked encased)
    (when verbose
      (format *trace-output* "~&starting parsing of file ~S~%" name))
    (setf synthetic (with-open-file (src pathname :direction :input 
                                         :if-does-not-exist :error
                                         :external-format external-format)
                      (let ((parser (make-html-parser 
                                     :scanner (make-html-scanner :source src))))
                        (advance parser)
                        (advance parser)
                        (parse-file parser))))
    (when verbose (format *trace-output* "~&file ~S parsed~%" name))
    (setf walked (walk synthetic))
    (when verbose (format *trace-output* "~&file ~S walked~%" name))
    (setf encased (encase walked))
    (when verbose (format *trace-output* "~&file ~S encased -- done.~%" name))
    encased))


(defun parse-html-string (string &key (start 0) (end (length string)) (verbose nil))
   "
DO:                 Parse the HTML in the STRING (between START and END)
VERBOSE:            When true, writes some information in the *TRACE-OUTPUT*.
RETURN:             A list of html elements.
SEE ALSO:           HTML-TAG, HTML-ATTRIBUTES, HTML-ATTRIBUTE, HTML-CONTENTS.
" 
  (when verbose
    (format *trace-output* "~&starting string parsing from ~D~%" start))
  (let ((synthetic  (with-input-from-string (src string :start start :end end)
                      (let ((parser (make-html-parser
                                     :scanner (make-html-scanner :source src))))
                        (advance parser)
                        (advance parser)
                        (parse-file parser))))
        walked encased)
    (when verbose (format *trace-output* "~&string parsed~%"))
    (setf walked (walk synthetic))
    (when verbose (format *trace-output* "~&string walked~%"))
    (setf encased (encase walked))
    (when verbose (format *trace-output* "~&string encased -- done.~%"))
    encased))



(defun html-tag        (html)
  "RETURN: The TAG of the HTML element."
  (first  html))
(defun html-attributes (html)
  "RETURN: The ATTRIBUTES of the HTML element."
  (second html))
(defun html-contents   (html)
  "RETURN: The CONTENTS of the HTML element."
  (cddr   html))
(defun html-attribute  (html key)
  "RETURN: The ATTRIBUTE named KEY in the HTML element."
  (cadr (member key (second html))))


(defparameter *nl* (make-hash-table)
  "
This hash-table maps tag symbols (interned in this package)
to a list of two elements:

- a list of keywords indicating the newlines that should be written
  around the element when writing HTML:
       :bo  before open tag.
       :ao  after open tag.
       :bc  before close tag.
       :ac  after open tag.

- a function taking the element as parameted (named SELF), used
  to format the element as text.
")



(defun write-text (element)
  (typecase element
    (string (princ (melt-entities element)))
    (atom   (princ element))
    (otherwise
     (let ((entry (gethash (html-tag element) *nl*)))
       (if entry
           (funcall (second entry) element)
           (princ element))))))

(defun write-children-text (self)
  (dolist (child (html-contents self))
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
                                (write-children-text self)) #(#\newline)))
    (princ "   ") (princ line) (terpri)))

(defun write-parenthesized-children (self left right)
  (princ left)
  (write-children-text self)
  (princ right))

(defmacro define-element-writer (tag nls &body body)
  `(progn
     (setf (gethash (intern (string-upcase ,(symbol-name tag)) +tag-package+) *nl*)
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

(define-element-writer a                (:bo :ac)          :children)
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
(define-element-writer cite             (:bo :ac)          :children)
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
  (let ((alt (html-attribute self :alt)))
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
(define-element-writer span             (:bo :ac)          :children)
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


(define-modify-macro appendf (&rest args) 
  append "Append onto list")


(defun collect-table-cells (element)
  (when (listp element)
    (case (html-tag element)
      ((:table)  (let ((*row-kind* :body)
                       (rows       '()))
                   (dolist (child (html-contents element) rows)
                     (when (listp child)
                       (case (html-tag child)
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
                  :tag        (html-tag element)
                  :attributes (html-attributes element)
                  :cells      (mapcar (function collect-table-cells)
                                      (remove-if-not (lambda (element)
                                                       (and (listp element)
                                                            (eql :td (html-tag element))))
                                                     (html-contents element)))))
      ((:td)     (make-cell :attributes (html-attributes element)
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



(defun unparse-html (html &optional (stream *standard-output*))
  "Writes back on STREAM the reconstituted HTML source."
  (if (atom html)
      (format stream "~A" html)
      (let ((nl (gethash (html-tag html) *nl*)))
        (format stream "~:[~;~&~]<~A~{ ~A=~S~}>~:[~;~&~]" 
                (member :bo nl)
                (string-downcase (html-tag html))
                (html-attributes html)
                (member :ao nl))
        (dolist (item (html-contents html))
          (unparse-html item stream))
        (format stream "~:[~;~&~]</~A>~:[~;~&~]"
                (member :bc nl)
                (string-downcase (html-tag html))
                (member :ac nl)))))



(defun write-html-text (html &optional (stream *standard-output*))
  "Writes on STREAM a textual rendering of the HTML.
Some reStructuredText formating is used.
Simple tables are rendered, but colspan and rowspan are ignored.
"
  (let ((*standard-output* stream))
    (write-text html)))


;;;; THE END ;;;;

