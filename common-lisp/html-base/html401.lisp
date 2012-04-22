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
