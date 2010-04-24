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
