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
;;;;    2005-02-22 <PJB> Optimized WALK for HTML-SEQ.
;;;;    2003-11-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2005
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-PARSER.PARSE-HTML"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.FILE.PEEK-STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-ENTITIES")
  (:EXPORT "HTML-ATTRIBUTE" "HTML-CONTENTS" "HTML-ATTRIBUTES" "HTML-TAG"
           "UNPARSE-HTML" "WRITE-HTML-TEXT"
           "PARSE-HTML-STRING" "PARSE-HTML-FILE")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING" "UNSPLIT-STRING"
                "SPLIT-STRING" "STRING-REPLACE")
  (:DOCUMENTATION "This package exports functions to parse HTML pages."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-PARSER.PARSE-HTML")


;; ------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +tag-package+ (find-package "KEYWORD"))
  (DEFVAR *ATTRIBUTES* () "List of symbols of all attributes defined.")
  (DEFVAR *ELEMENTS*   () "List of symbols of all elements defined."))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (DEFMACRO DEFATTRIBUTE (ATTR-NAME ELEMENTS TYPE DEFAULT OPTIONS DOCUMENTATION)
    "
DO:       Defines an HTML attribute.
"
    (declare (ignore ATTR-NAME ELEMENTS TYPE DEFAULT OPTIONS DOCUMENTATION))
    ;; NOP
    (VALUES))
  );;eval-when

(eval-when (:compile-toplevel :load-toplevel :execute)
  (DEFSTRUCT ELEMENT
    NAME OPTIONS DOCUMENTATION))


(defun find-element (element-name) (getf *elements* element-name))


(DEFUN ELEMENT-EMPTY-P (ELEMENT-NAME)
  (and (find-element ELEMENT-NAME)
       (MEMBER :EMPTY (ELEMENT-OPTIONS (find-element ELEMENT-NAME))
               :TEST (FUNCTION EQ))))


(DEFUN ELEMENT-START-OPTIONAL-P (ELEMENT-NAME)
  (or (not (find-element ELEMENT-NAME))
      (MEMBER :START-OPTIONAL (ELEMENT-OPTIONS (find-element ELEMENT-NAME))
              :TEST (FUNCTION EQ))))


(DEFUN ELEMENT-END-OPTIONAL-P (ELEMENT-NAME)
  (or (not (find-element ELEMENT-NAME))
      (MEMBER :END-OPTIONAL (ELEMENT-OPTIONS (find-element ELEMENT-NAME))
              :TEST (FUNCTION EQ))))


(DEFUN ELEMENT-END-FORBIDDEN-P (ELEMENT-NAME)
  (and (find-element ELEMENT-NAME)
       (MEMBER :END-FORBIDDEN (ELEMENT-OPTIONS (find-element ELEMENT-NAME))
               :TEST (FUNCTION EQ))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (DEFMACRO DEFELEMENT (NAME OPTIONS &OPTIONAL (DOCUMENTATION "A HTML element."))
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
    (WITH-GENSYMS (GNAME)
      `(LET ((,GNAME (INTERN ,(STRING NAME) +tag-package+)))
            (PUSH (MAKE-ELEMENT :NAME ,GNAME
                                :OPTIONS ',OPTIONS
                                :DOCUMENTATION ',DOCUMENTATION) *ELEMENTS*)
            (PUSH ,GNAME *ELEMENTS*))))
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
         (has-char ()
           "Whether value is not empty."
           (plusp (fill-pointer value)))
         (last-char ()
           "Return the last character in value."
           (aref value (1- (fill-pointer value)))))
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
                        ((:OPEN-DEF)  (parse-definition parser))
                        ((:foreign)   (prog1 (make-foreign
                                              :data (html-parser-value parser))
                                        (advance parser)))
                        ((:COMMENT)   (prog1 (make-comment 
                                              :data (html-parser-value parser))
                                        (advance parser)))
                        ((:OPEN-TAG)  (parse-open-tag parser))
                        ((:CLOSE-TAG) (parse-close-tag parser))
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

:u(defun parse-open-tag (parser)
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

(DEFMETHOD WALK ((SELF HTML-SEQ))
  (do ((current self (html-seq-rest current))
       (result '()))
      ((not (typep current 'html-seq))
       (assert (null current))
       (nreverse result))
    (push (WALK (HTML-SEQ-FIRST current)) result)))


(DEFMETHOD WALK ((SELF COMMENT))
  (LIST :COMMENT
        '() (WALK (COMMENT-DATA SELF))))


(DEFMETHOD WALK ((SELF foreign))
  (LIST :foreign
        '() (WALK (foreign-DATA SELF))))


(DEFMETHOD WALK ((SELF ATTRIBUTE))
  (LIST (INTERN (STRING-UPCASE (ATTRIBUTE-NAME SELF)) +tag-package+)
        (OR (ATTRIBUTE-VALUE SELF) T)))


(DEFMETHOD WALK ((SELF OPEN-TAG))
  (LIST :OPEN
        (INTERN (STRING-UPCASE (OPEN-TAG-NAME SELF)) +tag-package+)      
        (WALK (OPEN-TAG-ATTRIBUTES SELF))))


(DEFMETHOD WALK ((SELF CLOSE-TAG))
  (LIST :CLOSE
        (INTERN (STRING-UPCASE (CLOSE-TAG-NAME SELF)) +tag-package+)
        (WALK (close-TAG-ATTRIBUTES SELF))))


(DEFMETHOD WALK ((SELF DEFINITION))
  (LIST :DEFINITION
        (INTERN (STRING-UPCASE (DEFINITION-NAME SELF)) +tag-package+)      
        (WALK (DEFINITION-ATTRIBUTES SELF))))


(DEFMETHOD WALK ((SELF T))  SELF)


(DEFUN CLEAN-ATTRIBUTE (ATTR)
  (cond ((not (stringp attr)) attr)
        ((OR (AND (<= 2 (length attr))
                  (CHAR= (CHARACTER "'") (CHAR ATTR 0))
                  (CHAR= (CHARACTER "'") (CHAR ATTR (1- (LENGTH ATTR)))))
             (AND (<= 2 (length attr))
                  (CHAR= (CHARACTER "\"") (CHAR ATTR 0))
                  (CHAR= (CHARACTER "\"") (CHAR ATTR (1- (LENGTH ATTR))))))
         (SUBSEQ ATTR 1 (- (LENGTH ATTR) 1)))
        (t    ATTR)))


(DEFUN ENCASE (TAG-LIST)
  ;;     content-list-reversed
  ;;     ( (tag (attributes)) content-list-reversed)
  ;;     ( (tag (attributes)) content-list-reversed)
  ;;     ( (tag (attributes)) content-list-reversed)
  ;;     ( (tag (attributes)) )
  (DO* ((STACK    (list NIL))
        (TAG-LIST TAG-LIST       (CDR TAG-LIST))
        (TAG      (CAR TAG-LIST) (CAR TAG-LIST)))
       ((NULL TAG-LIST)      (nreverse (pop stack)))
    (COND
      ((OR (ATOM TAG) (EQ (CAR TAG) :COMMENT))
       (PUSH TAG (CAR STACK)))
      ((EQ (CAR TAG) :DEFINITION)
       ;; ignore
       )
      ((EQ (CAR TAG) :OPEN)
       (PUSH (CONS (SECOND TAG)
                   (LIST
                    (MAPCAN (LAMBDA (KV)
                              (LIST (FIRST KV)
                                    (CLEAN-ATTRIBUTE (SECOND KV))))
                            (THIRD TAG)))) (CAR STACK))
       (unless (ELEMENT-END-FORBIDDEN-P (second tag))
         (PUSH NIL STACK)))
      ((and (EQ (CAR TAG) :CLOSE)
            (position (cadr tag) stack :key (lambda (item)  (and (consp item) 
                                                                 (consp (car item)) 
                                                                 (caar item)))))
       (UNTIL (AND (CONSP (CAAR STACK)) (EQ (CADR TAG) (CAAAR STACK)))
              (LET ((ATTRIBUTES (NREVERSE (POP STACK))))
                (SETF (CAAR STACK) (APPEND (CAAR STACK) ATTRIBUTES))))))))
;; (eq (car tag) :close) and no corresponding open )))


(defun parse-html-file (pathname &key (verbose nil) (external-format :default))
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


(defun parse-html-string (string &key (start 0) (verbose nil))
  (when verbose
    (format *trace-output* "~&starting string parsing from ~D~%" start))
  (let ((synthetic  (with-input-from-string (src string :start start)
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



(defun html-tag        (html)     (first  html))
(defun html-attributes (html)     (second html))
(defun html-contents   (html)     (cddr   html))
(defun html-attribute  (html key) (cadr (member key (second html))))


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

(define-element-writer A                (:bo :ac)          :children)
(define-element-writer ABBR             ()                 :children)
(define-element-writer ACRONYM          ()                 :children)
(define-element-writer ADDRESS          (:bo :ac)          :children)
(define-element-writer APPLET           (:bo :ao :bc :ac)  :skip)
(define-element-writer AREA             (:bo :ac)          :children)
(define-element-writer B                ()                 (write-parenthesized-children self "**" "**"))
(define-element-writer BASE             ()                 :children)
(define-element-writer BASEFONT         ()                 :children)
(define-element-writer BDO              ()                 :children)
(define-element-writer BIG              ()                 :children)
(define-element-writer BLOCKQUOTE       (:bo :ao :bc :ac)  (write-indented-children self))
(define-element-writer BODY             (:bo :ao :bc :ac)  :children)
(define-element-writer BR               (:bo :ac)
  (terpri)
  (write-children-text self))
(define-element-writer BUTTON           (:bo :ac)          :children)
(define-element-writer CENTER           (:bo :ac)          :children)
(define-element-writer CITE             (:bo :ac)          :children)
(define-element-writer CODE             ()                 (write-parenthesized-children self "`" "`"))
(define-element-writer DEL              ()                 :children)
(define-element-writer DFN              ()                 :children)
(define-element-writer DIR              ()                 :children)
(define-element-writer DIV              (:bo :ac)          :children)
(define-element-writer EM               ()                 (write-parenthesized-children self "*" "*"))
(define-element-writer FIELDSET         ()                 :children)
(define-element-writer FONT             ()                 :children)
(define-element-writer FORM             (:bo :ao :bc :ac)  :children)
(define-element-writer FRAME            (:bo :ao :bc :ac)  :children)
(define-element-writer FRAMESET         (:bo :ao :bc :ac)  :children)
(define-element-writer H1               (:bo :ac)          (write-title self #\#))
(define-element-writer H2               (:bo :ac)          (write-title self #\*))
(define-element-writer H3               (:bo :ac)          (write-title self #\=))
(define-element-writer H4               (:bo :ac)          (write-title self #\-))
(define-element-writer H5               (:bo :ac)          (write-title self #\^))
(define-element-writer H6               (:bo :ac)          (write-title self #\.))
(define-element-writer HEAD             (:bo :ao :bc :ac)
  :children)
(define-element-writer HR               (:bo :ac)
  (terpri)
  (princ (make-string 78 :initial-element #\-))
  (terpri)
  (write-children-text self))
(define-element-writer HTML             (:bo :ao :bc :ac)  :children)
(define-element-writer I                ()                 (write-parenthesized-children self "/" "/"))
(define-element-writer IFRAME           (:bo :ao :bc :ac)  :children)
(define-element-writer IMG              (:bo :ac)
  (let ((alt (html-attribute self :alt)))
    (when alt (princ alt))))
(define-element-writer INPUT            (:bo :ac)          :children)
(define-element-writer INS              ()                 :children)
(define-element-writer ISINDEX          ()                 :children)
(define-element-writer KBD              ()                 (write-parenthesized-children self "[" "]"))
(define-element-writer LABEL            ()                 :children)
(define-element-writer LEGEND           ()                 :children)
(define-element-writer LINK             (:bo :ac)          :children)
(define-element-writer MAP              (:bo :ao :bc :ac)  :children)
(define-element-writer MENU             (:bo :ao :bc :ac)  :children)
(define-element-writer META             (:bo :ac)          :children)
(define-element-writer NOFRAMES         (:bo :ao :bc :ac)  :children)
(define-element-writer NOSCRIPT         (:bo :ao :bc :ac)  :children)
(define-element-writer OBJECT           (:bo :ao :bc :ac)  :children)
(define-element-writer OPTGROUP         (:bo :ao :bc :ac)  :children)
(define-element-writer OPTION           (:bo :ac)          :children)
(define-element-writer P                (:bo :ac)
  (terpri) (terpri)
  (write-children-text self))
(define-element-writer PARAM            (:bo :ac)          :children)
(define-element-writer PRE              (:bo :ac)
  (terpri)
  (princ "::")
  (terpri) (terpri)
  (write-indented-children self)
  (terpri) (terpri))
(define-element-writer Q                ()                 :children)
(define-element-writer S                ()                 :children)
(define-element-writer SAMP             ()                 :children) 
(define-element-writer SCRIPT           (:bo :ao :bc :ac)  :skip)
(define-element-writer SELECT           (:bo :ao :bc :ac)  :children)
(define-element-writer SMALL            ()                 :children)
(define-element-writer SPAN             (:bo :ac)          :children)
(define-element-writer STRIKE           ()                 :children)
(define-element-writer STRONG           ()                 (write-parenthesized-children self "**" "**"))
(define-element-writer STYLE            (:bo :ac)          :children)
(define-element-writer SUB              ()                 :children)
(define-element-writer SUP              ()                 :children)
(define-element-writer TEXTAREA         (:bo :ac)          :children)
(define-element-writer TITLE            (:bo :ac)          (write-title self #\# t))
(define-element-writer TT               ()                 (write-parenthesized-children self "`" "`"))
(define-element-writer U                ()                 :children)
(define-element-writer VAR              ()                 :children)


(defvar *ol-index* nil)
(defvar *ol-stack* '())

(define-element-writer DL               (:bo :ac)
  (push *ol-index* *ol-stack*)
  (setf *ol-index* nil)
  (terpri) (write-children-text self) )
(define-element-writer DT               (:bo)
  (terpri)
  (write-children-text self))
(define-element-writer DD               (:bo)
  (terpri)
  (write-indented-children self))

(define-element-writer OL               (:bo :ao :bc :ac)
  (push *ol-index* *ol-stack*)
  (setf *ol-index* 0)
  (terpri) (write-children-text self) )
(define-element-writer UL               (:bo :ao :bc :ac)
  (push *ol-index* *ol-stack*)
  (setf *ol-index* nil)
  (terpri) (write-children-text self))
(define-element-writer LI               (:bo :ac)
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

(define-element-writer TABLE            (:bo :ao :bc :ac)
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
(define-element-writer CAPTION          ()                 :children)
(define-element-writer COLGROUP         ()                 :children)
(define-element-writer COL              ()                 :children)
(define-element-writer THEAD            (:bo :ao :bc :ac)  :children)
(define-element-writer TFOOT            (:bo :ao :bc :ac)  :children)
(define-element-writer TBODY            (:bo :ao :bc :ac)  :children)
(define-element-writer TR               (:bo :ac)          :children)
(define-element-writer TH               (:bo :ac)          :children)
(define-element-writer TD               (:bo :ac)          :children)



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


;;;; parse-html.lisp                  --                     --          ;;;;

