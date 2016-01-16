;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               markups.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-13 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
;;;;**************************************************************************


(defparameter *contents*
  '((no-markup   "A string without newlines or markup.")
    (markup-line "A string without newlines, with possibly markup.")
    (multi-line  "A string possibly with newlines and markup.")
    (url         "A string containing an URL.")))

;; markup ::= (item…) | compound .
;; item   ::= STRING | content .
;; content ::= no-markup | markup-line | multi-line | url .
;; compound ::= (or markup…)
;;            | (each-line markup)
;;            | (lines markup…)
;;            | (same-length content STRING) .



(defparameter *markups*
  '(("AsciiDoc"
     ;; double operators ignore special characters
     :bold      (or ("*" markup-line "*") ("**" no-markup "**"))
     :italic    (or ("_" markup-line "_") ("__" no-markup "__"))
     :monospace (or ("+" markup-line "+") ("++" no-markup "++"))
     :header1   (or ("=" markup-line (optional "="))
                 (lines (markup-line)
                  (same-length markup-line "=")))
     :header2   (or ("==" markup-line (optional "=="))
                 (lines (markup-line)
                  (same-length markup-line "-")))
     :header3   (or ("===" markup-line (optional "==="))
                 (lines (markup-line)
                  (same-length markup-line "~")))
     :link      (or (url) (url "[" markup-line "]") ))
    ("BBCode"
     :bold      ("[b]" multi-line "[/b]")
     :italic    ("[i]" multi-line "[/i]")
     :monospace ("[code]" multi-line "[/code]")
     :link      (or ("[url]" url "[/url]")
                 ("[url=" url "]" markup-line "[/url]")))
    ("Creole"
     ;; Triple curly braces are for nowiki which is optionally
     ;; monospace in Creole (the choice of the implementor). Future
     ;; Creole additions may introduce double hash marks (##) for
     ;; monospace.
     :bold      ("**" markup-line "**")
     :italic    ("//" markup-line "//")
     :monospace ("{{{" multi-line "}}}")
     :header1   ("=" markup-line (optional "="))
     :header2   ("==" markup-line (optional "=="))
     :header3   ("===" markup-line (optional "==="))
     :link      (or ("[[" url "]]")
                 ("[[" url "|" markup-line "]]")))
    ("deplate"
     :bold      ("{text style=bold: " multi-line "}")
     :italic    ("__" markup-line "__")
     :monospace ("''" markup-line "''")
     :header1   ("*" markup-line)
     :header2   ("**" markup-line)
     :header3   ("***" markup-line)
     :link      (or ("[[" url "]]")
                 ("[[" url "][" markup-line "]]")))
    ("Markdown"
     :bold      ("**" markup-line "**")
     :italic    ("*" markup-line "*")
     ;; Monospace text is created by indenting that line 4 spaces or
     ;; one tab character, or enclosing text in backticks:
     ;; `monospaces`.
     :monospace (or
                 ("`" markup-line "`")
                 (each-line ("    " markup-line)))
     :header1   (or ("#" markup-line (optional "#"))
                 (lines (markup-line)
                  (same-length markup-line "=")))
     :header2   (or ("##" markup-line (optional "##"))
                 (lines (markup-line)
                  (same-length markup-line "-")))
     :header3    ("###" markup-line (optional "###"))
     :header4    ("####" markup-line (optional "####"))
     :header5    ("#####" markup-line (optional "#####"))
     :header6    ("######" markup-line (optional "######"))
     :link       ("[" markup-line  "](" url ")")
     ;; Also [link-text][linkref]
     ;; followed by [linkref]: url "title"
     )
    ("MediaWiki"
     :bold      ("'''" markup-line "'''")
     :italic    ("''" markup-line "''")
     :monospace ("<code>" markup-line "</code>")
     :header1    ("=" markup-line)
     :header2    ("==" markup-line)
     :header3    ("===" markup-line)
     :header4    ("====" markup-line)
     :header5    ("=====" markup-line)
     :header6    ("======" markup-line)
     :link       (or ("[[" url "]]")
                  ("[[" url "|" markup-line "]]")))
    ("Org-mode"
     :bold           ("*" markup-line "*")
     :italic         ("/" markup-line "/")
     :underlined     ("_" markup-line "_")
     :strike-through ("+" markup-line "+")
     :monospace      ("=" markup-line "=")
     :verbatim       ("~" markup-line "~")
     :header1    ("*" markup-line)
     :header2    ("**" markup-line)
     :header3    ("***" markup-line)
     :link      (or ("[[" url "]]")
                 ("[[" url "][" markup-line "]]")))
    ("PmWiki"
     :bold      ("'''" markup-line "'''")
     :italic    ("''" markup-line "''")
     :monospace ("@@" markup-line "@@")
     :header1    ("!" markup-line)
     :header2    ("!!" markup-line)
     :header3    ("!!!" markup-line)
     :header4    ("!!!!" markup-line)
     :header5    ("!!!!!" markup-line)
     :header6    ("!!!!!!" markup-line)
     :link      (or ("[[" url "]]")
                 ("[[" url "][" markup-line "]]")))
    ("POD"
     :bold      ("B<" markup-line ">")
     :italic    ("I<" markup-line ">")
     :monospace (or
                 ("C<" markup-line ">")
                 (each-line ("    " markup-line)))
     :header1    ("=head1 " markup-line)
     :header2    ("=head2 " markup-line)
     :link       ("L<" url ">"))
    ("reStructuredText"
     :bold      ("**" markup-line "**")
     :italic    ("*" markup-line "*")
     :monospace ("``" markup-line "``")
     ;; Any of the following characters can be used as the
     ;; "underline": = - ` : ' " ~ ^ _ * + # < >. The same character
     ;; must be used for the same indentation level and may not be
     ;; used for a new indentation level.
     ;; And the lines can also appear above and below, not just below.
     :header1   (lines (markup-line)
                 (same-length markup-line "#"))
     :header2   (lines (markup-line)
                 (same-length markup-line "="))
     :header3   (lines (markup-line)
                 (same-length markup-line "-"))
     :header4   (lines (markup-line)
                 (same-length markup-line "+"))
     :header5   (lines (markup-line)
                 (same-length markup-line "~"))
     :header6   (lines (markup-line)
                 (same-length markup-line "^"))
     :link      ("`" markup-line "<" url ">`_")
     ;; Also: Linkref_
     ;; followed by: .. _Linkref: url
     )
    ("Setext"
     :bold      ("**" markup-line "**")
     :italic    ("~" markup-line "~"))
    ("Textile"
     :bold      (or ("*" markup-line "*") ("**" markup-line "**"))
     :italic    (or ("_" markup-line "_") ("__" markup-line "__"))
     :monospace ("@" multi-line "@")
     :header1    ("h1. " markup-line)
     :header2    ("h2. " markup-line)
     :header3    ("h3. " markup-line)
     :header4    ("h4. " markup-line)
     :header5    ("h5. " markup-line)
     :header6    ("h6. " markup-line)
     :link       ("\"" markup-line "\":" url)
     ;; Also: "link text (optional title attribute)":url
     ;; Also: "link text":linkref
     ;; followed by : [linkref (optional title attribute)]url
     )
    ("Texy!"
     :bold      ("**" markup-line "**")
     :italic    (or ("*" markup-line "*") ("//" markup-line "//"))
     :monospace ("`" multi-line "`")
     :header1   (or ("######" markup-line (optional "######"))
                 (lines (markup-line)
                  (same-length markup-line "#")))
     :header2   (or ("#####" markup-line (optional "#####"))
                 (lines (markup-line)
                  (same-length markup-line "*")))
     :header3   (or ("####" markup-line (optional "####"))
                 (lines (markup-line)
                  (same-length markup-line "=")))
     :header4   (or ("###" markup-line (optional "###"))
                 (lines (markup-line)
                  (same-length markup-line "-")))
     :header5    ("##" markup-line (optional "##"))
     :header6   ("#" markup-line (optional "#"))
     :link     ("\"" markup-line "\":" url)
     ;; Also: "link text .(optional title attribute)[optional class or id]{optional style}":url
     ;; Also: "link text":linkref
     ;; followed by : [linkref]:url .(optional title attribute)[optional class or id]{optional style}]
     )
    ("txt2tags"
     :bold           ("**" markup-line "**")
     :italic         ("//" markup-line "//")
     :monospace      ("``" multi-line "``")
     :underlined     ("__" markup-line "__")    
     :strike-through ("--" markup-line "--")
     :header1    (or ("=" markup-line "=") ("+" markup-line "+"))
     :header2    (or ("==" markup-line "==") ("++" markup-line "++"))
     :header3    (or ("===" markup-line "===") ("+++" markup-line "+++"))
     :header4    (or ("====" markup-line "====") ("++++" markup-line "++++"))
     :header5    (or ("=====" markup-line "=====") ("+++++" markup-line "+++++"))
     :header6    (or ("======" markup-line "======") ("++++++" markup-line "++++++"))
     :link       ("[" markup-line url "]"))))

  

(docutils:read-rst (com.informatimago.common-lisp.cesarum.file:text-file-contents
                    #p"/home/pjb/d/specifications.rst"))


;;;; THE END ;;;;


