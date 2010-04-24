;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               html-iso8879-1.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;   ISO 8879:1986 SGML entities (HTML 3.2).
;;;;   (Related to, but distinct from: ISO 8859-1).
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-11-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2004
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTML-ISO8879-1"
  (:USE "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.ARRAY"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING")
  (:EXPORT "yuml" "thorn" "yacute" "uuml" "ucirc" "uacute" "ugrave" "oslash"
           "divide" "ouml" "otilde" "ocirc" "oacute" "ograve" "ntilde" "eth" "iuml"
           "icirc" "iacute" "igrave" "euml" "ecirc" "eacute" "egrave" "ccedil" "aelig"
           "aring" "auml" "atilde" "acirc" "aacute" "agrave" "szlig" "THORN" "Yacute"
           "Uuml" "Ucirc" "Uacute" "Ugrave" "Oslash" "times" "Ouml" "Otilde" "Ocirc"
           "Oacute" "Ograve" "Ntilde" "ETH" "Iuml" "Icirc" "Iacute" "Igrave" "Euml"
           "Ecirc" "Eacute" "Egrave" "Ccedil" "AElig" "Aring" "Auml" "Atilde" "Acirc"
           "Aacute" "Agrave" "iquest" "frac34" "frac12" "frac14" "raquo" "ordm" "sup1"
           "cedil" "middot" "para" "micro" "acute" "sup3" "sup2" "plusmn" "deg" "macr"
           "reg" "shy" "not" "laquo" "ordf" "copy" "uml" "sect" "brvbar" "yen" "curren"
           "pound" "cent" "iexcl" "nbsp" "MELT-ENTITIES")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.ARRAY" "DISPLACED-VECTOR")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING" "STRING-REPLACE")
  (:DOCUMENTATION
   "ISO 8879:1986 SGML entities (HTML 3.2).
    (Related to, but distinct from: ISO 8859-1).
    
    Copyright Pascal J. Bourguignon 2003 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTML-ISO8879-1")




(DEFPARAMETER *ENTITIES* NIL)



(DEFUN MELT-ENTITIES (STRING)
  "
RETURN: A string with any HTML ISO-Latin-1 entity occurence replaced by
        the corresponding character.
BUG:    We don't manage the encodings, assuming that ISO-Latin-1 is active.
"
  (let ((chunks (list string)))
    (DOLIST (SUBSTITUTION *ENTITIES*
             (apply (function concatenate) 'string chunks))
      (when (some (lambda (chunk) (search (car substitution) chunk
                                          :test (function string=))) chunks)
        (setf chunks
              (mapcan
               (lambda (chunk) 
                 (do* ((start 0 (+ pos (length (car substitution))))
                       (pos (search (car substitution) chunk 
                                    :start2 start
                                    :test (function string=))
                            (search (car substitution) chunk 
                                    :start2 start
                                    :test (function string=)))
                       (result '())
                       )
                      ((null pos)
                       (cond
                         ((= 0 start) 
                          (push chunk result))
                         ((< start (length chunk)) 
                          (push (displaced-vector chunk start) result)))
                       (nreverse result))
                   (when (< start pos)
                     (push (displaced-vector chunk start pos) result))
                   (push (cdr substitution) result)))
               chunks)))))) ;;MELT-ENTITIES

                                

(DEFMACRO DEFENTITY (NAME CODE &OPTIONAL DOCUMENTATION)
  ;; TODO: "This implementation of DEFENTITY is NOT strict ANSI."
  `(PROGN
     (DEFCONSTANT ,NAME (CODE-CHAR ,CODE) ,DOCUMENTATION)
     (PUSH (CONS (FORMAT NIL "&~A;" ',NAME) (FORMAT NIL "~C" (CODE-CHAR ,CODE)))
           *ENTITIES*))) ;;DEFENTITY


;; Not strictly ISO-8879, but it's convenient here...
(DEFENTITY |amp|         38  "ampersand")
(DEFENTITY |gt|          62  "greater than")
(DEFENTITY |lt|          60  "less than")


;; The meat:
(DEFENTITY |nbsp|       160  "no-break space")
(DEFENTITY |iexcl|      161  "inverted exclamation mark")
(DEFENTITY |cent|       162  "cent sign")
(DEFENTITY |pound|      163  "pound sterling sign")
(DEFENTITY |curren|     164  "general currency sign")
(DEFENTITY |yen|        165  "yen sign")
(DEFENTITY |brvbar|     166  "broken (vertical) bar")
(DEFENTITY |sect|       167  "section sign")
(DEFENTITY |uml|        168  "umlaut (dieresis)")
(DEFENTITY |copy|       169  "copyright sign")
(DEFENTITY |ordf|       170  "ordinal indicator, feminine")
(DEFENTITY |laquo|      171  "angle quotation mark, left")
(DEFENTITY |not|        172  "not sign")
(DEFENTITY |shy|        173  "soft hyphen")
(DEFENTITY |reg|        174  "registered sign")
(DEFENTITY |macr|       175  "macron")
(DEFENTITY |deg|        176  "degree sign")
(DEFENTITY |plusmn|     177  "plus-or-minus sign")
(DEFENTITY |sup2|       178  "superscript two")
(DEFENTITY |sup3|       179  "superscript three")
(DEFENTITY |acute|      180  "acute accent")
(DEFENTITY |micro|      181  "micro sign")
(DEFENTITY |para|       182  "pilcrow (paragraph sign)")
(DEFENTITY |middot|     183  "middle dot")
(DEFENTITY |cedil|      184  "cedilla")
(DEFENTITY |sup1|       185  "superscript one")
(DEFENTITY |ordm|       186  "ordinal indicator, masculine")
(DEFENTITY |raquo|      187  "angle quotation mark, right")
(DEFENTITY |frac14|     188  "fraction one-quarter")
(DEFENTITY |frac12|     189  "fraction one-half")
(DEFENTITY |frac34|     190  "fraction three-quarters")
(DEFENTITY |iquest|     191  "inverted question mark")
(DEFENTITY |Agrave|     192  "capital A, grave accent")
(DEFENTITY |Aacute|     193  "capital A, acute accent")
(DEFENTITY |Acirc|      194  "capital A, circumflex accent")
(DEFENTITY |Atilde|     195  "capital A, tilde")
(DEFENTITY |Auml|       196  "capital A, dieresis or umlaut mark")
(DEFENTITY |Aring|      197  "capital A, ring")
(DEFENTITY |AElig|      198  "capital AE diphthong (ligature)")
(DEFENTITY |Ccedil|     199  "capital C, cedilla")
(DEFENTITY |Egrave|     200  "capital E, grave accent")
(DEFENTITY |Eacute|     201  "capital E, acute accent")
(DEFENTITY |Ecirc|      202  "capital E, circumflex accent")
(DEFENTITY |Euml|       203  "capital E, dieresis or umlaut mark")
(DEFENTITY |Igrave|     204  "capital I, grave accent")
(DEFENTITY |Iacute|     205  "capital I, acute accent")
(DEFENTITY |Icirc|      206  "capital I, circumflex accent")
(DEFENTITY |Iuml|       207  "capital I, dieresis or umlaut mark")
(DEFENTITY |ETH|        208  "capital Eth, Icelandic")
(DEFENTITY |Ntilde|     209  "capital N, tilde")
(DEFENTITY |Ograve|     210  "capital O, grave accent")
(DEFENTITY |Oacute|     211  "capital O, acute accent")
(DEFENTITY |Ocirc|      212  "capital O, circumflex accent")
(DEFENTITY |Otilde|     213  "capital O, tilde")
(DEFENTITY |Ouml|       214  "capital O, dieresis or umlaut mark")
(DEFENTITY |times|      215  "multiply sign")
(DEFENTITY |Oslash|     216  "capital O, slash")
(DEFENTITY |Ugrave|     217  "capital U, grave accent")
(DEFENTITY |Uacute|     218  "capital U, acute accent")
(DEFENTITY |Ucirc|      219  "capital U, circumflex accent")
(DEFENTITY |Uuml|       220  "capital U, dieresis or umlaut mark")
(DEFENTITY |Yacute|     221  "capital Y, acute accent")
(DEFENTITY |THORN|      222  "capital THORN, Icelandic")
(DEFENTITY |szlig|      223  "small sharp s, German (sz ligature)")
(DEFENTITY |agrave|     224  "small a, grave accent")
(DEFENTITY |aacute|     225  "small a, acute accent")
(DEFENTITY |acirc|      226  "small a, circumflex accent")
(DEFENTITY |atilde|     227  "small a, tilde")
(DEFENTITY |auml|       228  "small a, dieresis or umlaut mark")
(DEFENTITY |aring|      229  "small a, ring")
(DEFENTITY |aelig|      230  "small ae diphthong (ligature)")
(DEFENTITY |ccedil|     231  "small c, cedilla")
(DEFENTITY |egrave|     232  "small e, grave accent")
(DEFENTITY |eacute|     233  "small e, acute accent")
(DEFENTITY |ecirc|      234  "small e, circumflex accent")
(DEFENTITY |euml|       235  "small e, dieresis or umlaut mark")
(DEFENTITY |igrave|     236  "small i, grave accent")
(DEFENTITY |iacute|     237  "small i, acute accent")
(DEFENTITY |icirc|      238  "small i, circumflex accent")
(DEFENTITY |iuml|       239  "small i, dieresis or umlaut mark")
(DEFENTITY |eth|        240  "small eth, Icelandic")
(DEFENTITY |ntilde|     241  "small n, tilde")
(DEFENTITY |ograve|     242  "small o, grave accent")
(DEFENTITY |oacute|     243  "small o, acute accent")
(DEFENTITY |ocirc|      244  "small o, circumflex accent")
(DEFENTITY |otilde|     245  "small o, tilde")
(DEFENTITY |ouml|       246  "small o, dieresis or umlaut mark")
(DEFENTITY |divide|     247  "divide sign")
(DEFENTITY |oslash|     248  "small o, slash")
(DEFENTITY |ugrave|     249  "small u, grave accent")
(DEFENTITY |uacute|     250  "small u, acute accent")
(DEFENTITY |ucirc|      251  "small u, circumflex accent")
(DEFENTITY |uuml|       252  "small u, dieresis or umlaut mark")
(DEFENTITY |yacute|     253  "small y, acute accent")
(DEFENTITY |thorn|      254  "small thorn, Icelandic")
(DEFENTITY |yuml|       255  "small y, dieresis or umlaut mark")


;;;; html-iso8879-1.lisp              --                     --          ;;;;
