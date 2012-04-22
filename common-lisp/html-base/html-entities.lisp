;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               html-entities.lisp
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
;;;;    2010-10-16 <PJB> Renamed HTML-ENTITIES.
;;;;                     Added handling of numerical &#...; and &#x...; entities.
;;;;    2003-11-14 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-ENTITIES"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "yuml" "thorn" "yacute" "uuml" "ucirc" "uacute" "ugrave" "oslash"
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
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY" "DISPLACED-VECTOR")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING" "STRING-REPLACE")
  (:documentation
   "ISO 8879:1986 SGML entities (HTML 3.2).
    (Related to, but distinct from: ISO 8859-1).
    
    Copyright Pascal J. Bourguignon 2003 - 2010
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-ENTITIES")




(defparameter *entities*
  (make-hash-table :test (function equal))
  "Maps entity names (case sensitive strings)
to a string containing their corresponding characters.")



(defun melt-entities (text)
  "
RETURN: A string with any HTML ISO-Latin-1 entity occurence replaced by
        the corresponding character.
BUG:    We don't manage the encodings, assuming that ISO-Latin-1 is active.
"
  (with-output-to-string (*standard-output*)
    (loop
       :with state = :normal
       :with buffer = (make-array 8
                                  :element-type 'character
                                  :adjustable t
                                  :fill-pointer 0)
       :for ch :across text
       ;; :do (print (list state ch))
       :do (ecase state
             ((:normal) (case ch
                          ((#\&)
                           (setf state :ampersand))
                          ((#\newline  #\space)
                           (princ " ")
                           (setf state :space))
                          (otherwise
                           (princ ch))))
             ((:space) (case ch
                         ((#\newline #\space))
                         ((#\&)
                          (setf state :ampersand))
                         (otherwise
                          (princ ch)
                          (setf state :normal))))
             ((:ampersand)
              (case ch
                ((#\#)
                 (setf state :numeric-ampersand))
                (otherwise
                 (setf (fill-pointer buffer) 0)
                 (vector-push-extend ch buffer)
                 (setf state :named-entity))))
             ((:numeric-ampersand)
              (case ch
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                 (setf (fill-pointer buffer) 0)
                 (vector-push-extend ch buffer)
                 (setf state :decimal-entity))
                ((#\x #\X)
                 (setf (fill-pointer buffer) 0)
                 (vector-push-extend ch buffer)
                 (setf state :hexadecimal-entity))
                (otherwise
                 ;; broken
                 (format t "&#~C" ch)
                 (setf state :normal))))
             ((:named-entity)
              (case ch
                ((#\; #\space)        ; space is for buggy entities...
                 (let ((ch (gethash buffer *entities*)))
                   (if ch
                       (princ ch)
                       (format t "&~A;" buffer)))
                 (setf state :normal))
                (otherwise
                 (vector-push-extend ch buffer))))
             ((:decimal-entity)
              (case ch
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                 (vector-push-extend ch buffer))
                ((#\; #\space)        ; space is for buggy entities...
                 (let ((ch  (ignore-errors (code-char (parse-integer buffer)))))
                   (if ch
                       (princ ch)
                       (format t "&~A;" buffer)))
                 (setf state :normal))
                (otherwise
                 ;; broken
                 (format t "&#~A~C" buffer ch)
                 (setf state :normal))))
             ((:hexadecimal-entity)
              (case ch
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                      #\a #\b #\c #\d #\e #\f
                      #\A #\B #\C #\D #\E #\F)
                 (vector-push-extend ch buffer))
                ((#\; #\space)        ; space is for buggy entities...
                 (let ((ch (ignore-errors (code-char (parse-integer buffer :radix 16.)))))
                   (if ch
                       (princ ch)
                       (format t "&x~A;" buffer)))
                 (setf state :normal))
                (otherwise
                 ;; broken
                 (format t "&#x~A~C" buffer ch)
                 (setf state :normal)))))
       :finally (case state
                  ((:normal))
                  (otherwise (princ buffer)))))

  ;; (let ((chunks (list string)))
  ;;   (DOLIST (SUBSTITUTION *ENTITIES*
  ;;            (apply (function concatenate) 'string chunks))
  ;;     (when (some (lambda (chunk) (search (car substitution) chunk
  ;;                                         :test (function string=))) chunks)
  ;;       (setf chunks
  ;;             (mapcan
  ;;              (lambda (chunk) 
  ;;                (do* ((start 0 (+ pos (length (car substitution))))
  ;;                      (pos (search (car substitution) chunk 
  ;;                                   :start2 start
  ;;                                   :test (function string=))
  ;;                           (search (car substitution) chunk 
  ;;                                   :start2 start
  ;;                                   :test (function string=)))
  ;;                      (result '()))
  ;;                     ((null pos)
  ;;                      (cond
  ;;                        ((= 0 start) 
  ;;                         (push chunk result))
  ;;                        ((< start (length chunk)) 
  ;;                         (push (displaced-vector chunk start) result)))
  ;;                      (nreverse result))
  ;;                  (when (< start pos)
  ;;                    (push (displaced-vector chunk start pos) result))
  ;;                  (push (cdr substitution) result)))
  ;;              chunks)))))
  )

                                

(defmacro defentity (name code &optional documentation)
  `(progn
     (setf (gethash (string ',name) *entities*) (code-char ,code))
     (defconstant ,name (code-char ,code) ,documentation)))


;; Not strictly ISO-8879, but it's convenient here...
(defentity |amp|         38  "ampersand")
(defentity |gt|          62  "greater than")
(defentity |lt|          60  "less than")


;; The meat:
(defentity |nbsp|       160  "no-break space")
(defentity |iexcl|      161  "inverted exclamation mark")
(defentity |cent|       162  "cent sign")
(defentity |pound|      163  "pound sterling sign")
(defentity |curren|     164  "general currency sign")
(defentity |yen|        165  "yen sign")
(defentity |brvbar|     166  "broken (vertical) bar")
(defentity |sect|       167  "section sign")
(defentity |uml|        168  "umlaut (dieresis)")
(defentity |copy|       169  "copyright sign")
(defentity |ordf|       170  "ordinal indicator, feminine")
(defentity |laquo|      171  "angle quotation mark, left")
(defentity |not|        172  "not sign")
(defentity |shy|        173  "soft hyphen")
(defentity |reg|        174  "registered sign")
(defentity |macr|       175  "macron")
(defentity |deg|        176  "degree sign")
(defentity |plusmn|     177  "plus-or-minus sign")
(defentity |sup2|       178  "superscript two")
(defentity |sup3|       179  "superscript three")
(defentity |acute|      180  "acute accent")
(defentity |micro|      181  "micro sign")
(defentity |para|       182  "pilcrow (paragraph sign)")
(defentity |middot|     183  "middle dot")
(defentity |cedil|      184  "cedilla")
(defentity |sup1|       185  "superscript one")
(defentity |ordm|       186  "ordinal indicator, masculine")
(defentity |raquo|      187  "angle quotation mark, right")
(defentity |frac14|     188  "fraction one-quarter")
(defentity |frac12|     189  "fraction one-half")
(defentity |frac34|     190  "fraction three-quarters")
(defentity |iquest|     191  "inverted question mark")
(defentity |Agrave|     192  "capital A, grave accent")
(defentity |Aacute|     193  "capital A, acute accent")
(defentity |Acirc|      194  "capital A, circumflex accent")
(defentity |Atilde|     195  "capital A, tilde")
(defentity |Auml|       196  "capital A, dieresis or umlaut mark")
(defentity |Aring|      197  "capital A, ring")
(defentity |AElig|      198  "capital AE diphthong (ligature)")
(defentity |Ccedil|     199  "capital C, cedilla")
(defentity |Egrave|     200  "capital E, grave accent")
(defentity |Eacute|     201  "capital E, acute accent")
(defentity |Ecirc|      202  "capital E, circumflex accent")
(defentity |Euml|       203  "capital E, dieresis or umlaut mark")
(defentity |Igrave|     204  "capital I, grave accent")
(defentity |Iacute|     205  "capital I, acute accent")
(defentity |Icirc|      206  "capital I, circumflex accent")
(defentity |Iuml|       207  "capital I, dieresis or umlaut mark")
(defentity |ETH|        208  "capital Eth, Icelandic")
(defentity |Ntilde|     209  "capital N, tilde")
(defentity |Ograve|     210  "capital O, grave accent")
(defentity |Oacute|     211  "capital O, acute accent")
(defentity |Ocirc|      212  "capital O, circumflex accent")
(defentity |Otilde|     213  "capital O, tilde")
(defentity |Ouml|       214  "capital O, dieresis or umlaut mark")
(defentity |times|      215  "multiply sign")
(defentity |Oslash|     216  "capital O, slash")
(defentity |Ugrave|     217  "capital U, grave accent")
(defentity |Uacute|     218  "capital U, acute accent")
(defentity |Ucirc|      219  "capital U, circumflex accent")
(defentity |Uuml|       220  "capital U, dieresis or umlaut mark")
(defentity |Yacute|     221  "capital Y, acute accent")
(defentity |THORN|      222  "capital THORN, Icelandic")
(defentity |szlig|      223  "small sharp s, German (sz ligature)")
(defentity |agrave|     224  "small a, grave accent")
(defentity |aacute|     225  "small a, acute accent")
(defentity |acirc|      226  "small a, circumflex accent")
(defentity |atilde|     227  "small a, tilde")
(defentity |auml|       228  "small a, dieresis or umlaut mark")
(defentity |aring|      229  "small a, ring")
(defentity |aelig|      230  "small ae diphthong (ligature)")
(defentity |ccedil|     231  "small c, cedilla")
(defentity |egrave|     232  "small e, grave accent")
(defentity |eacute|     233  "small e, acute accent")
(defentity |ecirc|      234  "small e, circumflex accent")
(defentity |euml|       235  "small e, dieresis or umlaut mark")
(defentity |igrave|     236  "small i, grave accent")
(defentity |iacute|     237  "small i, acute accent")
(defentity |icirc|      238  "small i, circumflex accent")
(defentity |iuml|       239  "small i, dieresis or umlaut mark")
(defentity |eth|        240  "small eth, Icelandic")
(defentity |ntilde|     241  "small n, tilde")
(defentity |ograve|     242  "small o, grave accent")
(defentity |oacute|     243  "small o, acute accent")
(defentity |ocirc|      244  "small o, circumflex accent")
(defentity |otilde|     245  "small o, tilde")
(defentity |ouml|       246  "small o, dieresis or umlaut mark")
(defentity |divide|     247  "divide sign")
(defentity |oslash|     248  "small o, slash")
(defentity |ugrave|     249  "small u, grave accent")
(defentity |uacute|     250  "small u, acute accent")
(defentity |ucirc|      251  "small u, circumflex accent")
(defentity |uuml|       252  "small u, dieresis or umlaut mark")
(defentity |yacute|     253  "small y, acute accent")
(defentity |thorn|      254  "small thorn, Icelandic")
(defentity |yuml|       255  "small y, dieresis or umlaut mark")


;;;; THE END ;;;;
