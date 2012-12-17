;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               iso639a.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports functions and data to process
;;;;    iso639a language codes.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-09-10 <PJB> Created.
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ISO639A"
  (:use "COMMON-LISP")
  (:export "GET-LANGUAGES")
  (:documentation
   "

This package exports functions and data to process iso639a language codes.


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
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ISO639A")



;; http://www.loc.gov/standards/iso639-2/langcodes.html

(defvar +languages+
  '(
    ( aymara          ay      amerindian )
    ( guarani         gn      amerindian )
    ( quechua         qu      amerindian )
    ( bhutani         dz      asian )
    ( burmese         my      asian )
    ( cambodian       km      asian )
    ( chinese         zh      asian )
    ( japanese        ja      asian )
    ( korean          ko      asian )
    ( laothian        lo      asian )
    ( thai            th      asian )
    ( tibetan         bo      asian )
    ( vietnamese      vi      asian )
    ( latvian         lv      baltic )
    ( lithuanian      lt      baltic )
    ( basque          eu      basque )
    ( breton          br      celtic )
    ( irish           ga      celtic )
    ( scots-gaelic    gd      celtic )
    ( welsh           cy      celtic )
    ( kannada         kn      dravidian )
    ( malayalam       ml      dravidian )
    ( tamil           ta      dravidian )
    ( telugu          te      dravidian )
    ( greenlandic     kl      eskimo )
    ( inupiak         ik      eskimo )
    ( estonian        et      finno-ugric )
    ( finnish         fi      finno-ugric )
    ( hungarian       hu      finno-ugric )
    ( afrikaans       af      germanic )
    ( danish          da      germanic )
    ( dutch           nl      germanic )
    ( english         en      germanic )
    ( faroese         fo      germanic )
    ( frisian         fy      germanic )
    ( german          de      germanic )
    ( icelandic       is      germanic )
    ( norwegian       no      germanic )
    ( swedish         sv      germanic )
    ( yiddish         yi      germanic )
    ( afan            om      hamitic )
    ( afar            aa      hamitic )
    ( somali          so      hamitic )
    ( abkhazian       ab      ibero-caucasian )
    ( georgian        ka      ibero-caucasian )
    ( assamese        as      indian )
    ( bengali         bn      indian )
    ( bihari          bh      indian )
    ( gujarati        gu      indian )
    ( hindi           hi      indian )
    ( kashmiri        ks      indian )
    ( marathi         mr      indian )
    ( nepali          ne      indian )
    ( oriya           or      indian )
    ( punjabi         pa      indian )
    ( sanskrit        sa      indian )
    ( sindhi          sd      indian )
    ( singhalese      si      indian )
    ( urdu            ur      indian )
    ( albanian        sq      indo-european/other)
    ( armenian        hy      indo-european/other)
    ( esperanto       eo      international )
    ( interlingua     ia      international )
    ( interlingue     ie      international )
    ( volapuk         vo      international )
    ( kurdish         ku      iranian )
    ( pashto          ps      iranian )
    ( persian         fa      iranian )
    ( tajik           tg      iranian )
    ( greek           el      latin/greek )
    ( latin           la      latin/greek )
    ( hausa           ha      negro-african )
    ( kinyarwanda     rw      negro-african )
    ( kurundi         rn      negro-african )
    ( lingala         ln      negro-african )
    ( sangho          sg      negro-african )
    ( sesotho         st      negro-african )
    ( setswana        tn      negro-african )
    ( shona           sn      negro-african )
    ( siswati         ss      negro-african )
    ( swahili         sw      negro-african )
    ( tsonga          ts      negro-african )
    ( twi             tw      negro-african )
    ( wolof           wo      negro-african )
    ( xhosa           xh      negro-african )
    ( yoruba          yo      negro-african )
    ( zulu            zu      negro-african )
    ( fiji            fj      oceanic/indonesian )
    ( indonesian      id      oceanic/indonesian )
    ( javanese        jv      oceanic/indonesian )
    ( malagasy        mg      oceanic/indonesian )
    ( malay           ms      oceanic/indonesian )
    ( maori           mi      oceanic/indonesian )
    ( samoan          sm      oceanic/indonesian )
    ( sundanese       su      oceanic/indonesian )
    ( tagalog         tl      oceanic/indonesian )
    ( tonga           to      oceanic/indonesian )
    ( catalan         ca      romance )
    ( corsican        co      romance )
    ( french          fr      romance )
    ( galician        gl      romance )
    ( italian         it      romance )
    ( moldavian       mo      romance )
    ( occitan         oc      romance )
    ( portuguese      pt      romance )
    ( rhaeto-romance  rm      romance )
    ( romanian        ro      romance )
    ( spanish         es      romance )
    ( amharic         am      semitic )
    ( arabic          ar      semitic )
    ( hebrew          he      semitic )
    ( maltese         mt      semitic )
    ( tigrinya        ti      semitic )
    ( bulgarian       bg      slavic )
    ( byelorussian    be      slavic )
    ( croatian        hr      slavic )
    ( czech           cs      slavic )
    ( macedonian      mk      slavic )
    ( polish          pl      slavic )
    ( russian         ru      slavic )
    ( serbian         sr      slavic )
    ( serbo-croatian  sh      slavic )
    ( slovak          sk      slavic )
    ( slovenian       sl      slavic )
    ( ukrainian       uk      slavic )
    ( azerbaijani     az      turkic/altaic )
    ( bashkir         ba      turkic/altaic )
    ( kazakh          kk      turkic/altaic )
    ( kirghiz         ky      turkic/altaic )
    ( tatar           tt      turkic/altaic )
    ( turkish         tr      turkic/altaic )
    ( turkmen         tk      turkic/altaic )
    ( uzbek           uz      turkic/altaic )
    ( bislama         bi      miscellaneous )
    ( mongolian       mn      miscellaneous )
    ( nauru           na      miscellaneous )
    )
  "A list of language records: ( name code family )."
  ) ;;+LANGUAGES+


(defun get-field (record order-code)
  (case order-code
    (:name     (first  record))
    (:code     (second record))
    (:family   (third  record))
    (otherwise  "")))


(defun split-groups (list cut-indicator)
  "
RETURN:        A list of sublists of LIST, split where the
               CUT-INDICATOR function indicated.
LIST:          A list.
CUT-INDICATOR: A function of two successive elements of the list,
               indicating whether the list must be split between the
               two elements.
"
  (do* ((groups '())
        (group  '())
        (list    list        (cdr list))
        (current (car list)  (car list))
        (next    (cadr list) (cadr list)))
       ((null list)
        (progn (when group (push (nreverse group) groups)) (nreverse groups)))
    (push current group)
    (if next
        (when (funcall cut-indicator current next)
          (push (nreverse group) groups)
          (setq group nil)))))


(defun make-compare (order)
  (lambda (r1 r2)
    (do ((order-list order (cdr order-list))
         (cmp 0))
        ((or (/= 0 cmp) (null order-list))   (<= cmp 0))
      (let ((f1 (get-field r1 (car order-list)))
            (f2 (get-field r2 (car order-list))))
        (setq cmp (cond ((string< f1 f2) -1)
                        ((string> f1 f2)  1)
                        (t                0)))))))



(defun get-languages (&key (group-per-family nil) (order nil))
  "
RETURN:  If group-per-family is true,
         then a list of ( family (name code)* )
         else a list of ( name code family ).
         In both cases, the list(s) of languages are ordered as indicated
         by the order list, which may contain any combination of:
         :NAME :CODE :FAMILY.
"
  (when group-per-family
    (setq order (cons :family (remove :family order))))
  (let ((languages  (sort (copy-seq +languages+) (make-compare order))))
    (if group-per-family
        (mapcar
         (lambda (group)
           (cons (third (car group))
                 (mapcar (lambda (item) (list (first item) (second item)))
                         group)))
         (split-groups languages
                       (lambda (curr next) (string/= (third curr) (third next)))))
        languages)))


(defun ncapitalize (tree)
  "
DO:    Replace in place in TREE all occurence of a string or a symbol
       of length>2 by a string-capitalize'd copy.
"
  (do ((items tree (cdr items)))
      ((null items) tree)
    (setf (car items)
          (cond
            ((listp (car items))
             (ncapitalize (car items)))
            ((< 2 (length (string (car items))))
             (string-capitalize (car items)))
            (t
             (string (car items)))))))


;;;; THE END ;;;;
