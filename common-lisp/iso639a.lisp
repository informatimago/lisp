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
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ISO639A"
  (:USE "COMMON-LISP")
  (:EXPORT "SPLIT-GROUPS" "NCAPITALIZE" "GET-LANGUAGES")
  (:DOCUMENTATION
   "This package exports functions and data to process iso639a language codes.
    
    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ISO639A")



;; http://www.loc.gov/standards/iso639-2/langcodes.html

(defvar +LANGUAGES+
  '(
    ( AYMARA          AY      AMERINDIAN )
    ( GUARANI         GN      AMERINDIAN )
    ( QUECHUA         QU      AMERINDIAN )
    ( BHUTANI         DZ      ASIAN )
    ( BURMESE         MY      ASIAN )
    ( CAMBODIAN       KM      ASIAN )
    ( CHINESE         ZH      ASIAN )
    ( JAPANESE        JA      ASIAN )
    ( KOREAN          KO      ASIAN )
    ( LAOTHIAN        LO      ASIAN )
    ( THAI            TH      ASIAN )
    ( TIBETAN         BO      ASIAN )
    ( VIETNAMESE      VI      ASIAN )
    ( LATVIAN         LV      BALTIC )
    ( LITHUANIAN      LT      BALTIC )
    ( BASQUE          EU      BASQUE )
    ( BRETON          BR      CELTIC )
    ( IRISH           GA      CELTIC )
    ( SCOTS-GAELIC    GD      CELTIC )
    ( WELSH           CY      CELTIC )
    ( KANNADA         KN      DRAVIDIAN )
    ( MALAYALAM       ML      DRAVIDIAN )
    ( TAMIL           TA      DRAVIDIAN )
    ( TELUGU          TE      DRAVIDIAN )
    ( GREENLANDIC     KL      ESKIMO )
    ( INUPIAK         IK      ESKIMO )
    ( ESTONIAN        ET      FINNO-UGRIC )
    ( FINNISH         FI      FINNO-UGRIC )
    ( HUNGARIAN       HU      FINNO-UGRIC )
    ( AFRIKAANS       AF      GERMANIC )
    ( DANISH          DA      GERMANIC )
    ( DUTCH           NL      GERMANIC )
    ( ENGLISH         EN      GERMANIC )
    ( FAROESE         FO      GERMANIC )
    ( FRISIAN         FY      GERMANIC )
    ( GERMAN          DE      GERMANIC )
    ( ICELANDIC       IS      GERMANIC )
    ( NORWEGIAN       NO      GERMANIC )
    ( SWEDISH         SV      GERMANIC )
    ( YIDDISH         YI      GERMANIC )
    ( AFAN            OM      HAMITIC )
    ( AFAR            AA      HAMITIC )
    ( SOMALI          SO      HAMITIC )
    ( ABKHAZIAN       AB      IBERO-CAUCASIAN )
    ( GEORGIAN        KA      IBERO-CAUCASIAN )
    ( ASSAMESE        AS      INDIAN )
    ( BENGALI         BN      INDIAN )
    ( BIHARI          BH      INDIAN )
    ( GUJARATI        GU      INDIAN )
    ( HINDI           HI      INDIAN )
    ( KASHMIRI        KS      INDIAN )
    ( MARATHI         MR      INDIAN )
    ( NEPALI          NE      INDIAN )
    ( ORIYA           OR      INDIAN )
    ( PUNJABI         PA      INDIAN )
    ( SANSKRIT        SA      INDIAN )
    ( SINDHI          SD      INDIAN )
    ( SINGHALESE      SI      INDIAN )
    ( URDU            UR      INDIAN )
    ( ALBANIAN        SQ      INDO-EUROPEAN/OTHER)
    ( ARMENIAN        HY      INDO-EUROPEAN/OTHER)
    ( ESPERANTO       EO      INTERNATIONAL )
    ( INTERLINGUA     IA      INTERNATIONAL )
    ( INTERLINGUE     IE      INTERNATIONAL )
    ( VOLAPUK         VO      INTERNATIONAL )
    ( KURDISH         KU      IRANIAN )
    ( PASHTO          PS      IRANIAN )
    ( PERSIAN         FA      IRANIAN )
    ( TAJIK           TG      IRANIAN )
    ( GREEK           EL      LATIN/GREEK )
    ( LATIN           LA      LATIN/GREEK )
    ( HAUSA           HA      NEGRO-AFRICAN )
    ( KINYARWANDA     RW      NEGRO-AFRICAN )
    ( KURUNDI         RN      NEGRO-AFRICAN )
    ( LINGALA         LN      NEGRO-AFRICAN )
    ( SANGHO          SG      NEGRO-AFRICAN )
    ( SESOTHO         ST      NEGRO-AFRICAN )
    ( SETSWANA        TN      NEGRO-AFRICAN )
    ( SHONA           SN      NEGRO-AFRICAN )
    ( SISWATI         SS      NEGRO-AFRICAN )
    ( SWAHILI         SW      NEGRO-AFRICAN )
    ( TSONGA          TS      NEGRO-AFRICAN )
    ( TWI             TW      NEGRO-AFRICAN )
    ( WOLOF           WO      NEGRO-AFRICAN )
    ( XHOSA           XH      NEGRO-AFRICAN )
    ( YORUBA          YO      NEGRO-AFRICAN )
    ( ZULU            ZU      NEGRO-AFRICAN )
    ( FIJI            FJ      OCEANIC/INDONESIAN )
    ( INDONESIAN      ID      OCEANIC/INDONESIAN )
    ( JAVANESE        JV      OCEANIC/INDONESIAN )
    ( MALAGASY        MG      OCEANIC/INDONESIAN )
    ( MALAY           MS      OCEANIC/INDONESIAN )
    ( MAORI           MI      OCEANIC/INDONESIAN )
    ( SAMOAN          SM      OCEANIC/INDONESIAN )
    ( SUNDANESE       SU      OCEANIC/INDONESIAN )
    ( TAGALOG         TL      OCEANIC/INDONESIAN )
    ( TONGA           TO      OCEANIC/INDONESIAN )
    ( CATALAN         CA      ROMANCE )
    ( CORSICAN        CO      ROMANCE )
    ( FRENCH          FR      ROMANCE )
    ( GALICIAN        GL      ROMANCE )
    ( ITALIAN         IT      ROMANCE )
    ( MOLDAVIAN       MO      ROMANCE )
    ( OCCITAN         OC      ROMANCE )
    ( PORTUGUESE      PT      ROMANCE )
    ( RHAETO-ROMANCE  RM      ROMANCE )
    ( ROMANIAN        RO      ROMANCE )
    ( SPANISH         ES      ROMANCE )
    ( AMHARIC         AM      SEMITIC )
    ( ARABIC          AR      SEMITIC )
    ( HEBREW          HE      SEMITIC )
    ( MALTESE         MT      SEMITIC )
    ( TIGRINYA        TI      SEMITIC )
    ( BULGARIAN       BG      SLAVIC )
    ( BYELORUSSIAN    BE      SLAVIC )
    ( CROATIAN        HR      SLAVIC )
    ( CZECH           CS      SLAVIC )
    ( MACEDONIAN      MK      SLAVIC )
    ( POLISH          PL      SLAVIC )
    ( RUSSIAN         RU      SLAVIC )
    ( SERBIAN         SR      SLAVIC )
    ( SERBO-CROATIAN  SH      SLAVIC )
    ( SLOVAK          SK      SLAVIC )
    ( SLOVENIAN       SL      SLAVIC )
    ( UKRAINIAN       UK      SLAVIC )
    ( AZERBAIJANI     AZ      TURKIC/ALTAIC )
    ( BASHKIR         BA      TURKIC/ALTAIC )
    ( KAZAKH          KK      TURKIC/ALTAIC )
    ( KIRGHIZ         KY      TURKIC/ALTAIC )
    ( TATAR           TT      TURKIC/ALTAIC )
    ( TURKISH         TR      TURKIC/ALTAIC )
    ( TURKMEN         TK      TURKIC/ALTAIC )
    ( UZBEK           UZ      TURKIC/ALTAIC )
    ( BISLAMA         BI      MISCELLANEOUS )
    ( MONGOLIAN       MN      MISCELLANEOUS )
    ( NAURU           NA      MISCELLANEOUS )
    )
  "A list of language records: ( name code family )."
  ) ;;+LANGUAGES+


(DEFUN GET-FIELD (RECORD ORDER-CODE)
  (CASE ORDER-CODE
    (:NAME     (FIRST  RECORD))
    (:CODE     (SECOND RECORD))
    (:FAMILY   (THIRD  RECORD))
    (OTHERWISE  ""))
  ) ;;GET-FIELD


(DEFUN SPLIT-GROUPS ( LIST CUT-INDICATOR )
  (DO* ((GROUPS '())
        (GROUP  '())
        (LIST    LIST        (CDR LIST))
        (CURRENT (CAR LIST)  (CAR LIST))
        (NEXT    (CADR LIST) (CADR LIST)))
       ((NULL LIST)
        (PROGN (WHEN GROUP (PUSH (NREVERSE GROUP) GROUPS)) (NREVERSE GROUPS)))
    (PUSH CURRENT GROUP)
    (IF NEXT
        (WHEN (FUNCALL CUT-INDICATOR CURRENT NEXT)
          (PUSH (NREVERSE GROUP) GROUPS)
          (SETQ GROUP NIL))))
  ) ;;SPLIT-GROUPS


(DEFUN MAKE-COMPARE (ORDER)
  (LAMBDA (R1 R2)
    (DO ((ORDER-LIST ORDER (CDR ORDER-LIST))
         (CMP 0))
        ((OR (/= 0 CMP) (NULL ORDER-LIST))   (<= CMP 0))
      (LET ((F1 (GET-FIELD R1 (CAR ORDER-LIST)))
            (F2 (GET-FIELD R2 (CAR ORDER-LIST))))
        (SETQ CMP (COND ((STRING< F1 F2) -1)
                        ((STRING> F1 F2)  1)
                        (T                0)))))))



(DEFUN GET-LANGUAGES (&KEY (GROUP-PER-FAMILY NIL) (ORDER NIL))
  "
RETURN:  If group-per-family is true,
         then a list of ( family (name code)* )
         else a list of ( name code family ).
         In both cases, the list(s) of languages are ordered as indicated
         by the order list, which may contain any combination of:
         :NAME :CODE :FAMILY.
"
  (WHEN GROUP-PER-FAMILY
    (SETQ ORDER (CONS :FAMILY (REMOVE :FAMILY ORDER))))
  (LET ((LANGUAGES  (SORT (COPY-SEQ +LANGUAGES+) (MAKE-COMPARE ORDER))))
    (IF GROUP-PER-FAMILY
        (MAPCAR
         (LAMBDA (GROUP)
           (CONS (THIRD (CAR GROUP))
                 (MAPCAR (LAMBDA (ITEM) (LIST (FIRST ITEM) (SECOND ITEM)))
                         GROUP)))
         (SPLIT-GROUPS LANGUAGES
                       (LAMBDA (CURR NEXT) (STRING/= (THIRD CURR) (THIRD NEXT)))))
        LANGUAGES)
    )) ;;GET-LANGUAGES


(DEFUN NCAPITALIZE (TREE)
  "
DO:    Replace in place in TREE all occurence of a string or a symbol
       of length>2 by a string-capitalize'd copy.
"
  (DO ((ITEMS TREE (CDR ITEMS)))
      ((NULL ITEMS) TREE)
    (SETF (CAR ITEMS)
          (COND
            ((LISTP (CAR ITEMS))
             (NCAPITALIZE (CAR ITEMS)))
            ((< 2 (LENGTH (STRING (CAR ITEMS))))
             (STRING-CAPITALIZE (CAR ITEMS)))
            (T
             (STRING (CAR ITEMS))))))
  ) ;;NCAPITALIZE


;;;; iso639a.lisp                     --                     --          ;;;;
