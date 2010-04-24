;;;; -*- coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:               iso3166.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports functions and data to process
;;;;    iso3166 country codes.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-10-14 <PJB> Created.
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

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ISO3166"
  (:USE "COMMON-LISP")
  (:EXPORT "GET-COUNTRIES")
  (:DOCUMENTATION
   "This package exports functions and data to process iso3166 country codes.
    
    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ISO3166")




(DEFMACRO C-CODE    (C) `(FIRST  ,C))
(DEFMACRO C-EXISTS  (C) `(SECOND ,C))
(DEFMACRO C-FR-NAME (C) `(THIRD  ,C))
(DEFMACRO C-EN-NAME (C) `(FOURTH ,C))


(defvar +COUNTRIES+ 
  '(
    (CD NIL
        "La République Démocratique Du Congo"
        "The Democratic Republic Of The Congo")
    (MK NIL
        "L'Ex-République Yougoslave De Macédoine"
        "The Former Yugoslav Republic Of Macedonia")
    (PS NIL
        "Territoire Palestinien Occupé"
        "Occupied Palestinian Territory")
    (TL NIL
        "Timor-Leste"
        "Timor-Leste")
    (AD T
        "Andorre"
        "Andorra")
    (AE T
        "Émirats Arabes Unis"
        "United Arab Emirates")
    (AF T
        "Afghanistan"
        "Afghanistan")
    (AG T
        "Antigua et Barbuda"
        "Antigua and Barbuda")
    (AI T
        "Anguilla"
        "Anguilla")
    (AL T
        "Albanie"
        "Albania")
    (AM T
        "Arménie"
        "Armenia")
    (AN T
        "Antilles Néerlandaises"
        "Netherlands Antilles")
    (AO T
        "Angola"
        "Angola")
    (AQ T
        "Antarctique"
        "Antarctica")
    (AR T
        "Argentine"
        "Argentina")
    (AS T
        "Samoa Américaines"
        "American Samoa")
    (AT T
        "Autriche"
        "Austria")
    (AU T
        "Australie"
        "Australia")
    (AW T
        "Aruba"
        "Aruba")
    (AZ T
        "Azerbaïdjan"
        "Azerbaijan")
    (BA T
        "Bosnie-Herzégovine"
        "Bosnia and Herzegovina")
    (BB T
        "Barbade"
        "Barbados")
    (BD T
        "Bangladesh"
        "Bangladesh")
    (BE T
        "Belgique"
        "Belgium")
    (BF T
        "Burkina Faso"
        "Burkina Faso")
    (BG T
        "Bulgarie"
        "Bulgaria")
    (BH T
        "Bahreïn"
        "Bahrain")
    (BI T
        "Burundi"
        "Burundi")
    (BJ T
        "Bénin"
        "Benin")
    (BM T
        "Bermudes"
        "Bermuda")
    (BN T
        "Brunéi Darussalam"
        "Brunei Darussalam")
    (BO T
        "Bolivie"
        "Bolivia")
    (BR T
        "Brésil"
        "Brazil")
    (BS T
        "Bahamas"
        "Bahamas")
    (BT T
        "Bhoutan"
        "Bhutan")
    (BU NIL
        "Burma"
        "Burma")
    (BV T
        "Île Bouvet"
        "Bouvet Island")
    (BW T
        "Botswana"
        "Botswana")
    (BY T
        "Bélarus"
        "Belarus")
    (BZ T
        "Belize"
        "Belize")
    (CA T
        "Canada"
        "Canada")
    (CC T
        "Îles Cocos (Keeling)"
        "Cocos (Keeling) Islands")
    (CF T
        "République Centrafricaine"
        "Central African Republic")
    (CG NIL
        "Congo"
        "Congo")
    (CH T
        "Suisse"
        "Switzerland")
    (CI T
        "Côte D'Ivoire"
        "Ivory Coast")
    (CK T
        "Îles Cook"
        "Cook Islands")
    (CL T
        "Chili"
        "Chile")
    (CM T
        "Cameroun"
        "Cameroon")
    (CN T
        "Chine"
        "China")
    (CO T
        "Colombie"
        "Colombia")
    (CR T
        "Costa Rica"
        "Costa Rica")
    (CS NIL
        "Serbie et Monténégro"
        "Serbia and Montenegro")
    (CU T
        "Cuba"
        "Cuba")
    (CV T
        "Cap-Vert"
        "Cape Verde")
    (CX T
        "Île Christmas"
        "Christmas Island")
    (CY T
        "Chypre"
        "Cyprus")
    (CZ T
        "République Tchèque"
        "Czech Republic")
    (DD NIL
        "République Démocratique d'Allemagne"
        "German Democratic Republic")
    (DE T
        "Allemagne"
        "Germany")
    (DJ T
        "Djibouti"
        "Djibouti")
    (DK T
        "Danemark"
        "Denmark")
    (DM T
        "Dominique"
        "Dominica")
    (DO T
        "République Dominicaine"
      "Dominican Republic")
    (DZ T
        "Algérie"
        "Algeria")
    (EC T
        "Équateur"
        "Ecuador")
    (EE T
        "Estonie"
        "Estonia")
    (EG T
        "Égypte"
        "Egypt")
    (EH T
        "Sahara Occidental"
        "Western Sahara")
    (ER T
        "Érythrée"
        "Eritrea")
    (ES T
        "Espagne"
        "Spain")
    (ET T
        "Éthiopie"
        "Ethiopia")
    (FI T
        "Finlande"
        "Finland")
    (FJ T
        "Fidji"
        "Fiji")
    (FK T
        "Îles Malouines"
        "Falkland Islands")
    (FM T
        "États Fédérés De Micronésie"
        "Federated States Of Micronesia")
    (FO T
        "Îles Féroé"
        "Faroe Islands")
    (FR T
        "France"
        "France")
    (FX T
        "France Métropolitaine"
        "Metropolitan France")
    (GA T
        "Gabon"
        "Gabon")
    (GB T
        "Royaume-Uni"
        "United Kingdom")
    (GD T
        "Grenade"
        "Grenada")
    (GE T
        "Géorgie"
        "Georgia")
    (GF T
        "Guyane Française"
        "French Guiana")
    (GH T
        "Ghana"
        "Ghana")
    (GI T
        "Gibraltar"
        "Gibraltar")
    (GL T
        "Groenland"
        "Greenland")
    (GM T
        "Gambie"
        "Gambia")
    (GN T
        "Guinée"
        "Guinea")
    (GP T
        "Guadeloupe"
        "Guadeloupe")
    (GQ T
        "Guinée Équatoriale"
        "Equatorial Guinea")
    (GR T
        "Grèce"
        "Greece")
    (GS T
        "Géorgie Du Sud et Les Îles Sandwich Du Sud"
        "South Georgia and the South Sandwich Islands")
    (GT T
        "Guatemala"
        "Guatemala")
    (GU T
        "Guam"
        "Guam")
    (GW T
        "Guinée-Bissau"
        "Guinea-Bissau")
    (GY T
        "Guyana"
        "Guyana")
    (HK T
        "Hong-Kong"
        "Hong Kong")
    (HM T
        "Île Mcdonald et îles Heard"
        "Heard Island and Mcdonald Islands")
    (HN T
        "Honduras"
        "Honduras")
    (HR T
        "Croatie"
        "Croatia")
    (HT T
        "Haïti"
        "Haiti")
    (HU T
        "Hongrie"
        "Hungary")
    (ID T
        "Indonésie"
        "Indonesia")
    (IE T
        "Irlande"
        "Ireland")
    (IL T
        "Israël"
        "Israel")
    (IN T
        "Inde"
        "India")
    (IO T
        "Territoire Britannique de l'Océan Indien"
        "British Indian Ocean Territory")
    (IQ T
        "Iraq"
        "Iraq")
    (IR T
        "République Islamique D' Iran"
        "Islamic Republic of Iran")
    (IS T
        "Islande"
        "Iceland")
    (IT T
        "Italie"
        "Italy")
    (JM T
        "Jamaïque"
        "Jamaica")
    (JO T
        "Jordanie"
        "Jordan")
    (JP T
        "Japon"
        "Japan")
    (KE T
        "Kenya"
        "Kenya")
    (KG T
        "Kirghizistan"
        "Kyrgyzstan")
    (KH T
        "Cambodge"
        "Cambodia")
    (KI T
        "Kiribati"
        "Kiribati")
    (KM T
        "Comores"
        "Comoros")
    (KN T
        "Saint-Kitts et Nevis"
        "Saint Kitts and Nevis")
    (KP T
        "République Populaire Démocratique De Corée"
        "Democratic People's Republic of Korea")
    (KR T
        "République De Corée"
        "Republic of Korea")
    (KW T
        "Koweït"
        "Kuwait")
    (KY T
        "Îles Caïmanes"
        "Cayman Islands")
    (KZ T
        "Kazakhstan"
        "Kazakhstan")
    (LA T
        "République Démocratique Populaire Lao"
        "Lao People's Democratic Republic")
    (LB T
        "Liban"
        "Lebanon")
    (LC T
        "Sainte-Lucie"
        "Saint Lucia")
    (LI T
        "Liechtenstein"
        "Liechtenstein")
    (LK T
        "Sri Lanka"
        "Sri Lanka")
    (LR T
        "Libéria"
        "Liberia")
    (LS T
        "Lesotho"
        "Lesotho")
    (LT T
        "Lituanie"
        "Lithuania")
    (LU T
        "Luxembourg"
        "Luxembourg")
    (LV T
        "Lettonie"
        "Latvia")
    (LY T
        "Jamahiriya Arabe Libyenne"
        "Libyan Arab Jamahiriya")
    (MA T
        "Maroc"
        "Morocco")
    (MC T
        "Monaco"
        "Monaco")
    (MD T
        "République De Moldova"
        "Republic of Moldova")
    (MG T
        "Madagascar"
        "Madagascar")
    (MH T
        "Îles Marshall"
        "Marshall Islands")
    (ML T
        "Mali"
        "Mali")
    (MM T
        "Myanmar"
        "Myanmar")
    (MN T
        "Mongolie"
        "Mongolia")
    (MO T
        "Macao"
        "Macao"
        "Macau")
    (MP T
        "Îles Mariannes Du Nord"
        "Northern Mariana Islands")
    (MQ T
        "Martinique"
        "Martinique")
    (MR T
        "Mauritanie"
        "Mauritania")
    (MS T
        "Montserrat"
        "Montserrat"
        "Monserrat")
    (MT T
        "Malte"
        "Malta")
    (MU T
        "Maurice"
        "Mauritius")
    (MV T
        "Maldives"
        "Maldives")
    (MW T
        "Malawi"
        "Malawi")
    (MX T
        "Mexique"
        "Mexico")
    (MY T
        "Malaisie"
        "Malaysia")
    (MZ T
        "Mozambique"
        "Mozambique")
    (NA T
        "Namibie"
        "Namibia"
        "Nambia")
    (NC T
        "Nouvelle-Calédonie"
        "New Caledonia")
    (NE T
        "Niger"
        "Niger")
    (NF T
        "Île Norfolk"
        "Norfolk Island")
    (NG T
        "Nigéria"
        "Nigeria")
    (NI T
        "Nicaragua"
        "Nicaragua")
    (NL T
        "Pays-Bas"
        "Netherlands")
    (NO T
        "Norvège"
        "Norway")
    (NP T
        "Népal"
        "Nepal")
    (NR T
        "Nauru"
        "Nauru")
    (NT NIL
        "Nil"
        "Nil"
        "Neutral Zone")
    (NU T
        "Niué"
        "Niue")
    (NZ T
        "Nouvelle-Zélande"
        "New Zealand")
    (OM T
        "Oman"
        "Oman")
    (PA T
        "Panama"
        "Panama")
    (PE T
        "Pérou"
        "Peru")
    (PF T
        "Polynésie Française"
        "French Polynesia")
    (PG T
        "Papouasie-Nouvelle-Guinée"
        "Papua New Guinea")
    (PH T
        "Philippines"
        "Philippines")
    (PK T
        "Pakistan"
        "Pakistan")
    (PL T
        "Pologne"
        "Poland")
    (PM T
        "Saint-Pierre et Miquelon"
        "Saint Pierre and Miquelon")
    (PN T
        "Pitcairn"
        "Pitcairn")
    (PR T
        "Porto Rico"
        "Puerto Rico")
    (PT T
        "Portugal"
        "Portugal")
    (PW T
        "Palaos"
        "Palau")
    (PY T
        "Paraguay"
        "Paraguay")
    (QA T
        "Qatar"
        "Qatar")
    (RE T
        "Réunion"
        "Reunion")
    (RO T
        "Roumanie"
        "Romania")
    (RU T
        "Fédération De Russie"
        "Russian Federation")
    (RW T
        "Rwanda"
        "Rwanda")
    (SA T
        "Arabie Saoudite"
        "Saudi Arabia")
    (SB T
        "Îles Salomon"
        "Solomon Islands")
    (SC T
        "Seychelles"
        "Seychelles")
    (SD T
        "Soudan"
        "Sudan")
    (SE T
        "Suède"
        "Sweden")
    (SG T
        "Singapour"
        "Singapore")
    (SH T
        "Sainte-Hélène"
        "Saint Helena")
    (SI T
        "Slovénie"
        "Slovenia")
    (SJ T
        "Svalbard et île Jan Mayen"
        "Svalbard and Jan Mayen Islands")
    (SK T
        "Slovaquie"
        "Slovakia")
    (SL T
        "Sierra Leone"
        "Sierra Leone")
    (SM T
        "Saint-Marin"
        "San Marino")
    (SN T
        "Sénégal"
        "Senegal")
    (SO T
        "Somalie"
        "Somalia")
    (SR T
        "Suriname"
        "Suriname")
    (ST T
        "Sao Tomé et Principe"
        "Sao Tome and Principe")
    (SU NIL
        "Union des Républiques Socialistes Soviétiques"
        "Union of Soviet Socialist Republics")
    (SV T
        "El Salvador"
        "El Salvador")
    (SY T
        "République Arabe Syrienne"
        "Syrian Arab Republic")
    (SZ T
        "Swaziland"
        "Swaziland")
    (TC T
        "Îles Turks et Caïques"
        "Turks and Caicos Islands")
    (TD T
        "Tchad"
        "Chad")
    (TF T
        "Terres Australes Françaises"
        "French Southern Territories")
    (TG T
        "Togo"
        "Togo")
    (TH T
        "Thaïlande"
        "Thailand")
    (TJ T
        "Tadjikistan"
        "Tajikistan")
    (TK T
        "Tokelau"
        "Tokelau")
    (TM T
        "Turkménistan"
        "Turkmenistan")
    (TN T
        "Tunisie"
        "Tunisia")
    (TO T
        "Tonga"
        "Tonga")
    (TP T
        "Timor Oriental"
        "East Timor")
    (TR T
        "Turquie"
        "Turkey")
    (TT T
        "Trinité et Tobago"
        "Trinidad and Tobago")
    (TV T
        "Tuvalu"
        "Tuvalu")
    (TW T
        "Province de Chine Taïwan"
        "Province of China Taiwan")
    (TZ T
        "République-Unie de Tanzanie"
        "United Republic of Tanzania")
    (UA T
        "Ukraine"
        "Ukraine")
    (UG T
        "Ouganda"
        "Uganda")
    (UM T
        "Îles Mineures Éloignées des États-Unis"
        "United States Minor Outlying Islands")
    (US T
        "États-Unis"
        "United States of America")
    (UY T
        "Uruguay"
        "Uruguay")
    (UZ T
        "Ouzbékistan"
        "Uzbekistan")
    (VA T
        "Saint-Siège (État de La Cité Du Vatican)"
        "Holy See (Vatican City State)")
    (VC T
        "Saint-Vincent et Les Grenadines"
        "Saint Vincent and The Grenadines")
    (VE T
        "Venezuela"
        "Venezuela")
    (VG T
        "Îles Vierges Britanniques"
        "British Virgin Islands")
    (VI T
        "Îles Vierges Des États-Unis"
        "United States Virgin Islands")
    (VN T
        "Viet Nam"
        "Viet Nam")
    (VU T
        "Vanuatu"
        "Vanuatu")
    (WF T
        "Wallis Et Futuna"
        "Wallis and Futuna Islands")
    (WS T
        "Samoa"
        "Samoa")
    (YD NIL
        "Yemen Démocratique"
        "Democratic Yemen")
    (YE T
        "Yémen"
        "Yemen")
    (YT T
        "Mayotte"
        "Mayotte")
    (YU T
        "Yougoslavie"
        "Yugoslavia")
    (ZA T
        "Afrique Du Sud"
        "South Africa")
    (ZM T
        "Zambie"
        "Zambia")
    (ZR T
        "Zaïre"
        "Zaire")
    (ZW T
        "Zimbabwe"
        "Zimbabwe")
    (ZZ T
        "Pays inconnu ou non spécifié"
        "Unknown or unspecified country")
    ));;+COUNTRIES+



(DEFUN GET-COUNTRIES (&KEY ONLY-EXISTING (LANGUAGE "EN") ORDER)
  "
only-existing:  T <=> filter out codes for countries that don't exist anymore.
language:       EN or FR
                (case insensitive string or symbol whose name is EN or FR)
order:          :NAME or :CODE; if not specified, no ordering is done.
RETURN:         A list of (code name).
"
  (LET* ((INCLUDE-ALL (NOT ONLY-EXISTING))
         (RESULT (MAPCAN (LAMBDA (C)
                           (WHEN (OR INCLUDE-ALL (C-EXISTS C))
                             (list (LIST (C-CODE C)
                                         (IF (STRING-EQUAL "EN" LANGUAGE)
                                           (C-EN-NAME C)
                                           (C-FR-NAME C))))))
                         +COUNTRIES+)))
    (WHEN ORDER
      (SETQ RESULT
            (SORT RESULT
                  (IF (EQ :CODE ORDER)
                    (LAMBDA (A B) (STRING-LESSP (FIRST  A) (FIRST  B)))
                    (LAMBDA (A B) (STRING-LESSP (SECOND A) (SECOND B)))))))
    RESULT)
  );;GET-COUNTRIES




;; (defconstant +countries+en+
;;   '(
;;     (AD  "ANDORRA")
;;     (AE  "UNITED ARAB EMIRATES")
;;     (AF  "AFGHANISTAN")
;;     (AG  "ANTIGUA AND BARBUDA")
;;     (AI  "ANGUILLA")
;;     (AL  "ALBANIA")
;;     (AM  "ARMENIA")
;;     (AN  "NETHERLANDS ANTILLES")
;;     (AO  "ANGOLA")
;;     (AQ  "ANTARCTICA")
;;     (AR  "ARGENTINA")
;;     (AS  "AMERICAN SAMOA")
;;     (AT  "AUSTRIA")
;;     (AU  "AUSTRALIA")
;;     (AW  "ARUBA")
;;     (AZ  "AZERBAIJAN")
;;     (BA  "BOSNIA AND HERZEGOVINA")
;;     (BB  "BARBADOS")
;;     (BD  "BANGLADESH")
;;     (BE  "BELGIUM")
;;     (BF  "BURKINA FASO")
;;     (BG  "BULGARIA")
;;     (BH  "BAHRAIN")
;;     (BI  "BURUNDI")
;;     (BJ  "BENIN")
;;     (BM  "BERMUDA")
;;     (BN  "BRUNEI DARUSSALAM")
;;     (BO  "BOLIVIA")
;;     (BR  "BRAZIL")
;;     (BS  "BAHAMAS")
;;     (BT  "BHUTAN")
;;     (BV  "BOUVET ISLAND")
;;     (BW  "BOTSWANA")
;;     (BY  "BELARUS")
;;     (BZ  "BELIZE")
;;     (CA  "CANADA")
;;     (CC  "COCOS (KEELING) ISLANDS")
;;     (CD  "CONGO, THE DEMOCRATIC REPUBLIC OF THE")
;;     (CF  "CENTRAL AFRICAN REPUBLIC")
;;     (CG  "CONGO")
;;     (CH  "SWITZERLAND")
;;     (CI  "COTE D'IVOIRE")
;;     (CK  "COOK ISLANDS")
;;     (CL  "CHILE")
;;     (CM  "CAMEROON")
;;     (CN  "CHINA")
;;     (CO  "COLOMBIA")
;;     (CR  "COSTA RICA")
;;     (CS  "SERBIA AND MONTENEGRO")
;;     (CU  "CUBA")
;;     (CV  "CAPE VERDE")
;;     (CX  "CHRISTMAS ISLAND")
;;     (CY  "CYPRUS")
;;     (CZ  "CZECH REPUBLIC")
;;     (DE  "GERMANY")
;;     (DJ  "DJIBOUTI")
;;     (DK  "DENMARK")
;;     (DM  "DOMINICA")
;;     (DO  "DOMINICAN REPUBLIC")
;;     (DZ  "ALGERIA")
;;     (EC  "ECUADOR")
;;     (EE  "ESTONIA")
;;     (EG  "EGYPT")
;;     (EH  "WESTERN SAHARA")
;;     (ER  "ERITREA")
;;     (ES  "SPAIN")
;;     (ET  "ETHIOPIA")
;;     (FI  "FINLAND")
;;     (FJ  "FIJI")
;;     (FK  "FALKLAND ISLANDS (MALVINAS)")
;;     (FM  "MICRONESIA, FEDERATED STATES OF")
;;     (FO  "FAROE ISLANDS")
;;     (FR  "FRANCE")
;;     (GA  "GABON")
;;     (GB  "UNITED KINGDOM")
;;     (GD  "GRENADA")
;;     (GE  "GEORGIA")
;;     (GF  "FRENCH GUIANA")
;;     (GH  "GHANA")
;;     (GI  "GIBRALTAR")
;;     (GL  "GREENLAND")
;;     (GM  "GAMBIA")
;;     (GN  "GUINEA")
;;     (GP  "GUADELOUPE")
;;     (GQ  "EQUATORIAL GUINEA")
;;     (GR  "GREECE")
;;     (GS  "SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS")
;;     (GT  "GUATEMALA")
;;     (GU  "GUAM")
;;     (GW  "GUINEA-BISSAU")
;;     (GY  "GUYANA")
;;     (HK  "HONG KONG")
;;     (HM  "HEARD ISLAND AND MCDONALD ISLANDS")
;;     (HN  "HONDURAS")
;;     (HR  "CROATIA")
;;     (HT  "HAITI")
;;     (HU  "HUNGARY")
;;     (ID  "INDONESIA")
;;     (IE  "IRELAND")
;;     (IL  "ISRAEL")
;;     (IN  "INDIA")
;;     (IO  "BRITISH INDIAN OCEAN TERRITORY")
;;     (IQ  "IRAQ")
;;     (IR  "IRAN, ISLAMIC REPUBLIC OF")
;;     (IS  "ICELAND")
;;     (IT  "ITALY")
;;     (JM  "JAMAICA")
;;     (JO  "JORDAN")
;;     (JP  "JAPAN")
;;     (KE  "KENYA")
;;     (KG  "KYRGYZSTAN")
;;     (KH  "CAMBODIA")
;;     (KI  "KIRIBATI")
;;     (KM  "COMOROS")
;;     (KN  "SAINT KITTS AND NEVIS")
;;     (KP  "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF")
;;     (KR  "KOREA, REPUBLIC OF")
;;     (KW  "KUWAIT")
;;     (KY  "CAYMAN ISLANDS")
;;     (KZ  "KAZAKHSTAN")
;;     (LA  "LAO PEOPLE'S DEMOCRATIC REPUBLIC")
;;     (LB  "LEBANON")
;;     (LC  "SAINT LUCIA")
;;     (LI  "LIECHTENSTEIN")
;;     (LK  "SRI LANKA")
;;     (LR  "LIBERIA")
;;     (LS  "LESOTHO")
;;     (LT  "LITHUANIA")
;;     (LU  "LUXEMBOURG")
;;     (LV  "LATVIA")
;;     (LY  "LIBYAN ARAB JAMAHIRIYA")
;;     (MA  "MOROCCO")
;;     (MC  "MONACO")
;;     (MD  "MOLDOVA, REPUBLIC OF")
;;     (MG  "MADAGASCAR")
;;     (MH  "MARSHALL ISLANDS")
;;     (MK  "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF")
;;     (ML  "MALI")
;;     (MM  "MYANMAR")
;;     (MN  "MONGOLIA")
;;     (MO  "MACAO")
;;     (MP  "NORTHERN MARIANA ISLANDS")
;;     (MQ  "MARTINIQUE")
;;     (MR  "MAURITANIA")
;;     (MS  "MONTSERRAT")
;;     (MT  "MALTA")
;;     (MU  "MAURITIUS")
;;     (MV  "MALDIVES")
;;     (MW  "MALAWI")
;;     (MX  "MEXICO")
;;     (MY  "MALAYSIA")
;;     (MZ  "MOZAMBIQUE")
;;     (NA  "NAMIBIA")
;;     (NC  "NEW CALEDONIA")
;;     (NE  "NIGER")
;;     (NF  "NORFOLK ISLAND")
;;     (NG  "NIGERIA")
;;     (NI  "NICARAGUA")
;;     (NL  "NETHERLANDS")
;;     (NO  "NORWAY")
;;     (NP  "NEPAL")
;;     (NR  "NAURU")
;;     (NU  "NIUE")
;;     (NZ  "NEW ZEALAND")
;;     (OM  "OMAN")
;;     (PA  "PANAMA")
;;     (PE  "PERU")
;;     (PF  "FRENCH POLYNESIA")
;;     (PG  "PAPUA NEW GUINEA")
;;     (PH  "PHILIPPINES")
;;     (PK  "PAKISTAN")
;;     (PL  "POLAND")
;;     (PM  "SAINT PIERRE AND MIQUELON")
;;     (PN  "PITCAIRN")
;;     (PR  "PUERTO RICO")
;;     (PS  "PALESTINIAN TERRITORY, OCCUPIED")
;;     (PT  "PORTUGAL")
;;     (PW  "PALAU")
;;     (PY  "PARAGUAY")
;;     (QA  "QATAR")
;;     (RE  "REUNION")
;;     (RO  "ROMANIA")
;;     (RU  "RUSSIAN FEDERATION")
;;     (RW  "RWANDA")
;;     (SA  "SAUDI ARABIA")
;;     (SB  "SOLOMON ISLANDS")
;;     (SC  "SEYCHELLES")
;;     (SD  "SUDAN")
;;     (SE  "SWEDEN")
;;     (SG  "SINGAPORE")
;;     (SH  "SAINT HELENA")
;;     (SI  "SLOVENIA")
;;     (SJ  "SVALBARD AND JAN MAYEN")
;;     (SK  "SLOVAKIA")
;;     (SL  "SIERRA LEONE")
;;     (SM  "SAN MARINO")
;;     (SN  "SENEGAL")
;;     (SO  "SOMALIA")
;;     (SR  "SURINAME")
;;     (ST  "SAO TOME AND PRINCIPE")
;;     (SV  "EL SALVADOR")
;;     (SY  "SYRIAN ARAB REPUBLIC")
;;     (SZ  "SWAZILAND")
;;     (TC  "TURKS AND CAICOS ISLANDS")
;;     (TD  "CHAD")
;;     (TF  "FRENCH SOUTHERN TERRITORIES")
;;     (TG  "TOGO")
;;     (TH  "THAILAND")
;;     (TJ  "TAJIKISTAN")
;;     (TK  "TOKELAU")
;;     (TL  "TIMOR-LESTE")
;;     (TM  "TURKMENISTAN")
;;     (TN  "TUNISIA")
;;     (TO  "TONGA")
;;     (TR  "TURKEY")
;;     (TT  "TRINIDAD AND TOBAGO")
;;     (TV  "TUVALU")
;;     (TW  "TAIWAN, PROVINCE OF CHINA")
;;     (TZ  "TANZANIA, UNITED REPUBLIC OF")
;;     (UA  "UKRAINE")
;;     (UG  "UGANDA")
;;     (UM  "UNITED STATES MINOR OUTLYING ISLANDS")
;;     (US  "UNITED STATES")
;;     (UY  "URUGUAY")
;;     (UZ  "UZBEKISTAN")
;;     (VA  "HOLY SEE (VATICAN CITY STATE)")
;;     (VC  "SAINT VINCENT AND THE GRENADINES")
;;     (VE  "VENEZUELA")
;;     (VG  "VIRGIN ISLANDS, BRITISH")
;;     (VI  "VIRGIN ISLANDS, U.S.")
;;     (VN  "VIET NAM")
;;     (VU  "VANUATU")
;;     (WF  "WALLIS AND FUTUNA")
;;     (WS  "SAMOA")
;;     (YE  "YEMEN")
;;     (YT  "MAYOTTE")
;;     (ZA  "SOUTH AFRICA")
;;     (ZM  "ZAMBIA")
;;     (ZW  "ZIMBABWE")
;;     ));;+countries+en+
;; 
;; 
;; (defconstant +countries+fr+
;;   '(
;;     (AD  "ANDORRE")
;;     (AE  "ÉMIRATS ARABES UNIS")
;;     (AF  "AFGHANISTAN")
;;     (AG  "ANTIGUA-ET-BARBUDA")
;;     (AI  "ANGUILLA")
;;     (AL  "ALBANIE")
;;     (AM  "ARMÉNIE")
;;     (AN  "ANTILLES NÉERLANDAISES")
;;     (AO  "ANGOLA")
;;     (AQ  "ANTARCTIQUE")
;;     (AR  "ARGENTINE")
;;     (AS  "SAMOA AMÉRICAINES")
;;     (AT  "AUTRICHE")
;;     (AU  "AUSTRALIE")
;;     (AW  "ARUBA")
;;     (AZ  "AZERBAÏDJAN")
;;     (BA  "BOSNIE-HERZÉGOVINE")
;;     (BB  "BARBADE")
;;     (BD  "BANGLADESH")
;;     (BE  "BELGIQUE")
;;     (BF  "BURKINA FASO")
;;     (BG  "BULGARIE")
;;     (BH  "BAHREÏN")
;;     (BI  "BURUNDI")
;;     (BJ  "BÉNIN")
;;     (BM  "BERMUDES")
;;     (BN  "BRUNÉI DARUSSALAM")
;;     (BO  "BOLIVIE")
;;     (BR  "BRÉSIL")
;;     (BS  "BAHAMAS")
;;     (BT  "BHOUTAN")
;;     (BV  "BOUVET, ÎLE")
;;     (BW  "BOTSWANA")
;;     (BY  "BÉLARUS")
;;     (BZ  "BELIZE")
;;     (CA  "CANADA")
;;     (CC  "COCOS (KEELING), ÎLES")
;;     (CD  "CONGO, LA RÉPUBLIQUE DÉMOCRATIQUE DU")
;;     (CF  "CENTRAFRICAINE, RÉPUBLIQUE")
;;     (CG  "CONGO")
;;     (CH  "SUISSE")
;;     (CI  "CÔTE D'IVOIRE")
;;     (CK  "COOK, ÎLES")
;;     (CL  "CHILI")
;;     (CM  "CAMEROUN")
;;     (CN  "CHINE")
;;     (CO  "COLOMBIE")
;;     (CR  "COSTA RICA")
;;     (CS  "SERBIE-ET-MONTÉNÉGRO")
;;     (CU  "CUBA")
;;     (CV  "CAP-VERT")
;;     (CX  "CHRISTMAS, ÎLE")
;;     (CY  "CHYPRE")
;;     (CZ  "TCHÈQUE, RÉPUBLIQUE")
;;     (DE  "ALLEMAGNE")
;;     (DJ  "DJIBOUTI")
;;     (DK  "DANEMARK")
;;     (DM  "DOMINIQUE")
;;     (DO  "DOMINICAINE, RÉPUBLIQUE")
;;     (DZ  "ALGÉRIE")
;;     (EC  "ÉQUATEUR")
;;     (EE  "ESTONIE")
;;     (EG  "ÉGYPTE")
;;     (EH  "SAHARA OCCIDENTAL")
;;     (ER  "ÉRYTHRÉE")
;;     (ES  "ESPAGNE")
;;     (ET  "ÉTHIOPIE")
;;     (FI  "FINLANDE")
;;     (FJ  "FIDJI")
;;     (FK  "FALKLAND, ÎLES (MALVINAS)")
;;     (FM  "MICRONÉSIE, ÉTATS FÉDÉRÉS DE")
;;     (FO  "FÉROÉ, ÎLES")
;;     (FR  "FRANCE")
;;     (GA  "GABON")
;;     (GB  "ROYAUME-UNI")
;;     (GD  "GRENADE")
;;     (GE  "GÉORGIE")
;;     (GF  "GUYANE FRANÇAISE")
;;     (GH  "GHANA")
;;     (GI  "GIBRALTAR")
;;     (GL  "GROENLAND")
;;     (GM  "GAMBIE")
;;     (GN  "GUINÉE")
;;     (GP  "GUADELOUPE")
;;     (GQ  "GUINÉE ÉQUATORIALE")
;;     (GR  "GRÈCE")
;;     (GS  "GÉORGIE DU SUD ET LES ÎLES SANDWICH DU SUD")
;;     (GT  "GUATEMALA")
;;     (GU  "GUAM")
;;     (GW  "GUINÉE-BISSAU")
;;     (GY  "GUYANA")
;;     (HK  "HONG-KONG")
;;     (HM  "HEARD, ÎLE ET MCDONALD, ÎLES")
;;     (HN  "HONDURAS")
;;     (HR  "CROATIE")
;;     (HT  "HAÏTI")
;;     (HU  "HONGRIE")
;;     (ID  "INDONÉSIE")
;;     (IE  "IRLANDE")
;;     (IL  "ISRAËL")
;;     (IN  "INDE")
;;     (IO  "OCÉAN INDIEN, TERRITOIRE BRITANNIQUE DE L'")
;;     (IQ  "IRAQ")
;;     (IR  "IRAN, RÉPUBLIQUE ISLAMIQUE D'")
;;     (IS  "ISLANDE")
;;     (IT  "ITALIE")
;;     (JM  "JAMAÏQUE")
;;     (JO  "JORDANIE")
;;     (JP  "JAPON")
;;     (KE  "KENYA")
;;     (KG  "KIRGHIZISTAN")
;;     (KH  "CAMBODGE")
;;     (KI  "KIRIBATI")
;;     (KM  "COMORES")
;;     (KN  "SAINT-KITTS-ET-NEVIS")
;;     (KP  "CORÉE, RÉPUBLIQUE POPULAIRE DÉMOCRATIQUE DE")
;;     (KR  "CORÉE, RÉPUBLIQUE DE")
;;     (KW  "KOWEÏT")
;;     (KY  "CAÏMANES, ÎLES")
;;     (KZ  "KAZAKHSTAN")
;;     (LA  "LAO, RÉPUBLIQUE DÉMOCRATIQUE POPULAIRE")
;;     (LB  "LIBAN")
;;     (LC  "SAINTE-LUCIE")
;;     (LI  "LIECHTENSTEIN")
;;     (LK  "SRI LANKA")
;;     (LR  "LIBÉRIA")
;;     (LS  "LESOTHO")
;;     (LT  "LITUANIE")
;;     (LU  "LUXEMBOURG")
;;     (LV  "LETTONIE")
;;     (LY  "LIBYENNE, JAMAHIRIYA ARABE")
;;     (MA  "MAROC")
;;     (MC  "MONACO")
;;     (MD  "MOLDOVA, RÉPUBLIQUE DE")
;;     (MG  "MADAGASCAR")
;;     (MH  "MARSHALL, ÎLES")
;;     (MK  "MACÉDOINE, L'EX-RÉPUBLIQUE YOUGOSLAVE DE")
;;     (ML  "MALI")
;;     (MM  "MYANMAR")
;;     (MN  "MONGOLIE")
;;     (MO  "MACAO")
;;     (MP  "MARIANNES DU NORD, ÎLES")
;;     (MQ  "MARTINIQUE")
;;     (MR  "MAURITANIE")
;;     (MS  "MONTSERRAT")
;;     (MT  "MALTE")
;;     (MU  "MAURICE")
;;     (MV  "MALDIVES")
;;     (MW  "MALAWI")
;;     (MX  "MEXIQUE")
;;     (MY  "MALAISIE")
;;     (MZ  "MOZAMBIQUE")
;;     (NA  "NAMIBIE")
;;     (NC  "NOUVELLE-CALÉDONIE")
;;     (NE  "NIGER")
;;     (NF  "NORFOLK, ÎLE")
;;     (NG  "NIGÉRIA")
;;     (NI  "NICARAGUA")
;;     (NL  "PAYS-BAS")
;;     (NO  "NORVÈGE")
;;     (NP  "NÉPAL")
;;     (NR  "NAURU")
;;     (NU  "NIUÉ")
;;     (NZ  "NOUVELLE-ZÉLANDE")
;;     (OM  "OMAN")
;;     (PA  "PANAMA")
;;     (PE  "PÉROU")
;;     (PF  "POLYNÉSIE FRANÇAISE")
;;     (PG  "PAPOUASIE-NOUVELLE-GUINÉE")
;;     (PH  "PHILIPPINES")
;;     (PK  "PAKISTAN")
;;     (PL  "POLOGNE")
;;     (PM  "SAINT-PIERRE-ET-MIQUELON")
;;     (PN  "PITCAIRN")
;;     (PR  "PORTO RICO")
;;     (PS  "PALESTINIEN OCCUPÉ, TERRITOIRE")
;;     (PT  "PORTUGAL")
;;     (PW  "PALAOS")
;;     (PY  "PARAGUAY")
;;     (QA  "QATAR")
;;     (RE  "RÉUNION")
;;     (RO  "ROUMANIE")
;;     (RU  "RUSSIE, FÉDÉRATION DE")
;;     (RW  "RWANDA")
;;     (SA  "ARABIE SAOUDITE")
;;     (SB  "SALOMON, ÎLES")
;;     (SC  "SEYCHELLES")
;;     (SD  "SOUDAN")
;;     (SE  "SUÈDE")
;;     (SG  "SINGAPOUR")
;;     (SH  "SAINTE-HÉLÈNE")
;;     (SI  "SLOVÉNIE")
;;     (SJ  "SVALBARD ET ÎLE JAN MAYEN")
;;     (SK  "SLOVAQUIE")
;;     (SL  "SIERRA LEONE")
;;     (SM  "SAINT-MARIN")
;;     (SN  "SÉNÉGAL")
;;     (SO  "SOMALIE")
;;     (SR  "SURINAME")
;;     (ST  "SAO TOMÉ-ET-PRINCIPE")
;;     (SV  "EL SALVADOR")
;;     (SY  "SYRIENNE, RÉPUBLIQUE ARABE")
;;     (SZ  "SWAZILAND")
;;     (TC  "TURKS ET CAÏQUES, ÎLES")
;;     (TD  "TCHAD")
;;     (TF  "TERRES AUSTRALES FRANÇAISES")
;;     (TG  "TOGO")
;;     (TH  "THAÏLANDE")
;;     (TJ  "TADJIKISTAN")
;;     (TK  "TOKELAU")
;;     (TL  "TIMOR-LESTE")
;;     (TM  "TURKMÉNISTAN")
;;     (TN  "TUNISIE")
;;     (TO  "TONGA")
;;     (TR  "TURQUIE")
;;     (TT  "TRINITÉ-ET-TOBAGO")
;;     (TV  "TUVALU")
;;     (TW  "TAÏWAN, PROVINCE DE CHINE")
;;     (TZ  "TANZANIE, RÉPUBLIQUE-UNIE DE")
;;     (UA  "UKRAINE")
;;     (UG  "OUGANDA")
;;     (UM  "ÎLES MINEURES ÉLOIGNÉES DES ÉTATS-UNIS")
;;     (US  "ÉTATS-UNIS")
;;     (UY  "URUGUAY")
;;     (UZ  "OUZBÉKISTAN")
;;     (VA  "SAINT-SIÈGE (ÉTAT DE LA CITÉ DU VATICAN)")
;;     (VC  "SAINT-VINCENT-ET-LES GRENADINES")
;;     (VE  "VENEZUELA")
;;     (VG  "ÎLES VIERGES BRITANNIQUES")
;;     (VI  "ÎLES VIERGES DES ÉTATS-UNIS")
;;     (VN  "VIET NAM")
;;     (VU  "VANUATU")
;;     (WF  "WALLIS ET FUTUNA")
;;     (WS  "SAMOA")
;;     (YE  "YÉMEN")
;;     (YT  "MAYOTTE")
;;     (ZA  "AFRIQUE DU SUD")
;;     (ZM  "ZAMBIE")
;;     (ZW  "ZIMBABWE")
;;     ));;+countries+fr+
;; 
;; 
;; (defconstant +countries+
;;   ;;CC exist english-name
;;   '(
;;     (AD  T   "Andorra")
;;     (AE  T   "United Arab Emirates")
;;     (AF  T   "Afghanistan")
;;     (AG  T   "Antigua & Barbuda")
;;     (AI  T   "Anguilla")
;;     (AL  T   "Albania")
;;     (AM  T   "Armenia")
;;     (AN  T   "Netherlands Antilles")
;;     (AO  T   "Angola")
;;     (AQ  T   "Antarctica")
;;     (AR  T   "Argentina")
;;     (AS  T   "American Samoa")
;;     (AT  T   "Austria")
;;     (AU  T   "Australia")
;;     (AW  T   "Aruba")
;;     (AZ  T   "Azerbaijan")
;;     (BA  T   "Bosnia and Herzegovina")
;;     (BB  T   "Barbados")
;;     (BD  T   "Bangladesh")
;;     (BE  T   "Belgium")
;;     (BF  T   "Burkina Faso")
;;     (BG  T   "Bulgaria")
;;     (BH  T   "Bahrain")
;;     (BI  T   "Burundi")
;;     (BJ  T   "Benin")
;;     (BM  T   "Bermuda")
;;     (BN  T   "Brunei Darussalam")
;;     (BO  T   "Bolivia")
;;     (BR  T   "Brazil")
;;     (BS  T   "Bahama")
;;     (BT  T   "Bhutan")
;;     (BU  NIL "Burma ")
;;     (BV  T   "Bouvet Island")
;;     (BW  T   "Botswana")
;;     (BY  T   "Belarus")
;;     (BZ  T   "Belize")
;;     (CA  T   "Canada")
;;     (CC  T   "Cocos (Keeling) Islands")
;;     (CF  T   "Central African Republic")
;;     (CG  T   "Congo")
;;     (CH  T   "Switzerland")
;;     (CI  T   "Côte D'ivoire (Ivory Coast)")
;;     (CK  T   "Cook Iislands")
;;     (CL  T   "Chile")
;;     (CM  T   "Cameroon")
;;     (CN  T   "China")
;;     (CO  T   "Colombia")
;;     (CR  T   "Costa Rica")
;;     (CS  NIL "Czechoslovakia ")
;;     (CU  T   "Cuba")
;;     (CV  T   "Cape Verde")
;;     (CX  T   "Christmas Island")
;;     (CY  T   "Cyprus")
;;     (CZ  T   "Czech Republic")
;;     (DD  NIL "German Democratic Republic ")
;;     (DE  T   "Germany")
;;     (DJ  T   "Djibouti")
;;     (DK  T   "Denmark")
;;     (DM  T   "Dominica")
;;     (DO  T   "Dominican Republic")
;;     (DZ  T   "Algeria")
;;     (EC  T   "Ecuador")
;;     (EE  T   "Estonia")
;;     (EG  T   "Egypt")
;;     (EH  T   "Western Sahara")
;;     (ER  T   "Eritrea")
;;     (ES  T   "Spain")
;;     (ET  T   "Ethiopia")
;;     (FI  T   "Finland")
;;     (FJ  T   "Fiji")
;;     (FK  T   "Falkland Islands (Malvinas)")
;;     (FM  T   "Micronesia")
;;     (FO  T   "Faroe Islands")
;;     (FR  T   "France")
;;     (FX  T   "France, Metropolitan")
;;     (GA  T   "Gabon")
;;     (GB  T   "United Kingdom (Great Britain)")
;;     (GD  T   "Grenada")
;;     (GE  T   "Georgia")
;;     (GF  T   "French Guiana")
;;     (GH  T   "Ghana")
;;     (GI  T   "Gibraltar")
;;     (GL  T   "Greenland")
;;     (GM  T   "Gambia")
;;     (GN  T   "Guinea")
;;     (GP  T   "Guadeloupe")
;;     (GQ  T   "Equatorial Guinea")
;;     (GR  T   "Greece")
;;     (GS  T   "South Georgia and the South Sandwich Islands")
;;     (GT  T   "Guatemala")
;;     (GU  T   "Guam")
;;     (GW  T   "Guinea-Bissau")
;;     (GY  T   "Guyana")
;;     (HK  T   "Hong Kong")
;;     (HM  T   "Heard & McDonald Islands")
;;     (HN  T   "Honduras")
;;     (HR  T   "Croatia")
;;     (HT  T   "Haiti")
;;     (HU  T   "Hungary")
;;     (ID  T   "Indonesia")
;;     (IE  T   "Ireland")
;;     (IL  T   "Israel")
;;     (IN  T   "India")
;;     (IO  T   "British Indian Ocean Territory")
;;     (IQ  T   "Iraq")
;;     (IR  T   "Islamic Republic of Iran")
;;     (IS  T   "Iceland")
;;     (IT  T   "Italy")
;;     (JM  T   "Jamaica")
;;     (JO  T   "Jordan")
;;     (JP  T   "Japan")
;;     (KE  T   "Kenya")
;;     (KG  T   "Kyrgyzstan")
;;     (KH  T   "Cambodia")
;;     (KI  T   "Kiribati")
;;     (KM  T   "Comoros")
;;     (KN  T   "St. Kitts and Nevis")
;;     (KP  T   "Korea, Democratic People's Republic of")
;;     (KR  T   "Korea, Republic of")
;;     (KW  T   "Kuwait")
;;     (KY  T   "Cayman Islands")
;;     (KZ  T   "Kazakhstan")
;;     (LA  T   "Lao People's Democratic Republic")
;;     (LB  T   "Lebanon")
;;     (LC  T   "Saint Lucia")
;;     (LI  T   "Liechtenstein")
;;     (LK  T   "Sri Lanka")
;;     (LR  T   "Liberia")
;;     (LS  T   "Lesotho")
;;     (LT  T   "Lithuania")
;;     (LU  T   "Luxembourg")
;;     (LV  T   "Latvia")
;;     (LY  T   "Libyan Arab Jamahiriya")
;;     (MA  T   "Morocco")
;;     (MC  T   "Monaco")
;;     (MD  T   "Moldova, Republic of")
;;     (MG  T   "Madagascar")
;;     (MH  T   "Marshall Islands")
;;     (ML  T   "Mali")
;;     (MM  T   "Myanmar")
;;     (MN  T   "Mongolia")
;;     (MO  T   "Macau")
;;     (MP  T   "Northern Mariana Islands")
;;     (MQ  T   "Martinique")
;;     (MR  T   "Mauritania")
;;     (MS  T   "Monserrat")
;;     (MT  T   "Malta")
;;     (MU  T   "Mauritius")
;;     (MV  T   "Maldives")
;;     (MW  T   "Malawi")
;;     (MX  T   "Mexico")
;;     (MY  T   "Malaysia")
;;     (MZ  T   "Mozambique")
;;     (NA  T   "Nambia")
;;     (NC  T   "New Caledonia")
;;     (NE  T   "Niger")
;;     (NF  T   "Norfolk Island")
;;     (NG  T   "Nigeria")
;;     (NI  T   "Nicaragua")
;;     (NL  T   "Netherlands")
;;     (NO  T   "Norway")
;;     (NP  T   "Nepal")
;;     (NR  T   "Nauru")
;;     (NT  NIL "Neutral Zone ")
;;     (NU  T   "Niue")
;;     (NZ  T   "New Zealand")
;;     (OM  T   "Oman")
;;     (PA  T   "Panama")
;;     (PE  T   "Peru")
;;     (PF  T   "French Polynesia")
;;     (PG  T   "Papua New Guinea")
;;     (PH  T   "Philippines")
;;     (PK  T   "Pakistan")
;;     (PL  T   "Poland")
;;     (PM  T   "St. Pierre & Miquelon")
;;     (PN  T   "Pitcairn")
;;     (PR  T   "Puerto Rico")
;;     (PT  T   "Portugal")
;;     (PW  T   "Palau")
;;     (PY  T   "Paraguay")
;;     (QA  T   "Qatar")
;;     (RE  T   "Réunion")
;;     (RO  T   "Romania")
;;     (RU  T   "Russian Federation")
;;     (RW  T   "Rwanda")
;;     (SA  T   "Saudi Arabia")
;;     (SB  T   "Solomon Islands")
;;     (SC  T   "Seychelles")
;;     (SD  T   "Sudan")
;;     (SE  T   "Sweden")
;;     (SG  T   "Singapore")
;;     (SH  T   "St. Helena")
;;     (SI  T   "Slovenia")
;;     (SJ  T   "Svalbard & Jan Mayen Islands")
;;     (SK  T   "Slovakia")
;;     (SL  T   "Sierra Leone")
;;     (SM  T   "San Marino")
;;     (SN  T   "Senegal")
;;     (SO  T   "Somalia")
;;     (SR  T   "Suriname")
;;     (ST  T   "Sao Tome & Principe")
;;     (SU  NIL "Union of Soviet Socialist Republics ")
;;     (SV  T   "El Salvador")
;;     (SY  T   "Syrian Arab Republic")
;;     (SZ  T   "Swaziland")
;;     (TC  T   "Turks & Caicos Islands")
;;     (TD  T   "Chad")
;;     (TF  T   "French Southern Territories")
;;     (TG  T   "Togo")
;;     (TH  T   "Thailand")
;;     (TJ  T   "Tajikistan")
;;     (TK  T   "Tokelau")
;;     (TM  T   "Turkmenistan")
;;     (TN  T   "Tunisia")
;;     (TO  T   "Tonga")
;;     (TP  T   "East Timor")
;;     (TR  T   "Turkey")
;;     (TT  T   "Trinidad & Tobago")
;;     (TV  T   "Tuvalu")
;;     (TW  T   "Taiwan, Province of China")
;;     (TZ  T   "Tanzania, United Republic of")
;;     (UA  T   "Ukraine")
;;     (UG  T   "Uganda")
;;     (UM  T   "United States Minor Outlying Islands")
;;     (US  T   "United States of America")
;;     (UY  T   "Uruguay")
;;     (UZ  T   "Uzbekistan")
;;     (VA  T   "Vatican City State (Holy See)")
;;     (VC  T   "St. Vincent & the Grenadines")
;;     (VE  T   "Venezuela")
;;     (VG  T   "British Virgin Islands")
;;     (VI  T   "United States Virgin Islands")
;;     (VN  T   "Viet Nam")
;;     (VU  T   "Vanuatu")
;;     (WF  T   "Wallis & Futuna Islands")
;;     (WS  T   "Samoa")
;;     (YD  NIL "Democratic Yemen ")
;;     (YE  T   "Yemen")
;;     (YT  T   "Mayotte")
;;     (YU  NIL "Yugoslavia")
;;     (ZA  T   "South Africa")
;;     (ZM  T   "Zambia")
;;     (ZR  T   "Zaire")
;;     (ZW  T   "Zimbabwe")
;;     (ZZ  T   "Unknown or unspecified country")
;;     ));;+countries+
;; 
;; 
;; (show (set-difference
;;        (mapcar (function car) +countries+en+)
;;        (mapcan (lambda (c) (if (second c) (list (car c)))) +countries+)))
;; 
;; 
;; (defun un-coma (str)
;;  (let ((coma (position (character ", ") str)))
;;    (if (not coma)
;;      str
;;      (concatenate 'string (subseq str (+ 2 coma)) " " (subseq str 0 coma))))
;;  );;un-coma
;; 
;; 
;; (progn
;;   (insert "\n\n(defconstant +countries+ \n'(\n")
;;   (mapc (lambda (c)
;;           (insert (format "(%S %S\n%S\n%S" (first c) (second c) (third c) (fourth c)))
;;           (insert (if (fifth c) (format "\n%S)\n" (fifth c)) ")\n")) )
;;         (mapcar
;;          (lambda (cc)
;;            (let* ((en-sn (cadr (assoc cc +countries+en+)))
;;                   (fr-sn (string-capitalize (cadr (assoc cc +countries+fr+))))
;;                   (data (assoc cc +countries+))
;;                   (exists (second data))
;;                   (en-n (and data
;;                              (replace-regexp-in-string "\\&" "and"
;;                                                        (third data)))))
;;              (if (string-equal (string-downcase en-sn) (string-downcase en-n))
;;                (list cc exists (un-coma fr-sn) (un-coma en-n))
;;                (list cc exists (un-coma fr-sn)
;;                      (string-capitalize (un-coma en-sn)) (un-coma en-n)))))
;;          (delete-duplicates
;;           (append (mapcar (function car) +countries+en+)
;;                   (mapcar (function car) +countries+fr+)
;;                   (mapcar (function car) +countries+))))
;;         )
;;   (insert "))\n"))





;;;; iso3166.utf-8.lisp               --                     --          ;;;;
