;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
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
;;;;    2004-10-14 <PJB> Converted Alpha code symbols to strings.
;;;;    2004-03-16 <PJB> Added alpha-3 and numeric codes.
;;;;    2003-10-14 <PJB> Created.
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ISO3166"
  (:USE "COMMON-LISP")
  (:EXPORT "GET-COUNTRIES")
  (:DOCUMENTATION
   "This package exports functions and data to process iso3166 country codes.
    
    Copyright Pascal J. Bourguignon 2003 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ISO3166")




(DEFMACRO C-CODE-N  (C) `(FIRST   ,C))
(DEFMACRO C-CODE-3  (C) `(SECOND  ,C))
(DEFMACRO C-CODE-2  (C) `(THIRD   ,C))
(DEFMACRO C-EXISTS  (C) `(FOURTH  ,C))
(DEFMACRO C-FR-NAME (C) `(FIFTH   ,C))
(DEFMACRO C-EN-NAME (C) `(SIXTH   ,C))

;; https://online.correos.es/ayuda/codigoiso.asp

(defvar +COUNTRIES+ 
  '( 
    ( 180 "COD" "CD" NIL   
     "La République Démocratique Du Congo"
     "The Democratic Republic Of Congo")
    ( 807 "MKD" "MK" NIL  
     "L'Ex-République Yougoslave De Macédoine"
     "The Former Yugoslav Republic Of Macedonia")
    ( 275 "PSE" "PS" NIL  
     "Territoire Palestinien Occupé"
     "Occupied Palestinian Territory")
    ( 626 "TLS" "TL" NIL  
     "Timor-Leste"
     "Timor-Leste")
    ( 020 "AND" "AD" T    
     "Andorre"
     "Andorra")
    ( 784 "ARE" "AE" T    
     "Émirats Arabes Unis"
     "United Arab Emirates")
    ( 004 "AFG" "AF" T    
     "Afghanistan"
     "Afghanistan")
    ( 028 "ATG" "AG" T    
     "Antigua et Barbuda"
     "Antigua and Barbuda")
    ( 660 "AIA" "AI" T    
     "Anguilla"
     "Anguilla")
    ( 008 "ALB" "AL" T    
     "Albanie"
     "Albania")
    ( 051 "ARM" "AM" T    
     "Arménie"
     "Armenia")
    ( 530 "ANT" "AN" T    
     "Antilles Néerlandaises"
     "Netherlands Antilles")
    ( 024 "AGO" "AO" T    
     "Angola"
     "Angola")
    ( 010 "ATA" "AQ" T    
     "Antarctique"
     "Antarctica")
    ( 032 "ARG" "AR" T    
     "Argentine"
     "Argentina")
    ( 016 "ASM" "AS" T    
     "Samoa Américaines"
     "American Samoa")
    ( 040 "AUT" "AT" T    
     "Autriche"
     "Austria")
    ( 036 "AUS" "AU" T    
     "Australie"
     "Australia")
    ( 533 "ABW" "AW" T    
     "Aruba"
     "Aruba")
    ( 031 "AZE" "AZ" T    
     "Azerbaïdjan"
     "Azerbaijan")
    ( 070 "BIH" "BA" T    
     "Bosnie-Herzégovine"
     "Bosnia and Herzegovina")
    ( 052 "BRB" "BB" T    
     "Barbade"
     "Barbados")
    ( 050 "BGD" "BD" T    
     "Bangladesh"
     "Bangladesh")
    ( 056 "BEL" "BE" T    
     "Belgique"
     "Belgium")
    ( 854 "BFA" "BF" T    
     "Burkina Faso"
     "Burkina Faso")
    ( 100 "BGR" "BG" T    
     "Bulgarie"
     "Bulgaria")
    ( 048 "BHR" "BH" T    
     "Bahreïn"
     "Bahrain")
    ( 108 "BDI" "BI" T    
     "Burundi"
     "Burundi")
    ( 204 "BEN" "BJ" T    
     "Bénin"
     "Benin")
    ( 060 "BMU" "BM" T    
     "Bermudes"
     "Bermuda")
    ( 096 "BRN" "BN" T    
     "Brunéi Darussalam"
     "Brunei Darussalam")
    ( 068 "BOL" "BO" T    
     "Bolivie"
     "Bolivia")
    ( 076 "BRA" "BR" T    
     "Brésil"
     "Brazil")
    ( 044 "BHS" "BS" T    
     "Bahamas"
     "Bahamas")
    ( 064 "BTN" "BT" T    
     "Bhoutan"
     "Bhutan")
    ( 000 "NIL" "BU" NIL  
     "Burma"
     "Burma")
    ( 074 "BVT" "BV" T    
     "Île Bouvet"
     "Bouvet Island")
    ( 072 "BWA" "BW" T    
     "Botswana"
     "Botswana")
    ( 112 "BLR" "BY" T    
     "Bélarus"
     "Belarus")
    ( 084 "BLZ" "BZ" T    
     "Belize"
     "Belize")
    ( 124 "CAN" "CA" T    
     "Canada"
     "Canada")
    ( 166 "CCK" "CC" T    
     "Îles Cocos (Keeling)"
     "Cocos (Keeling) Islands")
    ( 140 "CAF" "CF" T    
     "République Centrafricaine"
     "Central African Republic")
    ( 178 "COG" "CG" T    
     "Congo"
     "Republic of the Congo")
    ( 756 "CHE" "CH" T    
     "Suisse"
     "Switzerland")
    ( 384 "CIV" "CI" T    
     "Côte d'Ivoire"
     "Ivory Coast")
    ( 184 "COK" "CK" T    
     "Îles Cook"
     "Cook Islands")
    ( 152 "CHL" "CL" T    
     "Chili"
     "Chile")
    ( 120 "CMR" "CM" T    
     "Cameroun"
     "Cameroon")
    ( 156 "CHN" "CN" T    
     "Chine"
     "China")
    ( 170 "COL" "CO" T    
     "Colombie"
     "Colombia")
    ( 188 "CRI" "CR" T    
     "Costa Rica"
     "Costa Rica")
    ( 891 "SCG" "CS" T    
     "Serbie et Monténégro"
     "Serbia and Montenegro")
    ( 192 "CUB" "CU" T    
     "Cuba"
     "Cuba")
    ( 132 "CPV" "CV" T    
     "Cap-Vert"
     "Cape Verde")
    ( 162 "CXR" "CX" T    
     "Île Christmas"
     "Christmas Island")
    ( 196 "CYP" "CY" T    
     "Chypre"
     "Cyprus")
    ( 203 "CZE" "CZ" T    
     "République Tchèque"
     "Czech Republic")
    ( 000 "NIL" "DD" NIL  
     "République Démocratique d'Allemagne"
     "German Democratic Republic")
    ( 276 "DEU" "DE" T    
     "Allemagne"
     "Germany")
    ( 262 "DJI" "DJ" T    
     "Djibouti"
     "Djibouti")
    ( 208 "DNK" "DK" T    
     "Danemark"
     "Denmark")
    ( 212 "DMA" "DM" T    
     "Dominique"
     "Dominica")
    ( 214 "DOM" "DO" T    
     "République Dominicaine"
     "Dominican Republic")
    ( 012 "DZA" "DZ" T    
     "Algérie"
     "Algeria")
    ( 218 "ECU" "EC" T    
     "Équateur"
     "Ecuador")
    ( 233 "EST" "EE" T    
     "Estonie"
     "Estonia")
    ( 818 "EGY" "EG" T    
     "Égypte"
     "Egypt")
    ( 732 "ESH" "EH" T    
     "Sahara Occidental"
     "Western Sahara")
    ( 232 "ERI" "ER" T    
     "Érythrée"
     "Eritrea")
    ( 724 "ESP" "ES" T    
     "Espagne"
     "Spain")
    ( 231 "ETH" "ET" T    
     "Éthiopie"
     "Ethiopia")
    ( 246 "FIN" "FI" T    
     "Finlande"
     "Finland")
    ( 242 "FJI" "FJ" T    
     "Fidji"
     "Fiji")
    ( 238 "FLK" "FK" T    
     "Îles Malouines"
     "Falkland Islands")
    ( 583 "FSM" "FM" T    
     "États Fédérés De Micronésie"
     "Federated States Of Micronesia")
    ( 234 "FRO" "FO" T    
     "Îles Féroé"
     "Faroe Islands")
    ( 250 "FRA" "FR" T    
     "France"
     "France")
    ( 000 "NIL" "FX" T    
     "France Métropolitaine"
     "Metropolitan France")
    ( 266 "GAB" "GA" T    
     "Gabon"
     "Gabon")
    ( 826 "GBR" "GB" T    
     "Royaume-Uni"
     "United Kingdom")
    ( 308 "GRD" "GD" T    
     "Grenade"
     "Grenada")
    ( 268 "GEO" "GE" T    
     "Géorgie"
     "Georgia")
    ( 254 "GUF" "GF" T    
     "Guyane Française"
     "French Guiana")
    ( 288 "GHA" "GH" T    
     "Ghana"
     "Ghana")
    ( 292 "GIB" "GI" T    
     "Gibraltar"
     "Gibraltar")
    ( 304 "GRL" "GL" T    
     "Groenland"
     "Greenland")
    ( 270 "GMB" "GM" T    
     "Gambie"
     "Gambia")
    ( 324 "GIN" "GN" T    
     "Guinée"
     "Guinea")
    ( 312 "GLP" "GP" T    
     "Guadeloupe"
     "Guadeloupe")
    ( 226 "GNQ" "GQ" T    
     "Guinée Équatoriale"
     "Equatorial Guinea")
    ( 300 "GRC" "GR" T    
     "Grèce"
     "Greece")
    ( 239 "SGS" "GS" T    
     "Géorgie Du Sud et Les Îles Sandwich Du Sud"
     "South Georgia and the South Sandwich Islands")
    ( 320 "GTM" "GT" T    
     "Guatemala"
     "Guatemala")
    ( 316 "GUM" "GU" T    
     "Guam"
     "Guam")
    ( 624 "GNB" "GW" T    
     "Guinée-Bissau"
     "Guinea-Bissau")
    ( 328 "GUY" "GY" T    
     "Guyana"
     "Guyana")
    ( 344 "HKG" "HK" T    
     "Hong-Kong"
     "Hong Kong")
    ( 334 "HMD" "HM" T    
     "Île Mcdonald et îles Heard"
     "Heard Island and Mcdonald Islands")
    ( 340 "HND" "HN" T    
     "Honduras"
     "Honduras")
    ( 191 "HRV" "HR" T    
     "Croatie"
     "Croatia")
    ( 332 "HTI" "HT" T    
     "Haïti"
     "Haiti")
    ( 348 "HUN" "HU" T    
     "Hongrie"
     "Hungary")
    ( 360 "IDN" "ID" T    
     "Indonésie"
     "Indonesia")
    ( 372 "IRL" "IE" T    
     "Irlande"
     "Republic of Ireland")
    ( 376 "ISR" "IL" T    
     "Israël"
     "Israel")
    ( 356 "IND" "IN" T    
     "Inde"
     "India")
    ( 092 "IOT" "IO" T    
     "Territoire Britannique de l'Océan Indien"
     "British Indian Ocean Territory")
    ( 368 "IRQ" "IQ" T    
     "Iraq"
     "Iraq")
    ( 364 "IRN" "IR" T    
     "République Islamique d'Iran"
     "Islamic Republic of Iran")
    ( 352 "ISL" "IS" T    
     "Islande"
     "Iceland")
    ( 380 "ITA" "IT" T    
     "Italie"
     "Italy")
    ( 388 "JAM" "JM" T    
     "Jamaïque"
     "Jamaica")
    ( 400 "JOR" "JO" T    
     "Jordanie"
     "Jordan")
    ( 392 "JPN" "JP" T    
     "Japon"
     "Japan")
    ( 404 "KEN" "KE" T    
     "Kenya"
     "Kenya")
    ( 417 "KGZ" "KG" T    
     "Kirghizistan"
     "Kyrgyzstan")
    ( 116 "KHM" "KH" T    
     "Cambodge"
     "Cambodia")
    ( 296 "KIR" "KI" T    
     "Kiribati"
     "Kiribati")
    ( 174 "COM" "KM" T    
     "Comores"
     "Comoros")
    ( 659 "KNA" "KN" T    
     "Saint-Kitts et Nevis"
     "Saint Kitts and Nevis")
    ( 408 "PRK" "KP" T    
     "République Populaire Démocratique De Corée"
     "Democratic People's Republic of Korea")
    ( 410 "KOR" "KR" T    
     "République De Corée"
     "Republic of Korea")
    ( 414 "KWT" "KW" T    
     "Koweït"
     "Kuwait")
    ( 136 "CYM" "KY" T    
     "Îles Caïmanes"
     "Cayman Islands")
    ( 398 "KAZ" "KZ" T    
     "Kazakhstan"
     "Kazakhstan")
    ( 418 "LAO" "LA" T    
     "République Démocratique Populaire Lao"
     "Lao People's Democratic Republic")
    ( 422 "LBN" "LB" T    
     "Liban"
     "Lebanon")
    ( 662 "LCA" "LC" T    
     "Sainte-Lucie"
     "Saint Lucia")
    ( 438 "LIE" "LI" T    
     "Liechtenstein"
     "Liechtenstein")
    ( 144 "LKA" "LK" T    
     "Sri Lanka"
     "Sri Lanka")
    ( 430 "LBR" "LR" T    
     "Libéria"
     "Liberia")
    ( 426 "LSO" "LS" T    
     "Lesotho"
     "Lesotho")
    ( 440 "LTU" "LT" T    
     "Lituanie"
     "Lithuania")
    ( 442 "LUX" "LU" T    
     "Luxembourg"
     "Luxembourg")
    ( 428 "LVA" "LV" T    
     "Lettonie"
     "Latvia")
    ( 434 "LBY" "LY" T    
     "Jamahiriya Arabe Libyenne"
     "Libyan Arab Jamahiriya")
    ( 504 "MAR" "MA" T    
     "Maroc"
     "Morocco")
    ( 492 "MCO" "MC" T    
     "Monaco"
     "Monaco")
    ( 498 "MDA" "MD" T    
     "République De Moldova"
     "Republic of Moldova")
    ( 450 "MDG" "MG" T    
     "Madagascar"
     "Madagascar")
    ( 584 "MHL" "MH" T    
     "Îles Marshall"
     "Marshall Islands")
    ( 466 "MLI" "ML" T    
     "Mali"
     "Mali")
    ( 104 "MMR" "MM" T    
     "Myanmar"
     "Myanmar")
    ( 496 "MNG" "MN" T    
     "Mongolie"
     "Mongolia")
    ( 446 "MAC" "MO" T    
     "Macao"
     "Macao")
    ( 580 "MNP" "MP" T    
     "Îles Mariannes Du Nord"
     "Northern Mariana Islands")
    ( 474 "MTQ" "MQ" T    
     "Martinique"
     "Martinique")
    ( 478 "MRT" "MR" T    
     "Mauritanie"
     "Mauritania")
    ( 500 "MSR" "MS" T    
     "Montserrat"
     "Montserrat")
    ( 470 "MLT" "MT" T    
     "Malte"
     "Malta")
    ( 480 "MUS" "MU" T    
     "Maurice"
     "Mauritius")
    ( 462 "MDV" "MV" T    
     "Maldives"
     "Maldives")
    ( 454 "MWI" "MW" T    
     "Malawi"
     "Malawi")
    ( 484 "MEX" "MX" T    
     "Mexique"
     "Mexico")
    ( 458 "MYS" "MY" T    
     "Malaisie"
     "Malaysia")
    ( 508 "MOZ" "MZ" T    
     "Mozambique"
     "Mozambique")
    ( 516 "NAM" "NA" T    
     "Namibie"
     "Namibia")
    ( 540 "NCL" "NC" T    
     "Nouvelle-Calédonie"
     "New Caledonia")
    ( 562 "NER" "NE" T    
     "Niger"
     "Niger")
    ( 574 "NFK" "NF" T    
     "Île Norfolk"
     "Norfolk Island")
    ( 566 "NGA" "NG" T    
     "Nigéria"
     "Nigeria")
    ( 558 "NIC" "NI" T    
     "Nicaragua"
     "Nicaragua")
    ( 528 "NLD" "NL" T    
     "Pays-Bas"
     "Netherlands")
    ( 578 "NOR" "NO" T    
     "Norvège"
     "Norway")
    ( 524 "NPL" "NP" T    
     "Népal"
     "Nepal")
    ( 520 "NRU" "NR" T    
     "Nauru"
     "Nauru")
    ( 000 "NIL" "NT" NIL  
     "Nil"
     "Nil")
    ( 570 "NIU" "NU" T    
     "Niué"
     "Niue")
    ( 554 "NZL" "NZ" T    
     "Nouvelle-Zélande"
     "New Zealand")
    ( 512 "OMN" "OM" T    
     "Oman"
     "Oman")
    ( 591 "PAN" "PA" T    
     "Panama"
     "Panama")
    ( 604 "PER" "PE" T    
     "Pérou"
     "Peru")
    ( 258 "PYF" "PF" T    
     "Polynésie Française"
     "French Polynesia")
    ( 598 "PNG" "PG" T    
     "Papouasie-Nouvelle-Guinée"
     "Papua New Guinea")
    ( 608 "PHL" "PH" T    
     "Philippines"
     "Philippines")
    ( 586 "PAK" "PK" T    
     "Pakistan"
     "Pakistan")
    ( 616 "POL" "PL" T    
     "Pologne"
     "Poland")
    ( 666 "SPM" "PM" T    
     "Saint-Pierre et Miquelon"
     "Saint Pierre and Miquelon")
    ( 612 "PCN" "PN" T    
     "Pitcairn"
     "Pitcairn")
    ( 630 "PRI" "PR" T    
     "Porto Rico"
     "Puerto Rico")
    ( 620 "PRT" "PT" T    
     "Portugal"
     "Portugal")
    ( 585 "PLW" "PW" T    
     "Palaos"
     "Palau")
    ( 600 "PRY" "PY" T    
     "Paraguay"
     "Paraguay")
    ( 634 "QAT" "QA" T    
     "Qatar"
     "Qatar")
    ( 638 "REU" "RE" T    
     "Réunion"
     "Reunion")
    ( 642 "ROU" "RO" T    
     "Roumanie"
     "Romania")
    ( 643 "RUS" "RU" T    
     "Fédération De Russie"
     "Russian Federation")
    ( 646 "RWA" "RW" T    
     "Rwanda"
     "Rwanda")
    ( 682 "SAU" "SA" T    
     "Arabie Saoudite"
     "Saudi Arabia")
    ( 090 "SLB" "SB" T    
     "Îles Salomon"
     "Solomon Islands")
    ( 690 "SYC" "SC" T    
     "Seychelles"
     "Seychelles")
    ( 736 "SDN" "SD" T    
     "Soudan"
     "Sudan")
    ( 752 "SWE" "SE" T    
     "Suède"
     "Sweden")
    ( 702 "SGP" "SG" T    
     "Singapour"
     "Singapore")
    ( 654 "SHN" "SH" T    
     "Sainte-Hélène"
     "Saint Helena")
    ( 705 "SVN" "SI" T    
     "Slovénie"
     "Slovenia")
    ( 744 "SJM" "SJ" T    
     "Svalbard et île Jan Mayen"
     "Svalbard and Jan Mayen Islands")
    ( 703 "SVK" "SK" T    
     "Slovaquie"
     "Slovakia")
    ( 694 "SLE" "SL" T    
     "Sierra Leone"
     "Sierra Leone")
    ( 674 "SMR" "SM" T    
     "Saint-Marin"
     "San Marino")
    ( 686 "SEN" "SN" T    
     "Sénégal"
     "Senegal")
    ( 706 "SOM" "SO" T    
     "Somalie"
     "Somalia")
    ( 740 "SUR" "SR" T    
     "Suriname"
     "Suriname")
    ( 678 "STP" "ST" T    
     "Sáo Tomé et Principe"
     "Sao Tome and Principe")
    ( 000 "NIL" "SU" NIL  
     "Union des Républiques Socialistes Soviétiques"
     "Union of Soviet Socialist Republics")
    ( 222 "SLV" "SV" T    
     "El Salvador"
     "El Salvador")
    ( 760 "SYR" "SY" T    
     "République Arabe Syrienne"
     "Syrian Arab Republic")
    ( 748 "SWZ" "SZ" T    
     "Swaziland"
     "Swaziland")
    ( 796 "TCA" "TC" T    
     "Îles Turks et Caïques"
     "Turks and Caicos Islands")
    ( 148 "TCD" "TD" T    
     "Tchad"
     "Chad")
    ( 260 "ATF" "TF" T    
     "Terres Australes Françaises"
     "French Southern Territories")
    ( 768 "TGO" "TG" T    
     "Togo"
     "Togo")
    ( 764 "THA" "TH" T    
     "Thaïlande"
     "Thailand")
    ( 762 "TJK" "TJ" T    
     "Tadjikistan"
     "Tajikistan")
    ( 772 "TKL" "TK" T    
     "Tokelau"
     "Tokelau")
    ( 795 "TKM" "TM" T    
     "Turkménistan"
     "Turkmenistan")
    ( 788 "TUN" "TN" T    
     "Tunisie"
     "Tunisia")
    ( 776 "TON" "TO" T    
     "Tonga"
     "Tonga")
    ( 000 "NIL" "TP" T    
     "Timor Oriental"
     "East Timor")
    ( 792 "TUR" "TR" T    
     "Turquie"
     "Turkey")
    ( 780 "TTO" "TT" T    
     "Trinité et Tobago"
     "Trinidad and Tobago")
    ( 798 "TUV" "TV" T    
     "Tuvalu"
     "Tuvalu")
    ( 158 "TWN" "TW" T    
     "Province de Chine Taïwan"
     "Province of China Taiwan")
    ( 834 "TZA" "TZ" T    
     "République-Unie de Tanzanie"
     "United Republic of Tanzania")
    ( 804 "UKR" "UA" T    
     "Ukraine"
     "Ukraine")
    ( 800 "UGA" "UG" T    
     "Ouganda"
     "Uganda")
    ( 581 "UMI" "UM" T    
     "Îles Mineures Éloignées des États-Unis"
     "United States Minor Outlying Islands")
    ( 840 "USA" "US" T    
     "États-Unis"
     "United States of America")
    ( 858 "URY" "UY" T    
     "Uruguay"
     "Uruguay")
    ( 860 "UZB" "UZ" T    
     "Ouzbékistan"
     "Uzbekistan")
    ( 336 "VAT" "VA" T    
     "Saint-Siège (État de La Cité Du Vatican)"
     "Holy See (Vatican City State)")
    ( 670 "VCT" "VC" T    
     "Saint-Vincent et Les Grenadines"
     "Saint Vincent and The Grenadines")
    ( 862 "VEN" "VE" T    
     "Venezuela"
     "Venezuela")
    ( 092 "VGB" "VG" T    
     "Îles Vierges Britanniques"
     "British Virgin Islands")
    ( 850 "VIR" "VI" T    
     "Îles Vierges Des États-Unis"
     "United States Virgin Islands")
    ( 704 "VNM" "VN" T    
     "Viet Nam"
     "Viet Nam")
    ( 548 "VUT" "VU" T    
     "Vanuatu"
     "Vanuatu")
    ( 876 "WLF" "WF" T    
     "Wallis Et Futuna"
     "Wallis and Futuna Islands")
    ( 882 "WSM" "WS" T    
     "Samoa"
     "Samoa")
    ( 000 "NIL" "YD" NIL  
     "Yemen Démocratique"
     "Democratic Yemen")
    ( 887 "YEM" "YE" T    
     "Yémen"
     "Yemen")
    ( 175 "MYT" "YT" T    
     "Mayotte"
     "Mayotte")
    ( 000 "NIL" "YU" T    
     "Yougoslavie"
     "Yugoslavia")
    ( 710 "ZAF" "ZA" T    
     "Afrique Du Sud"
     "South Africa")
    ( 894 "ZMB" "ZM" T    
     "Zambie"
     "Zambia")
    ( 000 "NIL" "ZR" T    
     "Zaïre"
     "Zaire")
    ( 716 "ZWE" "ZW" T    
     "Zimbabwe"
     "Zimbabwe")
    ( 000 "NIL" "ZZ" T    
     "Pays inconnu ou non spécifié"
     "Unknown or unspecified country")
    ( 248 "ALA" "AX" T    
     "Îles Aland"
     "Áland Islands")
    )) ;;+COUNTRIES+



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
                             (list (LIST (C-CODE-2 C)
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
    RESULT))



;;;; iso3166.lisp                     --                     --          ;;;;
