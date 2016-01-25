;;;; -*- mode:lisp; coding:utf-8 -*-
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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ISO3166"
  (:use "COMMON-LISP")
  (:export "GET-COUNTRIES")
  (:documentation
   "
This package exports functions and data to process iso3166 country codes.
    

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ISO3166")




(defmacro c-code-n  (c) `(first   ,c))
(defmacro c-code-3  (c) `(second  ,c))
(defmacro c-code-2  (c) `(third   ,c))
(defmacro c-exists  (c) `(fourth  ,c))
(defmacro c-fr-name (c) `(fifth   ,c))
(defmacro c-en-name (c) `(sixth   ,c))

;; https://online.correos.es/ayuda/codigoiso.asp

(defvar +countries+ 
  '( 
    ( 180 "COD" "CD" nil   
     "La République Démocratique Du Congo"
     "The Democratic Republic Of Congo")
    ( 807 "MKD" "MK" nil  
     "L'Ex-République Yougoslave De Macédoine"
     "The Former Yugoslav Republic Of Macedonia")
    ( 275 "PSE" "PS" nil  
     "Territoire Palestinien Occupé"
     "Occupied Palestinian Territory")
    ( 626 "TLS" "TL" nil  
     "Timor-Leste"
     "Timor-Leste")
    ( 020 "AND" "AD" t    
     "Andorre"
     "Andorra")
    ( 784 "ARE" "AE" t    
     "Émirats Arabes Unis"
     "United Arab Emirates")
    ( 004 "AFG" "AF" t    
     "Afghanistan"
     "Afghanistan")
    ( 028 "ATG" "AG" t    
     "Antigua et Barbuda"
     "Antigua and Barbuda")
    ( 660 "AIA" "AI" t    
     "Anguilla"
     "Anguilla")
    ( 008 "ALB" "AL" t    
     "Albanie"
     "Albania")
    ( 051 "ARM" "AM" t    
     "Arménie"
     "Armenia")
    ( 530 "ANT" "AN" t    
     "Antilles Néerlandaises"
     "Netherlands Antilles")
    ( 024 "AGO" "AO" t    
     "Angola"
     "Angola")
    ( 010 "ATA" "AQ" t    
     "Antarctique"
     "Antarctica")
    ( 032 "ARG" "AR" t    
     "Argentine"
     "Argentina")
    ( 016 "ASM" "AS" t    
     "Samoa Américaines"
     "American Samoa")
    ( 040 "AUT" "AT" t    
     "Autriche"
     "Austria")
    ( 036 "AUS" "AU" t    
     "Australie"
     "Australia")
    ( 533 "ABW" "AW" t    
     "Aruba"
     "Aruba")
    ( 031 "AZE" "AZ" t    
     "Azerbaïdjan"
     "Azerbaijan")
    ( 070 "BIH" "BA" t    
     "Bosnie-Herzégovine"
     "Bosnia and Herzegovina")
    ( 052 "BRB" "BB" t    
     "Barbade"
     "Barbados")
    ( 050 "BGD" "BD" t    
     "Bangladesh"
     "Bangladesh")
    ( 056 "BEL" "BE" t    
     "Belgique"
     "Belgium")
    ( 854 "BFA" "BF" t    
     "Burkina Faso"
     "Burkina Faso")
    ( 100 "BGR" "BG" t    
     "Bulgarie"
     "Bulgaria")
    ( 048 "BHR" "BH" t    
     "Bahreïn"
     "Bahrain")
    ( 108 "BDI" "BI" t    
     "Burundi"
     "Burundi")
    ( 204 "BEN" "BJ" t    
     "Bénin"
     "Benin")
    ( 060 "BMU" "BM" t    
     "Bermudes"
     "Bermuda")
    ( 096 "BRN" "BN" t    
     "Brunéi Darussalam"
     "Brunei Darussalam")
    ( 068 "BOL" "BO" t    
     "Bolivie"
     "Bolivia")
    ( 076 "BRA" "BR" t    
     "Brésil"
     "Brazil")
    ( 044 "BHS" "BS" t    
     "Bahamas"
     "Bahamas")
    ( 064 "BTN" "BT" t    
     "Bhoutan"
     "Bhutan")
    ( 000 "NIL" "BU" nil  
     "Burma"
     "Burma")
    ( 074 "BVT" "BV" t    
     "Île Bouvet"
     "Bouvet Island")
    ( 072 "BWA" "BW" t    
     "Botswana"
     "Botswana")
    ( 112 "BLR" "BY" t    
     "Bélarus"
     "Belarus")
    ( 084 "BLZ" "BZ" t    
     "Belize"
     "Belize")
    ( 124 "CAN" "CA" t    
     "Canada"
     "Canada")
    ( 166 "CCK" "CC" t    
     "Îles Cocos (Keeling)"
     "Cocos (Keeling) Islands")
    ( 140 "CAF" "CF" t    
     "République Centrafricaine"
     "Central African Republic")
    ( 178 "COG" "CG" t    
     "Congo"
     "Republic of the Congo")
    ( 756 "CHE" "CH" t    
     "Suisse"
     "Switzerland")
    ( 384 "CIV" "CI" t    
     "Côte d'Ivoire"
     "Ivory Coast")
    ( 184 "COK" "CK" t    
     "Îles Cook"
     "Cook Islands")
    ( 152 "CHL" "CL" t    
     "Chili"
     "Chile")
    ( 120 "CMR" "CM" t    
     "Cameroun"
     "Cameroon")
    ( 156 "CHN" "CN" t    
     "Chine"
     "China")
    ( 170 "COL" "CO" t    
     "Colombie"
     "Colombia")
    ( 188 "CRI" "CR" t    
     "Costa Rica"
     "Costa Rica")
    ( 891 "SCG" "CS" t    
     "Serbie et Monténégro"
     "Serbia and Montenegro")
    ( 192 "CUB" "CU" t    
     "Cuba"
     "Cuba")
    ( 132 "CPV" "CV" t    
     "Cap-Vert"
     "Cape Verde")
    ( 162 "CXR" "CX" t    
     "Île Christmas"
     "Christmas Island")
    ( 196 "CYP" "CY" t    
     "Chypre"
     "Cyprus")
    ( 203 "CZE" "CZ" t    
     "République Tchèque"
     "Czech Republic")
    ( 000 "NIL" "DD" nil  
     "République Démocratique d'Allemagne"
     "German Democratic Republic")
    ( 276 "DEU" "DE" t    
     "Allemagne"
     "Germany")
    ( 262 "DJI" "DJ" t    
     "Djibouti"
     "Djibouti")
    ( 208 "DNK" "DK" t    
     "Danemark"
     "Denmark")
    ( 212 "DMA" "DM" t    
     "Dominique"
     "Dominica")
    ( 214 "DOM" "DO" t    
     "République Dominicaine"
     "Dominican Republic")
    ( 012 "DZA" "DZ" t    
     "Algérie"
     "Algeria")
    ( 218 "ECU" "EC" t    
     "Équateur"
     "Ecuador")
    ( 233 "EST" "EE" t    
     "Estonie"
     "Estonia")
    ( 818 "EGY" "EG" t    
     "Égypte"
     "Egypt")
    ( 732 "ESH" "EH" t    
     "Sahara Occidental"
     "Western Sahara")
    ( 232 "ERI" "ER" t    
     "Érythrée"
     "Eritrea")
    ( 724 "ESP" "ES" t    
     "Espagne"
     "Spain")
    ( 231 "ETH" "ET" t    
     "Éthiopie"
     "Ethiopia")
    ( 246 "FIN" "FI" t    
     "Finlande"
     "Finland")
    ( 242 "FJI" "FJ" t    
     "Fidji"
     "Fiji")
    ( 238 "FLK" "FK" t    
     "Îles Malouines"
     "Falkland Islands")
    ( 583 "FSM" "FM" t    
     "États Fédérés De Micronésie"
     "Federated States Of Micronesia")
    ( 234 "FRO" "FO" t    
     "Îles Féroé"
     "Faroe Islands")
    ( 250 "FRA" "FR" t    
     "France"
     "France")
    ( 000 "NIL" "FX" t    
     "France Métropolitaine"
     "Metropolitan France")
    ( 266 "GAB" "GA" t    
     "Gabon"
     "Gabon")
    ( 826 "GBR" "GB" t    
     "Royaume-Uni"
     "United Kingdom")
    ( 308 "GRD" "GD" t    
     "Grenade"
     "Grenada")
    ( 268 "GEO" "GE" t    
     "Géorgie"
     "Georgia")
    ( 254 "GUF" "GF" t    
     "Guyane Française"
     "French Guiana")
    ( 288 "GHA" "GH" t    
     "Ghana"
     "Ghana")
    ( 292 "GIB" "GI" t    
     "Gibraltar"
     "Gibraltar")
    ( 304 "GRL" "GL" t    
     "Groenland"
     "Greenland")
    ( 270 "GMB" "GM" t    
     "Gambie"
     "Gambia")
    ( 324 "GIN" "GN" t    
     "Guinée"
     "Guinea")
    ( 312 "GLP" "GP" t    
     "Guadeloupe"
     "Guadeloupe")
    ( 226 "GNQ" "GQ" t    
     "Guinée Équatoriale"
     "Equatorial Guinea")
    ( 300 "GRC" "GR" t    
     "Grèce"
     "Greece")
    ( 239 "SGS" "GS" t    
     "Géorgie Du Sud et Les Îles Sandwich Du Sud"
     "South Georgia and the South Sandwich Islands")
    ( 320 "GTM" "GT" t    
     "Guatemala"
     "Guatemala")
    ( 316 "GUM" "GU" t    
     "Guam"
     "Guam")
    ( 624 "GNB" "GW" t    
     "Guinée-Bissau"
     "Guinea-Bissau")
    ( 328 "GUY" "GY" t    
     "Guyana"
     "Guyana")
    ( 344 "HKG" "HK" t    
     "Hong-Kong"
     "Hong Kong")
    ( 334 "HMD" "HM" t    
     "Île Mcdonald et îles Heard"
     "Heard Island and Mcdonald Islands")
    ( 340 "HND" "HN" t    
     "Honduras"
     "Honduras")
    ( 191 "HRV" "HR" t    
     "Croatie"
     "Croatia")
    ( 332 "HTI" "HT" t    
     "Haïti"
     "Haiti")
    ( 348 "HUN" "HU" t    
     "Hongrie"
     "Hungary")
    ( 360 "IDN" "ID" t    
     "Indonésie"
     "Indonesia")
    ( 372 "IRL" "IE" t    
     "Irlande"
     "Republic of Ireland")
    ( 376 "ISR" "IL" t    
     "Israël"
     "Israel")
    ( 356 "IND" "IN" t    
     "Inde"
     "India")
    ( 092 "IOT" "IO" t    
     "Territoire Britannique de l'Océan Indien"
     "British Indian Ocean Territory")
    ( 368 "IRQ" "IQ" t    
     "Iraq"
     "Iraq")
    ( 364 "IRN" "IR" t    
     "République Islamique d'Iran"
     "Islamic Republic of Iran")
    ( 352 "ISL" "IS" t    
     "Islande"
     "Iceland")
    ( 380 "ITA" "IT" t    
     "Italie"
     "Italy")
    ( 388 "JAM" "JM" t    
     "Jamaïque"
     "Jamaica")
    ( 400 "JOR" "JO" t    
     "Jordanie"
     "Jordan")
    ( 392 "JPN" "JP" t    
     "Japon"
     "Japan")
    ( 404 "KEN" "KE" t    
     "Kenya"
     "Kenya")
    ( 417 "KGZ" "KG" t    
     "Kirghizistan"
     "Kyrgyzstan")
    ( 116 "KHM" "KH" t    
     "Cambodge"
     "Cambodia")
    ( 296 "KIR" "KI" t    
     "Kiribati"
     "Kiribati")
    ( 174 "COM" "KM" t    
     "Comores"
     "Comoros")
    ( 659 "KNA" "KN" t    
     "Saint-Kitts et Nevis"
     "Saint Kitts and Nevis")
    ( 408 "PRK" "KP" t    
     "République Populaire Démocratique De Corée"
     "Democratic People's Republic of Korea")
    ( 410 "KOR" "KR" t    
     "République De Corée"
     "Republic of Korea")
    ( 414 "KWT" "KW" t    
     "Koweït"
     "Kuwait")
    ( 136 "CYM" "KY" t    
     "Îles Caïmanes"
     "Cayman Islands")
    ( 398 "KAZ" "KZ" t    
     "Kazakhstan"
     "Kazakhstan")
    ( 418 "LAO" "LA" t    
     "République Démocratique Populaire Lao"
     "Lao People's Democratic Republic")
    ( 422 "LBN" "LB" t    
     "Liban"
     "Lebanon")
    ( 662 "LCA" "LC" t    
     "Sainte-Lucie"
     "Saint Lucia")
    ( 438 "LIE" "LI" t    
     "Liechtenstein"
     "Liechtenstein")
    ( 144 "LKA" "LK" t    
     "Sri Lanka"
     "Sri Lanka")
    ( 430 "LBR" "LR" t    
     "Libéria"
     "Liberia")
    ( 426 "LSO" "LS" t    
     "Lesotho"
     "Lesotho")
    ( 440 "LTU" "LT" t    
     "Lituanie"
     "Lithuania")
    ( 442 "LUX" "LU" t    
     "Luxembourg"
     "Luxembourg")
    ( 428 "LVA" "LV" t    
     "Lettonie"
     "Latvia")
    ( 434 "LBY" "LY" t    
     "Jamahiriya Arabe Libyenne"
     "Libyan Arab Jamahiriya")
    ( 504 "MAR" "MA" t    
     "Maroc"
     "Morocco")
    ( 492 "MCO" "MC" t    
     "Monaco"
     "Monaco")
    ( 498 "MDA" "MD" t    
     "République De Moldova"
     "Republic of Moldova")
    ( 450 "MDG" "MG" t    
     "Madagascar"
     "Madagascar")
    ( 584 "MHL" "MH" t    
     "Îles Marshall"
     "Marshall Islands")
    ( 466 "MLI" "ML" t    
     "Mali"
     "Mali")
    ( 104 "MMR" "MM" t    
     "Myanmar"
     "Myanmar")
    ( 496 "MNG" "MN" t    
     "Mongolie"
     "Mongolia")
    ( 446 "MAC" "MO" t    
     "Macao"
     "Macao")
    ( 580 "MNP" "MP" t    
     "Îles Mariannes Du Nord"
     "Northern Mariana Islands")
    ( 474 "MTQ" "MQ" t    
     "Martinique"
     "Martinique")
    ( 478 "MRT" "MR" t    
     "Mauritanie"
     "Mauritania")
    ( 500 "MSR" "MS" t    
     "Montserrat"
     "Montserrat")
    ( 470 "MLT" "MT" t    
     "Malte"
     "Malta")
    ( 480 "MUS" "MU" t    
     "Maurice"
     "Mauritius")
    ( 462 "MDV" "MV" t    
     "Maldives"
     "Maldives")
    ( 454 "MWI" "MW" t    
     "Malawi"
     "Malawi")
    ( 484 "MEX" "MX" t    
     "Mexique"
     "Mexico")
    ( 458 "MYS" "MY" t    
     "Malaisie"
     "Malaysia")
    ( 508 "MOZ" "MZ" t    
     "Mozambique"
     "Mozambique")
    ( 516 "NAM" "NA" t    
     "Namibie"
     "Namibia")
    ( 540 "NCL" "NC" t    
     "Nouvelle-Calédonie"
     "New Caledonia")
    ( 562 "NER" "NE" t    
     "Niger"
     "Niger")
    ( 574 "NFK" "NF" t    
     "Île Norfolk"
     "Norfolk Island")
    ( 566 "NGA" "NG" t    
     "Nigéria"
     "Nigeria")
    ( 558 "NIC" "NI" t    
     "Nicaragua"
     "Nicaragua")
    ( 528 "NLD" "NL" t    
     "Pays-Bas"
     "Netherlands")
    ( 578 "NOR" "NO" t    
     "Norvège"
     "Norway")
    ( 524 "NPL" "NP" t    
     "Népal"
     "Nepal")
    ( 520 "NRU" "NR" t    
     "Nauru"
     "Nauru")
    ( 000 "NIL" "NT" nil  
     "Nil"
     "Nil")
    ( 570 "NIU" "NU" t    
     "Niué"
     "Niue")
    ( 554 "NZL" "NZ" t    
     "Nouvelle-Zélande"
     "New Zealand")
    ( 512 "OMN" "OM" t    
     "Oman"
     "Oman")
    ( 591 "PAN" "PA" t    
     "Panama"
     "Panama")
    ( 604 "PER" "PE" t    
     "Pérou"
     "Peru")
    ( 258 "PYF" "PF" t    
     "Polynésie Française"
     "French Polynesia")
    ( 598 "PNG" "PG" t    
     "Papouasie-Nouvelle-Guinée"
     "Papua New Guinea")
    ( 608 "PHL" "PH" t    
     "Philippines"
     "Philippines")
    ( 586 "PAK" "PK" t    
     "Pakistan"
     "Pakistan")
    ( 616 "POL" "PL" t    
     "Pologne"
     "Poland")
    ( 666 "SPM" "PM" t    
     "Saint-Pierre et Miquelon"
     "Saint Pierre and Miquelon")
    ( 612 "PCN" "PN" t    
     "Pitcairn"
     "Pitcairn")
    ( 630 "PRI" "PR" t    
     "Porto Rico"
     "Puerto Rico")
    ( 620 "PRT" "PT" t    
     "Portugal"
     "Portugal")
    ( 585 "PLW" "PW" t    
     "Palaos"
     "Palau")
    ( 600 "PRY" "PY" t    
     "Paraguay"
     "Paraguay")
    ( 634 "QAT" "QA" t    
     "Qatar"
     "Qatar")
    ( 638 "REU" "RE" t    
     "Réunion"
     "Reunion")
    ( 642 "ROU" "RO" t    
     "Roumanie"
     "Romania")
    ( 643 "RUS" "RU" t    
     "Fédération De Russie"
     "Russian Federation")
    ( 646 "RWA" "RW" t    
     "Rwanda"
     "Rwanda")
    ( 682 "SAU" "SA" t    
     "Arabie Saoudite"
     "Saudi Arabia")
    ( 090 "SLB" "SB" t    
     "Îles Salomon"
     "Solomon Islands")
    ( 690 "SYC" "SC" t    
     "Seychelles"
     "Seychelles")
    ( 736 "SDN" "SD" t    
     "Soudan"
     "Sudan")
    ( 752 "SWE" "SE" t    
     "Suède"
     "Sweden")
    ( 702 "SGP" "SG" t    
     "Singapour"
     "Singapore")
    ( 654 "SHN" "SH" t    
     "Sainte-Hélène"
     "Saint Helena")
    ( 705 "SVN" "SI" t    
     "Slovénie"
     "Slovenia")
    ( 744 "SJM" "SJ" t    
     "Svalbard et île Jan Mayen"
     "Svalbard and Jan Mayen Islands")
    ( 703 "SVK" "SK" t    
     "Slovaquie"
     "Slovakia")
    ( 694 "SLE" "SL" t    
     "Sierra Leone"
     "Sierra Leone")
    ( 674 "SMR" "SM" t    
     "Saint-Marin"
     "San Marino")
    ( 686 "SEN" "SN" t    
     "Sénégal"
     "Senegal")
    ( 706 "SOM" "SO" t    
     "Somalie"
     "Somalia")
    ( 740 "SUR" "SR" t    
     "Suriname"
     "Suriname")
    ( 678 "STP" "ST" t    
     "Sáo Tomé et Principe"
     "Sao Tome and Principe")
    ( 000 "NIL" "SU" nil  
     "Union des Républiques Socialistes Soviétiques"
     "Union of Soviet Socialist Republics")
    ( 222 "SLV" "SV" t    
     "El Salvador"
     "El Salvador")
    ( 760 "SYR" "SY" t    
     "République Arabe Syrienne"
     "Syrian Arab Republic")
    ( 748 "SWZ" "SZ" t    
     "Swaziland"
     "Swaziland")
    ( 796 "TCA" "TC" t    
     "Îles Turks et Caïques"
     "Turks and Caicos Islands")
    ( 148 "TCD" "TD" t    
     "Tchad"
     "Chad")
    ( 260 "ATF" "TF" t    
     "Terres Australes Françaises"
     "French Southern Territories")
    ( 768 "TGO" "TG" t    
     "Togo"
     "Togo")
    ( 764 "THA" "TH" t    
     "Thaïlande"
     "Thailand")
    ( 762 "TJK" "TJ" t    
     "Tadjikistan"
     "Tajikistan")
    ( 772 "TKL" "TK" t    
     "Tokelau"
     "Tokelau")
    ( 795 "TKM" "TM" t    
     "Turkménistan"
     "Turkmenistan")
    ( 788 "TUN" "TN" t    
     "Tunisie"
     "Tunisia")
    ( 776 "TON" "TO" t    
     "Tonga"
     "Tonga")
    ( 000 "NIL" "TP" t    
     "Timor Oriental"
     "East Timor")
    ( 792 "TUR" "TR" t    
     "Turquie"
     "Turkey")
    ( 780 "TTO" "TT" t    
     "Trinité et Tobago"
     "Trinidad and Tobago")
    ( 798 "TUV" "TV" t    
     "Tuvalu"
     "Tuvalu")
    ( 158 "TWN" "TW" t    
     "Province de Chine Taïwan"
     "Province of China Taiwan")
    ( 834 "TZA" "TZ" t    
     "République-Unie de Tanzanie"
     "United Republic of Tanzania")
    ( 804 "UKR" "UA" t    
     "Ukraine"
     "Ukraine")
    ( 800 "UGA" "UG" t    
     "Ouganda"
     "Uganda")
    ( 581 "UMI" "UM" t    
     "Îles Mineures Éloignées des États-Unis"
     "United States Minor Outlying Islands")
    ( 840 "USA" "US" t    
     "États-Unis"
     "United States of America")
    ( 858 "URY" "UY" t    
     "Uruguay"
     "Uruguay")
    ( 860 "UZB" "UZ" t    
     "Ouzbékistan"
     "Uzbekistan")
    ( 336 "VAT" "VA" t    
     "Saint-Siège (État de La Cité Du Vatican)"
     "Holy See (Vatican City State)")
    ( 670 "VCT" "VC" t    
     "Saint-Vincent et Les Grenadines"
     "Saint Vincent and The Grenadines")
    ( 862 "VEN" "VE" t    
     "Venezuela"
     "Venezuela")
    ( 092 "VGB" "VG" t    
     "Îles Vierges Britanniques"
     "British Virgin Islands")
    ( 850 "VIR" "VI" t    
     "Îles Vierges Des États-Unis"
     "United States Virgin Islands")
    ( 704 "VNM" "VN" t    
     "Viet Nam"
     "Viet Nam")
    ( 548 "VUT" "VU" t    
     "Vanuatu"
     "Vanuatu")
    ( 876 "WLF" "WF" t    
     "Wallis Et Futuna"
     "Wallis and Futuna Islands")
    ( 882 "WSM" "WS" t    
     "Samoa"
     "Samoa")
    ( 000 "NIL" "YD" nil  
     "Yemen Démocratique"
     "Democratic Yemen")
    ( 887 "YEM" "YE" t    
     "Yémen"
     "Yemen")
    ( 175 "MYT" "YT" t    
     "Mayotte"
     "Mayotte")
    ( 000 "NIL" "YU" t    
     "Yougoslavie"
     "Yugoslavia")
    ( 710 "ZAF" "ZA" t    
     "Afrique Du Sud"
     "South Africa")
    ( 894 "ZMB" "ZM" t    
     "Zambie"
     "Zambia")
    ( 000 "NIL" "ZR" t    
     "Zaïre"
     "Zaire")
    ( 716 "ZWE" "ZW" t    
     "Zimbabwe"
     "Zimbabwe")
    ( 000 "NIL" "ZZ" t    
     "Pays inconnu ou non spécifié"
     "Unknown or unspecified country")
    ( 248 "ALA" "AX" t    
     "Îles Aland"
     "Áland Islands")
    )) ;;+COUNTRIES+



(defun get-countries (&key only-existing (language "EN") order)
  "
only-existing:  T <=> filter out codes for countries that don't exist anymore.
language:       EN or FR
                (case insensitive string or symbol whose name is EN or FR)
order:          :NAME or :CODE; if not specified, no ordering is done.
RETURN:         A list of (code name).
"
  (let* ((include-all (not only-existing))
         (result (mapcan (lambda (c)
                           (when (or include-all (c-exists c))
                             (list (list (c-code-2 c)
                                         (if (string-equal "EN" language)
                                             (c-en-name c)
                                             (c-fr-name c))))))
                         +countries+)))
    (when order
      (setq result
            (sort result
                  (if (eq :code order)
                      (lambda (a b) (string-lessp (first  a) (first  b)))
                      (lambda (a b) (string-lessp (second a) (second b)))))))
    result))



;;;; iso3166.lisp                     --                     --          ;;;;
