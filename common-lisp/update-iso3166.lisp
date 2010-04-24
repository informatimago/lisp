;;;; -*- coding:utf-8 -*-

(defparameter new
  '(
    ( 004 AFG AF  T   "Afghanistan")
    ( 248 ALA AX  T   "Áland Islands")
    ( 008 ALB AL  T   "Albania")
    ( 012 DZA DZ  T   "Algeria")
    ( 016 ASM AS  T   "American Samoa")
    ( 020 AND AD  T   "Andorra")
    ( 024 AGO AO  T   "Angola")
    ( 660 AIA AI  T   "Anguilla")
    ( 010 ATA AQ  T   "Antarctica")
    ( 028 ATG AG  T   "Antigua and Barbuda")
    ( 032 ARG AR  T   "Argentina")
    ( 051 ARM AM  T   "Armenia")
    ( 533 ABW AW  T   "Aruba")
    ( 036 AUS AU  T   "Australia")
    ( 040 AUT AT  T   "Austria")
    ( 031 AZE AZ  T   "Azerbaijan")
    ( 044 BHS BS  T   "Bahamas")
    ( 048 BHR BH  T   "Bahrain")
    ( 050 BGD BD  T   "Bangladesh")
    ( 052 BRB BB  T   "Barbados")
    ( 112 BLR BY  T   "Belarus")
    ( 056 BEL BE  T   "Belgium")
    ( 084 BLZ BZ  T   "Belize")
    ( 204 BEN BJ  T   "Benin")
    ( 060 BMU BM  T   "Bermuda")
    ( 064 BTN BT  T   "Bhutan")
    ( 068 BOL BO  T   "Bolivia")
    ( 070 BIH BA  T   "Bosnia and Herzegovina")
    ( 072 BWA BW  T   "Botswana")
    ( 074 BVT BV  T   "Bouvet Island")
    ( 076 BRA BR  T   "Brazil")
    ( 092 IOT IO  T   "British Indian Ocean Territory")
    ( 096 BRN BN  T   "Brunei Darussalam")
    ( 100 BGR BG  T   "Bulgaria")
    ( 854 BFA BF  T   "Burkina Faso")
    ( 108 BDI BI  T   "Burundi")
    ( 116 KHM KH  T   "Cambodia")
    ( 120 CMR CM  T   "Cameroon")
    ( 124 CAN CA  T   "Canada")
    ( 132 CPV CV  T   "Cape Verde")
    ( 136 CYM KY  T   "Cayman Islands")
    ( 140 CAF CF  T   "Central African Republic")
    ( 148 TCD TD  T   "Chad")
    ( 152 CHL CL  T   "Chile")
    ( 156 CHN CN  T   "China, mainland")
    ( 162 CXR CX  T   "Christmas Island")
    ( 166 CCK CC  T   "Cocos (Keeling) Islands")
    ( 170 COL CO  T   "Colombia")
    ( 174 COM KM  T   "Comoros")
    ( 178 COG CG  T   "Congo, Republic of the ")
    ( 180 COD CD  T   "Congo, The Democratic Republic Of The")
    ( 184 COK CK  T   "Cook Islands")
    ( 188 CRI CR  T   "Costa Rica")
    ( 384 CIV CI  T   "Côte d'Ivoire")
    ( 191 HRV HR  T   "Croatia")
    ( 192 CUB CU  T   "Cuba")
    ( 196 CYP CY  T   "Cyprus")
    ( 203 CZE CZ  T   "Czech Republic")
    ( 208 DNK DK  T   "Denmark")
    ( 262 DJI DJ  T   "Djibouti")
    ( 212 DMA DM  T   "Dominica")
    ( 214 DOM DO  T   "Dominican Republic")
    ( 218 ECU EC  T   "Ecuador")
    ( 818 EGY EG  T   "Egypt")
    ( 222 SLV SV  T   "El Salvador")
    ( 226 GNQ GQ  T   "Equatorial Guinea")
    ( 232 ERI ER  T   "Eritrea")
    ( 233 EST EE  T   "Estonia")
    ( 231 ETH ET  T   "Ethiopia")
    ( 238 FLK FK  T   "Falkland Islands")
    ( 234 FRO FO  T   "Faroe Islands")
    ( 242 FJI FJ  T   "Fiji")
    ( 246 FIN FI  T   "Finland")
    ( 250 FRA FR  T   "France")
    ( 254 GUF GF  T   "French Guiana")
    ( 258 PYF PF  T   "French Polynesia")
    ( 260 ATF TF  T   "French Southern Territories")
    ( 266 GAB GA  T   "Gabon")
    ( 270 GMB GM  T   "Gambia")
    ( 268 GEO GE  T   "Georgia")
    ( 276 DEU DE  T   "Germany")
    ( 288 GHA GH  T   "Ghana")
    ( 292 GIB GI  T   "Gibraltar")
    ( 300 GRC GR  T   "Greece")
    ( 304 GRL GL  T   "Greenland")
    ( 308 GRD GD  T   "Grenada")
    ( 312 GLP GP  T   "Guadeloupe")
    ( 316 GUM GU  T   "Guam")
    ( 320 GTM GT  T   "Guatemala")
    ( 324 GIN GN  T   "Guinea")
    ( 624 GNB GW  T   "Guinea-Bissau")
    ( 328 GUY GY  T   "Guyana")
    ( 332 HTI HT  T   "Haiti")
    ( 334 HMD HM  T   "Heard Island and McDonald Islands")
    ( 336 VAT VA  T   "Vatican City State")
    ( 340 HND HN  T   "Honduras")
    ( 344 HKG HK  T   "Hong Kong")
    ( 348 HUN HU  T   "Hungary")
    ( 352 ISL IS  T   "Iceland")
    ( 356 IND IN  T   "India")
    ( 360 IDN ID  T   "Indonesia")
    ( 364 IRN IR  T   "Iran, Islamic Republic of")
    ( 368 IRQ IQ  T   "Iraq")
    ( 372 IRL IE  T   "Ireland, Republic of")
    ( 376 ISR IL  T   "Israel")
    ( 380 ITA IT  T   "Italy")
    ( 388 JAM JM  T   "Jamaica")
    ( 392 JPN JP  T   "Japan")
    ( 400 JOR JO  T   "Jordan")
    ( 398 KAZ KZ  T   "Kazakhstan")
    ( 404 KEN KE  T   "Kenya")
    ( 296 KIR KI  T   "Kiribati")
    ( 408 PRK KP  T   "Korea, Democratic People's Republic of")
    ( 410 KOR KR  T   "Korea, Republic of")
    ( 414 KWT KW  T   "Kuwait")
    ( 417 KGZ KG  T   "Kyrgyzstan")
    ( 418 LAO LA  T   "Lao People's Democratic Republic")
    ( 428 LVA LV  T   "Latvia")
    ( 422 LBN LB  T   "Lebanon")
    ( 426 LSO LS  T   "Lesotho")
    ( 430 LBR LR  T   "Liberia")
    ( 434 LBY LY  T   "Libyan Arab Jamahiriya")
    ( 438 LIE LI  T   "Liechtenstein")
    ( 440 LTU LT  T   "Lithuania")
    ( 442 LUX LU  T   "Luxembourg")
    ( 446 MAC MO  T   "Macao")
    ( 807 MKD MK  T   "Macedonia, The Former Yugoslav Republic of")
    ( 450 MDG MG  T   "Madagascar")
    ( 454 MWI MW  T   "Malawi")
    ( 458 MYS MY  T   "Malaysia")
    ( 462 MDV MV  T   "Maldives")
    ( 466 MLI ML  T   "Mali")
    ( 470 MLT MT  T   "Malta")
    ( 584 MHL MH  T   "Marshall Islands")
    ( 474 MTQ MQ  T   "Martinique")
    ( 478 MRT MR  T   "Mauritania")
    ( 480 MUS MU  T   "Mauritius")
    ( 175 MYT YT  T   "Mayotte")
    ( 484 MEX MX  T   "Mexico")
    ( 583 FSM FM  T   "Micronesia, Federated States of")
    ( 498 MDA MD  T   "Moldova, Republic of")
    ( 492 MCO MC  T   "Monaco")
    ( 496 MNG MN  T   "Mongolia")
    ( 500 MSR MS  T   "Montserrat")
    ( 504 MAR MA  T   "Morocco")
    ( 508 MOZ MZ  T   "Mozambique")
    ( 104 MMR MM  T   "Myanmar")
    ( 516 NAM NA  T   "Namibia")
    ( 520 NRU NR  T   "Nauru")
    ( 524 NPL NP  T   "Nepal")
    ( 528 NLD NL  T   "Netherlands")
    ( 530 ANT AN  T   "Netherlands Antilles")
    ( 540 NCL NC  T   "New Caledonia")
    ( 554 NZL NZ  T   "New Zealand")
    ( 558 NIC NI  T   "Nicaragua")
    ( 562 NER NE  T   "Niger")
    ( 566 NGA NG  T   "Nigeria")
    ( 570 NIU NU  T   "Niue")
    ( 574 NFK NF  T   "Norfolk Island")
    ( 580 MNP MP  T   "Northern Mariana Islands")
    ( 578 NOR NO  T   "Norway")
    ( 512 OMN OM  T   "Oman")
    ( 586 PAK PK  T   "Pakistan")
    ( 585 PLW PW  T   "Palau")
    ( 275 PSE PS  T   "Palestinian Territory, Occupied")
    ( 591 PAN PA  T   "Panama")
    ( 598 PNG PG  T   "Papua New Guinea")
    ( 600 PRY PY  T   "Paraguay")
    ( 604 PER PE  T   "Peru")
    ( 608 PHL PH  T   "Philippines")
    ( 612 PCN PN  T   "Pitcairn")
    ( 616 POL PL  T   "Poland")
    ( 620 PRT PT  T   "Portugal")
    ( 630 PRI PR  T   "Puerto Rico")
    ( 634 QAT QA  T   "Qatar")
    ( 638 REU RE  T   "Réunion")
    ( 642 ROU RO  T   "Romania")
    ( 643 RUS RU  T   "Russian Federation")
    ( 646 RWA RW  T   "Rwanda")
    ( 654 SHN SH  T   "Saint Helena")
    ( 659 KNA KN  T   "Saint Kitts and Nevis")
    ( 662 LCA LC  T   "Saint Lucia")
    ( 666 SPM PM  T   "Saint Pierre and Miquelon")
    ( 670 VCT VC  T   "Saint Vincent and the Grenadines")
    ( 882 WSM WS  T   "Samoa")
    ( 674 SMR SM  T   "San Marino")
    ( 678 STP ST  T   "Sáo Tomó and Príncipe")
    ( 682 SAU SA  T   "Saudi Arabia")
    ( 686 SEN SN  T   "Senegal")
    ( 891 SCG CS  T   "Serbia and Montenegro")
    ( 690 SYC SC  T   "Seychelles")
    ( 694 SLE SL  T   "Sierra Leone")
    ( 702 SGP SG  T   "Singapore")
    ( 703 SVK SK  T   "Slovakia")
    ( 705 SVN SI  T   "Slovenia")
    ( 090 SLB SB  T   "Solomon Islands")
    ( 706 SOM SO  T   "Somalia")
    ( 710 ZAF ZA  T   "South Africa")
    ( 239 SGS GS  T   "South Georgia and the South Sandwich Islands")
    ( 724 ESP ES  T   "Spain")
    ( 144 LKA LK  T   "Sri Lanka")
    ( 736 SDN SD  T   "Sudan")
    ( 740 SUR SR  T   "Suriname")
    ( 744 SJM SJ  T   "Svalbard and Jan Mayen")
    ( 748 SWZ SZ  T   "Swaziland")
    ( 752 SWE SE  T   "Sweden")
    ( 756 CHE CH  T   "Switzerland")
    ( 760 SYR SY  T   "Syrian Arab Republic")
    ( 158 TWN TW  T   "Taiwan (Republic of China)")
    ( 762 TJK TJ  T   "Tajikistan")
    ( 834 TZA TZ  T   "Tanzania, United Republic Of")
    ( 764 THA TH  T   "Thailand")
    ( 626 TLS TL  T   "Timor-Leste")
    ( 768 TGO TG  T   "Togo")
    ( 772 TKL TK  T   "Tokelau")
    ( 776 TON TO  T   "Tonga")
    ( 780 TTO TT  T   "Trinidad and Tobago")
    ( 788 TUN TN  T   "Tunisia")
    ( 792 TUR TR  T   "Turkey")
    ( 795 TKM TM  T   "Turkmenistan")
    ( 796 TCA TC  T   "Turks and Caicos Islands")
    ( 798 TUV TV  T   "Tuvalu")
    ( 800 UGA UG  T   "Uganda")
    ( 804 UKR UA  T   "Ukraine")
    ( 784 ARE AE  T   "United Arab Emirates")
    ( 826 GBR GB  T   "United Kingdom")
    ( 840 USA US  T   "United States")
    ( 581 UMI UM  T   "United States Minor Outlying Islands")
    ( 858 URY UY  T   "Uruguay")
    ( 860 UZB UZ  T   "Uzbekistan")
    ( 548 VUT VU  T   "Vanuatu")
    ( 862 VEN VE  T   "Venezuela")
    ( 704 VNM VN  T   "Viet Nam")
    ( 092 VGB VG  T   "Virgin Islands, British")
    ( 850 VIR VI  T   "Virgin Islands, U.S.")
    ( 876 WLF WF  T   "Wallis and Futuna")
    ( 732 ESH EH  T   "Western Sahara")
    ( 887 YEM YE  T   "Yemen")
    ( 894 ZMB ZM  T   "Zambia")
    ( 716 ZWE ZW  T   "Zimbabwe")
    )
  );;new


(defparameter +COUNTRIES+ 
  '( 
    ( CD NIL
        "La République Démocratique Du Congo"
        "The Democratic Republic Of The Congo")
    ( MK NIL
        "L'Ex-République Yougoslave De Macédoine"
        "The Former Yugoslav Republic Of Macedonia")
    ( PS NIL
        "Territoire Palestinien Occupé"
        "Occupied Palestinian Territory")
    ( TL NIL
        "Timor-Leste"
        "Timor-Leste")
    ( AD T
        "Andorre"
        "Andorra")
    ( AE T
        "Émirats Arabes Unis"
        "United Arab Emirates")
    ( AF T
        "Afghanistan"
        "Afghanistan")
    ( AG T
        "Antigua et Barbuda"
        "Antigua and Barbuda")
    ( AI T
        "Anguilla"
        "Anguilla")
    ( AL T
        "Albanie"
        "Albania")
    ( AM T
        "Arménie"
        "Armenia")
    ( AN T
        "Antilles Néerlandaises"
        "Netherlands Antilles")
    ( AO T
        "Angola"
        "Angola")
    ( AQ T
        "Antarctique"
        "Antarctica")
    ( AR T
        "Argentine"
        "Argentina")
    ( AS T
        "Samoa Américaines"
        "American Samoa")
    ( AT T
        "Autriche"
        "Austria")
    ( AU T
        "Australie"
        "Australia")
    ( AW T
        "Aruba"
        "Aruba")
    ( AZ T
        "Azerbaïdjan"
        "Azerbaijan")
    ( BA T
        "Bosnie-Herzégovine"
        "Bosnia and Herzegovina")
    ( BB T
        "Barbade"
        "Barbados")
    ( BD T
        "Bangladesh"
        "Bangladesh")
    ( BE T
        "Belgique"
        "Belgium")
    ( BF T
        "Burkina Faso"
        "Burkina Faso")
    ( BG T
        "Bulgarie"
        "Bulgaria")
    ( BH T
        "Bahreïn"
        "Bahrain")
    ( BI T
        "Burundi"
        "Burundi")
    ( BJ T
        "Bénin"
        "Benin")
    ( BM T
        "Bermudes"
        "Bermuda")
    ( BN T
        "Brunéi Darussalam"
        "Brunei Darussalam")
    ( BO T
        "Bolivie"
        "Bolivia")
    ( BR T
        "Brésil"
        "Brazil")
    ( BS T
        "Bahamas"
        "Bahamas")
    ( BT T
        "Bhoutan"
        "Bhutan")
    ( BU NIL
        "Burma"
        "Burma")
    ( BV T
        "Île Bouvet"
        "Bouvet Island")
    ( BW T
        "Botswana"
        "Botswana")
    ( BY T
        "Bélarus"
        "Belarus")
    ( BZ T
        "Belize"
        "Belize")
    ( CA T
        "Canada"
        "Canada")
    ( CC T
        "Îles Cocos ( Keeling)"
        "Cocos ( Keeling) Islands")
    ( CF T
        "République Centrafricaine"
        "Central African Republic")
    ( CG NIL
        "Congo"
        "Congo")
    ( CH T
        "Suisse"
        "Switzerland")
    ( CI T
        "Côte D'Ivoire"
        "Ivory Coast")
    ( CK T
        "Îles Cook"
        "Cook Islands")
    ( CL T
        "Chili"
        "Chile")
    ( CM T
        "Cameroun"
        "Cameroon")
    ( CN T
        "Chine"
        "China")
    ( CO T
        "Colombie"
        "Colombia")
    ( CR T
        "Costa Rica"
        "Costa Rica")
    ( CS NIL
        "Serbie et Monténégro"
        "Serbia and Montenegro")
    ( CU T
        "Cuba"
        "Cuba")
    ( CV T
        "Cap-Vert"
        "Cape Verde")
    ( CX T
        "Île Christmas"
        "Christmas Island")
    ( CY T
        "Chypre"
        "Cyprus")
    ( CZ T
        "République Tchèque"
        "Czech Republic")
    ( DD NIL
        "République Démocratique d'Allemagne"
        "German Democratic Republic")
    ( DE T
        "Allemagne"
        "Germany")
    ( DJ T
        "Djibouti"
        "Djibouti")
    ( DK T
        "Danemark"
        "Denmark")
    ( DM T
        "Dominique"
        "Dominica")
    ( DO T
        "République Dominicaine"
      "Dominican Republic")
    ( DZ T
        "Algérie"
        "Algeria")
    ( EC T
        "Équateur"
        "Ecuador")
    ( EE T
        "Estonie"
        "Estonia")
    ( EG T
        "Égypte"
        "Egypt")
    ( EH T
        "Sahara Occidental"
        "Western Sahara")
    ( ER T
        "Érythrée"
        "Eritrea")
    ( ES T
        "Espagne"
        "Spain")
    ( ET T
        "Éthiopie"
        "Ethiopia")
    ( FI T
        "Finlande"
        "Finland")
    ( FJ T
        "Fidji"
        "Fiji")
    ( FK T
        "Îles Malouines"
        "Falkland Islands")
    ( FM T
        "États Fédérés De Micronésie"
        "Federated States Of Micronesia")
    ( FO T
        "Îles Féroé"
        "Faroe Islands")
    ( FR T
        "France"
        "France")
    ( FX T
        "France Métropolitaine"
        "Metropolitan France")
    ( GA T
        "Gabon"
        "Gabon")
    ( GB T
        "Royaume-Uni"
        "United Kingdom")
    ( GD T
        "Grenade"
        "Grenada")
    ( GE T
        "Géorgie"
        "Georgia")
    ( GF T
        "Guyane Française"
        "French Guiana")
    ( GH T
        "Ghana"
        "Ghana")
    ( GI T
        "Gibraltar"
        "Gibraltar")
    ( GL T
        "Groenland"
        "Greenland")
    ( GM T
        "Gambie"
        "Gambia")
    ( GN T
        "Guinée"
        "Guinea")
    ( GP T
        "Guadeloupe"
        "Guadeloupe")
    ( GQ T
        "Guinée Équatoriale"
        "Equatorial Guinea")
    ( GR T
        "Grèce"
        "Greece")
    ( GS T
        "Géorgie Du Sud et Les Îles Sandwich Du Sud"
        "South Georgia and the South Sandwich Islands")
    ( GT T
        "Guatemala"
        "Guatemala")
    ( GU T
        "Guam"
        "Guam")
    ( GW T
        "Guinée-Bissau"
        "Guinea-Bissau")
    ( GY T
        "Guyana"
        "Guyana")
    ( HK T
        "Hong-Kong"
        "Hong Kong")
    ( HM T
        "Île Mcdonald et îles Heard"
        "Heard Island and Mcdonald Islands")
    ( HN T
        "Honduras"
        "Honduras")
    ( HR T
        "Croatie"
        "Croatia")
    ( HT T
        "Haïti"
        "Haiti")
    ( HU T
        "Hongrie"
        "Hungary")
    ( ID T
        "Indonésie"
        "Indonesia")
    ( IE T
        "Irlande"
        "Ireland")
    ( IL T
        "Israël"
        "Israel")
    ( IN T
        "Inde"
        "India")
    ( IO T
        "Territoire Britannique de l'Océan Indien"
        "British Indian Ocean Territory")
    ( IQ T
        "Iraq"
        "Iraq")
    ( IR T
        "République Islamique D' Iran"
        "Islamic Republic of Iran")
    ( IS T
        "Islande"
        "Iceland")
    ( IT T
        "Italie"
        "Italy")
    ( JM T
        "Jamaïque"
        "Jamaica")
    ( JO T
        "Jordanie"
        "Jordan")
    ( JP T
        "Japon"
        "Japan")
    ( KE T
        "Kenya"
        "Kenya")
    ( KG T
        "Kirghizistan"
        "Kyrgyzstan")
    ( KH T
        "Cambodge"
        "Cambodia")
    ( KI T
        "Kiribati"
        "Kiribati")
    ( KM T
        "Comores"
        "Comoros")
    ( KN T
        "Saint-Kitts et Nevis"
        "Saint Kitts and Nevis")
    ( KP T
        "République Populaire Démocratique De Corée"
        "Democratic People's Republic of Korea")
    ( KR T
        "République De Corée"
        "Republic of Korea")
    ( KW T
        "Koweït"
        "Kuwait")
    ( KY T
        "Îles Caïmanes"
        "Cayman Islands")
    ( KZ T
        "Kazakhstan"
        "Kazakhstan")
    ( LA T
        "République Démocratique Populaire Lao"
        "Lao People's Democratic Republic")
    ( LB T
        "Liban"
        "Lebanon")
    ( LC T
        "Sainte-Lucie"
        "Saint Lucia")
    ( LI T
        "Liechtenstein"
        "Liechtenstein")
    ( LK T
        "Sri Lanka"
        "Sri Lanka")
    ( LR T
        "Libéria"
        "Liberia")
    ( LS T
        "Lesotho"
        "Lesotho")
    ( LT T
        "Lituanie"
        "Lithuania")
    ( LU T
        "Luxembourg"
        "Luxembourg")
    ( LV T
        "Lettonie"
        "Latvia")
    ( LY T
        "Jamahiriya Arabe Libyenne"
        "Libyan Arab Jamahiriya")
    ( MA T
        "Maroc"
        "Morocco")
    ( MC T
        "Monaco"
        "Monaco")
    ( MD T
        "République De Moldova"
        "Republic of Moldova")
    ( MG T
        "Madagascar"
        "Madagascar")
    ( MH T
        "Îles Marshall"
        "Marshall Islands")
    ( ML T
        "Mali"
        "Mali")
    ( MM T
        "Myanmar"
        "Myanmar")
    ( MN T
        "Mongolie"
        "Mongolia")
    ( MO T
        "Macao"
        "Macao"
        "Macau")
    ( MP T
        "Îles Mariannes Du Nord"
        "Northern Mariana Islands")
    ( MQ T
        "Martinique"
        "Martinique")
    ( MR T
        "Mauritanie"
        "Mauritania")
    ( MS T
        "Montserrat"
        "Montserrat"
        "Monserrat")
    ( MT T
        "Malte"
        "Malta")
    ( MU T
        "Maurice"
        "Mauritius")
    ( MV T
        "Maldives"
        "Maldives")
    ( MW T
        "Malawi"
        "Malawi")
    ( MX T
        "Mexique"
        "Mexico")
    ( MY T
        "Malaisie"
        "Malaysia")
    ( MZ T
        "Mozambique"
        "Mozambique")
    ( NA T
        "Namibie"
        "Namibia"
        "Nambia")
    ( NC T
        "Nouvelle-Calédonie"
        "New Caledonia")
    ( NE T
        "Niger"
        "Niger")
    ( NF T
        "Île Norfolk"
        "Norfolk Island")
    ( NG T
        "Nigéria"
        "Nigeria")
    ( NI T
        "Nicaragua"
        "Nicaragua")
    ( NL T
        "Pays-Bas"
        "Netherlands")
    ( NO T
        "Norvège"
        "Norway")
    ( NP T
        "Népal"
        "Nepal")
    ( NR T
        "Nauru"
        "Nauru")
    ( NT NIL
        "Nil"
        "Nil"
        "Neutral Zone")
    ( NU T
        "Niué"
        "Niue")
    ( NZ T
        "Nouvelle-Zélande"
        "New Zealand")
    ( OM T
        "Oman"
        "Oman")
    ( PA T
        "Panama"
        "Panama")
    ( PE T
        "Pérou"
        "Peru")
    ( PF T
        "Polynésie Française"
        "French Polynesia")
    ( PG T
        "Papouasie-Nouvelle-Guinée"
        "Papua New Guinea")
    ( PH T
        "Philippines"
        "Philippines")
    ( PK T
        "Pakistan"
        "Pakistan")
    ( PL T
        "Pologne"
        "Poland")
    ( PM T
        "Saint-Pierre et Miquelon"
        "Saint Pierre and Miquelon")
    ( PN T
        "Pitcairn"
        "Pitcairn")
    ( PR T
        "Porto Rico"
        "Puerto Rico")
    ( PT T
        "Portugal"
        "Portugal")
    ( PW T
        "Palaos"
        "Palau")
    ( PY T
        "Paraguay"
        "Paraguay")
    ( QA T
        "Qatar"
        "Qatar")
    ( RE T
        "Réunion"
        "Reunion")
    ( RO T
        "Roumanie"
        "Romania")
    ( RU T
        "Fédération De Russie"
        "Russian Federation")
    ( RW T
        "Rwanda"
        "Rwanda")
    ( SA T
        "Arabie Saoudite"
        "Saudi Arabia")
    ( SB T
        "Îles Salomon"
        "Solomon Islands")
    ( SC T
        "Seychelles"
        "Seychelles")
    ( SD T
        "Soudan"
        "Sudan")
    ( SE T
        "Suède"
        "Sweden")
    ( SG T
        "Singapour"
        "Singapore")
    ( SH T
        "Sainte-Hélène"
        "Saint Helena")
    ( SI T
        "Slovénie"
        "Slovenia")
    ( SJ T
        "Svalbard et île Jan Mayen"
        "Svalbard and Jan Mayen Islands")
    ( SK T
        "Slovaquie"
        "Slovakia")
    ( SL T
        "Sierra Leone"
        "Sierra Leone")
    ( SM T
        "Saint-Marin"
        "San Marino")
    ( SN T
        "Sénégal"
        "Senegal")
    ( SO T
        "Somalie"
        "Somalia")
    ( SR T
        "Suriname"
        "Suriname")
    ( ST T
        "Sao Tomé et Principe"
        "Sao Tome and Principe")
    ( SU NIL
        "Union des Républiques Socialistes Soviétiques"
        "Union of Soviet Socialist Republics")
    ( SV T
        "El Salvador"
        "El Salvador")
    ( SY T
        "République Arabe Syrienne"
        "Syrian Arab Republic")
    ( SZ T
        "Swaziland"
        "Swaziland")
    ( TC T
        "Îles Turks et Caïques"
        "Turks and Caicos Islands")
    ( TD T
        "Tchad"
        "Chad")
    ( TF T
        "Terres Australes Françaises"
        "French Southern Territories")
    ( TG T
        "Togo"
        "Togo")
    ( TH T
        "Thaïlande"
        "Thailand")
    ( TJ T
        "Tadjikistan"
        "Tajikistan")
    ( TK T
        "Tokelau"
        "Tokelau")
    ( TM T
        "Turkménistan"
        "Turkmenistan")
    ( TN T
        "Tunisie"
        "Tunisia")
    ( TO T
        "Tonga"
        "Tonga")
    ( TP T
        "Timor Oriental"
        "East Timor")
    ( TR T
        "Turquie"
        "Turkey")
    ( TT T
        "Trinité et Tobago"
        "Trinidad and Tobago")
    ( TV T
        "Tuvalu"
        "Tuvalu")
    ( TW T
        "Province de Chine Taïwan"
        "Province of China Taiwan")
    ( TZ T
        "République-Unie de Tanzanie"
        "United Republic of Tanzania")
    ( UA T
        "Ukraine"
        "Ukraine")
    ( UG T
        "Ouganda"
        "Uganda")
    ( UM T
        "Îles Mineures Éloignées des États-Unis"
        "United States Minor Outlying Islands")
    ( US T
        "États-Unis"
        "United States of America")
    ( UY T
        "Uruguay"
        "Uruguay")
    ( UZ T
        "Ouzbékistan"
        "Uzbekistan")
    ( VA T
        "Saint-Siège ( État de La Cité Du Vatican)"
        "Holy See ( Vatican City State)")
    ( VC T
        "Saint-Vincent et Les Grenadines"
        "Saint Vincent and The Grenadines")
    ( VE T
        "Venezuela"
        "Venezuela")
    ( VG T
        "Îles Vierges Britanniques"
        "British Virgin Islands")
    ( VI T
        "Îles Vierges Des États-Unis"
        "United States Virgin Islands")
    ( VN T
        "Viet Nam"
        "Viet Nam")
    ( VU T
        "Vanuatu"
        "Vanuatu")
    ( WF T
        "Wallis Et Futuna"
        "Wallis and Futuna Islands")
    ( WS T
        "Samoa"
        "Samoa")
    ( YD NIL
        "Yemen Démocratique"
        "Democratic Yemen")
    ( YE T
        "Yémen"
        "Yemen")
    ( YT T
        "Mayotte"
        "Mayotte")
    ( YU T
        "Yougoslavie"
        "Yugoslavia")
    ( ZA T
        "Afrique Du Sud"
        "South Africa")
    ( ZM T
        "Zambie"
        "Zambia")
    ( ZR T
        "Zaïre"
        "Zaire")
    ( ZW T
        "Zimbabwe"
        "Zimbabwe")
    ( ZZ T
        "Pays inconnu ou non spécifié"
        "Unknown or unspecified country")
    ));;+COUNTRIES+


(defun merge-countries ()
  (let ((newrest (copy-seq new)))
    (map nil
         (lambda (country)
           (let ((newc (car (member (first country) new
                                   :test (function eq)
                                   :key (function third)))))
             (apply (function format) t "( ~3,'0D ~3A ~2A  ~3A  ~%~S~%~S)~%"
                      (if (null newc)
                        (list 0 nil (first country) (second country)
                              (third country) (fourth country))
                        (progn
                          (setf newrest (delete newc newrest
                                                :test (function eq)))
                          (if (string-equal (fifth newc) (fourth country))
                            (list (first newc) (second newc) (third newc) t
                                  (third country) (fourth country))
                            (list (first newc) (second newc) (third newc) t
                                  (third country)
                                  (concatenate 'string (fourth country)
                                              " ## " (fifth newc)))))))))
         +countries+)
    (map nil
         (lambda (newc)
           (apply (function format) t "( ~3,'0D ~3A ~2A  ~3A  ~%~S~%~S)~%"
                    (list (first newc) (second newc) (third newc) t
                          "" (fifth newc))))
         newrest)));;merge-countries


;;;; update-iso3166.lisp              -- 2004-03-16 16:50:09 -- pascal   ;;;;
