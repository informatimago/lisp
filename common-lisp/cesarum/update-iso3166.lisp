;;;; -*- mode:lisp; coding:utf-8 -*-

(defparameter new
  '(
    ( 004 afg af  t   "Afghanistan")
    ( 248 ala ax  t   "Áland Islands")
    ( 008 alb al  t   "Albania")
    ( 012 dza dz  t   "Algeria")
    ( 016 asm as  t   "American Samoa")
    ( 020 and ad  t   "Andorra")
    ( 024 ago ao  t   "Angola")
    ( 660 aia ai  t   "Anguilla")
    ( 010 ata aq  t   "Antarctica")
    ( 028 atg ag  t   "Antigua and Barbuda")
    ( 032 arg ar  t   "Argentina")
    ( 051 arm am  t   "Armenia")
    ( 533 abw aw  t   "Aruba")
    ( 036 aus au  t   "Australia")
    ( 040 aut at  t   "Austria")
    ( 031 aze az  t   "Azerbaijan")
    ( 044 bhs bs  t   "Bahamas")
    ( 048 bhr bh  t   "Bahrain")
    ( 050 bgd bd  t   "Bangladesh")
    ( 052 brb bb  t   "Barbados")
    ( 112 blr by  t   "Belarus")
    ( 056 bel be  t   "Belgium")
    ( 084 blz bz  t   "Belize")
    ( 204 ben bj  t   "Benin")
    ( 060 bmu bm  t   "Bermuda")
    ( 064 btn bt  t   "Bhutan")
    ( 068 bol bo  t   "Bolivia")
    ( 070 bih ba  t   "Bosnia and Herzegovina")
    ( 072 bwa bw  t   "Botswana")
    ( 074 bvt bv  t   "Bouvet Island")
    ( 076 bra br  t   "Brazil")
    ( 092 iot io  t   "British Indian Ocean Territory")
    ( 096 brn bn  t   "Brunei Darussalam")
    ( 100 bgr bg  t   "Bulgaria")
    ( 854 bfa bf  t   "Burkina Faso")
    ( 108 bdi bi  t   "Burundi")
    ( 116 khm kh  t   "Cambodia")
    ( 120 cmr cm  t   "Cameroon")
    ( 124 can ca  t   "Canada")
    ( 132 cpv cv  t   "Cape Verde")
    ( 136 cym ky  t   "Cayman Islands")
    ( 140 caf cf  t   "Central African Republic")
    ( 148 tcd td  t   "Chad")
    ( 152 chl cl  t   "Chile")
    ( 156 chn cn  t   "China, mainland")
    ( 162 cxr cx  t   "Christmas Island")
    ( 166 cck cc  t   "Cocos (Keeling) Islands")
    ( 170 col co  t   "Colombia")
    ( 174 com km  t   "Comoros")
    ( 178 cog cg  t   "Congo, Republic of the ")
    ( 180 cod cd  t   "Congo, The Democratic Republic Of The")
    ( 184 cok ck  t   "Cook Islands")
    ( 188 cri cr  t   "Costa Rica")
    ( 384 civ ci  t   "Côte d'Ivoire")
    ( 191 hrv hr  t   "Croatia")
    ( 192 cub cu  t   "Cuba")
    ( 196 cyp cy  t   "Cyprus")
    ( 203 cze cz  t   "Czech Republic")
    ( 208 dnk dk  t   "Denmark")
    ( 262 dji dj  t   "Djibouti")
    ( 212 dma dm  t   "Dominica")
    ( 214 dom do  t   "Dominican Republic")
    ( 218 ecu ec  t   "Ecuador")
    ( 818 egy eg  t   "Egypt")
    ( 222 slv sv  t   "El Salvador")
    ( 226 gnq gq  t   "Equatorial Guinea")
    ( 232 eri er  t   "Eritrea")
    ( 233 est ee  t   "Estonia")
    ( 231 eth et  t   "Ethiopia")
    ( 238 flk fk  t   "Falkland Islands")
    ( 234 fro fo  t   "Faroe Islands")
    ( 242 fji fj  t   "Fiji")
    ( 246 fin fi  t   "Finland")
    ( 250 fra fr  t   "France")
    ( 254 guf gf  t   "French Guiana")
    ( 258 pyf pf  t   "French Polynesia")
    ( 260 atf tf  t   "French Southern Territories")
    ( 266 gab ga  t   "Gabon")
    ( 270 gmb gm  t   "Gambia")
    ( 268 geo ge  t   "Georgia")
    ( 276 deu de  t   "Germany")
    ( 288 gha gh  t   "Ghana")
    ( 292 gib gi  t   "Gibraltar")
    ( 300 grc gr  t   "Greece")
    ( 304 grl gl  t   "Greenland")
    ( 308 grd gd  t   "Grenada")
    ( 312 glp gp  t   "Guadeloupe")
    ( 316 gum gu  t   "Guam")
    ( 320 gtm gt  t   "Guatemala")
    ( 324 gin gn  t   "Guinea")
    ( 624 gnb gw  t   "Guinea-Bissau")
    ( 328 guy gy  t   "Guyana")
    ( 332 hti ht  t   "Haiti")
    ( 334 hmd hm  t   "Heard Island and McDonald Islands")
    ( 336 vat va  t   "Vatican City State")
    ( 340 hnd hn  t   "Honduras")
    ( 344 hkg hk  t   "Hong Kong")
    ( 348 hun hu  t   "Hungary")
    ( 352 isl is  t   "Iceland")
    ( 356 ind in  t   "India")
    ( 360 idn id  t   "Indonesia")
    ( 364 irn ir  t   "Iran, Islamic Republic of")
    ( 368 irq iq  t   "Iraq")
    ( 372 irl ie  t   "Ireland, Republic of")
    ( 376 isr il  t   "Israel")
    ( 380 ita it  t   "Italy")
    ( 388 jam jm  t   "Jamaica")
    ( 392 jpn jp  t   "Japan")
    ( 400 jor jo  t   "Jordan")
    ( 398 kaz kz  t   "Kazakhstan")
    ( 404 ken ke  t   "Kenya")
    ( 296 kir ki  t   "Kiribati")
    ( 408 prk kp  t   "Korea, Democratic People's Republic of")
    ( 410 kor kr  t   "Korea, Republic of")
    ( 414 kwt kw  t   "Kuwait")
    ( 417 kgz kg  t   "Kyrgyzstan")
    ( 418 lao la  t   "Lao People's Democratic Republic")
    ( 428 lva lv  t   "Latvia")
    ( 422 lbn lb  t   "Lebanon")
    ( 426 lso ls  t   "Lesotho")
    ( 430 lbr lr  t   "Liberia")
    ( 434 lby ly  t   "Libyan Arab Jamahiriya")
    ( 438 lie li  t   "Liechtenstein")
    ( 440 ltu lt  t   "Lithuania")
    ( 442 lux lu  t   "Luxembourg")
    ( 446 mac mo  t   "Macao")
    ( 807 mkd mk  t   "Macedonia, The Former Yugoslav Republic of")
    ( 450 mdg mg  t   "Madagascar")
    ( 454 mwi mw  t   "Malawi")
    ( 458 mys my  t   "Malaysia")
    ( 462 mdv mv  t   "Maldives")
    ( 466 mli ml  t   "Mali")
    ( 470 mlt mt  t   "Malta")
    ( 584 mhl mh  t   "Marshall Islands")
    ( 474 mtq mq  t   "Martinique")
    ( 478 mrt mr  t   "Mauritania")
    ( 480 mus mu  t   "Mauritius")
    ( 175 myt yt  t   "Mayotte")
    ( 484 mex mx  t   "Mexico")
    ( 583 fsm fm  t   "Micronesia, Federated States of")
    ( 498 mda md  t   "Moldova, Republic of")
    ( 492 mco mc  t   "Monaco")
    ( 496 mng mn  t   "Mongolia")
    ( 500 msr ms  t   "Montserrat")
    ( 504 mar ma  t   "Morocco")
    ( 508 moz mz  t   "Mozambique")
    ( 104 mmr mm  t   "Myanmar")
    ( 516 nam na  t   "Namibia")
    ( 520 nru nr  t   "Nauru")
    ( 524 npl np  t   "Nepal")
    ( 528 nld nl  t   "Netherlands")
    ( 530 ant an  t   "Netherlands Antilles")
    ( 540 ncl nc  t   "New Caledonia")
    ( 554 nzl nz  t   "New Zealand")
    ( 558 nic ni  t   "Nicaragua")
    ( 562 ner ne  t   "Niger")
    ( 566 nga ng  t   "Nigeria")
    ( 570 niu nu  t   "Niue")
    ( 574 nfk nf  t   "Norfolk Island")
    ( 580 mnp mp  t   "Northern Mariana Islands")
    ( 578 nor no  t   "Norway")
    ( 512 omn om  t   "Oman")
    ( 586 pak pk  t   "Pakistan")
    ( 585 plw pw  t   "Palau")
    ( 275 pse ps  t   "Palestinian Territory, Occupied")
    ( 591 pan pa  t   "Panama")
    ( 598 png pg  t   "Papua New Guinea")
    ( 600 pry py  t   "Paraguay")
    ( 604 per pe  t   "Peru")
    ( 608 phl ph  t   "Philippines")
    ( 612 pcn pn  t   "Pitcairn")
    ( 616 pol pl  t   "Poland")
    ( 620 prt pt  t   "Portugal")
    ( 630 pri pr  t   "Puerto Rico")
    ( 634 qat qa  t   "Qatar")
    ( 638 reu re  t   "Réunion")
    ( 642 rou ro  t   "Romania")
    ( 643 rus ru  t   "Russian Federation")
    ( 646 rwa rw  t   "Rwanda")
    ( 654 shn sh  t   "Saint Helena")
    ( 659 kna kn  t   "Saint Kitts and Nevis")
    ( 662 lca lc  t   "Saint Lucia")
    ( 666 spm pm  t   "Saint Pierre and Miquelon")
    ( 670 vct vc  t   "Saint Vincent and the Grenadines")
    ( 882 wsm ws  t   "Samoa")
    ( 674 smr sm  t   "San Marino")
    ( 678 stp st  t   "Sáo Tomó and Príncipe")
    ( 682 sau sa  t   "Saudi Arabia")
    ( 686 sen sn  t   "Senegal")
    ( 891 scg cs  t   "Serbia and Montenegro")
    ( 690 syc sc  t   "Seychelles")
    ( 694 sle sl  t   "Sierra Leone")
    ( 702 sgp sg  t   "Singapore")
    ( 703 svk sk  t   "Slovakia")
    ( 705 svn si  t   "Slovenia")
    ( 090 slb sb  t   "Solomon Islands")
    ( 706 som so  t   "Somalia")
    ( 710 zaf za  t   "South Africa")
    ( 239 sgs gs  t   "South Georgia and the South Sandwich Islands")
    ( 724 esp es  t   "Spain")
    ( 144 lka lk  t   "Sri Lanka")
    ( 736 sdn sd  t   "Sudan")
    ( 740 sur sr  t   "Suriname")
    ( 744 sjm sj  t   "Svalbard and Jan Mayen")
    ( 748 swz sz  t   "Swaziland")
    ( 752 swe se  t   "Sweden")
    ( 756 che ch  t   "Switzerland")
    ( 760 syr sy  t   "Syrian Arab Republic")
    ( 158 twn tw  t   "Taiwan (Republic of China)")
    ( 762 tjk tj  t   "Tajikistan")
    ( 834 tza tz  t   "Tanzania, United Republic Of")
    ( 764 tha th  t   "Thailand")
    ( 626 tls tl  t   "Timor-Leste")
    ( 768 tgo tg  t   "Togo")
    ( 772 tkl tk  t   "Tokelau")
    ( 776 ton to  t   "Tonga")
    ( 780 tto tt  t   "Trinidad and Tobago")
    ( 788 tun tn  t   "Tunisia")
    ( 792 tur tr  t   "Turkey")
    ( 795 tkm tm  t   "Turkmenistan")
    ( 796 tca tc  t   "Turks and Caicos Islands")
    ( 798 tuv tv  t   "Tuvalu")
    ( 800 uga ug  t   "Uganda")
    ( 804 ukr ua  t   "Ukraine")
    ( 784 are ae  t   "United Arab Emirates")
    ( 826 gbr gb  t   "United Kingdom")
    ( 840 usa us  t   "United States")
    ( 581 umi um  t   "United States Minor Outlying Islands")
    ( 858 ury uy  t   "Uruguay")
    ( 860 uzb uz  t   "Uzbekistan")
    ( 548 vut vu  t   "Vanuatu")
    ( 862 ven ve  t   "Venezuela")
    ( 704 vnm vn  t   "Viet Nam")
    ( 092 vgb vg  t   "Virgin Islands, British")
    ( 850 vir vi  t   "Virgin Islands, U.S.")
    ( 876 wlf wf  t   "Wallis and Futuna")
    ( 732 esh eh  t   "Western Sahara")
    ( 887 yem ye  t   "Yemen")
    ( 894 zmb zm  t   "Zambia")
    ( 716 zwe zw  t   "Zimbabwe")
    )
  );;new


(defparameter +countries+ 
  '( 
    ( cd nil
        "La République Démocratique Du Congo"
        "The Democratic Republic Of The Congo")
    ( mk nil
        "L'Ex-République Yougoslave De Macédoine"
        "The Former Yugoslav Republic Of Macedonia")
    ( ps nil
        "Territoire Palestinien Occupé"
        "Occupied Palestinian Territory")
    ( tl nil
        "Timor-Leste"
        "Timor-Leste")
    ( ad t
        "Andorre"
        "Andorra")
    ( ae t
        "Émirats Arabes Unis"
        "United Arab Emirates")
    ( af t
        "Afghanistan"
        "Afghanistan")
    ( ag t
        "Antigua et Barbuda"
        "Antigua and Barbuda")
    ( ai t
        "Anguilla"
        "Anguilla")
    ( al t
        "Albanie"
        "Albania")
    ( am t
        "Arménie"
        "Armenia")
    ( an t
        "Antilles Néerlandaises"
        "Netherlands Antilles")
    ( ao t
        "Angola"
        "Angola")
    ( aq t
        "Antarctique"
        "Antarctica")
    ( ar t
        "Argentine"
        "Argentina")
    ( as t
        "Samoa Américaines"
        "American Samoa")
    ( at t
        "Autriche"
        "Austria")
    ( au t
        "Australie"
        "Australia")
    ( aw t
        "Aruba"
        "Aruba")
    ( az t
        "Azerbaïdjan"
        "Azerbaijan")
    ( ba t
        "Bosnie-Herzégovine"
        "Bosnia and Herzegovina")
    ( bb t
        "Barbade"
        "Barbados")
    ( bd t
        "Bangladesh"
        "Bangladesh")
    ( be t
        "Belgique"
        "Belgium")
    ( bf t
        "Burkina Faso"
        "Burkina Faso")
    ( bg t
        "Bulgarie"
        "Bulgaria")
    ( bh t
        "Bahreïn"
        "Bahrain")
    ( bi t
        "Burundi"
        "Burundi")
    ( bj t
        "Bénin"
        "Benin")
    ( bm t
        "Bermudes"
        "Bermuda")
    ( bn t
        "Brunéi Darussalam"
        "Brunei Darussalam")
    ( bo t
        "Bolivie"
        "Bolivia")
    ( br t
        "Brésil"
        "Brazil")
    ( bs t
        "Bahamas"
        "Bahamas")
    ( bt t
        "Bhoutan"
        "Bhutan")
    ( bu nil
        "Burma"
        "Burma")
    ( bv t
        "Île Bouvet"
        "Bouvet Island")
    ( bw t
        "Botswana"
        "Botswana")
    ( by t
        "Bélarus"
        "Belarus")
    ( bz t
        "Belize"
        "Belize")
    ( ca t
        "Canada"
        "Canada")
    ( cc t
        "Îles Cocos ( Keeling)"
        "Cocos ( Keeling) Islands")
    ( cf t
        "République Centrafricaine"
        "Central African Republic")
    ( cg nil
        "Congo"
        "Congo")
    ( ch t
        "Suisse"
        "Switzerland")
    ( ci t
        "Côte D'Ivoire"
        "Ivory Coast")
    ( ck t
        "Îles Cook"
        "Cook Islands")
    ( cl t
        "Chili"
        "Chile")
    ( cm t
        "Cameroun"
        "Cameroon")
    ( cn t
        "Chine"
        "China")
    ( co t
        "Colombie"
        "Colombia")
    ( cr t
        "Costa Rica"
        "Costa Rica")
    ( cs nil
        "Serbie et Monténégro"
        "Serbia and Montenegro")
    ( cu t
        "Cuba"
        "Cuba")
    ( cv t
        "Cap-Vert"
        "Cape Verde")
    ( cx t
        "Île Christmas"
        "Christmas Island")
    ( cy t
        "Chypre"
        "Cyprus")
    ( cz t
        "République Tchèque"
        "Czech Republic")
    ( dd nil
        "République Démocratique d'Allemagne"
        "German Democratic Republic")
    ( de t
        "Allemagne"
        "Germany")
    ( dj t
        "Djibouti"
        "Djibouti")
    ( dk t
        "Danemark"
        "Denmark")
    ( dm t
        "Dominique"
        "Dominica")
    ( do t
        "République Dominicaine"
      "Dominican Republic")
    ( dz t
        "Algérie"
        "Algeria")
    ( ec t
        "Équateur"
        "Ecuador")
    ( ee t
        "Estonie"
        "Estonia")
    ( eg t
        "Égypte"
        "Egypt")
    ( eh t
        "Sahara Occidental"
        "Western Sahara")
    ( er t
        "Érythrée"
        "Eritrea")
    ( es t
        "Espagne"
        "Spain")
    ( et t
        "Éthiopie"
        "Ethiopia")
    ( fi t
        "Finlande"
        "Finland")
    ( fj t
        "Fidji"
        "Fiji")
    ( fk t
        "Îles Malouines"
        "Falkland Islands")
    ( fm t
        "États Fédérés De Micronésie"
        "Federated States Of Micronesia")
    ( fo t
        "Îles Féroé"
        "Faroe Islands")
    ( fr t
        "France"
        "France")
    ( fx t
        "France Métropolitaine"
        "Metropolitan France")
    ( ga t
        "Gabon"
        "Gabon")
    ( gb t
        "Royaume-Uni"
        "United Kingdom")
    ( gd t
        "Grenade"
        "Grenada")
    ( ge t
        "Géorgie"
        "Georgia")
    ( gf t
        "Guyane Française"
        "French Guiana")
    ( gh t
        "Ghana"
        "Ghana")
    ( gi t
        "Gibraltar"
        "Gibraltar")
    ( gl t
        "Groenland"
        "Greenland")
    ( gm t
        "Gambie"
        "Gambia")
    ( gn t
        "Guinée"
        "Guinea")
    ( gp t
        "Guadeloupe"
        "Guadeloupe")
    ( gq t
        "Guinée Équatoriale"
        "Equatorial Guinea")
    ( gr t
        "Grèce"
        "Greece")
    ( gs t
        "Géorgie Du Sud et Les Îles Sandwich Du Sud"
        "South Georgia and the South Sandwich Islands")
    ( gt t
        "Guatemala"
        "Guatemala")
    ( gu t
        "Guam"
        "Guam")
    ( gw t
        "Guinée-Bissau"
        "Guinea-Bissau")
    ( gy t
        "Guyana"
        "Guyana")
    ( hk t
        "Hong-Kong"
        "Hong Kong")
    ( hm t
        "Île Mcdonald et îles Heard"
        "Heard Island and Mcdonald Islands")
    ( hn t
        "Honduras"
        "Honduras")
    ( hr t
        "Croatie"
        "Croatia")
    ( ht t
        "Haïti"
        "Haiti")
    ( hu t
        "Hongrie"
        "Hungary")
    ( id t
        "Indonésie"
        "Indonesia")
    ( ie t
        "Irlande"
        "Ireland")
    ( il t
        "Israël"
        "Israel")
    ( in t
        "Inde"
        "India")
    ( io t
        "Territoire Britannique de l'Océan Indien"
        "British Indian Ocean Territory")
    ( iq t
        "Iraq"
        "Iraq")
    ( ir t
        "République Islamique D' Iran"
        "Islamic Republic of Iran")
    ( is t
        "Islande"
        "Iceland")
    ( it t
        "Italie"
        "Italy")
    ( jm t
        "Jamaïque"
        "Jamaica")
    ( jo t
        "Jordanie"
        "Jordan")
    ( jp t
        "Japon"
        "Japan")
    ( ke t
        "Kenya"
        "Kenya")
    ( kg t
        "Kirghizistan"
        "Kyrgyzstan")
    ( kh t
        "Cambodge"
        "Cambodia")
    ( ki t
        "Kiribati"
        "Kiribati")
    ( km t
        "Comores"
        "Comoros")
    ( kn t
        "Saint-Kitts et Nevis"
        "Saint Kitts and Nevis")
    ( kp t
        "République Populaire Démocratique De Corée"
        "Democratic People's Republic of Korea")
    ( kr t
        "République De Corée"
        "Republic of Korea")
    ( kw t
        "Koweït"
        "Kuwait")
    ( ky t
        "Îles Caïmanes"
        "Cayman Islands")
    ( kz t
        "Kazakhstan"
        "Kazakhstan")
    ( la t
        "République Démocratique Populaire Lao"
        "Lao People's Democratic Republic")
    ( lb t
        "Liban"
        "Lebanon")
    ( lc t
        "Sainte-Lucie"
        "Saint Lucia")
    ( li t
        "Liechtenstein"
        "Liechtenstein")
    ( lk t
        "Sri Lanka"
        "Sri Lanka")
    ( lr t
        "Libéria"
        "Liberia")
    ( ls t
        "Lesotho"
        "Lesotho")
    ( lt t
        "Lituanie"
        "Lithuania")
    ( lu t
        "Luxembourg"
        "Luxembourg")
    ( lv t
        "Lettonie"
        "Latvia")
    ( ly t
        "Jamahiriya Arabe Libyenne"
        "Libyan Arab Jamahiriya")
    ( ma t
        "Maroc"
        "Morocco")
    ( mc t
        "Monaco"
        "Monaco")
    ( md t
        "République De Moldova"
        "Republic of Moldova")
    ( mg t
        "Madagascar"
        "Madagascar")
    ( mh t
        "Îles Marshall"
        "Marshall Islands")
    ( ml t
        "Mali"
        "Mali")
    ( mm t
        "Myanmar"
        "Myanmar")
    ( mn t
        "Mongolie"
        "Mongolia")
    ( mo t
        "Macao"
        "Macao"
        "Macau")
    ( mp t
        "Îles Mariannes Du Nord"
        "Northern Mariana Islands")
    ( mq t
        "Martinique"
        "Martinique")
    ( mr t
        "Mauritanie"
        "Mauritania")
    ( ms t
        "Montserrat"
        "Montserrat"
        "Monserrat")
    ( mt t
        "Malte"
        "Malta")
    ( mu t
        "Maurice"
        "Mauritius")
    ( mv t
        "Maldives"
        "Maldives")
    ( mw t
        "Malawi"
        "Malawi")
    ( mx t
        "Mexique"
        "Mexico")
    ( my t
        "Malaisie"
        "Malaysia")
    ( mz t
        "Mozambique"
        "Mozambique")
    ( na t
        "Namibie"
        "Namibia"
        "Nambia")
    ( nc t
        "Nouvelle-Calédonie"
        "New Caledonia")
    ( ne t
        "Niger"
        "Niger")
    ( nf t
        "Île Norfolk"
        "Norfolk Island")
    ( ng t
        "Nigéria"
        "Nigeria")
    ( ni t
        "Nicaragua"
        "Nicaragua")
    ( nl t
        "Pays-Bas"
        "Netherlands")
    ( no t
        "Norvège"
        "Norway")
    ( np t
        "Népal"
        "Nepal")
    ( nr t
        "Nauru"
        "Nauru")
    ( nt nil
        "Nil"
        "Nil"
        "Neutral Zone")
    ( nu t
        "Niué"
        "Niue")
    ( nz t
        "Nouvelle-Zélande"
        "New Zealand")
    ( om t
        "Oman"
        "Oman")
    ( pa t
        "Panama"
        "Panama")
    ( pe t
        "Pérou"
        "Peru")
    ( pf t
        "Polynésie Française"
        "French Polynesia")
    ( pg t
        "Papouasie-Nouvelle-Guinée"
        "Papua New Guinea")
    ( ph t
        "Philippines"
        "Philippines")
    ( pk t
        "Pakistan"
        "Pakistan")
    ( pl t
        "Pologne"
        "Poland")
    ( pm t
        "Saint-Pierre et Miquelon"
        "Saint Pierre and Miquelon")
    ( pn t
        "Pitcairn"
        "Pitcairn")
    ( pr t
        "Porto Rico"
        "Puerto Rico")
    ( pt t
        "Portugal"
        "Portugal")
    ( pw t
        "Palaos"
        "Palau")
    ( py t
        "Paraguay"
        "Paraguay")
    ( qa t
        "Qatar"
        "Qatar")
    ( re t
        "Réunion"
        "Reunion")
    ( ro t
        "Roumanie"
        "Romania")
    ( ru t
        "Fédération De Russie"
        "Russian Federation")
    ( rw t
        "Rwanda"
        "Rwanda")
    ( sa t
        "Arabie Saoudite"
        "Saudi Arabia")
    ( sb t
        "Îles Salomon"
        "Solomon Islands")
    ( sc t
        "Seychelles"
        "Seychelles")
    ( sd t
        "Soudan"
        "Sudan")
    ( se t
        "Suède"
        "Sweden")
    ( sg t
        "Singapour"
        "Singapore")
    ( sh t
        "Sainte-Hélène"
        "Saint Helena")
    ( si t
        "Slovénie"
        "Slovenia")
    ( sj t
        "Svalbard et île Jan Mayen"
        "Svalbard and Jan Mayen Islands")
    ( sk t
        "Slovaquie"
        "Slovakia")
    ( sl t
        "Sierra Leone"
        "Sierra Leone")
    ( sm t
        "Saint-Marin"
        "San Marino")
    ( sn t
        "Sénégal"
        "Senegal")
    ( so t
        "Somalie"
        "Somalia")
    ( sr t
        "Suriname"
        "Suriname")
    ( st t
        "Sao Tomé et Principe"
        "Sao Tome and Principe")
    ( su nil
        "Union des Républiques Socialistes Soviétiques"
        "Union of Soviet Socialist Republics")
    ( sv t
        "El Salvador"
        "El Salvador")
    ( sy t
        "République Arabe Syrienne"
        "Syrian Arab Republic")
    ( sz t
        "Swaziland"
        "Swaziland")
    ( tc t
        "Îles Turks et Caïques"
        "Turks and Caicos Islands")
    ( td t
        "Tchad"
        "Chad")
    ( tf t
        "Terres Australes Françaises"
        "French Southern Territories")
    ( tg t
        "Togo"
        "Togo")
    ( th t
        "Thaïlande"
        "Thailand")
    ( tj t
        "Tadjikistan"
        "Tajikistan")
    ( tk t
        "Tokelau"
        "Tokelau")
    ( tm t
        "Turkménistan"
        "Turkmenistan")
    ( tn t
        "Tunisie"
        "Tunisia")
    ( to t
        "Tonga"
        "Tonga")
    ( tp t
        "Timor Oriental"
        "East Timor")
    ( tr t
        "Turquie"
        "Turkey")
    ( tt t
        "Trinité et Tobago"
        "Trinidad and Tobago")
    ( tv t
        "Tuvalu"
        "Tuvalu")
    ( tw t
        "Province de Chine Taïwan"
        "Province of China Taiwan")
    ( tz t
        "République-Unie de Tanzanie"
        "United Republic of Tanzania")
    ( ua t
        "Ukraine"
        "Ukraine")
    ( ug t
        "Ouganda"
        "Uganda")
    ( um t
        "Îles Mineures Éloignées des États-Unis"
        "United States Minor Outlying Islands")
    ( us t
        "États-Unis"
        "United States of America")
    ( uy t
        "Uruguay"
        "Uruguay")
    ( uz t
        "Ouzbékistan"
        "Uzbekistan")
    ( va t
        "Saint-Siège ( État de La Cité Du Vatican)"
        "Holy See ( Vatican City State)")
    ( vc t
        "Saint-Vincent et Les Grenadines"
        "Saint Vincent and The Grenadines")
    ( ve t
        "Venezuela"
        "Venezuela")
    ( vg t
        "Îles Vierges Britanniques"
        "British Virgin Islands")
    ( vi t
        "Îles Vierges Des États-Unis"
        "United States Virgin Islands")
    ( vn t
        "Viet Nam"
        "Viet Nam")
    ( vu t
        "Vanuatu"
        "Vanuatu")
    ( wf t
        "Wallis Et Futuna"
        "Wallis and Futuna Islands")
    ( ws t
        "Samoa"
        "Samoa")
    ( yd nil
        "Yemen Démocratique"
        "Democratic Yemen")
    ( ye t
        "Yémen"
        "Yemen")
    ( yt t
        "Mayotte"
        "Mayotte")
    ( yu t
        "Yougoslavie"
        "Yugoslavia")
    ( za t
        "Afrique Du Sud"
        "South Africa")
    ( zm t
        "Zambie"
        "Zambia")
    ( zr t
        "Zaïre"
        "Zaire")
    ( zw t
        "Zimbabwe"
        "Zimbabwe")
    ( zz t
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
