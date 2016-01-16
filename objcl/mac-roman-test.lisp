;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mac-roman-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test mac-roman.lisp
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
;;;;**************************************************************************
(defpackage "COM.INFORMATIMAGO.OBJCL.MAC-ROMAN.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.OBJCL.MAC-ROMAN")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.OBJCL.MAC-ROMAN.TEST")

(defparameter *mac-roman-codes*
  '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
    25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
    48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70
    71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93
    94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
    112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 160
    161 162 163 164 165 167 168 169 170 171 172 174 175 176 177 180 181
    182 183 184 186 187 191 192 193 194 195 196 197 198 199 200 201 202
    203 204 205 206 207 209 210 211 212 213 214 216 217 218 219 220 223
    224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 241
    242 243 244 245 246 247 248 249 250 251 252 255 305 338 339 376 402
    710 711 728 729 730 731 732 733 960 8211 8212 8216 8217 8218 8220
    8221 8222 8224 8225 8226 8230 8240 8249 8250 8260 8482 8486 8706
    8710 8719 8721 8730 8734 8747 8776 8800 8804 8805 9674 64257 64258))


(defun equiv (a b) (eq (not a) (not b)))

(defun test/mac-roman-char-p ()
  (assert-true (loop
                 :for code :from 0 :to 65535
                 :always (equiv (member code *mac-roman-codes*)
                                (mac-roman-char-p (code-char code))))))

(define-test test/all ()
  (test/mac-roman-char-p))

;;;; THE END ;;;;
