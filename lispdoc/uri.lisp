;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               uri.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    URI parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-03 <PJB> Extracted from lispdoc.lisp
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
;;;;    
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;    
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;    
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;    
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LISPDOC.URI")


;;; URIs:
;;; http://www.ietf.org/rfc/rfc3986.txt

(define-parse-tree-synonym digit
    (:char-class (:range #\0 #\9)))

(define-parse-tree-synonym alpha
    (:char-class (:range #\A #\Z) (:range #\a #\z)))

(define-parse-tree-synonym alphanum
    (:char-class (:range #\A #\Z) (:range #\a #\z) (:range #\0 #\9)))

(define-parse-tree-synonym hexdig
    (:char-class (:range #\A #\F) (:range #\a #\f) (:range #\0 #\9)))




;; dec-octet     = DIGIT                 ; 0-9
;;               / %x31-39 DIGIT         ; 10-99
;;               / "1" 2DIGIT            ; 100-199
;;               / "2" %x30-34 DIGIT     ; 200-249
;;               / "25" %x30-35          ; 250-255

(define-parse-tree-synonym dec-octet
    (:alternation digit
                  (:sequence (:char-class (:range #\1 #\9)) digit)
                  (:sequence #\1 digit digit)
                  (:sequence #\2 (:char-class (:range #\0 #\4)) digit)
                  (:sequence #\2 #\5 (:char-class (:range #\0 #\5)))))


;; IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet

(define-parse-tree-synonym ipv4address
    (:sequence dec-octet #\. dec-octet #\. dec-octet #\. dec-octet))


;; h16           = 1*4HEXDIG

(define-parse-tree-synonym h16
    (:greedy-repetition 1 4 hexdig))


;; ls32          = ( h16 ":" h16 ) / IPv4address

(define-parse-tree-synonym ls32
    (:alternation (:sequence h16 #\: h16) ipv4address))


;; IPv6address   =                            6( h16 ":" ) ls32
;;               /                       "::" 5( h16 ":" ) ls32
;;               / [               h16 ] "::" 4( h16 ":" ) ls32
;;               / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
;;               / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
;;               / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
;;               / [ *4( h16 ":" ) h16 ] "::"              ls32
;;               / [ *5( h16 ":" ) h16 ] "::"              h16
;;               / [ *6( h16 ":" ) h16 ] "::"

(define-parse-tree-synonym ipv6address
    (:alternation
     (:sequence                                                                                           (:greedy-repetition 6 6 (:sequence h16 #\:)) ls32)
     (:sequence                                                                                   #\: #\: (:greedy-repetition 5 5 (:sequence h16 #\:)) ls32)
     (:sequence (:alternation :void h16)                                                          #\: #\: (:greedy-repetition 4 4 (:sequence h16 #\:)) ls32)
     (:sequence (:alternation :void (:sequence (:greedy-repetition 0 1 (:sequence h16 #\:)) h16)) #\: #\: (:greedy-repetition 3 3 (:sequence h16 #\:)) ls32)
     (:sequence (:alternation :void (:sequence (:greedy-repetition 0 2 (:sequence h16 #\:)) h16)) #\: #\: (:greedy-repetition 2 2 (:sequence h16 #\:)) ls32)
     (:sequence (:alternation :void (:sequence (:greedy-repetition 0 3 (:sequence h16 #\:)) h16)) #\: #\:                         (:sequence h16 #\:)  ls32)
     (:sequence (:alternation :void (:sequence (:greedy-repetition 0 4 (:sequence h16 #\:)) h16)) #\: #\:                                              ls32)
     (:sequence (:alternation :void (:sequence (:greedy-repetition 0 5 (:sequence h16 #\:)) h16)) #\: #\:                                    h16)
     (:sequence (:alternation :void (:sequence (:greedy-repetition 0 6 (:sequence h16 #\:)) h16)) #\: #\:)))


;; IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )

(define-parse-tree-synonym ipvfuture
    (:sequence #\v (:greedy-repetition 1 nil hexdig) #\.  (:greedy-repetition 1 nil (:alternation unreserved sub-delims #\:))))


;; IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"

(define-parse-tree-synonym ip-literal
    (:sequence #\[  (:alternation unreserved ipv6address ipvfuture) #\]))




;; gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"

(define-parse-tree-synonym gen-delims
    (:char-class #\: #\/ #\? #\# #\[ #\] #\@))


;; sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
;;               / "*" / "+" / "," / ";" / "="

(define-parse-tree-synonym sub-delims
    (:char-class #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))


;; reserved      = gen-delims / sub-delims

(define-parse-tree-synonym reserved
    (:alternation gen-delims sub-delims))


;; unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"

(define-parse-tree-synonym unreserved
    (:alternation alpha digit #\- #\. #\_ #\~))


;; pct-encoded   = "%" HEXDIG HEXDIG
(define-parse-tree-synonym pct-encoded
    (:sequence #\% hexdig hexdig))


;; pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

(define-parse-tree-synonym pchar
    (:alternation unreserved pct-encoded sub-delims #\" #\@))




;; segment       = *pchar
;; segment-nz    = 1*pchar
;; segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
;;               ; non-zero-length segment without any colon ":"

(define-parse-tree-synonym segment
    (:greedy-repetition 0 nil pchar))

(define-parse-tree-synonym segment-nz
    (:greedy-repetition 1 nil pchar))

(define-parse-tree-synonym segment-nz-nc
    (:greedy-repetition 1 nil (:alternation unreserved pct-encoded sub-delims #\@)))



;; path-abempty  = *( "/" segment )
;; path-absolute = "/" [ segment-nz *( "/" segment ) ]
;; path-noscheme = segment-nz-nc *( "/" segment )
;; path-rootless = segment-nz *( "/" segment )
;; path-empty    = 0<pchar>

(define-parse-tree-synonym path-abempty
    (:greedy-repetition 0 nil (:sequence #\/ segment)))

(define-parse-tree-synonym path-absolute
    (:greedy-repetition 0 nil
                        (:alternation :void
                                      (:sequence segment-nz
                                                 (:greedy-repetition 0 nil
                                                                     (:sequence #\/ segment))))))

(define-parse-tree-synonym path-noscheme
    (:sequence segment-nz-nc (:greedy-repetition 0 nil (:sequence #\/ segment))))

(define-parse-tree-synonym path-rootless
    (:sequence segment-nz    (:greedy-repetition 0 nil (:sequence #\/ segment))))

(define-parse-tree-synonym path-empty
    :void)


;; path          = path-abempty    ; begins with "/" or is empty
;;               / path-absolute   ; begins with "/" but not "//"
;;               / path-noscheme   ; begins with a non-colon segment
;;               / path-rootless   ; begins with a segment
;;               / path-empty      ; zero characters

(define-parse-tree-synonym path
    (:alternation path-abempty
                  path-absolute
                  path-noscheme
                  path-rootless
                  path-empty))


;; reg-name      = *( unreserved / pct-encoded / sub-delims )

(define-parse-tree-synonym reg-name
    (:greedy-repetition 0 nil (:alternation unreserved pct-encoded sub-delims)))



;; scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )

(define-parse-tree-synonym scheme
    (:sequence alpha (:greedy-repetition 0 nil (:alternation alpha digit #\+ #\- #\.))))

   
;; userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )

(define-parse-tree-synonym userinfo
    (:greedy-repetition 0 nil (:alternation unreserved pct-encoded sub-delims #\:)))


;; host          = IP-literal / IPv4address / reg-name

(define-parse-tree-synonym host
    (:alternation ip-literal ipv4address reg-name))


;; port          = *DIGIT

(define-parse-tree-synonym port
    (:greedy-repetition 0 nil digit))


;; authority     = [ userinfo "@" ] host [ ":" port ]

(define-parse-tree-synonym authority
    (:sequence (:alternation :void (:sequence userinfo #\@))
               host
               (:alternation :void (:sequence #\: port))))



;; query         = *( pchar / "/" / "?" )

(define-parse-tree-synonym query
    (:greedy-repetition 0 nil (:alternation pchar #\/ #\?)))


;; fragment      = *( pchar / "/" / "?" )

(define-parse-tree-synonym fragment
    (:greedy-repetition 0 nil (:alternation pchar #\/ #\?)))


;; relative-part = "//" authority path-abempty
;;               / path-absolute
;;               / path-noscheme
;;               / path-empty

(define-parse-tree-synonym relative-part
    (:alternation (:sequence #\/ #\/ authority path-abempty)
                  path-absolute
                  path-noscheme
                  path-empty))


;; relative-ref  = relative-part [ "?" query ] [ "#" fragment ]

(define-parse-tree-synonym relative-ref
    (:sequence relative-part 
               (:alternation :void (:sequence #\? query))
               (:alternation :void (:sequence #\# fragment))))


;; hier-part     = "//" authority path-abempty
;;               / path-absolute
;;               / path-rootless
;;               / path-empty

(define-parse-tree-synonym hier-part
    (:alternation (:sequence #\/ #\/ authority path-abempty)
                  path-absolute
                  path-rootless
                  path-empty))


;; absolute-URI  = scheme ":" hier-part [ "?" query ]

(define-parse-tree-synonym absolute-uri
    (:sequence scheme #\: hier-part
               (:alternation :void (:sequence #\? query))))



;; URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]

(define-parse-tree-synonym uri
    (:sequence scheme #\: hier-part
               (:alternation :void (:sequence #\? query))
               (:alternation :void (:sequence #\# fragment))))


;; URI-reference = URI / relative-ref

(define-parse-tree-synonym uri-reference
    (:alternation uri relative-ref))


;;;; THE END ;;;;
