;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               simple-parse-uri.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a simple uri parsing function.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-04-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
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



#|
RFC-3986 says:
  

      pct-encoded = "%" HEXDIG HEXDIG

      reserved    = gen-delims / sub-delims

      gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"

      sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
                        / "*" / "+" / "," / ";" / "="


      unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"

      URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]

      hier-part   = "//" authority path-abempty
                  / path-absolute
                  / path-rootless
                  / path-empty
  
      scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
      authority   = [ userinfo "@" ] host [ ":" port ]
      userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
      host        = IP-literal / IPv4address / reg-name
      IP-literal = "[" ( IPv6address / IPvFuture  ) "]"

      IPvFuture  = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
      IPv6address =                            6( h16 ":" ) ls32
                  /                       "::" 5( h16 ":" ) ls32
                  / [               h16 ] "::" 4( h16 ":" ) ls32
                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
                  / [ *4( h16 ":" ) h16 ] "::"              ls32
                  / [ *5( h16 ":" ) h16 ] "::"              h16
                  / [ *6( h16 ":" ) h16 ] "::"

      ls32        = ( h16 ":" h16 ) / IPv4address
                  ; least-significant 32 bits of address

      h16         = 1*4HEXDIG
                  ; 16 bits of address represented in hexadecimal

      IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet

      dec-octet   = DIGIT                 ; 0-9
                  / %x31-39 DIGIT         ; 10-99
                  / "1" 2DIGIT            ; 100-199
                  / "2" %x30-34 DIGIT     ; 200-249
                  / "25" %x30-35          ; 250-255

      reg-name    = *( unreserved / pct-encoded / sub-delims )

      port        = *DIGIT

      path          = path-abempty    ; begins with "/" or is empty
                    / path-absolute   ; begins with "/" but not "//"
                    / path-noscheme   ; begins with a non-colon segment
                    / path-rootless   ; begins with a segment
                    / path-empty      ; zero characters

      path-abempty  = *( "/" segment )
      path-absolute = "/" [ segment-nz *( "/" segment ) ]
      path-noscheme = segment-nz-nc *( "/" segment )
      path-rootless = segment-nz *( "/" segment )
      path-empty    = 0<pchar>

      segment       = *pchar
      segment-nz    = 1*pchar
      segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
                    ; non-zero-length segment without any colon ":"

      pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"


      query         = *( pchar / "/" / "?" )

      fragment    = *( pchar / "/" / "?" )



      URI-reference = URI / relative-ref

      relative-ref  = relative-part [ "?" query ] [ "#" fragment ]

      relative-part = "//" authority path-abempty
                    / path-absolute
                    / path-noscheme
                    / path-empty

      absolute-URI  = scheme ":" hier-part [ "?" query ]
  
  
  
   The authority component is preceded by a double slash ("//") and is
   terminated by the next slash ("/"), question mark ("?"), or number
   sign ("#") character, or by the end of the URI.


 
|#

;;        URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]



(defclass uri ()
  ((scheme
    :initarg :scheme
    :accessor scheme
    :initform nil)
   (userinfo
    :initarg :userinfo
    :accessor userinfo
    :initform nil)
   (host
    :initarg :host
    :accessor host
    :initform nil)
   (port
    :initarg :port
    :accessor port
    :initform nil)
   (path
    :initarg :path
    :accessor path
    :initform nil)
   (query
    :initarg :query
    :accessor query
    :initform nil)
   (fragment
    :initarg :fragment
    :accessor fragment
    :initform nil)))
(defclass url (uri)
  ())

(defmethod format-url ((uri uri) stream)
  (format stream "~A:~*~:[~*~;//~2:*~@[~A@~]~A~@[:~A~]~]~A~@[?~A~]~@[#~A~]"
          (scheme uri)
          (userinfo uri)
          (host uri)
          (port uri)
          (path uri)
          (query uri)
          (fragment uri)))

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :identity t :type t)
    (format-url uri stream))
  uri)

(defmethod urlstring ((uri uri))
  (format-url uri nil))

(defmethod hostname ((uri uri)) (host uri))
(defmethod (setf hostname) (new-host (uri uri)) (setf (host uri) new-host))
(defmethod user ((uri uri))
  (when (userinfo uri)
    (subseq (userinfo uri) 0 (or (position #\: (userinfo uri))))))
(defmethod password ((uri uri))
  (when (userinfo uri)
    (let ((colon  (position #\: (userinfo uri))))
      (when colon
        (subseq (userinfo uri) (1+ colon))))))
(defmethod (setf user) (new-user (uri uri))
  (setf (userinfo uri) (format nil "~A~@[:~A~]" new-user  (password uri)))
  new-user)
(defmethod (setf password) (new-password (uri uri))
  (setf (userinfo uri) (format nil "~A~@[:~A~]" (user uri) new-password))
  new-password)


(defun merge-urls (url1 url2)
  (let ((url1 (url url1))
        (url2 (url url2)))
    (make-instance 'url
                   :scheme (or (scheme url1)
                               (scheme url2))
                   :userinfo (or (userinfo url1)
                                 (userinfo url2))
                   :host (or (host url1)
                             (host url2))
                   :port (or (port url1)
                             (port url2))
                   :path (or (path url1)
                             (path url2))
                   :query (or (query url1)
                              (query url2))
                   :fragment (or (fragment url1)
                                 (fragment url2)))))


(defun simple-parse-uri (string)
  (flet ((prefixp (prefix string) (and (<= (length prefix) (length string))
                                       (string= prefix string :end2 (length prefix))))
         (after (index) (when index (1+ index))))
    (let* ((scheme-end      (or (after (position #\: string))
                                (error "Only URI (RFC-3986) are accepted, not relative-refs.")))
           (scheme          (subseq string 0 (1- scheme-end)))
           (without-scheme  (subseq string scheme-end))
           (authorityp      (prefixp "//" without-scheme))
           (abpath-start    (if authorityp
                                (or (position #\/ without-scheme :start 2) (length without-scheme))
                                0))
           (path-and-qf     (subseq without-scheme abpath-start))
           
           (sharp           (position #\# path-and-qf :from-end t))
           (query-end       (or sharp (length path-and-qf)))
           (fragment-start  (after sharp))
           (question        (position #\? path-and-qf :end sharp))
           (path-end        (or question sharp (length path-and-qf)))
           (query-start     (after question))

           (query           (when query-start    (subseq path-and-qf query-start query-end)))
           (fragment        (when fragment-start (subseq path-and-qf fragment-start)))
           (path            (subseq path-and-qf 0 path-end)))
      (if authorityp
          (let* ((authority       (subseq without-scheme 2 abpath-start))
                 (userinfo-end    (position #\@ authority))
                 (host-start      (if userinfo-end (1+ userinfo-end) 0))
                 (colon           (position #\: authority
                                            :start (if (char= #\[ (aref authority host-start))
                                                       (after (position #\] authority :start host-start))
                                                       host-start)))
                 (port-start      (or (after colon) (length authority)))
                 (userinfo        (when userinfo-end
                                    (subseq authority 0 userinfo-end)))
                 (host            (subseq authority host-start colon))
                 (port-string     (subseq authority port-start))
                 (port            (when (plusp (length port-string))
                                    (parse-integer port-string :junk-allowed nil))))
            (make-instance 'uri
                           :scheme scheme
                           :userinfo userinfo
                           :host host
                           :port port
                           :path path
                           :query query
                           :fragment fragment))
          (make-instance 'uri
                         :scheme scheme
                         :path path
                         :query query
                         :fragment fragment)))))


(defun test/simple-parse-uri ()
  (map 'list (lambda (test)
             (let* ((uri (simple-parse-uri (first test)))
                    (result (list (first test)
                                  :scheme (scheme uri)
                                  :userinfo (userinfo uri)
                                  :host (host uri)
                                  :port (port uri)
                                  :path (path uri)
                                  :query (query uri)
                                  :fragment (fragment uri))))
               (assert (equalp test result))
               uri))
    '(("ftp://ftp.is.co.za/rfc/rfc1808.txt"                    :scheme "ftp"    :userinfo nil             :host "ftp.is.co.za"  :port nil :path "/rfc/rfc1808.txt"                                :query nil               :fragment nil) 
      ("ftp://user@ftp.is.co.za/rfc/rfc1808.txt"               :scheme "ftp"    :userinfo "user"          :host "ftp.is.co.za"  :port nil :path "/rfc/rfc1808.txt"                                :query nil               :fragment nil) 
      ("ftp://user:password@ftp.is.co.za/rfc/rfc1808.txt"      :scheme "ftp"    :userinfo "user:password" :host "ftp.is.co.za"  :port nil :path "/rfc/rfc1808.txt"                                :query nil               :fragment nil) 
      ("http://www.ietf.org/rfc/rfc2396.txt"                   :scheme "http"   :userinfo nil             :host "www.ietf.org"  :port nil :path "/rfc/rfc2396.txt"                                :query nil               :fragment nil) 
      ("http://www.ietf.org/rfc/rfc2396.txt#sect1"             :scheme "http"   :userinfo nil             :host "www.ietf.org"  :port nil :path "/rfc/rfc2396.txt"                                :query nil               :fragment "sect1") 
      ("http://www.ietf.org/rfc/rfc2396.txt?format=text"       :scheme "http"   :userinfo nil             :host "www.ietf.org"  :port nil :path "/rfc/rfc2396.txt"                                :query "format=text"     :fragment nil) 
      ("http://www.ietf.org/rfc/rfc2396.txt?format=text#sect1" :scheme "http"   :userinfo nil             :host "www.ietf.org"  :port nil :path "/rfc/rfc2396.txt"                                :query "format=text"     :fragment "sect1") 
      ("ldap://[2001:db8::7]/c=GB?objectClass?one"             :scheme "ldap"   :userinfo nil             :host "[2001:db8::7]" :port nil :path "/c=GB"                                           :query "objectClass?one" :fragment nil) 
      ("telnet://192.0.2.16:80/"                               :scheme "telnet" :userinfo nil             :host "192.0.2.16"    :port 80  :path "/"                                               :query nil               :fragment nil) 
      ("mailto:John.Doe@example.com"                           :scheme "mailto" :userinfo nil             :host nil             :port nil :path "John.Doe@example.com"                            :query nil               :fragment nil) 
      ("news:comp.infosystems.www.servers.unix"                :scheme "news"   :userinfo nil             :host nil             :port nil :path "comp.infosystems.www.servers.unix"               :query nil               :fragment nil) 
      ("tel:+1-816-555-1212"                                   :scheme "tel"    :userinfo nil             :host nil             :port nil :path "+1-816-555-1212"                                 :query nil               :fragment nil) 
      ("urn:oasis:names:specification:docbook:dtd:xml:4.1.2"   :scheme "urn"    :userinfo nil             :host nil             :port nil :path "oasis:names:specification:docbook:dtd:xml:4.1.2" :query nil               :fragment nil))))



#|
(test/simple-parse-uri)

(#<uri ftp://ftp.is.co.za/rfc/rfc1808.txt #x30200223E76D>
 #<uri ftp://user@ftp.is.co.za/rfc/rfc1808.txt #x30200223E3DD>
 #<uri ftp://user:password@ftp.is.co.za/rfc/rfc1808.txt #x30200223DFED>
 #<uri http://www.ietf.org/rfc/rfc2396.txt #x30200223DCAD>
 #<uri http://www.ietf.org/rfc/rfc2396.txt#sect1 #x30200223D91D>
 #<uri http://www.ietf.org/rfc/rfc2396.txt?format=text #x30200223D53D>
 #<uri http://www.ietf.org/rfc/rfc2396.txt?format=text#sect1 #x30200223D10D>
 #<uri ldap://[2001:db8::7]/c=GB?objectClass?one #x30200223CD7D>
 #<uri telnet://192.0.2.16:80/ #x30200223CAFD>
 #<uri mailto:John.Doe@example.com #x30200223C84D>
 #<uri news:comp.infosystems.www.servers.unix #x30200223C50D>
 #<uri tel:+1-816-555-1212 #x30200223C28D>
 #<uri urn:oasis:names:specification:docbook:dtd:xml:4.1.2 #x30200223BE8D>)
|#


(defun url (thing)
  (if (stringp thing)
      (simple-parse-uri thing)
      thing))
