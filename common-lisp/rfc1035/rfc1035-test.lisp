;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rfc1035-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Test rfc1035.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-08-15 <PJB> Created.
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.RFC1035.RFC1035")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"))

(defun test-read-zone ()
  (with-input-from-string (stream "
$TTL     30m
$ORIGIN  example.com.

@       1D      IN      SOA  ns1.example.com. admin.example.com. (
                                        2018080200      ; serial (d. adams)
                                        1H              ; refresh
                                        15M             ; retry
                                        14D             ; expiry
                                        1H )            ; negative cache

@  1H IN NS ns1.domain.com.
@  1H IN NS ns2.domain.com.
@  IN 1H NS ns3.domain.com.
@  1H    NS ns4.domain.com.
@     IN NS ns5.domain.com.
@        NS ns6.domain.com.

ns1				3600  IN  A      127.1.0.1
ns1				3600  IN  AAAA   ::ffff:127.1.0.1
ns2				3600  IN  A      127.1.0.2
ns3				3600  IN  A      127.1.0.3
ns4				3600  IN  A      127.1.0.4
ns5				3600  IN  A      127.1.0.5
ns6				3600  IN  A      127.1.0.6

mx1				          A      127.2.0.1
mx2				          A      127.2.0.2

host                      A      127.0.0.1
host                      AAAA   ::ffff:127.1.0.1

host                      TXT    \"v=spf1 include:_spf.google.com ipv4:127.0.0.1 ipv6:::ffff:127.1.0.1 -all\"
host                      SPF    v=spf1 include:_spf.google.com ipv4:127.0.0.1 ipv6:::ffff:127.1.0.1 -all
host.example.com.         TXT    \"google-site-verification=H-AnQ4Mc58qDb2fLBqHXrllwKCSfkl6d1YR9eCj_SyM\"

www                       CNAME  host
ftp             1H        CNAME  host

; MX Records

@                          MX     0   mx1
@                          MX     10  mx2.example.com.

;; THE END
")
    (read-zone stream (make-zone))))


(defun test-include ()
  (setf (com.informatimago.common-lisp.cesarum.file:text-file-contents "/tmp/zone.inc")
        "
@  1H IN NS ns1.domain.com.
@  1H IN NS ns2.domain.com.
@  IN 1H NS ns3.domain.com.
@  1H    NS ns4.domain.com.
@     IN NS ns5.domain.com.
@        NS ns6.domain.com.

ns1				3600  IN  A      127.1.0.1
ns1				3600  IN  AAAA   ::ffff:127.1.0.1
ns2				3600  IN  A      127.1.0.2
ns3				3600  IN  A      127.1.0.3
ns4				3600  IN  A      127.1.0.4
ns5				3600  IN  A      127.1.0.5
ns6				3600  IN  A      127.1.0.6
")
  (with-input-from-string (stream "
$TTL     30m
$ORIGIN  example.com.

@       1D      IN      SOA  ns1.example.com. admin.example.com. (
                                        2018080200      ; serial (d. adams)
                                        1H              ; refresh
                                        15M             ; retry
                                        14D             ; expiry
                                        1H )            ; negative cache

$INCLUDE /tmp/zone.inc

mx1				          A      127.2.0.1
mx2				          A      127.2.0.2

host                      A      127.0.0.1
host                      AAAA   ::ffff:127.1.0.1

host                      TXT    \"v=spf1 include:_spf.google.com ipv4:127.0.0.1 ipv6:::ffff:127.1.0.1 -all\"
host                      SPF    v=spf1 include:_spf.google.com ipv4:127.0.0.1 ipv6:::ffff:127.1.0.1 -all
host.example.com.         TXT    \"google-site-verification=H-AnQ4Mc58qDb2fLBqHXrllwKCSfkl6d1YR9eCj_SyM\"

www                       CNAME  host
ftp             1H        CNAME  host

; MX Records

@                          MX     0   mx1
@                          MX     10  mx2.example.com.

;; THE END
")
    (read-zone stream (make-zone))))

(define-test test/read-simple-zone-file ()
  (test equalp
        (test-read-zone)
        (make-zone :ttl 3600
                   :origin "example.com."
                   :class :in
                   :includes nil
                   :current-domain "example.com."
                   :records '(("example.com." 3600 :in :mx 10 "mx2.example.com.")
                              ("example.com." 3600 :in :mx 0 "mx1")
                              ("ftp.example.com." 3600 :in :cname "host")
                              ("www.example.com." 3600 :in :cname "host")
                              ("host.example.com." 3600 :in :txt "google-site-verification=H-AnQ4Mc58qDb2fLBqHXrllwKCSfkl6d1YR9eCj_SyM")
                              ("host.example.com." 3600 :in :spf "v=spf1" "include:_spf.google.com" "ipv4:127.0.0.1" "ipv6:::ffff:127.1.0.1" "-all")
                              ("host.example.com." 3600 :in :txt "v=spf1 include:_spf.google.com ipv4:127.0.0.1 ipv6:::ffff:127.1.0.1 -all")
                              ("host.example.com." 3600 :in :aaaa "::ffff:127.1.0.1")
                              ("host.example.com." 3600 :in :a "127.0.0.1")
                              ("mx2.example.com." 3600 :in :a "127.2.0.2")
                              ("mx1.example.com." 3600 :in :a "127.2.0.1")
                              ("ns6.example.com." 3600 :in :a "127.1.0.6")
                              ("ns5.example.com." 3600 :in :a "127.1.0.5")
                              ("ns4.example.com." 3600 :in :a "127.1.0.4")
                              ("ns3.example.com." 3600 :in :a "127.1.0.3")
                              ("ns2.example.com." 3600 :in :a "127.1.0.2")
                              ("ns1.example.com." 3600 :in :aaaa "::ffff:127.1.0.1")
                              ("ns1.example.com." 3600 :in :a "127.1.0.1")
                              ("example.com." 3600 :in :ns "ns6.domain.com.")
                              ("example.com." 3600 :in :ns "ns5.domain.com.")
                              ("example.com." 3600 :in :ns "ns4.domain.com.")
                              ("example.com." 3600 :in :ns "ns3.domain.com.")
                              ("example.com." 3600 :in :ns "ns2.domain.com.")
                              ("example.com." 3600 :in :ns "ns1.domain.com.")
                              ("example.com." 86400 :in :soa "ns1.example.com." "admin.example.com." 2018080200 3600 900 1209600 3600)))))

(define-test test/read-zone-file-with-include ()
  (test equalp (test-include)
        (make-zone :ttl 3600
                   :origin "example.com."
                   :class :in
                   :includes nil
                   :current-domain "example.com."
                   :records '(("example.com." 3600 :in :mx 10 "mx2.example.com.")
                              ("example.com." 3600 :in :mx 0 "mx1")
                              ("ftp.example.com." 3600 :in :cname "host")
                              ("www.example.com." 3600 :in :cname "host")
                              ("host.example.com." 3600 :in :txt "google-site-verification=H-AnQ4Mc58qDb2fLBqHXrllwKCSfkl6d1YR9eCj_SyM")
                              ("host.example.com." 3600 :in :spf "v=spf1" "include:_spf.google.com" "ipv4:127.0.0.1" "ipv6:::ffff:127.1.0.1" "-all")
                              ("host.example.com." 3600 :in :txt "v=spf1 include:_spf.google.com ipv4:127.0.0.1 ipv6:::ffff:127.1.0.1 -all")
                              ("host.example.com." 3600 :in :aaaa "::ffff:127.1.0.1")
                              ("host.example.com." 3600 :in :a "127.0.0.1")
                              ("mx2.example.com." 3600 :in :a "127.2.0.2")
                              ("mx1.example.com." 3600 :in :a "127.2.0.1")
                              ("ns6.example.com." 3600 :in :a "127.1.0.6")
                              ("ns5.example.com." 3600 :in :a "127.1.0.5")
                              ("ns4.example.com." 3600 :in :a "127.1.0.4")
                              ("ns3.example.com." 3600 :in :a "127.1.0.3")
                              ("ns2.example.com." 3600 :in :a "127.1.0.2")
                              ("ns1.example.com." 3600 :in :aaaa "::ffff:127.1.0.1")
                              ("ns1.example.com." 3600 :in :a "127.1.0.1")
                              ("example.com." 3600 :in :ns "ns6.domain.com.")
                              ("example.com." 3600 :in :ns "ns5.domain.com.")
                              ("example.com." 3600 :in :ns "ns4.domain.com.")
                              ("example.com." 3600 :in :ns "ns3.domain.com.")
                              ("example.com." 3600 :in :ns "ns2.domain.com.")
                              ("example.com." 3600 :in :ns "ns1.domain.com.")
                              ("example.com." 86400 :in :soa "ns1.example.com." "admin.example.com." 2018080200 3600 900 1209600 3600)))))

(define-test test/all ()
  (test/read-simple-zone-file)
  (test/read-zone-file-with-include)
  :success)


;;;; THE END ;;;;
