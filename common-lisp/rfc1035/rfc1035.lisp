;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rfc1035.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Parses zone files.
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.RFC1035.RFC1035"
  (:nicknames "COM.INFORMATIMAGO.COMMON-LISP.RFC1035.ZONE-FILE")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (:export "LOAD-ZONE" "READ-ZONE"
           "ZONE"
           "MAKE-ZONE"
           "ZONE-P"
           "COPY-ZONE"
           "ZONE-TTL"
           "ZONE-ORIGIN"
           "ZONE-CLASS"
           "ZONE-RECORDS")
  (:documentation "
Parses zone files. The format is specified in:
https://www.ietf.org/rfc/rfc1035.txt

Copyright Pascal J. Bourguignon 2018 - 2018

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.RFC1035.RFC1035")

(defun first-element (vector) (aref vector 0))
(defun last-element  (vector) (aref vector (1- (length vector))))

;; <character-string> is expressed in one or two ways: as a contiguous set
;; of characters without interior spaces, or as a string beginning with a "
;; and ending with a ".  Inside a " delimited string any character can
;; occur, except for a " itself, which must be quoted using \ (back slash).
;;
;; Because these files are text files several special encodings are
;; necessary to allow arbitrary data to be loaded.  In particular:
;;
;;
;; @               A free standing @ is used to denote the current origin.
;;
;; \X              where X is any character other than a digit (0-9), is
;;                 used to quote that character so that its special meaning
;;                 does not apply.  For example, "\." can be used to place
;;                 a dot character in a label.
;;
;; \DDD            where each D is a digit is the octet corresponding to
;;                 the decimal number described by DDD.  The resulting
;;                 octet is assumed to be text and is not checked for
;;                 special meaning.
;;
;; ( )             Parentheses are used to group data that crosses a line
;;                 boundary.  In effect, line terminations are not
;;                 recognized within parentheses.
;;
;; ;               Semicolon is used to start a comment; the remainder of
;;                 the line is ignored.
;;
;;
;; 1. All RRs in the file should have the same class.
;;
;; 2. Exactly one SOA RR should be present at the top of the zone.



(defun scan (stream)
  (with-input-from-string (nl (format nil "~%"))
    (let ((stream (make-concatenated-stream nl stream))
          (spacenl #(#\space #\tab #\newline))
          (space   #(#\space #\tab))
          (escape  #\\)
          (strend  #\"))

      (labels ((spacep      (ch) (find ch space))
               (spacenlp    (ch) (find ch spacenl))
               (get-char    () (read-char     stream nil nil))
               (next-char   () (peek-char nil stream nil nil))
               (read-token  () (list :token
                                     (coerce (loop
                                               :for ch := (next-char)
                                               :while (and ch (not (spacenlp ch)))
                                               :collect (get-char))
                                             'string)))
               (read-string () (list :string
                                     (coerce (loop
                                               :for ch := (get-char)
                                               :while (and ch (char/= ch strend))
                                               :collect (if (char= ch escape)
                                                            (get-char)
                                                            ch))
                                             'string)))
               (skip-spaces () (loop
                                 :for ch := (next-char)
                                 :while (spacep ch)
                                 :do (get-char))))

        (declare (inline spacep spacenlp get-char next-char))

        (loop
          :with tokens := '()
          :with in-paren := nil
          :for ch := (next-char)
          :while ch
          :do (case ch

                ((#\newline)
                 (get-char)
                 (if in-paren
                     (skip-spaces)
                     (push (if (spacep (next-char))
                               :nl-space
                               :nl-token)
                           tokens)))

                ((#\;)
                 (read-line stream)
                 (if in-paren
                     (skip-spaces)
                     (push (if (spacep (next-char))
                               :nl-space
                               :nl-token)
                           tokens)))

                ((#\space #\tab)
                 (skip-spaces))

                ((#\")
                 (get-char)
                 (push (read-string) tokens))

                ((#\()
                 (get-char)
                 (setf in-paren t))

                ((#\))
                 (get-char)
                 (setf in-paren nil))

                ;; ((#\\)
                ;;  (get-char)
                ;;  (let* ((text (make-array 3 :element-type 'character :initial-contents (list (get-char) (get-char) (get-char))))
                ;;         (byte (parse-integer text)))
                ;;    (unless (typep byte '(unsigned-byte 8))
                ;;      (error "Invalid token \\~A : the value should be between 0 and 255 inclusive" text))
                ;;    (code-char byte)))

                (t
                 (push (read-token) tokens)))

          :finally (return (nreverse tokens)))))))


;;    <blank>[<comment>]
;;
;;     $TTL <ttl>
;;
;;     $ORIGIN <domain-name> [<comment>]
;;
;;     $INCLUDE <file-name> [<domain-name>] [<comment>]
;;
;;     <domain-name><rr> [<comment>]
;;
;;     <blank><rr> [<comment>]
;;
;;
;; <rr> contents take one of the following forms:
;;
;;     [<TTL>] [<class>] <type> <RDATA>
;;
;;     [<class>] [<TTL>] <type> <RDATA>


(defun unwrap (token)
  (when (consp token)
    (ecase (first token)
      (:token  (second token))
      (:string token))))

(defstruct zone
  ttl
  origin
  class
  includes
  current-domain
  records)

(defun decode-duration (duration)
  (multiple-value-bind (value position) (parse-integer duration :junk-allowed t)
    (* value
       (if (< position (length duration))
           (let ((unit (subseq duration position)))
             (cond
               ((string-equal unit "S") 1)
               ((string-equal unit "M") 60)
               ((string-equal unit "H") 3600)
               ((string-equal unit "D") #.(* 24 3600))
               ((string-equal unit "W") #.(* 7 24 3600))
               (t (error "Unexpected unit in duration ~S" duration))))
           1))))

(defun record-ttlp (text)
  (when (digit-char-p (first-element text))
    (decode-duration text)))

(defparameter *classes* '(:IN :CS :CH :HS))

(defun record-classp (text)
  (first (member text *classes* :test (function string-equal))))

(defparameter *types*   '((:A     ipv4)
                          (:NS    domain)
                          (:MD    domain)
                          (:MF    domain)
                          (:CNAME domain)
                          (:SOA   domain domain integer time time time time)
                          (:MB    domain)
                          (:MG    domain)
                          (:MR    domain)
                          (:NULL  string)
                          (:PTR   domain)
                          (:HINFO string string)
                          (:MINFO domain domain)
                          (:MX    integer domain)
                          (:TXT   string &etc)
                          (:AAAA  ipv6)
                          (:SPF   string &etc))
  "https://en.wikipedia.org/wiki/List_of_DNS_record_types")


; (time ipv4 byte bitmap integer domain ipv6 string &etc)

(defun parse-ipv4 (address)
  address)

(defun parse-ipv6 (address)
  address)


(defun convert-time (token)
  (decode-duration (second token)))

(defun convert-ipv4 (token)
  (parse-ipv4 (second token)))

(defun convert-ipv6 (token)
  (parse-ipv6 (second token)))

(defun convert-byte (token)
  (let ((value (parse-integer (second token))))
    (assert (typep value '(unsigned-byte 8)))
    value))

(defun convert-integer (token)
  (let ((value (parse-integer (second token))))
    (assert (typep value '(unsigned-byte 32)))
    value))

(defun convert-domain (token)
  (second token))

(defun convert-string (token)
  (second token))

(defun convert-record (types record)
  (let ((etc (member '&etc types)))
    (if etc
        (unless (<= (1- (length types)) (length record))
          (error "Missing record data, expected ~{~A~^ ~}, got ~A" types record))
        (unless (= (length types) (length record))
          (error "Invalid record data count, expected ~{~A~^ ~}, got ~A" types record)))
    (loop
      :with last-type
      :for type := (pop types)
      :for item :in record
      :do (unless (member type '(nil &etc))
            (setf last-type type))
      :collect (labels ((convert-function (type)
                          (ecase type
                            (time       #'convert-time)
                            (ipv4       #'convert-ipv4)
                            (ipv6       #'convert-ipv6)
                            (byte       #'convert-byte)
                            (integer    #'convert-integer)
                            (domain     #'convert-domain)
                            (string     #'convert-string)
                            ((&etc nil) (convert-function last-type)))))
                 (funcall (convert-function type) item)))))


(defun record-typep (text)
  (first (member text *types* :test (function string-equal) :key (function first))))

(defun process-record (record zone)

  (destructuring-bind (domain &rest record) (if (eq :nl-space (first record))
                                                (cons (or (zone-current-domain zone)
                                                          (error "No current domain name when processing record ~S" record))
                                                      (rest record))
                                                (cons (unwrap (second record))
                                                      (cddr record)))
    (setf (zone-current-domain zone)
          (setf domain (cond
                         ((string= "@" domain)               (or (zone-origin zone)
                                                                 (error "No origin domain name when processing record ~S" record)))
                         ((char= #\. (last-element domain))  domain)
                         (t                                  (concatenate 'string domain "."
                                                                          (or (zone-origin zone)
                                                                              (error "No origin domain name when processing record ~S" record)))))))

    (flet ((process-ttl   (item)
             (setf (zone-ttl zone) (decode-duration item)))
           (process-class (item)
             (cond ((null (zone-class zone))
                    (setf (zone-class zone) (record-classp item)))
                   ((not (eq (zone-class zone) (record-classp item)))
                    (error "Inconsistent class in record. Expected ~S, record ~S" item record)))))

      ;; TTL CLASS     TYPE RDATA
      ;; TTL           TYPE RDATA
      ;;     CLASS     TYPE RDATA
      ;;     CLASS TTL TYPE RDATA
      ;;               TYPE RDATA

      (let ((item (unwrap (first record))))
        (cond

          ((record-ttlp item)
           (process-ttl item)
           (pop record)
           (let ((item (unwrap (first record))))
             (cond
               ((record-classp item)
                (process-class item)
                (pop record))
               ((not (record-typep item))
                (error "Invalid record, expected class or type. ~S" record)))))

          ((record-classp item)
           (process-class item)
           (pop record)
           (let ((item (unwrap (first record))))
             (cond
               ((record-ttlp item)
                (process-ttl item)
                (pop record))
               ((not (record-typep item))
                (error "Invalid record, expected ttl or type. ~S" record)))))

          ((not (record-typep item))
           (error "Invalid record, expected ttl, class or type. ~S" record))))

      (let ((type (record-typep (unwrap (first record)))))
        (unless TYPE
          (error "Invalid record, expected a type. ~S" record))
        (pop record)
        (list* domain (zone-ttl zone) (zone-class zone) (first type) (convert-record (rest type) record))))))

(defun read-zone (stream zone)
  (loop
    :for record :in (nsplit-list-on-indicator
                     (scan stream)
                     (lambda (token newline)
                       (declare (ignore token))
                       (member newline '(:nl-token :nl-space))))
    :for newline := (first record)
    :when (second record)
      :do
         (ecase newline
           ((:nl-token)
            (let ((keystr (unwrap (second record))))
              (cond
                ((string= "$TTL"     keystr) (setf (zone-ttl    zone) (decode-duration (unwrap (third record)))))
                ((string= "$ORIGIN"  keystr) (setf (zone-origin zone) (unwrap (third record))))
                ((string= "$INCLUDE" keystr)
                 (push (unwrap (third record)) (zone-includes zone))
                 (with-open-file (stream (first (zone-includes zone)))
                   (read-zone stream zone))
                 (pop (zone-includes zone)))
                (t
                 (push (process-record record zone) (zone-records zone))))))
           ((:nl-space)
            (push (process-record record zone) (zone-records zone))))
    :finally (return zone)))

(defun load-zone (pathname)
  (let ((zone (with-open-file (stream pathname)
                (read-zone stream (make-zone)))))
    (setf (zone-records zone) (nreverse  (zone-records zone)))
    zone))


#-(and) (progn
          (test/read-zone)
          (test/include)
          (load-zone #P"~/src/public/domains/zones-master/informatimago.com.zone")
          )

;;;; THE END ;;;;
