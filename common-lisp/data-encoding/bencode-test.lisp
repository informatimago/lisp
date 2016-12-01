;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bencode-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Test bencode.lisp.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from bencode.lisp.
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.BENCODE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.BENCODE")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.BENCODE.TEST")


(define-test test/string (&key (verbose nil))
  (let* ((str    (concatenate 'string
                              "l" "i43e"
                              "4:spam"
                              "l" "4:spam"
                              "4:eggs" "e"
                              "d" "3:cow" "3:moo"
                              "4:spam" "4:eggs" "e"
                              "d" "4:spam" "l" "1:a"
                              "1:b" "e" "e" "e"))
         (object (bdecode-from-string str))
         (obj    (maptree (lambda (node)
                            (if (hash-table-p node)
                                (cons 'dict (hash-table-entries node))
                                node))
                          object)))
    (when verbose
      (print str)
      (print obj))
    (assert-true (equal obj
                        '(43 "spam" ("spam" "eggs")
                          (dict (:cow . "moo") (:spam . "eggs"))
                          (dict (:spam "a" "b")))))
    (assert-true (string= (bencode-to-string object) str))))


;; (defun dictp (dict)
;;    (and (listp dict) (eql 'dict (first dict))))
;;
;; (defun dict-get (dict key &optional (default nil))
;;   (assert (dictp dict))
;;   (let ((entry (if (keywordp key)
;;                    (assoc key (rest dict))
;;                    (assoc key (rest dict) :test (function string=)))))
;;     (if entry
;;         (values (cdr entry) t)
;;         (values default nil))))
;;
;; (defun dict-get* (dict &rest keys)
;;   (if (null keys)
;;       dict
;;       (apply (function dict-get*) (dict-get dict (first keys)) (rest keys))))
;;
;; (defun (setf dict-get) (new-value dict key &optional (default nil))
;;   (declare (ignore default))
;;   (assert (dictp dict))
;;   (let ((entry (if (keywordp key)
;;                    (assoc key (rest dict))
;;                    (assoc key (rest dict) :test (function string=)))))
;;     (if entry
;;         (setf (cdr entry) new-value)
;;         (push (rest dict) (cons key new-value)))
;;     new-value))
;;
;; (defun dict-count (dict)
;;   (assert (dictp dict))
;;   (length (rest dict)))
;;
;; (defun dict-keys (dict)
;;   (assert (dictp dict))
;;   (mapcpar (function car) (rest dict)))


#||

(defparameter  *sizes*
  (mapcar (lambda (path)
            (finish-output)
            (handler-case
                (print
                 (cons path
                       (hash-table-path
                        (with-open-file (torrent path :element-type '(unsigned-byte 8))
                          (let ((*key-map-exceptions*  *torrent-key-map-exceptions*))
                            (bdecode-from-binary-stream torrent)))
                        :info :length)))
              (error (err)
                (format t "~%~A: ~A~%" path err))))
          (directory "/home/p2p/incoming/files/wikileaks/torrent/**/*.torrent")))

(reduce (function +) *sizes* :key (lambda (x) (or (cdr x) 0)))


(let ((inpath  "/home/p2p/incoming/files/wikileaks/torrent/iran-telco-attachments-2009/5th-neshast.pdf.torrent")
      (outpath "/tmp/test.bencode"))
  (with-open-file (out outpath
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (let ((*key-map-exceptions* *torrent-key-map-exceptions* ))
      (bencode-to-binary-stream
       (with-open-file (torrent inpath :element-type '(unsigned-byte 8))
         (let ((*key-map-exceptions* *torrent-key-map-exceptions* ))
           (bdecode-from-binary-stream torrent)))
       out))))

#S(HASH-TABLE :TEST EXT:FASTHASH-EQL
              (:URL-LIST . "http://88.80.16.63/file/iran-telco-attachments-2009%2F5th-neshast.pdf")
              (:INFO . #S(HASH-TABLE :TEST EXT:FASTHASH-EQL
                                     (:PIECES . #(35 67 174 26 34 98 126 29 18 21 102 158 184 27 231 17 96 241 244 126
                                                  83 244 165 89 75 132 178 62 76 94 64 120 0 179 45 188 230 49 41 15 205
                                                  251 190 150 68 235 201 12 14 65 60 160 192 250 217 53 217 188 207 188))
                                     (:PIECE-LENGTH . 262144)
                                     (:NAME . "iran-telco-attachments-2009/5th-neshast.pdf")
                                     (:LENGTH . 601447)))
              (:CREATION-DATE . 1291401958)
              (:CREATED-BY . "mktorrent 1.0")
              (:COMMENT . "WikiLeaks release - iran-telco-attachments-2009/5th-neshast.pdf")
              (:ANNOUNCE-LIST . (("http://tracker.openbittorrent.com/announce")
                                 ("udp://tracker.openbittorrent.com:80/announce")))
              (:ANNOUNCE . "http://tracker.openbittorrent.com/announce"))

||#


(define-test test/all ()
  (test/string))

;;;; THE END ;;;;
