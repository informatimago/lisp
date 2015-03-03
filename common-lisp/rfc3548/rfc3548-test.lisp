;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rfc3548-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests rfc3548.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from rfc3548.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.RFC3548.RFC3548.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.RFC3548.RFC3548")
  (:export "TEST/ALL")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
                "BVSTREAM-WRITE-BYTE" "BVSTREAM-READ-BYTE"
                "WITH-INPUT-FROM-BYTE-VECTOR" "WITH-OUTPUT-TO-BYTE-VECTOR"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.RFC3548.RFC3548.TEST")


(define-test test/encoding (encoding &key line-width ignore-crlf)
  (let ((data (map 'vector (function char-code)
                #-(and)
                (with-open-file (in "/home/pascal/tmp/misc/wang.accented"
                                    :direction :input
                                    :if-does-not-exist :error)
                  (loop
                    :for ch = (read-char in nil nil)
                    :while ch
                    :collect ch into result
                    :finally (return result)))
                "
Hao Wang, logicien americain.

L'algorithme en  question  a  été  publié  en  1960  dans l'IBM Journal,
article intitule \"Toward  Mechanical Mathematics\", avec des variantes et
une  extension au calcul  des  prédicats.  Il  s'agit  ici  du  \"premier
programme\" de Wang, systeme \"P\".

L'article a été écrit en 1958, et les expériences effectuées sur IBM 704
­ machine à lampes, 32 k  mots  de 36 bits, celle­là même qui vit naître
LISP à la même époque. Le programme  a  été écrit en assembleur (Fortran
existait, mais il ne s'était pas encore imposé)  et  l'auteur estime que
\"there is very little in the program that is not straightforward\".

Il observe que les preuves engendrées sont \"essentiellement des arbres\",
et  annonce  que  la  machine  a  démontre 220 théorèmes du  calcul  des
propositions  (tautologies)  en  3  minutes. Il en tire argument pour la
supériorité  d'une  approche  algorithmique  par  rapport à une approche
heuristique comme celle du \"Logic Theorist\" de Newell, Shaw et  Simon (à
partir de 1956 sur la machine JOHNNIAC de la Rand Corporation): un débat
qui dure encore...

Cet  algorithme  a  été popularisé par J. McCarthy, comme exemple­fanion
d'application  de LISP. Il figure dans le manuel de la première  version
de  LISP  (LISP  1,  sur IBM 704 justement, le manuel est daté  de  Mars
1960), et il a été repris dans le celebre \"LISP 1.5 Programmer's Manual\"
publié en 1962 par MIT Press, un des maîtres­livres de l'Informatique.
"
                ))
        enc dec encoded decoded)
    (case encoding
      ((:base16)     (setf enc (function base16-encode-bytes)
                           dec (function base16-decode-bytes)))
      ((:base32)     (setf enc (function base32-encode-bytes)
                           dec (function base32-decode-bytes)))
      ((:base64)     (setf enc (function base64-encode-bytes)
                           dec (function base64-decode-bytes)))
      ((:filebase64) (setf enc (function filebase64-encode-bytes)
                           dec (function filebase64-decode-bytes)))
      (:otherwise (error "Unknown encoding ~S~%" encoding)))
    (dotimes (i 8)
      (setf encoded (funcall enc data :line-width line-width))
      ;; (print encoded)
      (setf decoded (funcall dec encoded :ignore-crlf ignore-crlf))
      (assert-true (equalp data decoded))
      (setf data (subseq data 0 (1- (length data)))))))


(define-test test/all-encodings ()
  (dolist (enc '(:base16 :base32 :base64 :filebase64)) 
    (dolist (line '(nil t))
      (format t "~&TESTING ~A ~:[~;with lines~]" enc line)
      (finish-output)
      (test/encoding enc :line-width (when line 40) :ignore-crlf line))))


(defun interactive-test/base16-encode ()
  (base16-encode
   (lambda () (let ((ch (read-char))) (if (char= #\newline ch) nil (char-code ch))))
   (function write-char)))


(defun interactive-test/base16-decode ()
  (base16-decode
   (lambda () (let ((ch (read-char))) (if (char= #\newline ch) nil ch)))
   (lambda (byte) (write-char (code-char byte)))))

(define-test test/all ()
  (test/all-encodings))

;;;; THE END ;;;;
