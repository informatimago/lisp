;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ecp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;nDESCRIPTION
;;;;    
;;;;    Minitel-1b Error Correction Procedure.
;;;;    Reference: page 55 and on in http://543210.free.fr/TV/stum1b.pdf
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-04 <PJB> Converted from C.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.ECP"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.P127N2")
  (:export "+ECP-BLOCK-SIZE+"
           "ECP-DATA" "ECP-ACTIVE" "ECP-STATE"
           "START-ECP" "STOP-ECP"
           "STATISTICS" "STATISTICS-BYTES-RECEIVED" "STATISTICS-PARITY-ERRORS"
           "STATISTICS-UNCORRECTABLE-COUNT" "STATISTICS-INVALID-COUNT"
           "STATISTICS-CORRECT-COUNT" "STATISTICS-VALID-SYN-COUNT"
           "STATISTICS-INVALID-SYN-COUNT"
           "PROCESS-INPUT-BUFFER"
           "PROCESS-OUTPUT-BUFFER")
  (:documentation "
Minitel-1b Error Correction Procedure.

Reference: page 55 and on in http://543210.free.fr/TV/stum1b.pdf


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2012 - 2012
    
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
    If not, see http://www.gnu.org/licenses/


"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.ECP")




(defstruct statistics
  "Statistics for the ECP algorithm."
  (bytes-received      0)
  (parity-errors       0)
  (uncorrectable-count 0)
  (uncorrected-count   0)
  (invalid-count       0)
  (correct-count       0)
  (valid-syn-count     0)
  (invalid-syn-count   0))

(setf (documentation 'statistics-bytes-received 'function)
      "ECP statistics: number of bytes processed."
      (documentation 'statistics-parity-errors 'function)
      "ECP statistics: number of parity errors."
      (documentation 'statistics-uncorrectable-count 'function)
      "ECP statistics: number of uncorrectable errors."
      (documentation 'statistics-uncorrected-count 'function)
      "ECP statistics: number of uncorrected errors."
      (documentation 'statistics-invalid-count 'function)
      "ECP statistics: number of invalid blocks."
      (documentation 'statistics-correct-count 'function)
      "ECP statistics: number of correct blocks."
      (documentation 'statistics-valid-syn-count 'function)
      "ECP statistics: number of valid syn."
      (documentation 'statistics-invalid-syn-count 'function)
      "ECP statistics: number of invalid syn.")



(deftype ecp-state () '(member :ecp-start :ecp-block :ecp-syn))

(defconstant +ecp-block-size+ 17
  "Size of an ECP block.")

(defstruct (ecp-data (:conc-name ecp-))
  "State for the ECP algorithm."
  (active nil :type boolean)
  (state  :ecp-start :type ecp-state)
  (count 0) ; bytes in block
  (parity-index 0) ; of first error
  (parity-errors 0)
  (block-number 0) ; current block
  (block (make-array +ecp-block-size+ :element-type '(unsigned-byte 8) :initial-element 0)))

(setf (documentation 'ecp-active 'function)
      "Whether the ECP is active."
      (documentation 'ecp-state 'function)
      "State of the ECP, one of :ecp-start, :ecp-block or :ecp-syn.")


(defun start-ecp (ecp)
  "Initialize the ECP data."
  (check-type ecp ecp-data)
  (setf (ecp-active ecp) t
        (ecp-state ecp) :ecp-start
        (ecp-count ecp) 0
        (ecp-parity-index ecp) 0
        (ecp-parity-errors ecp) 0
        (ecp-block-number ecp) 0)
  ecp)


(defun stop-ecp (ecp)
  "Resets the ECP data."
  (setf (ecp-active ecp) nil)
  (assert (zerop (ecp-count ecp)))
  ecp)




(defun clear-parity (buffer &key (start 0) (end nil))
  "
DO:     Resets the parity bit in each  octets of the BUFFER between
        START and END,
RETURN: BUFFER
"
  (let ((end (or end (length buffer))))
    (loop
       :for i :from start :below end
       :do (setf (aref buffer i) (logand (aref buffer i) #x7f)))
    buffer))


(defun set-parity (buffer &key (start 0) (end nil))
  "
DO:     Modifies the octets in the BUFFER between START and END,
        setting the parity bit to even parity.
RETURN: BUFFER
"
  (let ((end (or end (length buffer))))
    (loop
       :for i :from start :below end
       :do (setf (aref buffer i) (even-parity (aref buffer i))))
    buffer))



(defun remove-escapes (buffer &key (start 0) (end nil))
  "
PRE:		bytes in buffer are with no parity.
POST:		DLE escapes and the trailling NULs are removed from the buffer.
DO:         Modifies the buffer.
RETURN:		the new byte count.
"
  (let* ((end (or end (length buffer)))
         (i start)
         (j start)
         (ncount 0)
         (count (- end start)))
    (loop :while (plusp count) :do
       (cond
         ((= (aref buffer j) dle)
          (incf j)
          (decf count)
          (setf (aref buffer i) (aref buffer j))
          (incf i)
          (incf ncount))
         ((plusp (aref buffer j)) ; BUG: not only the trailing NULs are removed, but also NULs in the middle, if they're not escaped by a DLE.
          (setf (aref buffer i) (aref buffer j))
          (incf i)
          (incf ncount)))
       (incf j)
       (decf count))
    ncount))


(defconstant +g+ (+ (ash 1 7) (ash  1 3) (ash 1 0)))

(defvar *ecp-rank*
  (make-array 128 :element-type '(unsigned-byte 8)
              :initial-contents '(
                                  0 0 1 31 2 62 32 103 3 7 63 15 33 84 104 93 
                                  4 124 8 121 64 79 16 115 34 11 85 38 105 46 
                                  94 51 5 82 125 60 9 44 122 77 65 67 80 42 17 
                                  69 116 23 35 118 12 28 86 25 39 57 106 19 47 
                                  89 95 71 52 110 6 14 83 92 126 30 61 102 10 
                                  37 45 50 123 120 78 114 66 41 68 22 81 59 43 
                                  76 18 88 70 109 117 27 24 56 36 49 119 113 
                                  13 91 29 101 87 108 26 55 40 21 58 75 107 54 
                                  20 74 48 112 90 100 96 97 72 98 53 73 111 99
                                  )))



;; A ECP block contains 17 octets: 15 bytes of payload, one octet of
;; check and one nul octet.  Each octet contains 7 bit of data and one
;; bit of even parity in the most significant position.
;; 
;; +------------------------+-----------------+--------------+
;; |   payload (15 octets)  | check (1 octet) | 00 (1 octet) |
;; +------------------------+-----------------+--------------+
;; 
;; The value of check is such that the remainder of the polynom
;; divided by x^7+x^3+x^0 is 0.



(defun process-input-buffer (ecp buffer &key (start 0) (end nil) ((:statistics stats) nil) (send-nak nil) (cancel-nak nil))
  "
DO:         Process the bytes in the BUFFER from START to END.
ECP:        An ECP-DATA structure maintaining the persistent data
            between calls to PROCESS-INPUT-BUFFER.
BUFFER:     Input octet vector.
START:      Index of the first octet in BUFFER to process.
END:        Index of the first octet in BUFFER after the last to process.
STATS:      A STATISTICS structure.
SEND-NAK:   A thunk to call when an uncorrectable error is detected.
CANCEL-NAK: A thunk to call when a SYN SYN 4x sequence is received (so it can cancel the NAK timer).
RETURN:     NIL;         next; NIL;          STATS -- in case of incomplete or erroneous block.
            data-vector; next; block-number; STATS -- when a valid block is received.
            next is the index of the next start in buffer.
"
  (check-type ecp      ecp-data)
  (check-type stats    (or null statistics))
  (check-type start    (integer 0))
  (check-type end      (or null (integer 0)))
  (check-type send-nak (or null function))
  (let* ((stats (or stats (make-statistics)))
         (end   (or end (length buffer)))
         (b     start)
         (c     (- end start))
         (count c))
    (flet ((process-block ()
             ;; (print `(process-block :ecp ,ecp :start ,start :end ,end :b ,b :c ,c :count ,count :stats ,stats)) (finish-output)
             (loop :named accumulates-until-end-of-block
                :while (and (plusp c) (< (ecp-count ecp) +ecp-block-size+))
                :do                       ;; check parity errors
                (when (/= (aref buffer b) (even-parity (aref buffer b)))
                  (when (< (ecp-parity-errors ecp) 2)
                    (when (zerop (ecp-parity-errors ecp))
                      (setf (ecp-parity-index ecp) (ecp-count ecp)))
                    (incf (ecp-parity-errors ecp)))
                  (incf (statistics-parity-errors stats)))
                (setf (aref (ecp-block ecp) (ecp-count ecp)) (aref buffer b))
                (incf (ecp-count ecp))
                (incf b)
                (decf c))
             (incf (statistics-bytes-received stats) (- count c))
             (setf count c)
             (cond
               ((< 1 (ecp-parity-errors ecp))
                ;; (print '(< 1 (ecp-parity-errors ecp))) (finish-output)
                (incf (statistics-uncorrectable-count stats))
                (setf (ecp-state ecp) :ecp-start)
                (when send-nak (funcall send-nak)))
               ((= (ecp-count ecp) +ecp-block-size+)
                ;; (print '(= (ecp-count ecp) +ecp-block-size+)) (finish-output)
                ;; check the block
                (if (/= 0 (aref (ecp-block ecp) 16)) ; 17th byte
                    (progn                           ; invalid block
                      ;; (print '(/= 0 (aref (ecp-block ecp) 16))) (finish-output)
                      (incf (statistics-invalid-count stats))
                      (setf (ecp-state ecp) :ecp-start)
                      (when send-nak (funcall send-nak)))
                    (let ((poly (poly-from-bytes (ecp-block ecp)))
                          remainder)
                      ;; (print '(= 0 (aref (ecp-block ecp) 16))) (finish-output)
                      (remove-bit7 poly)
                      (setf remainder (remainder32 poly +g+))
                      (assert (< remainder 128)) ;; since deg(+g+)=7
                      (cond
                        ((and (zerop remainder) (zerop (ecp-parity-errors ecp)))
                         ;; correct block
                         ;; (print '(and (zerop remainder) (zerop (ecp-parity-errors ecp)))) (finish-output)
                         (incf (statistics-correct-count stats))
                         (clear-parity (ecp-block ecp) :end (- +ecp-block-size+ 2))
                         (let ((cnt (remove-escapes (ecp-block ecp) :end (- +ecp-block-size+ 2)))
                               (bn  (ecp-block-number ecp)))
                           (setf (ecp-block-number ecp) (mod (1+ (ecp-block-number ecp)) 16)
                                 (ecp-state ecp) :ecp-start)
                           (return-from process-input-buffer
                             (values (subseq (ecp-block ecp) 0 cnt) b bn stats))))
                        ((and (plusp remainder) (= 1 (ecp-parity-errors ecp)))
                         ;; (print '(and (plusp remainder) (= 1 (ecp-parity-errors ecp)))) (finish-output)
                         ;; correctable block
                         (let* ((rank (aref *ecp-rank* remainder))
                                (index (- (- +ecp-block-size+ 2) (truncate (1+ rank) 8))))
                           ;; NOTE:
                           ;; 	rank |----> (index,bit)
                           ;; 	              = (16,rank) if rank < 7
                           ;; 				  = (16-((rank+1)/8),(rank+1)%8) if rank >= 7
                           ;; 	but we don't care and don't correct the CRC byte when rank<7 because 
                           ;; 	the CRC byte is not used thereafter.
                           (if (= index (ecp-parity-index ecp))
                               (progn     ; corrected block
                                 (incf (statistics-corrected-count stats))
                                 (setf (aref (ecp-block ecp) index) (logxor (aref (ecp-block ecp) index)
                                                                            (ash 1 (mod (1+ rank) 8))))
                                 (clear-parity (ecp-block ecp) :end (- +ecp-block-size+ 2))
                                 (let ((cnt (remove-escapes (ecp-block ecp) :end (- +ecp-block-size+ 2)))
                                       (bn  (ecp-block-number ecp)))
                                   (setf (ecp-block-number ecp) (mod (1+ (ecp-block-number ecp)) 16)
                                         (ecp-state ecp) :ecp-start)
                                   (return-from process-input-buffer
                                     (values (subseq (ecp-block ecp) 0 cnt) b bn stats))))
                               (progn     ; uncorrected block
                                 (incf (statistics-uncorrected-count stats))
                                 (setf (ecp-state ecp) :ecp-start)
                                 (when send-nak (funcall send-nak))))))
                        (t
                         ;; (print 'else-2) (finish-output)
                         ;; uncorrectable block
                         (incf (statistics-uncorrectable-count stats))
                         (setf (ecp-state ecp) :ecp-start)
                         (when send-nak (funcall send-nak)))))))
               (t
                ;; else wait for a full block
                ;; (print 'else) (finish-output)
                ))))
      (loop :while (plusp c) :do
         (ecase (ecp-state ecp)
           (:ecp-start
            (loop :named skip-nuls
               :while (and (plusp c) (zerop (aref buffer b)))
               :do (decf c) (incf b))
            (setf (ecp-state ecp) :ecp-block
                  (ecp-parity-errors ecp) 0
                  (ecp-count ecp) 0)
            (incf (statistics-bytes-received stats) (- count c))
            (setf count c)
            (process-block))
           (:ecp-block
            (process-block))
           (:ecp-syn
            ;; wait for a SYN SYN 4x sequence
            (loop :while (and (plusp c) (eql :ecl-syn (ecp-state ecp))) :do
               (let ((byte (logand (aref buffer b) #x7f)))
                 (if (= (ecp-count ecp) 2) ; get the 4x
                     (progn
                       (when cancel-nak (funcall cancel-nak))
                       (if (and (= (aref buffer b) (even-parity byte))
                                (= byte (+ #x40 (ecp-block-number ecp))))
                           (progn
                             (incf (statistics-valid-syn-count stats))
                             (setf (ecp-state ecp) :ecp-start
                                   (ecp-parity-errors ecp) 0
                                   (ecp-count ecp) 0))
                           (progn
                             (incf (statistics-invalid-syn-count stats))
                             (when send-nak (funcall send-nak)))))
                     (progn
                       (if (= (aref buffer b) (even-parity byte))
                           (if (= byte #x16) 
                               (incf (ecp-count ecp)) ; one more SYN
                               (progn                 ; Not SYN
                                 (incf (statistics-invalid-syn-count stats) (ecp-count ecp))
                                 (setf (ecp-count ecp) 0)))
                                        ; parity error
                           (progn
                             (incf (statistics-parity-error stats))
                             (when (= byte #x16)
                               (incf (statistics-invalid-syn-count stats))
                               (when send-nak (funcall send-nak)))
                             (setf (ecp-count ecp) 0)))))
                 (incf b)
                 (decf c)))
            (incf (statistics-bytes-received stats) (- count c))
            (setf count c)))))
    (values nil b nil stats)))




(defun process-output-buffer (ecp buffer &key (start 0) (end nil) (last nil))
  "
ECP:        An ECP-DATA structure maintaining the persistent data
            between calls to PROCESS-OUTPUT-BUFFER.
BUFFER:     Source data octet vector (only 7-bit values).
START:      Index of the first octet in BUFFER to process.
END:        Index of the first octet in BUFFER after the last to process.
LAST:       When true, if there's not enough bytes in the BUFFER to
            fill a packet, the packet is padded with NULs.
DO:         Encode an ECP packet from bytes obtained from BUFFER, between START and END.
RETURN:     index of first byte not processed from BUFFER; (ecp-block ecp) -- when a packet is complete,
            index of first byte not processed from BUFFER; nil             -- when the package is not complete.
"
  (let* ((end (or end (length buffer)))
         (i start))
    (flet ((generate (byte)
             (setf (aref (ecp-block ecp) (ecp-count ecp)) (even-parity byte))
             (incf (ecp-count ecp))
             (when (<= (- +ecp-block-size+ 2) (ecp-count ecp))
               (setf (aref (ecp-block ecp) (- +ecp-block-size+ 1)) 0
                     (aref (ecp-block ecp) (- +ecp-block-size+ 2)) 0)
                ;; (set-parity (ecp-block ecp) :end (- +ecp-block-size+ 2))
                (let ((poly (poly-from-bytes (ecp-block ecp)))
                      remainder)
                  (remove-bit7 poly)
                  (setf remainder (remainder32 poly +g+)
                        (aref (ecp-block ecp) (- +ecp-block-size+ 2)) (even-parity remainder)
                       (ecp-count ecp) 0))
               (return-from process-output-buffer (values i (ecp-block ecp))))))
      (loop
         :while (and (< (ecp-count ecp) (- +ecp-block-size+ 2))
                     (< i end))
         :do (let ((byte (logand (aref buffer i) #x7f)))
               (incf i)
               (if (or (= byte dle) (= byte nul) (= byte syn) (= byte nak))
                   (if (< (ecp-count ecp) (- +ecp-block-size+ 4))
                       (progn (generate dle) (generate nul))
                       (generate nul))
                   (generate byte))))
      (when last
        (loop
           :while (< (ecp-count ecp) (- +ecp-block-size+ 2))
           :do (generate 0)))
      (values i nil))))



(defun test ()
  (let* ((text "Hao Wang, logicien americain.
L'algorithme en question a ete publie en 1960 dans l'IBM Journal,
article intitule \"Toward Mechanical Mathematics\", avec des variantes et
une extension au calcul des predicats. Il s'agit ici du \"premier
programme\" de Wang, systeme \"P\".
")
         (data (map '(vector (unsigned-byte 8)) (function char-code) text))
         (ecp (make-ecp-data))
         (stats (make-statistics))
         (packets '()))
    (start-ecp ecp)
    (loop
       :with start = 0
       :while (< start (length data))
       :do (multiple-value-bind (next packet) (process-output-buffer ecp data :start start :last t)
             (when packet
               ;; (print packet) (finish-output)
               (push (copy-seq packet) packets))
             (when (= start next)
               (error "no change in start ~D" start))
             (setf start next)))
    (setf packets (reverse packets))
    (let ((stream (make-array (* 20 (length packets))
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (loop
         :with i = 0
         :for packet :in packets
         :do (progn (replace stream packet :start1 i)
                    ;; (print i) (finish-output)
                    (incf i (+ 17 (random 3)))))
      ;; (print stream) (print (map 'string (lambda (x) (code-char (logand x #x7f))) stream)) (finish-output)
      (start-ecp ecp)
      (assert (string= (let ((*print-pretty* nil)
                             (*print-escape* nil))
                         (with-output-to-string (out)
                           (loop
                              :with start = 0
                              :while (< start (length stream))
                              :do (multiple-value-bind (data-chunk next block-number) (process-input-buffer ecp stream :start start :statistics stats)
                                    (setf start next)
                                    ;; (print (list start block-number data-chunk  (map 'string (function code-char) data-chunk))) (finish-output)                 
                                    (princ (map 'string (function code-char) data-chunk) out))
                              :finally (print stats) (terpri) (finish-output)
                              )))
                       text))
      :success)))
 
;;;; THE END ;;;;
