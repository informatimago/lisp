;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pipe-stream-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests the pipe streams.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-09-13 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.CLEXT.PIPE.TEST"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.CLEXT.PIPE")
  #+debug-gate (:import-from "COM.INFORMATIMAGO.CLEXT.GATE" "TR" "*TR-OUTPUT*")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.CLEXT.PIPE.TEST")


#-(and)
(:export "MAKE-PIPE" "PIPE" "PIPE-INPUT-STREAM" "PIPE-OUTPUT-STREAM" "PIPE-ELEMENT-TYPE"
         "PIPE-CHARACTER-INPUT-STREAM"
         "PIPE-CHARACTER-OUTPUT-STREAM"
         "PIPE-BINARY-INPUT-STREAM"
         "PIPE-BINARY-OUTPUT-STREAM")

(deftype octet () '(unsigned-byte 8))


(defparameter *text-data*
  "
Hao Wang, logicien americain.

L'algorithme en  question  a  ete  publie  en  1960  dans l'IBM Journal,
article intitule \"Toward  Mechanical Mathematics\", avec des variantes et
une  extension au calcul  des  predicats.  Il  s'agit  ici  du  \"premier
programme\" de Wang, systeme \"P\".

L'article a ete ecrit en 1958, et les experiences effectuees sur IBM 704
- machine a lampes, 32 k  mots  de 36 bits, celle-la meme qui vit naitre
LISP a la meme epoque. Le programme  a  ete ecrit en assembleur (Fortran
existait, mais il ne s'etait pas encore impose)  et  l'auteur estime que
\"there is very little in the program that is not straightforward\".

Il observe que les preuves engendrees sont \"essentiellement des arbres\",
et  annonce  que  la  machine  a  demontre 220 theoremes du  calcul  des
propositions  (tautologies)  en  3  minutes. Il en tire argument pour la
superiorite  d'une  approche  algorithmique  par  rapport a une approche
heuristique comme celle du \"Logic Theorist\" de Newell, Shaw et  Simon (a
partir de 1956 sur la machine JOHNNIAC de la Rand Corporation): un debat
qui dure encore...

Cet  algorithme  a  ete popularise par J. McCarthy, comme exemple-fanion
d'application  de LISP. Il figure dans le manuel de la premiere  version
de  LISP  (LISP  1,  sur IBM 704 justement, le manuel est date  de  Mars
1960), et il a ete repris dans le celebre \"LISP 1.5 Programmer's Manual\"
publie en 1962 par MIT Press, un des maitres-livres de l'Informatique.

")



(defparameter *binary-data*
  (make-array (length *text-data*)
              :element-type 'octet
              :initial-contents (map 'vector (function char-code) *text-data*)))

(defun make-binary-input     (stream buffer-size)
  (declare (ignore buffer-size))
  (lambda ()
    (let ((i 0)
          (expected-byte 0)
          (read-byte 0))
     (unwind-protect
          (loop
            :do (setf expected-byte (and (< i (length *binary-data*))
                                         (aref *binary-data* i))
                      read-byte     (read-byte stream nil nil))
            :while read-byte
            :do (incf i)
            :do (assert-true (= expected-byte read-byte)))
       (check = i (length *binary-data*))
       (assert-true (and (null expected-byte)
                         (null read-byte)))
       (expect-condition 'end-of-file (read-byte stream))
       (close stream)))))

(defun make-binary-output    (stream buffer-size)
  (declare (ignore buffer-size))
  (lambda ()
    (unwind-protect
         (loop :for byte :across *binary-data*
               :do (write-byte byte stream))
      (close stream))))

(defun make-binary-buffer (size)
  (make-array size :element-type 'octet :initial-element 0))

(defun make-sequence-input   (stream buffer-size)
  (lambda ()
    (let* ((buffer-size     (or buffer-size 128))
           (validated-index 0)
           (read-position   0)
           (read-byte       0)
           (small-buffer    (make-binary-buffer (ceiling buffer-size 7)))
           (exact-buffer    (make-binary-buffer buffer-size))
           (big-buffer      (make-binary-buffer (truncate (* 5/2 buffer-size)))))
      (flet ((check-read-chunk (buffer size validated-index)
               (loop
                 :for i :below size
                 :for read-byte = (aref buffer i)
                 :for expected-byte = (aref *binary-data* validated-index)
                 :do (check = read-byte expected-byte)
                     (incf validated-index)
                     (assert-true (or (= (1+ i) size)
                                      (< validated-index (length *binary-data*))))
                 :finally (return validated-index))))
        (unwind-protect
             (progn
               (loop
                 :repeat 3
                 :while (< validated-index (length *binary-data*))
                 :do (setf read-position (read-sequence exact-buffer stream))
                     (check = read-position (length exact-buffer))
                     (setf validated-index (check-read-chunk exact-buffer read-position validated-index))
                 :finally ;; check that we're not done yet:
                          (assert-true (< validated-index (length *binary-data*))))
               (loop
                 :repeat 9
                 :while (< validated-index (length *binary-data*))
                 :do (setf read-position (read-sequence small-buffer stream))
                     (check = read-position (length small-buffer))
                     (setf validated-index (check-read-chunk small-buffer read-position validated-index))
                 :finally ;; check that we're not done yet:
                          (assert-true (< validated-index (length *binary-data*))))
               (loop
                 :while (< validated-index (length *binary-data*))
                 :do (setf read-byte (read-byte stream nil nil))
                     (assert-true read-byte)
                     (setf validated-index (check-read-chunk (vector read-byte) 1 validated-index))
                 :while (/= read-byte 10)
                 :finally ;; check that we're not done yet:
                          (assert-true (< validated-index (length *binary-data*))))
               (loop
                 :while (< validated-index (length *binary-data*))
                 :do (setf read-position (read-sequence big-buffer stream))
                     (setf validated-index (check-read-chunk big-buffer read-position validated-index))
                     (when (< validated-index (length *binary-data*))
                       (check = read-position (length big-buffer)))
                 :finally ;; check that we're done:
                          (assert-false (< validated-index (length *binary-data*)))))
          (expect-condition 'end-of-file (read-byte stream))
          (close stream))))))

(defun make-sequence-output  (stream buffer-size)
  (lambda ()
    (let* ((buffer-size     (or buffer-size 128))
           (sent-index      0)
           (sent-byte       0)
           (end             0)
           (small-buffer    (make-binary-buffer (ceiling buffer-size 7)))
           (exact-buffer    (make-binary-buffer buffer-size))
           (big-buffer      (make-binary-buffer (truncate (* 5/2 buffer-size)))))
      (flet ((get-chunk (buffer sent-index)
               (let ((size (min (length buffer) (- (length *binary-data*) sent-index))))
                 (replace buffer *binary-data* :end1 size :start2 sent-index)
                 (values (incf sent-index size) size))))
        (unwind-protect
             (progn
               (loop
                 :repeat 2
                 :while (< sent-index (length *binary-data*))
                 :do (multiple-value-setq (sent-index end) (get-chunk exact-buffer sent-index))
                     (write-sequence exact-buffer stream :end end)
                 :finally ;; check that we're not done yet:
                          (assert-true (< sent-index (length *binary-data*))))
               (loop
                 :repeat 8
                 :while (< sent-index (length *binary-data*))
                 :do (multiple-value-setq (sent-index end) (get-chunk small-buffer sent-index))
                     (write-sequence small-buffer stream :end end)
                 :finally ;; check that we're not done yet:
                          (assert-true (< sent-index (length *binary-data*))))
               (loop
                 :while (< sent-index (length *binary-data*))
                 :do (setf sent-byte (aref *binary-data* sent-index))
                     (incf sent-index)
                     (write-byte sent-byte stream)
                 :while (/= sent-byte 10)
                 :finally ;; check that we're not done yet:
                          (assert-true (< sent-index (length *binary-data*))))
               (loop
                 :while (< sent-index (length *binary-data*))
                 :do (multiple-value-setq (sent-index end) (get-chunk big-buffer sent-index))
                     (write-sequence big-buffer stream :end end)
                 :finally ;; check that we're done:
                          (assert-false (< sent-index (length *binary-data*)))))
          (close stream))))))



(defun make-character-buffer (size)
  (make-array size :element-type 'character :initial-element #\space))

(defun make-character-input  (stream buffer-size)
  (declare (ignore buffer-size))
  (lambda ()
    (let ((i 0)
          (expected-char 0)
          (read-char 0))
      (unwind-protect
           (loop
             :do (setf expected-char (and (< i (length *text-data*))
                                          (aref *text-data* i))
                       read-char     (read-char stream nil nil))
             :while read-char
             :do (incf i)
             :do (check char= read-char expected-char))
        (check = i (length *text-data*))
        (assert-true (and (null expected-char)
                          (null read-char)))
        (expect-condition 'end-of-file (read-char stream))
        (close stream)))))

(defun make-character-output (stream buffer-size)
  (declare (ignore buffer-size))
  (lambda ()
    (unwind-protect
         (loop :for char :across *text-data*
               :do (write-char char stream))
      (close stream))))

(defun make-line-input       (stream buffer-size)
  (declare (ignore buffer-size))
  (lambda ()
    (unwind-protect
         (with-input-from-string (inp *text-data*)
           (loop :for expected-line := (read-line inp nil nil)
                 :for read-line := (read-line stream nil nil)
                 :do (check equal read-line expected-line)
                 :while expected-line
                 :finally (assert-true (null read-line))))
      (close stream))))

(defun make-line-output      (stream buffer-size)
  (declare (ignore buffer-size))
  (lambda ()
    (unwind-protect
         (with-input-from-string (inp *text-data*)
           (loop :for line := (read-line inp nil nil)
                 :while line
                 :do (write-line line stream)))
      (close stream))))

(defun make-string-input     (stream buffer-size)
  (let ((expected-buffer (make-character-buffer (or buffer-size 128)))
        (read-buffer     (make-character-buffer (or buffer-size 128))))
    (lambda ()
      (unwind-protect
           (with-input-from-string (inp *text-data*)
             (loop :for i :from 0
                   :for expected-size := (read-sequence expected-buffer inp)
                   :for read-size     := (read-sequence read-buffer  stream)
                   :while (plusp read-size)
                   :do (check equal read-size expected-size)
                       (assert-true (string= read-buffer expected-buffer
                                             :end1 read-size
                                             :end2 expected-size))
                   :finally (assert-true (zerop read-size))))
        (close stream)))))

(defun make-string-output    (stream buffer-size)
  (let ((buffer (make-character-buffer (or buffer-size 128))))
    (lambda ()
      (unwind-protect
           (with-input-from-string (inp *text-data*)
             (loop :for i :from 0
                   :for size := (read-sequence buffer inp)
                   :while (plusp size)
                   :do (write-string buffer stream :end size)
                       (when (zerop (mod i 10))
                         (finish-output stream))))
        (close stream)))))


(defvar *last-pipe* nil)

(defun test/io (pipe-kind out-kind in-kind debug element-type stem makers)
  (check-type debug (or null (member :producer :consumer)))
  (let* ((buffer-size (ecase pipe-kind
                        (:queued   nil)
                        (:buffered 133)))
         (pipe        (make-pipe :element-type element-type
                                 :buffer-size buffer-size
                                 :name (format nil "~A/~A/~A/~A"
                                               stem pipe-kind out-kind in-kind)))
         (output      (pipe-output-stream pipe))
         (input       (pipe-input-stream  pipe))
         (bindings    `((*standard-output* . ,*standard-output*)
                        (*standard-input* . ,*standard-input*)
                        (*error-output* . ,*error-output*)
                        (*trace-output* . ,*trace-output*)
                        (*debug-io* . ,*debug-io*)
                        (*query-io* . ,*query-io*)
                        #+debug-gate (*tr-output* . ,*trace-output*)))
         (producer    (funcall (or (third (assoc out-kind makers))
                                   (error "Invalid out-kind ~S, expected one of ~{~S~^ ~}"
                                          out-kind (mapcar (function first) makers)))
                               output buffer-size))
         (consumer    (funcall (or (second (assoc out-kind makers))
                                   (error "Invalid in-kind ~S, expected one of ~{~S~^ ~}"
                                          in-kind (mapcar (function first) makers)))
                               input buffer-size))
         producer-thread consumer-thread)
    (setf *last-pipe* pipe) ; for debugging the test.
    (when (member debug '(nil :consumer))
      (setf producer-thread (make-thread producer
                                         :name (format nil "test/~A-io/producer" stem)
                                         :initial-bindings bindings)))
    (when (member debug '(nil :producer))
      (setf consumer-thread (make-thread consumer
                                         :name (format nil "test/~A-io/consumer" stem)
                                         :initial-bindings bindings)))
    (case debug
      ((:consumer) (funcall consumer))
      ((:producer) (funcall producer)))
    (when producer-thread (join-thread producer-thread))
    (when consumer-thread (join-thread consumer-thread))))


(define-test test/character-io (pipe-kind out-kind in-kind &key debug)
  (test/io pipe-kind out-kind in-kind debug 'character "character"
           '((:char     make-character-input  make-character-output)
             (:line     make-line-input       make-line-output     )
             (:sequence make-string-input     make-string-output   ))))

(define-test test/binary-io (pipe-kind out-kind in-kind &key debug)
  (test/io pipe-kind out-kind in-kind debug 'octet "binary"
           '((:byte     make-binary-input    make-binary-output  )
             (:sequence make-sequence-input  make-sequence-output))))

(define-test test/all ()
  (loop
    :for pipe :in '(:queued :buffered)
    :do (loop
          :for out :in '(:char :line :sequence)
          :do (loop
                :for in :in '(:char :line :sequence)
                :do (test/character-io pipe out in))))
  (loop
    :for pipe :in '(:queued :buffered)
    :do (loop
          :for out :in '(:byte :sequence)
          :do (loop
                :for in :in '(:byte :sequence)
                :do (test/binary-io pipe out in)))))


;;;; THE END ;;;;
