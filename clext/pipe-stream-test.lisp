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
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.CLEXT.PIPE.TEST")


#-(and)
(:export "MAKE-PIPE" "PIPE" "PIPE-INPUT-STREAM" "PIPE-OUTPUT-STREAM" "PIPE-ELEMENT-TYPE"
         "PIPE-CHARACTER-INPUT-STREAM"
         "PIPE-CHARACTER-OUTPUT-STREAM"
         "PIPE-BINARY-INPUT-STREAM"
         "PIPE-BINARY-OUTPUT-STREAM")

(deftype octet () '(unsigned-byte 8))


(define-test test/character-io (pipe-kind out-kind in-kind)
  (let* ((buffer-size (ecase pipe-kind
                        (:queued   nil)
                        (:buffered 133)))
         (pipe        (make-pipe :element-type 'character
                                 :buffer-size buffer-size))
         (output      (pipe-output-stream pipe))
         (input       (pipe-input-stream  pipe))
         (producer    (make-thread
                       (ecase out-kind
                         (:char     (make-character-output output buffer-size))
                         (:line     (make-line-output      output buffer-size))
                         (:sequence (make-string-output    output buffer-size)))
                       :named "test/character-io/producer"))
         (consumer    (make-thread
                       (ecase in-kind
                         (:char     (make-character-input input buffer-size))
                         (:line     (make-line-input      input buffer-size))
                         (:sequence (make-string-input    input buffer-size)))
                       :named "test/character-io/consumer")))
    (join-thread producer)
    (join-thread consomer)))

(define-test test/binary-io (pipe-kind out-kind in-kind)
  (let* ((buffer-size (ecase pipe-kind
                        (:queued   nil)
                        (:buffered 133)))
         (pipe        (make-pipe :element-type 'octet
                                 :buffer-size buffer-size))
         (output      (pipe-output-stream pipe))
         (input       (pipe-input-stream  pipe))
         (producer    (make-thread
                       (ecase out-kind
                         (:byte     (make-binary-output   output buffer-size))
                         (:sequence (make-sequence-output output buffer-size)))
                       :named "test/binary-io/producer"))
         (consumer    (make-thread
                       (ecase in-kind
                         (:byte     (make-binary-input    input buffer-size))
                         (:sequence (make-sequence-input  input buffer-size)))
                       :named "test/binary-io/consumer")))
    (join-thread producer)
    (join-thread consomer)))

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
