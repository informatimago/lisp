;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               queue-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests the atomic non-negative queue, blocking on decrement at 0.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2016-01-16 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.CLEXT.QUEUE.TEST"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS"
        "COM.INFORMATIMAGO.CLEXT.QUEUE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.TIME")
  (:export
   "TEST/ALL")
  (:documentation "Tests the thread-safe message queue."))
(in-package "COM.INFORMATIMAGO.CLEXT.QUEUE.TEST")


(define-test test/queue (&optional (*standard-output* *standard-output*))
  "A simple little test.  Check the output."
  (slow-test 480
    (let ((queue (make-queue "test-queue"))
          (out   (make-lock "out")))
      (check string= (queue-name queue) "test-queue")
      (check =       (queue-count queue) 0)
      (make-thread (lambda ()
                     (loop
                       :named producer
                       :with message := 1000
                       :repeat 50
                       :do (with-lock-held (out)
                             (format t "~&Will enqueue: ~A~%" (incf message)) (finish-output))
                           (enqueue queue message)
                           (sleep (random 0.1))))
                   :name "test-queue-consummer"
                   :initial-bindings `((*standard-output* . ,*standard-output*)))
      (loop :named consumer
            :with expected := 1000
            :repeat 5
            :do (loop :repeat 10
                      :do (let ((message (dequeue queue)))
                            (with-lock-held (out)
                              (format t "~&Did  dequeue: ~A~%" message) (finish-output))
                            (check = message (incf expected))))
                (terpri) (force-output)
                (sleep 2))
      (check string= (queue-name queue) "test-queue")
      (check =       (queue-count queue) 0))))


(define-test test/all ()
  (let ((*test-output* *standard-output*))
    (test/queue (make-broadcast-stream))))


;;;; THE END ;;;;
