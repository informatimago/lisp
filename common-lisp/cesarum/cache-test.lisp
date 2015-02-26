;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cache-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests cache.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from cache.lisp.
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CACHE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CACHE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CACHE"
                "CACHE-MAP-ENTRIES")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CACHE.TEST")

(defvar *test-counter* 0)
(defvar *test-cache*   nil)
(defvar *test-cache-2* nil)

(define-test test/cache (&key (verbose t))
  (let ((*standard-output* (if verbose
                               *standard-output*
                               (make-broadcast-stream))))
   (ignore-errors (map nil (function delete-file)
                    (directory "/tmp/cache/**/*.*")))
   (setf *test-counter* 0)
   (let ((delay 7))
     (flet ((producer (key) (values (format nil "~A-~A" key 
                                            (incf *test-counter* ))
                                    (+ delay (get-universal-time))))
            (print-files ()
              (dolist (file (sort (mapcar (function namestring) (directory "/tmp/cache/**/*.*"))
                                  (function string<)))
                (princ file) (terpri))))
       (setf *test-cache* (make-cache #p"/tmp/cache/" (function producer) 
                                      :value-file-type "SYM"))
       (assert-true (string= (cache-get *test-cache* :one)   "ONE-1"))
       (assert-true (string= (cache-get *test-cache* :two)   "TWO-2"))
       (assert-true (string= (cache-get *test-cache* :three) "THREE-3"))
       (assert-true (string= (cache-get *test-cache* :one)   "ONE-1"))
       (assert-true (string= (cache-get *test-cache* :two)   "TWO-2"))
       (assert-true (string= (cache-get *test-cache* :three) "THREE-3"))
       (setf *test-cache-2* (make-cache #p"/tmp/cache/" (function producer)))
       (assert-true (string= (cache-get *test-cache-2* :one)   "ONE-1"))
       (assert-true (string= "SYM" (cache-value-file-type *test-cache-2*)))
       (format t "~2&filled:~%")(finish-output)
       (print-files)
       (cache-expire *test-cache* :one)
       (cache-expire *test-cache* :two :keep-file t)
       (format t "~2&expired :one and :two:~%")(finish-output)
       (print-files)
       (assert-true (string= (cache-get *test-cache* :one)   "ONE-4"))
       (format t "~2&expirations~%~:{~15A in ~4D seconds~%~}"
               (cache-map-entries *test-cache*
                                  'list (lambda (entry)
                                          (list
                                           (entry-key entry)
                                           (- (entry-expire-date entry)
                                              (get-universal-time))))))
       (format t "~2&waiting ~D s expiration of :one and :three:~%" delay)
       (finish-output)
       (sleep (1+ delay))
       (assert-true (string= (cache-get *test-cache* :one)   "ONE-5"))
       (assert-true (string= (cache-get *test-cache* :three) "THREE-6"))
       (cache-expire-all *test-cache*)
       (format t "~2&expired all~%")(finish-output)
       (print-files)
       (assert-true (string= (cache-get *test-cache* :one)   "ONE-7"))
       (assert-true (string= (cache-get *test-cache* :three) "THREE-8"))
       (assert-true (string= (cache-get *test-cache-2* :one)   "ONE-7"))
       (assert-true (string= (cache-get *test-cache-2* :three) "THREE-8"))
       (cache-map-entries *test-cache* nil (function print))))))




#||

(define-message html-page-req (sender uri))
(define-message html-page-rep (sender page-ref))
(define-message html-tree-req (sender uri))
(define-message html-tree-rep (sender tree-ref))


(send (make-instance 'html-page-req :sender myself :uri uri))

(loop for mesg = (message-receive-sexp queue +cache-message-type+)
     (case (car mesg)
       ((:get-html-page)
        (let* ((sender (first mesg))
               (uri    (second mesg))
               (page   (get-resource-at-uri uri)))
          (if page 
           ;; TODO: actually copy page to shared memory and send only a reference.
              (message-send-sexp queue sender (list :html-page uri page))
              (progn
            ;; if the request is already in the queue, then forget it.
            ;; if it comes from somebody else, then keep it
            ;; keep the request in a queue:
                (save-request mesg)
            ;; only proceed if the uri is not in the request queue.
                (message-send-sexp queue *fetcher* (list :fetch-uri uri))))))
       ((:get-html-tree)
    ;; about the same, but if the tree is not in the cache, check first for
    ;; the page and skip fetching: just request processing
        )
       ((:fetched-resource)
        )))

||#

(define-test test/all ()
  (test/cache :verbose nil))

;;;; THE END ;;;;
