;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               lua-scanner-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test lua-scanner.lisp
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-24 <PJB> Extracted tests from lua-scanner.lisp
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
(defpackage "COM.INFORMATIMAGO.LANGUAGES.LUA.SCANNER.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
        "COM.INFORMATIMAGO.LANGUAGES.LUA.SCANNER")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LUA.SCANNER.TEST")



(defun test/scan-stream (src)
  (loop
    :with scanner = (make-instance 'lua-scanner :source src)
    :for token = (scan-next-token scanner)
    :while token
    :do (progn
          (format t "~&~20A ~32S ~32S ~A~%"
                  (token-kind  (scanner-current-token scanner))
                  (token-value (scanner-current-token scanner))
                  (token-text  (scanner-current-token scanner))
                  (type-of (scanner-current-token scanner)))
          (finish-output))))

(defun test/scan-file (path)
  (with-open-file (src path)
    (test/scan-stream src)))

(defun test/scan-string (source)
  (with-input-from-string (src source)
    (test/scan-stream src)))

;; (test/scan-file #P "~/mission.lua")

(define-test test/lua-scanner ()
  (assert-true (with-output-to-string (*standard-output*)
                 (test/scan-file (load-time-value
                                  (merge-pathnames #P"test-1.lua"
                                                   *load-truename* nil))))))

(define-test test/all/lua-scanner ()
  (test/lua-scanner))

(defun test/all ()
  (test/all/lua-scanner))

;;;; THE END ;;;;
