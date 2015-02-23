;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pbxproj-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    pbxproj tests.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-23 <PJB> Extracted from pbxproj.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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
(defpackage "COM.INFORMATIMAGO.XCODE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.XCODE"
        "COM.INFORMATIMAGO.RDP"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.XCODE.TEST")

(defun test-scan-stream (src)
  (loop
    :with scanner = (make-instance 'com.informatimago.xcode::pbxproj-scanner :source src :state 0)
    ;; :initially (progn
    ;;              (advance-line scanner)
    ;;              (format t "~2%;; ~A~%;; ~A~%"
    ;;                      (scanner-buffer scanner)
    ;;                      (scanner-current-token scanner)))
    :do (progn
          (scan-next-token scanner)
          (format t "~&~3A ~20A ~20S ~3A ~3A ~20A ~A~%"
                  (scanner-state scanner)
                  (token-kind (scanner-current-token scanner))
                  (token-text (scanner-current-token scanner))
                  (eofp (scanner-current-token scanner))
                  (eofp (scanner-current-token scanner))
                  "-" ;; (scanner-previous-token-kind scanner) 
                  (type-of (scanner-current-token scanner)))
          (finish-output))
    :while (scanner-current-token scanner)))

(defun test-scan-file (path)
  (with-open-file (src path)
    (test-scan-stream src)))

(defun test-scan-string (source)
  (with-input-from-string (src source)
    (test-scan-stream src)))

(defun test-parse-stream (src)
  (let ((scanner (make-instance 'com.informatimago.xcode::pbxproj-scanner :source src :state 0)))
    (com.informatimago.xcode::parse-pbxproj scanner)))

(defun test-parse-file (path)
  (with-open-file (src path)
    (test-parse-stream src)))

(defun test-parse-string (source)
  (with-input-from-string (src source)
    (test-parse-stream src)))

;; (test-scan-file  #P"~/works/abalone-macosx/Abalone-10.7/Abalone.xcodeproj/project.pbxproj")

(define-test test/parse-file ()
  (let ((pbxproj-path (merge-pathnames #P"test.pbxproj" (load-time-value *load-truename*) nil)))
    (assert-true (with-output-to-string (*standard-output*) (test-scan-file  pbxproj-path)))
    (assert-true (with-output-to-string (*standard-output*) (test-parse-file pbxproj-path)))))


(define-test test/all ()
  (test/parse-file))

;;;; THE END ;;;;
