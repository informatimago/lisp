;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               string-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test string.lisp 
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from string.lisp
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

(defpackage "COM.INFORMATIMAGO.CLISP.STRING.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.CLISP.STRING")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.CLISP.STRING.TEST")


(defun rris-result (test-case res)
  "Common-Lisp"
  (if (or (and (eq :error res) (eq res (car test-case)))
          (string= (car test-case) res))
      (progn
        (format t "Ok  ~50W --> ~W~%" (cdr test-case) res)
        (progress-success))
      (progress-failure-message (cdr test-case)
                                "~&    expected ~W~%    got      ~W~%"
                                (car test-case) res)))


(defun rris-test ()
  "
Test cases for REPLACE-REGEXP-IN-STRING
"
  (let ((*case-fold-search* t))
    (do* ((test-cases
           ;; We use basic POSIX regexps, so no 're+'.
           ;;  result      regexp      replac      string      fix lit sub start
           '( ("xy"        ""          "x"         "y"         t   t)
             ("xyxyxyxy"  ""          "x"         "yyyy"      t   t)
             (""          "."         "x"         ""          t   t)
             ("x"         "."         "x"         "y"         t   t)
             ("xxxx"      "."         "x"         "yyyy"      t   t)
             ("xxxx"      "."         "x"         "yaya"      t   t)
             ("good"      "a"         "bad"       "good"      t   t)
             ("good"      "bad"       "good"      "bad"       t   t)
             ("good rest" "bad"       "good"      "bad rest"  t   t)
             ("rest good" "bad"       "good"      "rest bad"  t   t)
             ("good"      "bad"  (lambda (x) "good") "bad"    t   t)
             ("good rest" "bad"  (lambda (x) "good") "bad rest" t   t)
             ("rest good" "bad"  (lambda (x) "good") "rest bad" t   t)
             ("test"      "r\\(e\\)g" "good"      "test"     nil nil 2)
             (:error      "r\\(e\\)g" "good"      "reg"      nil nil 2)
             ("rgoodg"    "r\\(e\\)g" "good"      "reg"      nil nil 1)
             ("BOC NEW VALUE hoc" "pattern"   "nEW VAlue" "BOC PATTERN hoc")
             ("BOC nEW VAlue hoc" "pattern"   "nEW VAlue" "BOC pattern hoc")
             ("BOC new value hoc" "pattern"   "new value" "BOC pattern hoc")
             ("BOC NEW VAlue hoc" "pattern"   "nEW VAlue" "BOC Pattern hoc")
             ("BOC New Value hoc" "pattern"   "new value" "BOC Pattern hoc")
             ("BOC nEW VAlue hoc" "pattern"   "nEW VAlue" "BOC pATteRN hoc")
             ("BOC New1value hoc" "pattern"   "new1value" "BOC Pattern hoc")
             ("BOC New-Value hoc" "pattern"   "new-value" "BOC Pattern hoc")
             ("rrrr-www-bb rr-w-bbb"
              "\\(bb*\\) \\(ww*\\) \\(rr*\\)"
              "\\3-\\2-\\1" "bb www rrrr bbb w rr")
             ("\\4-www-bb \\4-w-bbb"
              "\\(bb*\\) \\(ww*\\) \\(rr*\\)"
              "\\4-\\2-\\1" "bb www rrrr bbb w rr")
             (:error "blue" "\\\\b\\l\\&" "blue box and bluetooth")
             ("\\bblue box and \\bbluetooth"
              "blue" "\\\\b\\&" "blue box and bluetooth"))
           (cdr test-cases))
          (test-case (car test-cases) (car test-cases))
          (tc test-case test-case)
          (all-ok t))
         ((null test-cases) all-ok)
      (when (listp (nth 2 tc))
        (setq tc (copy-seq tc))
        (setf (nth 2 tc) (coerce (nth 2 tc) 'function)))
      (let ((res (handler-case
                     (apply (function replace-regexp-in-string) (cdr tc))
                   (error () :error))) )
        (if (eq :error res)
            (unless (eq res (car test-case)) (setq all-ok nil))
            (unless (string= (car test-case) res) (setq all-ok nil)))
        (rris-result  test-case res)))))



;;  (rris-test)
;;; (let ((start 0) (case-sensitive nil) (extended nil) (newline nil) (nosub nil))
;;;       (REGEXP:MATCH "blue" "blue box and bluetooth"
;;;                     :START START :IGNORE-CASE (NOT CASE-SENSITIVE)
;;;                     :EXTENDED EXTENDED  :NEWLINE NEWLINE :NOSUB NOSUB) )

;; (replace-regexp-in-string "blue" "\\\\b\\X\\&" "blue box and bluetooth")

(define-test test/rris ()
  (rris-test))

(define-test test/all ()
  (test/rris))

;;;; THE END ;;;;
