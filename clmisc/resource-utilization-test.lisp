;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               resource-utilization-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests resource-utilization.lisp
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from resource-utilization.lisp
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

(defpackage "COM.INFORMATIMAGO.CLMISC.RESOURCE-UTILIZATION.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.CLMISC.RESOURCE-UTILIZATION")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.CLMISC.RESOURCE-UTILIZATION.TEST")


(define-test test/read-parenthesized-string ()
  (loop
     :with success = 0
     :for tcount :from 0
     :for (input . output)
     :in '(("" :eof) ("  " :eof) ("(" :eof) (" ( " :eof)
           (" (a(b)" :eof) (" (a(b)c" :eof) (" (a\\" :eof)  (" (a\\b" :eof)
           ("  (howdy doo ?)" "howdy doo ?")
           ("(howdy \\( doo ?)" "howdy ( doo ?")
           ("(howdy \\) doo ?)" "howdy ) doo ?")
           ("(a(b(c(d)e)f)g)h" "a(b(c(d)e)f)g")
           )
     :for result = (with-input-from-string (stream input)
                     (multiple-value-list
                      (ignore-errors
                        (read-parenthesized-string stream nil :eof))))
     :do (if (equal result output)
             (progn
               (incf success)
               (progress-success))
             (progress-failure input "~2%Reading parenthesized string ~S~
                                       ~%     --> ~S~%expected ~S~%"
                               input result output))
     :finally  (format t "~&~30A ~4D cases, ~4D successful  (~6,1F %)~%"
                       'read-parenthesized-string
                       tcount success (/ success tcount 0.01))))


(define-test test/all ()
  (test/read-parenthesized-string))



#||

(reporting-sru ()
  (with-open-file (input "/usr/share/dict/words")
    (loop :for line = (read-line input nil nil) :while line))
  (loop :repeat 5000 :collect (make-string 1000) :finally (terpri) (return  nil)))

||#


;;;; THE END ;;;;
