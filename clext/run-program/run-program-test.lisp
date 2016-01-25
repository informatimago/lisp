;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               run-program-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests the run-program function.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-25 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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

(defpackage "COM.INFORMATIMAGO.CLEXT.RUN-PROGRAM.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.CLEXT.RUN-PROGRAM")
  (:export "TEST/RUN-PROGRAM")
  (:documentation "
"))
(in-package "COM.INFORMATIMAGO.CLEXT.RUN-PROGRAM.TEST")



(defun dump-stream (stream)
  (loop :for line = (read-line stream nil nil) :while line :do (write-line line)))

(defun dump-file (path)
 (with-open-file (inp path) (dump-stream inp)))

(defun text-file-contents (path)
  (with-open-file (inp path)
    (loop
      :for line = (read-line inp nil nil)
      :while line :collect line)))

(defun text-stream-contents (inp)
  (loop
    :for line = (read-line inp nil nil)
    :while line :collect line))


(defun create-test-input-file ()
  (with-open-file (testinp  "TESTINP.TXT"
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (write-string "Hao
Wang
logicien
americain
algorithme
en
question
a
ete
publie
en
1960
dans
IBM
Journal
"
                  testinp)))


(defun check-sorted-output-stream (stream)
  (let ((stream-contents (text-stream-contents stream))
        (sorted-list (sort (copy-seq '("Hao" "Wang" "logicien" "americain"
                                       "algorithme" "en" "question" "a" "ete" "publie"
                                       "en" "1960" "dans" "IBM" "Journal"))
                           (function string<))))
    (check equal sorted-list stream-contents
           () "~%~20A=~S~%~20A=~S~%"
           "sorted-list" sorted-list
           "stream-contents" stream-contents)))


(defun check-sorted-output-file ()
  (with-open-file (stream "TESTOUT.TXT")
    (check-sorted-output-stream stream)))



(defvar *verbose* nil)


(define-test test/run-program (&key ((:verbose *verbose*) *verbose*))

  (create-test-input-file)
  
  (assert-true (zerop (process-status (run-program "true" '() :wait t))))
  (assert-true (zerop (process-status (prog1 (run-program "true" '() :wait nil)
                                        (sleep 1)))))

  (assert-true (plusp (process-status (run-program "false" '() :wait t))))
  (assert-true (plusp (process-status (prog1 (run-program "false" '() :wait nil)
                                        (sleep 1)))))

  (run-program "true" '() :wait nil :error "TESTERR.TXT")
  (sleep 1)
  (check equal '() (text-file-contents "TESTERR.TXT"))

  (run-program "true" '() :wait t :error "TESTERR.TXT")
  (check equal '() (text-file-contents "TESTERR.TXT"))



  (let ((process (run-program "sh" '("-c" "echo error 1>&2")
                              :wait nil :input nil :output nil :error :stream)))
    (check equal '("error")
          (unwind-protect
               (text-stream-contents (process-error process))
            (close (process-error process)))))

  
  (ignore-errors (delete-file "TESTERR.TXT"))
  (run-program "sh" '("-c" "echo error 1>&2")
               :wait t :input nil :output nil :error "TESTERR.TXT")
  (check equal '("error") (text-file-contents "TESTERR.TXT"))

  
  (with-open-file (err "TESTERR.TXT"
                       :direction :output :if-does-not-exist :create :if-exists :supersede
                       #+ccl :sharing #+ccl :lock
                       )
    (run-program "sh" '("-c" "echo error 1>&2")
                 :wait t :input nil :output nil :error err))
  (check equal '("error") (text-file-contents "TESTERR.TXT"))


  (run-program "printf" '("Hello\\nWorld\\n") :wait t)

  (run-program "printf" '("Hello\\nWorld\\n") :wait nil)

  (let ((process (run-program "printf" '("Hello\\nWorld\\n") :wait nil :output :stream)))
    (check equal '("Hello" "World")
          (unwind-protect
               (loop
                 :for line = (read-line (process-output process) nil nil)
                 :while line :collect line)
            (close (process-output process)))))


  (with-open-file (out  "TESTOUT.TXT"
                        :direction :output :if-does-not-exist :create :if-exists :supersede
                       #+ccl :sharing #+ccl :lock)
     (run-program "printf" '("Hello\\nWorld\\n") :wait nil :output out)
     (sleep 1))
  (check equal '("Hello" "World") (text-file-contents "TESTOUT.TXT"))


  (with-open-file (out  "TESTOUT.TXT"
                        :direction :output :if-does-not-exist :create :if-exists :supersede
                       #+ccl :sharing #+ccl :lock)
    (run-program "printf" '("Hello\\nWorld\\n") :wait t :output out))
  (check equal '("Hello" "World") (text-file-contents "TESTOUT.TXT"))


  (run-program "sort" '() :environment '(("LC_CTYPE"   . "C") ("LC_COLLATE" . "C"))
               :wait nil :input "TESTINP.TXT" :output "TESTOUT.TXT")
  (sleep 1)
  (check-sorted-output-file)

  (with-open-file (out "TESTOUT.TXT"
                       :direction :output :if-does-not-exist :create :if-exists :supersede
                       #+ccl :sharing #+ccl :lock)
    (run-program "sort" '() :environment '(("LC_CTYPE"   . "C") ("LC_COLLATE" . "C"))
                 :wait t :input "TESTINP.TXT" :output out))
  (check-sorted-output-file)

  (with-open-file (inp  "TESTINP.TXT"
                       #+ccl :sharing #+ccl :lock)
    (run-program "sort" '() :environment '(("LC_CTYPE"   . "C") ("LC_COLLATE" . "C"))
                 :wait t :input inp :output "TESTOUT.TXT"))
  (check-sorted-output-file)

  (with-open-file (out "TESTOUT.TXT"
                       :direction :output :if-does-not-exist :create :if-exists :supersede
                       #+ccl :sharing #+ccl :lock)
    (with-open-file (inp  "TESTINP.TXT"
                       #+ccl :sharing #+ccl :lock)
      (run-program "sort" '() :environment '(("LC_CTYPE"   . "C") ("LC_COLLATE" . "C"))
                   :wait t :input inp :output out)))
  (check-sorted-output-file)


  (let ((process (run-program "sort" '() :environment '(("LC_CTYPE"   . "C") ("LC_COLLATE" . "C"))
                              :wait nil :input :stream :output :stream :error nil)))
    (with-open-file (testinp  "TESTINP.TXT"
                       #+ccl :sharing #+ccl :lock)
      (loop :for line = (read-line testinp nil nil)
        :while line :do (write-line line (process-input process))))
    (close (process-input process))
    (check-sorted-output-stream (process-output process))
    (close (process-output process)))

  (mapc (lambda (file) (ignore-errors (delete-file file)))
        '("TESTINP.TXT"  "TESTOUT.TXT"  "TESTERR.TXT")))


(defun test/all ()
  (test/run-program :verbose nil))

;;;; THE END ;;;;
