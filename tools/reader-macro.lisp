;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               reader-macro.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tools to deal with reader macro characters.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-21 <PJB> Renamed all-macro-characters -> list-all-macro-characters.
;;;;                     changed the tag symbols into keywords.
;;;;    2015-04-03 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.TOOLS.READER-MACRO"
  (:use "COMMON-LISP")
  (:export "LIST-ALL-MACRO-CHARACTERS"
           "REMOVE-ALL-MACRO-CHARACTERS"))
(in-package "COM.INFORMATIMAGO.TOOLS.READER-MACRO")

(defun remove-all-macro-characters (readtable)
  "DO: Removes all the reader macros and dispatching reader macros from the READTABLE.
RETURN: READTABLE."
  (loop
    :for code :below char-code-limit
    :for ch = (code-char code)
    :when (and ch (get-macro-character ch readtable))
      :do (set-macro-character  ch nil t readtable)
          (set-syntax-from-char ch #\a   readtable))
  readtable)

(defun list-all-macro-characters (&optional (*readtable* *readtable*))
  "
RETURN: A list of all the macro and dispatch-macro characters in the readtable.
NOTE:   We have the same function in the com.informatimago.common-lisp.lisp-reader.reader
        package, working on com.informatimago.common-lisp.lisp-reader.reader:readtable
        instead of cl:readtable.
"
  (check-type *readtable* readtable)
  (loop
    :with result = '()
    :for code :below char-code-limit
    :for ch = (code-char code)
    :when ch
      :do (multiple-value-bind (mc nt) (get-macro-character ch)
            (when mc
              (if (ignore-errors (progn (get-dispatch-macro-character ch #\a) t))
                  (loop :for code :below char-code-limit
                        :for sub = (code-char code)
                        :when (and sub
                                   (not (and (alpha-char-p sub) (lower-case-p sub)))
                                   (get-dispatch-macro-character ch sub))
                          :do (push (list :dispatch-macro-character nt ch sub
                                          #-(and) (format nil "~C~C" ch sub))
                                    result))
                  (push (list :macro-character nt ch
                              #-(and) (string ch))
                        result))))
    :finally (return (nreverse result))))


#-(and) (
         
         ;; (length (all-macro-characters)

         (map nil 'print (list-all-macro-characters))

         (:macro-character nil #\") 
         (:dispatch-macro-character t #\# #\Null) 
         (:dispatch-macro-character t #\# #\Tab) 
         (:dispatch-macro-character t #\# #\Newline) 
         (:dispatch-macro-character t #\# #\Page) 
         (:dispatch-macro-character t #\# #\Return) 
         (:dispatch-macro-character t #\# #\ ) 
         (:dispatch-macro-character t #\# #\#) 
         (:dispatch-macro-character t #\# #\$) 
         (:dispatch-macro-character t #\# #\&) 
         (:dispatch-macro-character t #\# #\') 
         (:dispatch-macro-character t #\# #\() 
         (:dispatch-macro-character t #\# #\)) 
         (:dispatch-macro-character t #\# #\*) 
         (:dispatch-macro-character t #\# #\+) 
         (:dispatch-macro-character t #\# #\-) 
         (:dispatch-macro-character t #\# #\.) 
         (:dispatch-macro-character t #\# #\:) 
         (:dispatch-macro-character t #\# #\<) 
         (:dispatch-macro-character t #\# #\=) 
         (:dispatch-macro-character t #\# #\>) 
         (:dispatch-macro-character t #\# #\A) 
         (:dispatch-macro-character t #\# #\B) 
         (:dispatch-macro-character t #\# #\C) 
         (:dispatch-macro-character t #\# #\O) 
         (:dispatch-macro-character t #\# #\P) 
         (:dispatch-macro-character t #\# #\R) 
         (:dispatch-macro-character t #\# #\S) 
         (:dispatch-macro-character t #\# #\X) 
         (:dispatch-macro-character t #\# #\\) 
         (:dispatch-macro-character t #\# #\_) 
         (:dispatch-macro-character t #\# #\|) 
         (:dispatch-macro-character t #\# #\Latin_Capital_Letter_E_With_Circumflex) 
         (:macro-character nil #\') 
         (:macro-character nil #\() 
         (:macro-character nil #\)) 
         (:macro-character nil #\,) 
         (:macro-character nil #\;) 
         (:macro-character nil #\`)          


         )

;;;; THE END ;;;;
