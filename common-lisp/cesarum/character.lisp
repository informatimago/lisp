;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               character.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    See package docstring.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-07-27 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII")
  (:export "STANDARD-CHARACTER-IS-ASCII-CODED-P"
           "STANDARD-CHARACTERS"
           "HAS-ASCII-CODES-P"
           "HAS-CHARACTER-NAMED-P"
           "PUSH-FEATURE-FOR-CHARACTER-NAMED")
  (:documentation "
Define features (both at compilation time, load time and execute) for
present semi-standard character names and other ASCII features.

   #+has-rubout    can read #\rubout
   #+has-page      can read #\page
   #+has-tab       can read #\tab
   #+has-backspace can read #\backspace
   #+has-return    can read #\return
   #+has-linefeed  can read #\linefeed

   #+has-escape    can read #\escape
   #+has-bell      can read #\bell
   #+has-vt        can read #\vt
   #+has-null      can read #\null

   #+has-ascii-code  The characters in the STANDARD-CHARACTER
                     set are encoded with the ASCII code by
                     char-code, and the codes between 0 and 31
                     inclusive plus 127 have a bijection with
                     other characters, thru code-char and
                     char-code.

   #+newline-is-return   <=> (char= #\newline #\return)
   #+newline-is-linefeed <=> (char= #\newline #\linefeed)


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2013 - 2013
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package  "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER")



#+mocl
(setf *features* (append '(:newline-is-linefeed :has-ascii-code
                           :has-vt :has-bell :has-escape :has-linefeed
                           :has-return :has-backspace :has-tab
                           :has-page :has-rubout) *features*))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defparameter *standard-characters*
    #.(concatenate 'string
                   " !\"#$%&'()*+,-./0123456789:;<=>?"
                   "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                   "`abcdefghijklmnopqrstuvwxyz{|}~")
    "A string containing all the STANDARD-CHARACTER.
Notice: it's the same character set as
COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII:*ASCII-CHARACTERS*.")


  (defun standard-characters ()
    "Return a string containing all the standard-characters."
    (copy-seq *standard-characters*))


  (defun has-character-named-p (name)
    "
NAME:       A case-insensitive string designator for the semi-standard
            character names:

                Rubout      The rubout or delete character.
                Page        The form-feed or page-separator character.
                Tab         The tabulate character.
                Backspace   The backspace character.
                Return      The carriage return character.
                Linefeed    The line-feed character.

Return:     Whether reading #\{name} will not produce an error.
"
    (ignore-errors (read-from-string (format nil "#\\~A" name))))


  (defun push-feature-for-character-named (name)
    "
NAME:       A case-insensitive string designator for the semi-standard
            character names:

                Rubout      The rubout or delete character.
                Page        The form-feed or page-separator character.
                Tab         The tabulate character.
                Backspace   The backspace character.
                Return      The carriage return character.
                Linefeed    The line-feed character.

DO:         If the implementation has the semi standard character
            named NAME, then push a feature :HAS-{NAME}, with NAME
            upcased.

"
    (when (has-character-named-p name)
      (pushnew (intern (format nil "~:@(HAS-~A~)" name)
                       (load-time-value (find-package"KEYWORD")))
               *features*)))


  #-mocl
  (dolist (name '("Rubout" "Page" "Tab" "Backspace" "Return" "Linefeed"
                  ;; Non standard character names:
                  "Escape" "Bell" "Vt" "Null"))
    (push-feature-for-character-named name))

  );;eval-when




;; Must be a separate form:
(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun standard-character-is-ascii-coded-p ()
    "Whether the char-code of the standard-characters are their ASCII codes."
    (load-time-value
     (ignore-errors
       (every (lambda (ch) (= (char-code ch) (ascii-code ch)))
              *standard-characters*))))

  (defun has-ascii-code-p ()
    "Whether it looks like ASCII is implemented by char-code and code-char."
    (let ((codes (cons 127 (loop :for code :from 0 :to 31 :collect code))))
      (and (standard-character-is-ascii-coded-p)
           (ignore-errors
             (every (lambda (code) (= (char-code (code-char code)) code))
                    codes))
           (= 33 (length (delete-duplicates (mapcar (function code-char) codes)
                                            :test (function char=))))
           #+has-bell      (=   7 (char-code #\bell))
           #+has-backspace (=   8 (char-code #\backspace))
           #+has-tab       (=   9 (char-code #\tab))
           #+has-linefeed  (=  10 (char-code #\linefeed))
           #+has-vt        (=  11 (char-code #\vt))
           #+has-page      (=  12 (char-code #\page))
           #+has-return    (=  13 (char-code #\return))
           #+has-escape    (=  27 (char-code #\escape))
           #+has-rubout    (= 127 (char-code #\rubout)))))
  
  #-mocl
  (progn
    (when (has-ascii-code-p)
      (pushnew :has-ascii-code *features*))
    
    #+has-return   (when (char= #\newline #\return)
                     (pushnew :newline-is-return *features*))
    
    #+has-linefeed (when (char= #\newline #\linefeed)
                     (pushnew :newline-is-linefeed *features*)))

  );;eval-when


;;;; THEN END ;;;;
