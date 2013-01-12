;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               index.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Generate the index.html for small-cl-pgms.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-01-12 <PJB> Switched to quicklisp to load dependencies.
;;;;    2004-03-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2004 - 2013
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************


(defparameter *sections*
  '(
    ("Lisp History"
     ;;===========
     ""

     ("The original LISP"
      ;;----------------
      (("aim-8/" "The LISP of the AI Memo 8"))
      "Implements the lisp of AIM-8 (4 MARCH 1959 by John McCarthy).")

     ("LISP 1.5 Sources"
      ;;----------------
      (("http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/67b1cabdf271870c?pli=1"
        "Original announce")
       ("http://www.softwarepreservation.org/projects/LISP/index.html#LISP_I_and_LISP_1.5_for_IBM_704,_709,_7090_"
        "Software preservation"))
      "The sources of LISP 1.5 in machine readable form.
       They are now available with:
       git clone http://git.informatimago.com/public/lisp15")

     ("A Parser for M-expressions"
      ;;-------------------------
      (("m-expression/" "A Parser for M-expressions"))
      "Implements a parser and a REPL for the M-expressions defined 
       in  AIM-8 (4 MARCH 1959 by John McCarthy).")
     
     ("Old LISP programs still run in Common Lisp"
      ;;-----------------------------------------
      (("wang.html"
        "Wang's Algorithm in LISP 1, runs on COMMON-LISP"))
      "The Wang's algorithm, implemented in LISP 1 on IBM 704
       in March 1960 still runs well on Common Lisp in 2006."))

    
    
    ("Lisp Cookbook"
     ;;============
     ""

     ("Image Based Development"
      ;;----------------------
      (("ibcl/"
        "A package that saves the definitions typed at the REPL"))
      "")
     
     ("Small Simple Structural Editor"
      ;;----------------------
      (("sedit/"
        "A Structural Editor"))
      "This is a simple structural editor to edit lisp
       sources considered as syntactic forests.")

     ("Recursive Descent Parser Generator"
      ;;---------------------------------
      (("rdp/"
        "A Quick and Dirty Recursive Descent Parser Generator"))
      "But not so ugly. Can generate the parser in lisp and in pseudo-basic."))



    ("Lisp Curiosities"
     ;;============
     ""

     ("Common-Lisp quines"
      ;;-----------------
      ("quine.lisp")
      "Three Common-Lisp quines (autogenerating programs).")

     ("Intersection between Common-Lisp, Emacs-Lisp and R5RS Scheme"
      ;;-----------------
      ("intersection-r5rs-common-lisp-emacs-lisp/")
      "A unique program that can be run on Common Lisp,
       Emacs Lisp or R5RS Scheme."))

    
    ("Lisp Tidbits"
     ;;============
     ""

     ("Author Signature" 
      ;;---------------
      ("author-signature.lisp")

      "This program computes an \"author signature\" from a text, with
       the algorithm from http://unix.dsu.edu/~~johnsone/comp.html")

     ("Demographic Simulator"
      ;;--------------------
      ("douze.lisp")
      "Assuming an Adam and an Eve 20 years old each, assuming the
       current US life table, and assuming an \"intensive\"
       reproduction rate, with gene selection, simulate the population
       growth during 80 years and draw the final age pyramid.")

     ("Common-Lisp quines"
      ;;-----------------
      ("quine.lisp")
      "Three Common-Lisp quines (autogenerating programs).")
     
     ("BASIC"
      ;;----
      (("basic/"  "A Quick, Dirty and Ugly Basic interpreter."))
      "")

     ("Brainfuck"
      ;;--------
      (("brainfuck/"
        "A brainfuck virtual machine, and brainfuck compiler."))
      ""))

    
    ("Little Games"
     ;;============
     ""

     ("Solitaire"
      ;;--------------------
      ("solitaire.lisp")
      "A solitaire playing program.  The user just watch the
       program play solitaire.")

     ("Conway's Life Game"
      ;;-----------------
      ("life.lisp")
      "A small life game program.")

     ("Cube Puzzle" 
      ;;----------
      ("cube.lisp")

      "This program tries to resolve the Cube Puzzle, where a cube
       composed of 27 smaller cubes linked with a thread  must be
       recomposed.")

     ("Sudoku Solver" 
      ;;----------
      (("sudoku-solver/" "This program solves sudoku boards."))
      "")

     ("Geek Day"
      ;;-------
      ("geek-day/geek-day.lisp" "geek-day/Makefile" "geek-day/README")
      "The famous Geek Day games by userfriendly.org's Illiad.
       See: http://ars.userfriendly.org/cartoons/?id=20021215"))))




(defmacro redirecting-stdout-to-stderr (&body body)
  (let ((verror  (gensym))
        (voutput (gensym)))
   `(let* ((,verror  nil)
           (,voutput (with-output-to-string (stream)
                       (let ((*standard-output* stream)
                             (*error-output*    stream)
                             (*trace-output*    stream))
                         (handler-case (progn ,@body)
                           (error (err) (setf ,verror err)))))))
      (when ,verror
        (terpri *error-output*)
        (princ ,voutput *error-output*)
        (terpri *error-output*)
        (princ ,verror *error-output*)
        (terpri *error-output*)
        (terpri *error-output*)
        #-testing-script (ext:exit 1)))))

(redirecting-stdout-to-stderr
 (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))

(redirecting-stdout-to-stderr
 (ql:quickload :com.informatimago.common-lisp))

(defun clean-text (text)
  (with-output-to-string (*standard-output*)
    (with-input-from-string (*standard-input* text)
      (loop
        :for last-was-space = nil :then (or ch-is-space (char= ch #\newline))
        :for ch = (read-char *standard-input* nil nil)
        :for ch-is-space = (and ch (char= ch #\space))
        :while ch
        :do (if ch-is-space
                (unless last-was-space
                  (write-char ch))
                (write-char ch))))))



(com.informatimago.common-lisp.cesarum.package:add-nickname
 "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML" "<")


(<:with-html-output (*standard-output* :kind :html :encoding :iso-8859-1)
  (<:doctype :strict
             (<:html ()
                     (<:pcdata (format nil "~%"))
                     (<:comment "PLEASE DO NOT EDIT THIS FILE!")
                     (<:comment "The source of this file is index.lisp")
                     (<:head ()
                             ;; (<:link (:rel "SHORTCUT ICON" :HREF "informatimago.ico" :TITLE "EXTERNAL:informatimago.ico"))
                             (<:title () (<:pcdata "Common-Lisp Small Programs and Tidbits"))
                             (<:meta (:http-equiv "Content-Type" :content "text/html; charset=iso-8859-1"))
                             (<:meta (:name "author" :content "Pascal J. Bourguignon"))
                             (<:meta (:http-equiv "Description" :name "description"  :content "Small Common-Lisp Programs and Tidbits"))
                             (<:meta (:name "keywords"  :content "software,logiciel,programas,GPL,LGPL,Lisp,Common-Lisp")))
                     (<:body ()
                             (<:pcdata (format nil "~%"))
                             (<:comment "TOP-BEGIN")
                             (<:comment "TOP-END")
                             (<:comment "MENU-BEGIN")
                             (<:comment "MENU-END")

                             (<:h1 () (<:pcdata "Common-Lisp Small Programs and Tidbits"))
                             
                             (<:h2 () (<:pcdata "Downloading the sources"))
                             (<:p ()
                                  (<:pcdata (clean-text "The sources of these small Common-Lisp
                                                         programs can be downloaded via "))
                                  (<:a (:href "http://git-scm.com/") (<:pcdata "git"))
                                  (<:pcdata ". Use the following command to fetch them:"))
                             (<:pre ()
                                    (<:pcdata "
mkdir com
git clone https://git.gitorious.org/com-informatimago/com-informatimago.git com/informatimago
ls com/informatimago/small-cl-pgms
"))

                             (dolist (section  *sections*)
                               (<:h2 () (<:pcdata (clean-text (first section))))
                               (<:p  () (<:pcdata (clean-text (second section))))
                               (dolist (soft (cddr section))
                                 (<:h3 () (<:pcdata (clean-text (first soft))))
                                 (<:p  () (<:pcdata (clean-text (third soft))))
                                 (<:ul ()
                                       (dolist (file (second soft))
                                         (<:li ()
                                               (if (stringp file)
                                                   (<:a (:href file)
                                                        (<:pcdata (clean-text file)))
                                                   (<:a (:href (first file))
                                                        (<:pcdata (clean-text (second file))))))))))
                             (<:pcdata (format nil "~%"))
                             (<:COMMENT "MENU-BEGIN")
                             (<:COMMENT "MENU-END")
                             (<:comment "BOTTOM-BEGIN")
                             (<:comment "BOTTOM-END")
                             ))))

;;;; THE END ;;;;
