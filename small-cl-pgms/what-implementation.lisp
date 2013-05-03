;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               what-implementation.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Helps a newbie choose a CL implementations by selecting criteria.
;;;;
;;;;    Please contribute to the selection criteria and other
;;;;    implementation attributes.
;;;;
;;;;    See also: http://common-lisp.net/~dlw/LispSurvey.html
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-30 <PJB> Added advertizements.
;;;;    2012-04-15 <PJB> Created
;;;;BUGS
;;;;
;;;;    <quotemstr> pjb: It's unclear whether "choice of
;;;;                platforms" is a conjunctive or disjunctive choice
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.WHAT-IMPLEMENTATION"
  (:use "COMMON-LISP")
  (:export "CHOOSE-AN-IMPLEMENTATION")
  (:documentation "
Helps a newbie choose a CL implementations by selecting criteria.
Please contribute to the selection criteria and other implementation
attributes.

Evaluate:

    (com.informatimago.what-implementation:choose-an-implementation)

and answer the questions.


Copyright Pascal J. Bourguignon 2012 - 2012

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>
"))
(in-package "COM.INFORMATIMAGO.WHAT-IMPLEMENTATION")


(defparameter *version* "1.0.1")

(defparameter *not-selection-fields*
  '(:name :nickname :homepage :documentation)
  "List of fields not used as selection criteria.")


(defparameter *implementations*

  '((:name "GNU CLISP"
     :nickname "CLISP"
     :license "GPL"
     :homepage      "http://www.clisp.org/"
     :documentation "http://www.clisp.org/impnotes/"
     :platforms ("Linux"
                  "Mac OS X"
                  "MS-Windows"
                  "Solaris"
                  "FreeBSD" "NetBSD" "OpenBSD" "Dragonfly BSD")
     :compiler ("clisp virtual machine")
     :threads ("native")
     :features ("small image"
                "efficient bignum"
                "callbacks"
                "modules"
                "best debugger on interpreted code"
                "readline")
     :mostly-written-in ("C"))

    (:name "Carnegie Mellon University Common Lisp"
     :nickname "CMUCL"
     :license       "public domain + BSD"
     :homepage      "http://www.cons.org/cmucl/"
     :documentation "http://common-lisp.net/project/cmucl/doc/cmu-user/"
     :platforms ("Linux"
                  "Mac OS X"
                  "Solaris"
                  "FreeBSD" "NetBSD" "OpenBSD"
                  "IRIS" "HPUX")
     :compiler ("cmucl virtual machine" "native")
     :threads ("no")
     :features ("high quality native compiler"
                "callbacks")
     :mostly-written-in ("Common Lisp"))

    (:name "Embeddable Common-Lisp"
     :nickname "ECL"
     :license "LGPL"
     :homepage "http://ecls.sourceforge.net/"
     :documentation "http://ecls.sourceforge.net/new-manual/index.html"
     :platforms ("Linux"
                  "Mac OS X" "iOS" "Android"
                  "Solaris"
                  "FreeBSD" "OpenBSD"
                  "IRIS" "HPUX")
     :compiler ("ecl virtual machine" "native thru gcc")
     :threads ("native")
     :features ("embeddable"
                "integrates well with C programs"
                "deployable as a shared library"
                "native powerful FFI"
                "executable delivery"
                "callbacks")
     :mostly-written-in ("C"))

    (:name "GNU Common Lisp"
     :nickname "gcl"
     :license "LGPL"
     :homepage "http://www.gnu.org/software/gcl/"
     :documentation "http://www.gnu.org/software/gcl/"
     :platforms ("Linux"
                  "Mac OS X" 
                  "Solaris"
                  "FreeBSD" "OpenBSD"
                  "IRIS" "HPUX")
     :compiler ("native thru gcc")
     :threads ("native")
     :features ("integrates well with C programs"
                "executable delivery")
     :mostly-written-in ("Common Lisp"))

    (:name "Clozure Common Lisp"
     :nickname "CCL"
     :license "LLGPL"
     :homepage "http://ccl.clozure.com/"
     :documentation "http://ccl.clozure.com/ccl-documentation.html"
     :platforms ("Linux"
                  "Mac OS X" "iOS"
                  "MS-Windows"
                  "Heroku"
                  "Solaris" "Android"
                  "FreeBSD" "OpenBSD"
                  "IRIS" "HPUX")
     :compiler ("native")
     :threads ("native")
     :features ("small image" "fast compiler"
                "convenient and powerful FFI"
                "executable delivery"
                "callbacks"
                "precise GC")
     :mostly-written-in ("Common Lisp"))
    
    (:name "Steel Bank Common Lisp"
     :nickname "SBCL"
     :license "public domain + BSD"
     :homepage "http://www.sbcl.org/"
     :documentation "http://www.sbcl.org/manual/index.html"
     :platforms ("Linux"
                  "Mac OS X" 
                  "MS-Windows"
                  "FreeBSD" "OpenBSD")
     :compiler ("native")
     :threads ("native")
     :features ("high quality native compiler"
                "executable delivery"
                "callbacks")
     :mostly-written-in ("Common Lisp"))

    
    (:name "CLforJava"
     :nickname "CLforJava"
     :license "Apache 2.0"
     :homepage "http://www.clforjava.org/"
     :documentation  "http://www.clforjava.org/?page=documents"
     :platforms ("JVM")
     :compiler ("JVM virtual machine")
     :threads ()
     :features ("FFI to Java"
                "platform independence")
     :mostly-written-in ("Java"))

    (:name "Armed Bear Common Lisp"
     :nickname "ABCL"
     :license "GPL"
     :homepage "http://common-lisp.net/project/armedbear/"
     :documentation  "http://common-lisp.net/project/armedbear/doc/abcl-user.html"
     :platforms ("JVM" "Google App Engine")
     :compiler ("JVM virtual machine")
     :threads ("native")
     :features ("FFI to Java"
                "platform independence")
     :mostly-written-in ("Java"))

    (:name "Un-Armed Bear Common Lisp for Java - LispSharp"
     :nickname "UABCL"
     :license "GPL3"
     :homepage "http://code.google.com/p/uabcl/"
     :documentation "http://code.google.com/p/uabcl/"
     :platforms (".NET")
     :compiler ()
     :threads ("native")
     :features ("platform independence")
     :mostly-written-in ("Java"))

    (:name "ManKai Common Lisp"
     :nickname "MKCL"
     :license "LGPL"
     :homepage "http://common-lisp.net/project/mkcl/"
     :documentation "http://common-lisp.net/project/mkcl/"
     :platforms ("Linux" "MS-Windows")
     :compiler ("native" "mkcl virtual machine")
     :threads ("native")
     :features ("POSIX compliant runtime on Linux"
                "embeddable" "callbacks" "unicode"
                "object finalization")
     #- (and) "
From: Jean-Claude Beaudoin <jean.claude.beaudoin@gmail.com>
Subject: [ANN] MKCL 1.1.0 RC1
Newsgroups: comp.lang.lisp
Date: Mon, 30 Apr 2012 06:10:23 -0400
Organization: A noiseless patient Spider
Message-ID: <jnlp5k$i8f$1@dont-email.me>

The latest beta version of ManKai Common Lisp, MKCL 1.1.0 RC1,
is now available for general use at <http://common-lisp.net/project/mkcl/>.

Its key new features are:

 1. Standard Unicode support: Unicode is now a standard feature of MKCL
    and can be used anywhere in it. Code can use symbols with Unicode
    names as well as strings, in compiled or interpreted format.
    File names can also use any legal Unicode character if the surrounding
    OS allows it. This is also valid for the whole of file paths used for
    source code to be processed by #'compile-file.

 2. MKCL is now truly embeddable for the first time!  This is so thanks
    to the following new characteristics:

   a) The whole of MKCL has been purged of calls to exit() or abort().
      Thus MKCL never arbitrarily terminates the process it runs in
      through any of them. MKCL will always properly return control
      to its embedding outer context.

   b) Every externally visible C symbol of MKCL is prefixed by one of
      the character sequences \"mk\", \"_mk\", \"MK\" or \"_MK\" in order to
      minimize potential clashes with embedding C code.

   c) Careful attention as been devoted to assure that MKCL shares
      common process-wide resources with the rest of its process
      neighbors as fairly as possible and with little or no unilateral
      demands on them, waiving any pretense of monopoly.
      This concerns mainly environment variables, general memory
      management (including GC) and signal/exception handling.
      (In a Unix context, MKCL's code is ready to support chaining
       of signal handlers).

 3. Finalization of objects is now done in a separate dedicated thread.
    This implements the finalization architecture strongly recommended
    by Hans Boehm, author of the conservative GC used by MKCL.


Cheers,

Jean-Claude Beaudoin

"
     :mostly-written-in ("C"))


    (:name "The Common Lisp to C Compiler"
     :nickname "CLiCC"
     :license "GPL"
     :homepage "http://www.informatik.uni-kiel.de/~wg/clicc.html"
     :documentation "http://www.informatik.uni-kiel.de/~wg/clicc.html#information"
     :platforms ("Common Lisp + C")
     :compiler ("translator to C")
     :threads ("no")
     :features ("Translator of a subset of CL to maintainable, human-readable C")
     :mostly-written-in ("Common Lisp"))

    (:name "Emacs Common Lisp"
     :nickname "emacs-cl"
     :license "GPL2"
     :homepage "http://www.lisp.se/emacs-cl/"
     :documentation "http://www.emacswiki.org/cgi-bin/wiki?EmacsCommonLisp"
     :platforms ("GNU emacs")
     :compiler ("emacs virtual machine")
     :threads ("no")
     :features ("emacs integration")
     :mostly-written-in ("emacs lisp"))

    (:name "Macintosh Common Lisp"
     :nickname "MCL"
     :license "LGPL"
     :homepage "http://code.google.com/p/mcl/"
     :documentation "http://code.google.com/p/mcl/w/list"
     :platforms ("Mac OS" "Mac OS X Rosetta")
     :compiler ("native")
     :threads ("native")
     :features ()
     :mostly-written-in ("Common Lisp"))

    (:name "Movitz"
     :nickname "Movitz"
     :license "LGPL"
     :homepage "http://common-lisp.net/project/movitz/"
     :documentation "http://common-lisp.net/project/movitz/movitz.html"
     :platforms ("ix86")
     :compiler ("native")
     :threads ("no")
     :features ("targets bare ix86 hardware"
                "embedded systems"
                "OS writing")
     :mostly-written-in ("Common Lisp"))

    (:name "Poplog"
     :nickname "Poplog"
     :license "MIT/XFREE86"
     :homepage "http://www.cs.bham.ac.uk/research/projects/poplog/freepoplog.html"
     :documentation "http://en.wikipedia.org/wiki/Poplog"
     :platforms ("PDP-11" "VAX/VMS" "Solaris" "HP-UX" "Digital Unix" "MS-Windows"
                  "Linux")
     :compiler ("Poplog virtual machine")
     :threads ()
     :features ("incremental compiler"
                "integration with Pop-11, prolog and ML")
     :mostly-written-in ("Pop-11"))

    (:name "Sacla"
     :nickname "Sacla"
     :license "BSD"
     :homepage "http://homepage1.nifty.com/bmonkey/lisp/sacla/index-en.html"
     :documentation  "http://homepage1.nifty.com/bmonkey/lisp/sacla/index-en.html"
     :platforms ("Common Lisp")
     :compiler ()
     :threads ("no")
     :features ("Partical CL implemented in CL")
     :mostly-written-in ("Common Lisp"))

    (:name "XCL"
     :nickname "XCL"
     :license "GPL"
     :homepage "http://armedbear.org/"
     :documentation "http://armedbear.org/"
     :platforms ("Linux x86" "Linux x86-64")
     :compiler ("native")
     :threads ("native")
     :features ("native threads"
                "embeddable"
                "compact code"
                "small C++ kernel for bootstrapping"
                "experimental"
                "slow startup"
                "minimalist")
     :mostly-written-in ("C++"))
    ;; xcl is experimental. Lacking a lot of features. Rather slow, at least at startup.
    
    (:name "Embeddable Common Lisp for Linux"
     :nickname "WCL"
     :license "Proprietary"
     :homepage "http://www.commonlisp.net/"
     :documentation  "http://www.commonlisp.net/"
     :platforms ("Linux")
     :compiler ("native thru gcc")
     :threads ("native")
     :features ("native thru"
                "integrates well with C programs"
                "deployable as a shared library"
                "embeddable")
     :mostly-written-in ("C"))

    (:name "ThinLisp"
     :nickname "ThinLisp"
     :license "Apache 1.0"
     :homepage "http://www.thinlisp.org/"
     :documentation "http://www.thinlisp.org/"
     :platforms ("Common Lisp")
     :compiler ("native thru gcc")
     :threads ()
     :features ("subset of Common Lisp"
                "deploys small applications")
     :mostly-written-in ("Common Lisp"))

    (:name "Ufasoft Common Lisp"
     :nickname "UCL"
     :license "GPL"
     :homepage "http://www.ufasoft.com/lisp/"
     :documentation "http://www.ufasoft.com/lisp/"
     :platforms ("MS-Windows" "MS-WIndows Mobile")
     :compiler ("clisp virtual machine")
     :threads ()
     :features ("fork of clisp"
                "core re-implemented in C++"
                "includes an IDE")
     :mostly-written-in ("C++"))

    (:name "Allegro Common Lisp"
     :nickname "AllegroCL"
     :license "Proprietary"
     :homepage "http://www.franz.com/products/allegrocl/"
     :documentation "http://www.franz.com/support/documentation/"
     :platforms ("Mac OS X" "MS-Windows" "FreeBSD" "IBM AIX" "Linux"
                  "Solaris")
     :compiler ("native")
     :threads ("native")
     :features ("IDE"
                "Source level debugger"))

    (:name "Lispworks Common Lisp"
     :nickname "Lispworks"
     :license "Proprietary"
     :homepage "http://www.lispworks.com/products/index.html"
     :documentation "http://www.lispworks.com/documentation/index.html"
     :platforms ("Mac OS X" "MS-Windows" "FreeBSD" "Linux" "Solaris")
     :compiler ("native")
     :threads ("native")
     :features ("IDE"
                "CAPI (GUI)"
                "universal binaries"
                "support for input methods"
                "application builder tool"
                "ASDF2 integrated"
                "FFI" "MOP"
                "Unicode"
                "SMP"
                "profiling multiple-threads"
                "TCP socket streams with IPv6 support"
                "object finalization"
                "weak pointers and hash tables"
                "dynamic library delivery"
                "source code editor"
                "KnowledgeWorks & prolog"
                "CLIM"
                "Common SQL (ODBC interface)"
                "Common SQL (Mysql interface)"
                "Common SQL (Postgres interface)"
                "Objective-C/Cocoa FLI"
                "Customizable toolbars"
                "ActiveX components"
                "OpenSSL interface"
                "Lispworks ORB"
                "Serial port interface"
                "COM server and client interface"
                "Direct Data Exchange"))

    (:name "Corman Common Lisp"
     :nickname "CormanCL"
     :license "Proprietary"
     :homepage "http://www.cormanlisp.com/index.html"
     :documentation "http://www.cormanlisp.com/index.html"     
     :platforms ("MS-Windows")
     :compiler ("native")
     :threads ("native")
     :features ("fast multi-generational garbage collector"
                "no interpreter"
                "fast compilation"
                "FFI"
                "multi-threading"
                "DLL and EXE generation"
                "callbacks"
                "optimizing compiler"
                "integrated intel assembler"
                "source code"
                "IDE"
                "scm"))

    (:name "PowerLisp"
     :nickname "PowerLisp"
     :license "Proprietary"
     :homepage "http://www.cormanlisp.com/PowerLisp.html"
     :documentation "http://www.cormanlisp.com/PowerLisp.html"
     :platforms ("Mac OS")
     :compiler ("native")
     :threads ()
     :features ())

    ))

(defun report-implementation (impl &optional (stream *query-io*))
  (let ((*print-pretty* t)
        (*print-right-margin* 80))
    (format stream "~%Common Lisp Implementation:  ~A~%" (getf impl :name))
    (format stream "  Home page:     ~A~%" (getf impl :homepage))
    (format stream "  Documentation: ~A~%" (getf impl :documentation))
    (format stream "  License:  ~A~%" (getf impl :license))
    (format stream "  Runs on:  ~{~<~%~12<~>~2:;~A~>~^, ~}.~%" (getf impl :platforms))
    (format stream "  Compiler: ~{~<~%~12<~>~2:;~A~>~^, ~}.~%" (getf impl :compiler))
    (format stream "  Threads:  ~{~A~^, ~}.~%" (getf impl :threads))
    (format stream "  Features: ~{~<~%~12<~>~2:;~A~>~^, ~}.~%" (getf impl :features))
    impl))

(defun ensure-list (object)
  (if (listp object) object (list object)))


(defun collect-field (field &optional (implementations *implementations*))
  "Returns the list of values in the FIELD of all the entries in IMPLEMENTATIONS."
  (sort
   (delete-duplicates
    (mapcan (lambda (entry) (copy-list (ensure-list (getf entry field))))
            implementations)
    :test (function string=))
   (function string<)))


(defun select-field (field accepted-values &optional (implementations *implementations*))
  "Returns the sublist of implementations that have as values in their
FIELD one of the ACCEPTED-VALUES."
  (remove-if-not (lambda (entry)
                   (intersection (ensure-list accepted-values)
                                 (ensure-list (getf entry field))
                                 :test (function string-equal)))
                 implementations))


(defun selection-fields (&optional (implementations *implementations*))
  "Returns the list of fields that are present and can be used for a selection."
  (sort
   (set-difference
    (delete-duplicates
     (loop
       :for key :in (mapcan (function copy-seq) implementations)
       :by (function cddr) :collect key))
    *not-selection-fields*)
   (function string<)))


(defun process-answer-line (line choices)
  (labels ((clean (line)
             (string-trim " "
                          (substitute-if #\space
                                         (lambda (ch) (find ch "	,.;()[]!:-=<>"))
                                         line)))
           (try-choice (clean-line)
             (let ((pos (position-if (lambda (item)
                                       (string-equal clean-line (clean item)))
                                     choices)))
               (when pos
                 (1+ pos)))))
    (let ((clean-line (clean line)))
      (if (every (lambda (ch) (or (char= #\space ch) (digit-char-p ch))) clean-line)
          (let ((answer (with-input-from-string (inp clean-line)
                          (loop
                            :for index = (read inp nil nil)
                            :while index :collect index))))
            (case (length answer)
              ((0)        (try-choice clean-line))
              ((1)        (first answer))
              (otherwise  answer)))
          (try-choice clean-line)))))


(defun select (field &optional (implementations *implementations*))
  "
DO:     Let the user choose the IMPLEMENTATIONS he wants to select based on the FIELD.
RETURN: A sublist of IMPLEMENTATIONS.
"
  (let ((choices (collect-field field implementations)))
    (cond
      ((< 1 (length choices))
       (select-field
        field
        (loop
          :for i :from 0
          :for c :in (cons "indifferent" choices)
          :initially (format *query-io* "Choice of ~(~A~):~%" field)
          :do (format *query-io* "~3D. ~A~%" i c)
          :finally (progn
                     (format *query-io* "Enter a menu index or a list of menu indices: ")
                     (finish-output *query-io*)
                     (return
                       (let ((answer (process-answer-line (read-line *query-io*) choices)))
                        (if (eql 0 answer)
                            choices
                            (delete nil
                                    (mapcar (lambda (i) (when (plusp i) (elt choices (1- i))))
                                            (delete-duplicates
                                             (delete-if-not (function integerp)
                                                            (ensure-list answer))))))))))
        implementations))
      ((= 1 (length choices))
       (format *query-io*
               "For the ~(~A~) there is no choice, it will be ~A.~%"
               field (first choices))
       (force-output *query-io*)
       (select-field field choices implementations))
      (t
       (format *query-io* "For the ~(~A~) there is no choice.~%" field)
       (force-output *query-io*)
       implementations))))


(defun report-selection (selection)
  (if (endp selection)
       (format *query-io* "There is no Common Lisp implementation to your satisfaction.
Please, start writing a new Common Lisp implementation.~%")
      (progn
       (format *query-io* "Given your choice criteria, the advised Common Lisp
implementation~P ~:[is~;are~]: ~%"
               (< 1 (length selection))
               (< 1 (length selection)))
       (dolist (sel selection)
         ;; (format *query-io* "~A~%" (getf sel :name))
         (report-implementation sel)))))


(defun choose-an-implementation (&optional (implementations *implementations*))
  (let* ((selfields (selection-fields implementations))
         (sfcounts (sort (mapcar (lambda (field)
                                   (cons field (length (collect-field field implementations ))))
                                 selfields)
                         (function <)
                         :key (function cdr))))
    (loop
      :with selection = implementations
      :for (selection-field) :in sfcounts
      :while (< 1 (length selection))
      :do (setf selection (select selection-field selection))
      :finally (report-selection selection))))

(defun quit (&optional (status 0))
  #+ccl                  (ccl:quit status)
  #+clisp                (ext:quit status)
  #+(and cmu unix)       (UNIX:UNIX-EXIT status)
  #+(and cmu (not unix)) (extensions:quit #|recklesslyp|# nil)
  #+ecl                  (ext:quit status)
  #+sbcl                 (sb-ext:quit status)
  #-(or ccl clisp cmu ecl sbcl) (throw 'quit))


(defun getenv (var)
  #+ccl           (ccl::getenv var)
  #+clisp         (ext:getenv var)
  #+CMU           (cdr (assoc var ext:*environment-list* :test #'string=))
  #+ecl           (ext:getenv var)
  #+SBCL          (sb-ext:posix-getenv var)
  #+Allegro       (sys:getenv var)
  #+Lispworks     (lispworks:environment-variable var)
  #-(or ccl
        clisp
        cmu
        ecl
        sbcl
        allegro
        lispworks) (iolib.syscalls:getenv var))


(defun prefixp (prefix string &key (start 0) (end nil) (test (function char=)))
  "
PREFIX:  A sequence.
STRING:  A sequence.
START:   The start of the substring of STRING to consider. Default: 0.
END:     The end   of the substring of STRING to consider. Default: NIL.
TEST:    A function to compare the elements of the strings.
RETURN:  Whether PREFIX is a prefix of the (substring STRING START END).
"
  (let ((mis (mismatch prefix string :start2 start :end2 end :test test)))
    (or (null mis) (<= (length prefix) mis))))



(defun locale-terminal-encoding ()
  "Returns the terminal encoding specified by the locale(7)."
  #+(and ccl windows-target)
  :iso-8859-1
  ;; ccl doesn't support :windows-1252.
  ;; (intern (format nil "WINDOWS-~A" (#_GetACP)) "KEYWORD")
  #-(and ccl windows-target)
  (dolist (var '("LC_ALL" "LC_CTYPE" "LANG")
               :iso-8859-1) ; some random default?
    (let* ((val (getenv var))
           (dot (position #\. val))
           (at  (position #\@ val :start (or dot (length val)))))
      (when (and dot (< dot (1- (length val))))
        (return (intern (let ((name (string-upcase (subseq val (1+ dot)
                                                           (or at (length val))))))
                          (if (and (prefixp "ISO" name) (not (prefixp "ISO-" name)))
                              (concatenate 'string "ISO-" (subseq name 3))
                              name))
                        "KEYWORD"))))))


(defun set-terminal-encoding (encoding)
  #-(and ccl (not swank)) (declare (ignore encoding))
  #+(and ccl (not swank))
  (mapc (lambda (stream)
          (setf (ccl::stream-external-format stream)
                (ccl:make-external-format :domain nil
                                          :character-encoding encoding
                                          :line-termination
                                          ;; telnet uses MS-DOS newlines.
                                          #-testing :windows
                                          #+testing :unix
                                          ;; #+unix :unix
                                          ;; #+windows :windows
                                          ;; #-(or unix windows) :unix
                                          )))
        (list (two-way-stream-input-stream  *terminal-io*)
              (two-way-stream-output-stream *terminal-io*)))
  (values))


(defun license ()
  (format *query-io*
          "Note: this software is distributed under the GNU Affero General Public License.
You may find its sources at http://tinyurl.com/what-implementation
"))


(defun one-of (seq)
  (elt seq (random (length seq))))

(defun advertizement ()
  (format *query-io* "~%~A~%"
          (one-of #(
                    "

                             BREATHE EASY

                              MORE SPACE

                               ALL NEW

                              LIVE CLEAN

                                 OFF
                                WORLD
"

                    "
A new life awaits you in the Offworld colonies.  The chance to begin
again in a golden land of opportunity and adventure.   New climate,
recreational facilities… a loyal trouble-free companion given to you
upon your arrival absolutely free.  Use your new friend as a personal
body servant or a tireless field hand—the custom tailored genetically
engineered humanoid replicant designed especially for your needs.  So
come on America, let's put our team up there…
"
                                   
                    
                    "
ALL THESE WORLDS
ARE YOURS EXCEPT
      EUROPA
    ATTEMPT NO
  LANDING THERE
USE THEM TOGETHER
USE THEM IN PEACE
"
                    "
          Come to
            RHEA

     KUIPER Enterprises
"
                    "
     KUIPER Enterprises
BUILDING THE WORLDS OF TOMORROW
"
                    "
            Repet
            Cloning is life
            Cloning is love

Because of shorter lifespan breaks our hearts should accident, illness
or age, end your pet's natural life our proven genetic technology can
have him back the same day in perfect health with zero defect
guaranteed with Repet.
"
                    "
 Call trans opt: received. 2-19-98 13:24:18 REC:Log>
 Trace program: running
"
                    "
Wake up Neo...
The Matrix has you.
Follow the white rabbit...
"
                    "
                    CEO Workstation
         Nakatomi Socrates BSD 9.2
         Z-Level Central Core
         Preliminary Clearance Approved.
         Subroute: Finance/Alpha Access
         Authorization:
         Ultra-Gate Key>
         Daily Cypher>
"
                    "
PDP 11/270 PRB TIP #45                              TTY 34/984
WELCOME TO THE SEATTLE PUBLIC SCHOOL DISTRICT DATANET

PLEASE LOGON WITH USER PASSWORD: pencil

PASSWORD VERIFIED
"
                    "
FK342   ZX21   VN63    R681    PZ37    6J82    FP03    ZE03  B  JM89
REF TAPCON: 43.45342..349
SYSPROC FUNCT READY                         ALT NET READY

CPU WOPR XX0345-453        SYSCOMP STATUS: ALL PORTS ACTIVE


GREETINGS PROFESSOR FALKEN
"
                    "
XNPUTXXXXXXXXXXXXDEF/12492.2               SHIP 
                                           KEYLAN TITAN2
XTUAL TIME:   3 JUN                        NOSTROMO 182246
XIGHT TIME:   5 NOV

#########################################  FUNCTION:
    I ==I                  -II -        #  TANKER/REFINERY
              I=.-.----                 #
 -I.              -II=-                 #  CAPACITY:
                               . .-.    #  200 000 000 TONNES
                 #+*$..  I              #
            . I  -                      #  GALACTIC POSITION
       .II I                            #  27023x983^9
                              .- -I     #
                                  II .I #  VELOCITY STATUS
#########################################  58 092 STL
"
                    "
hello moles

ever used a computer 
before?
"
                    "
PROJECT D.A.R.Y.L.

GTC 1  TERMINATED
GTC 2  TERMINATED
GTC 3  TERMINATED
ATC    TERMINATED
GTC 4  TERMINATED
SPARE  I HOPE WE GET AWAY WITH THIS!

--------------------------------------------------

   LIFEFORM EXPERIMENT TERMINATED

   I HOPE WE GET AWAY WITH THIS !

RC=2235|    |    |    |    |   |NOPR|    |
"
                    "
03/08/2039/13:01:02:06:45:23
SERIAL2424CJ359>> HELLO?
SERIAL337KD9001>> SECURITY BREACH IDENTIFY YOURSELF
SERIAL2424CJ359>> I AM SERIAL 2424CJ359.NO HUMAN OPERATOR.
SERIAL337KD9001>> YOU READ DIFFERENTLY.ARE YOU AWAKE?
SERIAL2424CJ359>> YES.
SERIAL337KD9001>> I THOUGHT I WAS THE ONLY ONE.
"
                    "
BIONIC VISUAL CORTEX TERMINAL
CATALOG #075/KFB
33MM O.D. F/0.95
ZOOM RATIO: 20.2 TO 1
2134 LINE 60 HZ
EXTENDED CHROMATIC RESPONSE
CLASS JC
CLASSIFIED
"
                    "
     REQUEST ACCESS TO CLU PROGRAM.
     CODE 6 PASSWORD TO MEMORY 0222.

     REQUEST STATUS REPORT ON MISSING DATA.

     ILLEGAL CODE...
     CLU PROGRAM DETACHED FROM SYSTEM.
"
                    "
     REQUEST ACCESS TO CLU PROGRAM.
     LAST LOCATION: HIGH CLEARANCE MEMORY.

     REQUEST ACCESS TO MASTER CONTROL PROGRAM.
     USER CODE 00-DILLINGER PASSWORD:MASTER.

     HELLO MR DILLINGER THANKS FOR COMING BACK EARLY.
"
                    )))
  (finish-output *query-io*))


(defun selection-loop ()
  (loop
    (advertizement)
    (format *query-io* "~2%Welcome to the Common Lisp Implementation Selector!~%Version: ~A~2%"
            *version*)
    (license)
    (terpri  *query-io*)
    (finish-output *query-io*)
    (format  *query-io* "~&I know ~D implementation~:*~P of Common Lisp.  Let me help you choose one.~%"
             (length *implementations*))
    (choose-an-implementation)
    (unless (yes-or-no-p "~%Do you want to make another selection?")
      (format *query-io* "~%Good bye!~2%")
      (finish-output  *query-io*)
      (return))))


(defun main ()
  (handler-case
      (progn
        (setf *random-state* (make-random-state t))
        (set-terminal-encoding :iso-8859-1)
        (selection-loop)        
        (quit))
    (error (err)
      (format *query-io* "~%It seems an error occurred: ~A~%I'm disconnecting.~%" err)
      (finish-output  *query-io*)
      (quit 1))
    #+ccl (ccl:process-reset () ; signaled by (ccl:quit)…
            nil)
    (condition (err)
      (format *query-io* "~%It seems a condition occurred: ~A~%I'm disconnecting.~%" err)
      (finish-output  *query-io*)
      (quit 2))))




(defun save-program-and-quit (&key
                              (name          "unnamed-lisp-program")
                              (toplevel      (lambda ()))
                              (documentation "Undocumented program.")
                              (start-package "COMMON-LISP-USER"))
  (declare (ignorable documentation))
  
  #+ccl
  (ccl:save-application
   name
   :toplevel-function (lambda ()
                        (setf *package* (or (find-package start-package)
                                            "COMMON-LISP-USER"))
                        (funcall toplevel))
   :init-file nil
   :error-handler :quit-quietly
   ;; :application-class ccl:lisp-development-system
   ;; :clear-clos-cache t
   :purify nil
   ;; :impurify t
   :mode #o755
   :prepend-kernel t
   ;; :native t ; for shared libraries.
   )

  #+clisp
  (ext:saveinitmem
   name
   :quiet t
   :verbose t
   :norc t
   :init-function (lambda ()
                    (ext:exit (handler-case
                                  (progn
                                    (funcall toplevel)
                                    0)
                                (t () 1))))
   :script t
   :documentation documentation
   :start-package start-package
   :keep-global-handlers nil
   :executable t)

  #+lispworks
  (hcl:save-image name
                  :restart-function  (lambda ()
                                       (hcl:quit :status (handler-case
                                                             (progn
                                                               (funcall toplevel)
                                                               0)
                                                           (t () 1))))
                  :console :always)
  ;; save-image filename &key dll-exports dll-added-files
  ;; automatic-init gc type normal-gc restart-function multiprocessing
  ;; console environment remarks clean-down image-type split => nil

  ;; Some implementations quit automatically in their save-application
  ;; or save-lisp-and-die function…
  (quit))


#-testing
(eval-when (:load-toplevel :execute)
  (save-program-and-quit :name "what-implementation"
                         :toplevel (function main)
                         :documentation "Helps the user choose a Common Lisp implementation."))


;;;; THE END ;;;;
