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
;;;;    2012-04-15 <PJB> Created
;;;;BUGS
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"))
(in-package "COM.INFORMATIMAGO.WHAT-IMPLEMENTATION")


(defparameter *not-selection-fields*
  '(:name :homepage :documentation)
  "List of fields not used as selection criteria.")


(defparameter *implementations*

  '((:name "CLISP"
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

    (:name "CMUCL"
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

    (:name "ECL"
     :license "LGPL"
     :homepage "http://ecls.sourceforge.net/"
     :documentation "http://ecls.sourceforge.net/new-manual/index.html"
     :platforms ("Linux"
                  "Mac OS X" "iOS"
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

    (:name "gcl"
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

    (:name "CCL"
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
    
    (:name "SBCL"
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

    (:name "ABCL"
     :license "GPL"
     :homepage "http://common-lisp.net/project/armedbear/"
     :documentation  "http://common-lisp.net/project/armedbear/doc/abcl-user.html"
     :platforms ("JVM" "Google App Engine")
     :compiler ("JVM virtual machine")
     :threads ("native")
     :features ("FFI to Java"
                "platform independence")
     :mostly-written-in ("Java"))

    (:name "UABCL"
     :license "GPL3"
     :homepage "http://code.google.com/p/uabcl/"
     :documentation "http://code.google.com/p/uabcl/"
     :platforms (".NET")
     :compiler ()
     :threads ("native")
     :features ("platform independence")
     :mostly-written-in ("Java"))

    (:name "MKCL"
     :license "LGPL"
     :homepage "http://common-lisp.net/project/mkcl/"
     :documentation "http://common-lisp.net/project/mkcl/"
     :platforms ("Linux" "MS-Windows")
     :compiler ("native" "mkcl virtual machine")
     :threads ("native")
     :features ("POSIX compliant runtime on Linux"
                "embeddable" "callbacks")
     :mostly-written-in ("C"))

    (:name "CLiCC"
     :license "GPL"
     :homepage "http://www.informatik.uni-kiel.de/~wg/clicc.html"
     :documentation "http://www.informatik.uni-kiel.de/~wg/clicc.html#information"
     :platforms ("Common Lisp + C")
     :compiler ("translator to C")
     :threads ("no")
     :features ("Translator of a subset of CL to maintainable, human-readable C")
     :mostly-written-in ("Common Lisp"))

    (:name "emacs-cl"
     :license "GPL2"
     :homepage "http://www.lisp.se/emacs-cl/"
     :documentation "http://www.emacswiki.org/cgi-bin/wiki?EmacsCommonLisp"
     :platforms ("GNU emacs")
     :compiler ("emacs virtual machine")
     :threads ("no")
     :features ("emacs integration")
     :mostly-written-in ("emacs lisp"))

    (:name "MCL"
     :license "LGPL"
     :homepage "http://code.google.com/p/mcl/"
     :documentation "http://code.google.com/p/mcl/w/list"
     :platforms ("Mac OS" "Mac OS X Rosetta")
     :compiler ("native")
     :threads ("native")
     :features ()
     :mostly-written-in ("Common Lisp"))

    (:name "Movitz"
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
     :license "BSD"
     :homepage "http://homepage1.nifty.com/bmonkey/lisp/sacla/index-en.html"
     :documentation  "http://homepage1.nifty.com/bmonkey/lisp/sacla/index-en.html"
     :platforms ("Common Lisp")
     :compiler ()
     :threads ("no")
     :features ("Partical CL implemented in CL")
     :mostly-written-in ("Common Lisp"))

    (:name "XCL"
     :license "GPL"
     :homepage "http://armedbear.org/"
     :documentation  "http://homepage1.nifty.com/bmonkey/lisp/sacla/index-en.html"
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
    
    (:name "WCL"
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
             (string-trim #\space
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


(defun main ()
  (handler-case
      (loop
        (set-terminal-encoding :iso-8859-1)
        (format *query-io* "~2%Welcome to the Common Lisp implementation selector!~2%")
        (license)
        (terpri  *query-io*)
        (finish-output *query-io*)
        (choose-an-implementation)
        (unless (yes-or-no-p "~%Do you want to make another selection?")
          (format *query-io* "~%Good bye!~2%")
          (finish-output  *query-io*)
          (quit)))
    (error (err)
      (format *query-io* "~%It seems an error occured: ~A~%I'm disconnecting.~%" err)
      (finish-output  *query-io*)
      (quit 1))
    (condition (err)
      (format *query-io* "~%It seems a condition occured: ~A~%I'm disconnecting.~%" err)
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
