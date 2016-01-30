;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               manifest.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Check the licenses of the dependencies, write a manifest.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-14 <PJB> Extracted from generate-cli.lisp
;;;;    2014-02-21 <PJB> Exported print-manifest.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "ASDF"))
(defpackage "COM.INFORMATIMAGO.TOOLS.MANIFEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.VERSION"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "SPLIT-SEQUENCE")
  (:export "ASDF-SYSTEM-NAME"
           "ASDF-SYSTEM-LICENSE"
           "SYSTEM-DEPENDS-ON"
           "SYSTEM-DEPENDS-ON/RECURSIVE"
           "LISP-IMPLEMENTATION-TYPE-KEYWORD"
           "VERSION"
           "MACHINE-TYPE-KEYWORD"
           "DISTRIBUTION" 
           "EXECUTABLE-NAME"
           "EXECUTABLE-FILENAME"
           "PRINT-MANIFEST"
           "WRITE-MANIFEST"
           "PRINT-BUG-REPORT-INFO"))
(in-package "COM.INFORMATIMAGO.TOOLS.MANIFEST")


(defparameter *system-licenses*
  '(("cl-ppcre"       . "BSD-2")
    ("split-sequence" . :unknown)
    ("terminfo"       . "MIT")
    ("closer-mop"     . "MIT")))

(defun asdf-system-name (system)
  (slot-value system 'asdf::name))

(defun asdf-system-license (system-name)
  (let ((system  (asdf:find-system system-name)))
    (or (cdr (assoc system-name *system-licenses* :test 'string-equal))
        (and (slot-boundp system 'asdf::licence)
             (slot-value system 'asdf::licence))
        :unknown)))

#+mocl
(defun system-depends-on (system)
  '())

#-mocl
(defun system-depends-on (system)
  (delete (string-downcase system)
          (let ((system (asdf:find-system system)))
           (delete-duplicates
            (mapcar (lambda (system)
                      (etypecase system
                        (asdf:system (asdf:component-name system))
                        (string  system)
                        (symbol  (string-downcase system))))
                    (mapcan (lambda (x)
                              (delete-if-not (lambda (component)
                                               (typep component 'asdf:system))
                                             (copy-seq (rest x))))
                            (asdf:component-depends-on 'asdf:load-op system)))
            :test 'string=))
          :test 'string=))

(defun system-depends-on/recursive (system)
  (delete-duplicates
   (transitive-closure 
    (function system-depends-on)
    (list (string-downcase system)))
   :test 'string=))



;; kuiper                Linux kuiper 2.6.38-gentoo-r6-pjb-c9 #2 SMP Wed Jul 13 00:23:08 CEST 2011 x86_64 Intel(R) Core(TM) i7 CPU 950 @ 3.07GHz GenuineIntel GNU/Linux
;; hubble                Linux hubble 2.6.34-gentoo-r1-d3 #5 SMP PREEMPT Mon Sep 6 13:17:41 CEST 2010 i686 QEMU Virtual CPU version 0.13.0 GenuineIntel GNU/Linux
;; voyager               Linux voyager.informatimago.com 2.6.18-6-k7 #1 SMP Mon Oct 13 16:52:47 UTC 2008 i686 GNU/Linux
;; triton   10.5.8       Darwin triton.lan.informatimago.com 9.8.0 Darwin Kernel Version 9.8.0: Wed Jul 15 16:57:01 PDT 2009; root:xnu-1228.15.4~1/RELEASE_PPC Power Macintosh powerpc PowerBook6,8 Darwin
;; neuron   10.5.8       Darwin neuron.intergruas.com 9.8.0 Darwin Kernel Version 9.8.0: Wed Jul 15 16:55:01 PDT 2009; root:xnu-1228.15.4~1/RELEASE_I386 i386
;; galatea               Darwin galatea.lan.informatimago.com 11.3.0 Darwin Kernel Version 11.3.0: Thu Jan 12 18:48:32 PST 2012; root:xnu-1699.24.23~1/RELEASE_I386 i386
;; galatea  10.7.5       Darwin galatea.lan.informatimago.com 11.4.2 Darwin Kernel Version 11.4.2: Thu Aug 23 16:26:45 PDT 2012; root:xnu-1699.32.7~1/RELEASE_I386 i386
;; despina  10.8         Darwin despina.home 12.5.0 Darwin Kernel Version 12.5.0: Sun Sep 29 13:33:47 PDT 2013; root:xnu-2050.48.12~1/RELEASE_X86_64 x86_64
;; larissa  10.9.2       Darwin larissa.home 13.1.0 Darwin Kernel Version 13.1.0: Thu Jan 16 19:40:37 PST 2014; root:xnu-2422.90.20~2/RELEASE_X86_64 x86_64 i386 MacBookAir6,2 Darwin



(defun shell-command-to-string (command &rest arguments)
  "Execute the COMMAND with asdf:run-shell-command and returns its
stdout in a string (going thru a file)."
  (let ((*default-pathname-defaults* #P"")
        (path (format nil "~:@(out-~36,8,'0R.txt~)" (random (expt 2 32)))))
    (unwind-protect
         (when (zerop (asdf:run-shell-command (format nil "~? > ~S" command arguments path)))
           (with-output-to-string (out)
             (with-open-file (file path)
               (loop
                 :for line = (read-line file nil nil)
                 :while line :do (write-line line out)))))
      (ignore-errors (delete-file path)))))



(defun prepare-options (options)
  (mapcar (lambda (option)
            (typecase option
              (keyword (format nil "-~(~A~)" option))
              (symbol  (string-downcase option))
              (string  option)
              (t       (prin1-to-string option))))
          options))

(declaim (inline trim))
(defun trim  (string)
  (and string (string-trim #(#\space #\tab #\newline) string)))

(defun uname (&rest options)
  "Without OPTIONS, return a keyword naming the system (:LINUX, :DARWIN, etc).
With options, returns the first line output by uname(1)."
  (flet ((first-line (text) (subseq text 0 (position #\newline text))))
    (let ((uname  (shell-command-to-string "uname ~{~A~^ ~}" (prepare-options options))))
      (if (and uname (plusp (length (trim uname))))
          (values (if options
                      (first-line uname)
                      (intern (string-upcase (first-line uname))
                              "KEYWORD")))
          :unknown))))


(defun distribution ()
  "Return a list identifying the system, distribution and release.
RETURN: (system distrib release)
System and distrib are keywords, release is a string."
  (flet ((words (string) (split-sequence-if (lambda (ch) (find ch #(#\space #\tab)))
                                            string :remove-empty-subseqs t)))
    (let ((system (or #+windows                            :windows
                      ;; #+(and ccl windows-target)
                      ;; '(:cygwin :unknown "1.7.11,0.260,5,3")
                      #+linux                              :linux
                      #+darwin                             :darwin
                      #+(and unix (not (or linux darwin))) (uname)
                                                           :unknown))
          (distrib :unknown)
          (release :unknown))
      (case system
        #+(or unix linux)
        (:linux
         (cond
           ;; Checked with Linux Mandrake 6.1
           ((with-open-file (inp "/etc/mandrake-release"
                                 :if-does-not-exist nil)
              (when inp
                (setf release (fourth (words (read-line inp)))
                      distrib :mandrake))))

           ;; Checked with Linux RedHat 6.1, 6.2, 7.0
           ;; Checked with Linux Immunix 6.2.
           ;; There seems to be no way to differenciate 
           ;; a RedHat 6.2 from an Immunix 6.2.
           ((with-open-file (inp "/etc/redhat-release"
                                 :if-does-not-exist nil)
              (when inp
                (setf release (fourth (words (read-line inp)))
                      distrib :redhat))))

           ;; Checked with Linux Conectiva 6.5
           ((with-open-file (inp "/etc/conectiva-release"
                                 :if-does-not-exist nil)
              (when inp
                (setf release (third (words (read-line inp)))
                      distrib :conectiva))))

           ;; Checked with Linux SuSE 7.0, 7.1
           ((with-open-file (inp  "/etc/SuSE-release"
                                  :if-does-not-exist nil)
              (when inp
                (setf release (loop
                                :for line = (read-line inp nil nil)
                                :while line
                                :when (search "VERSION" line)
                                  :return (subseq line (let ((p (position #\= line)))
                                                         (if p (1+ p) 0)))
                                :finally (return :unknown))
                      distrib :suse))))

           ;; Checked with Linux DebIan 5.0.4
           ((with-open-file (inp "/etc/debian_version"
                                 :if-does-not-exist nil)
              (when inp
                (setf release (read-line inp)
                      distrib :debian))))

           ;; Checked with Linux gentoo 1.12.9, 1.12.13, 2.0.3
           ((with-open-file (inp "/etc/gentoo-release"
                                 :if-does-not-exist nil)
              (when inp
                (setf release (first (last (words (read-line inp))))
                      distrib :gentoo))))))
        #+(or unix)
        (:nextstep
         (setf distrib :next)
         (setf release (trim (shell-command-to-string "uname -r")))
         (setf release (or release :unknown)))
        #+(or unix darwin)
        (:darwin
         (when (probe-file "/System/Library/Frameworks/AppKit.framework/AppKit")
           (setf distrib :apple))
         (let ((hostinfo (shell-command-to-string "hostinfo")))
           (when hostinfo
             (setf release (with-input-from-string (inp hostinfo)
                             (loop
                               :for line = (read-line inp nil nil)
                               :while line
                               :when (search "Darwin Kernel Version" line)
                                 :return (let ((release (fourth (words line))))
                                           (subseq release 0 (position #\: release)))
                               :finally (return :unknown))))
             (setf release :unknown))))
        #-(or linux darwin windowd)
        (:unknown
         (let ((host (trim (shell-command-to-string "hostinfo"))))
           (cond
             ((prefixp "Mach" host)
              (let ((words (words host)))
                (setf distrib (fourth words)
                      release (sixth words))))))))
      (list system distrib release))))






(defun lisp-implementation-type-keyword ()
  "Return the keyword specific to each implementation (as found in *features*),
or else interns the (lisp-implementation-type), with space substituted by dashes
into the keyword package."
  (or (cdr (assoc (lisp-implementation-type)
                  '(("Armed Bear Common Lisp"                        . :abcl)
                    ("International Allegro CL Free Express Edition" . :allegro-cl-express)
                    ("Clozure Common Lisp"                           . :ccl)
                    ("CLISP"                                         . :clisp)
                    ("CMU Common Lisp"                               . :cmu)
                    ("ECL"                                           . :ecl)
                    ("SBCL"                                          . :sbcl))
                  :test (function string-equal)))
      (intern (substitute #\- #\space (lisp-implementation-type)) "KEYWORD")))


(defun machine-type-keyword ()
  "Return the keyword specific to machine type (as found in *features*),
or else interns the (machine-type), with space substituted by dashes
into the keyword package."
  (or (cdr (assoc (machine-type)
                 '(("Power Macintosh" . :ppc)
                   ("x86-64"          . :x86-64)
                   ("x86_64"          . :x86-64)
                   ("x64"             . :x86-64)
                   ("x86"             . :x86)
                   ("i686"            . :i686)
                   ("i386"            . :i386))
                 :test (function string-equal)))
      (intern (substitute #\- #\space (machine-type)) "KEYWORD")))



(defun lisp-version ()
  (let ((v (lisp-implementation-version)))
    (when (prefixp "Version " v)
      (setf v (subseq v (length "Version "))))
    (with-output-to-string (out)
      (with-input-from-string (inp v)
        (loop
          :with state = :normal
          :for ch = (read-char inp nil nil)
          :while ch
          :do (if (char= ch #\space)
                  (when (eq state :normal)
                    (setf state :space)
                    (write-char #\_ out))
                  (progn
                    (setf state :normal)
                    (case ch
                      ((#\( #\))) ; deleted characters
                      ((#\-) ; substituted characters
                       (write-char #\_ out))
                      (otherwise ; plain characters
                       (write-char ch out))))))))))

;; (list (lisp-implementation-version) (lisp-version))

(defun executable-name (base)
  (format nil "~(~A-~A-~A-~{~A-~A-~A~}-~A~)"
          base
          (or (lisp-implementation-type-keyword) "unknown")
          (lisp-version)
          (distribution)
          (or (machine-type-keyword) "unknown")))


(defun executable-filename (base)
  (format nil "~A~A" (executable-name base)
          #+(or windows win32) ".exe"
          #-(or windows win32) ""))


(defun date ()
  (multiple-value-bind (se mi ho da mo ye dow dls tz)
      (decode-universal-time (get-universal-time))
    (declare (ignore dow dls))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D ~:[+~;-~]~4,'0:D"
            ye mo da ho mi se (minusp tz) (abs (* 100 tz)))))


(defun print-manifest (&optional system)
  (let* ((entries '(date
                    lisp-implementation-type
                    lisp-implementation-version
                    machine-type
                    machine-version
                    machine-instance
                    distribution))
         (width (reduce 'max entries :key (lambda (x) (length (string x))))))
    (dolist (fun entries)
      (format t "~(~VA~) : ~A~%" width fun (funcall fun))))
  (terpri)
  (when system
    (let* ((entries      (sort (mapcar (lambda (system)
                                         (list system (asdf-system-license system)))
                                       (system-depends-on/recursive system))
                               'string< :key 'first))
           (system-width  (reduce 'max entries :key (lambda (x) (length (first x)))))
           (license-width (reduce 'max entries :key (lambda (x) (length (string (second x)))))))
      (format t "~:(~VA~)  ~:(~A~)~%~V,,,'-<~>  ~V,,,'-<~>~%"
              system-width 'system "license"
              system-width license-width)
      (loop
        :for (system license) :in entries
        :do (format t "~VA  ~A~%" system-width system license))
      (format t "~V,,,'-<~>  ~V,,,'-<~>~%" system-width license-width))))


(defun write-manifest (program-name system)
  "
DO:     write a {program-name}-{distribution}.manifest file for the given SYSTEM.
"
  (let ((base   (executable-name     program-name))
        (exec   (executable-filename program-name)))
    (with-open-file (*standard-output*
                     ;; Bug in ccl:
                     ;; "Version 1.9-r15757  (LinuxX8664)"
                     ;; "Version 1.9-r15759  (DarwinX8664)"
                     ;; doesn't use *default-pathname-defaults* :-(
                     (merge-pathnames (format nil "~A.manifest" base)
                                      *default-pathname-defaults*)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
      (format t "Manifest for ~A~%~V,,,'-<~>~2%" exec
              (+ (length "Manifest for ") (length exec)))
      (print-manifest system)
      (terpri)))
  (values))



;; TODO: see if we couldn't merge print-bug-report-info and print-manifest.

(defun print-bug-report-info ()
  "Prints information for a bug report."
  (let ((*print-pretty* t)
        (*print-right-margin* 80))
   (format t "~2%~{~28A ~S~%~}~2%"
           (list "LISP-IMPLEMENTATION-TYPE"    (lisp-implementation-type)
                 "LISP-IMPLEMENTATION-VERSION" (lisp-implementation-version)
                 "SOFTWARE-TYPE"               (software-type)
                 "SOFTWARE-VERSION"            (software-version)
                 "MACHINE-INSTANCE"            (machine-instance)
                 "MACHINE-TYPE"                (machine-type)
                 "MACHINE-VERSION"             (machine-version)
                 "distribution"                (distribution)
                 "uname -a"                    (ignore-errors (uname :a))
                 "*FEATURES*"                  *features*)))
  (let ((cpuinfo (text-file-contents "/proc/cpuinfo" :if-does-not-exist nil)))
    (when cpuinfo
      (format t "~2%/proc/cpuinfo~%-------------~2%")
      (write-string cpuinfo)
      (terpri)))
  #+clisp (with-open-stream (input (ext:run-program "uname" :arguments '("-a") :output :stream))
            (format t ";;; uname -a~%")
            (loop :for line = (read-line input nil nil) :while line :do (format t "~A~%" line)))
  #+clisp (format t ";;; (EXT:ARGV)~%~S~%" (ext:argv))
  #+clisp (ignore-errors
           (let ((path (make-pathname
                        :type nil
                        :defaults (merge-pathnames
                                   (make-pathname
                                    :directory '(:relative :up :up :up "bin")
                                    :name "clisp"  :type nil :version nil)
                                   (aref (ext:argv) 0) nil))))
             (with-open-stream (input (ext:run-program path
                                                       :arguments '("--version")
                                                       :output :stream))
               (format t ";;; ~A --version~%" path)
               (loop :for line = (read-line input nil nil)
                     :while line
                     :do (format t "~A~%" line)))))
  (values))


;;;; THE END ;;;;
