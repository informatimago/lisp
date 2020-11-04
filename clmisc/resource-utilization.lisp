;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               resource-utilization.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-11-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2006 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.CLMISC.RESOURCE-UTILIZATION"
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SYMBOL"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.CLEXT.SHELL")
  (:export "REPORTING-SRU"
           "SUMMARY-RESOURCE-UTILIZATION" )
  (:documentation
   "
Gather resource utilization statistics and report them.

Usage:

    (reporting-sru (:job-origin (remote-client) :stream (remote-stream))
       (do-something-lengthy))

    (reporting-sru (:job-origin (remote-client) :stream (remote-stream)
                    :report-to (lambda (cpu-time sys-time device-i/o paging-i/o
                                   job-origin &key (stream t))
                                (SUMMARY-RESOURCE-UTILIZATION
                                   cpu-time sys-time device-i/o paging-i/o
                                   job-origin :stream stream)))
       (do-something-lengthy))

Example:

    (reporting-sru (:job-origin \"REPL\")
       (asdf-load :com.informatimago.clext))

    prints:

    Summary of resource utilization
    -------------------------------
     CPU time:       0.300 sec                Device I/O:      175
     Overhead CPU:   0.012 sec                Paging I/O:        1
     CPU model:   AMD Athlon(tm) Processor 6.4.2 1200.303 MHz (2402.66 bogomips)
     Job origin:  REPL


License:

    AGPL3

    Copyright Pascal J. Bourguignon 2006 - 2012

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
(in-package "COM.INFORMATIMAGO.CLMISC.RESOURCE-UTILIZATION")

(defun split-attribute-line (line)
  (let* ((colon (position #\: line))
         (var   (and colon (string-trim " 	" (subseq line 0 colon))))
         (val   (and colon (string-trim " 	" (subseq line (1+ colon))))))
    (when (and var val)
      (cons (keywordize
             (string-upcase
              (substitute-if #\- (lambda (ch) (position ch "_ ")) var)))
            (multiple-value-bind (n p) (parse-integer val :junk-allowed t)
              (if (= p (length val))
                  n
                  val))))))

(defun sysctl-info ()
  "
RETURN: An A-list containing the data from sysctl -a.
"
  (let ((text (shell-command-to-string "sysctl -a")))
    (when text
      (delete nil
              (mapcar (function split-attribute-line)
                      (split-sequence #\newline text))))))

(defun cpu-info ()
  "
RETURN: An A-list containing the data from /proc/cpuinfo.
"
  (let ((text (text-file-contents "/proc/cpuinfo"
                                  :if-does-not-exist nil)))
    (when text
      (when text
        (delete nil
                (mapcar (function split-attribute-line)
                        (split-sequence #\newline text)))))))

(defun cpu-short-description ()
  "
RETURN: A short description of the CPU.
"
  (let ((info (append (cpu-info) (sysctl-info))))
    (flet ((gac (x) (cdr (assoc x info))))
      (format nil "~A ~A.~A.~A ~A MHz (~A bogomips)"
              (or (gac :model-name)
                  (gac :machdep.cpu.brand-string)
                  "")
              (or (gac :cpu-family)
                  (gac :machdep.cpu.family)
                  "")
              (or (gac :model)
                  (gac :machdep.cpu.model)
                  "")
              (or (gac :stepping)
                  (gac :machdep.cpu.stepping)
                  "")
              (or (gac :cpu-mhz)
                  (truncate (gac :hw.cpufrequency) 1e6)
                  "")
              (or (gac :bogomips)
                  (let ((freq (gac :hw.cpufrequency)))
                    (if freq
                        (* 2.5e-6 freq )
                        0)))))))


(defun read-parenthesized-string (&optional (stream t)
                                  (eof-error-p t) (eof-value nil)
                                  (recursive-p nil))
  "
DO:     Skip spaces, and read a string in parentheses (like in Postscript).
RETURN: The string read (without the external parentheses), or the EOF-VALUE
        if EOF occured and EOF-ERROR-P is NIL.
        NIL is returned if the next non whitespace character is not a left
        parenthesis.
NOTE:   Parentheses inside the string must be escaped by \ unless balanced.
"
  (let ((token (peek-char t stream  nil :eof recursive-p)))
    (cond
      ((eq :eof token) (if eof-error-p
                           (error 'end-of-file :stream stream)
                            eof-value))
      ((eql #\( token)
       (read-char stream)
       (loop
          :with buffer = (make-array 8 :adjustable t :fill-pointer 0
                                     :element-type 'character
                                     :initial-element #\space)
          :with level = 0
          :with escape = nil
          :for ch = (read-char stream nil nil recursive-p)
          :while ch
          :do (cond
                (escape          (vector-push-extend ch buffer) (setf escape nil))
                ((char= #\( ch)  (vector-push-extend ch buffer) (incf level))
                ((char= #\) ch)  (decf level) (if (minusp level)
                                                  #-mocl (loop-finish)
                                                  #+moc (if ch
                                                            (return buffer)
                                                            (if eof-error-p
                                                                (error 'end-of-file :stream stream)
                                                                (return eof-value)))
                                                  (vector-push-extend ch buffer)))
                ((char= #\\ ch)  (setf escape t))
                (t               (vector-push-extend ch buffer)))
          :finally (if ch
                       (return buffer)
                       (if eof-error-p
                           (error 'end-of-file :stream stream)
                           (return eof-value))))))))



(defun process-status (&optional (pid "self"))
  "
PID:  Normally it's a small integer, pid_t number.
      But for /proc/, we can also use ''self'', as in '/proc/self/stat'.
RETURN: The status of the specified process.
"
  (loop
     :for input :in '(("/proc/~A/stat"
                       :pid (:comm read-parenthesized-string)
                       :state :ppid :pgrp :session :tty-nr
                       :tpgid :flags :minflt :cminflt :majflt :cmajflt
                       :utime :stime :cutime :cstime :priority
                       :nice nil :it-real-value :start-time
                       :vsize :rss :rlim :start-code :end-code :start-stack
                       :ktskesp :kstkeip :signal :blocked :sigignore :sigcatch
                       :wchan :nswap :cnswap :exit-signal :processor)
                      ("/proc/~A/statm"
                       :size :resident :share :trs :drs :lrs :dt))
     :nconc (with-open-file (info (format nil (pop input) pid)
                                  :if-does-not-exist nil)
              (and info
                   (loop
                      :for field :in input
                      :for tag    = (if (atom field) field (first field))
                      :for reader = (if (atom field) 'read (second field))
                      :when tag :collect (cons tag (funcall reader info)))))))


(defun disk-statistics (&optional disk)
  "
RETURN: Statistics from the DISK usage, obtained from /proc/diskstats.
"
  (declare (ignore disk))
  ;; TODO: Implement disk filter.
  (with-open-file (info "/proc/diskstats"
                        :if-does-not-exist nil)
    (and info
         (let ((*readtable* (copy-readtable)))
           (setf (readtable-case *readtable*) :preserve)
           (loop
              :with part-keys = '(:device-major :device-minor :device-name
                                  :completed-reads  :merged-reads
                                  :read-sectors     :read-time
                                  :completed-writes :merged-writes
                                  :written-sectors  :write-time
                                  :current-i/os     :current-i/o-time
                                  :current-i/o-load)
              :with part-nfields = (length part-keys)
              :with disk-keys = '(:device-major :device-minor :device-name
                                  :completed-reads  :read-sectors
                                  :completed-writes :written-sectors)
              :with disk-nfields = (length disk-keys)
              :for line = (read-line info nil nil)
              :while line
              :collect (let* ((nfields 0)
                              (data (with-input-from-string (fields line)
                                      (loop
                                         :for item = (read fields nil nil)
                                         :while item
                                         :do (incf nfields)
                                         :collect (if (symbolp item)
                                                      (string item)
                                                      item)))))
                         (cond
                           ((= nfields part-nfields)
                            (pairlis part-keys data '((:type . :partition))))
                           ((= nfields disk-nfields)
                            (pairlis disk-keys data '((:type . :disk)))))))))))


(defun device-i/o ()
  "
RETURN: The number of disk I/O collected by (DISK-STATISTICS).
"
  (reduce (function +)
          (remove-if (lambda (entry) (eq  :partition (cdr (assoc :type entry))))
                     (disk-statistics))
          :key (lambda (entry)
                 (+ (or (cdr (assoc :written-sectors entry)) 0)
                    (or (cdr (assoc :read-sectors    entry)) 0)))
          :initial-value 0))


(defparameter *jiffy*
  ;; TODO: Use a CL implementation of gzip/zlib.
 #-(and clisp #.(cl:if (cl:find-package "LINUX") '(and) '(or))) 1/250
 #+(and clisp #.(cl:if (cl:find-package "LINUX") '(and) '(or)))
 (or (ignore-errors
      (with-open-stream (config
                         (cond
                           ((probe-file "/proc/config")
                            (open "/proc/config"))
                           ((probe-file "/proc/config.gz")
                            (ext:run-program "gzip" :arguments '("-d")
                                                    :input  "/proc/config.gz"
                                                    :output :stream))
                           (t (error "No such file."))))
        (and config
             (loop
               :with target = "CONFIG_HZ="
               :for line = (read-line config nil nil)
               :while (and line
                           (or (< (length line) (length target))
                               (not (string-equal line target
                                                  :end1 (length target)))))
               :finally (return (when line
                                  (/ (parse-integer line :start (length target)
                                                         :junk-allowed t))))))))
      1/250)
  "The JIFFY value of the Linux kernel (1/CONFIG_HZ)")



(defun summary-resource-utilization (cpu-time sys-time device-i/o paging-i/o
                                     job-origin &key (stream t))
  "
DO:         Reports resource utilisaty summary.
CPU-TIME:   CPU time used, in seconds.
SYS-TIME:   System time used, in seconds.
DEVICE-I/O: Number of Disk I/O.
PAGING-I/O: Number of Swap I/O.
JOB-ORIGIN: Label of the originator of the job.
STREAM:     Output stream (the default T means *standard-output*).
"
  (let ((*print-circle* nil))
   (format stream
           "Summary of resource utilization
-------------------------------
 CPU time:    ~8,3F sec                Device I/O: ~8D
 Overhead CPU:~8,3F sec                Paging I/O: ~8D
 CPU model:   ~A
 Job origin:  ~A
"
           cpu-time device-i/o
           sys-time paging-i/o
           (cpu-short-description)
           job-origin)))


(defmacro reporting-sru ((&key (job-origin '(short-site-name)) (stream t)
                               (report-to nil report-to-p))
                         &body body)
  "
DO:         Execute the BODY collecting resource usage statistics, and
            finally reporting them.
JOB-ORIGIN: Label of the originator of the job; defaults to (SHORT-SITE-NAME).
STREAM:     Output stream (the default T means *standard-output*).
REPORT-TO:  If provided, it's a function with the same signature as
            SUMMARY-RESOURCE-UTILIZATION, ie.:
            (cpu-time sys-time device-i/o paging-i/o job-origin &key (stream t))
            which is called to report the collected statistics.
            The default is SUMMARY-RESOURCE-UTILIZATION.
"
  (let ((vstart-run   'sr)
        (vend-run     'er)
        (vstat-before 'sb)
        (vstat-after  'sa)
        (vdeio-before 'db)
        (vdeio-after  'da))
    `(let ((,vstat-before (process-status))
           (,vstat-after)
           (,vstart-run  (get-internal-run-time))
           (,vend-run)
           (,vdeio-before (device-i/o))
           (,vdeio-after))
       (unwind-protect (progn ,@body)
         (setf ,vend-run  (get-internal-run-time)
               ,vstat-after (process-status)
               ,vdeio-after (device-i/o))
         (flet ((before (x) (or (cdr (assoc x ,vstat-before)) 0))
                (after  (x) (or (cdr (assoc x ,vstat-after))  0)))
           (let* ((page-io (+ (- (after :majflt) (before :majflt))
                              #|(- (after :minflt) (before :minflt))|#))
                  (devi-io (max 0 (- ,vdeio-after ,vdeio-before page-io))))
             (,@(if report-to-p
                    (list 'funcall report-to)
                    '(summary-resource-utilization))
                (/ (- ,vend-run ,vstart-run) internal-time-units-per-second)
                (* *jiffy* (- (after :stime) (before :stime)))
                devi-io page-io ,job-origin :stream ,stream)))))))


;;;; THE END ;;;;
