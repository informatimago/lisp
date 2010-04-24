;;;;**************************************************************************
;;;;FILE:               resource-utilization.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Display resource utilisation summary.
;;;;    
;;;;    This package exports a macro that gather resource utilization statistics
;;;;    and report them.
;;;; 
;;;;    Usage:
;;;; 
;;;;     (reporting-sru (:job-origin (remote-client) :stream (remote-stream))
;;;;        (do-something-lengthy))
;;;; 
;;;;     (reporting-sru (:job-origin (remote-client) :stream (remote-stream)
;;;;                     :report-to (lambda (cpu-time sys-time device-i/o paging-i/o
;;;;                                    job-origin &key (stream t))
;;;;                                 (SUMMARY-RESOURCE-UTILIZATION
;;;;                                    cpu-time sys-time device-i/o paging-i/o
;;;;                                    job-origin :stream stream)))
;;;;        (do-something-lengthy))
;;;;
;;;;    Example:
;;;;
;;;;       (reporting-sru (:job-origin "REPL")
;;;;          (asdf-load :com.informatimago.clext))
;;;;
;;;;       prints:
;;;;
;;;;     Summary of resource utilization
;;;;     -------------------------------
;;;;      CPU time:       0.300 sec                Device I/O:      175
;;;;      Overhead CPU:   0.012 sec                Paging I/O:        1
;;;;      CPU model:   AMD Athlon(tm) Processor 6.4.2 1200.303 MHz (2402.66 bogomips)
;;;;      Job origin:  REPL
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-11-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************


(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.CLMISC.RESOURCE-UTILIZATION"
  (:USE "COMMON-LISP")
  (:EXPORT "REPORTING-SRU"
           "SUMMARY-RESOURCE-UTILIZATION" )
  (:DOCUMENTATION
   "This package exports a macro that gather resource utilization statistics
    and report them.

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


    Copyright Pascal J. Bourguignon 2006 - 2006
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.CLMISC.RESOURCE-UTILIZATION")



(defun cpu-info ()
  (with-open-file (info "/proc/cpuinfo" :if-does-not-exist nil)
    (and info
         (loop
            :for line = (read-line info nil nil)
            :for colon = (and line (position #\: line))
            :for var = (and colon (string-trim " 	" (subseq line 0 colon)))
            :for val = (and colon (string-trim " 	" (subseq line (1+ colon))))
            :while line
            :when var
            :collect (cons (intern
                            (string-upcase
                             (substitute-if #\- (lambda (ch) (position ch "_ ")) var))
                            "KEYWORD") val)))))


(defun cpu-short-description ()
  (let ((info (cpu-info)))
    (flet ((gac (x) (cdr (assoc x info))))
      (format nil "~A ~A.~A.~A ~A MHz (~A bogomips)" (gac :model-name)
              (gac :cpu-family) (gac :model) (gac :stepping)
              (gac :cpu-mhz) (gac :bogomips)))))



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
                           (error 'END-OF-FILE :stream stream)
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
                                                  (loop-finish)
                                                  (vector-push-extend ch buffer)))
                ((char= #\\ ch)  (setf escape t))
                (t               (vector-push-extend ch buffer)))
          :finally (if ch
                       (return buffer)
                       (if eof-error-p
                           (error 'END-OF-FILE :stream stream)
                           (return eof-value))))))))


(defun test-read-parenthesized-string ()
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
             (incf success)
             (format t "~2%Reading parenthesized string ~S~
                         ~%     --> ~S~%expected ~S~%"
                     input result output))
     :finally  (format t "~&~30A ~4D cases, ~4D successful  (~6,1F %)~%"
                       'read-parenthesized-string
                       tcount success (/ success tcount 0.01))))


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
        (with-open-stream (config (ext:run-program "gzip" :arguments '("-d")
                                                   :input  "/proc/config.gz"
                                                   :output :stream))
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
    job-origin))


(defmacro reporting-sru ((&key (job-origin '(short-site-name)) (stream t)
                               (report-to nil report-to-p))
                         &body body)
  (let ((vstart-run   'sr)
        (vend-run     'er)
        (vstat-before 'sb)
        (vstat-after  'sa)
        (vdeio-before 'db)
        (vdeio-after  'da))
    `(let ((,vstat-before (process-status))
           (,vstat-after)
           (,vstart-run  (GET-INTERNAL-RUN-TIME))
           (,vend-run)
           (,vdeio-before (device-i/o))
           (,vdeio-after))
       (unwind-protect (progn ,@body)
         (setf ,vend-run  (GET-INTERNAL-RUN-TIME)
               ,vstat-after (process-status)
               ,vdeio-after (device-i/o))
         (flet ((before (x) (cdr (assoc x ,vstat-before)))
                (after  (x) (cdr (assoc x ,vstat-after))))
           (let* ((page-io (+ (- (after :majflt) (before :majflt))
                              #|(- (after :minflt) (before :minflt))|#))
                  (devi-io (max 0 (- ,vdeio-after ,vdeio-before page-io))))
             (,@(if report-to-p
                    (list 'funcall report-to)
                    '(summary-resource-utilization))
                (/ (- ,vend-run ,vstart-run) INTERNAL-TIME-UNITS-PER-SECOND)
                (* *jiffy* (- (after :stime) (before :stime)))
                devi-io page-io ,job-origin :stream ,stream)))))))



#||

(test-read-parenthesized-string)
(reporting-sru ()
  (with-open-file (input "/usr/share/dict/words")
    (loop :for line = (read-line input nil nil) :while line))
  (loop :repeat 5000 :collect (make-string 1000) :finally (terpri) (return  nil)))

||#



;; Local Variables:
;; eval: (cl-indent 'reporting-sru 1)
;; End:
