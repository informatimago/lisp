;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               syslog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    FFI to syslog.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-04-19 <PJB> Made use of slave logger processes.
;;;;    2003-08-31 <PJB> Created.
;;;;BUGS
;;;;    Not implemented as FFI, we're using the external program logger(1)
;;;;    in the mean time.
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")

(defpackage "COM.INFORMATIMAGO.CLISP.SYSLOG"
  (:documentation "This module exports unix syslog functions.
Since FFI is not always available with clisp, we rather use logger(1).")
  (:use "COMMON-LISP")
  (:export
   "OPENLOG" "SYSLOG" "CLOSELOG"

   "+LOG-PID+" "+LOG-CONS+" "+LOG-ODELAY+" "+LOG-NDELAY+" 
   "+LOG-NOWAIT+" "+LOG-PERROR+"

   "+LOG-EMERG+" "+LOG-ALERT+" "+LOG-CRIT+" "+LOG-ERR+" 
   "+LOG-WARNING+" "+LOG-NOTICE+"
   "+LOG-INFO+" "+LOG-DEBUG+"

   "+LOG-KERN+" "+LOG-USER+" "+LOG-MAIL+" "+LOG-DAEMON+" 
   "+LOG-AUTH+" "+LOG-SYSLOG+"
   "+LOG-LPR+" "+LOG-NEWS+" "+LOG-UUCP+" "+LOG-CRON+" 
   "+LOG-AUTHPRIV+" "+LOG-FTP+"
   "+LOG-LOCAL0+" "+LOG-LOCAL1+" "+LOG-LOCAL2+" "+LOG-LOCAL3+" "+LOG-LOCAL4+"
   "+LOG-LOCAL5+" "+LOG-LOCAL6+" "+LOG-LOCAL7+"))
(in-package "COM.INFORMATIMAGO.CLISP.SYSLOG")


;; options
(defconstant +log-pid+     1 "log the pid with each message ")
(defconstant +log-cons+    2 "log on the console if errors in sending ")
(defconstant +log-odelay+  4 "delay open until first syslog() (default) ")
(defconstant +log-ndelay+  8 "don't delay open ")
(defconstant +log-nowait+ 16 "don't wait for console forks: DEPRECATED ")
(defconstant +log-perror+ 32 "log to stderr as well ")

;; priorities
(defconstant +log-emerg+   0 "system is unusable ")
(defconstant +log-alert+   1 "action must be taken immediately ")
(defconstant +log-crit+    2 "critical conditions ")
(defconstant +log-err+     3 "error conditions ")
(defconstant +log-warning+ 4 "warning conditions ")
(defconstant +log-notice+  5 "normal but significant condition ")
(defconstant +log-info+    6 "informational ")
(defconstant +log-debug+   7 "debug-level messages ")

;; facilities
(defconstant +log-kern+       0 "kernel messages ")
(defconstant +log-user+       8 "random user-level messages ")
(defconstant +log-mail+      16 "mail system ")
(defconstant +log-daemon+    24 "system daemons ")
(defconstant +log-auth+      32 "security/authorization messages ")
(defconstant +log-syslog+    40 "messages generated internally by syslogd ")
(defconstant +log-lpr+       48 "line printer subsystem ")
(defconstant +log-news+      56 "network news subsystem ")
(defconstant +log-uucp+      64 "UUCP subsystem ")
(defconstant +log-cron+      72 "clock daemon ")
(defconstant +log-authpriv+  80 "security/authorization messages (private) ")
(defconstant +log-ftp+       88 "ftp daemon ")
(defconstant +log-local0+   128 "reserved for local use ")
(defconstant +log-local1+   136 "reserved for local use ")
(defconstant +log-local2+   144 "reserved for local use ")
(defconstant +log-local3+   152 "reserved for local use ")
(defconstant +log-local4+   160 "reserved for local use ")
(defconstant +log-local5+   168 "reserved for local use ")
(defconstant +log-local6+   176 "reserved for local use ")
(defconstant +log-local7+   184 "reserved for local use ")





(defvar *ident*      "clisp")
(defvar *facility*   +log-local0+)
(defvar *log-pid*    nil "log the pid with each message ")
(defvar *log-cons*   nil "log on the console if errors in sending ")
(defvar *log-odelay* nil "delay open until first syslog() (default) ")
(defvar *log-ndelay* nil "don't delay open ")
(defvar *log-nowait* nil "don't wait for console forks: DEPRECATED ")
(defvar *log-perror* nil "log to stderr as well ")


(defvar *loggers* (make-array '(256) :initial-element nil)
  "Array of logger streams (opened with ext:run-program).")


(defun get-logger (facility priority)
  "
RETURN: A logger for the (facility priority) couple.
"
  (unless (and (integerp facility)
               (integerp priority)
               (<= 0 (+ facility priority) 255))
    (error "Invalid (facility=~S priority=~S) couple." facility priority))
  (let ((logger (aref *loggers* (+ facility priority))))
    (unless logger
      (setf logger (ext:run-program "logger"
                     :arguments
                     (append
                      (when *ident* (list "-t" *ident*))
                      (when *log-pid* (list "-i"))
                      (when *log-perror* (list "-s"))
                      (list "-p" (format nil "~D" (+ facility priority))))
                     :input :stream
                     :output nil))
      (setf (aref *loggers* (+ facility priority)) logger))
    logger))



;;; (DEF-CALL-OUT OPENLOG
;;;   (:NAME "openlog")
;;;   (:LANGUAGE :STDC)
;;;   (:ARGUMENTS (IDENT C-STRING) (OPTION INT) (FACILITY INT))
;;;   (:RETURN-TYPE NIL));;OPENLOG
;;; 
;;; 
;;; (DEF-CALL-OUT SYSLOG1
;;;   (:NAME "syslog")
;;;   (:LANGUAGE :STDC)
;;;   (:ARGUMENTS (PRIORITY INT) (FORMAT C-STRING) (VALUE C-STRING))
;;;   (:RETURN-TYPE NIL));;SYSLOG1
;;; 
;;; 
;;; (DEFMACRO SYSLOG (PRIORITY FCTRL &REST ARGUMENTS)
;;;   (SYSLOG1 PRIORITY "%s" (APPLY (FUNCTION FORMAT) NIL FCTRL ARGUMENTS))
;;;   );;SYSLOG
;;; 
;;; 
;;; (DEF-CALL-OUT CLOSELOG
;;;   (:NAME "closelog")
;;;   (:LANGUAGE :STDC)
;;;   (:ARGUMENTS)
;;;   (:RETURN-TYPE NIL));;CLOSELOG




(defun openlog (ident option facility)
  (setq *ident*      ident
        *facility*   facility
        *log-pid*    (/= 0 (logand option +log-pid+))
        *log-cons*   (/= 0 (logand option +log-cons+))
        *log-odelay* (/= 0 (logand option +log-odelay+))
        *log-ndelay* (/= 0 (logand option +log-ndelay+))
        *log-nowait* (/= 0 (logand option +log-nowait+))
        *log-perror* (/= 0 (logand option +log-perror+)))
  (values))


(defun old-syslog (priority fctrl &rest arguments)
  (ext:run-program "logger"
    :arguments  (append (when *log-pid*    (list "-i"))
                        (when *log-perror* (list "-s"))
                        (list "-p" (format nil "~D" (+ *facility* priority))
                              "-t" *ident*
                              "--" (apply (function format) nil fctrl arguments)))
    :input nil :output nil :wait nil)
  (values))


(defun newlinep (ch)
  (member ch '(#\newline #\return #\newline)))

(defun syslog (priority fctrl &rest arguments)
  (let ((logger (get-logger *facility* priority)))
    (let ((lines (apply (function format) nil fctrl arguments)))
      (princ lines logger)
      (unless (newlinep (aref lines (1- (length lines))))
        (terpri logger))
      (finish-output logger)))
  (values))


(defun closelog ()
  (setf *loggers*
        (map 'array (lambda (logger) (when logger (close logger)) nil) *loggers*))
  (values))


;;;; THE END ;;;;
