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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")

(defpackage "COM.INFORMATIMAGO.CLISP.SYSLOG"
  (:DOCUMENTATION "This module exports unix syslog functions.
Since FFI is not always available with clisp, we rather use logger(1).")
  (:use "COMMON-LISP")
  (:EXPORT
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
(DEFCONSTANT +LOG-PID+     1 "log the pid with each message ")
(DEFCONSTANT +LOG-CONS+    2 "log on the console if errors in sending ")
(DEFCONSTANT +LOG-ODELAY+  4 "delay open until first syslog() (default) ")
(DEFCONSTANT +LOG-NDELAY+  8 "don't delay open ")
(DEFCONSTANT +LOG-NOWAIT+ 16 "don't wait for console forks: DEPRECATED ")
(DEFCONSTANT +LOG-PERROR+ 32 "log to stderr as well ")

;; priorities
(DEFCONSTANT +LOG-EMERG+   0 "system is unusable ")
(DEFCONSTANT +LOG-ALERT+   1 "action must be taken immediately ")
(DEFCONSTANT +LOG-CRIT+    2 "critical conditions ")
(DEFCONSTANT +LOG-ERR+     3 "error conditions ")
(DEFCONSTANT +LOG-WARNING+ 4 "warning conditions ")
(DEFCONSTANT +LOG-NOTICE+  5 "normal but significant condition ")
(DEFCONSTANT +LOG-INFO+    6 "informational ")
(DEFCONSTANT +LOG-DEBUG+   7 "debug-level messages ")

;; facilities
(DEFCONSTANT +LOG-KERN+       0 "kernel messages ")
(DEFCONSTANT +LOG-USER+       8 "random user-level messages ")
(DEFCONSTANT +LOG-MAIL+      16 "mail system ")
(DEFCONSTANT +LOG-DAEMON+    24 "system daemons ")
(DEFCONSTANT +LOG-AUTH+      32 "security/authorization messages ")
(DEFCONSTANT +LOG-SYSLOG+    40 "messages generated internally by syslogd ")
(DEFCONSTANT +LOG-LPR+       48 "line printer subsystem ")
(DEFCONSTANT +LOG-NEWS+      56 "network news subsystem ")
(DEFCONSTANT +LOG-UUCP+      64 "UUCP subsystem ")
(DEFCONSTANT +LOG-CRON+      72 "clock daemon ")
(DEFCONSTANT +LOG-AUTHPRIV+  80 "security/authorization messages (private) ")
(DEFCONSTANT +LOG-FTP+       88 "ftp daemon ")
(DEFCONSTANT +LOG-LOCAL0+   128 "reserved for local use ")
(DEFCONSTANT +LOG-LOCAL1+   136 "reserved for local use ")
(DEFCONSTANT +LOG-LOCAL2+   144 "reserved for local use ")
(DEFCONSTANT +LOG-LOCAL3+   152 "reserved for local use ")
(DEFCONSTANT +LOG-LOCAL4+   160 "reserved for local use ")
(DEFCONSTANT +LOG-LOCAL5+   168 "reserved for local use ")
(DEFCONSTANT +LOG-LOCAL6+   176 "reserved for local use ")
(DEFCONSTANT +LOG-LOCAL7+   184 "reserved for local use ")





(DEFVAR *IDENT*      "clisp")
(DEFVAR *FACILITY*   +LOG-LOCAL0+)
(DEFVAR *LOG-PID*    NIL "log the pid with each message ")
(DEFVAR *LOG-CONS*   NIL "log on the console if errors in sending ")
(DEFVAR *LOG-ODELAY* NIL "delay open until first syslog() (default) ")
(DEFVAR *LOG-NDELAY* NIL "don't delay open ")
(DEFVAR *LOG-NOWAIT* NIL "don't wait for console forks: DEPRECATED ")
(DEFVAR *LOG-PERROR* NIL "log to stderr as well ")


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




(DEFUN OPENLOG (IDENT OPTION FACILITY)
  (SETQ *IDENT*      IDENT
        *FACILITY*   FACILITY
        *LOG-PID*    (/= 0 (LOGAND OPTION +LOG-PID+))
        *LOG-CONS*   (/= 0 (LOGAND OPTION +LOG-CONS+))
        *LOG-ODELAY* (/= 0 (LOGAND OPTION +LOG-ODELAY+))
        *LOG-NDELAY* (/= 0 (LOGAND OPTION +LOG-NDELAY+))
        *LOG-NOWAIT* (/= 0 (LOGAND OPTION +LOG-NOWAIT+))
        *LOG-PERROR* (/= 0 (LOGAND OPTION +LOG-PERROR+)))
  (VALUES))


(DEFUN old-SYSLOG (PRIORITY FCTRL &REST ARGUMENTS)
  (EXT:RUN-PROGRAM "logger"
    :ARGUMENTS  (APPEND (WHEN *LOG-PID*    (LIST "-i"))
                        (WHEN *LOG-PERROR* (LIST "-s"))
                        (LIST "-p" (format nil "~D" (+ *facility* PRIORITY))
                              "-t" *IDENT*
                              "--" (APPLY (FUNCTION FORMAT) NIL FCTRL ARGUMENTS)))
    :INPUT NIL :OUTPUT NIL :WAIT NIL)
  (VALUES))


(defun newlinep (ch)
  (member ch '(#\Newline #\Return #\Newline)))

(DEFUN SYSLOG (PRIORITY FCTRL &REST ARGUMENTS)
  (let ((logger (get-logger *facility* priority)))
    (let ((lines (apply (function format) nil fctrl arguments)))
      (princ lines logger)
      (unless (newlinep (aref lines (1- (length lines))))
        (terpri logger))
      (finish-output logger)))
  (values))


(DEFUN CLOSELOG ()
  (setf *loggers*
        (map 'array (lambda (logger) (when logger (close logger)) nil) *loggers*))
  (VALUES))


;;;; THE END ;;;;
