;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               xterm.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports functions to open xterm streams.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-01-06 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2012
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
(declaim (declaration also-use-packages))
(declaim (also-use-packages "EXT" "COM.INFORMATIMAGO.CLISP.SUSV3"))
(defpackage "COM.INFORMATIMAGO.CLISP.XTERM"
  (:documentation "
    This package exports functions to open xterm streams.

    Copyright Pascal J. Bourguignon 2005 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:use "COMMON-LISP" "CLOS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "WITH-GENSYMS")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST" "PROPER-LIST-P")
  (:export
   "*XTERM-FONT*" "MAKE-XTERM-IO-STREAM" "SERVER-REPL" 
   "XTERM-LISTENER" "FORK-XTERM-LISTENER"))
(in-package "COM.INFORMATIMAGO.CLISP.XTERM")



(defvar *xterm-font* "-*-fixed-medium-r-normal-*-15-*-*-*-*-*-*-*")
;;(setf   *xterm-font* "-dec-terminal-bold-r-normal-*-14-*-*-*-*-*-dec-dectech")
;;(setf   *xterm-font* "-dec-terminal-medium-r-normal-*-14-*-*-*-*-*-*-*")
;;(setf   *xterm-font* "-*-lucidatypewriter-medium-r-normal-*-14-*-*-*-*-*-*-*")
;;(setf   *xterm-font* "-*-console-medium-r-normal-*-16-*-*-*-*-*-*-*")


;; [named-pipe]
;; [status-pipe] bash -c "rm -f named-pipe;mknod named-pipe p;(xterm -tm 'intr ^C' -e 'tty>>named-pipe;cat named-pipe'||echo :failure)&sleep 1; echo :success"
;; [pty]

(defun make-xterm-io-stream (&key (display nil) (title "CLISP I/O")
                             (foreground "green") (background "black")
                             (font *xterm-font*)
                             (element-type 'character)
                             (external-format :default))
  (let* ((pipe (with-open-stream (s (ext:make-pipe-input-stream
                                     "mktemp /tmp/clisp-x-io-XXXXXX"
                                     :element-type element-type
                                     :external-format external-format))
                 (read-line s)))
         tty-name xio)
    (with-open-stream 
        (status (ext:run-program "bash"
                  :arguments 
                  (list "-c"
                        (format nil 
                          "rm -f ~S;~
                           mknod ~S p;~
                           (xterm ~:[~;~:*-display ~S~] ~
                             -fg ~S -bg ~S ~
                             -fn '~A' -n ~S -T ~S ~
                             -e 'stty intr undef quit undef susp undef;~
                                 tty>>~S;cat ~S'||echo :FAILURE)~
                           &sleep 1;echo :SUCCESS" 
                          pipe pipe display
                          foreground background font
                          title title pipe pipe))
                  :input nil :output :stream))
      (case (let ((*read-eval* nil)) (read status nil nil))
        ((:failure nil) (return-from make-xterm-io-stream nil)))
      (setq tty-name (with-open-file (s pipe :direction :input) (read-line s))
            xio (make-two-way-stream
                 (open tty-name :direction :input)
                 (open tty-name :direction :output)))
      (defmethod close :after ((x (eql xio)) &rest junk)
        (declare (ignore x junk))
        (with-open-file (s pipe :direction :output) (write-line "Bye." s))
        (delete-file pipe)
        (close (two-way-stream-input-stream xio))
        (close (two-way-stream-output-stream xio))
        (remove-method (function close) 
                       (find-method (function close)
                                    '(:after) `((eql ,xio)))))
      xio)))


;; (with-open-file (out "/tmp/err" :direction :output
;;                  :if-does-not-exist :create :if-exists :append) 
;;   (format out "~&simple-condition: ")
;;   (apply (function format) out
;;          (simple-condition-format-control   err)
;;          (simple-condition-format-arguments err))
;;   (format out "~&streams = ~%~{    ~S~%~}~%" 
;;           (list *STANDARD-INPUT* *STANDARD-OUTPUT* 
;;                 *ERROR-OUTPUT* *QUERY-IO* *TRACE-OUTPUT* *DEBUG-IO*))
;;   (format out "~&DONE~%~%"))
;; (apply (function format) *standard-output*
;;        (simple-condition-format-control   err)
;;        (simple-condition-format-arguments err))
;; (with-open-file (out "/tmp/err" :direction :output 
;;                 :if-does-not-exist :create :if-exists :append) 
;;   (format out "~&done on standard output~&"))


(defun server-repl ()
  (do ((hist 1 (1+ hist))
       (+eof+ (gensym)))
      (nil)
    (format t "~%~A[~D]> " (package-name *package*) hist)
    (handling-errors
     (let ((input (read *standard-input* nil +eof+)))
       (when (or (equal '(exit) input)
                 (equal '(quit) input)
                 (eq +eof+ input))
         (return-from server-repl))
       (setf +++ ++   ++ +   + -   - input)
       (setf /// //   // /   / (multiple-value-list (eval -)))
       (setf *** **   ** *   * (first /))
       (format t "~& --> ~{~S~^ ;~%     ~}~%" /)))))


(defun xterm-listener (&key (display ":0.0") (title "CLISP LISTENER") 
                       (foreground "green") (background "black")
                       (font  *xterm-font*)
                       (element-type 'character)
                       (external-format :default)
                       (repl (function server-repl)))
  (let ((xterm (make-xterm-io-stream :display display :title title
                                     :foreground foreground
                                     :background background
                                     :font font
                                     :element-type element-type
                                     :external-format external-format)))
    (when xterm
      (unwind-protect 
           (let ((*query-io*        xterm)
                 (*standard-input*  xterm)
                 (*standard-output* xterm)
                 #||
                 (*ERROR-OUTPUT*    xterm)
                 (*DEBUG-IO*        xterm)
                 (*TRACE-OUTPUT*    xterm)||#)
             #|(loop (SYSTEM::MAIN-LOOP))|#
             (funcall repl))
        (close xterm)))))


(defun fork-xterm-listener (&key (display ":0.0"))
  (when (zerop (com.informatimago.clisp.susv3:fork))
    (xterm-listener :display display)
    (ext:exit 0)))


;;;; THE END ;;;;
