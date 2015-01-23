;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               clisp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    clisp specific functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-11 <PJB> Extracted from editor.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(in-package "COM.INFORMATIMAGO.EDITOR")



(defun make-xterm-io-stream (&key display geometry)
  (let* ((pipe (with-open-stream (s (ext:make-pipe-input-stream
                                     "mktemp /tmp/clisp-x-io-XXXXXX"))
                 (read-line s)))
         (title "CLISP I/O")
         ;; (clos::*warn-if-gf-already-called* nil)
         (font nil
               #+(or) "-*-console-medium-r-normal-*-16-*-*-*-*-*-*-*"
               #+(or)"-dec-terminal-bold-r-normal-*-14-*-*-*-*-*-dec-dectech"))
    ;; xterm creates a pty, forks, hooks the pty to stdin/stdout
    ;; and exec bash with the commands given in -e.
    ;; We write this pty path to our pipe,
    ;; and cat our pipe to wait for the end.
    ;; Meanwhile, we'll be reading and writing this pty.
    (ext:shell (format nil "rm -f ~S; mknod ~S p; xterm ~
                            ~:[~;~:*-geometry ~S~] ~:[~;~:*-display ~S~] ~
                            -fg green -bg black ~:[~;~:*-fn '~A'~] -n ~S -T ~S ~
                            -e 'tty >> ~S ; cat ~S' &" 
                       pipe pipe geometry display font title title pipe pipe))
    (let* ((tty-name (with-open-file (s pipe) (read-line s)))
           (xio (make-two-way-stream
                 (open tty-name :direction :input  :buffered nil)
                 (open tty-name :direction :output :buffered nil))))
      (system::terminal-raw (two-way-stream-input-stream  xio) t t)
      (defmethod close :after ((x (eql xio)) &rest junk)
        (declare (ignore x junk))
        (ignore-errors
          (with-open-file (s pipe :direction :output)
            (write-line "Bye." s)))
        (delete-file pipe)
        (close (two-way-stream-input-stream  xio))
        (close (two-way-stream-output-stream xio))
        (let () ;; ((clos::*warn-if-gf-already-called* nil))
          (remove-method #'close (find-method #'close '(:after) `((eql ,xio))))))
      xio)))


(defun screen-editor (&key log)
  (cond
    ((string= "xterm" (uiop/os:getenv "TERM"))
     (setf custom:*terminal-encoding* (ext:make-encoding
                                       :charset charset:iso-8859-1
                                       :line-terminator :unix)))
    ((string= "kterm" (uiop/os:getenv "TERM"))
     (setf custom:*terminal-encoding* (ext:make-encoding
                                       :charset charset:utf-8
                                       :line-terminator :unix))))
  (editor-reset)
  (let ((*log* (typecase log
                 ((member :xterm) (make-xterm-io-stream :geometry "100x24+0+0"))
                 ((or string pathname)  (open log
                                              :direction :output
                                              :if-exists :append
                                              :if-does-not-exist :create))
                 (file  log)
                 (otherwise (make-broadcast-stream)))))
    (unwind-protect
         (with-open-screen (make-instance 'clisp-screen)
           (editor-initialize *current-screen*)
           (unwind-protect
                (keyboard-loop)
             (set-screen-cursor-position *current-screen*
                                         0 (screen-size *current-screen*))
             (clear-screen *current-screen*))
           (editor-terminate))
      (close *log*))))



(defun keyboard-test ()
  (screen:with-window nil
    (screen:set-window-cursor-position screen:*window* 2 10)
    (format t "Hi")
    (EXT:WITH-KEYBOARD
        (LOOP
           :for ki = (READ-CHAR EXT:*KEYBOARD-INPUT*)
           :do
           (print ki)
           (print `((ext:char-key ki) ,(ext:char-key ki)))
           (print `((character ki)
                    ,(and (not (ext:char-key ki))
                          (zerop (ext:char-bits ki))
                          (character ki))))
           (print `((ext:char-font ki) ,(ext:char-font ki)))
           (print `((ext:char-bits ki) ,(ext:char-bits ki)))
           (dolist (modifier '(:control :meta :super :hyper))
             (print `((ext:char-bit ki ,modifier) ,(ext:char-bit ki modifier))))
           (finish-output)
           :until (EQL (and (not (ext:char-key ki))
                            (zerop (ext:char-bits ki))
                            (character ki)) #\q)))))




(defun xexample (&key (display ":0.0"))
  (let* ((old-terminal-io   *terminal-io*)
         (xterm-io          (make-xterm-io-stream :display display :geometry "+0+0"))
         (*terminal-io*     xterm-io)
         (*standard-output* (make-synonym-stream '*terminal-io*))
         (*standard-input*  (make-synonym-stream '*terminal-io*))
         (*error-output*    (make-synonym-stream '*terminal-io*))
         (*query-io*        (make-synonym-stream '*terminal-io*))
         ;; (*debug-io*        (make-synonym-stream '*terminal-io*))
         ;; (*trace-output*    (make-synonym-stream '*terminal-io*))
         (old-term          (uiop/os:getenv "TERM")))
    (setf (uiop/os:getenv "TERM") "xterm")
    (unwind-protect
         (progn (format *query-io* "~&Hello!~%") 
                (format *query-io* "~&X = ")
                (finish-output *query-io*)
                (let ((x (read *query-io*)))
                  (format *query-io* "~&~S = ~A~%" '(- (* 2 x) 3) (- (* 2 x) 3)))
                (y-or-n-p "Happy?"))
      (setf *terminal-io* old-terminal-io)
      (close xterm-io)
      (setf (uiop/os:getenv "TERM") old-term))))


;;;; THE END ;;;;






