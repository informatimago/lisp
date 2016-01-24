;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               xterm.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-03-16 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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


(defvar *mktemp-program* "mktemp")
(defvar *mkfifo-program* "mkfifo")
(defvar *xterm-program*  "/opt/X11/bin/xterm")


(defun mktemp (pattern)
  (with-open-stream (input (process-output (run-program *mktemp-program* (list pattern) :output :stream)))
    (read-line input)))


(defun make-xterm-io-stream (&key (display ":0.0") (geometry "80x24")
                               (foreground "black") (background "white")
                               (title "lisp") (font "fixed")
                               (element-type 'character) (external-format :default))
  (let ((pipe (mktemp "/tmp/xterm-io-XXXXXX")))
    (ignore-errors (delete-file  pipe))
    (when (zerop (process-status (run-program *mkfifo-program* (list pipe) :wait t)))
      (run-program *xterm-program* (list
                                    "-geometry" geometry
                                    "-display"  display
                                    "-fg"       foreground
                                    "-bg"       background
                                    "-title"    title
                                    "-fn"       font
                                    "-e" (let ((*print-circle* nil)
                                               (*print-escape* t))
                                           (format nil "bash -c 'tty>>~A && cat ~:*~A'" pipe)))
                   :wait nil)
      (let* ((tty-name (with-open-file (s pipe) (read-line s)))
             (xio (make-two-way-stream
                   (open tty-name :direction :input  :element-type element-type :external-format external-format
                                  #+clisp :buffered #+clisp nil)
                   (open tty-name :direction :output :element-type element-type :external-format external-format
                                  :if-exists :append
                                  #+clisp :buffered #+clisp nil))))
        ;; (system::terminal-raw (two-way-stream-input-stream  xio) t t)
        (defmethod close :after ((x (eql xio)) &rest junk)
          (declare (ignore x junk))
          (ignore-errors
           (with-open-file (s pipe :direction :output)
             (write-line "Bye." s)
             (finish-output s)))
          (delete-file pipe)
          (close (two-way-stream-input-stream  xio))
          (close (two-way-stream-output-stream xio))
          (let () ;; ((clos::*warn-if-gf-already-called* nil))
            (remove-method #'close (find-method #'close '(:after) `((eql ,xio))))))
        xio))))


(cffi:defcfun "fdopen" :pointer (fd :int) (flag :string))
(cffi:defcfun "fclose" :int (file :pointer))

(defun make-xterm-io-filedes (&key (display ":0.0") (geometry "80x24")
                                (foreground "black") (background "white")
                                (title "lisp") (font "fixed"))
  #-ccl (error "Not implemented yet for ~S" (lisp-implementation-type))
  (let ((pipe (mktemp "/tmp/xterm-io-XXXXXX")))
    (ignore-errors (delete-file  pipe))
    (when (zerop (process-status (run-program *mkfifo-program* (list pipe) :wait t)))
      (run-program *xterm-program* (list
                                    "-geometry" geometry
                                    "-display"  display
                                    "-fg"       foreground
                                    "-bg"       background
                                    "-title"    title
                                    "-fn"       font
                                    "-e" (let ((*print-circle* nil)
                                               (*print-escape* t))
                                           (format nil "bash -c 'tty>>~A && cat ~:*~A'" pipe)))
                   :wait nil)
      (let ((tty-name (with-open-file (s pipe) (read-line s))))
        #+ccl (cffi:with-foreign-strings ((in  "r")
                                          (out "a"))
                (values (fdopen (ccl::fd-open tty-name #$O_RDONLY)  in)
                        (fdopen (ccl::fd-open tty-name (logior #$O_WRONLY #$O_APPEND)) out)))))))


(defclass xterm-screen (charms-screen)
  ((term :reader xterm-screen-term)
   (win  :reader xterm-screen-win))
  (:documentation
   #+french "Cette sous-classe de SCREEN utilise un xterm via un pty."
   #-(or french) "This SCREEN subclass uses an xterm thru a pty."))

(defmethod initialize-instance :after ((self xterm-screen) &key &allow-other-keys)
  (setf (slot-value self 'term) (multiple-value-bind (in out) (make-xterm-io-filedes)
                                  (cffi:with-foreign-string (term "xterm")
                                    (cl-charms/low-level:newterm term in out)))))

(defmethod screen-size ((self xterm-screen))
  (call-next-method))
(defmethod screen-cursor-position ((self xterm-screen))
  (call-next-method))
(defmethod set-screen-cursor-position ((self xterm-screen) line column)
  (call-next-method))
(defmethod clear-screen-to-eot ((self xterm-screen))
  (call-next-method))
(defmethod clear-screen-to-eol ((self xterm-screen))
  (call-next-method))
(defmethod delete-screen-line ((self xterm-screen))
  (call-next-method))
(defmethod insert-screen-line ((self xterm-screen))
  (call-next-method))
(defmethod screen-highlight-on ((self xterm-screen))
  (call-next-method))
(defmethod screen-highlight-off ((self xterm-screen))
  (call-next-method))
(defmethod screen-cursor-on ((self xterm-screen))
  (call-next-method))
(defmethod screen-cursor-off ((self xterm-screen))
  (call-next-method))
(defmethod keyboard-chord-no-hang ((self xterm-screen))
  (call-next-method))
(defmethod call-with-screen ((self xterm-screen) thunk)
  (unwind-protect
       (progn
         (cl-charms/low-level:set-term (xterm-screen-term self))
         (setf (slot-value self 'win) (cl-charms:make-window 0 0 0 0))
         (let ((charms:*standard-window* (xterm-screen-win self)))
           (charms:disable-echoing)
           (charms:enable-raw-input :interpret-control-characters nil)
           (charms:enable-non-blocking-mode charms:*standard-window*)
           (charms:enable-extra-keys        charms:*standard-window*) ; keypad t
           (charms::check-status (charms/ll:meta (charms::window-pointer charms:*standard-window*) charms/ll:TRUE))
           (funcall thunk self)))
    (charms::check-status (charms/ll:delwin (slot-value self 'win)))
    (setf (slot-value self 'win) nil)
    (charms:finalize)))




#-(and) (
         (defvar *x* (make-xterm-io-stream))



         (defparameter *screen* )

         (cl-charms/low-level:set-term *screen*)
         (defparameter *win* (cl-charms:make-window 0 0 0 0))

         (let ((buffer  (make-array 128 :element-type '(unsigned-byte 8))))
           (values (read-sequence buffer
                                  (process-output *p*))
                   buffer))

         (close (process-input *p*))
         (close (process-output *p*))

         (progn
           (defvar *in*)
           (defvar *out*)
           (multiple-value-setq (*in* *out*) (make-xterm-io-filedes))
           (values *in* *out*))

         
         (defparameter *screen* (multiple-value-bind (in out) (make-xterm-io-filedes)
                                  (cffi:with-foreign-string (term "xterm")
                                    (cl-charms/low-level:newterm term in out))))

         (cl-charms/low-level:set-term *screen*)
         (defparameter *win* (cl-charms:make-window 0 0 0 0))
         

         
         (make-xterm-io-filedes)
         

         )
