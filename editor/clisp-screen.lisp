;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               clisp-screen.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements a SCREEN using clisp screen package.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-11 <PJB> Extracted from editor.
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.EDITOR")


(defclass clisp-screen (screen)
  ((stream :reader screen-stream :initform (screen:make-window)))
  (:documentation "This SCREEN subclass uses the CLISP SCREEN package."))

(defmethod screen-open ((screen clisp-screen))
  (setf (screen-stream screen) (screen:make-window)))

(defmethod screen-close ((screen clisp-screen))
  (close (screen-stream screen)))

(defmethod screen-initialize-for-terminal ((screen clisp-screen) terminal)
  (cond
    ((string= "xterm" terminal)
     (setf custom:*terminal-encoding* (ext:make-encoding
                                       :charset charset:iso-8859-1
                                       :line-terminator :unix)))
    ((string= "kterm" terminal)
     (setf custom:*terminal-encoding* (ext:make-encoding
                                       :charset charset:utf-8
                                       :line-terminator :unix)))
    (t
     (warn "Unexpected terminal ~S" terminal))))

(defmethod screen-size ((screen clisp-screen))
  (screen:window-size (screen-stream screen)))

(defmethod screen-cursor-position ((screen clisp-screen))
  (screen:window-cursor-position (screen-stream screen)))

(defmethod set-screen-cursor-position ((screen clisp-screen) line column)
  (screen:set-window-cursor-position (screen-stream screen) line column))

(defmethod clear-screen ((screen clisp-screen))
  (screen:clear-window  (screen-stream screen)))

(defmethod clear-screen-to-eot ((screen clisp-screen))
  (screen:clear-window-to-eot  (screen-stream screen)))

(defmethod clear-screen-to-eol ((screen clisp-screen))
  (screen:clear-window-to-eol  (screen-stream screen)))

(defmethod delete-screen-line ((screen clisp-screen))
  (screen:delete-window-line (screen-stream screen)))

(defmethod insert-screen-line ((screen clisp-screen))
  (screen:insert-window-line (screen-stream screen)))

(defmethod screen-highlight-on ((screen clisp-screen))
  (screen:highlight-on (screen-stream screen)))

(defmethod screen-highlight-off ((screen clisp-screen))
  (screen:highlight-off (screen-stream screen)))

(defmethod screen-cursor-on ((screen clisp-screen))
  (screen:window-cursor-on (screen-stream screen)))

(defmethod screen-cursor-off ((screen clisp-screen))
  (screen:window-cursor-off (screen-stream screen)))

(defmethod screen-write-string ((screen clisp-screen) string)
  (write-string string (screen-stream screen))
  (finish-output (screen-stream screen))
  string)

(defmethod screen-refresh ((screen clisp-screen))
  screen)

(defmethod keyboard-chord-no-hang ((screen clisp-screen))
  (declare (ignorable screen))
  (let ((ki (ext:with-keyboard (read-char-no-hang ext:*keyboard-input*))))
    (when ki
      (make-instance
       'chord
       :modifiers (loop
                    :with bits = (ext:char-bits ki)
                    :for (bit modifier)
                      :in (load-time-value
                           (list (list EXT:CHAR-CONTROL-BIT +control+)
                                 (list EXT:CHAR-META-BIT    +meta+)
                                 (list EXT:CHAR-SUPER-BIT   +super+)
                                 (list EXT:CHAR-HYPER-BIT   +hyper+)))
                    :when (logand bits bit)
                      :sum (expt 2 modifier))
       :character (let ((ch (or (ext:char-key ki) (character ki))))
                    (if (ext:char-bit ki :control)
                        (char-downcase ch)
                        ch))))))

(defmethod call-with-screen ((screen clisp-screen) thunk)
  (unwind-protect (screen:with-window
                      (setf (screen-stream screen) screen:*window*)
                    (funcall thunk screen))
    (setf (screen-stream screen) nil)))

;; (defmethod call-with-current-screen ((screen clisp-screen) thunk)
;;   (assert (and screen:*window* (eql screen:*window* (screen-stream screen))))
;;   (funcall thunk screen))


;;;; THE END ;;;;
