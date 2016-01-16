;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               screen.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the SCREEN interface.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-11 <PJB> Extracted from editor.lisp
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

;;;---------------------------------------------------------------------
;;; Screen interface
;;;---------------------------------------------------------------------


(defclass screen ()
  ((stream :reader screen-stream))
  (:documentation "This object represents the screen.
There are subclasses specific to each available screen device.
There are methods specialized on these subclasses to write on the screen."))

(defgeneric screen-open (screen))
(defgeneric screen-close (screen))
(defgeneric screen-initialize-for-terminal (screen terminal)
  (:method ((screen screen) terminal) terminal))

(defgeneric screen-size (screen))
(defgeneric screen-cursor-position (screen)
  (:documentation "Return the width and height of the screen."))
(defgeneric set-screen-cursor-position (screen line column))
(defgeneric clear-screen (screen)
  (:method ((screen screen))
    (set-screen-cursor-position screen 0 0)
    (clear-screen-to-eot screen)))
(defgeneric clear-screen-to-eot (screen))
(defgeneric clear-screen-to-eol (screen))
(defgeneric delete-screen-line (screen))
(defgeneric insert-screen-line (screen))
(defgeneric screen-highlight-on (screen))
(defgeneric screen-highlight-off (screen))
(defgeneric screen-cursor-on (screen)
  (:documentation "Show up the cursor."))
(defgeneric screen-cursor-off (screen)
  (:documentation "Hide the cursor."))

(defgeneric screen-write-string (screen string)
  (:documentation "Write the STRING on the SCREEN at the current cursor position."))
(defgeneric screen-format (screen format-control &rest arguments)
  (:documentation "Format and write on the Screen at the current cursor position.")
  (:method (screen format-control &rest arguments)
    (screen-write-string screen (apply (function format) nil format-control arguments))))

(defgeneric screen-refresh (screen))

(defgeneric chord-character (chord))
(defgeneric chord-modifiers (chord))

(defconstant +shift+   0)
(defconstant +control+ 1)
(defconstant +meta+    2)
(defconstant +alt+     3)
(defconstant +super+   4)
(defconstant +hyper+   5)
(defconstant +command+ 6)

(defgeneric chord-modifierp (chord modifier)
  (:method (chord (modifier integer))
    (logbitp modifier (chord-modifiers chord)))
  (:method (chord (modifier symbol))
    (chord-modifierp chord (ecase modifier
                             (:shift +shift+)
                             (:control +control+)
                             (:meta +meta+)
                             (:alt +alt+)
                             (:super +super+)
                             (:hyper +hyper+)
                             (:command +command+)))))


(defun symbolic-modifiers (modifiers)
  (loop
     :for bit = 1  :then (* 2 bit)
     :for modifier :in '(:shift :control :meta :alt :super :hyper :command)
     :unless (zerop (logand bit modifiers)) :collect modifier))

(defclass chord ()
  ((character :initarg :character :reader chord-character)
   (modifiers :initarg :modifiers :reader chord-modifiers)))

(defgeneric keyboard-chord-no-hang (screen)
  (:documentation "Returns the next keyboard chord, or NIL."))



(defvar *current-screen* nil
  "The current SCREEN instance.  In this version, there's only
one SCREEN instance, but a future version may be ''multitty'' (or
''multiframe'') like GNU emacs.")

(defgeneric call-with-screen (screen body)
  (:documentation "Calls the BODY function with as argument, the SCREEN,
while having activated this screen into the bidimentional mode.
This methods sets up and terminates the global state of the screen system."))

(defmacro with-screen (SCREEN &body body)
  "Executes the BODY with *CURRENT-SCREEN* bound to SCREEN,
while displaying this screen on the terminal.
No other WITH-SCREEN may be called in the BODY. This macros is used
for global setup and termination of the screen system."
  `(call-with-screen ,screen (lambda (*current-screen*) ,@body)))

;; (defgeneric call-with-current-screen (screen body)
;;   (:documentation "Calls the BODY function with as argument, the SCREEN,
;; while having activated this screen into the bidimentional mode."))
;; 
;; (defmacro with-current-screen (SCREEN &body body)
;;   "Executes the BODY with *CURRENT-SCREEN* bound to SCREEN,
;; while displaying this screen on the terminal.
;; The screen system must be currently activated with WITH-SCREEN.
;; This macro may be used to switch between alternate subscreens.
;; "
;;   `(call-with-current-screen ,screen (lambda (*current-screen*) ,@body)))


;;;; THE END ;;;;
