;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               charm-screen.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements a SCREEN using CL-CHARMS.
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.EDITOR")


(defclass charms-screen (screen)
  ()
  (:documentation "This SCREEN subclass uses cl-charms (ncurses)."))

(defmethod screen-size ((screen charms-screen))
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
    (values height (1- width))))

(defmethod screen-cursor-position ((screen charms-screen))
  (charms:cursor-position charms:*standard-window*))

(defmethod set-screen-cursor-position ((screen charms-screen) line column)
  (charms:move-cursor charms:*standard-window* column line))

(defmethod clear-screen-to-eot ((screen charms-screen))
  (charms:clear-window-after-cursor charms:*standard-window*))

(defmethod clear-screen-to-eol ((screen charms-screen))
  (charms:clear-line-after-cursor charms:*standard-window*))

(defmethod delete-screen-line ((screen charms-screen))
  ;; (charms/ll:deleteln)
  )

(defmethod insert-screen-line ((screen charms-screen))
  ;; (charms/ll:insertln)
  )

(defmethod screen-highlight-on ((screen charms-screen))
  )

(defmethod screen-highlight-off ((screen charms-screen))
  )

(defmethod screen-cursor-on ((screen charms-screen))
  )

(defmethod screen-cursor-off ((screen charms-screen))
  )

(defmethod screen-write-string ((screen charms-screen) string)
  (loop :for ch :across string
        :do (charms:write-char-at-cursor charms:*standard-window* ch))
  ;; (charms:write-string-at-cursor charms:*standard-window* string)
  string)

(defmethod screen-refresh ((screen charms-screen))
  (charms:refresh-window charms:*standard-window*))

(defmethod keyboard-chord-no-hang ((screen charms-screen))
  (let ((ch (charms:get-char charms:*standard-window* :ignore-error t)))
    (format *trace-output* "got char ~A~%" ch) (force-output *trace-output*)
    (when ch
      (if (find ch #(#\newline #\tab #\esc #\return #\rubout))
          (make-instance 'chord :character ch :modifiers 0)
          (let* ((code     (char-code ch))
                 (kode     (mod code 128))
                 (controlp (< kode 32))
                 (metap    (< 128 code)))
            (make-instance 'chord
                           :character (code-char (+ kode
                                                    (if controlp
                                                        (if (< 0 kode 27)
                                                            (load-time-value (char-code #\`))
                                                            (load-time-value (char-code #\@)))
                                                        0)))
                           :modifiers (+ (if controlp
                                             (expt 2 +control+)
                                             0)
                                         (if metap
                                             (expt 2 +meta+)
                                             0))))))))

(defmethod call-with-screen ((screen charms-screen) thunk)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters nil)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (charms:enable-extra-keys        charms:*standard-window*) ; keypad t
    (charms::check-status (charms/ll:meta (charms::window-pointer charms:*standard-window*) charms/ll:TRUE))
    (funcall thunk screen)))

;; (defmethod call-with-current-screen ((screen charms-screen) thunk)
;;   )

;;;; THE END ;;;;
