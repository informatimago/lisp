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


(defclass charms-screen (screen)
  ()
  (:documentation "This SCREEN subclass uses cl-charms (ncurses)."))

(defmethod screen-size ((screen charms-screen))
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
    (values height width)))

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

(defmethod keyboard-chord-no-hang ((screen charms-screen))
  (charms:get-char charms:*standard-window* :ignore-errors t))

(defmethod call-with-screen ((screen charms-screen) thunk)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters nil)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (funcall thunk screen)))

;;;; THE END ;;;;
