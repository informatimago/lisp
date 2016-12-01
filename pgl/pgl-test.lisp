;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pgl-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Tests interactively the PGL.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-11-13 <PJB> Created.
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
(in-package "PGL")


(defun test/string-escape ()
  (assert (string= (string-escape (coerce #(#\bel #\bs #\page #\newline #\return #\tab #\vt #\" #\\) 'string))
                   "\"\\a\\b\\f\\n\\r\\t\\v\\042\\\\\""))
  (assert (string= (string-unescape "\"\\a\\b\\f\\n\\r\\t\\v\\042\\\\\"")
                   (coerce #(#\bel #\bs #\page #\newline #\return #\tab #\vt #\" #\\) 'string)))
  (assert (string= (string-escape "Hello\\ World\"!")
                   "\"Hello\\\\ World\\042!\""))
  (assert (string= (string-unescape "\"Hello\\\\ World\\042!\"")
                   "Hello\\ World\"!"))
  :success)

(defun test/scanner ()
  (assert (equal (let ((s (make-scanner " hello(\"Howdy\", 42,-123.456e+78,false,true,foo)")))
                   (loop
                     :for token := (next-token s)
                     :while token :collect token))
                 '((symbol . "hello")
                   #\( "Howdy" #\, 42 #\, -1.2345600000000003D+80 #\,
                   (boolean) #\, (boolean . t) #\, (symbol . "foo") #\))))
  :success)

(defun test/all ()
  (test/string-escape)
  (test/scanner))



(defun test-event-loop ()
  (unwind-protect
       (loop :for e := (get-next-event +any-event+)
             :when e
               :do (format *console-io* "~&~20,3F Got event ~A for window ~A~%"
                           (event-time e) (event-type-keyword e) (event-window e))
                   (case (event-type-keyword e)
                     (:window-closed      (loop-finish))
                     (:window-resized)
                     (:last-window-closed (loop-finish))
                     (:action-performed
                      (scase (event-action-command e)
                             (("OK")   (format *console-io* "~&Yay!~%"))
                             (("TEXT") (format *console-io* "~&Got text: ~S~%"
                                               (text *t*)))
                             (otherwise (format *console-io* "~&Got action ~S~%"
                                                (event-action-command e)))))
                     (:mouse-clicked)
                     (:mouse-pressed)
                     (:mouse-released)
                     (:mouse-moved)
                     (:mouse-dragged)
                     (:key-pressed)
                     (:key-released)
                     (:key-typed)
                     (:timer-ticked)))
    (format *console-io* "~2%Test Event Loop Done.~2%")))

(defun make-test-window-1 ()
  (let ((w 512)
        (h 342))
    (make-instance
     'window
     :x 20 :y 40
     :width w :height h
     :components (loop
                   :repeat 20
                   :collect (make-instance
                             (elt #(rect round-rect oval line)
                                  (random 4))
                             :x (random (- w 20.0d0))
                             :y (random (- h 20.0d0))
                             :width (+ 20 (random 100.0d0))
                             :height (+ 20 (random 100.0d0))
                             :color (elt *colors* (random (length *colors*)))
                             :fill-color (elt *colors* (random (length *colors*)))
                             :line-width (random 10.0d0)
                             :filled (zerop (random 2)))))))



'(
  (ccl:setenv "JBETRACE" "true" t)
  (ccl:setenv "JBETRACE" "false" t)

  (close-backend)
  (open-backend :program-name "Test Program")

  (defparameter *w* (make-instance 'window :title "Test Window"
                                           :width 512.0d0
                                           :height 342.0d0
                                           :x 50.0d0
                                           :y 50.0d0))

  (progn
    (compound-add *w* (make-instance 'label :text "Text:"
                                            :x 10 :y 40 :width 100 :height 20))

    (let ((tf (make-instance 'text-field :nchars 20  :action-command "TEXT"
                                         :x 60 :y 60 :width 100 :height 20)))
      (compound-add *w* tf)
      (set-text tf "Doctor Who")
      (defparameter *t* tf))

    (compound-add *w* (make-instance 'button  :label "OK" :action-command "OK"
                                              :x 10 :y 60 :width 60 :height 20))

    (defparameter *c* (make-instance 'chooser :items '("Red" "Green" "Blue")
                                              :x 20 :y 80))
    (compound-add *c*))





  (compound-remove *w* (aref (components *w*) 2))
  (defparameter *l1* (aref (components *w*) 0))
  (defparameter *t1* (aref (components *w*) 1))
  (defparameter *l2* (aref (components *w*) 2))
  (defparameter *t2* (aref (components *w*) 3))
  (progn
    (set-window-resizable *w*)
    (progn (set-object-size *w* 512 342)
           (repaint-windows)
           (set-object-location *w* 30 30))
    (progn (set-object-location *l1* 10 40) (set-object-location *t1* 50 20))
    (set-object-location *l2* 10 70) (set-object-location *t2* 50 50)
    (set-object-location (aref (components *w*) 2) 60 60)
    (components *w*)
    (text *t1*)"Doctor Who and the Daleks")
  (object.contains *w* 11.0d0 61.0d0)


  )

;;;; THE END ;;;;
