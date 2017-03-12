;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pgl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This package implements a Portable Graphics Library using the
;;;;    JavaBackEnd from the Stanford Portable Library.
;;;;    http://cs.stanford.edu/~eroberts/papers/ITiCSE-2013/PortableGraphicsLibrary.pdf
;;;;    https://github.com/cs50/spl
;;;;    https://cs.stanford.edu/people/eroberts/jtf/tutorial/UsingTheGraphicsPackage.html
;;;;
;;;;    The spl must be installed:
;;;;
;;;;         # Required system packages:
;;;;         # bash binutils coreutils findutils gcc java-1.?.0-openjdk-devel
;;;;
;;;;         cd /usr/local/src
;;;;         git clone git@github.com:cs50/spl.git
;;;;         cd spl
;;;;         make
;;;;         make install
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-03-10 <PJB> Added support for clisp.
;;;;    2016-01-30 <PJB> Moved SLOTED-OBJECT to UTLITY; renamed SLOTS to
;;;;                     SLOTS-FOR-PRINT, use EXTRACT-SLOTS.
;;;;    2015-11-12 <PJB> Created.
;;;;BUGS
;;;;    Currently only implemented on CCL using CCL:RUN-PROGRAM.
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2017
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
(defpackage "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.LOW-LEVEL"
  (:nicknames "PGL.LOW-LEVEL")
  (:use)
  (:documentation "

This package exports the low-level functions
that send messages to the JavaBackEnd.
There shouldn't be a need to use them directly.

Copyright Pascal J. Bourguignon 2015 - 2015
Licensed under the AGPL3.

")
  (:export
   ;; The JavaBackEnd API functions.
   "FILE.OPEN-FILE-DIALOG" "3D-RECT.CREATE" "3D-RECT.SET-RAISED"
   "ARC.CREATE" "ARC.SET-FRAME-RECTANGLE" "ARC.SET-START-ANGLE"
   "ARC.SET-SWEEP-ANGLE" "BUTTON.CREATE" "CHECK-BOX.CREATE"
   "CHECK-BOX.IS-SELECTED" "CHECK-BOX.SET-SELECTED" "COMPOUND.ADD"
   "COMPOUND.CREATE" "EVENT.GET-NEXT-EVENT" "EVENT.WAIT-FOR-EVENT"
   "IMAGE.CREATE" "INTERACTOR.SET-ACTION-COMMAND" "INTERACTOR.GET-SIZE"
   "LABEL.CREATE" "LABEL.GET-FONT-ASCENT" "LABEL.GET-FONT-DESCENT"
   "LABEL.GET-SIZE" "LABEL.SET-FONT" "LABEL.SET-LABEL" "LINE.CREATE"
   "LINE.SET-END-POINT" "LINE.SET-START-POINT" "OBJECT.CONTAINS"
   "OBJECT.DELETE" "OBJECT.GET-BOUNDS" "OBJECT.REMOVE" "OBJECT.ROTATE"
   "OBJECT.SCALE" "OBJECT.SEND-BACKWARD" "OBJECT.SEND-FORWARD"
   "OBJECT.SEND-TO-BACK" "OBJECT.SEND-TO-FRONT" "OBJECT.SET-COLOR"
   "OBJECT.SET-FILL-COLOR" "OBJECT.SET-FILLED" "OBJECT.SET-LINE-WIDTH"
   "OBJECT.SET-LOCATION" "OBJECT.SET-SIZE" "OBJECT.SET-VISIBLE"
   "OVAL.CREATE" "POLYGON.ADD-VERTEX" "POLYGON.CREATE" "RECT.CREATE"
   "ROUND-RECT.CREATE" "SLIDER.CREATE" "SLIDER.GET-VALUE"
   "SLIDER.SET-VALUE" "TEXT-FIELD.CREATE" "TEXT-FIELD.GET-TEXT"
   "TEXT-FIELD.SET-TEXT" "CHOOSER.CREATE" "CHOOSER.ADD-ITEM"
   "CHOOSER.GET-SELECTED-ITEM" "CHOOSER.SET-SELECTED-ITEM" "TIMER.CREATE"
   "TIMER.DELETE" "TIMER.PAUSE" "TIMER.START" "TIMER.STOP"
   "WINDOW.ADD-TO-REGION" "WINDOW.SET-REGION-ALIGNMENT" "WINDOW.CLEAR"
   "WINDOW.CLOSE" "WINDOW.CREATE" "WINDOW.DELETE" "WINDOW.DRAW"
   "WINDOW.EXIT-GRAPHICS" "WINDOW.GET-SCREEN-HEIGHT"
   "WINDOW.GET-SCREEN-WIDTH" "WINDOW.REPAINT" "WINDOW.REQUEST-FOCUS"
   "WINDOW.SET-RESIZABLE" "WINDOW.SET-TITLE" "WINDOW.SET-VISIBLE"
   "TOP-COMPOUND.CREATE" "CONSOLE.CLEAR" "CONSOLE.GET-LINE"
   "CONSOLE.PRINT" "CONSOLE.PRINTLN" "CONSOLE.SET-FONT"
   "CONSOLE.SET-SIZE" "SOUND.CREATE" "SOUND.DELETE" "SOUND.PLAY"))

(defpackage "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY"

  (:nicknames "PGL" "COM.INFORMATIMAGO.PGL")

  (:documentation "

This package implements a Portable Graphics Library using the
JavaBackEnd from the Stanford Portable Library.
http://cs.stanford.edu/~eroberts/papers/ITiCSE-2013/PortableGraphicsLibrary.pdf
https://github.com/cs50/spl

It defines and export a set of CLOS classes to represent the GUI
objects of the JavaBackEnd, along with methods to send the requests
and process the results.

The access to the JavaBackEnd is guarded by a mutex, so it should be
possible to accessed from multiple threads.  However, the objects
defined in this library are not thread-safe, so care should be taken
when mutating the same object from several threads.  It is assumed
that distinct threads will work with different objects.

Copyright Pascal J. Bourguignon 2015 - 2017
Licensed under the AGPL3.

")

  (:use ;; "COMMON-LISP"
        "CL-STEPPER"
        "TRIVIAL-GRAY-STREAMS"
        "BORDEAUX-THREADS"
        "ORG.MAPCAR.PARSE-NUMBER"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE"
        "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.LOW-LEVEL")

  (:import-from "UIOP" "GETENV")

  (:export
   ;; The base types and structures:
   "INT" "DOUBLE"

   "DIMENSION" "MAKE-DIMENSION" "COPY-DIMENSION" "DIMENSION-P"
   "DIMENSION-WIDTH" "DIMENSION-HEIGHT"

   "POINT" "MAKE-POINT" "COPY-POINT" "POINT-P" "X" "Y"

   "RECTANGLE" "MAKE-RECTANGLE" "COPY-RECTANGLE" "RECTANGLE-P"
   "X" "Y" "WIDTH" "HEIGHT"
   "RECTANGLE-EMPTYP" "RECTANGLE-CONTAINS-POINT-P")

  (:export
   ;; Event Masks:
   "+ACTION-EVENT+" "+KEY-EVENT+" "+TIMER-EVENT+" "+WINDOW-EVENT+"
   "+MOUSE-EVENT+" "+CLICK-EVENT+" "+ANY-EVENT+"
   ;; Event Types:
   "+WINDOW-CLOSED+" "+WINDOW-RESIZED+" "+ACTION-PERFORMED+"
   "+MOUSE-CLICKED+" "+MOUSE-PRESSED+" "+MOUSE-RELEASED+" "+MOUSE-MOVED+"
   "+MOUSE-DRAGGED+" "+KEY-PRESSED+" "+KEY-RELEASED+" "+KEY-TYPED+"
   "+TIMER-TICKED+"
   ;; Event class:
   "EVENT" "EVENT-TYPE" "EVENT-MODIFIERS" "EVENT-TIME" "EVENT-WINDOW"
   "EVENT-SOURCE" "EVENT-ACTION-COMMAND" "EVENT-X" "EVENT-Y"
   "EVENT-KEY-CHAR" "EVENT-KEY-CODE" "EVENT-TIMER"
   "EVENT-TYPE-KEYWORD"
   ;; Event functions:
   "WAIT-FOR-CLICK" "WAIT-FOR-EVENT" "GET-NEXT-EVENT")

  (:export
   "*DEFAULT-LABEL-FONT*" "*DEFAULT-CORNER*" "*ARC-TOLERANCE*"
   "*LINE-TOLERANCE*"
   "SQUARE" "DEGREE-TO-RADIAN" "RADIAN-TO-DEGREE" "COS-DEGREE" "SIN-DEGREE"
   "SCREEN-WIDTH" "SCREEN-HEIGHT" "PAUSE")

  (:export
   ;; Portable Graphic Library classes and methods:
   "TIMER" "DURATION-MS" "START-TIMER" "STOP-TIMER"
   "OBJECT" "ID" "X" "Y" "WIDTH" "HEIGHT" "COLOR" "FILL-COLOR"
   "LINE-WIDTH" "FILLED" "VISIBLE"  "SET-LOCATION" "SET-SIZE"
   "SET-COLOR" "SET-FILL-COLOR" "SET-LINE-WIDTH" "SET-FILLED"
   "SET-VISIBLE" "ROTATE" "SCALE" "SEND-BACKWARD" "SEND-FORWARD"
   "SEND-TO-BACK" "SEND-TO-FRONT"
   "RECT" "3DRECT" "RAISED" "ROUND-RECT" "CORNER" "OVAL" "CONTAINS"
   "LINE" "START-POINT" "END-POINT" "SET-START-POINT" "SET-END-POINT"
   "ARC" "START" "SWEEP" "SET-FRAME-RECTANGLE" "CONTAINS-ANGLE"
   "BOUNDS" "POLYGON" "VERTICES" "CX" "CY" "ADD-VERTEX" "ADD-VERTICES"
   "ADD-EDGE" "ADD-POLAR-EDGE" "COMPONENTS" "COMPOUND-ADD"
   "COMPOUND-REMOVE" "GET-OBJECT-AT" "COMPOUND" "TOP-COMPOUND" "LABEL"
   "FONT" "TEXT" "ASCENT" "DESCENT" "SET-FONT" "SET-TEXT" "IMAGE"
   "FILENAME" "INTERACTOR" "ACTION-COMMAND" "LABEL"
   "SET-ACTION-COMMAND" "GET-SIZE" "BUTTON" "CHECK-BOX" "SELECTED"
   "SELECTED" "SET-SELECTED" "SLIDER" "MINIMUM" "MAXIMUM" "VALUE"
   "SET-VALUE" "TEXT-FIELD" "NCHARS" "CHOOSER" "ITEMS" "SELECTED-ITEM"
   "ADD-ITEM" "SET-SELECTED-ITEM" "WINDOW" "COLOR" "TITLE" "RESIZABLE"
   "TOP-COMPOUND" "CLOSE-WINDOW" "REQUEST-FOCUS" "REPAINT-WINDOW" "CLEAR-WINDOW"
   "SET-VISIBLE" "SET-RESIZABLE" "SET-TITLE" "DRAW-LINE"
   "DRAW-POLAR-LINE" "DRAW-OVAL" "FILL-OVAL" "DRAW-RECT" "FILL-RECT"
   "DRAW" "DRAW-AT" "COMPOUND-ADD-AT" "COMPOUND-ADD-TO-REGION" "FREE"
   "LOCATION")

  (:export
   ;; The *console-io* stream:
   "*CONSOLE-IO*" "CONSOLE-STREAM" "CONSOLE-SET-SIZE"
   "CONSOLE-SET-FONT" "CONSOLE-CLEAR")

  (:export
   "*COLORS*" "*BLACK*" "*DARK-GRAY*" "*GRAY*" "*LIGHT-GRAY*"
   "*WHITE*" "*RED*" "*YELLOW*" "*GREEN*" "*CYAN*" "*BLUE*"
   "*MAGENTA*" "*ORANGE*" "*PINK*")

  (:export
   ;; Backend.
   "OPEN-BACKEND" "CLOSE-BACKEND" "*PROGRAM-NAME*" "*JAVA-PROGRAM*"
   "*SPL-PATH*"  "JBE-ERROR" "JBE-SYNTAX-ERROR")

  (:export
   ;; Extensions
   "WINDOWS" "INTERACTORS" "TIMERS"))

(in-package "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY")


(declaim (declaration stepper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic types and geometric structures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftype int    () `(integer ,(- (expt 2 31)) ,(- (expt 2 31) 1)))
(deftype double () 'double-float)
(defun int (real) (round real))
(defun double (real) (coerce real 'double))
(defstruct point     (x 0.0d0) (y 0.0d0))
(defstruct dimension (width 0.0d0) (height 0.0d0))
(defstruct rectangle (x 0.0d0) (y 0.0d0) (width 0.0d0) (height 0.0d0))

(defgeneric x (object)
  (:method ((p point)) (point-x p))
  (:method ((r rectangle)) (rectangle-x r)))
(defgeneric y (object)
  (:method ((p point)) (point-y p))
  (:method ((r rectangle)) (rectangle-y r)))
(defgeneric width (object)
  (:method ((p point)) 0)
  (:method ((r rectangle)) (rectangle-width r))
  (:method ((d dimension)) (dimension-width d)))
(defgeneric height (object)
  (:method ((p point)) 0)
  (:method ((r rectangle)) (rectangle-height r))
  (:method ((d dimension)) (dimension-height d)))

(defun rectangle-emptyp (r)
  "Whether the rectangle R is empty (null area)."
  (or (not (plusp (width r)))
      (not (plusp (height r)))))

(defun rectangle-contains-point-p (r p)
  "Whether the point P is inside the rectangle R (inclusive of the perimeter."
  (and (<= (x r) (x p) (+ (x r) (width r)))
       (<= (y r) (y p) (+ (y r) (height r)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Events
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +action-event+        #x010)
  (defconstant +key-event+           #x020)
  (defconstant +timer-event+         #x040)
  (defconstant +window-event+        #x080)
  (defconstant +mouse-event+         #x100)
  (defconstant +click-event+         #x200)
  (defconstant +any-event+           #x3f0))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +window-closed+       (+ +window-event+ 1))
  (defconstant +window-resized+      (+ +window-event+ 2))
  (defconstant +last-window-closed+  (+ +window-event+ 15))
  (defconstant +action-performed+    (+ +action-event+ 1))
  (defconstant +mouse-clicked+       (+ +mouse-event+ 1))
  (defconstant +mouse-pressed+       (+ +mouse-event+ 2))
  (defconstant +mouse-released+      (+ +mouse-event+ 3))
  (defconstant +mouse-moved+         (+ +mouse-event+ 4))
  (defconstant +mouse-dragged+       (+ +mouse-event+ 5))
  (defconstant +key-pressed+         (+ +key-event+ 1))
  (defconstant +key-released+        (+ +key-event+ 2))
  (defconstant +key-typed+           (+ +key-event+ 3))
  (defconstant +timer-ticked+        (+ +timer-event+ 1)))

(defclass event ()
  ((type           :initarg :type           :initform 0     :type int                 :reader event-type)
   (modifiers      :initarg :modifiers      :initform 0     :type int                 :reader event-modifiers)
   (time           :initarg :time           :initform 0.0d0 :type double              :reader event-time)
   (window         :initarg :window         :initform nil   :type (or null window)    :reader event-window)
   (source         :initarg :source         :initform nil   :type (or null object)    :reader event-source)
   (action-command :initarg :action-command :initform nil   :type (or null string)    :reader event-action-command)
   (x              :initarg :x              :initform 0.0d0 :type double              :reader event-x)
   (y              :initarg :y              :initform 0.0d0 :type double              :reader event-y)
   (key-char       :initarg :key-char       :initform nil   :type (or null character) :reader event-key-char)
   (key-code       :initarg :key-code       :initform 0     :type int                 :reader event-key-code)
   (timer          :initarg :timer          :initform nil   :type (or null timer)     :reader event-timer)))

(defmethod print-object ((self event) stream)
  (declare (stepper disable))
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~{~S~^ ~}" (list :type (event-type-keyword self)
                                     :modifiers (event-modifiers self)
                                     :time (event-time self)
                                     :window (event-window self)))
    (case (logand +any-event+ (event-type self))
      ((#.+action-event+)
       (format stream "~{ ~S~}" (list :source (event-source self)
                                      :action-command (event-action-command self))))
      ((#.+key-event+)
       (format stream "~{ ~S~}" (list :key-char (event-key-char self)
                                      :key-code (event-key-code self))))
      ((#.+timer-event+)
       (format stream "~{ ~S~}" (list :timer (event-timer self))))
      ((#.+window-event+))
      ((#.+mouse-event+ #.+click-event+)
       (format stream "~{ ~S~}" (list :x (event-x self)
                                      :y (event-y self))))))
  self)

(defmethod event-type ((self null)) 0)

(defgeneric event-type-keyword (event)
  (:documentation "
RETURN: the event type as a lisp keyword, one of:
        :WINDOW-CLOSED :WINDOW-RESIZED :LAST-WINDOW-CLOSED
        :ACTION-PERFORMED :MOUSE-CLICKED :MOUSE-PRESSED
        :MOUSE-RELEASED :MOUSE-MOVED :MOUSE-DRAGGED :KEY-PRESSED
        :KEY-RELEASED :KEY-TYPED :TIMER-TICKED
")
  (:method ((event null))
    nil)
  (:method ((event event))
    (ecase (event-type event)
      ((#.+window-closed+)      :window-closed)
      ((#.+window-resized+)     :window-resized)
      ((#.+last-window-closed+) :last-window-closed)
      ((#.+action-performed+)   :action-performed)
      ((#.+mouse-clicked+)      :mouse-clicked)
      ((#.+mouse-pressed+)      :mouse-pressed)
      ((#.+mouse-released+)     :mouse-released)
      ((#.+mouse-moved+)        :mouse-moved)
      ((#.+mouse-dragged+)      :mouse-dragged)
      ((#.+key-pressed+)        :key-pressed)
      ((#.+key-released+)       :key-released)
      ((#.+key-typed+)          :key-typed)
      ((#.+timer-ticked+)       :timer-ticked))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Backend
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *backend-lock* (bt:make-lock "*BACKEND*"))
(defvar *backend* nil)

(defstruct backend
  output
  input
  process
  lock)


;; TODO: deal with encodings explicitely in ccl.

#+clisp (defparameter *command-encoding*
          ;; Note: on MS-Windows you may need to use another charset.
          (ext:make-encoding :charset charset:utf-8
                             :line-terminator :unix
                             :input-error-action :ignore
                             :output-error-action #\uFFFD)
          "This is the encoding for the java command (we pass the *program-name*
on the command line so it may be outside of ASCII.")

#+clisp (defparameter *jbe-encoding*
          (ext:make-encoding :charset charset:utf-8
                             :line-terminator :dos
                             :input-error-action #\uFFFD
                             :output-error-action #\uFFFD)
          "This is the encoding for the data exchanged with the java backend.
It shall be UTF-8.")

(defun shell-quote-argument (argument)
  "Quote ARGUMENT for passing as argument to an inferior shell."
  #+(or MSWINDOWS WIN32)
  ;; Quote using double quotes, but escape any existing quotes in
  ;; the argument with backslashes.
  (let ((result "")
        (start 0)
        (end)
        (match-beginning)
        (match-end))
    (when (or (null (setf match-end (position #\" argument)))
              (< match-end (length argument)))
      (loop
        :while (setf match-beginning (position #\" argument :start start))
        :do (setf end (1+ match-beginning)
                  result (concatenate 'string result (subseq argument start end)
                                      "\\" (subseq argument end (1+ end)))
                  start (1+ end))))
    (concatenate 'string "\"" result (subseq argument start) "\""))
  #-(or MSWINDOWS WIN32)
  (if (equal argument "")
      "''"
      ;; Quote everything except POSIX filename characters.
      ;; This should be safe enough even for really weird shells.
      (let ((result "")
            (start 0)
            (end)
            (match-beginning)
            (match-end))
        (loop
          :while (setf match-end (position-if-not (lambda (ch) (or (alphanumericp ch) (position ch "-_./")))  argument :start start))
          :do (setf end match-end
                    result (concatenate 'string result (subseq argument start end)
                                        "\\" (subseq argument end (1+ end)))
                    start (1+ end)))
        (concatenate 'string result (subseq argument start)))))

(defun make-backend-pipe (command arguments)
  (let ((process (progn
                   #+ccl (ccl:run-program command arguments
                                          :wait nil :pty t
                                          :input :stream
                                          :output :stream
                                          :error *error-output*
                                          :sharing :lock)
                   #-(and) (multiple-value-list
                             (ext:letf ((custom:*misc-encoding*         *command-encoding*)
                                        (custom:*default-file-encoding* *jbe-encoding*))
                               (ext:run-program command
                                                :arguments arguments
                                                :wait nil
                                                :input :stream
                                                :output :stream)))
                   #+clisp (multiple-value-list
                            (ext:letf ((custom:*misc-encoding*         *command-encoding*)
                                       (custom:*default-file-encoding* *jbe-encoding*))
                              (ext:make-pipe-io-stream (format nil "~A ~{~A~^ ~}"
                                                               command
                                                               (mapcar (function shell-quote-argument)
                                                                       arguments))
                                                       :element-type 'character
                                                       :external-format custom:*default-file-encoding*
                                                       :buffered nil)))
                   #-(or ccl clisp) (progn
                                      (warn "~S not implemented yet." 'make-backend-pipe)
                                      t))))
    #+clisp (close (first process))
    (make-backend
     :process (progn
                #+ccl process
                #+clisp nil
                #-(or ccl clisp) (progn
                                   (warn "~S not implemented yet." 'backend-process)
                                   process))
     :output  (progn
                #+ccl (ccl::external-process-input process)
                #+clisp (third process)
                #-(or ccl clisp) (progn
                                   (warn "~S not implemented yet." 'backend-output)
                                   *standard-output*))
     :input   (progn
                #+ccl (ccl::external-process-output process)
                #+clisp (second process)
                #-(or ccl clisp) (progn
                                   (warn "~S not implemented yet." 'backend-input)
                                   *standard-output*))
     :lock    (bt:make-lock "JBEBackend"))))

(defmacro with-backend-locked (backend &body body)
  `(bt:with-lock-held ((backend-lock ,backend))
     ,@body))


(defvar *java-program* "java"
  "The path to the java program.")

(defvar *spl-path*     "/usr/local/lib/spl.jar"
  "The path to the spl.jar; used unless OPEN-BACKEND :CLASSPATH
argument or the CLASSPATH environment variable says otherwise.")

(defvar *program-name* "Untitled"
  "The default program name, displayed in the back-end GUI menubar.")


(defun open-backend (&key (program-name *program-name*) classpath)
  "
Launches the JavaBackEnd GUI.
If the backend is already open, nothing is done.

CLASSPATH:    If given, then it should be the path to the spl.jar file.
              else if the environment variable CLASSPATH is set,
                   then use it to find the spl.jar,
              otherwise use *SPL-PATH* by default.

PROGRAM-NAME: (defaults to *PROGRAM-NAME*) gives the name of
              the program to be displayed in the GUI menubar.
"
  (bt:with-lock-held (*backend-lock*)
    (unless *backend*
      (setf *program-name* program-name)
      (let ((classpath (or classpath (getenv "CLASSPATH") *spl-path*)))
        (setf *backend* (make-backend-pipe *java-program*
                                           (list (format nil "-Xdock:name=~A" program-name)
                                                 "-classpath"
                                                 classpath
                                                 "stanford/spl/JavaBackEnd"
                                                 program-name)))))))

(defvar *closing* nil)
(defun close-backend ()
  "
Quits the JavaBackEnd GUI.
If the backend is not open, nothing is done.
"
  (unless *closing*
    (let ((*closing* t))
      (bt:with-lock-held (*backend-lock*)
        (when *backend*
          (unwind-protect
               (progn
                 (ignore-errors (window.exit-graphics))
                 ;; #+ccl (ignore-errors (ccl:signal-external-process (backend-process *backend*) 9 :error-if-exited nil))
                 (close (backend-output *backend*))
                 (close (backend-input  *backend*)))
            (clear-registers)
            (setf *backend* nil)))))))



(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun decode-boolean (value)
   (and (not (null value))
        (plusp (length value))
        (char-equal #\t (aref value 0)))))

(defvar *jbetrace* (decode-boolean (getenv "JBETRACE")))

(defun send (command &rest arguments)
  "Sends a Java Back End command to the Java Back End."
  (let ((stream (if *backend*
                    (backend-output *backend*)
                    *standard-output*))
        (cmd (format nil "~A(~{~A~^,~})" command arguments)))
    (when *jbetrace* (format *trace-output* "~&-> ~A~%" cmd))
    (clear-input (backend-input *backend*))
    (write-line cmd stream)
    (force-output stream)))


(define-condition jbe-error (simple-error)
  ()
  (:documentation "Condition for Java Back End errors."))

(define-condition jbe-syntax-error (jbe-error)
  ()
  (:documentation "Condition for Java Back End errors in parsing responses."))

(defvar *event-queue*         (make-queue))

(defvar *source-registry*     (make-hash-table))
(defvar *window-registry*     (make-hash-table))
(defvar *timer-registry*      (make-hash-table))

(defun clear-registers ()
  (clrhash *source-registry*)
  (clrhash *window-registry*)
  (clrhash *timer-registry*)
  (setf *event-queue* (make-queue)))

(defun get-source (id) (gethash id *source-registry*))
(defun get-window (id) (gethash id *window-registry*))
(defun get-timer  (id) (gethash id *timer-registry*))

(defun windows     ()  (hash-table-values *window-registry*))
(defun interactors ()  (hash-table-values *source-registry*))
(defun timers      ()  (hash-table-values *timer-registry*))

(defgeneric register (self))
(defgeneric unregister (self))




(defun string-unescape (string)
  (with-output-to-string (*standard-output*)
    (with-input-from-string (*standard-input* string)
      (let ((ch (read-char *standard-input* nil)))
        (unless (char= ch #\")
          (error 'jbe-syntax-error
                 :format-control "Unexpected character ~S found in string literal ~S"
                 :format-arguments (list ch string))))
      (loop
        :with escaped := nil
        :for ch := (read-char *standard-input* nil)
        :do (if escaped
                (progn
                  (case ch
                   ((#\a) (princ #\Bel))
                   ((#\b) (princ #\bs))
                   ((#\f) (princ #\page))
                   ((#\n) (princ #\newline))
                   ((#\r) (princ #\return))
                   ((#\t) (princ #\tab))
                   ((#\v) (princ #\vt))
                   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                    (let ((buffer (make-string 3)))
                      (setf (aref buffer 0) ch
                            (aref buffer 1) (read-char *standard-input*)
                            (aref buffer 2) (read-char *standard-input*))
                      (unless (and (digit-char-p (aref buffer 1) 8)
                                   (digit-char-p (aref buffer 2) 8))
                        (error 'jbe-syntax-error
                               :format-control "Unexpected character ~S found in escape sequence in string literal ~S"
                               :format-arguments (list ch string)))
                      (princ (code-char (parse-integer buffer :radix 8)))))
                   (otherwise (princ ch)))
                  (setf escaped nil))
                (case ch
                  ((#\\) (setf escaped t))
                  ((#\") (loop-finish))
                  (otherwise (princ ch))))))))

(defun string-escape (string)
  (with-output-to-string (*standard-output*)
    (with-input-from-string (*standard-input* string)
      (princ "\"")
      (loop
        :for ch := (read-char *standard-input* nil nil)
        :while ch
        :do (case ch
              ((#\Bel)     (princ "\\a"))
              ((#\bs)      (princ "\\b"))
              ((#\page)    (princ "\\f"))
              ((#\newline) (princ "\\n"))
              ((#\return)  (princ "\\r"))
              ((#\tab)     (princ "\\t"))
              ((#\vt)      (princ "\\v"))
              ((#\")       (format t "\\~3,'0o" (char-code ch)))
              ((#\\)       (princ "\\\\"))
              (otherwise
               (if (= 3 (length (prin1-to-string ch)))
                   (princ ch)
                   ;; bug for bug compatible with spl:
                   (format t "\\~3,'0o" (logand #xff (char-code ch)))))))
      (princ "\""))))

(defun gid (id) (format nil "\"0x~X\"" id))

(defun decode-id (id)
  (assert (prefixp "0x" id))
  (parse-integer id :start 2 :radix 16))

(defun encode-double (value)
  ;; We must generate Java floating points.
  (substitute #\e #\D (format nil "~:@(~,,,,,,'dE~)" value)
              :test (function char-equal)))


(defun get-result ()
  (let ((stream   (backend-input *backend*)))
    (handler-case
        (loop
          (let ((line (read-line stream)))
            (when *jbetrace* (format *trace-output* "~&<- ~A~%" line))
            (cond ((prefixp "result:" line)
                   (return-from get-result (subseq line 7)))
                  ((prefixp "event:" line)
                   (queue-enqueue *event-queue* (parse-event (subseq line 6)))))))
      (error (err)
        (princ-to-string err)))))

(defun get-error ()
  (let ((result (get-result)))
    (unless (or (string-equal result "ok") (string-equal result "ack"))
      (error 'jbe-error :format-control "~A" :format-arguments (list result)))))

(defun get-int ()
  (parse-integer (get-result)))

(defun parse-double (string)
  (let ((*read-default-float-format* 'double-float))
    (parse-number (substitute #\d #\e string :test (function char-equal)))))

(defun get-double ()
  (parse-double (get-result)))

(defun get-boolean ()
  (decode-boolean (get-result)))

(defun get-dimension ()
  (let ((scanner (make-scanner (get-result)))
        width height)
    (eat-token scanner '(symbol . "GDimension"))
    (eat-token scanner #\()
    (setf width (ensure-token (next-token scanner) 'double))
    (eat-token scanner #\,)
    (setf height (ensure-token (next-token scanner) 'double))
    (eat-token scanner #\))
    (make-dimension :width width :height height)))

(defun get-rectangle ()
  (let ((scanner (make-scanner (get-result)))
        x y width height)
    (eat-token scanner '(symbol . "GRectangle"))
    (eat-token scanner #\()
    (setf x (ensure-token (next-token scanner) 'double))
    (eat-token scanner #\,)
    (setf y (ensure-token (next-token scanner) 'double))
    (eat-token scanner #\,)
    (setf width (ensure-token (next-token scanner) 'double))
    (eat-token scanner #\,)
    (setf height (ensure-token (next-token scanner) 'double))
    (eat-token scanner #\))
    (make-instance 'rectangle :x (double x)
                              :y (double y)
                              :width (double width)
                              :height (double height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scanner
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-scanner (string)
  (cons 0 string))

(defun next-char (scanner)
  (with-accessors ((pos car) (src cdr)) scanner
    (when (< pos (length src)) (aref src pos))))

(defun eat-char (scanner)
  (with-accessors ((pos car) (src cdr)) scanner
    (when (< pos (length src)) (incf pos))))

(declaim (inline whitespacep))
(defun whitespacep (ch)
  (find ch #(#\space #\tab #\newline #\return #\page #\vt)))

(defun skip-spaces (scanner)
  (loop :while (whitespacep (next-char scanner))
        :do (eat-char scanner)))

(defun next-token (scanner)
  (skip-spaces scanner)
  (let ((ch (next-char scanner)))
    (with-accessors ((pos car) (src cdr)) scanner
      (case ch
        ((nil) ch)
        ((#\( #\, #\)) (eat-char scanner) ch)
        ((#\")     (let ((start pos))
                     (eat-char scanner)
                     (loop
                       :for ch := (next-char scanner)
                       :while ch
                       :do (eat-char scanner)
                           (case ch
                             ((#\\)     (eat-char scanner))
                             ((#\")     (loop-finish)))
                       :finally (return (string-unescape (nsubseq src start pos))))))
        ((#\+ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (let ((start pos)
               (dot   nil)
               (exp   nil))
           (eat-char scanner)
           (loop
             :for ch := (next-char scanner)
             :while ch
             :do (case ch
                   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                    (eat-char scanner))
                   ((#\.)
                    (if dot
                        (loop-finish)
                        (setf dot t))
                    (eat-char scanner))
                   ((#\e #\E)
                    (if exp
                        (loop-finish)
                        (setf exp '+))
                    (eat-char scanner))
                   ((#\+ #\-)
                    (if (eql exp '+)
                        (setf exp t)
                        (loop-finish))
                    (eat-char scanner))
                   (otherwise
                    (loop-finish)))
             :finally (return (funcall (if (or dot exp)
                                           (function parse-double)
                                           (function parse-integer))
                                       (nsubseq src start pos))))))
        ((#\A #\a #\B #\b #\C #\c #\D #\d #\E #\e #\F #\f #\G #\g #\H
         #\h #\I #\i #\J #\j #\K #\k #\L #\l #\M #\m #\N #\n #\O #\o
         #\P #\p #\Q #\q #\R #\r #\S #\s #\T #\t #\U #\u #\V #\v #\W
         #\w #\X #\x #\Y #\y #\Z #\z)
         (let ((start pos))
           (loop
             :while (and ch (or (alphanumericp ch) (find ch "_.")))
             :do (eat-char scanner)
                 (setf ch (next-char scanner)))
           (let ((token (nsubseq src start pos)))
             (cond
               ((string-equal token "true")  '(boolean . t))
               ((string-equal token "false") '(boolean . nil))
               (t                             (cons 'symbol token))))))
        (otherwise (error 'jbe-syntax-error
                          :format-control "Unexpected character ~S found in reponse ~S"
                       :format-arguments (list ch src)))))))

(defun expect (scanner expected)
  (let ((token (next-token scanner)))
    (unless (eql token expected)
      (with-accessors ((pos car) (src cdr)) scanner
        (error 'jbe-syntax-error
               :format-control "Unexpected token ~S found in reponse ~S; expected ~S"
               :format-arguments (list token src expected))))))


(defun ensure-token (token expected)
  (flet ((e1 () (error 'jbe-syntax-error
                       :format-control "Expected a ~S, got the ~S ~A"
                       :format-arguments (list expected (type-of token) token)))
         (e2 () (error 'jbe-syntax-error
                       :format-control "Expected a ~S, got the ~S ~A"
                       :format-arguments (list expected (car token) (cdr token)))))
    (case expected
      ((symbol boolean)
       (if (and (consp token)
                (eq expected (car token)))
           (cdr token)
           (e2)))
      ((int)     (if (typep token 'int)       token                   (e1)))
      ((double)  (if (typep token 'double)    token                   (e1)))
      ((string)  (if (typep token 'string)    (string-unescape token) (e1)))
      ((special) (if (typep token 'character) token                   (e1)))
      (otherwise (cond
                   ((equal token expected) token)
                   ((consp expected)       (e2))
                   (t                      (e1)))))))

(defun eat-token (scanner expected)
  (let ((token (next-token scanner)))
    (ensure-token token expected)))

(defun parse-mouse-event (parameters type)
  (destructuring-bind (id time modifiers x y) parameters
    (make-instance 'event :type      type
                          :window    (get-window (decode-id id))
                          :time      time
                          :modifiers modifiers
                          :x         (double x)
                          :y         (double y))))

(defun parse-key-event (parameters type)
  (destructuring-bind (id time modifiers key-char key-code) parameters
    (make-instance 'event :type      type
                          :window    (get-window (decode-id id))
                          :time      time
                          :modifiers modifiers
                          :key-char  (code-char key-char)
                          :key-code  key-code)))

(defun parse-timer-event (parameters type)
  (destructuring-bind (id time) parameters
    (make-instance 'event :type type
                          :timer (get-timer (decode-id id))
                          :time time)))

(defun parse-window-event (parameters type)
  (destructuring-bind (id time) parameters
    (make-instance 'event :type type
                          :window (get-window (decode-id id))
                          :time time)))

(defun parse-action-event (parameters type)
  (destructuring-bind (id action time) parameters
    (make-instance 'event :type type
                          :window (get-window (decode-id id))
                          :time time
                          :action-command action)))

(defun parse-parameters (scanner)
  (let ((token (next-token scanner)))
    (flet ((ep () (error 'jbe-syntax-error
                         :format-control "Expected a ( or nothing, got ~S"
                         :format-arguments (list token)))
           (eu () (error 'jbe-syntax-error
                         :format-control "Expected a , or a ) got ~S"
                         :format-arguments (list token)))
           (ef () (error 'jbe-syntax-error
                         :format-control "Missing a )")))
      (case token
        ((nil) nil)
        ((#\()
         (loop :for token := (next-token scanner)
               :until (eql #\) token)
               :collect token
               :do (setf token (next-token scanner))
                   (case token
                     ((#\,))
                     ((#\)) (loop-finish))
                     ((nil) (ef))
                     (otherwise (eu)))))
        (otherwise (ep))))))

(defun parse-event (line)
  (let* ((scanner    (make-scanner line))
         (name       (ensure-token (next-token scanner) 'symbol))
         (parameters (parse-parameters scanner)))
    (scase name
           (("mousePressed")            (parse-mouse-event  parameters +MOUSE-PRESSED+))
           (("mouseReleased")           (parse-mouse-event  parameters +MOUSE-RELEASED+))
           (("mouseClicked")            (parse-mouse-event  parameters +MOUSE-CLICKED+))
           (("mouseMoved")              (parse-mouse-event  parameters +MOUSE-MOVED+))
           (("mouseDragged")            (parse-mouse-event  parameters +MOUSE-DRAGGED+))
           (("keyPressed")              (parse-key-event    parameters +KEY-PRESSED+))
           (("keyReleased")             (parse-key-event    parameters +KEY-RELEASED+))
           (("keyTyped")                (parse-key-event    parameters +KEY-TYPED+))
           (("actionPerformed")         (parse-action-event parameters +ACTION-PERFORMED+))
           (("timerTicked")             (parse-timer-event  parameters +TIMER-TICKED+))
           (("windowClosed")            (let ((e (parse-window-event parameters +WINDOW-CLOSED+)))
                                          (close-window (event-window e))
                                          e))
           (("windowResized")           (parse-window-event parameters +WINDOW-RESIZED+))
           (("lastWindowClosed")        (parse-window-event parameters +LAST-WINDOW-CLOSED+)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The JavaBackEnd Protocol.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *c*)
(defun handle-pipe-error (err)
  (setf *c* err)
  (princ err) (terpri)
  (if (and *backend*
           (or (eql (stream-error-stream err) (backend-output *backend*))
               (eql (stream-error-stream err) (backend-input *backend*)))
           (find-restart 'retry err))
      (invoke-restart 'retry)
      (error err)))

(defmacro with-jbe-pipe-error-handler (&body forms)
  (let ((continue (gensym)))
    `(loop :named ,continue
           :do (handler-bind
                   ((stream-error (function handle-pipe-error)))
                 (with-simple-restart (retry "Retry after closing and re-opening the backend")
                   (return-from ,continue (progn ,@forms))))
               (ignore-errors (close-backend))
               (open-backend))))

(defmacro generate-JBE-functions (&rest definitions)
  `(progn
     ,@(mapcar (lambda (definition)
                 (destructuring-bind (name jbe-name lambda-list &rest options)
                     definition
                   (let ((nolock      (find :nolock options))
                         (errorp      (find :error  options))
                         (result-type (first (remove :error (remove :nolock options))))
                         (parameters  (mapcar (function first) lambda-list)))
                     `(defun ,name ,parameters
                        (declare (stepper disable))
                        (let ,(mapcar (lambda (parameter)
                                        (destructuring-bind (name type) parameter
                                          `(,name ,(ecase type
                                                     (id         `(gid (id ,name)))
                                                     (string     `(string-escape (string ,name)))
                                                     (boolean    `(cond (,name "true")
                                                                        (t "false")))
                                                     (int        `(round ,name))
                                                     (double     `(encode-double (coerce ,name 'double)))))))
                               lambda-list)
                          (with-jbe-pipe-error-handler
                            ,@(let ((body `((send ,jbe-name ,@parameters)
                                            ,@(when errorp
                                                '((get-error)))
                                            ,@(ecase result-type
                                                ((nil)       '((values)))
                                                ((string)    '((get-result)))
                                                ((boolean)   '((get-boolean)))
                                                ((int)       '((get-int)))
                                                ((double)    '((get-double)))
                                                ((dimension) '((get-dimension)))
                                                ((rectangle) '((get-rectangle)))))))
                                (if nolock
                                    body
                                    `((with-backend-locked *backend*
                                        ,@body))))))))))
               definitions)))

(generate-JBE-functions
 (file.open-file-dialog         "File.openFileDialog"          ((title string) (mode string) (path string))              string)
 (3drect.create                 "G3DRect.create"               ((id id) (width double) (height double) (raised boolean)))
 (3drect.set-raised             "G3DRect.setRaised"            ((id id) (raised boolean)))
 (arc.create                    "GArc.create"                  ((id id) (width double) (height double) (start double) (sweep double)))
 (arc.set-frame-rectangle       "GArc.setFrameRectangle"       ((id id) (x double) (y double) (width double) (height double)))
 (arc.set-start-angle           "GArc.setStartAngle"           ((id id) (angle double)))
 (arc.set-sweep-angle           "GArc.setSweepAngle"           ((id id) (angle double)))
 (button.create                 "GButton.create"               ((id id) (label string)))
 (check-box.create              "GCheckBox.create"             ((id id) (label string)))
 (check-box.is-selected         "GCheckBox.isSelected"         ((id id))                                                 boolean)
 (check-box.set-selected        "GCheckBox.setSelected"        ((id id) (selected boolean)))
 (compound.add                  "GCompound.add"                ((top-compound id) (compound id)))
 (compound.create               "GCompound.create"             ((id id)))
 (event.get-next-event          "GEvent.getNextEvent"          ((mask int))                                              :nolock)
 (event.wait-for-event          "GEvent.waitForEvent"          ((mask int))                                              :nolock)
 (image.create                  "GImage.create"                ((id id) (filename string))                               dimension)
 (interactor.set-action-command "GInteractor.setActionCommand" ((id id) (cmd string)))
 (interactor.get-size           "GInteractor.getSize"          ((id id))                                                 dimension)
 (label.create                  "GLabel.create"                ((id id) (str string)))
 (label.get-font-ascent         "GLabel.getFontAscent"         ((id id))                                                 double)
 (label.get-font-descent        "GLabel.getFontDescent"        ((id id))                                                 double)
 (label.get-size                "GLabel.getGLabelSize"         ((id id))                                                 dimension)
 (label.set-font                "GLabel.setFont"               ((id id) (font string)))
 (label.set-label               "GLabel.setLabel"              ((id id) (str string)))
 (line.create                   "GLine.create"                 ((id id) (x1 double) (y1 double) (x2 double) (y2 double)))
 (line.set-end-point            "GLine.setEndPoint"            ((id id) (x double) (y double)))
 (line.set-start-point          "GLine.setStartPoint"          ((id id) (x double) (y double)))
 (object.contains               "GObject.contains"             ((id id) (x double) (y double)) boolean)
 (object.delete                 "GObject.delete"               ((id id)))
 (object.get-bounds             "GObject.getBounds"            ((id id))                                                 rectangle)
 (object.remove                 "GObject.remove"               ((id id) (object id)))
 (object.rotate                 "GObject.rotate"               ((id id) (theta double)))
 (object.scale                  "GObject.scale"                ((id id) (sx double) (sy double)))
 (object.send-backward          "GObject.sendBackward"         ((id id)))
 (object.send-forward           "GObject.sendForward"          ((id id)))
 (object.send-to-back           "GObject.sendToBack"           ((id id)))
 (object.send-to-front          "GObject.sendToFront"          ((id id)))
 (object.set-color              "GObject.setColor"             ((id id) (color string)))
 (object.set-fill-color         "GObject.setFillColor"         ((id id) (color string)))
 (object.set-filled             "GObject.setFilled"            ((id id) (filled boolean)))
 (object.set-line-width         "GObject.setLineWidth"         ((id id) (line-width double)))
 (object.set-location           "GObject.setLocation"          ((id id) (x double) (y double)))
 (object.set-size               "GObject.setSize"              ((id id) (width double) (height double)))
 (object.set-visible            "GObject.setVisible"           ((id id) (visible boolean)))
 (oval.create                   "GOval.create"                 ((id id) (width double) (height double)))
 (polygon.add-vertex            "GPolygon.addVertex"           ((id id) (x double) (y double)))
 (polygon.create                "GPolygon.create"              ((id id)))
 (rect.create                   "GRect.create"                 ((id id) (width double) (height double)))
 (round-rect.create             "GRoundRect.create"            ((id id) (width double) (height double) (arc double)))
 (slider.create                 "GSlider.create"               ((id id) (min int) (max int) (value int)))
 (slider.get-value              "GSlider.getValue"             ((id id))                                                 int)
 (slider.set-value              "GSlider.setValue"             ((id id) (value int)))
 (text-field.create             "GTextField.create"            ((id id) (nchars int)))
 (text-field.get-text           "GTextField.getText"           ((id id))                                                 string)
 (text-field.set-text           "GTextField.setText"           ((id id) (str string)))
 (chooser.create                "GChooser.create"              ((id id)))
 (chooser.add-item              "GChooser.addItem"             ((id id) (item string)))
 (chooser.get-selected-item     "GChooser.getSelectedItem"     ((id id))                                                 string)
 (chooser.set-selected-item     "GChooser.setSelectedItem"     ((id id) (item string)))
 (timer.create                  "GTimer.create"                ((id id) (msec double)))
 (timer.delete                  "GTimer.deleteTimer"           ((id id)))
 (timer.pause                   "GTimer.pause"                 ((milliseconds double))                                   :error)
 (timer.start                   "GTimer.startTimer"            ((id id)))
 (timer.stop                    "GTimer.stopTimer"             ((id id)))
 (window.add-to-region          "GWindow.addToRegion"          ((window id) (object id) (region id)))
 (window.set-region-alignment   "GWindow.setRegionAlignment"   ((id id) (region string) (align string)))
 (window.clear                  "GWindow.clear"                ((id id)))
 (window.close                  "GWindow.close"                ((id id)))
 (window.create                 "GWindow.create"               ((id id) (width int) (height int) (top id))               :error)
 (window.delete                 "GWindow.delete"               ((id id)))
 (window.draw                   "GWindow.draw"                 ((id id) (object id)))
 (window.exit-graphics          "GWindow.exitGraphics"         () :nolock)
 (window.get-screen-height      "GWindow.getScreenHeight"      ()                                                        double)
 (window.get-screen-width       "GWindow.getScreenWidth"       ()                                                        double)
 (window.repaint                "GWindow.repaint"              ((id id)))
 (window.request-focus          "GWindow.requestFocus"         ((id id)))
 (window.set-resizable          "GWindow.setResizable"         ((id id) (resizable boolean)))
 (window.set-title              "GWindow.setTitle"             ((id id) (title string)))
 (window.set-visible            "GWindow.setVisible"           ((id id) (visible boolean)))
 (top-compound.create           "TopCompound.create"           ((id id)))
 (console.clear                 "JBEConsole.clear"             ())
 (console.get-line              "JBEConsole.getLine"           ()                                                        string)
 (console.print                 "JBEConsole.print"             ((str string)))
 (console.println               "JBEConsole.println"           ())
 (console.set-font              "JBEConsole.setFont"           ((font string)))
 (console.set-size              "JBEConsole.setSize"           ((width int) (height int)))
 (sound.create                  "Sound.create"                 ((id id) (filename string))                               :error)
 (sound.delete                  "Sound.delete"                 ((id id)))
 (sound.play                    "Sound.play"                   ((id id))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLOS API
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *colors*
  '("BLACK" "DARK_GRAY" "GRAY" "LIGHT_GRAY" "WHITE" "RED" "YELLOW"
    "GREEN" "CYAN" "BLUE" "MAGENTA" "ORANGE" "PINK")
  "The list of possible colors.")

(defparameter *black*      "BLACK")
(defparameter *dark-gray*  "DARK_GRAY")
(defparameter *gray*       "GRAY")
(defparameter *light-gray* "LIGHT_GRAY")
(defparameter *white*      "WHITE")
(defparameter *red*        "RED")
(defparameter *yellow*     "YELLOW")
(defparameter *green*      "GREEN")
(defparameter *cyan*       "CYAN")
(defparameter *blue*       "BLUE")
(defparameter *magenta*    "MAGENTA")
(defparameter *orange*     "ORANGE")
(defparameter *pink*       "PINK")

(defun screen-width ()
  "The width of the screen in pixels."
  (window.get-screen-width))

(defun screen-height ()
  "The height of the screen in pixels."
  (window.get-screen-height))

(defun pause (milliseconds)
  "Suspends execution for the time given in MILLISECONDS."
  (timer.pause milliseconds))


;;;----------------------------------------------------------------------------------------

(defvar *last-object-id* 0)

(defclass timer (sloted-object)
  ((id          :initform (incf *last-object-id*)
                :type integer
                :reader id)
   (duration-ms :initarg :duration-ms
                :initform 0.0d0
                :type 'double
                :reader duration-ms)))

(defmethod slots-for-print append ((self timer))
  (extract-slots self '(duration-ms)))

(defmethod initialize-instance :before ((self timer) &key &allow-other-keys)
  (declare (stepper disable))
  (open-backend))

(defmethod initialize-instance :after ((self timer) &key &allow-other-keys)
  (declare (stepper disable))
  (timer.create self (duration-ms self))
  (register self))

(defgeneric start-timer (self)
  (:method   ((self timer))
    (timer.start self)))

(defgeneric stop-timer (self)
  (:method   ((self timer))
    (timer.stop self)))

;;;----------------------------------------------------------------------------------------


(defclass object (sloted-object)
  ((id         :initform (incf *last-object-id*)      :type integer          :reader id)
   (x          :initarg :x          :initform 0.0d0   :type double           :reader x)
   (y          :initarg :y          :initform 0.0d0   :type double           :reader y)
   (width      :initarg :width      :initform 0.0d0   :type double           :reader width)
   (height     :initarg :height     :initform 0.0d0   :type double           :reader height)
   (color      :initarg :color      :initform *black* :type string           :reader color)
   (fill-color :initarg :fill-color :initform *black* :type string           :reader fill-color)
   (line-width :initarg :line-width :initform 1.0d0   :type double           :reader line-width)
   (filled     :initarg :filled     :initform nil     :type boolean          :reader filled)
   (visible    :initarg :visible    :initform t       :type boolean          :reader visible)))

(defun %set-fillable-attributes (object)
  "To be called by subclasses."
  (object.set-location   object (x object) (y object))
  (object.set-color      object (color      object))
  (object.set-fill-color object (fill-color object))
  (object.set-filled     object (filled     object))
  (object.set-line-width object (line-width object))
  (object.set-visible    object (visible    object)))

(defun %set-object-attributes (object)
  "To be called by subclasses."
  (object.set-location   object (x object) (y object))
  (object.set-color      object (color      object))
  (object.set-line-width object (line-width object))
  (object.set-visible    object (visible    object)))

(defmethod slots-for-print append ((self object))
  (extract-slots self '(id x y width height color fill-color filled visible)))

(defmethod print-object ((self object) stream)
  (declare (stepper disable))
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~{~S~^ ~}" (slots-for-print self)))
  self)

(defmethod initialize-instance :before ((self object) &key &allow-other-keys)
  (declare (stepper disable))
  (open-backend))

(defmethod initialize-instance ((self object) &rest keys &key x y width height &allow-other-keys)
  (declare (stepper disable))
  (apply (function call-next-method) self
         (append (when x (list :x (double x)))
                 (when y (list :y (double y)))
                 (when width (list :width (double width)))
                 (when height (list :height (double height)))
                 keys)))


(defmethod set-location ((self object) x y)
  (setf (slot-value self 'x) (double x)
        (slot-value self 'y) (double y))
  (object.set-location self (double x) (double y)))
(defmethod location ((self object))
  (make-point :x (x self) :y (y self)))
(defmethod (setf location) (point (self object))
  (set-location self (x point) (y point)))

(defmethod set-size ((self object) width height)
  (setf (slot-value self 'width)  (double width)
        (slot-value self 'height) (double height))
  (object.set-size self (double width) (double height)))
(defmethod size ((self object))
  (make-dimension :width (width self)
                  :height (height self)))
(defmethod (setf size) ((size dimension) (self object))
  (set-size self (dimension-width size) (dimension-height size)))

(defmethod set-color ((self object) color)
  (setf (slot-value self 'color) (string color))
  (object.set-color self  (slot-value self 'color)))
(defmethod (setf color) (color (self object))
  (set-color self color))

(defmethod set-fill-color ((self object) color)
  (setf (slot-value self 'fill-color) (string color))
  (object.set-fill-color self  (slot-value self 'fill-color)))
(defmethod (setf fill-color) (color (self object))
  (set-fill-color self color))

(defmethod set-line-width ((self object) line-width)
  (setf (slot-value self 'line-width) line-width)
  (object.set-line-width self line-width))
(defmethod (setf line-width) (line-width (self object))
  (set-line-width self line-width))

(defmethod set-visible ((self object) visible)
  (setf (slot-value self 'visible) visible)
  (object.set-visible self visible))
(defmethod (setf visible) (visible (self object))
  (set-visible self visible))

(defmethod set-filled ((self object) filled)
  (setf (slot-value self 'filled) filled)
  (object.set-filled self filled))
(defmethod (setf filled) (filled (self object))
  (set-filled self filled))

(defgeneric rotate (self theta)
  (:method ((self object) theta)
    (object.rotate self (double theta))))

(defgeneric scale (self sx sy)
  (:method ((self object) sx sy)
    (object.scale self (double sx) (double sy))))

(defgeneric send-backward (self)
  (:method ((self object))
    (object.send-backward self)))
(defgeneric send-forward (self)
  (:method ((self object))
    (object.send-forward self)))
(defgeneric send-to-back (self)
  (:method ((self object))
    (object.send-to-back self)))
(defgeneric send-to-front (self)
  (:method ((self object))
    (object.send-to-front self)))


(defgeneric contains (object x y)
  (:method ((self object) x y)
    (let* ((x0 (x self))
           (y0 (y self))
           (x1 (+ x0 (width  self)))
           (y1 (+ y0 (height self))))
      (and (<= x0 x x1) (<= y0 y y1)))))

(defgeneric bounds (self)
  (:method ((self object))
    (frame-rectangle self)))

(defgeneric frame-rectangle (self)
  (:method ((self object))
    (make-rectangle :x (x self)
                    :y (y self)
                    :width  (width  self)
                    :height (height self))))

(defgeneric set-frame-rectangle (self x y width height)
  (:method ((self object) x y width height)
    (object.set-location self (double x) (double y))
    (object.set-size self (double width) (double height))))

(defgeneric (setf frame-rectangle) (new-rect self)
  (:method (new-rect (self object))
    (set-frame-rectangle self
                         (x new-rect)
                         (y new-rect)
                         (width new-rect)
                         (height new-rect))))

;;;----------------------------------------------------------------------------------------

(defvar *default-label-font* "Dialog-13" "The default font for labels.")
(defvar *default-corner* 10.0d0 "The default corner radius for round-rects.")
(defvar *arc-tolerance*  2.5d0  "The tolerance in pixel to detect points on arcs.")
(defvar *line-tolerance* 1.5d0  "The tolerance in pixel to detect points on lines.")

(declaim (inline square degree-to-radian radian-to-degree cos-degree sin-degree))
(defun square (x)
  "Returns the square of the argument."
  (* x x))
(defun degree-to-radian (angle)
  "Convert the ANGLE given in degrees to radians."
  (/ (* angle pi) 180.0d0))
(defun radian-to-degree (angle)
  "Converts the ANGLE given in radians to degrees."
  (/ (* angle 180.0d0) pi))
(defun cos-degree (angle)
  "Computes the cosinus of the ANGLE given in degrees."
  (cos (degree-to-radian angle)))
(defun sin-degree (angle)
  "Computes the sinus of the ANGLE given in degrees."
  (sin (degree-to-radian angle)))


(defun adjustable-vector (&key size initial-contents key)
  (when (and size initial-contents)
    (assert (<= (length initial-contents) size)))
  (if initial-contents
      (let* ((len (length initial-contents))
             (size (or size len)))
        (if (or (null key)
                (eql key 'identity)
                (eql key #'identity))
            (make-array size :fill-pointer len
                             :initial-contents initial-contents
                             :adjustable t)
            (map-into (make-array size :fill-pointer len
                                       :initial-element nil
                                       :adjustable t)
                      key initial-contents)))
      (make-array (or size 8) :fill-pointer 0
                              :initial-element nil
                              :adjustable t)))

;;;----------------------------------------------------------------------------------------

(defclass rect (object)
  ())

(defmethod initialize-instance :after ((self rect) &key &allow-other-keys)
  (declare (stepper disable))
  (rect.create self (width self) (height self))
  (%set-fillable-attributes self))

;; 3drect and round-rect are not subclasses of rect to avoid creating
;; a rect when we create a 3drect or round-rect.

;;;----------------------------------------------------------------------------------------

(defclass 3drect (object)
  ((raised :initarg :raised :type boolean   :reader raised)))

(defmethod slots-for-print append ((self 3drect))
  (extract-slots self '(raised)))

(defmethod initialize-instance :after ((self 3drect) &key &allow-other-keys)
  (declare (stepper disable))
  (3drect.create self (width self) (height self) (raised self))
  (%set-fillable-attributes self))

(defmethod (setf raised) (new-value (self 3drect))
  (setf (slot-value self 'raised) new-value)
  (3drect.set-raised self new-value))

;;;----------------------------------------------------------------------------------------

(defclass round-rect (object)
  ((corner :initarg :corner :initform *default-corner* :type double :reader corner)))

(defmethod slots-for-print append ((self round-rect))
  (extract-slots self '(corner)))

(defmethod initialize-instance :after ((self round-rect) &key &allow-other-keys)
  (declare (stepper disable))
  (round-rect.create self (width self) (height self) (corner self))
  (%set-fillable-attributes self))

;;;----------------------------------------------------------------------------------------

(defclass oval (object)
  ())

(defmethod initialize-instance :after ((self oval) &key &allow-other-keys)
  (declare (stepper disable))
  (oval.create self (width self) (height self))
  (%set-fillable-attributes self))

(defmethod contains ((self oval) x y)
  (let ((rx (/ (width self) 2))
        (ry (/ (height self) 2)))
    (if (or (zerop rx) (zerop ry))
        nil
        (let ((dx (- x (x self) rx))
              (dy (- y (y self) ry)))
          (<= (+ (/ (square dx) (square rx))
                 (/ (square dy) (square ry)))
              1.0d0)))))

;;;----------------------------------------------------------------------------------------

(defclass line (object)
  ())

(defmethod initialize-instance :after ((self line) &key (x0 0 x0p) (y0 0 y0p) (x1 0 x1p) (y1 0 y1p) &allow-other-keys)
  (declare (stepper disable))
  (let* ((x0 (double (if x0p x0 (x self))))
         (y0 (double (if y0p y0 (y self))))
         (x1 (double (if x1p x1 (+ x0 (width  self)))))
         (y1 (double (if y1p y1 (+ y0 (height self))))))
    (when x0p (setf (slot-value self 'x) x0))
    (when y0p (setf (slot-value self 'y) y0))
    (when x1p (setf (slot-value self 'width) (- x1 x0)))
    (when y1p (setf (slot-value self 'height) (- y1 y0)))
    (line.create self x0 y0 x1 y1)
    (%set-object-attributes self)))

(defgeneric start-point (self)
  (:method ((self line))
    (let* ((x0 (x self))
           (y0 (y self)))
      (make-point :x x0 :y y0))))

(defgeneric end-point (self)
  (:method ((self line))
    (let* ((x0 (x self))
           (y0 (y self))
           (x1 (+ x0 (width  self)))
           (y1 (+ y0 (height self))))
      (make-point :x x1 :y y1))))

(defgeneric set-start-point (self x y)
  (:method ((self line) x y)
    (setf (slot-value self 'x) (double x)
          (slot-value self 'y) (double y))
    (line.set-start-point self
                          (slot-value self 'x)
                          (slot-value self 'y))))

(defgeneric set-end-point (self x y)
  (:method ((self line) x y)
    (setf (slot-value self 'width)  (double (- x (x self)))
          (slot-value self 'height) (double (- y (y self))))
    (line.set-end-point self (double x) (double y))))

(defgeneric (setf start-point) (new-point self)
  (:method (new-point (self line))
    (set-start-point self (x new-point) (y new-point))))

(defgeneric (setf end-point) (new-point self)
  (:method (new-point (self line))
    (set-end-point self (x new-point) (y new-point))))

(declaim (inline dsq))
(defun dsq (x0 y0 x1 y1)
  (+ (square (- x0 x1)) (square (- y0 y1))))

(defmethod contains ((self line) x y)
  (let* ((x  (double x))
         (y  (double y))
         (x0 (x self))
         (y0 (y self))
         (x1 (+ x0 (width  self)))
         (y1 (+ y0 (height self)))
         (tsq (square *line-tolerance*)))
    (cond
      ((< (dsq x y x0 y0) tsq) t)
      ((< (dsq x y x1 y1) tsq) t)
      ((< x (- (min x0 x1) *line-tolerance*)) nil)
      ((> x (+ (max x0 x1) *line-tolerance*)) nil)
      ((< y (- (min y0 y1) *line-tolerance*)) nil)
      ((> y (+ (max y0 y1) *line-tolerance*)) nil)
      ((and (= x0 x1) (= y0 y1)) nil)
      (t (let ((u (/ (+ (* (- x x0) (- x1 x0))
                        (* (- y y0) (- y1 y0)))
                     (dsq x0 y0 x1 y1))))
           (< (dsq x y (+ x0 (* u (- x1 x0))) (+ y0 (* u (- y1 y0)))) tsq))))))

;;;----------------------------------------------------------------------------------------

(defclass arc (object)
  ((start :initarg :start :type double :reader start)
   (sweep :initarg :sweep :type double :reader sweep)))

(defmethod slots-for-print append ((self arc))
  (extract-slots self '(start sweep)))

(defmethod initialize-instance :after ((self arc) &key &allow-other-keys)
  (declare (stepper disable))
  (arc.create self (width self) (height self)
              (start self) (sweep self))
  (%set-fillable-attributes self))

(defmethod initialize-instance ((self arc) &rest keys &key start sweep &allow-other-keys)
  (declare (stepper disable))
  (apply (function call-next-method) self
         (append (when start (list :start (double start)))
                 (when sweep (list :sweep (double sweep)))
                 keys)))

(defgeneric (setf start) (start self)
  (:method (start (self arc))
    (arc.set-start-angle self start)))

(defgeneric (setf sweep) (sweep self)
  (:method (sweep (self arc))
    (arc.set-sweep-angle self sweep)))

(defmethod set-frame-rectangle ((self arc) x y width height)
  (arc.set-frame-rectangle self (double x) (double y) (double width) (double height)))

(defmethod contains-angle ((self arc) theta)
  (let ((start (min (start self)
                    (+ (start self) (sweep self))))
        (sweep (abs (sweep self)))
        (turn  360.0d0))
    (or (<= turn sweep)
        (let ((theta (if (minusp theta)
                         (- turn (mod (- theta) turn))
                         (mod theta turn)))
              (start (if (minusp start)
                         (- turn (mod (- start) turn))
                         (mod start turn))))
          (if (< turn (+ start sweep))
              (or (<= start theta) (<= theta (+ start sweep (- turn))))
              (and (<= start theta) (<= theta (+ start sweep))))))))

(defmethod contains ((self arc) x y)
  (let ((rx (/ (width self) 2))
        (ry (/ (height self) 2)))
    (if (or (zerop rx) (zerop ry))
        nil
        (let* ((dx (- x (x self) rx))
               (dy (- y (y self) ry))
               (r  (+ (/ (square dx) (square rx))
                      (/ (square dy) (square ry)))))
          (when (if (filled self)
                    (< 1.0d0 r)
                    (let ((tt (/ *arc-tolerance* (/ (+ rx ry) 2))))
                      (< tt (abs (- 1.0d0 r)))))
            (contains-angle self (radian-to-degree (atan (- dy) dx))))))))

(defmethod bounds ((self arc))
  (let* ((rx (/ (width  self) 2))
         (ry (/ (height self) 2))
         (cx (+ (x self) rx))
         (cy (+ (y self) ry))
         (p1x (+ cx (* rx (cos-degree (start self)))))
         (p1y (+ cy (* ry (sin-degree (start self)))))
         (p2x (+ cx (* rx (cos-degree (+ (start self) (sweep self))))))
         (p2y (+ cy (* ry (sin-degree (+ (start self) (sweep self))))))
         (xmin (min p1x p2x))
         (xmax (max p1x p2x))
         (ymin (min p1y p2y))
         (ymax (max p1y p2y)))
    (when (contains-angle self   0.0d0) (setf xmax (+ cx rx)))
    (when (contains-angle self  90.0d0) (setf ymin (- cy ry)))
    (when (contains-angle self 180.0d0) (setf xmin (- cx rx)))
    (when (contains-angle self 270.0d0) (setf ymax (+ cy ry)))
    (when (filled self)
      (setf xmin (min xmin cx)
            ymin (min ymin cy)
            xmax (max xmax cx)
            ymax (max ymax cy)))
    (make-rectangle :x xmin :y ymin :width (- xmax xmin) :height (- ymax ymin))))

;;;----------------------------------------------------------------------------------------

(defclass polygon (object)
  ((vertices :initform (adjustable-vector) :type vector :reader vertices)
   (cx       :initform 0.0d0               :type double :reader cx)
   (cy       :initform 0.0d0               :type double :reader cy)))

(defmethod slots-for-print append ((self polygon))
  (extract-slots self '(cx cy vertices)))

(defmethod initialize-instance :after ((self polygon) &key vertices &allow-other-keys)
  (declare (stepper disable))
  (polygon.create self)
  (when vertices (%set-vertices self vertices))
  (%set-fillable-attributes self))

(defun %set-vertices (poly vertices)
  (setf (slot-value poly 'vertices)
        (if (and (every (function realp) vertices)
                 (evenp (length vertices)))
            (adjustable-vector :initial-contents
                               (if (listp vertices)
                                   (loop :for (x y) :on vertices :by (function cddr)
                                         :collect (make-point :x (double x)
                                                              :y (double y)))
                                   (loop :for x :from 0
                                         :for y :from 1 :below (length vertices)
                                         :collect (make-point :x (double x)
                                                              :y (double y)))))
            (adjustable-vector :initial-contents vertices
                               :key (lambda (item)
                                      (etypecase item
                                        (point item)
                                        (sequence (make-point :x (double (elt item 0))
                                                              :y (double (elt item 1)))))))))
  (let ((vertices (slot-value poly 'vertices)))
    (loop :for p :across vertices
          :do (polygon.add-vertex poly (x p) (y p)) )
    (let ((maxi (1- (length vertices))))
      (if (minusp maxi)
          (setf (slot-value poly 'cx) 0.0d0
                (slot-value poly 'cy) 0.0d0)
          (setf (slot-value poly 'cx) (x (aref vertices maxi))
                (slot-value poly 'cy) (y (aref vertices maxi)))))))

(defmethod add-vertex ((self polygon) x y)
  (setf (slot-value self 'cx) (double x)
        (slot-value self 'cy) (double y))
  (vector-push-extend (make-point :x (double x) :y (double y))
                      (vertices self))
  (polygon.add-vertex self (double x) (double y)))

(defmethod add-vertices ((self polygon) vertices)
  (map nil (lambda (vertex) (add-vertex self (x vertex) (y vertex))) vertices))

(defmethod add-edge ((self polygon) dx dy)
  (add-vertex self (+ dx (cx self))  (+ dy (cy self))))

(defmethod add-polar-edge ((self polygon) r theta)
  (add-vertex self (* r (cos-degree theta)) (* r (sin-degree theta))))

(defmethod bounds ((self polygon))
  (let* ((vertices (vertices self))
         (n (length vertices)))
    (if (zerop n)
        (make-rectangle)
        (let ((xmin (x (aref vertices 0)))
              (ymin (y (aref vertices 0)))
              (xmax (x (aref vertices 0)))
              (ymax (y (aref vertices 0))))
          (loop :for pt :across vertices
                :for x := (x pt)
                :for y := (y pt)
                :do (setf xmin (min x xmin)
                          xmax (max x xmax)
                          ymin (min y ymin)
                          ymax (max y ymax)))
          (make-rectangle :x xmin :y ymin
                          :width (- xmax xmin)
                          :height (- ymax ymin))))))

(defmethod contains ((self polygon) x y)
  (let* ((vertices (vertices self))
         (n (length vertices))
         (crossings 0))
    (if (< n 2)
        nil
        (let* ((p0 (aref vertices 0))
               (x0 (x p0))
               (y0 (y p0))
               (p1 (aref vertices (1- n)))
               (x1 (x p1))
               (y1 (y p1)))
          (when (and (= x0 x1) (= y0 y1)) (decf n))
          (loop :for i :from 1 :to n
                :for p1 := (aref vertices (mod i n))
                :for x1 := (x p1)
                :for y1 := (y p1)
                :do (when (and (not (eq (not (< y y0)) (not (< y y1))))
                               (< (- x x0)
                                  (/ (* (- x1 x0) (- y y0))
                                     (- y1 y0))))
                      (incf crossings))
                    (setf x0 x1
                          y0 y1))
          (= 1 (mod crossings 2))))))


;;;----------------------------------------------------------------------------------------

(defclass compound-mixin ()
  ((components :initform (adjustable-vector)
               :type vector
               :reader components))
  (:documentation "
The TOP-COMPOUND must be initialized by a different JBE message than
normal COMPOUND.  THEREFORE those classes mustn't share an
INITIALIZE-INSTANCE in a common superclass.  Hence we implement them
in teh compound-mixin and have both TOP-COMPOUND and COMPOUND inherit
from OBJECT and COMPOUND-MIXIN.
"))

(defmethod slots-for-print append ((self compound-mixin))
  (extract-slots self '(components)))

(defgeneric compound-add (self other)
  (:method   ((self compound-mixin) other)
    (vector-push-extend other (components self))
    (compound.add self other)
    (set-location other (x other) (y other))))

(defgeneric compound-remove (self other)
  (:method   ((self compound-mixin) other)
    (setf (slot-value self 'components) (delete other (components self) :count 1))
    (object.remove self other)))

(defgeneric get-object-at (self x y))
(defmethod get-object-at ((self compound-mixin) x y)
  (find-if (lambda (object) (contains object x y))
           (components self)))

(defun %set-components (compound components)
  (setf (slot-value compound 'components) (adjustable-vector :initial-contents components))
  (loop :for component :across (slot-value compound 'components)
        :do (compound.add compound component)
            (set-location component (x component) (y component))))

;;;----------------------------------------------------------------------------------------

(defclass compound (object compound-mixin)
  ())

(defmethod initialize-instance :after ((self compound) &key components &allow-other-keys)
  (declare (stepper disable))
  (compound.create self)
  (%set-object-attributes self)
  (when components (%set-components self components)))

;;;----------------------------------------------------------------------------------------

(defclass top-compound (object compound-mixin)
  ())

(defmethod print-object ((self top-compound) stream)
  (declare (stepper disable))
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~{~S~^ ~}" (list :components (length (components self)))))
  self)

(defmethod initialize-instance :after ((self top-compound) &key components &allow-other-keys)
  (declare (stepper disable))
  (top-compound.create self)
  (%set-object-attributes self)
  (when components (%set-components self components)))

;;;----------------------------------------------------------------------------------------

(defclass label (object)
  ((font    :initarg :font    :initform *default-label-font* :type string :reader font)
   (text    :initarg :text    :type string :reader text)
   (ascent  :initarg :ascent  :type double :reader ascent)
   (descent :initarg :descent :type double :reader descent)))

(defmethod slots-for-print append ((self label))
  (extract-slots self '(font ascent descent text)))

(defmethod initialize-instance :after ((self label) &key &allow-other-keys)
  (declare (stepper disable))
  (label.create self (text self))
  (%set-fillable-attributes self)
  (set-font self (font self))
  (set-text self (text self)))

(defmethod bounds ((self label))
  (make-rectangle :x (x self)
                  :y (- (y self) (ascent self))
                  :width (width self)
                  :height (height self)))

(defmethod set-font ((self label) font)
  (setf (slot-value self 'font) font)
  (label.set-font self font)
  (let ((size (label.get-size self)))
    (setf (slot-value self 'ascent)  (label.get-font-ascent  self)
          (slot-value self 'descent) (label.get-font-descent self)
          (slot-value self 'width)  (dimension-width size)
          (slot-value self 'height) (dimension-height size))))

(defmethod (setf font) (new-font (self label))
  (set-font self new-font))

(defmethod set-text ((self label) text)
  (setf (slot-value self 'text) text)
  (label.set-label self text)
  (let ((size (label.get-size self)))
    (setf (slot-value self 'width)  (dimension-width size)
          (slot-value self 'height) (dimension-height size))))

(defmethod (setf text) (new-text (self label))
  (set-text self new-text))

;;;----------------------------------------------------------------------------------------

(defclass image (object)
  ((filename :initarg :filename :type string :reader filename)))

(defmethod slots-for-print append ((self image))
  (extract-slots self '(filename)))

(defmethod initialize-instance :after ((self image) &key &allow-other-keys)
  (unless (probe-file (filename self))
    (error 'file-error :pathname (filename self)))
  (let ((size (image.create self (filename self))))
    (setf (slot-value self 'width)  (dimension-width size)
          (slot-value self 'height) (dimension-height size))
    (%set-object-attributes self)))

;;;----------------------------------------------------------------------------------------

(defclass interactor (object)
  ((action-command :initarg :action-command :initform "" :type string :reader action-command)
   (label          :initarg :label          :initform "" :type string :reader label)))

(defmethod initialize-instance :after ((self interactor) &key &allow-other-keys)
  (declare (stepper disable))
  (register self))

(defmethod slots-for-print append ((self interactor))
  (extract-slots self '(action-command label)))

(defgeneric set-action-command (self command)
  (:method   ((self interactor) command)
    (setf (slot-value self 'action-command) command)
    (interactor.set-action-command self command)))

(defgeneric (setf action-command) (new-command self)
  (:method   (new-command (self interactor))
    (set-action-command self new-command)))

(defgeneric get-size (self)
  (:method   ((self interactor))
    (interactor.get-size self)))

;;;----------------------------------------------------------------------------------------

(defclass button (interactor)
  ())

(defmethod initialize-instance :after ((self button) &key &allow-other-keys)
  (declare (stepper disable))
  (button.create self (label self))
  (setf (action-command self) (action-command self))
  (%set-fillable-attributes self))

;;;----------------------------------------------------------------------------------------

(defclass check-box (interactor)
  ())

(defmethod initialize-instance :after ((self check-box) &key &allow-other-keys)
  (declare (stepper disable))
  (check-box.create self (label self))
  (setf (action-command self) (action-command self))
  (%set-fillable-attributes self))

(defmethod selected ((self check-box))
  (check-box.is-selected self))
(defmethod set-selected ((self check-box) selected)
  (check-box.set-selected self selected))
(defmethod (setf selected) (selected (self check-box))
  (set-selected self selected))

;;;----------------------------------------------------------------------------------------

(defclass slider (interactor)
  ((minimum :initarg :min   :type int :reader minimum)
   (maximum :initarg :max   :type int :reader maximum)
   (value   :initarg :value :type int)))

(defmethod slots-for-print append ((self slider))
  (extract-slots self '(minimum value maximum)))

(defmethod initialize-instance :after ((self slider) &key &allow-other-keys)
  (declare (stepper disable))
  (slider.create self (minimum self) (maximum self) (slot-value self 'value))
  ;; Side effect: sends action-command to the backend slider.
  (setf (action-command self) (action-command self))
  (%set-fillable-attributes self))

(defmethod value ((self slider))
  (setf (slot-value self 'value) (slider.get-value self)))
(defmethod set-value ((self slider) value)
  (setf (slot-value self 'value) value)
  (slider.set-value self value))
(defmethod (setf value) (value (self slider))
  (set-value self value))

;;;----------------------------------------------------------------------------------------

(defclass text-field (interactor)
  ((nchars :initarg :nchars :type int :reader nchars)))

(defmethod slots-for-print append ((self text-field))
  (extract-slots self '(nchars)))

(defmethod initialize-instance :after ((self text-field) &key &allow-other-keys)
  (declare (stepper disable))
  (text-field.create self (nchars self))
  ;; Side effect: sends action-command to the backend slider.
  (setf (action-command self) (action-command self))
  (%set-fillable-attributes self))

(defmethod text ((self text-field))
  (text-field.get-text self))
(defmethod set-text ((self text-field) str)
  (text-field.set-text self str))
(defmethod (setf text) (str (self text-field))
  (set-text self str))


;;;----------------------------------------------------------------------------------------

(defclass chooser (interactor)
  ((items         :initarg :items
                  :initform (adjustable-vector)
                  :type vector
                  :reader items)
   (selected-item :initarg :selected
                  :type string
                  :reader selected-item)))

(defmethod slots-for-print append ((self chooser))
  (extract-slots self '(items selected-item)))

(defmethod initialize-instance :after ((self chooser) &key items &allow-other-keys)
  (declare (stepper disable))
  (chooser.create self)
  (setf (action-command self) (action-command self))
  (when items
    (assert (every (function stringp) items))
    (setf (slot-value self 'items) (adjustable-vector :initial-contents items))
    (loop :for item :across (slot-value self 'items)
          :do (chooser.add-item self item)))
  (%set-fillable-attributes self))

(defmethod add-item ((self chooser) item)
  (vector-push-extend item (slot-value self 'items))
  (chooser.add-item self item))

(defmethod selected-item ((self chooser))
  (setf (slot-value self 'selected-item) (chooser.get-selected-item self)))
(defmethod set-selected-item ((self chooser) item)
  (chooser.set-selected-item self item))
(defmethod (setf selected-item) (selected  (self chooser))
  (set-selected-item self selected))


;;;----------------------------------------------------------------------------------------

(defclass window (object)
  ((color :initarg :color :initform *black*    :type string :accessor color)
   (title :initarg :title :initform "Untitled" :type string :reader   title)
   (resizable :initarg :resizable :initform nil :type boolean :reader resizable)
   (top-compound :reader  top-compound)))

(defmethod slots-for-print append ((self window))
  (extract-slots self '(title top-compound)))

(defmethod initialize-instance :after ((self window) &key components resizable &allow-other-keys)
  (declare (stepper disable))
  (setf (slot-value self 'top-compound)
        (apply (function make-instance) 'top-compound
               (when components (list :components components))))
  (window.create self (width self) (height self) (top-compound self))
  (register self)
  (set-title self (title self))
  (when resizable (set-resizable self resizable))
  (%set-object-attributes self))

(defgeneric close-window (self)
  (:method ((self window))
    (window.close self)
    (unregister self)))

(defgeneric repaint-window (self)
  (:method ((self window))
    (window.repaint self)))

(defgeneric request-focus (self)
  (:method ((self window))
    (window.request-focus self)))

(defgeneric clear-window (self)
  (:method ((self window))
    (window.clear self)))

(defmethod set-visible ((self window) visible)
  (setf (slot-value self 'visible) visible)
  (window.set-visible self visible))
(defmethod (setf visible) (visible (self window))
  (set-visible self visible))

(defmethod set-resizable ((self window) &optional (resizable t))
  (window.set-resizable self resizable))
(defmethod (setf resizable) (resizable (self window))
  (set-resizable self resizable))

(defmethod set-title ((self window) (title string))
  (setf (slot-value self 'title) title)
  (window.set-title self title))
(defmethod (setf title) ((title string) (self window))
  (set-title self title))

(defmethod draw-line ((self window) x0 y0 x1 y1)
  (window.draw self (make-instance 'line :x (double x0)
                                         :y (double y0)
                                         :width (double (- x1 x0))
                                         :height (double (- y1 y0)))))

(defmethod draw-polar-line ((self window) x y rho theta)
  (draw-line self (double x) (double y)
             (- (* (double rho) (cos-degree theta)) x)
             (- (* (double rho) (sin-degree theta)) y)))

(defmethod draw-oval ((self window) x y width height)
  (let ((obj (make-instance 'oval :x (double x)
                                  :y (double y)
                                  :width (double width)
                                  :height (double height))))
    (setf (color obj) (color self))
    (window.draw self obj)))

(defmethod fill-oval ((self window) x y width height)
  (let ((obj (make-instance 'oval :x (double x)
                                  :y (double y)
                                  :width (double width)
                                  :height (double height))))
    (setf (slot-value obj 'filled) t)
    (setf (color obj) (color self))
    (window.draw self obj)))

(defmethod draw-rect ((self window) x y width height)
  (let ((obj (make-instance 'rect :x (double x)
                                  :y (double y)
                                  :width (double width)
                                  :height (double height))))
    (setf (color obj) (color self))
    (window.draw self obj)))

(defmethod fill-rect ((self window) x y width height)
  (let ((obj (make-instance 'rect :x (double x)
                                  :y (double y)
                                  :width (double width)
                                  :height (double height))))
    (setf (slot-value obj 'filled) t)
    (setf (color obj) (color self))
    (window.draw self obj)))

(defmethod draw ((self window) (obj object))
  (window.draw self obj))

(defmethod draw-at ((self window) (obj object) x y)
  (set-location obj (double x) (double y))
  (window.draw self obj))

(defmethod components ((self window))
  (components (top-compound self)))

(defmethod compound-add ((self window) (obj object))
  (compound-add (top-compound self) obj))

(defmethod compound-add-at ((self window) (obj object) x y)
  (set-location obj (double x) (double y))
  (compound-add (top-compound self) obj))

(defmethod compound-add-to-region ((self window) (obj object) region)
  (window.add-to-region self obj region))

(defmethod compound-remove ((self window) (obj object))
  (compound-remove (top-compound self) obj))

(defmethod get-object-at ((self window) x y)
  (get-object-at (top-compound self) (double x) (double y)))

;;;-----------------------------------------------------------------------------

(defun wait-for-click ()
  (wait-for-event +click-event+))

(defun wait-for-event (mask)
  (loop :while (queue-empty-p *event-queue*)
        :do (with-jbe-pipe-error-handler
              (with-backend-locked *backend*
                (event.wait-for-event mask)
                (get-result))))
  (with-backend-locked *backend*
    (queue-dequeue *event-queue*)))

(defun get-next-event (mask)
  (when (queue-empty-p *event-queue*)
    (with-jbe-pipe-error-handler
      (with-backend-locked *backend*
        (event.get-next-event mask)
        (get-result))))
  (with-backend-locked *backend*
    (unless (queue-empty-p *event-queue*)
      (queue-dequeue *event-queue*))))

;;;-----------------------------------------------------------------------------

(defmethod register   ((self window))
  (setf (gethash (id self) *window-registry*) self))
(defmethod register   ((self interactor))
  (setf (gethash (id self) *source-registry*) self))
(defmethod register   ((self timer))
  (setf (gethash (id self) *timer-registry*) self))

(defmethod unregister   ((self window))
  (remhash (id self) *window-registry*))
(defmethod unregister   ((self interactor))
  (remhash (id self) *source-registry*))
(defmethod unregister   ((self timer))
  (remhash (id self) *timer-registry*))

;; We would use directly unregister, but let's say "free" is a better
;; name for a public API for the target audience.

(defgeneric free (self)
  (:documentation "")
  (:method ((self t))
    (declare (ignorable self))
    (values))
  (:method ((self timer))
    (unregister self)
    (values))
  (:method ((self interactor))
    (unregister self)
    (values))
  (:method ((self window))
    (unregister self)
    (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gray Stream Interface to the console.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun console-set-size (width height)
  "Sets the size of the console window.
WIDTH, HEIGHT must be coerceable to int."
  (console.set-size (max 523 (int width)) (max 342 (int height))))

(defun console-set-font (font)
  "Sets the font used for the console window.
Example:
    (console-set-font \"Monaco-13\")
"
  (console.set-font font))

(defun console-clear ()
  "Clears the console window."
  (console.clear))


;;;----------------------------------------------------------------------------------------

(defclass console-stream (fundamental-character-input-stream
                          fundamental-character-output-stream)
  ((column      :initform 0  :accessor column)
   (input-line  :initform "" :accessor input-line)
   (input-index :initform 0  :accessor input-index)))

;;; character input

(defun update-column (stream ch)
  (when (characterp ch)
    (if (char= ch #\newline)
        (setf (column stream) 0)
        (incf (column stream))))
  ch)

(defun fill-input-line (stream)
  (unless (<= (input-index stream) (length (input-line stream)))
    (setf (input-line stream) (prog1 (console.get-line)
                                (console.print ""))
          (input-index stream) 0
          (column stream) 0)))

(defun char-or-newline (stream &optional peek)
  (prog1 (if (= (input-index stream) (length (input-line stream)))
             #\Newline
             (aref (input-line stream) (input-index stream)))
    (unless peek
      (incf (input-index stream)))))

(defmethod stream-read-char ((stream console-stream))
  (fill-input-line stream)
  (update-column stream (char-or-newline stream)))

(defmethod stream-read-char-no-hang ((stream console-stream))
  (when (<= (input-index stream) (length (input-line stream)))
    (update-column stream (char-or-newline stream))))

(defmethod stream-peek-char ((stream console-stream))
  (fill-input-line stream)
  (char-or-newline stream :peek))

(defmethod stream-read-line ((stream console-stream))
  (unless (<= (input-index stream) (length (input-line stream)))
    (fill-input-line stream))
  (values (prog1 (nsubseq (input-line stream) (input-index stream))
            (setf (input-index stream) (1+ (length (input-line stream)))
                  (column stream) 0))
          (null (input-line stream))))

(defmethod stream-listen ((stream console-stream))
  (<= (input-index stream) (length (input-line stream))))

(defmethod stream-unread-char ((stream console-stream) ch)
  (if (<= (input-index stream) (length (input-line stream)))
      (progn
        (if (plusp (input-index stream))
            (progn (decf (input-index stream))
                   (setf (aref (input-line stream) (input-index stream)) ch))
            (setf (input-line stream)
                  (concatenate 'string (string ch) (input-line stream))))
        (setf (column stream) (max 0 (1- (column stream)))))
      (setf (input-line stream) (string ch)
            (input-index stream) 0
            (column stream) 0))
  ch)

;;; character output

(defmethod stream-write-char ((stream console-stream) ch)
    (if (char= #\newline ch)
      (progn
        (console.println)
        (setf (column stream) 0))
      (progn
        (console.print (string ch))
       (incf (column stream))))
  ch)

(defmethod stream-terpri ((stream console-stream))
  (stream-write-char stream #\newline)
  nil)

(defmethod stream-write-string ((stream console-stream) string &optional (start 0) end)
  (let* ((end  (or end (length string)))
         (nlp  (position #\newline string :start start :end end :from-end t)))
    (console.print (nsubseq string start end))
    (if nlp
        (setf (column stream) (- end nlp))
        (incf (column stream) (- end start))))
  string)

(defmethod stream-line-column ((stream console-stream))
  (column stream))

(defmethod stream-start-line-p ((stream console-stream))
  (zerop (column stream)))

(defmethod stream-advance-to-column ((stream console-stream) column)
  (let ((delta (- column (column stream))))
    (when (plusp delta)
      (stream-write-string stream (make-string delta :initial-element #\space))
      delta)))

(defmethod close ((stream console-stream) &key abort)
  (declare (ignorable stream abort)))


(defvar *console-io* (make-instance 'console-stream)
  "This is a I/O stream to the Console window.")

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THE END ;;;;
