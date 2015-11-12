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
;;;;    2015-11-12 <PJB> Created.
;;;;BUGS
;;;;    Currently only implemented on CCL using CCL:RUN-PROGRAM.
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


(defpackage "COM.INFORMATIMAGO.CLEXT.PORTABLE-GRAPHICS-LIBRARY.LOW-LEVEL"
  (:nicknames "PGL.LOW-LEVEL")
  (:use)
  (:documentation "

This package exports the low-level functions
that send messages to the JavaBackEnd.
There shouldn't be a need to use them directly.

Copyright Pascal J. Bourguignon 2015 - 2015
Licensed under the AGPL3.

")
  (:export "FILE.OPEN-FILE-DIALOG" "3D-RECT.CREATE" "3D-RECT.SET-RAISED"
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
           "CONSOLE.SET-SIZE" "SOUND.CREATE" "SOUND.DELETE" "SOUND.PLAY")
  (:export
   ;; The base types and structures:
   "INT" "DOUBLE" "DIMENSION" "MAKE-DIMENSION" "COPY-DIMENSION"
   "DIMENSION-P" "DIMENSION-WIDTH" "DIMENSION-HEIGHT" "POINT"
   "MAKE-POINT" "COPY-POINT" "POINT-P" "POINT-X" "POINT-Y"
   "RECTANGLE" "MAKE-RECTANGLE" "COPY-RECTANGLE" "RECTANGLE-P"
   "RECTANGLE-X" "RECTANGLE-Y" "RECTANGLE-WIDTH"
   "RECTANGLE-HEIGHT"))


(defpackage "COM.INFORMATIMAGO.CLEXT.PORTABLE-GRAPHICS-LIBRARY"
  (:nicknames "PGL")
  (:documentation "

This package implements a Portable Graphics Library using the
JavaBackEnd from the Stanford Portable Library.
http://cs.stanford.edu/~eroberts/papers/ITiCSE-2013/PortableGraphicsLibrary.pdf
https://github.com/cs50/spl

It defines and export a set of CLOS classes to represent the GUI
objects of the JavaBackEnd, along with methods to send the requests
and process the results.

Copyright Pascal J. Bourguignon 2015 - 2015
Licensed under the AGPL3.

")
  (:use "COMMON-LISP"
        "TRIVIAL-GRAY-STREAMS"
        "ORG.MAPCAR.PARSE-NUMBER"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE"
        "COM.INFORMATIMAGO.CLEXT.PORTABLE-GRAPHICS-LIBRARY.LOW-LEVEL")
  (:import-from "UIOP" "GETENV")

  (:export


   )
  
  (:export
   ;; The base types and structures:
   "INT" "DOUBLE" "DIMENSION" "MAKE-DIMENSION" "COPY-DIMENSION"
   "DIMENSION-P" "DIMENSION-WIDTH" "DIMENSION-HEIGHT" "POINT"
   "MAKE-POINT" "COPY-POINT" "POINT-P" "POINT-X" "POINT-Y"
   "RECTANGLE" "MAKE-RECTANGLE" "COPY-RECTANGLE" "RECTANGLE-P"
   "RECTANGLE-X" "RECTANGLE-Y" "RECTANGLE-WIDTH"
   "RECTANGLE-HEIGHT")

  (:export
   ;; Event Masks:
   "+ACTION-EVENT+" "+KEY-EVENT+" "+TIMER-EVENT+" "+WINDOW-EVENT+"
   "+MOUSE-EVENT+" "+CLICK-EVENT+" "+ANY-EVENT+"
   ;; Event Types:
   "+WINDOW-CLOSED+" "+WINDOW-RESIZED+" "+ACTION-PERFORMED+"
   "+MOUSE-CLICKED+" "+MOUSE-PRESSED+" "+MOUSE-RELEASED+" "+MOUSE-MOVED+"
   "+MOUSE-DRAGGED+" "+KEY-PRESSED+" "+KEY-RELEASED+" "+KEY-TYPED+"
   "+TIMER-TICKED+")
  
  (:export "OPEN-BACKEND" "CLOSE-BACKEND"

           )
  (:export
   ;; The *console-io* stream:
   "*CONSOLE-IO*" "CONSOLE-STREAM"
   "CONSOLE-SET-SIZE" "CONSOLE-SET-FONT" "CONSOLE-CLEAR"))
(in-package "COM.INFORMATIMAGO.CLEXT.PORTABLE-GRAPHICS-LIBRARY")


(deftype int    () `(integer ,(- (expt 2 31)) ,(- (expt 2 31) 1)))
(deftype double () 'double-float)
(defstruct point     (x 0.0d0) (y 0.0d0))
(defstruct dimension (width 0.0d0) (height 0.0d0))
(defstruct rectangle (x 0.0d0) (y 0.0d0) (width 0.0d0) (height 0.0d0))
(defun int (real) (round real))
(defun double (real) (coerce real 'double))

(defun rectangle-emptyp (r)
  (or (not (plusp (rectangle-width r)))
      (not (plusp (rectangle-height r)))))

(defun rectangle-contains-point-p (r p)
  (and (<= (rectangle-x r) (point-x p) (+ (rectangle-x r) (rectangle-width r)))
       (<= (rectangle-y r) (point-y p) (+ (rectangle-y r) (rectangle-height r)))))


(defvar *backend* nil)

(defvar *spl-path* "/usr/local/lib/spl.jar")

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
  (unless *backend*
    (setf *program-name* program-name)
    (let ((classpath (or classpath (getenv "CLASSPATH") *spl-path*)))
      #+ccl
      (setf *backend* (ccl:run-program "java" (list (format nil "-Xdock:name=~A" program-name)
                                                    "-classpath"
                                                    classpath
                                                    "stanford/spl/JavaBackEnd"
                                                    program-name)  
                                       :wait nil :pty t
                                       :input :stream
                                       :output :stream
                                       :error *error-output*
                                       :sharing :lock)))))

(defun close-backend ()
  "
Quits the JavaBackEnd GUI.
If the backend is not open, nothing is done.
"
  (when *backend*
    #+ccl (ignore-errors (ccl:signal-external-process *backend* 9 :error-if-exited nil))
    (setf *backend* nil)))

(defun send (command &rest arguments)
  (let ((stream (if *backend*
                    (ccl::external-process-input *backend*)
                    *standard-output*))
        (cmd (format nil "~A(~{~A~^,~})" command arguments))
        (jbetrace (decode-boolean (getenv "JBETRACE"))))
    (when jbetrace (format *trace-output* "~&-> ~A~%" cmd))
    (write-line cmd stream)
    (force-output stream)))


(define-condition jbe-error (simple-error)
  ())

(define-condition jbe-syntax-error (jbe-error)
  ())


(defvar *sources*     (make-hash-table))
(defvar *windows*     (make-hash-table))
(defvar *timers*      (make-hash-table))
(defvar *event-queue* (make-queue))

(defun gid (id) (format nil "\"0x~X\"" id))

(defun decode-id (id)
  (assert (prefixp "0x" id))
  (parse-integer id :start 2 :radix 16))

(defun get-source (id) (gethash id *sources*))
(defun get-window (id) (gethash id *windows*))
(defun get-timer  (id) (gethash id *timers*))

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


(defun encode-double (value)
  (substitute #\e #\D (format nil "~:@(~,,,,,,'dE~)" value)
              :test (function char-equal)))

(defun decode-boolean (value)
  (and (not (null value))
       (plusp (length value))
       (char-equal #\t (aref value 0))))


(defun get-result ()
  (let ((stream   (ccl::external-process-output *backend*))
        (jbetrace (decode-boolean (getenv "JBETRACE"))))
    (handler-case
        (loop
          (let ((line (read-line stream)))
            (when jbetrace (format *trace-output* "~&<- ~A~%" line))
            (cond ((prefixp "result:" line)
                   (return-from get-result (subseq line 7)))
                  ((prefixp "event:" line)
                   (queue-enqueue *event-queue* (parse-event (subseq line 6)))))))
      (error ()
        nil))))

(defun get-error ()
  (let ((result (get-result)))
    (unless (string-equal result "ok")
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

;;; --------------------
;;; scanner
;;; --------------------

(defun make-scanner (string)
  (cons 0 string))

(defun next-char (scanner)
  (with-accessors ((pos car) (src cdr)) scanner
    (when (< pos (length src)) (aref src pos))))

(defun eat-char (scanner)
  (with-accessors ((pos car) (src cdr)) scanner
    (when (< pos (length src)) (incf pos))))

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
             :while (or (alphanumericp ch) (find ch "_."))
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

(defun test/scanner ()
  (assert (equal (let ((s (make-scanner " hello(\"Howdy\", 42,-123.456e+78,false,true,foo)")))
                   (loop
                     :for token := (next-token s)
                     :while token :collect token))
                 '((symbol . "hello")
                   #\( "Howdy" #\, 42 #\, -1.2345600000000003D+80 #\,
                   (boolean) #\, (boolean . t) #\, (symbol . "foo") #\))))
  :success)

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

;;; --------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defconstant +action-event+        #x010)
 (defconstant +key-event+           #x020)
 (defconstant +timer-event+         #x040)
 (defconstant +window-event+        #x080)
 (defconstant +mouse-event+         #x100)
 (defconstant +click-event+         #x200)
 (defconstant +any-event+           #x3f0)
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

(defgeneric event-type-keyword (event)
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
                          :window (get-timer (decode-id id))
                          :time time)))

(defun parse-window-event (parameters type)
  (destructuring-bind (id time) parameters
    (make-instance 'event :type type
                          :window (get-window (decode-id id))
                          :time time)))

(defun parse-action-event (parameters type)
  (destructuring-bind (id action time) parameters
    (make-instance 'event :type type
                          :window (get-source (decode-id id))
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


(defmacro generate-JBE-functions (&rest definitions)
  `(progn
     ,@(mapcar (lambda (definition)
                 (destructuring-bind (name jbe-name lambda-list &optional result-type)
                     definition
                   (let ((parameters (mapcar (function first) lambda-list)))
                     `(defun ,name ,parameters
                        (let ,(mapcar (lambda (parameter)
                                        (destructuring-bind (name type) parameter
                                          `(,name ,(ecase type
                                                     (id         `(gid (object-id ,name)))
                                                     (string     `(string-escape (string ,name)))
                                                     (boolean    `(cond (,name "true")
                                                                        (t "false")))
                                                     (int        `(round ,name))
                                                     (double     `(encode-double (coerce ,name 'double)))))))
                               lambda-list)
                          (send ,jbe-name ,@parameters))
                        ,@(ecase result-type
                            ((nil)       '((values)))
                            ((string)    '((get-result)))
                            ((boolean)   '((get-boolean)))
                            ((int)       '((get-int)))
                            ((double)    '((get-double)))
                            ((dimension) '((get-dimension)))
                            ((rectangle) '((get-rectangle)))
                            ((:error)    '((get-error))))))))
               definitions)))


(generate-JBE-functions
 (file.open-file-dialog         "File.openFileDialog"          ((title string) (mode string) (path string))                             string)
 (3drect.create                 "G3DRect.create"               ((id id) (width double) (height double) (raised boolean)))
 (3drect.set-raised             "G3DRect.setRaised"            ((id id) (raised boolean)))
 (arc.create                    "GArc.create"                  ((id id) (width double) (height double) (start double) (sweep double)))
 (arc.set-frame-rectangle       "GArc.setFrameRectangle"       ((id id) (x double) (y double) (width double) (height double)))
 (arc.set-start-angle           "GArc.setStartAngle"           ((id id) (angle double)))
 (arc.set-sweep-angle           "GArc.setSweepAngle"           ((id id) (angle double)))
 (button.create                 "GButton.create"               ((id id) (label string)))
 (check-box.create              "GCheckBox.create"             ((id id) (label string)))
 (check-box.is-selected         "GCheckBox.isSelected"         ((id id))                                                                boolean)
 (check-box.set-selected        "GCheckBox.setSelected"        ((id id) (selected boolean)))
 (compound.add                  "GCompound.add"                ((top-compound id) (compound id)))
 (compound.create               "GCompound.create"             ((id id)))
 (event.get-next-event          "GEvent.getNextEvent"          ((mask int)))
 (event.wait-for-event          "GEvent.waitForEvent"          ((mask int)))
 (image.create                  "GImage.create"                ((id id) (filename string))                                              dimension)
 (interactor.set-action-command "GInteractor.setActionCommand" ((id id) (cmd string)))
 (interactor.get-size           "GInteractor.getSize"          ((id id))                                                                dimension)
 (label.create                  "GLabel.create"                ((id id) (str string)))
 (label.get-font-ascent         "GLabel.getFontAscent"         ((id id))                                                                double)
 (label.get-font-descent        "GLabel.getFontDescent"        ((id id))                                                                double)
 (label.get-size                "GLabel.getGLabelSize"         ((id id))                                                                dimension)
 (label.set-font                "GLabel.setFont"               ((id id) (font string)))
 (label.set-label               "GLabel.setLabel"              ((id id) (str string)))
 (line.create                   "GLine.create"                 ((id id) (x1 double) (y1 double) (x2 double) (y2 double)))
 (line.set-end-point            "GLine.setEndPoint"            ((id id) (x double) (y double)))
 (line.set-start-point          "GLine.setStartPoint"          ((id id) (x double) (y double)))
 (object.contains               "GObject.contains"             ((id id) (x double) (y double)))
 (object.delete                 "GObject.delete"               ((id id)))
 (object.get-bounds             "GObject.getBounds"            ((id id))                                                   rectangle)
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
 (slider.get-value              "GSlider.getValue"             ((id id))                                                   int)
 (slider.set-value              "GSlider.setValue"             ((id id) (value int)))
 (text-field.create             "GTextField.create"            ((id id) (nchars int)))
 (text-field.get-text           "GTextField.getText"           ((id id))                                                              string)
 (text-field.set-text           "GTextField.setText"           ((id id) (str string)))
 (chooser.create                "GChooser.create"              ((id id)))
 (chooser.add-item              "GChooser.addItem"             ((id id) (item string)))
 (chooser.get-selected-item     "GChooser.getSelectedItem"     ((id id))                                                              string)
 (chooser.set-selected-item     "GChooser.setSelectedItem"     ((id id) (item string)))
 (timer.create                  "GTimer.create"                ((id id) (msec double)))
 (timer.delete                  "GTimer.deleteTimer"           ((id id)))
 (timer.pause                   "GTimer.pause"                 ((milliseconds double))                                         :error)
 (timer.start                   "GTimer.startTimer"            ((id id)))
 (timer.stop                    "GTimer.stopTimer"             ((id id)))
 (window.add-to-region          "GWindow.addToRegion"          ((window id) (object id) (region id)))
 (window.set-region-alignment   "GWindow.setRegionAlignment"   ((id id) (region string) (align string)))
 (window.clear                  "GWindow.clear"                ((id id)))
 (window.close                  "GWindow.close"                ((id id)))
 (window.create                 "GWindow.create"               ((id id) (width int) (height int) (top id))                            :error)
 (window.delete                 "GWindow.delete"               ((id id)))
 (window.draw                   "GWindow.draw"                 ((id id) (object id)))
 (window.exit-graphics          "GWindow.exitGraphics"         ())
 (window.get-screen-height      "GWindow.getScreenHeight"      ()                                                                     double)
 (window.get-screen-width       "GWindow.getScreenWidth"       ()                                                                     double)
 (window.repaint                "GWindow.repaint"              ((id id)))
 (window.request-focus          "GWindow.requestFocus"         ((id id)))
 (window.set-resizable          "GWindow.setResizable"         ((id id)))
 (window.set-title              "GWindow.setTitle"             ((id id) (title string)))
 (window.set-visible            "GWindow.setVisible"           ((id id) (visible boolean)))
 (top-compound.create           "TopCompound.create"           ((id id)))
 (console.clear                 "JBEConsole.clear"             ())
 (console.get-line              "JBEConsole.getLine"           ()                                                                     string)
 (console.print                 "JBEConsole.print"             ((str string)))
 (console.println               "JBEConsole.println"           ())
 (console.set-font              "JBEConsole.setFont"           ((font string)))
 (console.set-size              "JBEConsole.setSize"           ((width int) (height int)))
 (sound.create                  "Sound.create"                 ((id id) (filename string))                                            :error)
 (sound.delete                  "Sound.delete"                 ((id id)))
 (sound.play                    "Sound.play"                   ((id id))))


(defgeneric object-slots (object)
  (:method-combination append))

(defclass timer ()
  ((duration-ms :initarg :duration-ms :initform 0.0d0 :type 'double :reader timer-duration-ms)))

(defmethod object-slots append ((self timer))
  (list :duration-ms (timer-duration-ms self)))

(defmethod print-object ((self timer) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~{~S~^ ~}" (object-slots self)))
  self)

(defmethod initialize-instance :before ((self timer) &key &allow-other-keys)
  (open-backend))

(defmethod initialize-instance :after ((self timer) &key &allow-other-keys)
  (timer.create self (timer-duration-ms self))
  (register self))

(defgeneric start-timer (self)
  (:method   ((self timer))
    (timer.start self)))

(defgeneric stop-timer (self)
  (:method   ((self timer))
    (timer.stop self)))

(defgeneric free-timer (self)
  (:method  ((self timer))
    (unregister self)))


(defvar *last-object-id* 0)

(defclass object ()
  ((id         :initform (incf *last-object-id*)      :type integer          :reader object-id)
   (x          :initarg :x          :initform 0.0d0   :type double           :reader object-x)
   (y          :initarg :y          :initform 0.0d0   :type double           :reader object-y)
   (width      :initarg :width      :initform 0.0d0   :type double           :reader object-width)
   (height     :initarg :height     :initform 0.0d0   :type double           :reader object-height)
   (color      :initarg :color      :initform "BLACK" :type string           :reader object-color)
   (fill-color :initarg :fill-color :initform nil     :type (or string null) :reader object-fill-color)
   (filled     :initarg :filled     :initform nil     :type boolean          :reader object-filledp)
   (visible    :initarg :visible    :initform t       :type boolean          :reader object-visiblep)
   (parent     :initarg :parent     :initform nil     :type (or null object) :reader object-parent)))

(defmethod object-slots append ((self object))
  (list :id (object-id self)
        :x (object-x self)
        :y (object-y self)
        :width (object-width self)
        :height (object-height self)
        :color (object-color self)
        :fill-color (object-fill-color self)
        :filled (object-filledp self)
        :visible (object-visiblep self)
        :parent (when (object-parent self)
                  (class-of (object-parent self)))))

(defmethod print-object ((self object) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~{~S~^ ~}" (object-slots self)))
  self)

(defmethod initialize-instance :before ((self object) &key &allow-other-keys)
  (open-backend))

(defmethod initialize-instance ((self object) &rest keys &key x y width height &allow-other-keys)
  (apply (function call-next-method) self
         (append (when x (list :x (double x)))
                 (when y (list :y (double y)))
                 (when width (list :width (double width)))
                 (when height (list :height (double height)))
                 keys)))

(defmethod set-object-location ((self object) x y)
  (setf (slot-value self 'x) (double x)
        (slot-value self 'y) (double y))
  (object.set-location self (double x) (double y)))
(defmethod object-location ((self object))
  (make-point :x (object-x self) :y (object-y self)))
(defmethod (setf object-location) (point (self object))
  (set-object-location self (point-x point) (point-y point)))

(defmethod set-object-size ((self object) width height)
  (setf (slot-value self 'width)  (double width)
        (slot-value self 'height) (double height))
  (object.set-size self (double width) (double height)))
(defmethod object-size ((self object))
  (make-dimension :width (object-width self)
                  :height (object-height self)))
(defmethod (setf object-size) ((size dimension) (self object))
  (set-object-size self (dimension-width size) (dimension-height size)))

(defmethod set-object-color ((self object) color)
  (setf (slot-value self 'color) (string color))
  (object.set-color self  (slot-value self 'color)))
(defmethod (setf object-color) (color (self object))
  (set-object-color self color))

(defmethod set-object-fill-color ((self object) color)
  (setf (slot-value self 'fill-color) (string color))
  (object.set-fill-color self  (slot-value self 'fill-color)))
(defmethod (setf object-fill-color) (color (self object))
  (set-object-fill-color self color))

(defgeneric object-contains (object x y)
  (:method ((self object) x y)
    (let* ((x0 (object-x self))
           (y0 (object-y self))
           (x1 (+ x0 (object-width  self)))
           (y1 (+ y0 (object-height self))))
      (and (<= x0 x x1) (<= y0 y y1)))))

(defgeneric bounds (self)
  (:method ((self object))
    (frame-rectangle self)))

(defgeneric frame-rectangle (self)
  (:method ((self object))
    (make-rectangle :x (object-x self)
                    :y (object-y self)
                    :width  (object-width  self)
                    :height (object-height self))))

(defgeneric set-frame-rectangle (self x y width height)
  (:method ((self object) x y width height)
    (object.set-location self (double x) (double y))
    (object.set-size self (double width) (double height))))

(defgeneric (setf frame-rectangle) (new-rect self)
  (:method (new-rect (self object))
    (set-frame-rectangle self
                         (rectangle-x new-rect)
                         (rectangle-y new-rect)
                         (rectangle-width new-rect)
                         (rectangle-height new-rect))))

(defmethod set-object-visible ((self object) visible)
  (setf (slot-value self 'visible) visible)
  (object.set-visible self visible))

(defmethod (setf object-visible) (visible (self object))
  (set-object-visible self visible))




(defvar *default-label-font* "Dialog-13")
(defvar *default-corner* 10)
(defvar *arc-tolerance*  2.5d0)
(defvar *line-tolerance* 1.5d0)

(declaim (inline square degree-to-radian radian-to-degree cos-degree sin-degree))
(defun square (x) (* x x))
(defun degree-to-radian (angle) (/ (* angle pi) 180.0d0))
(defun radian-to-degree (angle) (/ (* angle 180.0d0) pi))
(defun cos-degree (angle) (cos (degree-to-radian angle)))
(defun sin-degree (angle) (sin (degree-to-radian angle)))

(defclass rect (object)
  ())

(defmethod initialize-instance :after ((self rect) &key &allow-other-keys)
  (rect.create self (object-width self) (object-height self))
  (object.set-location self (object-x self) (object-y self)))

;; 3drect and round-rect are not subclasses of rect to avoid creating
;; a rect when we create a 3drect or round-rect.

(defclass 3drect (object) 
  ((raised :initarg :raised :type boolean   :reader 3drect-raisedp)))

(defmethod object-slots append ((self 3drect))
  (list :raisedp (3drect-raisedp self)))

(defmethod initialize-instance :after ((self 3drect) &key &allow-other-keys)
  (3drect.create self (object-width self) (object-height self)
                 (3drect-raisedp self))
  (object.set-location self (object-x self) (object-y self)))

(defmethod (setf 3drect-raisedp) (new-value (self 3drect))
  (setf (slot-value self 'raised) new-value)
  (3drect.set-raised self new-value))

(defclass round-rect (object)
  ((corner :initarg :corner :initform *default-corner* :type double :reader round-rect-corner)))

(defmethod object-slots append ((self round-rect))
  (list :corner (round-rect-corner self)))

(defmethod initialize-instance :after ((self round-rect) &key &allow-other-keys)
  (round-rect.create self (object-width self) (object-height self) (round-rect-corner self))
  (object.set-location self (object-x self) (object-y self)))

(defclass oval (object)
  ())

(defmethod initialize-instance :after ((self oval) &key &allow-other-keys)
  (oval.create self (object-width self) (object-height self))
  (object.set-location self (object-x self) (object-y self)))

(defmethod object-contains ((self oval) x y)
  (let ((rx (/ (object-width self) 2))
        (ry (/ (object-height self) 2)))
    (if (or (zerop rx) (zerop ry))
        nil
        (let ((dx (- x (object-x self) rx))
              (dy (- y (object-y self) ry)))
          (<= (+ (/ (square dx) (square rx))
                 (/ (square dy) (square ry)))
              1.0d0)))))

(defclass line (object)
  ())

(defmethod initialize-instance :after ((self line) &key &allow-other-keys)
  (let* ((x0 (object-x self))
         (y0 (object-y self))
         (x1 (+ x0 (object-width  self)))
         (y1 (+ y0 (object-height self))))
    (line.create self x0 y0 x1 y1)))

(defgeneric start-point (self)
  (:method ((self line))
    (let* ((x0 (object-x self))
           (y0 (object-y self)))
      (make-point :x x0 :y y0))))

(defgeneric end-point (self)
  (:method ((self line))
    (let* ((x0 (object-x self))
           (y0 (object-y self))
           (x1 (+ x0 (object-width  self)))
           (y1 (+ y0 (object-height self))))
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
    (setf (slot-value self 'width)  (double (- x (object-x self)))
          (slot-value self 'height) (double (- y (object-y self))))
    (line.set-end-point self (double x) (double y))))

(defgeneric (setf start-point) (new-point self)
  (:method (new-point (self line))
    (set-start-point self (point-x new-point) (point-y new-point))))

(defgeneric (setf end-point) (new-point self)
  (:method (new-point (self line))
    (set-end-point self (point-x new-point) (point-y new-point))))

(declaim (inline dsq))
(defun dsq (x0 y0 x1 y1)
  (+ (square (- x0 x1)) (square (- y0 y1))))

(defmethod object-contains ((self line) x y)
  (let* ((x  (double x))
         (y  (double y))
         (x0 (object-x self))
         (y0 (object-y self))
         (x1 (+ x0 (object-width  self)))
         (y1 (+ y0 (object-height self)))
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

(defclass arc (object)
  ((start :initarg :start :type double :reader arc-start)
   (sweep :initarg :sweep :type double :reader arc-sweep)))

(defmethod object-slots append ((self arc))
  (list :start (arc-start self)
        :sweep (arc-sweep self)))

(defmethod initialize-instance :after ((self arc) &key &allow-other-keys)
  (arc.create self (object-width self) (object-height self)
              (arc-start self) (arc-sweep self))
  (object.set-location self (object-x self) (object-y self)))

(defgeneric (setf arc-start) (start self)
  (:method (start (self arc))
    (arc.set-start-angle self start)))

(defgeneric (setf arc-sweep) (sweep self)
  (:method (sweep (self arc))
    (arc.set-sweep-angle self sweep)))

(defmethod set-frame-rectangle ((self arc) x y width height)
  (arc.set-frame-rectangle self (double x) (double y) (double width) (double height)))

(defmethod contains-angle ((self arc) theta)
  (let ((start (min (arc-start self)
                    (+ (arc-start self) (arc-sweep self))))
        (sweep (abs (arc-sweep self)))
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

(defmethod object-contains ((self arc) x y)
  (let ((rx (/ (object-width self) 2))
        (ry (/ (object-height self) 2)))
    (if (or (zerop rx) (zerop ry))
        nil
        (let* ((dx (- x (object-x self) rx))
               (dy (- y (object-y self) ry))
               (r  (+ (/ (square dx) (square rx))
                      (/ (square dy) (square ry)))))
          (when (if (object-filledp self)
                    (< 1.0d0 r)
                    (let ((tt (/ *arc-tolerance* (/ (+ rx ry) 2))))
                      (< tt (abs (- 1.0d0 r)))))
            (contains-angle self (radian-to-degree (atan (- dy) dx))))))))

(defmethod bounds ((self arc))
  (let* ((rx (/ (object-width  self) 2))
         (ry (/ (object-height self) 2))
         (cx (+ (object-x self) rx))
         (cy (+ (object-y self) ry))
         (p1x (+ cx (* rx (cos-degree (arc-start self)))))
         (p1y (+ cy (* ry (sin-degree (arc-start self)))))
         (p2x (+ cx (* rx (cos-degree (+ (arc-start self) (arc-sweep self))))))
         (p2y (+ cy (* ry (sin-degree (+ (arc-start self) (arc-sweep self))))))
         (xmin (min p1x p2x))
         (xmax (max p1x p2x))
         (ymin (min p1y p2y))
         (ymax (max p1y p2y)))
    (when (contains-angle self   0.0d0) (setf xmax (+ cx rx)))
    (when (contains-angle self  90.0d0) (setf ymin (- cy ry)))
    (when (contains-angle self 180.0d0) (setf xmin (- cx rx)))
    (when (contains-angle self 270.0d0) (setf ymax (+ cy ry)))
    (when (object-filledp self)
      (setf xmin (min xmin cx)
            ymin (min ymin cy)
            xmax (max xmax cx)
            ymax (max ymax cy)))
    (make-rectangle :x xmin :y ymin :width (- xmax xmin) :height (- ymax ymin))))

(defun adjustable-vector ()
  (make-array 8 :fill-pointer 0 :adjustable t))

(defclass polygon (object)
  ((vertices :initform (adjustable-vector)
             :type vector  :reader polygon-vertices)
   (cx       :initform 0.0d0       :type double :reader polygon-cx)
   (cy       :initform 0.0d0       :type double :reader polygon-cy)))

(defmethod object-slots append ((self polygon))
  (list :cx (polygon-cx self)
        :cy (polygon-cy self)
        :vertices (polygon-vertices self)))

(defmethod initialize-instance :after ((self polygon) &key &allow-other-keys)
  (polygon.create self))

(defmethod add-vertex ((self polygon) x y)
  (setf (slot-value self 'cx) x
        (slot-value self 'cy) y)
  (vector-push-extend (make-point :x (double x) :y (double y))
                      (polygon-vertices self))
  (polygon.add-vertex self (double x) (double y)))

(defmethod add-edge ((self polygon) dx dy)
  (add-vertex self (+ dx (polygon-cx self))  (+ dy (polygon-cy self))))

(defmethod add-polar-edge ((self polygon) r theta)
  (add-vertex self (* r (cos-degree theta)) (* r (sin-degree theta))))

(defmethod bounds ((self polygon))
  (let* ((vertices (polygon-vertices self))
         (n (length vertices)))
    (if (zerop n)
        (make-rectangle)        
        (let ((xmin (point-x (aref vertices 0)))
              (ymin (point-y (aref vertices 0)))
              (xmax (point-x (aref vertices 0)))
              (ymax (point-y (aref vertices 0))))
          (loop :for pt :across vertices
                :for x := (point-x pt)
                :for y := (point-y pt)
                :do (setf xmin (min x xmin)
                          xmax (max x xmax)
                          ymin (min y ymin)
                          ymax (max y ymax)))
          (make-rectangle :x xmin :y ymin
                          :width (- xmax xmin)
                          :height (- ymax ymin))))))

(defmethod object-contains ((self polygon) x y)
  (let* ((vertices (polygon-vertices self))
         (n (length vertices))
         (crossings 0))
    (if (< n 2)
        nil        
        (let* ((p0 (aref vertices 0))
               (x0 (point-x p0))
               (y0 (point-y p0))
               (p1 (aref vertices (1- n)))
               (x1 (point-x p1))
               (y1 (point-y p1)))
          (when (and (= x0 x1) (= y0 y1)) (decf n))
          (loop :for i :from 1 :to n
                :for p1 := (aref vertices (mod i n))
                :for x1 := (point-x p1)
                :for y1 := (point-y p1)
                :do (when (and (not (eq (not (< y y0)) (not (< y y1))))
                               (< (- x x0)
                                  (/ (* (- x1 x0) (- y y0))
                                     (- y1 y0))))
                      (incf crossings))
                    (setf x0 x1
                          y0 y1))
          (= 1 (mod crossings 2))))))


(defclass compound (object)
  ((components :initform (adjustable-vector)
               :type vector   :reader compound-components)))

(defmethod object-slots append ((self compound))
  (list :components (compound-components self)))

(defmethod initialize-instance :after ((self compound) &key &allow-other-keys)
  (compound.create self))

(defgeneric compound-add (self other)
  (:method   ((self compound) other)
    (vector-push-extend other (compound-components self))
    (compound.add self other)))

(defgeneric compound-remove (self other)
  (:method   ((self compound) other)
    (setf (slot-value self 'components) (delete (compound-components self) other :count 1))
    (object.remove self other)))

(defgeneric get-object-at (self x y))
(defmethod get-object-at ((self compound) x y)
  (find-if (lambda (object) (object-contains object x y))
           (compound-components self)))


(defclass label (object)
  ((font    :initarg :font    :initform *default-label-font* :type string :reader label-font)
   (text    :initarg :text    :type string :reader label-text)
   (ascent  :initarg :ascent  :type double :reader label-ascent)
   (descent :initarg :descent :type double :reader label-descent)))

(defmethod object-slots append ((self label))
  (list :font (label-font self)
        :ascent (label-ascent self)
        :descent (label-descent self)
        :text (label-text self)))

(defmethod initialize-instance :after ((self label) &key &allow-other-keys)
  (label.create self (label-text self))
  (set-label-font self (label-font self))
  (set-label-text self (label-text self)))

(defmethod set-label-font ((self label) font)
  (setf (slot-value self 'font) font)
  (label.set-font self font)
  (let ((size (label.get-size self)))
    (setf (slot-value self 'ascent)  (label.get-font-ascent  self)
          (slot-value self 'descent) (label.get-font-descent self)
          (slot-value self 'width)  (dimension-width size)
          (slot-value self 'height) (dimension-height size))))

(defmethod (setf label-font) (new-font (self label))
  (set-label-font self new-font))


(defmethod set-label-text ((self label) text)
  (setf (slot-value self 'text) text)
  (label.set-label self text)
  (let ((size (label.get-size self)))
    (setf (slot-value self 'width)  (dimension-width size)
          (slot-value self 'height) (dimension-height size))))

(defmethod (setf label-text) (new-text (self label))
  (set-label-text self new-text))

(defmethod bounds ((self label))
  (make-rectangle :x (object-x self)
                  :y (- (object-y self) (label-ascent self))
                  :width (object-width self)
                  :height (object-height self)))

(defclass image (object)
  ((file-name :initarg :file-name :type string :reader image-file-name)))

(defmethod object-slots append ((self image))
  (list :file-name (image-file-name self)))

(defmethod initialize-instance :after ((self image) &key &allow-other-keys)
  (let ((size (image.create self (image-file-name self))))
    (setf (slot-value self 'width)  (dimension-width size)
          (slot-value self 'height) (dimension-height size))))


(defclass interactor (object)
  ((action-command :initarg :action-command :type string   :reader interactor-action-command)
   (label          :initarg :label          :type string   :reader interactor-label)))

(defmethod object-slots append ((self interactor))
  (list :action-command (interactor-action-command self)
        :label (interactor-label self)))

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

(defclass button (interactor)
  ())

(defmethod initialize-instance :after ((self button) &key &allow-other-keys)
  (button.create self (interactor-label self))
  (setf (action-command self) (interactor-action-command self)))

(defclass check-box (interactor)
  ())

(defmethod initialize-instance :after ((self button) &key &allow-other-keys)
  (check-box.create self (interactor-label self)))

(defmethod check-box-selectedp ((self check-box))
  (check-box.is-selected self))

(defmethod set-check-box-selected ((self check-box) selected)
  (check-box.set-selected self selected))

(defmethod (setf check-box-selectedp) (selected (self check-box))
  (set-check-box-selected self selected))

(defclass slider (interactor)
  ((min   :initarg :min   :type int :reader slider-min)
   (max   :initarg :max   :type int :reader slider-max)
   (value :initarg :value :type int)))

(defmethod object-slots append ((self slider))
  (list :min (slider-min self)
        :value (slider-value self)
        :max (slider-max self)))

(defmethod initialize-instance :after ((self slider) &key &allow-other-keys)
  (slider.create self (slider-min self) (slider-max self) (slot-value self 'value)))

(defmethod set-slider-value ((self slider) value)
  (setf (slot-value self 'value) value)
  (slider.set-value self value))

(defmethod (setf slider-value) (value (self slider))
  (set-slider-value self value))

(defmethod slider-value ((self slider))
  (slider.get-value self))

(defclass text-field (interactor)
  ((nchars :initarg :nchars :type int :reader text-field-nchars)))

(defmethod object-slots append ((self text-field))
  (list :nchars (text-field-nchars self)))

(defmethod initialize-instance :after ((self text-field) &key &allow-other-keys)
  (text-field.create self (text-field-nchars self)))

(defmethod text-field-text ((self text-field))
  (text-field.get-text self))

(defmethod set-text-field-text ((self text-field) str)
  (text-field.set-text self str))

(defmethod (setf text-field-text) (str (self text-field))
  (set-text-field-text self str))


(defclass chooser (interactor)
  ((items         :initarg :items    :initform (adjustable-vector)  :type vector   :reader chooser-items)
   (selected-item :initarg :selected :type string :reader chooser-selected-item)))

(defmethod object-slots append ((self chooser))
  (list :items (chooser-items self)
        :selected-item (slot-value self 'selected-item)))

(defmethod initialize-instance :after ((self chooser) &key &allow-other-keys)
  (chooser.create self))

(defmethod chooser-add-item ((self chooser) item)
  (chooser.add-item self item))

(defmethod chooser-selected-item ((self chooser))
  (setf (slot-value self 'selected-item) (chooser.get-selected-item self)))

(defmethod set-chooser-selected-item ((self chooser) item)
  (chooser.set-selected-item self item))

(defmethod (setf chooser-selected-item) (selected  (self chooser))
  (set-chooser-selected-item self selected))




(defclass window (object)
  ((color :initarg :color :initform "BLACK"    :type string :accessor object-color)
   (title :initarg :title :initform "Untitled" :type string :reader   window-title)
   (top   :reader  window-top)))

(defmethod object-slots append ((self window))
  (list :title (window-title self)
        :top (window-top self)))

(defmethod initialize-instance :after ((self window) &key &allow-other-keys)
  (setf (slot-value self 'top) (make-instance 'compound))
  (top-compound.create (window-top self))
  (window.create self (object-width self) (object-height self) (window-top self))
  (register self)
  (window.set-title (window-title self)))

(defgeneric close-window (self)
  (:method   ((self window))
    (window.close self)
    (unregister self)))

(defgeneric request-focus (self)
  (:method   ((self window))
    (window.request-focus self)))

(defgeneric clear-window (self)
  (:method   ((self window))
    (window.clear self)))

(defgeneric repaint-window (self)
  (:method   ((self window))
    (window.repaint self)))

(defmethod set-object-visible ((self window) visible)
  (setf (slot-value self 'visible) visible)
  (window.set-visible self visible))

(defmethod set-window-resizable ((self window))
  (window.set-resizable self))

(defmethod set-window-title ((self window) title)
  (setf (slot-value self 'title) title)
  (window.set-title self title))

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
    (setf (object-color obj) (object-color self))
    (window.draw self obj)))

(defmethod fill-oval ((self window) x y width height)
  (let ((obj (make-instance 'oval :x (double x)
                                  :y (double y)
                                  :width (double width)
                                  :height (double height))))
    (setf (slot-value obj 'filledp) t)
    (setf (object-color obj) (object-color self))
    (window.draw self obj)))

(defmethod draw-rect ((self window) x y width height)
  (let ((obj (make-instance 'rect :x (double x)
                                  :y (double y)
                                  :width (double width)
                                  :height (double height))))
    (setf (object-color obj) (object-color self))
    (window.draw self obj)))

(defmethod fill-rect ((self window) x y width height)
  (let ((obj (make-instance 'rect :x (double x)
                                  :y (double y)
                                  :width (double width)
                                  :height (double height))))
    (setf (slot-value obj 'filledp) t)
    (setf (object-color obj) (object-color self))
    (window.draw self obj)))

(defmethod draw ((self window) (obj object))
  (window.draw self obj))

(defmethod draw-at ((self window) (obj object) x y)
  (set-object-location obj (double x) (double y))
  (window.draw self obj))

(defmethod compound-add ((self window) (obj object))
  (compound-add (window-top self) obj))

(defmethod compound-add-at ((self window) (obj object) x y)
  (set-object-location obj (double x) (double y))
  (compound-add (window-top self) obj))

(defmethod compound-add-to-region ((self window) (obj object) region)
  (window.add-to-region self obj region))

(defmethod compound-remove ((self window) (obj object))
  (compound-remove (window-top self) obj))

(defmethod get-object-at ((self window) x y)
  (get-object-at (window-top self) (double x) (double y)))

(defun screen-width  () (window.get-screen-width))
(defun screen-height () (window.get-screen-height))


(defmethod register   ((self window))
  (setf (gethash (object-id self) *windows*) self))
(defmethod register   ((self interactor))
  (setf (gethash (object-id self) *sources*) self))
(defmethod register   ((self timer))
  (setf (gethash (object-id self) *timers*) self))

(defmethod unregister   ((self window))
  (remhash (object-id self) *windows*))
(defmethod unregister   ((self interactor))
  (remhash (object-id self) *sources*))
(defmethod unregister   ((self timer))
  (remhash (object-id self) *timers*))


(defun wait-for-click ()
  (wait-for-event +click-event+))

(defun wait-for-event (mask)
  (loop :while (queue-empty-p *event-queue*)
        :do (event.wait-for-event mask)
            (get-result))
  (queue-dequeue *event-queue*))

(defun get-next-event (mask)
  (when (queue-empty-p *event-queue*)
    (event.get-next-event mask)
    (get-result))
  (unless (queue-empty-p *event-queue*)
    (queue-dequeue *event-queue*)))



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


(defun test/all ()
  (test/string-escape)
  (test/scanner))

(test/all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-(and)
(progn

  (open-backend :program-name "Test Program")
  (defparameter *w* (make-instance 'window :title "Test Window"
                                           :height 100.0d0
                                           :width 200.0d0
                                           :x 50.0d0
                                           :y 50.0d0))
  (compound-add *w* (make-instance 'label :text "Label:"
                                          :x 10 :y 10 :width 100 :height 20))
  )


