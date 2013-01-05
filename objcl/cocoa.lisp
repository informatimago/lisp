;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cocoa.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Some fun with Objective-C and Cocoa.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-12-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2012
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "OBJCL" "OCLO" "NS"))
(defpackage "COCOA-PLAYGROUND"
  (:use "CL" "COM.INFORMATIMAGO.LAYOUT")
  (:import-from "OBJCL" "@"))
(in-package "COCOA-PLAYGROUND")

(objcl:set-objective-cl-syntax)
(oclo:define-classname-translation "NSApplication" ns:ns-application)

;; (eval-when (:execute :compile-toplevel :load-toplevel)
;;   ;; oclo:*COCOA-APPLICATION-FRAMEWORKS*
;;  (oclo:load-framework  "Cocoa"  :cocoa)
;;  (oclo:UPDATE-OBJC-METHOD-INFO))



(eval-when (:execute :compile-toplevel :load-toplevel)
  @[nsview subclass:icals
           slots: ((stack :initform (make-array 4) :accessor icals-stack))])


@[icals classmethod:(newviewwithframe:(<nsr>ect)frame)
  resulttype:(:id)
  body: [[icals alloc] initwithframe:frame]]


;; @[Icals method:(drawRect:(<NSR>ect)rect) resultType:(:void)
;;         body: (let ((context [NSGraphicsContext currentContext])
;;                     (bounds  (oclo:stret [self bounds])))
;;                 (unwind-protect
;;                      (progn
;;                        [context saveGraphicsState]
;;                        
;;                        )
;;                   [context restoreGraphicsState]))]

;; For test-window:
@[icals method:(greet:(:id)sender)
        resulttype:(:id)
        body:
        (declare (ignore sender))
        (#_nsrunalertpanel @"Icals"
                           @"Hello, World!"
                           @"OK"
                           @"Welcome!"
                           @"Go Away")]


@[icals method:(initwithframe:(<nsr>ect)frame)
        resulttype:(:id)
        body: (when (setf self [super initwithframe:frame])
                (create-interface self)
                self)]

@[icals method:(setstackfield:(:id #|NSTextField*|#)field atindex:(:int)index)
        resulttype:(:void)
        body: (when (<= 0 index 3)
                (setf (aref (icals-stack self) index) field))]


@[icals method:(pressdigit:(:id)sender)
        resulttype:(:id)
        body:
        (let ((acc (aref (icals-stack self) 0)))
          [acc setintvalue:(+ [sender intvalue] (* 10 [acc intvalue]))])]

  

@[icals method: (reporterror:(:id #|<NSS>tring|#)text)
        resulttype:(:id)
        body:
        nslog(@"Icals error: %@" text)]

@[icals method:(pressenter:(:id)sender)
        resulttype:(:id)
        body:
        (declare (ignore sender))
        (loop
           :for i :from 3 :above 0
           :do [(aref (icals-stack self) i) setintvalue:[(aref (icals-stack self) (1- i)) intvalue]]
           :finally [(aref (icals-stack self) 0) setintvalue:0])]

@[icals method:(presspop:(:id)sender)
        resulttype:(:id)
        body:
        (declare (ignore sender))
        (loop
           :for i :from 0 :below 3
           :do [(aref (icals-stack self) i) setintvalue:[(aref (icals-stack self) (1+ i)) intvalue]])]

@[icals method:(pressoperation:(:id)sender)
        resulttype:(:id)
        body:
        (let ((right (aref (icals-stack self) 0))
              (left  (aref (icals-stack self) 1))
              (op    (cdr (assoc (objc:lisp-string-from-nsstring  [sender stringvalue])
                                 '(("+" . +)
                                   ("-" . -)
                                   ("*" . *)
                                   ("/" . /))
                                 :test (function string=)))))
          (if op
              (progn
                [self presspop:sender]
                [(aref (icals-stack self) 0) setdoublevalue:(or (ignore-errors (funcall op left right)) 0)])
              [self reporterror:@"Invalid Operation Button"]))]




(defun create-interface (ical)
  ;; 7 8 9 /     [        ]
  ;; 4 5 6 *     [        ]
  ;; 1 2 3 -     [        ]
  ;; P 0 E +     [        ]
  (let ((size (make-rect)))
    (flet ((make-button (title action)
             (let ((button (make-push-button :title title :action action :target ical)))
               [button sizetofit] 
               (oclo:slet ((frame  [button frame]))
                           (setf size  (rect-union size frame))
                           (cons button frame))))
           (make-field (value)
             (let ((field (make-text-field :frame (make-rect :x 0 :y 0 :width 120 :height 32))))
               [field setstringvalue:value]
               (oclo:slet ((frame [field frame]))
                           (cons field frame)))))
      (let ((digits (loop
                       :for i :from 0 :to 9
                       :collect (make-button (objc:make-nsstring (princ-to-string i))
                                             (objc:@selector "pressDigit:"))))
            (ops    (loop
                       :for op :in '("+" "-" "*" "/")
                       :collect (make-button (objc:make-nsstring op)
                                             (objc:@selector "pressOperation:"))))
            (pop    (make-button @"P" (objc:@selector "pressPop:")))
            (enter  (make-button @"E" (objc:@selector "pressEnter:")))
            (fields (loop :repeat 4 :collect (make-field @"0"))))

        (loop
           :for (object . frame) :in (list* enter pop (append ops digits))
           :do (setf (rect-size frame) (rect-size size)))
        
        (setf (rect-origin (cdr pop)) (make-point :x 20 :y 20))
        (stack-up (mapcar (function cdr) (list pop (elt digits 1) (elt digits 4) (elt digits 7)))
                  :align :center :spacing 6)
        (place (elt digits 0) (right-of pop 6))
        (stack-up (mapcar (function cdr) (list (elt digits 0) (elt digits 2) (elt digits 5) (elt digits 8)))
                  :align :center :spacing 6)
        (place enter (right-of (elt digits 0) 6))
        (stack-up (mapcar (function cdr) (list enter (elt digits 3) (elt digits 6) (elt digits 9)))
                  :align :center :spacing 6)
        (place (first ops) (right-of enter 6))
        (stack-up (mapcar (function cdr) ops)
                  :align :center :spacing 6)
        (place (first fields) (right-of (first ops) 12))
        (stack-up (mapcar (function cdr) fields)
                  :align :center :spacing 6)

        (loop
           :for (object . frame) :in (list* enter pop (append ops digits fields))
           :do (progn
                 [object setframe:frame]
                 [ical addsubview:object]))
        (loop
           :for i :from 0
           :for (field . frame) :in fields
           :do [ical setstackfield:field atindex:i])))))






(defconstant yes 1)
(defconstant no  0)
(defconstant +default-autoresizing-mask+ (logior #$nsviewminxmargin
                                                 #$nsviewwidthsizable
                                                 #$nsviewminymargin
                                                 #$nsviewheightsizable))



(defun make-window (&key (title @"Untitled")
                    (frame (ns:make-ns-rect 0 0 200 100))
                    (style-mask (logior #$nstitledwindowmask #$nsresizablewindowmask))
                    (order-front nil))
  (let ((window [[nswindow alloc]
                 initwithcontentrect:frame
                 stylemask:style-mask
                 backing:#$nsbackingstorebuffered
                 defer:nil]))
    [window settitle:title]
    [window setautodisplay:t]
    [window setviewsneeddisplay:yes]
    [window flushwindowifneeded]
    (when order-front
      [window orderfront:nil])
    window))

(defun close-window (window)
  [window close]
  [window dealloc])



(defun make-text-field (&key (frame (ns:make-ns-rect 20 50 120 32))
                        (title @"Field")
                        (autoresizing-mask +default-autoresizing-mask+)
                        target action
                        (editable t)
                        (selectable t))
  (let ((field [[nstextfield alloc] initwithframe:frame]))
    [field setimageposition:#$nsnoimage]
    [field settitle:title]
    [field setautoresizingmask:autoresizing-mask]
    [field setenabled:yes]
    [field seteditable:editable]
    [field setselectable:selectable]
    (when target [field settarget:target])
    (when action [field setaction:action])
    field))

(defun make-push-button (&key (frame (ns:make-ns-rect 20 50 120 32))
                         (title @"Button")
                         (autoresizing-mask +default-autoresizing-mask+)
                         target action)
  (let ((button [[nsbutton alloc] initwithframe:frame]))
    [button setbuttontype:#$nsmomentarypushinbutton]
    [button setimageposition:#$nsnoimage]
    [button setbezelstyle:#$nsroundedbezelstyle]
    [button settitle:title]
    [button setautoresizingmask:autoresizing-mask]
    [button setenabled:yes]
    (when target [button settarget:target])
    (when action [button setaction:action])
    button))


(defun make-text-field (&key (frame (ns:make-ns-rect 20 50 120 32))
                        value))

(defun make-icals-window (&key (title @"Untitled")
                          (frame (ns:make-ns-rect 0 0 200 100))
                          (style-mask (logior #$nstitledwindowmask #$nsresizablewindowmask))
                          (order-front nil))
  (let ((window (make-window :title title
                             :frame frame
                             :style-mask style-mask
                             :order-front order-front))
        (view   [icals newviewwithframe:frame]))
    [window setcontentview:view]
    [view addsubview: (make-push-button :title @"Hello"
                                        :target view
                                        :action (objc:@selector "greet:"))]
    window))





(defvar *window* nil)

(defun make-test-window ()
  (close-test-window)
  (setf *window* (make-icals-window :frame (ns:make-ns-rect 0 0 200 100)
                                    :title @"Test Window"
                                    :order-front t)))

(defun close-test-window ()
  (when *window* (close-window *window*))
  (setf  *window* nil))






(defun print-sorted-hash-table-keys (table)
  (let ((keys (make-array (hash-table-count table) :fill-pointer 0))
        (klen 0))
    (maphash (lambda (k v)
               (let ((key (prin1-to-string k)))
                (vector-push (cons key (prin1-to-string v)) keys)
                (setf klen (max klen (length key)))))
             table)
    (loop
       :for line :across (sort keys (function string<) :key (function car))
       :do (format t "~VA ~A~%" klen (car line) (cdr line)))))

#-(and)
(progn
  (print-sorted-hash-table-keys (ccl::objc-class-map))
  (print-sorted-hash-table-keys ccl::*lisp-classname-table*)
  (print-sorted-hash-table-keys ccl::*objc-classname-table*))


;; [*window* setFrame:(NS:MAKE-NS-RECT 0 80 400 200) display:t animate:t]
;; 
;; (defun update ()
;;   [[*window* contentView] setNeedsDisplay:YES]
;;   [*window* setViewsNeedDisplay:YES]
;;   [*window* display]
;;   [*window* update]
;;   [*window* flushWindowIfNeeded]
;;   [[NSApplication sharedApplication] updateWindows])

;; [NSApplication sharedApplication]
;; [[NSApplication sharedApplication] updateWindows]
;; [[NSApplication sharedApplication] currentEvent]
;; [[NSApplication sharedApplication] currentSystemPresentationOptions]




;;;; THE END ;;;;
