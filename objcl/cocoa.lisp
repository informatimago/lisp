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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "OBJCL" "OCLO" "NS"))
(defpackage "COCOA-PLAYGROUND"
  (:use "CL" "COM.INFORMATIMAGO.LAYOUT")
  (:import-from "OBJCL" "@"))
(in-package "COCOA-PLAYGROUND")

(objcl:set-objective-cl-syntax)
(oclo:define-classname-translation "NSApplication" NS:NS-APPLICATION)

;; (eval-when (:execute :compile-toplevel :load-toplevel)
;;   ;; oclo:*COCOA-APPLICATION-FRAMEWORKS*
;;  (oclo:load-framework  "Cocoa"  :cocoa)
;;  (oclo:UPDATE-OBJC-METHOD-INFO))



(eval-when (:execute :compile-toplevel :load-toplevel)
  @[NSView subClass:Icals
           slots: ((stack :initform (make-array 4) :accessor icals-stack))])


@[Icals classMethod:(newViewWithFrame:(<NSR>ect)frame)
  resultType:(:id)
  body: [[Icals alloc] initWithFrame:frame]]


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
@[Icals method:(greet:(:id)sender)
        resultType:(:id)
        body:
        (declare (ignore sender))
        (#_NSRunAlertPanel @"Icals"
                           @"Hello, World!"
                           @"OK"
                           @"Welcome!"
                           @"Go Away")]


@[Icals method:(initWithFrame:(<NSR>ect)frame)
        resultType:(:id)
        body: (when (setf self [super initWithFrame:frame])
                (create-interface self)
                self)]

@[Icals method:(setStackField:(:id #|NSTextField*|#)field atIndex:(:int)index)
        resultType:(:void)
        body: (when (<= 0 index 3)
                (setf (aref (icals-stack self) index) field))]


@[Icals method:(pressDigit:(:id)sender)
        resultType:(:id)
        body:
        (let ((acc (aref (icals-stack self) 0)))
          [acc setIntValue:(+ [sender intValue] (* 10 [acc intValue]))])]

  

@[Icals method: (reportError:(:id #|<NSS>tring|#)text)
        resultType:(:id)
        body:
        NSLog(@"Icals error: %@" text)]

@[Icals method:(pressEnter:(:id)sender)
        resultType:(:id)
        body:
        (declare (ignore sender))
        (loop
           :for i :from 3 :above 0
           :do [(aref (icals-stack self) i) setIntValue:[(aref (icals-stack self) (1- i)) intValue]]
           :finally [(aref (icals-stack self) 0) setIntValue:0])]

@[Icals method:(pressPop:(:id)sender)
        resultType:(:id)
        body:
        (declare (ignore sender))
        (loop
           :for i :from 0 :below 3
           :do [(aref (icals-stack self) i) setIntValue:[(aref (icals-stack self) (1+ i)) intValue]])]

@[Icals method:(pressOperation:(:id)sender)
        resultType:(:id)
        body:
        (let ((right (aref (icals-stack self) 0))
              (left  (aref (icals-stack self) 1))
              (op    (cdr (assoc (objc:lisp-string-from-nsstring  [sender stringValue])
                                 '(("+" . +)
                                   ("-" . -)
                                   ("*" . *)
                                   ("/" . /))
                                 :test (function string=)))))
          (if op
              (progn
                [self pressPop:sender]
                [(aref (icals-stack self) 0) setDoubleValue:(or (ignore-errors (funcall op left right)) 0)])
              [self reportError:@"Invalid Operation Button"]))]




(defun create-interface (ical)
  ;; 7 8 9 /     [        ]
  ;; 4 5 6 *     [        ]
  ;; 1 2 3 -     [        ]
  ;; P 0 E +     [        ]
  (let ((size (make-rect)))
    (flet ((make-button (title action)
             (let ((button (make-push-button :title title :action action :target ical)))
               [button sizeToFit] 
               (oclo:slet ((frame  [button frame]))
                           (setf size  (rect-union size frame))
                           (cons button frame))))
           (make-field (value)
             (let ((field (make-text-field :frame (make-rect :x 0 :y 0 :width 120 :height 32))))
               [field setStringValue:value]
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
                 [object setFrame:frame]
                 [ical addSubview:object]))
        (loop
           :for i :from 0
           :for (field . frame) :in fields
           :do [ical setStackField:field atIndex:i])))))






(defconstant yes 1)
(defconstant no  0)
(defconstant +default-autoresizing-mask+ (logior #$NSViewMinXMargin
                                                 #$NSViewWidthSizable
                                                 #$NSViewMinYMargin
                                                 #$NSViewHeightSizable))



(defun make-window (&key (title @"Untitled")
                    (frame (ns:make-ns-rect 0 0 200 100))
                    (style-mask (logior #$NSTitledWindowMask #$NSResizableWindowMask))
                    (order-front nil))
  (let ((window [[NSWindow alloc]
                 initWithContentRect:frame
                 styleMask:style-mask
                 backing:#$NSBackingStoreBuffered
                 defer:nil]))
    [window setTitle:title]
    [window setAutodisplay:t]
    [window setViewsNeedDisplay:YES]
    [window flushWindowIfNeeded]
    (when order-front
      [window orderFront:nil])
    window))

(defun close-window (window)
  [window close]
  [window dealloc])



(defun make-text-field (&key (frame (NS:MAKE-NS-RECT 20 50 120 32))
                        (title @"Field")
                        (autoresizing-mask +default-autoresizing-mask+)
                        target action
                        (editable t)
                        (selectable t))
  (let ((field [[NSTextField alloc] initWithFrame:frame]))
    [field setImagePosition:#$NSNoImage]
    [field setTitle:title]
    [field setAutoresizingMask:autoresizing-mask]
    [field setEnabled:yes]
    [field setEditable:editable]
    [field setSelectable:selectable]
    (when target [field setTarget:target])
    (when action [field setAction:action])
    field))

(defun make-push-button (&key (frame (NS:MAKE-NS-RECT 20 50 120 32))
                         (title @"Button")
                         (autoresizing-mask +default-autoresizing-mask+)
                         target action)
  (let ((button [[NSButton alloc] initWithFrame:frame]))
    [button setButtonType:#$NSMomentaryPushInButton]
    [button setImagePosition:#$NSNoImage]
    [button setBezelStyle:#$NSRoundedBezelStyle]
    [button setTitle:title]
    [button setAutoresizingMask:autoresizing-mask]
    [button setEnabled:yes]
    (when target [button setTarget:target])
    (when action [button setAction:action])
    button))


(defun make-text-field (&key (frame (NS:MAKE-NS-RECT 20 50 120 32))
                        value))

(defun make-icals-window (&key (title @"Untitled")
                          (frame (ns:make-ns-rect 0 0 200 100))
                          (style-mask (logior #$NSTitledWindowMask #$NSResizableWindowMask))
                          (order-front nil))
  (let ((window (make-window :title title
                             :frame frame
                             :style-mask style-mask
                             :order-front order-front))
        (view   [Icals newViewWithFrame:frame]))
    [window setContentView:view]
    [view addSubview: (make-push-button :title @"Hello"
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
