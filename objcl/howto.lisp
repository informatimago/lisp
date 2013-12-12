;;; This file contains the equivalent of the "UI Elements HOWTO",
;;; written with the ObjCL reader macros.


;; UI Elements HOWTO
;; 
;; This HOWTO shows how you can create Cocoa user-interface elements
;; by making lisp calls to instantiate and initialize Objective-C
;; objects.
;; 
;; Cocoa programmers usually create UI elements using Apple's
;; InterfaceBuilder application, and then load those elements from a 
;; nibfile, but Cocoa supports creating all the same UI elements by
;; making Objective-C method calls. In fact, that's how it loads
;; nibfiles: by making method calls to instantiate the objects
;; described in them.
;; 
;; For Lisp programmers, accustomed to working incrementally and
;; interactively, it may sometimes make more sense to create
;; user-interface elements by making method calls interactively,
;; rather than by constructing a complete user interface in
;; InterfaceBuilder. This HOWTO shows how you can use Objective-C
;; method calls to create and display windows and other UI elements.
;; 
;; For more information about how to load nibfiles from Lisp, see the
;; "nib-loading" example. For a complete discussion of how to
;; construct a Cocoa application using nibfiles created with
;; InterfaceBuilder, see the "currency-converter" example.
;; 
;; Creating a Window
;; 
;; Every user-interface element under Mac OS X appears either in a
;; window or in a menu. We'll begin by exploring how to create and
;; display windows.
;; 
;; First, switch to the CCL package, for convenience. Most of Clozure
;; CL's Objective-C utilities are in the CCL package:
;; 
;; ? (in-package :ccl)
;; #<Package "CCL">

#| This is bad practice.  Instead we will work in our own package,
   that will use the objcl, oclo and ns packages.  |#

(defpackage "COCOA-HOWTO"
  (:use "CL"))
(in-package "COCOA-HOWTO")

(objcl:set-objective-cl-syntax)


;; Creating a Cocoa window follows the common Objective-C pattern of
;; allocating an object and then initializing it with some starting
;; data. To allocate a window, just call the alloc method of the
;; NSWindow class:
;; 
;; ? (setf my-window (#/alloc (@class ns-window)))
;; #<NS-WINDOW <NSWindow: 0x13b68580> (#x13B68580)>
;; 
;; The above expression creates a new window, but doesn't display it.
;; Before it shows up on the screen we must initialize it with some
;; appropriate values. For that, we'll use the method
;; initWithContentRect:styleMask:backing:defer:.
;; 
;; As always in Objective-C, the name of the method reveals something
;; about the arguments it expects. The NSRect that we pass for the
;; initWithContentRect: segment of the method name describes the
;; shape of the window. The mask for styleMask: is a sequence of bits
;; that specify which window features are turned on. The backing:
;; argument is a constant of type NSBackingStoreType that specifies
;; how Cocoa will draw the contents of the window. Finally, the
;; defer: argument is a Boolean that determines whether to display
;; the window as soon as it's created.
;; 
;; Next, we'll create data values to pass in these parameters, so
;; that we can display our new window on the screen. We'll build the
;; proper initialization form up piece-by-piece.
;; 
;; The first argument, of course, is the window object to be
;; initialized. We pass the window that we created before:
;; 
;; (#/initWithContentRect:styleMask:backing:defer: my-window ...)
;; 
;; The next argument, the NSRect, is a structure that we need only
;; temporarily. Because NSRect values appear so often in Cocoa code,
;; Clozure CL provides a handy way to allocate them temporarily,
;; disposing of them automatically. The with-ns-rect macro (in the NS
;; package) creates an NSRect value, and then disposes of it when
;; control leaves the scope of the macro; for example:
;; 
;; (ns:with-ns-rect (r 100 100 400 300)
;;    ...)
;; 
;; We can use this rectangle to initialize the shape of our new
;; window:
;; 
;; (ns:with-ns-rect (r 100 100 400 300)
;;    (#/initWithContentRect:styleMask:backing:defer: 
;;     my-window 
;;     r 
;;     ...))
;; 
;; To specify the window features we want, we must combine several
;; flags to form the proper style mask. Cocoa provides named
;; constants for each of the various window features. To create the
;; syle mask that describes a new window, use inclusive-or to combine
;; the named flags into a style mask:
;; 
;; (logior  #$NSTitledWindowMask 
;;          #$NSClosableWindowMask  
;;          #$NSMiniaturizableWindowMask 
;;          #$NSResizableWindowMask)
;; 
;; You can find definitions for all the window masks in the Apple
;; Developer documentation for NSWindow Constants.
;; 
;; Passing the window mask as the next argument gives us this
;; expression:
;; 
;; (ns:with-ns-rect (r 100 100 400 300)
;;   (#/initWithContentRect:styleMask:backing:defer: 
;;    my-window 
;;    r 
;;    (logior  #$NSTitledWindowMask 
;;             #$NSClosableWindowMask  
;;             #$NSMiniaturizableWindowMask 
;;             #$NSResizableWindowMask)
;;    ...))
;;
;; Like the style masks, the NSBackingStoreType value is a named
;; constant that describes which drawing strategy Cocoa should use
;; for the contents of the window. The value can be
;; NSBackingStoreRetained, NSBackingStoreNonretained, or
;; NSBackingStoreBuffered. For this example, we'll use
;; NSBackingStoreBuffered:
;; 
;; (ns:with-ns-rect (r 100 100 400 300)
;;   (#/initWithContentRect:styleMask:backing:defer: 
;;    my-window 
;;    r 
;;    (logior  #$NSTitledWindowMask 
;;             #$NSClosableWindowMask  
;;             #$NSMiniaturizableWindowMask 
;;             #$NSResizableWindowMask)
;;    #$NSBackingStoreBuffered
;;    ...))
;; 
;; Finally, the defer argument is just a Boolean. If we pass a true
;; value, Cocoa will defer displaying the window until we explicitly
;; tell it to. If we pass a False value, it will instead display the
;; window right away. We can pass the Lisp values T or NIL, and the
;; Objective-C bridge automatically converts them for us, but in the
;; spirit of using Objective-C values for Objective-C operations,
;; let's use the Objective-C constants #$YES and #$NO:
;; 
;; (ns:with-ns-rect (r 100 100 400 300)
;;   (#/initWithContentRect:styleMask:backing:defer: 
;;    my-window 
;;    r 
;;    (logior  #$NSTitledWindowMask 
;;             #$NSClosableWindowMask  
;;             #$NSMiniaturizableWindowMask 
;;             #$NSResizableWindowMask)
;;    #$NSBackingStoreBuffered
;;    #$NO))
;; 
;; There; the expression to initialize our window object is finally
;; complete. We can evaluate it in the Listener to initialize the
;; window:
;; 
;; (ns:with-ns-rect (r 100 100 400 300)
;;   (#/initWithContentRect:styleMask:backing:defer: 
;;    my-window 
;;    r 
;;    (logior  #$NSTitledWindowMask 
;;             #$NSClosableWindowMask  
;;             #$NSMiniaturizableWindowMask 
;;             #$NSResizableWindowMask)
;;    #$NSBackingStoreBuffered
;;    #$NO))


#| Just write: |#

(defvar *my-window* [[NSWindow alloc]
                     initWithContentRect:(ns:make-ns-rect 100 100 400 300)
                     styleMask:(logior  #$NSTitledWindowMask 
                                        #$NSClosableWindowMask  
                                        #$NSMiniaturizableWindowMask 
                                        #$NSResizableWindowMask)
                     backing:#$NSBackingStoreBuffered
                     defer:nil])

[*my-window* setTitle: "Hello World"]

;; Then we can call makeKeyAndOrderFront: to display the window:
;; 
;; (#/makeKeyAndOrderFront: my-window nil)

[*my-window* makeKeyAndOrderFront:nil]


;; The window, empty, but with the shape and features we specified,
;; appears on the left lower corner of the screen.
;; 
;; Adding a Button
;; 
;; Once we have a window on the screen, we might like to put
;; something in it. Let's start by adding a button.
;; 
;; Creating a button object is as simple as creating a window object;
;; we simply allocate one:
;; 
;; (setf my-button (#/alloc ns:ns-button))
;; #<NS-BUTTON <NSButton: 0x13b7bec0> (#x13B7BEC0)>
;; 
;; As with the window, most of the interesting work is in configuring
;; the allocated button after it's allocated.
;; 
;; Instances of NSButton include pushbuttons with either text or
;; image labels (or both), checkboxes, and radio buttons. In order to
;; make a text pushbutton, we need to tell our button to use a
;; button-type of NSMomentaryPushInButton, an image position of
;; NSNoImage, and a border style of NSRoundedBezelStyle. These style
;; options are represented by Cocoa constants.
;; 
;; We also need to give the button a frame rectangle that defines its
;; size and position. We can once again use ns:with-ns-rect to
;; specify a temporary rectangle for the purpose of initializing our
;; button:
;; 
;; (ns:with-ns-rect (frame 10 10 72 32)
;;   (#/initWithFrame: my-button frame)
;;   (#/setButtonType: my-button #$NSMomentaryPushInButton)
;;   (#/setImagePosition: my-button #$NSNoImage)
;;   (#/setBezelStyle: my-button #$NSRoundedBezelStyle))
;; ;Compiler warnings :
;; ;   Undeclared free variable MY-BUTTON (4 references), in an anonymous lambda form
;; NIL
;;
;; Now we just need to add the button to the window. This we do by
;; asking the window for its content view, and asking that view to
;; add the button as a subview:
;; 
;; (#/addSubview: (#/contentView my-window) my-button)


#| Again, it's much simplier to write just: |#

(defparameter *my-button*
  (let ((button [[NSButton alloc] initWithFrame:(ns:make-ns-rect 10 50 72 32)]))
   [button setButtonType:#$NSMomentaryPushInButton]
   [button setImagePosition:#$NSNoImage]
   [button setBezelStyle:#$NSRoundedBezelStyle]
   [[*my-window* contentView] addSubview:button]
   button))

 
;; The button appears in the window with the rather uninspired title
;; "Button". Clicking it highlights the button but, since we didn't
;; give it any action to perform, does nothing else.
;; 
;; We can give the button a more interesting title and, perhaps more
;; importantly, an action to perform, by passing a string and an
;; action to it. First, let's set the button title:
;; 
;; (let ((label (%make-nsstring "Hello!")))
;;   (#/setTitle: my-button label)
;;   (#/release label))
;; ;Compiler warnings :
;; ;   Undeclared free variable MY-BUTTON, in an anonymous lambda form
;; NIL

[*my-button* setTitle:@"Hello!"]

;; The button changes to display the text "Hello!". Notice that we
;; are careful to save a reference to the button text and release it
;; after changing the button title. The normal memory-management
;; policy in Cocoa is that if we allocate an object (like the
;; NSString "Hello!") we are responsible for releasing it. Unlike
;; Lisp, Cocoa does not automatically garbage-collect all allocated
;; objects by default.
;; 
;; Giving the button an action is slightly more complicated. Clicking
;; a button causes the button object to send a message to a target
;; object. We haven't given our button a message to send, nor a
;; target object to send it to, so it doesn't do anything. In order
;; to get it do perform some kind of action, we need to give it a
;; target object and a message to send. Then, when we click the
;; button, it will send the message we specify to the target we
;; provide. Naturally, the target object had better be able to
;; respond to the message, or else we'll just see a runtime error.
;; 
;; Let's define a class that knows how to respond to a greeting
;; message, and then make an object of that class to serve as our
;; button's target.
;; 
;; We can define a subclass of NSObject to handle our button's
;; message:
;; 
;; (defclass greeter (ns:ns-object)
;;   ()
;;   (:metaclass ns:+ns-object))
;; #<OBJC:OBJC-CLASS GREETER (#x13BAF810)>


#|
   To create classes, class method and instance methods, we have
   pseudo messages that can be sent to the class objects (like in
   Smalltalk or Ruby).
|#

@[NSObject subClass:Greeter slots: ()]



;; We'll need to define a method to execute in response to the
;; button's message. Action methods accept one argument (in addition
;; to the receiver): a sender. Normally Cocoa passes the button
;; object itself as the sender argument; the method can do anything
;; it likes (or nothing at all) with the sender.
;; 
;; Here's a method that displays an alert dialog:
;; 
;; (objc:defmethod #/greet: ((self greeter) (sender :id))
;;   (declare (ignore sender))
;;   (let ((title (%make-nsstring "Hello!"))
;;         (msg (%make-nsstring "Hello, World!"))
;;         (default-button (%make-nsstring "Hi!"))
;;         (alt-button (%make-nsstring "Hello!"))
;;         (other-button (%make-nsstring "Go Away")))
;;     (#_NSRunAlertPanel title msg default-button alt-button other-button)
;;     (#/release title)
;;     (#/release msg)
;;     (#/release default-button)
;;     (#/release other-button)))

@[Greeter method:(greet:(:id)sender)
          resultType:(:id)
          body:
          (declare (ignore sender))
          (#_NSRunAlertPanel @"Hello!"
                             @"Hello, World!"
                             @"Hi!"
                             @"Hello!"
                             @"Go Away")
          oclo:*null*]


;; Now we can create an instance of the Greeter class and use it as
;; the button's target:
;; 
;; (setf my-greeter (#/init (#/alloc greeter)))
;; #<GREETER <Greeter: 0x136c58e0> (#x136C58E0)>

(defparameter *my-greeter* [[Greeter alloc] init])


;; (#/setTarget: my-button my-greeter)
;; NIL
;; (#/setAction: my-button (@SELECTOR "greet:"))
;; NIL


[*my-button* setTarget:*my-greeter*]
[*my-button* setAction:(objc:@selector "greet:")]
 
;; Now, if you click the button, an Alert panel appears.


#|

;;; In summary:

    (defpackage "COCOA-HOWTO"
      (:use "CL"))
    (in-package "COCOA-HOWTO")

    (objcl:set-objective-cl-syntax)

    (defvar *my-window* [[NSWindow alloc]
                         initWithContentRect:(ns:make-ns-rect 100 100 400 300)
                         styleMask:(logior  #$NSTitledWindowMask 
                                            #$NSClosableWindowMask  
                                            #$NSMiniaturizableWindowMask 
                                            #$NSResizableWindowMask)
                         backing:#$NSBackingStoreBuffered
                         defer:nil])

    [*my-window* makeKeyAndOrderFront:nil]

    (defparameter *my-button*
      (let ((button [[NSButton alloc] initWithFrame:(ns:make-ns-rect 10 50 72 32)]))
       [button setButtonType:#$NSMomentaryPushInButton]
       [button setImagePosition:#$NSNoImage]
       [button setBezelStyle:#$NSRoundedBezelStyle]
       [[*my-window* contentView] addSubview:button]
       button))
    [*my-button* setTitle:@"Hello!"]



    @[NSObject subClass:Greeter slots: ()]

    @[Greeter method:(greet:(:id)sender)
              resultType:(:id)
              body:
              (declare (ignore sender))
              (#_NSRunAlertPanel @"Hello!"
                                 @"Hello, World!"
                                 @"Hi!"
                                 @"Hello!"
                                 @"Go Away")]

    (defparameter *my-greeter* [[Greeter alloc] init])

    [*my-button* setTarget:*my-greeter*]
    [*my-button* setAction:(objc:@selector "greet:")]


;; Notice: classes need to be defined before they can be read in [...]
;; forms.


|#

;;;; THE END ;;;;
