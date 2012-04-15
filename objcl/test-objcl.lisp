;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-objcl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests the objcl reader macro parser functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-12-14 <PJB> Created.
;;;;BUGS
;;;;    Should add more tests for error cases.
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

(in-package "COM.INFORMATIMAGO.OBJECTIVE-CL")



;; When reading expressions in the tests, we need to set the package
;; to ensure the symbols are read in the expected package.


(defmacro with-string-check ((readtable stream string) &body body)
  `(with-input-from-string (,stream ,string)
     (let ((*readtable* ,readtable)) (locally ,@body))))


(define-test test/read-identifier ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
   (test string=
         (with-string-check (*objc-readtable*
                             stream "hello42World:")
           (read-identifier stream))
         "hello42World")))


(define-test test/read-type-specifier ()
  "
    type-specifier :='(' type-identifier ')' .
"
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "(int)arg")
           (read-type-specifier stream))
         :int)))


(define-test test/read-method-signature ()
  "
    signature          := simple-signature | compound-signature final-signature .

    simple-signature   := objcl-identifier .
    compound-signature := [objcl-identifier] ':' '(' type-identifier ')' objcl-identifier compound-signature
                        | [objcl-identifier] ':' '(' type-identifier ')' objcl-identifier .
"
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "simpleSelector)")
           (read-method-signature stream))
         '("simpleSelector" NIL nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "singleArgComplexSelector:(int)arg)")
           (read-method-signature stream))
         '("singleArgComplexSelector:" ((:int arg)) NIL))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:(int)arg1 complexSelector:(int)arg2)")
           (read-method-signature stream))
         '("multipleArg:complexSelector:" ((:int arg1) (:int arg2)) NIL))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArgWithEmptyPart:(int)arg1 :(int)arg2)")
           (read-method-signature stream))
         '("multipleArgWithEmptyPart::" ((:int arg1) (:int arg2)) NIL))

   (test equal
         (with-string-check (*objc-readtable*
                             stream "singleArgComplexSelector:(int)arg &rest others)")
           (read-method-signature stream))
         '("singleArgComplexSelector:" ((:int arg)) others))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:(int)arg1 complexSelector:(int)arg2  &rest others)")
           (read-method-signature stream))
         '("multipleArg:complexSelector:" ((:int arg1) (:int arg2)) others))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArgWithEmptyPart:(int)arg1 :(int)arg2  &rest others)")
           (read-method-signature stream))
         '("multipleArgWithEmptyPart::" ((:int arg1) (:int arg2)) others))))



(define-test test/read-final-arguments ()
  "
    final-arguments    := | '(' type-identifier ')' objcl-expression  final-arguments .
    type-identifier    := symbol .
"
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "]")
           (read-final-arguments stream))
         '())
   (test equal
         (with-string-check (*objc-readtable*
                             stream "(integer)(+ one 2)]")
           (read-final-arguments stream))
         '((:INTEGER (+ one 2))))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "(integer)(+ 1 2) (float)(+ 1.0 2.0)]")
           (read-final-arguments stream))
         '((:INTEGER (+ 1 2)) (:FLOAT (+ 1.0 2.0))))
   (test eql
         (handler-case
             (progn
               (with-string-check (*objc-readtable*
                                   stream "(integer)(+ 1 2) (float)]")
                 (read-final-arguments stream))
               nil)
           (error () :success))
         :success)))



(define-test test/read-message ()
  "
    message            := simple-selector | compound-selector final-arguments .

    simple-selector    := objcl-identifier .
    compound-selector  := [objcl-identifier] ':' objcl-expression compound-selector
                        | [objcl-identifier] ':' objcl-expression .
"
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "simpleSelector]")
           (read-message stream))
         '("simpleSelector" NIL nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "singleArgComplexSelector:42]")
           (read-message stream))
         '("singleArgComplexSelector:" (42) NIL))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:42 complexSelector:24]")
           (read-message stream))
         '("multipleArg:complexSelector:" (42 24) NIL))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:(+ 4 2) complexSelector:(* 2 4)]")
           (read-message stream))
         '("multipleArg:complexSelector:" ((+ 4 2) (* 2 4)) NIL))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:[self one] complexSelector:[self two]]")
           (read-message stream))
         `("multipleArg:complexSelector:"
           (,(generate-message-send 'SELF '"one" 'nil 'nil)
             ,(generate-message-send 'SELF '"two" 'nil 'nil))
           NIL))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArgWithEmptyPart:42 :24]")
           (read-message stream))
         '("multipleArgWithEmptyPart::" (42 24) NIL))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "singleArgComplexSelectorWithFinalArgs:42
                                                    (int)1]")
           (read-message stream))
         '("singleArgComplexSelectorWithFinalArgs:" (42) ((:INT 1))))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:42
                               complexSelectorWithFinalArgs:24
                                             (int)1 (float)2.0]")
           (read-message stream))
         '("multipleArg:complexSelectorWithFinalArgs:" (42 24) ((:INT 1) (:float 2.0))))))


(define-test test/read-message-send ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "simpleSelector]")
            (read-message-send stream 'self (function read-message)))
          '(self "simpleSelector" NIL nil))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "singleArgComplexSelector:42]")
            (read-message-send stream 'self (function read-message)))
          '(self "singleArgComplexSelector:" (42) NIL))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArg:42 complexSelector:24]")
            (read-message-send stream 'self (function read-message)))
          '(self "multipleArg:complexSelector:" (42 24) NIL))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArg:(+ 4 2) complexSelector:(* 2 4)]")
            (read-message-send stream 'self (function read-message)))
          '(self "multipleArg:complexSelector:" ((+ 4 2) (* 2 4)) NIL))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArg:[self one] complexSelector:[self two]]")
            (read-message-send stream 'self (function read-message)))
          `(self "multipleArg:complexSelector:"
                 (,(generate-message-send 'SELF '"one" 'nil 'nil)
                   ,(generate-message-send 'SELF '"two" 'nil 'nil))
                 NIL))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArgWithEmptyPart:42 :24]")
            (read-message-send stream 'self (function read-message)))
          '(self "multipleArgWithEmptyPart::" (42 24) NIL))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "singleArgComplexSelectorWithFinalArgs:42
                                                    (int)1]")
            (read-message-send stream 'self (function read-message)))
          '(self "singleArgComplexSelectorWithFinalArgs:" (42) ((:INT 1))))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArg:42
                               complexSelectorWithFinalArgs:24
                                             (int)1 (float)2.0]")
            (read-message-send stream 'self (function read-message)))
          '(self "multipleArg:complexSelectorWithFinalArgs:" (42 24) ((:INT 1) (:float 2.0))))))


(define-test test/message-send ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))                  
   (flet ((gen (args) (apply (function generate-message-send) args)))
     (declare (inline gen))
     (test equal
           (gen '(self "simpleSelector" NIL nil))
           '(oclo:SEND SELF 'SIMPLE-SELECTOR))
     (test equal
           (gen '(self "singleArgComplexSelector:" (42) NIL))
           '(oclo:SEND SELF :SINGLE-ARG-COMPLEX-SELECTOR 42))
     (test equal
           (gen '(self "multipleArg:complexSelector:" (42 24) NIL))
           '(oclo:SEND SELF :MULTIPLE-ARG 42 :COMPLEX-SELECTOR 24))
     (test equal
           (gen '(self "multipleArg:complexSelector:" ((+ 4 2) (* 2 4)) NIL))
           '(oclo:SEND SELF :MULTIPLE-ARG (+ 4 2) :COMPLEX-SELECTOR (* 2 4)))
     (test equal
           (gen `(self "multipleArg:complexSelector:"
                       (,(generate-message-send 'SELF '"one" 'nil 'nil)
                         ,(generate-message-send 'SELF '"two" 'nil 'nil))
                       NIL))
           '(oclo:SEND SELF :MULTIPLE-ARG (oclo:SEND SELF 'ONE) :COMPLEX-SELECTOR (oclo:SEND SELF 'TWO)))
     (test equal
           (gen '(self "multipleArgWithEmptyPart::" (42 24) NIL))
           '(oclo:SEND SELF :MULTIPLE-ARG-WITH-EMPTY-PART 42 :|| 24))
     (test equal
           (gen '(self "singleArgComplexSelectorWithFinalArgs:" (42) ((:INT 1))))
           '(oclo:SEND SELF :SINGLE-ARG-COMPLEX-SELECTOR-WITH-FINAL-ARGS 42 (:INT 1)))
     (test equal
           (gen '(self "multipleArg:complexSelectorWithFinalArgs:" (42 24) ((:INT 1) (:float 2.0))))
           '(oclo:SEND SELF :MULTIPLE-ARG 42 :COMPLEX-SELECTOR-WITH-FINAL-ARGS 24 (:INT 1 :FLOAT 2.0))))))


(define-test test/read-objcl-message-send ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
   (flet ((read-objc (source)
            (let ((*readtable* *objective-cl-readtable*))
              (read-from-string source))))
     (declare (inline read-objc))
     (test equal
           (read-objc "[self simpleSelector]")
           '(oclo:SEND SELF 'SIMPLE-SELECTOR))
     (test equal
           (read-objc "[[NSData alloc] init]")
           '(oclo:send (oclo:send ns:ns-data 'alloc) 'init))
     (test equal
           (read-objc "[[my-obj doSomething] doSomethingElse]")
           '(oclo:send (oclo:send my-obj 'do-something) 'do-something-else))
     (test equal
           (read-objc "[self singleArgComplexSelector:42]")
           '(oclo:SEND SELF :SINGLE-ARG-COMPLEX-SELECTOR 42))
     (test equal
           (read-objc "[self singleArgComplexSelector:(+ 4 2)]")
           '(oclo:SEND SELF :SINGLE-ARG-COMPLEX-SELECTOR (+ 4 2)))
     (test equal
           (read-objc "[self singleArgComplexSelector:(+ 4 XYZ)]")
           '(oclo:SEND SELF :SINGLE-ARG-COMPLEX-SELECTOR (+ 4 XYZ)))
     (test equal
           (read-objc "[self singleArgComplexSelector:abc]")
           '(oclo:SEND SELF :SINGLE-ARG-COMPLEX-SELECTOR abc))
     (test equal
           (read-objc "[self multipleArg:42 complexSelector:24]")
           '(oclo:SEND SELF :MULTIPLE-ARG 42 :COMPLEX-SELECTOR 24))
     (test equal
           (read-objc "[self multipleArg: (+ 4 2) complexSelector: (* 2 4) ]")
           '(oclo:SEND SELF :MULTIPLE-ARG (+ 4 2) :COMPLEX-SELECTOR (* 2 4)))
     (test equal
           (read-objc "[self multipleArg:[self one]complexSelector:[self two]]")
           '(oclo:SEND SELF :MULTIPLE-ARG (oclo:SEND SELF 'ONE) :COMPLEX-SELECTOR (oclo:SEND SELF 'TWO)))
     (test equal
           (read-objc "[self multipleArg:[self one]
             complexSelector:[self two]]")
           '(oclo:SEND SELF :MULTIPLE-ARG (oclo:SEND SELF 'ONE) :COMPLEX-SELECTOR (oclo:SEND SELF 'TWO)))
     (test equal
           (read-objc "[self multipleArg: [self one]
             complexSelector: [self two]  ]")
           '(oclo:SEND SELF :MULTIPLE-ARG (oclo:SEND SELF 'ONE) :COMPLEX-SELECTOR (oclo:SEND SELF 'TWO)))
     (test equal
           (read-objc "[self multipleArgWithEmptyPart:42 :24]")
           '(oclo:SEND SELF :MULTIPLE-ARG-WITH-EMPTY-PART 42 :|| 24))
     (test equal
           (read-objc "[self multipleArgWithEmptyPart:42 : 24 ]")
           '(oclo:SEND SELF :MULTIPLE-ARG-WITH-EMPTY-PART 42 :|| 24))
     (test equal
           (read-objc "[self multipleArgWithEmptyPart:ABC :DEF]")
           '(oclo:SEND SELF :MULTIPLE-ARG-WITH-EMPTY-PART abc :|| def))
     (test equal
           (read-objc "[self singleArgComplexSelectorWithFinalArgs:42 (:int)1]")
           '(oclo:SEND SELF :SINGLE-ARG-COMPLEX-SELECTOR-WITH-FINAL-ARGS 42 (:INT 1)))
     (test equal
           (read-objc "[self singleArgComplexSelectorWithFinalArgs:42(:int)  1 ]")
           '(oclo:SEND SELF :SINGLE-ARG-COMPLEX-SELECTOR-WITH-FINAL-ARGS 42 (:INT 1)))
     (test equal
           (read-objc "[self multipleArg:42 complexSelectorWithFinalArgs:24 (:int)1 (:float)2.0]")
           '(oclo:SEND SELF
             :MULTIPLE-ARG 42
             :COMPLEX-SELECTOR-WITH-FINAL-ARGS 24
             (:INT 1 :FLOAT 2.0)))
     (test equal
           (read-objc "(progn [self simpleSelector]
                             [self multipleArg:42 complexSelector:24])")
           '(progn
             (oclo:SEND SELF 'SIMPLE-SELECTOR)
             (oclo:SEND SELF :MULTIPLE-ARG 42 :COMPLEX-SELECTOR 24))))))

(define-test test/read-objcl-class-definition ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
   (flet ((read-objc (source)
            (let ((*readtable* *objective-cl-readtable*))
              (read-from-string source))))
     (declare (inline read-objc))
     (test equal
           (read-objc "@[NSObject subClass:Example slots:(
                         one
                         two
                         three)]")
           '(DEFCLASS EXAMPLE (ns:ns-object)
             (ONE
              TWO
              THREE)
             (:METACLASS NS:+NS-OBJECT))))))



(defun equal-modulo-constant-strings (a b)
  (cond
    ((typep a 'ns:ns-string)
     (and (consp b) (eql '@ (car b))))
    ((and (consp a) (consp b))
     (and (equal-modulo-constant-strings (car a) (car b))
          (equal-modulo-constant-strings (cdr a) (cdr b))))
    (t
     (equal a b))))

(define-test test/read-objcl-class-method-definition ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
    (flet ((read-objc (source)
            (let ((*readtable* *objective-cl-readtable*))
              (read-from-string source))))
     (declare (inline read-objc))
     (test equal-modulo-constant-strings
           (read-objc "@[Example classMethod:(multipleArg:(:int)a complexSelector:(:int)b)
                                resultType:(:id)
                                      body:
                         (ns-log @\"Example %d %d\" a b)
                         [[NSNumber alloc]initWithInteger:(+ a b)]]")
           '(oclo:DEFINE-OBJC-CLASS-METHOD ((:ID :MULTIPLE-ARG (:INT A) :COMPLEX-SELECTOR (:INT B)) EXAMPLE)
             (NS-LOG (@ "Example %d %d") A B)
             (oclo:SEND (oclo:SEND NS:NS-NUMBER 'ALLOC) :INIT-WITH-INTEGER (+ A B)))))))


(define-test test/read-objcl-instance-method-definition ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
   (flet ((read-objc (source)
            (let ((*readtable* *objective-cl-readtable*))
              (read-from-string source))))
     (declare (inline read-objc))
     (test equal-modulo-constant-strings
           (read-objc "@[Example method:(multipleArg:(:int)a complexSelector:(:int)b)
                           resultType:(:id)
                                 body:
                         (ns-log @\"Example %d %d\" a b)
                         [[NSNumber alloc]initWithInteger:(+ a b)]]")
           '(oclo:DEFINE-OBJC-METHOD ((:ID :MULTIPLE-ARG (:INT A) :COMPLEX-SELECTOR (:INT B)) EXAMPLE)
             (NS-LOG (@ "Example %d %d") A B)
             (oclo:SEND (oclo:SEND NS:NS-NUMBER 'ALLOC) :INIT-WITH-INTEGER (+ A B))))
     )))


(define-test test/all ()
  (test/read-identifier)
  (test/read-type-specifier)
  (test/read-method-signature)
  (test/read-final-arguments)
  (test/read-message)
  (test/read-message-send)
  (test/message-send)
  (test/read-objcl-class-definition)
  (test/read-objcl-class-method-definition)
  (test/read-objcl-instance-method-definition)
  (test/read-objcl-message-send))


(test/all)



#-(and)
(progn
  
 '(progn
   [w alphaValue]
   [w setAlphaValue:0.5]
   [v mouse:p inRect:r]
   [[w getFrame] mouse:p inRect:r]
   [NSString stringWithInteger: (* 42 ten)]
   [NSString stringWithFormat:@"%f %i %f" (double-float)2 (int)3 (double-float)4]
   [[NSNumber alloc] initWithInt:42]
   (let ((controller [NSWindowController alloc]))
     [controller initWithWindowNibName:@"DataWindow" owner:controller])
   [self doSomething]
   [super doSomething])

 (PROGN
   (OBJC:SEND W 'ALPHA-VALUE)
   (OBJC:SEND W :SET-ALPHA-VALUE 0.5)
   (OBJC:SEND V :MOUSE P :IN-RECT R)
   (OBJC:SEND (OBJC:SEND W 'GET-FRAME) :MOUSE P :IN-RECT R)
   (OBJC:SEND NS:NS-STRING :STRING-WITH-INTEGER (* 42 TEN))
   (OBJC:SEND NS:NS-STRING :STRING-WITH-FORMAT (CCL:@ "%f %i %f") (:DOUBLE-FLOAT 2 :INT 3 :DOUBLE-FLOAT 4))
   (OBJC:SEND (OBJC:SEND NS:NS-NUMBER 'ALLOC) :INIT-WITH-INT 42)
   (LET ((CONTROLLER (OBJC:SEND NS:NS-WINDOW-CONTROLLER 'ALLOC)))
     (OBJC:SEND CONTROLLER :INIT-WITH-WINDOW-NIB-NAME (CCL:@ "DataWindow") :OWNER CONTROLLER))
   (OBJC:SEND SELF 'DO-SOMETHING)
   (OBJC:SEND-SUPER SELF 'DO-SOMETHING)))


;;;; THE END ;;;;
