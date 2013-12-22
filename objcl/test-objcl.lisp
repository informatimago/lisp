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

(in-package "COM.INFORMATIMAGO.OBJECTIVE-CL")



;; When reading expressions in the tests, we need to set the package
;; to ensure the symbols are read in the expected package.


(defmacro with-string-check ((readtable stream string) &body body)
  `(with-input-from-string (,stream ,string)
     (let ((*readtable* ,readtable)
           (*read-default-float-format* 'double-float))
       (locally ,@body))))


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
         '("simpleSelector" nil nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "singleArgComplexSelector:(int)arg)")
           (read-method-signature stream))
         '("singleArgComplexSelector:" ((:int arg)) nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:(int)arg1 complexSelector:(int)arg2)")
           (read-method-signature stream))
         '("multipleArg:complexSelector:" ((:int arg1) (:int arg2)) nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArgWithEmptyPart:(int)arg1 :(int)arg2)")
           (read-method-signature stream))
         '("multipleArgWithEmptyPart::" ((:int arg1) (:int arg2)) nil))

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
         '((:integer (+ one 2))))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "(integer)(+ 1 2) (float)(+ 1.0 2.0)]")
           (read-final-arguments stream))
         '((:integer (+ 1 2)) (:float (+ 1.0d0 2.0d0))))
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
         '("simpleSelector" nil nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "singleArgComplexSelector:42]")
           (read-message stream))
         '("singleArgComplexSelector:" (42) nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:42 complexSelector:24]")
           (read-message stream))
         '("multipleArg:complexSelector:" (42 24) nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:(+ 4 2) complexSelector:(* 2 4)]")
           (read-message stream))
         '("multipleArg:complexSelector:" ((+ 4 2) (* 2 4)) nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:[self one] complexSelector:[self two]]")
           (read-message stream))
         `("multipleArg:complexSelector:"
           (,(generate-message-send 'self '"one" 'nil 'nil)
             ,(generate-message-send 'self '"two" 'nil 'nil))
           nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArgWithEmptyPart:42 :24]")
           (read-message stream))
         '("multipleArgWithEmptyPart::" (42 24) nil))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "singleArgComplexSelectorWithFinalArgs:42
                                                    (int)1]")
           (read-message stream))
         '("singleArgComplexSelectorWithFinalArgs:" (42) ((:int 1))))
   (test equal
         (with-string-check (*objc-readtable*
                             stream "multipleArg:42
                               complexSelectorWithFinalArgs:24
                                             (int)1 (float)2.0]")
           (read-message stream))
         '("multipleArg:complexSelectorWithFinalArgs:" (42 24) ((:int 1) (:float 2.0d0))))))


(define-test test/read-message-send ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "simpleSelector]")
            (read-message-send stream 'self (function read-message)))
          '(self "simpleSelector" nil nil))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "singleArgComplexSelector:42]")
            (read-message-send stream 'self (function read-message)))
          '(self "singleArgComplexSelector:" (42) nil))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArg:42 complexSelector:24]")
            (read-message-send stream 'self (function read-message)))
          '(self "multipleArg:complexSelector:" (42 24) nil))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArg:(+ 4 2) complexSelector:(* 2 4)]")
            (read-message-send stream 'self (function read-message)))
          '(self "multipleArg:complexSelector:" ((+ 4 2) (* 2 4)) nil))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArg:[self one] complexSelector:[self two]]")
            (read-message-send stream 'self (function read-message)))
          `(self "multipleArg:complexSelector:"
                 (,(generate-message-send 'self '"one" 'nil 'nil)
                   ,(generate-message-send 'self '"two" 'nil 'nil))
                 nil))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArgWithEmptyPart:42 :24]")
            (read-message-send stream 'self (function read-message)))
          '(self "multipleArgWithEmptyPart::" (42 24) nil))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "singleArgComplexSelectorWithFinalArgs:42
                                                    (int)1]")
            (read-message-send stream 'self (function read-message)))
          '(self "singleArgComplexSelectorWithFinalArgs:" (42) ((:int 1))))
    (test equal
          (with-string-check (*objc-readtable*
                              stream "multipleArg:42
                               complexSelectorWithFinalArgs:24
                                             (int)1 (float)2.0]")
            (read-message-send stream 'self (function read-message)))
          '(self "multipleArg:complexSelectorWithFinalArgs:" (42 24) ((:int 1) (:float 2.0d0))))))


(define-test test/message-send ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))                  
   (flet ((gen (args) (apply (function generate-message-send) args)))
     (declare (inline gen))
     (test equal
           (gen '(self "simpleSelector" nil nil))
           '(oclo:send self 'simple-selector))
     (test equal
           (gen '(self "singleArgComplexSelector:" (42) nil))
           '(oclo:send self :single-arg-complex-selector 42))
     (test equal
           (gen '(self "multipleArg:complexSelector:" (42 24) nil))
           '(oclo:send self :multiple-arg 42 :complex-selector 24))
     (test equal
           (gen '(self "multipleArg:complexSelector:" ((+ 4 2) (* 2 4)) nil))
           '(oclo:send self :multiple-arg (+ 4 2) :complex-selector (* 2 4)))
     (test equal
           (gen `(self "multipleArg:complexSelector:"
                       (,(generate-message-send 'self '"one" 'nil 'nil)
                         ,(generate-message-send 'self '"two" 'nil 'nil))
                       nil))
           '(oclo:send self :multiple-arg (oclo:send self 'one) :complex-selector (oclo:send self 'two)))
     (test equal
           (gen '(self "multipleArgWithEmptyPart::" (42 24) nil))
           '(oclo:send self :multiple-arg-with-empty-part 42 :|| 24))
     (test equal
           (gen '(self "singleArgComplexSelectorWithFinalArgs:" (42) ((:int 1))))
           '(oclo:send self :single-arg-complex-selector-with-final-args 42 (:int 1)))
     (test equal
           (gen '(self "multipleArg:complexSelectorWithFinalArgs:" (42 24) ((:int 1) (:float 2.0d0))))
           '(oclo:send self :multiple-arg 42 :complex-selector-with-final-args 24 (:int 1 :float 2.0d0))))))


(define-test test/read-objcl-message-send ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.OBJECTIVE-CL")))
   (flet ((read-objc (source)
            (let ((*readtable* *objective-cl-readtable*))
              (read-from-string source))))
     (declare (inline read-objc))
     (test equal
           (read-objc "[self simpleSelector]")
           '(oclo:send self 'simple-selector))
     (test equal
           (read-objc "[[NSData alloc] init]")
           '(oclo:send (oclo:send ns:ns-data 'alloc) 'init))
     (test equal
           (read-objc "[[my-obj doSomething] doSomethingElse]")
           '(oclo:send (oclo:send my-obj 'do-something) 'do-something-else))
     (test equal
           (read-objc "[self singleArgComplexSelector:42]")
           '(oclo:send self :single-arg-complex-selector 42))
     (test equal
           (read-objc "[self singleArgComplexSelector:(+ 4 2)]")
           '(oclo:send self :single-arg-complex-selector (+ 4 2)))
     (test equal
           (read-objc "[self singleArgComplexSelector:(+ 4 XYZ)]")
           '(oclo:send self :single-arg-complex-selector (+ 4 xyz)))
     (test equal
           (read-objc "[self singleArgComplexSelector:abc]")
           '(oclo:send self :single-arg-complex-selector abc))
     (test equal
           (read-objc "[self multipleArg:42 complexSelector:24]")
           '(oclo:send self :multiple-arg 42 :complex-selector 24))
     (test equal
           (read-objc "[self multipleArg: (+ 4 2) complexSelector: (* 2 4) ]")
           '(oclo:send self :multiple-arg (+ 4 2) :complex-selector (* 2 4)))
     (test equal
           (read-objc "[self multipleArg:[self one]complexSelector:[self two]]")
           '(oclo:send self :multiple-arg (oclo:send self 'one) :complex-selector (oclo:send self 'two)))
     (test equal
           (read-objc "[self multipleArg:[self one]
             complexSelector:[self two]]")
           '(oclo:send self :multiple-arg (oclo:send self 'one) :complex-selector (oclo:send self 'two)))
     (test equal
           (read-objc "[self multipleArg: [self one]
             complexSelector: [self two]  ]")
           '(oclo:send self :multiple-arg (oclo:send self 'one) :complex-selector (oclo:send self 'two)))
     (test equal
           (read-objc "[self multipleArgWithEmptyPart:42 :24]")
           '(oclo:send self :multiple-arg-with-empty-part 42 :|| 24))
     (test equal
           (read-objc "[self multipleArgWithEmptyPart:42 : 24 ]")
           '(oclo:send self :multiple-arg-with-empty-part 42 :|| 24))
     (test equal
           (read-objc "[self multipleArgWithEmptyPart:ABC :DEF]")
           '(oclo:send self :multiple-arg-with-empty-part abc :|| def))
     (test equal
           (read-objc "[self singleArgComplexSelectorWithFinalArgs:42 (:int)1]")
           '(oclo:send self :single-arg-complex-selector-with-final-args 42 (:int 1)))
     (test equal
           (read-objc "[self singleArgComplexSelectorWithFinalArgs:42(:int)  1 ]")
           '(oclo:send self :single-arg-complex-selector-with-final-args 42 (:int 1)))
     (test equal
           (read-objc "[self multipleArg:42 complexSelectorWithFinalArgs:24 (:int)1 (:float)2.0]")
           '(oclo:send self
             :multiple-arg 42
             :complex-selector-with-final-args 24
             (:int 1 :float 2.0d0)))
     (test equal
           (read-objc "(progn [self simpleSelector]
                             [self multipleArg:42 complexSelector:24])")
           '(progn
             (oclo:send self 'simple-selector)
             (oclo:send self :multiple-arg 42 :complex-selector 24))))))

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
           '(defclass example (ns:ns-object)
             (one
              two
              three)
             (:metaclass ns:+ns-object))))))



(defun equal-modulo-constant-strings (a b)
  (cond
    ((typep a 'ns:ns-string)
     (and (consp b) (eql '\@ (car b))))
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
           '(oclo:define-objc-class-method ((:id :multiple-arg (:int a) :complex-selector (:int b)) example)
             (ns-log (@ "Example %d %d") a b)
             (oclo:send (oclo:send ns:ns-number 'alloc) :init-with-integer (+ a b)))))))


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
           '(oclo:define-objc-method ((:id :multiple-arg (:int a) :complex-selector (:int b)) example)
             (ns-log (@ "Example %d %d") a b)
             (oclo:send (oclo:send ns:ns-number 'alloc) :init-with-integer (+ a b))))
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
   [w alphavalue]
   [w setalphavalue:0.5]
   [v mouse:p inrect:r]
   [[w getframe] mouse:p inrect:r]
   [nsstring stringwithinteger: (* 42 ten)]
   [nsstring stringwithformat:@"%f %i %f" (double-float)2 (int)3 (double-float)4]
   [[nsnumber alloc] initwithint:42]
   (let ((controller [nswindowcontroller alloc]))
     [controller initwithwindownibname:@"DataWindow" owner:controller])
   [self dosomething]
   [super dosomething])

 (progn
   (objc:send w 'alpha-value)
   (objc:send w :set-alpha-value 0.5)
   (objc:send v :mouse p :in-rect r)
   (objc:send (objc:send w 'get-frame) :mouse p :in-rect r)
   (objc:send ns:ns-string :string-with-integer (* 42 ten))
   (objc:send ns:ns-string :string-with-format (ccl:@ "%f %i %f") (:double-float 2 :int 3 :double-float 4))
   (objc:send (objc:send ns:ns-number 'alloc) :init-with-int 42)
   (let ((controller (objc:send ns:ns-window-controller 'alloc)))
     (objc:send controller :init-with-window-nib-name (ccl:@ "DataWindow") :owner controller))
   (objc:send self 'do-something)
   (objc:send-super self 'do-something)))


;;;; THE END ;;;;
