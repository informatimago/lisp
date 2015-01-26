;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               reader-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-25 <PJB> Extracted from reader.
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
(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
                          "READTABLE"
                          "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
                          "READ" "READ-PRESERVING-WHITESPACE"
                          "READ-DELIMITED-LIST"
                          "READ-FROM-STRING"
                          "READTABLE-CASE" "READTABLEP"
                          "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
                          "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
                          "SET-SYNTAX-FROM-CHAR"
                          "WITH-STANDARD-IO-SYNTAX"
                          "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
                          "*READ-SUPPRESS*" "*READTABLE*"
                          ;; Extensions:
                          "READTABLE-SYNTAX-TABLE" "READTABLE-PARSE-TOKEN"
                          "SET-INDIRECT-DISPATCH-MACRO-CHARACTER" 
                          "SET-INDIRECT-MACRO-CHARACTER"
                          "LIST-ALL-MACRO-CHARACTERS"
                          "SIMPLE-READER-ERROR" "SIMPLE-END-OF-FILE"
                          "MISSING-PACKAGE-ERROR" "SYMBOL-IN-MISSING-PACKAGE-ERROR"
                          "MISSING-SYMBOL-ERROR" "SYMBOL-MISSING-IN-PACKAGE-ERROR"
                          "INTERN-HERE" "RETURN-UNINTERNED"
                          ;; Utilities:
                          "POTENTIAL-NUMBER-P")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
                "EVAL-FEATURE")
  (:export "TEST/ALL"
           "TEST/READER"
           "TEST/SYMBOLS"
           "TEST/FEATURES"
           "TEST/LISTS"
           "TEST/SHARP-PLUS-WITH-SHARP-REFERENCES"
           "TEST/BIT-VECTOR--NUMBERS--PATHNAMES"
           "TEST/COMPLEX-NUMBERS"
           "TEST/READ-DELIMITED-LIST/COMMENTS"
           "TEST/LISTS/COMMENTS"
           "TEST/VECTOR-WITH-TOO-MUCH-DATA"
           "TEST/NON-EMPTY-VECTOR-WITH-TOO-LITTLE-DATA"
           "TEST/VECTORS"
           "TEST/CHECK-SYMBOLS"
           "TEST/POTENTIAL-NUMBER-P")
  (:documentation
   "
This package tests the COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER package.

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2006 - 2015
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER.TEST")


(define-test test/reader ()
  (let ((*read-base* 10)
        (*read-eval* t)
        (*read-suppress* nil)
        (*read-default-float-format* 'single-float)
        (*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER.TEST")))
    (dolist (test
             '(
               ;; integer  ::= [sign] digit+      
               (nil "0"  0.)
               (nil "1"  1.)
               (nil "2"  2.)
               (nil "9"  9.)
               (nil "10" 10.)
               (nil "11" 11.)
               (nil "12" 12.)
               (nil "19" 19.)
               (((*read-base* 3.)) "0"  0.)
               (((*read-base* 3.)) "1"  1.)
               (((*read-base* 3.)) "2"  2.)
               (((*read-base* 3.)) "9"  |9|)
               (((*read-base* 3.)) "10" 3.)
               (((*read-base* 3.)) "11" 4.)
               (((*read-base* 3.)) "13" |13|)
               (nil "-0"  -0.)
               (nil "-1"  -1.)
               (nil "-2"  -2.)
               (nil "-9"  -9.)
               (nil "-10" -10.)
               (nil "-11" -11.)
               (nil "-12" -12.)
               (nil "-19" -19.)
               (((*read-base* 3.)) "-0"  -0.)
               (((*read-base* 3.)) "-1"  -1.)
               (((*read-base* 3.)) "-2"  -2.)
               (((*read-base* 3.)) "-9"  |-9|)
               (((*read-base* 3.)) "-10" -3.)
               (((*read-base* 3.)) "-11" -4.)
               (((*read-base* 3.)) "-13" |-13|)
               (nil "+0"  +0.)
               (nil "+1"  +1.)
               (nil "+2"  +2.)
               (nil "+9"  +9.)
               (nil "+10" +10.)
               (nil "+11" +11.)
               (nil "+12" +12.)
               (nil "+19" +19.)
               (((*read-base* 3.)) "+0"  +0.)
               (((*read-base* 3.)) "+1"  +1.)
               (((*read-base* 3.)) "+2"  +2.)
               (((*read-base* 3.)) "+9"  |+9|)
               (((*read-base* 3.)) "+10" +3.)
               (((*read-base* 3.)) "+11" +4.)
               (((*read-base* 3.)) "+13" |+13|)
               ;; integer  ::= [sign] decimal-digit+ decimal-point 
               (nil "0."  0.)
               (nil "1."  1.)
               (nil "2."  2.)
               (nil "9."  9.)
               (nil "10." 10.)
               (nil "11." 11.)
               (nil "12." 12.)
               (nil "19." 19.)
               (((*read-base* 3.)) "0."  0.)
               (((*read-base* 3.)) "1."  1.)
               (((*read-base* 3.)) "2."  2.)
               (((*read-base* 3.)) "9."  9.)
               (((*read-base* 3.)) "10." 10.)
               (((*read-base* 3.)) "11." 11.)
               (((*read-base* 3.)) "13." 13.)
               (nil "-0."  -0.)
               (nil "-1."  -1.)
               (nil "-2."  -2.)
               (nil "-9."  -9.)
               (nil "-10." -10.)
               (nil "-11." -11.)
               (nil "-12." -12.)
               (nil "-19." -19.)
               (((*read-base* 3.)) "-0."  -0.)
               (((*read-base* 3.)) "-1."  -1.)
               (((*read-base* 3.)) "-2."  -2.)
               (((*read-base* 3.)) "-9."  -9.)
               (((*read-base* 3.)) "-10." -10.)
               (((*read-base* 3.)) "-11." -11.)
               (((*read-base* 3.)) "-13." -13.)
               (nil "+0."  +0.)
               (nil "+1."  +1.)
               (nil "+2."  +2.)
               (nil "+9."  +9.)
               (nil "+10." +10.)
               (nil "+11." +11.)
               (nil "+12." +12.)
               (nil "+19." +19.)
               (((*read-base* 3.)) "+0."  +0.)
               (((*read-base* 3.)) "+1."  +1.)
               (((*read-base* 3.)) "+2."  +2.)
               (((*read-base* 3.)) "+9."  +9.)
               (((*read-base* 3.)) "+10." +10.)
               (((*read-base* 3.)) "+11." +11.)
               (((*read-base* 3.)) "+13." +13.)
               ;; ratio    ::= [sign] {digit}+ slash {digit}+
               (nil "0/0"    nil division-by-zero)
               (nil "1/0"    nil division-by-zero)
               (nil "10/000" nil division-by-zero)
               (nil "0/1" 0)
               (nil "1/1" 1)
               (nil "2/1" 2)
               (nil "20/10" 2)
               (nil "200/100" 2)
               (nil "0/2" 0)
               (nil "1/2" 1/2)
               (nil "0/20" 0)
               (nil "10/20" 1/2)
               (nil "100/200" 1/2)
               (nil "001/2" 1/2)
               (nil "000/20" 0)
               (nil "010/20" 1/2)
               (nil "100/200" 1/2)
               (nil "12345/54321" 12345/54321)
               (nil "+0/0"    nil division-by-zero)
               (nil "+1/0"    nil division-by-zero)
               (nil "+10/000" nil division-by-zero)
               (nil "+0/1" 0)
               (nil "+1/1" 1)
               (nil "+2/1" 2)
               (nil "+20/10" 2)
               (nil "+200/100" 2)
               (nil "+0/2" 0)
               (nil "+1/2" 1/2)
               (nil "+0/20" 0)
               (nil "+10/20" 1/2)
               (nil "+100/200" 1/2)
               (nil "+001/2" 1/2)
               (nil "+000/20" 0)
               (nil "+010/20" 1/2)
               (nil "+100/200" 1/2)
               (nil "+12345/54321" 12345/54321)
               (nil "-0/0"    nil division-by-zero)
               (nil "-1/0"    nil division-by-zero)
               (nil "-10/000" nil division-by-zero)
               (nil "-0/1" -0)
               (nil "-1/1" -1)
               (nil "-2/1" -2)
               (nil "-20/10" -2)
               (nil "-200/100" -2)
               (nil "-0/2" -0)
               (nil "-1/2" -1/2)
               (nil "-0/20" -0)
               (nil "-10/20" -1/2)
               (nil "-100/200" -1/2)
               (nil "-001/2" -1/2)
               (nil "-000/20" -0)
               (nil "-010/20" -1/2)
               (nil "-100/200" -1/2)
               (nil "-12345/54321" -12345/54321)
;;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ exponent
;;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ 
;;; float    ::= [sign] {decimal-digit}+ exponent
;;; float    ::= [sign] {decimal-digit}+ decimal-point {decimal-digit}* exponent
;;; exponent ::=  exponent-marker [sign] {digit}+
;;; 
;;; consing-dot   ::= dot
;;; 
;;; symbol        ::= symbol-name
;;;                 | package-marker symbol-name
;;;                 | package-marker package-marker symbol-name
;;;                 | package-name package-marker symbol-name
;;;                 | package-name package-marker package-marker symbol-name
               )
             :success)
      (multiple-value-bind (val err)
          (ignore-errors
           (eval `(progv
                      ',(mapcar (function first)  (first test))
                      ',(mapcar (function second) (first test))
                    (read-from-string ,(second test)))))
        (assert-true
            (if (fourth test)
                (typep err (fourth test))
                (eql   val (third test)))
            (err (fourth test) val (third test))
            "~S gives ~:[~S~;~:*~S~*~]; expected: ~S"
            `(let ,(first test) (read-from-string ,(second test)))
            err val
            (or (fourth test) (third test)))))))


(defun test-cases (test-name cases)
  (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER.TEST")))
    (dolist (test cases :success)
      (destructuring-bind (expression expected-values expected-error) test
        (multiple-value-bind (actual-values actual-error)
            (ignore-errors (multiple-value-list (eval expression)))
          (assert-true (or (and (null expected-error) (null actual-error))
                           (typep actual-error expected-error))
              (expression expected-error actual-error)
              "Test ~A~%Testing ~S, expected ~
                      ~:[no error~;an error of type ~:*~S~], ~
                      got this error: ~A"
              test-name expression expected-error actual-error)
          (assert-true (equalp expected-values actual-values)
              (expression expected-values actual-values)
              "Test ~A~%Testing ~S, expected ~S, got ~S" expression
              test-name expected-values actual-values))))))


(defmacro tests (&rest cases)
  (if (stringp (first cases))
      `(test-cases ,(first cases) ',(rest cases))
      `(test-cases "unamed" ',cases)))

(define-test test/symbols ()
  (tests "symbols"
         ((read-from-string "( abc ab a || |a| |ab| |a b c| )")
          ((abc ab a || |a| |ab| |a b c|) ;
           32)
          nil)))

(define-test test/features ()
 (let ((*features* '(:a :b :c)))
   (tests "*features*"
          ((eval-feature ':a *standard-input*)           (t)   nil)
          ((eval-feature ':z *standard-input*)           (nil) nil)
          ((eval-feature '42 *standard-input*)           (nil) nil)
          ((eval-feature '(:not :a)    *standard-input*) (nil) nil)
          ((eval-feature '(:not :z)    *standard-input*) (t)   nil)
          ((eval-feature '(:not :a :b) *standard-input*) ()    reader-error)
          ((eval-feature '(:and)       *standard-input*) (t)   nil)
          ((eval-feature '(:and :a)    *standard-input*) (t)   nil)
          ((eval-feature '(:and :a :b) *standard-input*) (t)   nil)
          ((eval-feature '(:and :a :c) *standard-input*) (t)   nil)
          ((eval-feature '(:and :a :z) *standard-input*) (nil) nil)
          ((eval-feature '(:and :y :z) *standard-input*) (nil) nil)
          ((eval-feature '(:or)        *standard-input*) (nil) nil)
          ((eval-feature '(:or :a)     *standard-input*) (t)   nil)
          ((eval-feature '(:or :a :b)  *standard-input*) (t)   nil)
          ((eval-feature '(:or :a :c)  *standard-input*) (t)   nil)
          ((eval-feature '(:or :a :z)  *standard-input*) (t)   nil)
          ((eval-feature '(:or :y :z)  *standard-input*) (nil) nil)
          ((eval-feature '(:or (:and :a (:not :z)) (:and (:not :a) :z))
                         *standard-input*)               (t)   nil)
          ((eval-feature '(:and (:or :a (:not :z)) (:or (:not :a) :z))
                         *standard-input*)               (nil) nil)
          ((eval-feature '(:and :a :b (:or :y :z (:not :a)))
                         *standard-input*)               (nil) nil)
          ((eval-feature '(:and :a :b (:or :y :z (:not 42)))
                         *standard-input*)               (t)   nil))))


(define-test test/lists ()
 (tests "lists"
        ((read-from-string "()")                       (() 2)           nil)
        ((read-from-string "(a)")                      ((a) 3)          nil)
        ((read-from-string "(a b)")                    ((a b) 5)        nil)
        ((read-from-string "(a b c)")                  ((a b c) 7)      nil)
        ((read-from-string "(a b c d)")                ((a b c d) 9)    nil)
        ((read-from-string "(a b c . d)")              ((a b c . d) 11)  nil)
        ((read-from-string "(a b c . d e)")            nil            reader-error)
        ((read-from-string "(a b c . . d)")            nil            reader-error)
        ((read-from-string "(a b c . d .)")            nil            reader-error)
        ((let ((*features* '(:test)))
           (read-from-string "(a b c #+test d)"))      ((a b c d) 16)    nil)
        ((let ((*features* '(:test)))
           (read-from-string "(a b c #-test d)"))      ((a b c) 16)      nil)
        ((let ((*features* '(:test)))
           (read-from-string "(a b c . #+test d)"))    ((a b c . d) 18)  nil)
        ((let ((*features* '(:test)))
           (read-from-string "(a b c . #-test d e)"))  ((a b c . e) 20)  nil)
        ((let ((*features* '(:test)))
           (read-from-string "(a b c #+test . d)"))    ((a b c . d) 18)  nil)
        ((let ((*features* '(:test)))
           (read-from-string "(a b c #-test . d)"))    ((a b c d) 18)    nil)
        ((read-from-string "(#+(or) #$foo       xyz)") nil               reader-error)
        ((read-from-string "(#+(or) abc:def:ghi xyz)") ((xyz) 24)        nil)))

(define-test test/sharp-plus-with-sharp-references ()
 (tests "#+ with #= and ##"
        ((let ((*features* (quote (:a :b))))
           (read-from-string "(#+#1=(or a b) #1#)"))
         (((:or :a :b)) 19)
         nil)
        ((let ((*features* (quote (:a :b))))
           (read-from-string "(#+#.(cl:if (cl:eq :a (cl:first cl:*features*)) '(:and) '(:or)) equal)"))
         ((equal) 70)
         nil)))


#-(and)
(tests 
 ((let ((*features* (quote (:a :b)))) 
    (read-from-string "#+#1=(or a b) #1#"))
  ((:or :a :b) 44)
  nil))

(define-test test/bit-vector--numbers--pathnames ()
 (tests "bit vectors, numbers, and pathnames"
        ((read-from-string "(#*101111 #6*10111110101 #6*101111 #6*1010 #6*1011 #* #0*11010)")
         ((#*101111 #*101111 #*101111 #*101000 #*101111 #* #*) 63)
         nil)
        ((read-from-string "(#b10111101 #o275 #xbd #36r59)")
         ((189 189 189 189) 30)
         nil)
        ((read-from-string "#P\"/tmp/a.c\"")
         (#.(make-pathname :directory '(:absolute "tmp")
                           :name "a"
                           :type "c"
                           :version #+(or ecl sbcl) :newest #-(or ecl sbcl) nil
                           :case :local) 12)
         nil)))

#- (and)
(tests
 ((progn
    (defstruct s a b c) (read-from-string "#S(s a 1 b 2 c 3)"))
  (#s(s :a 1 :b 2 :c 3) 17)
  nil))

(define-test test/complex-numbers ()
  #-clisp
  (tests "complex numbers"
         ((read-from-string "( #C(123 456) #c(-123 456)
                             #C(12.3f0 456) #c(-123 45.6f0)
                             #C(123/10 456/100) #c(-123/10 456/100))")
          (( #c(123 456) #c(-123 456)
               #c(12.3f0 456) #c(-123 45.6f0)
               #c(123/10 456/100) #c(-123/10 456/100) )
           155)
          nil)))


(define-test test/read-delimited-list/comments ()
  (tests "read-delimited-list with comments"
        ((with-input-from-string (src " \"!A\"
) def)
")
           (values (read-delimited-list #\) src)
                   (read-delimited-list #\) src)))
         (("!A") (def))
         nil)

        ((with-input-from-string (src "#( \"!A\" 
) (def)
")
           (values (read src)
                   (read src)))
         (#("!A") (def))
         nil)

        ((with-input-from-string (src "( \"!A\"
) (def)
")
           (values (read src)
                   (read src)))
         (("!A") (def))
         nil)

        ((with-input-from-string (src " \"!A\" ; comment
) def)
")
           (values (read-delimited-list #\) src)
                   (read-delimited-list #\) src)))
         (("!A") (def))
         nil)
       
        ((with-input-from-string (src "#( \"!A\"  ; comment
) (def)
")
           (values (read src)
                   (read src)))
         (#("!A") (def))
         nil)

        ((with-input-from-string (src "( \"!A\" ; comment
) (def)
")
           (values (read src)
                   (read src)))
         (("!A") (def))
         nil)))


(define-test test/lists/comments ()
 (tests "lists with comments"
        ((read-from-string "( () (a) (a b) (a b c) (a . ()) (a . b) (a b . ()) (a b . c)
                       ( ; comment
                    ) (a ; comment
                    ) (a ; comment
               b) (a b c ; comment
                    ) (a ; comment
              . ()) (a . ; comment
                  ()) (a ; comment
               . b) (a . ; comment
               b) (a . b ; comment
                 ) (a b .; comment
             ()) (a b . c;comment
                      ))")
         ((nil (a) (a b) (a b c) (a) (a . b) (a b) (a b . c) nil (a) (a b) (a b c) (a)
               (a) (a . b) (a . b) (a . b) (a b) (a b . c)) 
          469)
         nil)))

(define-test test/vector-with-too-much-data ()
 (tests "vector with too much data"
        ((with-input-from-string (input "#2(a b c) d e")
           (values (read input) (read-line input)))
         (#(a b)                         
           " d e")
         nil)))

(define-test test/vector-with-too-little-data ()
 (tests "non-empty vector with too little data"
        ((length (read-from-string "#2()"))
         (2)
         nil)))

(define-test test/vectors ()
 (tests "vectors and vectors with comments"
        ((read-from-string "( #() #(a) #(a b) #(a b c) #(a  #()) #(a  b) #(a b  #()) #(a b  c)
                              #2(a) #2(a b) #2(a b c) #2(a  #()) #2(a  b) #2(a b  #()) #2(a b  c)
                       #( ; comment
                    ) #(a ; comment
                    ) #(a ; comment
               b) #(a b c ; comment
                    ) #(a ; comment
               #()) #(a  ; comment
                  #()) #(a ; comment
                b) #(a  ; comment
               b) #(a  b ; comment
                 ) #(a b ; comment
             #()) #(a b  c;comment
                      ))")
         ((#() #(a) #(a b) #(a b c) #(a #()) #(a b) #(a b #()) #(a b c)
               #(a a) #(a b) #(a b) #(a #()) #(a b) #(a b) #(a b) #() #(a)
               #(a b) #(a b c) #(a #()) #(a #()) #(a b) #(a b) #(a b) #(a b #())
               #(a b c))
          580)
         nil)))



(define-test test/check-symbols ()
  (dolist (sym '("READTABLE"
                 "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
                 "READ" "READ-PRESERVING-WHITESPACE"
                 "READ-DELIMITED-LIST"
                 "READ-FROM-STRING"
                 "READTABLE-CASE" "READTABLEP"
                 "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
                 "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
                 "SET-SYNTAX-FROM-CHAR"
                 "WITH-STANDARD-IO-SYNTAX"
                 "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
                 "*READ-SUPPRESS*" "*READTABLE*")
               :success)
    (let ((s-here (find-symbol sym *package*))
          (s-cl   (find-symbol sym "COMMON-LISP")))
      (assert-true (not (eq s-here s-cl))
          () "The symbol ~S is interned both in COMMON-LISP and in ~A"
          s-here (package-name *package*)))))




(define-test test/potential-number-p ()
  (assert-true (every (function potential-number-p)
                      '("1b5000"
                        "777777q"
                        "1.7J"
                        "-3/4+6.7J"
                        "12/25/83"
                        "27^19"
                        "3^4/5"
                        "6//7"
                        "3.1.2.6"
                        "^-43^"     
                        "3.141_592_653_589_793_238_4"
                        "-3.7+2.6i-6.17j+19.6k"
                        "+.e2")))
  (assert-true (notany (function potential-number-p)
                       '("/"
                         "/5"
                         "+"
                         "1+"
                         "1-"   
                         "foo+"
                         "ab.cd"
                         "_"
                         "^"
                         "^/-")))
  (let ((pns '("bad-face"
               "25-dec-83"
               "a/b"
               "fad_cafe"
               "f^" )))
    (let ((*read-base* 16.))
      (assert-true (every (function potential-number-p) pns)))
    (let ((*read-base* 10.))
      (assert-true (notany (function potential-number-p) pns))))
  :success)


(define-test test/all ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER.TEST")))
    (test/reader)
    (test/symbols)
    (test/features)
    (test/lists)
    (test/sharp-plus-with-sharp-references)
    (test/bit-vector--numbers--pathnames)
    (test/complex-numbers)
    (test/read-delimited-list/comments)
    (test/lists/comments)
    (test/vector-with-too-much-data)
    (test/vector-with-too-little-data)
    (test/vectors)
    (test/check-symbols)
    (test/potential-number-p)
    :success))

;;;; THE END ;;;;



