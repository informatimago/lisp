;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               sexp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Reads and writes simple S-Expressions.
;;;;    This implements a simple parser, subset of the
;;;;    standard Common Lisp parser.
;;;;
;;;;    We can read and write:
;;;;        - integers (expressed in base ten),
;;;;        - floating point numbers,
;;;;        - strings,
;;;;        - symbols,
;;;;        - lists,
;;;;        - arrays and vectors,
;;;;        - structures,
;;;;        - hash tables.
;;;;
;;;;    We don't implement most of the macro character or dispatching macro
;;;;    characters.  No comment, no quote, one can use (quote x), nothing fancy.
;;;;    Only " for strings, ( for lists and dotted lists, #( for vectors,
;;;;    #nA for arrays, #S for structures and hash-tables.
;;;;    Symbols may be qualified, but it's up to the user supplied %make-symbol
;;;;    routine to handle the packages.
;;;; 
;;;;    string ::= "\"([^\\\"]|\\\\|\\\)\""
;;;;    number ::= "[-+]?[0-9]+(\.[0-9]*)([eE][-+]?[0-9]+)?"
;;;;    cardinal ::= [0-9]+
;;;;    symbol ::= [[ident]':']ident
;;;;    ident  ::= constituent+
;;;;    char   ::= #\\. | #\\space | #\\newline
;;;;    
;;;;    vector ::= '#(' sexp* ')'
;;;;    array ::= '#'cardinal'A(' sexp* ')'
;;;;    list ::= '(' [ sexp+ [ '.' sexp ] ] ')'
;;;;    hash ::= '#S(' 'HASH-TABLE' pair* ')'
;;;;    pair ::= '(' sexp '.' sexp ')'
;;;;    
;;;;    sexp ::= string | number | symbol | list | array | hash
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-10-25 <PJB> 
;;;;    Complete %stuff (arrays, conses).
;;;;    Implement SIMPLE-PRIN1-TO-STRING
;;;;    2007-10-25 <PJB> Created.
;;;;BUGS
;;;;
;;;;    Perhaps we should implement River's Sexp format.
;;;;
;;;;    Simplify the implementation (use a subset of Common Lisp, to
;;;;    be easily translatable into other programming languages).
;;;;
;;;;    Implement translators to other programming languages (to be
;;;;    able to exchange data between languages).
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2007 - 2016
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

(load "repr.lisp")



;;;
;;;--------------------


(defun make-scanner (string)
  (let ((i -1))
    (lambda (message)
      (ecase message
        ((currchar) (when (<     i  (length string)) (aref string     i)))
        ((nextchar) (when (< (1+ i) (length string)) (aref string (1+ i))))
        ((advance)  (when (<     i  (length string)) (incf i))
                    (when (<     i  (length string)) (aref string     i)))))))
(defun advance  (s) (funcall s 'advance))
(defun currchar (s) (funcall s 'currchar))
(defun nextchar (s) (funcall s 'nextchar))

(defun test-scanner ()
  (let ((s (make-scanner "(\"a\" b c d)")))
    (advance s)
    (do ()
        ((not  (currchar s)))
      (advance s))))

(define-condition simple-end-of-file (simple-error end-of-file) ())
(define-condition simple-reader-error (simple-error reader-error) ())

(defun reject-eos (object)
  (unless object
    (error 'simple-end-of-file
           :format-control "Reached end of string while reading."))
  object)

(defun reject-dots (token)
  (when (%dotsp token)
    (error 'simple-reader-error
           :format-control
           "A token consisting only of dots cannot be meaningfully read in."))
  token)



(defmacro defparser (name arguments &body body)
  "Defines a token parser function, which parses its argument token and returns
three values: a ok flag; a type of value; and a value parsed from the token.
When the ok flag is false, the type indicates whether it's a strong error,
and the value returned is an error message.
A strong error is a lexical error that is not ambiguous.  A weak error is
when the token could still be of another lexical category.
In the body of the parser, there are macrolet defined to REJECT or ACCEPT
the token, and to describe the parsed syntax with ALT, ZERO-OR-MORE, 
ONE-OR-MORE and OPT-SIGN."
  `(defun ,name ,arguments
     ,@(when (stringp (first body)) (list (pop body)))
     ,@(loop                            ; declarations
          :while (and (listp (car body))  (eq 'declare (caar body)))
          :collect (pop body))
     (macrolet ((reject (strongp &rest ctrlstring-and-args)
                  `(return-from ,',name
                     (values nil ,strongp
                             ,(when ctrlstring-and-args
                                    `(format nil ,@ctrlstring-and-args)))))
                (accept (type token)
                  `(return-from ,',name (values t ,type ,token)))
                (alt (&rest clauses)
                  `(cond ,@clauses))
                (zero-or-more (test &body body)
                  `(loop :while ,test :do ,@body))
                (one-or-more  (test &body body)
                  `(progn
                     (if ,test (progn ,@body) (reject nil))
                     (loop :while ,test :do ,@body))))
       ,@body)))


(defparser parse-integer-token (token)
  "integer ::= [sign] digit+ [decimal-point]"
  (let ((sign 1)
        (mant 0)
        (i 0))
    (unless (< i (length token)) (reject nil))
    (alt ((char= #\- (aref token i)) (incf i) (setf sign -1))
         ((char= #\+ (aref token i)) (incf i)))
    (one-or-more (and  (< i (length token)) (digit-char-p (aref token i)))
                 (setf mant (+ (* 10. mant) (digit-char-p (aref token i)))
                       i (1+ i)))
    (alt ((and (< i (length token)) (char= #\. (aref token i))) (incf i)))
    (if (= i (length token))
        (accept 'integer (* sign mant))
        (reject t "Junk after integer in ~S" token))))


(defparser parse-float-token (token)
  "
float ::= [sign] {decimal-digit}+ [decimal-point {decimal-digit}*] exponent
float ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ [exponent]
exponent ::=  exponent-marker [sign] {digit}+"
  (let ((sign 1)
        (mant 0)
        (esgn 1)
        (mexp 0)
        (expo 0)
        (i    0)
        (type 'float)
        (fp   nil))
    (unless (< i (length token)) (reject nil))
    (alt ((char= #\- (aref token i)) (incf i) (setf sign -1))
         ((char= #\+ (aref token i)) (incf i)))
    (zero-or-more (and  (< i (length token)) (digit-char-p (aref token i)))
                  (setf mant (+ (* 10. mant) (digit-char-p (aref token i)))
                        i (1+ i)))
    (alt ((and (< i (length token)) (char= #\. (aref token i)))
          (setf fp t)
          (incf i)
          (zero-or-more
           (and  (< i (length token)) (digit-char-p (aref token i)))
           (setf mant (+ (* 10. mant) (digit-char-p (aref token i)))
                 mexp (1- mexp)
                 i    (1+ i)))))
    (when (and (< i (length token))
               ;; Marker  Meaning                                  
               ;; D or d  double-float                             
               ;; E or e  float (see *read-default-float-format*)  
               ;; F or f  single-float                             
               ;; L or l  long-float                               
               ;; S or s  short-float                              
               (setf type (cdr (assoc (aref token i)
                                      '((#\d . double-float)
                                        (#\e . float)
                                        (#\f . single-float)
                                        (#\l . long-float)
                                        (#\s . short-float))
                                      :test (function char-equal)))))
      (setf fp t)
      (incf i)
      (unless (< i (length token)) (reject nil))
      (alt ((char= #\- (aref token i)) (incf i) (setf esgn -1))
           ((char= #\+ (aref token i)) (incf i)))
      (one-or-more (and  (< i (length token)) (digit-char-p (aref token i)))
                   (setf expo (+ (* 10. expo) (digit-char-p (aref token i)))
                         i (1+ i))))
    (if fp
        (if (= i (length token))
            (accept type
                    (* (coerce (* sign mant) type)
                       (expt 10.0 (+ mexp (* esgn expo)))))
            (reject t "Junk after floating point number ~S" token))    
        (reject nil))))



 (defun test-%make-integer ()
   (dolist (test '(("123"  123)
                   ("+123" 123)
                   ("-123" -123)
                   ("123."  123)
                   ("+123." 123)
                   ("-123." -123)))
     (assert (= (%make-integer (first test)) (second test))
             () "(%MAKE-INTEGER ~S) returned ~S instead of ~S"
             (first test) (%make-integer (first test)) (second test)))
   :success)


(defun test-%make-float ()
  (dolist (test '(("123.0"  123.0)
                  ("+123.0" 123.0)
                  ("-123.0" -123.0)
                  ("123.0"  123.0)
                  ("+123.0" 123.0)
                  ("-123.0" -123.0)
                  

                  ("123e0"  123e0)
                  ("+123e0" 123e0)
                  ("-123e0" -123e0)
                  ("123e0"  123e0)
                  ("+123e0" 123e0)
                  ("-123e0" -123e0)

                  (".123e3"  123e0)
                  ("+.123e3" 123e0)
                  ("-.123e3" -123e0)
                  (".123e3"  123e0)
                  ("+.123e3" 123e0)
                  ("-.123e3" -123e0)

                  ("0.123e3"  123e0)
                  ("+0.123e3" 123e0)
                  ("-0.123e3" -123e0)
                  ("0.123e3"  123e0)
                  ("+0.123e3" 123e0)
                  ("-0.123e3" -123e0)
                  
                  (".123e+3"  123e0)
                  ("+.123e+3" 123e0)
                  ("-.123e+3" -123e0)
                  (".123e+3"  123e0)
                  ("+.123e+3" 123e0)
                  ("-.123e+3" -123e0)

                  ("0.123e+3"  123e0)
                  ("+0.123e+3" 123e0)
                  ("-0.123e+3" -123e0)
                  ("0.123e+3"  123e0)
                  ("+0.123e+3" 123e0)
                  ("-0.123e+3" -123e0)
                                    
                  ("1230e-1"  123e0)
                  ("+1230e-1" 123e0)
                  ("-1230e-1" -123e0)
                  ("1230.0e-1"  123e0)
                  ("+1230.0e-1" 123e0)
                  ("-1230.0e-1" -123e0)



                  ("123s0"  123s0)
                  ("+123s0" 123s0)
                  ("-123s0" -123s0)
                  ("123s0"  123s0)
                  ("+123s0" 123s0)
                  ("-123s0" -123s0)

                  (".123s3"  123s0)
                  ("+.123s3" 123s0)
                  ("-.123s3" -123s0)
                  (".123s3"  123s0)
                  ("+.123s3" 123s0)
                  ("-.123s3" -123s0)

                  ("0.123s3"  123s0)
                  ("+0.123s3" 123s0)
                  ("-0.123s3" -123s0)
                  ("0.123s3"  123s0)
                  ("+0.123s3" 123s0)
                  ("-0.123s3" -123s0)
                  
                  (".123s+3"  123s0)
                  ("+.123s+3" 123s0)
                  ("-.123s+3" -123s0)
                  (".123s+3"  123s0)
                  ("+.123s+3" 123s0)
                  ("-.123s+3" -123s0)

                  ("0.123s+3"  123s0)
                  ("+0.123s+3" 123s0)
                  ("-0.123s+3" -123s0)
                  ("0.123s+3"  123s0)
                  ("+0.123s+3" 123s0)
                  ("-0.123s+3" -123s0)
                                    
                  ("1230s-1"  123s0)
                  ("+1230s-1" 123s0)
                  ("-1230s-1" -123s0)
                  ("1230.0s-1"  123s0)
                  ("+1230.0s-1" 123s0)
                  ("-1230.0s-1" -123s0)



                  ("123f0"  123f0)
                  ("+123f0" 123f0)
                  ("-123f0" -123f0)
                  ("123f0"  123f0)
                  ("+123f0" 123f0)
                  ("-123f0" -123f0)

                  (".123f3"  123f0)
                  ("+.123f3" 123f0)
                  ("-.123f3" -123f0)
                  (".123f3"  123f0)
                  ("+.123f3" 123f0)
                  ("-.123f3" -123f0)

                  ("0.123f3"  123f0)
                  ("+0.123f3" 123f0)
                  ("-0.123f3" -123f0)
                  ("0.123f3"  123f0)
                  ("+0.123f3" 123f0)
                  ("-0.123f3" -123f0)
                  
                  (".123f+3"  123f0)
                  ("+.123f+3" 123f0)
                  ("-.123f+3" -123f0)
                  (".123f+3"  123f0)
                  ("+.123f+3" 123f0)
                  ("-.123f+3" -123f0)

                  ("0.123f+3"  123f0)
                  ("+0.123f+3" 123f0)
                  ("-0.123f+3" -123f0)
                  ("0.123f+3"  123f0)
                  ("+0.123f+3" 123f0)
                  ("-0.123f+3" -123f0)
                                    
                  ("1230f-1"  123f0)
                  ("+1230f-1" 123f0)
                  ("-1230f-1" -123f0)
                  ("1230.0f-1"  123f0)
                  ("+1230.0f-1" 123f0)
                  ("-1230.0f-1" -123f0)


                  ("123d0"  123d0)
                  ("+123d0" 123d0)
                  ("-123d0" -123d0)
                  ("123d0"  123d0)
                  ("+123d0" 123d0)
                  ("-123d0" -123d0)

                  (".123d3"  123d0)
                  ("+.123d3" 123d0)
                  ("-.123d3" -123d0)
                  (".123d3"  123d0)
                  ("+.123d3" 123d0)
                  ("-.123d3" -123d0)

                  ("0.123d3"  123d0)
                  ("+0.123d3" 123d0)
                  ("-0.123d3" -123d0)
                  ("0.123d3"  123d0)
                  ("+0.123d3" 123d0)
                  ("-0.123d3" -123d0)
                  
                  (".123d+3"  123d0)
                  ("+.123d+3" 123d0)
                  ("-.123d+3" -123d0)
                  (".123d+3"  123d0)
                  ("+.123d+3" 123d0)
                  ("-.123d+3" -123d0)

                  ("0.123d+3"  123d0)
                  ("+0.123d+3" 123d0)
                  ("-0.123d+3" -123d0)
                  ("0.123d+3"  123d0)
                  ("+0.123d+3" 123d0)
                  ("-0.123d+3" -123d0)
                                    
                  ("1230d-1"  123d0)
                  ("+1230d-1" 123d0)
                  ("-1230d-1" -123d0)
                  ("1230.0d-1"  123d0)
                  ("+1230.0d-1" 123d0)
                  ("-1230.0d-1" -123d0)
                  

                  
                  ("123l0"  123l0)
                  ("+123l0" 123l0)
                  ("-123l0" -123l0)
                  ("123l0"  123l0)
                  ("+123l0" 123l0)
                  ("-123l0" -123l0)

                  (".123l3"  123l0)
                  ("+.123l3" 123l0)
                  ("-.123l3" -123l0)
                  (".123l3"  123l0)
                  ("+.123l3" 123l0)
                  ("-.123l3" -123l0)

                  ("0.123l3"  123l0)
                  ("+0.123l3" 123l0)
                  ("-0.123l3" -123l0)
                  ("0.123l3"  123l0)
                  ("+0.123l3" 123l0)
                  ("-0.123l3" -123l0)
                  
                  (".123l+3"  123l0)
                  ("+.123l+3" 123l0)
                  ("-.123l+3" -123l0)
                  (".123l+3"  123l0)
                  ("+.123l+3" 123l0)
                  ("-.123l+3" -123l0)

                  ("0.123l+3"  123l0)
                  ("+0.123l+3" 123l0)
                  ("-0.123l+3" -123l0)
                  ("0.123l+3"  123l0)
                  ("+0.123l+3" 123l0)
                  ("-0.123l+3" -123l0)
                                    
                  ("1230l-1"  123l0)
                  ("+1230l-1" 123l0)
                  ("-1230l-1" -123l0)
                  ("1230.0l-1"  123l0)
                  ("+1230.0l-1" 123l0)
                  ("-1230.0l-1" -123l0)
                  
                  ))
    (assert (string= (format nil "~7,3F" (%make-float (first test)))
                     (format nil "~7,3F" (second test)))
            () "(%MAKE-FLOAT ~S) returned ~S instead of ~S"
            (first test) (%make-float (first test)) (second test)))
  :success)






(declaim (inline whitespacep terminating-macro-char-p))
(defun whitespacep (ch) (member ch '(#\space #\newline #\tab)))
(defun terminating-macro-char-p (ch) (member ch '(#\( #\))))

(defun skip-spaces (s)
  (do ()
      ((not (and (currchar s) (whitespacep (currchar s)))))
    (advance s))
  (assert (or (null (currchar s)) (not (whitespacep (currchar s))))))


(defun unescape (token)
  ;; WARNING: This destroys the contents of TOKEN, which must be mutable.
  (let ((dst 0)
        (state :normal))
    (do ((src 0 (1+ src)))
        ((>= src (length token))
         (unless (eq state :normal)
           (error "end-of-file with unfinished token escape."))
         (subseq token 0 dst))
      (ecase state
        ((:normal)
         (case (aref token src)
           ((#\\) (setf state :single))
           ((#\|) (setf state :double))
           (otherwise (setf (aref token dst) (aref token src))
                      (incf dst))))
        ((:single)
         (setf state :normal)
         (setf (aref token dst) (aref token src))
         (incf dst))
        ((:double)
         (case (aref token src)
           ((#\|) (setf state :normal))
           ((#\\) (setf state :double-single))
           (otherwise (setf (aref token dst) (aref token src))
                      (incf dst))))
        ((:double-single)
         (setf state :double)
         (setf (aref token dst) (aref token src))
         (incf dst))))))


(defun test-unescape ()
  (dolist (test '((""  "")
                  ("Hello World"  "Hello World")
                  ("xHello World!" "\\xHello \\World\\!")
                  ("\\Hello \"World\"\\"  "\\\\Hello \\\"World\\\"\\\\")
                  ("Hello World" "|Hello World|")
                  ("Hello World" "|Hello|| World|")
                  ("Hello World" "|Hello| |World|")
                  ("Hello| |World" "|Hello\\| \\|World|")
                  ("Hello\"\\World" "|Hello\\\"\\\\World|")))
    (assert (string= (first test) (unescape (copy-seq (second test))))
            ()
            "(unescape ~S) should give ~S instead of ~S"
            (second test) (first test) (unescape (copy-seq (second test)))))
  :success)


(defun parse-list (s)
  (advance s)                           ; skip over #\(
  (let ((list (%nil)))
    (loop
       (skip-spaces s)  
       (when (char= (reject-eos (currchar s)) #\))
         (advance s)
         (return-from parse-list (%nreverse list)))
       (when (and list
                  (char= (currchar s) #\.)
                  (or (null (nextchar s))
                      (whitespacep (nextchar s))
                      (terminating-macro-char-p (nextchar s))))
         (collect-token s)
         (let ((last-cdr  (parse-object s)))
           (if (char= (reject-eos (currchar s)) #\))
               (progn
                 (advance s)
                 (return-from parse-list (%nreconc list last-cdr)))
               (error "There can be only one object after the dot in a dotted-list."))))
       (%push (parse-object s) list))))

(defun parse-vector (length s)
  (advance s)                           ; skip over #\(
  (if length
      (let ((object nil)
            (vector  (%make-array (list length) nil)))
        (skip-spaces s)
        (do ((i 0 (1+ i)))
            ((not (char/= (reject-eos (currchar s)) #\)))
             (do ((i i (1+ i)))
                 ((not (< i length)))
               (setf (aref vector i) object))
             (advance s)
             vector)
          (setf object (parse-object s))
          (when (< i length)
            (setf (aref vector i) object))
           (skip-spaces s)))
      #- (and)
      (loop
         :with object = nil
         :with vector = (%make-array (list length) nil)
         :for i :from 0
         :do (skip-spaces s)
         :while (char/= (reject-eos (currchar s)) #\))
         :do (setf object (parse-object s))
         :do (when (< i length)
               (setf (aref vector i) object))
         :finally (progn (loop
                            :while (< i length)
                            :do (setf (aref vector i) object))
                         (advance s)
                         (return vector)))
      (let ((object nil)
            (list '()))
         (skip-spaces s)
        (do ((i 0 (1+ i)))
            ((not (char/= (reject-eos (currchar s)) #\)))
             (advance s)
             (%make-array (list (%length list)) (%nreverse list)))
          (%push (parse-object s) list)
          (skip-spaces s)))
      #- (and)
      (loop
         :with object = nil
         :with list = '()
         :for i :from 0
         :do (skip-spaces s)
         :while (char/= (reject-eos (currchar s)) #\))
         :do (push (parse-object s) list)
         :finally (progn (advance s)
                         (return (coerce (nreverse list) 'vector))))))



(defun parse-struct-or-hash (s)
  (let ((data (parse-list s)))
    (if (%symbol-eql (%make-symbol "HASH-TABLE") (%car data))
        (let ((cur (%cdr data)))
          (do ((cur     (%cdr data)
                        (%cdr (%cdr cur)))
               (options (%nil)
                        (%cons (%cons (%car cur) (%car (%cdr cur))) options)))
              ((not (and (%symbolp (%car cur))
                         (char= #\: (aref (%symbol-name (%car cur)) 0))))
               (%make-hash-table options cur))))
        #- (and)
        (loop ; read a hash table
           :with cur = (%cdr data)
           :while (and (%symbolp (%car cur))
                       (char= #\: (aref (%symbol-name (%car cur)) 0)))
           :nconc (%cons (%car cur) (%car (%cdr cur))) :into options
           :do (setf cur (%cdr (%cdr cur)))
           :finally (return))
        (%make-struct (%car data) (%cdr data)))))


;; (let ((h (make-hash-table :test (function equal))))
;;   (setf (gethash "One" h) 1
;;         (gethash "Two" h) 2
;;         (gethash "Three" h) 3)
;;   h)
;; 


(defun parse-array (dimensions s)
  (let ((initial-contents (parse-object s)))
    (labels ((collect-dimensions (n contents dimensions)
             (if (zerop n)
                 (nreverse dimensions)
                 (collect-dimensions (1- n) (first contents)
                                     (cons (length contents) dimensions)))))
      ;; TODO: we rely on make-array to raise some errors that it may not raise...
      (%make-array (collect-dimensions dimensions initial-contents '())
                   initial-contents))))


(declaim (inline make-buffer))
(defun make-buffer ()
  (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))


(defun collect-token (s)
  (case (currchar s)
    ((#\")               ; a string; this should move to macro char...
     (let ((string (make-buffer))
           (state :normal))
       (advance s)
       (do ()
           ((not (currchar s))
            (error 'simple-end-of-file
                   :format-control "Reached end-of-file while reading a string."))
         (ecase state
           ((:normal)
            (case (currchar s)
              ((#\")     (advance s)
               (return-from collect-token (%make-string string)))
              ((#\\)     (setf state :single))
              (otherwise (vector-push-extend (currchar s) string))))
           ((:single)
            (vector-push-extend (currchar s) string)
            (setf state :normal)))
         (advance s))
       #-(and)
       (loop
          :with state = :normal
          :do (advance s)
          :while (currchar s)
          :do (ecase state
                ((:normal)
                 (case (currchar s)
                   ((#\")     (advance s)
                    (return-from collect-token (%make-string string)))
                   ((#\\)     (setf state :single))
                   (otherwise (vector-push-extend (currchar s) string))))
                ((:single)
                 (vector-push-extend (currchar s) string)
                 (setf state :normal)))
          :finally
          (error 'simple-end-of-file
                 :format-control "Reached end-of-file while reading a string."))))
    (otherwise
     (let ((escapedp nil)
           (token (make-buffer))
           (state :normal))
       (do ()
           ((not (and (currchar s)
                      (not (or (whitespacep (currchar s))
                               (terminating-macro-char-p (currchar s))))))
            (unless (eq state :normal)
              (error "end-of-file with unfinished token escape."))
            (cond
              (escapedp
               (%make-symbol token))
              ((every (lambda (ch) (char= #\. ch)) token)
               (%make-dots    token))
              ((%make-float   token))
              ((%make-integer token))
              (t
               (%make-symbol token))))
         (ecase state
           ((:normal)
            (case (currchar s)
              ((#\\) (setf state :single escapedp t))
              ((#\|) (setf state :double escapedp t))
              (otherwise (vector-push-extend (currchar s) token))))
           ((:single)
            (setf state :normal)
            (vector-push-extend (currchar s) token))
           ((:double)
            (case (currchar s)
              ((#\|) (setf state :normal))
              ((#\\) (setf state :double-single))
              (otherwise (vector-push-extend (currchar s) token))))
           ((:double-single)
            (setf state :double)
            (vector-push-extend (currchar s) token)))
         (advance s)))
     #- (and)
     (loop
        :with escapedp = nil
        :with token = (make-buffer)
        :with state = :normal
        :while (and (currchar s)
                    (not (or (whitespacep (currchar s))
                             (terminating-macro-char-p (currchar s)))))
        :do (progn
              (ecase state
                ((:normal)
                 (case (currchar s)
                   ((#\\) (setf state :single escapedp t))
                   ((#\|) (setf state :double escapedp t))
                   (otherwise (vector-push-extend (currchar s) token))))
                ((:single)
                 (setf state :normal)
                 (vector-push-extend (currchar s) token))
                ((:double)
                 (case (currchar s)
                   ((#\|) (setf state :normal))
                   ((#\\) (setf state :double-single))
                   (otherwise (vector-push-extend (currchar s) token))))
                ((:double-single)
                 (setf state :double)
                 (vector-push-extend (currchar s) token)))
              (advance s))
        :finally (progn
                   (unless (eq state :normal)
                     (error "end-of-file with unfinished token escape."))
                   (return
                     (cond
                       (escapedp
                        (%make-symbol token))
                       ((every (lambda (ch) (char= #\. ch)) token)
                        (%make-dots    token))
                       ((%make-float   token))
                       ((%make-integer token))
                       (t
                        (%make-symbol token)))))))))


(defun scan-cardinal (s)
  (let ((token (make-buffer)))
    (do ()
        ((not  (digit-char-p (currchar s)))
         (%make-integer token))
      (vector-push-extend  (currchar s) token)
      (advance s)))
  #- (and)
  (loop
     :with token = (make-buffer)
     :while (digit-char-p (currchar s))
     :do (vector-push-extend  (currchar s) token) (advance s)
     :finally (return (%make-integer token))))


(defun parse-object (s)
  (skip-spaces s)
  (case (reject-eos (currchar s))
    ((#\()
     (parse-list s))
    ((#\#)
     (advance s)
     (case (reject-eos (currchar s))
       ((#\() (parse-vector nil s))
       ((#\A)
        (error "Missing a dimensions argument between # and A"))
       ((#\S) (advance s) (parse-struct-or-hash s))
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
        (let ((arg (scan-cardinal s)))
          (case (reject-eos (currchar s))
            ((#\() (parse-vector arg s))
            ((#\A) (advance s) (parse-array  arg s))
            ((#\S)
             (error "No number allowed between # and S"))
            ((#\@ #\: #\,)
             (error "This simple reader doesn't implement multiple argument or flags for dispatching reader macros."))
            (otherwise
             (error "This simple reader doesn't implement dispatching reader macros other than #(, #A and #S. Rejecting: #~A" (currchar s))))))
       ((#\@ #\: #\,)
        (error "This simple reader doesn't implement multiple argument or flags for dispatching reader macros."))))
    (otherwise (reject-dots (collect-token s)))))


(defun simple-read-from-string (string)
  (let ((s (make-scanner string)))
    (advance s)
    (parse-object s)))



(defun simple-prin1-to-string (object)
  "
OBJECT: is one of the objects made by the various %MAKE- functions.
"
  (with-output-to-string (out)
    (cond
      ((%symbolp object)
       (princ (%symbol-name object) out))
      ((%stringp object)
       (princ "\"" out)
       (do ((i 0 (1+ i)))
           ((>= i (%string-length object)))
         (let ((ch (%string-ref object i)))
           (when (member ch '(#\\ #\"))
             (princ "\\" out))
           (princ ch out)))
       #- (and)
       (loop
          :for ch :in object
          :do (when (member ch '(#\\ #\"))
                (princ "\\" out))
          :do (princ ch out))
       (princ "\"" out))
      ((%integerp object) ; TODO: We'd need an accessor for the integer value
       (princ object out))
      ((%floatp object) ; TODO: We'd need an accessor for the float value (eg decode-float).
       (princ object out))
      ((%consp object)
       (princ "(" out)
       (do ((cur object (%cdr cur))
            (sep "" " "))
           ((not (%consp cur))
            (unless (%null cur)
              (princ " . " out)
              (princ (simple-prin1-to-string cur) out)))
         (princ sep out)
         (princ (simple-prin1-to-string (%car cur)) out))
       (princ ")" out))
      ((%hash-table-p object)
       (princ "#S(HASH-TABLE" out)
       (dolist (item (%hash-table-options object))
         (princ " " out)
         (princ (simple-prin1-to-string (%car item)) out)
         (princ " " out)
         (princ (simple-prin1-to-string (%cdr item)) out))
       (dolist (pair (%hash-table-data object))
         (princ " (" out)
         (princ (simple-prin1-to-string (%car pair)) out)
         (princ " . " out)
         (princ (simple-prin1-to-string (%cdr pair)) out)
         (princ ")" out))
       (princ ")" out))
      ((%structp object)
       (princ "#S(" out)
       (princ (simple-prin1-to-string (%struct-type object)) out)
       (dolist (item (%struct-data object))
         (princ " " out)
         (princ (simple-prin1-to-string item) out))
       (princ ")" out))
      ((%arrayp object)
       (let ((dims  (%array-dimensions object))
             (contents  (%array-collect-contents object)))
         (if (= 1 (%length dims))
             ;; a vector
             (progn
               (princ "#" out)
               (princ (simple-prin1-to-string contents) out))
             ;; a multi-D array
             (progn
               (princ "#" out) (princ (%length dims) out) (princ "A" out)
               (princ (simple-prin1-to-string contents) out)))))
      (t
       (cond
         ((subtypep (class-of (class-of object)) 'structure-class)
          (princ "#S(" out)
          (princ (symbol-name (type-of object)) out)
          (dolist (slot (clos::class-slots (find-class 'dots)))
            (princ " :" out)
            (princ (symbol-name (clos:slot-definition-name slot)) out)
            (princ " " out)
            (princ (simple-prin1-to-string
                    (slot-value object (clos:slot-definition-name slot))) out))
          (princ ")" out))
         (t
          (error "Cannot print objects of type ~S like: ~S"
                 (type-of object) object)))))))

;; #- (and)
;; (untrace make-scanner advance currchar nextchar reject-eos reject-dots 
;;        whitespacep terminating-macro-char-p skip-spaces unescape
;;        test-unescape parse-list parse-vector parse-struct-or-hash parse-array
;;        collect-token parse-object simple-read-from-string)
;; 
;; (print (simple-read-from-string "(\"a\" (a b c) b c-c . d)"))
;; (print (simple-read-from-string "(\"a\" #S(dots :contents \"...\") #(a b c) b c-c . (d 123 123.0 123l0))"))
;; (print (simple-read-from-string "(#1A(1 2 3) #0Afoo \"a\"  #S(dots :contents \"...\") #S(HASH-TABLE :TEST EXT:FASTHASH-EQUAL (\"Three\" . 3) (\"Two\" . 2) (\"One\" . 1)) #(a b c) b c-c . (d 123 123.0 123l0))"))

(defun test-read-print ()
  (dolist
      (test
        '((#1="(\"a\" (a b c) b c-c . d)" #1#)
          ("(\"a\" #S(dots :contents \"...\") #(a b c) b c-c . (d 123 123.0 123l0))"
           "(\"a\" #S(dots :contents \"...\") #(a b c) b c-c d 123 123.0 123.0L0)")
          ("(#1A(1 2 3) #0Afoo \"a\"  #S(dots :contents \"...\") #S(HASH-TABLE :TEST EXT:FASTHASH-EQUAL (\"Three\" . 3) (\"Two\" . 2) (\"One\" . 1)) #(a b c) b c-c . (d 123 123.0 123l0))"
           "(#(1 2 3) #0Afoo \"a\" #S(dots :contents \"...\") #S(HASH-TABLE :TEST EXT:FASTHASH-EQUAL (\"Three\" . 3) (\"Two\" . 2) (\"One\" . 1)) #(a b c) b c-c d 123 123.0 123.0L0)")))
    (assert (string= (simple-prin1-to-string
                      (simple-read-from-string (first test)))
                     (second test))
            ()
            "Test failed:~% ~S~% ~S"
            (simple-prin1-to-string (simple-read-from-string (first test)))
            (second test)))
  :success)


;;;; THE END ;;;;

