;;;; -*- mode:scheme; coding:us-ascii -*-
;;;;**************************************************************************
;;;;FILE:               happy.lisp
;;;;LANGUAGES:          scheme, emacs lisp, Common Lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file demonstrates how one can write a lisp program that can run
;;;;    on both scheme, emacs lisp and Common Lisp.
;;;;
;;;;    Since emacs lisp is close to Common Lisp, we (require 'cl),
;;;;    and implement in scheme the CL primitives we need.
;;;;
;;;;    It can be run for example with:
;;;;
;;;;        mzscheme -f happy.lisp
;;;;        emacs -Q --batch -l happy.lisp -q
;;;;        clisp -q -norc -ansi happy.lisp
;;;;
;;;;
;;;;    Note: this has been tested with emacs-23, not emacs-24, which
;;;;          provides lexical closures, and therefore should be
;;;;          closer to Common Lisp, but which would require some
;;;;          changes to this program.
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-05-27 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2011 - 2016
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

;; mzscheme -f happy.lisp ; emacs -Q --batch -l happy.lisp -q  ; clisp -q -norc -ansi happy.lisp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BOOTSTRAP SECTION
;;;
;;;

((lambda (funcall
          ;; The funcall trick is that in Lisp-2, the parameter is not used,
          schemify
          ;; The schemify function will define some scheme primitive
          ;; in emacs-lisp and Common Lisp to  help us bootstrap in
          ;; the following forms.
          scheme common-lisp emacs-lisp
          ;; We select one of these three thunks depending on the language
          ;; evaluating this.
          ;; but instead the native function.
          )
   (if '()
       (funcall scheme)
       (let ((language emacs-lisp))
         (funcall (let ((language common-lisp)) (lambda () (funcall language schemify)))))))
 (lambda (f) ; funcall
   (f))
 (lambda () ; schemify
   ;; In Common Lisp or Emacs Lisp, we define some scheme
   ;; primitives, to be able to bootstrap as on scheme.

   ;; The problem is that we cannot use eval in scheme
   ;; for lack of an environment
   ;; (interaction-environment is optional and absent
   ;; on eg. mzscheme), so we need to write the rest of
   ;; the file in scheme syntax.

   ;; Perhaps it would be better to write two different schemify for
   ;; emacs-lisp and Common-Lisp...

   (eval (list 'progn

               (if (boundp 'emacs-version)
                   '(progn
                     (defmacro define-symbol-macro (name expansion)
                       (list 'defvar name expansion))
                     (defmacro eval-in-emacs-lisp (&rest expressions)
                       (list* 'progn expressions))
                     (defmacro eval-in-common-lisp (&rest expressions)
                       'nil)
                     (defmacro eval-in-scheme (&rest expressions)
                       'nil))

                   '(progn
                     (defmacro eval-in-emacs-lisp (&body expressions)
                       'nil)
                     (defmacro eval-in-common-lisp (&body expressions)
                       (list* 'progn expressions))
                     (defmacro eval-in-scheme (&body expressions)
                       'nil)))

               (list 'defmacro (intern (if (boundp 'emacs-version)
                                           "begin"
                                           "BEGIN"))
                     '(&body body) '(list* 'progn body))

               (if (boundp 'emacs-version)
                   '(defmacro define-lexical-global (variable expression)
                     (list 'defvar variable expression))
                   '(defmacro define-lexical-global (variable expression)
                     (let ((global (gensym (symbol-name variable))))
                       (list 'progn
                             (list 'define-symbol-macro variable
                                   (list 'symbol-value (list 'quote global)))
                             (list 'setf variable expression)
                             (list 'quote variable)))))

               (list 'defmacro  (intern (if (boundp 'emacs-version)
                                            "define"
                                            "DEFINE"))
                     '(variable expression)
                     '(list 'progn
                       (list  'define-lexical-global variable expression)
                       (list 'defun variable '(&rest arguments)
                        (list 'apply  variable 'arguments))))

               (list  (if (boundp 'emacs-version)
                          'defmacro*
                          'defmacro)
                      (intern (if (boundp 'emacs-version)
                                  "define-syntax"
                                  "DEFINE-SYNTAX"))
                      '(name (syntax-rules vars &rest rules))
                      ;; '(declare (ignore syntax-rule vars rules))
                      ;; We don't do anything, this is only used in
                      ;; scheme to make CL like macros.
                      '(list 'quote name)))))
 (lambda () ; scheme thunk
   'scheme)
 (lambda (schemify) ; common-lisp thunk
   (funcall schemify)
   'common-lisp)
 (lambda (schemify) ; emacs-lisp thunk
   (eval '(require 'cl))
   (funcall schemify)
   'emacs-lisp))


;; We define a language variable bound to a symbol naming the language.

(define language ((lambda ()
                    (if '()
                        'scheme
                        (let ((language 'emacs-lisp))
                          (funcall (let ((language 'common-lisp)) (lambda () language))))))))


(case language
  ((common-lisp)

   ;; We define this reader macro to neutralize read-error on "(expr ...)"
   ;; Notice this form is read and parsed by scheme.  Therefore no LOOP.

   (define-condition simple-reader-error (simple-error reader-error)
     (dummy))

   (defun reader-macro-scheme-list (stream ch)
     "Left parenthesis macro reader that accepts ... in lists, and read |...| for them."
     (declare (ignore ch))
     (flet ((serror (condition stream control-string &rest arguments)
              ;; Keywords are not necessarily accepted by all scheme readers,
              ;; and this must be read by a lisp reader (and even parsed).
              (error condition
                     :stream stream
                     :format-control control-string
                     :format-arguments arguments)))
       (let* ((result     (cons nil nil))
              (last-cons  result)
              (last-cdr-p nil))
         (do ((ch (progn (peek-char t stream nil t)
                         (read-char stream t nil t))
                  (progn (peek-char t stream nil t)
                         (read-char stream t nil t))))
             ((char= (character ")") ch)
              (if (eq last-cdr-p 't)
                  (serror 'simple-reader-error stream
                          "illegal end of dotted list")
                  (cdr result)))
           (labels ((collect (objects)
                      (assert (listp objects))
                      (when objects
                        (case last-cdr-p
                          ((nil)
                           (setf (cdr last-cons) objects
                                 ;; (list (first objects))
                                 last-cons       (cdr last-cons)))
                          ((t)
                           (setf (cdr last-cons) (first objects)
                                 last-cdr-p      'done))
                          ((done)
                           (serror 'simple-reader-error stream
                                   "illegal end of dotted list"))))))
             (cond
               ((char= (character ";") ch)
                ;; temporary kludge, we reset the readtable when done with scheme.
                (read-line stream))
               ((char= (character ".") ch)
                (let ((nextch (peek-char nil stream t nil t)))
                  (cond
                    ((char= (character ".") nextch)
                     ;; We got two dots, let's assume it'll only be dots,
                     ;; and collect a symbol.
                     (collect (list (do ((res (list ch) (cons ch res)))
                                        ((char/= ch (peek-char nil stream t nil t))
                                         (intern (coerce res 'string)))
                                      (read-char stream nil nil t)))))
                    ((or (find nextch " ()\";#")
                         (char= nextch (code-char 13))
                         (char= nextch (code-char 10)))
                     (if (eql result last-cons)
                         (serror 'simple-reader-error stream
                                 "missing an object before the \".\" in a cons cell")
                         (case last-cdr-p
                           ((nil)
                            (setf last-cdr-p t))
                           ((t)
                            (serror 'simple-reader-error stream
                                    "token \".\" not allowed here"))
                           ((done)
                            (serror 'simple-reader-error stream
                                    "illegal end of dotted list")))))
                    (t
                     (collect (list
                               (with-input-from-string (dot ".")
                                 (read (make-concatenated-stream dot stream) t nil t))))))))
                 (t
                   (unread-char ch stream)
                   (collect (list (read stream t nil t))))))))))

   (set-macro-character (character "(")
                        (lambda (stream ch)
                          ;; This stub is so that when we redefine
                          ;; the reader while debugging, the new
                          ;; version be taken into account immediately.
                          (reader-macro-scheme-list stream ch)))

   ;; (set-syntax-from-char (character ".") (character "A"))
   ;; (defvar *normal-readtable* (copy-readtable))
   ;; (set-macro-character
   ;;  (character ".")
   ;;  (lambda (stream ch)
   ;;    (print (list 'dot-reader stream))
   ;;    ;; Keywords are not necessarily accepted by scheme readers,
   ;;    ;; but life would be difficult without them.
   ;;    (if (char= ch (peek-char nil stream))
   ;;        (values
   ;;         ;; We got two dots, let's assume it'll only be dots,
   ;;         ;; and return a symbol.
   ;;         (do ((res (list ch) (cons ch res)))
   ;;             ((char/= ch (peek-char nil stream))
   ;;              (intern (coerce res 'string)))
   ;;           (read-char stream nil)))
   ;;        (let ((*readtable* *normal-readtable*))
   ;;          ;; We got only one dot, let's assume it's a normal token,
   ;;          ;; and let the normal reader handle it.
   ;;          (with-input-from-string (dot ".")
   ;;            (read (make-concatenated-stream dot stream) t nil t))))))

   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UPGRADING SCHEME TO COMMON-LISP
;;;
;;;


;; Notice that the form in eval-in-* must still be read in all three
;; languages.

(define-syntax eval-in-scheme
    (syntax-rules ()
                  ((eval-in-scheme expr)
                   expr)
                  ((eval-in-scheme expr ...)
                   (begin expr ...))))

(define-syntax eval-in-emacs-lisp
    (syntax-rules ()
                  ((eval-in-emacs-lisp expr ...)
                   'nil)))

(define-syntax eval-in-common-lisp
    (syntax-rules ()
                  ((eval-in-common-lisp expr ...)
                   'nil)))


;; Remember, define-syntax is defined by schemify above in emacs-lisp
;; and Common-Lisp to do nothing.

(define-syntax defun
    (syntax-rules ()
                  ((defun name parameters body ...)
                   (define name (lambda parameters body ...)))))

(define-syntax defparameter
    (syntax-rules ()
                  ((defparameter name value)
                   (define name value))))

(define-syntax define-lexical-global
    (syntax-rules ()
                  ((define-lexical-global name value)
                   (define name value))
                  ((define-lexical-global name value docstring)
                   (define name value))))

(define-syntax defconstant
    (syntax-rules ()
                  ((defconstant name value)
                   (define name value))
                  ((defconstant name value docstring)
                   (define name value))))


(eval-in-scheme

 ;; We don't have &rest yet, so we use a dotted parameter list:
 (define funcall  (lambda (function . arguments) (apply function arguments)))
 (defun function (name)   name)
 (defun identity (object) object)



 (defun eql   (a b) (eqv?   a b)) ;; TODO: implement it correctly.
 (defun equal (a b) (equal? a b)) ;; TODO: implement it correctly.


 (defconstant nil '())  ; nil is false.
 (defun null  (x) (eql nil x))

 (defconstant t 't)
 (defun .boolean (generalized-boolean)
   "Transforms the GENERALIZED-BOOLEAN into a native boolean value.
false is the symbol nil, the empty list, and not true (#f)."
   (not (or (not (eql 'nil generalized-boolean))
            (not (eql '()  generalized-boolean))
            (not generalized-boolean)))))

(define-syntax setq
    (syntax-rules ()
                  ((setq var expr)
                   (set! var expr))
                  ((setq var expr other ...)
                   (begin (set! var expr)
                          (setq other ...)))))

(define-syntax setf ; for now, only variables and a few built-ins
    (syntax-rules ()
                  ((setf (car place) expr)
                   (set-car! place expr))
                  ((setf (cdr place) expr)
                   (set-cdr! place expr))
                  ((setf (aref place index) expr)
                   (vector-set! place index expr))
                  ;; ...
                  ((setf var expr)
                   (set! var expr))
                  ((setf var expr other ...)
                   (begin (set! var expr)
                          (setf other ...)))))

(eval-in-scheme

 (defun aref (vos index)
   (if (vector? vos)
       (vector-ref vos index)
       (string-ref vos index)))

 (defun nth (index list)
   (let loop ((list  list)
              (index index))
        (cond
          ((null list) list)
          ((= 0 index) (car list))
          (else        (loop (- index 1) (cdr list))))))

 (defun elt (seq index)
   (cond
     ((vector? seq) (vector-ref seq index))
     ((string? seq) (string-ref seq index))
     ((list?   seq) (nth index seq))))



 (define terpri
     (lambda optional-parameter
       (apply newline optional-parameter)))

 (define print
     (lambda parameters
       (if (null (cdr parameters))
           (newline)
           (newline (cadr parameters)))
       (apply write parameters)))

 )


;;; We don't need to read ... anymore, so we revert to the standard
;;; readtable in CL:

(eval-in-common-lisp
 (setf *readtable* (copy-readtable))
 (setf *print-case* :downcase))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A FEW EMACS-LISP ADJUSTMENTS
;;;
;;;

(eval-in-emacs-lisp
 (defun print (object &optional printcharfun)
   (terpri printcharfun)
   (princ (prin1-to-string object) printcharfun)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The language defined so far.
;;;

;; The intersection of r5rs, emacs-lisp and Common Lisp,
;; defined as a Common Lisp package.

(eval-in-common-lisp

 (defpackage "INTERSECTION-CL-EL-R5RS"
   (:use "COMMON-LISP")
   (:export quote lambda if cond case
            and or not
            let let* do
            = < > <= >=
            max min
            + * - /
            floor ceiling truncate round
            rationalize
            exp log sin cos tan asin acos atan sqrt expt
            gcd lcm
            numerator denominator
            cons
            car cdr
            caar cadr cdar cddr
            caaar caadr cadar caddr cdaar cdadr cddar cdddr
            caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
            cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
            list length append reverse
            char-upcase char-downcase
            make-string
            vector
            apply
            read read-char peek-char
            write write-char
            load)

   ;; we added (with restrictions!):
   (:export funcall function identity
            defun defconstant
            eql equal
            nil null t
            aref elt nth
            setq setf
            terpri print)

   ;; We defined those additionnal symbols:
   (:import-from :common-lisp-user
                 language scheme emacs-lisp common-lisp
                 define-lexical-global .boolean)

   (:export language
            scheme emacs-lisp common-lisp ; names of the underlying languages
            define-lexical-global ; to be used instead of define or defparameter
            .boolean          ; to be used to get a native boolean from a value
            )

   (:documentation "

This package exports the operators from COMMON-LISP that exist also in
R5RS scheme, with the same behavior (for some subset of the parameter
ranges).  If we take care of restricting our scheme, then we can write
code that can be evaluated with the same semantics both in scheme and
in Common Lisp.

It may be tried out in the  INTERSECTION-CL-EL-R5RS-USER package.

For examples:

    COND is CL:COND therefore we cannot use ELSE; instead write a true
    conditions, like (= 0 0).

    CASE is CL:CASE therefore we cannot use ELSE.
    (In scheme, CASE is scheme case, therefore we cannot use OTHERWISE either).

    LAMBDA is CL:LAMBDA therefore we cannot use a symbol or a dotted list for
    the lambda-list. On the other hand, CL:lambda also accepts
    extended parameter lists that scheme programs must refrain from
    using.

    MAKE-STRING is CL:MAKE-STRING that takes keyword arguments.
    Scheme programs must call it with only one argument and cannot
    pass an initial element.

    IF and COND are CL:IF and CL:COND and take only CL:NIL as
    false. Therefore scheme programs must restrict themselves to the
    available predicates as test expressions, and must avoid to give
    the empty list.

    While DEFINE and LETREC are missing, we can write:
      (let ((fact (lambda (fact x)
                     (if (< x 1)
                         1
                         (* x (apply fact fact (- x 1) '()))))))
         (apply fact fact 20 '()))
      --> 2432902008176640000

A few additionnal primitives are defined in scheme, and exported from
this package:

    FUNCALL FUNCTION IDENTITY
    DEFUN DEFCONSTANT
    EQL EQUAL
    NIL NULL T
    AREF ELT NTH
    SETQ SETF
    TERPRI PRINT

along with:
    DEFINE-LEXICAL-GLOBAL ; to be used instead of define or defparameter
    .BOOLEAN              ; to be used to get a native boolean from a value


The rest of Common Lisp can be implemented from here.
")))

(eval-in-common-lisp
 (defpackage "INTERSECTION-CL-EL-R5RS-USER"
   (:use "INTERSECTION-CL-EL-R5RS"))
 (in-package "INTERSECTION-CL-EL-R5RS-USER"))


;; Now we can rewrite Common Lisp with INTERSECTION-CL-EL-R5RS ;-)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; APPLICATION SPECIFIC CODE
;;;
;;;

(print (list 'booted 'a 'lisp 'over language))

(defun fact (x)
  (if (< x 1)
      1
      (* x (fact (- x 1)))))

(print (list '(fact 10) '= (fact 10))) ; don't try more, emacs-lisp has only int32...
(terpri)



;; [pjb@kuiper :0 lisp]$ mzscheme -f happy.lisp ; emacs -Q --batch -l happy.lisp -q  ; clisp -q -norc -ansi happy.lisp
;;
;; (booted a lisp over scheme)
;; ((fact 10) = 3628800)
;;
;; (booted a lisp over emacs-lisp)
;; ((fact 10) = 3628800)
;;
;; (booted a lisp over common-lisp)
;; ((fact 10) = 3628800)
;; [pjb@kuiper :0 lisp]$

;;;; THE END ;;;;
