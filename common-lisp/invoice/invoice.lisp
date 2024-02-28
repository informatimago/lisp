;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               invoices.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     Common-Lisp
;;;;DESCRIPTION
;;;;
;;;;    This package exports classes and functions used for accounting:
;;;;    invoices, customers/providers, movements, taxes...
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-10-17 <PJB> Completed conversion to Common-Lisp.
;;;;    2004-10-11 <PJB> Converted to Common-Lisp from emacs lisp.
;;;;    2002-09-09 <PJB> Added generate-invoice.
;;;;    199?-??-?? <PJB> Creation.
;;;;BUGS
;;;;    Currencies are handled, but multicurrency accounting is not.
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2016
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
;;;;****************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.INVOICE.INVOICE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ISO4217")
  #+mocl (:shadowing-import-from "COM.INFORMATIMAGO.MOCL.KLUDGES.MISSING"
                                 "*TRACE-OUTPUT*"
                                 "*LOAD-VERBOSE*"
                                 "*LOAD-PRINT*"
                                 "ARRAY-DISPLACEMENT"
                                 "CHANGE-CLASS"
                                 "COMPILE"
                                 "COMPLEX"
                                 "ENSURE-DIRECTORIES-EXIST"
                                 "FILE-WRITE-DATE"
                                 "INVOKE-DEBUGGER" "*DEBUGGER-HOOK*"
                                 "LOAD"
                                 "LOGICAL-PATHNAME-TRANSLATIONS"
                                 "MACHINE-INSTANCE"
                                 "MACHINE-VERSION"
                                 "NSET-DIFFERENCE"
                                 "RENAME-FILE"
                                 "SUBSTITUTE-IF"
                                 "TRANSLATE-LOGICAL-PATHNAME"
                                 "PRINT-NOT-READABLE"
                                 "PRINT-NOT-READABLE-OBJECT")
  (:export "LOAD-JOURNAL" "JOURNAL-ENTRY" "LINE" "PERSON" "GENERATE" "TRIMESTRE"
           "MAKE-BANK-REFERENCE" "JOURNAL" "MOVEMENT" "INVOICE-SET" "INVOICE"
           "INVOICE-LINE" "FISCAL-PERSON" "BANK-REFERENCE" "PJB-OBJECT" "*JOURNAL*"
           "*INVOICE-SET*" "*CURRENCY-READTABLE*")
  (:shadow "ABS" "ZEROP" "ROUND" "/=" "=" ">=" ">" "<=" "<" "/" "*" "-" "+")
  (:documentation "

This package exports classes and functions used for accounting:
invoices, customers/providers, movements, taxes...


License:

    AGPL3

    Copyright Pascal J. Bourguignon 1990 - 2012

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.INVOICE.INVOICE")


;;; parameters

(defparameter *vat-rates* '(0/100 4/100 7/100 16/100)
  "The valid VAT rates in the country of the user.")


(defparameter *invoice-directory-path*
  '(:absolute "HOME""PASCAL""JOBS""FREE-LANCE""INVOICES")
  "The directory where the generated invoices are stored.")


(defparameter *invoice-set-file-path*
  (make-pathname :directory *invoice-directory-path*
                 :name "INVOICES" :type "DATA")
  "Path to the file where invoices are stored.")


;;; global variables:

(defparameter *default-currency* (find-currency :eur)
  "The currency used when no prefix currency code is given to #m")


(defparameter *max-movement-amount* nil
  "The maximum movement amount (ht or ttc, expressed in the currency of the
movement (weak, I know).")


(defparameter *invoice-set* nil
  "Current Invoice Set (instance of INVOICE-SET).") ;;invoice-set


(defparameter *journal* nil
  "Current Journal (instance of JOURNAL).")


;;;---------------------------------------------------------------------

(defgeneric abs (self))
(defgeneric add-entry (self entry))
(defgeneric add-invoice (self invoice))
(defgeneric add-line (self line))
(defgeneric add-person (self person &optional fisc))
(defgeneric amount-ht (self))
(defgeneric compute-totals (self))
(defgeneric credit-ht (self))
(defgeneric credit-vat (self))
(defgeneric currency (self))
(defgeneric amount-magnitude (self))
(defgeneric debit-ht (self))
(defgeneric debit-vat (self))
(defgeneric debit-vat-corriente (self))
(defgeneric debit-vat-inversion (self))
(defgeneric ensure-sorted (self))
(defgeneric extract (self year trimestre))
(defgeneric generate (invoice &key stream verbose language &allow-other-keys)
  (:documentation   "
DO:      Generate this invoice into a file in the directory
         *INVOICE-DIRECTORY-PATH*.
RETURN:  The path to the file generated.
"))
(defgeneric get-invoice-with-issuer-and-number (self issuer-fiscal-id invoice-number))
(defgeneric get-person-with-fiscal-id (self fiscal-id))
(defgeneric invoices (self))
(defgeneric is-credit (self))
(defgeneric is-refund (self))
(defgeneric negativep (self))
(defgeneric positivep (self))
(defgeneric reset (self))
(defgeneric round (self &optional divisor))
(defgeneric vat-rate (self))
(defgeneric write-invoice-file (self &key language))
(defgeneric zerop (self))

;;;---------------------------------------------------------------------
;;; Monetary Amounts & Currency Syntax
;;;---------------------------------------------------------------------

;; Since floating point arithmetic is not adapted to accounting,
;; we will use integers for monetary amounts, and
;; percentages will be expressed as rationnals: 16 % = 16/100
;;
;; 123.45 ¤ = 12345 ¢ = #978m123.45
;;
;; In addition monetary amounts are tagged with a currency, and
;; arithmetic operations are type-restricted.
;;
;; The reader syntax is: # [currency-code] m|M [+|-] digit+ [ . digit* ]
;; The currency code must be a numeric code of a currency found
;; in (com.informatimago.common-lisp.cesarum.iso4217:get-currencies),
;; otherwise a read-time error is issued.
;; When the currency-code is not present, the currency designated
;; by *DEFAULT-CURRENCY* is used.
;; The number of digits after the decimal point must not be superior
;; to the minor unit attribute of the currency.
;; The value is converted to an integer number of minor unit and
;; the read result is an AMOUNT structure gathering the currency
;; and the value.
;;
;; The operations defined on AMOUNT values are:
;;
;; c: amount* --> boolean
;; with c in { <, <=, >, >=, =, /= }.
;;
;; +: amount* --> amount
;; -: amount* --> amount
;; *: amount X real* --> amount (commutatif and associatif)
;; /: amount X real* --> amount (not commutatif and not associatif)
;;
;; [ set* = Kleene closure of the set ]
;;
;; For now, all these operations work only when the currency of all amount
;; involved is the same.
;;
;; These Common-Lisp operators are shadowed, and functions are defined for
;; them, that extend the normal numeric functions for amounts.
;;
;; The AMOUNT structure has a printer that prints different format
;; depending on the *PRINT-READABLY*. It uses the reader syntax defined
;; above when *PRINT-READABLY* is true, or a "~V$ ~3A" format printing
;; the value followed by the alphabetic code of the currency.


(defstruct (amount (:predicate amountp)
                   #|(:PRINT-OBJECT PRINT-OBJECT)|#)
  "An amount of money."
  currency
  (value 0 :type integer))


(defmethod print-object ((self amount) stream)
  (if *print-readably*
      (format stream "#~DM~V$"
              (currency-numeric-code (amount-currency self))
              (currency-minor-unit (amount-currency self))
              (amount-magnitude self))
      (format stream "~V$ ~A"
              (currency-minor-unit (amount-currency self))
              (amount-magnitude self)
              (currency-alphabetic-code (amount-currency self))))
  self) ;;PRINT-OBJECT


(defmethod currency ((self number))
  (declare (ignorable self))
  nil)


(defmethod currency ((self amount))
  (amount-currency self))


(defmethod amount-magnitude ((self number))
  self)


(defmethod amount-magnitude ((self amount))
  "
RETURN: A real equal to the value of the amount.
"
  (* (amount-value self)
     (aref #(1 1/10 1/100 1/1000 1/10000)
           (currency-minor-unit (amount-currency self)))))


(defparameter *zero-amounts* (make-hash-table :test (function eq))
  "A cache of 0 amount for the various currencies used.")


(defun amount-zero (currency)
  "
RETURN: A null amount of the given currency.
"
  (let ((zero (gethash (find-currency currency) *zero-amounts*)))
    (unless zero
      (setf zero
            (setf (gethash (find-currency currency) *zero-amounts*)
                  (make-amount :currency (find-currency currency) :value 0))))
    zero)) ;;AMOUNT-ZERO


(defmethod abs       ((self number)) (common-lisp:abs   self))
(defmethod abs       ((self amount))
  (make-amount :currency (amount-currency self)
               :value    (common-lisp:abs   (amount-value self))))


(defmethod zerop     ((self number)) (common-lisp:zerop self))
(defmethod zerop     ((self amount)) (common-lisp:zerop (amount-value self)))


(defmethod positivep ((self number)) (common-lisp:<= 0 self))
(defmethod positivep ((self amount)) (common-lisp:<= 0 (amount-value self)))


(defmethod negativep ((self number)) (common-lisp:> 0 self))
(defmethod negativep ((self amount)) (common-lisp:> 0 (amount-value self)))


(defmethod round ((self real) &optional (divisor 1))
  (common-lisp:round self divisor))


(defmethod round ((self amount) &optional (divisor 1))
  (make-amount :currency (amount-currency self)
               :value (common-lisp:round (amount-value self) divisor)))


(defun euro-round (magnitude currency)
  "
MAGNITUDE:  A REAL
CURRENCY:   The currency of the amount.
RETURN:     An integer in minor unit rounded according to the Euro rule."
  (let ((rounder (aref #(1 1/10 1/100 1/1000 1/10000)
                       (currency-minor-unit currency))))
    (round (+ magnitude (* (signum magnitude) (/ rounder 10))) rounder)))


(defun euro-value-round (value)
  "
VALUE:      A REAL
CURRENCY:   The currency of the amount.
RETURN:     An integer in minor unit rounded according to the Euro rule."
  (round (+ value (* (signum value) 1/10)))) ;;EURO-VALUE-ROUND



;; (with-output-to-string (out)
;;   (dolist (*print-readably* '(t nil))
;;     (print (make-amount :currency (find-currency :EUR) :value 12345) out)))


(define-condition multi-currency-error (error)
  ((format-control   :initarg :format-control   :accessor format-control)
   (format-arguments :initarg :format-arguments :accessor format-arguments)
   (operation :initarg :operation :accessor multi-currency-error-operation)
   (amounts   :initarg :amounts   :accessor multi-currency-error-amounts))
  (:report (lambda (self stream)
             (let ((*print-pretty* nil))
               (format stream "~A: (~A  ~{~A~^, ~})~%~A"
                       (class-name (class-of self))
                       (multi-currency-error-operation self)
                       (multi-currency-error-amounts self)
                       (apply (function format) nil (format-control self)
                              (format-arguments self)))))))


(defun mcerror (operation amounts format-control &rest format-arguments)
  (error 'multi-currency-error
         :operation operation :amounts amounts
         :format-control format-control
         :format-arguments format-arguments))



(defun types-of-arguments (args)
  (labels ((display
               (item)
             (cond ((symbolp item) (symbol-name item))
                   ((atom item) item)
                   (t (mapcar (function display) item)))))
    (mapcar (lambda (arg) (display (type-of arg))) args)))


(defmacro make-comparison-method (name operator)
  "
DO:     Generate a comparison method.
"
  `(defun ,name (&rest args)
     (cond
       ((every (function numberp) args)
        (apply (function ,operator) args))
       ((every (function amountp) args)
        (let ((currency (find-currency (amount-currency (first args)))))
          (if (every (lambda (x) (eq currency (find-currency (amount-currency x))))
                     (cdr args))
              (apply (function ,operator)  (mapcar (function amount-value) args))
              (mcerror ',name args  "Comparison not implemented yet."))))
       (t (mcerror ',name args  "Incompatible types: ~A"
                   (types-of-arguments args))))))


(make-comparison-method <  common-lisp:<)
(make-comparison-method <= common-lisp:<=)
(make-comparison-method >  common-lisp:>)
(make-comparison-method >= common-lisp:>=)
(make-comparison-method =  common-lisp:=)
(make-comparison-method /= common-lisp:/=)


(defun + (&rest args)
  "
DO:    A Generic addition with numbers or amounts.
"
  (setf args (remove 0 args
                     :key  (lambda (x) (if (typep x 'amount) (amount-value x) x))
                     :test (function equal)))
  (cond
    ((every (function numberp) args)
     (apply (function common-lisp:+) args))
    ((every (function amountp) args)
     (let ((currency (find-currency (amount-currency (first args)))))
       (if (every (lambda (x) (eq currency (find-currency (amount-currency x))))
                  (cdr args))
           (make-amount :currency currency
                        :value (apply (function common-lisp:+)
                                      (mapcar (function amount-value) args)))
           (mcerror '+ args  "Addtion not implemented yet."))))
    (t   (mcerror '+ args  "Incompatible types: ~A" (types-of-arguments args)))))


(defun - (&rest args)
  "
DO:    A Generic substraction with numbers or amounts.
"
  (setf args (cons (car args)
                   (remove 0 (cdr args)
                           :key (lambda (x) (if (typep x 'amount) (amount-value x) x))
                           :test (function equal))))
  (cond
    ((every (function numberp) args)
     (apply (function common-lisp:-) args))
    ((zerop (first args))
     (- (apply (function +) (rest args))))
    ((every (function amountp) args)
     (let ((currency (find-currency (amount-currency (first args)))))
       (if (every (lambda (x) (eq currency (find-currency (amount-currency x))))
                  (cdr args))
           (make-amount :currency currency
                        :value (apply (function common-lisp:-)
                                      (mapcar (function amount-value) args)))
           (mcerror '- args  "Substraction not implemented yet."))))
    (t   (mcerror '- args  "Incompatible types: ~A" (types-of-arguments args)))))


(defun * (&rest args)
  "
DO:    A Generic multiplication with numbers or amounts.
"
  (if (every (function numberp) args)
      (apply (function common-lisp:*) args)
      (let ((p (position-if (function amountp) args)))
        (cond
          ((or (null p) (not (every (lambda (x) (or (amountp x)(realp x))) args)))
           (mcerror '* args  "Incompatible types: ~A" (types-of-arguments args)))
          ((position-if (function amountp) args :start (1+ p))
           (mcerror '* args  "Cannot multiply moneys."))
          (t
           (make-amount
            :currency (amount-currency (nth p args))
            :value (euro-value-round
                    (apply (function common-lisp:*)
                           (mapcar (lambda (x) (if (amountp x) (amount-value x) x))
                                   args)))))))))


(defun / (&rest args)
  "
DO:    A Generic division with numbers or amounts.
"
  (cond
    ((every (function numberp) args)
     (apply (function common-lisp:/) args))
    ((and (cadr args)
          (not (cddr args))             ; two arguments
          (amountp (first  args))
          (amountp (second args)))      ; both amounts
     ;; then return a number:
     (/ (amount-value (first args)) (amount-value (second args))))
    ((and (amountp (car args))
          (cdr args) ;; cannot take the inverse of an amount!
          (every (function realp) (cdr args)))
     (make-amount
      :currency (amount-currency (car args))
      :value (euro-value-round (apply (function common-lisp:/)
                                      (amount-value (car args)) (cdr args)))))
    (t (mcerror '/ args  "Incompatible types: ~A" (types-of-arguments args)))))



(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun currency-syntax (stream char infix)
    (declare (ignore char))
    (let ((currency (or infix *default-currency*)))
      (setf currency (find-currency currency))
      (unless currency
        (mcerror 'currency-syntax (or infix *default-currency*)
                 "Invalid currency designator ~S" (or infix *default-currency*)))
      (assert (<= 0 (currency-minor-unit currency) 4) ()
              "Unexpected  minor unit for currency: ~S" currency)
      (let ((left '())
            (right '())
            (dot nil)
            (sign 1))
        (let ((ch (read-char stream nil nil)))
          (cond
            ((null ch))
            ((char= ch (character "-" )) (setf sign -1))
            ((char= ch (character "+" )))
            (t (unread-char ch stream))))
        (loop for ch = (peek-char nil stream nil nil)
           while (and ch (digit-char-p ch))
           do (push (read-char stream) left)
           finally (setf dot (and ch (char= (character ".") ch))))
        (when (zerop (length left))
          (mcerror 'currency-syntax currency "Missing an amount after #M"))
        (when dot
          (when (zerop (currency-minor-unit currency))
            (mcerror 'currency-syntax currency
                     "There is no decimal point in ~A" (currency-name currency)))
          (read-char stream) ;; eat the dot
          (loop for ch = (peek-char nil stream nil nil)
             while (and ch (digit-char-p ch))
             do (push (read-char stream) right))
          (when (< (currency-minor-unit currency) (length right))
            (mcerror 'currency-syntax currency
                     "Too many digits after the decimal point for ~A"
                     (currency-name currency))))
        (loop for i from (length right) below (currency-minor-unit currency)
           do (push (character "0") right))
        (make-amount
         :currency currency
         ;; (WITH-STANDARD-IO-SYNTAX
         ;;     (INTERN (CURRENCY-ALPHABETIC-CODE CURRENCY) "KEYWORD"))
         :value (* sign (parse-integer
                         (map 'string (function identity)
                              (nreverse (nconc right left)))))
         ;;:divisor (AREF #(1 10 100 1000 10000)
         ;;   (CURRENCY-MINOR-UNIT CURRENCY))
         )))) ;;currency-syntax


  (defparameter *currency-readtable* (copy-readtable *readtable*)
    "The readtable used to read currencies.")


  (set-dispatch-macro-character  #\# #\M (function currency-syntax)
                                 *currency-readtable*)

  (set-dispatch-macro-character  #\# #\M (function currency-syntax)
                                 *currency-readtable*)
  ) ;;eval-when


;; (let ((*readtable* *currency-readtable*))
;;   (mapcar
;;    (lambda (s)
;;      (let ((cnt 0))
;;        (list s
;;              (handler-case (multiple-value-bind (r l) (read-from-string s)
;;                              (setf cnt l) r)
;;                (error (err)
;;                  (apply (function format) nil #||*error-output*||#
;;                         (simple-condition-format-control err)
;;                         (simple-condition-format-arguments err))))
;;              (subseq s cnt))))
;;    '("#M123.45"  "#840m123.45" "#548m123" "#788m123.456"
;;      "#840m123" "#840m123.xyz" "#840m123.4" "#840m123.45" "#840m123.456"
;;      "#197M123.45"  "#548m123" "#548m123.xyz" "#548m123.4" "#548m123.45"
;;      "#548m123.456" "#788m123" "#788m123.xyz" "#788m123.4" "#788m123.45"
;;      "#788m123.456" "#788m123.4567" "#788m123.45678" )))
;;
;; (let ((*readtable* *currency-readtable*)) (read-from-string "#197M123.45" ))
;; (let ((*readtable* *currency-readtable*)) (read-from-string "#M960" ))
;; (let ((*readtable* *currency-readtable*)) (read-from-string "#978M978" ))


;;;---------------------------------------------------------------------
;;; DATE
;;;---------------------------------------------------------------------


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +seconds-in-a-day+ (cl:* 24 3600) "Number of seconds in a day.")
  );;eval-when

(defstruct (date #|(:PRINT-OBJECT PRINT-OBJECT)|#) year month day)


(defmethod print-object ((self date) stream)
  (format stream "~4,'0D-~2,'0D-~2,'0D"
          (date-year self) (date-month self) (date-day self))
  self)


(defun date-from-string (yyyy-mm-dd)
  (let ((ymd (split-string yyyy-mm-dd "-")))
    (make-date :year  (parse-integer (nth 0 ymd))
               :month (parse-integer (nth 1 ymd))
               :day   (parse-integer (nth 2 ymd))))) ;;DATE-FROM-STRING


(defun date-after (a b)
  (or (> (date-year a) (date-year b))
      (and (= (date-year a) (date-year b))
           (or (> (date-month a) (date-month b))
               (and (= (date-month a) (date-month b))
                    (> (date-day a) (date-day b))))))) ;;DATE-AFTER


(defun date-in-year-trimestre (date year trimestre)
  "
RETURN: Whether the given date is within the given YEAR and TRIMESTRE.
"
  (and (= (date-year date) year)
       (member (date-month date)
               (elt '((1 2 3) (4 5 6) (7 8 9) (10 11 12))
                    (- trimestre 1))))) ;;DATE-IN-YEAR-TRIMESTRE


(defun calendar-current-date ()
  "
RETURN: The date today.
"
  (multiple-value-bind (se mi ho da mo ye dw ds zo) (get-decoded-time)
    (declare (ignore se mi ho dw ds zo))
    (make-date :year ye :month mo :day da))) ;;CALENDAR-CURRENT-DATE


(defun local-time-zone ()
  "
RETURN: The local time zone, as returned by GET-DECODED-TIME.
"
  (multiple-value-bind (se mi ho da mo ye dw ds zone) (get-decoded-time)
    (declare (ignore se mi ho da mo ye dw ds))
    zone)) ;;LOCAL-TIME-ZONE


(defun universal-time-to-date (utime)
  "
RETURN: the given universal time formated in the ISO8601 YYYY-MM-DD format.
"
  (multiple-value-bind (se mi ho da mo ye dw ds zo)
      (decode-universal-time utime 0)
    (declare (ignore se mi ho dw ds zo))
    (format nil "~4,'0D-~2,'0D--~2,'0D" ye mo da))) ;;UNIVERSAL-TIME-TO-DATE


(defun date-to-universal-time (date-string)
  "
DATE-STRING:  A date in the ISO8601 format 'YYYY-MM-DD'.
RETURN:       A number of seconds since 1900-01-01 00:00:00 GMT.
"
  (let ((ymd (split-string date-string "-")))
    (encode-universal-time 0 0 0
                           (parse-integer (third  ymd))
                           (parse-integer (second ymd))
                           (parse-integer (first  ymd))
                           0))) ;;DATE-TO-UNIVERSAL-TIME


(defun date-format (utime &key (language :en))
  (multiple-value-bind (se mi ho day month year dw ds zo)
      (decode-universal-time utime 0)
    (declare (ignore se mi ho dw ds zo))
    (case language
      ((:fr)
       (format nil "~D~A ~A ~D"
               day
               (if (= 1 day) "er" "")
               (aref #("Janvier" "Février" "Mars" "Avril"
                       "Mai" "Juin" "Juillet" "Août"
                       "Septembre" "Octobre" "Novembre" "Décembre") (1- month))
               year))
      ((:es)
       (format nil "~D de ~A de ~D"
               day
               (aref #("Enero" "Febrero" "Marzo" "Abril"
                       "Mayo" "Junio" "Julio" "Augosto"
                       "Septiembre" "Octobre" "Noviembre" "Diciembre") (1- month))
               year))
      (otherwise
       (format nil "~A ~D~A, ~D"
               (aref #("January" "February" "March" "April"
                       "May" "June" "July" "August"
                       "September" "October" "November" "December") (1- month))
               day
               (case (mod day 10)
                 ((1) "st")
                 ((2) "nd")
                 ((3) "rd")
                 (otherwise "th"))
               year)))))


;;;---------------------------------------------------------------------
;;; Pjb-Object
;;;---------------------------------------------------------------------


(defclass pjb-object ()
  ((object-id
    :initform nil
    :initarg  :object-id
    :accessor object-id
    :type     (or null string)
    :documentation "The user-level ID of this object."))
  (:documentation "This is a root class for my classes."))


;;;---------------------------------------------------------------------
;;; BANK-REFERENCE
;;;---------------------------------------------------------------------


(defclass bank-reference (pjb-object)
  ((bank-name
    :initform nil
    :initarg  :bank-name
    :accessor bank-name
    :type     (or null string)
    :documentation "The name of the bank.")
   (bank-address
    :initform nil
    :initarg  :bank-address
    :accessor bank-address
    :type     (or null string)
    :documentation "The address of the bank.")
   (branch-name
    :initform nil
    :initarg  :branch-name
    :accessor branch-name
    :type     (or null string)
    :documentation "The name of the branch.")
   (swift-code
    :initform nil
    :initarg  :swift-code
    :accessor swift-code
    :type     (or null string)
    :documentation "The swift-code of the bank.")
   (account-number
    :initform nil
    :initarg  :account-number
    :accessor account-number
    :type     (or null string)
    :documentation "The account number. It should be an IBAN in Europe.")
   (beneficiary-name
    :initform nil
    :initarg  :beneficiary-name
    :accessor beneficiary-name
    :type     (or null string)
    :documentation "The beneficiary's name."))
  (:documentation "A bank account reference.")) ;;BANK-REFERENCE


;;;---------------------------------------------------------------------
;;; FISCAL-PERSON
;;;---------------------------------------------------------------------


(defclass fiscal-person (pjb-object)
  ((fiscal-id
    :initform nil
    :initarg  :fiscal-id
    :accessor fiscal-id
    :type     (or null string)
    :documentation "The fiscal ID of the person, ie. the European fiscal ID.")
   (name
    :initform nil
    :initarg  :name
    :accessor name
    :type     (or null string)
    :documentation "The name of the person.")
   (address
     :initform nil
     :initarg  :address
     :accessor address
     :type     (or null string)
     :documentation "The address of the person.")
   (phone
    :initform nil
    :initarg  :phone
    :accessor phone
    :type     (or null string)
    :documentation "The phone number of the person.")
   (fax
    :initform nil
    :initarg  :fax
    :accessor fax
    :type     (or null string)
    :documentation "The fax number of the person.")
   (web
    :initform nil
    :initarg  :web
    :accessor web
    :type     (or null string)
    :documentation "The URL of the web site of this person.")
   (email
    :initform nil
    :initarg  :email
    :accessor email
    :type     (or null string)
    :documentation "The fax number of the person.")
   (bank-reference
    :initform nil
    :initarg  :bank-reference
    :accessor bank-reference
    :type     (or null bank-reference)
    :documentation "The bank reference of the person.")
   (language
    :initform :es
    :initarg  :language
    :accessor language
    :type     symbol ;; :es :en :fr :de
    :documentation "The language (two-letter code) used by this person.")
   (fisc
    :initform nil
    :initarg :fisc
    :accessor fisc
    :type     boolean
    :documentation "Whether this person is the fiscal administration."))
  (:documentation
   "A person (physical or moral) identified by a fiscal identification number."
   )) ;;FISCAL-PERSON


(defmethod initialize-instance :after ((self fiscal-person) &rest arguments)
  (unless (getf arguments :object-id)
    (setf (object-id self) (or (fiscal-id self) (name self))))
  self)


;;;---------------------------------------------------------------------
;;; Invoice-Line
;;;---------------------------------------------------------------------


(defclass invoice-line (pjb-object)
  ((description
    :initform ""
    :initarg  :description
    :accessor description
    :type     string
    :documentation
    "The description of this line.")
   (currency
    :initform (find-currency :eur)
    :initarg  :currency
    :accessor currency
    :type     symbol
    :documentation
    "The currency of this line.")
   (amount-ht
    :initform (amount-zero *default-currency*)
    :initarg  :amount-ht
    :accessor amount-ht
    :type     amount
    :documentation
    "The amount excluding the taxes of this line.")
   (vat-rate
    :initform 0/100
    :initarg  :vat-rate
    :accessor vat-rate
    :type     rational
    :documentation
    "The rate of VAT for this line (0.00 <= vat-rate <= 0.50).")
   (amount-vat
    :initform (amount-zero *default-currency*)
    :initarg  :amount-vat
    :accessor amount-vat
    :type     amount
    :documentation
    "The amount of VAT for this line. ( = amount-ht * (1+vat-rate) )")
   (amount-ttc
    :initform (amount-zero *default-currency*)
    :initarg  :amount-ttc
    :accessor amount-ttc
    :type     amount
    :documentation
    "The amount including the taxes of this line."))
  (:documentation "An Invoice Line."))


;;;---------------------------------------------------------------------
;;; INVOICE
;;;---------------------------------------------------------------------


(defclass invoice (pjb-object)
  ((date
    :initform nil
    :initarg  :date
    :accessor date
    :type     (or null string)
    :documentation
    "'YYYY-MM-DD' The date of the invoice.")
   (issuer-fiscal-id
    :initform nil
    :initarg  :issuer-fiscal-id
    :accessor issuer-fiscal-id
    :type     (or null string)
    :documentation
    "The fiscal ID of the issuer of this invoice.")
   (invoice-number
    :initform nil
    :initarg  :invoice-number
    :accessor invoice-number
    :type     (or null string)
    :documentation
    "The invoice number.")
   (payer-fiscal-id
    :initform nil
    :initarg  :payer-fiscal-id
    :accessor payer-fiscal-id
    :type     (or null string)
    :documentation
    "The fiscal ID of the payer of this invoice.")
   (title
     :initform ""
     :initarg  :title
     :accessor title
     :type     (or null string)
     :documentation
     "The title of this invoice.")
   (currency
    :initform (find-currency :eur)
    :initarg  :currency
    :accessor currency
    :type     symbol
    :documentation
    "The currency of this invoice.")
   (lines
    :initform nil
    :accessor lines
    :type     list
    :documentation
    "(list of Invoice-Line) The line items of this invoice.")
   (total-ht
    :initform (amount-zero *default-currency*)
    :accessor total-ht
    :type     amount
    :documentation
    "The total excluding taxes of this invoice.")
   (total-vat
    :initform (amount-zero *default-currency*)
    :accessor total-vat
    :type     amount
    :documentation
    "The total of VAT.")
   (total-ttc
    :initform (amount-zero *default-currency*)
    :accessor total-ttc
    :type     amount
    :documentation
    "The total including taxes of this invoice.")
   )
  (:documentation
   "An invoice, either outgoing or incoming.
The amounts of the invoice may be negative when it's a refund."))


(defmethod initialize-instance :after ((self invoice) &rest arguments)
  (unless (getf arguments :object-id)
    (setf (object-id self) (concatenate 'string
                             (issuer-fiscal-id self) ":" (invoice-number self))))
  self)

;;;---------------------------------------------------------------------
;;; INVOICE-SET
;;;---------------------------------------------------------------------


(defclass invoice-set (pjb-object)
  ((fiscal-id
    :initform nil
    :initarg  :fiscal-id
    :accessor fiscal-id
    :type     (or null string)
    :documentation
    "The fiscal id of the owner of this invoice set.")
   (fisc-fiscal-ids
    :initform nil
    :initarg  :fisc-fiscal-ids
    :accessor fisc-fiscal-ids
    :type     list
    :documentation
    "(list of string) List of fiscal-id of fisc entity. An invoice issued by
     on of these entities is actually a tax.")
   (persons
    :initform nil
    :initarg  :persons
    :accessor persons
    :type     list
    :documentation
    "The list of known Fiscal-Person.")
   (invoices
    :initform nil
    :initarg  :invoices
    :accessor invoices
    :type     list
    :documentation
    "The list of known Invoices.")
   )
  (:documentation
   "This class gather all the data sets about invoices and fiscal persons.")
  ) ;;INVOICE-SET


;;;---------------------------------------------------------------------
;;; INVOICE-LINE
;;;---------------------------------------------------------------------


;; check = ( a-ttc | a-ht ) & ( a-vat | vat-rate )
;; error = ~check | (  ~a-ttc & ~a-ht ) | ( ~a-vat & ~vat-rate ) )
;;
;; (insert
;;  (carnot '( a-ttc a-ht a-vat vat-rate check)
;;          '((check . (lambda (a-ttc a-ht a-vat vat-rate check)
;;                       (and (or a-ttc a-ht) (or a-vat vat-rate))
;;                       ))
;;            (error . (lambda (a-ttc a-ht a-vat vat-rate check)
;;                       (or (not check)
;;                           (and (not a-ttc) (not  a-ht))
;;                           (and (not a-vat) (not  vat-rate))) )))))
;;
;; +-------+------+-------+----------+-------+-------+-------+
;; | a-ttc | a-ht | a-vat | vat-rate | check | check | error |
;; +-------+------+-------+----------+-------+-------+-------+
;; |  OUI  |  OUI |  OUI  |    OUI   |  OUI  |   X   |   .   |
;; |  OUI  |  OUI |  OUI  |    OUI   |  NON  |   X   |   X   |
;; |  OUI  |  OUI |  OUI  |    NON   |  OUI  |   X   |   .   |
;; |  OUI  |  OUI |  OUI  |    NON   |  NON  |   X   |   X   |
;; |  OUI  |  OUI |  NON  |    OUI   |  OUI  |   X   |   .   |
;; |  OUI  |  OUI |  NON  |    OUI   |  NON  |   X   |   X   |
;; |  OUI  |  OUI |  NON  |    NON   |  OUI  |   .   |   X   |
;; |  OUI  |  OUI |  NON  |    NON   |  NON  |   .   |   X   |
;; |  OUI  |  NON |  OUI  |    OUI   |  OUI  |   X   |   .   |
;; |  OUI  |  NON |  OUI  |    OUI   |  NON  |   X   |   X   |
;; |  OUI  |  NON |  OUI  |    NON   |  OUI  |   X   |   .   |
;; |  OUI  |  NON |  OUI  |    NON   |  NON  |   X   |   X   |
;; |  OUI  |  NON |  NON  |    OUI   |  OUI  |   X   |   .   |
;; |  OUI  |  NON |  NON  |    OUI   |  NON  |   X   |   X   |
;; |  OUI  |  NON |  NON  |    NON   |  OUI  |   .   |   X   |
;; |  OUI  |  NON |  NON  |    NON   |  NON  |   .   |   X   |
;; |  NON  |  OUI |  OUI  |    OUI   |  OUI  |   X   |   .   |
;; |  NON  |  OUI |  OUI  |    OUI   |  NON  |   X   |   X   |
;; |  NON  |  OUI |  OUI  |    NON   |  OUI  |   X   |   .   |
;; |  NON  |  OUI |  OUI  |    NON   |  NON  |   X   |   X   |
;; |  NON  |  OUI |  NON  |    OUI   |  OUI  |   X   |   .   |
;; |  NON  |  OUI |  NON  |    OUI   |  NON  |   X   |   X   |
;; |  NON  |  OUI |  NON  |    NON   |  OUI  |   .   |   X   |
;; |  NON  |  OUI |  NON  |    NON   |  NON  |   .   |   X   |
;; |  NON  |  NON |  OUI  |    OUI   |  OUI  |   .   |   X   |
;; |  NON  |  NON |  OUI  |    OUI   |  NON  |   .   |   X   |
;; |  NON  |  NON |  OUI  |    NON   |  OUI  |   .   |   X   |
;; |  NON  |  NON |  OUI  |    NON   |  NON  |   .   |   X   |
;; |  NON  |  NON |  NON  |    OUI   |  OUI  |   .   |   X   |
;; |  NON  |  NON |  NON  |    OUI   |  NON  |   .   |   X   |
;; |  NON  |  NON |  NON  |    NON   |  OUI  |   .   |   X   |
;; |  NON  |  NON |  NON  |    NON   |  NON  |   .   |   X   |
;; +-------+------+-------+----------+-------+-------+-------+


(defmethod initialize-instance ((self invoice-line)
                                &key object-id description currency
                                amount-ht vat-rate amount-vat amount-ttc
                                &allow-other-keys)
  "
DO:      Checks that the values for the fields are within limits.
"
  (when (or (and (not amount-ttc) (not  amount-ht))
            (and (not amount-vat) (not  vat-rate)))
    (error "Not enought amount data defined for this line ~S." self))
  (unless amount-ttc
    (setf amount-ttc
          (cond
            (amount-vat (+ amount-ht amount-vat))
            (vat-rate   (* amount-ht (+ 1 vat-rate)))
            ;; last case should not occur.
            (t          (if (zerop vat-rate)
                            amount-ht
                            (/ (* amount-vat (+ 1 vat-rate)) vat-rate))))))
  (unless amount-ht
    (setf amount-ht
          (cond
            (amount-vat (- amount-ttc amount-vat))
            (vat-rate   (/ amount-ttc (+ 1 vat-rate)))
            ;; last case should not occur.
            (t          (if (zerop vat-rate)
                            amount-ttc
                            (/ amount-vat vat-rate))))))
  (unless amount-vat
    (setf amount-vat    (- amount-ttc amount-ht)))
  (unless vat-rate
    (setf vat-rate (if (zerop (amount-magnitude amount-ht))
                       0
                       (/ (round (* (/ (amount-magnitude amount-vat)
                                       (amount-magnitude amount-ht)) 100)) 100))))
  (when (null currency)
    (setf currency (currency amount-ttc)))
  ;; (check-vat amount-ttc amount-ht amount-vat vat-rate)
  (call-next-method self
                    :object-id     object-id
                    :description   description
                    :currency      currency
                    :amount-ht     amount-ht
                    :vat-rate      vat-rate
                    :amount-vat    amount-vat
                    :amount-ttc    amount-ttc))


;;;---------------------------------------------------------------------
;;; INVOICE
;;;---------------------------------------------------------------------


(defmethod compute-totals ((self invoice))
  "
DO:      Compute the totals.
"
  (let* ((th (amount-zero (currency self))) (tv th) (tt th))
    (dolist (line  (lines self))
      (setf th (+ th (amount-ht  line))
            tv (+ tv (amount-vat line))
            tt (+ tt (amount-ttc line))))
    (setf (total-ht  self) th
          (total-vat self) tv
          (total-ttc self) tt))) ;;COMPUTE-TOTALS


(defmethod vat-rate ((self invoice))
  "
RETURN: A computed VAT rate for this invoice.
"
  (if (zerop (amount-magnitude (total-ht self)))
      0
      (/ (round (* 100 (/ (amount-magnitude (total-vat self))
                          (amount-magnitude (total-ht self))))) 100)))


(defmethod add-line ((self invoice) (line invoice-line))
  "
PRE:     (eq (find-currency (currency self))(find-currency (currency line)))
DO:      Add the line.
"
  (assert (eq (find-currency (currency self)) (find-currency (currency line))))
  (setf (lines self) (append (lines self) (list line)))
  (compute-totals self)) ;;ADD-LINE


(defmethod is-refund ((self invoice))
  "
RETURN: Whether this invoice is a refund invoice.
"
  (negativep (total-ttc self))) ;;IS-REFUND


(deftranslation *invoice-strings* "Phone:"
  :en :idem
  :fr "Téléphone :"
  :es "Teléfono :")

(deftranslation *invoice-strings* "Fax:"
  :en :idem
  :fr "Télécopie :"
  :es "Telécopia :")

(deftranslation *invoice-strings* "Email:"
  :en :idem
  :fr "Couriel :"
  :es "Email :")

(deftranslation *invoice-strings* "VAT Immatriculation:"
  :en :idem
  :fr "TVA Intracommunautaire :"
  :es "Imatriculación IVA :")

(deftranslation *invoice-strings* "INVOICE"
  :en :idem
  :fr "FACTURE"
  :es "FACTURA")

(deftranslation *invoice-strings* "Date:"
  :en :idem
  :fr "Date :"
  :es "Fecha :")

(deftranslation *invoice-strings* "Invoice no.:"
  :en :idem
  :fr "Facture nº :"
  :es "Nº de factura :")

(deftranslation *invoice-strings* "Billing address:"
  :en :idem
  :fr "Adresse de facturation :"
  :es "Dirección de factura :")

(deftranslation *invoice-strings* "Description"
  :en :idem
  :fr "Description"
  :es "Descripción")

(deftranslation *invoice-strings* "Price"
  :en :idem
  :fr "Prix"
  :es "Precio")

(deftranslation *invoice-strings* "Total"
  :en :idem
  :fr "Total HT"
  :es "Base imponible")

(deftranslation *invoice-strings* "VAT  ~5,1F %"
  :en :idem
  :fr "TVA  ~5,1F %"
  :es "IVA  ~5,1F %")

(deftranslation *invoice-strings* "IRPF ~5,1F %"
  :en ""
  :fr ""
  :es :idem)

(deftranslation *invoice-strings* "Total VAT Incl."
  :en :idem
  :fr "Total TTC"
  :es "Total factura")

(deftranslation *invoice-strings* "PAYMENT-METHOD"
  :en "Method of Payment:  Bank Transfer
       Please make your payment using the details below,
       before ~A."
  :fr "Mode de règlement :  À régler par virement bancaire au compte suivant,
       avant le ~A."
  :es "Forma de pago :  Transferencia bancaria a la cuenta siguiente,
       antes del ~A.") ;;*INVOICE-STRINGS*

(deftranslation *invoice-strings* "Payment Bank"
  :en :idem
  :fr "Banque destinataire"
  :es "Banco")

(deftranslation *invoice-strings* "Branch Name"
  :en :idem
  :fr "Agence"
  :es "Oficina")

(deftranslation *invoice-strings* "Account Number (IBAN)"
  :en :idem
  :fr "Numéro de compte (IBAN)"
  :es "Número de cuenta (IBAN)")

(deftranslation *invoice-strings* "Beneficiary"
  :en :idem
  :fr "Bénéficiaire"
  :es "Beneficiario")

(deftranslation *invoice-strings* "SWIFT Code"
  :en :idem
  :fr "Code SWIFT"
  :es "Código SWIFT")

(deftranslation *invoice-strings* "Currency change"
  :en :idem
  :fr "Change devises"
  :es "Cambio devisas")


(defmacro longest-localized-length (table language fields)
  `(loop for fname in ,fields
      maximize (length (localize ,table ,language fname)) into increment
      finally (return increment))) ;;LONGEST-LOCALIZED-LENGTH


(defparameter +line-chars+
  (coerce #(#\LINEFEED #\RETURN #\NEWLINE #\PAGE) 'string)
  "A string containing the new-line characters.")


(defun split-lines (text &key delete-empty-lines)
  "
DELETE-EMPTY-LINES:  When true, lines that are stripped empty are removed.
RETURN:              A list of stripped and splitted lines from the TEXT.
"
  (let ((lines (split-string text +line-chars+)))
    (map-into lines (lambda (line) (string-trim " " line)) lines)
    (if delete-empty-lines
        (delete "" lines :test (function string=))
        lines))) ;;SPLIT-LINES


(defun align-following-lines (text left-margin)
  "
DO:     Format the TEXT inserting LEFT-MARGIN spaces before each line
        but the first.
"
  (format nil (format nil "~~{~~A~~^~~%~VA~~}" left-margin "")
          (split-lines text))) ;;ALIGN-FOLLOWING-LINES


(defun print-person-address (title left-margin person
                                   &key (language :fr) (stream t))
  "
DO:          Insert into the current buffer at the current point the address
             and phone, fax and email of the given person,
             prefixed by the title and with a left-margin of `left-margin'
             characters.  If the length of the title is greater than the
             `left-margin' then the length of the title is used instead.
LANGUAGE:     The default language is French (:FR),
              :EN and :ES are also available for English and Spanish.
"
  (unless title (setf title ""))
  (when (< left-margin (length title))
    (setf left-margin (length title)))
  ;; title / name
  (format stream "~A~A~%" (string-pad title left-margin) (name person))
  ;; address
  (format stream (format nil "~~{~VA~~A~~%~~}" left-margin "")
          (split-lines (address person) :delete-empty-lines t))
  ;; other fields
  (let* ((fields '("Phone:" "Fax:" "Email:" "VAT Immatriculation:"))
         (slots  '(phone fax email fiscal-id))
         (increment
          (loop for fname in fields
             for slot  in slots
             when (slot-value person slot)
             maximize (length (localize *invoice-strings*
                                        language fname)) into increment
             finally (return increment))))
    (loop for fname in fields
       for slot  in slots
       when (slot-value person slot)
       do (format stream "~VA~A ~A~%"
                  left-margin ""
                  (string-pad (localize *invoice-strings* language fname)
                              increment)
                  (slot-value person slot))))) ;;PRINT-PERSON-ADDRESS


(defun show-tva (montant-ht &key (stream t)
                 (vat-rate 16/100 vat-rate-p) (irpf nil irpf-p)
                 (language :es) (alt-language :es))
  "Affiche le montant HT donné, la TVA, le montant TTC.
En option le taux de TVA.
La facture est dans la devise du montant.
Une ou deux langues peuvent aussi être indiquées (:es, :fr, :en).
Une option :irpf ou :no-irpf peut être indiquée pour forcer la déduction IRPF,
sinon elle est appliquée par défaut uniquement dans le cas où le taux de TVA
est 16% et la langue est 'es (sans langue secondaire) et la currency :EUR.

 (show-tva #978m750.00 :vat-rate 16/100 :language :en :alt-language :es)

donne :

 ----------------------------------------------------   -----------------
                (Base imponible  ) Total            :          750.00 EUR
                (IVA   16.0 %    ) VAT   16.0 %     :   +      120.00 EUR
                (Total factura   ) Total VAT Incl.  :   =      870.00 EUR
 ----------------------------------------------------   -----------------
"
  (let* ((line-form " ~52A   ~17A~%")
         (desc-line (make-string 52 :initial-element (character "-")))
         (pric-line (make-string 17 :initial-element (character "-")))
         (base-lab-gau "")
         (tvat-lab-gau "")
         (irpf-lab-gau "")
         (tota-lab-gau "")
         (base-lab-dro)
         (tvat-lab-dro)
         (irpf-lab-dro)
         (tota-lab-dro)
         (taux-tva         nil)
         (taux-tva-present nil)
         ;; empeze la actividad el 2000/07 entonces desde el 2003/07 es -18%.
         (taux-irpf  (if (date-after (calendar-current-date)
                                     (make-date :year 2003 :month 6 :day 30))
                         -18/100 -9/100))
         (show-irpf        nil)
         (force-show-irpf  nil)
         (montant-tva)
         (montant-irpf)
         (montant-ttc)
         (lang-pri     nil)
         (lang-sec     nil))
    (when irpf-p
      (if irpf
          (setf show-irpf t   force-show-irpf t)
          (setf show-irpf nil force-show-irpf t)))
    (setf lang-pri language
          lang-sec alt-language)
    (setf taux-tva vat-rate)
    (if vat-rate-p
        (setf taux-tva-present t)
        (setf taux-tva 0/100
              taux-tva-present nil))
    (when (equal lang-pri lang-sec)
      (setf lang-sec nil))
    (if (and (null lang-sec)
             (not taux-tva-present)
             (string-equal  lang-pri :es))
        (setf taux-tva 16/100))
    (unless force-show-irpf
      (setf show-irpf (and (eq (find-currency :eur)
                               (currency montant-ht))
                           (string-equal lang-pri :es)
                           (null lang-sec)
                           (= taux-tva 16/100)
                           ;; (equal (fiscal-id *invoice-set*)
                           ;;        (issuer-fiscal-id self))
                           )))
    (setf montant-tva  (* montant-ht taux-tva))
    (setf montant-irpf (if show-irpf (* montant-ht taux-irpf)
                           (amount-zero (currency montant-ht))))
    (setf montant-ttc  (+ montant-ht montant-tva montant-irpf))
    (setf base-lab-dro
          (format nil "~16@A :"
                  (localize *invoice-strings* lang-pri "Total")))
    (setf tvat-lab-dro
          (format nil "~16@A :"
                  (format nil (localize *invoice-strings* lang-pri "VAT  ~5,1F %")
                          (* 100 taux-tva))))
    (setf irpf-lab-dro
          (format nil "~16@A :"
                  (format nil (localize *invoice-strings* lang-pri "IRPF ~5,1F %")
                          (* 100 taux-irpf))))
    (setf tota-lab-dro
          (format nil "~16@A :"
                  (localize *invoice-strings* lang-pri "Total VAT Incl.")))
    (when lang-sec
      (setf base-lab-gau
            (format nil "(~16@A) "
                    (localize *invoice-strings* lang-sec "Total")))
      (setf tvat-lab-gau
            (format nil "(~16@A) "
                    (format nil (localize *invoice-strings* lang-sec "VAT  ~5,1F %")
                            (* 100 taux-tva))))
      (setf irpf-lab-gau
            (format nil "(~16@A) "
                    (format nil (localize *invoice-strings* lang-sec "IRPF ~5,1F %")
                            (* 100 taux-irpf))))
      (setf tota-lab-gau
            (format nil "(~16@A) "
                    (localize *invoice-strings* lang-sec "Total VAT Incl."))))
    (format stream "~%")
    (format stream line-form desc-line pric-line)
    (format stream line-form
            (concatenate 'string base-lab-gau base-lab-dro)
            (format nil " ~16@A" montant-ht))
    (format stream line-form
            (concatenate 'string tvat-lab-gau tvat-lab-dro)
            (format nil "+~16@A" montant-tva))
    (when show-irpf
      (format stream line-form
              (concatenate 'string irpf-lab-gau irpf-lab-dro)
              (format nil "-~16@A" (- montant-irpf))))
    (format stream line-form
            (concatenate 'string tota-lab-gau tota-lab-dro)
            (format nil "=~16@A" montant-ttc))
    (format stream line-form desc-line pric-line))) ;;SHOW-TVA



(defun clean-title-for-file-name (title-string)
  "
RETURN: A string containing the first word of title-string as plain ASCII.
DO:     Remove accents from the returned word.
"
  ;;(STRING-REMOVE-ACCENTS
  (string-downcase
   (subseq title-string 0
           (position-if-not (function alphanumericp)
                            title-string)))) ;;CLEAN-TITLE-FOR-FILE-NAME


(defmethod generate ((self invoice) &key (stream t) (verbose nil)
                     (language :es language-p))
  "
DO:      Generate this invoice into a file in the directory
         *INVOICE-DIRECTORY-PATH*.
RETURN:  The path to the file generated.
"
  (let* ((payer  (get-person-with-fiscal-id *invoice-set*
                                            (payer-fiscal-id self)))
         (issuer (get-person-with-fiscal-id *invoice-set*
                                            (issuer-fiscal-id self))))
    (when verbose
      (format *trace-output* "Generating ~A ~A ~A ~A~%"
              (class-name (class-of self)) (date self)
              issuer (invoice-number self)))
    (unless language-p
      (setf language  (or (language payer) :es)))
    (print-person-address "" 1 issuer :language language :stream stream)
    (format stream " ~%")
    (let* ((title (localize *invoice-strings* language "INVOICE"))
           (width (+ 8 (length title)))
           (title-b (concatenate 'string "|"
                                 (string-pad title width
                                             :justification :center) "|"))
           (line-b  (concatenate
                        'string "+"
                        (make-string width
                                     :initial-element (character "-")) "+")))
      (format stream " ~A~%" (string-pad line-b  72 :justification :center))
      (format stream " ~A~%" (string-pad title-b 72 :justification :center))
      (format stream " ~A~%" (string-pad line-b  72 :justification :center)))
    (format stream " ~%")
    (let ((increment (longest-localized-length
                      *invoice-strings* language
                      '("Date:" "Invoice no.:" "Billing address:"))))
      (format stream " ~A ~A~%"
              (string-pad (localize *invoice-strings* language
                                    "Date:") increment)
              (date-format (date-to-universal-time (date self))
                           :language language))
      (format stream " ~%")
      (format stream " ~A ~A~%"
              (string-pad (localize *invoice-strings* language
                                    "Invoice no.:") increment)
              (invoice-number self))
      (format stream " ~%")
      (print-person-address
       (concatenate 'string " "
                    (localize *invoice-strings* language "Billing address:"))
       (+ 2 increment) payer :language language)
      (format stream " ~%"))
    (let ((line-form " ~52@A   ~17@A~%")
          (desc-line (make-string 52 :initial-element (character "-")))
          (pric-line (make-string 17 :initial-element (character "-"))))
      (format stream line-form desc-line pric-line)
      (format stream line-form
              (localize *invoice-strings* language "Description")
              (localize *invoice-strings* language "Price"))
      (format stream line-form desc-line pric-line)
      (dolist (invo-line  (lines self))
        (let* ((desc (split-lines (description invo-line)))
               (last-length (length (car (last desc)))))
          (format stream "~{~% ~A~}" desc)
          (if (<= last-length 55)
              (format stream "~VA ~16@A"
                      (- 55 last-length) "" (amount-ht invo-line))
              (format stream "~% ~52@A   ~16@A"  "" (amount-ht invo-line)))))
      (format stream " ~%")) ;;let
    (show-tva (total-ht self) :language language :alt-language :es)
    (format stream " ~%")
    (let ((bankref (bank-reference issuer)))
      (when bankref
        (format stream "~{ ~A~%~}"
                (split-lines
                 (format nil
                   (localize *invoice-strings* language "PAYMENT-METHOD")
                   (date-format (+ (* 30 +seconds-in-a-day+)
                                   (date-to-universal-time (date self)))
                                :language language))))
        (format stream " ~%")
        (let* ((fields '("Payment Bank" "" "Branch Name"
                         "Account Number (IBAN)" "Beneficiary"
                         "SWIFT Code"))
               (slots  '(bank-name bank-address branch-name
                         account-number beneficiary-name swift-code))
               (increment (longest-localized-length
                           *invoice-strings* language fields)))
          (loop for fname in fields
             for slot  in slots
             when (slot-value bankref slot)
             do (format stream
                  " ~A~A : ~A~%"
                  (string-pad "" 8)
                  (string-pad (localize *invoice-strings* language fname)
                              increment)
                  (align-following-lines (slot-value bankref slot)
                                         (+ increment 10)))) )))
    (format stream " ~%")
    (format stream " ~%"))) ;;GENERATE


(defmethod write-invoice-file ((self invoice) &key (language :en language-p))
  (let* ((payer  (get-person-with-fiscal-id *invoice-set*
                                            (payer-fiscal-id self)))
         (file-path
          (make-pathname
           :directory *invoice-directory-path*
           :name (format nil  "~A-~A-~A"
                         (delete (character "/") (invoice-number self))
                         (clean-title-for-file-name (object-id payer))
                         (clean-title-for-file-name (title self)))
           :type "txt")))
    (unless language-p
      (setf language  (or (language payer) :es)))
    (with-open-file (stream file-path :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (generate self :stream stream :language language))
    file-path)) ;;WRITE-INVOICE-FILE


;;;---------------------------------------------------------------------
;;; INVOICE-SET
;;;---------------------------------------------------------------------


(defmethod get-person-with-fiscal-id ((self invoice-set) fiscal-id)
  (car (member fiscal-id (persons self)
               :test (function string-equal) :key (function fiscal-id))))


(defmethod add-person ((self invoice-set) (person fiscal-person)
                       &optional fisc)
  (let ((old (get-person-with-fiscal-id self (fiscal-id person))))
    (if old
        (substitute person old (persons self) :count 1)
        (push person (persons self))))
  (when (and fisc  (not (member (fiscal-id person) (fisc-fiscal-ids self)
                                :test (function string-equal))))
    (push (fiscal-id person) (fisc-fiscal-ids self)))) ;;ADD-PERSON


(defmethod get-invoice-with-issuer-and-number ((self invoice-set)
                                               issuer-fiscal-id invoice-number)
  (find-if (lambda (i)  (and (equal issuer-fiscal-id (issuer-fiscal-id  i))
                             (equal invoice-number   (invoice-number    i))))
           (invoices self))) ;;GET-INVOICE-WITH-ISSUER-AND-NUMBER


(defmethod add-invoice ((self invoice-set) (invoice invoice))
  (let ((old (get-invoice-with-issuer-and-number
              self
              (issuer-fiscal-id invoice)
              (invoice-number   invoice))))
    (if old
        (substitute invoice old (invoices self) :count 1)
        (push invoice (invoices self))))) ;;ADD-INVOICE


;;;---------------------------------------------------------------------
;;; MOVEMENT
;;;---------------------------------------------------------------------


(defclass movement (pjb-object)
  ((date
    :initform nil
    :initarg  :date
    :accessor date
    :type     (or null string)
    :documentation
    "'YYYY-MM-DD' Date of the movement.")
   (amount-ttc
    :initform (amount-zero *default-currency*)
    :initarg  :amount-ttc
    :accessor amount-ttc
    :type     amount
    :documentation
    "(number) The amount paid (including taxes).")
   (amount-vat
    :initform 0/100
    :initarg  :amount-vat
    :accessor amount-vat
    :type     rational
    :documentation
    "(number) The VAT of the movement.")
   (description
    :initform ""
    :initarg  :description
    :accessor description
    :type     string
    :documentation
    "(string) A description of the movement.")
   (kind
    :initform nil
    :initarg  :kind
    :accessor kind
    :type     (or null symbol)
    :documentation
    "(symbol) A kind of movement, for tax reporting purpose.
PRESTACION-NACIONAL, PRESTACION-INTRACOMUNITARIA, IMPUESTO, INVERSION,
GASTO-CORRIENTE, ADQUISICION-INTRACOMUNITARIA")
   (invoice-fiscal-id
    :initform nil
    :initarg  :invoice-fiscal-id
    :accessor invoice-fiscal-id
    :type     (or null string)
    :documentation
    "The fiscal id of the common issuer of the following invoices
related to this movement.")
   (invoice-numbers
    :initform nil
    :initarg  :invoice-numbers
    :accessor invoice-numbers
    :type     list
    :documentation
    "(list of string) The list of invoice numbers related to this entry.
Note that one journal entry may relate to several invoices (grouped payment)
and one invoice may relate to several movements (part payments, or
corrections.")
   )
  (:documentation
   "An entry in the journal.
A movement with a positive amount is a credit,
while a movement with a negative amount is a debit.")) ;;MOVEMENT


(defmethod initialize-instance ((self movement)
                                &key amount-ttc amount-ht invoice-fiscal-id
                                &allow-other-keys)
  "
DOES:    Checks that the values for the fields are within limits.
"
  (declare (ignorable self))
  (when *max-movement-amount*
    (when amount-ttc
      (when (< *max-movement-amount* (abs amount-ttc))
        (error "amount-ttc too big ~S." amount-ttc)))
    (when amount-ht
      (if (< *max-movement-amount* (abs  amount-ht))
          (error "amount-ht too big ~S." amount-ht))))
  (when invoice-fiscal-id
    (unless (get-person-with-fiscal-id *invoice-set* invoice-fiscal-id)
      (warn "Unknown person (fiscal-id=~S)." invoice-fiscal-id)))
  (call-next-method)) ;;INITIALIZE-INSTANCE


(defmethod amount-ht ((self movement)) (- (amount-ttc self) (amount-vat self)))


(defparameter *movement-kinds*
  '(:prestacion-nacional
    :prestacion-intracomunitaria
    :impuesto
    :inversion
    :gasto-corriente
    :adquisicion-intracomunitaria)) ;;*MOVEMENT-KINDS*


(defun make-movement-from-invoice (invoice)
  "
RETURN: A new instance of MOVEMENT filled with data from invoice.
"
  (let (kind amount-sign fiscal-id)
    (cond
      ((member (issuer-fiscal-id invoice) (fisc-fiscal-ids *invoice-set*)
               :test (function string-equal))
       (setf kind :impuesto
             amount-sign -1
             fiscal-id (issuer-fiscal-id invoice)))
      ((equalp (issuer-fiscal-id invoice) (fiscal-id *invoice-set*))
       (setf kind (if (zerop (total-vat invoice))
                      :prestacion-intracomunitaria
                      :prestacion-nacional)
             amount-sign 1
             fiscal-id (payer-fiscal-id invoice)))
      (t
       (setf kind :gasto-corriente
             amount-sign -1
             fiscal-id (issuer-fiscal-id invoice))))
    (make-instance 'movement
      :date                (date      invoice)
      :amount-ttc       (* (total-ttc invoice) amount-sign)
      :amount-vat       (* (total-vat invoice) amount-sign)
      :description         (title     invoice)
      :kind                kind
      :invoice-fiscal-id   fiscal-id
      :invoice-numbers
      (list (invoice-number invoice))))) ;;MAKE-MOVEMENT-FROM-INVOICE


(define-condition movement-error (simple-error)
  ((date         :accessor movement-error-date
                 :initarg :date)
   (amount-ttc   :accessor movement-error-amount-ttc
                 :initarg :amount-ttc)
   (vat-rate     :accessor movement-error-vat-rate
                 :initarg :vat-rate)
   (nif          :accessor movement-error-nif
                 :initarg :nif)
   (fac-l        :accessor movement-error-fac-l
                 :initarg :fac-l)
   (description  :accessor movement-error-description
                 :initarg :description)
   (kind         :accessor movement-error-kind
                 :initarg :kind)))

(defun make-movement (date amount-ttc vat-rate nif fac-l description kind)
  "
RETURN: A new instance of MOVEMENT filled with the given data
."
  (macrolet ((err (ctrl &rest args)
               `(error 'movement-error
                       :format-control ,ctrl
                       :format-arguments (list ,@args)
                       :date date :amount-ttc amount-ttc :vat-rate vat-rate
                       :nif nif :fac-l fac-l
                       :description description :kind kind)))
    (unless (member kind  *movement-kinds*)
      (err "Invalid kind ~A." kind))
    (when (< vat-rate -2)
      (err "VAT-RATE must always be >= 0."))
    (unless (eq kind :prestacion-intracomunitaria)
      (when (eq kind :prestacion-nacional)
        (unless (negativep amount-ttc)
          (err "AMOUNT-TTC must be > 0 for an entry of kind ~A." kind))))
    (cond
      ((eq kind :prestacion-nacional)
       (when (<= vat-rate 0)
         (err "VAT-RATE must be > 0.00 for an entry of kind ~A." kind)))
      ((member kind '(:prestacion-intracomunitaria :impuesto))
       (if (< 0 vat-rate)
           (err "VAT-RATE must be = 0 for an entry of kind ~A." kind))))
    (make-instance 'movement
      :date              date
      :amount-ttc        amount-ttc
      :amount-vat         (/ (* amount-ttc vat-rate) (+ 1 vat-rate))
      :description       description
      :kind              kind
      :invoice-fiscal-id nif
      :invoice-numbers   fac-l)))


(defmethod is-credit   ((self movement))
  "
RETURN: Whether the SELF is a credit movement.
"
  (positivep (amount-ht self))) ;;IS-CREDIT


(defmethod vat-rate    ((self movement))
  "
RETURN: A computed VAT rate for this movement.
"
  (if (zerop (amount-magnitude (amount-ht  self)))
      0
      (/ (round (* 100 (/ (amount-magnitude (amount-vat self))
                          (amount-magnitude (amount-ht  self))))) 100)))


(defmethod credit-ht   ((self movement))
  (if (is-credit self)
      (amount-ht self)
      (amount-zero (currency (amount-ht self)))))


(defmethod credit-vat  ((self movement))
  (if (is-credit self)
      (amount-vat self)
      (amount-zero (currency (amount-vat self)))))


(defmethod debit-ht    ((self movement))
  (if (is-credit self)
      (amount-zero (currency (amount-ht self)))
      (- (amount-ht self))))


(defmethod debit-vat   ((self movement))
  (if (is-credit self)
      (amount-zero (currency (amount-vat self)))
      (- (amount-vat self))))


(defmethod debit-vat-inversion  ((self movement))
  (if (eq :inversion (kind self))
      (debit-vat self)
      (amount-zero (currency (debit-vat self)))))


(defmethod debit-vat-corriente  ((self movement))
  (if (eq :gasto-corriente (kind self))
      (debit-vat self)
      (amount-zero (currency (debit-vat self)))))


(defmethod invoices ((self movement))
  "
RETURN: A list of INVOICE instances related to this entry.
"
  (let ((fiscal-id  (if (and (is-credit self)
                             (not (equal (kind self) :gasto-corriente)))
                        (fiscal-id *invoice-set*)
                        (invoice-fiscal-id self))))
    (remove nil (mapcar
                 (lambda (number)
                   (get-invoice-with-issuer-and-number
                    *invoice-set* fiscal-id number))
                 (invoice-numbers self))))) ;;INVOICES


(defun trim-justify-and-split-text (text width)
  "
DOES:    Trim spaces on each line. justify each paragraph.
RETURN:  The justified text.
"
  (let ((lines (split-lines text))
        (paragraphs        '())
        (current-paragraph '()))
    (dolist (line lines                 ; group the paragraphs.
             (when current-paragraph
               (push (apply (function concatenate) 'string
                            (nreverse current-paragraph)) paragraphs)))
      (if (string= line "")
          (when current-paragraph
            (push (apply (function concatenate) 'string
                         (nreverse current-paragraph)) paragraphs))
          (progn (push " " current-paragraph)
                 (push line current-paragraph))))
    (or (mapcan (lambda (para) (split-lines (string-justify-left para width 0)))
                (nreverse paragraphs))
        (list " ")))) ;;TRIM-JUSTIFY-AND-SPLIT-TEXT


(defmethod generate ((self movement) &key (stream t) (verbose nil)
                     (language :en))
  "
DOES:    format and insert this entry.
"
  (let ((id  (invoice-fiscal-id self))
        person
        (name "")  name-l
        (movement-sign (if (is-credit self) 1 -1))
        (invoices-sign 1))
    (when verbose
      (format *trace-output* "Generating ~A ~A ~A~%"
              (class-name (class-of self)) (date self) (amount-ttc self)))
    ;; first line:
    (if id
        (progn
          (setf person (get-person-with-fiscal-id *invoice-set* id))
          (when person (setf name (name person))))
        (setf id ""))
    (setf name-l (trim-justify-and-split-text name 38))
    ;; === SEN FECHA IDENTIFICATION NOMBRE =============================
    (format stream "~3A ~10A ~23@A ~A~%"
            (cond  ((is-credit self)           "ING")
                   ((eq :impuesto (kind self)) "IMP")
                   (t                          "GAS"))
            (date self) id (car name-l))
    ;; === OPTIONAL NEXT LINES =========================================
    ;;                          NOMBRE (continuación)
    (dolist (name (cdr name-l)) (format stream "~38A ~A~%" "" name))
    ;; === INVOICE LINES ===============================================
    ;;     IMPORTE  IVA%   +IVA   TOTAL  NUMERO FACTURA o DESCRIPCION
    ;;                                   INVOICE TITLE
    (let* ((zero (amount-zero (currency (amount-ht self))))
           (t-ht  zero)
           (t-vat zero)
           (t-ttc zero))
      (let ((t-ttc zero))               ; Let's find the invoices-sign
        (dolist (invoice (invoices self))
          (setf t-ttc (+ t-ttc (total-ttc invoice))))
        (setf invoices-sign (* movement-sign (if (negativep t-ttc) -1 1))))
      (dolist (invoice (invoices self)) ; Let's print the invoices
        (let* ((title-l (trim-justify-and-split-text (title invoice) 38))
               (i-ht    (total-ht  invoice))
               (i-vat   (total-vat invoice))
               (i-ttc   (total-ttc invoice)))
          (setf t-ht  (+ t-ht   i-ht)
                t-vat (+ t-vat  i-vat)
                t-ttc (+ t-ttc  i-ttc))
          (format stream "    ~2,,9$ ~4,1F% ~2,,8$ ~2,,9$ ~A~%"
                  (amount-magnitude (* i-ht invoices-sign))
                  (* 100 (vat-rate invoice))
                  (amount-magnitude (* i-vat invoices-sign))
                  (amount-magnitude (* i-ttc invoices-sign))
                  (invoice-number invoice))
          (dolist (title title-l)
            (format stream  "~38A ~A~%" ""  title))))
      ;; === DIFFERNCE LINE ============================================
      ;;    AMOUNT-HT AMOUNT-VAT AMOUNT-TTC Diferencia
      (unless (and (zerop t-ht) (zerop t-vat) (zerop t-ttc))
        ;; Invoices, let's see if there's a difference.
        (handler-case
            (unless (= (amount-ttc self) (* t-ttc invoices-sign))
              (let* ((diff-ht  (- (amount-ht  self) (* t-ht  invoices-sign)))
                     (diff-vat (- (amount-vat self) (* t-vat invoices-sign)))
                     (diff-ttc (- (amount-ttc self) (* t-ttc invoices-sign))))
                (format stream "    ~2,,9$ ~5A ~2,,8$ ~2,,9$ ~A~%"
                        (amount-magnitude diff-ht) ""
                        (amount-magnitude diff-vat)
                        (amount-magnitude diff-ttc) "Diferencia")))
          (multi-currency-error
              ()
            (let ((cambio (if (zerop t-ttc)
                              0
                              (/ (amount-ttc self) (* t-ttc invoices-sign)))))
              (format stream "    ~9A ~5A ~8A ~2,,9$ ~A ~A -> ~A~%"
                      "" "" "" cambio
                      (localize *invoice-strings* language "Currency change")
                      (currency-alphabetic-code (currency t-ttc))
                      (currency-alphabetic-code (currency (amount-ttc self)))))))
        (format stream "    --------- ----- -------- ---------~%")))
    ;; === TOTAL ENTRY LINES ===========================================
    (let* ((desc-l (trim-justify-and-split-text (description self) 38))
           (desc   (pop desc-l)))
      (format stream "    ~2,,9$ ~4,1F% ~2,,8$ ~2,,9$ ~A~%"
              (amount-magnitude (amount-ht  self))
              (* 100 (vat-rate self))
              (amount-magnitude (amount-vat self))
              (amount-magnitude (amount-ttc self)) desc)
      (dolist (desc desc-l)
        (format stream "~38A ~A~%" "" desc))))
  (format stream "--- ---------- ----------------------- ~
                    ----------------------------------------~%")) ;;GENERATE


;;;---------------------------------------------------------------------
;;; JOURNAL-ENTRY
;;;---------------------------------------------------------------------

;; (defstruct (journal-entry (:type list))
;;   (date        (calendar-current-date)          :type date)
;;   (amount-ht   (amount-zero *default-currency*) :type amount)
;;   (vat-rate    16/100                           :type ratio)
;;   (description "Default journal entry"          :type string)
;;   (kind        :PRESTACION-NACIONAL             :type keyword)
;;   (nif         "s/n"                            :type string)
;;   (invoices    '()                              :type list));;journal-entry
;;
;; (defmethod date        ((self journal-entry)) (journal-entry-date        self))
;; (defmethod amount-ht   ((self journal-entry)) (journal-entry-amount-ht   self))
;; (defmethod vat-rate    ((self journal-entry)) (journal-entry-vat-rate    self))
;; (defmethod description ((self journal-entry)) (journal-entry-description self))
;; (defmethod kind        ((self journal-entry)) (journal-entry-kind        self))
;; (defmethod nif         ((self journal-entry)) (journal-entry-nif         self))
;; (defmethod invoices    ((self journal-entry)) (journal-entry-invoices    self))
;;
;;
;; (defmethod initialize-instance ((self journal-entry)
;;                                 &key AMOUNT-HT VAT-RATE KIND &allow-other-keys)
;;   (unless (MEMBER KIND
;;                   '(:PRESTACION-NACIONAL
;;                     :PRESTACION-INTRACOMUNITARIA
;;                     :IMPUESTO
;;                     :INVERSION :GASTO-CORRIENTE
;;                     :ADQUISICION-INTRACOMUNITARIA))
;;     (ERROR "Invalid kind ~A." KIND))
;;   (when (negativep VAT-RATE)
;;     (ERROR "VAT-RATE must always be >= 0.00."))
;;   (unless (EQ KIND :PRESTACION-INTRACOMUNITARIA)
;;     (unless (MEMBER KIND '(:PRESTACION-NACIONAL)) ;;
;;       (when (>= 0.00 AMOUNT-HT)
;;         (ERROR "AMOUNT-HT must be > 0.00 for an entry of kind ~A." KIND))))
;;   (COND
;;     ((EQ KIND :PRESTACION-NACIONAL)
;;      (when (negativep VAT-RATE)
;;        (ERROR "VAT-RATE must be > 0.00 for an entry of kind ~A." KIND)))
;;     ((MEMBER KIND '(:PRESTACION-INTRACOMUNITARIA :IMPUESTO))
;;      (when (positivep VAT-RATE)
;;        (ERROR "VAT-RATE must be = 0.00 for an entry of kind ~A." KIND)))
;;     (T
;;      (IF (zerop VAT-RATE)
;;          (ERROR "VAT-RATE must be > 0.00 for an entry of kind ~A." KIND))))
;;   ());;initialize-instance
;;
;;
;; (DEFMETHOD IS-CREDIT   ((SELF JOURNAL-ENTRY))
;;   "RETURN: Whether the SELF is a credit."
;;   (positivep (AMOUNT-HT SELF)))
;;
;;
;; (DEFMETHOD CREDIT-HT   ((SELF JOURNAL-ENTRY))
;;   (IF (IS-CREDIT SELF)
;;       (AMOUNT-HT SELF)
;;       (amount-zero (currency (AMOUNT-HT SELF)))))
;;
;;
;; (DEFMETHOD CREDIT-VAT  ((SELF JOURNAL-ENTRY))
;;   (* (CREDIT-HT SELF) (VAT-RATE SELF)))
;;
;;
;; (DEFMETHOD DEBIT-HT    ((SELF JOURNAL-ENTRY))
;;   (IF (IS-CREDIT SELF)
;;       (amount-zero (currency (AMOUNT-HT SELF)))
;;       (- (AMOUNT-HT SELF))))
;;
;;
;; (DEFMETHOD DEBIT-VAT   ((SELF JOURNAL-ENTRY))
;;   (* (DEBIT-HT SELF) (VAT-RATE SELF)))
;;
;;
;; (DEFMETHOD DEBIT-VAT-INVERSION  ((SELF JOURNAL-ENTRY))
;;   (IF (EQ :INVERSION (KIND SELF))
;;       (DEBIT-VAT SELF)
;;       (amount-zero (currency (AMOUNT-HT SELF)))))
;;
;;
;; (DEFMETHOD DEBIT-VAT-CORRIENTE  ((SELF JOURNAL-ENTRY))
;;   (IF (EQ :GASTO-CORRIENTE (KIND SELF))
;;       (DEBIT-VAT SELF)
;;       (amount-zero (currency (AMOUNT-HT SELF)))))


;;;---------------------------------------------------------------------
;;; JOURNAL
;;;---------------------------------------------------------------------


(defgeneric trimestre (journal)
  (:documentation "RETURN: The quarter of the journal (1 2 3 or 4)."))

(defclass journal ()
  ((sorted :initform nil
           :accessor sorted
           :type boolean
           :documentation "Indicates whether entries are sorted.")
   (entries :initform '()
            :initarg :entries
            :accessor entries
            :type list)
   (year :initform (date-year (calendar-current-date))
         :initarg :year
         :accessor year
         :type (integer 1998 2070))
   (trimestre :initform (1+ (truncate
                             (1- (date-month (calendar-current-date))) 3))
              :initarg :trimestre
              :accessor trimestre
              :type (member 1 2 3 4)))
  (:documentation "An account journal."))


(defmethod reset ((self journal))
  "
POST: (null (entries self))
"
  (setf (entries self) '()
        (sorted  self) nil)) ;;RESET


(defmethod add-entry ((self journal) (entry movement))
  "
DOES:   Add the ENTRY into the journal.
POST:   (AND (MEMBER ENTRY (ENTRIES SELF)) (NOT (SORTED SELF)))
"
  (push entry (entries self))
  (setf (sorted self) nil)) ;;ADD-ENTRY


(defmethod ensure-sorted ((self journal))
  "
POST:   (sorted *journal*)
"
  (unless (sorted self)
    (setf (entries self) (sort (entries self)
                               (lambda (a b) (date-after (date b) (date a))))
          (sorted self) t))) ;;ENSURE-SORTED


(defmethod extract ((self journal) year trimestre)
  "
RETURN: The entries of the journal corresponding
        to the given YEAR and TRIMESTRE.
"
  (ensure-sorted self)
  (remove-if
   (lambda (entry) (not (date-in-year-trimestre (date entry) year trimestre)))
   (entries self))) ;;EXTRACT


(defun journal-totals-of-entries (entries)
  "PRIVATE
RETURN: a list containing the totals:
        credit-ht credit-vat debit-ht debit-vat-inversion debit-vat-corriente.
"
  (if (null entries)
      (make-list 5 :initial-element (amount-zero *default-currency*))
      (let* ((zero (amount-zero (currency (credit-ht (first entries)))))
             (credit-ht   zero)
             (credit-vat  zero)
             (debit-ht    zero)
             (debit-vat-c zero)
             (debit-vat-i zero))
        (mapcar
         (lambda (entry)
           (setf credit-ht   (+ credit-ht   (credit-ht  entry))
                 credit-vat  (+ credit-vat  (credit-vat entry))
                 debit-ht    (+ debit-ht    (debit-ht   entry))
                 debit-vat-c (+ debit-vat-c (debit-vat-corriente  entry))
                 debit-vat-i (+ debit-vat-i (debit-vat-inversion  entry))))
         entries)
        (list credit-ht credit-vat debit-ht debit-vat-i debit-vat-c))))


(defun journal-split-and-justify-description (description width)
  "PRIVATE"
  (or (mapcan (lambda (line) (split-lines(string-justify-left line width 0)))
              (split-lines description))
      (list " ")))


(defun journal-print-header (year trimestre &key (stream t))
  "PRIVATE"
  (format stream "----------------------------------------~
                  ---------------------------------------~%")
  (format stream  "~36D - TRIMESTRE ~D~%" year trimestre)
  (format stream "--- ---------- ----------------------- ~
                  ----------------------------------------~%")
  (format stream "SEN FECHA      IDENTIFICACION          NOMBRE~%")
  (format stream "TID   IMPORTE  IVA%     +IVA     TOTAL ~
                  NUMERO FACTURA / DESCRIPTION~%")
  (format stream "--- --------- ----- -------- --------- ~
                  ---------------------------------------~%")
  (values))


(defun journal-print-trailer (&key (stream t))
  "PRIVATE"
  (format stream "      IMPORTE            IVA     TOTAL~%")
  (format stream "--- --------- ----- -------- --------- ~
                  ---------------------------------------~%")
  (values))


(defun journal-print-totals (totals &key (stream t))
  "PRIVATE"
  (let ((credit-ht  (nth 0 totals))
        (credit-vat (nth 1 totals))
        (debit-ht   (nth 2 totals))
        (debit-vat  (+ (nth 3 totals) (nth 4 totals))))
    (format stream  "    ~2,,9$ ~5A ~2,,8$ ~9A ~A~%"
            (amount-magnitude credit-ht) ""
            (amount-magnitude credit-vat) "" "Credito")
    (format stream  "    ~2,,9$ ~5A ~2,,8$ ~9A ~A~%"
            (amount-magnitude (- debit-ht)) ""
            (amount-magnitude (- debit-vat)) "" "Debido")
    (format stream "    ~2,,9$ ~5A ~2,,8$ ~9A ~A~%"
            (amount-magnitude (- credit-ht  debit-ht))  ""
            (amount-magnitude (- credit-vat debit-vat)) ""
            "Saldo")
    (values)))


(defun kind-to-order (kind)
  (case kind
    ((:prestacion-nacional)          1)
    ((:prestacion-intracomunitaria)  2)
    ((:impuesto)                     3)
    ((:inversion)                    4)
    ((:gasto-corriente)              5)
    ((:adquisicion-intracomunitaria) 6)
    (otherwise                       7)))


(defmethod generate ((self journal) &key (stream t) (verbose nil)
                     (language :en))
  "
DOES:   Prints the formated entries of the journal onto the stream.
"
  (ensure-sorted self)
  (let* ((entries
          (sort (extract self (year self) (trimestre self))
                (lambda (a b)
                  (cond
                    ((= (kind-to-order (kind a)) (kind-to-order (kind b)))
                     (not (date-after  (date a) (date b))))
                    (t (< (kind-to-order (kind a)) (kind-to-order (kind b))))))))
         (totals  (journal-totals-of-entries entries)))
    (format stream "~2%")
    (journal-print-header (year self) (trimestre self) :stream stream)
    (mapcar (lambda (entry) (generate entry :stream stream :verbose verbose
                                      :language language)) entries)
    (journal-print-trailer       :stream stream)
    (journal-print-totals totals :stream stream)
    (format stream "~2%")
    (values)))


;;;---------------------------------------------------------------------
;;; Reading Journal File.
;;;---------------------------------------------------------------------


(defmacro person (&rest args)
  "
DO:       Add to the *INVOICE-SET* a new FISCAL-PERSON instance
          created with the give initargs.
"
  `(add-person *invoice-set* (make-instance 'fiscal-person ,@args)))


(defmacro make-bank-reference (&rest args)
  "
RETURN: A new instance of BANK-REFERENCE with the given initargs.
"
  `(make-instance 'bank-reference ,@args))


(defmacro invoice (&rest args)
  (do ((args args)
       (attributes '())
       (lines '())
       (vinst (gensym)))
      ((null args)
       `(let ((,vinst (make-instance 'invoice ,@(nreverse attributes))))
          ,@(mapcar
             (lambda (line)
               `(add-line ,vinst (make-instance 'invoice-line ,@line)))
             (nreverse lines))
          (add-invoice *invoice-set* ,vinst)))
    (cond
      ((keywordp (car args))
       (push (pop args) attributes)
       (push (pop args) attributes))
      ((atom (car args)) (error "Invalid invoice attribute ~S" (pop args)))
      ((eql 'line (caar args))
       (push (list* :object-id (string (gensym "L")) (cdr (pop args))) lines))
      (t (error "Invalid invoice attribute ~S" (pop args)))))) ;;INVOICE


(defmacro journal-entry (date amount-ttc vat-rate nif fac description kind)
  "
DOES:   Add a new journal entry.
        AMOUNT-TTC is the total paid (including VAT) expressed in Euros.
        VAT-RATE is the V.A.T percentage.
"
  `(add-entry *journal*
              (make-movement (date-from-string ',date) ',amount-ttc ',vat-rate ',nif
                             (if (listp ',fac) ',fac (list ',fac))
                             ',description ',kind))) ;;JOURNAL-ENTRY


(defun load-journal (path &key (verbose *load-verbose*) (print *load-print*))
  "
DO:        Load the journal at PATH.
"
  (let ((*readtable* *currency-readtable*)))
  (load path :verbose verbose :print print))


;; (in-package :common-lisp-user)
;; (cd "/home/pascal/jobs/free-lance/accounting/")
;; (load  "invoice")


;;;; THE END ;;;;
