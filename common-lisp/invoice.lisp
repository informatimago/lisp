;;;; -*- coding:utf-8 -*-
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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 1990 - 2004
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
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.INVOICE"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.ISO4217"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "COMMON-LISP")
  (:EXPORT "LOAD-JOURNAL" "JOURNAL-ENTRY" "LINE" "PERSON" "GENERATE" "TRIMESTRE"
           "MAKE-BANK-REFERENCE" "JOURNAL" "MOVEMENT" "INVOICE-SET" "INVOICE"
           "INVOICE-LINE" "FISCAL-PERSON" "BANK-REFERENCE" "PJB-OBJECT" "*JOURNAL*"
           "*INVOICE-SET*" "*CURRENCY-READTABLE*")
  (:SHADOW "ABS" "ZEROP" "ROUND" "/=" "=" ">=" ">" "<=" "<" "/" "*" "-" "+")
  (:DOCUMENTATION ""))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.INVOICE")


;;; parameters

(DEFPARAMETER *VAT-RATES* '(0/100 4/100 7/100 16/100) 
  "The valid VAT rates in the country of the user.")


(DEFPARAMETER *INVOICE-DIRECTORY-PATH*
  '(:ABSOLUTE "HOME""PASCAL""JOBS""FREE-LANCE""INVOICES")
  "The directory where the generated invoices are stored.")


(DEFPARAMETER *INVOICE-SET-FILE-PATH*
  (MAKE-PATHNAME :DIRECTORY *INVOICE-DIRECTORY-PATH*
                 :NAME "INVOICES" :TYPE "DATA")
  "Path to the file where invoices are stored.")


;;; global variables:

(DEFPARAMETER *DEFAULT-CURRENCY* (FIND-CURRENCY :EUR)
  "The currency used when no prefix currency code is given to #m")


(DEFPARAMETER *MAX-MOVEMENT-AMOUNT* NIL
  "The maximum movement amount (ht or ttc, expressed in the currency of the 
movement (weak, I know).")


(DEFPARAMETER *INVOICE-SET* NIL
  "Current Invoice Set (instance of INVOICE-SET).") ;;invoice-set


(DEFPARAMETER *JOURNAL* NIL
  "Current Journal (instance of JOURNAL).")


;;;---------------------------------------------------------------------

(DEFGENERIC ABS (SELF))
(DEFGENERIC ADD-ENTRY (SELF ENTRY))
(DEFGENERIC ADD-INVOICE (SELF INVOICE))
(DEFGENERIC ADD-LINE (SELF LINE))
(DEFGENERIC ADD-PERSON (SELF PERSON &OPTIONAL FISC))
(DEFGENERIC AMOUNT-HT (SELF))
(DEFGENERIC COMPUTE-TOTALS (SELF))
(DEFGENERIC CREDIT-HT (SELF))
(DEFGENERIC CREDIT-VAT (SELF))
(DEFGENERIC CURRENCY (SELF))
(defgeneric amount-magnitude (self))
(DEFGENERIC DEBIT-HT (SELF))
(DEFGENERIC DEBIT-VAT (SELF))
(DEFGENERIC DEBIT-VAT-CORRIENTE (SELF))
(DEFGENERIC DEBIT-VAT-INVERSION (SELF))
(DEFGENERIC ENSURE-SORTED (SELF))
(DEFGENERIC EXTRACT (SELF YEAR TRIMESTRE))
(DEFGENERIC GENERATE (SELF &KEY STREAM VERBOSE language &ALLOW-OTHER-KEYS))
(DEFGENERIC GET-INVOICE-WITH-ISSUER-AND-NUMBER (SELF ISSUER-FISCAL-ID INVOICE-NUMBER))
(DEFGENERIC GET-PERSON-WITH-FISCAL-ID (SELF FISCAL-ID))
(DEFGENERIC INVOICES (SELF))
(DEFGENERIC IS-CREDIT (SELF))
(DEFGENERIC IS-REFUND (SELF))
(DEFGENERIC NEGATIVEP (SELF))
(DEFGENERIC POSITIVEP (SELF))
(DEFGENERIC RESET (SELF))
(DEFGENERIC ROUND (SELF &OPTIONAL DIVISOR))
(DEFGENERIC VAT-RATE (SELF))
(DEFGENERIC WRITE-INVOICE-FILE (SELF &KEY LANGUAGE))
(DEFGENERIC ZEROP (SELF))

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
;; in (com.informatimago.common-lisp.iso4217:get-currencies), 
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


(DEFSTRUCT (AMOUNT (:PREDICATE AMOUNTP) #+(or)(:PRINT-OBJECT PRINT-OBJECT))
  "An amount of money."
  CURRENCY
  (VALUE 0 :TYPE INTEGER))


(DEFMETHOD PRINT-OBJECT ((SELF AMOUNT) STREAM)
  (IF *PRINT-READABLY*
      (FORMAT STREAM "#~DM~V$"
              (CURRENCY-NUMERIC-CODE (AMOUNT-CURRENCY SELF))
              (CURRENCY-MINOR-UNIT (AMOUNT-CURRENCY SELF))
              (AMOUNT-MAGNITUDE SELF))
      (FORMAT STREAM "~V$ ~A"
              (CURRENCY-MINOR-UNIT (AMOUNT-CURRENCY SELF))
              (AMOUNT-MAGNITUDE SELF)
              (CURRENCY-ALPHABETIC-CODE (AMOUNT-CURRENCY SELF))))
  SELF) ;;PRINT-OBJECT


(DEFMETHOD CURRENCY ((SELF number))
  nil)


(DEFMETHOD CURRENCY ((SELF AMOUNT))
  (AMOUNT-CURRENCY SELF))


(defmethod amount-magnitude ((self number))
  self)


(defmethod AMOUNT-MAGNITUDE ((SELF amount))
  "
RETURN: A real equal to the value of the amount.
"
  (* (AMOUNT-VALUE SELF)
     (AREF #(1 1/10 1/100 1/1000 1/10000)
           (CURRENCY-MINOR-UNIT (AMOUNT-CURRENCY SELF)))))


(DEFPARAMETER *ZERO-AMOUNTS* (MAKE-HASH-TABLE :TEST (FUNCTION EQ))
  "A cache of 0 amount for the various currencies used.")


(DEFUN AMOUNT-ZERO (CURRENCY)
  "
RETURN: A null amount of the given currency.
"
  (LET ((ZERO (GETHASH (FIND-CURRENCY CURRENCY) *ZERO-AMOUNTS*)))
    (UNLESS ZERO
      (SETF ZERO
            (SETF (GETHASH (FIND-CURRENCY CURRENCY) *ZERO-AMOUNTS*) 
                  (MAKE-AMOUNT :CURRENCY (FIND-CURRENCY CURRENCY) :VALUE 0))))
    ZERO)) ;;AMOUNT-ZERO


(DEFMETHOD ABS       ((SELF NUMBER)) (COMMON-LISP:ABS   SELF))
(DEFMETHOD ABS       ((SELF AMOUNT)) 
  (MAKE-AMOUNT :CURRENCY (AMOUNT-CURRENCY SELF)
               :VALUE    (COMMON-LISP:ABS   (AMOUNT-VALUE SELF))))


(DEFMETHOD ZEROP     ((SELF NUMBER)) (COMMON-LISP:ZEROP SELF))
(DEFMETHOD ZEROP     ((SELF AMOUNT)) (COMMON-LISP:ZEROP (AMOUNT-VALUE SELF)))


(DEFMETHOD POSITIVEP ((SELF NUMBER)) (COMMON-LISP:<= 0 SELF))
(DEFMETHOD POSITIVEP ((SELF AMOUNT)) (COMMON-LISP:<= 0 (AMOUNT-VALUE SELF)))


(DEFMETHOD NEGATIVEP ((SELF NUMBER)) (COMMON-LISP:> 0 SELF))
(DEFMETHOD NEGATIVEP ((SELF AMOUNT)) (COMMON-LISP:> 0 (AMOUNT-VALUE SELF)))


(DEFMETHOD ROUND ((SELF REAL) &OPTIONAL (DIVISOR 1))
  (COMMON-LISP:ROUND SELF DIVISOR))


(DEFMETHOD ROUND ((SELF AMOUNT) &OPTIONAL (DIVISOR 1))
  (MAKE-AMOUNT :CURRENCY (AMOUNT-CURRENCY SELF)
               :VALUE (COMMON-LISP:ROUND (AMOUNT-VALUE SELF) DIVISOR)))


(DEFUN EURO-ROUND (MAGNITUDE CURRENCY)
  "
MAGNITUDE:  A REAL
CURRENCY:   The currency of the amount.
RETURN:     An integer in minor unit rounded according to the Euro rule."
  (LET ((ROUNDER (AREF #(1 1/10 1/100 1/1000 1/10000)
                       (CURRENCY-MINOR-UNIT CURRENCY))))
    (ROUND (+ MAGNITUDE (* (SIGNUM MAGNITUDE) (/ ROUNDER 10))) ROUNDER)))


(DEFUN EURO-VALUE-ROUND (VALUE)
  "
VALUE:      A REAL
CURRENCY:   The currency of the amount.
RETURN:     An integer in minor unit rounded according to the Euro rule."
  (ROUND (+ VALUE (* (SIGNUM VALUE) 1/10)))) ;;EURO-VALUE-ROUND



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
  (ERROR 'multi-currency-error
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


(DEFMACRO MAKE-COMPARISON-METHOD (NAME OPERATOR)
  "
DO:     Generate a comparison method.
"
  `(DEFUN ,NAME (&REST ARGS)
     (COND
       ((EVERY (FUNCTION NUMBERP) ARGS) 
        (APPLY (FUNCTION ,OPERATOR) ARGS))
       ((EVERY (FUNCTION AMOUNTP) ARGS)
        (LET ((CURRENCY (FIND-CURRENCY (AMOUNT-CURRENCY (FIRST ARGS)))))
          (IF (EVERY (LAMBDA (X) (EQ CURRENCY (FIND-CURRENCY (AMOUNT-CURRENCY X))))
                     (CDR ARGS))
              (APPLY (FUNCTION ,OPERATOR)  (MAPCAR (FUNCTION AMOUNT-VALUE) ARGS))
              (mcerror ',name args  "Comparison not implemented yet."))))
       (T (mcerror ',name args  "Incompatible types: ~A"
                   (types-of-arguments args))))))


(MAKE-COMPARISON-METHOD <  COMMON-LISP:<)
(MAKE-COMPARISON-METHOD <= COMMON-LISP:<=)
(MAKE-COMPARISON-METHOD >  COMMON-LISP:>)
(MAKE-COMPARISON-METHOD >= COMMON-LISP:>=)
(MAKE-COMPARISON-METHOD =  COMMON-LISP:=)
(MAKE-COMPARISON-METHOD /= COMMON-LISP:/=)


(DEFUN + (&REST ARGS)
  "
DO:    A Generic addition with numbers or amounts.
"
  (setf args (remove 0 args
                     :key  (lambda (x) (if (typep x 'amount) (amount-value x) x))
                     :test (function equal)))
  (COND
    ((EVERY (FUNCTION NUMBERP) ARGS) 
     (APPLY (FUNCTION COMMON-LISP:+) ARGS))
    ((EVERY (FUNCTION AMOUNTP) ARGS)
     (LET ((CURRENCY (FIND-CURRENCY (AMOUNT-CURRENCY (FIRST ARGS)))))
       (IF (EVERY (LAMBDA (X) (EQ CURRENCY (FIND-CURRENCY (AMOUNT-CURRENCY X))))
                  (CDR ARGS))
           (MAKE-AMOUNT :CURRENCY CURRENCY 
                        :VALUE (APPLY (FUNCTION COMMON-LISP:+)
                                      (MAPCAR (FUNCTION AMOUNT-VALUE) ARGS)))
           (mcerror '+ args  "Addtion not implemented yet."))))
    (T   (mcerror '+ args  "Incompatible types: ~A" (types-of-arguments args)))))


(DEFUN - (&REST ARGS)
  "
DO:    A Generic substraction with numbers or amounts.
"
  (setf args (cons (car args)
                   (remove 0 (cdr args)
                           :key (lambda (x) (if (typep x 'amount) (amount-value x) x))
                           :test (function equal))))
  (COND
    ((EVERY (FUNCTION NUMBERP) ARGS)
     (APPLY (FUNCTION COMMON-LISP:-) ARGS))
    ((zerop (first args))
     (- (apply (function +) (rest args))))
    ((EVERY (FUNCTION AMOUNTP) ARGS)
     (LET ((CURRENCY (FIND-CURRENCY (AMOUNT-CURRENCY (FIRST ARGS)))))
       (IF (EVERY (LAMBDA (X) (EQ CURRENCY (FIND-CURRENCY (AMOUNT-CURRENCY X))))
                  (CDR ARGS))
           (MAKE-AMOUNT :CURRENCY CURRENCY 
                        :VALUE (APPLY (FUNCTION COMMON-LISP:-)
                                      (MAPCAR (FUNCTION AMOUNT-VALUE) ARGS)))
           (mcerror '- args  "Substraction not implemented yet."))))
    (T   (mcerror '- args  "Incompatible types: ~A" (types-of-arguments args)))))


(DEFUN * (&REST ARGS)
  "
DO:    A Generic multiplication with numbers or amounts.
"
  (IF (EVERY (FUNCTION NUMBERP) ARGS)
      (APPLY (FUNCTION COMMON-LISP:*) ARGS)
      (LET ((P (POSITION-IF (FUNCTION AMOUNTP) ARGS)))
        (COND
          ((OR (NULL P) (NOT (EVERY (LAMBDA (X) (OR (AMOUNTP X)(REALP X))) ARGS)))
           (mcerror '* args  "Incompatible types: ~A" (types-of-arguments args)))
          ((POSITION-IF (FUNCTION AMOUNTP) ARGS :START (1+ P))
           (mcerror '* args  "Cannot multiply moneys."))
          (T
           (MAKE-AMOUNT
            :CURRENCY (AMOUNT-CURRENCY (NTH P ARGS))
            :VALUE (EURO-VALUE-ROUND
                    (APPLY (FUNCTION COMMON-LISP:*)
                           (MAPCAR (LAMBDA (X) (IF (AMOUNTP X) (AMOUNT-VALUE X) X))
                                   ARGS)))))))))


(DEFUN / (&REST ARGS)
  "
DO:    A Generic division with numbers or amounts.
"
  (COND
    ((EVERY (FUNCTION NUMBERP) ARGS)
     (APPLY (FUNCTION COMMON-LISP:/) ARGS))
    ((and (cadr args)
          (not (cddr args))             ; two arguments
          (amountp (first  args))
          (amountp (second args)))      ; both amounts
     ;; then return a number:
     (/ (amount-value (first args)) (amount-value (second args))))
    ((AND (AMOUNTP (CAR ARGS))
          (CDR ARGS) ;; cannot take the inverse of an amount!
          (EVERY (FUNCTION REALP) (CDR ARGS)))
     (MAKE-AMOUNT 
      :CURRENCY (AMOUNT-CURRENCY (CAR ARGS))
      :VALUE (EURO-VALUE-ROUND (APPLY (FUNCTION COMMON-LISP:/) 
                                      (AMOUNT-VALUE (CAR ARGS)) (CDR ARGS)))))
    (T (mcerror '/ args  "Incompatible types: ~A" (types-of-arguments args)))))



(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)

  (DEFUN CURRENCY-SYNTAX (STREAM CHAR INFIX)
    (DECLARE (IGNORE CHAR))
    (LET ((CURRENCY (OR INFIX *DEFAULT-CURRENCY*)))
      (SETF CURRENCY (FIND-CURRENCY CURRENCY))
      (UNLESS CURRENCY
        (mcerror 'currency-syntax (OR INFIX *DEFAULT-CURRENCY*)
                 "Invalid currency designator ~S" (OR INFIX *DEFAULT-CURRENCY*)))
      (ASSERT (<= 0 (CURRENCY-MINOR-UNIT CURRENCY) 4) ()
              "Unexpected  minor unit for currency: ~S" CURRENCY)
      (LET ((LEFT '())
            (RIGHT '())
            (DOT NIL)
            (SIGN 1))
        (LET ((CH (READ-CHAR STREAM NIL NIL)))
          (COND
            ((NULL CH))
            ((CHAR= CH (CHARACTER "-" )) (SETF SIGN -1))
            ((CHAR= CH (CHARACTER "+" )))
            (T (UNREAD-CHAR CH STREAM))))
        (LOOP FOR CH = (PEEK-CHAR NIL STREAM NIL NIL)
           WHILE (AND CH (DIGIT-CHAR-P CH))
           DO (PUSH (READ-CHAR STREAM) LEFT)
           FINALLY (SETF DOT (AND CH (CHAR= (CHARACTER ".") CH))))
        (WHEN (ZEROP (LENGTH LEFT))
          (mcerror 'currency-syntax currency "Missing an amount after #M"))
        (WHEN DOT
          (WHEN (ZEROP (CURRENCY-MINOR-UNIT CURRENCY))
            (mcerror 'currency-syntax currency
                     "There is no decimal point in ~A" (CURRENCY-NAME CURRENCY)))
          (READ-CHAR STREAM) ;; eat the dot
          (LOOP FOR CH = (PEEK-CHAR NIL STREAM NIL NIL)
             WHILE (AND CH (DIGIT-CHAR-P CH))
             DO (PUSH (READ-CHAR STREAM) RIGHT))
          (WHEN (< (CURRENCY-MINOR-UNIT CURRENCY) (LENGTH RIGHT))
            (mcerror 'currency-syntax currency
                     "Too many digits after the decimal point for ~A"
                     (CURRENCY-NAME CURRENCY))))
        (LOOP FOR I FROM (LENGTH RIGHT) BELOW (CURRENCY-MINOR-UNIT CURRENCY)
           DO (PUSH (CHARACTER "0") RIGHT))      
        (MAKE-AMOUNT
         :CURRENCY CURRENCY
         ;; (WITH-STANDARD-IO-SYNTAX
         ;;     (INTERN (CURRENCY-ALPHABETIC-CODE CURRENCY) "KEYWORD"))
         :VALUE (* SIGN (PARSE-INTEGER
                         (MAP 'STRING (FUNCTION IDENTITY)
                              (NREVERSE (NCONC RIGHT LEFT)))))
         ;;:divisor (AREF #(1 10 100 1000 10000)
         ;;   (CURRENCY-MINOR-UNIT CURRENCY))
         )))) ;;currency-syntax


  (DEFPARAMETER *CURRENCY-READTABLE* (COPY-READTABLE *READTABLE*)
    "The readtable used to read currencies.")


  (SET-DISPATCH-MACRO-CHARACTER  #\# #\M (FUNCTION CURRENCY-SYNTAX) 
                                 *CURRENCY-READTABLE*)

  (SET-DISPATCH-MACRO-CHARACTER  #\# #\M (FUNCTION CURRENCY-SYNTAX)
                                 *CURRENCY-READTABLE*)
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
  (DEFCONSTANT +SECONDS-IN-A-DAY+ (cl:* 24 3600) "Number of seconds in a day.")
  );;eval-when

(DEFSTRUCT (DATE #+(or)(:PRINT-OBJECT PRINT-OBJECT)) YEAR MONTH DAY)


(DEFMETHOD PRINT-OBJECT ((SELF DATE) STREAM)
  (FORMAT STREAM "~4,'0D-~2,'0D-~2,'0D" 
          (DATE-YEAR SELF) (DATE-MONTH SELF) (DATE-DAY SELF))
  SELF)


(DEFUN DATE-FROM-STRING (YYYY-MM-DD)
  (LET ((YMD (SPLIT-STRING YYYY-MM-DD "-")))
    (MAKE-DATE :YEAR  (PARSE-INTEGER (NTH 0 YMD))
               :MONTH (PARSE-INTEGER (NTH 1 YMD))
               :DAY   (PARSE-INTEGER (NTH 2 YMD))))) ;;DATE-FROM-STRING


(DEFUN DATE-AFTER (A B)
  (OR (> (DATE-YEAR A) (DATE-YEAR B))
      (AND (= (DATE-YEAR A) (DATE-YEAR B))
           (OR (> (DATE-MONTH A) (DATE-MONTH B))
               (AND (= (DATE-MONTH A) (DATE-MONTH B))
                    (> (DATE-DAY A) (DATE-DAY B))))))) ;;DATE-AFTER


(DEFUN DATE-IN-YEAR-TRIMESTRE (DATE YEAR TRIMESTRE)
  "
RETURN: Whether the given date is within the given YEAR and TRIMESTRE.
"
  (AND (= (DATE-YEAR DATE) YEAR)
       (MEMBER (DATE-MONTH DATE)
               (ELT '((1 2 3) (4 5 6) (7 8 9) (10 11 12))
                    (- TRIMESTRE 1))))) ;;DATE-IN-YEAR-TRIMESTRE


(DEFUN CALENDAR-CURRENT-DATE ()
  "
RETURN: The date today.
"
  (MULTIPLE-VALUE-BIND (SE MI HO DA MO YE DW DS ZO) (GET-DECODED-TIME) 
    (DECLARE (IGNORE SE MI HO DW DS ZO))
    (MAKE-DATE :YEAR YE :MONTH MO :DAY DA))) ;;CALENDAR-CURRENT-DATE


(DEFUN LOCAL-TIME-ZONE ()
  "
RETURN: The local time zone, as returned by GET-DECODED-TIME.
"
  (MULTIPLE-VALUE-BIND (SE MI HO DA MO YE DW DS ZONE) (GET-DECODED-TIME) 
    (DECLARE (IGNORE SE MI HO DA MO YE DW DS))
    ZONE)) ;;LOCAL-TIME-ZONE


(DEFUN UNIVERSAL-TIME-TO-DATE (UTIME)
  "
RETURN: the given universal time formated in the ISO8601 YYYY-MM-DD format.
"
  (MULTIPLE-VALUE-BIND (SE MI HO DA MO YE DW DS ZO) 
      (DECODE-UNIVERSAL-TIME UTIME 0)
    (DECLARE (IGNORE SE MI HO DW DS ZO))
    (FORMAT NIL "~4,'0D-~2,'0D--~2,'0D" YE MO DA))) ;;UNIVERSAL-TIME-TO-DATE


(DEFUN DATE-TO-UNIVERSAL-TIME (DATE-STRING)
  "
DATE-STRING:  A date in the ISO8601 format 'YYYY-MM-DD'.
RETURN:       A number of seconds since 1900-01-01 00:00:00 GMT.
"
  (LET ((YMD (SPLIT-STRING DATE-STRING "-")))
    (ENCODE-UNIVERSAL-TIME 0 0 0
                           (PARSE-INTEGER (THIRD  YMD))
                           (PARSE-INTEGER (SECOND YMD))
                           (PARSE-INTEGER (FIRST  YMD))
                           0))) ;;DATE-TO-UNIVERSAL-TIME


(DEFUN DATE-FORMAT (UTIME &KEY (LANGUAGE :EN))
  (MULTIPLE-VALUE-BIND (SE MI HO DAY MONTH YEAR DW DS ZO) 
      (DECODE-UNIVERSAL-TIME UTIME 0)
    (DECLARE (IGNORE SE MI HO DW DS ZO))
    (CASE LANGUAGE
      ((:FR)
       (FORMAT NIL "~D~A ~A ~D"
               DAY
               (IF (= 1 DAY) "er" "")
               (AREF #("Janvier" "Février" "Mars" "Avril"
                       "Mai" "Juin" "Juillet" "Août"
                       "Septembre" "Octobre" "Novembre" "Décembre") (1- MONTH))
               YEAR))
      ((:ES)
       (FORMAT NIL "~D de ~A de ~D"
               DAY
               (AREF #("Enero" "Febrero" "Marzo" "Abril"
                       "Mayo" "Junio" "Julio" "Augosto"
                       "Septiembre" "Octobre" "Noviembre" "Diciembre") (1- MONTH))
               YEAR))
      (OTHERWISE
       (FORMAT NIL "~A ~D~A, ~D"
               (AREF #("January" "February" "March" "April"
                       "May" "June" "July" "August"
                       "September" "October" "November" "December") (1- MONTH))
               DAY
               (CASE (MOD DAY 10)
                 ((1) "st")
                 ((2) "nd")
                 ((3) "rd")
                 (OTHERWISE "th"))
               YEAR)))))


;;;---------------------------------------------------------------------
;;; Pjb-Object
;;;---------------------------------------------------------------------


(DEFCLASS PJB-OBJECT ()
  ((OBJECT-ID
    :INITFORM NIL
    :INITARG  :OBJECT-ID
    :ACCESSOR OBJECT-ID
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The user-level ID of this object."))
  (:DOCUMENTATION "This is a root class for my classes."))


;;;---------------------------------------------------------------------
;;; BANK-REFERENCE
;;;---------------------------------------------------------------------


(DEFCLASS BANK-REFERENCE (PJB-OBJECT)
  ((BANK-NAME
    :INITFORM NIL
    :INITARG  :BANK-NAME
    :ACCESSOR BANK-NAME
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The name of the bank.")
   (BANK-ADDRESS
    :INITFORM NIL
    :INITARG  :BANK-ADDRESS
    :ACCESSOR BANK-ADDRESS
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The address of the bank.")
   (BRANCH-NAME
    :INITFORM NIL
    :INITARG  :BRANCH-NAME
    :ACCESSOR BRANCH-NAME
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The name of the branch.")
   (SWIFT-CODE
    :INITFORM NIL
    :INITARG  :SWIFT-CODE
    :ACCESSOR SWIFT-CODE
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The swift-code of the bank.")
   (ACCOUNT-NUMBER
    :INITFORM NIL
    :INITARG  :ACCOUNT-NUMBER
    :ACCESSOR ACCOUNT-NUMBER
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The account number. It should be an IBAN in Europe.")
   (BENEFICIARY-NAME
    :INITFORM NIL
    :INITARG  :BENEFICIARY-NAME
    :ACCESSOR BENEFICIARY-NAME
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The beneficiary's name."))
  (:DOCUMENTATION "A bank account reference.")) ;;BANK-REFERENCE


;;;---------------------------------------------------------------------
;;; FISCAL-PERSON
;;;---------------------------------------------------------------------


(DEFCLASS FISCAL-PERSON (PJB-OBJECT)
  ((FISCAL-ID
    :INITFORM NIL
    :INITARG  :FISCAL-ID
    :ACCESSOR FISCAL-ID
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The fiscal ID of the person, ie. the European fiscal ID.")
   (NAME
    :INITFORM NIL
    :INITARG  :NAME
    :ACCESSOR NAME
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The name of the person.")
   (ADDRESS
     :INITFORM NIL
     :INITARG  :ADDRESS
     :ACCESSOR ADDRESS
     :TYPE     (OR NULL STRING)
     :DOCUMENTATION "The address of the person.")
   (PHONE
    :INITFORM NIL
    :INITARG  :PHONE
    :ACCESSOR PHONE
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The phone number of the person.")
   (FAX
    :INITFORM NIL
    :INITARG  :FAX
    :ACCESSOR FAX
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The fax number of the person.")
   (WEB
    :INITFORM NIL
    :INITARG  :WEB
    :ACCESSOR WEB
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The URL of the web site of this person.")
   (EMAIL
    :INITFORM NIL
    :INITARG  :EMAIL
    :ACCESSOR EMAIL
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The fax number of the person.")
   (BANK-REFERENCE
    :INITFORM NIL
    :INITARG  :BANK-REFERENCE
    :ACCESSOR BANK-REFERENCE
    :TYPE     (OR NULL BANK-REFERENCE)
    :DOCUMENTATION "The bank reference of the person.")
   (LANGUAGE
    :INITFORM :ES
    :INITARG  :LANGUAGE
    :ACCESSOR LANGUAGE
    :TYPE     SYMBOL ;; :es :en :fr :de
    :DOCUMENTATION "The language (two-letter code) used by this person.")
   (FISC
    :INITFORM NIL
    :INITARG :FISC
    :ACCESSOR FISC
    :TYPE     BOOLEAN
    :DOCUMENTATION "Whether this person is the fiscal administration."))
  (:DOCUMENTATION
   "A person (physical or moral) identified by a fiscal identification number."
   )) ;;FISCAL-PERSON


(defmethod initialize-instance :after ((self fiscal-person) &rest arguments)
  (unless (getf arguments :object-id)
    (setf (object-id self) (or (fiscal-id self) (name self))))
  self)


;;;---------------------------------------------------------------------
;;; Invoice-Line
;;;---------------------------------------------------------------------


(DEFCLASS INVOICE-LINE (PJB-OBJECT)
  ((DESCRIPTION
    :INITFORM ""
    :INITARG  :DESCRIPTION
    :ACCESSOR DESCRIPTION
    :TYPE     STRING
    :DOCUMENTATION
    "The description of this line.")
   (CURRENCY
    :INITFORM (FIND-CURRENCY :EUR)
    :INITARG  :CURRENCY
    :ACCESSOR CURRENCY
    :TYPE     SYMBOL
    :DOCUMENTATION
    "The currency of this line.")
   (AMOUNT-HT
    :INITFORM (AMOUNT-ZERO *DEFAULT-CURRENCY*)
    :INITARG  :AMOUNT-HT
    :ACCESSOR AMOUNT-HT
    :TYPE     AMOUNT
    :DOCUMENTATION
    "The amount excluding the taxes of this line.")
   (VAT-RATE
    :INITFORM 0/100
    :INITARG  :VAT-RATE
    :ACCESSOR VAT-RATE
    :TYPE     RATIO
    :DOCUMENTATION
    "The rate of VAT for this line (0.00 <= vat-rate <= 0.50).")
   (AMOUNT-VAT
    :INITFORM (AMOUNT-ZERO *DEFAULT-CURRENCY*)
    :INITARG  :AMOUNT-VAT
    :ACCESSOR AMOUNT-VAT
    :TYPE     AMOUNT
    :DOCUMENTATION
    "The amount of VAT for this line. ( = amount-ht * (1+vat-rate) )")
   (AMOUNT-TTC
    :INITFORM (AMOUNT-ZERO *DEFAULT-CURRENCY*)
    :INITARG  :AMOUNT-TTC
    :ACCESSOR AMOUNT-TTC
    :TYPE     AMOUNT
    :DOCUMENTATION
    "The amount including the taxes of this line."))
  (:DOCUMENTATION "An Invoice Line.")) ;;INVOICE-LINE


;;;---------------------------------------------------------------------
;;; INVOICE
;;;---------------------------------------------------------------------


(DEFCLASS INVOICE (PJB-OBJECT)
  ((DATE
    :INITFORM NIL
    :INITARG  :DATE
    :ACCESSOR DATE
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION
    "'YYYY-MM-DD' The date of the invoice.")
   (ISSUER-FISCAL-ID
    :INITFORM NIL
    :INITARG  :ISSUER-FISCAL-ID
    :ACCESSOR ISSUER-FISCAL-ID
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION
    "The fiscal ID of the issuer of this invoice.")
   (INVOICE-NUMBER
    :INITFORM NIL
    :INITARG  :INVOICE-NUMBER
    :ACCESSOR INVOICE-NUMBER
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION
    "The invoice number.")
   (PAYER-FISCAL-ID
    :INITFORM NIL
    :INITARG  :PAYER-FISCAL-ID
    :ACCESSOR PAYER-FISCAL-ID
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION
    "The fiscal ID of the payer of this invoice.")
   (TITLE
     :INITFORM ""
     :INITARG  :TITLE
     :ACCESSOR TITLE
     :TYPE     (OR NULL STRING)
     :DOCUMENTATION
     "The title of this invoice.")
   (CURRENCY
    :INITFORM (FIND-CURRENCY :EUR)
    :INITARG  :CURRENCY
    :ACCESSOR CURRENCY
    :TYPE     SYMBOL
    :DOCUMENTATION
    "The currency of this invoice.")
   (LINES
    :INITFORM NIL
    :ACCESSOR LINES
    :TYPE     LIST
    :DOCUMENTATION
    "(list of Invoice-Line) The line items of this invoice.")
   (TOTAL-HT
    :INITFORM (AMOUNT-ZERO *DEFAULT-CURRENCY*)
    :ACCESSOR TOTAL-HT
    :TYPE     AMOUNT
    :DOCUMENTATION
    "The total excluding taxes of this invoice.")
   (TOTAL-VAT
    :INITFORM (AMOUNT-ZERO *DEFAULT-CURRENCY*)
    :ACCESSOR TOTAL-VAT
    :TYPE     AMOUNT
    :DOCUMENTATION
    "The total of VAT.")
   (TOTAL-TTC
    :INITFORM (AMOUNT-ZERO *DEFAULT-CURRENCY*)
    :ACCESSOR TOTAL-TTC
    :TYPE     AMOUNT
    :DOCUMENTATION
    "The total including taxes of this invoice.")
   )
  (:DOCUMENTATION
   "An invoice, either outgoing or incoming.
    The amounts of the invoice may be negative when it's a refund.
    ")) ;;INVOICE


(defmethod initialize-instance :after ((self invoice) &rest arguments)
  (unless (getf arguments :object-id)
    (setf (object-id self) (concatenate 'string
                             (ISSUER-FISCAL-ID self) ":" (INVOICE-NUMBER self))))
  self)

;;;---------------------------------------------------------------------
;;; INVOICE-SET
;;;---------------------------------------------------------------------


(DEFCLASS INVOICE-SET (PJB-OBJECT)
  ((FISCAL-ID
    :INITFORM NIL
    :INITARG  :FISCAL-ID
    :ACCESSOR FISCAL-ID
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION
    "The fiscal id of the owner of this invoice set.")
   (FISC-FISCAL-IDS
    :INITFORM NIL
    :INITARG  :FISC-FISCAL-IDS
    :ACCESSOR FISC-FISCAL-IDS
    :TYPE     LIST
    :DOCUMENTATION
    "(list of string) List of fiscal-id of fisc entity. An invoice issued by
     on of these entities is actually a tax.")
   (PERSONS
    :INITFORM NIL
    :INITARG  :PERSONS
    :ACCESSOR PERSONS
    :TYPE     LIST
    :DOCUMENTATION
    "The list of known Fiscal-Person.")
   (INVOICES
    :INITFORM NIL
    :INITARG  :INVOICES
    :ACCESSOR INVOICES
    :TYPE     LIST
    :DOCUMENTATION
    "The list of known Invoices.")
   )
  (:DOCUMENTATION
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


(DEFMETHOD INITIALIZE-INSTANCE ((SELF INVOICE-LINE) 
                                &KEY OBJECT-ID DESCRIPTION CURRENCY
                                AMOUNT-HT VAT-RATE AMOUNT-VAT AMOUNT-TTC
                                &ALLOW-OTHER-KEYS)
  "
DO:      Checks that the values for the fields are within limits.
"
  (WHEN (OR (AND (NOT AMOUNT-TTC) (NOT  AMOUNT-HT))
            (AND (NOT AMOUNT-VAT) (NOT  VAT-RATE)))
    (ERROR "Not enought amount data defined for this line ~S." SELF))
  (UNLESS AMOUNT-TTC
    (SETF AMOUNT-TTC
          (COND
            (AMOUNT-VAT (+ AMOUNT-HT AMOUNT-VAT))
            (VAT-RATE   (* AMOUNT-HT (+ 1 VAT-RATE)))
            ;; last case should not occur.
            (T          (if (zerop vat-rate)
                            amount-ht
                            (/ (* AMOUNT-VAT (+ 1 VAT-RATE)) VAT-RATE))))))
  (UNLESS AMOUNT-HT
    (SETF AMOUNT-HT
          (COND
            (AMOUNT-VAT (- AMOUNT-TTC AMOUNT-VAT))
            (VAT-RATE   (/ AMOUNT-TTC (+ 1 VAT-RATE)))
            ;; last case should not occur.
            (T          (if (zerop vat-rate)
                            amount-ttc
                            (/ AMOUNT-VAT VAT-RATE))))))
  (UNLESS AMOUNT-VAT
    (SETF AMOUNT-VAT    (- AMOUNT-TTC AMOUNT-HT)))
  (UNLESS VAT-RATE
    (SETF VAT-RATE (if (zerop (AMOUNT-MAGNITUDE AMOUNT-HT))
                       0
                       (/ (ROUND (* (/ (AMOUNT-MAGNITUDE AMOUNT-VAT)
                                       (AMOUNT-MAGNITUDE AMOUNT-HT)) 100)) 100))))
  (WHEN (NULL CURRENCY)
    (SETF CURRENCY (CURRENCY AMOUNT-TTC)))
  ;; (check-vat amount-ttc amount-ht amount-vat vat-rate)
  (CALL-NEXT-METHOD SELF 
                    :OBJECT-ID     OBJECT-ID
                    :DESCRIPTION   DESCRIPTION
                    :CURRENCY      CURRENCY
                    :AMOUNT-HT     AMOUNT-HT
                    :VAT-RATE      VAT-RATE
                    :AMOUNT-VAT    AMOUNT-VAT
                    :AMOUNT-TTC    AMOUNT-TTC))


;;;---------------------------------------------------------------------
;;; INVOICE
;;;---------------------------------------------------------------------


(DEFMETHOD COMPUTE-TOTALS ((SELF INVOICE))
  "
DO:      Compute the totals.
"
  (LET* ((TH (AMOUNT-ZERO (CURRENCY SELF))) (TV TH) (TT TH))
    (DOLIST (LINE  (LINES SELF))
      (SETF TH (+ TH (AMOUNT-HT  LINE))
            TV (+ TV (AMOUNT-VAT LINE))
            TT (+ TT (AMOUNT-TTC LINE))))
    (SETF (TOTAL-HT  SELF) TH
          (TOTAL-VAT SELF) TV
          (TOTAL-TTC SELF) TT))) ;;COMPUTE-TOTALS
  

(DEFMETHOD VAT-RATE ((SELF INVOICE))
  "
RETURN: A computed VAT rate for this invoice.
"
  (if (zerop (AMOUNT-MAGNITUDE (TOTAL-HT SELF)))
      0
      (/ (ROUND (* 100 (/ (AMOUNT-MAGNITUDE (TOTAL-VAT SELF))
                          (AMOUNT-MAGNITUDE (TOTAL-HT SELF))))) 100)))


(DEFMETHOD ADD-LINE ((SELF INVOICE) (LINE INVOICE-LINE))
  "
PRE:     (eq (find-currency (currency self))(find-currency (currency line)))
DO:      Add the line.
"
  (ASSERT (EQ (FIND-CURRENCY (CURRENCY SELF)) (FIND-CURRENCY (CURRENCY LINE))))
  (SETF (LINES SELF) (APPEND (LINES SELF) (LIST LINE)))
  (COMPUTE-TOTALS SELF)) ;;ADD-LINE


(DEFMETHOD IS-REFUND ((SELF INVOICE))
  "
RETURN: Whether this invoice is a refund invoice.
"
  (NEGATIVEP (TOTAL-TTC SELF))) ;;IS-REFUND


(DEFTRANSLATION *INVOICE-STRINGS* "Phone:" 
  :EN :IDEM
  :FR "Téléphone :"
  :ES "Teléfono :")

(DEFTRANSLATION *INVOICE-STRINGS* "Fax:" 
  :EN :IDEM
  :FR "Télécopie :"
  :ES "Telécopia :")

(DEFTRANSLATION *INVOICE-STRINGS* "Email:" 
  :EN :IDEM
  :FR "Couriel :"
  :ES "Email :")

(DEFTRANSLATION *INVOICE-STRINGS* "VAT Immatriculation:" 
  :EN :IDEM
  :FR "TVA Intracommunautaire :"
  :ES "Imatriculación IVA :")

(DEFTRANSLATION *INVOICE-STRINGS* "INVOICE" 
  :EN :IDEM
  :FR "FACTURE"
  :ES "FACTURA")

(DEFTRANSLATION *INVOICE-STRINGS* "Date:" 
  :EN :IDEM
  :FR "Date :"
  :ES "Fecha :")

(DEFTRANSLATION *INVOICE-STRINGS* "Invoice no.:" 
  :EN :IDEM
  :FR "Facture nº :"
  :ES "Nº de factura :")

(DEFTRANSLATION *INVOICE-STRINGS* "Billing address:" 
  :EN :IDEM
  :FR "Adresse de facturation :"
  :ES "Dirección de factura :")

(DEFTRANSLATION *INVOICE-STRINGS* "Description" 
  :EN :IDEM
  :FR "Description"
  :ES "Descripción")

(DEFTRANSLATION *INVOICE-STRINGS* "Price" 
  :EN :IDEM
  :FR "Prix"
  :ES "Precio")

(DEFTRANSLATION *INVOICE-STRINGS* "Total" 
  :EN :IDEM
  :FR "Total HT"
  :ES "Base imponible")

(DEFTRANSLATION *INVOICE-STRINGS* "VAT  ~5,1F %" 
  :EN :IDEM
  :FR "TVA  ~5,1F %"
  :ES "IVA  ~5,1F %")

(DEFTRANSLATION *INVOICE-STRINGS* "IRPF ~5,1F %" 
  :EN ""
  :FR ""
  :ES :IDEM)

(DEFTRANSLATION *INVOICE-STRINGS* "Total VAT Incl." 
  :EN :IDEM
  :FR "Total TTC"
  :ES "Total factura")

(DEFTRANSLATION *INVOICE-STRINGS* "PAYMENT-METHOD" 
  :EN "Method of Payment:  Bank Transfer
       Please make your payment using the details below,
       before ~A."
  :FR "Mode de règlement :  À régler par virement bancaire au compte suivant,
       avant le ~A."
  :ES "Forma de pago :  Transferencia bancaria a la cuenta siguiente,
       antes del ~A.") ;;*INVOICE-STRINGS*

(DEFTRANSLATION *INVOICE-STRINGS* "Payment Bank" 
  :EN :IDEM
  :FR "Banque destinataire"
  :ES "Banco")

(DEFTRANSLATION *INVOICE-STRINGS* "Branch Name" 
  :EN :IDEM
  :FR "Agence"
  :ES "Oficina")

(DEFTRANSLATION *INVOICE-STRINGS* "Account Number (IBAN)" 
  :EN :IDEM
  :FR "Numéro de compte (IBAN)"
  :ES "Número de cuenta (IBAN)")

(DEFTRANSLATION *INVOICE-STRINGS* "Beneficiary" 
  :EN :IDEM
  :FR "Bénéficiaire"
  :ES "Beneficiario")

(DEFTRANSLATION *INVOICE-STRINGS* "SWIFT Code" 
  :EN :IDEM
  :FR "Code SWIFT"
  :ES "Código SWIFT")

(DEFTRANSLATION *INVOICE-STRINGS* "Currency change" 
  :EN :IDEM
  :FR "Change devises"
  :ES "Cambio devisas")


(DEFMACRO LONGEST-LOCALIZED-LENGTH (TABLE LANGUAGE FIELDS)
  `(LOOP FOR FNAME IN ,FIELDS
      MAXIMIZE (LENGTH (LOCALIZE ,TABLE ,LANGUAGE FNAME)) INTO INCREMENT
      FINALLY (RETURN INCREMENT))) ;;LONGEST-LOCALIZED-LENGTH


(DEFPARAMETER +LINE-CHARS+ 
  (MAP 'STRING (FUNCTION IDENTITY) 
       '(#\LINEFEED #\RETURN #\NEWLINE #\PAGE #\VT))
  "A string containing the new-line characters.")


(DEFUN SPLIT-LINES (TEXT &KEY DELETE-EMPTY-LINES)
  "
DELETE-EMPTY-LINES:  When true, lines that are stripped empty are removed.
RETURN:              A list of stripped and splitted lines from the TEXT.
"
  (LET ((LINES (SPLIT-STRING TEXT +LINE-CHARS+)))
    (MAP-INTO LINES (LAMBDA (LINE) (STRING-TRIM " " LINE)) LINES)
    (IF DELETE-EMPTY-LINES
        (DELETE "" LINES :TEST (FUNCTION STRING=))
        LINES))) ;;SPLIT-LINES


(DEFUN ALIGN-FOLLOWING-LINES (TEXT LEFT-MARGIN)
  "
DO:     Format the TEXT inserting LEFT-MARGIN spaces before each line 
        but the first.
"
  (FORMAT NIL (FORMAT NIL "~~{~~A~~^~~%~VA~~}" LEFT-MARGIN "")
          (SPLIT-LINES TEXT))) ;;ALIGN-FOLLOWING-LINES


(DEFUN PRINT-PERSON-ADDRESS (TITLE LEFT-MARGIN PERSON 
                                   &KEY (LANGUAGE :FR) (STREAM T))
  "
DO:          Insert into the current buffer at the current point the address
             and phone, fax and email of the given person,
             prefixed by the title and with a left-margin of `left-margin'
             characters.  If the length of the title is greater than the
             `left-margin' then the length of the title is used instead.
LANGUAGE:     The default language is French (:FR),
              :EN and :ES are also available for English and Spanish.
"
  (UNLESS TITLE (SETF TITLE ""))
  (WHEN (< LEFT-MARGIN (LENGTH TITLE))
    (SETF LEFT-MARGIN (LENGTH TITLE)))
  ;; title / name
  (FORMAT STREAM "~A~A~%" (STRING-PAD TITLE LEFT-MARGIN) (NAME PERSON))
  ;; address
  (FORMAT STREAM (FORMAT NIL "~~{~VA~~A~~%~~}" LEFT-MARGIN "")
          (SPLIT-LINES (ADDRESS PERSON) :DELETE-EMPTY-LINES T))
  ;; other fields
  (LET* ((FIELDS '("Phone:" "Fax:" "Email:" "VAT Immatriculation:"))
         (SLOTS  '(PHONE FAX EMAIL FISCAL-ID))
         (INCREMENT
          (LOOP FOR FNAME IN FIELDS
             FOR SLOT  IN SLOTS
             WHEN (SLOT-VALUE PERSON SLOT)
             MAXIMIZE (LENGTH (LOCALIZE *INVOICE-STRINGS*
                                        LANGUAGE FNAME)) INTO INCREMENT
             FINALLY (RETURN INCREMENT))))
    (LOOP FOR FNAME IN FIELDS
       FOR SLOT  IN SLOTS
       WHEN (SLOT-VALUE PERSON SLOT)
       DO (FORMAT STREAM "~VA~A ~A~%"
                  LEFT-MARGIN ""
                  (STRING-PAD (LOCALIZE *INVOICE-STRINGS* LANGUAGE FNAME)
                              INCREMENT)
                  (SLOT-VALUE PERSON SLOT))))) ;;PRINT-PERSON-ADDRESS


(DEFUN SHOW-TVA (MONTANT-HT &KEY (STREAM T) 
                 (VAT-RATE 16/100 VAT-RATE-P) (IRPF NIL IRPF-P)
                 (LANGUAGE :ES) (ALT-LANGUAGE :ES))
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
  (LET* ((LINE-FORM " ~52A   ~17A~%")
         (DESC-LINE (MAKE-STRING 52 :INITIAL-ELEMENT (CHARACTER "-")))
         (PRIC-LINE (MAKE-STRING 17 :INITIAL-ELEMENT (CHARACTER "-")))
         (BASE-LAB-GAU "")
         (TVAT-LAB-GAU "")
         (IRPF-LAB-GAU "")
         (TOTA-LAB-GAU "")
         (BASE-LAB-DRO)
         (TVAT-LAB-DRO)
         (IRPF-LAB-DRO)
         (TOTA-LAB-DRO)
         (TAUX-TVA         NIL)
         (TAUX-TVA-PRESENT NIL)
         ;; empeze la actividad el 2000/07 entonces desde el 2003/07 es -18%.
         (TAUX-IRPF  (IF (DATE-AFTER (CALENDAR-CURRENT-DATE)
                                     (MAKE-DATE :YEAR 2003 :MONTH 6 :DAY 30))
                         -18/100 -9/100))
         (SHOW-IRPF        NIL)
         (FORCE-SHOW-IRPF  NIL)
         (MONTANT-TVA)
         (MONTANT-IRPF)
         (MONTANT-TTC)
         (LANG-PRI     NIL)
         (LANG-SEC     NIL))
    (WHEN IRPF-P
      (IF IRPF
          (SETF SHOW-IRPF T   FORCE-SHOW-IRPF T)
          (SETF SHOW-IRPF NIL FORCE-SHOW-IRPF T)))
    (SETF LANG-PRI LANGUAGE
          LANG-SEC ALT-LANGUAGE)
    (SETF TAUX-TVA VAT-RATE)
    (IF VAT-RATE-P
        (SETF TAUX-TVA-PRESENT T)
        (SETF TAUX-TVA 0/100
              TAUX-TVA-PRESENT NIL))
    (WHEN (EQUAL LANG-PRI LANG-SEC)
      (SETF LANG-SEC NIL))
    (IF (AND (NULL LANG-SEC)
             (NOT TAUX-TVA-PRESENT)
             (STRING-EQUAL  LANG-PRI :ES))
        (SETF TAUX-TVA 16/100))
    (UNLESS FORCE-SHOW-IRPF
      (SETF SHOW-IRPF (AND (EQ (FIND-CURRENCY :EUR)
                               (CURRENCY MONTANT-HT))
                           (STRING-EQUAL LANG-PRI :ES)
                           (NULL LANG-SEC)
                           (= TAUX-TVA 16/100)
                           ;; (equal (fiscal-id *invoice-set*)
                           ;;        (issuer-fiscal-id self))
                           )))
    (SETF MONTANT-TVA  (* MONTANT-HT TAUX-TVA))
    (SETF MONTANT-IRPF (IF SHOW-IRPF (* MONTANT-HT TAUX-IRPF) 
                           (AMOUNT-ZERO (CURRENCY MONTANT-HT))))
    (SETF MONTANT-TTC  (+ MONTANT-HT MONTANT-TVA MONTANT-IRPF))
    (SETF BASE-LAB-DRO
          (FORMAT NIL "~16@A :"
                  (LOCALIZE *INVOICE-STRINGS* LANG-PRI "Total")))
    (SETF TVAT-LAB-DRO
          (FORMAT NIL "~16@A :" 
                  (FORMAT NIL (LOCALIZE *INVOICE-STRINGS* LANG-PRI "VAT  ~5,1F %")
                          (* 100 TAUX-TVA))))
    (SETF IRPF-LAB-DRO
          (FORMAT NIL "~16@A :"
                  (FORMAT NIL (LOCALIZE *INVOICE-STRINGS* LANG-PRI "IRPF ~5,1F %")
                          (* 100 TAUX-IRPF))))
    (SETF TOTA-LAB-DRO
          (FORMAT NIL "~16@A :"
                  (LOCALIZE *INVOICE-STRINGS* LANG-PRI "Total VAT Incl.")))
    (WHEN LANG-SEC
      (SETF BASE-LAB-GAU
            (FORMAT NIL "(~16@A) "
                    (LOCALIZE *INVOICE-STRINGS* LANG-SEC "Total")))
      (SETF TVAT-LAB-GAU
            (FORMAT NIL "(~16@A) "
                    (FORMAT NIL (LOCALIZE *INVOICE-STRINGS* LANG-SEC "VAT  ~5,1F %")
                            (* 100 TAUX-TVA))))
      (SETF IRPF-LAB-GAU
            (FORMAT NIL "(~16@A) "
                    (FORMAT NIL (LOCALIZE *INVOICE-STRINGS* LANG-SEC "IRPF ~5,1F %")
                            (* 100 TAUX-IRPF))))
      (SETF TOTA-LAB-GAU
            (FORMAT NIL "(~16@A) "
                    (LOCALIZE *INVOICE-STRINGS* LANG-SEC "Total VAT Incl."))))
    (FORMAT STREAM "~%")
    (FORMAT STREAM LINE-FORM DESC-LINE PRIC-LINE)
    (FORMAT STREAM LINE-FORM
            (CONCATENATE 'STRING BASE-LAB-GAU BASE-LAB-DRO)
            (FORMAT NIL " ~16@A" MONTANT-HT))
    (FORMAT STREAM LINE-FORM
            (CONCATENATE 'STRING TVAT-LAB-GAU TVAT-LAB-DRO)
            (FORMAT NIL "+~16@A" MONTANT-TVA))
    (WHEN SHOW-IRPF
      (FORMAT STREAM LINE-FORM
              (CONCATENATE 'STRING IRPF-LAB-GAU IRPF-LAB-DRO)
              (FORMAT NIL "-~16@A" (- MONTANT-IRPF))))
    (FORMAT STREAM LINE-FORM
            (CONCATENATE 'STRING TOTA-LAB-GAU TOTA-LAB-DRO)
            (FORMAT NIL "=~16@A" MONTANT-TTC))
    (FORMAT STREAM LINE-FORM DESC-LINE PRIC-LINE))) ;;SHOW-TVA



(DEFUN CLEAN-TITLE-FOR-FILE-NAME (TITLE-STRING)
  "
RETURN: A string containing the first word of title-string as plain ASCII.
DO:     Remove accents from the returned word.
"
  ;;(STRING-REMOVE-ACCENTS
  (STRING-DOWNCASE
   (SUBSEQ TITLE-STRING 0
           (POSITION-IF-NOT (FUNCTION ALPHANUMERICP) 
                            TITLE-STRING)))) ;;CLEAN-TITLE-FOR-FILE-NAME


(DEFMETHOD GENERATE ((SELF INVOICE) &KEY (STREAM T) (VERBOSE NIL)
                     (LANGUAGE :ES LANGUAGE-P))
  "
DO:      Generate this invoice into a file in the directory 
         *INVOICE-DIRECTORY-PATH*.
RETURN:  The path to the file generated.
"
  (LET* ((PAYER  (GET-PERSON-WITH-FISCAL-ID *INVOICE-SET* 
                                            (PAYER-FISCAL-ID SELF)))
         (ISSUER (GET-PERSON-WITH-FISCAL-ID *INVOICE-SET*
                                            (ISSUER-FISCAL-ID SELF))))
    (WHEN VERBOSE
      (FORMAT *TRACE-OUTPUT* "Generating ~A ~A ~A ~A~%"
              (class-name (class-of self)) (date self)
              issuer (invoice-number self)))
    (UNLESS LANGUAGE-P
      (SETF LANGUAGE  (OR (LANGUAGE PAYER) :ES)))
    (PRINT-PERSON-ADDRESS "" 1 ISSUER :LANGUAGE LANGUAGE :STREAM STREAM)
    (FORMAT STREAM " ~%")
    (LET* ((TITLE (LOCALIZE *INVOICE-STRINGS* LANGUAGE "INVOICE"))
           (WIDTH (+ 8 (LENGTH TITLE)))
           (TITLE-B (CONCATENATE 'STRING "|" 
                                 (STRING-PAD TITLE WIDTH
                                             :JUSTIFICATION :CENTER) "|"))
           (LINE-B  (CONCATENATE
                        'STRING "+" 
                        (MAKE-STRING WIDTH 
                                     :INITIAL-ELEMENT (CHARACTER "-")) "+")))
      (FORMAT STREAM " ~A~%" (STRING-PAD LINE-B  72 :JUSTIFICATION :CENTER))
      (FORMAT STREAM " ~A~%" (STRING-PAD TITLE-B 72 :JUSTIFICATION :CENTER))
      (FORMAT STREAM " ~A~%" (STRING-PAD LINE-B  72 :JUSTIFICATION :CENTER)))
    (FORMAT STREAM " ~%")
    (LET ((INCREMENT (LONGEST-LOCALIZED-LENGTH
                      *INVOICE-STRINGS* LANGUAGE
                      '("Date:" "Invoice no.:" "Billing address:"))))
      (FORMAT STREAM " ~A ~A~%"
              (STRING-PAD (LOCALIZE *INVOICE-STRINGS* LANGUAGE
                                    "Date:") INCREMENT)
              (DATE-FORMAT (DATE-TO-UNIVERSAL-TIME (DATE SELF))
                           :LANGUAGE LANGUAGE))
      (FORMAT STREAM " ~%")
      (FORMAT STREAM " ~A ~A~%"
              (STRING-PAD (LOCALIZE *INVOICE-STRINGS* LANGUAGE
                                    "Invoice no.:") INCREMENT)
              (INVOICE-NUMBER SELF))
      (FORMAT STREAM " ~%")
      (PRINT-PERSON-ADDRESS
       (CONCATENATE 'STRING " " 
                    (LOCALIZE *INVOICE-STRINGS* LANGUAGE "Billing address:"))
       (+ 2 INCREMENT) PAYER :LANGUAGE LANGUAGE)
      (FORMAT STREAM " ~%"))
    (LET ((LINE-FORM " ~52@A   ~17@A~%")
          (DESC-LINE (MAKE-STRING 52 :INITIAL-ELEMENT (CHARACTER "-")))
          (PRIC-LINE (MAKE-STRING 17 :INITIAL-ELEMENT (CHARACTER "-"))))
      (FORMAT STREAM LINE-FORM DESC-LINE PRIC-LINE)
      (FORMAT STREAM LINE-FORM
              (LOCALIZE *INVOICE-STRINGS* LANGUAGE "Description")
              (LOCALIZE *INVOICE-STRINGS* LANGUAGE "Price"))
      (FORMAT STREAM LINE-FORM DESC-LINE PRIC-LINE)
      (DOLIST (INVO-LINE  (LINES SELF))
        (LET* ((DESC (SPLIT-LINES (DESCRIPTION INVO-LINE)))
               (LAST-LENGTH (LENGTH (CAR (LAST DESC)))))
          (FORMAT STREAM "~{~% ~A~}" DESC)
          (IF (<= LAST-LENGTH 55)
              (FORMAT STREAM "~VA ~16@A" 
                      (- 55 LAST-LENGTH) "" (AMOUNT-HT INVO-LINE))
              (FORMAT STREAM "~% ~52@A   ~16@A"  "" (AMOUNT-HT INVO-LINE)))))
      (FORMAT STREAM " ~%")) ;;let
    (SHOW-TVA (TOTAL-HT SELF) :LANGUAGE LANGUAGE :ALT-LANGUAGE :ES)
    (FORMAT STREAM " ~%")
    (LET ((BANKREF (BANK-REFERENCE ISSUER)))
      (WHEN BANKREF
        (FORMAT STREAM "~{ ~A~%~}"
                (SPLIT-LINES
                 (FORMAT NIL
                   (LOCALIZE *INVOICE-STRINGS* LANGUAGE "PAYMENT-METHOD")
                   (DATE-FORMAT (+ (* 30 +SECONDS-IN-A-DAY+)
                                   (DATE-TO-UNIVERSAL-TIME (DATE SELF)))
                                :LANGUAGE LANGUAGE))))
        (FORMAT STREAM " ~%")
        (LET* ((FIELDS '("Payment Bank" "" "Branch Name"
                         "Account Number (IBAN)" "Beneficiary"
                         "SWIFT Code"))
               (SLOTS  '(BANK-NAME BANK-ADDRESS BRANCH-NAME
                         ACCOUNT-NUMBER BENEFICIARY-NAME SWIFT-CODE))
               (INCREMENT (LONGEST-LOCALIZED-LENGTH
                           *INVOICE-STRINGS* LANGUAGE FIELDS)))
          (LOOP FOR FNAME IN FIELDS
             FOR SLOT  IN SLOTS
             WHEN (SLOT-VALUE BANKREF SLOT)
             DO (FORMAT STREAM
                  " ~A~A : ~A~%"
                  (STRING-PAD "" 8)
                  (STRING-PAD (LOCALIZE *INVOICE-STRINGS* LANGUAGE FNAME)
                              INCREMENT)
                  (ALIGN-FOLLOWING-LINES (SLOT-VALUE BANKREF SLOT)
                                         (+ INCREMENT 10)))) )))
    (FORMAT STREAM " ~%")
    (FORMAT STREAM " ~%"))) ;;GENERATE


(DEFMETHOD WRITE-INVOICE-FILE ((SELF INVOICE) &KEY (LANGUAGE :EN LANGUAGE-P))
  (LET* ((PAYER  (GET-PERSON-WITH-FISCAL-ID *INVOICE-SET* 
                                            (PAYER-FISCAL-ID SELF)))
         (FILE-PATH
          (MAKE-PATHNAME
           :DIRECTORY *INVOICE-DIRECTORY-PATH*
           :NAME (FORMAT NIL  "~A-~A-~A"
                         (DELETE (CHARACTER "/") (INVOICE-NUMBER SELF))
                         (CLEAN-TITLE-FOR-FILE-NAME (OBJECT-ID PAYER))
                         (CLEAN-TITLE-FOR-FILE-NAME (TITLE SELF)))
           :TYPE "txt")))
    (UNLESS LANGUAGE-P
      (SETF LANGUAGE  (OR (LANGUAGE PAYER) :ES)))
    (WITH-OPEN-FILE (STREAM FILE-PATH :DIRECTION :OUTPUT
                            :IF-DOES-NOT-EXIST :CREATE
                            :IF-EXISTS :SUPERSEDE)
      (GENERATE SELF :STREAM STREAM :LANGUAGE LANGUAGE))
    FILE-PATH)) ;;WRITE-INVOICE-FILE


;;;---------------------------------------------------------------------
;;; INVOICE-SET
;;;---------------------------------------------------------------------


(DEFMETHOD GET-PERSON-WITH-FISCAL-ID ((SELF INVOICE-SET) FISCAL-ID)
  (CAR (MEMBER FISCAL-ID (PERSONS SELF) 
               :TEST (FUNCTION STRING-EQUAL) :KEY (FUNCTION FISCAL-ID))))


(DEFMETHOD ADD-PERSON ((SELF INVOICE-SET) (PERSON FISCAL-PERSON)
                       &OPTIONAL FISC)
  (LET ((OLD (GET-PERSON-WITH-FISCAL-ID SELF (FISCAL-ID PERSON))))
    (IF OLD
        (SUBSTITUTE PERSON OLD (PERSONS SELF) :COUNT 1)
        (PUSH PERSON (PERSONS SELF))))
  (WHEN (AND FISC  (NOT (MEMBER (FISCAL-ID PERSON) (FISC-FISCAL-IDS SELF)
                                :TEST (FUNCTION STRING-EQUAL))))
    (PUSH (FISCAL-ID PERSON) (FISC-FISCAL-IDS SELF)))) ;;ADD-PERSON


(DEFMETHOD GET-INVOICE-WITH-ISSUER-AND-NUMBER ((SELF INVOICE-SET)
                                               ISSUER-FISCAL-ID INVOICE-NUMBER)
  (FIND-IF (LAMBDA (I)  (AND (EQUAL ISSUER-FISCAL-ID (ISSUER-FISCAL-ID  I))
                             (EQUAL INVOICE-NUMBER   (INVOICE-NUMBER    I)))) 
           (INVOICES SELF))) ;;GET-INVOICE-WITH-ISSUER-AND-NUMBER


(DEFMETHOD ADD-INVOICE ((SELF INVOICE-SET) (INVOICE INVOICE))
  (LET ((OLD (GET-INVOICE-WITH-ISSUER-AND-NUMBER
              SELF
              (ISSUER-FISCAL-ID INVOICE)
              (INVOICE-NUMBER   INVOICE))))
    (IF OLD
        (SUBSTITUTE INVOICE OLD (INVOICES SELF) :COUNT 1)
        (PUSH INVOICE (INVOICES SELF))))) ;;ADD-INVOICE


;;;---------------------------------------------------------------------
;;; MOVEMENT
;;;---------------------------------------------------------------------


(DEFCLASS MOVEMENT (PJB-OBJECT)
  ((DATE
    :INITFORM NIL
    :INITARG  :DATE
    :ACCESSOR DATE
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION
    "'YYYY-MM-DD' Date of the movement.")
   (AMOUNT-TTC
    :INITFORM (AMOUNT-ZERO *DEFAULT-CURRENCY*)
    :INITARG  :AMOUNT-TTC
    :ACCESSOR AMOUNT-TTC
    :TYPE     AMOUNT
    :DOCUMENTATION
    "(number) The amount paid (including taxes).")
   (AMOUNT-VAT
    :INITFORM 0/100
    :INITARG  :AMOUNT-VAT
    :ACCESSOR AMOUNT-VAT
    :TYPE     RATIO
    :DOCUMENTATION
    "(number) The VAT of the movement.")
   (DESCRIPTION
    :INITFORM ""
    :INITARG  :DESCRIPTION
    :ACCESSOR DESCRIPTION
    :TYPE     STRING
    :DOCUMENTATION
    "(string) A description of the movement.")
   (KIND
    :INITFORM NIL
    :INITARG  :KIND
    :ACCESSOR KIND
    :TYPE     (OR NULL SYMBOL)
    :DOCUMENTATION
    "(symbol) A kind of movement, for tax reporting purpose.
PRESTACION-NACIONAL, PRESTACION-INTRACOMUNITARIA, IMPUESTO, INVERSION,
GASTO-CORRIENTE, ADQUISICION-INTRACOMUNITARIA")
   (INVOICE-FISCAL-ID
    :INITFORM NIL
    :INITARG  :INVOICE-FISCAL-ID
    :ACCESSOR INVOICE-FISCAL-ID
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION
    "The fiscal id of the common issuer of the following invoices
related to this movement.")
   (INVOICE-NUMBERS
    :INITFORM NIL
    :INITARG  :INVOICE-NUMBERS
    :ACCESSOR INVOICE-NUMBERS
    :TYPE     LIST
    :DOCUMENTATION
    "(list of string) The list of invoice numbers related to this entry.
Note that one journal entry may relate to several invoices (grouped payment)
and one invoice may relate to several movements (part payments, or
corrections.")
   )
  (:DOCUMENTATION
   "An entry in the journal.
A movement with a positive amount is a credit,
while a movement with a negative amount is a debit.")) ;;MOVEMENT


(DEFMETHOD INITIALIZE-INSTANCE ((SELF MOVEMENT) 
                                &KEY AMOUNT-TTC AMOUNT-HT INVOICE-FISCAL-ID
                                &ALLOW-OTHER-KEYS)
  "
DOES:    Checks that the values for the fields are within limits.
"
  (WHEN *MAX-MOVEMENT-AMOUNT*
    (WHEN AMOUNT-TTC
      (WHEN (< *MAX-MOVEMENT-AMOUNT* (ABS AMOUNT-TTC))
        (ERROR "amount-ttc too big ~S." AMOUNT-TTC)))
    (WHEN AMOUNT-HT
      (IF (< *MAX-MOVEMENT-AMOUNT* (ABS  AMOUNT-HT))
          (ERROR "amount-ht too big ~S." AMOUNT-HT))))
  (WHEN INVOICE-FISCAL-ID
    (UNLESS (GET-PERSON-WITH-FISCAL-ID *INVOICE-SET* INVOICE-FISCAL-ID)
      (WARN "Unknown person (fiscal-id=~S)." INVOICE-FISCAL-ID)))
  (CALL-NEXT-METHOD)) ;;INITIALIZE-INSTANCE


(DEFMETHOD AMOUNT-HT ((SELF MOVEMENT)) (- (AMOUNT-TTC SELF) (AMOUNT-VAT SELF)))


(DEFPARAMETER *MOVEMENT-KINDS*
  '(:PRESTACION-NACIONAL  
    :PRESTACION-INTRACOMUNITARIA
    :IMPUESTO
    :INVERSION
    :GASTO-CORRIENTE
    :ADQUISICION-INTRACOMUNITARIA)) ;;*MOVEMENT-KINDS*


(DEFUN MAKE-MOVEMENT-FROM-INVOICE (INVOICE)
  "
RETURN: A new instance of MOVEMENT filled with data from invoice.
"
  (LET (KIND AMOUNT-SIGN FISCAL-ID)
    (COND
      ((MEMBER (ISSUER-FISCAL-ID INVOICE) (FISC-FISCAL-IDS *INVOICE-SET*)
               :TEST (FUNCTION STRING-EQUAL))
       (SETF KIND :IMPUESTO
             AMOUNT-SIGN -1
             FISCAL-ID (ISSUER-FISCAL-ID INVOICE)))
      ((EQUALP (ISSUER-FISCAL-ID INVOICE) (FISCAL-ID *INVOICE-SET*))
       (SETF KIND (IF (ZEROP (TOTAL-VAT INVOICE))
                      :PRESTACION-INTRACOMUNITARIA
                      :PRESTACION-NACIONAL)
             AMOUNT-SIGN 1
             FISCAL-ID (PAYER-FISCAL-ID INVOICE)))
      (T
       (SETF KIND :GASTO-CORRIENTE
             AMOUNT-SIGN -1
             FISCAL-ID (ISSUER-FISCAL-ID INVOICE))))
    (MAKE-INSTANCE 'MOVEMENT
      :DATE                (DATE      INVOICE)
      :AMOUNT-TTC       (* (TOTAL-TTC INVOICE) AMOUNT-SIGN)
      :AMOUNT-VAT       (* (TOTAL-VAT INVOICE) AMOUNT-SIGN)
      :DESCRIPTION         (TITLE     INVOICE)
      :KIND                KIND
      :INVOICE-FISCAL-ID   FISCAL-ID
      :INVOICE-NUMBERS
      (LIST (INVOICE-NUMBER INVOICE))))) ;;MAKE-MOVEMENT-FROM-INVOICE


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

(DEFUN MAKE-MOVEMENT (DATE AMOUNT-TTC VAT-RATE NIF FAC-L DESCRIPTION KIND)
  "
RETURN: A new instance of MOVEMENT filled with the given data
."
  (macrolet ((err (ctrl &rest args)
               `(ERROR 'movement-error
                       :format-control ,ctrl
                       :format-arguments (list ,@args)
                       :date date :amount-ttc amount-ttc :vat-rate vat-rate
                       :nif nif :fac-l fac-l
                       :description description :kind kind)))
    (UNLESS (MEMBER KIND  *MOVEMENT-KINDS*)
      (err "Invalid kind ~A." kind))
    (WHEN (< VAT-RATE -2)
      (ERR "VAT-RATE must always be >= 0."))
    (UNLESS (EQ KIND :PRESTACION-INTRACOMUNITARIA)
      (WHEN (EQ KIND :PRESTACION-NACIONAL)
        (UNLESS (NEGATIVEP AMOUNT-TTC)
          (ERR "AMOUNT-TTC must be > 0 for an entry of kind ~A." KIND))))
    (COND
      ((EQ KIND :PRESTACION-NACIONAL)
       (WHEN (<= VAT-RATE 0)
         (ERR "VAT-RATE must be > 0.00 for an entry of kind ~A." KIND)))
      ((MEMBER KIND '(:PRESTACION-INTRACOMUNITARIA :IMPUESTO))
       (IF (< 0 VAT-RATE)
           (ERR "VAT-RATE must be = 0 for an entry of kind ~A." KIND))))
    (MAKE-INSTANCE 'MOVEMENT
      :DATE              DATE
      :AMOUNT-TTC        AMOUNT-TTC
      :AMOUNT-VAT         (/ (* AMOUNT-TTC VAT-RATE) (+ 1 VAT-RATE))
      :DESCRIPTION       DESCRIPTION
      :KIND              KIND
      :INVOICE-FISCAL-ID NIF
      :INVOICE-NUMBERS   FAC-L)))


(DEFMETHOD IS-CREDIT   ((SELF MOVEMENT))
  "
RETURN: Whether the SELF is a credit movement.
"
  (POSITIVEP (AMOUNT-HT SELF))) ;;IS-CREDIT


(DEFMETHOD VAT-RATE    ((SELF MOVEMENT))
  "
RETURN: A computed VAT rate for this movement.
"
  (if (zerop (AMOUNT-MAGNITUDE (AMOUNT-HT  SELF)))
      0
      (/ (ROUND (* 100 (/ (AMOUNT-MAGNITUDE (AMOUNT-VAT SELF))
                          (AMOUNT-MAGNITUDE (AMOUNT-HT  SELF))))) 100)))


(DEFMETHOD CREDIT-HT   ((SELF MOVEMENT))
  (IF (IS-CREDIT SELF)
      (AMOUNT-HT SELF)
      (AMOUNT-ZERO (CURRENCY (AMOUNT-HT SELF)))))


(DEFMETHOD CREDIT-VAT  ((SELF MOVEMENT))
  (IF (IS-CREDIT SELF)
      (AMOUNT-VAT SELF)
      (AMOUNT-ZERO (CURRENCY (AMOUNT-VAT SELF)))))


(DEFMETHOD DEBIT-HT    ((SELF MOVEMENT))
  (IF (IS-CREDIT SELF)
      (AMOUNT-ZERO (CURRENCY (AMOUNT-HT SELF)))
      (- (AMOUNT-HT SELF))))


(DEFMETHOD DEBIT-VAT   ((SELF MOVEMENT))
  (IF (IS-CREDIT SELF)
      (AMOUNT-ZERO (CURRENCY (AMOUNT-VAT SELF)))
      (- (AMOUNT-VAT SELF))))


(DEFMETHOD DEBIT-VAT-INVERSION  ((SELF MOVEMENT))
  (IF (EQ :INVERSION (KIND SELF))
      (DEBIT-VAT SELF)
      (AMOUNT-ZERO (CURRENCY (DEBIT-VAT SELF)))))


(DEFMETHOD DEBIT-VAT-CORRIENTE  ((SELF MOVEMENT))
  (IF (EQ :GASTO-CORRIENTE (KIND SELF))
      (DEBIT-VAT SELF)
      (AMOUNT-ZERO (CURRENCY (DEBIT-VAT SELF)))))


(DEFMETHOD INVOICES ((SELF MOVEMENT))
  "
RETURN: A list of INVOICE instances related to this entry.
"
  (LET ((FISCAL-ID  (IF (AND (IS-CREDIT SELF)
                             (NOT (EQUAL (KIND SELF) :GASTO-CORRIENTE)))
                        (FISCAL-ID *INVOICE-SET*)
                        (INVOICE-FISCAL-ID SELF))))
    (REMOVE NIL (MAPCAR
                 (LAMBDA (NUMBER)
                   (GET-INVOICE-WITH-ISSUER-AND-NUMBER 
                    *INVOICE-SET* FISCAL-ID NUMBER))
                 (INVOICE-NUMBERS SELF))))) ;;INVOICES


(DEFUN TRIM-JUSTIFY-AND-SPLIT-TEXT (TEXT WIDTH)
  "
DOES:    Trim spaces on each line. justify each paragraph.
RETURN:  The justified text.
"
  (LET ((LINES (SPLIT-LINES TEXT))
        (PARAGRAPHS        '())
        (CURRENT-PARAGRAPH '()))
    (DOLIST (LINE LINES                 ; group the paragraphs.
             (WHEN CURRENT-PARAGRAPH
               (PUSH (APPLY (FUNCTION CONCATENATE) 'STRING 
                            (NREVERSE CURRENT-PARAGRAPH)) PARAGRAPHS)))
      (IF (STRING= LINE "")
          (WHEN CURRENT-PARAGRAPH
            (PUSH (APPLY (FUNCTION CONCATENATE) 'STRING 
                         (NREVERSE CURRENT-PARAGRAPH)) PARAGRAPHS))
          (PROGN (PUSH " " CURRENT-PARAGRAPH)
                 (PUSH LINE CURRENT-PARAGRAPH))))
    (OR (MAPCAN (LAMBDA (PARA) (SPLIT-LINES (STRING-JUSTIFY-LEFT PARA WIDTH 0)))
                (NREVERSE PARAGRAPHS))
        (LIST " ")))) ;;TRIM-JUSTIFY-AND-SPLIT-TEXT


(DEFMETHOD GENERATE ((SELF MOVEMENT) &KEY (STREAM T) (VERBOSE NIL)
                     (language :en))
  "
DOES:    format and insert this entry.
"
  (LET ((ID  (INVOICE-FISCAL-ID SELF))
        PERSON
        (NAME "")  NAME-L
        (MOVEMENT-SIGN (IF (IS-CREDIT SELF) 1 -1))
        (INVOICES-SIGN 1))
    (WHEN VERBOSE
      (FORMAT *TRACE-OUTPUT* "Generating ~A ~A ~A~%"
              (class-name (class-of self)) (date self) (amount-ttc self)))
    ;; first line:
    (IF ID
        (PROGN
          (SETF PERSON (GET-PERSON-WITH-FISCAL-ID *INVOICE-SET* ID))
          (WHEN PERSON (SETF NAME (NAME PERSON))))
        (SETF ID ""))
    (SETF NAME-L (TRIM-JUSTIFY-AND-SPLIT-TEXT NAME 38))
    ;; === SEN FECHA IDENTIFICATION NOMBRE =============================
    (FORMAT STREAM "~3A ~10A ~23@A ~A~%" 
            (COND  ((IS-CREDIT SELF)           "ING")
                   ((EQ :IMPUESTO (KIND SELF)) "IMP")
                   (T                          "GAS"))
            (DATE SELF) ID (CAR NAME-L))
    ;; === OPTIONAL NEXT LINES =========================================
    ;;                          NOMBRE (continuación)
    (DOLIST (NAME (CDR NAME-L)) (FORMAT STREAM "~38A ~A~%" "" NAME))
    ;; === INVOICE LINES ===============================================
    ;;     IMPORTE  IVA%   +IVA   TOTAL  NUMERO FACTURA o DESCRIPCION
    ;;                                   INVOICE TITLE
    (LET* ((ZERO (AMOUNT-ZERO (CURRENCY (AMOUNT-HT SELF))))
           (T-HT  ZERO)
           (T-VAT ZERO)
           (T-TTC ZERO))
      (LET ((T-TTC ZERO))               ; Let's find the invoices-sign
        (DOLIST (INVOICE (INVOICES SELF))
          (SETF T-TTC (+ T-TTC (TOTAL-TTC INVOICE))))
        (SETF INVOICES-SIGN (* MOVEMENT-SIGN (IF (NEGATIVEP T-TTC) -1 1))))
      (DOLIST (INVOICE (INVOICES SELF)) ; Let's print the invoices
        (LET* ((TITLE-L (TRIM-JUSTIFY-AND-SPLIT-TEXT (TITLE INVOICE) 38))
               (I-HT    (TOTAL-HT  INVOICE))
               (I-VAT   (TOTAL-VAT INVOICE))
               (I-TTC   (TOTAL-TTC INVOICE)))
          (SETF T-HT  (+ T-HT   I-HT)
                T-VAT (+ T-VAT  I-VAT)
                T-TTC (+ T-TTC  I-TTC))
          (FORMAT STREAM "    ~2,,9$ ~4,1F% ~2,,8$ ~2,,9$ ~A~%"
                  (AMOUNT-MAGNITUDE (* I-HT INVOICES-SIGN))
                  (* 100 (VAT-RATE INVOICE))
                  (AMOUNT-MAGNITUDE (* I-VAT INVOICES-SIGN))
                  (AMOUNT-MAGNITUDE (* I-TTC INVOICES-SIGN))
                  (INVOICE-NUMBER INVOICE))
          (DOLIST (TITLE TITLE-L)
            (FORMAT STREAM  "~38A ~A~%" ""  TITLE))))
      ;; === DIFFERNCE LINE ============================================
      ;;    AMOUNT-HT AMOUNT-VAT AMOUNT-TTC Diferencia
      (UNLESS (AND (ZEROP T-HT) (ZEROP T-VAT) (ZEROP T-TTC))
        ;; Invoices, let's see if there's a difference.
        (handler-case
            (unless (= (AMOUNT-TTC SELF) (* T-TTC INVOICES-SIGN))
              (LET* ((DIFF-HT  (- (AMOUNT-HT  SELF) (* T-HT  INVOICES-SIGN)))
                     (DIFF-VAT (- (AMOUNT-VAT SELF) (* T-VAT INVOICES-SIGN)))
                     (DIFF-TTC (- (AMOUNT-TTC SELF) (* T-TTC INVOICES-SIGN))))
                (FORMAT STREAM "    ~2,,9$ ~5A ~2,,8$ ~2,,9$ ~A~%"
                        (AMOUNT-MAGNITUDE DIFF-HT) "" 
                        (AMOUNT-MAGNITUDE DIFF-VAT)
                        (AMOUNT-MAGNITUDE DIFF-TTC) "Diferencia")))
          (multi-currency-error
              ()
            (LEt ((cambio (if (zerop t-ttc)
                              0
                              (/ (AMOUNT-TTC SELF) (* T-TTC INVOICES-SIGN)))))
              (FORMAT STREAM "    ~9A ~5A ~8A ~2,,9$ ~A ~A -> ~A~%"
                      "" "" "" cambio
                      (LOCALIZE *INVOICE-STRINGS* LANGUAGE "Currency change")
                      (CURRENCY-ALPHABETIC-CODE (currency t-ttc))
                      (CURRENCY-ALPHABETIC-CODE (currency (amount-ttc self)))))))
        (FORMAT STREAM "    --------- ----- -------- ---------~%")))
    ;; === TOTAL ENTRY LINES ===========================================
    (LET* ((DESC-L (TRIM-JUSTIFY-AND-SPLIT-TEXT (DESCRIPTION SELF) 38))
           (DESC   (POP DESC-L)))
      (FORMAT STREAM "    ~2,,9$ ~4,1F% ~2,,8$ ~2,,9$ ~A~%"
              (AMOUNT-MAGNITUDE (AMOUNT-HT  SELF))
              (* 100 (VAT-RATE SELF)) 
              (AMOUNT-MAGNITUDE (AMOUNT-VAT SELF))
              (AMOUNT-MAGNITUDE (AMOUNT-TTC SELF)) DESC)
      (DOLIST (DESC DESC-L)
        (FORMAT STREAM "~38A ~A~%" "" DESC))))
  (FORMAT STREAM "--- ---------- ----------------------- ~
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


(DEFCLASS JOURNAL ()
  ((SORTED :INITFORM NIL
           :ACCESSOR SORTED
           :TYPE BOOLEAN
           :DOCUMENTATION "Indicates whether entries are sorted.")
   (ENTRIES :INITFORM '()
            :INITARG :ENTRIES
            :ACCESSOR ENTRIES
            :TYPE LIST)
   (YEAR :INITFORM (DATE-YEAR (CALENDAR-CURRENT-DATE))
         :INITARG :YEAR
         :ACCESSOR YEAR
         :TYPE (INTEGER 1998 2070))
   (TRIMESTRE :INITFORM (1+ (TRUNCATE 
                             (1- (DATE-MONTH (CALENDAR-CURRENT-DATE))) 3))
              :INITARG :TRIMESTRE
              :ACCESSOR TRIMESTRE
              :TYPE (MEMBER 1 2 3 4)))) ;;JOURNAL


(DEFMETHOD RESET ((SELF JOURNAL))
  "
POST: (null (entries self))
"
  (SETF (ENTRIES SELF) '()
        (SORTED  SELF) NIL)) ;;RESET


(DEFMETHOD ADD-ENTRY ((SELF JOURNAL) (ENTRY MOVEMENT))
  "
DOES:   Add the ENTRY into the journal.
POST:   (AND (MEMBER ENTRY (ENTRIES SELF)) (NOT (SORTED SELF)))
"
  (PUSH ENTRY (ENTRIES SELF))
  (SETF (SORTED SELF) NIL)) ;;ADD-ENTRY


(DEFMETHOD ENSURE-SORTED ((SELF JOURNAL))
  "
POST:   (sorted *journal*)
"
  (UNLESS (SORTED SELF)
    (SETF (ENTRIES SELF) (SORT (ENTRIES SELF) 
                               (LAMBDA (A B) (DATE-AFTER (DATE B) (DATE A))))
          (SORTED SELF) T))) ;;ENSURE-SORTED


(DEFMETHOD EXTRACT ((SELF JOURNAL) YEAR TRIMESTRE)
  "
RETURN: The entries of the journal corresponding
        to the given YEAR and TRIMESTRE.
"
  (ENSURE-SORTED SELF)
  (REMOVE-IF 
   (LAMBDA (ENTRY) (NOT (DATE-IN-YEAR-TRIMESTRE (DATE ENTRY) YEAR TRIMESTRE)))
   (ENTRIES SELF))) ;;EXTRACT


(DEFUN JOURNAL-TOTALS-OF-ENTRIES (ENTRIES)
  "PRIVATE
RETURN: a list containing the totals:
        credit-ht credit-vat debit-ht debit-vat-inversion debit-vat-corriente.
"
  (IF (NULL ENTRIES)
      (make-list 5 :initial-element (AMOUNT-ZERO *DEFAULT-CURRENCY*))
      (LET* ((ZERO (AMOUNT-ZERO (CURRENCY (CREDIT-HT (FIRST ENTRIES)))))
             (CREDIT-HT   ZERO)
             (CREDIT-VAT  ZERO)
             (DEBIT-HT    ZERO)
             (DEBIT-VAT-C ZERO)
             (DEBIT-VAT-I ZERO))
        (MAPCAR
         (LAMBDA (ENTRY)
           (SETF CREDIT-HT   (+ CREDIT-HT   (CREDIT-HT  ENTRY))
                 CREDIT-VAT  (+ CREDIT-VAT  (CREDIT-VAT ENTRY))
                 DEBIT-HT    (+ DEBIT-HT    (DEBIT-HT   ENTRY))
                 DEBIT-VAT-C (+ DEBIT-VAT-C (DEBIT-VAT-CORRIENTE  ENTRY))
                 DEBIT-VAT-I (+ DEBIT-VAT-I (DEBIT-VAT-INVERSION  ENTRY))))
         ENTRIES)
        (LIST CREDIT-HT CREDIT-VAT DEBIT-HT DEBIT-VAT-I DEBIT-VAT-C))))


(DEFUN JOURNAL-SPLIT-AND-JUSTIFY-DESCRIPTION (DESCRIPTION WIDTH)
  "PRIVATE"
  (OR (MAPCAN (LAMBDA (LINE) (SPLIT-LINES(STRING-JUSTIFY-LEFT LINE WIDTH 0)))
              (SPLIT-LINES DESCRIPTION)) '(" ")))


(DEFUN JOURNAL-PRINT-HEADER (YEAR TRIMESTRE &KEY (STREAM T))
  "PRIVATE"
  (FORMAT STREAM "----------------------------------------~
                  ---------------------------------------~%")
  (FORMAT STREAM  "~36D - TRIMESTRE ~D~%" YEAR TRIMESTRE)
  (FORMAT STREAM "--- ---------- ----------------------- ~
                  ----------------------------------------~%")
  (FORMAT STREAM "SEN FECHA      IDENTIFICACION          NOMBRE~%")
  (FORMAT STREAM "TID   IMPORTE  IVA%     +IVA     TOTAL ~
                  NUMERO FACTURA / DESCRIPTION~%")
  (FORMAT STREAM "--- --------- ----- -------- --------- ~
                  ---------------------------------------~%")
  (values))


(DEFUN JOURNAL-PRINT-TRAILER (&KEY (STREAM T))
  "PRIVATE"
  (FORMAT STREAM "      IMPORTE            IVA     TOTAL~%")
  (FORMAT STREAM "--- --------- ----- -------- --------- ~
                  ---------------------------------------~%")
  (values))


(DEFUN JOURNAL-PRINT-TOTALS (TOTALS &KEY (STREAM T))
  "PRIVATE"
  (LET ((CREDIT-HT  (NTH 0 TOTALS))
        (CREDIT-VAT (NTH 1 TOTALS))
        (DEBIT-HT   (NTH 2 TOTALS))
        (DEBIT-VAT  (+ (NTH 3 TOTALS) (NTH 4 TOTALS))))
    (FORMAT STREAM  "    ~2,,9$ ~5A ~2,,8$ ~9A ~A~%"
            (AMOUNT-MAGNITUDE CREDIT-HT) "" 
            (AMOUNT-MAGNITUDE CREDIT-VAT) "" "Credito")
    (FORMAT STREAM  "    ~2,,9$ ~5A ~2,,8$ ~9A ~A~%"
            (AMOUNT-MAGNITUDE (- DEBIT-HT)) "" 
            (AMOUNT-MAGNITUDE (- DEBIT-VAT)) "" "Debido")
    (FORMAT STREAM "    ~2,,9$ ~5A ~2,,8$ ~9A ~A~%"
            (AMOUNT-MAGNITUDE (- CREDIT-HT  DEBIT-HT))  ""
            (AMOUNT-MAGNITUDE (- CREDIT-VAT DEBIT-VAT)) ""
            "Saldo")
    (values)))


(DEFUN KIND-TO-ORDER (KIND)
  (CASE KIND
    ((:PRESTACION-NACIONAL)          1)
    ((:PRESTACION-INTRACOMUNITARIA)  2)
    ((:IMPUESTO)                     3)
    ((:INVERSION)                    4)
    ((:GASTO-CORRIENTE)              5)
    ((:ADQUISICION-INTRACOMUNITARIA) 6)
    (OTHERWISE                       7)))


(DEFMETHOD GENERATE ((SELF JOURNAL) &KEY (STREAM T) (VERBOSE NIL)
                     (language :en))
  "
DOES:   Prints the formated entries of the journal onto the stream.
"
  (ENSURE-SORTED SELF)
  (LET* ((ENTRIES 
          (SORT (EXTRACT SELF (YEAR SELF) (TRIMESTRE SELF))
                (LAMBDA (A B)
                  (COND
                    ((= (KIND-TO-ORDER (KIND A)) (KIND-TO-ORDER (KIND B)))
                     (NOT (DATE-AFTER  (DATE A) (DATE B))))
                    (T (< (KIND-TO-ORDER (KIND A)) (KIND-TO-ORDER (KIND B))))))))
         (TOTALS  (JOURNAL-TOTALS-OF-ENTRIES ENTRIES)))
    (FORMAT STREAM "~2%")
    (JOURNAL-PRINT-HEADER (YEAR SELF) (TRIMESTRE SELF) :STREAM STREAM)
    (MAPCAR (LAMBDA (ENTRY) (GENERATE ENTRY :STREAM STREAM :VERBOSE VERBOSE
                                      :language language)) ENTRIES)
    (JOURNAL-PRINT-TRAILER       :STREAM STREAM)
    (JOURNAL-PRINT-TOTALS TOTALS :STREAM STREAM)
    (FORMAT STREAM "~2%")
    (values)))


;;;---------------------------------------------------------------------
;;; Reading Journal File.
;;;---------------------------------------------------------------------


(DEFMACRO PERSON (&REST ARGS)
  `(ADD-PERSON *INVOICE-SET* (MAKE-INSTANCE 'FISCAL-PERSON ,@ARGS)))


(DEFMACRO MAKE-BANK-REFERENCE (&REST ARGS)
  `(MAKE-INSTANCE 'BANK-REFERENCE ,@ARGS))


(DEFMACRO INVOICE (&REST ARGS)
  (DO ((ARGS ARGS)
       (ATTRIBUTES '())
       (LINES '())
       (VINST (GENSYM)))
      ((NULL ARGS) 
       `(LET ((,VINST (MAKE-INSTANCE 'INVOICE ,@(NREVERSE ATTRIBUTES))))
          ,@(MAPCAR
             (LAMBDA (LINE)
               `(ADD-LINE ,VINST (MAKE-INSTANCE 'INVOICE-LINE ,@LINE)))
             (NREVERSE LINES))
          (ADD-INVOICE *INVOICE-SET* ,VINST)))
    (COND
      ((KEYWORDP (CAR ARGS))
       (PUSH (POP ARGS) ATTRIBUTES)
       (PUSH (POP ARGS) ATTRIBUTES))
      ((ATOM (CAR ARGS)) (ERROR "Invalid invoice attribute ~S" (POP ARGS)))
      ((EQL 'LINE (CAAR ARGS))
       (PUSH (LIST* :OBJECT-ID (STRING (GENSYM "L")) (CDR (POP ARGS))) LINES))
      (T (ERROR "Invalid invoice attribute ~S" (POP ARGS)))))) ;;INVOICE


(DEFMACRO JOURNAL-ENTRY (DATE AMOUNT-TTC VAT-RATE NIF FAC DESCRIPTION KIND)
  "
DOES:   Add a new journal entry.
        AMOUNT-TTC is the total paid (including VAT) expressed in Euros.
        VAT-RATE is the V.A.T percentage.
"
  `(ADD-ENTRY *JOURNAL*
              (MAKE-MOVEMENT (DATE-FROM-STRING ',DATE) ',AMOUNT-TTC ',VAT-RATE ',NIF
                             (IF (LISTP ',FAC) ',FAC (LIST ',FAC)) 
                             ',DESCRIPTION ',KIND))) ;;JOURNAL-ENTRY


(DEFUN LOAD-JOURNAL (PATH &KEY (VERBOSE *LOAD-VERBOSE*) (PRINT *LOAD-PRINT*))
  (LET ((*READTABLE* *CURRENCY-READTABLE*)))
  (LOAD PATH :VERBOSE VERBOSE :PRINT PRINT))


;; (in-package :common-lisp-user)
;; (cd "/home/pascal/jobs/free-lance/accounting/")
;; (load  "invoice")


;;;; invoice.lisp                     --                     --          ;;;;
