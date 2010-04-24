;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               htrans.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;NOWEB:              t
;;;;DESCRIPTION
;;;;    
;;;;    A "transaction" manager for CGI.
;;;;
;;;;
;;;; html-client                  transac-manager                         cgi
;;;;   |                                 |                                 |
;;;;   |                                                                  
;;;;   |                                 /                                 
;;;;   |--------(initial-request)------->|                                 /
;;;;   |                                 |-----initialrequest(sessid)----->|
;;;;   |                                 |                                 |
;;;;   |                                 |<------reply(sessid,trid)--------|
;;;;   |<-------(html-form)--------------|                                 /
;;;;   |                                 /                                 
;;;;   |                                                                  
;;;;   |                                 /                                 
;;;;   |----------(action.get)---------->|                                 /
;;;;   |                                 |----request(sesid,trid,data)---->|
;;;;   |                                 |                                 |
;;;;   |                                 |<------reply(sessid,trid+1)------|
;;;;   |<-------(html-form)--------------|                                 /
;;;;   |                                 /                                
;;;;   |                                                                  
;;;;   |                                 |                                 |
;;;;   V                                 V                                 V
;;;;
;;;;
;;;;    In this implementation transac-manager and cgi are linked together in
;;;;    the CGI process and this CGI process lives from the HTML request to
;;;;    the response.
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-10 <PJB> Removed depdency on de.pmsf.md5.
;;;;    2003-09-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2005
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
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.HTML"
                            "COM.INFORMATIMAGO.COMMON-LISP.HQUERY"))
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:ADD-NICKNAME
   "COM.INFORMATIMAGO.COMMON-LISP.HTML" "HTML")
  (COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:ADD-NICKNAME
   "COM.INFORMATIMAGO.COMMON-LISP.HQUERY" "HQUERY"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTRANS"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING")
  (:EXPORT "GETCMDS" "GETARG" "SEND-TABLE" "SEND-REPLY" "PROCESS-TRANSACTION"
           "GENERATE-HTML-FOOTER" "GENERATE-HTML-HEADER" "REFUSE-SESSION"
           "REFUSE-REMOTE" "PROCESS-REQUEST" "BODY-ATTRIBUTES" "TITLE" "ACTION"
           "REFUSED-NETS" "ALLOWED-NETS" "ENVIRONMENT" "ARGUMENTS" "HPROGRAM")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING" "PREFIXP" "SPLIT-STRING")
  (:DOCUMENTATION
   "A ''TRANSACTION'' manager for CGI.
    
    Copyright Pascal J. Bourguignon 2003 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTRANS")


(defvar +crlf+ (format nil "~C~C" (code-char 13) (code-char 10)))

(DEFGENERIC PROCESS-TRANSACTION (HTP))
(DEFGENERIC SEND-REPLY (HTP SESSION-ID TRANSAC-ID TITLE DATA COMMANDS))
(DEFGENERIC SEND-TABLE (HTP SESSION-ID TRANSAC-ID TITLE ROW-DESC DATA COMMANDS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HTML Transaction Program Interface
;;;

(DEFCLASS HPROGRAM ()
  (
   (ARGUMENTS
    :INITFORM NIL
    :INITARG  :ARGUMENTS
    :ACCESSOR ARGUMENTS
    :TYPE     LIST
    :DOCUMENTATION "A list of command-line arguments.")
   (ENVIRONMENT
    :INITFORM NIL
    :INITARG  :ENVIRONMENT
    :ACCESSOR ENVIRONMENT
    :TYPE     LIST
    :DOCUMENTATION "An alist of unix environment variables: (var . value)*")
   (ALLOWED-NETS
    :INITFORM NIL
    :INITARG  :ALLOWED-NETS
    :ACCESSOR ALLOWED-NETS
    :TYPE     LIST
    :DOCUMENTATION "A list of (ip-address ip-mask).")
   (REFUSED-NETS
    :INITFORM NIL
    :INITARG  :REFUSED-NETS
    :ACCESSOR REFUSED-NETS
    :TYPE     LIST
    :DOCUMENTATION "A list of (ip-address ip-mask).")
   (ACTION
    :INITFORM "action"
    :INITARG  :ACTION
    :ACCESSOR ACTION
    :TYPE     STRING
    :DOCUMENTATION "A string naming the action used in the HTML forms.")
   (TITLE
     :INITFORM "Untitled"
     :INITARG  :TITLE
     :ACCESSOR TITLE
     :TYPE     STRING
     :DOCUMENTATION "A string used as page title.")
   (BODY-ATTRIBUTES
    :INITFORM NIL
    :INITARG  :BODY-ATTRIBUTES
    :ACCESSOR BODY-ATTRIBUTES
    :TYPE     LIST
    :DOCUMENTATION "A list containing the HTML:BODY attributes.")
   )
  (:DOCUMENTATION "An abstract class of a HTML Transaction Program.
   This is the interface used by the HTRANS package to communicate with it.")
  ) ;;HPROGRAM


(DEFGENERIC PROCESS-REQUEST (SELF SESSION-ID TRANS-ID TRANS-DATA)
  (:DOCUMENTATION "A call-back function (hprogram session-id trans-id data)
 that will be called with the decoded request.")
  ) ;;PROCESS-REQUEST


(DEFGENERIC REFUSE-REMOTE (SELF SESSION-ID REMOTE-IP)
  (:DOCUMENTATION "A call-back function called to display a message
  indicating that the remote was refused access for it's IP address.")
  ) ;;REFUSE-REMOTE


(DEFGENERIC REFUSE-SESSION (SELF SESSION-ID)
  (:DOCUMENTATION "A call-back function called to display a message
  indicating that the session-id is bad.")
  ) ;;REFUSE-SESSION


(DEFGENERIC GENERATE-HTML-HEADER (SELF SESSION-ID)
  (:DOCUMENTATION "A hook allowing the hprogram to display a header.
  It should not generate a form with the same action!")
  ) ;;GENERATE-HTML-HEADER


(DEFGENERIC GENERATE-HTML-FOOTER (SELF SESSION-ID)
  (:DOCUMENTATION "A hook allowing the hprogram to display a footer.
  It should not generate a form with the same action!")
  ) ;;GENERATE-HTML-FOOTER




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; unix environment
;;;

(DEFUN UNIX-ENVIRONMENT-GET (ENVIRONMENT NAME)
  "
RETURN: The value of the unix environment variable named NAME.
"
  (CDR (ASSOC NAME ENVIRONMENT :TEST (FUNCTION STRING=)))
  ) ;;UNIX-ENVIRONMENT-GET



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IP ADDRESS MANIPULATION
;;;

(DEFUN BYTEP (THING)
  (AND (INTEGERP THING) (<= 0 THING) (<= THING 255))) ;;BYTEP


(DEFUN IP-STRING-TO-ADDRESS (IP-ADDRESS)
  (LET ((BYTES (SPLIT-STRING IP-ADDRESS ".")))
    ;; NOTE: This SPLIT-STRING takes only a literal pattern!
    (IF (= 4 (LENGTH BYTES))
        (LET* ((A (READ-FROM-STRING (POP BYTES)))
               (B (READ-FROM-STRING (POP BYTES)))
               (C (READ-FROM-STRING (POP BYTES)))
               (D (READ-FROM-STRING (POP BYTES))))
          (IF (AND (BYTEP A) (BYTEP B) (BYTEP C) (BYTEP D))
              (+ (* (+ (* (+ (* A 256) B) 256) C) 256) D)
              NIL))
        NIL))
  ) ;;IP-STRING-TO-ADDRESS


(DEFUN ADDRESS-IN-LAN-P (IP-STRING LAN-ADDRESS LAN-MASK)
  (LET ((ADDRESS (IP-STRING-TO-ADDRESS IP-STRING)))
    (SETQ LAN-ADDRESS (IP-STRING-TO-ADDRESS LAN-ADDRESS)
          LAN-MASK    (IP-STRING-TO-ADDRESS LAN-MASK))
    (AND ADDRESS
         (= (LOGAND ADDRESS LAN-MASK)
            (LOGAND    LAN-ADDRESS LAN-MASK))))
  ) ;;ADDRESS-IN-LAN-P



(DEFUN ADDRESS-IN-SUBNETS-P (ADDRESS SUBNETS)
  "
ADDRESS: A string containing an IP address.
SUBNETS: A list of (''address''  ''mask'')  identifying the subnets.
RETURN:  Whether ADDRESS is on some of the SUBNETS.
"
  (SOME (LAMBDA (SUBNET)
          (ADDRESS-IN-LAN-P ADDRESS (FIRST SUBNET) (SECOND SUBNET)))
        SUBNETS)
  ) ;;ADDRESS-IN-SUBNETS-P


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Session-ID
;;;

(DEFVAR *SESSION-COUNTER* 0)

(defgeneric make-session-id (program ip-string))
(defgeneric check-session-id (program session-id ip-string))

(defmethod MAKE-SESSION-ID ((htp hprogram) IP-STRING)
  (LET ((STAMP   (+ (INCF *SESSION-COUNTER* (* 1000000 (GET-UNIVERSAL-TIME)))))
        (ADDRESS (IP-STRING-TO-ADDRESS  IP-STRING)))
    (FORMAT NIL "~16R-~D" STAMP ADDRESS)))

(defmethod CHECK-SESSION-ID ((htp hprogram) SESSION-ID IP-STRING)
  (LET* ((FIELDS (SPLIT-STRING SESSION-ID "-"))
         (STAMP (LET ((*READ-BASE* 16)) (READ-FROM-STRING (FIRST FIELDS))))
         (ADDRESS (IP-STRING-TO-ADDRESS  IP-STRING))
         (CHECK-ID (FORMAT NIL "~16R-~D" STAMP ADDRESS)))
    (STRING-EQUAL CHECK-ID SESSION-ID)))



(defvar +COMMAND-PREFIX+ "COMMAND-"
  "A prefix used for the INPUT names.") ;;+COMMAND-PREFIX+


(DEFUN GETCMDS (TRANS-DATA)
  "
RETURN:  A list of commands appearing in TRANS-DATA.
NOTE:    Normaly, only one command is present. But if the GET url is hacked,
         zero or multiple commands may be present.
"
  (MAPCAN (LAMBDA (VAR-VAL)
            (LET ((VAR (FIRST VAR-VAL)))
              (IF (PREFIXP +COMMAND-PREFIX+ VAR)
                  (LIST (SUBSEQ VAR (LENGTH +COMMAND-PREFIX+)))
                  NIL)))
          TRANS-DATA))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HTRANS
;;;


;; html-client                  transac-manager                         cgi
;;   |                                 |                                 |
;;   |                                                                  
;;   |                                 /                                 
;;   |--------(initial-request)------->|                                 /
;;   |                                 |-----initialrequest(sessid)----->|
;;   |                                 |                                 |
;;   |                                 |<------reply(sessid,trid)--------|
;;   |<-------(html-form)--------------|                                 /
;;   |                                 /                                 
;;   |                                                                  
;;   |                                 /                                 
;;   |----------(action.get)---------->|                                 /
;;   |                                 |----request(sesid,trid,data)---->|
;;   |                                 |                                 |
;;   |                                 |<------reply(sessid,trid+1)------|
;;   |<-------(html-form)--------------|                                 /
;;   |                                 /                                
;;   |                                                                  
;;   |                                 |                                 |
;;   V                                 V                                 V

;;                               
;; (cgi:run args)
;;   +--> (htrans:process-transaction hprog) 
;;          +--> (request hprog sess-id trans-id data)
;;                 +--> (htrans:reply hprog sess-id trans-id' title data cmds)
;;                        +--> (html:form...)



               
(DEFMETHOD PROCESS-TRANSACTION ((HTP HPROGRAM))
  "
DO:             Parses the CGI request and calls the REQUEST function passing
                it the session-id, transaction-id and request data.
                For the initial request, transaction-id and request data will
                be NIL.
NOTE:           Begins with outputing the HTML header and ends with
                the HTML footer.
"
  (FORMAT T "Content-type: text/html; charset=iso-8859-1~A~A" +CRLF+ +CRLF+)
  (html:with-html-output (*standard-output*)
    (HTML:DOCTYPE :STRICT
      (HTML:HTML ()
        (HTML:HEAD ()
          (HTML:TITLE () (HTML:PCDATA "~A" (TITLE HTP)))
          (HTML:META (:HTTP-EQUIV "Content-Type"
                                  :CONTENT  "text/html; charset=iso-8859-1"))
          (HTML:META (:NAME "generator"
                            :CONTENT "COM.INFORMATIMAGO.COMMON-LISP.HTRANS")))
        (html:collect-element
         (HTML:BODY*
          (BODY-ATTRIBUTES HTP)
          (LET* ((ENVIRONMENT    (ENVIRONMENT HTP))
                 (REMOTE-ADDRESS (UNIX-ENVIRONMENT-GET ENVIRONMENT "REMOTE_ADDR"))
                 (QUERY-STRING (UNIX-ENVIRONMENT-GET ENVIRONMENT "QUERY_STRING"))
                 (QUERY-ARGS   (hquery:QUERY-PARSE QUERY-STRING))
                 (SESSION-ID   (OR (hquery:QUERY-ARGUMENT "SESSION-ID" QUERY-ARGS)
                                   (MAKE-SESSION-ID htp REMOTE-ADDRESS))))
            (list
             (GENERATE-HTML-HEADER HTP SESSION-ID)
             (COND
               ((NOT (CHECK-SESSION-ID htp SESSION-ID REMOTE-ADDRESS))
                (REFUSE-SESSION HTP SESSION-ID))
               ((AND (OR (NOT (ALLOWED-NETS HTP))
                         (ADDRESS-IN-SUBNETS-P REMOTE-ADDRESS (ALLOWED-NETS HTP)))
                     (OR (NOT (REFUSED-NETS HTP))
                         (NOT (ADDRESS-IN-SUBNETS-P REMOTE-ADDRESS
                                                    (REFUSED-NETS HTP)))))
                (PROCESS-REQUEST HTP SESSION-ID
                                 (hquery:QUERY-ARGUMENT "TRANSAC-ID" QUERY-ARGS)
                                 QUERY-ARGS))
               (T
                (REFUSE-REMOTE HTP SESSION-ID REMOTE-ADDRESS)))
             (GENERATE-HTML-FOOTER HTP SESSION-ID)))))))))



  
;; transactions
;;
;; <-- command  id command (name value)*
;; --> display  id (title name ktype value comment)*  command*
;;
;; label ::= NIL | STRING .
;; ktype ::= :LABEL
;;         | :TEXTAREA | (:TEXTAREA [:COLS NUM] [:ROWS NUM])
;;         | :TEXT     | (:TEXT     [:MAXLENGTH NUMBER] [:SIZE NUMBER])
;;         | :PASSWORD | (:PASSWORD [:MAXLENGTH NUMBER] [:SIZE NUMBER])
;;         | :HIDDEN
;;         | (:CHECKBOX (STRING [:CHECKED]))
;;         | (:RADIO    [:VERTICAL|:HORIZONTAL]  (STRING [:CHECKED])*)
;;         | (:MENU   (NAME LABEL [:CHECKED])
;;                  | (:GROUP LABEL (NAME LABEL [:CHECKED])*) * )
;;         | :FILE ;; ?
;; command ::= (:SUBMIT command label     [:SIZE NUMBER])
;;           | (:RESET  command label     [:SIZE NUMBER])
;;           | (:IMAGE  command image-url [:SIZE NUMBER])

(DEFUN KTYPE (TYPE)
  (IF (LISTP TYPE) (FIRST TYPE) TYPE))



(DEFUN KSIZE (TYPE)
  (IF (LISTP TYPE)
      (CADR (MEMBER :SIZE (CDR TYPE)))
      NIL))


(DEFUN KMAXLENGTH (TYPE)
  (IF (LISTP TYPE)
      (CADR (MEMBER :MAXLENGTH (CDR TYPE)))
      NIL))



;; | FIELD-TITLE | FIELD-NAME [ FIELD-VALUE ] | FIELD-COMMENT |
;;                       FIELD-TYPE


(DEFUN GENERATE-MENU-ITEMS (ITEMS)
  (mapcan
   (lambda (item)
     (COND
       ((SYMBOLP ITEM) '())
       ((EQ :GROUP (FIRST ITEM))
        (list (HTML:OPTGROUP* (list :LABEL (SECOND ITEM))
                              (GENERATE-MENU-ITEMS (CDDR ITEM)))))
       (T
        (list (HTML:OPTION* (list :VALUE (FIRST ITEM)
                                  (AND (MEMBER :SELECTED ITEM) :SELECTED))
                            (list (HTML:PCDATA* "~A" (SECOND ITEM))))))))
   items))


(DEFUN GENERATE-MENU (NAME ITEMS)
  (HTML:SELECT* (list :NAME NAME (WHEN (MEMBER :MULTIPLE ITEMS) :MULTIPLE))
                (GENERATE-MENU-ITEMS ITEMS)))


(DEFUN GENERATE-COMMAND-BUTTONS (COMMANDS)
  (mapcar
   (lambda (BUTTON)
     (LET ((COMMAND (SECOND BUTTON))
           (LABEL   (THIRD BUTTON))
           (SIZE    (CADR (ASSOC :SIZE BUTTON))))
       (CASE (KTYPE BUTTON)
         (:SUBMIT
          (HTML:INPUT* (list :NAME (FORMAT NIL "~A~A"
                                           +COMMAND-PREFIX+ COMMAND)
                             :TYPE "SUBMIT"
                             :VALUE LABEL
                             (AND SIZE :SIZE) SIZE)))
         (:RESET
          (HTML:INPUT* (list :NAME (FORMAT NIL "~A~A"
                                           +COMMAND-PREFIX+ COMMAND)
                             :TYPE "RESET"
                             :VALUE LABEL
                             (AND SIZE :SIZE) SIZE)))
         (:IMAGE
          (HTML:INPUT* (list :NAME (FORMAT NIL "~A~A"
                                           +COMMAND-PREFIX+ COMMAND)
                             :TYPE "IMAGE"
                             :SRC LABEL
                             (AND SIZE :SIZE)))))))
   COMMANDS))


(DEFMETHOD SEND-REPLY ((HTP HPROGRAM) SESSION-ID TRANSAC-ID TITLE DATA COMMANDS)
  (HTML:COMMENT "SEND-REPLY")
  (HTML:COMMENT "DATA=~S" DATA)
  (HTML:COMMENT "COMMANDS=~S" COMMANDS)
  (HTML:H2 () (HTML:PCDATA "~A" TITLE))
  (HTML:FORM (:METHOD "GET" :ACTION (ACTION HTP))
    (LET ((HAS-COMMENTS
           (SOME (LAMBDA (FIELD)
                   (AND (FIFTH FIELD)
                        (NOT (EQ :HIDDEN (KTYPE (THIRD FIELD))))))  DATA))
          (HIDDEN-FIELDS  '())
          (VISIBLE-FIELDS '()))
      (HTML:DIV ()
        (HTML:INPUT (:NAME "SESSION-ID"   :TYPE "HIDDEN" :VALUE SESSION-ID))
        (HTML:INPUT (:NAME "TRANSAC-ID"   :TYPE "HIDDEN" :VALUE TRANSAC-ID))
        (DOLIST (FIELD DATA)
          (IF (EQ :HIDDEN (KTYPE (THIRD FIELD)))
              (PUSH FIELD HIDDEN-FIELDS)
              (PUSH FIELD VISIBLE-FIELDS)))
        (SETQ HIDDEN-FIELDS (NREVERSE HIDDEN-FIELDS)
              VISIBLE-FIELDS (NREVERSE VISIBLE-FIELDS))
        (DOLIST (FIELD HIDDEN-FIELDS)
          (LET ((NAME    (SECOND FIELD))
                (VALUE   (FOURTH FIELD)))
            (HTML:INPUT (:NAME NAME :TYPE 'HIDDEN (WHEN VALUE :VALUE) VALUE)))))
      (HTML:TABLE (:SUMMARY (FORMAT NIL "SESSION ~A, TRANSACTION ~A"
                                    SESSION-ID TRANSAC-ID)
                            :WIDTH "95%")
        (DOLIST (FIELD VISIBLE-FIELDS)
          (LET ((TITLE   (FIRST  FIELD))
                (NAME    (SECOND FIELD))
                (TYPE    (THIRD  FIELD))
                (VALUE   (FOURTH FIELD))
                (COMMENT (FIFTH  FIELD)))
            (HTML:TR (:VALIGN "TOP")
              (HTML:TD ()
                (IF TITLE
                    (HTML:PCDATA "~A" TITLE)
                    (FORMAT T "&nbsp;"))) ;; don't escape &nbsp;!
              ;; TODO: We need an HTML:PRINTQ or something...
              (HTML:TD ()
                (CASE (KTYPE TYPE)
                  (:LABEL
                    (HTML:PCDATA "~A" VALUE))
                  ((:TEXT :PASSWORD)
                   (LET ((ML (KMAXLENGTH TYPE))
                         (SZ (KSIZE TYPE)))
                     (HTML:INPUT
                         (list :NAME NAME
                               :TYPE (IF (EQ (KTYPE TYPE) :TEXT)
                                         "TEXT" "PASSWORD")
                               (WHEN ML :MAXLENGTH) ML
                               (WHEN SZ :SIZE)      SZ
                               (WHEN VALUE :VALUE)  VALUE))))
                  (:TEXTAREA (ERROR ":TEXTAREA NOT IMPLEMENTED IN DISPLAY."))
                  (:FILE     (ERROR ":FILE NOT IMPLEMENTED IN DISPLAY."))
                  (:CHECKBOX
                   (HTML:INPUT
                       (list :NAME NAME
                             :TYPE "CHECKBOX"
                             :VALUE (OR VALUE "ON")
                             (AND (EQ :CHECKED (THIRD TYPE)) :CHECKED)))
                   (HTML:PCDATA "~A" (SECOND TYPE)))
                  (:RADIO
                   (LET ((VERTICAL (MEMBER :VERTICAL TYPE))
                         (TITLES (CDR TYPE)))
                     (WHEN (SYMBOLP (CAR TITLES)) (POP TITLES))
                     (IF VERTICAL
                         (HTML:TABLE (:SUMMARY "radio buttons")
                           (DOLIST (TITLE TITLES)
                             (HTML:TR ()
                               (HTML:TD ()
                                 (HTML:INPUT
                                     (list :NAME NAME
                                           :TYPE "RADIO"
                                           :VALUE (OR (FIRST TITLE) "ON")
                                           (AND (MEMBER :CHECKED TITLE)
                                                :CHECKED)))
                                 (HTML:PCDATA "~A" (SECOND TITLE))))))
                         (DOLIST (TITLE TITLES)
                           (HTML:INPUT
                               (list :NAME NAME
                                     :TYPE "RADIO"
                                     :VALUE (OR (FIRST TITLE) "ON")
                                     (AND (MEMBER :CHECKED TITLE) :CHECKED)))
                           (HTML:PCDATA "~A " (SECOND TITLE))))))
                  (:MENU (html:collect-element
                          (GENERATE-MENU NAME TYPE)))))
              (WHEN HAS-COMMENTS
                (HTML:TD ()
                  (IF COMMENT
                      (HTML:PCDATA "~A" COMMENT)
                      (html:html-string "&nbsp;")))))))
        (HTML:TR ()
          (HTML:TD (:COLSPAN (IF HAS-COMMENTS 3 2))
            (html:p ()
              (html:collect-element
               (GENERATE-COMMAND-BUTTONS COMMANDS)))))))))


(DEFMETHOD SEND-TABLE ((HTP HPROGRAM) SESSION-ID TRANSAC-ID TITLE
                       ROW-DESC DATA COMMANDS)
  (HTML:COMMENT "SEND-TABLE")
  (HTML:COMMENT "ROW-DESC=~S" ROW-DESC)
  (HTML:COMMENT "DATA=~S" DATA)
  (HTML:COMMENT "COMMANDS=~S" COMMANDS)
  (HTML:H2 () (HTML:PCDATA "~A" TITLE))
  (HTML:TABLE (:SUMMARY TITLE :WIDTH "95%")
    (MAPC
     (LAMBDA (ROW-DATA)
       ;; interpret a row
       (HTML:TR (:VALIGN "TOP")
         (MAPC
          (LAMBDA (COL-DESC)
            (CASE (POP COL-DESC)
              (:LABEL
                (HTML:TD ()
                  (HTML:PCDATA "~A" (NTH (POP COL-DESC) ROW-DATA))))
              (:SUBMIT
               (LET ((COMMAND (POP COL-DESC))
                     (LABEL   (POP COL-DESC))
                     (SIZE    (WHEN (EQ :SIZE (CAR COL-DESC))
                                (POP COL-DESC) (POP COL-DESC)))
                     (HFIELDS COL-DESC))
                 (HTML:TD ()
                   (HTML:FORM (:METHOD "GET" :ACTION (ACTION HTP))
                     (HTML:DIV ()
                       (HTML:INPUT (list :NAME "SESSION-ID"
                                         :TYPE "HIDDEN" :VALUE SESSION-ID))
                       (HTML:INPUT (list :NAME "TRANSAC-ID"
                                         :TYPE "HIDDEN" :VALUE TRANSAC-ID))
                       (HTML:INPUT
                           (list
                            :NAME (FORMAT NIL "~A~A" +COMMAND-PREFIX+ COMMAND)
                            :TYPE "SUBMIT" :VALUE LABEL
                            (AND SIZE :SIZE) SIZE))
                       (MAPC
                        (LAMBDA (HFIELD)
                          (HTML:INPUT
                              (list :NAME (FIRST HFIELD)
                                    :TYPE "HIDDEN"
                                    :VALUE (NTH (SECOND HFIELD) ROW-DATA))))
                        HFIELDS))))))))
          ROW-DESC)))
     DATA))
  (HTML:FORM (:METHOD "GET" :ACTION (ACTION HTP))
    (HTML:P ()
      (HTML:INPUT (list :NAME "SESSION-ID"
                         :TYPE "HIDDEN" :VALUE SESSION-ID))
      (HTML:INPUT (list :NAME "TRANSAC-ID"
                         :TYPE "HIDDEN" :VALUE TRANSAC-ID))
      (html:collect-element (GENERATE-COMMAND-BUTTONS COMMANDS)))))


;;;; THE END ;;;;
