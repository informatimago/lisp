;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               htrans.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;NOWEB:              t
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-10 <PJB> Removed depdency on de.pmsf.md5.
;;;;    2003-09-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2015
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

(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML"
                            "COM.INFORMATIMAGO.COMMON-LISP.HTTP.HQUERY"))

#-mocl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (com.informatimago.common-lisp.cesarum.package:add-nickname
   "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML" "HTML"))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.HTTP.HTRANS"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "GETCMDS" "GETARG" "SEND-TABLE" "SEND-REPLY" "PROCESS-TRANSACTION"
           "GENERATE-HTML-FOOTER" "GENERATE-HTML-HEADER" "REFUSE-SESSION"
           "REFUSE-REMOTE" "PROCESS-REQUEST" "BODY-ATTRIBUTES" "TITLE" "ACTION"
           "REFUSED-NETS" "ALLOWED-NETS" "ENVIRONMENT" "ARGUMENTS" "HPROGRAM")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING" "PREFIXP" "SPLIT-STRING")
  (:documentation
   "
A simple \"transaction\" manager for CGI.


   html-client                  transac-manager                         cgi
     |                                 |                                 |
     |                                                                  
     |                                 /                                 
     |--------(initial-request)------->|                                 /
     |                                 |-----initialrequest(sessid)----->|
     |                                 |                                 |
     |                                 |<------reply(sessid,trid)--------|
     |<-------(html-form)--------------|                                 /
     |                                 /                                 
     |                                                                  
     |                                 /                                 
     |----------(action.get)---------->|                                 /
     |                                 |----request(sesid,trid,data)---->|
     |                                 |                                 |
     |                                 |<------reply(sessid,trid+1)------|
     |<-------(html-form)--------------|                                 /
     |                                 /                                
     |                                                                  
     |                                 |                                 |
     V                                 V                                 V


In this implementation transac-manager and cgi are linked together in
the CGI process and this CGI process lives from the HTML request to
the response.




License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2003 - 2012
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.HTTP.HTRANS")


(defvar +crlf+ (format nil "~C~C" (code-char 13) (code-char 10)))

(defgeneric process-transaction (htp)
  (:documentation   "
DO:             Parses the CGI request and calls the REQUEST function passing
                it the session-id, transaction-id and request data.
                For the initial request, transaction-id and request data will
                be NIL.
NOTE:           Begins by outputing the HTML header and ends with
                the HTML footer.
"))
(defgeneric send-reply (htp session-id transac-id title data commands)
  (:documentation   "DO:       Sends a reply page."))
(defgeneric send-table (htp session-id transac-id title row-desc data commands)
  (:documentation   "DO:       Sends a table."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HTML Transaction Program Interface
;;;

(defgeneric action (program)
  (:documentation "A string naming the action used in the HTML forms."))
(defgeneric allowed-nets (program)
  (:documentation "A list of (ip-address ip-mask)."))
(defgeneric arguments (program)
  (:documentation "A list of command-line arguments."))
(defgeneric body-attributes (program)
  (:documentation "A list containing the HTML:BODY attributes."))
(defgeneric environment (program)
  (:documentation  "An a-list of unix environment variables: (var . value)*"))
(defgeneric refused-nets (program)
  (:documentation "A list of (ip-address ip-mask)."))
(defgeneric title (program)
  (:documentation "A string used as page title."))


(defclass hprogram ()
  ((arguments
    :initform nil
    :initarg  :arguments
    :accessor arguments
    :type     list
    :documentation "A list of command-line arguments.")
   (environment
    :initform nil
    :initarg  :environment
    :accessor environment
    :type     list
    :documentation "An a-list of unix environment variables: (var . value)*")
   (allowed-nets
    :initform nil
    :initarg  :allowed-nets
    :accessor allowed-nets
    :type     list
    :documentation "A list of (ip-address ip-mask).")
   (refused-nets
    :initform nil
    :initarg  :refused-nets
    :accessor refused-nets
    :type     list
    :documentation "A list of (ip-address ip-mask).")
   (action
    :initform "action"
    :initarg  :action
    :accessor action
    :type     string
    :documentation "A string naming the action used in the HTML forms.")
   (title
     :initform "Untitled"
     :initarg  :title
     :accessor title
     :type     string
     :documentation "A string used as page title.")
   (body-attributes
    :initform nil
    :initarg  :body-attributes
    :accessor body-attributes
    :type     list
    :documentation "A list containing the HTML:BODY attributes."))
  (:documentation "An abstract class of a HTML Transaction Program.
This is the interface used by the HTRANS package to communicate with it."))


(defgeneric process-request (self session-id trans-id trans-data)
  (:documentation "A call-back function (hprogram session-id trans-id data)
that will be called with the decoded request."))


(defgeneric refuse-remote (self session-id remote-ip)
  (:documentation "A call-back function called to display a message
indicating that the remote was refused access for it's IP address."))


(defgeneric refuse-session (self session-id)
  (:documentation "A call-back function called to display a message
indicating that the session-id is bad."))


(defgeneric generate-html-header (self session-id)
  (:documentation "A hook allowing the hprogram to display a header.
It should not generate a form with the same action!"))


(defgeneric generate-html-footer (self session-id)
  (:documentation "A hook allowing the hprogram to display a footer.
It should not generate a form with the same action!"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; unix environment
;;;

(defun unix-environment-get (environment name)
  "
RETURN: The value of the unix environment variable named NAME.
"
  (cdr (assoc name environment :test (function string=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IP ADDRESS MANIPULATION
;;;

(defun bytep (thing)
  (and (integerp thing) (<= 0 thing) (<= thing 255)))


(defun ip-string-to-address (ip-address)
  (let ((bytes (split-string ip-address ".")))
    ;; NOTE: This SPLIT-STRING takes only a literal pattern!
    (if (= 4 (length bytes))
        (let* ((a (read-from-string (pop bytes)))
               (b (read-from-string (pop bytes)))
               (c (read-from-string (pop bytes)))
               (d (read-from-string (pop bytes))))
          (if (and (bytep a) (bytep b) (bytep c) (bytep d))
              (+ (* (+ (* (+ (* a 256) b) 256) c) 256) d)
              nil))
        nil)))


(defun address-in-lan-p (ip-string lan-address lan-mask)
  (let ((address (ip-string-to-address ip-string)))
    (setq lan-address (ip-string-to-address lan-address)
          lan-mask    (ip-string-to-address lan-mask))
    (and address
         (= (logand address lan-mask)
            (logand    lan-address lan-mask)))))



(defun address-in-subnets-p (address subnets)
  "
ADDRESS: A string containing an IP address.
SUBNETS: A list of (''address''  ''mask'')  identifying the subnets.
RETURN:  Whether ADDRESS is on some of the SUBNETS.
"
  (some (lambda (subnet)
          (address-in-lan-p address (first subnet) (second subnet)))
        subnets))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Session-ID
;;;

(defvar *session-counter* 0)

(defgeneric make-session-id (program ip-string))
(defgeneric check-session-id (program session-id ip-string))

(defmethod make-session-id ((htp hprogram) ip-string)
  (let ((stamp   (+ (incf *session-counter* (* 1000000 (get-universal-time)))))
        (address (ip-string-to-address  ip-string)))
    (format nil "~16R-~D" stamp address)))

(defmethod check-session-id ((htp hprogram) session-id ip-string)
  (let* ((fields (split-string session-id "-"))
         (stamp (let ((*read-base* 16)) (read-from-string (first fields))))
         (address (ip-string-to-address  ip-string))
         (check-id (format nil "~16R-~D" stamp address)))
    (string-equal check-id session-id)))



(defvar +command-prefix+ "COMMAND-"
  "A prefix used for the INPUT names.")


(defun getcmds (trans-data)
  "
RETURN:  A list of commands appearing in TRANS-DATA.
NOTE:    Normaly, only one command is present. But if the GET url is hacked,
         zero or multiple commands may be present.
"
  (mapcan (lambda (var-val)
            (let ((var (first var-val)))
              (if (prefixp +command-prefix+ var)
                  (list (subseq var (length +command-prefix+)))
                  nil)))
          trans-data))



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



               
(defmethod process-transaction ((htp hprogram))
  "
DO:             Parses the CGI request and calls the REQUEST function passing
                it the session-id, transaction-id and request data.
                For the initial request, transaction-id and request data will
                be NIL.
NOTE:           Begins with outputing the HTML header and ends with
                the HTML footer.
"
  (format t "Content-type: text/html; charset=iso-8859-1~A~A" +crlf+ +crlf+)
  (html:with-html-output (*standard-output*)
    (html:doctype :strict
      (html:html ()
        (html:head ()
          (html:title () (html:pcdata "~A" (title htp)))
          (html:meta (:http-equiv "Content-Type"
                                  :content  "text/html; charset=iso-8859-1"))
          (html:meta (:name "generator"
                            :content "COM.INFORMATIMAGO.COMMON-LISP.HTTP.HTRANS")))
        (html:collect-element
         (html:body*
          (body-attributes htp)
          (let* ((environment    (environment htp))
                 (remote-address (unix-environment-get environment "REMOTE_ADDR"))
                 (query-string (unix-environment-get environment "QUERY_STRING"))
                 (query-args   (com.informatimago.common-lisp.http.hquery:query-parse query-string))
                 (session-id   (or (com.informatimago.common-lisp.http.hquery:query-argument "SESSION-ID" query-args)
                                   (make-session-id htp remote-address))))
            (list
             (generate-html-header htp session-id)
             (cond
               ((not (check-session-id htp session-id remote-address))
                (refuse-session htp session-id))
               ((and (or (not (allowed-nets htp))
                         (address-in-subnets-p remote-address (allowed-nets htp)))
                     (or (not (refused-nets htp))
                         (not (address-in-subnets-p remote-address
                                                    (refused-nets htp)))))
                (process-request htp session-id
                                 (com.informatimago.common-lisp.http.hquery:query-argument "TRANSAC-ID" query-args)
                                 query-args))
               (t
                (refuse-remote htp session-id remote-address)))
             (generate-html-footer htp session-id)))))))))



  
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

(defun ktype (type)
  (if (listp type) (first type) type))



(defun ksize (type)
  (if (listp type)
      (cadr (member :size (cdr type)))
      nil))


(defun kmaxlength (type)
  (if (listp type)
      (cadr (member :maxlength (cdr type)))
      nil))



;; | FIELD-TITLE | FIELD-NAME [ FIELD-VALUE ] | FIELD-COMMENT |
;;                       FIELD-TYPE


(defun generate-menu-items (items)
  (mapcan
   (lambda (item)
     (cond
       ((symbolp item) '())
       ((eq :group (first item))
        (list (html:optgroup* (list :label (second item))
                              (generate-menu-items (cddr item)))))
       (t
        (list (html:option* (list :value (first item)
                                  (and (member :selected item) :selected))
                            (list (html:pcdata* "~A" (second item))))))))
   items))


(defun generate-menu (name items)
  (html:select* (list :name name (when (member :multiple items) :multiple))
                (generate-menu-items items)))


(defun generate-command-buttons (commands)
  (mapcar
   (lambda (button)
     (let ((command (second button))
           (label   (third button))
           (size    (cadr (assoc :size button))))
       (case (ktype button)
         (:submit
          (html:input* (list :name (format nil "~A~A"
                                           +command-prefix+ command)
                             :type "SUBMIT"
                             :value label
                             (and size :size) size)))
         (:reset
          (html:input* (list :name (format nil "~A~A"
                                           +command-prefix+ command)
                             :type "RESET"
                             :value label
                             (and size :size) size)))
         (:image
          (html:input* (list :name (format nil "~A~A"
                                           +command-prefix+ command)
                             :type "IMAGE"
                             :src label
                             (and size :size)))))))
   commands))


(defmethod send-reply ((htp hprogram) session-id transac-id title data commands)
  (html:comment "SEND-REPLY")
  (html:comment "DATA=~S" data)
  (html:comment "COMMANDS=~S" commands)
  (html:h2 () (html:pcdata "~A" title))
  (html:form (:method "GET" :action (action htp))
    (let ((has-comments
           (some (lambda (field)
                   (and (fifth field)
                        (not (eq :hidden (ktype (third field))))))  data))
          (hidden-fields  '())
          (visible-fields '()))
      (html:div ()
        (html:input (:name "SESSION-ID"   :type "HIDDEN" :value session-id))
        (html:input (:name "TRANSAC-ID"   :type "HIDDEN" :value transac-id))
        (dolist (field data)
          (if (eq :hidden (ktype (third field)))
              (push field hidden-fields)
              (push field visible-fields)))
        (setq hidden-fields (nreverse hidden-fields)
              visible-fields (nreverse visible-fields))
        (dolist (field hidden-fields)
          (let ((name    (second field))
                (value   (fourth field)))
            (html:input (:name name :type 'hidden (when value :value) value)))))
      (html:table (:summary (format nil "SESSION ~A, TRANSACTION ~A"
                                    session-id transac-id)
                            :width "95%")
        (dolist (field visible-fields)
          (let ((title   (first  field))
                (name    (second field))
                (type    (third  field))
                (value   (fourth field))
                (comment (fifth  field)))
            (html:tr (:valign "TOP")
              (html:td ()
                (if title
                    (html:pcdata "~A" title)
                    (format t "&nbsp;"))) ;; don't escape &nbsp;!
              ;; TODO: We need an HTML:PRINTQ or something...
              (html:td ()
                (case (ktype type)
                  (:label
                    (html:pcdata "~A" value))
                  ((:text :password)
                   (let ((ml (kmaxlength type))
                         (sz (ksize type)))
                     (html:input
                         (list :name name
                               :type (if (eq (ktype type) :text)
                                         "TEXT" "PASSWORD")
                               (when ml :maxlength) ml
                               (when sz :size)      sz
                               (when value :value)  value))))
                  (:textarea (error ":TEXTAREA NOT IMPLEMENTED IN DISPLAY."))
                  (:file     (error ":FILE NOT IMPLEMENTED IN DISPLAY."))
                  (:checkbox
                   (html:input
                       (list :name name
                             :type "CHECKBOX"
                             :value (or value "ON")
                             (and (eq :checked (third type)) :checked)))
                   (html:pcdata "~A" (second type)))
                  (:radio
                   (let ((vertical (member :vertical type))
                         (titles (cdr type)))
                     (when (symbolp (car titles)) (pop titles))
                     (if vertical
                         (html:table (:summary "radio buttons")
                           (dolist (title titles)
                             (html:tr ()
                               (html:td ()
                                 (html:input
                                     (list :name name
                                           :type "RADIO"
                                           :value (or (first title) "ON")
                                           (and (member :checked title)
                                                :checked)))
                                 (html:pcdata "~A" (second title))))))
                         (dolist (title titles)
                           (html:input
                               (list :name name
                                     :type "RADIO"
                                     :value (or (first title) "ON")
                                     (and (member :checked title) :checked)))
                           (html:pcdata "~A " (second title))))))
                  (:menu (html:collect-element
                          (generate-menu name type)))))
              (when has-comments
                (html:td ()
                  (if comment
                      (html:pcdata "~A" comment)
                      (html:html-string "&nbsp;")))))))
        (html:tr ()
          (html:td (:colspan (if has-comments 3 2))
            (html:p ()
              (html:collect-element
               (generate-command-buttons commands)))))))))


(defmethod send-table ((htp hprogram) session-id transac-id title
                       row-desc data commands)
    "
DO:       Sends a table.
"
  (html:comment "SEND-TABLE")
  (html:comment "ROW-DESC=~S" row-desc)
  (html:comment "DATA=~S" data)
  (html:comment "COMMANDS=~S" commands)
  (html:h2 () (html:pcdata "~A" title))
  (html:table (:summary title :width "95%")
    (mapc
     (lambda (row-data)
       ;; interpret a row
       (html:tr (:valign "TOP")
         (mapc
          (lambda (col-desc)
            (case (pop col-desc)
              (:label
                (html:td ()
                  (html:pcdata "~A" (nth (pop col-desc) row-data))))
              (:submit
               (let ((command (pop col-desc))
                     (label   (pop col-desc))
                     (size    (when (eq :size (car col-desc))
                                (pop col-desc) (pop col-desc)))
                     (hfields col-desc))
                 (html:td ()
                   (html:form (:method "GET" :action (action htp))
                     (html:div ()
                       (html:input (list :name "SESSION-ID"
                                         :type "HIDDEN" :value session-id))
                       (html:input (list :name "TRANSAC-ID"
                                         :type "HIDDEN" :value transac-id))
                       (html:input
                           (list
                            :name (format nil "~A~A" +command-prefix+ command)
                            :type "SUBMIT" :value label
                            (and size :size) size))
                       (mapc
                        (lambda (hfield)
                          (html:input
                              (list :name (first hfield)
                                    :type "HIDDEN"
                                    :value (nth (second hfield) row-data))))
                        hfields))))))))
          row-desc)))
     data))
  (html:form (:method "GET" :action (action htp))
    (html:p ()
      (html:input (list :name "SESSION-ID"
                         :type "HIDDEN" :value session-id))
      (html:input (list :name "TRANSAC-ID"
                         :type "HIDDEN" :value transac-id))
      (html:collect-element (generate-command-buttons commands)))))


;;;; THE END ;;;;
