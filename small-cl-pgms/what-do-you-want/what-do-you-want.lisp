;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               what-do-you-want.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Collects wishes.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-11 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2012 - 2012
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

(ql:quickload "com.informatimago.common-lisp.cesarum")
(ql:quickload "com.informatimago.common-lisp.html-generator")
(ql:quickload "split-sequence")
(ql:quickload "hunchentoot")
(in-package "CL-USER")
(defpackage "COM.INFORMATIMAGO.WHAT-DO-YOU-WANT"
  (:nicknames "WDYW")
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export
   "*APPLICATION-NAME*"               ; application name
   "*EMAIL*"                          ; email
   "*SWANK-SERVER-PORT*"              ; port for remote swank
   "*HOME-URI*"                       ; url of the home website
   "*COMMON-RESOURCES-URI*"           ; common resources url
   "*APPLICATION-RESOURCES-URI*"      ; application base url
   ;; "*APPLICATION-BASE-URI*"           ; application base url
   "*APPLICATION-FILE-BASE*"   ; base directory for application files.
   "START-SERVER"                     ; starts the application server
   "STOP-SERVER"                      ; stops the application server
   )
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML" "MAP")
  (:documentation "
Ask the user what they want, and for what price.
"))
(in-package "COM.INFORMATIMAGO.WHAT-DO-YOU-WANT")


(defvar *application-name*               "What do you want")
(defvar *application-port*               8008)
(defvar *swank-server-port*              9008)
(defvar *email*                          "pjb@ogamita.org")

(defvar *home-uri*                       "http://want.ogamita.org/"
  "URL of the main web site.")

(defvar *common-resources-uri*           "http://want.ogamita.org/"
  "URL where the common (static) resources are to be found.
Final / is mandatory.")

(defvar *application-resources-uri*      "http://want.ogamita.org:8005/"
  "URL where the application specific (static) resources are to be found.
Final / is mandatory.")

(defvar *application-file-base*          #P"/srv/www/org.ogamita.want/"
  "Base directory for application files.")


(defparameter *utf-8*
  (flex:make-external-format :utf-8 :eol-style :lf)
  "The UTF-8 encoding.")

(defparameter *spaces*
  #(#\space #\tab #\newline #\linefeed #\return #\formfeed)
  "A bag of spaces.")


(defun common-resources-uri      (path)
  (format nil "~A~A" *common-resources-uri* path))
(defun application-resources-uri (path)
  (format nil "~A~A" *application-resources-uri* path))


(defun in-etc (path)
  (merge-pathnames path
                   (merge-pathnames "etc/" *application-file-base* nil)
                   nil))

(defun in-log (path)
  (merge-pathnames path
                   (merge-pathnames "log/" *application-file-base* nil)
                   nil))

(defun in-incoming (path)
  (merge-pathnames path
                   (merge-pathnames "incoming/" *application-file-base* nil)
                   nil))


(defun in-document-root (path)
  (merge-pathnames path
                   (merge-pathnames "htdocs/" *application-file-base* nil)
                   nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-size (size)
  (cond
    ((< size 1024)
     (format nil "~D octet~:*~P" size))
    ((< size #.(* 1024 1024))
     (format nil "~D Ko" (round size 1024)))
    ((< size #.(* 1024 1024 1024))
     (format nil "~D Mo" (round size #.(* 1024 1024))))
    ((< size #.(* 1024 1024 1024 1024))
     (format nil "~D Go" (round size #.(* 1024 1024 1024))))
    (t
     (format nil "~D To" (round size #.(* 1024 1024 1024 1024))))))


(defun format-date (universal-time)
  (multiple-value-bind (se mi ho da mo ye)
      (decode-universal-time universal-time)
    (format nil "~2,'0D/~2,'0D/~4,'0D ~2,'0D:~2,'0D:~2,'0D"
            da mo ye  ho mi se)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defmacro with-app-page ((&key (title *application-name*)
                               logo-url logo-alt
                               head) &body body)
  (WITH-GENSYMS (vlogo-url vlogo-alt)
    `(with-output-to-string (*html-output-stream*)
       (setf (hunchentoot:content-type*) "text/html")
       (let ((,vlogo-url ,logo-url)
             (,vlogo-alt ,logo-alt))
          (with-html-output (*html-output-stream* :encoding "UTF-8" :kind :html)
           (doctype
             :loose
             (html ()
                   (head ()
                         (title () (pcdata "~A" ,title))
                         (meta (:http-equiv "Content-Type"
                                            :content "text/html; charset=utf-8"))
                         (link (:rel "stylesheet"
                                     :href (common-resources-uri "css/screen.css")
                                     :media "screen, projection"
                                     :rel "stylesheet"
                                     :type "text/css"))
                         ,@head)
                   (body (:class "what-do-you-want")
                         (div (:id "wrap")
                             ;; (div (:id "header")
                             ;;     (table (:class "header")
                             ;;            (tr ()
                             ;;                (td (:class "logo")
                             ;;                    (a (:class "logo" :href *home-uri*)
                             ;;                       (img (:class "logo"
                             ;;                                    :src (common-resources-uri "images/logo-white.png")
                             ;;                                    :alt "Ogamita"))))
                             ;;                (if ,vlogo-url
                             ;;                    (td (:class "logo-client")
                             ;;                        (img (:class "logo-client" :src ,vlogo-url ,@(when vlogo-alt `(:alt ,vlogo-alt)))))
                             ;;                    (td (:class "title")
                             ;;                        (h1 (:class "title") (pcdata "~A" ,title)))))))
                           (div (:id "content-wrap")
                               (div (:id "content")
                                   ,@body))
                           (div (:id "footer")
                               (hr)
                             (small ()
                                    (a (:href (format nil "mailto:~A" *email*) )
                                       (pcdata "~A" *email*))
                                    (br)
                                    (p ()
                                       (pcdata "This service is licensed under the ")
                                       (a (:href "http://www.gnu.org/licenses/agpl-3.0.html")
                                          (pcdata  "GNU Affero General Public License"))
                                       (pcdata ".  As such, you can ")
                                       (a (:href "what-do-you-want.lisp")
                                          (pcdata "get the sources."))))))))))))))


(defmacro reporting-errors (&body body)
  `(handler-bind
       ((error (lambda  (err)
                 (with-app-page (:title "Error")
                   (hunchentoot:log-message* :error "Got an error: ~A" err)
                   #+sbcl (dolist (frame (SB-DEBUG:BACKTRACE-AS-LIST))
                            (hunchentoot:log-message* :error "Backtrace: ~S" frame))
                   #+swank (hunchentoot:log-message* :error "Backtrace: ~S" (swank:backtrace 0 nil))
                   
                   (pcdata "Got an error: ~A" err)
                   #+sbcl (table ()
                                 (dolist (frame (SB-DEBUG:BACKTRACE-AS-LIST))
                                   (tr () (td () (code () (pcdata "~S" frame))))))
                   #+swank (pre () (pcdata "~S" (swank:backtrace 0 nil)))))))
     (progn ,@body)))


(defun make-form (enctype action form-key-and-values other-key-and-values body)
  `(form (:action ,action
                  :method "POST"
                  :accept-charset :utf-8
                  :enctype ,enctype
                  ,@form-key-and-values)
         ,@(loop
              :for (key value) :on other-key-and-values :by (function cddr)
              :collect `(input (:type "hidden"
                                      :name ,(string-downcase key)
                                      :value ,value)))
         ,@body))


(defmacro insert-form ((action (&rest form-key-and-values &key &allow-other-keys)
                               &rest other-key-and-values &key &allow-other-keys)
                       &body body)
  (make-form "application/x-www-form-urlencoded"
             action
             form-key-and-values
             other-key-and-values
             body))


(defmacro insert-file-form ((action (&rest form-key-and-values &key &allow-other-keys)
                                    &rest other-key-and-values &key &allow-other-keys)
                            &body body)
  (make-form "multipart/form-data"
             action
             (list* :id "upload" :name "upload" form-key-and-values)
             other-key-and-values
             body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun what-do-you-want-form ()
  (insert-form
   ("i-want" ())
   (p () (pcdata "What do you want?"))
   (textarea (:name "what-you-want"
                    :rows "10"
                    :cols "60"))
   (p () (pcdata "How much would you pay for it? ")
      (input (:type "text" :size "20" :name "how-much")))
   (input (:type "submit" :value "I want it!"))
   (br) (br)))

(hunchentoot:define-easy-handler (what-do-you-want :uri "") ()
  (with-app-page (:title "What do you want?")
    (what-do-you-want-form)))

(hunchentoot:define-easy-handler (what-do-you-want :uri "/what-do-you-want") ()
  (with-app-page (:title "What do you want?")
    (what-do-you-want-form)))

(hunchentoot:define-easy-handler (i-want :uri "/i-want") (what-you-want how-much)
  (with-app-page (:title "What do you want?")
    (hunchentoot:log-message* :info "~S ~S ~S"
                              (hunchentoot:real-remote-addr)
                              how-much
                              what-you-want)
    (p () (pcdata "Thank you.  We'll see what we can do."))
    (hr)
    (p () (pcdata "If you want something else:"))
    (what-do-you-want-form)))



(defun create-dispatcher (script-name page-function)
  "Creates a dispatch function which will dispatch to the
function denoted by PAGE-FUNCTION if the file name of the current
request is PATH."
  (lambda (request)
      (hunchentoot:log-message* :info "~S"  (hunchentoot:script-name request))
      (and (string=  (hunchentoot:script-name request) script-name)
         page-function)))


(defvar *server* nil)


(defun start-server ()
  (setf hunchentoot:*hunchentoot-default-external-format*  *utf-8*
        hunchentoot:*default-content-type* "text/html; charset=UTF-8")
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port *application-port*))
  (setf hunchentoot:*dispatch-table*
        (list (create-dispatcher "/" 'what-do-you-want)
              (hunchentoot:create-static-file-dispatcher-and-handler
               "/what-do-you-want.lisp"
               (in-document-root "what-do-you-want.lisp")
               "text/plain")
              (hunchentoot:create-folder-dispatcher-and-handler
               "/css/"
               (in-document-root "css/"))
              'hunchentoot:dispatch-easy-handlers))
  (setf (hunchentoot:acceptor-access-log-destination *server*)
        (in-log "what-do-you-want.access_log")
        (hunchentoot:acceptor-message-log-destination *server*)
        (in-log "what-do-you-want.log"))
  (hunchentoot:start *server*))




(defun stop-server ()
  (hunchentoot:stop *server*)
  (setf *server* nil))


;;;; THE END ;;;;
