;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the telnet package.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.TELNET"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:shadow "DO" "ABORT")
  (:export
   ;; Conditions:
   "TELNET-WARNING" "TELNET-WARNING-NVT" "TELNET-WARNING-FORMAT-CONTROL"
   "TELNET-WARNING-FORMAT-ARGUMENTS"
   "TELNET-OPTION-WARNING" "TELNET-OPTION-WARNING-OPTION"
   "TELNET-ERROR" "TELNET-ERROR-NVT" "TELNET-ERROR-FORMAT-CONTROL"
   "TELNET-ERROR-FORMAT-ARGUMENTS"
   "TELNET-OPTION-ERROR" "TELNET-OPTION-ERROR-OPTION"
   "TELNET-INVALID-OPTION-NAME-ERROR" "TELNET-INVALID-OPTION-NAME"   
   "TELNET-INVALID-CONTROL-ERROR" "TELNET-INVALID-CONTROL"
   "TELNET-PROTOCOL-ERROR" "TELNET-PROTOCOL-ERROR-OPTION"
   "TELNET-PROTOCOL-ERROR-ACTION" "TELNET-PROTOCOL-ERROR-MESSAGE"
   ;; Classes:
   "NETWORK-VIRTUAL-TERMINAL" "NVT-OPTIONS" "NVT-CLIENT-P"
   "NVT-SEND-WAIT-P" "NVT-URGENT-MODE-P" "NVT-UP-SENDER"
   "NVT-DOWN-SENDER"
   "OPTION" "OPTION-CODE" "OPTION-NAME"
   ;; Up interface (from up):
   "SEND-BINARY" "SEND-TEXT" "SEND-CONTROL"
   ;; Up interface (to up):
   "WANT-OPTION-P" "RECEIVE-BINARY" "RECEIVE-TEXT" "RECEIVE-CONTROL"
   ;; Down interface (to down):
   "SEND"
   ;; Down interface (from down):
   "RECEIVE"
   ;; Option control:
   "GET-OPTION" "OPTION-ENABLED-P" "OPTION-NEGOTIATING-P"
   "ENABLE-OPTION" "DISABLE-OPTION" "OPTION-REGISTER-CLASS"
   "OPTION-REGISTER-DEFAULT-CLASSES"
   ;; Implemented by subclasses of OPTION-STATE:
   "RECEIVE-SUBNEGOTIATION"
   "ENCODE-SUBNEGOTIATION"
   "DECODE-SUBNEGOTIATION"
   ;; Options:
   ;; STATUS
   "STATUS" "SEND-STATUS"
   ;;
   
   )
  (:documentation "
Implements the TELNET protocol.

"))


;;;; THE END ;;;;
