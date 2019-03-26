;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.clext.pkcs11.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Lispy interface over Cryptoki pkcs11 version 2.02
;;;;
;;;;    Depends on git@github.com:fjames86/asinine.git
;;;;            or git@github.com:informatimago/asinine.git
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-09-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
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

(asdf:defsystem "com.informatimago.clext.pkcs11"
  :description "PKCS11 wrapper."
  :author "Pascal J. Bourguignon"
  :version "0.0.0"
  :license "AGPL3"
  :depends-on ("cffi" "babel" "asinine")
  :components ((:file "pkcs11-cffi" :depends-on ())
               (:file "cffi-utils"  :depends-on ())
               (:file "cffi-debug"  :depends-on ())
               (:file "pkcs11"      :depends-on ("pkcs11-cffi" "cffi-utils" "cffi-debug")))
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op (asdf:test-op "com.informatimago.clext.pkcs11.tests"))))

;;;; THE END ;;;;
