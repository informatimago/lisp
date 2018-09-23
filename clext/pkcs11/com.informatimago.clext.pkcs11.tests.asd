;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.clext.pkcs11.tests.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    System to test the com.informatimago.clext.pkcs11 package.
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
;;;;

(asdf:defsystem "com.informatimago.clext.pkcs11.tests"
  :description "PKCS11 wrapper tests."
  :author "Pascal J. Bourguignon"
  :version "0.0.0"
  :license "AGPL3"
  :depends-on ("com.informatimago.clext.pkcs11"
               "com.informatimago.common-lisp.interactive")
  :components ((:file "tests"       :depends-on ("pkcs11"))
               (:file "debug"       :depends-on ("pkcs11"))))

;;;; THE END ;;;;
