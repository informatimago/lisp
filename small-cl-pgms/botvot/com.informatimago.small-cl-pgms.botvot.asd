;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.small-cl-pgms.botvot.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Botvot: An IRC bot to manage votes in irc.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-04-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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

(asdf:defsystem "com.informatimago.small-cl-pgms.botvot"
  :description "An IRC bot to manage votes in irc."
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "AGPL3"
  :depends-on ("com.informatimago.common-lisp.cesarum"
               "com.informatimago.common-lisp.interactive"
               "cl-irc"  "split-sequence" "cl-ppcre"
               "ironclad" "babel")
  :components ((:file "botvot"))
  #+asdf3 :in-order-to
  #+asdf3 ((test-op (test-op "com.informatimago.small-cl-pgms.botvot.test"))))

(asdf:defsystem "com.informatimago.small-cl-pgms.botvot.test"
  :description "Tests an IRC bot to manage votes in irc."
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "AGPL3"
  :depends-on ("com.informatimago.common-lisp.cesarum"
               "com.informatimago.small-cl-pgms.botvot")
  :components ((:file "botvot-test"))
  #+asdf3 :perform
  #+asdf3 (test-op (operation system)
                   (let ((*package* (find-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.BOTVOT")))
                     (uiop:symbol-call *package* "TEST/ALL"))))


;;;; THE END ;;;;
