;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-support.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file loads Objective-C support.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-05-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2016
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
(defpackage "COM.INFORMATIMAGO.OBJECTIVE-CL.READTABLE"
  (:nicknames "COM.INFORMATIMAGO.OBJCL.READTABLE")
  (:use "COMMON-LISP")
  (:export "*CCL-READTABLE*" "*COCOA-READTABLE*"))
(in-package "COM.INFORMATIMAGO.OBJECTIVE-CL.READTABLE")

#+ccl
(defparameter *ccl-readtable* (copy-readtable ccl::%initial-readtable%))

;; We'll try to catch in this variable the objective-c reader macros
;; installed by ccl require cocoa.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cocoa-readtable* nil))


#+(and ccl darwin); for now, not on non-darwin
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable*
        #-(and ccl darwin)
        (copy-readtable nil)
        #+(and ccl darwin) ; #+ccl (require :cocoa) needs the botched readtable.
        (copy-readtable ccl::%initial-readtable%))

  ;; When we (require :objc-support) before (require :cocoa), ccl
  ;; can't find the main bundle.  So we must require :cocoa for the
  ;; applications that need it.
  #-darwin (require :objc-support)
  #+darwin (require :cocoa)

  (defun nsfun-reader-macro (stream subchar numarg)
    (declare (ignorable subchar numarg))
    (let* ((token (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
      (loop
        (let* ((char (read-char stream nil nil)))
          (if (or (eql char #\:)
                  (eql char #\_)
                  (and char (digit-char-p char 36)))
              (vector-push-extend char token)
              (progn
                (when char
                  (unread-char char stream))
                (return)))))
      (unless *read-suppress*
        (unless (> (length token) 0)
          (error reader-error :stream stream)) ; "Invalid token after #/."
        #+ccl(ccl::check-objc-message-name token)
        (intern token "NSFUN"))))

  (unless *cocoa-readtable*
    (setf *cocoa-readtable* (copy-readtable *readtable*))
    (set-dispatch-macro-character #\# #\/ (function nsfun-reader-macro) *cocoa-readtable*))
  (pushnew :objc-support *features*))


;;
;;                                      ccl::%initial-readtable%     CCL readtable
;; com.informatimago.objective-cl.readtable::*ccl-readtable*         CCL readtable.
;; com.informatimago.objective-cl.readtable::*cocoa-readtable*       cocoa readtable.
;;
;;;; THE END ;;;;
