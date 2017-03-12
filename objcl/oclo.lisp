;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               oclo.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file defines additions to the oclo package not provided by
;;;;    the implementation or implementation specific code.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-12-17 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.OBJECTIVE-C.LOWER")

(defmacro stret (expression &environment env)
  (let ((result (gensym "structure-result-")))
    `(slet ((,result ,(macroexpand expression env))) ,result)))


#-(and)
(defun needs-stret (o msg args env &optional sclassname)
  (multiple-value-bind (msg args vargs) (ccl::parse-message (cons msg args))
    (let ((message-info (ccl::get-objc-message-info msg)))
      (unless message-info
        (error "Unknown message: ~S" msg))
      ;; If a vararg exists, make sure that the message can accept it
      (when (and vargs (not (getf (ccl::objc-message-info-flags message-info)
                                  :accepts-varargs)))
        (error "Message ~S cannot accept a variable number of arguments" msg))
      (unless (= (length args) (ccl::objc-message-info-req-args message-info))
        (error "Message ~S requires ~a ~d args, but ~d were provided."
               msg
               (if vargs "at least" "exactly")
               (ccl::objc-message-info-req-args message-info)
               (length args)))
      (multiple-value-bind (args svarforms sinitforms) (ccl::sletify-message-args args)
        (let* ((ambiguous   (getf (ccl::objc-message-info-flags message-info) :ambiguous))
               (methods     (ccl::objc-message-info-methods message-info))
               (method-info (if ambiguous
                                (let ((class (if sclassname
                                                 (ccl::find-objc-class sclassname)
                                                 (ccl::get-objc-class-from-declaration (ccl::declared-type o env)))))
                                  (when class
                                    (dolist (m methods)
                                      (unless (getf (ccl::objc-method-info-flags m) :protocol)
                                        (let ((mclass (or (ccl::get-objc-method-info-class m)
                                                          (error "Can't find ObjC class named ~s"
                                                                 (ccl::objc-method-info-class-name m)))))
                                          (when (subtypep class mclass)
                                            (return m)))))))
                                (car methods))))
          (if method-info
              (ccl::result-type-requires-structure-return
               (ccl::objc-method-info-result-type method-info))
              (error "Cannot find method result type for message -~A sent to ~S.  Try declaring the class of the recipient."
                     (ccl::objc-message-info-message-name message-info) o)))))))


#-(and)
(defmacro send (&whole w o msg &rest args &environment env)
  (if (needs-stret o msg args env)
      `(stret ,w)
      (ccl::make-optimized-send o msg args env)))

#-(and)
(defmacro send/stret (&whole w s o msg &rest args &environment env)
  (if (needs-stret o msg args env)
      (if s
          (ccl::make-optimized-send o msg args env s)
          `(stret (send ,@(cddr w))))
      (ccl::make-optimized-send o msg args env)))


;;;; THE END ;;;;
