;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               filter.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a grep I/O filter,
;;;;    and a macro to build unix-like pipes, using
;;;;    com.informatimago.clext.pipe.
;;;;    Note com.informatimago.interactive.browser:{cat,more,less}
;;;;    as any other expression reading *standard-input* and writing
;;;;    *standard-output* can be used in a filter,
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-10-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(defpackage "COM.INFORMATIMAGO.CLEXT.FILTER"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS"
        "CL-PPCRE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.CLEXT.PIPE")
  (:export "FILTER" "IN" "GREP" "OUT")
  (:documentation "

This package exports a grep I/O filter,
and a macro to build unix-like pipes, using
com.informatimago.clext.pipe.
Note com.informatimago.interactive.browser:{cat,more,less}
as any other expression reading *standard-input* and writing
*standard-output* can be used in a filter,

LEGAL

Copyright Pascal J. Bourguignon 2015 - 2015

AGPL3

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTYwithout even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

"))
(in-package "COM.INFORMATIMAGO.CLEXT.FILTER")


(defun in (pathname &key (external-format :default) (if-does-not-exist :error))
  "

DO:                 Reads the text file at PATHNAME and writes it out
                    to *STANDARD-OUTPUT*.

EXTERNAL-FORMAT:    The external-format for the file.

IF-DOES-NOT-EXIST:  (or (member :ERROR :END-OF-FILE NIL) string stream)

                    If the file doesn't exist, by default an error is
                    signaled.  Alternatives are:

                    :END-OF-FILE closes the stream bound to
                    *STANDARD-OUTPUT* Note: you probably should don't
                    do that on the system standard output.

                    NIL: do nothing.

                    a STRING: writes the string to *STANDARD-OUTPUT*.

                    a STREAM: copy the stream to *STANDARD-OUTPUT*.

SEE ALSO:           COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER:CAT

"
  (with-open-file (input pathname
                    :direction :input
                    :external-format external-format
                    :if-does-not-exist (if (eq :error if-does-not-exist)
                                           :error
                                           nil))
    (if input
        (copy-stream input *standard-output*)
        (etypecase if-does-not-exist
          (null     #|do nothing|#)
          (string   (write-string if-does-not-exist *standard-output*))
          (stream   (copy-stream  if-does-not-exist *standard-output*))))))


(defun out (pathname &key (external-format :default) (if-exists :error))
  "

DO:                 Writes the data from *STANDARD-INPUT* into the
                    text file at PATHNAME.

EXTERNAL-FORMAT:    The external-format for the file.

IF-EXISTS:          same as for OPEN.

"
  (with-open-file (output pathname
                          :direction :output
                          :external-format external-format
                          :if-does-not-exist :create
                          :if-exists if-exists)
    (when output
      (copy-stream *standard-input* output))))


(defun grep (regexp &key case-insensitive (extended t) count line-number filename)
  (loop
    :with filename := (typecase filename
                        ((or string pathname) filename)
                        (null nil)
                        (t   (if (typep *standard-input* 'file-stream)
                                 (pathname *standard-input*)
                                 (princ-to-string *standard-input*))))
    :with scanner := (create-scanner regexp
                                     :case-insensitive-mode case-insensitive
                                     :single-line-mode t
                                     :extended-mode extended
                                     :destructive nil)
    :with counter := 0
    :for line := (read-line *standard-input* nil nil)
    :for linum :from 1
    :while line
    :when (scan scanner line)
      :do (if count
              (incf counter)
              (if line-number
                  (if filename
                      (format t "~A:~D:~A~%" filename linum line)
                      (format t "~D:~A~%" linum line))
                  (if filename
                      (format t "~A:~A~%" filename line)
                      (write-line line))))
    :finally (when count
               (format t "~D line~:*~P match~:[~;es~]~%"
                       counter (= 1 counter)))))



(defvar *buffer-size* 4000)

(defmacro filter (&body filter-forms)
  (let* ((vpipe (gensym "pipe")))
    ;; *standard-input* a pipe b pipe c *standard-output* main thread.
    ;;                  -      -      
    (if (rest filter-forms)
        `(let ((,vpipe (make-pipe :buffer-size *buffer-size*)))
           (bt:make-thread (lambda ()
                             (unwind-protect
                                  (filter ,@(butlast filter-forms))
                               (close *standard-output*)))
                           :initial-bindings (list (cons '*standard-output* (pipe-output-stream ,vpipe))
                                                   (cons '*standard-input* *standard-input*)))
           
           (let ((*standard-input* (pipe-input-stream ,vpipe)))
             ,@(last filter-forms)))
        (first filter-forms))))


;; (pprint (macroexpand-1 '(filter (in "/etc/passwd") (out "/tmp/p"))))
;; (filter (in "/etc/passwd") (out "/tmp/p"))


;;;; THE END ;;;;
