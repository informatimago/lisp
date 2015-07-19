;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               try-systems.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tries to compile all the systems in an environment similar to
;;;;    the one used by quicklisp when validating systems.
;;;;
;;;;    Report errors.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.TOOLS.TRY-SYSTEMS"
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.TOOLS.ASDF-FILE"
        "COM.INFORMATIMAGO.TOOLS.SCRIPT"))
(in-package "COM.INFORMATIMAGO.TOOLS.TRY-SYSTEMS")

(defmacro in-home (relative-path)
  `(load-time-value (merge-pathnames ,relative-path
                                     (user-homedir-pathname)
                                     nil)))

(defvar *reports-directory*
  (in-home #P"try-system-reports/")
  "Pathname of the directory where to store reports.")

(defvar *asdf*
  ;; We cannot use quicklisp/asdf.lisp since it's too old an asdf.
  ;;   (in-home #P"quicklisp/asdf.lisp")
  (let ((dir #.(or *compile-file-truename* *load-truename* #P"./")))
    (merge-pathnames (make-pathname :name "asdf" :type "lisp" :version nil :defaults dir)
                     dir nil))
  "Pathname of the asdf.lisp source file.")

(defvar *releases-file*
  (in-home #P"quicklisp/dists/quicklisp/releases.txt")
  "The quicklisp files listing all the releases.")

(defvar *local-projects-directory*
  (in-home #P"quicklisp/local-projects/")
  "The quicklisp files listing all the releases.")

(defvar *software-directory*
  (in-home #P"quicklisp/dists/quicklisp/software/")
  "The directory where the quicklisp systems are stored.")


(defun directory-of (pathname)
  (make-pathname :name nil :type nil :version nil
                 :defaults pathname))

(defun find-asd-systems-in-directory (root-directory)
  (mapcar (lambda (asd-file-pathname)
            (cons (directory-of asd-file-pathname)
                  (asd-systems-in-asd-file asd-file-pathname)))
          (find-asd-files root-directory)))

(defun quicklisp-registry ()
  "Returns a list of all the directories where there's a ASD file managed by quicklisp."
  (let ((paths '()))
    (flet ((process-files (files)
             (dolist (asdf files)
               (push (directory-of asdf) paths))))
      (dolist (line (remove-if (lambda (line) (or (zerop (length line)) (char= #\# (aref line 0))))
                               (string-list-text-file-contents *releases-file*)))
        (destructuring-bind (dir &rest files) (nthcdr 5 (split-sequence #\space line :remove-empty-subseqs t))
          (let ((base (merge-pathnames (make-pathname :directory (list :relative dir)
                                                      :defaults  *software-directory*)
                                       *software-directory*
                                       nil)))
            (process-files (mapcar (lambda (asdf) (merge-pathnames asdf base nil)) files)))))
      (process-files (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
                                                                :name :wild :type "asd" :case :local
                                                                :defaults *local-projects-directory*)
                                                 *local-projects-directory* nil))))
    (remove-duplicates paths :test (function equalp))))

(defun run (date system operation
            lisp &rest arguments)
  (let* ((report-dir  (merge-pathnames (make-pathname :directory (list :relative date)
                                                      :defaults *reports-directory*)
                                       *reports-directory* nil))
         (output-file (make-pathname :name (format nil "~A-~A-~A" date operation system)
                                     :type "output"
                                     :case :local
                                     :defaults report-dir))
         (error-file  (make-pathname :type "error"
                                     :case :local
                                     :defaults output-file)))
    (ensure-directories-exist error-file)
    (ignore-errors
     (progn
       (format *trace-output* "~A~%" output-file)
       (force-output *trace-output*)
       (uiop:run-program (mapconcat (function shell-quote-argument)
                                    (cons lisp arguments)
                                    " ")
                         :input nil
                         :output output-file
                         :error-output error-file
                         :if-output-exists :supersede
                         :if-error-output-exists :supersede)
       :success))))

(defun date ()
  "Return the current date in YYYYMMDD format."
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time (get-universal-time) 0)
    (declare (ignore se mi ho))
    (format nil "~4,'0D~2,'0D~2,'0D" ye mo da)))

(defun try-systems-in-directory (root-directory
                                 &key
                                   (asdf *asdf*)
                                   ((:reports-directory *reports-directory*) *reports-directory*))
  (setf (uiop:getenv "LC_CTYPE") "en_US.UTF-8")
  (loop
    :with results  := '()
    :with date     := (date)
    :with registry := (merge-pathnames (make-pathname :name "registry" :type "lisp" :case :local
                                                      :directory (list :relative date)
                                                      :defaults *reports-directory*)
                                       *reports-directory* nil)
    :for (asd-directory . systems) :in (find-asd-systems-in-directory root-directory)
      :initially (ensure-directories-exist registry)
                 (with-open-file (src registry :direction :output
                                               :if-does-not-exist :create
                                               :if-exists :supersede)
                   (let ((*print-pretty*   t)
                         (*print-readably* t))
                     (format src "(setf asdf:*central-registry* '~S)~%" (quicklisp-registry))))
    :do (loop
          :for asd-system :in systems
          :for success := (run date asd-system "load"
                               "sbcl"
                               "--noinform"
                               "--no-userinit"
                               "--non-interactive"
                               "--load" (namestring asdf)
                               "--load" (namestring registry)
                               ;; We cannot use prin1-to-string in case we don't have the same asdf version.
                               "--eval" (format nil "(let ((asdf:*compile-file-warnings-behaviour* :warn) (asdf:*compile-file-failure-behaviour* :error)) (asdf:oos 'asdf:load-op ~S))" asd-system))
          :do (push (list success asd-directory asd-system) results))
    :finally (loop :for (success nil asd-system) :in results
                   :when (and success (not (test-system-p `(defsystem ,asd-system))))
                     :do (run date asd-system "test"
                              "sbcl"
                              "--noinform"
                              "--no-userinit"
                              "--non-interactive"
                              "--load" (namestring asdf)
                              "--load" (namestring registry)
                              ;; We cannot use prin1-to-string in case we don't have the same asdf version.
                              "--eval" (format nil "(let ((asdf:*compile-file-warnings-behaviour* :warn) (asdf:*compile-file-failure-behaviour* :error)) (asdf:oos 'asdf:test-op ~S))" asd-system)))))



;; asdf:*compile-file-warnings-behaviour*
;; asdf:*compile-file-errors-behaviour*
;; control the handling of any such events.
;; The valid values for these variables are :error, :warn, and :ignore.





#-(and)
(run "sbcl"
     "--noinform"
     "--no-userinit"
     "--non-interactive"
     "--load" (namestring (merge-pathnames "quicklisp/asdf.lisp"
                                           (user-homedir-pathname) nil))
     "--eval" (prin1-to-string `(push ,asd-directory asdf:*central-registry*))
     "--eval" (prin1-to-string `(asdf:oos 'asdf:load-op ,asd-system)))

(let ((*package* (load-time-value (find-package "KEYWORD"))))
  (format t "~2%;; Usage:~2%~S~2%"
          '(try-systems-in-directory #P"~/src/public/lisp/")))

;;;; THE END ;;;;
