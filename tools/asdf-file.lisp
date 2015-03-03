;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               asdf-file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Reads ASDF files.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-02 <PJB> Added generate-dot.
;;;;    2013-09-06 <PJB> Updated for publication.
;;;;    2012-04-09 <PJB> Created.
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "ASDF")
    (defpackage "ASDF" (:use "COMMON-LISP") (:export "DEFSYSTEM")))
  (unless (find-package "UIOP")
    (defpackage "UIOP" (:use "COMMON-LISP") (:export "SYMBOL-CALL"))))


(defpackage "COM.INFORMATIMAGO.TOOLS.ASDF-FILE"
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.TOOLS.DEPENDENCY-CYCLES"
        "COM.INFORMATIMAGO.TOOLS.SOURCE")
  (:shadow "DEPENDENCIES")
  (:export
   ;; Generating simple ASD files:
   "MAKE-COMPONENTS" 
   "GENERATE-ASD"
   ;; Reading and writing asd files:
   "READ-ASDF-SYSTEM-DEFINITIONS"
   "WRITE-ASDF-SYSTEM-DEFINITION"
   "SAVE-ASDF-SYSTEM-FILE"
   "DEFAULT-HEADERS-FOR-SYSTEM"
   ;; Generating test systems:
   "TEST-SYSTEM-FOR-SYSTEM"
   "TEST-SYSTEM-P"
   "TEST-SYSTEM-FOR-SYSTEM"
   "GENERATE-TEST-SYSTEM-FOR-SYSTEM-AT-PATH")
  (:documentation "

Reads simple .asd files, without instanciating ASDF objects.
============================================================

  (LOAD-SIMPLE-ASD-FILE path-to-asd-file)
  --> hashtable mapping file names to ASDF-FILE structures.

NOTE: The current implementation expects the defsystem form to be the
      first and only form in the asd file.


Generate simple .asd files:
============================================================

  (GENERATE-ASD :system-name (list \"source-1\" \"source-2\") \"lisp\"
                :description \"Short description\"
                :version \"1.0.0\"
                :author \"Name <email@address>\"
                :license \"AGPL3\"
                :predefined-packages '(\"COMMON-LISP\"))
                :implicit-dependencies '())
                :depends-on '(:other-system))
                :load-paths (list (make-pathname :directory '(:relative))))
                :vanillap t)

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2012 - 2014
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"))
(in-package "COM.INFORMATIMAGO.TOOLS.ASDF-FILE")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERATE-ASD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-components (paths &key (predefined-packages '("COMMON-LISP"))
                                (component-class :file)
                                (implicit-dependencies '())
                                (load-paths (list (make-pathname
                                                   :directory '(:relative)))))
  (mapcar
   (lambda (depend)
     (let* ((depend (mapcar (lambda (path) (pathname-name path)) depend))
            (target (first depend))
            (depends  (delete (first depend)
                              (append implicit-dependencies (rest depend))
                              :test (function string=))))
       (list* component-class target (when depends (list :depends-on depends)))))
   (get-depends paths predefined-packages load-paths)))


(defun gen-defsystem-form (name paths &key  description (version "0.0.0") 
                                        author maintainer licence license
                                        (component-class :file)
                                        (predefined-packages '("COMMON-LISP"))
                                        (implicit-dependencies '())
                                        (depends-on '())
                                        (load-paths (list (make-pathname
                                                           :directory '(:relative)))))
  "
DO:             Generate an ASD file for ASDF.
NAME:           Name of the generated ASDF system.
PATHS:          List of pathnames to the source files of this ASDF system.
DESCRIPTION:    A description string for the ASDF system.
VERSION:        A version string for the ASDF system.
AUTHOR:         An author string for the ASDF system.
LICENSE:        A licence string for the ASDF system.
PREDEFINED-PACKAGES:   A list of packages that are removed from the dependencies.
IMPLICIT-DEPENDENCIES: A list of dependencies added to all targets.
LOAD:-PATHS     A list of directory paths where the sources are searched in.
"
  (flet ((enumerate (list) (format nil "~{~A, ~}~:[none~;~1@*~{~A~^ and ~A~}~]"
                                   (butlast list 2) (last  list 2))))
    (let* ((headers (mapcar (lambda (path) (list* :path path
                                                  (with-open-file (stream path)
                                                    (read-source-header stream))))
                            paths))
           (authors (or author
                        (enumerate (delete-duplicates 
                                    (apply (function append)
                                           (mapcar (function header-authors)
                                                   headers))
                                    :test (function string-equal)))))
           (licence (or licence license
                        (enumerate (delete-duplicates
                                    (mapcar (function header-licence) headers)
                                    :test (function string-equal)))))
           (description
             (unsplit-string 
              (or (ensure-list description)
                  (mapcan
                   (lambda (header)
                     (append (list (format nil "~2%PACKAGE: ~A~2%"
                                           (second
                                            (get-package (header-slot header :path)))))
                             (mapcar (lambda (line) (format nil "~A~%" line))
                                     (header-description header))
                             (list (format nil "~%"))))
                   headers))
              " "))
           (components (make-components
                        paths
                        :component-class component-class
                        :predefined-packages (append depends-on
                                                     predefined-packages)
                        :implicit-dependencies implicit-dependencies
                        :load-paths load-paths)))
      `(asdf:defsystem ,name
         :description ,description
         :version     ,version
         :author      ,authors
         :maintainer  ,maintainer
         :licence     ,licence
         :depends-on  ,depends-on
         :components  ,components))))


(defun generate-asd (system-name sources source-type
                     &key description (version "0.0.0")
                     author licence license
                     (predefined-packages '("COMMON-LISP"))
                     (implicit-dependencies '())
                     (depends-on '())
                     (load-paths (list (make-pathname :directory '(:relative))))
                     (vanillap t))
  "
VANILLAP:  if true, then generate a simple, vanilla system.
           Otherwise, decorate it with PJB output-files.
"
  (let ((*package* (find-package :com.informatimago.tools.make-depends.make-depends)))
    (with-open-file (out (make-pathname :directory '(:relative)
                                        :name "system"
                                        ;;(string-downcase system-name)
                                        :type "asd" :version nil)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      #+(or)(push (truename (merge-pathnames
                             (make-pathname :directory '(:relative)
                                            :name nil :type nil :version nil)
                             out)) asdf::*central-registry*)
      (format out ";; -*- mode:lisp -*-~%")
      (mapc
       (lambda (sexp) (print sexp out) (terpri out))
       ;; Out to the asd file:
       (append
        (unless vanillap
          `((defpackage "COM.INFORMATIMAGO.ASDF" (:use "COMMON-LISP"))
            (in-package "COM.INFORMATIMAGO.ASDF")
            ;; ASDF imposes the file type classes to be
            ;; in the same package as the defsystem.
            (unless (handler-case (find-class 'pjb-cl-source-file) (t () nil))
              (defclass pjb-cl-source-file (asdf::cl-source-file) ())
              (flet ((output-files (c)
                       (flet ((implementation-id ()
                                (flet ((first-word (text)
                                         (let ((pos (position (character " ")
                                                              text)))
                                           (remove (character ".")
                                                   (if pos
                                                       (subseq text 0 pos)
                                                       text)))))
                                  (format
                                   nil "~A-~A-~A"
                                   (cond 
                                     ((string-equal
                                       "International Allegro CL Enterprise Edition"
                                       (lisp-implementation-type))
                                      "ACL")
                                     (t (first-word (lisp-implementation-type))))
                                   (first-word (lisp-implementation-version))
                                   (first-word (machine-type))))))
                         (let* ((object (compile-file-pathname
                                         (asdf::component-pathname c)))
                                (path (merge-pathnames
                                       (make-pathname
                                        :directory
                                        (list :relative
                                              (format nil "OBJ-~:@(~A~)"
                                                      (implementation-id)))
                                        :name (pathname-name object)
                                        :type (pathname-type object))
                                       object)))
                           (ensure-directories-exist path)
                           (list path)))))
                (defmethod asdf::output-files ((operation asdf::compile-op)
                                               (c pjb-cl-source-file))
                  (output-files c))
                (defmethod asdf::output-files ((operation asdf::load-op)
                                               (c pjb-cl-source-file))
                  (output-files c))))))
        
        `(,(gen-defsystem-form
            system-name
            (mapcar
             (lambda (source) (make-pathname :name (string-downcase (string source))
                                             :type source-type))
             sources)
            :description (or description
                             (format nil
                                     "This ASDF system gathers all the ~A packages."
                                     (string-upcase system-name)))
            :version version
            :author author
            :maintainer author
            :licence (or licence license)
            :component-class (if vanillap :cl-source-file :pjb-cl-source-file)
            :predefined-packages predefined-packages
            :implicit-dependencies implicit-dependencies
            :depends-on depends-on
            :load-paths load-paths)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reading and writing ASD files as sexps.
;;;

(defun read-asdf-system-definitions (stream)
  "
Reads an ASD file stream and return a list of asdf:defsystem forms
found.  DEFPACKAGE and IN-PACKAGE forms are evaluated.
"
  (let ((forms (read-source-code stream
                                 :test (lambda (sexp)
                                         (and (consp sexp)
                                              (eql (first sexp) 'asdf:defsystem))))))
    (cdr (assoc :test forms))))


(defun write-asdf-system-definition (stream defsystem-form)
  "Writes the defsystem-form to the STREAM."
  (pop defsystem-form)
  (with-standard-io-syntax
    (let ((name        (pop defsystem-form))
          (description (getf defsystem-form :description))
          (author      (getf defsystem-form :author))
          (maintainer  (getf defsystem-form :maintainer))
          (licence     (or (getf defsystem-form :license) (getf defsystem-form :licence)))
          (version     (or (getf defsystem-form :version) "1.0.0"))
          (properties  (getf defsystem-form :properties))
          (encoding    (getf defsystem-form :encoding))
          (depends-on  (getf defsystem-form :depends-on))
          (perform     (getf defsystem-form :perform))
          (in-order-to (getf defsystem-form :in-order-to))
          (components  (getf defsystem-form :components))
          (*print-pretty* t)
          (*print-case*   :downcase))
      (format stream "~&(asdf:defsystem ~S" name)
      (format stream "~&  ;; system attributes:")
      (format stream "~&  ~15S ~S" :description description)
      (format stream "~&  ~15S ~S" :author author)
      (format stream "~&  ~15S ~S" :maintainer (or author maintainer))
      (format stream "~&  ~15S ~S" :licence licence)
      (format stream "~&  ;; component attributes:")
      (format stream "~&  ~15S ~S" :version version)
      (format stream "~&  ~15S ~S" :properties properties)
      (when encoding
        (format stream "~&  #+asdf-unicode ~S #+asdf-unicode ~S" :encoding encoding))
      (format stream "~&  ~15S (~{~S~^~%~19<~>~})" :depends-on depends-on)
      (format stream "~&  ~15S (~{~S~^~%~19<~>~})" :components components)
      (when perform
       (format stream "~&  ~15S (~{~S~^~%~19<~>~})" :perform perform))
      (when in-order-to
       (format stream "~&  ~15S (~{~S~^~%~19<~>~})" :in-order-to in-order-to))
      (format stream ")~%")))
  defsystem-form)


(defun initials (name)
  (coerce (loop
            :for word :in (split-sequence #\space name :remove-empty-subseqs t)
            :while (alpha-char-p (aref word 0))
            :collect (aref word 0)) 'string))

(defun default-headers-for-system (pathname defsystem-form
                                   &key
                                     (default-author "Pascal J. Bourguignon")
                                     (default-email  "pjb@informatimago.com")
                                     (default-initials "PJB"))
  "
RETURN: A p-list containing a default source file header for the
        file at PATHNAME containing the DEFSYSTEM-FORM.
"
  (flet ((ref (key)
           (case key
             (:name (second defsystem-form))
             (otherwise (getf (cddr defsystem-form) key)))))
    (multiple-value-bind (se mi ho da mo ye)
        (decode-universal-time (get-universal-time))
      (declare (ignore se mi ho))
      (list
       :file (file-namestring pathname)
       :language "Common-Lisp"
       :system "None"
       :user-interface "None"
       :description (append
                     (list (format nil "This file defines the ~A system." (ref :name)))
                     (ensure-list (ref :description))
                     (and (ref :long-description)
                          (split-sequence #\Newline (ref :long-description))))
       :usage '()
       :authors (flet ((add-initials (name)
                         (format nil "<~A> ~A" (initials name) name)))
                  (if (ref :author)
                      (mapcar (function add-initials)
                              (if (ref :maintainer)
                                  (if (string-equal (ref :author) (ref :maintainer))
                                      (ensure-list (ref :author))
                                      (list (ref :author) (ref :maintainer)))
                                  (ensure-list (ref :author))))
                     (if (ref :maintainer)
                         (mapcar (function add-initials)
                                 (ensure-list (ref :maintainer)))
                         (list (format nil "<~A> ~A <~A>"
                                       default-initials
                                       default-author
                                       default-email)))))
       :modifications (list
                       (format nil "~4,'0D-~2,'0D-~2,'0D <~A> Created."
                               ye mo da default-initials))
       :bugs '()
       :legal (list
               "AGPL3"
               ""
               (format nil "Copyright ~A ~A - ~:*~A" default-author ye)
               ""
               "This program is free software: you can redistribute it and/or modify"
               "it under the terms of the GNU Affero General Public License as published by"
               "the Free Software Foundation, either version 3 of the License, or"
               "(at your option) any later version."
               ""
               "This program is distributed in the hope that it will be useful,"
               "but WITHOUT ANY WARRANTY; without even the implied warranty of"
               "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
               "GNU Affero General Public License for more details."
               ""
               "You should have received a copy of the GNU Affero General Public License"
               "along with this program.  If not, see <http://www.gnu.org/licenses/>")))))


(defun save-asdf-system-file (pathname defsystem-form
                              &key
                                (external-format :utf-8)
                                (emacs-head-variables '((:|mode| "lisp") (:|coding| "utf-8")))
                                (headers '()))
  "Saves the DEFSYSTEM-FORM into the ASD file at PATHNAME (superseded)."
  (with-open-file (stream pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :external-format external-format)
    (write-emacs-head-variables emacs-head-variables stream)
    (write-source-header (or headers
                             (default-headers-for-system pathname defsystem-form))
                         stream)
    (terpri stream)
    (write-asdf-system-definition stream defsystem-form)
    (format stream "~%;;;; THE END ;;;;~%")))



;; (defun test-system-asd-file-header (pathname def-tested-system)
;;   "Returns: headers for"
;;   (default-headers-for-system
;;    (file-namestring pathname)
;;    (list* 'asdf:defsystem (second tested-system)
;;           :description (list "This file defines a system to test the system"
;;                              (string tested-system))
;;           (cddr tested-system))))


(defun test-system-p (defsystem-form)
  "Predicate whether DEFSYSTEM-FORM defines a test system
ie. whether the system name ends in \".test\"."
  (let ((name  (string (second defsystem-form))))
    (suffixp ".test" name :test (function char-equal))))


(defun test-system-for-system (defsystem-form)
  "
RETURN: A defsystem form for a test system for the system defined by
        DEFSYSTEM-FORM.
"
  (flet ((ref (key)
           (case key
             (:name (string-downcase (second defsystem-form)))
             (otherwise (getf (cddr defsystem-form) key)))))
    (multiple-value-bind (se mi ho da mo ye)
        (decode-universal-time (get-universal-time))
      (declare (ignore se mi ho da))
      (let* ((author-email       "pjb@informatimago.com")
             (date               (format nil "~[Winter~;Spring~;Summer~;Automn~] ~D"
                                         (truncate mo 3) ye))
             (tested-system-name (ref :name))
             (test-system-name   (format nil "~A.test" tested-system-name))
             (output-directory   (format nil "/tmp/documentation/~A/" test-system-name)))
        `(asdf:defsystem ,test-system-name
           ;; system attributes:
           :description  ,(format nil "Tests the ~A system." tested-system-name)
           :long-description ,(or (ref :long-decription) (ref :decription))
           :author       ,(ref :author)
           :maintainer   ,(ref :maintainer)
           :licence      ,(or (ref :licence) (ref :license))
           ;; component attributes:
           :version      "1.0.0" ; ,(ref :version)
           :properties ((#:author-email                   . ,author-email)
                        (#:date                           . ,date)
                        ((#:albert #:output-dir)          . ,output-directory)
                        ((#:albert #:formats)             . ("docbook"))
                        ((#:albert #:docbook #:template)  . "book")
                        ((#:albert #:docbook #:bgcolor)   . "white")
                        ((#:albert #:docbook #:textcolor) . "black"))
           :encoding :utf-8
           :depends-on (,(ref :name)
                        "com.informatimago.common-lisp.cesarum") ; simple-test
           :perform (asdf:test-op (cl-user::operation cl-user::system)
                                  (declare (ignore cl-user::operation cl-user::system))
                                  ;; template:
                                  (let ((*package* (find-package "TESTED-PACKAGE")))
                                    (uiop:symbol-call "TESTED-PACKAGE" "TEST/ALL")))
           :components ((:file "source-test"   :depends-on ())))))))


(defun generate-test-system-for-system-at-path (asdf-system-pathname
                                                &key (verbose t))
  "
Writes asd files defining test systems for each system found in the
asdf file at ASDF-SYSTEM-PATHNAME, unless such a file already exists.
"
  (with-open-file (stream asdf-system-pathname)
    (when verbose
      (format *trace-output* "~&;; Reading system asd file ~A~%" asdf-system-pathname))
    (dolist (defsys (read-asdf-system-definitions stream))
      (if (test-system-p defsys)
          (when verbose
            (format *trace-output* "~&;;     Already a test system.~%"))
          (let* ((test-defsys (test-system-for-system defsys))
                 (test-pathname (merge-pathnames
                                 (make-pathname :name (string-downcase (second test-defsys))
                                                :type "asd"
                                                :version nil
                                                :case :local)
                                                 asdf-system-pathname
                                                 nil)))
            (if (probe-file test-pathname)
                (when verbose
                  (format *trace-output* "~&;;     Test system file ~A already exists.~%" test-pathname))
                (progn
                  (when verbose
                    (format *trace-output* "~&;; Generating test system asd file ~A~%" test-pathname))
                  (save-asdf-system-file test-pathname test-defsys
                                         :headers (default-headers-for-system test-pathname test-defsys)))))))))



#-(and) (progn

          (default-headers-for-system
           "/tmp/a.lisp"
           '(asdf:defsystem "com.informatimago.common-lisp.cesarum.test"
             ;; system attributes:
             :description  "Tests the cesarum library."
             :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
             :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
             :licence "AGPL3"
             ;; component attributes:
             :version "1.3.3"
             :properties ((#:author-email                   . "pjb@informatimago.com")
                          (#:date                           . "Winter 2015")
                          ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.cesarum-test/")
                          ((#:albert #:formats)             . ("docbook"))
                          ((#:albert #:docbook #:template)  . "book")
                          ((#:albert #:docbook #:bgcolor)   . "white")
                          ((#:albert #:docbook #:textcolor) . "black"))
             #+asdf-unicode :encoding #+asdf-unicode :utf-8
             :depends-on ("com.informatimago.common-lisp.cesarum")
             :perform (asdf:test-op (cl-user::o cl-user::s)
                       (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET")))
                         (uiop:symbol-call "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET"       "TEST/ALL"))
                       (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET")))
                         (uiop:symbol-call "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET" "TEST/ALL")))
             :components ((:file "set-test"       :depends-on ())
                          (:file "index-set-test" :depends-on ("set-test")))))

          (map nil (function generate-test-system-for-system-at-path)
            (directory #P "~/src/public/lisp/**/*.asd"))
          
          );;progn

;;;; THE END ;;;;
