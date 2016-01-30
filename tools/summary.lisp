;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              summary.lisp
;;;;LANGUAGE:          Common-Lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;
;;;;    This script generates HTML summaries of lisp packages.
;;;;
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-23 <PJB> Extracted from make-depends.lisp
;;;;    2003-05-04 <PJB> Converted to Common-Lisp from emacs.
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2002 - 2016
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
;;;;****************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML"))

#-mocl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (com.informatimago.common-lisp.cesarum.package:add-nickname
   "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML" "HTML"))

(defpackage "COM.INFORMATIMAGO.TOOLS.SUMMARY"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.CLEXT.CHARACTER-SETS"
        "COM.INFORMATIMAGO.TOOLS.SOURCE")
  (:export "GENERATE-SUMMARY")
  (:documentation "

This script generates HTML summaries of lisp packages.

USAGE

    (generate-summary '(\"read-source\") 
                      :repository-url (lambda (path)
                                         (format nil \"http://localhost/doc/~A.html\"
                                                 (translate-logical-pathname path))))

LICENSE

    AGPL3
    
    Copyright Pascal J. Bourguignon 2002 - 2015
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>
"))
(in-package "COM.INFORMATIMAGO.TOOLS.SUMMARY")


(defun generate-summary (sources &key (summary-path #p"SUMMARY.HTML")
                         (character-set "US-ASCII")
                         (source-type "LISP")
                         (verbose nil) (repository-url nil))
  "Generates a HTML summary of the sources"
  (assert (functionp repository-url) (repository-url)
          "REPOSITORY-URL must be a (function (pathname) string)")
  (let ((cs (etypecase character-set
              (character-set character-set)
              (string (find-character-set character-set))
              (symbol (find-character-set (string character-set))))))
    (unless cs (error "Cannot find the character set ~A" character-set))
    (with-open-file (html summary-path
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :external-format (character-set-to-lisp-encoding cs))
      (html:with-html-output (html :encoding cs)
        (html:doctype :transitional
          (html:comment "-*- coding:~A -*-"
                        (character-set-to-emacs-encoding cs))
          (html:html -
            (html:head -
              (html:title - (html:pcdata "Summary"))
              (html:meta
                  (list :http-equiv "Content-Type"
                        :content (format nil "text/html;charset=~A"
                                         (character-set-to-mime-encoding cs)))))
            (html:body -
              (dolist (source sources)
                (let* ((path             (make-pathname
                                          :name (string-downcase source)
                                          :type source-type
                                          :case :local))
                       (source-file     (get-source-file path))
                       (header          (source-file-header source-file))
                       (package (or (header-slot header :package)
                                    (first (source-file-packages-defined source-file))
                                    ;; for files without a package (eg emacs files)
                                    ;; we make a pseudo-package named as the file.
                                    (make-instance 'source-package
                                        :name (pathname-name path)
                                        :nicknames '()
                                        :documentation nil
                                        :use '()
                                        :shadow '()
                                        :shadowing-import-from '()
                                        :import-from '()
                                        :export '()
                                        :intern '()))))
                  (when verbose
                    (format *trace-output* ";; Processing ~S~%" source)
                    (format *trace-output* ";;   PATH    = ~S~%" path)
                    ;;(format *trace-output* ";;   HEADER  = ~S~%" header)
                    (format *trace-output* ";;   PACKAGE = ~S~%"
                            (source-package-name package))
                    (finish-output *trace-output*))
                  (unless (header-slot header :noweb)
                    (html:li -
                      (html:tt -
                        (html:b -
                          (html:a
                              (:href (funcall repository-url
                                              (com.informatimago.common-lisp.cesarum.package:package-pathname
                                               (source-package-name package))))
                            (html:pcdata "~A" (source-package-name package)))))
                      (html:pre -
                        (dolist (line (header-description header))
                          (html:cdata "~A~%" line))))))))))))))


;;;; THE END ;;;;

