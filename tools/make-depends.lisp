;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              make-depends.lisp
;;;;LANGUAGE:          Common-Lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;
;;;;    This script generates dependencies for lisp sources, based on 
;;;;    (require) sexps, a load-path, a set of logical pathname translations
;;;;    and ad-hoc processing.
;;;;    
;;;;    Object files can be either elisp compiled (.elc) or clisp compiled
;;;;    (.fas), cmucl compiled (.x86f), or sbcl compiled (.fasl).
;;;;    and source files can be either elisp (.el) or clisp or cmucl (.lisp,
;;;;    .lsp, .cl), and elisp sources may (require) common-lisp files
;;;;    (.lisp, .lsp, .cl extensions for sources, but .elc compiled form).
;;;;
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-22 <PJB> Moved MAKE-COMPONENTS MAKE-ASD-SEXP GENERATE-ASD to
;;;;                     com.informatimago.tools.asdf-file.
;;;;    2005-08-10 <PJB> Completed MAKE-ASD.
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

(defpackage "COM.INFORMATIMAGO.TOOLS.MAKE-DEPENDS"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.CLEXT.CHARACTER-SETS"
        "COM.INFORMATIMAGO.TOOLS.SOURCE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
                "SAFE-TEXT-FILE-TO-STRING-LIST")
  (:export "MAKE-DEPENDS")
  (:documentation
   "

This script generates dependencies for lisp sources, based on 
(require) sexps, a load-path, a set of logical pathname translations
and ad-hoc processing.

LICENSE:

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
(in-package "COM.INFORMATIMAGO.TOOLS.MAKE-DEPENDS")


(defun make-depends (object-files packages translations load-paths
                     &key (idf nil) (verbose nil))
  "
DO:             Finds the dependencies of the object files.
PACKAGES:       A list of names of the packages preloaded.
NOTE:           Since the list of preloaded packages depends on the lisp
                compiler, it should be given along with object files
                compiled by this compiler.  However, if it is known that
                only a common list of predefined package is used in the
                source packages, it can be given.
TRANSLATIONS:   A list of (CONS HOST TRANSLATIONS) that will be used to
                set the logical pathname translations. These translations
                are used first
NOTE:           We set the logical pathname translations only in here to avoid
                problems loading this program.
LOAD-PATHS:     A list of directory path names where to find the files when
                not found thru the logical pathname translations.
                The presence in LOAD-PATHS of a logical pathname warrants 
                the presence in HOST-LPT of an entry mapping it to a physical
                path.
IDF:            If NIL, write the dependencies on the standard output,
                else, write the dependencies of each object file into its
                own .d file.
"
  (mapc (lambda (args)
          (setf (logical-pathname-translations (car args)) (cdr args)))
        translations)
  (setq packages (mapcar (function string) packages))
  (dolist (object object-files)
    (when verbose
      (format *trace-output* "# Processing ~A~%" object))
    (let ((line (format nil "~A :: ~A" object
                        (unsplit-string
                         (mapcar
                          (lambda (item)
                            (if item
                                (string
                                 (namestring
                                  (translate-logical-pathname item)))
                                ""))
                          (get-dependencies object packages load-paths))
                         " "))))
      (if idf
          (with-open-file
              (out (merge-pathnames
                    (make-pathname
                     :name (concatenate 'string (pathname-name object)
                                        "." (pathname-type object))
                     :type "d")
                    object)
                   :direction :output
                   :if-exists :overwrite
                   :if-does-not-exist :create)
            (format out "~A~%" line))
          (format t "~A~%" line)))))


;;;; THE END ;;;;

