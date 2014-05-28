;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loads verrazano and tools.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-12-19 <PJB> Added this comment.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2007 - 2007
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

(unless (find-package :split-sequence)
  (asdf:operate 'asdf:load-op :split-sequence))

(unless (and (find-package :COM.INFORMATIMAGO.COMMON-LISP.SOURCE-FORM)
             (find-package :COM.INFORMATIMAGO.COMMON-LISP.utility))
  (asdf:operate 'asdf:load-op :COM.INFORMATIMAGO.COMMON-LISP))

(load "packages.lisp")
(load "c-syntax.lisp")

#-(and)
(progn 
   
  (defparameter *base-dir*
    (or *load-pathname*
        "/home/pjb/src/pjb/linc/"))

  (defparameter *install-dir* (format nil "~Ainstall/" *base-dir*))

  #+(or)
  (ext:shell (format nil "cd cmake-2.2.1 ; ./configure --prefix ~S && make && make install" *install-dir*))

  #+(or)
  (ext:shell (format nil "cd gccxml ; ~A~:*/bin/cmake -DCMAKE_INSTALL_PREFIX:PATH=~A && make && make install"  *install-dir*))



  ;; (load "asdf/asdf.lisp")

  (setf ASDF:*CENTRAL-REGISTRY*
        (nconc
         (sort
          (DELETE-DUPLICATES 
           (mapcar
            (lambda (d) (MAKE-PATHNAME :NAME NIL :TYPE NIL :VERSION NIL :DEFAULTS d))
            (directory "**/*.asd"))
           :test (function equal))
          (LAMBDA (A B) (if (= (length a) (length b))
                     (string< a b)
                     (< (length a) (length b))))
          :key (function namestring))
         ASDF:*CENTRAL-REGISTRY*))

  (asdf:operate 'asdf:load-op :cffi)
  (asdf:operate 'asdf:load-op :s-xml)
  (asdf:operate 'asdf:load-op :parse-number)
  (asdf:operate 'asdf:load-op :split-sequence)
  (asdf:operate 'asdf:load-op :verrazano-support)
  (asdf:operate 'asdf:load-op :verrazano)


  (ext:shell (format nil "~A~:*/bin/gccxml /usr/include/string.h -fxml=~A/../string.xml" *install-dir*))


  (defparameter *conf* (verrazano::make-configuration
                        :binding-name "test"
                        :binding-nicknames '()
                        :included-files '("/usr/include/string.h")
                        :gccxml-flags nil
                        :symbols-hidden '()
                        :overrides '()
                        :options '()
                        :output-filename (format nil "~Astring.what" *base-dir*)))

  (let ((cfg *conf*)(dbg? t))
    (let ((ir (verrazano::parse-gccxml-output cfg "string.xml" "string.mac")))
      (verrazano::simplify-ir ir cfg)
      (when dbg? 
        (verrazano::print-ir ir)
        (verrazano::print-class-bases ir)
        (verrazano::print-class-vtables ir))
      (let ((dq (verrazano::generate-definition-queue ir)))
        (inspect dq))))
  (verrazano::generate-package backend dq cfg)
  (when (not dbg?) (verrazano::cleanup temp-c temp-xml temp-mac))


  (verrazano::defbinding "test"
    (nicknames "str")
    (flags "")
    (include
     "/usr/include/string.h")
    (export "")
    (override "not supported yet"))





  (verrazano:create-binding
   *conf* filename outname backend &optional dbg?)
            
  ;;   (let ((cfg (parse-configuration-file filename outname))
  ;;         (temp-c (merge-pathnames "vzntemp.cpp" (working-directory bconf)))
  ;;         (temp-xml (merge-pathnames "vzntemp.xml" (working-directory bconf)))
  ;; 	(temp-mac (merge-pathnames "vzntemp.mac" (working-directory bconf))))
  ;;     (handler-case (generate-temporary-c-file cfg temp-c)
  ;;       (file-error () (error 'tempcpp-error)))
  ;;     (run-gccxml cfg (gccxml-path bconf) temp-c temp-xml temp-mac)
  ;;     (let ((ir (parse-gccxml-output cfg temp-xml temp-mac)))
  ;;       (simplify-ir ir cfg)
  ;;       (when dbg? 
  ;; 	(print-ir ir)
  ;; 	(print-class-bases ir)
  ;; 	(print-class-vtables ir))
  ;;       (let ((dq (generate-definition-queue ir)))
  ;; 	(generate-package backend dq cfg))
  ;;       (when (not dbg?) (cleanup temp-c temp-xml temp-mac)))))
  )
