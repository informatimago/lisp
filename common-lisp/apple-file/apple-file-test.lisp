;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               apple-file-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests apple-file.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from apple-file.lisp.
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.APPLE-FILE.APPLE-FILE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.APPLE-FILE.APPLE-FILE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.APPLE-FILE.APPLE-FILE"
                "APPLE-FILE-FORK-PATHNAME"
                "TREE-STRUCTURE-AND-LEAF-DIFFERENCE")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.APPLE-FILE.APPLE-FILE.TEST")


(define-test test/apple-file-fork-pathname ()
  #+unix
  (let ((*default-pathname-defaults* #P"/"))
    (assert-true
     (tree-structure-and-leaf-difference 
      (mapcar (lambda (format)
                (mapcar (lambda (fork)
                          (apple-file-fork-pathname (make-pathname :name "test" :type "single" :case :local)
                                                    format fork))
                        '(:info :data :resource)))
              '(:apple-single :apple-double :apple-triple))
      (list (list (make-pathname :name "test" :type "single" :case :local)
                  (make-pathname :name "test" :type "single" :case :local)
                  (make-pathname :name "test" :type "single" :case :local))
            (list (make-pathname :name "._test" :type "single" :case :local)
                  (make-pathname :name "test" :type "single" :case :local)
                  (make-pathname :name "._test" :type "single" :case :local))
            (list (make-pathname :name "test" :type "info" :case :local)
                  (make-pathname :name "test" :type "data" :case :local)
                  (make-pathname :name "test" :type "rsrc" :case :local)))
      :test 'pathname-match-p))))


(define-test test/all ()
  (test/apple-file-fork-pathname))


;;;; THE END ;;;;

