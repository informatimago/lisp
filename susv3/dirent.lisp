;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               dirent.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    SUSv3 dirent functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-04-04 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2016
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


(in-package "COMMON-LISP-USER")

(declaim (declaration also-use-packages)
         (also-use-packages "FFI"))

;; TODO: This nicknaming should be done outside of the sources.
#-mocl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (com.informatimago.common-lisp.cesarum.package:add-nickname
   "COM.INFORMATIMAGO.CLISP.SUSV3"   "SUSV3")
  (com.informatimago.common-lisp.cesarum.package:add-nickname
     "COM.INFORMATIMAGO.CLISP.SUSV3-XSI" "SUSV3-XSI"))

(defpackage "COM.INFORMATIMAGO.SUSV3.DIRENT"
  (:documentation 
   "An API over SUSV3 and SUSV3-XSI dirent API.")
  (:use "COMMON-LISP"
        ;; "COM.INFORMATIMAGO.CLISP.RAW-MEMORY"
        "COM.INFORMATIMAGO.SUSV3.TOOLS"
        "FFI")
  (:import-from "COM.INFORMATIMAGO.CLISP.SUSV3"
                "CHECK-ERRNO"  "CHECK-POINTER")
  (:export "DIR" "DIRENT" "DIRENT-INO" "DIRENT-NAME"
           "OPENDIR" "CLOSEDIR" "READDIR" "REWINDDIR" 
           ;; XSI:
           "SEEKDIR" "TELLDIR" ))
(in-package "COM.INFORMATIMAGO.SUSV3.DIRENT")


(deftype dir () 
  "A type representing a directory stream."
  `t)


(defstruct dirent 
  (ino  0  :type integer) ;; File serial number
  (name "" :type string)) ;; Name of entry [NAME-MAX]



(declaim
 (ftype (function (dir)         integer)          closedir)
 (ftype (function (string)      (or null dir))    opendir)
 (ftype (function (dir)         (or null dirent)) readdir)
 (ftype (function (dir)         nil)              rewinddir))

(declaim ;; XSI
 (ftype (function (dir integer) nil)         seekdir)
 (ftype (function (dir)         integer)     telldir))


(define-ffi-copiers (dirent susv3:dirent dirent->c-dirent c-dirent->dirent)
    (dirent-ino    susv3::dirent-d_ino)
  (dirent-name   susv3::dirent-d_name))



(defun opendir  (path)        (check-pointer (susv3:opendir path)
                                             :function  'susv3:opendir
                                             :arguments (list path)
                                             :caller    'opendir))

(defun closedir (dir-stream)  (check-errno   (susv3:closedir dir-stream)
                                             :function  'susv3:closedir
                                             :arguments (list dir-stream)
                                             :caller    'closedir))

(defun readdir (dir-stream)
  (setf susv3:errno 0)
  (let ((c-dirent (check-pointer (susv3:readdir dir-stream)
                                 :function  'susv3:readdir
                                 :arguments (list dir-stream)
                                 :caller    'readdir)))
    ;; :no-error (list susv3:ENOENT))))
    (unless (zerop c-dirent)
      (let* ((ino   (deref (cast (foreign-value c-dirent) '(pointer uint32))))
             (name  (coerce (loop
                              :for dirent = (cast (foreign-value c-dirent) '(pointer uchar))
                              :for i :from 11
                              :for code = (element (foreign-value dirent) i)
                              :until (zerop code)
                              :collect (code-char code)) 'string)))
        (make-dirent :ino ino :name name)))))


(defun rewinddir (dir-stream) (susv3:rewinddir dir-stream))


(defun seekdir (dir-stream position)
  (check-errno (susv3:seekdir dir-stream position)
               :function  'susv3:seekdir
               :arguments (list dir-stream position)
               :caller    'seekdir))
                                           
 
(defun telldir (dir-stream)
  (check-errno (susv3:telldir dir-stream)
               :function  'susv3:telldir
               :arguments (list dir-stream)
               :caller    'telldir))


(defun dirent-test ()
  (do* ((dir-stream (opendir "/tmp"))
        (entry (readdir dir-stream) (readdir dir-stream)))
       ((null entry))
    (format t "entry: ~8D ~S~%" (dirent-ino entry) (dirent-name entry))))


;;;; THE END ;;;;
