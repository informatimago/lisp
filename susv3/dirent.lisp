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
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-04-04 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************


(in-package "COMMON-LISP-USER")

(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "FFI"))

;; TODO: This nicknaming should be done outside of the sources.
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE:ADD-NICKNAME
   "COM.INFORMATIMAGO.CLISP.SUSV3"   "SUSV3")
  (COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE:ADD-NICKNAME
     "COM.INFORMATIMAGO.CLISP.SUSV3-XSI" "SUSV3-XSI"))

(DEFPACKAGE "COM.INFORMATIMAGO.SUSV3.DIRENT"
  (:DOCUMENTATION 
   "An API over SUSV3 and SUSV3-XSI dirent API.")
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.CLISP.RAW-MEMORY"
        "COM.INFORMATIMAGO.SUSV3.TOOLS"
        "FFI")
  (:import-from "COM.INFORMATIMAGO.CLISP.SUSV3"
                "CHECK-ERRNO"  "CHECK-POINTER")
  (:EXPORT "DIR" "DIRENT" "DIRENT-INO" "DIRENT-NAME"
           "OPENDIR" "CLOSEDIR" "READDIR" "REWINDDIR" 
           ;; XSI:
           "SEEKDIR" "TELLDIR" ))
(in-package "COM.INFORMATIMAGO.SUSV3.DIRENT")


(DEFTYPE DIR () 
  "A type representing a directory stream."
  `T)


(DEFSTRUCT DIRENT 
  (INO  0  :TYPE integer) ;; File serial number
  (NAME "" :TYPE string)) ;; Name of entry [NAME-MAX]



(DECLAIM
 (FTYPE (FUNCTION (DIR)         INTEGER)          CLOSEDIR)
 (FTYPE (FUNCTION (STRING)      (OR NULL DIR))    OPENDIR)
 (FTYPE (FUNCTION (DIR)         (OR NULL DIRENT)) READDIR)
 (FTYPE (FUNCTION (DIR)         NIL)              REWINDDIR))

(DECLAIM ;; XSI
 (FTYPE (FUNCTION (DIR INTEGER) NIL)         SEEKDIR)
 (FTYPE (FUNCTION (DIR)         INTEGER)     TELLDIR))


(define-ffi-copiers (dirent susv3:dirent dirent->c-dirent c-dirent->dirent)
    (dirent-ino    susv3::dirent-d_ino)
  (dirent-name   susv3::dirent-d_name))



(DEFUN OPENDIR  (PATH)        (check-pointer (susv3:opendir path)
                                             :function  'susv3:opendir
                                             :arguments (list path)
                                             :caller    'opendir))

(DEFUN CLOSEDIR (DIR-STREAM)  (check-errno   (susv3:closedir dir-stream)
                                             :function  'susv3:closedir
                                             :arguments (list dir-stream)
                                             :caller    'closedir))

(DEFUN READDIR (DIR-STREAM)
  (setf susv3:errno 0)
  (let ((c-dirent (check-pointer (susv3:readdir DIR-STREAM)
                                 :function  'susv3:readdir
                                 :arguments (list DIR-STREAM)
                                 :caller    'readdir)))
    ;; :no-error (list susv3:ENOENT))))
    (unless (zerop c-dirent)
      (let* ((ino   (peek-uint32 c-dirent))
             (name  (coerce (loop for i from 0 
                               for a from (+ c-dirent 11)
                               until (zerop (peek-uint8 a))
                               collect (code-char (peek-uint8 a))) 'string)))
        (make-dirent :ino ino :name name)))))


(DEFUN REWINDDIR (DIR-STREAM) (susv3:rewinddir DIR-STREAM))


(DEFUN SEEKDIR (DIR-STREAM POSITION)
  (CHECK-ERRNO (susv3:seekdir DIR-STREAM POSITION)
               :function  'susv3:seekdir
               :arguments (list DIR-STREAM position)
               :caller    'seekdir))
                                           
 
(DEFUN TELLDIR (DIR-STREAM)
  (CHECK-ERRNO (susv3:telldir DIR-STREAM)
               :function  'susv3:telldir
               :arguments (list DIR-STREAM)
               :caller    'telldir))


(DEFUN DIRENT-TEST ()
  (DO* ((DIR-STREAM (OPENDIR "/tmp"))
        (ENTRY (READDIR DIR-STREAM) (READDIR DIR-STREAM)))
       ((NULL ENTRY))
    (FORMAT T "entry: ~8D ~S~%" (DIRENT-INO ENTRY) (DIRENT-NAME ENTRY))))


;;;; THE END ;;;;
