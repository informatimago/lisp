;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               susv3-mc3.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An implementation of SUSV3-MC3 for clisp.
;;;;
;;;;    Implemented:
;;;;        mmap/munmap
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-11-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2012
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
(declaim (declaration also-use-packages))
(declaim (also-use-packages "FFI" "LINUX"))
(eval-when (:compile-toplevel :load-toplevel :execute) (require "linux"))
(defpackage "COM.INFORMATIMAGO.CLISP.SUSV3-MC3"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.CLISP.SUSV3")
  (:export 
   "PROT-NONE" "PROT-READ" "PROT-WRITE" "PROT-EXEC" 
   "MAP-SHARED" "MAP-PRIVATE" "MAP-FIXED" "MAP-FILE"
   "MAP-ANONYMOUS" "MAP-GROWSDOWN" "MAP-DENYWRITE" 
   "MAP-EXECUTABLE" "MAP-LOCKED" "MAP-NORESERVE" 
   "MAP-FAILED" 
   "MMAP" "MUNMAP")
  (:documentation "
    An implementation of SUSV3-MC3 for clisp.

    Implemented:
        mmap/munmap
    
    Copyright Pascal J. Bourguignon 2004 - 2004

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
"))
(in-package "COM.INFORMATIMAGO.CLISP.SUSV3-MC3")

    
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO: Actually, we should include the features only if it's proven to exist on the current system. At run-time.
  (pushnew :susv3 *features*)
  (pushnew :susv3-mc3 *features*))



(ffi:default-foreign-library "libc.so.6")



(defconstant prot-none  0 "Page can not be accessed.")
(defconstant prot-read  1	"Page can be read.")
(defconstant prot-write 2 "Page can be written.")
(defconstant prot-exec  4 "Page can be executed.")

(defconstant map-shared       #x01 "Share changes. ")
(defconstant map-private      #x02 "Changes are private. ")
(defconstant map-fixed        #x10 "Interpret addr exactly. ")
(defconstant map-file            0)
(defconstant map-anonymous    #x20 "Don't use a file. ")
(defconstant map-growsdown  #x0100 "Stack-like segment. ")
(defconstant map-denywrite  #x0800 "ETXTBSY")
(defconstant map-executable #x1000 "Mark it as an executable. ")
(defconstant map-locked     #x2000 "Lock the mapping. ")
(defconstant map-noreserve  #x4000 "Don't check for reservations. ")

(defconstant map-failed     #xffffffff) ; -1


(ffi:def-c-type pointer ffi:ulong)


(ffi:def-call-out mmap  (:name "mmap")
  (:arguments (start pointer) (size ffi:size_t)
              (prot ffi:int) (flags ffi:int)
              (fd ffi:int) (offset linux:|off_t|))
  (:return-type pointer)
  (:language :stdc))


(ffi:def-call-out munmap  (:name "munmap")
  (:arguments  (start pointer) (size ffi:size_t))
  (:return-type pointer)
  (:language :stdc))

;;;; THE END ;;;;
