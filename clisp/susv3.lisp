;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               susv3.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             CLISP
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An implementation of SUSv3 for clisp.
;;;;
;;;;    The Open Group Base Specifications Issue 6
;;;;    IEEE Std 1003.1, 2003 Edition
;;;;
;;;;    http://www.opengroup.org/onlinepubs/007904975/index.html
;;;;
;;;;    Rules: 
;;;;       - The various scalar types are all mapped to INTEGER.
;;;;         [There is a multitude of scalar type declaration in 
;;;;         the C POSIX API (pid_t, gid_t, mode_t, etc) because
;;;;         C has modulo integers. Lisp have true integers, so 
;;;;         they're not useful.]
;;;;         http://www.opengroup.org/onlinepubs/009695399/xrat/xsh_chap02.html#tag_03_02_12
;;;;       - symbol are upcased, underlines replaced with dash,
;;;;         structure field prefixes are removed. 
;;;;         Constant names are NOT decorated by any '+'.
;;;;       - pointers: addresses are returned as integers.
;;;;         [It's easiest to keep addresses as integers instead of
;;;;         fighting with the various FFI notions of a pointer.]
;;;;       - errors are reported as result/errno.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-06-30 <PJB> Improved selection of libc library.
;;;;    2004-12-12 <PJB> Added getpid, fork, etc...
;;;;    2003-06-13 <PJB> Added dirent stuff.
;;;;    2003-05-13 <PJB> Created.
;;;;BUGS
;;;;
;;;;    Check if the name is correct: there is a hierarchy of specifications
;;;;    in sus3. I want to avoid using #+XSI, but rather have different
;;;;    interfaces: (:USE SUSV3) (:USE SUSV3-XSI).
;;;;
;;;;    Actually, we should include the features only if it's proven to exist
;;;;    on the current system. At run-time.
;;;;
;;;;    The type of arguments and results of FFI function should be pure
;;;;    Common-Lisp objects. We shouldn't need to do any FFI stuff outside 
;;;;    of here.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2013
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
(declaim (also-use-packages "EXT" "FFI" "LINUX"))
(eval-when (:compile-toplevel :load-toplevel :execute) (require "linux"))
(defpackage "COM.INFORMATIMAGO.CLISP.SUSV3"
  (:documentation "
    This packages exports SUSV3 functions.
    This is the CLISP specific implementation of the SUSV3 API.

    Copyright Pascal J. Bourguignon 2003 - 2004

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
")
  (:use "COMMON-LISP")
  (:export
   "UNIX-ERROR" "UNIX-ERROR-NUMBER" "UNIX-ERROR-MESSAGE"
   "UNIX-ERROR-FUNCTION" "UNIX-ERROR-ARGUMENTS" "UNIX-ERROR-CALLER"
   "CHECK-POINTER" "CHECK-ERRNO" "REPORT-ERROR"
   ;; 
   "GETENV"
   
   ;; limits.h
   "NAME-MAX"

   "STAT-DEV" "STAT-INO" "STAT-MODE" "STAT-NLINK" "STAT-UID"
   "STAT-GID" "STAT-RDEV" "STAT-SIZE" "STAT-ATIME" "STAT-MTIME"
   "STAT-CTIME" "STAT-BLKSIZE" "STAT-BLOCKS"

   "CHMOD" "FCHMOD" "STAT" "LSTAT" "FSTAT" "MKDIR" "MKFIFO" 
   "UMASK" "MKNOD"

   "S-ISUID" "S-ISGID" "S-ISVTX" "S-IREAD" "S-IWRITE" "S-IEXEC"
   "S-IRUSR" "S-IWUSR" "S-IXUSR" "S-IRWXU" "S-IRGRP" "S-IWGRP" "S-IXGRP"
   "S-IRWXG" "S-IROTH" "S-IWOTH" "S-IXOTH" "S-IRWXO"
   "S-IFMT"
   "S-IFDIR" "S-IFCHR" "S-IFBLK" "S-IFREG" "S-IFIFO"  "S-IFLNK" "S-IFSOCK" 
   "S-ISDIR" "S-ISCHR" "S-ISBLK" "S-ISREG" "S-ISFIFO" "S-ISLNK" "S-ISSOCK"

   ;; dirent.h
   "DIR" "DIRENT" "DIRENT-INO" "DIRENT-NAME"
   "OPENDIR" "READDIR" "REWINDDIR" "CLOSEDIR"
   ;; readdir_r ;; TSF ;; not implemented, do we need it? 
   "SEEKDIR" "TELLDIR" ;; XSI


   "GETPID" "FORK" 

   "ERRNO" "STRERROR"

   "EPERM" "ENOENT" "ESRCH" "EINTR" "EIO" "ENXIO" "E2BIG" "ENOEXEC"
   "EBADF" "ECHILD" "EAGAIN" "ENOMEM" "EACCES" "EFAULT" "ENOTBLK" "EBUSY"
   "EEXIST" "EXDEV" "ENODEV" "ENOTDIR" "EISDIR" "EINVAL" "ENFILE"
   "EMFILE" "ENOTTY" "ETXTBSY" "EFBIG" "ENOSPC" "ESPIPE" "EROFS" "EMLINK"
   "EPIPE" "EDOM" "ERANGE" "EDEADLK" "ENAMETOOLONG" "ENOLCK" "ENOSYS"
   "ENOTEMPTY" "ELOOP" "EWOULDBLOCK" "ENOMSG" "EIDRM" "ECHRNG" "EL2NSYNC"
   "EL3HLT" "EL3RST" "ELNRNG" "EUNATCH" "ENOCSI" "EL2HLT" "EBADE" "EBADR"
   "EXFULL" "ENOANO" "EBADRQC" "EBADSLT" "EDEADLOCK" "EBFONT" "ENOSTR"
   "ENODATA" "ETIME" "ENOSR" "ENONET" "ENOPKG" "EREMOTE" "ENOLINK" "EADV"
   "ESRMNT" "ECOMM" "EPROTO" "EMULTIHOP" "EDOTDOT" "EBADMSG" "EOVERFLOW"
   "ENOTUNIQ" "EBADFD" "EREMCHG" "ELIBACC" "ELIBBAD" "ELIBSCN" "ELIBMAX"
   "ELIBEXEC" "EILSEQ" "ERESTART" "ESTRPIPE" "EUSERS" "ENOTSOCK"
   "EDESTADDRREQ" "EMSGSIZE" "EPROTOTYPE" "ENOPROTOOPT" "EPROTONOSUPPORT"
   "ESOCKTNOSUPPORT" "EOPNOTSUPP" "EPFNOSUPPORT" "EAFNOSUPPORT"
   "EADDRINUSE" "EADDRNOTAVAIL" "ENETDOWN" "ENETUNREACH" "ENETRESET"
   "ECONNABORTED" "ECONNRESET" "ENOBUFS" "EISCONN" "ENOTCONN" "ESHUTDOWN"
   "ETOOMANYREFS" "ETIMEDOUT" "ECONNREFUSED" "EHOSTDOWN" "EHOSTUNREACH"
   "EALREADY" "EINPROGRESS" "ESTALE" "EUCLEAN" "ENOTNAM" "ENAVAIL"
   "EISNAM" "EREMOTEIO" "EDQUOT" "ENOMEDIUM" "EMEDIUMTYPE"

   "POINTER"
   
   ;; NOT IN SUSV3 API (TEST FUNCTIONS):
   "DIRENT-TEST"))
(in-package  "COM.INFORMATIMAGO.CLISP.SUSV3")



(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO: Actually, we should include the features only if it's proven to exist on the current system. At run-time.
  (pushnew :susv3 *features*))

(ffi:default-foreign-library "libc.so.6")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; If we want to have only Lisp type in the SUSV3 API, we cannot use
  ;; FFI:C-POINTER for addresses.
  ;; Internal routines can convert integers between FFI:C-POINTER
  ;; with FFI:UNSIGNED-FOREIGN-ADDRESS and FFI:FOREIGN-ADDRESS-UNSIGNED.
  (ffi:def-c-type pointer ffi:ulong))




(define-condition unix-error (error)
  ((number    :type     integer
              :initarg  :number
              :accessor unix-error-number)
   (message   :type     string
              :initarg  :message  
              :accessor unix-error-message)
   (function  :type     (or string symbol)
              :initarg  :function 
              :accessor unix-error-function)
   (arguments :type     list
              :initarg  :arguments
              :accessor unix-error-arguments)
   (caller    :type     symbol
              :initform nil
              :initarg  :caller
              :accessor unix-error-caller))
  (:report report-error)) ;;unix-error


(defgeneric report-error (condition  &optional stream))


(defmethod report-error ((condition unix-error) &optional (stream t))
  (format stream "[~D] Unix error ~D~A: ~A~:[~;, in ~:*~A~]"
          (getpid)
          (unix-error-number condition)
          (if (unix-error-function condition)
              (format nil " from (~S~{ ~S~})" 
                      (unix-error-function  condition)
                      (unix-error-arguments condition))
              "")
          (unix-error-message condition)
          (unix-error-caller condition))) ;;report-error


(define-symbol-macro errno linux:|errno|)
(defun getpid   ()      (linux:|getpid|))
(defun fork     ()      (linux:|fork|))
(defun strerror (errno) (linux:|strerror| errno))


(defun check-pointer (result &key function arguments caller (no-error '()))
  "
A NULL result means there's an error.
The error message is retrieved automatically from the errno.
The argument no-error is a list containing the values of errno that won't
be signaled (but check-pointer returns (values nil :errno errno) instead).
"
  (if (not (zerop result))
      (values result :result)
      (let ((errno errno))
        (if (or (zerop errno) (member errno no-error))
            (values 0 :errno errno)
            (error (make-condition
                    'unix-error 
                    :number errno
                    :message (strerror errno)
                    :function function
                    :arguments arguments
                    :caller caller))))))


(defun check-errno (result &key function arguments caller (no-error '()))
  "
A result = -1 means there's an error.
The error message is retrieved automatically from the errno.
The argument no-error is a list containing the values of errno that won't
be signaled (but that check-errno returns instead of nil).
"
  (if (/= -1 result)
      (values result :result)
      (let ((errno errno))
        (if (or (zerop  errno) (member errno no-error)) 
            (values errno :errno)
            (error (make-condition
                    'unix-error 
                    :number errno
                    :message (strerror errno)
                    :function function
                    :arguments arguments
                    :caller caller))))))


;; ---------------------------- <asm/errno.h> ----------------------------------

(defconstant eperm           linux:|EPERM|)  ; Operation not permitted
(defconstant enoent          linux:|ENOENT|) ; No such file or directory
(defconstant esrch           linux:|ESRCH|)  ; No such process
(defconstant eintr           linux:|EINTR|)  ; Interrupted system call
(defconstant eio             linux:|EIO|)    ; I/O error
(defconstant enxio           linux:|ENXIO|) ; No such device or address
(defconstant e2big           linux:|E2BIG|) ; Arg list too long
(defconstant enoexec         linux:|ENOEXEC|) ; Exec format error
(defconstant ebadf           linux:|EBADF|)   ; Bad file number
(defconstant echild          linux:|ECHILD|)  ; No child processes
(defconstant eagain          linux:|EAGAIN|)  ; Try again
(defconstant enomem          linux:|ENOMEM|)  ; Out of memory
(defconstant eacces          linux:|EACCES|)  ; Permission denied
(defconstant efault          linux:|EFAULT|)  ; Bad address
(defconstant enotblk         linux:|ENOTBLK|) ; Block device required
(defconstant ebusy           linux:|EBUSY|)  ; Device or resource busy
(defconstant eexist          linux:|EEXIST|) ; File exists
(defconstant exdev           linux:|EXDEV|)  ; Cross-device link
(defconstant enodev          linux:|ENODEV|) ; No such device
(defconstant enotdir         linux:|ENOTDIR|) ; Not a directory
(defconstant eisdir          linux:|EISDIR|)  ; Is a directory
(defconstant einval          linux:|EINVAL|)  ; Invalid argument
(defconstant enfile          linux:|ENFILE|)  ; File table overflow
(defconstant emfile          linux:|EMFILE|)  ; Too many open files
(defconstant enotty          linux:|ENOTTY|)  ; Not a typewriter
(defconstant etxtbsy         linux:|ETXTBSY|) ; Text file busy
(defconstant efbig           linux:|EFBIG|)   ; File too large
(defconstant enospc          linux:|ENOSPC|) ; No space left on device
(defconstant espipe          linux:|ESPIPE|) ; Illegal seek
(defconstant erofs           linux:|EROFS|)  ; Read-only file system
(defconstant emlink          linux:|EMLINK|) ; Too many links
(defconstant epipe           linux:|EPIPE|)  ; Broken pipe
(defconstant edom            linux:|EDOM|) ; Math argument out of domain of func
(defconstant erange          linux:|ERANGE|) ; Math result not representable
(defconstant edeadlk         linux:|EDEADLK|) ; Resource deadlock would occur
(defconstant enametoolong    linux:|ENAMETOOLONG|) ; File name too long
(defconstant enolck          linux:|ENOLCK|) ; No record locks available
(defconstant enosys          linux:|ENOSYS|) ; Function not implemented
(defconstant enotempty       linux:|ENOTEMPTY|) ; Directory not empty
(defconstant eloop           linux:|ELOOP|) ; Too many symbolic links encountered
(defconstant ewouldblock     linux:|EWOULDBLOCK|) ; Operation would block
(defconstant enomsg          linux:|ENOMSG|) ; No message of desired type
(defconstant eidrm           linux:|EIDRM|)  ; Identifier removed
(defconstant echrng          linux:|ECHRNG|) ; Channel number out of range
(defconstant el2nsync        linux:|EL2NSYNC|) ; Level 2 not synchronized
(defconstant el3hlt          linux:|EL3HLT|)   ; Level 3 halted
(defconstant el3rst          linux:|EL3RST|)   ; Level 3 reset
(defconstant elnrng          linux:|ELNRNG|) ; Link number out of range
(defconstant eunatch         linux:|EUNATCH|) ; Protocol driver not attached
(defconstant enocsi          linux:|ENOCSI|) ; No CSI structure available
(defconstant el2hlt          linux:|EL2HLT|) ; Level 2 halted
(defconstant ebade           linux:|EBADE|)  ; Invalid exchange
(defconstant ebadr           linux:|EBADR|) ; Invalid request descriptor
(defconstant exfull          linux:|EXFULL|) ; Exchange full
(defconstant enoano          linux:|ENOANO|) ; No anode
(defconstant ebadrqc         linux:|EBADRQC|) ; Invalid request code
(defconstant ebadslt         linux:|EBADSLT|) ; Invalid slot
(defconstant edeadlock       linux:|EDEADLOCK|) ; File locking deadlock error
(defconstant ebfont          linux:|EBFONT|)    ; Bad font file format
(defconstant enostr          linux:|ENOSTR|)    ; Device not a stream
(defconstant enodata         linux:|ENODATA|)   ; No data available
(defconstant etime           linux:|ETIME|)     ; Timer expired
(defconstant enosr           linux:|ENOSR|) ; Out of streams resources
(defconstant enonet          linux:|ENONET|) ; Machine is not on the network
(defconstant enopkg          linux:|ENOPKG|) ; Package not installed
(defconstant eremote         linux:|EREMOTE|) ; Object is remote
(defconstant enolink         linux:|ENOLINK|) ; Link has been severed
(defconstant eadv            linux:|EADV|)    ; Advertise error
(defconstant esrmnt          linux:|ESRMNT|)  ; Srmount error
(defconstant ecomm           linux:|ECOMM|) ; Communication error on send
(defconstant eproto          linux:|EPROTO|)    ; Protocol error
(defconstant emultihop       linux:|EMULTIHOP|) ; Multihop attempted
(defconstant edotdot         linux:|EDOTDOT|)   ; RFS specific error
(defconstant ebadmsg         linux:|EBADMSG|)   ; Not a data message
(defconstant eoverflow       linux:|EOVERFLOW|) ; Value too large for defined data type
(defconstant enotuniq        linux:|ENOTUNIQ|) ; Name not unique on network
(defconstant ebadfd          linux:|EBADFD|) ; File descriptor in bad state
(defconstant eremchg         linux:|EREMCHG|) ; Remote address changed
(defconstant elibacc         linux:|ELIBACC|) ; Can not access a needed shared library
(defconstant elibbad         linux:|ELIBBAD|) ; Accessing a corrupted shared library
(defconstant elibscn         linux:|ELIBSCN|) ; .lib section in a.out corrupted
(defconstant elibmax         linux:|ELIBMAX|) ; Attempting to link in too many shared libraries
(defconstant elibexec        linux:|ELIBEXEC|) ; Cannot exec a shared library directly
(defconstant eilseq          linux:|EILSEQ|)   ; Illegal byte sequence
(defconstant erestart        linux:|ERESTART|) ; Interrupted system call should be restarted
(defconstant estrpipe        linux:|ESTRPIPE|) ; Streams pipe error
(defconstant eusers          linux:|EUSERS|)   ; Too many users
(defconstant enotsock        linux:|ENOTSOCK|) ; Socket operation on non-socket
(defconstant edestaddrreq    linux:|EDESTADDRREQ|) ; Destination address required
(defconstant emsgsize        linux:|EMSGSIZE|)     ; Message too long
(defconstant eprototype      linux:|EPROTOTYPE|) ; Protocol wrong type for socket
(defconstant enoprotoopt     linux:|ENOPROTOOPT|) ; Protocol not available
(defconstant eprotonosupport linux:|EPROTONOSUPPORT|) ; Protocol not supported
(defconstant esocktnosupport linux:|ESOCKTNOSUPPORT|) ; Socket type not supported
(defconstant eopnotsupp      linux:|EOPNOTSUPP|) ; Operation not supported on transport endpoint
(defconstant epfnosupport    linux:|EPFNOSUPPORT|) ; Protocol family not supported
(defconstant eafnosupport    linux:|EAFNOSUPPORT|) ; Address family not supported by protocol
(defconstant eaddrinuse      linux:|EADDRINUSE|) ; Address already in use
(defconstant eaddrnotavail   linux:|EADDRNOTAVAIL|) ; Cannot assign requested address
(defconstant enetdown        linux:|ENETDOWN|)      ; Network is down
(defconstant enetunreach     linux:|ENETUNREACH|) ; Network is unreachable
(defconstant enetreset       linux:|ENETRESET|) ; Network dropped connection because of reset
(defconstant econnaborted    linux:|ECONNABORTED|) ; Software caused connection abort
(defconstant econnreset      linux:|ECONNRESET|) ; Connection reset by peer
(defconstant enobufs         linux:|ENOBUFS|) ; No buffer space available
(defconstant eisconn         linux:|EISCONN|) ; Transport endpoint is already connected
(defconstant enotconn        linux:|ENOTCONN|) ; Transport endpoint is not connected
(defconstant eshutdown       linux:|ESHUTDOWN|) ; Cannot send after transport endpoint shutdown
(defconstant etoomanyrefs    linux:|ETOOMANYREFS|) ; Too many references: cannot splice
(defconstant etimedout       linux:|ETIMEDOUT|) ; Connection timed out
(defconstant econnrefused    linux:|ECONNREFUSED|) ; Connection refused
(defconstant ehostdown       linux:|EHOSTDOWN|)    ; Host is down
(defconstant ehostunreach    linux:|EHOSTUNREACH|) ; No route to host
(defconstant ealready        linux:|EALREADY|) ; Operation already in progress
(defconstant einprogress     linux:|EINPROGRESS|) ; Operation now in progress
(defconstant estale          linux:|ESTALE|)  ; Stale NFS file handle
(defconstant euclean         linux:|EUCLEAN|) ; Structure needs cleaning
(defconstant enotnam         linux:|ENOTNAM|) ; Not a XENIX named type file
(defconstant enavail         linux:|ENAVAIL|) ; No XENIX semaphores available
(defconstant eisnam          linux:|EISNAM|)  ; Is a named type file
(defconstant eremoteio       linux:|EREMOTEIO|) ; Remote I/O error
(defconstant edquot          linux:|EDQUOT|)    ; Quota exceeded
(defconstant enomedium       linux:|ENOMEDIUM|) ; No medium found
(defconstant emediumtype     linux:|EMEDIUMTYPE|) ; Wrong medium type




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp/C support stuff


(deftype bound-string (min max)
  "A TYPE REPRESENTING STRINGS OF MINIMUM SIZE MIN AND MAXIMUM SIZE MAX."
  (if (= (eval min) (eval max))
      `(string ,(eval min))
      `string)) ;; TODO: (OR (STRING MIN) (STRING (1+ MIN)) ... (STRING MAX)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ???


(declaim (ftype (function (string) (or null string)) getenv))


(defun getenv (name)
  "
URL:        <http://www.opengroup.org/onlinepubs/007904975/functions/getenv.html>
RETURN:     NIL or the value of the environment variable named NAME.
"
  (ext:getenv name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sys/types.h


(deftype ino-t ()
  "The type of file serial numbers."
  `(unsigned-byte 32))


(deftype dev-t ()
  "Device ID."
  `(unsigned-byte 32))


(deftype mode-t ()
  "Mode of file."
  `(unsigned-byte 32))


(deftype nlink-t ()
  "Number of hard links to the file."
  `(unsigned-byte 32))


(deftype uid-t ()
  "User ID."
  `(unsigned-byte 32))


(deftype gid-t ()
  "Group ID."
  `(unsigned-byte 32))


(deftype time-t ()
  "Time in seconds since epoch."
  `(unsigned-byte 32))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sys/stat.h


(deftype blksize-t ()
  ""
  `(unsigned-byte 32))


(deftype blkcnt-t ()
  ""
  `(unsigned-byte 32))



(defstruct stat
  (dev     0 :type dev-t) ;; Device ID of device containing file. 
  (ino     0 :type ino-t) ;; File serial number. 
  (mode    0 :type mode-t)  ;; Mode of file (see below).
  (nlink   0 :type nlink-t) ;; Number of hard links to the file.
  (uid     0 :type uid-t)   ;; User ID of file.
  (gid     0 :type gid-t)   ;; Group ID of file.
  (rdev    0 :type dev-t) ;; XSI: Device ID (if file is char or block special).
  (size    0 :type off-t) ;; For regular files, the file size in bytes. 
  ;;                      For symbolic links, the length in bytes of the 
  ;;                      pathname contained in the symbolic link. 
  ;;                      SHM: For a shared memory object, the length in bytes.
  ;;                      TYM: For a typed memory object, the length in bytes. 
  ;;                      For other file types, the use of this field is 
  ;;                      unspecified.
  (atime   0 :type time-t) ;; Time of last access.
  (mtime   0 :type time-t) ;; Time of last data modification.
  (ctime   0 :type time-t) ;; Time of last status change.
  (blksize 0 :type blksize-t) ;; XSI: A file system-specific preferred I/O 
  ;;                      block size for this object. In some file system 
  ;;                      types, this may vary from file to file.
  (blocks  0 :type blkcnt-t)) ;; XSI: Num. of blocks allocated for this object.)


;; The st_ino and st_dev fields taken together uniquely identify the
;; file within the system. The blkcnt_t, blksize_t, dev_t, ino_t,
;; mode_t, nlink_t, uid_t, gid_t, off_t, and time_t types shall be
;; defined as described in <sys/types.h> . Times shall be given in
;; seconds since the Epoch.

;; Unless otherwise specified, the structure members st_mode, st_ino,
;; st_dev, st_uid, st_gid, st_atime, st_ctime, and st_mtime shall have
;; meaningful values for all file types defined in IEEE Std
;; 1003.1-2001.

;; For symbolic links, the st_mode member shall contain meaningful
;; information, which can be used with the file type macros described
;; below, that take a mode argument. The st_size member shall contain
;; the length, in bytes, of the pathname contained in the symbolic
;; link. File mode bits and the contents of the remaining members of
;; the stat structure are unspecified. The value returned in the
;; st_size field shall be the length of the contents of the symbolic
;; link, and shall not count a trailing null if one is present.


;; The following symbolic names for the values of type mode_t shall
;; also be defined.

;; File type:
;; 
;; S_IFMT
;;     [XSI] [Option Start] Type of file.
;; 
;;     S_IFBLK
;;     Block special.S_IFCHR
;;     Character special.S_IFIFO
;;     FIFO special.S_IFREG
;;     Regular.S_IFDIR
;;     Directory.S_IFLNK
;;     Symbolic link.S_IFSOCK
;;     Socket. [Option End]

(defconstant s-ifmt  #o0170000)
(defconstant s-ifdir  #o040000)
(defconstant s-ifchr  #o020000)
(defconstant s-ifblk  #o060000)
(defconstant s-ifreg  #o100000)
(defconstant s-ififo  #o010000)
(defconstant s-iflnk  #o120000)
(defconstant s-ifsock #o140000)


;; File mode bits:
;; 
;; S_IRWXU
;;     Read, write, execute/search by owner.
;; 
;;     S_IRUSR
;;     Read permission, owner.S_IWUSR
;;     Write permission, owner.S_IXUSR
;;     Execute/search permission, owner.
;; S_IRWXG
;;     Read, write, execute/search by group.
;; 
;;     S_IRGRP
;;     Read permission, group.S_IWGRP
;;     Write permission, group.S_IXGRP
;;     Execute/search permission, group.
;; S_IRWXO
;;     Read, write, execute/search by others.
;; 
;;     S_IROTH
;;     Read permission, others.S_IWOTH
;;     Write permission, others.S_IXOTH
;;     Execute/search permission, others.
;; S_ISUID
;; Set-user-ID on execution.S_ISGID
;; Set-group-ID on execution.S_ISVTX
;; [XSI] [Option Start] On directories, restricted deletion flag. [Option End]

;; The bits defined by S_IRUSR, S_IWUSR, S_IXUSR, S_IRGRP, S_IWGRP,
;; S_IXGRP, S_IROTH, S_IWOTH, S_IXOTH, S_ISUID, S_ISGID, [XSI] [Option
;; Start]  and S_ISVTX [Option End]  shall be unique.

;; S_IRWXU is the bitwise-inclusive OR of S_IRUSR, S_IWUSR, and S_IXUSR.
;; 
;; S_IRWXG is the bitwise-inclusive OR of S_IRGRP, S_IWGRP, and S_IXGRP.
;; 
;; S_IRWXO is the bitwise-inclusive OR of S_IROTH, S_IWOTH, and S_IXOTH.

;; Implementations may OR other implementation-defined bits into
;; S_IRWXU, S_IRWXG, and S_IRWXO, but they shall not overlap any of
;; the other bits defined in this volume of IEEE Std 1003.1-2001. The
;; file permission bits are defined to be those corresponding to the
;; bitwise-inclusive OR of S_IRWXU, S_IRWXG, and S_IRWXO.


(defconstant s-isuid  #o004000)
(defconstant s-isgid  #o002000)
(defconstant s-isvtx  #o001000)

(define-symbol-macro s-iread  s-irusr)
(define-symbol-macro s-iwrite s-iwusr)
(define-symbol-macro s-iexec  s-ixusr)

(defconstant s-irusr  #o000400)
(defconstant s-iwusr  #o000200)
(defconstant s-ixusr  #o000100)
(defconstant s-irwxu  (logior s-irusr s-iwusr s-ixusr))
(defconstant s-irgrp  #o000040)
(defconstant s-iwgrp  #o000020)
(defconstant s-ixgrp  #o000010)
(defconstant s-irwxg  (logior s-irgrp s-iwgrp s-ixgrp))
(defconstant s-iroth  #o000004)
(defconstant s-iwoth  #o000002)
(defconstant s-ixoth  #o000001)
(defconstant s-irwxo  (logior s-iroth s-iwoth s-ixoth))


;; The following macros shall be provided to test whether a file is of
;; the specified type. The value m supplied to the macros is the value
;; of st_mode from a stat structure. The macro shall evaluate to a
;; non-zero value if the test is true; 0 if the test is false.

;; S_ISBLK(m)
;; 
;; Test for a block special file.S_ISCHR(m)
;; Test for a character special file.S_ISDIR(m)
;; Test for a directory.S_ISFIFO(m)
;; Test for a pipe or FIFO special file.S_ISREG(m)
;; Test for a regular file.S_ISLNK(m)
;; Test for a symbolic link.S_ISSOCK(m)
;; Test for a socket.

(defmacro s-isdir  (m) `(= (logand ,m s-ifmt) s-ifdir))
(defmacro s-ischr  (m) `(= (logand ,m s-ifmt) s-ifchr))
(defmacro s-isblk  (m) `(= (logand ,m s-ifmt) s-ifblk))
(defmacro s-isreg  (m) `(= (logand ,m s-ifmt) s-ifreg))
(defmacro s-isfifo (m) `(= (logand ,m s-ifmt) s-iffifo))
(defmacro s-islnk  (m) `(= (logand ,m s-ifmt) s-iflnk))
(defmacro s-issock (m) `(= (logand ,m s-ifmt) s-ifsock))


;; The implementation may implement message queues, semaphores, or
;; shared memory objects as distinct file types. The following macros
;; shall be provided to test whether a file is of the specified
;; type. The value of the buf argument supplied to the macros is a
;; pointer to a stat structure. The macro shall evaluate to a non-zero
;; value if the specified object is implemented as a distinct file
;; type and the specified file type is contained in the stat structure
;; referenced by buf. Otherwise, the macro shall evaluate to zero.

;; S_TYPEISMQ(buf)    Test for a message queue.
;; S_TYPEISSEM(buf)   Test for a semaphore.
;; S_TYPEISSHM(buf)   Test for a shared memory object.

;; [TYM] [Option Start] The implementation may implement typed memory
;; objects as distinct file types, and the following macro shall test
;; whether a file is of the specified type. The value of the buf
;; argument supplied to the macros is a pointer to a stat
;; structure. The macro shall evaluate to a non-zero value if the
;; specified object is implemented as a distinct file type and the
;; specified file type is contained in the stat structure referenced
;; by buf. Otherwise, the macro shall evaluate to zero.

;; S_TYPEISTMO(buf)
;; Test macro for a typed memory object.
;; [Option End]

;; The following shall be declared as functions and may also be
;; defined as macros. Function prototypes shall be provided.

;; int    chmod(const char *, mode_t)
;; int    fchmod(int, mode_t)
;; int    fstat(int, struct stat *)
;; int    lstat(const char *restrict, struct stat *restrict)
;; int    mkdir(const char *, mode_t)
;; int    mkfifo(const char *, mode_t)
;; [XSI][Option Start]
;; int    mknod(const char *, mode_t, dev_t)
;; [Option End]
;; int    stat(const char *restrict, struct stat *restrict)
;; mode_t umask(mode_t)

(declaim (ftype (function (string mode-t)  nil)    chmod))
(declaim (ftype (function (integer mode-t) nil)    fchmod))
(declaim (ftype (function (integer)        stat)   fstat))
(declaim (ftype (function (string)         stat)   lstat))
(declaim (ftype (function (string)         stat)   stat))
(declaim (ftype (function (string mode-t)  nil)    mkdir))
(declaim (ftype (function (string mode-t)  nil)    mkfifo))
(declaim (ftype (function (mode-t)         mode-t) umask))


(declaim ;; XSI
 '(ftype (function (string mode-t dev-t) nil) mknod))



(defun chmod (path mode)
  (check-errno (linux:|chmod| path mode))
  (values))


(defun fchmod (fd mode)
  (check-errno (linux:|fchmod| fd mode))
  (values))


(defmacro linux-stat->susv3-stat (sb)
  "
PRIVATE
"
  `(make-stat 
    :dev (linux::|stat-st_dev| ,sb)
    :ino (linux::|stat-st_ino| ,sb)
    :mode (linux::|stat-st_mode| ,sb)
    :nlink (linux::|stat-st_nlink| ,sb)
    :uid (linux::|stat-st_uid| ,sb)
    :gid (linux::|stat-st_gid| ,sb)
    :rdev (linux::|stat-st_rdev| ,sb)
    :size (linux::|stat-st_size| ,sb)
    :atime (linux::|stat-st_atime| ,sb)
    :mtime (linux::|stat-st_mtime| ,sb)
    :ctime (linux::|stat-st_ctime| ,sb)
    :blksize (linux::|stat-st_blksize| ,sb)
    :blocks (linux::|stat-st_blocks| ,sb)))


(defun stat (path)
  (linux-stat->susv3-stat (check-errno (linux:|stat| path))))


(defun lstat (path)
  (linux-stat->susv3-stat (check-errno (linux:|lstat| path))))


(defun fstat (fd)
  (linux-stat->susv3-stat (check-errno (linux:|fstat| fd))))


(defun mkdir (path mode)
  (check-errno (linux:|mkdir| path mode))
  (values))


(defun mkfifo (path mode)
  (check-errno (linux:|mkfifo| path mode))
  (values))


(defun umask (mode)
  (linux:|umask| mode))


;;XSI
(defun mknod (path mode device)
  (check-errno (linux:|mknod| path mode device))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dirent.h

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant name-max 255))


(ffi:def-c-type dirp   pointer)
(ffi:def-c-type ino_t  ffi:ulong)
(ffi:def-c-type off_t  ffi:long)


(ffi:def-c-struct dirent
    (d_ino       ino_t)
  (d_off       off_t)
  (d_reclen    ffi:ushort)
  (d_type      ffi:uchar)
  (d_name      (ffi:c-array ffi:char #.(1+ name-max))))
(defmacro dirent-name (d) `(dirent-d_name ,d))
(defmacro dirent-ino  (d) `(dirent-d_ino  ,d))

(ffi:def-call-out opendir (:name "opendir")
  (:arguments (name ffi:c-string))
  (:return-type dirp)
  (:language :stdc))

(ffi:def-call-out closedir (:name "closedir")
  (:arguments (dir dirp))
  (:return-type ffi:int)
  (:language :stdc))

(ffi:def-call-out readdir (:name "readdir")
  (:arguments (dir dirp))
  (:return-type pointer)
  (:language :stdc))

(ffi:def-call-out rewinddir (:name "rewinddir")
  (:arguments (dir dirp))
  (:return-type nil)
  (:language :stdc))

(ffi:def-call-out telldir (:name "telldir")
  (:arguments (dir dirp))
  (:return-type off_t)
  (:language :stdc))

(ffi:def-call-out seekdir (:name "seekdir")
  (:arguments (dir dirp) (offset off_t))
  (:return-type nil)
  (:language :stdc))


;;;; THE END ;;;;
