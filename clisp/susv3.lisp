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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2004
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

(cl:in-package "COMMON-LISP-USER")
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "EXT" "FFI" "LINUX"))
(defpackage "COM.INFORMATIMAGO.CLISP.SUSV3"
  (:DOCUMENTATION "
    This packages exports SUSV3 functions.
    This is the CLISP specific implementation of the SUSV3 API.

    Copyright Pascal J. Bourguignon 2003 - 2004

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
")
  (:USE "COMMON-LISP")
  (:EXPORT
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +libc+ "/lib/libc.so.6")

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

(defconstant EPERM           linux:|EPERM|)  ; Operation not permitted
(defconstant ENOENT          linux:|ENOENT|) ; No such file or directory
(defconstant ESRCH           linux:|ESRCH|)  ; No such process
(defconstant EINTR           linux:|EINTR|)  ; Interrupted system call
(defconstant EIO             linux:|EIO|)    ; I/O error
(defconstant ENXIO           linux:|ENXIO|) ; No such device or address
(defconstant E2BIG           linux:|E2BIG|) ; Arg list too long
(defconstant ENOEXEC         linux:|ENOEXEC|) ; Exec format error
(defconstant EBADF           linux:|EBADF|)   ; Bad file number
(defconstant ECHILD          linux:|ECHILD|)  ; No child processes
(defconstant EAGAIN          linux:|EAGAIN|)  ; Try again
(defconstant ENOMEM          linux:|ENOMEM|)  ; Out of memory
(defconstant EACCES          linux:|EACCES|)  ; Permission denied
(defconstant EFAULT          linux:|EFAULT|)  ; Bad address
(defconstant ENOTBLK         linux:|ENOTBLK|) ; Block device required
(defconstant EBUSY           linux:|EBUSY|)  ; Device or resource busy
(defconstant EEXIST          linux:|EEXIST|) ; File exists
(defconstant EXDEV           linux:|EXDEV|)  ; Cross-device link
(defconstant ENODEV          linux:|ENODEV|) ; No such device
(defconstant ENOTDIR         linux:|ENOTDIR|) ; Not a directory
(defconstant EISDIR          linux:|EISDIR|)  ; Is a directory
(defconstant EINVAL          linux:|EINVAL|)  ; Invalid argument
(defconstant ENFILE          linux:|ENFILE|)  ; File table overflow
(defconstant EMFILE          linux:|EMFILE|)  ; Too many open files
(defconstant ENOTTY          linux:|ENOTTY|)  ; Not a typewriter
(defconstant ETXTBSY         linux:|ETXTBSY|) ; Text file busy
(defconstant EFBIG           linux:|EFBIG|)   ; File too large
(defconstant ENOSPC          linux:|ENOSPC|) ; No space left on device
(defconstant ESPIPE          linux:|ESPIPE|) ; Illegal seek
(defconstant EROFS           linux:|EROFS|)  ; Read-only file system
(defconstant EMLINK          linux:|EMLINK|) ; Too many links
(defconstant EPIPE           linux:|EPIPE|)  ; Broken pipe
(defconstant EDOM            linux:|EDOM|) ; Math argument out of domain of func
(defconstant ERANGE          linux:|ERANGE|) ; Math result not representable
(defconstant EDEADLK         linux:|EDEADLK|) ; Resource deadlock would occur
(defconstant ENAMETOOLONG    linux:|ENAMETOOLONG|) ; File name too long
(defconstant ENOLCK          linux:|ENOLCK|) ; No record locks available
(defconstant ENOSYS          linux:|ENOSYS|) ; Function not implemented
(defconstant ENOTEMPTY       linux:|ENOTEMPTY|) ; Directory not empty
(defconstant ELOOP           linux:|ELOOP|) ; Too many symbolic links encountered
(defconstant EWOULDBLOCK     linux:|EWOULDBLOCK|) ; Operation would block
(defconstant ENOMSG          linux:|ENOMSG|) ; No message of desired type
(defconstant EIDRM           linux:|EIDRM|)  ; Identifier removed
(defconstant ECHRNG          linux:|ECHRNG|) ; Channel number out of range
(defconstant EL2NSYNC        linux:|EL2NSYNC|) ; Level 2 not synchronized
(defconstant EL3HLT          linux:|EL3HLT|)   ; Level 3 halted
(defconstant EL3RST          linux:|EL3RST|)   ; Level 3 reset
(defconstant ELNRNG          linux:|ELNRNG|) ; Link number out of range
(defconstant EUNATCH         linux:|EUNATCH|) ; Protocol driver not attached
(defconstant ENOCSI          linux:|ENOCSI|) ; No CSI structure available
(defconstant EL2HLT          linux:|EL2HLT|) ; Level 2 halted
(defconstant EBADE           linux:|EBADE|)  ; Invalid exchange
(defconstant EBADR           linux:|EBADR|) ; Invalid request descriptor
(defconstant EXFULL          linux:|EXFULL|) ; Exchange full
(defconstant ENOANO          linux:|ENOANO|) ; No anode
(defconstant EBADRQC         linux:|EBADRQC|) ; Invalid request code
(defconstant EBADSLT         linux:|EBADSLT|) ; Invalid slot
(defconstant EDEADLOCK       linux:|EDEADLOCK|) ; File locking deadlock error
(defconstant EBFONT          linux:|EBFONT|)    ; Bad font file format
(defconstant ENOSTR          linux:|ENOSTR|)    ; Device not a stream
(defconstant ENODATA         linux:|ENODATA|)   ; No data available
(defconstant ETIME           linux:|ETIME|)     ; Timer expired
(defconstant ENOSR           linux:|ENOSR|) ; Out of streams resources
(defconstant ENONET          linux:|ENONET|) ; Machine is not on the network
(defconstant ENOPKG          linux:|ENOPKG|) ; Package not installed
(defconstant EREMOTE         linux:|EREMOTE|) ; Object is remote
(defconstant ENOLINK         linux:|ENOLINK|) ; Link has been severed
(defconstant EADV            linux:|EADV|)    ; Advertise error
(defconstant ESRMNT          linux:|ESRMNT|)  ; Srmount error
(defconstant ECOMM           linux:|ECOMM|) ; Communication error on send
(defconstant EPROTO          linux:|EPROTO|)    ; Protocol error
(defconstant EMULTIHOP       linux:|EMULTIHOP|) ; Multihop attempted
(defconstant EDOTDOT         linux:|EDOTDOT|)   ; RFS specific error
(defconstant EBADMSG         linux:|EBADMSG|)   ; Not a data message
(defconstant EOVERFLOW       linux:|EOVERFLOW|) ; Value too large for defined data type
(defconstant ENOTUNIQ        linux:|ENOTUNIQ|) ; Name not unique on network
(defconstant EBADFD          linux:|EBADFD|) ; File descriptor in bad state
(defconstant EREMCHG         linux:|EREMCHG|) ; Remote address changed
(defconstant ELIBACC         linux:|ELIBACC|) ; Can not access a needed shared library
(defconstant ELIBBAD         linux:|ELIBBAD|) ; Accessing a corrupted shared library
(defconstant ELIBSCN         linux:|ELIBSCN|) ; .lib section in a.out corrupted
(defconstant ELIBMAX         linux:|ELIBMAX|) ; Attempting to link in too many shared libraries
(defconstant ELIBEXEC        linux:|ELIBEXEC|) ; Cannot exec a shared library directly
(defconstant EILSEQ          linux:|EILSEQ|)   ; Illegal byte sequence
(defconstant ERESTART        linux:|ERESTART|) ; Interrupted system call should be restarted
(defconstant ESTRPIPE        linux:|ESTRPIPE|) ; Streams pipe error
(defconstant EUSERS          linux:|EUSERS|)   ; Too many users
(defconstant ENOTSOCK        linux:|ENOTSOCK|) ; Socket operation on non-socket
(defconstant EDESTADDRREQ    linux:|EDESTADDRREQ|) ; Destination address required
(defconstant EMSGSIZE        linux:|EMSGSIZE|)     ; Message too long
(defconstant EPROTOTYPE      linux:|EPROTOTYPE|) ; Protocol wrong type for socket
(defconstant ENOPROTOOPT     linux:|ENOPROTOOPT|) ; Protocol not available
(defconstant EPROTONOSUPPORT linux:|EPROTONOSUPPORT|) ; Protocol not supported
(defconstant ESOCKTNOSUPPORT linux:|ESOCKTNOSUPPORT|) ; Socket type not supported
(defconstant EOPNOTSUPP      linux:|EOPNOTSUPP|) ; Operation not supported on transport endpoint
(defconstant EPFNOSUPPORT    linux:|EPFNOSUPPORT|) ; Protocol family not supported
(defconstant EAFNOSUPPORT    linux:|EAFNOSUPPORT|) ; Address family not supported by protocol
(defconstant EADDRINUSE      linux:|EADDRINUSE|) ; Address already in use
(defconstant EADDRNOTAVAIL   linux:|EADDRNOTAVAIL|) ; Cannot assign requested address
(defconstant ENETDOWN        linux:|ENETDOWN|)      ; Network is down
(defconstant ENETUNREACH     linux:|ENETUNREACH|) ; Network is unreachable
(defconstant ENETRESET       linux:|ENETRESET|) ; Network dropped connection because of reset
(defconstant ECONNABORTED    linux:|ECONNABORTED|) ; Software caused connection abort
(defconstant ECONNRESET      linux:|ECONNRESET|) ; Connection reset by peer
(defconstant ENOBUFS         linux:|ENOBUFS|) ; No buffer space available
(defconstant EISCONN         linux:|EISCONN|) ; Transport endpoint is already connected
(defconstant ENOTCONN        linux:|ENOTCONN|) ; Transport endpoint is not connected
(defconstant ESHUTDOWN       linux:|ESHUTDOWN|) ; Cannot send after transport endpoint shutdown
(defconstant ETOOMANYREFS    linux:|ETOOMANYREFS|) ; Too many references: cannot splice
(defconstant ETIMEDOUT       linux:|ETIMEDOUT|) ; Connection timed out
(defconstant ECONNREFUSED    linux:|ECONNREFUSED|) ; Connection refused
(defconstant EHOSTDOWN       linux:|EHOSTDOWN|)    ; Host is down
(defconstant EHOSTUNREACH    linux:|EHOSTUNREACH|) ; No route to host
(defconstant EALREADY        linux:|EALREADY|) ; Operation already in progress
(defconstant EINPROGRESS     linux:|EINPROGRESS|) ; Operation now in progress
(defconstant ESTALE          linux:|ESTALE|)  ; Stale NFS file handle
(defconstant EUCLEAN         linux:|EUCLEAN|) ; Structure needs cleaning
(defconstant ENOTNAM         linux:|ENOTNAM|) ; Not a XENIX named type file
(defconstant ENAVAIL         linux:|ENAVAIL|) ; No XENIX semaphores available
(defconstant EISNAM          linux:|EISNAM|)  ; Is a named type file
(defconstant EREMOTEIO       linux:|EREMOTEIO|) ; Remote I/O error
(defconstant EDQUOT          linux:|EDQUOT|)    ; Quota exceeded
(defconstant ENOMEDIUM       linux:|ENOMEDIUM|) ; No medium found
(defconstant EMEDIUMTYPE     linux:|EMEDIUMTYPE|) ; Wrong medium type




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp/C support stuff


(DEFTYPE BOUND-STRING (MIN MAX)
  "A TYPE REPRESENTING STRINGS OF MINIMUM SIZE MIN AND MAXIMUM SIZE MAX."
  (IF (= (EVAL MIN) (EVAL MAX))
      `(STRING ,(EVAL MIN))
      `STRING)) ;; TODO: (OR (STRING MIN) (STRING (1+ MIN)) ... (STRING MAX)))

     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ???


(DECLAIM (FTYPE (FUNCTION (STRING) (OR NULL STRING)) GETENV))


(DEFUN GETENV (NAME)
  "
URL:        http://www.opengroup.org/onlinepubs/007904975/functions/getenv.html
RETURN:     NIL or the value of the environment variable named NAME.
"
  (EXT:GETENV NAME))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sys/types.h


(DEFTYPE INO-T ()
  "The type of file serial numbers."
  `(UNSIGNED-BYTE 32))


(DEFTYPE DEV-T ()
  "Device ID."
  `(UNSIGNED-BYTE 32))


(DEFTYPE MODE-T ()
  "Mode of file."
  `(UNSIGNED-BYTE 32))


(DEFTYPE NLINK-T ()
  "Number of hard links to the file."
  `(UNSIGNED-BYTE 32))


(DEFTYPE UID-T ()
  "User ID."
  `(UNSIGNED-BYTE 32))


(DEFTYPE GID-T ()
  "Group ID."
  `(UNSIGNED-BYTE 32))


(DEFTYPE TIME-T ()
  "Time in seconds since epoch."
  `(UNSIGNED-BYTE 32))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sys/stat.h


(DEFTYPE BLKSIZE-T ()
  ""
  `(UNSIGNED-BYTE 32))


(DEFTYPE BLKCNT-T ()
  ""
  `(UNSIGNED-BYTE 32))


  
(DEFSTRUCT STAT
  (DEV     0 :TYPE DEV-T) ;; Device ID of device containing file. 
  (INO     0 :TYPE INO-T) ;; File serial number. 
  (MODE    0 :TYPE MODE-T)  ;; Mode of file (see below).
  (NLINK   0 :TYPE NLINK-T) ;; Number of hard links to the file.
  (UID     0 :TYPE UID-T)   ;; User ID of file.
  (GID     0 :TYPE GID-T)   ;; Group ID of file.
  (RDEV    0 :TYPE DEV-T) ;; XSI: Device ID (if file is char or block special).
  (SIZE    0 :TYPE OFF-T) ;; For regular files, the file size in bytes. 
  ;;                      For symbolic links, the length in bytes of the 
  ;;                      pathname contained in the symbolic link. 
  ;;                      SHM: For a shared memory object, the length in bytes.
  ;;                      TYM: For a typed memory object, the length in bytes. 
  ;;                      For other file types, the use of this field is 
  ;;                      unspecified.
  (ATIME   0 :TYPE TIME-T) ;; Time of last access.
  (MTIME   0 :TYPE TIME-T) ;; Time of last data modification.
  (CTIME   0 :TYPE TIME-T) ;; Time of last status change.
  (BLKSIZE 0 :TYPE BLKSIZE-T) ;; XSI: A file system-specific preferred I/O 
  ;;                      block size for this object. In some file system 
  ;;                      types, this may vary from file to file.
  (BLOCKS  0 :TYPE BLKCNT-T)) ;; XSI: Num. of blocks allocated for this object.)


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

(DEFCONSTANT S-IFMT  #O0170000)
(DEFCONSTANT S-IFDIR  #O040000)
(DEFCONSTANT S-IFCHR  #O020000)
(DEFCONSTANT S-IFBLK  #O060000)
(DEFCONSTANT S-IFREG  #O100000)
(DEFCONSTANT S-IFIFO  #O010000)
(DEFCONSTANT S-IFLNK  #O120000)
(DEFCONSTANT S-IFSOCK #O140000)


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


(DEFCONSTANT S-ISUID  #O004000)
(DEFCONSTANT S-ISGID  #O002000)
(DEFCONSTANT S-ISVTX  #O001000)

(DEFINE-SYMBOL-MACRO S-IREAD  S-IRUSR)
(DEFINE-SYMBOL-MACRO S-IWRITE S-IWUSR)
(DEFINE-SYMBOL-MACRO S-IEXEC  S-IXUSR)

(DEFCONSTANT S-IRUSR  #O000400)
(DEFCONSTANT S-IWUSR  #O000200)
(DEFCONSTANT S-IXUSR  #O000100)
(DEFCONSTANT S-IRWXU  (LOGIOR S-IRUSR S-IWUSR S-IXUSR))
(DEFCONSTANT S-IRGRP  #O000040)
(DEFCONSTANT S-IWGRP  #O000020)
(DEFCONSTANT S-IXGRP  #O000010)
(DEFCONSTANT S-IRWXG  (LOGIOR S-IRGRP S-IWGRP S-IXGRP))
(DEFCONSTANT S-IROTH  #O000004)
(DEFCONSTANT S-IWOTH  #O000002)
(DEFCONSTANT S-IXOTH  #O000001)
(DEFCONSTANT S-IRWXO  (LOGIOR S-IROTH S-IWOTH S-IXOTH))


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

(DEFMACRO S-ISDIR  (M) `(= (LOGAND ,M S-IFMT) S-IFDIR))
(DEFMACRO S-ISCHR  (M) `(= (LOGAND ,M S-IFMT) S-IFCHR))
(DEFMACRO S-ISBLK  (M) `(= (LOGAND ,M S-IFMT) S-IFBLK))
(DEFMACRO S-ISREG  (M) `(= (LOGAND ,M S-IFMT) S-IFREG))
(DEFMACRO S-ISFIFO (M) `(= (LOGAND ,M S-IFMT) S-IFFIFO))
(DEFMACRO S-ISLNK  (M) `(= (LOGAND ,M S-IFMT) S-IFLNK))
(DEFMACRO S-ISSOCK (M) `(= (LOGAND ,M S-IFMT) S-IFSOCK))


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

(DECLAIM (FTYPE (FUNCTION (STRING MODE-T)  NIL)    CHMOD))
(DECLAIM (FTYPE (FUNCTION (INTEGER MODE-T) NIL)    FCHMOD))
(DECLAIM (FTYPE (FUNCTION (INTEGER)        STAT)   FSTAT))
(DECLAIM (FTYPE (FUNCTION (STRING)         STAT)   LSTAT))
(DECLAIM (FTYPE (FUNCTION (STRING)         STAT)   STAT))
(DECLAIM (FTYPE (FUNCTION (STRING MODE-T)  NIL)    MKDIR))
(DECLAIM (FTYPE (FUNCTION (STRING MODE-T)  NIL)    MKFIFO))
(DECLAIM (FTYPE (FUNCTION (MODE-T)         MODE-T) UMASK))


(DECLAIM ;; XSI
 '(FTYPE (FUNCTION (STRING MODE-T DEV-T) NIL) MKNOD))



(DEFUN CHMOD (PATH MODE)
  (CHECK-ERRNO (LINUX:|chmod| PATH MODE))
  (VALUES))


(DEFUN FCHMOD (FD MODE)
  (CHECK-ERRNO (LINUX:|fchmod| FD MODE))
  (VALUES))


(DEFMACRO LINUX-STAT->SUSV3-STAT (SB)
  "
PRIVATE
"
  `(MAKE-STAT 
    :DEV (LINUX::|stat-st_dev| ,SB)
    :INO (LINUX::|stat-st_ino| ,SB)
    :MODE (LINUX::|stat-st_mode| ,SB)
    :NLINK (LINUX::|stat-st_nlink| ,SB)
    :UID (LINUX::|stat-st_uid| ,SB)
    :GID (LINUX::|stat-st_gid| ,SB)
    :RDEV (LINUX::|stat-st_rdev| ,SB)
    :SIZE (LINUX::|stat-st_size| ,SB)
    :ATIME (LINUX::|stat-st_atime| ,SB)
    :MTIME (LINUX::|stat-st_mtime| ,SB)
    :CTIME (LINUX::|stat-st_ctime| ,SB)
    :BLKSIZE (LINUX::|stat-st_blksize| ,SB)
    :BLOCKS (LINUX::|stat-st_blocks| ,SB)))


(DEFUN STAT (PATH)
  (LINUX-STAT->SUSV3-STAT (CHECK-ERRNO (LINUX:|stat| PATH))))


(DEFUN LSTAT (PATH)
  (LINUX-STAT->SUSV3-STAT (CHECK-ERRNO (LINUX:|lstat| PATH))))


(DEFUN FSTAT (FD)
  (LINUX-STAT->SUSV3-STAT (CHECK-ERRNO (LINUX:|fstat| FD))))


(DEFUN MKDIR (PATH MODE)
  (CHECK-ERRNO (LINUX:|mkdir| PATH MODE))
  (VALUES))


(DEFUN MKFIFO (PATH MODE)
  (CHECK-ERRNO (LINUX:|mkfifo| PATH MODE))
  (VALUES))


(DEFUN UMASK (MODE)
  (LINUX:|umask| MODE))


;;XSI
(DEFUN MKNOD (PATH MODE DEVICE)
  (CHECK-ERRNO (LINUX:|mknod| PATH MODE DEVICE))
  (VALUES))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dirent.h

(eval-when (:compile-toplevel :load-toplevel :execute)
  (DEFCONSTANT NAME-MAX 255))


(ffi:def-c-type dirp   pointer)
(ffi:def-c-type ino_t  ffi:ulong)
(ffi:def-c-type off_t  ffi:long)


(ffi:def-c-struct dirent
  (d_ino       ino_t)
  (d_off       off_t)
  (d_reclen    ffi:ushort)
  (d_type      ffi:uchar)
  (d_name      (ffi:c-array ffi:char #.(1+ NAME-MAX))))
(defmacro dirent-name (d) `(dirent-d_name ,d))
(defmacro dirent-ino  (d) `(dirent-d_ino  ,d))

(ffi:def-call-out opendir (:name "opendir")
  (:arguments (name ffi:c-string))
  (:return-type dirp)
  (:library #.+libc+) (:language :stdc))

(ffi:def-call-out closedir (:name "closedir")
  (:arguments (dir dirp))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))

(ffi:def-call-out readdir (:name "readdir")
  (:arguments (dir dirp))
  (:return-type pointer)
  (:library #.+libc+) (:language :stdc))

(ffi:def-call-out rewinddir (:name "rewinddir")
  (:arguments (dir dirp))
  (:return-type nil)
  (:library #.+libc+) (:language :stdc))

(ffi:def-call-out telldir (:name "telldir")
  (:arguments (dir dirp))
  (:return-type off_t)
  (:library #.+libc+) (:language :stdc))

(ffi:def-call-out seekdir (:name "seekdir")
  (:arguments (dir dirp) (offset off_t))
  (:return-type nil)
  (:library #.+libc+) (:language :stdc))


;;;; THE END ;;;;
