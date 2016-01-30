;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               susv3.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             CLISP
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This packages exports SUSV3 functions.
;;;;    This is the CLISP specific implementation of the SUSV3 API.
;;;;
;;;;
;;;;    The Open Group Base Specifications Issue 6
;;;;    IEEE Std 1003.1, 2003 Edition
;;;;
;;;;    http://www.opengroup.org/onlinepubs/007904975/index.html
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2003-06-13 <PJB> Added dirent stuff.
;;;;    2003-05-13 <PJB> Created
;;;;BUGS
;;;;
;;;;    Check if the name is correct: there is a hierarchy of specifications
;;;;    in sus3. I want to avoid using #+XSI, but rather have different
;;;;    interfaces: (:USE SUSV3) (:USE SUSV3-XSI).
;;;;
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2016
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
(defpackage "COM.INFORMATIMAGO.CLISP.SUSV3"
  (:documentation "This packages exports SUSV3 functions.
    This is the CLISP specific implementation of the SUSV3 API.")
  (:use "COMMON-LISP"
        "EXT" "LINUX")
  (:export
   
   ;; NOT IN SUSV3 API (Lisp/C support stuff):
   "BOUND-STRING" ;; type (BOUND-STRING min max)
   "SUSV3-ERROR" ;; (SIGNAL 'SUSV3-ERROR errno)

   ;; 
   "GETENV"

   ;; sys/types.h
   "INO-T"
   

   ;; sys/stat.h

   
   ;; limits.h
   "+NAME-MAX+"

   ;; dirent.h
   "DIR" "DIRENT"
   "OPENDIR" "READDIR" "REWINDDIR" "CLOSEDIR"
   ;; readdir_r ;; TSF ;; not implemented, do we need it? 
   "SEEKDIR" "TELLDIR" ;; XSI



   
   ;; NOT IN SUSV3 API (TEST FUNCTIONS):
   "DIRENT-TEST"))
(in-package  "COM.INFORMATIMAGO.CLISP.SUSV3")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp/C support stuff


(deftype bound-string (min max)
  "A TYPE REPRESENTING STRINGS OF MINIMUM SIZE MIN AND MAXIMUM SIZE MAX."
  (if (= (eval min) (eval max))
    `(string ,(eval min))
    `string) ;; TODO: (OR (STRING MIN) (STRING (1+ MIN)) ... (STRING MAX))
  );;BOUND-STRING


(define-condition susv3-error ()
  (
   (errno :initarg :errno
          :accessor errno
          :type (signed-byte 32))
   ));;SUSV3-ERROR

  
(defmacro check-errno (&body body)
  `(progn
     (setq linux:|errno| 0)
     (let ((result (progn ,@body)))
       (if (/= 0 linux:|errno|)
         (signal (make-condition 'susv3-error  :errno linux:|errno|))
         result)))
  );;CHECK-ERRNO

     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ???


(declare (ftype (function (string) (or null string)) getenv))


(defun getenv (name)
  "
URL:        <http://www.opengroup.org/onlinepubs/007904975/functions/getenv.html>
RETURN:     NIL or the value of the environment variable named NAME.
"
  (ext:getenv name)
  );;GETENV


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sys/types.h


(deftype ino-t ()
  "The type of file serial numbers."
  `(unsigned-byte 32)
  );;INO-T


(deftype dev-t ()
  "Device ID."
  `(unsigned-byte 32)
  );;DEV-T


(deftype mode-t ()
  "Mode of file."
  `(unsigned-byte 32)
  );;MODE-T


(deftype nlink-t ()
  "Number of hard links to the file."
  `(unsigned-byte 32)
  );;NLINK-T


(deftype uid-t ()
  "User ID."
  `(unsigned-byte 32)
  );;UID-T


(deftype gid-t ()
  "Group ID."
  `(unsigned-byte 32)
  );;GID-T


(deftype time-t ()
  "Time in seconds since epoch."
  `(unsigned-byte 32)
  );;TIME-T



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sys/stat.h


(deftype blksize-t
  ""
  `(unsigned-byte 32)
  );;BLKSIZE-T


(deftype blkcnt-t
  ""
  `(unsigned-byte 32)
  );;BLKCNT-T


  
(defstruct stat
  (dev     0 :type dev-t) ;; Device ID of device containing file. 
  (ino     0 :type ino-t) ;; File serial number. 
  (mode    0 :type mode-t) ;; Mode of file (see below).
  (nlink   0 :type nlink-t) ;; Number of hard links to the file.
  (uid     0 :type uid-t) ;; User ID of file.
  (gid     0 :type gid-t) ;; Group ID of file.
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
  (blocks  0 :type blkcnt-t) ;; XSI: Num. of blocks allocated for this object.
  );;STAT


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

(define-symbol-macro s-iread s-irusr)
(define-symbol-macro s-iwrite s-iwusr)
(define-symbol-macro s-iexec s-ixusr)

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
 
;; S_TYPEISMQ(buf)
;; Test for a message queue.S_TYPEISSEM(buf)
;; Test for a semaphore.S_TYPEISSHM(buf)
;; Test for a shared memory object.
 
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
 
;; int    chmod(const char *, mode_t);
;; int    fchmod(int, mode_t);
;; int    fstat(int, struct stat *);
;; int    lstat(const char *restrict, struct stat *restrict);
;; int    mkdir(const char *, mode_t);
;; int    mkfifo(const char *, mode_t);
;; [XSI][Option Start]
;; int    mknod(const char *, mode_t, dev_t);
;; [Option End]
;; int    stat(const char *restrict, struct stat *restrict);
;; mode_t umask(mode_t);

(declare
 (ftype (function (string mode-t)  nil)    chmod)
 (ftype (function (integer mode-t) nil)    fchmod)
 (ftype (function (integer)        stat)   fstat)
 (ftype (function (string)         stat)   lstat)
 (ftype (function (string)         stat)   stat)
 (ftype (function (string mode-t)  nil)    mkdir)
 (ftype (function (string mode-t)  nil)    mkfifo)
 (ftype (function (mode-t)         mode-t) umask)
 )

(declare ;; XSI
 (ftype (function (string mode-t dev-t) nil) mknod)
)



(defun chmod (path mode)
  (check-errno (linux:|chmod| path mode))
  (values)
  );;CHMOD


(defun fchmod (fd mode)
  (check-errno (linux:|fchmod| fd mode))
  (values)
  );;FCHMOD


(defmacro linux-stat->susv3-stat (sb)
  "
PRIVATE
"
  `(make-stat 
    :dev (linux:|stat-st_dev| ,sb)
    :ino (linux:|stat-st_ino| ,sb)
    :mode (linux:|stat-st_mode| ,sb)
    :nlink (linux:|stat-st_nlink| ,sb)
    :uid (linux:|stat-st_uid| ,sb)
    :gid (linux:|stat-st_gid| ,sb)
    :rdev (linux:|stat-st_rdev| ,sb)
    :size (linux:|stat-st_size| ,sb)
    :atime (linux:|stat-st_atime| ,sb)
    :mtime (linux:|stat-st_mtime| ,sb)
    :ctime (linux:|stat-st_ctime| ,sb)
    :blksize (linux:|stat-st_blksize| ,sb)
    :blocks (linux:|stat-st_blocks| ,sb))
  );;LINUX-STAT->SUSV3-STAT


(defun stat (path)
    (linux-stat->susv3-stat (check-errno (linux:|stat| path)))
  );;STAT


(defun lstat (path)
    (linux-stat->susv3-stat (check-errno (linux:|lstat| path)))
  );;LSTAT


(defun fstat (fd)
    (linux-stat->susv3-stat (check-errno (linux:|fstat| fd)))
  );;FSTAT


(defun mkdir (path mode)
  (check-errno (linux:|mkdir| path mode))
  (values)
  );;MKDIR


(defun mkfifo (path mode)
  (check-errno (linux:|mkfifo| path mode))
  (values)
  );;MKFIFO


(defun umask (mode)
  (linux:|umask| mode)
  );;UMASK


  ;;XSI
(defun mknod (path mode device)
  (check-errno (linux:|mknod| path mode device))
  (values)
  );;MKNOD



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dirent.h

(defconstant +name-max+ 255)


(deftype dir () 
  "A type representing a directory stream."
  `t
  );;DIR


(defstruct dirent 
  (ino  0  :type ino-t) ;; File serial number
  (name "" :type (bound-string 0 +name-max+)) ;; Name of entry
  );;DIRENT


(declaim
 (ftype (function (dir)    integer)          closedir)
 (ftype (function (string) (or null dir))    opendir)
 (ftype (function (dir)    (or null dirent)) readdir)
 (ftype (function (dir)    nil)              rewinddir)
 )


(declaim ;; XSI
 (ftype (function (dir integer) nil)         seekdir)
 (ftype (function (dir)         integer)     telldir)
 )


(defun opendir (path)
  (check-errno (linux:|opendir| path))
  );;OPENDIR


(defun closedir (dir-stream)
  (check-errno (linux:|closedir| dir-stream))
  );;CLOSEDIR


(defun readdir (dir-stream)
  (let ((c-dirent (check-errno (linux:|readdir| dir-stream))))
    (and c-dirent
         (make-dirent :ino (linux::|dirent-d_ino| c-dirent)
                      :name (linux::|dirent-d_name| c-dirent))))
  );;READDIR


(defun rewinddir (dir-stream)
  (check-errno (linux:|rewinddir| dir-stream))
  (values)
  );;REWINDDIR



(defun seekdir (dir-stream position)
  (check-errno (linux:|seekdir| dir-stream position))
  (values)
  );;SEEKDIR


(defun telldir (dir-stream)
  (check-errno (linux:|telldir| dir-stream))
  );;TELLDIR




(defun dirent-test ()
  (do* ((dir-stream (opendir "/tmp"))
        (entry (readdir dir-stream) (readdir dir-stream)))
      ((null entry))
    (format t "entry: ~8D ~S~%" (dirent-ino entry) (dirent-name entry)))
  );;DIRENT-TEST



;;;; THE END ;;;;
