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
;;;;    <PJB> Pascal Bourguignon
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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2003
;;;;    mailto:pjb@informatimago.com
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


(defPACKAGE "COM.INFORMATIMAGO.CLISP.SUSV3"
  (:DOCUMENTATION "This packages exports SUSV3 functions.
    This is the CLISP specific implementation of the SUSV3 API.")
  (:use "COMMON-LISP"
        "EXT" "LINUX")
  (:EXPORT
   
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


(DEFTYPE BOUND-STRING (MIN MAX)
  "A TYPE REPRESENTING STRINGS OF MINIMUM SIZE MIN AND MAXIMUM SIZE MAX."
  (IF (= (EVAL MIN) (EVAL MAX))
    `(STRING ,(EVAL MIN))
    `STRING) ;; TODO: (OR (STRING MIN) (STRING (1+ MIN)) ... (STRING MAX))
  );;BOUND-STRING


(DEFINE-CONDITION SUSV3-ERROR ()
  (
   (ERRNO :INITARG :ERRNO
          :ACCESSOR ERRNO
          :TYPE (SIGNED-BYTE 32))
   ));;SUSV3-ERROR

  
(DEFMACRO CHECK-ERRNO (&BODY BODY)
  `(PROGN
     (SETQ LINUX:|errno| 0)
     (LET ((RESULT (PROGN ,@BODY)))
       (IF (/= 0 LINUX:|errno|)
         (SIGNAL (MAKE-CONDITION 'SUSV3-ERROR  :ERRNO LINUX:|errno|))
         RESULT)))
  );;CHECK-ERRNO

     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ???


(DECLARE (FTYPE (FUNCTION (STRING) (OR NULL STRING)) GETENV))


(DEFUN GETENV (NAME)
  "
URL:        http://www.opengroup.org/onlinepubs/007904975/functions/getenv.html
RETURN:     NIL or the value of the environment variable named NAME.
"
  (EXT:GETENV NAME)
  );;GETENV


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sys/types.h


(DEFTYPE INO-T ()
  "The type of file serial numbers."
  `(UNSIGNED-BYTE 32)
  );;INO-T


(DEFTYPE DEV-T ()
  "Device ID."
  `(UNSIGNED-BYTE 32)
  );;DEV-T


(DEFTYPE MODE-T ()
  "Mode of file."
  `(UNSIGNED-BYTE 32)
  );;MODE-T


(DEFTYPE NLINK-T ()
  "Number of hard links to the file."
  `(UNSIGNED-BYTE 32)
  );;NLINK-T


(DEFTYPE UID-T ()
  "User ID."
  `(UNSIGNED-BYTE 32)
  );;UID-T


(DEFTYPE GID-T ()
  "Group ID."
  `(UNSIGNED-BYTE 32)
  );;GID-T


(DEFTYPE TIME-T ()
  "Time in seconds since epoch."
  `(UNSIGNED-BYTE 32)
  );;TIME-T



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sys/stat.h


(DEFTYPE BLKSIZE-T
  ""
  `(UNSIGNED-BYTE 32)
  );;BLKSIZE-T


(DEFTYPE BLKCNT-T
  ""
  `(UNSIGNED-BYTE 32)
  );;BLKCNT-T


  
(DEFSTRUCT STAT
  (DEV     0 :TYPE DEV-T) ;; Device ID of device containing file. 
  (INO     0 :TYPE INO-T) ;; File serial number. 
  (MODE    0 :TYPE MODE-T) ;; Mode of file (see below).
  (NLINK   0 :TYPE NLINK-T) ;; Number of hard links to the file.
  (UID     0 :TYPE UID-T) ;; User ID of file.
  (GID     0 :TYPE GID-T) ;; Group ID of file.
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
  (BLOCKS  0 :TYPE BLKCNT-T) ;; XSI: Num. of blocks allocated for this object.
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

(DEFINE-SYMBOL-MACRO S-IREAD S-IRUSR)
(DEFINE-SYMBOL-MACRO S-IWRITE S-IWUSR)
(DEFINE-SYMBOL-MACRO S-IEXEC S-IXUSR)

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

(DECLARE
 (FTYPE (FUNCTION (STRING MODE-T)  NIL)    CHMOD)
 (FTYPE (FUNCTION (INTEGER MODE-T) NIL)    FCHMOD)
 (FTYPE (FUNCTION (INTEGER)        STAT)   FSTAT)
 (FTYPE (FUNCTION (STRING)         STAT)   LSTAT)
 (FTYPE (FUNCTION (STRING)         STAT)   STAT)
 (FTYPE (FUNCTION (STRING MODE-T)  NIL)    MKDIR)
 (FTYPE (FUNCTION (STRING MODE-T)  NIL)    MKFIFO)
 (FTYPE (FUNCTION (MODE-T)         MODE-T) UMASK)
 )

(DECLARE ;; XSI
 (FTYPE (FUNCTION (STRING MODE-T DEV-T) NIL) MKNOD)
)



(DEFUN CHMOD (PATH MODE)
  (CHECK-ERRNO (LINUX:|chmod| PATH MODE))
  (VALUES)
  );;CHMOD


(DEFUN FCHMOD (FD MODE)
  (CHECK-ERRNO (LINUX:|fchmod| FD MODE))
  (VALUES)
  );;FCHMOD


(DEFMACRO LINUX-STAT->SUSV3-STAT (SB)
  "
PRIVATE
"
  `(MAKE-STAT 
    :DEV (LINUX:|stat-st_dev| ,SB)
    :INO (LINUX:|stat-st_ino| ,SB)
    :MODE (LINUX:|stat-st_mode| ,SB)
    :NLINK (LINUX:|stat-st_nlink| ,SB)
    :UID (LINUX:|stat-st_uid| ,SB)
    :GID (LINUX:|stat-st_gid| ,SB)
    :RDEV (LINUX:|stat-st_rdev| ,SB)
    :SIZE (LINUX:|stat-st_size| ,SB)
    :ATIME (LINUX:|stat-st_atime| ,SB)
    :MTIME (LINUX:|stat-st_mtime| ,SB)
    :CTIME (LINUX:|stat-st_ctime| ,SB)
    :BLKSIZE (LINUX:|stat-st_blksize| ,SB)
    :BLOCKS (LINUX:|stat-st_blocks| ,SB))
  );;LINUX-STAT->SUSV3-STAT


(DEFUN STAT (PATH)
    (LINUX-STAT->SUSV3-STAT (CHECK-ERRNO (LINUX:|stat| PATH)))
  );;STAT


(DEFUN LSTAT (PATH)
    (LINUX-STAT->SUSV3-STAT (CHECK-ERRNO (LINUX:|lstat| PATH)))
  );;LSTAT


(DEFUN FSTAT (FD)
    (LINUX-STAT->SUSV3-STAT (CHECK-ERRNO (LINUX:|fstat| FD)))
  );;FSTAT


(DEFUN MKDIR (PATH MODE)
  (CHECK-ERRNO (LINUX:|mkdir| PATH MODE))
  (VALUES)
  );;MKDIR


(DEFUN MKFIFO (PATH MODE)
  (CHECK-ERRNO (LINUX:|mkfifo| PATH MODE))
  (VALUES)
  );;MKFIFO


(DEFUN UMASK (MODE)
  (LINUX:|umask| MODE)
  );;UMASK


  ;;XSI
(DEFUN MKNOD (PATH MODE DEVICE)
  (CHECK-ERRNO (LINUX:|mknod| PATH MODE DEVICE))
  (VALUES)
  );;MKNOD



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dirent.h

(DEFCONSTANT +NAME-MAX+ 255)


(DEFTYPE DIR () 
  "A type representing a directory stream."
  `T
  );;DIR


(DEFSTRUCT DIRENT 
  (INO  0  :TYPE INO-T) ;; File serial number
  (NAME "" :TYPE (BOUND-STRING 0 +NAME-MAX+)) ;; Name of entry
  );;DIRENT


(DECLAIM
 (FTYPE (FUNCTION (DIR)    INTEGER)          CLOSEDIR)
 (FTYPE (FUNCTION (STRING) (OR NULL DIR))    OPENDIR)
 (FTYPE (FUNCTION (DIR)    (OR NULL DIRENT)) READDIR)
 (FTYPE (FUNCTION (DIR)    NIL)              REWINDDIR)
 )


(DECLAIM ;; XSI
 (FTYPE (FUNCTION (DIR INTEGER) NIL)         SEEKDIR)
 (FTYPE (FUNCTION (DIR)         INTEGER)     TELLDIR)
 )


(DEFUN OPENDIR (PATH)
  (CHECK-ERRNO (LINUX:|opendir| PATH))
  );;OPENDIR


(DEFUN CLOSEDIR (DIR-STREAM)
  (CHECK-ERRNO (LINUX:|closedir| DIR-STREAM))
  );;CLOSEDIR


(DEFUN READDIR (DIR-STREAM)
  (LET ((C-DIRENT (CHECK-ERRNO (LINUX:|readdir| DIR-STREAM))))
    (AND C-DIRENT
         (MAKE-DIRENT :INO (LINUX::|dirent-d_ino| C-DIRENT)
                      :NAME (LINUX::|dirent-d_name| C-DIRENT))))
  );;READDIR


(DEFUN REWINDDIR (DIR-STREAM)
  (CHECK-ERRNO (LINUX:|rewinddir| DIR-STREAM))
  (VALUES)
  );;REWINDDIR



(DEFUN SEEKDIR (DIR-STREAM POSITION)
  (CHECK-ERRNO (LINUX:|seekdir| DIR-STREAM POSITION))
  (VALUES)
  );;SEEKDIR


(DEFUN TELLDIR (DIR-STREAM)
  (CHECK-ERRNO (LINUX:|telldir| DIR-STREAM))
  );;TELLDIR




(DEFUN DIRENT-TEST ()
  (DO* ((DIR-STREAM (OPENDIR "/tmp"))
        (ENTRY (READDIR DIR-STREAM) (READDIR DIR-STREAM)))
      ((NULL ENTRY))
    (FORMAT T "entry: ~8D ~S~%" (DIRENT-INO ENTRY) (DIRENT-NAME ENTRY)))
  );;DIRENT-TEST



;;;; THE END ;;;;
