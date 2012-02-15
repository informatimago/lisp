;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               susv3-xsi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This packages exports SUSV3 XSI functions.
;;;;    This is the CLISP specific implementation of the SUSV3 XSI API.
;;;;
;;;;
;;;;    The Open Group Base Specifications Issue 6
;;;;    IEEE Std 1003.1, 2003 Edition
;;;;
;;;;    http://www.opengroup.org/onlinepubs/007904975/index.html
;;;;
;;;;
;;;;    Implemented:
;;;;        ftw
;;;;        msgget/msgctl/msgsnd/msgrcv
;;;;        shmget/shmctl/shmat/shmdt
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-11-29 <PJB> Added IPC.
;;;;    2003-06-18 <PJB> Created (FTW).
;;;;BUGS
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
;;;;    Copyright Pascal J. Bourguignon 2004 - 2004
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
(DECLAIM (DECLARATION ALSO-USE-PACKAGES))
(declaim (ALSO-USE-PACKAGES "FFI" "LINUX"))
(eval-when (:compile-toplevel :load-toplevel :execute) (require "linux"))
(defPACKAGE "COM.INFORMATIMAGO.CLISP.SUSV3-XSI"
  (:DOCUMENTATION "This packages exports SUSV3 XSI functions.
    This is the CLISP specific implementation of the SUSV3 XSI API.")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.CLISP.SUSV3")
  (:EXPORT 
   ;; FTW
   "+FTW-F+" "+FTW-D+" "+FTW-DNR+" "+FTW-DP+" "+FTW-NS+" "+FTW-SL+"
   "+FTW-SLN+" "+FTW-PHYS+" "+FTW-MOUNT+" "+FTW-DEPTH+" "+FTW-CHDIR+"
   "FTW" "FTW-FILTER" "NFTW-FILTER" "FTW" "+FTW-F+" "+FTW-D+" "+FTW-DNR+"
   "+FTW-DP+" "+FTW-NS+" "+FTW-SL+" "+FTW-SLN+"
   ;; IPC:
   "IPC_CREAT" "IPC_EXCL" "IPC_NOWAIT" "IPC_RMID" "IPC_SET" "IPC_STAT"
   "IPC_INFO" "IPC_PRIVATE" "IPC_PERM" "FTOK" 
   "MSG_NOERROR" "MSG_EXCEPT" "MSGQNUM_T" "MSGLEN_T" "MSQID_DS"
   "MSG_STAT" "MSG_INFO" "MSGINFO"
   "MSGGET" "MSGCTL" "MSGSND" "MSGRCV"
   "+MAX-MTEXT-SIZE+" "MSGBUF" "MAKE-MSGBUF" 
   "SHM_R" "SHM_W" "SHM_RDONLY" "SHM_RND" "SHM_REMAP" "SHM_LOCK"
   "SHM_UNLOCK" "GETPAGESIZE" "SHMLBA" "SHM_STAT" "SHM_INFO" "SHM_DEST" 
   "SHM_LOCKED" "SHM_HUGETLB" "SHMID_DS" "SHMGET" "SHMCTL" "SHMAT" "SHMDT"
   "SEM_UNDO" "GETPID" "GETVAL" "GETALL" "GETNCNT" "GETZCNT" "SETVAL"
   "SETALL" "SEMMNI" "SEMMSL" "SEMMNS" "SEMOPM" "SEMVMX" "SEMAEM"
   "SEMID_DS" "SEM_STAT" "SEM_INFO" "SEMINFO" "SEMBUF" 
   "SEMGET" "SEMCTL" "SEMOP"
   ;; // WARNING // WARNING // WARNING // WARNING // WARNING // WARNING // 
   ;; The following are readers, not accessors!!!
   ;; // WARNING // WARNING // WARNING // WARNING // WARNING // WARNING // 
   "MSGBUF-MTYPE" "MSGBUF-MTEXT" "IPC_PERMKEY" "IPC_PERMUID"
   "IPC_PERMGID" "IPC_PERMCUID" "IPC_PERMCGID" "IPC_PERMMODE"
   "IPC_PERMSEQ" "MSGQID_DS-MSG_PERM" "MSGQID_DS-MSG_STIME"
   "MSQID_DS-MSG_RTIME" "MSQID_DS-MSG_CTIME" "MSQID_DS-MSG_CBYTES"
   "MSQID_DS-MSG_QNUM" "MSQID_DS-MSG_QBYTES" "MSQID_DS-MSG_LSPID"
   "MSQID_DS-MSG_LRPID" "MSGINFO-MSGPOOL" "MSGINFO-MSGMAP"
   "MSGINFO-MSGMAX" "MSGINFO-MSGMNB" "MSGINFO-MSGMNI" "MSGINFO-MSGSSZ"
   "MSGINFO-MSGTQL" "MSGINFO-MSGSEG"
   "SHMID_DS-SHM_PERM" "SHMID_DS-SHM_SEGSZ"
   "SHMID_DS-SHM_ATIME" "SHMID_DS-SHM_DTIME" "SHMID_DS-SHM_CTIME"
   "SHMID_DS-SHM_CPID" "SHMID_DS-SHM_LPID" "SHMID_DS-SHM_NATTCH"
   "SEMID_DS-SEM_PERM" "SEMID_DS-SEM_OTIME" "SEMID_DS-SEM_CTIME"
   "SEMID_DS-SEM_NSEMS" 
   "SEMINFO-SEMMAP" "SEMINFO-SEMMNI" "SEMINFO-SEMMNS"
   "SEMINFO-SEMMNU" "SEMINFO-SEMMSL" "SEMINFO-SEMOPM" "SEMINFO-SEMUME"
   "SEMINFO-SEMUSZ" "SEMINFO-SEMVMX" "SEMINFO-SEMAEM"
   "SEMBUF-SEM_NUM" "SEMBUF-SEM_OP" "SEMBUF-SEM_FLG" ))
(in-package "COM.INFORMATIMAGO.CLISP.SUSV3-XSI")


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO: Actually, we should include the features only if it's proven to exist on the current system. At run-time.
  (pushnew :susv3-xsi *features*))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +libc+ "/lib/libc.so.6"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FTW.H
;;
;; NOTE: WE DON'T USE THE LIBC IMPLEMENTATION OF FTW;
;;       WE IMPLEMENT IT IN LISP USING SUSV3:DIRENT AND SUSV3:STAT.
;;       ANOTHER IMPLEMENTATION COULD USE THE C VERSION (BUT WOULD HAVE
;;       TO USE C-TO-LISP CALLBACKS!
;;


(DEFCONSTANT +FTW-F+   0 "File.")
(DEFCONSTANT +FTW-D+   1 "Directory.")
(DEFCONSTANT +FTW-DNR+ 2 "Directory without read permission.")
(DEFCONSTANT +FTW-DP+  3 "Directory with subdirectories visited.")
(DEFCONSTANT +FTW-NS+  4 "Unknown type; stat() failed.")
(DEFCONSTANT +FTW-SL+  5 "Symbolic link.")
(DEFCONSTANT +FTW-SLN+ 6 "Symbolic link that names a nonexistent file.")


(DEFCONSTANT +FTW-PHYS+ 1
  "Physical walk, does not follow symbolic links. Otherwise, NFTW
follows links but does not walk down any path that crosses itself.")

(DEFCONSTANT +FTW-MOUNT+ 2 "The walk does not cross a mount point.")
(DEFCONSTANT +FTW-DEPTH+ 4
  "All subdirectories are visited before the directory itself.")
(DEFCONSTANT +FTW-CHDIR+ 8
  "The walk changes to each directory before reading it.")


(DEFSTRUCT FTW
  (BASE  0 :TYPE INTEGER)
  (LEVEL 0 :TYPE INTEGER))


(DEFTYPE FTW-FILTER  ()
  '(FUNCTION (SIMPLE-STRING STAT INTEGER)     INTEGER))
(DEFTYPE NFTW-FILTER ()
  '(FUNCTION (SIMPLE-STRING STAT INTEGER FTW) INTEGER))


(declaim
 (FTYPE (FUNCTION (SIMPLE-STRING FTW-FILTER  INTEGER)         INTEGER) FTW)
 (FTYPE (FUNCTION (SIMPLE-STRING NFTW-FILTER INTEGER INTEGER) INTEGER) NFTW))


;; ISSUE:  SHOULD THE FILTER RETURN NIL/T OR ZEROP/NOT ZEROP?
;;         AS DEFINED BY SUSV3, ZEROP/NOT ZEROP ALLOW IT TO RETURN
;;         A USEFUL VALUE THRU FTW.
;;
;; ISSUE:  SHOULD THE FILTER BE ALLOWED TO RETURN NIL/NOT NULL?
;;         THAT WOULD BE EVEN MORE USEFUL!
;;
;; ISSUE:  SPECIFY WHAT HAPPENS WHEN A CONDITION OCCURS IN THE FILTER
;;         --> THE FILTER IS ONLY UNWIND-PROTECTED.
;;
;; ISSUE:  SPECIFY THAT FILTER CAN RECEIVE A NIL STAT RATHER THAN AN
;;         UNDEFINED ONE. OR NOT?
;;
;; ISSUE:  specify that ftw and nfw must not give "." and ".." to the filter
;;         (but only when "." is the starting path). (This is underspecified
;;         in SUSv3).


(DEFUN FTW (PATH FILTER NDIRS)
  "
URL:        http://www.opengroup.org/onlinepubs/007904975/functions/ftw.html
"
  (DECLARE (TYPE (INTEGER 1 #|+OPEN-MAX+|#) NDIRS))
  (declare (ignore ndirs))
  ;; We'll have always only one DIR-STREAM open: we keep the list of
  ;; subdirectories in memory and process them after having read the directory.
  (LET ((DIR-STREAM (OPENDIR PATH)))
    (UNWIND-PROTECT
         (DO* ((ENTRY (READDIR DIR-STREAM) (READDIR DIR-STREAM))
               (DIRECTORIES '())
               SUBPATH STAT FLAG
               (RESULT 0))
              ((OR (NULL ENTRY) (/= 0 RESULT)) DIRECTORIES)
           (UNLESS (OR (STRING= (DIRENT-NAME ENTRY) "..")
                       (STRING= (DIRENT-NAME ENTRY) "."))
             (SETQ SUBPATH (CONCATENATE 'STRING PATH "/"
                                        (DIRENT-NAME ENTRY)))
             (HANDLER-CASE (SETQ STAT (LSTAT SUBPATH))
               (T () (SETQ STAT NIL)))
             (COND
               ((NULL STAT)
                (SETQ FLAG +FTW-NS+))
               ((S-ISREG (STAT-MODE STAT))
                (SETQ FLAG +FTW-F+))
               ((S-ISDIR (STAT-MODE STAT))
                (PUSH SUBPATH DIRECTORIES)
                (SETQ FLAG +FTW-F+))
               ((S-ISLNK (STAT-MODE STAT))
                (HANDLER-CASE (SETQ STAT (STAT SUBPATH)
                                    FLAG +FTW-SL+)))
               (T () (SETQ STAT NIL
                           FLAG +FTW-SLN+)))
             (SETQ RESULT
                   (HANDLER-CASE
                       (funcall FILTER (DIRENT-NAME ENTRY) STAT FLAG)
                     (T () -1)))))
      (CLOSEDIR DIR-STREAM))))

    
(DEFCONSTANT +FTW-F+   0 "File.")
(DEFCONSTANT +FTW-D+   1 "Directory.")
(DEFCONSTANT +FTW-DNR+ 2 "Directory without read permission.")
(DEFCONSTANT +FTW-DP+  3 "Directory with subdirectories visited.")
(DEFCONSTANT +FTW-NS+  4 "Unknown type; stat() failed.")
(DEFCONSTANT +FTW-SL+  5 "Symbolic link.")
(DEFCONSTANT +FTW-SLN+ 6 "Symbolic link that names a nonexistent file.")


;; int ftw(const char *,int (*)(const char *,const struct stat *,int),int)
;; int nftw(const char *,int (*)(const char *,const struct stat *, int,struct FTW*),int,int)


;; (DEFUN FIND ()
;;   (DO* ((DIR-STREAM (OPENDIR "/tmp"))
;;         (ENTRY (READDIR DIR-STREAM) (READDIR DIR-STREAM)))
;;       ((NULL ENTRY))
;;     (FORMAT T "entry: ~8D ~S~%" (DIRENT-INO ENTRY) (DIRENT-NAME ENTRY))))



;;----------------------------------------------------------------------
;; ipc
;;----------------------------------------------------------------------

(defconstant IPC_CREAT   #o01000 "Create key if key does not exist.")
(defconstant IPC_EXCL    #o02000 "Fail if key exists.")
(defconstant IPC_NOWAIT  #o04000 "Return error on wait.")

;; Control commands for `msgctl', `semctl', and `shmctl'. 
(defconstant IPC_RMID    0       "Remove identifier.")
(defconstant IPC_SET     1       "Set `ipc_perm' options.")
(defconstant IPC_STAT    2       "Get `ipc_perm' options.")
(defconstant IPC_INFO    3       "See ipcs.")

(defconstant IPC_PRIVATE 0 "Private key.")


(ffi:def-c-struct ipc_perm
  (key     linux:|key_t|)               ; Key.
  (uid     linux:|uid_t|)               ; Owner's user ID. 
  (gid     linux:|gid_t|)               ; Owner's group ID.
  (cuid    linux:|uid_t|)               ; Creator's user ID.
  (cgid    linux:|gid_t|)               ; Creator's group ID.
  (mode    ffi:ushort)                  ; Read/write permission.
  (pad1    ffi:ushort)
  (seq     ffi:ushort)                  ; Sequence number.
  (pad2    ffi:ushort)
  (unused1 ffi:ulong)
  (unused2 ffi:ulong))


(ffi:def-call-out ftok (:name "ftok")
  (:arguments (pathname ffi:c-string) (proj-id ffi:int))
  (:return-type linux:|key_t|)
  (:library #.+libc+) (:language :stdc))


;;----------------------------------------------------------------------
;; msg
;;----------------------------------------------------------------------

(defconstant MSG_NOERROR  #o010000 "no error if message is too big")
(defconstant MSG_EXCEPT   #o020000 "recv any msg except of specified type")

;; Types used in the structure definition.  
(ffi:def-c-type msgqnum_t ffi:ulong)
(ffi:def-c-type msglen_t  ffi:ulong)


;; Structure of record for one message inside the kernel.
;; The type `struct msg' is opaque.
(ffi:def-c-struct msqid_ds
  (msg_perm    ipc_perm)   ; structure describing operation permission
  (msg_stime   linux:|time_t|)          ; time of last msgsnd command 
  (__unused1   ffi:ulong)
  (msg_rtime   linux:|time_t|)          ; time of last msgrcv command 
  (__unused2   ffi:ulong)
  (msg_ctime   linux:|time_t|)          ; time of last change
  (__unused3   ffi:ulong)
  (msg_cbytes  ffi:ulong)          ;  current number of bytes on queue
  (msg_qnum    msgqnum_t)      ; number of messages currently on queue
  (msg_qbytes  msglen_t)        ; max number of bytes allowed on queue
  (msg_lspid   linux:|pid_t|)           ; pid of last msgsnd()
  (msg_lrpid   linux:|pid_t|)           ; pid of last msgrcv()
  (__unused4   ffi:ulong)
  (__unused5   ffi:ulong))

;; ipcs ctl commands
(defconstant MSG_STAT 11)
(defconstant MSG_INFO 12)


;; buffer for msgctl calls IPC_INFO, MSG_INFO 
(ffi:def-c-struct msginfo
  (msgpool ffi:int)
  (msgmap  ffi:int)
  (msgmax  ffi:int)
  (msgmnb  ffi:int)
  (msgmni  ffi:int)
  (msgssz  ffi:int)
  (msgtql  ffi:int)
  (msgseg  ffi:ushort))


(defstruct msgbuf
  (mtype 0   :type integer)
  (mtext #() :type (vector (unsigned-byte 8) *)))


(ffi:def-call-out msgget (:name "msgget")
  (:arguments (key linux:|key_t|) (msgflg ffi:int))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


(ffi:def-call-out msgctl (:name "msgctl")
  (:arguments (msqid ffi:int) (cmd ffi:int) (buf pointer))
  ;; We cannot use (ffi:c-ptr msqid_ds) because in that case
  ;; it would not be copied back as output parameter.
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


(ffi:def-call-out msgsnd (:name "msgsnd")
  (:arguments (msqid ffi:int) (msgbuf pointer) (msgsz ffi:size_t)
              (msgflg ffi:int))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


(ffi:def-call-out msgrcv (:name "msgrcv")
  (:arguments (msqid ffi:int) (msgbuf pointer) (msgsz ffi:size_t)
              (msgtyp ffi:long) (msgflg ffi:int))
  (:return-type ffi:ssize_t)
  (:library #.+libc+) (:language :stdc))


;;----------------------------------------------------------------------
;; shm
;;----------------------------------------------------------------------


;; Permission flag for shmget. 
(defconstant SHM_R #o0400 "or S_IRUGO from <linux/stat.h> *")
(defconstant SHM_W #o0200 "or S_IWUGO from <linux/stat.h> *")

;; Flags for `shmat'. 
(defconstant SHM_RDONLY #o010000 "attach read-only else read-write *")
(defconstant SHM_RND    #o020000 "round attach address to SHMLBA *")
(defconstant SHM_REMAP  #o040000 "take-over region on attach *")

;; Commands for `shmctl'.  
(defconstant SHM_LOCK   11 "lock segment (root only) *")
(defconstant SHM_UNLOCK 12 "unlock segment (root only) *")


(ffi:def-call-out getpagesize (:name "getpagesize")
  (:arguments)
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


(defun SHMLBA () 
  "Segment low boundary address multiple. "
  (getpagesize))


(ffi:def-c-type shmatt_t ffi:ulong)
;; Type to count number of attaches. 



(ffi:def-c-struct shmid_ds
  ;; Data structure describing a set of semaphores.
  (shm_perm    ipc_perm)   ; structure describing operation permission
  (shm_segsz   ffi:size_t)          ;  size of segment in bytes 
  (shm_atime   linux:|time_t|)          ;  time of last shmat() 
  (__unused1   ffi:ulong)               ; 
  (shm_dtime   linux:|time_t|)          ;  time of last shmdt() 
  (__unused2   ffi:ulong)               ; 
  (shm_ctime   linux:|time_t|)       ;time of last change by shmctl() 
  (__unused3   ffi:ulong)               ; 
  (shm_cpid    linux:|pid_t|)           ;  pid of creator 
  (shm_lpid    linux:|pid_t|)           ;  pid of last shmop 
  (shm_nattch  shmatt_t)                ; number of current attaches 
  (__unused4   ffi:ulong)               ; 
  (__unused5   ffi:ulong))

(ffi:def-c-struct shminfo
  (shmmax     ffi:ulong)
  (shmmin     ffi:ulong)
  (shmmni     ffi:ulong)
  (shmseg     ffi:ulong)
  (shmall     ffi:ulong)
  (__unused1  ffi:ulong)
  (__unused2  ffi:ulong)
  (__unused3  ffi:ulong)
  (__unused4  ffi:ulong))


(ffi:def-c-struct shm_info
  (used_ids ffi:int)                    ; 
  (shm_tot ffi:ulong)                   ; total allocated shm
  (shm_rss ffi:ulong)                   ; total resident shm*
  (shm_swp ffi:ulong)                   ; total swapped shm
  (swap_attempts ffi:ulong)             ; 
  (swap_successes ffi:ulong))


 
(ffi:def-call-out shmget (:name "shmget")
  ;; Get shared memory segment.
  (:arguments (key linux:|key_t|) (size ffi:size_t) (shmflg ffi:int))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


(ffi:def-call-out shmctl (:name "shmctl")
  ;; Shared memory control operation. 
  (:arguments (shmid ffi:int) (cmd ffi:int) (buf pointer))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


(ffi:def-call-out shmat (:name "shmat")
  ;; Attach shared memory segment. 
  (:arguments (shmid ffi:int) (shmaddr pointer) (shmflg ffi:int))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


(ffi:def-call-out shmdt (:name "shmdt")
  ;; Detach shared memory segment.
  (:arguments (shmaddr pointer))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


;;----------------------------------------------------------------------
;; sem
;;----------------------------------------------------------------------


;; Flags for `semop'.  
(defconstant SEM_UNDO   #x1000 "undo the operation on exit")

;; Commands for `semctl'.  
(defconstant  GETPID          11 "get sempid")
(defconstant  GETVAL          12 "get semval")
(defconstant  GETALL          13 "get all semval's")
(defconstant  GETNCNT         14 "get semncnt")
(defconstant  GETZCNT         15 "get semzcnt")
(defconstant  SETVAL          16 "set semval")
(defconstant  SETALL          17 "set all semval's")

(defconstant SEMMNI  128         "<= IPCMNI  max # of semaphore identifiers")
(defconstant SEMMSL  250         "<= 8 000 max num of semaphores per id")
(defconstant SEMMNS  32000 #|(* SEMMNI SEMMSL)|#
  "<= INT_MAX max # of semaphores in system")
(defconstant SEMOPM  32	         "<= 1 000 max num of ops per semop call")
(defconstant SEMVMX  32767       "<= 32767 semaphore maximum value")
(defconstant SEMAEM  32767 #|SEMVMX|#   "adjust on exit max value")


(ffi:def-c-struct semid_ds
  ;; Data structure describing a set of semaphores.  
  (sem_perm ipc_perm)                   ; operation permission struct 
  (sem_otime linux:|time_t|)            ; last semop() time 
  (__unused1 ffi:ulong)
  (sem_ctime linux:|time_t|)          ; last time changed by semctl() 
  (__unused2 ffi:ulong)
  (sem_nsems ffi:ulong)                 ; number of semaphores in set 
  (__unused3 ffi:ulong)
  (__unused4 ffi:ulong))


;;    The user should define a union like the following to use it for arguments
;;    for `semctl'.
;; 
;;    union semun
;;    {
;;      int val;                           <= value for SETVAL
;;      struct semid_ds *buf;              <= buffer for IPC_STAT & IPC_SET
;;      unsigned short int *array;         <= array for GETALL & SETALL
;;      struct seminfo *__buf;             <= buffer for IPC_INFO
;;    };
;; 
;;    Previous versions of this file used to define this union but this is
;;    incorrect.  One can test the macro _SEM_SEMUN_UNDEFINED to see whether
;;    one must define the union or not.  

;; ipcs ctl cmds 
(defconstant SEM_STAT 18)
(defconstant SEM_INFO 19)

(ffi:def-c-struct seminfo
  (semmap ffi:int)
  (semmni ffi:int)
  (semmns ffi:int)
  (semmnu ffi:int)
  (semmsl ffi:int)
  (semopm ffi:int)
  (semume ffi:int)
  (semusz ffi:int)
  (semvmx ffi:int)
  (semaem ffi:int))



(ffi:def-c-struct sembuf
  ;; Structure used for argument to `semop' to describe operations. 
  (sem_num ffi:ushort)                  ; semaphore number 
  (sem_op ffi:short)                    ; semaphore operation 
  (sem_flg ffi:short)                   ; operation flag
  )


(ffi:def-call-out semget (:name "semget")
  ;; Get semaphore. 
  (:arguments (key linux:|key_t|) (nsems ffi:int) (semflg ffi:int))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


(ffi:def-call-out semctl (:name "semctl")
  ;; Semaphore control operation.
  (:arguments (semid ffi:int) (semnum ffi:int) (cmt ffi:int) (arg pointer))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


(ffi:def-call-out semop (:name "semop")
  ;; Operate on semaphore. 
  (:arguments (semid ffi:int) (sops pointer) (nsops ffi:size_t))
  (:return-type ffi:int)
  (:library #.+libc+) (:language :stdc))


;;;; THE END ;;;;
