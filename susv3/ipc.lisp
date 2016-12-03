;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               ipc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    An API over SUSV3-XSI IPC.
;;;;
;;;;    Note: The number of queues in a system is limited.
;;;;          An application could use the message type as a recipient address.
;;;;
;;;; cliini:  There's always another way to achieve the same thing.
;;;;          But it's the lisp way to offer nice and intuitive interfaces
;;;;          to achieve the stuff, otherwise we might all be using assembler.
;;;;                                                      [2005-01-02 20:30:53]
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-11-29 <PJB> Created.
;;;;BUGS
;;;;    +MESSAGE-SIZE-LIMIT+  should be got dynamically from the system!
;;;;
;;;;    This package should not use FFI at all, the SUSV3-XSI package should
;;;;    export pure lisp.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2004 - 2016
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
(in-package "COMMON-LISP-USER")

;; TODO: This nicknaming should be done outside of the sources.
#-mocl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (com.informatimago.common-lisp.cesarum.package:add-nickname
   "COM.INFORMATIMAGO.CLISP.SUSV3"   "SUSV3")
  (com.informatimago.common-lisp.cesarum.package:add-nickname
     "COM.INFORMATIMAGO.CLISP.SUSV3-XSI" "SUSV3-XSI"))

(defpackage "COM.INFORMATIMAGO.SUSV3.IPC"
  (:documentation
   "An API over SUSV3-XSI IPC (msgget/msgctl/msgsnd/msgrcv).")
  (:use   "COMMON-LISP" "FFI"
          "COM.INFORMATIMAGO.SUSV3.TOOLS"
          "COM.INFORMATIMAGO.CLISP.SUSV3")
  (:export
   ;; ipc:
   "IPC-PERMISSIONS" "IPC-PERMISSIONS-KEY" "IPC-PERMISSIONS-UID"
   "IPC-PERMISSIONS-GID" "IPC-PERMISSIONS-CUID" "IPC-PERMISSIONS-CGID"
   "IPC-PERMISSIONS-MODE" "IPC-PERMISSIONS-SEQ"
   "MAKE-KEY"
   ;; msg:
   "MSGDESC" "MSGDESC-PERMISSIONS" "MSGDESC-SEND-TIME"
   "MSGDESC-RECEIVE-TIME" "MSGDESC-CHANGE-TIME"
   "MSGDESC-CURRENT-BYTES" "MSGDESC-MESSAGE-COUNT"
   "MSGDESC-MAXIMUM-BYTES" "MSGDESC-LAST-SEND-PID"
   "MSGDESC-LAST-RECEIVE-PID"
   "MESSAGE-GET" "MESSAGE-STATISTICS" "MESSAGE-MODIFY"
   "MESSAGE-REMOVE" "MESSAGE-SEND" "MESSAGE-RECEIVE" "MESSAGE-SEND-SEXP"
   "MESSAGE-RECEIVE-SEXP"
   ;; shm:
   "SHMDESC" "SHMDESC-PERMISSIONS" "SHMDESC-SEGMENT-SIZE"
   "SHMDESC-ATTACH-TIME" "SHMDESC-DETACH-TIME" "SHMDESC-CHANGE-TIME"
   "SHMDESC-CREATOR-PID" "SHMDESC-LAST-OPERATION-PID"
   "SHMDESC-ATTACH-COUNT"
   "MEMORY-GET" "MEMORY-STATISTICS"
   "MEMORY-MODIFY" "MEMORY-REMOVE" "MEMORY-LOCK" "MEMORY-UNLOCK"
   "MEMORY-PAGE-SIZE" "MEMORY-ATTACH" "MEMORY-DETACH"
   ;; sem:
   "SEMDESC" "SEMDESC-PERMISSIONS" "SEMDESC-NUMBER-OF-SEMAPHORES"
   "SEMDESC-OPERATION-TIME" "SEMDESC-CHANGE-TIME"
   "SEMAPHORE-GET" "SEMAPHORE-STATISTICS" "SEMAPHORE-MODIFY"
   "SEMAPHORE-REMOVE" "SEMAPHORE-GET-PID" "SEMAPHORE-GET-VALUE"
   "SEMAPHORE-GET-ALL-VALUES" "SEMAPHORE-NUMBER-WAITING-FOR-INCREASE"
   "SEMAPHORE-NUMBER-WAITING-FOR-ZERO" "SEMAPHORE-SET-VALUE"
   "SEMAPHORE-SET-ALL-VALUES" "SEMAPHORE-OPERATE" ))
(in-package "COM.INFORMATIMAGO.SUSV3.IPC")


(defun make-key (pathname project-id)
  "Converts a pathname and a project identifier to a System V IPC key."
  (declare (type pathname pathname)
           (type (integer project-id))) ;; not zerop!
  (let ((path  (namestring (truename pathname))))
    (check-errno (susv3-xsi:ftok path project-id)
                 :function  'susv3-xsi:ftok
                 :arguments  (list path project-id)
                 :caller 'make-key)))


(defstruct ipc-permissions
  (key  0 :type integer)                ; Key.
  (uid  0 :type integer)                ; Owner's user ID.
  (gid  0 :type integer)                ; Owner's group ID.
  (cuid 0 :type integer)                ; Creator's user ID.
  (cgid 0 :type integer)                ; Creator's group ID.
  (mode 0 :type integer)           ; Read/write permissions. rwxrwxrwx
  (seq  0 :type integer))               ; Sequence number.


(define-ffi-copiers ipc-permissions susv3-xsi:ipc_perm
  (ipc-permissions-key   susv3-xsi::key)
  (ipc-permissions-uid   susv3-xsi::uid)
  (ipc-permissions-gid   susv3-xsi::gid)
  (ipc-permissions-cuid  susv3-xsi::cuid)
  (ipc-permissions-cgid  susv3-xsi::cgid)
  (ipc-permissions-mode  susv3-xsi::mode)
  (ipc-permissions-seq   susv3-xsi::seq))


;;----------------------------------------------------------------------
;; msg
;;----------------------------------------------------------------------


(defun message-get (key &key (create nil) (private nil) (exclusive nil)
                    (access-rights #o600))
  "Returns the message queue identifier associated with the value
   of the key argument."
  (let ((flags (+ (if create    susv3-xsi:ipc_creat   0)
                  (if private   susv3-xsi:ipc_private 0)
                  (if exclusive susv3-xsi:ipc_excl    0)
                  (ldb (byte 9 0) access-rights))))
    (check-errno (susv3-xsi:msgget key flags)
                 :function 'susv3-xsi:msgget
                 :arguments (list key flags)
                 :caller 'message-get)))


(defstruct msgdesc
  (permissions (make-ipc-permissions) :type ipc-permissions)
  (send-time        0 :type integer)    ; time of last msgsnd command
  (receive-time     0 :type integer)    ; time of last msgrcv command
  (change-time      0 :type integer)    ; time of last change
  (current-bytes    0 :type integer) ; current number of bytes on queue
  (message-count    0 :type integer) ; number of messages currently on queue
  (maximum-bytes    0 :type integer) ; max number of bytes allowed on queue
  (last-send-pid    0 :type integer)    ; pid of last msgsnd()
  (last-receive-pid 0 :type integer))   ; pid of last msgrcv()


(define-ffi-copiers msgdesc susv3-xsi:msqid_ds
  ((msgdesc-permissions ipc-permissions) (susv3-xsi::msg_perm ipc_perm))
  (msgdesc-send-time             susv3-xsi::msg_stime)
  (msgdesc-receive-time          susv3-xsi::msg_rtime)
  (msgdesc-change-time           susv3-xsi::msg_ctime)
  (msgdesc-current-bytes         susv3-xsi::msg_cbytes)
  (msgdesc-message-count         susv3-xsi::msg_qnum)
  (msgdesc-maximum-bytes         susv3-xsi::msg_qbytes)
  (msgdesc-last-send-pid         susv3-xsi::msg_lspid)
  (msgdesc-last-receive-pid      susv3-xsi::msg_lrpid))


(defun message-statistics (queue)
  "Returns a MSGDESC structure, containing a copy of the information
from the message queue data structure associated with the msqid QUEUE.
The caller must have read permission on the message queue."
  (ffi:with-c-var (d 'susv3-xsi:msqid_ds)
    (check-errno (susv3-xsi:msgctl queue susv3-xsi:ipc_stat
                                   (ffi:foreign-address-unsigned
                                    (ffi:c-var-address d)))
                 :function 'susv3-xsi:msgctl
                 :arguments (list queue 'susv3-xsi:ipc_stat 'susv3-xsi:msqid_ds)
                 :caller 'message-statistics)
    (let ((result  (make-msgdesc)))
      (msqid_ds->msgdesc d result)
      result)))


(defun message-modify (queue msgdesc)
  "
Write the values of some members of the msqid_ds structure
to the  message  queue  data  structure, updating also its msg_ctime member.
The following members of the structure can be updated:
msg_perm.uid, msg_perm.gid, msg_perm.mode (only lowest 9-bits), msg_qbytes.
"
  (ffi:with-c-var (d 'susv3-xsi:msqid_ds)
    (msgdesc->msqid_ds msgdesc d)
    (check-errno (susv3-xsi:msgctl queue susv3-xsi:ipc_set
                                   (ffi:foreign-address-unsigned
                                    (ffi:c-var-address d)))
                 :function 'susv3-xsi:msgctl
                 :arguments (list queue 'susv3-xsi:ipc_set msgdesc)
                 :caller 'message-modify))) ;;message-modify


(defun message-remove (queue)
  "
Immediately  remove  the  message queue and its associated
data structure, awakening all waiting  reader  and  writer
processes  (with  an error return and errno set to EIDRM).
The calling process must have appropriate (probably, root)
privileges or its effective user-ID must be either that of
the creator or owner of the message queue.
"
  (check-errno (susv3-xsi:msgctl queue susv3-xsi:ipc_rmid 0)
               :function 'susv3-xsi:msgctl
               :arguments (list queue 'susv3-xsi:ipc_rmid 0)
               :caller 'message-remove)) ;;message-remove


(defun message-send (queue message-type message-text
                     &key (no-wait nil))
  (ffi:with-c-var (msg `(ffi:c-struct susv3-xsi:msgbuf
                                      (susv3-xsi::mtype ffi:long)
                                      (susv3-xsi::mtext
                                       (ffi:c-array ffi:uint8
                                                    ,(length message-text))))
                       (susv3-xsi:make-msgbuf :mtype message-type
                                              :mtext message-text))
    (check-errno (susv3-xsi:msgsnd queue
                                   (ffi:foreign-address-unsigned
                                    (ffi:c-var-address msg))
                                   (length message-text)
                                   (if no-wait susv3-xsi:ipc_nowait 0))
                 :function 'susv3-xsi:msgsnd
                 :arguments (list queue 'msg
                                  (length message-text)
                                  (if no-wait susv3-xsi:ipc_nowait 0))
                 :caller 'message-send))) ;;message-send


(defun message-receive (queue message-type message-size
                        &key (no-wait nil) (no-error nil) (except nil))
  (ffi:with-c-var (msg `(ffi:c-struct susv3-xsi:msgbuf
                                      (susv3-xsi::mtype ffi:long)
                                      (susv3-xsi::mtext
                                       (ffi:c-array ffi:uint8 ,message-size))))
    (let ((size (check-errno
                 (susv3-xsi:msgrcv queue
                                   (ffi:foreign-address-unsigned
                                    (ffi:c-var-address msg))
                                   message-size
                                   message-type
                                   (+ (if no-wait  susv3-xsi:ipc_nowait  0)
                                      (if no-error susv3-xsi:msg_noerror 0)
                                      (if except   susv3-xsi:msg_except  0)))
                 :function 'susv3-xsi:msgsnd
                 :arguments (list queue 'msg
                                  message-size
                                  message-type
                                  (+ (if no-wait  susv3-xsi:ipc_nowait  0)
                                     (if no-error susv3-xsi:msg_noerror 0)
                                     (if except   susv3-xsi:msg_except  0)))
                 :caller 'message-receive)))
      (let ((slice (make-array (list size)
                               :element-type '(unsigned-byte 8)
                               :displaced-to  (susv3-xsi:msgbuf-mtext msg))))
        (values (make-array (list size)
                            :element-type '(unsigned-byte 8)
                            :initial-contents slice)
                (susv3-xsi:msgbuf-mtype msg)))))) ;;message-receive


(defparameter +message-size-limit+ 8188
  "BUG: We should get dynamically the limit from the system!")


(defun message-send-sexp (queue message-type sexp &key (no-wait nil))
  (let ((mtext (ext:convert-string-to-bytes
                (format nil "~S" sexp) charset:iso-8859-1)))
    (when (< +message-size-limit+ (length mtext))
      (error "S-expression too big to be sent thru a message queue."))
    (tagbody
     :again
       (handler-case
           (return-from message-send-sexp
             (print (message-send queue message-type mtext :no-wait no-wait)))
         (unix-error (err) (print err)
                     (if (= susv3:eintr (unix-error-number err))
                         (go :again)
                         (error err))))))) ;;message-send-sexp


(defun message-receive-sexp (queue message-type &key (no-wait nil) (except nil))
  (multiple-value-bind (mtext mtype)
      (block :receive
        (tagbody
         :again
           (handler-case
               (return-from :receive
                 (message-receive queue message-type +message-size-limit+
                                  :no-wait no-wait :except except))
             (unix-error (err) (if (= susv3:eintr (unix-error-number err))
                                   (go :again)
                                   (error err))))))
    (values (let ((*read-eval* nil))
              (read-from-string
               (ext:convert-string-from-bytes mtext charset:iso-8859-1)))
            mtype))) ;;message-receive-sexp


;;----------------------------------------------------------------------
;; shm
;;----------------------------------------------------------------------


(defun memory-get (key size
                   &key (create nil) (exclusive nil) (access-rights #o600))
  "returns  the  identifier  of  the shared memory segment
   associated with the value of the argument key.  A new shared mem­
   ory segment, with size equal to the value of size rounded up to a
   multiple of PAGE_SIZE, is created if key has the  value  IPC_PRI­
   VATE  or  key  isn't IPC_PRIVATE, no shared memory segment corre­
   sponding to key exists, and IPC_CREAT is asserted in shmflg (i.e.
   shmflg&IPC_CREAT isn't zero)."
  (let ((flags (+ (if create     susv3-xsi:ipc_creat 0)
                  (if exclusive  susv3-xsi:ipc_excl  0)
                  (ldb (byte 9 0) access-rights))))
    (check-errno (susv3-xsi:shmget key size flags)
                 :function 'susv3-xsi:shmget
                 :arguments (list key size flags)
                 :caller 'memory-get))) ;;memory-get


(defstruct shmdesc
  ;; Data structure describing a shared memory segment.
  (permissions (make-ipc-permissions) :type ipc-permissions)
  (segment-size 0 :type integer)        ; size of segment in bytes
  (attach-time  0 :type integer)        ; time of last shmat()
  (detach-time  0 :type integer)        ; time of last shmdt()
  (change-time  0 :type integer)    ; time of last change by shmctl()
  (creator-pid  0 :type integer)        ; pid of creator
  (last-operation-pid  0 :type integer) ; pid of last shmop
  (attach-count  0 :type integer))      ; number of current attaches


(define-ffi-copiers shmdesc susv3-xsi:shmid_ds
  ((shmdesc-permissions ipc-permissions) (susv3-xsi::shm_perm ipc_perm))
  (shmdesc-segment-size       susv3-xsi::shm_segsz)
  (shmdesc-attach-time        susv3-xsi::shm_atime)
  (shmdesc-detach-time        susv3-xsi::shm_dtime)
  (shmdesc-change-time        susv3-xsi::shm_ctime)
  (shmdesc-creator-pid        susv3-xsi::shm_cpid)
  (shmdesc-last-operation-pid susv3-xsi::shm_lpid)
  (shmdesc-attach-count       susv3-xsi::shm_nattch)) ;;shmdesc


(defun memory-statistics (memory)
  "copy the information about the shared memory
   segment  into the buffer buf. The user must have
   read access to the shared memory segment."
  (ffi:with-c-var (d 'susv3-xsi:shmid_ds)
    (check-errno (susv3-xsi:shmctl memory susv3-xsi:ipc_stat
                                   (ffi:foreign-address-unsigned
                                    (ffi:c-var-address d)))
                 :function 'susv3-xsi:shmctl
                 :arguments (list memory 'susv3-xsi:ipc_stat
                                  'susv3-xsi:shmid_ds)
                 :caller 'memory-statistics)
    (let ((result  (make-shmdesc)))
      (shmid_ds->shmdesc d result)
      result))) ;;memory-statistics


(defun memory-modify (memory shmdesc)
  "apply the changes the user has made to the
   uid,  gid,  or  mode  members of the shm_perms field.
   Only the  lowest  9  bits  of  mode  are  used.   The
   shm_ctime  member  is also updated.  The user must be
   the owner, creator, or the super-user."
  (ffi:with-c-var (d 'susv3-xsi:shmid_ds)
    (shmdesc->shmid_ds shmdesc d)
    (check-errno (susv3-xsi:shmctl memory susv3-xsi:ipc_set
                                   (ffi:foreign-address-unsigned
                                    (ffi:c-var-address d)))
                 :function 'susv3-xsi:shmctl
                 :arguments (list memory 'susv3-xsi:ipc_set shmdesc)
                 :caller 'memory-modify))) ;;memory-modify


(defun memory-remove (memory)
  "Mark the segment  as  destroyed.  It  will
   actually  be destroyed after the last detach.  (I.e.,
   when the shm_nattch member of the  associated  struc­
   ture  shmid_ds is zero.)  The user must be the owner,
   creator, or the super-user."
  (check-errno (susv3-xsi:shmctl memory susv3-xsi:ipc_rmid 0)
               :function 'susv3-xsi:shmctl
               :arguments (list memory 'susv3-xsi:ipc_rmid 0)
               :caller 'memory-remove)) ;;memory-remove


(defun memory-lock (memory)
  "prevents  swapping  of  a  shared memory segment. The
   user must fault in any pages that are required to  be
   present after locking is enabled."
  (check-errno (susv3-xsi:shmctl memory susv3-xsi:shm_lock 0)
               :function 'susv3-xsi:shmctl
               :arguments (list memory 'susv3-xsi:shm_lock 0)
               :caller 'memory-remove)) ;;memory-lock


(defun memory-unlock (memory)
  "allows the shared memory segment to be swapped out."
  (check-errno (susv3-xsi:shmctl memory susv3-xsi:shm_lock 0)
               :function 'susv3-xsi:shmctl
               :arguments (list memory 'susv3-xsi:shm_lock 0)
               :caller 'memory-remove)) ;;memory-unlock


(defun memory-page-size ()
  "Return the page size, to which the shared memory page addresses
   must be rounded."
  (susv3-xsi:shmlba))


(defun memory-attach (memory address &key (round nil) (read-only nil)
                      (remap nil))
  ;; remap is linux specific
  "allows the shared memory segment to be swapped out."
  (check-errno (susv3-xsi:shmat memory address
                                (+ (if round     susv3-xsi:shm_rnd    0)
                                   (if read-only susv3-xsi:shm_rdonly 0)
                                   (if remap     susv3-xsi:shm_remap  0)))
               :function 'susv3-xsi:shmat
               :arguments (list memory address
                                (+ (if round     susv3-xsi:shm_rnd    0)
                                   (if read-only susv3-xsi:shm_rdonly 0)
                                   (if remap     susv3-xsi:shm_remap  0)))
               :caller 'memory-attach)) ;;memory-attach


(defun memory-detach (address)
  "detaches the shared memory segment located  at
   the  address  specified  by shmaddr from the address space of the
   calling process.  The to-be-detached segment  must  be  currently
   attached  with  shmaddr  equal  to  the value returned by the its
   attaching shmat call."
  (check-errno (susv3-xsi:shmdt address)
               :function 'susv3-xsi:shmdt
               :arguments (list address)
               :caller 'memory-detach)) ;;memory-detach


;;----------------------------------------------------------------------
;; sem
;;----------------------------------------------------------------------


(defun semaphore-get (key number-of-semaphores
                      &key (create nil) (exclusive nil) (access-rights #o600))
  "Returns  the  semaphore set identifier associated
   with the argument key.  A new set of nsems semaphores is  created
   if  key has the value IPC_PRIVATE or if no existing semaphore set
   is associated to key and IPC_CREAT is asserted  in  semflg  (i.e.
   semflg & IPC_CREAT isn't zero)."
  (let ((flags (+ (if create     susv3-xsi:ipc_creat 0)
                  (if exclusive  susv3-xsi:ipc_excl  0)
                  (ldb (byte 9 0) access-rights))))
    (check-errno (susv3-xsi:semget key number-of-semaphores flags)
                 :function 'susv3-xsi:semget
                 :arguments (list key number-of-semaphores flags)
                 :caller 'semaphore-get))) ;;semaphore-get


(defstruct semdesc
  ;; Data structure describing a set of semaphores.
  (permissions (make-ipc-permissions) :type ipc-permissions)
  (number-of-semaphores 0 :type integer)
  (operation-time       0 :type integer)  ; time of last semop().
  (change-time          0 :type integer)) ; time of last change by semctl())


(define-ffi-copiers semdesc susv3-xsi:semid_ds
  ((semdesc-permissions ipc-permissions) (susv3-xsi::sem_perm ipc_perm))
  (semdesc-number-of-semaphores           susv3-xsi::sem_nsems)
  (semdesc-operation-time                 susv3-xsi::sem_otime)
  (semdesc-change-time                    susv3-xsi::sem_ctime)) ;;semdesc



(defun semaphore-statistics (semaphore)
  "Copy info from the semaphore set data structure  into
   the  structure  pointed  to by arg.buf.  The argument
   semnum is ignored.  The  calling  process  must  have
   read access privileges on the semaphore set."
  (ffi:with-c-var (d 'susv3-xsi:semid_ds)
    (check-errno (susv3-xsi:semctl semaphore 0 susv3-xsi:ipc_stat
                                   (ffi:foreign-address-unsigned
                                    (ffi:c-var-address d)))
                 :function 'susv3-xsi:semctl
                 :arguments (list semaphore 0 'susv3-xsi:ipc_stat
                                  'susv3-xsi:semid_ds)
                 :caller 'semaphore-statistics)
    (let ((result  (make-semdesc)))
      (semid_ds->semdesc d result)
      result))) ;;semaphore-statistics


(defun semaphore-modify (semaphore semdesc)
  "Apply the changes the user has made to the
   uid,  gid,  or  mode  members of the sem_perms field.
   Only the  lowest  9  bits  of  mode  are  used.   The
   sem_ctime  member  is also updated.  The user must be
   the owner, creator, or the super-user."
  (ffi:with-c-var (d 'susv3-xsi:semid_ds)
    (semdesc->semid_ds semdesc d)
    (check-errno (susv3-xsi:semctl semaphore 0 susv3-xsi:ipc_set
                                   (ffi:foreign-address-unsigned
                                    (ffi:c-var-address d)))
                 :function 'susv3-xsi:semctl
                 :arguments (list semaphore 0 'susv3-xsi:ipc_set semdesc)
                 :caller 'semaphore-modify))) ;;semaphore-modify


(defun semaphore-remove (semaphore)
  "Mark the segment  as  destroyed.  It  will
   actually  be destroyed after the last detach.  (I.e.,
   when the sem_nattch member of the  associated  struc­
   ture  semid_ds is zero.)  The user must be the owner,
   creator, or the super-user."
  (check-errno (susv3-xsi:semctl semaphore 0 susv3-xsi:ipc_rmid 0)
               :function 'susv3-xsi:semctl
               :arguments (list semaphore 0 'susv3-xsi:ipc_rmid 0)
               :caller 'semaphore-remove)) ;;semaphore-remove


(defun semaphore-get-pid (semaphore)
  "The  system  call returns the value of sempid for the
   semnum-th semaphore of the set (i.e. the pid  of  the
   process  that  executed  the  last semop call for the
   semnum-th semaphore of the set).  The calling process
   must  have  read  access  privileges on the semaphore
   set."
  (check-errno (susv3-xsi:semctl semaphore 0 susv3-xsi:getpid 0)
               :function 'susv3-xsi:semctl
               :arguments (list semaphore 0 'susv3-xsi:getpid 0)
               :caller 'semaphore-get-pid)) ;;semaphore-get-pid


(defun semaphore-get-value (semaphore index)
  "The system call returns the value of semval  for  the
   semnum-th  semaphore of the set.  The calling process
   must have read access  privileges  on  the  semaphore
   set."
  (check-errno (susv3-xsi:semctl semaphore index susv3-xsi:getval 0)
               :function 'susv3-xsi:semctl
               :arguments (list semaphore index 'susv3-xsi:getval 0)
               :caller 'semaphore-get-value)) ;;semaphore-get-value


(defun semaphore-get-all-values (semaphore)
  "Return  semval  for  all  semaphores  of the set into
   arg.array.  The  argument  semnum  is  ignored.   The
   calling  process  must have read access privileges on
   the semaphore set."
  (let ((semnum (semdesc-number-of-semaphores
                 (semaphore-statistics semaphore))))
    (ffi:with-c-var (d `(ffi:c-array ffi:ushort ,semnum))
      (check-errno (susv3-xsi:semctl semaphore 0 susv3-xsi:getall
                                     (ffi:foreign-address-unsigned
                                      (ffi:c-var-address d)))
                   :function 'susv3-xsi:semctl
                   :arguments (list semaphore 0 'susv3-xsi:getall 'values)
                   :caller 'semaphore-get-all-values)
      (let ((result  (make-array (list semnum)
                                 :element-type '(unsigned-byte 16)
                                 :initial-element 0)))
        (dotimes (i semnum)
          (setf (aref result i) (ffi:element d i)))
        result)))) ;;semaphore-get-all-values


(defun semaphore-number-waiting-for-increase (semaphore index)
  "The system call returns the value of semncnt for  the
   semnum-th  semaphore  of  the set (i.e. the number of
   processes waiting for an increase of semval  for  the
   semnum-th semaphore of the set).  The calling process
   must have read access  privileges  on  the  semaphore
   set."
  (check-errno (susv3-xsi:semctl semaphore index susv3-xsi:getncnt 0)
               :function 'susv3-xsi:semctl
               :arguments (list semaphore index 'susv3-xsi:getncnt 0)
               :caller 'semaphore-number-waiting-for-increase))



(defun semaphore-number-waiting-for-zero (semaphore index)
  "The  system call returns the value of semzcnt for the
   semnum-th semaphore of the set (i.e.  the  number  of
   processes   waiting   for  semval  of  the  semnum-th
   semaphore of the set to become 0).  The calling  pro­
   cess   must   have  read  access  privileges  on  the
   semaphore set."
  (check-errno (susv3-xsi:semctl semaphore index susv3-xsi:getzcnt 0)
               :function 'susv3-xsi:semctl
               :arguments (list semaphore index 'susv3-xsi:getzcnt 0)
               :caller 'semaphore-number-waiting-for-zero))


(defun semaphore-set-value (semaphore index value)
  "Set  the value of semval to arg.val for the semnum-th
   semaphore of the set,  updating  also  the  sem_ctime
   member  of  the  semid_ds structure associated to the
   set.  Undo entries are cleared for altered semaphores
   in  all  processes.   Processes  sleeping on the wait
   queue are awakened if semval becomes 0 or  increases.
   The calling process must have alter access privileges
   on the semaphore set."
  (check-errno (susv3-xsi:semctl semaphore index susv3-xsi:setval value)
               :function 'susv3-xsi:semctl
               :arguments (list semaphore index 'susv3-xsi:setval value)
               :caller 'semaphore-set-value)) ;;semaphore-set-value


(defun semaphore-set-all-values (semaphore values)
  "Set semval  for  all  semaphores  of  the  set  using
   arg.array,  updating also the sem_ctime member of the
   semid_ds  structure  associated  to  the  set.   Undo
   entries  are  cleared  for  altered semaphores in all
   processes.  Processes sleeping on the wait queue  are
   awakened  if some semval becomes 0 or increases.  The
   argument semnum is ignored.  The calling process must
   have alter access privileges on the semaphore set."
  (let ((semnum (semdesc-number-of-semaphores
                 (semaphore-statistics semaphore))))
    (assert (= (length values) semnum))
    (ffi:with-c-var (d `(ffi:c-array ffi:ushort ,semnum) values)
      (check-errno (susv3-xsi:semctl semaphore 0 susv3-xsi:setall
                                     (ffi:foreign-address-unsigned
                                      (ffi:c-var-address d)))
                   :function 'susv3-xsi:semctl
                   :arguments (list semaphore 0 'susv3-xsi:setall 'values)
                   :caller 'semaphore-set-all-values))))


(defun semaphore-operate (semaphore operations &key (no-error '()))
  "
OPERATION: a list of (sem_num sem_op [:no-wait] [:undo])
"
  (ffi:with-c-var (d `(ffi:c-array susv3-xsi:sembuf ,(length operations)))
    (do ((ops operations (cdr ops))
         (i 0 (1+ i)))
        ((null ops))
      (setf (ffi:slot (ffi:element d i) 'susv3-xsi::sem_num) (first  (car ops))
            (ffi:slot (ffi:element d i) 'susv3-xsi::sem_op)  (second (car ops))
            (ffi:slot (ffi:element d i) 'susv3-xsi::sem_flg)
            (+ (if (member :no-wait (cddr (car ops))) susv3-xsi:ipc_nowait 0)
               (if (member :undo    (cddr (car ops))) susv3-xsi:sem_undo   0))))
    (check-errno (susv3-xsi:semop semaphore
                                  (ffi:foreign-address-unsigned
                                   (ffi:c-var-address d))
                                  (length operations))
                 :no-error (if (listp no-error) no-error (list no-error))
                 :function 'susv3-xsi:semctl
                 :arguments (list semaphore operations (length operations))
                 :caller 'semaphore-operate)))



;; (defun operate (sem op undo nowait)
;;  (with-slots (val adj zcnt ncnt wait-for-zero-q wait-for-increase-q) sem
;;   (cond
;;     ((plusp op)   (incf val op) (if undo (decf adj op)))
;;     ((zerop op)   (cond
;;                     ((zerop val))
;;                     (nowait
;;                      (error EAGAIN))
;;                     (t (incf zcnt)
;;                        (enqueue wait-for-zero-q *current-process*))))
;;     ((minusp op)  (cond
;;                     ((< (- op) val)
;;                      (incf val op) (if undo (decf adj op)))
;;                     (nowait
;;                      (error EAGAIN))
;;                     (t (incf ncnt)
;;                        (enqueue wait-for-increase-q *current-process*)))))))

;; (defun p (sem undo) (operate sem -1 undo nil))
;; (defun v (sem undo) (operate sem  1 undo nil))


;;   If sem_op is a positive integer, the operation adds this value to
;;   the  semaphore  value  (semval).   Furthermore,  if  SEM_UNDO  is
;;   asserted for this operation, the system updates the process  undo
;;   count  (semadj)  for  this  semaphore.  This operation can always
;;   proceed - it never forces a process to wait.  The calling process
;;   must have alter permission on the semaphore set.
;;----
;;   If  sem_op  is zero, the process must have read access permission
;;   on the semaphore set.  This is a  "wait-for-zero"  operation:  if
;;   semval  is  zero,  the operation can immediately proceed.  Other­
;;   wise, if IPC_NOWAIT is asserted in sem_flg, the system call fails
;;   with  errno  set to EAGAIN (and none of the operations in sops is
;;   performed).  Otherwise semzcnt (the count  of  processes  waiting
;;   until  this semaphore's value becomes zero) is incremented by one
;;   and the process sleeps until one of the following occurs:
;;
;;   ·      semval becomes 0, at which time the value  of  semzcnt  is
;;          decremented.
;;
;;   ·      The  semaphore set is removed: the system call fails, with
;;          errno set to EIDRM.
;;
;;   ·      The calling process catches a signal: the value of semzcnt
;;          is  decremented  and the system call fails, with errno set
;;          to EINTR.
;;----
;;   If sem_op is less than zero, the process must have alter  permis­
;;   sion on the semaphore set.  If semval is greater than or equal to
;;   the absolute value of sem_op, the operation can  proceed  immedi­
;;   ately:  the  absolute  value of sem_op is subtracted from semval,
;;   and, if SEM_UNDO is  asserted  for  this  operation,  the  system
;;   updates  the  process undo count (semadj) for this semaphore.  If
;;   the  absolute  value  of  sem_op  is  greater  than  semval,  and
;;   IPC_NOWAIT  is  asserted  in sem_flg, the system call fails, with
;;   errno set to EAGAIN (and none of the operations in sops  is  per­
;;   formed).  Otherwise semncnt (the counter of processes waiting for
;;   this semaphore's value to increase) is incremented by one and the
;;   process sleeps until one of the following occurs:
;;
;;   ·      semval becomes greater than or equal to the absolute value
;;          of sem_op, at which time the value of  semncnt  is  decre­
;;          mented,  the  absolute  value of sem_op is subtracted from
;;          semval and, if SEM_UNDO is asserted  for  this  operation,
;;          the  system  updates  the  process undo count (semadj) for
;;          this semaphore.
;;
;;   ·      The semaphore set is removed from the system:  the  system
;;          call fails with errno set to EIDRM.
;;
;;   ·      The calling process catches a signal: the value of semncnt
;;          is decremented and the system call fails with errno set to
;;          EINTR.

;;;; ipc.lisp                         --                     --          ;;;;
