;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              shell.lisp
;;;;LANGUAGE:          Common-Lisp
;;;;SYSTEM:            clisp
;;;;USER-INTERFACE:    clisp
;;;;DESCRIPTION
;;;;
;;;;    This package export shell primitives (fork, pipe, redirections, exec).
;;;;
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2002-12-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2002
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;****************************************************************************
(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "FFI"  "LINUX"))
(eval-when (:compile-toplevel :load-toplevel :execute) (require "linux"))
(defPACKAGE "COM.INFORMATIMAGO.CLISP.SHELL"
  (:DOCUMENTATION
   "This package export shell primitives (fork, pipe, redirections, exec).")
  (:use "COMMON-LISP")
  (:EXPORT
   ;; variables:
   "*TEMPORARY-PATHNAME*" ;; pathname of the temporary directory.
   ;; macro API:
   "EXECL"  "PIPE-AND-EXEC"  "PIPE"
   "FORK"  "WAIT"  
   ;; low-level:
   "PIPE-AND-EXEC-FUN"))
(in-package  "COM.INFORMATIMAGO.CLISP.SHELL")



(DEFVAR *TEMPORARY-PATHNAME* "/tmp" "A path to a temporary directory.")



;;; ;; DEF-C-CALL-OUT is deprecated, use FFI:DEF-CALL-OUT instead
;;; ;; *** - FFI::LOOKUP-FOREIGN-FUNCTION: A foreign function "execv" does not exist

;;; ;;(FFI:DEFAULT-FOREIGN-LANGUAGE :STDC)


;;; ;; From the Solaris 2.5 and the Linux manpage:

;;; ;; execve is the "primitive" syscall.
;;; (FFI:DEF-CALL-OUT EXECVE
;;;   (:ARGUMENTS (PATH C-STRING)
;;;               (ARGV (C-ARRAY-PTR C-STRING))
;;;               (ENVP (C-ARRAY-PTR C-STRING)))
;;;   (:RETURN-TYPE INT)
;;;   (:NAME "execve")
;;;   (:LANGUAGE :STDC)
;;;   );;EXECVE


;;; ;; execv and execvp are based on execve.
;;; (FFI:DEF-CALL-OUT EXECV
;;;   (:ARGUMENTS (PATH C-STRING) (ARGV (C-ARRAY-PTR C-STRING)))
;;;   (:RETURN-TYPE INT)
;;;   (:NAME "execv")
;;;   (:LANGUAGE :STDC)
;;;   );;EXECV


;;; (FFI:DEF-CALL-OUT EXECVP
;;;   (:ARGUMENTS (FILE C-STRING) (ARGV (C-ARRAY-PTR C-STRING)))
;;;   (:RETURN-TYPE INT)
;;;   (:NAME "execvp")
;;;   (:LANGUAGE :STDC)
;;;   );;EXECVP



;;; (DEFUN EXECL (PATH &REST ARGS)
;;;   "
;;; DO:         Execute the program at path, passing the arguments argv.
;;; EXAMPLE:    (execl \"/bin/ls\" \"ls\" \"-l\" \"-F\" \"/tmp\")
;;; PRE:        (<= 1 (length argv))
;;; NOTE:       Usually doesn't return!
;;;             The current process image is replaced by the executed program.
;;; "
;;;   (EXECV PATH (APPLY #'VECTOR ARGS))
;;;   );;EXECL


(DEFMACRO EXECL (PATH &REST ARGV)
  "
DO:         Execute the program at path, passing the arguments argv.
EXAMPLE:    (execl \"/bin/ls\" \"ls\" \"-l\" \"-F\" \"/tmp\")
PRE:        (<= 1 (length argv))
NOTE:       Doesn't return!  The current process image is replaced by
            the executed program.
"
  (LET* ((ARGC (1- (LENGTH ARGV)))
         (EXEC (INTERN (with-standard-io-syntax (FORMAT NIL "EXECL~D" ARGC)))))
    (IF (FBOUNDP EXEC)
        `(,EXEC ,PATH ,@ARGV NIL)
        `(PROGN
           (FFI:DEF-CALL-OUT
               ,EXEC
               (:LANGUAGE :STDC)
             (:ARGUMENTS
              (PATH FFI:C-STRING)
              ,@(DO ((I 0 (1+ I))
                     (ARGUMENTS NIL (CONS (LIST (INTERN (with-standard-io-syntax
                                                          (FORMAT NIL "ARGV~D" I)))
                                                'FFI:C-STRING) ARGUMENTS)) )
                    ((< ARGC I) (NREVERSE ARGUMENTS)))
              (NULL FFI:C-STRING))
             (:RETURN-TYPE FFI:INT)
             (:NAME "execl"))
           (,EXEC ,PATH ,@ARGV NIL)))))







;; -------------
;; pipe-and-exec
;; -------------

(DEFUN CHECK-PROCESS-LIST (PROCESS-LIST)
  "PRIVATE.
DO:         Checks and evaluates the process-list.
RETURN:     An evaluated process-list.
"
  (MAPCAR
   (LAMBDA (PROCESS)
     (COND
       ((ATOM PROCESS)
        (ERROR "Invalid process ~S." PROCESS))
       ((NOT (KEYWORDP (CAR PROCESS)))
        (ERROR "Invalid tag for process ~S." PROCESS))
       ((EQ :BEGIN (CADR PROCESS)) PROCESS)
       (T (CONS (CAR PROCESS)
                (MAPCAR (LAMBDA (ITEM)
                          (IF (STRINGP ITEM)
                              ITEM
                              (FORMAT NIL "~A" (EVAL ITEM))))
                        (CDR PROCESS))))))
   PROCESS-LIST))


(DEFUN CHECK-PROCESS-TAG (PROCESS-LIST TAG)
  "PRIVATE.
DO:         Check the process tag.
"
  (UNLESS (MEMBER TAG PROCESS-LIST
                  :TEST (FUNCTION EQ)
                  :KEY  (FUNCTION CAR))
    (ERROR "Tag ~S is not in the process-list." TAG)))


(DEFUN CHECK-PROCESS-FDES (PROCESS-LIST PROCESS-FDES &OPTIONAL TRIPLET)
  "PRIVATE.
DO:         Checks and evaluates the process fdes.
RETURN:     An evaluated process-fdes.
"
  (WHEN (ATOM PROCESS-FDES)
    (ERROR "Invalid file descriptor specification ~S (must be a list)."
           PROCESS-FDES))
  (LET ((TAG (CAR PROCESS-FDES)))
    (CHECK-PROCESS-TAG PROCESS-LIST TAG)
    (COND
      ((= 2 (LENGTH PROCESS-FDES))
       (WHEN TRIPLET
         (ERROR
          "Invalid file descriptor specification ~S (must have 3 elements)."
          PROCESS-FDES))
       (LET ((FDES (NTH 1 PROCESS-FDES)))
         (UNLESS (INTEGERP FDES)
           (SETQ FDES (EVAL FDES)))
         (UNLESS (INTEGERP FDES)
           (ERROR "Invalid file descriptor specification ~S (~S should evaluate to an integer)." PROCESS-FDES (NTH 1 PROCESS-FDES)))
         (LIST TAG FDES)))
      ((= 3 (LENGTH PROCESS-FDES))
       (UNLESS TRIPLET
         (ERROR
          "Invalid file descriptor specification ~S (must have 2 elements)."
          PROCESS-FDES))
       (LET ((FDES1 (NTH 1 PROCESS-FDES))
             (FDES2 (NTH 2 PROCESS-FDES)))
         (UNLESS (INTEGERP FDES1)
           (SETQ FDES1 (EVAL FDES1)))
         (UNLESS (INTEGERP FDES1)
           (ERROR "Invalid file descriptor specification ~S (~S should evaluate to an integer)." PROCESS-FDES (NTH 1 PROCESS-FDES)))
         (UNLESS (INTEGERP FDES2)
           (SETQ FDES2 (EVAL FDES2)))
         (UNLESS (INTEGERP FDES2)
           (ERROR "Invalid file descriptor specification ~S (~S should evaluate to an integer)." PROCESS-FDES (NTH 2 PROCESS-FDES)))
         (LIST TAG FDES1 FDES2)))
      (T
       (ERROR
        "Invalid file descriptor specification ~S (must have ~D elements)."
        PROCESS-FDES (IF TRIPLET 3 2))))))

    
(DEFUN CHECK-EDGE-LIST (PROCESS-LIST EDGE-LIST)
  "PRIVATE.
DO:         Checks the syntax of the edge-list and evalutes file names
            and descriptors. Issue error calls.
RETURN:     A canonized edge-list.
EDGE-LIST:  specifies the pipe and input or output as:
            pipe          ((process-tag fdes) (process-tag fdes))
                      --> (pipe (process-tag fdes) (process-tag fdes))
            input-file    ((file file-name)   (process-tag fdes))
                      --> (input (file file-name)   (process-tag fdes))
            input-file    (file-name          (process-tag fdes))
                      --> (input (file file-name)   (process-tag fdes))
            input-data    ((data data-form)   (process-tag fdes))
                      --> (data data-form     (process-tag fdes))
            output-file   ((process-tag fdes) (file file-name))
                      --> (output (process-tag fdes) (file file-name))
            output-file   ((process-tag fdes) file-name)
                      --> (output (process-tag fdes) (file file-name))
            append-file   ((process-tag fdes) (file file-name :append))
                      --> (output (process-tag fdes) (file file-name :append))
            close-fdes    (close (process-tag fdes)...)
            dup2-fdes     (dup2  (process-tag tfdes sfdes)...)
"
  (MAPCAR
   (LAMBDA (EDGE)
     (COND
       ((ATOM EDGE)
        (ERROR "An edge must be a list, not ~S." EDGE))
       ;; ------------
       ;; a close edge
       ;; ------------
       ((EQ :CLOSE (CAR EDGE))
        (CONS :CLOSE
              (MAPCAR (LAMBDA (PF) (CHECK-PROCESS-FDES PROCESS-LIST PF NIL))
                      (CDR EDGE))) )
       ;; -----------
       ;; a dup2 edge
       ;; -----------
       ((EQ :DUPLICATE (CAR EDGE))
        (CONS :DUPLICATE
              (MAPC (LAMBDA (PF) (CHECK-PROCESS-FDES PROCESS-LIST PF T))
                    (CDR EDGE))))
       ((/= 2 (LENGTH EDGE))
        (ERROR "Invalid edge ~S (must have two nodes)." EDGE))
       ;; -----------
       ;; a data edge
       ;; -----------
       ((AND (CONSP (CAR EDGE)) (EQ :DATA (CAAR EDGE)))
        (UNLESS (= 2 (LENGTH (CAR EDGE)))
          (ERROR "Invalid data node ~S. Expected (:data form)."  (CAR EDGE)))
        (CHECK-PROCESS-FDES PROCESS-LIST (CADR EDGE))
        (LIST :DATA
              (CADAR EDGE)
              (CHECK-PROCESS-FDES PROCESS-LIST (CADR EDGE))) )
       ;; -----------------
       ;; a file input edge
       ;; -----------------
       ((STRINGP (CAR EDGE))
        (LIST :INPUT
              (LIST :FILE (CAR EDGE))
              (CHECK-PROCESS-FDES PROCESS-LIST (CADR EDGE))) )
       ((AND (CONSP (CAR EDGE))
             (EQ :FILE (CAAR EDGE)))
        (UNLESS (= 2 (LENGTH (CAR EDGE)))
          (ERROR "Invalid input file specification ~S. Expected (:FILE fname)."
                 (CAR EDGE)))
        (LET ((FNAME (CADAR EDGE)) )
          (SETQ FNAME (IF (STRINGP FNAME) FNAME
                          (FORMAT NIL "~A" (EVAL FNAME))))
          (LIST :INPUT
                (LIST :FILE FNAME)
                (CHECK-PROCESS-FDES PROCESS-LIST (CADR EDGE)))))
       ;; ----------------------------
       ;; a file output or append edge
       ;; ----------------------------
       ((STRINGP (CADR EDGE))
        (LIST :OUTPUT
              (CHECK-PROCESS-FDES PROCESS-LIST (CAR EDGE))
              (LIST :FILE (CADR EDGE))))
       ((AND (CONSP (CADR EDGE))
             (EQ :FILE (CAADR EDGE)))
        (UNLESS (OR (= 2 (LENGTH (CADR EDGE)))
                    (AND (= 3 (LENGTH (CADR EDGE)))
                         (EQ :APPEND (NTH 2 (CADR EDGE)))))
          (ERROR (CONCATENATE 'STRING
                   "Invalid output file specification ~S. "
                   "Expected (LFILE fname [:APPEND]).")
                 (CADR EDGE)))
        (LET ((FNAME  (CADAR EDGE))
              (APPEND (MEMBER :APPEND (CADR EDGE) :TEST (FUNCTION EQ))) )
          (SETQ FNAME (IF (STRINGP FNAME) FNAME
                          (FORMAT NIL "~A" (EVAL FNAME))))
          (LIST :OUTPUT 
                (CHECK-PROCESS-FDES PROCESS-LIST (CAR EDGE))
                (IF APPEND
                    (LIST :FILE  FNAME :APPEND)
                    (LIST :FILE  FNAME)))))
       ;; -----------
       ;; a pipe edge
       ;; -----------
       ((AND (CONSP (NTH 0 EDGE))
             (CONSP (NTH 1 EDGE)))
        (LIST :PIPE
              (CHECK-PROCESS-FDES PROCESS-LIST (NTH 0 EDGE))
              (CHECK-PROCESS-FDES PROCESS-LIST (NTH 1 EDGE))))
       ;; -----------
       ;; other edges
       ;; -----------
       (T (ERROR "Invalid edge ~S." EDGE))
       ))
   EDGE-LIST))



(DEFUN CREATE-DATAFILES-AND-PIPES (EDGE-LIST)
  "PRIVATE.
"
  ;; pre-process edge-list:
  ;; 0. input-data must be evaluated and written to temporary files.
  ;; 1. create all the pipes
  ;;
  ;; (data data-form (process-tag fdes))
  ;; --> (data data-fdes data-fpath (process-tag fdes))
  ;; mkstemp returns an open file descriptor and the file name.
  ;; We reopen the file as input only before deleting it.
  ;;
  ;; (pipe (process-tag fdes) (process-tag fdes))
  ;; --> (pipe in-fdes out-fdes (process-tag fdes) (process-tag fdes))
  ;;
  (MAPCAR
   (LAMBDA (EDGE)
     (COND
       ;; ------------------------------- ;;
       ;; create the temporary file
       ;; data: mkstemp;open;write;open;unlink;close
       ((EQ :DATA (CAR EDGE))
        (MULTIPLE-VALUE-BIND
              (FDESC FPATH)
            (LINUX:|mkstemp| (FORMAT NIL "~a/lisp-inline-data-XXXXXX"
                                     *TEMPORARY-PATHNAME*))
          (WHEN (< FDESC 0)
            (ERROR "LINUX:mkstemp reported error errno=~a." LINUX::|errno|))
          ;; fill the temporary file
          (WITH-OPEN-FILE (DATA FPATH
                                :DIRECTION :OUTPUT
                                :IF-EXISTS :OVERWRITE
                                :IF-DOES-NOT-EXIST :CREATE)
            (FORMAT NIL "~A" (NTH 1 EDGE)))
          (PROG1
              (LIST :DATA (LINUX:|open| FPATH LINUX:O_RDONLY 0)
                    FPATH (NTH 2 EDGE))
            ;; close and delete the temporary file (we keep an input fd).
            (DELETE-FILE FPATH)
            (LINUX:|close| FDESC))))
       ;; ------------------------------- ;;
       ;; create the pipe
       ;; pipe: pipe
       ((EQ :PIPE (CAR EDGE))
        (MULTIPLE-VALUE-BIND (RESULT FDESCS) (LINUX:|pipe|)
          (WHEN (/= 0 RESULT)
            (ERROR "LINUX:pipe returned ~S." RESULT))
          (CONS :PIPE (CONS (AREF FDESCS 0) (CONS (AREF FDESCS 1)
                                                  (CDR EDGE))))))
       ;; ------------------------------- ;;
       ;; don't do anything.
       (T EDGE)))
   EDGE-LIST))


(DEFUN PREPARE-FD (PROCESS EDGE-LIST)
  "PRIVATE.
NOTE:       called in the child process `PROCESS'.
DO:         open files, assign pipe descriptors, close file descriptors,
            dup2 file descriptors, etc, following edge-list instructions,
            for the given process.
"
  ;; 3. in each child in the order specified,
  ;; 3.1. open the file, or
  ;; 3.2. assign the pipe descriptor, or
  ;; 3.3. close the file descriptor, or
  ;; 3.4. dup2 the file decriptor.
  (LET ((TAG (CAR PROCESS)))
    (MAPC
     (LAMBDA (EDGE)
       (COND
         ;; ---------------------------------------- ;;
         ;; (:data ddes dname (:tag fdes))
         ((AND (EQ :DATA (CAR EDGE))
               (EQ TAG (CAR (NTH 3 EDGE)) ))
          (LET* ((DDES  (NTH 1 EDGE))
                 (FDES  (CADR (NTH 3 EDGE))) )
            (WHEN (/= DDES FDES)
              (LINUX:|dup2| DDES FDES)
              (LINUX:|close| DDES)))) ;; "inline" data file.
         ;; ---------------------------------------- ;;
         ;; (:input (file fname) (:tag fdes))
         ((AND (EQ :INPUT (CAR EDGE))
               (EQ (CAR (NTH 2 EDGE)) TAG))
          (LET* ((FNAME (CADR (NTH 1 EDGE)))
                 (FDES  (CADR (NTH 2 EDGE)))
                 (ODES  (LINUX:|open| FNAME LINUX:O_RDONLY 0)) )
            (WHEN (< ODES 0)
              (ERROR "Can't open ~S for reading." FNAME))
            (WHEN (/= ODES FDES)
              (LINUX:|dup2| ODES FDES)
              (LINUX:|close| ODES)))) ;; input data file
         ;; ---------------------------------------- ;;
         ;; (:output  (:tag fdes) (file fname [:append]))
         ((AND (EQ :OUTPUT (CAR EDGE))
               (EQ (CAR (NTH 1 EDGE)) TAG))
          (LET* ((FDES   (CADR (NTH 1 EDGE)))
                 (FNAME  (CADR (NTH 2 EDGE)))
                 (APPEND (MEMBER :APPEND (NTH 2 EDGE) :TEST (FUNCTION EQ)))
                 (ODES (LINUX:|open| FNAME
                              (+ LINUX:O_WRONLY linux:O_CREAT
                                 (IF APPEND LINUX:O_APPEND LINUX:O_TRUNC))
                              438)) )
            (WHEN (< ODES 0)
              (ERROR "Can't open ~S for writting." FNAME))
            (WHEN (/= ODES FDES)
              (LINUX:|dup2| ODES FDES)
              (LINUX:|close| ODES)))) ;; output data file
         ;; ---------------------------------------- ;;
         ;; (:pipe ifdes ofdes (:tag fdes) (:tag fdes)) output pipe
         ((AND (EQ :PIPE (CAR EDGE))
               (EQ TAG (CAR (NTH 3 EDGE))))
          (LET* ((IFDES (NTH 1 EDGE))
                 (OFDES (NTH 2 EDGE))
                 (FDES (CADR (NTH 3 EDGE))))
            (WHEN (/= OFDES FDES)
              (LINUX:|dup2| OFDES FDES)
              (LINUX:|close| IFDES)
              (LINUX:|close| OFDES)))) ;; output pipe
         ;; ---------------------------------------- ;;
         ;; (:pipe ifdes ofdes (:tag fdes) (:tag fdes)) input pipe
         ((AND (EQ :PIPE (CAR EDGE) )
               (EQ TAG (CAR (NTH 4 EDGE)) ))
          (LET* ((IFDES (NTH 1 EDGE))
                 (OFDES (NTH 2 EDGE))
                 (FDES (CADR (NTH 4 EDGE))))
            (WHEN (/= IFDES FDES)
              (LINUX:|dup2| IFDES FDES)
              (LINUX:|close| IFDES)
              (LINUX:|close| OFDES)))) ;; input pipe
         ;; ---------------------------------------- ;;
         ;; (:close (:tag fdes)...)
         ((EQ :CLOSE (CAR EDGE))
          (MAPC (LAMBDA (TAG-FDES)
                  (WHEN (EQ (CAR TAG-FDES) TAG)
                    (LINUX:|close| (CADR TAG-FDES))))
                (CDR EDGE)))
         ;; ---------------------------------------- ;;
         ;; (:duplicate (:tag dfdes sfdes)...)
         ((EQ :DUPLICATE (CAR EDGE))
          (MAPC (LAMBDA (TAG-D-S)
                  (WHEN (EQ (CAR TAG-D-S) TAG)
                    (LET ((DST (NTH 1 TAG-D-S))
                          (SRC (NTH 2 TAG-D-S)))
                      (LINUX:|dup2| SRC DST)
                      ;; we don't close the src, we leave that to the client.
                      )))
                (CDR EDGE)))
         ;; ---------------------------------------- ;;
         (T (ERROR "Unknown edge type ~S." EDGE))))
     EDGE-LIST)))



(DEFUN PIPE-AND-EXEC (PROCESS-LIST EDGE-LIST &KEY WAIT)
  "
RETURN:         NOT UP TO DATE a list of processes
                (:tag status :begin form...) or
                (:tag status program arg...) for the processes
                that could be run, and of the form: (nil :tag :begin form...) or
                (nil :tag program arg...) for the processes that could not
                be forked.

PROCESS-LIST:   each process is specified as a list (:tag :begin form...)
                or (:tag program arg...).

EDGE-LIST:      specifies the pipe and input or output as:
                pipe          ((process-tag fdes) (process-tag fdes))
                input-file    ((:file file-name)  (process-tag fdes))
                input-file    (\"file-name\"      (process-tag fdes))
                input-data    ((:data data-expr)  (process-tag fdes))
                output-file   ((process-tag fdes) (file file-name))
                output-file   ((process-tag fdes) \"file-name\")
                append-file   ((process-tag fdes) (file file-name :append))
                close-fdes    (:close             (process-tag fdes)...)
                dup2-fdes     (:duplicate         (process-tag tfdes sfdes)...)

                file-name can be an expression only inside a (:file ...)
                In abreviated form, it must be a string literal.
                fdes can be expressions.

                program and arg, as well as data and file-name in the case it's
                encapsulated into a (file ...) will be evaluated once more
                (can be forms).

                Expressions in edge-list or in process-list are evaluated,
                except the forms in (:tag begin form...)
                wich are evaluated in the forked child process, and the
                data-expr in (data data-expr) which is evaluated in
                the result of this macro (in pipe-and-exec-fun).
"
  ;; 0. input-data must be evaluated and written to temporary files.
  ;; 1. create all the pipes
  ;; 2. fork the processes.
  ;; 3. in each child in the order specified,
  ;; 3.1. open the file, or
  ;; 3.2. assign the pipe descriptor, or
  ;; 3.3. close the file descriptor, or
  ;; 3.4. dup2 the file decriptor.
  ;; 4. exec the program or run the lisp form, then exit.
  ;; 5. in parent, close the pipes
  ;; 6. in parent, wait for all the children.
  ;;
  ;; check the syntax, and evalutate process-list:
  (SETQ PROCESS-LIST (CHECK-PROCESS-LIST PROCESS-LIST))
  ;; check the syntax, and evaluate edge-list and canonize:
  (SETQ EDGE-LIST (CHECK-EDGE-LIST PROCESS-LIST EDGE-LIST))
  ;; 0. input-data must be evaluated and written to temporary files.
  ;; 1. create all the pipes
  (SETQ EDGE-LIST (CREATE-DATAFILES-AND-PIPES EDGE-LIST))
  
  ;; 2. fork the processes.
  (SETQ PROCESS-LIST
        (MAPCAR
         (LAMBDA (PROCESS)
           (LET ((PID (LINUX:|fork|)))
             (COND
               ((< PID 0)
                ;; --------------------------------------------- error
                ;; TODO: We should keep the errno along with the process.
                (LIST :FORK-SUCCESS NIL
                      :FORK-ERRNO LINUX::|errno|
                      :PROCESS PROCESS))
               ((= PID 0)
                ;; --------------------------------------------- child
                ;; 3. in each child in the order specified,
                ;; 3.1. open the file, or
                ;; 3.2. assign the pipe descriptor, or
                ;; 3.3. close the file descriptor, or
                ;; 3.4. dup2 the file decriptor.
                (LET ((STATUS 69)) ;; EX_UNAVAILABLE
                  (UNWIND-PROTECT
                       (PROGN
                         (PREPARE-FD PROCESS EDGE-LIST)
                         ;; 4. exec the program or run the lisp form, then exit.
                         (IF (EQ :BEGIN (NTH 1 PROCESS))
                             ;; lisp form
                             (SETQ STATUS (EVAL (CONS 'PROGN (CDDR PROCESS))))
                             ;; program process
                             (EVAL (CONS 'EXECL (CONS (NTH 1 PROCESS)
                                                      (CDR PROCESS))))
                             ))
                    ;; no clean up
                    )
                  (EXT:EXIT STATUS)))
               (T
                ;; --------------------------------------------- parent
                (LIST :FORK-SUCCESS T
                      :CHILD-PID PID
                      :PROCESS PROCESS)) ) ;;COND
             ))                            ;;LAMBDA
         PROCESS-LIST))
  ;; 5. in parent, close the pipes 
  (MAPC (LAMBDA (EDGE)
          (COND
            ((EQ :DATA (CAR EDGE))
             (LINUX:|close| (NTH 1 EDGE))) ;; input fd we kept.
            ((EQ :PIPE (CAR EDGE))
             (LET* ((P1 (NTH 1 EDGE))
                    (P2 (NTH 2 EDGE)))
               (LINUX:|close| P1)
               (LINUX:|close| P2))) ;; pipe open in parent, used by children.
            ))                      ;;LAMBDA
        EDGE-LIST)
  (WHEN WAIT
    ;; 6. wait for all the children.
    (DO ((CHILD-COUNT (DO* ((PROCESSES PROCESS-LIST (CDR PROCESSES))
                            (COUNT 0) )
                           ((NULL PROCESSES) COUNT)
                        (SETQ COUNT (IF (GETF (CAR PROCESSES) :FORK-SUCCESS)
                                        (1+ COUNT) COUNT))))
         )
        ((= 0 CHILD-COUNT))
      (MULTIPLE-VALUE-BIND (PID STATUS) (LINUX:|wait|)
        (WHEN (< 0 PID)
          (LET* ((PROCESS-PLACE (MEMBER
                                 PID PROCESS-LIST
                                 :KEY (LAMBDA (PROCESS)
                                        (GETF PROCESS :CHILD-PID))))
                 (PROCESS (CAR PROCESS-PLACE)))
            (WHEN PROCESS
              (SETF (GETF PROCESS :CHILD-STATUS) STATUS)
              (SETF (CAR PROCESS-PLACE) PROCESS)
              (SETQ CHILD-COUNT (1- CHILD-COUNT))))))))
  PROCESS-LIST)
 


(DEFUN PIPE (PROCESS-LIST &KEY WAIT)
  (LET ((TAG-NUM 0))
    (SETQ PROCESS-LIST
          (MAPCAR (LAMBDA (PROCESS)
                    (SETQ TAG-NUM (1+ TAG-NUM))
                    (CONS (INTERN (with-standard-io-syntax (FORMAT NIL "PROCESS-~A" TAG-NUM))
                                  (FIND-PACKAGE "KEYWORD")) PROCESS))
                  PROCESS-LIST))
    (PIPE-AND-EXEC PROCESS-LIST
                   (DO ((PREVIOUS (CAAR PROCESS-LIST) (CAAR PROCESS))
                        (PROCESS  (CDR PROCESS-LIST)  (CDR PROCESS))
                        (EDGES NIL
                               (CONS (LIST (LIST PREVIOUS 1)
                                           (LIST (CAAR PROCESS) 0)) EDGES)))
                       ((NULL PROCESS) (NREVERSE EDGES)))
                   :WAIT WAIT)))


;;; ;; pipe          ((process-tag fdes) (process-tag fdes))
;;; ;; input-file    ((< file-name)      (process-tag fdes))
;;; ;; input-data    ((<< data)          (process-tag fdes))
;;; ;; output-file   ((process-tag fdes) (> file-name))
;;; ;; append-file   ((process-tag fdes) (>> file-name))
;;; ;; close-fdes    (- process-tag fdes)
;;; ;; dup2-fdes     (= process-tag tfdes sfdes)


;;; ;; pipe          ((process-tag fdes) (process-tag fdes))
;;; ;; input-file    ((< file-name)      (process-tag fdes))
;;; ;; input-data    ((<< data)          (process-tag fdes))
;;;       data: mkstemp;open;write;open;unlink;close
;;; ;; output-file   ((process-tag fdes) (> file-name))
;;; ;; append-file   ((process-tag fdes) (>> file-name))
;;; ;; close-fdes    (- process-tag fdes)
;;; ;; dup2-fdes     (= process-tag tfdes sfdes)

;;; '(
;;;   ((:pass 1)   (:gpg  6))
;;;   ((:tar  1)   (:gpg  0))
;;;   ((:gpg  1)   (> "root-0.tar.bz2.gpg"))
;;;   ((< "/dev/null") (:tar  0))
;;;   ((< "/dev/null") (:pass 0))
;;;   )


;;; ;; pipe          (\| (process-tag fdes) (process-tag fdes))
;;; ;; input-file    (<  file-name          (process-tag fdes))
;;; ;; input-data    (<< data               (process-tag fdes))
;;; ;; output-file   (>  (process-tag fdes) file-name)
;;; ;; append-file   (>> (process-tag fdes) file-name)
;;; ;; close-fdes    (-  (process-tag fdes)...)
;;; ;; dup2-fdes     (=  (process-tag tfdes sfdes)...)

;;; '(
;;;   (\|  (:pass 1)   (:gpg  6))
;;;   (<<  password    (:gpg  6))
;;;   (\|  (:tar  1)   (:gpg  0))
;;;   (>   (:gpg  1)   "root-0.tar.bz2.gpg")
;;;   (>>  (:tar  2)   "errors")
;;;   (>>  (:gpg  2)   "errors")
;;;   (-   (:tar  0) (:pass 0))
;;;   (=   (:pass 2 3) (:gpg 2 3))
;;;   )

;;; ;; pipe          ((process-tag fdes) (process-tag fdes))
;;; ;; input-file    (file-name          (process-tag fdes))
;;; ;; input-data    ((:data data)       (process-tag fdes))
;;; ;; output-file   ((process-tag fdes) file-name)
;;; ;; append-file   ((process-tag fdes) file-name :append)
;;; ;; close-fdes    (:close (process-tag fdes)...)
;;; ;; dup2-fdes     (:duplicate   (process-tag tfdes sfdes)...)

;;; '(
;;;   ((:pass 1)   (:gpg  6))
;;;   ((data  password)  (:gpg  6))
;;;   ((:tar  1)   (:gpg  0))
;;;   ((:gpg  1)   "root-0.tar.bz2.gpg")
;;;   ((:tar  2)   "errors" :append)
;;;   ((:gpg  2)   "errors" :append)
;;;   (close (:tar  0)   (:pass 0))
;;;   (dup   (:pass 2 3) (:gpg 2 3))
;;;   )




(DEFUN FORK (BODY-FUN)
  "
RETURN:  pid of child in parent ;
         never in child (exit with result of body-fun as status).
"
  (LET ((PID (LINUX:|fork|)))
    (IF (= 0 PID)
        ;; child
        (LET ((RESULT (FUNCALL BODY-FUN)))
          (EXT:EXIT
           (COND
             ((NUMBERP RESULT) (LOGAND 255 RESULT))
             ((NULL    RESULT) 1)
             ((EQ T    RESULT) 0)
             (T               0))))
        ;; parent
        PID)))


(DEFUN WAIT (PID)
  "
RETURN:  pid;status
"
  (LINUX:|waitpid| PID 0)) ; options: (LOGIOR LINUX:WNOHANG LINUX:WUNTRACED)




;; epf ::= ( pf redir1 ... redirn )
;;
;; pf ::= 
;;       (begin . scheme-code)          ; Run scheme-code in a fork.
;;       (| pf1 ... pfn)                ; Simple pipeline
;;       (|+ connect-list pf1 ... pfn)  ; Complex pipeline   
;;       (epf . epf)                    ; An extended process form.    
;;       (pgm arg1 ... argn)            ; Default: exec the program.
;;
;; pf ::= 
;;       (begin . common-lisp-code)        ; Run common-lisp-code in a fork.
;;       (pipe  pf1 ... pfn)               ; Simple pipeline
;;       (pipe+ connect-list pf1 ... pfn)  ; Complex pipeline   
;;       (epf . epf)                       ; An extended process form.    
;;       (pgm arg1 ... argn)               ; Default: exec the program.
;;
;; connect-list ::= ((from1 from2 ... to) ...)
;;
;; redir ::=
;;       (< [fdes] file-name)  Open file for read. 
;;       (> [fdes] file-name)  Open file create/truncate. 
;;       (<< [fdes] object)    Use object's printed rep. 
;;       (>> [fdes] file-name) Open file for append. 
;;       (= fdes fdes/port)    Dup2 
;;       (- fdes/port)         Close fdes/port. 
;;       stdports              0,1,2 dup'd from standard ports. 


;; For redirection we can implement two cases:
;; - it's an external file redirection,
;;     then we open the file and put it in the wanted fdes.
;;          the file can be closed once forked.
;;
;; - it's an internal data source or sink (<< [fdes] object), (begin cl-code)
;;     then we open a pipe and put it in the wanted fdes.
;;          These pipes associated with their stream should then be handled
;;          in lisp...




(DEFMACRO EXEC-PATH (&REST COMMAND)
  (WHEN (/= 1 (LENGTH COMMAND))
    (SIGNAL 'WRONG-NUMBER-OF-ARGUMENTS 'EXEC-PATH (LENGTH COMMAND)))
  (SETQ COMMAND 
        (MAPCAR (LAMBDA (ITEM)
                  (COND
                    ((SYMBOLP ITEM)
                     (SETQ ITEM (SYMBOL-NAME ITEM))
                     (LET ((UTEM (STRING-UPCASE ITEM)))
                       ;;(SHOW UTEM ITEM)
                       (IF (STRING= UTEM ITEM)
                           (STRING-DOWNCASE ITEM)
                           ITEM)))
                    (T (FORMAT NIL  "~a" ITEM))))
                (CAR COMMAND)))
  `(EXT:RUN-PROGRAM ,(CAR COMMAND)
     :ARGUMENTS (QUOTE ,(CDR COMMAND))
     :INPUT :TERMINAL
     :OUTPUT :TERMINAL
     :IF-OUTPUT-EXISTS :OVERWRITE
     :WAIT T))


(DEFMACRO EXEC-EPF (&REST EPF)
  (LET ((PF (CAR EPF))
        (REDIRECTIONS (CDR EPF)) )
    (DECLARE (IGNORE PF REDIRECTIONS)) (ERROR "NOT IMPLEMENTED YET")
    `( ,@(CAR EPF) )))




(DEFMACRO & (&REST EPF)   `(FORK (LAMBDA () (EXEC-EPF ,@EPF))))


(DEFMACRO RUN (&REST EPF)  `(WAIT (& ,@EPF)))


(DEFMACRO OR-ELSE (&REST PF-LIST)
  (DECLARE (IGNORE PF-LIST)) (ERROR "NOT IMPLEMENTED YET"))


(DEFMACRO AND-THEN (&REST PF-LIST)
  (DECLARE (IGNORE PF-LIST)) (ERROR "NOT IMPLEMENTED YET"))


;;;; THE END ;;;;


