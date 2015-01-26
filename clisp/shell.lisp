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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2002 - 2015
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
(declaim (also-use-packages "FFI"  "LINUX"))
(eval-when (:compile-toplevel :load-toplevel :execute) (require "linux"))
(defpackage "COM.INFORMATIMAGO.CLISP.SHELL"
  (:documentation
   "This package export shell primitives (fork, pipe, redirections, exec).")
  (:use "COMMON-LISP")
  (:export
   ;; variables:
   "*TEMPORARY-PATHNAME*" ;; pathname of the temporary directory.
   ;; macro API:
   "EXECL"  "PIPE-AND-EXEC"  "PIPE"
   "FORK"  "WAIT"  
   ;; low-level:
   "PIPE-AND-EXEC-FUN"))
(in-package  "COM.INFORMATIMAGO.CLISP.SHELL")



(defvar *temporary-pathname* "/tmp" "A path to a temporary directory.")



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


(defmacro execl (path &rest argv)
  "
DO:         Execute the program at path, passing the arguments argv.
EXAMPLE:    (execl \"/bin/ls\" \"ls\" \"-l\" \"-F\" \"/tmp\")
PRE:        (<= 1 (length argv))
NOTE:       Doesn't return!  The current process image is replaced by
            the executed program.
"
  (let* ((argc (1- (length argv)))
         (exec (intern (with-standard-io-syntax (format nil "EXECL~D" argc)))))
    (if (fboundp exec)
        `(,exec ,path ,@argv nil)
        `(progn
           (ffi:def-call-out
               ,exec
               (:language :stdc)
             (:arguments
              (path ffi:c-string)
              ,@(do ((i 0 (1+ i))
                     (arguments nil (cons (list (intern (with-standard-io-syntax
                                                          (format nil "ARGV~D" i)))
                                                'ffi:c-string) arguments)) )
                    ((< argc i) (nreverse arguments)))
              (null ffi:c-string))
             (:return-type ffi:int)
             (:name "execl"))
           (,exec ,path ,@argv nil)))))







;; -------------
;; pipe-and-exec
;; -------------

(defun check-process-list (process-list)
  "PRIVATE.
DO:         Checks and evaluates the process-list.
RETURN:     An evaluated process-list.
"
  (mapcar
   (lambda (process)
     (cond
       ((atom process)
        (error "Invalid process ~S." process))
       ((not (keywordp (car process)))
        (error "Invalid tag for process ~S." process))
       ((eq :begin (cadr process)) process)
       (t (cons (car process)
                (mapcar (lambda (item)
                          (if (stringp item)
                              item
                              (format nil "~A" (eval item))))
                        (cdr process))))))
   process-list))


(defun check-process-tag (process-list tag)
  "PRIVATE.
DO:         Check the process tag.
"
  (unless (member tag process-list
                  :test (function eq)
                  :key  (function car))
    (error "Tag ~S is not in the process-list." tag)))


(defun check-process-fdes (process-list process-fdes &optional triplet)
  "PRIVATE.
DO:         Checks and evaluates the process fdes.
RETURN:     An evaluated process-fdes.
"
  (when (atom process-fdes)
    (error "Invalid file descriptor specification ~S (must be a list)."
           process-fdes))
  (let ((tag (car process-fdes)))
    (check-process-tag process-list tag)
    (cond
      ((= 2 (length process-fdes))
       (when triplet
         (error
          "Invalid file descriptor specification ~S (must have 3 elements)."
          process-fdes))
       (let ((fdes (nth 1 process-fdes)))
         (unless (integerp fdes)
           (setq fdes (eval fdes)))
         (unless (integerp fdes)
           (error "Invalid file descriptor specification ~S (~S should evaluate to an integer)." process-fdes (nth 1 process-fdes)))
         (list tag fdes)))
      ((= 3 (length process-fdes))
       (unless triplet
         (error
          "Invalid file descriptor specification ~S (must have 2 elements)."
          process-fdes))
       (let ((fdes1 (nth 1 process-fdes))
             (fdes2 (nth 2 process-fdes)))
         (unless (integerp fdes1)
           (setq fdes1 (eval fdes1)))
         (unless (integerp fdes1)
           (error "Invalid file descriptor specification ~S (~S should evaluate to an integer)." process-fdes (nth 1 process-fdes)))
         (unless (integerp fdes2)
           (setq fdes2 (eval fdes2)))
         (unless (integerp fdes2)
           (error "Invalid file descriptor specification ~S (~S should evaluate to an integer)." process-fdes (nth 2 process-fdes)))
         (list tag fdes1 fdes2)))
      (t
       (error
        "Invalid file descriptor specification ~S (must have ~D elements)."
        process-fdes (if triplet 3 2))))))

    
(defun check-edge-list (process-list edge-list)
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
  (mapcar
   (lambda (edge)
     (cond
       ((atom edge)
        (error "An edge must be a list, not ~S." edge))
       ;; ------------
       ;; a close edge
       ;; ------------
       ((eq :close (car edge))
        (cons :close
              (mapcar (lambda (pf) (check-process-fdes process-list pf nil))
                      (cdr edge))) )
       ;; -----------
       ;; a dup2 edge
       ;; -----------
       ((eq :duplicate (car edge))
        (cons :duplicate
              (mapc (lambda (pf) (check-process-fdes process-list pf t))
                    (cdr edge))))
       ((/= 2 (length edge))
        (error "Invalid edge ~S (must have two nodes)." edge))
       ;; -----------
       ;; a data edge
       ;; -----------
       ((and (consp (car edge)) (eq :data (caar edge)))
        (unless (= 2 (length (car edge)))
          (error "Invalid data node ~S. Expected (:data form)."  (car edge)))
        (check-process-fdes process-list (cadr edge))
        (list :data
              (cadar edge)
              (check-process-fdes process-list (cadr edge))) )
       ;; -----------------
       ;; a file input edge
       ;; -----------------
       ((stringp (car edge))
        (list :input
              (list :file (car edge))
              (check-process-fdes process-list (cadr edge))) )
       ((and (consp (car edge))
             (eq :file (caar edge)))
        (unless (= 2 (length (car edge)))
          (error "Invalid input file specification ~S. Expected (:FILE fname)."
                 (car edge)))
        (let ((fname (cadar edge)) )
          (setq fname (if (stringp fname) fname
                          (format nil "~A" (eval fname))))
          (list :input
                (list :file fname)
                (check-process-fdes process-list (cadr edge)))))
       ;; ----------------------------
       ;; a file output or append edge
       ;; ----------------------------
       ((stringp (cadr edge))
        (list :output
              (check-process-fdes process-list (car edge))
              (list :file (cadr edge))))
       ((and (consp (cadr edge))
             (eq :file (caadr edge)))
        (unless (or (= 2 (length (cadr edge)))
                    (and (= 3 (length (cadr edge)))
                         (eq :append (nth 2 (cadr edge)))))
          (error (concatenate 'string
                   "Invalid output file specification ~S. "
                   "Expected (LFILE fname [:APPEND]).")
                 (cadr edge)))
        (let ((fname  (cadar edge))
              (append (member :append (cadr edge) :test (function eq))) )
          (setq fname (if (stringp fname) fname
                          (format nil "~A" (eval fname))))
          (list :output 
                (check-process-fdes process-list (car edge))
                (if append
                    (list :file  fname :append)
                    (list :file  fname)))))
       ;; -----------
       ;; a pipe edge
       ;; -----------
       ((and (consp (nth 0 edge))
             (consp (nth 1 edge)))
        (list :pipe
              (check-process-fdes process-list (nth 0 edge))
              (check-process-fdes process-list (nth 1 edge))))
       ;; -----------
       ;; other edges
       ;; -----------
       (t (error "Invalid edge ~S." edge))
       ))
   edge-list))



(defun create-datafiles-and-pipes (edge-list)
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
  (mapcar
   (lambda (edge)
     (cond
       ;; ------------------------------- ;;
       ;; create the temporary file
       ;; data: mkstemp;open;write;open;unlink;close
       ((eq :data (car edge))
        (multiple-value-bind
              (fdesc fpath)
            (linux:|mkstemp| (format nil "~a/lisp-inline-data-XXXXXX"
                                     *temporary-pathname*))
          (when (< fdesc 0)
            (error "LINUX:mkstemp reported error errno=~a." linux::|errno|))
          ;; fill the temporary file
          (with-open-file (data fpath
                                :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create)
            (format nil "~A" (nth 1 edge)))
          (prog1
              (list :data (linux:|open| fpath linux:|O_RDONLY| 0)
                    fpath (nth 2 edge))
            ;; close and delete the temporary file (we keep an input fd).
            (delete-file fpath)
            (linux:|close| fdesc))))
       ;; ------------------------------- ;;
       ;; create the pipe
       ;; pipe: pipe
       ((eq :pipe (car edge))
        (multiple-value-bind (result fdescs) (linux:|pipe|)
          (when (/= 0 result)
            (error "LINUX:pipe returned ~S." result))
          (cons :pipe (cons (aref fdescs 0) (cons (aref fdescs 1)
                                                  (cdr edge))))))
       ;; ------------------------------- ;;
       ;; don't do anything.
       (t edge)))
   edge-list))


(defun prepare-fd (process edge-list)
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
  (let ((tag (car process)))
    (mapc
     (lambda (edge)
       (cond
         ;; ---------------------------------------- ;;
         ;; (:data ddes dname (:tag fdes))
         ((and (eq :data (car edge))
               (eq tag (car (nth 3 edge)) ))
          (let* ((ddes  (nth 1 edge))
                 (fdes  (cadr (nth 3 edge))) )
            (when (/= ddes fdes)
              (linux:|dup2| ddes fdes)
              (linux:|close| ddes)))) ;; "inline" data file.
         ;; ---------------------------------------- ;;
         ;; (:input (file fname) (:tag fdes))
         ((and (eq :input (car edge))
               (eq (car (nth 2 edge)) tag))
          (let* ((fname (cadr (nth 1 edge)))
                 (fdes  (cadr (nth 2 edge)))
                 (odes  (linux:|open| fname linux:|O_RDONLY| 0)) )
            (when (< odes 0)
              (error "Can't open ~S for reading." fname))
            (when (/= odes fdes)
              (linux:|dup2| odes fdes)
              (linux:|close| odes)))) ;; input data file
         ;; ---------------------------------------- ;;
         ;; (:output  (:tag fdes) (file fname [:append]))
         ((and (eq :output (car edge))
               (eq (car (nth 1 edge)) tag))
          (let* ((fdes   (cadr (nth 1 edge)))
                 (fname  (cadr (nth 2 edge)))
                 (append (member :append (nth 2 edge) :test (function eq)))
                 (odes (linux:|open| fname
                              (+ linux:|O_WRONLY| linux:|O_CREAT|
                                 (if append linux:|O_APPEND| linux:|O_TRUNC|))
                              438)) )
            (when (< odes 0)
              (error "Can't open ~S for writting." fname))
            (when (/= odes fdes)
              (linux:|dup2| odes fdes)
              (linux:|close| odes)))) ;; output data file
         ;; ---------------------------------------- ;;
         ;; (:pipe ifdes ofdes (:tag fdes) (:tag fdes)) output pipe
         ((and (eq :pipe (car edge))
               (eq tag (car (nth 3 edge))))
          (let* ((ifdes (nth 1 edge))
                 (ofdes (nth 2 edge))
                 (fdes (cadr (nth 3 edge))))
            (when (/= ofdes fdes)
              (linux:|dup2| ofdes fdes)
              (linux:|close| ifdes)
              (linux:|close| ofdes)))) ;; output pipe
         ;; ---------------------------------------- ;;
         ;; (:pipe ifdes ofdes (:tag fdes) (:tag fdes)) input pipe
         ((and (eq :pipe (car edge) )
               (eq tag (car (nth 4 edge)) ))
          (let* ((ifdes (nth 1 edge))
                 (ofdes (nth 2 edge))
                 (fdes (cadr (nth 4 edge))))
            (when (/= ifdes fdes)
              (linux:|dup2| ifdes fdes)
              (linux:|close| ifdes)
              (linux:|close| ofdes)))) ;; input pipe
         ;; ---------------------------------------- ;;
         ;; (:close (:tag fdes)...)
         ((eq :close (car edge))
          (mapc (lambda (tag-fdes)
                  (when (eq (car tag-fdes) tag)
                    (linux:|close| (cadr tag-fdes))))
                (cdr edge)))
         ;; ---------------------------------------- ;;
         ;; (:duplicate (:tag dfdes sfdes)...)
         ((eq :duplicate (car edge))
          (mapc (lambda (tag-d-s)
                  (when (eq (car tag-d-s) tag)
                    (let ((dst (nth 1 tag-d-s))
                          (src (nth 2 tag-d-s)))
                      (linux:|dup2| src dst)
                      ;; we don't close the src, we leave that to the client.
                      )))
                (cdr edge)))
         ;; ---------------------------------------- ;;
         (t (error "Unknown edge type ~S." edge))))
     edge-list)))



(defun pipe-and-exec (process-list edge-list &key wait)
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
  (setq process-list (check-process-list process-list))
  ;; check the syntax, and evaluate edge-list and canonize:
  (setq edge-list (check-edge-list process-list edge-list))
  ;; 0. input-data must be evaluated and written to temporary files.
  ;; 1. create all the pipes
  (setq edge-list (create-datafiles-and-pipes edge-list))
  
  ;; 2. fork the processes.
  (setq process-list
        (mapcar
         (lambda (process)
           (let ((pid (linux:|fork|)))
             (cond
               ((< pid 0)
                ;; --------------------------------------------- error
                ;; TODO: We should keep the errno along with the process.
                (list :fork-success nil
                      :fork-errno linux::|errno|
                      :process process))
               ((= pid 0)
                ;; --------------------------------------------- child
                ;; 3. in each child in the order specified,
                ;; 3.1. open the file, or
                ;; 3.2. assign the pipe descriptor, or
                ;; 3.3. close the file descriptor, or
                ;; 3.4. dup2 the file decriptor.
                (let ((status 69)) ;; EX_UNAVAILABLE
                  (unwind-protect
                       (progn
                         (prepare-fd process edge-list)
                         ;; 4. exec the program or run the lisp form, then exit.
                         (if (eq :begin (nth 1 process))
                             ;; lisp form
                             (setq status (eval (cons 'progn (cddr process))))
                             ;; program process
                             (eval (cons 'execl (cons (nth 1 process)
                                                      (cdr process))))
                             ))
                    ;; no clean up
                    )
                  (ext:exit status)))
               (t
                ;; --------------------------------------------- parent
                (list :fork-success t
                      :child-pid pid
                      :process process)) ) ;;COND
             ))                            ;;LAMBDA
         process-list))
  ;; 5. in parent, close the pipes 
  (mapc (lambda (edge)
          (cond
            ((eq :data (car edge))
             (linux:|close| (nth 1 edge))) ;; input fd we kept.
            ((eq :pipe (car edge))
             (let* ((p1 (nth 1 edge))
                    (p2 (nth 2 edge)))
               (linux:|close| p1)
               (linux:|close| p2))) ;; pipe open in parent, used by children.
            ))                      ;;LAMBDA
        edge-list)
  (when wait
    ;; 6. wait for all the children.
    (do ((child-count (do* ((processes process-list (cdr processes))
                            (count 0) )
                           ((null processes) count)
                        (setq count (if (getf (car processes) :fork-success)
                                        (1+ count) count))))
         )
        ((= 0 child-count))
      (multiple-value-bind (pid status) (linux:|wait|)
        (when (< 0 pid)
          (let* ((process-place (member
                                 pid process-list
                                 :key (lambda (process)
                                        (getf process :child-pid))))
                 (process (car process-place)))
            (when process
              (setf (getf process :child-status) status)
              (setf (car process-place) process)
              (setq child-count (1- child-count))))))))
  process-list)
 


(defun pipe (process-list &key wait)
  (let ((tag-num 0))
    (setq process-list
          (mapcar (lambda (process)
                    (setq tag-num (1+ tag-num))
                    (cons (intern (with-standard-io-syntax (format nil "PROCESS-~A" tag-num))
                                  (find-package "KEYWORD")) process))
                  process-list))
    (pipe-and-exec process-list
                   (do ((previous (caar process-list) (caar process))
                        (process  (cdr process-list)  (cdr process))
                        (edges nil
                               (cons (list (list previous 1)
                                           (list (caar process) 0)) edges)))
                       ((null process) (nreverse edges)))
                   :wait wait)))


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




(defun fork (body-fun)
  "
RETURN:  pid of child in parent ;
         never in child (exit with result of body-fun as status).
"
  (let ((pid (linux:|fork|)))
    (if (= 0 pid)
        ;; child
        (let ((result (funcall body-fun)))
          (ext:exit
           (cond
             ((numberp result) (logand 255 result))
             ((null    result) 1)
             ((eq t    result) 0)
             (t               0))))
        ;; parent
        pid)))


(defun wait (pid)
  "
RETURN:  pid;status
"
  (linux:|waitpid| pid 0)) ; options: (LOGIOR LINUX:WNOHANG LINUX:WUNTRACED)




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




(defmacro exec-path (&rest command)
  (when (/= 1 (length command))
    (signal 'wrong-number-of-arguments 'exec-path (length command)))
  (setq command 
        (mapcar (lambda (item)
                  (cond
                    ((symbolp item)
                     (setq item (symbol-name item))
                     (let ((utem (string-upcase item)))
                       ;;(SHOW UTEM ITEM)
                       (if (string= utem item)
                           (string-downcase item)
                           item)))
                    (t (format nil  "~a" item))))
                (car command)))
  `(ext:run-program ,(car command)
     :arguments (quote ,(cdr command))
     :input :terminal
     :output :terminal
     :if-output-exists :overwrite
     :wait t))


(defmacro exec-epf (&rest epf)
  (let ((pf (car epf))
        (redirections (cdr epf)) )
    (declare (ignore pf redirections)) (error "NOT IMPLEMENTED YET")
    `( ,@(car epf) )))




(defmacro & (&rest epf)   `(fork (lambda () (exec-epf ,@epf))))


(defmacro run (&rest epf)  `(wait (& ,@epf)))


(defmacro or-else (&rest pf-list)
  (declare (ignore pf-list)) (error "NOT IMPLEMENTED YET"))


(defmacro and-then (&rest pf-list)
  (declare (ignore pf-list)) (error "NOT IMPLEMENTED YET"))


;;;; THE END ;;;;


