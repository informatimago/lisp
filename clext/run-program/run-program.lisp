;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               run-program.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This package exports a stand alone RUN-PROGRAM function that
;;;;    runs on various implementations.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-03-15 <PJB> Updated.
;;;;    2012-03-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.CLEXT.RUN-PROGRAM"
  (:use "COMMON-LISP")
  #+allegro        (:use "EXCL" "EXCL.OSI" )
  #+clisp          (:use "EXT"  "POSIX")
  #+ecl            (:use "EXT")
  #+(or abcl cmu)  (:use "EXTENSIONS")
  #+clozure        (:use "CCL")
  #+sbcl           (:use "SB-EXT")
  (:shadow "MAKE-PROCESS"
           . #1=("RUN-PROGRAM"
                 "PROCESS"
                 "PROCESS-P"
                 "PROCESS-ALIVE-P"
                 "PROCESS-INPUT"
                 "PROCESS-OUTPUT"
                 "PROCESS-ERROR"
                 "PROCESS-PID"
                 "PROCESS-STATUS"
                 "PROCESS-SIGNAL"))
  (:export . #1#)
  (:documentation "
"))
(in-package "COM.INFORMATIMAGO.CLEXT.RUN-PROGRAM")


;;;---------------------------------------------------------------------
;;; PROCESS
;;;---------------------------------------------------------------------

;; RUN-PROGRAM returns a PROCESS structure.

;; (PROCESS-ALIVE-P p) indicates whether the process p is still running (it calls waitpid).
;; if the process is alive, then:
;; (PROCESS-INPUT  p) returns the input  stream of the process, (an output-stream).
;; (PROCESS-OUTPUT p) returns the output stream of the process, (an input-stream).
;; (PROCESS-ERROR  p) returns the error  stream of the process, (an input-stream).
;; Either stream may be NIL, depending on the :input, :output and
;; :error arguments of RUN-PROGRAM.
;; (PROCESS-PID    p) returns the PID of the process.
;; When the process is no more alive:
;; (PROCESS-STATUS p) returns the exit status of the process
;; (PROCESS-SIGNAL p) returns the signal that killed the process (only on unix).

(defstruct process
  "The result of RUN-PROGRAM"
  #+(or abcl clozure cmu sbcl scl) process
  #+(or allegro clisp) input
  #+(or allegro clisp) output
  #+(or allegro clisp) error
  #+(or allegro clisp) pid
  #+(or allegro clisp) %exit-status
  #+(or allegro clisp) signal
  program arguments environment)


(defun decode-unix-status (status)
  (values (ldb (byte 7 0)  status)
          (ldb (byte 9 7)  status)))

#+(or allegro clisp)
(defun process-status (process)
  (or (process-%exit-status process)
      #+allegro (multiple-value-bind (exit-status pid signal)
                    (WAITPID (process-pid process) :wnohang t)
                  (declare (ignore pid))
                  (when exit-status
                    (setf (process-signal       process) signal
                          (process-%exit-status process) exit-status)))
      #+clisp   (multiple-value-bind (pid how status)
                    (handler-case
                        (WAIT :PID (process-pid process)
                              :NOHANG t
                              :UNTRACED nil :STOPPED nil :EXITED t
                              :CONTINUED nil :NOWAIT nil)
                      (OS-ERROR (err)
                        (format *trace-output* "~%Got os-error ~A~%" err)
                        (finish-output *trace-output*)
                        (values nil :exited 0)))
                  ;; (declare (ignore pid))
                  (format *trace-output* "~%Got wait result: pid = ~A  how = ~A  status = ~A~%"
                          pid how status)
                  (finish-output *trace-output*)
                  (case how
                    (:exited   (setf (process-%exit-status process) status))
                    (:signaled (setf (process-signal       process) status
                                     (process-%exit-status process) (- status)))))))

#+(or allegro clisp)
(defun process-alive-p (process)
  "Whether the process is still running."
  (not (process-status process)))


#+(or abcl cmu clozure sbcl)
(defun process-alive-p (process)
  #+(or abcl cmu sbcl)   (process-alive-p           (process-process process))
  #+clozure         (eql (external-process-status   (process-process process))  :running))

#+(or abcl cmu clozure sbcl)
(defun process-input (process)
  #+(or abcl cmu sbcl)   (process-input                    (process-process process))
  #+clozure              (external-process-input-stream    (process-process process)) )

#+(or abcl cmu clozure sbcl)
(defun process-output (process)
  #+(or abcl cmu sbcl)   (process-output                   (process-process process))
  #+clozure              (external-process-output-stream   (process-process process)))

#+(or abcl cmu clozure sbcl)
(defun process-error (process)
  #+(or abcl cmu sbcl)   (process-error                    (process-process process))
  #+clozure              (external-process-error-stream    (process-process process)) )

#+(or abcl cmu clozure sbcl)
(defun process-pid (process)
  #+abcl            (declare (ignore process))
  #+abcl            -1 ; no process-pid in abcl
  #+(or cmu sbcl)   (process-pid                (process-process process))
  #+clozure         (ccl::external-process-pid  (process-process process)))

#+(or abcl cmu clozure sbcl)
(defun process-status (process)
  #+abcl                 (process-exit-code         (process-process process))
  #+(or cmu sbcl)        (process-status            (process-process process))
  #+clozure (nth-value 1 (external-process-status   (process-process process))))



;;;---------------------------------------------------------------------
;;; RUN-PROGRAM
;;;---------------------------------------------------------------------

(defun run-program (program arguments &key (wait t) (input nil) (output nil) (error nil)
                                        (input-element-type 'character)
                                        (input-external-format :default)
                                        (output-element-type 'character)
                                        (output-external-format :default)
                                        (environment nil environmentp))
  "Runs the program with the given list of arguments.

If WAIT is true, then run-program returns only when the program is
finished, otherwise, it returns as soon as the program is launched,
and the caller must call PROCESS-ALIVE-P or PROCESS-STATUS on the
resulting process, to check when the program is finished.

INPUT, OUTPUT and ERROR specify the redirection of the stdin, stdout
and stderr of the program.  They can be NIL (/dev/null), :stream (a
pipe with the lisp process is created), a string or pathname to
redirect to or from a file, or a file or socket stream to redirect to
or from it.

ENVIRONMENT is an alist of STRINGs (name . value) describing the new
environment. The default is to copy the environment of the current
process.
"
  (declare (ignorable input-element-type  input-external-format
                      output-element-type output-external-format))
  (check-type input  (or null (member :stream) string pathname stream))
  (check-type output (or null (member :stream) string pathname stream))
  (check-type error  (or null (member :stream) string pathname stream))

  #+allegro
  (if wait
      (multiple-value-bind (exit-status signal)
          (decode-unix-status
           (apply (function run-shell-command)
                  (concatenate 'vector (list program program) arguments)
                  :wait t
                  :input input        :if-input-does-not-exist :error
                  :output output      :if-output-exists :supersede
                  :error-output error :if-error-output-exists :supersede
                  :separate-streams t
                  (when environmentp (list :environment environment))))
        (make-process :%exit-status exit-status :signal signal
                      :program program
                      :arguments arguments
                      :environment (if environmentp environment :inherit)))
      (multiple-value-bind (inp out err pid)
          (apply (function run-shell-command)
                 (concatenate 'vector (list program program) arguments)
                 :separate-streams t
                 :wait nil
                 :input input        :if-input-does-not-exist :error
                 :output output      :if-output-exists :supersede
                 :error-output error :if-error-output-exists :supersede
                 (when environmentp (list :environment environment)))
        (make-process :input inp :output out :error err :pid pid
                      :program program
                      :arguments arguments
                      :environment (if environmentp environment :inherit))))


  ;; clisp make-{input,output,io}-pipe an run-program don't work with
  ;; stderr.  The only function that allows to redirect stderr is
  ;; ext::launch. It takes streams  (with underlying file
  ;; descriptor/handle), nil, :pipe or :terminal.
  #+clisp
  (labels ((run (input-stream output-stream error-stream)
             (multiple-value-bind (pid-or-status inp out err)
                 (ext::launch program :ARGUMENTS arguments
                                      :WAIT wait
                                      :INPUT input-stream
                                      :OUTPUT output-stream
                                      :ERROR error-stream
                                      :BUFFERED t
                                      :ELEMENT-TYPE output-element-type
                                      :EXTERNAL-FORMAT output-external-format)
               (make-process :input inp :output out :error err
                             (if wait :%exit-status :pid) pid-or-status
                             :program program
                             :arguments arguments
                             :environment (if environmentp environment :inherit))))
           (call-with-stream (direction thing thunk)
             (check-type direction (member :input :output))
             (let ((if-does-not-exist :create)
                   (if-exists :supersede))
               (typecase thing
                 (null
                  (funcall thunk nil))
                 ((or string pathname)
                  (let ((path (pathname thing)))
                    (with-open-file (stream path
                                            :direction direction
                                            :if-exists if-exists
                                            :if-does-not-exist if-does-not-exist)
                      (funcall thunk stream))))
                 (stream
                  (funcall thunk stream))
                 ((member :stream)
                  (funcall thunk :pipe))))))
    (call-with-stream :input input
                      (lambda (input)
                        (call-with-stream :output output
                                          (lambda (output)
                                            (call-with-stream :output error
                                                              (lambda (error)
                                                                (run input output error))))))))


  #+abcl
  (let ((process (make-process :process (apply (function run-program) program arguments
                                               :wait nil
                                               (when environmentp
                                                 (list :environment environment)))
                               :program program
                               :arguments arguments
                               :environment (if environmentp environment :inherit))))
    ;; Note this is susceptible to deadlocks, depending on the
    ;; read/write pattern of the command.  It's up to the caller to
    ;; pass :stream and to deal with it itself.
    (flet ((copy-stream (inp out)
             (loop
               :for line = (read-line inp nil nil)
               :while line
               :do (write-line line out))))
      (typecase input
        (null                 (close (process-input process)))
        ((or string pathname) (with-open-file (inp input)
                                (unwind-protect
                                     (copy-stream inp (process-input process))
                                  (close (process-input process)))))
        (stream               (unwind-protect
                                   (copy-stream input (process-input process))
                                (close (process-input process))))))

    (flet ((call-with-streams (argument-stream process-stream thunk)
             (typecase argument-stream
               (null                 (close process-stream)
                (funcall thunk nil nil))
               ((or string pathname) (with-open-file (out argument-stream
                                                          :direction :output
                                                          :if-does-not-exist :create
                                                          :if-exists :supersede)
                                       (unwind-protect
                                            (funcall thunk process-stream out)
                                         (close process-stream))))
               (stream               (unwind-protect
                                          (funcall thunk process-stream argument-stream)
                                       (close process-stream)))
               (otherwise            (funcall thunk nil nil)))))
      (call-with-streams output (process-output process)
                         (lambda (out-in out-out)
                           (call-with-streams error (process-error process)
                                              (lambda (err-in err-out)
                                                (loop :while (or out-in err-in) :do
                                                  (when out-in
                                                    (let ((line (read-line out-in nil nil)))
                                                      (if line
                                                          (write-line line out-out)
                                                          (setf out-in nil))))
                                                  (when err-in
                                                    (let ((line (read-line err-in nil nil)))
                                                      (if line
                                                          (write-line line err-out)
                                                          (setf err-in nil))))))))))

    (when wait
      (loop :while (process-alive-p process) :do (sleep 0.1)))
    process)



  #+clozure
  (flet ((wrap-stream (direction stream)
           ;; Since stream may not be shared, we make a new stream for
           ;; the process.
           (declare (ignore direction)) stream #-(and)
                                               (typecase stream
                                                 (stream     (ccl::make-basic-stream-instance
                                                              (find-class (if (eql :input direction)
                                                                              'ccl::basic-file-character-input-stream
                                                                              'ccl::basic-file-character-output-stream))
                                                              :stream-device (ccl::stream-device stream direction)
                                                              :direction direction
                                                              :element-type output-element-type
                                                              :encoding output-external-format
                                                              :line-termination #+windows :windows #-windows :unix
                                                              :sharing :lock
                                                              :auto-close t))
                                                 (otherwise  stream))))
    (make-process
     :process (apply (function ccl:run-program)
                     program arguments
                     :wait wait
                     :input  (wrap-stream :input  input)  :if-input-does-not-exist :error
                     :output (wrap-stream :output output) :if-output-exists :supersede
                     :error  (wrap-stream :error  error)  :if-error-exists  :supersede
                     :element-type output-element-type
                     :external-format (list :domain nil
                                            :character-encoding output-external-format
                                            :line-termination #+windows :windows #-windows :unix)
                     :sharing :lock
                     (when environmentp
                       (list :env environment)))
     :program program
     :arguments arguments
     :environment (if environmentp environment :inherit)))


  ;; Note; in ecl run-program the :error arguments are limited to
  ;; :output nil t and string/pathname, no :stream :-(
  #+ecl
  (make-process :process (EXT:RUN-PROGRAM program arguments
                                          :wait wait
                                          :input input
                                          :output output
                                          :error error)
                :program program
                :arguments arguments
                :environment (if environmentp environment :inherit))

  #+sbcl
  (make-process :process (sb-ext:run-program
                          program arguments
                          :wait wait
                          :input  input  :if-input-does-not-exist :error
                          :output output :if-output-exists :supersede
                          :error  error  :if-error-exists  :supersede
                          :external-format output-external-format
                          :search #+win32 t #-win32 nil)
                :program program
                :arguments arguments
                :environment (if environmentp environment :inherit))
  #+(or cmu scl)
  (make-process :process (ext:run-program
                          program arguments
                          :wait wait
                          :input  input  :if-input-does-not-exist :error
                          :output output :if-output-exists :supersede
                          :error  error  :if-error-exists  :supersede)
                :program program
                :arguments arguments
                :environment (if environmentp environment :inherit))

  #-(or allegro clisp abcl clozure cmu ecl sbcl scl)
  (error "~S not implemented for ~A" 'run-program
         (lisp-implementation-version)))


;;--------------------------------------------------
;; Not necessary: ext::launch takes :pipe arguments.
;;--------------------------------------------------
;;
;;
;;
;;
;;
;; #+(and clisp unix) (ffi:def-call-out pipe (:name "pipe")
;;                      (:arguments (pipefd (FFI:C-PTR (FFI:C-ARRAY FFI:INT 2)) :out :alloca))
;;                      (:return-type ffi:int)
;;                      (:language :stdc)
;;                      (:library *libc*))
;; #+(and clisp unix) (ffi:def-c-const +cmd-setfd+        (:name "F_SETFD") (:type ffi:int))
;; #+(and clisp unix) (ffi:def-c-const +fd-close-on-exec+ (:name "FD_CLOEXEC")  (:type ffi:int))
;; #+(and clisp unix) (ffi:def-c-const +cmd-setfl+        (:name "F_SETFL") (:type ffi:int))
;; #+(and clisp unix) (ffi:def-c-const +fl-non-block+     (:name "O_NONBLOCK") (:type ffi:int))
;; #+(and clisp unix) (ffi:def-call-out fcntl (:name "fcntl")
;;                      (:arguments (fd ffi:int :in)
;;                                  (cmd ffi:int :in)
;;                                  (flag ffi:long :in))
;;                      (:return-type ffi:int)
;;                      (:language :stdc)
;;                      (:library *libc*))
;;
;; #+(and clisp win32) (ffi:def-c-type handle ffi:c-pointer)
;; #+(and clisp win32) (ffi:def-c-type dword  ffi:uint32)
;; #+(and clisp win32) (ffi:def-c-type word   ffi:uint16)
;; #+(and clisp win32) (ffi:def-c-type bool   ffi:uint8)
;;
;; #+(and clisp win32) (ffi:def-c-type SECURITY-ATTRIBUTES
;;                         (ffi:c-struct vector
;;                                       (nlength              dword)
;;                                       (lpsecuritydescriptor ffi:c-pointer)
;;                                       (binherithandle       bool)))
;;
;; #+(and clisp win32) (ffi:def-call-out create-pipe (:name "CreatePipe")
;;                       (:arguments (hreadpipe   (ffi:c-ptr handle) :out)
;;                                   (hwritepipe  (ffi:c-ptr handle) :out)
;;                                   (lppipeattributes (ffi:c-pointer security-attributes) :in)
;;                                   (nsize dword :in))
;;                       (:return-type bool)
;;                       (:language :stdc-stdcall)
;;                       (:library "kernel32.dll"))
;;
;; #+clisp
;; (defun make-pipe ()
;;   "Returns two streams, an output stream writing to the pipe and an
;; input stream reading from the pipe."
;;   (flet ((ms (fd dir)
;;             (ext:make-stream fd
;;                              :direction dir
;;                              :element-type 'character
;;                              :external-format CUSTOM:*DEFAULT-FILE-ENCODING*
;;                              :buffered t)))
;;     #+unix
;;     (multiple-value-bind (ret pipes) (pipe)
;;       (if (zerop ret)
;;           (values (ms  (aref pipes 1) :output)
;;                   (ms  (aref pipes 0) :input))
;;           (error "pipe(): returned ~A" ret)))
;;     #+win32
;;     (multiple-value-bind (ret pipe-read pipe-write) (create-pipe nil 4096)
;;       (if (zerop ret)
;;           (error "pipe(): returned ~A" ret)
;;           (values (ms  (FFI:FOREIGN-ADDRESS-UNSIGNED pipe-write) :output)
;;                   (ms  (FFI:FOREIGN-ADDRESS-UNSIGNED pipe-read)  :input))))
;;     #-(or unix win32)
;;     (error "~S not implemented for this target" 'make-pipe)))





;; Note: While it may seem reasonable to write:
;;
;;   (with-open-file (err "TESTERR.TXT"
;;                        :direction :output :if-does-not-exist :create
;;                        :if-exists :supersede)
;;     (write-line "The error output of the command is:" err)
;;     (finish-output err)
;;     (run-program "sh" '("-c" "echo error 1>&2")
;;                  :wait t :input nil :output nil :error err)
;;     (write-line "That was the error output of the command." err))
;;
;; and even:
;;
;;   (with-open-file (inp "TESTINP.TXT")
;;     (read-line inp) ; eat the first line
;;     (run-program "bash" '("-c" "read line ; exit 0")
;;                  :wait t :input inp :output nil :error nil)
;;     (read-line inp))
;;
;; In both cases, since we have dup'ed the file descriptor in the
;; parent and child processes,  we have two file positions, and
;; therefore the I/O done by the parent after the child is run will
;; overwrite or read the data written or read by the child.

;; Some implementations copy the input filee to a temporary file, and
;;
;;;; THE END ;;;;
