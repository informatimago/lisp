;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               process.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             clisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Multiprocessing for clisp.
;;;;
;;;;    This package doesn't implement threads like in other Common-Lisp
;;;;    implementations.  Rather it uses plain unix processes.
;;;;    This has sever implications for IPC: there is no shared memory,
;;;;    all inter-process communication must go thru pipes or sockets.
;;;;
;;;;    http://www.franz.com/support/documentation/7.0/doc/multiprocessing.htm
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-11-23 <PJB> Added MAKE-PIPE.
;;;;    2004-09-23 <PJB> Added MAKE-XTERM-IO-STREAM.
;;;;    2004-08-03 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2004 - 2004
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
         (ALSO-USE-PACKAGES "LINUX"))
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  ()
  (COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:ADD-NICKNAME
     "COM.INFORMATIMAGO.CLISP.RAW-MEMORY"  "RAW-MEMORY"))
(defpackage "COM.INFORMATIMAGO.SUSV3.PROCESS"
  (:NICKNAMES "PROCESS" "MP")
  (:DOCUMENTATION "Implement a multiprocessing API for clisp.")
  (:USE "COMMON-LISP" 
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.HEAP"
        "COM.INFORMATIMAGO.COMMON-LISP.MEMORY"
        "COM.INFORMATIMAGO.SUSV3.IPC")
  (:EXPORT ))
(in-package "COM.INFORMATIMAGO.SUSV3.PROCESS")

(defclass scheduler-object ()    
  ())


(defclass process (scheduler-object)
  (
   (name  
    :accessor process-name 
    :initarg :name
    :type string  
    :initform nil)
   (reset-action  
    :accessor process-reset-action 
    :initarg :reset-action
    :type string  
    :initform nil)
   (run-reasons  
    :accessor process-run-reasons 
    :initarg :run-reasons
    :type string  
    :initform nil
    :documentation "This function returns the list of run-reasons for
                    process. The list can be changed with setf or
                    related macros and this function or with
                    process-add-run-reason. Any Lisp object can be on
                    the list. A process will not run unless it has at
                    least one run reason and no arrest reasons (see
                    process-arrest-reasons).")
   (arrest-reasons  
    :accessor process-arrest-reasons 
    :initarg :arrest-reasons
    :type string  
    :initform nil
    :documentation "This function returns the list of arrest-reasons
                    for process. The list can be changed with setf or
                    related macros and this function or with
                    process-add-arrest-reason. Any Lisp object can be
                    on the list. A process will not run unless it has
                    at least one run reason (see process-run-reasons)
                    and no arrest reasons.")
   (priority  
    :accessor process-priority 
    :initarg :priority
    :type string  
    :initform nil
    :documentation   "
    This function returns the priority of process. It defaults to 0
    and may be set to any fixnum with setf.
    Returns the priority of process, which must be an instance of
    mp:process. Priority may be any real number. It defaults to 0. It
    may be reset with setf and this function.
    "
    ;; 
    ;; IMPLEMENTATION:
    ;; So, real or fixnum?
    ;; We could use it to set the nice level.
    ;;
    )
   (quantum  
    :accessor process-quantum 
    :initarg :quantum
    :type string  
    :initform nil
    :documentation   "
    This function returns the quantum for process.
    The quantum may be specified when the process is created; it
    defaults to the value of *default-process-quantum* and may be set
    to any real value between 0.1 and 20 with setf.
    ")
   (initial-bindings  
    :accessor process-initial-bindings 
    :initarg :initial-bindings
    :type string  
    :initform nil
    :documentation "This slot of a process stores an alist of initial 
                    special bindings which are established in a process
                    when it is started. The value may be set with setf.")
   (message-interrupt-function  
    :accessor process-message-interrupt-function 
    :initarg :message-interrupt-function
    :type string  
    :initform nil)
   (stack-allocation  
    :accessor process-stack-allocation 
    :initarg :stack-allocation
    :type string  
    :initform nil)
   (run-immediately  
    :accessor process-run-immediately 
    :initarg :run-immediately
    :type string  
    :initform nil)
   ;; ----------------------------------------
   (property-list 
    :accessor process-property-list
  	:initarg :property-list
  	:type list
    :initform nil
    :documentation "The property-list slot of a process implements
                    a generalized property list as a convenient place
                   to store additional information about a process.")
   (resume-hook 
    :accessor process-resume-hook
    :initarg :resume-hook
    :type (or null function)
    :initform nil
    :documentation "It is normal for execution of a process to be 
                    interrupted many times. This is transparent to 
                    the process and usually it is not necessary for 
                    the process to know when its execution is
                    suspended  and resumed. However, if these slots
                    are non-nil,  they should be functions of no
                    arguments which are called on the process'
                    stack-group or thread each time the execution is
                    suspended or resumed (but not when the process is
                    first started or when it is killed).")
   (suspend-hook 
    :accessor process-suspend-hook
    :initarg :suspend-hook
    :type (or null function)
    :initform nil
    :documentation "See documentation of RESUME-HOOK.")
   (thread
    :accessor process-thread
    :initform nil
    :documentation "The thread associated with process.")
   (running :initform :idle)
   (pid     :accessor process-pid :initform 0 :type integer ))
  (:documentation "")) ;;process




(defun symeval-in-process (symbol process)
  "
    This function returns two values. The first is the value of the
    special symbol in the given thread (which may be the current
    thread). It only looks for actual bindings on the thread; if the
    symbol has a global value but is not bound on the thread, the global
    value is not returned.
    The second returned value describes the status of the binding. t is
    returned if the symbol is bound on the thread, nil if the symbol has
    no binding, and :unbound if there is a binding which has been
    subjected to makunbound. In the latter two cases, the first returned
    value is nil.
    "
  ;;
  ;; IMPLEMENTATION:
  ;; If the process is self, evaluate the symbol here
  ;; Otherwise:
  ;; process-1                 scheduler                     process-2
  ;;    |-------(symeval-req)----->|                             |
  ;;    |                          |-------(symeval-req)-------->|
  ;;    |                          |<------(symeval-res)---------|
  ;;    |<------(symeval-res)------|                             |
  ;;
  (declare (ignore symbol process))
  (error "not implemented yet")) ;;symeval-in-process


(defun profile-process-p (process)
  "
    This function returns the value of the profile-process-p flag for
    the thread specified by process. If the value of the flag is
    non-nil, then the space and time runtime analyzers will collect
    samples when this process is executing.
    "
  ;;
  ;; IMPLEMENTATION:
  ;; The scheduler can send periodically 
  ;; a (progn (room t) (get-runt-time))-req to the profiled process.
  ;; (and perhaps backtrace too!)
  ;; We need the API to start the profiler and to collect statistics.
  ;;
  (declare (ignore process))
  (error "not implemented yet")) ;;profile-process-p


(defparameter *all-processes* nil
  "
    The value of this variable is a list of all processes that have ever
    been created and have never completed or been killed.
    The scheduler does not use this list; it is for debugging.
    ") ;;*all-processes*
;; IMPLEMENTATION: 
;; The scheduler keeps an authoritative list of all processes. 
;; *all-processes* is a symbol-macro that check the length of the list
;; and delete or create new process proxy objects as needed.
;;
;; 1- The process data is kept in shared memory pages and the processes
;;    proxies just read this data.
;;
;; 2- The *all-processes* symbol-macro sends a request to the scheduler
;;    and gets back process data to update the proxies.
;;
;; 1 is better, but implies a shared memory mechanisms (with FFI 
;; or print/readabbly).
;;

(defparameter *default-process-quantum* 100
  "
    Default quantum given to each process.
    This is not significant in the current implemetantion 
    where the underlying OS does the actual scheduling.
    ") ;;*default-process-quantum*
;; IMPLEMENTATION: Not significant.


(defun start-scheduler () 
  "
    Start the scheduler process and initialize multiprocessing.
    Multiprocessing is not automatically started in the default
    environment.
    This function starts multiprocessing, which is also started
    automatically the first time a process is started by
    process-reset, directly or indirectly (as by
    process-run-function). 
    "
  (error "not implemented yet")) ;;start-scheduler
;; IMPLEMENTATION:
;; The initial process forks a child process that continues 
;; and becomes the scheduler.
;; The child processes should ignore SIGINTR SIGTERM, etc, and let the 
;; scheduler receive them and forward them to the children.


(defun make-process (&key (name "Anonymous")
                     (class 'process)
                     (reset-action nil) (run-reasons '()) (arrest-reasons '())
                     (resume-hook nil) (suspend-hook nil) (initial-bindings nil)
                     ;; not useful:
                     (priority 0) (quantum 2) run-immediately 
                     message-interrupt-function stack-allocation)
  "
    This function returns a new process object, but does nothing about
    making it runnable.
    "
  ;; IMPLEMENTATION:
  ;; process-1                 scheduler                     process-2
  ;;    |-------(newproc-req)----->|                             |
  ;;    |                          |-----------(fork)----------->|
  ;;    |<------(newproc-rep)------|                             |
  ;; 
  (declare (ignore name class reset-action run-reasons arrest-reasons resume-hook suspend-hook initial-bindings priority quantum run-immediately message-interrupt-function stack-allocation))
  (error "not implemented yet")
  #-(and)
  (let ((pid (linux:|fork|)))
    (cond
      ((= 0 pid) ;; child
       (setf *current-process* (make-process :pid (linux:|getpid|) 
                                             :name name))
       (push *current-process* *process-list*)
       (funcall function)
       (ext:exit *process-status*))
      ((< pid 0)
       (error "cannot fork, errno=~D" linux:|errno|))
      (t ;; parent
       (push (make-process :pid pid :name name) *process-list*)
       (car *process-list*))))) ;;make-process


(defparameter *current-process* #-(and)(make-process :pid (linux:|getpid|))
              "
    The value of this variable is the process which is currently running. 
    After the process module is loaded (either automatically, or because 
    (require :process) is evaluated), the value will be non-nil. 
    This should be treated as a read-only variable.
    ") ;;*current-process*


(defun process-run-function (name-or-keywords function &rest args)
  "
    This function does a make-process, then presets the new process
    with function and args. The first argument is either a string,
    which is the name of the process, or is a list of keyword
    arguments accepted by make-process. The new process is
    returned. By default, the process is killed when and if it
    completes.
    "
  (declare (ignore name-or-keywords function args))
  (error "not implemented yet")) ;;process-run-function

 
(defun process-run-restartable-function (name-or-keywords function &rest args)
  "
    This function is just like process-run-function (just above), but
    automatically provides a :reset-action argument of t. The process
    thus started will restart if it is reset or completes.
    "
  (declare (ignore name-or-keywords function args))
  (error "not implemented yet")) ;;process-run-restartable-function


(defun process-enable (process)
  "
    Makes process active by removing all its run and arrest reasons, 
    then giving it a single run reason of :enable.
    "
  (declare (ignore process))
  (error "not implemented yet")) ;;process-enable


(defun process-disable (process)
  "
    This function makes process inactive by revoking all its run and 
    arrest reasons. The effect is immediate if a process disables itself.
    "
  (declare (ignore process))
  (error "not implemented yet")) ;;process-disable


(defun process-reset (process &optional no-unwind kill)
  "
    This function causes process when it next runs to throw out to its
    present computation, if any, then apply its initial function to
    its initial argument. The no-unwind argument controls what happens
    to process' present computation, if it has one. nil (the default)
    or :unless-current cause the process to be unwound unless it is
    the current process. t unconditionally skips the unwind. Any other
    value is equivalent to t, causing an unconditional unwind, which
    will throw out of the caller even if the process being reset is
    current.
    The argument kill must be nil. (It is maintained only for backward
    compatibility). An error is signaled if it is not.
    "
  ;; IMPLEMENTATION:
  ;; I don't think we will be able to accept no-unwind t
  (declare (ignore process no-unwind kill))
  (error "not implemented yet")) ;;process-reset


(defun process-preset (process function &rest arguments)
  "
    This function sets the initial function and arguments of process,
    then resets any computation in progress in it. This does not make
    process active if it was not already active.
    "
  (declare (ignore process function arguments))
  (error "not implemented yet")) ;;process-preset


(defun process-kill (process &key wait)
  "
    This function resets the process to unwind it, then removes it
    from consideration for sunning and from the *all-processes* list.
    If the wait keyword argument is non-nil, the calling process waits
    until the killed process is really gone. process-kill signals an
    error if the process to be killed is an active immigrant. An
    inactive immigrant is one that was created to handle a lisp call
    from a foreign thread, and has returned from the topmost lisp call
    back into the foreign regime. The thread may still be processing,
    but it has no lisp state. This will kill the lisp process
    associated with that foreign thread, but will not kill the foreign
    thread itself. If it later calls into lisp, a new immigrant
    process will be created for it.
    "
  (declare (ignore process wait))
  (error "not implemented yet")) ;;process-kill


(defun process-interrupt (process function &rest args)
  "
    This function forces process to apply function to args when it
    next executes. When function returns, the original computation of
    process continues. If process is waiting when interrupted, it runs
    the interrupt function and then continues waiting. If process is
    not active, mp:process-interrupt makes it active for the interrupt
    function, then makes it inactive again. If additional interrupts
    are posted to a process when one is already posted, they are all
    run, but in undetermined order.
    In order for process-interrupt to work as described, function must
    return normally. It cannot execute a non-local exit (via, for
    example, throw). If function does exit in a non-local manner,
    process will not continue its computation.
    Processing an interrupt function can be interrupted by additional
    process interrupts that occur before the current one has finished
    executing.
    "
  ;;
  ;; IMPLEMENTATION:
  ;; We won't be able to allow an interrupt function to be interrupted
  ;; unless we use a different signal. We could use a range of real-time signals.
  ;;
  (declare (ignore process function args))
  (error "not implemented yet")) ;;process-interrupt




(defun process-name-to-process (name &key abbrev (error t))
  "
    This function returns the process whose process-name is name. name
    must be a string or a symbol, in which case the print-name is
    used. If the abbrev keyword argument is specified true, then name
    is matched to the beginning of each process-name to find a
    match. The abbrev argument defaults to nil.
    If no process is found whose name is name (or, if abbrev is true,
    whose name begins with name), an error is signaled if error is
    unspecified or true, and nil is returned if error is specified nil.
    "
  (declare (ignore name abbrev error))
  (error "process-name-to-process not implemented yet")
  ) ;;process-name-to-process


(defun process-initial-form (process)
  "
    This function returns a cons of the initial function of process
    and its argument list.
    "
  (declare (ignore process))
  (error "process-initial-form not implemented yet")) ;;process-initial-form


(defun process-wait-function (process)
  "
    This function returns the function used to determine when a
    waiting process becomes runnable. The function is the one
    specified to the currently active call to process-wait on
    process. Wait functions are fully described in the
    process-waitdescription.  (If process is not waiting,
    process-wait-function returns nil.)
    "
  (declare (ignore process))
  (error "process-wait-function not implemented yet")) ;;process-wait-function


(defun process-wait-args (process)
  "
    This function returns the list of arguments passed to the wait
    function (see process-wait-function) when determining when a
    waiting process becomes runnable. See process-wait.
    "
  (declare (ignore process))
  (error "process-wait-args not implemented yet")) ;;process-wait-args


(defun process-add-run-reason (process object)
  "
    This function adds object to the list of run-reasons for process.
    "
  (declare (ignore process object))
  (error "process-add-run-reason not implemented yet")) ;;process-add-run-reason


(defun process-add-arrest-reason (process object)
  "
    This function adds object to the list of arrest-reasons for process.
    "
  (declare (ignore process object))
  (error "process-add-arrest-reason not implemented yet")
  ) ;;process-add-arrest-reason


(defun process-revoke-run-reason (process object)
  "
    This function removes object from the list of run reasons for process.
    "
  (declare (ignore process object))
  (error "process-revoke-run-reason not implemented yet")
  ) ;;process-revoke-run-reason


(defun process-revoke-arrest-reason (process object)
  "
    This function removes object from the list of arrest reasons for process.
    "
  (declare (ignore process object))
  (error "process-revoke-arrest-reason not implemented yet")
  ) ;;process-revoke-arrest-reason


(defun process-runnable-p (process)
  "
    These functions return t if, respectively, process is runnable or
    active. A process is active if it has been reset and not yet
    completed, and has at least one run reason and no arrest
    reasons. It is runnable if it is active and not waiting.
    "
  (declare (ignore process))
  (error "process-runnable-p not implemented yet")) ;;process-runnable-p


(defun process-active-p (process)
  "
    These functions return t if, respectively, process is runnable or
    active. A process is active if it has been reset and not yet
    completed, and has at least one run reason and no arrest
    reasons. It is runnable if it is active and not waiting.
    "
  (declare (ignore process))
  (error "process-active-p not implemented yet")) ;;process-active-p


#|| attributes:
(defun process-priority (process)
  (error "process-priority not implemented yet"));;process-priority


(defun process-quantum (process)
  (error "process-quantum not implemented yet"));;process-quantum
||#

(defun process-whostate (process)
  "
    This function returns the current who-line string of process. This
    string can be used by a window system, for example, as a prompt or
    to indicate something about the status of the process. May be set
    with setf.
    "
  (declare (ignore process))
  (error "process-whostate not implemented yet")) ;;process-whostate


(defmacro without-scheduling (&body body)
  "
    This macro inhibits the system from suspending a process
    involuntarily (asynchronously) during the execution of
    body. However, the system will run another process if the current
    process blocks, waits, or executes a process-allow-schedule. Note
    that without-scheduling returns a single value, not multiple
    values. without-scheduling is intended to be used around short
    critical sections of code, and perhaps to be called frequently, so
    possible overhead of allocating multiple returns is avoided by
    returning a single value.
    "
  ;; IMPLEMENTATION;
  ;; We cannot know when the process will block for another reason 
  ;; (like waitting for input). Otherwise, we can block the other processes.
  (declare (ignore body))
  (error "without-scheduling not implemented yet")) ;;without-scheduling


(defmacro without-interrupts (&body body)
  "
    This macro executes body, protecting against any handling of
    asynchronous interrupts. Execution of body is guaranteed to
    complete without any other process running, or any asynchronous
    interrupt being dispatched, unless the process does something to
    block or otherwise explicitly yield to the scheduler (e.g. with
    mp:process-allow-schedule).
    without-interrupts is implemented very efficiently and so may be
    executed frequently. It is generally bad style to wrap a
    without-interrupts around a body of code that takes a significant
    amount of time to execute because that may impose inappropriate
    high interrupt latency on other (possibly unrelated) interrupt
    handlers. without-interrupts is intended to be used around short
    critical sections of code; use of a process-lock may be more
    appropriate for protecting long bodies of code.
    In native threads (:os-threads) implementations of
    multiprocessing, a lisp process calling a foreign function can
    release control of the lisp environment so that another lisp
    process can run. Any attempt to perform such a heap-releasing call
    within the scope of a without-interrupts block signals an error
    and does not release the heap. Whether error processing overrides
    the without-interrupts block depends on the coding of the
    particular application.
    "
  (declare (ignore body))
  (error "without-interrupts not implemented yet")) ;;without-interrupts


(defparameter *disallow-scheduling* t
  "
    This special variable is bound to t whenever multiprocessing
    scheduling is disabled. For example, the system binds this
    variable to t during the execution of the forms within a
    without-scheduling form.
    This variable should be treated as read-only and should never be
    set or bound by user code.
    ") ;;*disallow-scheduling*


(defun process-sleep (seconds &optional whostate)
  "
    process-sleep suspends the current process for at least the number
    of seconds specified. That number may be any non-negative,
    non-complex number. While the process sleeps, other processes are
    allowed to run.)  
    The whostate (default ''Sleep'') is a string which temporarily
    replaces the process' whostate during the sleep.
    When multiprocessing is initialized, Common Lisp function sleep is
    changed to be equivalent to process-sleep. Instead of causing the
    entire Lisp world to suspend execution for the indicated time,
    only the executing process is suspended.)
    This is usually the desired action.
    "
  ;; IMPLEMENTATION:
  ;; This can be merely SLEEP, but with the temporary binding of the whostate.
  (declare (ignore seconds whostate))
  (error "process-sleep not implemented yet")) ;;process-sleep


(defun process-wait (whostate function &rest arguments)
  "
    This function suspends the current process (the value of
    *current-process*) until applying function to arguments yields
    true. The whostate argument must be a string which temporarily
    replaces the process' whostate for the duration of the wait.)
    This function returns nil.
    See the discussion under the headings Section 4.3 Waiting for
    input from a stream and Section 4.3.1 mp:process-wait vs
    mp:wait-for-input-available.
    "
  ;; IMPLEMENTATION:
  ;; The waiting process will wait (select, socket-status) or will read 
  ;; the scheduler message queue.
  ;; gate-open-p     --> scheduler sends message when gate opens.
  ;; read-no-hang-p  --> socket:socket-status (if possible)
  ;; write-no-hang-p --> socket:socket-status (if possible)
  ;; stream-listen   --> socket:socket-status (if possible)
  ;; other           --> scheduler sends periodic messages to 
  ;;                     the waiting process.
  ;; TO CHECK: whether socket-status works on message queue?
  (declare (ignore whostate function arguments))
  (error "process-wait not implemented yet")) ;;process-wait



(defun process-wait-with-timeout (whostate seconds function &rest args)
  "
    This function is similar to process-wait, but with a timeout. The
    units of time are seconds. The value of seconds may be any real
    number. Negative values are treated the same as 0.)
    The wait will timeout if function does not return true before the
    timeout period expires.
    "
  ;; IMPLEMENTATION:
  ;; Same as process-wait, but the scheduler will send a timeout message.
  (declare (ignore whostate seconds function args))
  (error "process-wait-with-timeout not implemented yet")
  ) ;;process-wait-with-timeout


(defun wait-for-input-available (streams &key wait-function whostate timeout)
  "
    This lower-level function extends the capabilities of process-wait
    and process-wait-with-timeout to allow a process to wait for input
    from multiple streams and to wait for input from a file.
    "
  ;; IMPLEMENTATION:
  ;; socket:socket-status should do it.
  ;; TO CHECK: whether socket-status works on message queue?
  (declare (ignore streams wait-function  whostate timeout))
  (error "wait-for-input-available not implemented yet")
  ) ;;wait-for-input-available


(defmacro with-timeout ((seconds . timeout-forms) &body body)
  "
    This macro evaluates the body as a progn body. If the evaluation
    of body does not complete within the specified interval, execution
    throws out of the body and the timeout-forms are evaluated as a
    progn body, returning the result of the last form.)  The
    timeout-forms are not evaluated if the body completes within
    seconds.
    "
  ;; IMPLEMENTATION:
  ;; We can use either alarm(2) and SIGALRM or ask the scheduler.
  ;; What could happen if SIGALRM occurs at a wrong time vs. the scheduler?
  (declare (ignore seconds timeout-forms body))
  (error "with-timeout not implemented yet")) ;;with-timeout


(defun process-allow-schedule (&optional other-process)
  "
    This function resumes multiprocessing, allowing other processes to
    run. All other processes of equal or higher priority will have a
    chance to run before the executing process is next run. If the
    optional argument is provided, it should be another process.
    "
  (declare (ignore other-process))
  (error "process-allow-schedule not implemented yet")) ;;process-allow-schedule


;;----------------------------------------------------------------------


(defclass gate (scheduler-object)
  ()
  (:documentation "A two-state object that can be 
                   process-waitted efficiently."))
;; IMPLEMENTATION:
;; The simpliest will be to make it only a proxy for a gate 
;; in the scheduler.


(defun make-gate (open)
  "
    Allocates and returns a gate object. The gate's initial state will
    be open if open is true and closed if open is nil.
    "
  (declare (ignore open))
  (error "make-gate not implemented yet")) ;;make-gate


(defun open-gate (gate)
  "
    The gate argument must have been allocated with make-gate. 
    Makes the state of gate open.
    "
  (declare (ignore gate))
  (error "open-gate not implemented yet")) ;;open-gate


(defun close-gate (gate)
  "
    The gate argument must have been allocated with make-gate. 
    Makes the state of gate closed.
    "
  (declare (ignore gate))
  (error "close-gate not implemented yet")) ;;close-gate


(defun gate-open-p (gate)
  "
    The gate argument must have been allocated with make-gate. Returns
    true if gate's state is open and returns nil if it is closed.

    As described in the documentation for gates linked to below,
    gate-open-p is handled specially by process-wait, and so code which
    uses gates runs more efficiently. The speedup can be significant if
    the process often waits.
    "
  (declare (ignore gate))
  (error "gate-open-p not implemented yet")) ;;gate-open-p


;;----------------------------------------------------------------------

(defclass queue (scheduler-object)
  ()
  (:documentation "A FIFO. ENQUEUE and DEQUEUE are atomic."))


(defmethod enqueue ((self queue) object)
  "
    "
  (declare (ignore self object))
  (error "enqueue not implemented yet")) ;;gate-open-p
  

(defmethod dequeue ((self queue) object)
  "
    Dequeuing is provided with a waiting facility, so a process that
    tries to dequeue an object from a queue will (optionally) wait, if
    the queue is empty, until something is placed on it.
    "
  (declare (ignore self object))
  (error "dequeue not implemented yet")) ;;dequeue
  

;;----------------------------------------------------------------------


(defclass process-lock (scheduler-object)
  ()
  (:documentation 
   "
    A process-lock is a defstruct which provides a mechanism for
    interlocking process execution. Lock objects are created with
    make-process-lock. A process-lock is either free or it is seized
    by exactly one process. When a process is seized, a non-nil value
    is stored in the lock object (in the slot named locker). Usually
    this is the process which seized the lock, but can be any Lisp
    object other than nil. Any process which tries to seize the lock
    before it is released will block. This includes the process which
    has seized the lock; the with-process-lock macro protects against
    such recursion.
    ")) ;;process-lock
;; IMPLEMENTATION:
;; The simpliest will be to make it only a proxy for a process lock 
;; in the scheduler.


(defun make-process-lock (&key name)
  "
    This function creates a new lock object. The value of the :name
    keyword argument should be a string which is used for
    documentation and in the whostate of processes waiting for the
    lock. (There are additional keyword argument for internal use not
    listed. They should not be set by user code.)
    "
  (declare (ignore name))
  (error "make-process-lock not implemented yet")) ;;make-process-lock


(defmethod process-lock ((lock process-lock)
                         &optional lock-value whostate timeout)
  "
    This function seizes lock with the value lock-value (which must be non-nil).
    "
  (declare (ignore lock lock-value whostate timeout))
  (error "process-lock not implemented yet")) ;;process-lock


(defmethod process-unlock ((lock process-lock) &optional lock-value)
  "
    This function unlocks lock, setting the value in the locker slot to nil.)
    The value of the locker slot of the lock must be the same as the
    lock-value argument. If it is not, an error is signaled.
    "
  (declare (ignore lock lock-value))
  (error "process-unlock not implemented yet")) ;;process-unlock


(defmethod process-lock-locker ((lock process-lock))
  "
    This function returns the value of the locker slot of lock.
    This value is usually the process holding the lock, but can be
    any Lisp value. If the value is nil, the lock is not locked.
    "
  (declare (ignore lock))
  (error "process-lock-locker not implemented yet")) ;;process-lock-locker


(defmethod process-lock-p ((object t))
  "
    Returns true if object is a lock (as returned by make-process-lock)
    and returns nil otherwise.
    "
  (declare (ignore object))
  nil) ;;process-lock-p


(defmethod process-lock-p ((object process-lock))
  "
    Returns true if object is a lock (as returned by make-process-lock)
    and returns nil otherwise.
    "
  (declare (ignore object))
  t) ;;process-lock-p


(defmacro with-process-lock ((lock &key norecursive) &body body)
  "
    This macro executes the body with lock seized.
    "
  (declare (ignore lock norecursive body))
  (error "with-process-lock not implemented yet")) ;;with-process-lock



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defparameter forks (make-circular
;;                       (loop for i from 0 below 5 collect (make-lock))))
;; 
;; (defparameter philosophers 
;;   (loop 
;;    for i from 0 below 5
;;    for prev = (car forks) then next
;;    for next in (cdr forks)
;;    collect
;;    (make-process
;;     (let ((left prev) (rigth next))
;;       (lambda () 
;;         (loop
;;          (when (with-lock-held (left 5)
;;                  (format t "philosopher ~D took left fork.~%" i)
;;                  (force-output)
;;                  (when (with-lock-held (right 5)
;;                          (format t "philosopher ~D took right fork.~%" i)
;;                          (force-output)
;;                          (format t "philosopher ~D is eating~%" i)
;;                          (force-output)
;;                          (sleep (random 5))
;;                          t)
;;                    (format t "philosopher ~D drop right fork.~%" i))
;;                  (force-output)
;;                  t)
;;            (format t "philosopher ~D drop left fork.~%" i)
;;            (force-output))
;;          (format t "philosopher ~D philosofies.~%" i)
;;          (force-output)))))));;philosophers



;;;;; (defclass process () ())
;;;;; (defun make-process (function &key name))
;;;;; (defgeneric destroy-process (process))
;;;;; (defgeneric suspend-process (process))
;;;;; (defgeneric resume-process (process))
;;;;; 
;;;;; 
;;;;; 
;;;;; (defvar *ALL-PROCESSES* '()
;;;;;   "A list of all alive processes.")
;;;;; 
;;;;; (defvar *CURRENT-PROCESS*  nil
;;;;;   "The current process")
;;;;; 
;;;;; *CURRENT-STACK-GROUP* 
;;;;; *INITIAL-STACK-GROUP* 
;;;;; *MULTI-PROCESSING* 
;;;;; 
;;;;; (defun ALL-PROCESSES ()
;;;;;   "Return a list of all the live processes.")
;;;;; 
;;;;; (defmacro ATOMIC-DECF (place &option increment)
;;;;;   "Decrements the reference by delta in a single atomic operation")
;;;;; 
;;;;; (defmacro ATOMIC-INCF (place &option increment)
;;;;; "Increaments the reference by delta in a single atomic operation")
;;;;; 
;;;;; (defmacro ATOMIC-POP (place)
;;;;; "Atomically pop place.")
;;;;; 
;;;;; (defmacro ATOMIC-PUSH (element place)
;;;;; "Atomically push object onto place.")
;;;;; 
;;;;; (defun CURRENT-PROCESS ()
;;;;; "Returns the current process.")
;;;;; 
;;;;; (defun DESTROY-PROCESS (process)
;;;;; "Destroy a process. The process is sent a interrupt which throws to
;;;;;   the end of the process allowing it to unwind gracefully.")
;;;;; 
;;;;; (defun DISABLE-PROCESS (process)
;;;;;   "Disable process from being runnable until enabled.")
;;;;; 
;;;;; (defun ENABLE-PROCESS (process)
;;;;; "Allow process to become runnable again after it has been disabled.")
;;;;; 
;;;;; INIT-STACK-GROUPS
;;;;; 
;;;;; (defclass LOCK () ())
;;;;; 
;;;;; (defun MAKE-LOCK (...))
;;;;; 
;;;;; MAKE-PROCESS
;;;;; "Make a process which will run FUNCTION when it starts up.  By
;;;;;   default the process is created in a runnable (active) state.
;;;;;   If FUNCTION is NIL, the process is started in a killed state; it may
;;;;;   be restarted later with process-preset.
;;;;; 
;;;;;   :NAME
;;;;; 	A name for the process displayed in process listings.
;;;;; 
;;;;;   :RUN-REASONS
;;;;; 	Initial value for process-run-reasons; defaults to (:ENABLE).  A
;;;;; 	process needs a at least one run reason to be runnable.  Together with
;;;;; 	arrest reasons, run reasons provide an alternative to process-wait for
;;;;; 	controling whether or not a process is runnable.  To get the default
;;;;; 	behavior of MAKE-PROCESS in Allegro Common Lisp, which is to create a
;;;;; 	process which is active but not runnable, initialize RUN-REASONS to
;;;;; 	NIL.
;;;;; 
;;;;;   :ARREST-REASONS
;;;;; 	Initial value for process-arrest-reasons; defaults to NIL.  A
;;;;; 	process must have no arrest reasons in order to be runnable.
;;;;; 
;;;;;   :INITIAL-BINDINGS
;;;;; 	An alist of initial special bindings for the process.  At
;;;;; 	startup the new process has a fresh set of special bindings
;;;;; 	with a default binding of *package* setup to the CL-USER
;;;;; 	package.  INITIAL-BINDINGS specifies additional bindings for
;;;;; 	the process.  The cdr of each alist element is evaluated in
;;;;; 	the fresh dynamic environment and then bound to the car of the
;;;;; 	element."
;;;;; NIL
;;;;; MAKE-STACK-GROUP
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-ACTIVE-P
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-ADD-ARREST-REASON
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-ADD-RUN-REASON
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-ALIVE-P
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-ARREST-REASONS
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-IDLE-TIME
;;;;; "Return the real time elapsed since the given process was last
;;;;;   descheduled. The returned time is a double-float in seconds."
;;;;; NIL
;;;;; PROCESS-INTERRUPT
;;;;; "Interrupt process and cause it to evaluate function."
;;;;; NIL
;;;;; PROCESS-NAME
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-PRESET
;;;;; "Restart process, unwinding it to its initial state and calls
;;;;;   function with args."
;;;;; NIL
;;;;; PROCESS-PROPERTY-LIST
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-REAL-TIME
;;;;; "Return the accrued real time elapsed while the given process was
;;;;;   scheduled. The returned time is a double-float in seconds."
;;;;; NIL
;;;;; PROCESS-REVOKE-ARREST-REASON
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-REVOKE-RUN-REASON
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-RUN-REASONS
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-RUN-TIME
;;;;; "Return the accrued run time elapsed for the given process. The returned
;;;;;   time is a double-float in seconds."
;;;;; NIL
;;;;; PROCESS-STATE
;;;;; NIL
;;;;; NIL
;;;;; PROCESS-WAIT
;;;;; "Causes the process to wait until predicate returns True. Processes
;;;;;   can only call process-wait when scheduling is enabled, and the predicate
;;;;;   can not call process-wait. Since the predicate may be evaluated may
;;;;;   times by the scheduler it should be relative fast native compiled code.
;;;;;   The single True predicate value is returned."
;;;;; NIL
;;;;; PROCESS-WAIT-UNTIL-FD-USABLE
;;;;; "Wait until FD is usable for DIRECTION and return True. DIRECTION should be
;;;;;   either :INPUT or :OUTPUT. TIMEOUT, if supplied, is the number of seconds to
;;;;;   wait before giving up and returing NIL."
;;;;; NIL
;;;;; PROCESS-WAIT-WITH-TIMEOUT
;;;;; "Causes the process to wait until predicate returns True, or the
;;;;;   number of seconds specified by timeout has elapsed. The timeout may
;;;;;   be a fixnum or a float in seconds.  The single True predicate value is
;;;;;   returned, or NIL if the timeout was reached."
;;;;; NIL
;;;;; PROCESS-WHOSTATE
;;;;; "Return the process state which is either Run, Killed, or a wait reason."
;;;;; NIL
;;;;; PROCESS-YIELD
;;;;; "Allow other processes to run."
;;;;; NIL
;;;;; PROCESSP
;;;;; NIL
;;;;; NIL
;;;;; RESTART-PROCESS
;;;;; "Restart process by unwinding it to its initial state and calling its
;;;;;   initial function."
;;;;; NIL
;;;;; SHOW-PROCESSES
;;;;; "Show the all the processes, their whostate, and state. If the optional
;;;;;   verbose argument is true then the run, real, and idle times are also
;;;;;   shown."
;;;;; NIL
;;;;; STACK-GROUP-RESUME
;;;;; NIL
;;;;; NIL
;;;;; WITH-LOCK-HELD
;;;;; "Execute the body with the lock held. If the lock is held by another
;;;;;   process then the current process waits until the lock is released or
;;;;;   an optional timeout is reached. The optional wait timeout is a time in
;;;;;   seconds acceptable to process-wait-with-timeout.  The results of the
;;;;;   body are return upon success and NIL is return if the timeout is
;;;;;   reached. When the wait key is NIL and the lock is held by another
;;;;;   process then NIL is return immediately without processing the body."
;;;;; NIL
;;;;; WITH-TIMEOUT
;;;;; "Executes body and returns the values of the last form in body. However, if
;;;;;   the execution takes longer than timeout seconds, abort it and evaluate
;;;;;   timeout-forms, returning the values of last form."
;;;;; NIL
;;;;; WITHOUT-SCHEDULING
;;;;; "Execute the body the scheduling disabled."
;;;;; NIL
;;;;; NIL
;;;;; *



"
Process for clisp

clisp cannot handle thread (yet), but it has linux:|fork|, (or FFI
fork on unix).  Hence the proposition to implement a PROCESS API for
clisp based on unix processes.

Communication between threads is done with shared variables (a common
address space), and conditions and mutex for synchronization.


Communication between (unix) processes can be done with:

    * shared memory (only a part of the address space) (mmap and SVID shm)
        + connection can be established after forking.
        + no copying need (if the data could be allocated into 
                           the shared memory).
        - no embeded synchronization.

    * process messaging (SVID msg)
        + connection can be established after forking.
        + synchronization (reader may blocks untin a message is received).

    * sockets
        + connection can be established after forking.
        + connection can be established between distributed processes.
        + synchronization (reader may blocks untin a message is received).
        - point to point.
        - uses network resources.

    * named pipes
        + connection can be established after forking.
        + synchronization (reader may blocks untin a message is received).
        - mainly point to point.

    * pipes
        + synchronization (reader may blocks untin a message is received).
        - pipes must be established before forking.
        - mainly point to point.


Synchronization between processes can be done with:
    * semaphores (SVID sem)
    * signals (asynchronously).
    
"

"
start-scheduler

    The first process becomes the scheduler (and forks immediately 
    a main process).


    The scheduler is a server that manages the processes, the locks,
    the shared variables. It communicates with its children thru SVID
    messages, and signals.


    Shared variables are stored as a byte sequence in a shared
    memory. Only values that are printable readably can be stored in
    a shared variable, and only if their types were defined before
    forking the processes.  The value is therefore copied deeply. No
    object. No structure defined after a fork (clisp structures are
    printable readably).  

    It seems we cannot have a symbol-macro at the same time as a
    special variable... So we'll need a special API.  

    For other values, such as PROCESS or LOCK objects, the main
    objects are stored in the heap of the scheduler, and the processes
    hold proxies that synchronize their state with their master in the
    scheduler.


    File descriptors? Forking doen't duplicates the file structure,
    include the file _marker_.



==> creation of shared variables,
    represented with a symbol macro,
    implemented as shared memory (mmap or shm?)

==> creation of locks
    represented with objects,
    implemented as semaphores.


- fork (for this is the only multiprocessing primitive available in clisp).

- processes use pipe to communicate between them.
  (alternatively, they could use socket if distribution over serveral hosts
                was needed)

- 1-parent/multiple-children.
"
;; (with-open-file (out "/tmp/test" :direction :output :if-does-not-exist :create :if-exists :supersede) (format out "abcdefghijk") (force-output out) (let ((pid (linux:|fork|))) (if (zerop pid) (format out "ABCDEFGHIJK") (format out "0123456789")) (force-output out) (when (zerop pid) (sleep 4) (format t "child quits~%") (force-output) (ext:quit))))

"
make-process 

process:fork
process:


- queued messages
- 

make-process function &key name
process-interrupt process function
    functions can't be passed as function,
    it must be a _symbol_ denoting a function.
    (because functions are not printable readably).


destroy-process process
current-process
all-processes
processp object
process-name process
process-state process

process-wait reason predicate
process-wait-with-timeout reason timeout predicate
    predicate can be evaluated
    but no sharing of memory ==> IPC must change state.
without-scheduling &body body
    free from interruptions, thru PM, ok, but otherwise needless
    since there's not memory sharing.


process-yield
    there is no way to really _yield_ the CPU in unix
    pause(2) waits for a signal, we could ask the MP to do some scheduling...


disable-process process
    kill -STOP
enable-process process
    kill -CONT
    but master could still communicate with the process, signaling messages.

restart-process process
"
"
atomic-incf reference
atomic-decf reference
    no memory sharing, but the mp could forward global values.

    each fork ==> duplication of file descriptors (and other resources).
              ==> problem with destructors (database, protocols, etc).

make-lock &optional name
with-lock-held (place &optional state) &body body
    We would need the list of global variables that are modified
    and that should be synchronized.

    Shallow references cannot be transmitted, only a deep copy.
    And only objects printable readably!
"


;; (ffi:def-call-out unix-read  (:name "read")
;;   (:arguments (fd ffi:int) (buf ffi:c-pointer) (nbytes linux:|size_t|))
;;   (:return-type linux:|ssize_t|)
;;   (:library "/lib/libc.so.6")
;;   (:language :stdc))
;; 
;; 
;; (ffi:def-call-out unix-write  (:name "write")
;;   (:arguments (fd ffi:int) (buf ffi:c-pointer) (nbytes linux:|size_t|))
;;   (:return-type linux:|ssize_t|)
;;   (:library "/lib/libc.so.6")
;;   (:language :stdc))
;; 
;; 
;; (defun test-unix-pipe-io ()
;;   (multiple-value-bind (res fds) (linux:|pipe|)
;;     (ffi:with-foreign-string (fstr flen fsiz "Hello")
;;       (print `(wrote ,(unix-write (aref fds 1) fstr fsiz)))
;;       (ffi:with-foreign-object (buf '(ffi:c-array ffi:uchar 512))
;;         (let ((rlen (unix-read (aref fds 0) buf 512)))
;;           (print `(read ,rlen))
;;           (dotimes (i rlen)
;;             (princ (code-char (ffi:element (ffi:foreign-value buf)
;;                                            i)))))))));;test-unix-pipe-io
;; 
;; (TEST-UNIX-PIPE-IO)
;; 
;; (print `(read ,rlen))
;; (dotimes (i rlen)
;;   (princ (code-char (ffi:element (ffi:foreign-value buf)
;;                                  i))))
;; 
;; 
;; (defun copy-from-c-buffer (buffer buflen sequence start)
;;   (typecase sequence
;;     (cons (let ((current (nthcdr start sequence)))
;;             (dotimes (i buflen)
;;               (setf (car current) (ffi:element (ffi:foreign-value buffer) i)
;;                     current (cdr current)))))
;;     (string (warn "You should give a string to UNIX-READ-SEQUENCE!")
;;             (
;;              (vector sequence)
;;              (setf
;;               )))));;copy-from-c-buffer


(defun valid-operator-name-p (string)
  "Test if STRING names a function, macro, or special-operator."
  (let ((symbol (let ((*read-eval* nil)) (read-from-string string))))
    (or (fboundp symbol)
        (macro-function symbol)
        (special-operator-p symbol)))) ;;valid-operator-name-p

;; (or (ignore-errors (return (ext:arglist fname))))

(defconstant +pipe-buffer-size+ 4096)

(defvar fd)

(defun unix-read (&rest args)
  (declare (ignore args))
  (error "unix-read Not implemented yet"))

(defun copy-from-c-buffer (&rest args)
  (declare (ignore args))
  (error "copy-from-c-buffer Not implemented yet"))

(defun unix-read-sequence (sequence &key (start 0) (end nil))
  (let ((count (if end (- end start) (length sequence))))
    (when (zerop count)
      (return-from unix-read-sequence (values 0 sequence)))
    (ffi:with-foreign-object (buffer
                              '(ffi:c-array ffi:uchar +pipe-buffer-size+))
      (loop named :reader do
           (let ((rlen (unix-read fd buffer (min count +pipe-buffer-size+))))
             (cond
               ((< 0 rlen)
                (return-from :reader
                  (values rlen (copy-from-c-buffer buffer rlen sequence start))))
               ((= 0 rlen)
                (return-from :reader (values rlen sequence)))
               (t (case (linux:|errno|)
                    ((linux:|EAGAIN| linux:|EINTR|))
                    ((linux:|EPIPE|) (return-from :reader nil)) ; EOF
                    (otherwise
                     (error "unix read: ~A" (linux:|strerror| (linux:|errno|)))
                     ))))))))) ;;unix-read-sequence



;;   (EXT:MAKE-BUFFERED-INPUT-STREAM function mode)
;;
;;  returns a buffered input STREAM. function is a FUNCTION of 0
;; arguments that returns either NIL (stands for end-of-stream) or up to
;; three values string, start, end. READ-CHAR returns the CHARACTERs of
;; the current string one after another, as delimited by start and end,
;; which default to 0 and NIL, respectively. When the string is consumed,
;; function is called again. The string returned by function should not
;; be changed, otherwise function should copy the string with COPY-SEQ or
;; SUBSEQ beforehand. mode determines the behavior of LISTEN when the
;; current string buffer is empty:

;;   NIL   the stream acts like a FILE-STREAM, i.e. function is called
;;
;;   T   the stream acts like an interactive stream without
;; end-of-stream, i.e. one can assume that further characters will always
;; arrive, without calling function FUNCTION this FUNCTION tells, upon
;; call, if further non-empty strings are to be expected.
;;
;;   CLEAR-INPUT discards the rest of the current string, so function
;;   will be called upon the next READ-CHAR operation.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clisp-version ()
    "Return the major and minor of clisp version as a floating point number."
    (let* ((v (lisp-implementation-version))
           (p (position (character ".") v))
           (p (position (character ".") v :start (1+ p))))
      (read-from-string v nil nil :end p))))


(defun make-unix-pipe (&key (element-type 'character)
                       (external-format CUSTOM:*FOREIGN-ENCODING*)
                       (buffered t))
  (multiple-value-bind (res fds) (linux:|pipe|)
    (unless (= 0 res)
      (error "unix pipe: ~A" (linux:|strerror| (linux:|errno|))))
    (let ((inp (ext:make-buffered-input-stream
                (lambda ()
                  (aref fds 0)
                  :direction :input
                  :element-type element-type
                  :external-format external-format
                  :buffered buffered)
                #+#.(cl:when
                        (cl:<= 2.33
                               (COM.INFORMATIMAGO.SUSV3.PROCESS::clisp-version))
                      :clisp)t))
          (out (ext:make-stream (aref fds 1)
                                :direction :output
                                :element-type element-type
                                :external-format external-format
                                :buffered buffered)))
      ;;(print (list  (aref fds 0) inp (aref fds 1) out))
      ;;(linux:|close| (aref fds 0))
      ;;(linux:|close| (aref fds 1))
      (values inp out))))






;;   (EXT:MAKE-BUFFERED-OUTPUT-STREAM function)
;;
;;   returns a buffered output STREAM. function is a FUNCTION expecting
;; one argument, a SIMPLE-STRING. WRITE-CHAR collects the CHARACTERs in a
;; STRING, until a newline character is written or
;; FORCE-OUTPUT/FINISH-OUTPUT is called. Then function is called with a
;; SIMPLE-STRING as argument, that contains the characters collected so
;; far. CLEAR-OUTPUT dicards the characters collected so far.

(defun make-unix-pipe/does-not-work (&key (element-type 'character)
                                     (external-format CUSTOM:*FOREIGN-ENCODING*)
                                     (buffered t))
  (multiple-value-bind (res fds) (linux:|pipe|)
    (unless (= 0 res)
      (error "unix pipe: ~A" (linux:|strerror| (linux:|errno|))))
    (let ((inp (ext:make-stream (aref fds 0)
                                :direction :input
                                :element-type element-type
                                :external-format external-format
                                :buffered buffered))
          (out (ext:make-stream (aref fds 1)
                                :direction :output
                                :element-type element-type
                                :external-format external-format
                                :buffered buffered)))
      ;;(print (list  (aref fds 0) inp (aref fds 1) out))
      ;;(linux:|close| (aref fds 0))
      ;;(linux:|close| (aref fds 1))
      (values inp out)))) ;;make-unix-pipe/does-not-work


#||
(defparameter *out* nil)
(defparameter *inp* nil)
(multiple-value-setq (*inp* *out*) (make-unix-pipe))
(when (zerop (linux:|fork|))
  (print :child)(force-output)
  (loop for i = (read *inp*) while (< i 10)
     do (format t "~%got ~D~%" i) (force-output))
  (ext:quit))
(loop for i from 0 to 10 do (print i *out*)(force-output *out*)(princ "."))


(loop repeat 2 do
     (linux:set-signal-handler 
      linux:SIGUSR1
      (lambda (signal)  (princ " Got signal ") (throw :hot-potatoe signal)))
     (catch :hot-potatoe
       (princ " Looping ")
       (loop do (sleep 5) (princ ".")))
     (princ " Caught "))


(linux:set-signal-handler linux:SIGALRM
                          (let ((i 0)) (lambda (signal)
                                         (princ ".") (print (incf i) *out*) (force-output *out*))))
(linux:ualarm 1000000 1000000)
||#

#||
(defclass process ()
  ((name :reader name
         :type string
         :initform "Unnamed"
         :initarg :name)
   (pid :reader pid
        :type integer
        :initform 0
        :initarg :pid))
  (:documentation "A process proxy."));;process

(defparameter *parent*
  (make-instance 'process :name "parent" :pid (linux:getppid)))


(defun parent ()
  "
RETURN: The parent process.
"
  *parent*);;parent


||#


(defun server-main (&key display)
  (if (or display (find-package "SWANK"))
      (let* ((xterm-io (make-xterm-io-stream :display display))
             (*standard-output* xterm-io)
             (*standard-input*  xterm-io)
             (*error-output*    xterm-io)
             (*terminal-io*     xterm-io)
             (*query-io*        xterm-io)
             (*debug-io*        xterm-io))
        (iotask-enqueue *standard-input*
                        (make-buffered-discipline (function server-input))
                        "xterm")
        (configuration-repl-start)
        (iotask-poll-loop))
      (ext:with-keyboard
          (let ((*standard-input* ext:*keyboard-input*))
            (iotask-enqueue ext:*keyboard-input*
                            (make-keyboard-discipline (function server-input))
                            "keyboard")
            (configuration-repl-start)
            (iotask-poll-loop))))) ;;server-main


(defvar *pipe-format* (ext:make-encoding
                       :charset 'charset:utf-8
                       :line-terminator :unix))




(defparameter +server-port+ 15000)

(defun server ()
  (let ((lsock (socket:socket-server +server-port+)))
    (unwind-protect
         (loop
            (when (socket:socket-wait lsock 0)
              (let ((remote (socket:socket-accept lsock
                                                  :element-type 'character
                                                  ;; :external-format
                                                  :buffered t
                                                  :timeout 1)))
                (when remote
                  ;; got an incoming connection, let's fork a worker
                  ;; but first, create a socket and connect to it to be
                  ;; able to communicate with this worker.
                  (let ((pid (linux:fork)))
                    (cond
                      ((< pid 0) ;; error
                       (error "Could not fork a worker."))
                      ((= pid 0) ;; child
                       )
                      (t ;; parent
                       (register-worker pid)
                       (format t "~& "))))
                  ))))
      (close lsock)))) ;;server


(DEFUN LIST-INSERT-SEPARATOR (LIST SEPARATOR)
  "
RETURN:  A list composed of all the elements in `list'
         with `separator' in-between.
EXAMPLE: (list-insert-separator '(a b (d e f)  c) 'x)
         ==> (a x b x (d e f) x c)
"
  (DO ((RESULT (IF LIST (LIST (CAR LIST))))
       (LIST (CDR LIST) (CDR LIST)))
      ((NULL LIST) (NREVERSE RESULT))
    (PUSH SEPARATOR RESULT)
    (PUSH (CAR LIST) RESULT)))


(DEFUN CHAR-OR-STRING-P (OBJECT)
  (OR (CHARACTERP OBJECT) (STRINGP OBJECT)))


(DEFUN PJB-UNSPLIT-STRING (STRING-LIST &REST SEPARATOR)
  "Does the inverse than pjb-split-string. If no separator is provided
then a simple space is used."
  (COND
    ((NULL SEPARATOR)         (SETQ SEPARATOR " "))
    ((/= 1 (LENGTH SEPARATOR))
     (ERROR "pjb-unsplit-string: Too many separator arguments."))
    ((NOT (CHAR-OR-STRING-P (CAR SEPARATOR)))
     (ERROR "pjb-unsplit-string: separator must be a string or a char."))
    (T (SETQ SEPARATOR (CAR SEPARATOR))))
  (APPLY 'CONCATENATE 'STRING
         (MAPCAR (LAMBDA (OBJECT)
                   (IF (STRINGP OBJECT)
                       OBJECT
                       (FORMAT NIL "~A" OBJECT)))
                 (LIST-INSERT-SEPARATOR STRING-LIST SEPARATOR)))
  ) ;;PJB-UNSPLIT-STRING


(DEFUN PJB-SPLIT-STRING (STRING &OPTIONAL SEPARATORS)
  "
note:   current implementation only accepts as separators
        a string containing only one character.
"
  (SETQ SEPARATORS (OR SEPARATORS " ")
        STRING (STRING STRING))
  (LET ((SEP (AREF SEPARATORS 0))
        (CHUNKS  '())
        (POSITION 0)
        (NEXTPOS  0)
        (STRLEN   (LENGTH STRING)) )
    (WHILE (<= POSITION STRLEN)
      (WHILE (AND (< NEXTPOS STRLEN)
                  (CHAR/= SEP (AREF STRING NEXTPOS)))
        (SETQ NEXTPOS (1+ NEXTPOS)))
      (SETQ CHUNKS (CONS (SUBSEQ STRING POSITION NEXTPOS) CHUNKS))
      (SETQ POSITION (1+ NEXTPOS))
      (SETQ NEXTPOS  POSITION) )
    (NREVERSE CHUNKS))) ;;PJB-SPLIT-STRING


(DEFUN IPV4-ADDRESS-P (ADDRESS)
  "
PRE:     (or (string address) (symbol address))
RETURN:  Whether ADDRESS as the aaa.bbb.ccc.ddd IPv4 address format.
"
  (LET ((BYTES (PJB-SPLIT-STRING (STRING ADDRESS) ".")))
    (AND (= 4 (LENGTH BYTES))
         (BLOCK :CONVERT
           (NREVERSE
            (MAPCAR (LAMBDA (BYTE)
                      (MULTIPLE-VALUE-BIND (VAL EATEN) (READ-FROM-STRING BYTE)
                        (IF (AND (= EATEN (LENGTH BYTE)) (INTEGERP VAL)
                                 (<= 0 VAL 255))
                            VAL
                            (RETURN-FROM :CONVERT NIL))))
                    (PJB-SPLIT-STRING ADDRESS "."))))))) ;;IPV4-ADDRESS-P






(defun server-repl ()
  (do ((hist 1 (1+ hist))
       (+eof+ (gensym)))
      (nil)
    (format t "~%~A[~D]> " (package-name *package*) hist)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit)(exit)(continue)) :test (function equal)))
       (return-from server-repl))
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (setf *** **   ** *   * (first /))
     (format t "~& --> ~{~S~^ ;~%     ~}~%" /)))) ;;server-repl



(defvar +eof+       (gensym))
(defvar *debugging* nil)

(defvar *prompt* "> ")
(defun configuration-repl (&key (debugging *debugging*))
  (catch :configuration-repl-exit
    (loop
       (format t "~&~A " *prompt*) (finish-output)
       (let ((sexp (read *standard-input* nil +eof+)))
         (if sexp
             (if debugging
                 (parse-one-command sexp)
                 (HANDLER-CASE (parse-one-command sexp)
                   (ERROR (ERR)
                     (apply (function format) *error-output*
                            (simple-condition-format-control err)
                            (simple-condition-format-arguments err)))))
             (throw :configuration-repl-exit nil)))))) ;;configuration-repl


(defun configuration-repl-start ()
  (format t "~&~A " *prompt*)
  (finish-output))


(defun configuration-repl-input (line)
  (let ((sexp (read-from-string line nil +eof+)))
    (unless (eq +eof+ sexp)
      (if *debugging*
          (parse-one-command sexp)
          (HANDLER-CASE (parse-one-command sexp)
            (ERROR (ERR)
              (apply (function format) *error-output*
                     (simple-condition-format-control err)
                     (simple-condition-format-arguments err)))))
      (configuration-repl-start)))) ;;configuration-repl-input


#+(or)(progn
        (load "loader.lisp")
        (configuration-repl :debugging t)
        (filter append allow "127.0.0.1")
        (filter append deny all)
        (connections max-number 40)
        (connections enable)
        (configuration save "/tmp/server.conf")
        (repl)
        )




(defstruct iotask  stream process-event name)


(defparameter *iotasks*   '())
(defparameter *bon-grain* '()
  "Sublist of *iotask* which can be handled by socket:socket-wait.")
(defparameter *ivray*     '()
  "Sublist of *iotask* which cannot be handled by socket:socket-wait.")


(defun iotask-enqueue (stream process-event &optional name)
  (let ((task (make-iotask :stream stream
                           :process-event process-event
                           :name name)))
    (push task *iotasks*)
    (handler-case (socket:socket-status (iotask-stream task) 0)
      (error     ()                           (push task *ivray*))
      (:no-error (s n) (declare (ignore s n)) (push task *bon-grain*)))
    )) ;;iotask-enqueue



(defun iotask-dequeue (task)
  (setf *iotasks*   (delete task *iotasks*))
  (setf *bon-grain* (delete task *bon-grain*))
  (setf *ivray*     (delete task *ivray*)))


(defun iotask-poll-loop ()
  (loop ;; each 0.1 seconds, see second argument of socket-status.
     (when (null *iotasks*) (return))
     (map nil
          (lambda (task status)
            (when status (funcall (iotask-process-event task) task status)))
          *ivray*
          (mapcar (lambda (task)
                    (let ((stream (iotask-stream task)))
                      (cond
                        ((input-stream-p stream)
                         (if (listen stream)
                             :input
                             (if (output-stream-p stream) :output nil)))
                        ((output-stream-p stream) :output)
                        (t  nil))))
                  *ivray*))
     (map nil
          (lambda (task status)
            (when status (funcall (iotask-process-event task) task status)))
          *bon-grain*
          (socket:socket-status
           (mapcar (function iotask-stream) *bon-grain*) 0.1))))


(defun make-buffered-discipline (process-input)
  (lambda (task event)
    (when (member event '(:input :error))
      (funcall process-input task (read-line (iotask-stream task))))))


;; (DEFCONSTANT ALLOW
;; (DEFCONSTANT DENY
;; (DEFCONSTANT ALL
;; (DEFCONSTANT MAX-NUMBER
;; (DEFCONSTANT ENABLE
;; (DEFCONSTANT SAVE
  
(DEFCONSTANT +CR+   13)
(DEFCONSTANT +BS+    8)
(DEFCONSTANT +DEL+ 127)

(defun make-keyboard-discipline (process-input)
  (let ((buffer (make-array '(128) :element-type 'character :fill-pointer 0)))
    (lambda (task event)
      (when (eq :input event)
        (let* ((ich (read-char (iotask-stream task)))
               (ch  (system::input-character-char ich)))
          (cond
            ((null ch))
            ((= (char-code ch) +CR+)
             (terpri)
             (funcall process-input
                      task (subseq buffer 0 (fill-pointer buffer)))
             (setf (fill-pointer buffer) 0))
            ((or (= (char-code ch) +BS+) (= (char-code ch) +DEL+))
             (when (< 0 (fill-pointer buffer))
               (princ (code-char +BS+))
               (princ " ")
               (princ (code-char +BS+))
               (decf (fill-pointer buffer))))
            (t
             (princ ch)
             (vector-push ch buffer))))
        (finish-output))))) ;;make-keyboard-discipline


(defun server-input (task line)
  (if (string-equal "(QUIT)" line)
      (iotask-dequeue task)
      (configuration-repl-input line))) ;;server-input


#||
(progn (ext:with-keyboard
           (socket:socket-status (list ext:*keyboard-input*) nil)
         (unread-char  (system::input-character-char
                        (read-char ext:*keyboard-input*))
                       *standard-input*))
       (print (read-line)))
||#


#||
(defun child.send (sexp)
  (print sexp child.pipe)
  (linux:|kill| child.pid linux:|SIGUSR1|))

(child.send '(throw :exit 1))
(child.send '(print test1))

;; set-signal-handler --> ONE signal is queued and processed when the handler exists.

(defmessage symeval-req (symbol process))
(defmessage symeval-res (symbol process status value))

(defmessage profile-req ())
(defmessage profile-res (status profile-data))

(defmessage newproc-req (name class reset-action run-reasons arrest-reasons
                              resume-hook suspend-hook initial-bindings))
(defmessage newproc-res (process status))


(defmessage setproc-req (process attribute value))
(defmessage setproc-res (process status))

(defmessage preset-req (process function args))
(defmessage preset-res (process status))

(defmessage reset-req (process))
(defmessage reset-res (process status))

(defmessage killproc-req (process wait))
(defmessage killproc-req (process status))

(defmessage interrupt-req (process function args))
(defmessage interrupt-res (process status value))
(defmessage disallow-interrupts-req ())
(defmessage disallow-interrupts-res (status))
(defmessage allow-interrupts-req ())
(defmessage allow-interrupts-res (status))

(defmessage disallow-scheduling-req ())
(defmessage disallow-scheduling-res (status))
(defmessage allow-scheduling-req ())
(defmessage allow-scheduling-res (status))

(defmessage start-wait-req (gate period timeout))
(defmessage start-wait-res (status gate counter))
(defmessage stop-wait-req ())
(defmessage stop-wait-res (status))

(defmessage timeout-req (timeout))
(defmessage timeout-res (status)) ;; immediate ; interrupt when timeout.

(defmessage lock-req (lock value timeout))
(defmessage lock-res (status))

(defmessage unlock-req (lock value))
(defmessage unlock-res (status))

||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defmessage
;; scheduler




(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  ;; implement with alarm and
  (declare (ignore seconds timeout-forms body))
  #+(or)(with-signal-handler LINUX:|SIGALRM|)
  (error "not implemented yet")
  ;; or implement with the scheduler when it's started.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

