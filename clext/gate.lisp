;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               gate.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements multi-threading gates, such as:
;;;;
;;;;    - when a thread waits on a gate, it blocks unconditionally.
;;;;
;;;;    - when a thread signals this gate, all the threads blocked on
;;;;      this gate are unblocked.
;;;;
;;;;    - atomicity of unlocking and waiting on the gate is ensured, so
;;;;      that no signal is "lost".
;;;;
;;;;    It is important that the waiting threads check their external
;;;;    condition (in its mutex) in a loop with the gate-wait call, if
;;;;    that external condition can be modified by waiting threads
;;;;    previously unblocked.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-09-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

(defpackage "COM.INFORMATIMAGO.CLEXT.GATE"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS")
  (:shadowing-import-from "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK"
                          "MAKE-HASH-TABLE")
  (:export "MAKE-GATE" "GATE-NAME" "GATE-WAIT" "GATE-SIGNAL")
  (:documentation "

The gate API is the same as for condition variables.  However, their
semantics differ:

- when a thread waits on a gate, it blocks unconditionally.

- when a thread signals a gate, all the threads blocked on
  this gate are unblocked.

- atomicity of unlocking and waiting on the gate is ensured, so that
  no signal is \"lost\". On the other hand, gate signals without
  blocking threads are ignored.

It is important that the waiting threads check their external
condition (in its mutex) in a loop with the gate-wait call, if
that external condition can be modified by waiting threads
previously unblocked.

"))
(in-package "COM.INFORMATIMAGO.CLEXT.GATE")


#|
The way condition variables are defined in `bordeaux-threads`, and
implemented by `bordeaux-threads` in `ccl` (and possibly in other
implementations) is quite deficient, or to the least, insufficient.

Here is the `bordeaux-threads` specifications:

Resource contention: condition variables

A condition variable provides a mechanism for threads to put
themselves to sleep while waiting for the state of something to
change, then to be subsequently woken by another thread which has
changed the state.

A condition variable must be used in conjunction with a lock to
protect access to the state of the object of interest. The procedure
is as follows:

Suppose two threads A and B, and some kind of notional event channel
C. A is consuming events in C, and B is producing them. CV is a
condition-variable:

1. A acquires the lock that safeguards access to C

2. A threads and removes all events that are available in C

3. When C is empty, A calls CONDITION-WAIT, which atomically
releases the lock and puts A to sleep on CV

4. Wait to be notified; CONDITION-WAIT will acquire the lock again
before returning

5. Loop back to step 2, for as long as threading should continue 

When B generates an event E, it:

1. acquires the lock guarding C

2. adds E to the channel

3. calls CONDITION-NOTIFY on CV to wake any sleeping thread

4. releases the lock 

To avoid the "lost wakeup" problem, the implementation must guarantee
that CONDITION-WAIT in thread A atomically releases the lock and
sleeps. If this is not guaranteed there is the possibility that thread
B can add an event and call CONDITION-NOTIFY between the lock release
and the sleep - in this case the notify call would not see A, which
would be left sleeping despite there being an event available.


`Bordeaux-threads` ``condition-variables`` are  implemented on `ccl`
using ccl ``semaphores``, which contain a counter.  This is good since
it avoids losing notifications, which is important given the way
``condition-wait`` is written for `ccl` in `bordeaux-threads`,
ie. without any atomicity.

On the other hand, the user must be careful to check the external
condition corresponding to the condition variable and to loop over
condition-wait, for this lack of atomicity allows the external
condition to be falsified after the awaking of the thread, or merely
between the notification and the awaking of the thread.


Now, in the presence of multiple consumers, or when the number of
times condition-wait is called must not match the number of times
condition-notify is called (eg. when writing to a pipe in different
chunk sizes than read), this abstraction is totally deficient.


We take the following assumptions:

- multiple consumer threads;

- multiple producer threads;

- the consumers want to block when an external condition is not met;

- the external condition can potentially be realized only by another
thread (not "miraculously"), therefore when another thread does
something that might realize this external condition, it will signal
the gate;

- several threads may signal the gate at about the same time;

- when a gate is signaled, all the threads blocked on it need to
run, in their respective mutex blocks, and check the external
condition for themselves;

- the external condition may persist for more or fewer steps,
processed by the consumer threads.  Ie. the number of gate
signalings doesn't necessarily match the number of gate waitings.
Once the gate is signaled and the blocked threads are awaken,
the gate signal is forgotten.

|#


(defvar *global-lock* (make-lock "gate-lock"))
(defvar *thread-vars* (make-hash-table :weak :key :test 'eq))

(defstruct (gate
            (:constructor %make-gate))
  name
  lock
  (waiting-threads '()))

(defun make-gate (&key (name ""))
  (%make-gate :name name
              :lock (make-lock (format nil "~A/gate-lock" name))))

(defun %get-thread-condition-variable (thread)
  (with-lock-held (*global-lock*)
    (or (gethash thread *thread-vars*)
        (setf (gethash thread *thread-vars*)
              (make-condition-variable
               :name (format nil "~A/gate" (thread-name thread)))))))


(defun condition-variable-name (x) x)

(defun gate-wait (gate lock)
  (let ((my-var (%get-thread-condition-variable (current-thread))))
    (with-lock-held ((gate-lock gate))
      (push my-var (gate-waiting-threads gate))
      (release-lock lock)
      (unwind-protect
           (condition-wait my-var (gate-lock gate))
        (acquire-lock lock)))))

(defun gate-signal (gate)
  (loop :until (acquire-lock (gate-lock gate) nil))
  (unwind-protect
       (progn
         (mapc (function condition-notify) (gate-waiting-threads gate))
         (setf (gate-waiting-threads gate) '()))
    (release-lock (gate-lock gate))))

;;;; THE END ;;;;
