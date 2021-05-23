
(in-package "COM.INFORMATIMAGO.BORDEAUX-THREAD.PATCH")

(defvar *names*  (make-hash-table :weak :key :test 'eq))

(defun make-condition-variable (&key name)
  (let ((semaphore (ccl:make-semaphore)))
    (setf (gethash semaphore *names*) name)
    semaphore))

(defmethod print-object :around ((semaphore ccl:semaphore) stream)
  (let ((name (gethash semaphore *names*)))
    (if name
        (print-unreadable-object (semaphore stream :identity t :type  t)
          (format stream ":NAME ~S" name))
        (call-next-method))))


(defvar *status* (make-hash-table :weak :key :test 'eq))

(defun caller () (third (ccl:backtrace-as-list)))
(defmacro with-lock-held ((place) &body body)
  (let ((vlock (gensym)))
    `(let ((,vlock ,place))
       (ccl:with-lock-grabbed (,vlock)
         (push (list :locking (caller) (bt:current-thread)) (gethash ,vlock *status* '()))
         (unwind-protect
              (progn ,@body)
           (pop (gethash ,vlock *status* '())))))))

(defmethod print-object :around ((lock ccl::recursive-lock) stream)
  (let ((status (gethash lock *status*)))
    (if status
        (print-unreadable-object (lock stream :identity t :type  t)
          (format stream "~S :status ~S" (ccl:lock-name lock) status))
        (call-next-method))))


#-(and)
(ignore

 bt:make-condition-variable
 (com.informatimago.common-lisp.cesarum.utility:hash-table-entries *names*)

 (map nil 'print (com.informatimago.common-lisp.cesarum.utility:hash-table-entries *status*))
 #|
 (#<recursive-lock "down-layer" [ptr @ #x605080] #x3020028D45FD>)
 (#<recursive-lock "Telnet REPL Server Lock" [ptr @ #x10D880] #x30200279729D>)
 (#<recursive-lock "telnet-stream" :status ((:locking
 (funcall "#<STANDARD-METHOD COM.INFORMATIMAGO.CLEXT.TELNET.STREAM::INPUT-BUFFER-FETCH-OCTET (COM.INFORMATIMAGO.CLEXT.TELNET.STREAM:TELNET-STREAM T)>" "#<TELNET-STREAM  #x3020028D75CD>" "NIL")
 #<process Telnet REPL Client #1(21) [semaphore wait] #x3020028D192D>)
 (:locking
 (com.informatimago.clext.telnet.stream::%stream-read-char "#<TELNET-STREAM #x3020028D75CD>" "NIL")
 #<process Telnet REPL Client #1(21) [semaphore wait] #x3020028D192D>)) #x3020028D74BD>
 (:locking
 (funcall "#<STANDARD-METHOD COM.INFORMATIMAGO.CLEXT.TELNET.STREAM::INPUT-BUFFER-FETCH-OCTET (COM.INFORMATIMAGO.CLEXT.TELNET.STREAM:TELNET-STREAM T)>" "#<TELNET-STREAM #x3020028D75CD>" "NIL")
 #<process Telnet REPL Client #1(21) [semaphore wait] #x3020028D192D>)
 (:locking
 (com.informatimago.clext.telnet.stream::%stream-read-char "#<TELNET-STREAM #x3020028D75CD>" "NIL")
 #<process Telnet REPL Client #1(21) [semaphore wait] #x3020028D192D>)) nil
 |#
 )

#|

# -*- mode:org -*-
* Debugging a dead-lock in a program using =bordeaux-threads= in =ccl=.

We have a program with three threads:

#+BEGIN_EXAMPLE
cl-user> (list-threads)
 1) #<process Telnet REPL Client #1 DOWN LAYER(22) [semaphore wait] #x302002A4903D>
 2) #<process Telnet REPL Client #1(21) [semaphore wait] #x302002A469FD>
 3) #<process Telnet REPL Server(20) [Active] #x30200291EB5D>
 4) …
#+END_EXAMPLE

The server thread listens to connections and forks client threads for
accepted connections.

The client thread forks a down layer thread that loops reading bytes
from the client socket, and forwarding them up the protocol layers, up
to a buffer in a =TELNET-STREAM= Gray stream.

The client thread then goes on into a REPL loop using the
=TELNET-STREAM= Gray stream as =*TERMINAL-IO*=.  Writing back to
=*TERMINAL-IO*= goes down to the client socket in this client thread.

Unfortunately, when sending a byte to the upper layer, the down layer
thread hangs waiting for the stream-lock.  Who has locked this stream?

Neither =ccl= nor =bordeaux-threads= are very helpful in debugging that…

What we'd want, is to know what thread are holding a lock.  So we will
implement a macro shadowing  =BT:WITH-LOCK-HELD=, to record that
information into a weak hash-table.  Happily, =ccl= has native weak
hash-tables so we don't have to use
=com.informatimago.clext.closer-weak=.

#+BEGIN_CODE lisp
#+(and ccl debug-condition-variables)
(defpackage "COM.INFORMATIMAGO.BORDEAUX-THREAD.PATCH"
  (:use "COMMON-LISP" "BORDEAUX-THREADS")
  (:shadow "MAKE-CONDITION-VARIABLE" "WITH-LOCK-HELD")
  (:export "MAKE-CONDITION-VARIABLE" "WITH-LOCK-HELD")
  (:documentation "Implements MAKE-CONDITION-VARIABLE on ccl to print the name,
and WITH-LOCK-HELD to record the locking thread."))

(defpackage "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM"
  (:use "COMMON-LISP" "BORDEAUX-THREADS"  …)
  #+(and ccl debug-condition-variables)
  (:shadowing-import-from "COM.INFORMATIMAGO.BORDEAUX-THREAD.PATCH"
                          "MAKE-CONDITION-VARIABLE" "WITH-LOCK-HELD")
  (:export "TELNET-STREAM" …))

(defpackage "COM.INFORMATIMAGO.CLEXT.TELNET.REPL"
  (:use "COMMON-LISP" "BORDEAUX-THREADS"  …)
  #+(and ccl debug-condition-variables)
  (:shadowing-import-from "COM.INFORMATIMAGO.BORDEAUX-THREAD.PATCH"
                          "MAKE-CONDITION-VARIABLE" "WITH-LOCK-HELD")
  (:export "START-REPL-SERVER" …))
#+END_CODE



#+BEGIN_CODE lisp
(in-package "COM.INFORMATIMAGO.BORDEAUX-THREAD.PATCH")

(defvar *status* (make-hash-table :weak :key :test 'eq))

(defun caller () (third (ccl:backtrace-as-list)))

(defmacro with-lock-held ((place) &body body)
  (let ((vlock (gensym)))
    `(let ((,vlock ,place))
       (ccl:with-lock-grabbed (,vlock)
         (push (list :locking (caller) (bt:current-thread)) (gethash ,vlock *status* '()))
         (unwind-protect
              (progn ,@body)
           (pop (gethash ,vlock *status* '())))))))

(defmethod print-object :around ((lock ccl::recursive-lock) stream)
  (let ((status (gethash lock *status*)))
    (if status
        (print-unreadable-object (lock stream :identity t :type  t)
          (format stream "~S :status ~S" (ccl:lock-name lock) status))
        (call-next-method))))
#+END_CODE

#+BEGIN_EXAMPLE
 (map nil 'print (com.informatimago.common-lisp.cesarum.utility:hash-table-entries *status*))

 (#<recursive-lock "down-layer" [ptr @ #x605080] #x3020028D45FD>)
 (#<recursive-lock "Telnet REPL Server Lock" [ptr @ #x10D880] #x30200279729D>)
 (#<recursive-lock "telnet-stream" :status ((:locking
                                             (funcall "#<STANDARD-METHOD COM.INFORMATIMAGO.CLEXT.TELNET.STREAM::INPUT-BUFFER-FETCH-OCTET (COM.INFORMATIMAGO.CLEXT.TELNET.STREAM:TELNET-STREAM T)>" "#<TELNET-STREAM  #x3020028D75CD>" "NIL")
                                             #<process Telnet REPL Client #1(21) [semaphore wait] #x3020028D192D>)
                                            (:locking
                                             (com.informatimago.clext.telnet.stream::%stream-read-char "#<TELNET-STREAM #x3020028D75CD>" "NIL")
                                             #<process Telnet REPL Client #1(21) [semaphore wait] #x3020028D192D>)) #x3020028D74BD>
                   (:locking
                    (funcall "#<STANDARD-METHOD COM.INFORMATIMAGO.CLEXT.TELNET.STREAM::INPUT-BUFFER-FETCH-OCTET (COM.INFORMATIMAGO.CLEXT.TELNET.STREAM:TELNET-STREAM T)>" "#<TELNET-STREAM #x3020028D75CD>" "NIL")
                    #<process Telnet REPL Client #1(21) [semaphore wait] #x3020028D192D>)
                   (:locking
                    (com.informatimago.clext.telnet.stream::%stream-read-char "#<TELNET-STREAM #x3020028D75CD>" "NIL")
                    #<process Telnet REPL Client #1(21) [semaphore wait] #x3020028D192D>)) nil
#+END_EXAMPLE




ccl condition-variables are not named;
bordeaux-threads ignores the name parameter.
So we shadow make-condition-variable and record the name
of the condition variables in a

#+BEGIN_CODE lisp
(in-package "COM.INFORMATIMAGO.BORDEAUX-THREAD.PATCH")
(defvar *names*  (make-hash-table :weak :key :test 'eq))

(defun make-condition-variable (&key name)
  (let ((semaphore (ccl:make-semaphore)))
    (setf (gethash semaphore *names*) name)
    semaphore))

(defmethod print-object :around ((semaphore ccl:semaphore) stream)
  (let ((name (gethash semaphore *names*)))
    (if name
        (print-unreadable-object (semaphore stream :identity t :type  t)
          (format stream ":NAME ~S" name))
        (call-next-method))))
#+END_CODE

|#

