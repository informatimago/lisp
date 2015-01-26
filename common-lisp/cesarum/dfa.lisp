;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dfa.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a DFA, Deterministic Finite Automaton
;;;;    (or Deterministic Finite State Machine).
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DFA"
  (:use "COMMON-LISP")
  (:export
   "DFA" "DFA-STATE" "DEFINE-STATE-MACHINE" "ON-ENTRY" "ON-EXIT"
   "GO-TO-STATE")
  
  (:documentation "

This package implements a DFA, Deterministic Finite Automaton (or
Deterministic Finite State Machine).

A DFA has a state.

Our DFAs also have a set of slots. 

Each DFA is implemented as a class.

Events are represented as generic functions called with the DFA
instance as first argument (and optionnaly other arguments).  They are
specialized on each specific class of DFA they're applicable to.



Example:

    (define-state-machine test-dfa
        :slots ((data :initarg :data :initform nil :accessor data))
        :initial zero
        :states ((zero
                  (:entry () (print `(entering zero)))
                  (:exit  () (print `(exiting zero)))
                  (got-a (sym) (push (cons 'a sym) data))
                  (got-b (sym) (push (cons 'b sym) data) (go-to-state one sym)))
                 (one
                  (:entry (sym) (print `(entering one ,sym)))
                  (:exit  ()    (print `(exiting one)))
                  (got-a (sym) (push (cons 'a sym) data))
                  (got-b (sym) (push (cons 'b sym) data) (go-to-state zero))))
        :documentation \"A test DFA\")

    (defparameter *d* (make-test-dfa '()))
    prints:
    (entering zero)
    --> *d*

    (got-a *d* 'a)
    --> ((a . a))

    (got-b *d* 'b)
    prints:
    (exiting zero) 
    (entering one b)
    --> (entering one b)

    (got-a *d* 'a)
    --> ((a . a) (b . b) (a . a))

    (slot-value *d* 'data)
    --> ((a . a) (b . b) (a . a))


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2012 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>


"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DFA")






(defgeneric dfa-state (dfa)
  (:documentation "The state of the DFA."))

(defclass dfa ()
  ((state
    :initarg  :state
    :accessor dfa-state))
  (:documentation "
A base class for DFAs.

A DFA has a state, and transitions from one state to another can be
performed by event methods.
"))


(defmethod print-object ((self dfa) stream)
  "Prints the DFA as an unreadable object, and showing its state."
  (print-unreadable-object (self stream :identity t :type t)
    (ignore-errors
     (format stream ":STATE ~S" (dfa-state self))))
  self)


(defgeneric on-entry (dfa state &rest rest)
  (:documentation "
This generic function is called on entry into the state,
ie. at the end of the transition, when the state has been updated. (Methods
may further change the state).

The REST arguments are those passed to the event.
")
  (:method (dfa state &rest rest)
    (declare (ignore dfa state rest))
    ;; do nothing by default.
    (values)))


(defgeneric on-exit (dfa state &rest rest)
  (:documentation "
This generic function is called on exit from the state,
ie. at the beginning of the transition, before the state changes.

The REST arguments are those passed to the event.
")
  (:method (dfa state &rest rest)
    (declare (ignore dfa state rest))
    ;; do nothing by default.
    (values)))


;; (make-<name> initial-slot-value...)
;; (event1 <dfa> ...)





(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Note we need these definitions at compilation-time for they're
  ;; used in the DEFINE-STATE-MACHINE macro.

  
  (defstruct dfa-event
    "Events are implemeted as a method on the DFA objects.

Each method dispatches on the state of the DFA."
    name lambda-list state-body-map)
  

  
  (defstruct dfa-entrex
    "
Entrexes are on-Entry and on-Exit actions.

They're implemented as normal functions specific to each DFA state.
"
    name state lambda-list body)


  
  (defun collect-events (states)
    "
STATES:  an A-list of state-name and events.
         Each event is a list containing the name of the event (a symbol),
         a lambda list, followed by the body of the event in that state.

Returns the list of dfa-events and the list of dfa-entrexes collected
from the STATES.

"
    (let ((entrexes '())
            (events   '()))
      (dolist (state states)
        (destructuring-bind (state-name &rest evs) state
          (dolist (ev evs)
            (destructuring-bind (name lambda-list &body body) ev
              (case name
                ((:entry :exit)
                 (when (find-if (lambda (entrex)
                                    (and
                                     (eql name       (dfa-entrex-name entrex))
                                     (eql state-name (dfa-entrex-state entrex))))
                                entrexes)
                   (error "Duplicate ~A for state ~S" name state-name))
                 (push (make-dfa-entrex :name name
                                        :state state-name
                                        :lambda-list lambda-list
                                        :body body)
                       entrexes))
                (otherwise
                 (let ((event (find name events :key (function dfa-event-name))))
                   (if event
                     (progn
                       (assert (equalp lambda-list (dfa-event-lambda-list event)))
                       (push (cons state-name body) (dfa-event-state-body-map event)))
                     (push (make-dfa-event :name name
                                           :lambda-list lambda-list
                                           :state-body-map (list (cons state-name body)))
                           events)))))))))
      (values events entrexes)))

  (defun state-names (states)
    "The list of names of states."
    (mapcar (function first) states))
  
  (defun slot-names (slots)
    "The list of names of slots."
    (mapcar (lambda (slot)
              (if (symbolp slot)
                  slot
                  (first slot)))
            slots))

  
  (defmacro with-go-to-state ((dfa-var state-names) &body body)
    "Execute the BODY in a context where GO-TO-STATE is defined as a
macro to transition to a new state."
    (let ((vgts (gensym "GO-TO-STATE-")))
      `(flet ((,vgts (dfa state &rest arguments)
                (on-exit dfa (dfa-state dfa))
                (setf (dfa-state dfa) state)
                (apply (function on-entry) dfa state arguments)))
         (macrolet ((go-to-state
                     (state &rest arguments)
                     (assert (member state ',state-names))
                     (list* ',vgts ',dfa-var (list 'quote state) arguments)))
             ,@body))))
  
  
  (defun generate-entrex-method (dfa-class slot-names state-names entrex)
    "
Generate the entrex method.

DFA-CLASS: name of the DFA.

The entrex method is named either ON-ENTRY or ON-EXIT
and dispatches on the DFA class and the state name.
"
    (let* ((vdfa   'dfa)
           (vstate (gensym "STATE-"))
           (vrest  (gensym "REST-"))
           (body   `(with-go-to-state (,vdfa ,state-names)
                      ,@(dfa-entrex-body entrex)))
           (body   (if (dfa-entrex-lambda-list entrex)
                     `(destructuring-bind  ,(dfa-entrex-lambda-list entrex) ,vrest
                        ,body)
                     `(progn
                        (assert (null ,vrest))
                        ,body)))
           (body   (if slot-names
                     `(with-slots ,slot-names ,vdfa
                        ,body)
                     body)))
      `(defmethod ,(ecase  (dfa-entrex-name entrex)
                           ((:entry) 'on-entry)
                           ((:exit)  'on-exit))
           ((,vdfa ,dfa-class) (,vstate (eql ',(dfa-entrex-state entrex))) &rest ,vrest)
         "A state entry or exit method."
         (declare (ignorable ,vdfa ,vstate)) ; needed with some implementations.
         ,body)))

  
  (defun generate-event-method (dfa-class slot-names state-names event)
    "
Generate the event method.

DFA-CLASS: name of the DFA.
"
    (let* ((vdfa   'dfa)
           (body   `(with-go-to-state (,vdfa ,state-names)
                      (case (dfa-state ,vdfa)
                        ,@(mapcar (lambda (state-body)
                                      (destructuring-bind (state &body body) state-body
                                        `((,state) ,@body)))
                                  (dfa-event-state-body-map event))
                        (otherwise #|ignore|# (values)))))
           (body   (if slot-names
                     `(with-slots ,slot-names ,vdfa
                        ,body)
                     body)))
      `(defmethod  ,(dfa-event-name event)  ((,vdfa ,dfa-class) ,@(dfa-event-lambda-list event))
         "A state transition event."
         ,body)))

  

  );eval-when



(defmacro define-state-machine (name &key slots initial states documentation)
  "
Defines a DFA class named NAME with the given SLOTS.

The CLAUSES define the states and transitions, that are implemented as
generic functions on the DFA class.
"
  (assert (and (symbolp name) (not (keywordp name))))
  (check-type slots         list)
  (check-type initial       symbol)
  (check-type states        cons)
  (check-type documentation (or null string))

  (unless initial
    (setf initial (first (first states))))

  (assert (every (function consp) states))
  (assert (find initial states :key (function first)))
  (assert (let ((state-names (mapcar (function first) states)))
            (equal state-names (remove-duplicates state-names))))

  (multiple-value-bind (events entrexes) (collect-events states)
    (let ((vdfa        (gensym "DFA-"))
          (initial-ex  (find-if (lambda (entrex)
                                    (and
                                     (eql :entry        (dfa-entrex-name entrex))
                                     (eql initial (dfa-entrex-state entrex))))
                                entrexes))
          (slot-names  (slot-names slots))
          (state-names (state-names states))
          (init-slots  (let ((init-slots '()))
                         (dolist (slot slots init-slots)
                           (when (and (listp slot) (getf (rest slot) :initarg))
                             (push (first slot) init-slots))))))
      `(progn

         (defclass ,name (dfa)
           ,slots
           ,@(when documentation `((:documentation ,documentation))))

         ;; TODO: add a INITIALIZE-INSTANCE method to let the user initialize the DFA with MAKE-INSTANCE.
         
         (defun ,(intern (with-standard-io-syntax (format nil "MAKE-~A" name)) (or (symbol-package name) *package*))
             ,(append
               ;; make-dfa takes as mandatory parameters all the slots that have a :initarg
               init-slots
               (when initial-ex (dfa-entrex-lambda-list initial-ex)))
           ,(format nil "Instanciates a new ~A DFA." name)
           (let ((,vdfa (make-instance ',name :state ',initial)))
             ,@(when slots
                     `((setf ,@(mapcan
                                (lambda (slot)
                                  (let ((sname (if (symbolp slot) slot (first slot))))
                                    (list `(slot-value ,vdfa ',sname) sname)))
                                init-slots))))
             (on-entry ,vdfa ',initial ,@(when initial-ex (dfa-entrex-lambda-list initial-ex)))
             ,vdfa))
         
         ,@(mapcar (lambda (ex) (generate-entrex-method name slot-names state-names ex))
                   entrexes)

         ,@(mapcar (lambda (ev) (generate-event-method  name slot-names state-names ev))
                   events)

         ',name))))



;; Example:
;; 
;; (define-state-machine session-dfa
;;     :slots   ((session :initarg :session)
;;               (timer   :accessor session-dfa-timer))
;;     :initial closed
;;     :states  ((closed
;;                (session-open        () (go-to-state connecting))
;;                (session-disconnect  () (logger :client :warn "SESSION-DFA got a SESSION-DISCONNECT while in CLOSED state.")))
;;               (connecting
;;                (:entry              ()
;;                                     (if (perform-open-session session)
;;                                         (go-to-state connected)
;;                                         (cannot-open-session (session-lo-dfa session))))
;;                (cannot-open-session ()
;;                                     (let ((delay  (random 20)))
;;                                       (logger :client :warn "Cannot connect right now, will try in ~A second~:*~P." delay)
;;                                       (go-to-state waiting delay)))
;;                (session-open        () (logger :client :warn "SESSION-DFA got a SESSION-OPEN while in CONNECTING state."))
;;                (session-close       () (perform-close-session session) (go-to-state closed))
;;                (session-disconnect  () (perform-close-session session) (go-to-state waiting (random 20))))
;;               (waiting
;;                (:entry              (delay)
;;                                     (setf timer (add-timer *event-base* (lambda () (session-timeout dfa)) delay :one-shot t)))
;;                (:exit               () (remove-timer *event-base* (session-dfa-timer dfa)))
;;                (session-timeout     () (go-to-state connecting))
;;                (session-close       () (go-to-state closed))
;;                (session-disconnect  () (logger :client :warn "SESSION-DFA got a SESSION-DISCONNECT while in WAITING state.")))
;;               (connected
;;                (:entry              () ;; TODO: activate the protocol FSM
;;                                     (receive-bytes session))
;;                (session-close       () (perform-close-session session) (go-to-state closed))
;;                (session-disconnect  () (perform-close-session session) (go-to-state connecting)))))

;;;; THE END ;;;;
