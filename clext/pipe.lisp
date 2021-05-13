;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pipe.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements a pipe using Gray streams and bordeaux-threads.
;;;;
;;;;    The data written to the pipe-output-stream is queued (if a maximum
;;;;    queue-size is specified for the stream, then the writing
;;;;    thread may block if the buffer is full).
;;;;
;;;;    The data queued can be read from the pipe-input-stream.  If the
;;;;    queue is empty, then the reading stream may block (unless it
;;;;    used listen, read-char-no-hang, etc).
;;;;
;;;;    When the stream is closed, one can still read from it until
;;;;    the EOF is reached.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-09-12 <PJB> Created.
;;;;BUGS
;;;;TODO
;;;;    - Check LISTEN; add a similar function to check whether writing an element will block
;;;;    - Implement a POLL function.
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(defpackage "COM.INFORMATIMAGO.CLEXT.PIPE"
  (:use "COMMON-LISP"
        ;; "CL-STEPPER"
        "TRIVIAL-GRAY-STREAMS"
        "BORDEAUX-THREADS"
        "COM.INFORMATIMAGO.CLEXT.GATE")
  (:export "MAKE-PIPE" "PIPE" "PIPE-INPUT-STREAM" "PIPE-OUTPUT-STREAM" "PIPE-ELEMENT-TYPE"
           "REOPEN-PIPE"
           "PIPE-CHARACTER-INPUT-STREAM"
           "PIPE-CHARACTER-OUTPUT-STREAM"
           "PIPE-BINARY-INPUT-STREAM"
           "PIPE-BINARY-OUTPUT-STREAM"
           "SIMPLE-STREAM-ERROR")
  (:documentation "

This package exports a pipe abstraction, that is, a pair of streams.

One thread writes to the pipe-output-stream, another thread reads from
the pipe-input-stream.


LEGAL

License AGPL3

Copyright Pascal J. Bourguignon 2015 - 2015

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

"))
(in-package "COM.INFORMATIMAGO.CLEXT.PIPE")

(declaim (declaration stepper))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public interface:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric pipe-element-type (pipe)
  (:documentation "RETURN: the pipe ELEMENT-TYPE."))
(defgeneric pipe-input-stream (pipe)
  (:documentation "RETURN: the pipe input stream."))
(defgeneric pipe-output-stream (pipe)
  (:documentation "RETURN: the pipe output stream."))

(defclass pipe ()
  ((element-type  :reader   pipe-element-type  :initarg :element-type)
   (input-stream  :reader   pipe-input-stream)
   (output-stream :reader   pipe-output-stream))
  ;; Data is written to the pipe-output-stream and then enqueued into the pipe.
  ;; Data is dequeued from the pipe, and then read from the pipe-input-stream.
  (:documentation "A PIPE is a synchronized queue accessed
from an input stream and an output stream."))

(defmethod initialize-instance ((pipe pipe) &key &allow-other-keys)
  (if (eql (class-of pipe) (find-class 'pipe))
      (error "~S is an abstract class, use ~S to create pipes."
             'pipe 'make-pipe)
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Private implementation:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass generic-pipe (pipe)
  ((name          :reader   pipe-name :initform "")
   (lock          :reader   lock)
   (not-empty     :reader   not-empty)
   (head          :accessor head      :initarg :head)
   (tail          :accessor tail      :initarg :tail)))

(defmethod initialize-instance :before ((pipe generic-pipe) &key &allow-other-keys)
  (when (eql (class-of pipe) (find-class 'generic-pipe))
      (error "~S is an abstract class, use ~S to create pipes."
             'generic-pipe 'make-pipe)))

(defmethod initialize-instance :after ((pipe generic-pipe) &key &allow-other-keys)
  (setf (slot-value pipe 'lock)      (bt:make-lock (format nil "~A/pipe/lock" (pipe-name pipe)))
        (slot-value pipe 'not-empty) (make-gate :name (format nil "~A/gate/not-empty" (pipe-name pipe)))))



(defclass buffered-pipe (generic-pipe)
  ((not-full      :reader   not-full)
   (buffer        :reader   buffer    :initarg :buffer))

  ;; Otherwise buffer is a vector of element-type, and head
  ;; and tail are indices in that vector.  In that case, not-full
  ;; is a gate.

  ;; New elements are enqueued on tail; old elements are dequeued
  ;; from head.

  (:documentation "A pipe with a fixed-size circular buffer."))


(defmethod print-object ((pipe buffered-pipe) stream)
  (declare (stepper disable))
  (print-unreadable-object (pipe stream :type t :identity t)
    (let ((bufsize (length (buffer pipe))))
      (format stream ":element-type ~S :input ~:[:closed~;:open~] :output ~:[:closed~;:open~] ~:[~;:full ~]~:[~;:empty ~]:buffer ([~A ~A] |~A|/~A) :name ~S"
              (pipe-element-type pipe)
              (open-stream-p (pipe-input-stream  pipe))
              (open-stream-p (pipe-output-stream pipe))
              (%pipe-fullp pipe)
              (%pipe-emptyp pipe)
              (head pipe) (tail pipe)
              (mod (- (tail pipe) (head pipe)) bufsize)
              bufsize
              (pipe-name pipe))))
  pipe)

(defmethod initialize-instance :after ((pipe buffered-pipe) &key &allow-other-keys)
  (setf (slot-value pipe 'not-full) (make-gate :name (format nil "~A/pipe/not-full" (pipe-name pipe)))))


(defclass queued-pipe (generic-pipe)
  ()

  ;; When buffer is NIL then head is a list of cells, with
  ;; tail refering the last cons in that list.  In this case,
  ;; not-full is NIL.

  ;; New elements are enqueued on tail; old elements are dequeued
  ;; from head.

  (:documentation "A pipe with a variable length queue of blocks."))

(defmethod print-object ((pipe queued-pipe) stream)
  (declare (stepper disable))
  (print-unreadable-object (pipe stream :type t :identity t)
    (let ((queue-size (length (head pipe))))
      (format stream ":element-type ~S :input ~:[:closed~;:open~] :output ~:[:closed~;:open~] ~:[~;:full ~]~:[~;:empty ~]:queued ~A :name ~S"
              (pipe-element-type pipe)
              (open-stream-p (pipe-input-stream  pipe))
              (open-stream-p (pipe-output-stream pipe))
              (%pipe-fullp pipe)
              (%pipe-emptyp pipe)
              queue-size
              (pipe-name pipe))))
  pipe)




(defclass pipe-stream ()
  ((pipe         :reader pipe-stream-pipe         :initarg :pipe )
   (element-type :reader pipe-stream-element-type :initarg :element-type)
   (open         :reader open-stream-p            :initform t)))

(defmethod print-object ((stream pipe-stream) output)
  (declare (stepper disable))
  (print-unreadable-object (stream output :type t :identity t)
    (format output "~:[:closed~;:open~] :element-type ~S"
            (open-stream-p stream)
            (pipe-stream-element-type stream)))
  stream)

(defclass pipe-character-input-stream (pipe-stream fundamental-character-input-stream)
  ((column :initform 0 :accessor column)))


(defclass pipe-character-output-stream (pipe-stream fundamental-character-output-stream)
  ((column :initform 0 :accessor column)))


(defclass pipe-binary-input-stream (pipe-stream fundamental-binary-input-stream)
  ())


(defclass pipe-binary-output-stream (pipe-stream fundamental-binary-output-stream)
  ())


(defun make-pipe (&key (element-type 'character) buffer-size (name ""))
  "

ELEMENT-TYPE:   The element-type of the pipe and the pipe streams.
                Since the pipe streams are streams, this should be a
                valid stream element type.

BUFFER-SIZE:    NIL or a FIXNUM; If NIL then an unbound queue is used,
                therefore the writer thread will never block.  If a
                fixnum, then it's the size of the allocated circular
                buffer for the pipe, and when this buffer is full, the
                writer thread will block until the reader thread
                empties some.

NAME:           A string, the name of the pipe.  It's used to build
                the name of the various internal structures such as
                lock and condition-variables, which is useful for
                debugging.

RETURN:         The new PIPE.

"
  (let ((pipe (if buffer-size
                  (make-instance 'buffered-pipe :name name
                                                :element-type element-type
                                                :buffer (make-array (1+ buffer-size) :element-type element-type)
                                                :head 0 :tail 0)
                  (make-instance 'queued-pipe   :name name
                                                :element-type element-type
                                                :head nil :tail nil))))
    (multiple-value-bind (input-class output-class)
        (if (subtypep element-type 'character)
            (values 'pipe-character-input-stream 'pipe-character-output-stream)
            (values 'pipe-binary-input-stream    'pipe-binary-output-stream))
      (setf (slot-value pipe 'input-stream)  (make-instance input-class  :pipe pipe :element-type element-type)
            (slot-value pipe 'output-stream) (make-instance output-class :pipe pipe :element-type element-type)))
    pipe))



(defgeneric %pipe-emptyp (pipe)
  (:documentation "Whether the PIPE is empty."))

(defgeneric %pipe-fullp  (pipe)
  (:documentation "Whether the PIPE is full."))


(defgeneric %pipe-peek-or-dequeue (pipe peek)
  (:documentation "
PRE:    (not (%pipe-emptyp pipe))
PEEK:   When true, then peek, else dequeue.
RETURN: RETURNP: whether we have a result;
        RESULT:  the result of peeking or dequeing;
        SIGNAL:  whether not-full condition should be signaled.
"))


(defgeneric pipe-enqueue-element (pipe element)
  (:documentation "
Enqueues the ELEMENT into the PIPE.
Blocks if the pipe is full.
"))

(defgeneric pipe-dequeue-element (pipe)
  (:documentation "
Dequeues and returns the ELEMENT from the PIPE.
If the pipe is empty,
then returns :EOF if the output stream is closed
or else blocks.
"))

(defgeneric pipe-peek-element (pipe)
  (:documentation "
Returns the first ELEMENT from the PIPE,
or NIL if the queue is empty,
or :EOF if the output-stream is closed.
"))

(defgeneric pipe-enqueue-sequence (pipe sequence start end)
  (:documentation "
If (- end start) is more than the buffer-size then signal an error
else if there's enough space in the buffer, for (- end start) elements,
then enqueues the subseq of SEQUENCE between START and END,
else blocks until there's enough space.
"))

(defgeneric pipe-dequeue-sequence (pipe sequence start end)
  (:documentation "
If (- end start) is more than the buffer-size then signal an error
else if there's more than (- end start) elements  in the buffer,
then dequeues them into the subseq of SEQUENCE between START and END,
else blocks until there's enough data available.

RETURN: The index of the first slot in SEQUENCE not modified.
        May be less than END when EOF is reached.
"))

(defgeneric pipe-dequeue-until-element (pipe element)
  (:documentation "
Dequeues elements from the buffer until an element equal to element is found.
Returns a sequence containing all the dequeued elements.
"))


(defgeneric sunk-pipe-p (pipe)
  (:documentation "Whether the reading stream has been closed;
when it's the case, no more data is written to the pipe.")
  (:method ((pipe pipe))
    (not (open-stream-p (pipe-input-stream pipe)))))

(defgeneric closed-pipe-p (pipe)
  (:documentation "Whether the writing stream has been closed;
when it's the case, end-of-file is detected upon reading on an empty pipe.")
  (:method ((pipe pipe))
    (not (open-stream-p (pipe-output-stream pipe)))))


;;; for circular buffers:

(defun mod-plus (value modulo &rest arguments)
  (mod (apply (function +) value arguments) modulo))

(defun mod-minus (value modulo &rest arguments)
  (mod (apply (function -) value arguments) modulo))

(define-modify-macro mod-incf (modulo &optional (increment 1)) mod-plus)
(define-modify-macro mod-decf (modulo &optional (decrement 1)) mod-minus)




(defmethod %pipe-emptyp ((pipe buffered-pipe))
  (= (head pipe) (tail pipe)))

(defmethod %pipe-fullp  ((pipe buffered-pipe))
  (= (head pipe) (mod-plus (tail pipe) (length (buffer pipe)) 1)))


;;; for queue lists, we enqueue blocks made of a start index and a sequence.

(declaim (inline make-block block-start block-sequence block-empty-p block-full-p
                 block-pop-char block-peek-char block-push-char))
(defun make-block         (buffer)        (cons 0 (copy-seq buffer)))
(defun block-start        (blk)           (car blk))
(defun (setf block-start) (new-value blk) (setf (car blk) new-value))
(defun block-sequence       (blk)           (cdr blk))
(defun block-empty-p      (blk)           (>= (block-start blk) (length (block-sequence blk))))
(defun block-full-p       (blk)           (zerop (block-start blk)))
(defun block-pop-char (blk)
  (assert (not (block-empty-p blk)))
  (prog1 (aref (block-sequence blk) (block-start blk))
    (incf (block-start blk))))
(defun block-peek-char (blk)
  (assert (not (block-empty-p blk)))
  (aref (block-sequence blk) (block-start blk)))
(defun block-push-char (blk ch)
  (assert (not (block-full-p blk)))
  (decf (block-start blk))
  (setf (aref (block-sequence blk) (block-start blk)) ch))


(defmethod %pipe-emptyp ((pipe queued-pipe))
  (null (head pipe)))

(defmethod %pipe-fullp  ((pipe queued-pipe))
  nil)

(defun %wait-not-empty-or-closed (pipe)
  (loop :while (%pipe-emptyp pipe)
        :do (if (closed-pipe-p pipe)
                (return :eof)
                (gate-wait (not-empty pipe) (lock pipe)))
        :finally (return nil)))


(defmethod pipe-enqueue-element ((pipe buffered-pipe) element)
  (with-lock-held ((lock pipe))
    (loop :while (%pipe-fullp pipe)
          :do (gate-wait (not-full pipe) (lock pipe)))
    (setf (aref (buffer pipe) (tail pipe)) element)
    (mod-incf (tail pipe) (length (buffer pipe))))
  (gate-signal (not-empty pipe)))

(defmethod pipe-enqueue-sequence ((pipe buffered-pipe) sequence start end)
  (assert (<= 0 start end (length sequence)))
  (let ((buflen (length (buffer pipe))))
    (loop
      :while (< start end)
      :do (with-lock-held ((lock pipe))
            (loop :while (%pipe-fullp pipe)
                  :do (gate-wait (not-full pipe) (lock pipe)))
            (when (%pipe-emptyp pipe)
              (setf (head pipe) 0
                    (tail pipe) 0))
            ;; [_____head-------tail__________]
            ;; [-------tail_______head--------]
            ;; write what we can:
            (let* ((seqlen (- end start))
                   (dsts  (tail pipe))
                   (dste  (min (cond ((zerop (head pipe))            (1- buflen))
                                     ((<= (head pipe) (tail pipe))   buflen)
                                     (t                              (1- (head pipe))))
                               (+ dsts seqlen)))
                   (len   (- dste dsts)))
              (replace (buffer pipe) sequence :start1 dsts :end1 dste :start2 start)
              (incf start len)
              (mod-incf (tail pipe) buflen len)))
          (gate-signal (not-empty pipe))))
  sequence)

(defmethod pipe-dequeue-sequence ((pipe buffered-pipe) sequence start end)
  (assert (<= 0 start end (length sequence)))
  (let ((buflen (length (buffer pipe))))
    (loop
      :with current := start
      :while (< current end)
      :do (with-lock-held ((lock pipe))
            (when (%wait-not-empty-or-closed pipe)
              (return current))
            ;; [_____head-------tail__________]
            ;; [-------tail_______head--------]
            ;; read what we can:
            (let* ((seqlen (- end current))
                   (srcs   (head pipe))
                   (srce   (min (if (<= (head pipe) (tail pipe))
                                    (tail pipe)
                                    buflen)
                                (+ srcs seqlen)))
                   (len    (- srce srcs)))
              (replace sequence (buffer pipe) :start2 srcs :end2 srce :start1 current)
              (incf current len)
              (mod-incf (head pipe) buflen len)))
          (gate-signal (not-full pipe))
      :finally (return current))))

(defmethod pipe-dequeue-until-element ((pipe buffered-pipe) element)
  (let ((buflen (length (buffer pipe)))
        (chunks '()))
    (flet ((concatenate-chunks ()
             (let ((result
                     (with-output-to-string (out)
                       (dolist (chunk (nreverse chunks))
                         (write-string chunk out)))))
               result)))
      (loop :named collect
            :do (with-lock-held ((lock pipe))
                  (when (%wait-not-empty-or-closed pipe)
                    (return-from collect (values (concatenate-chunks)
                                                 t #|=eof|#)))
                  ;; [_____head-------tail__________]
                  ;; [-------tail_______head--------]
                  ;; read what we can:
                  (let* ((srcs   (head pipe))
                         (srce   (if (<= (head pipe) (tail pipe))
                                     (tail pipe)
                                     buflen))
                         (pos    (position element (buffer pipe) :start srcs :end srce)))
                    (if pos
                        (progn
                          (push (subseq (buffer pipe) srcs pos) chunks)
                          (mod-incf (head pipe) buflen (- (1+ pos) srcs))
                          (return-from collect (values (concatenate-chunks)
                                                       nil #|=newline|#)))
                        (progn
                          (push (subseq (buffer pipe) srcs srce) chunks)
                          (mod-incf (head pipe) buflen (- srce srcs))))))
                (gate-signal (not-full pipe))
            :finally (gate-signal (not-full pipe))))))



(defmethod pipe-enqueue-element ((pipe queued-pipe) element)
  (pipe-enqueue-sequence pipe (vector element) 0 1))

(declaim (inline subsequence))
(defun subsequence (element-type sequence start end)
  (let ((result (make-array (- end start) :element-type element-type)))
    (replace result sequence :start2 start :end2 end)))

(defmethod pipe-enqueue-sequence ((pipe queued-pipe) sequence start end)
  (let ((blkl (list (make-block (subsequence (pipe-element-type pipe) sequence start end)))))
    (with-lock-held ((lock pipe))
      (if (tail pipe)
          (setf (cdr (tail pipe)) blkl
                (tail pipe) (cdr (tail pipe)))
          (setf (head pipe)
                (setf (tail pipe) blkl))))
    (gate-signal (not-empty pipe))))

(defmethod pipe-dequeue-sequence ((pipe queued-pipe) sequence start end)
  (assert (<= 0 start end (length sequence)))
  (with-lock-held ((lock pipe))
    (loop
      :with current := start
      :while (< current end)
      :do (when (%wait-not-empty-or-closed pipe)
            (return current))
          ;; read what we can:
          (let ((blk (car (head pipe))))
            (if (block-empty-p blk)
                (if (eql (head pipe) (tail pipe))
                    (setf (head pipe) nil
                          (tail pipe) nil)
                    (pop (head pipe)))
                (let* ((seqlen (- end current))
                       (srcs   (block-start blk))
                       (srce   (min (+ seqlen srcs)
                                    (length (block-sequence blk))))
                       (len    (- srce srcs)))
                  (replace sequence (block-sequence blk)
                           :start1 current :start2 (block-start blk) :end2 srce)
                  (incf current len)
                  (incf (block-start blk) len))))
      :finally (return current))))

(defmethod pipe-dequeue-until-element ((pipe queued-pipe) element)
  (let ((chunks '()))
    (flet ((concatenate-chunks ()
             (let ((result
                     (with-output-to-string (out)
                       (dolist (chunk (nreverse chunks))
                         (write-string chunk out)))))
               result)))
      (with-lock-held ((lock pipe))
        (loop
          (when (%wait-not-empty-or-closed pipe)
            (return (values (concatenate-chunks) t #|=eof|#)))
          ;; read what we can:
          (let ((blk (car (head pipe))))
            (if (block-empty-p blk)
                (if (eql (head pipe) (tail pipe))
                    (setf (head pipe) nil
                          (tail pipe) nil)
                    (pop (head pipe)))
                (let* ((srcs   (block-start blk))
                       (srce   (length (block-sequence blk)))
                       (pos    (position element (block-sequence blk) :start srcs :end srce)))
                  (if pos
                      (progn (push (subseq (block-sequence blk) (block-start blk) pos) chunks)
                             (setf (block-start blk) (1+ pos))
                             (return (values (concatenate-chunks) nil #|=newline|#)))
                      (progn (push (subseq (block-sequence blk) (block-start blk)) chunks)
                             (setf (block-start blk) srce)))))))))))


;;; Peek or dequeue element.

(defmethod %pipe-peek-or-dequeue ((pipe buffered-pipe) peek)
  (values t (prog1 (aref (buffer pipe) (head pipe))
              (unless peek
                (mod-incf (head pipe) (length (buffer pipe)))))
          (not peek)))

(defmethod %pipe-peek-or-dequeue ((pipe queued-pipe) peek)
  (let ((blk (car (head pipe))))
    (if (block-empty-p blk)
        (progn
          (if (eql (head pipe) (tail pipe))
              (setf (head pipe) nil
                    (tail pipe) nil)
              (pop (head pipe)))
          (values nil))
        (values t (if peek
                      (block-peek-char blk)
                      (block-pop-char  blk))))))

(defun %peek-or-dequeue (pipe peek no-hang)
  (let ((signal-not-full nil))
    (prog1 (loop
             (with-lock-held ((lock pipe))
               (if (%pipe-emptyp pipe)
                   (if (closed-pipe-p pipe)
                       (return :eof)
                       (if no-hang
                           (return nil)
                           (gate-wait (not-empty pipe) (lock pipe))))
                   (multiple-value-bind (returnp result signal) (%pipe-peek-or-dequeue pipe peek)
                     (setf signal-not-full signal)
                     (when returnp
                       (return result))))))
      (when signal-not-full (gate-signal (not-full pipe))))))


(defmethod pipe-dequeue-element ((pipe generic-pipe))
  (%peek-or-dequeue pipe nil nil))

(defmethod pipe-peek-element ((pipe generic-pipe))
  (%peek-or-dequeue pipe :peek nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; stream methods
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition simple-stream-error (stream-error simple-error)
  ())

(defun check-stream-open (stream where)
  (unless (open-stream-p stream)
    (error 'simple-stream-error
           :stream stream
           :format-control "~S cannot deal with closed stream ~S"
           :format-arguments (list where stream))))

;;; character input

(declaim (inline update-column))
(defun update-column (stream ch)
  (when (characterp ch)
    (if (char= ch #\newline)
        (setf (column stream) 0)
        (incf (column stream))))
  ch)

(defmethod stream-read-char ((stream pipe-character-input-stream))
  (check-stream-open stream 'stream-read-char)
  (update-column stream (pipe-dequeue-element (pipe-stream-pipe stream))))

(defmethod stream-read-char-no-hang ((stream pipe-character-input-stream))
  (check-stream-open stream 'stream-read-char-no-hang)
  (update-column stream (%peek-or-dequeue (pipe-stream-pipe stream) nil :no-hang)))

 (defmethod stream-peek-char ((stream pipe-character-input-stream))
  (check-stream-open stream 'stream-peek-char)
  (pipe-peek-element (pipe-stream-pipe stream)))

(defmethod stream-read-line ((stream pipe-character-input-stream))
  (check-stream-open stream 'stream-read-line)
  (multiple-value-bind (line eof) (pipe-dequeue-until-element (pipe-stream-pipe stream) #\newline)
    (setf (column stream) 0)
    (values line eof)))

(defmethod stream-listen ((stream pipe-character-input-stream))
  (check-stream-open stream 'stream-listen)
  (%peek-or-dequeue (pipe-stream-pipe stream) :peek :no-hang))

(defmethod stream-unread-char ((stream pipe-character-input-stream) ch)
  (check-stream-open stream 'stream-unread-char)
  (with-lock-held ((lock stream))
    (let ((head (head stream)))
      (if head
          (let ((blk (car head)))
            (if (block-full-p blk)
                (push (make-block (string ch)) (head stream))
                (block-push-char blk ch)))
          (setf (tail stream)
                (setf (head stream) (list (make-block (string ch))))))))
  (gate-signal (not-empty stream))
  (setf (column stream) (max 0 (1- (column stream))))
  ch)



;;; character output

(defmethod stream-write-char ((stream pipe-character-output-stream) ch)
  (check-stream-open stream 'stream-write-char)
  (let ((pipe (pipe-stream-pipe stream)))
    (unless (sunk-pipe-p pipe)
      (pipe-enqueue-element pipe ch)))
  (if (char= #\newline ch)
      (setf (column stream) 0)
      (incf (column stream)))
  ch)

(defmethod stream-terpri ((stream pipe-character-output-stream))
  (check-stream-open stream 'stream-terpri)
  (stream-write-char stream #\newline)
  nil)

(defmethod stream-write-string ((stream pipe-character-output-stream) string &optional (start 0) end)
  (check-stream-open stream 'stream-write-string)
  (let* ((pipe (pipe-stream-pipe stream))
         (end  (or end (length string)))
         (nlp  (position #\newline string :start start :end end :from-end t)))
    (unless (sunk-pipe-p pipe)
      (pipe-enqueue-sequence pipe string start end))
    (if nlp
        (setf (column stream) (- end nlp))
        (incf (column stream) (- end start))))
  string)

(defmethod stream-line-column ((stream pipe-character-output-stream))
  (column stream))

(defmethod stream-start-line-p ((stream pipe-character-output-stream))
  (zerop (column stream)))

(defmethod stream-advance-to-column ((stream pipe-character-output-stream) column)
  (check-stream-open stream 'stream-advance-to-column)
  (let ((delta (- column (column stream))))
    (when (plusp delta)
      (stream-write-string stream (make-string delta :initial-element #\space))
      delta)))

(defgeneric reopen-pipe (pipe)
  (:documentation "Reopens the streams of the PIPE."))
(defmethod reopen-pipe ((pipe generic-pipe))
  (with-lock-held ((lock pipe))
    (setf (slot-value (pipe-input-stream pipe)  'open) t
          (slot-value (pipe-output-stream pipe) 'open) t))
  (gate-signal (not-empty pipe)))

(defgeneric close-pipe (pipe))
(defmethod close-pipe ((pipe generic-pipe))
  (with-lock-held ((lock pipe))
    (setf (slot-value (pipe-output-stream pipe) 'open) nil))
  (gate-signal (not-empty pipe)))

(defun sink-pipe (pipe)
  (with-lock-held ((lock pipe))
    (setf (slot-value (pipe-input-stream pipe) 'open) nil)))

(defmethod close ((stream pipe-character-output-stream) &key abort)
  (declare (ignore abort))
  (close-pipe (pipe-stream-pipe stream)))

(defmethod close ((stream pipe-binary-output-stream) &key abort)
  (declare (ignore abort))
  (close-pipe (pipe-stream-pipe stream)))

(defmethod close ((stream pipe-character-input-stream) &key abort)
  (declare (ignore abort))
  (sink-pipe (pipe-stream-pipe stream)))

(defmethod close ((stream pipe-binary-input-stream) &key abort)
  (declare (ignore abort))
  (sink-pipe (pipe-stream-pipe stream)))



;; binary input

(defmethod stream-read-byte ((stream pipe-binary-input-stream))
  (check-stream-open stream 'stream-read-byte)
  (%peek-or-dequeue (pipe-stream-pipe stream) nil nil))

;; binary output

(defmethod stream-write-byte ((stream pipe-binary-output-stream) byte)
  (check-stream-open stream 'stream-write-byte)
  (let ((pipe (pipe-stream-pipe stream)))
    (unless (sunk-pipe-p pipe)
      (pipe-enqueue-element pipe byte)))
  byte)



;;; sequence I/O

(defun check-sequence-arguments (direction pipe sequence start end)
  (assert (<= 0 start end (length sequence))
          (sequence start end)
          "START = ~D or END = ~D are not sequence bounding indexes for a ~S of length ~D"
          start end (type-of sequence) (length sequence))
  (ecase direction
    (:read
     (assert (or (listp sequence)
                 (and (vectorp sequence)
                      (subtypep (pipe-element-type pipe) (array-element-type sequence))))
             (sequence)
             "For reading, the sequence element type ~S should be a supertype of the pipe element type ~S"
             (array-element-type sequence) (pipe-element-type pipe)))
    (:write
     (assert (or (listp sequence)
                 (and (vectorp sequence)
                      (subtypep (array-element-type sequence) (pipe-element-type pipe))))
             (sequence)
             "For writing, the sequence element type ~S should be a subtype of the pipe element type ~S"
             (array-element-type sequence) (pipe-element-type pipe)))))

(defun %stream-read-sequence (stream sequence start end)
  (let ((pipe (pipe-stream-pipe stream)))
    (check-sequence-arguments :read pipe sequence start end)
    (pipe-dequeue-sequence pipe sequence start end)))

(defun %stream-write-sequence (stream sequence start end)
  (let ((pipe (pipe-stream-pipe stream)))
    (check-sequence-arguments :write pipe sequence start end)
    (unless (sunk-pipe-p pipe)
      (pipe-enqueue-sequence pipe sequence start end))
    sequence))

(defmethod stream-read-sequence ((stream pipe-character-input-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-read-sequence)
  (%stream-read-sequence stream sequence start end))

(defmethod stream-read-sequence ((stream pipe-binary-input-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-read-sequence)
  (%stream-read-sequence stream sequence start end))

(defmethod stream-write-sequence ((stream pipe-character-output-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-write-sequence)
  (%stream-write-sequence stream sequence start end))

(defmethod stream-write-sequence ((stream pipe-binary-output-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-write-sequence)
  (%stream-write-sequence stream sequence start end))



;; For:
;; (defmethod stream-finish-output ((stream pipe-character-output-stream)))
;; (defmethod stream-finish-output ((stream pipe-binary-output-stream)))
;; Attempts to ensure that all output sent to the stream has reached its
;; destination, and only then returns false. Implements
;; cl:finish-output. The default method does nothing.
;;
;; We assign the semantics of waiting for the reader process to
;; have read all the data written so far.
;;
;; NO: This could be done with a empty condition, and the writer waiting
;;     on the empty condition.  The reader would notify it when reaching
;;     an empty pipe.
;; This assumes a single writer stream.  While waiting for the
;; condition another writer stream could further write data.
;; Therefore we need instead to be able to enqueue tokens into the pipe,
;; that could be used to message back to the exact writing thread.
;;
;; (defmethod pipe-finish-output ((pipe generic-pipe))
;;   (with-lock-held ((lock pipe))
;;     (loop :until (%pipe-emptyp pipe)
;;           :do (condition-wait (empty pipe) (lock pipe)))))
;;
;; (defmethod stream-finish-output ((stream pipe-character-output-stream))
;;   (pipe-finish-output (pipe-stream-pipe stream)))
;; (defmethod stream-finish-output ((stream pipe-binary-output-stream))
;;   (pipe-finish-output (pipe-stream-pipe stream)))

#-(and) (progn
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (pushnew :debug *features*)
            (setf *features*  (delete :debug *features*)))
          #+debug (:shadowing-import-from  "COM.INFORMATIMAGO.CLEXT.DEBUG" "WITH-LOCK-HELD")
          #+debug (:use "COM.INFORMATIMAGO.CLEXT.DEBUG")
          #-debug (declaim (inline tr))
          #-debug (defun tr (&rest args) (declare (ignore args)))

          (defun trace-pipe (where pipe)
            (flet ((sub (s e)
                     (substitute #\␤ #\newline (subseq (buffer pipe) s e))))
              (let ((head (head pipe))
                    (tail (tail pipe))
                    (e    (if (%pipe-emptyp pipe) "empty " ""))
                    (f    (if (%pipe-fullp  pipe) "full  " "")))
                (tr "~A: h:~S t:~S ~A~A" where head tail e f)
                (if (<= head tail)
                    (tr "~V,,,'_<~>~A~V,,,'_<~>"
                      head
                      (sub head tail)
                      (- (length (buffer pipe)) tail))
                    (tr "~A~V,,,'_<~>~A"
                      (sub 0 tail)
                      (- head tail)
                      (sub head (length (buffer pipe))))))))

          )

;;;; THE END ;;;;
