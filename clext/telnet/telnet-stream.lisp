;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               telnet-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Wraps a socket stream running the telnet protocol in a pair of
;;;;    bivalent gray-streams.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-14 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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

(in-package "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM")
(declaim (declaration stepper))

#|

# -*- mode:org -*-

** Telnet Controls

These controls may be received from the telnet layer into the
up:GSTREAM. The telnet-stream must process them.


| Controls          | I/O        | Description                                      |
|-------------------+------------+--------------------------------------------------|
| are-you-there     | output     | should send an answer                            |
|                   |            | at the REPL we could send the prompt             |
|                   |            | when reading, could issue a message or a prompt? |
|                   |            | when processing, could issue a message about the |
|                   |            | current function (short backtrace?)              |
|                   |            |                                                  |
| abort-output      | output     | clear output buffer (send-buffer, output buffer) |
|                   |            |                                                  |
| interrupt-process | interrupt  | enter the debugger, or resume REPL               |
|                   |            |                                                  |
| go-ahead          | output     | for half-duplex, resume sending.                 |
|                   |            |                                                  |
| erase-line        | input      | erase the input buffer                           |
|                   |            |                                                  |
| erase-character   | input      | erase the previous character in the input buffer |
|                   |            |                                                  |
| break             | interrupt? | enter the debugger, or resume REPL               |
|                   |            | at the REPL prompt, break could reissue          |
|                   |            | the prompt without entering the debugger.        |
|                   |            |                                                  |
| end-of-record     | input      | EOF?                                             |

Note: the input buffer may contain multiple lines. ERASE-LINE erase
back up to the previous newline, but cannot erase the previous line.
ERASE-CHARACTER cannot erase a newline in the buffer either.


** Telnet Options

We may set those options:

| transmit-binary   | output | depending on the encoding US-ASCII or non-US-ASCII. |
| echo              | input  | echo the character received.                        |
| suppress-go-ahead | I/O    | half-duplex ; enable this option to go full-duplex. |
| end-of-record     | output | we may ask this option to send eor on flush.        |
| end-of-record     | input  | ignore it?                                          |


| echo     | suppress-go-ahead | meaning          |
|----------+-------------------+------------------|
| enabled  | enabled           | character mode   |
| enabled  | disabled          | kludge line mode |
| disabled | enabled           | kludge line mode |
| disabled | disabled          |                  |

Note: The line mode option (RFC 1116, RFC 1184) is not implemented yet.

(defparameter *default-classes* '((:transmit-binary   . option)
                                  (:echo              . option)
                                  (:suppress-go-ahead . option)
                                  (:end-of-record     . option)
                                  ;; (:timing-mark       . option)
                                  (:status            . status))

  ;; NOTE: when the class is OPTION, it means the option has no
  ;;       specific behavior besides being enabled or disabled (but
  ;;       the NVT may alter its behavior according to the setting of
  ;;       the option).

  "An a-list of (option-name . class-name).")

** Threads

The client needs two threads:

- a client input loop thread that waits and read the bytes from the
  socket.  as soon as some are received, they're transmitted to the
  NVT, and queued into the Input Buffer of the gray stream.

- a client repl thread reads the data from the Input Buffer, and
  evaluates it.  Eventually, it may write some data to the thread.
  Some buffering (if asked) may occur until it's flushed
  (finish-output) or (terpri).  The data is sent down to the NVT.  The
  NVT sends the data to the down-sender, which calls write-sequence on
  the socket.  Since this may block, it will set send-wait-p to false
  first, so that if send down needs to be called from the other
  thread, data will be queued instead in the send-buffer.

SEND-WAIT-P is never set (yet).  It may be used for half-duplex and/or
flow control options (RFC 1372).

The DISPATCH-MESSAGE method calls (SEND down bytes) to send responses
and other control messages, in addition to data messages.

Therefore the serialization must be implemented in the NVT-DOWN-SENDER.


*** Gray Streams

Gray Streams don't specify anything about threading and mutex on the
stream operations.

For example, ccl has a :sharing option on streams, but only for
BASIC-STREAMS, not for Gray Streams.  I/O function fork to Gray Stream
messages without doing any locking.

So Gray Streams must implement their own mutexing.


** Encodings

When using US-ASCII, we can use SEND-TEXT, but when using any other
encoding, we need to negociate binary and an encoding, and use
SEND-BINARY.  The user will select the stream-external-format encoding
himself, so we don't need to negociate it (but we could once RFC 2066
is implemented, at least for the initial external-format).

** Buffering

Note: we may ask for the EOR option, and send an EOR when flushing
(automatic or explicit).

We may offer buffering options on the telnet-stream (up:GSTREAM):

| Setting                                                | Description                                                     |
|--------------------------------------------------------+-----------------------------------------------------------------|
| (stream-output-buffering stream)                       | the current output-buffering: :CHARACTER :LINE or buffer-size   |
|                                                        |                                                                 |
| (setf (stream-output-buffering stream) nil)            | no Output Buffer we send directly to the NVT                    |
| (setf (stream-output-buffering stream) :character)     | and (send down bytes);                                          |
|                                                        | we still may have to buffer when (send-wait-p nvt)              |
|                                                        |                                                                 |
| (setf (stream-output-buffering stream) :line)          | We write into the Output Buffer until a newline.                |
|                                                        | Then we automatically flush when a newline is written           |
|                                                        |                                                                 |
| (setf (stream-output-buffering stream) buffer-size)    | We write into the Output Buffer up to a buffer-size full        |
|                                                        | and then flush automatically when we the buffer is full.        |
|                                                        | We need to call flush-output explicitely.                       |
|--------------------------------------------------------+-----------------------------------------------------------------|
| (stream-input-buffering stream)                        | the current input-buffering: :CHARACTER or :LINE                |
|                                                        |                                                                 |
| (setf (stream-input-buffering stream) nil)             | Data is still stored in the Input Buffer, but is available      |
| (setf (stream-input-buffering stream) :character)      | (LISTEN stream) and can be READ/READ-CHAR/READ-SEQUENCE         |
|                                                        | as soon as received.                                            |
|                                                        |                                                                 |
| (setf (stream-input-buffering stream) :line)           | Data is stored in the Input Buffer, but is available            |
|                                                        | only when a newline (or EOR) is received.                       |
|                                                        | -> the only difference really is for LISTEN and READ-SEQUENCE   |
|--------------------------------------------------------+-----------------------------------------------------------------|
| (stream-echo stream)                                   | he current echo mode: T or NIL                                  |
| (setf (stream-echo stream) t)                          | Sets the telnet option. Default behavior.                       |
| (setf (stream-echo stream) nil)                        | Sets the telnet option. We could set this to read passwords.    |
|--------------------------------------------------------+-----------------------------------------------------------------|
| (stream-external-format stream)                        | the current external-format.  :default is us-ascii              |
|                                                        | Note: we use the native external-format values.                 |
|                                                        |                                                                 |
| (setf (stream-external-format stream) encoding)        | sets the external-format.                                       |
|                                                        | This also set the binary mode <=> the encoding is not us-ascii. |
|--------------------------------------------------------+-----------------------------------------------------------------|
| (stream-element-type stream)                           | the current element-type of the stream                          |
|                                                        |                                                                 |
| (setf (stream-element-type stream) 'character)         | set the text mode stream-external-format is used.               |
|                                                        |                                                                 |
| (setf (stream-element-type stream) '(unsigned-byte n)) | set the binary mode, and decode unsigned-byte to octets.        |
|                                                        | This could be useful to transfer and process binary data.       |
|--------------------------------------------------------+-----------------------------------------------------------------|

** TODOs
*** TODO We need to check thread safety of the NVT and the TELNET-STREAM.

#+BEGIN_CODE

-*- mode:text;mode:picture -*-

                                       +------------+
                                       |    REPL    |
                                       +------------+
                                          ^        |
                                          |       (write stream)
                              (listen stream)      |
                                          |        |
                                (read stream)      |
client repl thread                        |        |
..........................................|....    |
client input loop thread                  |   .    |
                                          |   .    v
         +------------------+          +------------+     +-------------------+
         |   Input Buffer   |<---------| up:GSTREAM |---->|   Output Buffer   |
         +------------------+          +------------+     +-------------------+
                                          ^   .    |
                                          |   .    |
                                          |   .    |
                                          |   .    |
                                          |   .    |
                           +---------------+  .  (send-binary nvt bytes)
                           | NVT-UP-SENDER |  .    |
                           +---------------+  .  (send-text nvt text)
                                          ^   .    |
                                          |   .  (send-control nvt control)
                                          |   .    |
                    (receive-binary up bytes) .    |
                                          |   .    |
                       (reveive-text up text) .    |
                                          |   .    |
                 (reveive-control up control) .    |
                                          |   .    |
                (want-option-p up option-name).    |
              (receive-option up option value).    |
                                          |   .    |
                                          |   .    |
                                          |   .    v
                               +--------------------------+
       options --------------->| NETWORK-VIRTUAL-TERMINAL |
                               +--------------------------+          (send-wait-p nvt)/
                                  |       ^   .    |---------------> (send-buffer nvt)
                                  |       |   . (send down bytes)
                                  |       |   .    |
                                  |       |   .    |
                                  v       |   .    v
                       +------------+     |   .  +-----------------+
                       | OPTION-MGR |*    |   .  | NVT-DOWN-SENDER |
                       +------------+     |   .  +-----------------+
                                          |   .    |       |        client repl thread
                          (receive nvt bytes) .    |mutex->|        until buffer is empty.
                                          |   .    |       |       +----------------------+
                                          |   .    v       |       |                      |
                                      +--------------+     +---->buffer                   |
                                      | down:GSTREAM |             |                      |
 client input loop thread             +--------------+             |                      |
       +-----------+                      ^   .    |               v                      |
       |           |                      |   . (write-sequence buffer socket)            |
       |           v                      |   .    |               |                      |
       |        (read-sequence buffer socket) .....|..........     |                      |
       |           |                      |        v         .     +----------------------+
       +-----------+                  +---------------+      .
                                      | socket-stream |      .
                                      +---------------+      .
                                                             .



#+END_CODE
|#

;; -*- mode:lisp -*-


;;;
;;; Telnet Streams
;;;


(defun call-with-telnet-on-stream (low-stream function)
  (let* ((stream (make-instance 'telnet-stream
                                :element-type 'character))
         (down   (make-instance 'down-layer
                                :stream low-stream
                                :client nil))
         (nvt (make-instance 'network-virtual-terminal
                             :name "TELNET SERVER"
                             :client nil
                             :up-sender stream
                             :down-sender down)))
    (setf (slot-value stream 'nvt) nvt
          (slot-value down   'nvt) nvt)
    (funcall function stream)))

(defmacro with-telnet-on-stream ((stream-var stream-expr) &body body)
  "Connects a telnet network-virtual-terminal to the remote stream
resulting from the STREAM-EXPR, and evaluates the BODY in a lexical
context where the STREAM-VAR is bound to a  local bidirectional stream
conected to the NVt."
  `(call-with-telnet-on-stream ,stream-expr (lambda (,stream-var) ,@body)))

(deftype octet () `(unsigned-byte 8))

(defun make-binary-buffer (size)
  (make-array size :element-type 'octet :adjustable t :fill-pointer 0))

(defun make-string-buffer (size)
  ;; base-char is enough for :us-ascii
  (make-array size :element-type 'base-char :adjustable t :fill-pointer 0))

(defun buffer-append (buffer sequence start end)
  (let* ((old-size (length buffer))
         (new-size (+ old-size (- end start))))
    (loop
      :while (< (array-dimension buffer 0) new-size)
      :do (setf buffer (adjust-array buffer
                                     (* 2 (array-dimension buffer 0))
                                     :element-type (array-element-type buffer)
                                     :fill-pointer (fill-pointer buffer))))
    (setf (fill-pointer buffer) new-size)
    (replace buffer sequence :start1 old-size :start2 start :end2 end)
    buffer))


;;
;; down:GSTREAM, NVT-DOWN-SENDER
;;

(defconstant +down-layer-buffer-size+ 1024)

(defclass down-layer ()
  ((nvt         :reader   nvt         :initarg  :nvt)
   (stream      :reader   down-stream :initarg  :stream)
   (client      :reader   client      :initarg  :client)
   (lock        :reader   down-lock)
   (writingp    :accessor %writingp   :initarg  nil)
   (down-buffer :reader   down-buffer :initform (make-binary-buffer +down-layer-buffer-size+))))

(defgeneric input-loop (down-layer))
(defgeneric stop-closure (client)) ;; TODO must be imported from telnet.repl

(defmethod input-loop ((self down-layer))
  ;; The input-loop runs in the client input loop thread
  (loop
    :with buffer := (make-binary-buffer +down-layer-buffer-size+)
    :until (stop-closure (client self))
    :do (setf (fill-pointer buffer) (array-dimension buffer 0))
        (let ((last-pos (read-sequence buffer (down-stream self))))
          (setf (fill-pointer buffer) last-pos))
        (receive (nvt (client self)) buffer)))


(defmethod send ((self down-layer) bytes &key start end)
  ;; The send method is called in the client repl thread
  ;; Down interface (to down):
  ;; Send the bytes to the remote NVT.
  ;; BYTE: a VECTOR of (UNSIGNED-BYTE 8).
  (let ((buffer (down-buffer self))
        (stream (down-stream self)))
    (with-lock-held ((down-lock self))
      (if (%writingp self)
          (progn
            (buffer-append buffer bytes start end)
            (return-from send))
          (setf (%writingp self) t)))
    (write-sequence bytes stream :start start :end end)
    ;; Either we write sequence with lock held, or we need a
    ;; double-buffering to be able to write a buffer while we append
    ;; to the other.  The double-buffer would be better in case of
    ;; symetrical writers, But it is expected that one writer will
    ;; send bigger data and more often than the other.
    ;; So this should do better:
    (with-lock-held ((down-lock self))
      (when (plusp (length buffer))
        (write-sequence buffer stream)
        (setf (fill-pointer buffer) 0))
      (setf (%writingp self) nil))))


;;
;; up:GSTREAM, NVT-UP-SENDER
;;

(defgeneric stream-output-buffering (stream)) ; *
(defgeneric stream-input-buffering  (stream)) ; *
(defgeneric stream-echo-mode        (stream)) ; *
(defgeneric stream-external-format  (stream)) ; *
(defgeneric stream-element-type     (stream)) ; *

(defgeneric (setf stream-output-buffering) (new-buffering       stream))
(defgeneric (setf stream-input-buffering)  (new-buffering       stream))
(defgeneric (setf stream-echo-mode)        (new-echo            stream))
(defgeneric (setf stream-external-format)  (new-external-format stream))
(defgeneric (setf stream-element-type)     (new-element-type    stream))


(defclass telnet-stream (fundamental-character-input-stream
                         fundamental-character-output-stream
                         fundamental-binary-input-stream
                         fundamental-binary-output-stream)
  ((element-type             :reader   stream-element-type      :initform 'character
                             :initarg :element-type)
   (external-format          :reader   stream-external-format   :initform :us-ascii)
   (input-buffering          :reader   stream-input-buffering   :initform :character)
   (output-buffering         :reader   stream-output-buffering  :initform :line)
   (echo-mode                :reader   stream-echo-mode         :initform t)

   (nvt                      :reader   nvt
                             :initarg :nvt)
   (open                     :reader   open-stream-p            :initform t)
   (lock                     :reader   stream-lock)
   (input-data-present       :reader   for-input-data-present
                             :documentation "A condition variable indicating that the input-buffer is not empty.")
   (input-free-space         :reader   for-input-free-space
                             :documentation "A condition variable indicating that the input-buffer has more free space.")
   (input-buffer             :reader   input-buffer)
   (output-buffer            :reader   output-buffer)
   (column                   :accessor column                   :initform 0)
   (partial-character-octets :reader   partial-character-octets :initform (make-binary-buffer 4))
   (unread-character         :accessor unread-character         :initform nil)
   (peeked-character         :accessor peeked-character         :initform nil)))


;; input-buffer is a vector of octet
;; listen peeks for a character
;; unread-char puts back a character
;; Depending on the encoding, a character may require several octets.
;; Therefore we use a small byte buffer and a character buffer in telnet-stream.

#|

# -*- mode:org -*-

The input characters are stored in a virtual buffer composed of the
following slots:

| unread-character         | nil, or the character that has been unread-char'ed, until read       |
| peeked-character         | nil, or the character that has been peek-char'ed, until read         |
| partial-character-octets | the octets read so far from input buffer that don't make a character |
| input-buffer             | the octets received so far                                           |

When reading characters, we take first the unread-character, next the
peeked-character, then we complete the partial-character-octets, then
we may decode them from the input-buffer.

| unread-character | peeked-character | read-char, peek-char |
|------------------+------------------+----------------------|
| t                | t                | unread-character     |
| t                | nil              | unread-character     |
| nil              | t                | peeked-character     |
| nil              | nil              | go to partial        |

# |#

;; -*- mode:lisp -*-


(defmethod print-object ((stream telnet-stream) output)
  (declare (stepper disable))
  (print-unreadable-object (stream output :type t :identity t)
    (format output "(~@{~<~%  ~1:;~S ~S~>~^ ~})"
            :open (open-stream-p stream)
            :element-type (stream-element-type stream)
            :external-format (stream-external-format stream)
            :input-buffering (stream-input-buffering stream)
            :output-buffering (stream-output-buffering stream)
            :echo-mode (stream-echo-mode stream)
            :length-input-buffer (length (input-buffer stream))
            :length-output-buffer (length (output-buffer stream))))
  stream)

(defconstant +input-buffer-size+  4096)
(defconstant +output-buffer-size+ 4096)

(defmethod initialize-instance :after ((stream telnet-stream) &key &allow-other-keys)
  (setf (slot-value stream 'lock)               (make-lock "telnet-stream")
        (slot-value stream 'input-data-present) (make-condition-variable :name "input-data-present")
        (slot-value stream 'input-free-space)   (make-condition-variable :name "input-free-space")
        (slot-value stream 'input-buffer)       (make-input-buffer (stream-input-buffering stream))
        (slot-value stream 'output-buffer)      (make-binary-buffer +output-buffer-size+)))


(defmethod (setf stream-output-buffering) (new-buffering       (stream telnet-stream))
  (let ((new-buffering (or new-buffering :character)))
    (check-type new-buffering (member :character :line))
    (setf (slot-value stream 'output-buffering) new-buffering)))

(defmethod (setf stream-input-buffering)  (new-buffering       (stream telnet-stream))
  (let ((new-buffering (or new-buffering :character))
        (buffer (slot-value stream 'input-buffer)))
    (check-type new-buffering (or (integer 1) (member :character :line)))
    (with-lock-held ((stream-lock stream))
      (setf (slot-value stream 'input-buffering) new-buffering)
      (when (integerp new-buffering)
        ;; TODO: check if the new buffer size is enough for the buffer content, and keep the old buffered input.
        (adjust-array (input-buffer-data buffer)
                      new-buffering :fill-pointer new-buffering)
        (setf (input-buffer-head buffer) 0
              (input-buffer-tail buffer) 0)))
    new-buffering))

(defmethod (setf stream-echo-mode)        (new-echo            (stream telnet-stream))
  (check-type new-echo boolean)
  (setf (slot-value stream 'echo-mode) new-echo)
  (setf (option-enabled-p (nvt stream) :echo :us) t)
  new-echo)

(defun type-equal-p (a b)
  (and (subtypep a b) (subtypep b a)))

(defmethod configure-mode ((stream telnet-stream))
  (setf (option-enabled-p (nvt stream) :transmit-binary :us)
        (not (and (eq (stream-external-format stream) :us-ascii)
                  (type-equal-p (stream-element-type stream) 'character)))))

(defmethod (setf stream-external-format)  (new-external-format (stream telnet-stream))
  (let* ((cs (typecase new-external-format
               ((member :default)  (find-character-set :us-ascii))
               ((or string symbol) (find-character-set new-external-format))
               (t ;; Assume implementation dependent external-format
                (character-set-for-lisp-encoding new-external-format))))
         (le (first (cs-lisp-encoding cs))))
    (unless le
      (error "Invalid external-format ~S, got character-set ~S and no lisp-encoding."
             new-external-format cs))

    (with-lock-held ((stream-lock stream))
      (let* ((old-external-format (stream-external-format stream))
             (new-external-format (intern le "KEYWORD"))
             (change (or (and (eq :us-ascii old-external-format)
                              (not (eq :us-ascii new-external-format)))
                         (and (not (eq :us-ascii old-external-format))
                              (eq :us-ascii new-external-format)))))

        ;; TODO: negociate with remote for a new encoding.
        ;; TODO: check element-type first:
        (setf (slot-value stream 'external-format) new-external-format)
        (when change
          ;; before configure-mode:
          (flush-output-buffer stream)))
      (configure-mode stream))
    new-external-format))


(defmethod (setf stream-element-type)     (new-element-type    (stream telnet-stream))
  ;; TODO: negociate with remote for a text or binary.
  (with-lock-held ((stream-lock stream))
    (setf (slot-value stream 'element-type)
          (cond
            ((or (type-equal-p new-element-type 'character)
                 (type-equal-p new-element-type 'base-char))
             'character)
            ((type-equal-p new-element-type '(unsigned-byte 8))
             '(unsigned-byte 8))
            (t (error "Unsupported element-type ~S" new-element-type))))
    (configure-mode stream))
  new-element-type)


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

(defun check-sequence-arguments (direction stream sequence start end)
  (assert (<= 0 start end (length sequence))
          (sequence start end)
          "START = ~D or END = ~D are not sequence bounding indexes for a ~S of length ~D"
          start end (type-of sequence) (length sequence))
  (ecase direction
    (:read
     (assert (or (listp sequence)
                 (and (vectorp sequence)
                      (subtypep (stream-element-type stream) (array-element-type sequence))))
             (sequence)
             "For reading, the sequence element type ~S should be a supertype of the stream element type ~S"
             (array-element-type sequence) (stream-element-type stream)))
    (:write
     (assert (or (listp sequence)
                 (and (vectorp sequence)
                      (subtypep (array-element-type sequence) (stream-element-type stream))))
             (sequence)
             "For writing, the sequence element type ~S should be a subtype of the stream element type ~S"
             (array-element-type sequence) (stream-element-type stream)))))


;;;
;;; Input
;;;

(defstruct (input-buffer
            (:constructor %make-input-buffer))
  data
  (head 0)
  (tail 0))

(defun make-input-buffer (buffering)
  (let* ((size (if (integerp buffering)
                   buffering
                   +input-buffer-size+))
         (buffer (make-binary-buffer size)))
    (setf (fill-pointer buffer) size)
    (%make-input-buffer :data buffer)))

(defun %input-buffer-free-space (stream)
  (let* ((buffer (input-buffer stream))
         (size   (length (input-buffer-data buffer))))
    (- size 1 (mod (- (input-buffer-tail buffer)
                      (input-buffer-head buffer))
                   size))))
(declaim (inline %input-buffer-free-space))

(defun %input-buffer-length (stream)
  (let* ((buffer (input-buffer stream))
         (size   (length (input-buffer-data buffer))))
    (mod (- (input-buffer-tail buffer)
            (input-buffer-head buffer))
         size)))
(declaim (inline %input-buffer-length))

(defun %input-buffer-ranges (buffer len)
  (let* ((size (length (input-buffer-data buffer)))
         (tail (input-buffer-tail buffer))
         (max1 (- size tail)))
    (if (<= len max1)
        (values tail len  nil nil          (mod (+ tail len) size))
        (values tail max1 0   (- len max1) (mod (+ tail len) size)))))
(declaim (inline %input-buffer-ranges))

(defun %wait-for-input-free-space (stream required)
  (loop
      :while (< (%input-buffer-free-space stream) required)
      :do (condition-wait (for-input-free-space stream) (stream-lock stream))))
(declaim (inline %wait-for-input-free-space))

(defun %wait-for-input-data-present (stream required)
  (loop
    :while (< (%input-buffer-length stream) required)
    :do (condition-wait (for-input-data-present stream) (stream-lock stream))))
(declaim (inline %wait-for-input-data-present))

(defmethod input-buffer-fetch-octet ((stream telnet-stream) nohang)
  ;;    then read the octet and convert it to character
  ;;    else (%wait-for-input-data-present)
  (if (and nohang (zerop (%input-buffer-length stream)))
      nil
      (with-lock-held ((stream-lock stream))
        (%wait-for-input-data-present stream 1)
        (let* ((buffer (input-buffer stream))
               (data   (input-buffer-data buffer))
               (head   (input-buffer-head buffer))
               (octet  (aref data head)))
          (setf (input-buffer-head buffer) (mod (+ head 1) (length data)))
          (condition-notify (for-input-free-space stream))
          octet))))

(defmethod input-buffer-append-octet ((stream telnet-stream) octet)
  (with-lock-held ((stream-lock stream))
    (%wait-for-input-free-space stream 1)
    ;; copy one octet
    (let ((buffer (input-buffer stream)))
      (multiple-value-bind (s1 e1 s2 e2 nt) (%input-buffer-ranges buffer 1)
        (declare (ignore e1 s2 e2))
        (setf (aref (input-buffer-data buffer) s1) octet)
        (setf (input-buffer-tail buffer) nt)))
    (condition-notify (for-input-data-present stream))))

(defmethod input-buffer-append-text ((stream telnet-stream) text)
  (let* ((end (length text))
         (len (+ end (count #\newline text))))
    (when (plusp len)
      (with-lock-held ((stream-lock stream))
        (%wait-for-input-free-space stream len)
        ;; copy the text octets.
        (let* ((buffer (input-buffer stream))
               (data   (input-buffer-data buffer)))
          (multiple-value-bind (s1 e1 s2 e2 nt) (%input-buffer-ranges buffer len)
            (let ((start2 (nth-value 1 (replace-ascii-bytes data text :newline :crlf :start1 s1 :end1 e1 :start2 0 :end2 end))))
              (when s2
                (replace-ascii-bytes data text :newline :crlf :start1 s2 :end1 e2 :start2 start2 :end2 end)))
            (setf (input-buffer-tail buffer) nt)))
        (condition-notify (for-input-data-present stream))))))

(defmethod input-buffer-append-octets ((stream telnet-stream) octets start end)
  (let ((len (- (or end (length octets)) start)))
    (when (plusp len)
      (with-lock-held ((stream-lock stream))
        (%wait-for-input-free-space stream len)
        ;; copy the binary octets.
        (let* ((buffer (input-buffer stream))
               (data   (input-buffer-data buffer)))
          (multiple-value-bind (s1 e1 s2 e2 nt) (%input-buffer-ranges buffer len)
            (replace data octets :start1 s1 :end1 e1 :start2 start :end2 (- e1 s1))
            (when s2
              (replace data octets :start1 s2 :end1 e2 :start2 (+ start (- e1 s1)) :end2 end))
            (setf (input-buffer-tail buffer) nt)))
        (condition-notify (for-input-data-present stream))))))

(defmethod input-buffer-erase-character ((stream telnet-stream))
  (with-lock-held ((stream-lock stream))
    (let* ((buffer (input-buffer stream))
           (data   (input-buffer-data buffer))
           (tail   (input-buffer-tail buffer)))
      (when (plusp (%input-buffer-length stream))
        (let ((last (mod (- tail 1) (length data))))
          (unless (or (= (aref data last) lf)
                      (= (aref data last) cr))
            (setf (input-buffer-tail buffer) last)
            (condition-notify (for-input-free-space stream))))))))

(defmethod input-buffer-erase-line ((stream telnet-stream))
  (with-lock-held ((stream-lock stream))
    (let* ((buffer (input-buffer stream))
           (data   (input-buffer-data buffer))
           (head   (input-buffer-head buffer))
           (tail   (input-buffer-tail buffer))
           (size   (length data)))
      (when (plusp (%input-buffer-length stream))
        (loop
          :with last := (mod (- tail 1) size)
          :while (and (/= (aref data last) lf)
                      (/= (aref data last) cr))
          :do (setf last (mod (- last 1) size))
          :until (= last head)
          :finally (setf (input-buffer-tail buffer) last)
                   (condition-notify (for-input-free-space stream)))))))

(defmethod input-buffer-peek-octet ((stream telnet-stream))
  (with-lock-held ((stream-lock stream))
    (let* ((buffer (input-buffer stream))
           (data   (input-buffer-data buffer))
           (head   (input-buffer-head buffer)))
      (when (plusp (%input-buffer-length stream))
        (aref data head)))))

(defmethod input-buffer-read-octet ((stream telnet-stream))
  (with-lock-held ((stream-lock stream))
    (%wait-for-input-data-present stream 1)
    (let* ((buffer (input-buffer stream))
           (data   (input-buffer-data buffer))
           (head   (input-buffer-head buffer))
           (size   (length data)))
      (prog1 (aref data head)
        (setf (input-buffer-data buffer) (mod (+ data 1) size))))))

;; (defmethod input-buffer-read-octets ((stream telnet-stream) octets &key (start 0) end)
;;   (with-lock-held ((stream-lock stream))
;;     (let ((len (- (or end (length octets)) start)))
;;       (if (< (length (input-buffer-data))))
;;       (%wait-for-input-data-present stream len)
;;       (let* ((buffer (input-buffer stream))
;;              (data   (input-buffer-data buffer))
;;              (head   (input-buffer-head buffer))
;;              (size   (length data)))
;;         (prog1 (aref data head)
;;           (setf (input-buffer-data buffer) (mod (+ data 1) size)))))))


;; Up interface (to up):

(defmethod want-option-p ((up-sender telnet-stream) option-code)
  (declare (ignore option-code))
  ;; Asks the upper layer whether the option is wanted.
  ;; OPTION-NAME: a keyword denoting the option.
  (warn "~S not implemented yet" 'want-option-p))

(defmethod receive-option  ((up-sender telnet-stream) option value)
  ;; Receive a result from an option request.
  ;; OPTION: the option instance.
  ;; VALUE:  a value the option sends back.
  (declare (ignore option value))
  (warn "~S not implemented yet" 'receive-option))

(defmethod receive-binary  ((up-sender telnet-stream) bytes &key (start 0) end)
  ;; Receive some binary text.
  ;; BYTE:       a VECTOR of (UNSIGNED-BYTE 8).
  ;; START, END: bounding index designators of sequence.
  ;;             The defaults are for START 0 and for END nil.
  (input-buffer-append-octets up-sender bytes start (or end (length bytes))))

(defmethod receive-text    ((up-sender telnet-stream) (text string) &key (start 0) end)
  ;; Receive some ASCII text
  ;; TEXT: a string containing only printable ASCII characters and #\newline.
  (assert (zerop start))
  (assert (null end))
  (input-buffer-append-text up-sender text))

(defmethod receive-text    ((up-sender telnet-stream) (text vector) &key (start 0) end)
  ;; Receive some ASCII text
  ;; TEXT: a string containing only printable ASCII characters and #\newline.
  (input-buffer-append-octets up-sender text start end))

(defmethod receive-control ((up-sender telnet-stream) control)
  ;; Receive a function code.
  ;; CONTROL: (member :are-you-there :abort-output :interrupt-process :go-ahead
  ;;                  :erase-line :erase-character
  ;;                  :break :cr :ff :vt :lf :ht :bs :bel :nul
  ;;                  :end-of-record).
  (case control
    ;; | Controls          | I/O        | Description                                      |
    ;; |-------------------+------------+--------------------------------------------------|
    ;; | are-you-there     | output     | should send an answer                            |
    ;; |                   |            | at the REPL we could send the prompt             |
    ;; |                   |            | when reading, could issue a message or a prompt? |
    ;; |                   |            | when processing, could issue a message about the |
    ;; |                   |            | current function (short backtrace?)              |
    ;; |                   |            |                                                  |
    ;; | abort-output      | output     | clear output buffer (send-buffer, output buffer) |
    ;; |                   |            |                                                  |
    ;; | interrupt-process | interrupt  | enter the debugger, or resume REPL               |
    ;; |                   |            |                                                  |
    ;; | go-ahead          | output     | for half-duplex, resume sending.                 |
    ;; |                   |            |                                                  |
    ;; | erase-line        | input      | erase the input buffer                           |
    ;; |                   |            |                                                  |
    ;; | erase-character   | input      | erase the previous character in the input buffer |
    ;; |                   |            |                                                  |
    ;; | break             | interrupt? | enter the debugger, or resume REPL               |
    ;; |                   |            | at the REPL prompt, break could reissue          |
    ;; |                   |            | the prompt without entering the debugger.        |
    ;; |                   |            |                                                  |
    ;; | end-of-record     | input      | EOF?                                             |
    ;;
    ;; Note: the input buffer may contain multiple lines. ERASE-LINE erase
    ;; back up to the previous newline, but cannot erase the previous line.
    ;; ERASE-CHARACTER cannot erase a newline in the buffer either.
    (:are-you-there
     ;; if output-buffer contains something,
     ;; then flush it
     ;; else send a nul or a message?
     )
    (:abort-output
     ;; clear-output-buffer
     )
    (:interrupt-process
     ;; signal keyboard-interrupt in the repl thread
     )
    (:break
     ;; signal keyboard-interrupt in the repl thread
     )
    (:go-ahead
     ;; we don't do half-duplex.
     ;; flush-output
     )
    (:erase-line
     ;; find last line in input-buffer and erase it
     (input-buffer-erase-line up-sender))
    (:erase-character
     ;; find last (non-newline) character in input-buffer and erase it
     (input-buffer-erase-character up-sender))
    (:end-of-record
     ;; mark an end-fo-file?
     )

    ((:cr :ff :vt :lf :ht :bs :bel :nul)
     (input-buffer-append-octet up-sender (case control
                                         (:cr CR)
                                         (:ff FF)
                                         (:vt VT)
                                         (:lf LF)
                                         (:ht HT)
                                         (:bs BS)
                                         (:bel BEL)
                                         (:nul NUL))))
    (otherwise
     ;; log an unknown control
     )))


;;; character input

(declaim (inline update-column))
(defun update-column (stream ch)
  (when (characterp ch)
    (if (char= ch #\newline)
        (setf (column stream) 0)
        (incf (column stream))))
  ch)

(defun %stream-read-char (stream no-hang)
  (with-lock-held ((stream-lock stream))
    (let ((char nil))
      (rotatef char (unread-character stream))
      (unless char
        (rotatef char (peeked-character stream)))
      (update-column
       stream
       (or char
           (let ((encoding (stream-external-format stream)))
             (if (= 1 (babel::enc-max-units-per-char
                       (babel::get-character-encoding
                        encoding)))
                 ;; 1-octet encoding:
                 (let ((code (input-buffer-fetch-octet stream no-hang)))
                   (when code
                     (let* ((octets (make-array 1 :element-type '(unsigned-byte 8) :initial-element code))
                            (char   (decode-character octets :encoding encoding)))
                       (unless char
                         (error "Decoding error code ~A encoding ~S, no such character code" code encoding))
                       char)))
                 ;; n-octets encoding:
                 (loop
                   :named read
                   :with partial := (partial-character-octets stream)
                   :for code := (input-buffer-fetch-octet stream no-hang)
                   :while code
                   :do (vector-push-extend code partial (length partial))
                       (multiple-value-bind (char validp size)
                           (decode-character partial :encoding encoding)
                         (when (and validp (<= size (length partial)))
                           (if char
                               (progn
                                 (replace partial partial :start2 size)
                                 (setf (fill-pointer partial) (- (length partial) size))
                                 (return-from read char))
                               (error "Decoding error code ~A encoding ~S, no such character code" partial encoding))))
                   :finally (return-from read nil)))))))))

(defmethod stream-read-char ((stream telnet-stream))
  (check-stream-open stream 'stream-read-char)
  (%stream-read-char stream nil))

(defmethod stream-read-char-no-hang ((stream telnet-stream))
  (check-stream-open stream 'stream-read-char-no-hang)
  (%stream-read-char stream t))

(defmethod stream-peek-char ((stream telnet-stream))
  (check-stream-open stream 'stream-peek-char)
  (with-lock-held ((stream-lock stream))
    (or (unread-character stream)
        (peeked-character stream)
        (setf (peeked-character stream) (stream-read-char stream)))))

(defmethod stream-read-line ((stream telnet-stream))
  (check-stream-open stream 'stream-read-line)
  (let ((line (make-array 80 :element-type 'character :fill-pointer 0)))
    (flet ((append-char (ch)
             (when (char= ch #\newline)
               (setf (column stream) 0)
               (return-from stream-read-line (values line nil #|TODO: EOF is always NIL?|#)))
             (vector-push-extend ch line (length line))))
      (with-lock-held ((stream-lock stream))
        (let ((char nil))
          (rotatef char (unread-character stream))
          (append-char (unread-character stream)))
        (let ((char nil))
          (rotatef char (peeked-character stream))
          (append-char (peeked-character stream)))
        (loop
           (append-char (stream-read-char stream)))))))

(defmethod stream-listen ((stream telnet-stream))
  (check-stream-open stream 'stream-listen)
  (with-lock-held ((stream-lock stream))
    (or (unread-character stream)
        (peeked-character stream)
        (setf (peeked-character stream) (stream-read-char-no-hang stream)))))

(defmethod stream-unread-char ((stream telnet-stream) char)
  (check-stream-open stream 'stream-unread-char)
  (with-lock-held ((stream-lock stream))
    (when (unread-character stream)
      (error "Two unread-char"))
    (setf (unread-character stream) char)
    (setf (column stream) (max 0 (1- (column stream)))))
  char)

;; binary input

(defmethod stream-read-byte ((stream telnet-stream))
  (check-stream-open stream 'stream-read-byte)
  (with-lock-held ((stream-lock stream))
    (error "~S Not Implemented Yet" 'stream-read-byte)))


;;; Sequence Input


(defmethod stream-read-sequence ((stream telnet-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-read-sequence)
  (check-sequence-arguments :read stream sequence start end)
  (with-lock-held ((stream-lock stream))
    (error "~S Not Implemented Yet" 'stream-read-sequence)))


;;;
;;; Output
;;;

;; ;; Up interface (from up):
;;
;; (defgeneric send-binary  (nvt bytes)
;;   (:documentation "Send the binary text.
;; NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
;; BYTE: a VECTOR of (UNSIGNED-BYTE 8)."))
;;
;; (defgeneric send-text    (nvt text)
;;   (:documentation "Send the ASCII text.
;; NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
;; TEXT: a string containing only printable ASCII characters and #\newline."))
;;
;; (defgeneric send-control (nvt control)
;;   (:documentation "Send a function control code.
;; NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
;; CONTROL: (member :synch :are-you-there :abort-output :interrupt-process :go-ahead
;;                  :erase-line :erase-character
;;                  :break :cr :ff :vt :lf :ht :bs :bel :nul
;;                  :end-of-record)."))

;;; character output

(defun flush-output-buffer (stream)
  (let ((buffer (output-buffer stream)))
    (when (plusp (length buffer))
      (if (eq :us-ascii  (stream-external-format stream))
          (send-text   (nvt stream) buffer)
          (send-binary (nvt stream) buffer))
      (setf (fill-pointer buffer) 0))))

(defmethod stream-write-char ((stream telnet-stream) char)
  (check-stream-open stream 'stream-write-char)
  (with-lock-held ((stream-lock stream))
    (let ((encoding  (stream-external-format stream))
          (buffering (stream-output-buffering stream)))
      (case buffering

        (:character                     ; no buffering

         ;; If we have something in the buffer flush it now.
         (flush-output-buffer stream)

         (cond
           ((char= #\newline char)
            (send-control (nvt stream) :cr)
            (send-control (nvt stream) :lf))
           ((eq encoding :us-ascii)
            (send-text (nvt stream) (string char)))
           (t
            (send-binary (nvt stream)
                         (string-to-octets (string char)
                                           :encoding encoding)))))

        (:line                          ; line buffering
         (cond
           ((char= #\newline char) ; if newline, flush the buffer now.
            (flush-output-buffer stream)
            (send-control (nvt stream) :cr)
            (send-control (nvt stream) :lf))
           ((eq encoding :us-ascii)
            (let ((buffer (output-buffer stream)))
              (vector-push-extend char buffer (length buffer)))
            (send-text (nvt stream) (string char)))
           (t
            (let* ((buffer (output-buffer stream))
                   (start (fill-pointer buffer)))
              (setf (fill-pointer buffer) (array-dimension buffer 0))
              (setf (fill-pointer buffer)
                    (nth-value 1 (replace-octets-by-string buffer (string char)
                                                           :start1 start
                                                           :encoding encoding)))))))
        (otherwise
         (assert (integerp buffering))
         (cond
           ((eq encoding :us-ascii)
            (let ((buffer (output-buffer stream)))
              (if (char= #\newline char)
                  (progn
                    (vector-push-extend #\return   buffer (length buffer))
                    (vector-push-extend #\linefeed buffer (length buffer)))
                  (vector-push-extend char buffer (length buffer)))
              (when (<= buffering (length buffer))
                (send-text (nvt stream) buffer)
                (setf (fill-pointer buffer) 0))))
           (t
            (let* ((buffer (output-buffer stream))
                   (start (fill-pointer buffer)))
              (setf (fill-pointer buffer) (array-dimension buffer 0))
              (setf (fill-pointer buffer)
                    (nth-value 1 (replace-octets-by-string buffer (string char)
                                                           :start1 start
                                                           :encoding encoding)))
              (when (<= buffering (length buffer))
                (send-binary (nvt stream) buffer)
                (setf (fill-pointer buffer) 0)))))))

      (if (char= #\newline char)
          (setf (column stream) 0)
          (incf (column stream)))))
  char)

(defmethod stream-terpri ((stream telnet-stream))
  (check-stream-open stream 'stream-terpri)
  (stream-write-char stream #\newline)
  nil)


(defgeneric encode-string-to-output-buffer (stream string &key start end))
(defmethod encode-string-to-output-buffer ((stream telnet-stream) string &key (start 0) end)
  ;; STRING doesn't contain #\newline
  (let* ((encoding (stream-external-format stream))
         (buffer   (output-buffer stream))
         (start1   (fill-pointer buffer)))
    (setf (fill-pointer buffer) (array-dimension buffer 0))
    (loop
       (multiple-value-bind (filledp end1)
           (replace-octets-by-string buffer string
                                     :start1 start1
                                     :start2 start
                                     :end2 end
                                     :encoding encoding
                                     :use-bom nil
                                     :errorp nil ; use replacement character
                                     :error-on-out-of-space-p nil)
         (when filledp
           (setf (fill-pointer buffer) end1)
           (return-from encode-string-to-output-buffer))
         (let ((size (* (ceiling end1 1024) 1024)))
           (setf (slot-value stream 'output-buffer)
                 (setf buffer (adjust-array buffer size :fill-pointer size))))))))

(defmacro for-each-line (((line-var start-var end-var)
                          (string-expression start-expr end-expr))
                         line-expression
                         &body newline-body)
  (let ((vstring      (gensym))
        (vnewlines    (gensym))
        (process-line (gensym))
        (vstart       (gensym))
        (vend         (gensym)))
    `(let* ((,vstring    ,string-expression)
            (,vnewlines  (positions #\newline ,vstring :start ,start-expr :end ,end-expr)))
       (flet ((,process-line (start end)
                (let ((,line-var ,vstring)
                      (,start-var start)
                      (,end-var end))
                  ,line-expression)))
         (loop
           :for ,vstart := 0 :then (1+ ,vend)
           :for ,vend :in ,vnewlines
           :do (progn
                 (when (< ,vstart ,vend)
                   (,process-line ,vstart ,vend))
                 ,@newline-body)
           :finally (when (< ,vstart (length ,vstring))
                      (,process-line ,vstart (length ,vstring))))))))

(defmethod stream-write-string ((stream telnet-stream) string &optional (start 0) end)
  (check-stream-open stream 'stream-write-string)
  (with-lock-held ((stream-lock stream))
    (let ((end (or end (length string))))
      ;; If we have something in the buffer flush it now.
      (flush-output-buffer stream)
      (for-each-line ((line lstart lend) (string start end))
                     (encode-string-to-output-buffer stream line lstart lend)
        (flush-output-buffer stream)
        (send-control (nvt stream) :cr)
        (send-control (nvt stream) :lf))
      (setf (column stream) (fill-pointer (output-buffer stream)))))
  string)

(defmethod stream-line-column ((stream telnet-stream))
  (column stream))

(defmethod stream-start-line-p ((stream telnet-stream))
  (zerop (column stream)))

(defmethod stream-advance-to-column ((stream telnet-stream) column)
  (check-stream-open stream 'stream-advance-to-column)
  (with-lock-held ((stream-lock stream))
    (let ((delta (- column (column stream))))
      (when (plusp delta)
        (stream-write-string stream (make-string delta :initial-element #\space))
        delta))))



(defmethod close ((stream telnet-stream) &key abort)
  (declare (ignore abort))
  (with-lock-held ((stream-lock stream))
    ;; TODO: close a telnet-stream

    ;; (with-lock-held ((lock stream))
    ;;   (setf (slot-value (stream-output-stream stream) 'open) nil))
    ;; (gate-signal (not-empty stream))
    ;; (sink-stream (telnet-stream stream))
    ))




;; binary output

(defmethod stream-write-byte ((stream telnet-stream) byte)
  (check-stream-open stream 'stream-write-byte)
  (with-lock-held ((stream-lock stream))
    ;; TODO
    (vector-push byte (output-buffer stream)))
  byte)

;;; Sequence Output

(defmethod stream-write-sequence ((stream telnet-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-write-sequence)
  (check-sequence-arguments :write stream sequence start end)
  ;; TODO
  (with-lock-held ((stream-lock stream))
    ;; TODO
    )
  sequence)

(defmethod stream-finish-output ((stream telnet-stream))
  ;; Attempts to ensure that all output sent to the stream has reached its
  ;; destination, and only then returns false. Implements
  ;; cl:finish-output. The default method does nothing.
  (with-lock-held ((stream-lock stream))
    (flush-output-buffer stream)
    ;; TODO: how to wait for write-sequence completed?
    ))

;; We assign the semantics of waiting for the reader process to
;; have read all the data written so far.
;;
;; NO: This could be done with a empty condition, and the writer waiting
;;     on the empty condition.  The reader would notify it when reaching
;;     an empty telnet.
;; This assumes a single writer stream.  While waiting for the
;; condition another writer stream could further write data.
;; Therefore we need instead to be able to enqueue tokens into the telnet,
;; that could be used to message back to the exact writing thread.
;;
;; (defmethod stream-finish-output ((stream telnet-stream))
;;   (with-lock-held ((lock stream
;;     (loop :until (%telnet-emptyp stream)
;;           :do (condition-wait (empty stream) (lock stream)))))


;; telnet-warning
;; telnet-error

#|



;; option control:

(defgeneric option-enabled-p (nvt option-name &optional who)
  (:documentation "Whether the option is currently enabled,
if WHO is nil, then for either end, otherwise for the indicated end.
OPTION-NAME: a keyword or fixnum  denoting the option.
WHO:         (member nil :us :him)."))

(defgeneric option-negotiating-p (nvt option-name &optional who)
  (:documentation "Whether the option is currently being negotiated,
if WHO is nil, then for either end, otherwise for the indicated end.
OPTION-NAME: a keyword or fixnum  denoting the option.
WHO:         (member nil :us :him)."))

(defgeneric enable-option    (nvt option-name &optional who)
  (:documentation "Initiate the negotiation to enable the option.
OPTION-NAME: a keyword or fixnum  denoting the option.
WHO:         (member nil :us :him)."))

(defgeneric disable-option   (nvt option-name &optional who)
  (:documentation "Initiate the negotiation to disable the option.
OPTION-NAME: a keyword or fixnum  denoting the option.
WHO:         (member nil :us :him)."))


(defun (setf option-enabled-p) (flag nvt option-name &optional who)
  "Enable or disable the option according to the boolean FLAG.
OPTION-NAME: a keyword or fixnum denoting an option."
  (if flag
      (enable-option  nvt option-name who)
      (disable-option nvt option-name who)))



(defgeneric option-register-class (nvt option-name option-class)
  (:documentation "Register OPTION-CLASS as the class for a given OPTION-NAME.
NOTE:         If the option is already initialized with a different
              class, then CHANGE-CLASS is called on the instance.
OPTION-NAME:  a keyword or fixnum denoting an option.
OPTION-CLASS: a class designator, should be a subclass of OPTION."))


(defgeneric option-register-default-classes (nvt option-names)
  (:documentation "Register the default option-classes for the option given in OPTION-NAMES.
NOTE:         If the option is already initialized with a different
              class, then CHANGE-CLASS is called on the instance.
OPTION-NAMES: a list of keyword or fixnum denoting options.
RETURN:       The subset of OPTION-NAMES (codes are converted into
              option-names) for which a specific default class
              exists."))


;; Implemented by subclasses of OPTION:

(defgeneric receive-subnegotiation (option nvt bytes &key start end)
  (:documentation "Processes the subnegotiation packet (subseq bytes start end)
starting with IAC SB and ending with IAC SE."))

|#


;;;; THE END ;;;;
