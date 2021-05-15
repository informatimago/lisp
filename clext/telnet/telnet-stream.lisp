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
                                          |   .    |                client repl thread                            
                                          |   .    |          until (send-buffer nvt) is empty.  
                                          |   .    v                        +-------------+   
                               +--------------------------+                 |             |
       options --------------->| NETWORK-VIRTUAL-TERMINAL |                 v             |
                               +--------------------------+          (send-wait-p nvt)/   |
                                  |       ^   .    |---------------> (send-buffer nvt)    |
                                  |       |   . (send down bytes)                         |
                                  |       |   .    |                                      |
                                  |       |   .    |                                      |
                                  v       |   .    v                                      |
                       +------------+     |   .  +-----------------+                      |
                       | OPTION-MGR |*    |   .  | NVT-DOWN-SENDER |                      |
                       +------------+     |   .  +-----------------+                      |
                                          |   .    |                                      |
                          (receive nvt bytes) .    |                                      |
                                          |   .    |                                      |
                                          |   .    v                                      |
                                      +--------------+                                    |
                                      | down:GSTREAM |                                    |
 client input loop thread             +--------------+                                    |
       +-----------+                      ^   .    |                                      |
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
;; 
;; -*- mode:lisp -*-




;;;
;;; Telnet Streams
;;;

(defmacro with-telnet-on-stream ((stream-var stream-expr) &body body)
  `(let ((,stream-var ,stream-expr))
    ,@body))

(deftype octet () `(unsigned-byte 8))

(defun make-buffer (size)
  (make-array size :element-type 'octet :adjustable t :fill-pointer 0))

;;
;; down:GSTREAM
;;

(defconstant +down-layer-buffer-size+ 1024)

(defclass down-layer ()
  ((nvt    :reader nvt         :initarg :nvt)
   (stream :reader down-stream :initarg :stream)
   (client :reader client      :initarg :client)))

(defgeneric input-loop (down-layer))
(defgeneric stop-closure (client)) ;; TODO must be imported from telnet.repl

(defmethod input-loop ((self down-layer))
  (loop
    :with buffer := (make-buffer +down-layer-buffer-size+)
    :until (stop-closure (client self))
    :do (setf (fill-pointer buffer) (array-dimension buffer 0))
        (let ((last-pos (read-sequence buffer (down-stream self))))
          (setf (fill-pointer buffer) last-pos))
        (receive (nvt (client self)) buffer)))


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
   (external-format          :reader   stream-external-format   :initform :default)
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
   (partial-character-octets :reader   partial-character-octets :initform (make-buffer 4))
   (unread-character         :accessor unread-character         :initform nil)))


;; input-buffer is a vector of octet
;; listen peeks for a character
;; unread-char puts back a character
;; Depending on the encoding, a character may require several octets.
;; Therefore we use a small byte buffer and a character buffer in telnet-stream.



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
  (setf (slot-value stream 'lock)                   (make-lock "telnet-stream")
        (slot-value stream 'for-input-data-present) (make-condition-variable :name "input-data-present")
        (slot-value stream 'for-input-free-space)   (make-condition-variable :name "input-free-space")
        (slot-value stream 'input-buffer)           (make-input-buffer (stream-input-buffering stream))
        (slot-value stream 'output-buffer)          (make-buffer +output-buffer-size+)))




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
  (%make-input-buffer
   :data (make-buffer (if (integerp buffering)
                          buffering
                          +input-buffer-size+))))

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

(defmethod receive-text    ((up-sender telnet-stream) text)
  ;; Receive some ASCII text
  ;; TEXT: a string containing only printable ASCII characters and #\newline.
  (input-buffer-append-text up-sender text))

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

(defmethod stream-read-char ((stream telnet-stream))
  (check-stream-open stream 'stream-read-char)
  (let ((ch (unread-character stream)))
    (if ch
        (progn
          (setf (unread-character stream) nil)
          (update-column stream ch))
        ;; if 1-1 encoding
        ;; then
        ;;    if there's an octet
        ;;    then read the octet and convert it to character
        ;;    else (%wait-for-input-free-space)
        ;; else
        ;;    while partial cannot convert
        ;;        if there's an octet
        ;;            then read the octet into partial
        ;;        else (%wait-for-input-free-space)
        ;;    convert partial to character and reset partial
        
        
        )))

(defmethod stream-read-char-no-hang ((stream telnet-stream))
  (check-stream-open stream 'stream-read-char-no-hang)
  (update-column stream (%peek-or-dequeue (telnet-stream-telnet stream) nil :no-hang)))

 (defmethod stream-peek-char ((stream telnet-stream))
  (check-stream-open stream 'stream-peek-char)
  (telnet-peek-element (telnet-stream-telnet stream)))

(defmethod stream-read-line ((stream telnet-stream))
  (check-stream-open stream 'stream-read-line)
  (multiple-value-bind (line eof) (telnet-dequeue-until-element (telnet-stream-telnet stream) #\newline)
    (setf (column stream) 0)
    (values line eof)))

(defmethod stream-listen ((stream telnet-stream))
  (check-stream-open stream 'stream-listen)

  ;; if 1-1 encoding then
  (input-buffer-peek-octet stream)
  
  
  (%peek-or-dequeue (telnet-stream-telnet stream) :peek :no-hang))

(defmethod stream-unread-char ((stream telnet-stream) ch)
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

;; binary input

(defmethod stream-read-byte ((stream telnet-stream))
  (check-stream-open stream 'stream-read-byte)
  (%peek-or-dequeue (telnet-stream-telnet stream) nil nil))


;;; Sequence Input


(defmethod stream-read-sequence ((stream telnet-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-read-sequence)
  (check-sequence-arguments :read stream sequence start end)
  (telnet-dequeue-sequence stream sequence start end))


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
        
(defmethod stream-write-char ((stream telnet-stream) ch)
  (check-stream-open stream 'stream-write-char)
  (let ((telnet (telnet-stream-nvt stream)))
    (unless (sunk-telnet-p telnet)
      (telnet-enqueue-element telnet ch)))
  (if (char= #\newline ch)
      (setf (column stream) 0)
      (incf (column stream)))
  ch)

(defmethod stream-terpri ((stream telnet-stream))
  (check-stream-open stream 'stream-terpri)
  (stream-write-char stream #\newline)
  nil)

(defmethod stream-write-string ((stream telnet-stream) string &optional (start 0) end)
  (check-stream-open stream 'stream-write-string)
  (let* ((telnet (telnet-stream-telnet stream))
         (end  (or end (length string)))
         (nlp  (position #\newline string :start start :end end :from-end t)))
    (unless (sunk-telnet-p telnet)
      (telnet-enqueue-sequence telnet string start end))
    (if nlp
        (setf (column stream) (- end nlp))
        (incf (column stream) (- end start))))
  string)

(defmethod stream-line-column ((stream telnet-stream))
  (column stream))

(defmethod stream-start-line-p ((stream telnet-stream))
  (zerop (column stream)))

(defmethod stream-advance-to-column ((stream telnet-stream) column)
  (check-stream-open stream 'stream-advance-to-column)
  (let ((delta (- column (column stream))))
    (when (plusp delta)
      (stream-write-string stream (make-string delta :initial-element #\space))
      delta)))

(defgeneric reopen-telnet (telnet)
  (:documentation "Reopens the streams of the TELNET."))
(defmethod reopen-telnet ((stream telnet-stream))
  (with-lock-held ((lock stream))
    (setf (slot-value (stream-input-stream stream)  'open) t
          (slot-value (stream-output-stream stream) 'open) t))
  (gate-signal (not-empty stream)))


(defun sink-stream (stream)
  (with-lock-held ((lock stream))
    (setf (slot-value (stream-input-stream stream) 'open) nil)))


(defmethod close ((stream telnet-stream) &key abort)
  (declare (ignore abort))
  (with-lock-held ((lock stream))
    (setf (slot-value (stream-output-stream stream) 'open) nil))
  ;; (gate-signal (not-empty stream))
  ;; (sink-stream (telnet-stream stream))
  )




;; binary output

(defmethod stream-write-byte ((stream telnet-stream) byte)
  (check-stream-open stream 'stream-write-byte)
  (let ((stream (stream-stream-stream stream)))
    (unless (sunk-stream-p stream)
      (stream-enqueue-element stream byte)))
  byte)

;;; Sequence Output

(defmethod stream-write-sequence ((stream stream-stream) sequence start end &key &allow-other-keys)
  (check-stream-open stream 'stream-write-sequence)
  (check-sequence-arguments :write stream sequence start end)
  (unless (sunk-stream-p stream)
    (stream-enqueue-sequence stream sequence start end))
  sequence)

(defmethod stream-finish-output ((stream telnet-stream))
  ;; Attempts to ensure that all output sent to the stream has reached its
  ;; destination, and only then returns false. Implements
  ;; cl:finish-output. The default method does nothing.
  )

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



;; Down interface (to down):

(defgeneric send (down-sender bytes &key start end)
  (:documentation "Send the bytes to the remote NVT.
BYTE: a VECTOR of (UNSIGNED-BYTE 8)."))


;; Down interface (from down):

(defgeneric receive (nvt bytes &key start end)
  (:documentation "Receive bytes from the remote NVT.
NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
BYTE: a VECTOR of (UNSIGNED-BYTE 8)."))


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

