;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               telnet.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements the TELNET protocol.
;;;;
;;;;    Features:
;;;;
;;;;    - Small interfaces with the upper layer and with the lower layer.
;;;;      (Clients may provide gray-streams or sockets implementing those interfaces.)
;;;;
;;;;    - Negotiation of options is implemented using Bernstein's Q Method.
;;;;
;;;;    - Subnegotiation of options is dereferred to option subclasses.
;;;;
;;;;    - Implements all control functions.
;;;;
;;;;    - Implements the TRANSMIT-BINARY option by forwarding bytes
;;;;      instead of characters.
;;;;
;;;;    - Implements the ECHO option (remote ECHO, not local terminal echo).
;;;;
;;;;    - Implements the SUPPRESS-GO-AHEAD option.
;;;;
;;;;    - Implements the END-OF-RECORD option.
;;;;
;;;;    
;;;;
;;;;
;;;;
;;;;    The following RFCs are implemented (checked 'v'):
;;;;    
;;;;  v RFC 854
;;;;  v RFC 855
;;;;  v RFC 856
;;;;  v RFC 857 ECHO
;;;;  v RFC 858 SUPPRESS-GO-AHEAD
;;;;    RFC 859 STATUS
;;;;    RFC 860 TIMING-MARK
;;;;    RFC 861 EXTENDED-OPTION-LIST
;;;;
;;;;    RFC 854, RFC 855, RFC 856, RFC 857, RFC 858. RFC 859, RFC 860, RFC 861
;;;;    
;;;;        The base telnet protocol specification. These describe the basic
;;;;        protocol, and the first set of supported sub-options.
;;;;    
;;;;  v RFC 885
;;;;    
;;;;        The End of Record option. I'm not sure what this is used for, but
;;;;        I suspect that the IBM 3270 telnet spec may use it. It also adds a
;;;;        new IAC escape sequence (EOR) to the set defined in RFC 854.
;;;;    
;;;;    RFC 927
;;;;    
;;;;        A way to send a user ID in order to avoid having to have a login
;;;;        prompt.
;;;;    
;;;;    RFC 933
;;;;    
;;;;        A way to send a piece of text flagged as a 'banner' that is
;;;;        constantly displayed on the screen. This is a DoD request so they
;;;;        can avoid constantly resending banners that label things as 'top
;;;;        secret'.
;;;;    
;;;;    RFC 1041
;;;;    
;;;;        This describes a method to handle 3270 terminals via telnet.
;;;;    
;;;;    RFC 1073
;;;;    
;;;;        This describes a way for telnet sessions to say something when the
;;;;        size of their display window changes.
;;;;    
;;;;    RFC 1079
;;;;    
;;;;        How to send information about what baud rate your connection is on
;;;;        so programs can make decisions based on this. emacs, for example,
;;;;        chooses and uglier, but much more efficient redraw method for
;;;;        lower baud rates.
;;;;    
;;;;    RFC 1091
;;;;    
;;;;        This describes a way for a server to query a client about terminal
;;;;        types supported by the client's display. One might believe that
;;;;        RFC 1408 obsoletes this, but this also describes a way for the
;;;;        client and server to negotiate to a mutually agreeable terminal
;;;;        type, which can't really be done using the protocol described in
;;;;        RFC 1408. For exchanging terminal type information, this protocol
;;;;        is preferred over RFC 1408.
;;;;    
;;;;    RFC 1096
;;;;    
;;;;        Describes a method by which a server can query a client about what
;;;;        X11 display it is on. This is definitely obsoleted by RFC
;;;;        1408. Future implementations need to udnerstand this mechanism of
;;;;        transferring X display information, but the protocol specififed in
;;;;        RFC 1408 is the preferred method.
;;;;    
;;;;    RFC 1097
;;;;    
;;;;        This describes a protocol that can be used to flag messages as
;;;;        being subliminal messages.
;;;;    
;;;; v  RFC 1143
;;;;    
;;;;        This describes, in detail, option negotion loop problems in the
;;;;        telnet protocol, and how to avoid them when writing a telnet
;;;;        implementation.
;;;;    
;;;;    RFC 1116, RFC 1184
;;;;    
;;;;        These two describe how to handle line-at-a-time mode. RFC 1184
;;;;        obsoletes RFC 1116, but linemode is complex, and having both RFCs
;;;;        would probably clarify things. Also, linemode adds several IAC
;;;;        escapes (EOF, SUSP, and ABORT) to the base set defined in RFC 854.
;;;;    
;;;;    RFC 1205
;;;;    
;;;;        So, you want to pretend to be an IBM 5250 terminal....
;;;;    
;;;;    RFC 1372
;;;;    
;;;;        Describes a protocol for handling flow control. This is mainly
;;;;        concerned with user-level flow control, like hitting Control-S to
;;;;        pause output.
;;;;    
;;;;    RFC 1408, RFC 1571
;;;;    
;;;;        Describes a protocol for sending environment variables from client
;;;;        to server in the telnet protocol. This protocol could be used
;;;;        instead of the terminal type (RFC 1091) and X11 display (RFC 1096)
;;;;        protocols.
;;;;    
;;;;    
;;;;    
;;;;        Both of these RFCs are mentioned because the reference
;;;;        implementation of RFC 1408 disagreed with the actual
;;;;        implementation. RFC 1571 describes a method for resolving the
;;;;        ambiguity. Because of this, the nearly identical protocol
;;;;        described in RFC 1572 is the preferred method of sending
;;;;        environment variables.
;;;;    
;;;;    RFC 1416
;;;;    
;;;;        Describes a protocol for sending authentication information back
;;;;        and forth between client and server. This protocol uses magic
;;;;        numbers defined in RFC 1700 to indicate which authentication
;;;;        method will be used, or is supported.
;;;;    
;;;;    RFC 1411
;;;;    
;;;;        Describes how to use the protocol described in RFC 1416 to do
;;;;        KERBEROS_V4 authentication.
;;;;    
;;;;    RFC 1572
;;;;    
;;;;        Describes an alternate environment variable passing protocol that
;;;;        fixes the ambiguities between the reference implementation, and
;;;;        the text of the spec in RFC 1408 and RFC 1571. This is the
;;;;        preferred environment variable passing protocol.
;;;;    
;;;;    RFC 2066
;;;;    
;;;;        Describes a protocol that can be used to negotiate a character set
;;;;        and input translations primarily for multi-lingual telnet.
;;;;    
;;;;    RFC 2217
;;;;    
;;;;        Describes a protocol that can be used to exchange RS232 parameters
;;;;        and modem control line state change information via telnet. This
;;;;        is so telnet can be used as an interface to a modem pool or
;;;;        terminal server.
;;;;    
;;;;    
;;;;    
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-18 <PJB> Created.
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

(in-package "COM.INFORMATIMAGO.COMMON-LISP.TELNET")


#-(and) "

TELNET is a symetrical protocol.

At both end lies a NVT, a Network Virtual Terminal.

The NVT can send and receive text (ASCII character codes and a few
ASCII control codes, or binary (notably for other encodings)), and can
send and receive option negotiation messages.


                                       +------------+
                                       | up:GSTREAM |
                                       +------------+
                                          ^     |
                                          |     |
                           +---------------+    |
                           | NVT-UP-SENDER |    |
                           +---------------+    |
                                          ^     |
                                          |     v
                               +--------------------------+
       options --------------->| NETWORK-VIRTUAL-TERMINAL |
                               +--------------------------+
                                  |       ^     |
                                  v       |     v             
                       +------------+     |   +-----------------+
                       | OPTION-MGR |*    |   | NVT-DOWN-SENDER |
                       +------------+     |   +-----------------+
                                          |     |
                                          |     v
                                      +--------------+
                                      | down:GSTREAM |
                                      +--------------+
                
                
Arrows represent method calls.

Typically, GSTREAM may be gray-streams, but that could be any kind of
object.  There's no class NVT-UP-SENDER or NVT-DOWN-SENDER, only a
protocol defined by a set of generic functions.

The bytes send by the up gstream to the NVT are transmitted (sometimes
after being buffered in the NVT) to the NVT-DOWN-SENDER.  The bytes
received from the down gstream are parsed for option messages,
updating in the NVT the set of configured options, and the text bytes
are forwared to the NVT-UP-SENDER.

Options can be queried and set by calling directly NVT methods.
Internally, the NVT object defers to option managers to manage
specific options subnegotiations.

"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public interfaces.
;;;


;; Conditions

(define-condition telnet-warning (warning)
  ((nvt              :initarg :nvt              :reader telnet-warning-nvt)
   (format-control   :initarg :format-control   :reader telnet-warning-format-control)
   (format-arguments :initarg :format-arguments :reader telnet-warning-format-arguments))
  (:report (lambda (condition stream)
             (format stream "Telnet Warning with NVT ~S: ~?"
                     (telnet-warning-nvt condition)
                     (telnet-warning-format-control condition)
                     (telnet-warning-format-arguments condition)))))


(define-condition telnet-option-warning (warning)
  ((option  :initarg :option  :reader telnet-option-warning-option :type option))
  (:report (lambda (condition stream)
             (format stream "Telnet Warning with NVT ~S, option ~A: ~?"
                     (telnet-warning-nvt condition)
                     (telnet-option-warning-option condition)
                     (telnet-warning-format-control condition)
                     (telnet-warning-format-arguments condition)))))


(define-condition telnet-error (error)
  ((nvt              :initarg :nvt              :reader telnet-error-nvt)
   (format-control   :initarg :format-control   :reader telnet-error-format-control)
   (format-arguments :initarg :format-arguments :reader telnet-error-format-arguments))
  (:report (lambda (condition stream)
             (if (and (slot-boundp condition 'format-control)
                      (slot-boundp condition 'format-arguments))
                 (format stream "Telnet Error with NVT ~S: ~?"
                         (telnet-error-nvt condition)
                         (telnet-error-format-control condition)
                         (telnet-error-format-arguments condition))
                 (format stream "Telnet Error with NVT ~S"
                         (telnet-error-nvt condition))))))


(define-condition telnet-option-error (telnet-error)
  ((option :initarg :option :reader telnet-option-error-option))
  (:report (lambda (condition stream)
             (if (and (slot-boundp condition 'format-control)
                      (slot-boundp condition 'format-arguments))
                 (format stream "Telnet Error with NVT ~S, option ~A: ~?"
                         (telnet-error-nvt condition)
                         (telnet-option-error-option condition)
                         (telnet-error-format-control condition)
                         (telnet-error-format-arguments condition))
                 (format stream "Telnet Error with NVT ~S, option ~A."
                         (telnet-error-nvt condition)
                         (telnet-option-error-option condition))))))


(define-condition telnet-invalid-option-name-error (telnet-error)
  ((option-name :initarg :option-name :reader telnet-invalid-option-name))
  (:report (lambda (condition stream)
             (format stream "Invalid option-name: ~S given to NVT ~S"
                     (telnet-invalid-option-name condition)
                     (telnet-error-nvt condition)))))


(define-condition telnet-invalid-control-error (telnet-error)
  ((control :initarg :control :reader telnet-invalid-control))
  (:report (lambda (condition stream)
             (format stream "Invalid control: ~S given to NVT ~S"
                     (telnet-invalid-control condition)
                     (telnet-error-nvt condition)))))

(define-condition telnet-unknown-command-error (telnet-error)
  ((command :initarg :command :reader telnet-unknown-command))
  (:report (lambda (condition stream)
             (format stream "Unknown comamnd ~S received by NVT ~S"
                     (telnet-unknown-command condition)
                     (telnet-error-nvt condition)))))


;; Up interface (from up):

(defgeneric send-binary  (nvt bytes)
  (:documentation "Send the binary text.
NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
BYTE: a VECTOR of (UNSIGNED-BYTE 8)."))

(defgeneric send-text    (nvt text)
  (:documentation "Send the ASCII text.
NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
TEXT: a string containing only printable ASCII characters and #\newline.")) 

(defgeneric send-control (nvt control)
  (:documentation "Send a function control code.
NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
CONTROL: (member :synch :are-you-there :abort-output :interrupt-process :go-ahead
                 :erase-line :erase-character
                 :break :cr :ff :vt :lf :ht :bs :bel :nul
                 :end-of-record)."))

;; Up interface (to up):

(defgeneric want-option-p (up-sender option-name)
  (:documentation "Asks the upper layer whether the option is wanted.
OPTION-NAME: a keyword denoting the option.")
  (:method (up-sender option-code)
    (declare (ignorable up-sender option-code))
    nil))

(defgeneric receive-option  (up-sender option value)
  (:documentation "Receive a result from an option request.
OPTION: the option instance.
VALUE:  a value the option sends back."))


(defgeneric receive-binary  (up-sender bytes &key start end)
  (:documentation "Receive some binary text.
BYTE:       a VECTOR of (UNSIGNED-BYTE 8).
START, END: bounding index designators of sequence.
            The defaults are for START 0 and for END nil."))

(defgeneric receive-text    (up-sender text)
  (:documentation "Receive some ASCII text
TEXT: a string containing only printable ASCII characters and #\newline."))

(defgeneric receive-control (up-sender control)
  (:documentation "Receive a function code.
CONTROL: (member :are-you-there :abort-output :interrupt-process :go-ahead
                 :erase-line :erase-character
                 :break :cr :ff :vt :lf :ht :bs :bel :nul
                 :end-of-record)."))



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Telnet protocol codes.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defconstant IAC  255
    "Interpret As Command.")

  (defconstant DONT 254
    "(option code) Indicates the demand that the other party stop
performing, or confirmation that you are no longer expecting the other
party to perform, the indicated option.")

  (defconstant DO   253
    "(option code) Indicates the request that the other party perform,
or confirmation that you are expecting the other party to perform,
the indicated option.")

  (defconstant WONT 252
    "(option code) Indicates the refusal to perform, or continue
performing, the indicated option.")

  (defconstant WILL 251
    "(option code) Indicates the desire to begin performing, or
confirmation that you are now performing, the indicated option.")

  (defconstant SB   250
    "(option code) Indicates that what follows is subnegotiation of the
indicated option.")

  (defconstant GA   249 "The Go Ahead signal.")
  (defconstant EL   248 "The Erase Line function.")
  (defconstant EC   247 "The Erase Character function.")
  (defconstant AYT  246 "The Are You There function.")
  (defconstant AO   245 "The Abort Output function.")
  (defconstant IP   244 "The Interrupt Process function.")
  (defconstant BRK  243 "The NVT character Break.")
  (defconstant DM   242
    "The data stream portion of a Synch. This should always be
accompanied by a TCP Urgent notification.")
  (defconstant NOP  241 "No operation.")
  (defconstant SE   240 "End of subnegotiation parameters.")
  (defconstant EOR  239 "end or record (IAC EOR)")
  (defconstant ABORT  238 "line-mode abort")
  (defconstant SUSP 237 "line-mode suspend")
  (defconstant EOF  236 "line-mode end-of-file")


  ;; (defparameter *iac-3*
  ;;   (list will wont do dont sb)
  ;;   "List of command codes taking an option code parameter.")


  ;; Telnet protocol options code
  ;; These ones all come from arpa/telnet.h

  (defconstant TRANSMIT-BINARY          0  "8-bit data path")
  (defconstant ECHO                     1  "echo")
  (defconstant RCP                      2  "prepare to reconnect")
  (defconstant SUPPRESS-GO-AHEAD        3  "suppress go ahead")
  (defconstant NAMS                     4  "approximate message size")
  (defconstant STATUS                   5  "give status")
  (defconstant TIMING-MARK              6  "timing mark")
  (defconstant RCTE                     7  "remote controlled transmission and echo")
  (defconstant NAOL                     8  "negotiate about output line width")
  (defconstant NAOP                     9  "negotiate about output page size")
  (defconstant NAOCRD                  10  "negotiate about CR disposition")
  (defconstant NAOHTS                  11  "negotiate about horizontal tabstops")
  (defconstant NAOHTD                  12  "negotiate about horizontal tab disposition")
  (defconstant NAOFFD                  13  "negotiate about formfeed disposition")
  (defconstant NAOVTS                  14  "negotiate about vertical tab stops")
  (defconstant NAOVTD                  15  "negotiate about vertical tab disposition")
  (defconstant NAOLFD                  16  "negotiate about output LF disposition")
  (defconstant XASCII                  17  "extended ascii character set")
  (defconstant LOGOUT                  18  "force logout")
  (defconstant BM                      19  "byte macro")
  (defconstant DET                     20  "data entry terminal")
  (defconstant SUPDUP                  21  "supdup protocol")
  (defconstant SUPDUPOUTPUT            22  "supdup output")
  (defconstant SNDLOC                  23  "send location")
  (defconstant TTYPE                   24  "terminal type")
  (defconstant END-OF-RECORD           25  "end or record (option)")
  (defconstant TUID                    26  "TACACS user identification")
  (defconstant OUTMRK                  27  "output marking")
  (defconstant TTYLOC                  28  "terminal location number")
  (defconstant VT3270REGIME            29  "3270 regime")
  (defconstant X3PAD                   30  "X.3 PAD")
  (defconstant NAWS                    31  "window size")
  (defconstant TSPEED                  32  "terminal speed")
  (defconstant LFLOW                   33  "remote flow control")
  (defconstant LINEMODE                34  "Linemode option")
  (defconstant XDISPLOC                35  "X Display Location")
  (defconstant OLD-ENVIRON             36  "Old - Environment variables")
  (defconstant AUTHENTICATION          37  "Authenticate")
  (defconstant ENCRYPT                 38  "Encryption option")
  (defconstant NEW-ENVIRON             39  "New - Environment variables")

  ;; the following ones come from
  ;; http://www.iana.org/assignments/telnet-options
  ;; Unfortunately, that document does not assign identifiers
  ;; to all of them, so we are making them up.

  (defconstant TN3270E                 40  "TN3270E")
  (defconstant XAUTH                   41  "XAUTH")
  (defconstant CHARSET                 42  "CHARSET")
  (defconstant RSP                     43  "Telnet Remote Serial Port")
  (defconstant COM-PORT-OPTION         44  "Com Port Control Option")
  (defconstant SUPPRESS-LOCAL-ECHO     45  "Telnet Suppress Local Echo")
  (defconstant TLS                     46  "Telnet Start TLS")
  (defconstant KERMIT                  47  "KERMIT")
  (defconstant SEND-URL                48  "SEND-URL")
  (defconstant FORWARD-X               49  "FORWARD-X")
  (defconstant PRAGMA-LOGON           138  "TELOPT PRAGMA LOGON")
  (defconstant SSPI-LOGON             139  "TELOPT SSPI LOGON")
  (defconstant PRAGMA-HEARTBEAT       140  "TELOPT PRAGMA HEARTBEAT")
  (defconstant EXTENDED-OPTION-LIST   255  "Extended-Options-List")
  (defconstant NOOPT                    0)


  ;; sub-option qualifiers

  (defconstant TQ-IS                    0 "option is... ")
  (defconstant TQ-SEND                  1 "send option ")
  (defconstant TQ-INFO                  2 "ENVIRON: informational version of IS ")
  (defconstant TQ-REPLY                 2 "AUTHENTICATION: client version of IS ")
  (defconstant TQ-NAME                  3 "AUTHENTICATION: client version of IS ")

  (defconstant FLOW-OFF                 0 "Disable remote flow control ")
  (defconstant FLOW-ON                  1 "Enable remote flow control ")
  (defconstant FLOW-RESTART-ANY         2 "Restart output on any char ")
  (defconstant FLOW-RESTART-XON         3 "Restart output only on XON ")

  ;; LINEMODE suboptions

  (defconstant MODE                     1)
  (defconstant FORWARD-MASK             2)
  (defconstant SLC                      3)

  (defconstant MODE-EDIT             #x01)
  (defconstant MODE-TRAPSIG          #x02)
  (defconstant MODE-ACK              #x04)
  (defconstant MODE-SOFT-TAB         #x08)
  (defconstant MODE-LIT-ECHO         #x10)

  (defconstant MODE-MASK             #x1f)

  ;; Not part of protocol, but needed to simplify things... 
  (defconstant MODE-FLOW            #x0100)
  (defconstant MODE-ECHO            #x0200)
  (defconstant MODE-INBIN           #x0400)
  (defconstant MODE-OUTBIN          #x0800)
  (defconstant MODE-FORCE           #x1000)

  (defconstant SLC-SYNCH                1)
  (defconstant SLC-BRK                  2)
  (defconstant SLC-IP                   3)
  (defconstant SLC-AO                   4)
  (defconstant SLC-AYT                  5)
  (defconstant SLC-EOR                  6)
  (defconstant SLC-ABORT                7)
  (defconstant SLC-EOF                  8)
  (defconstant SLC-SUSP                 9)
  (defconstant SLC-EC                  10)
  (defconstant SLC-EL                  11)
  (defconstant SLC-EW                  12)
  (defconstant SLC-RP                  13)
  (defconstant SLC-LNEXT               14)
  (defconstant SLC-XON                 15)
  (defconstant SLC-XOFF                16)
  (defconstant SLC-FORW1               17)
  (defconstant SLC-FORW2               18)

  (defconstant NSLC                    18)


  (defconstant SLC-NOSUPPORT            0)
  (defconstant SLC-CANTCHANGE           1)
  (defconstant SLC-VARIABLE             2)
  (defconstant SLC-DEFAULT              3)
  (defconstant SLC-LEVELBITS         #x03)

  (defconstant SLC-FUNC                 0)
  (defconstant SLC-FLAGS                1)
  (defconstant SLC-VALUE                2)

  (defconstant SLC-ACK               #x80)
  (defconstant SLC-FLUSHIN           #x40)
  (defconstant SLC-FLUSHOUT          #x20)

  (defconstant OLD-ENV-VAR              1)
  (defconstant OLD-ENV-VALUE            0)
  (defconstant ENV-VAR                  0)
  (defconstant ENV-VALUE                1)
  (defconstant ENV-ESC                  2)
  (defconstant ENV-USERVAR              3)



  ;; AUTHENTICATION suboptions

  ;;  Who is authenticating who ...

  (defconstant AUTH-WHO-CLIENT          0 "Client authenticating server ")
  (defconstant AUTH-WHO-SERVER          1 "Server authenticating client ")
  (defconstant AUTH-WHO-MASK            1)

  ;;  amount of authentication done

  (defconstant AUTH-HOW-ONE-WAY         0)
  (defconstant AUTH-HOW-MUTUAL          2)
  (defconstant AUTH-HOW-MASK            2)

  (defconstant AUTHTYPE-NULL            0)
  (defconstant AUTHTYPE-KERBEROS-V4     1)
  (defconstant AUTHTYPE-KERBEROS-V5     2)
  (defconstant AUTHTYPE-SPX             3)
  (defconstant AUTHTYPE-MINK            4)
  (defconstant AUTHTYPE-CNT             5)

  (defconstant AUTHTYPE-TEST           99)


  ;; ENCRYPTion suboptions

  (defconstant ENCRYPT-IS               0 "I pick encryption type ... ")
  (defconstant ENCRYPT-SUPPORT          1 "I support encryption types ... ")
  (defconstant ENCRYPT-REPLY            2 "Initial setup response ")
  (defconstant ENCRYPT-START            3 "Am starting to send encrypted ")
  (defconstant ENCRYPT-END              4 "Am ending encrypted ")
  (defconstant ENCRYPT-REQSTART         5 "Request you start encrypting ")
  (defconstant ENCRYPT-REQEND           6 "Request you send encrypting ")
  (defconstant ENCRYPT-ENC-KEYID        7)
  (defconstant ENCRYPT-DEC-KEYID        8)
  (defconstant ENCRYPT-CNT              9)

  (defconstant ENCTYPE-ANY              0)
  (defconstant ENCTYPE-DES-CFB64        1)
  (defconstant ENCTYPE-DES-OFB64        2)
  (defconstant ENCTYPE-CNT              3)

  );;eval-when


;; NVT codes
;; They are in COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII

;; (defconstant CR     13
;;   "")
;; 
;; (defconstant FF     12
;;   "Moves the printer to the top of the next page, keeping the same
;; horizontal position.")
;; 
;; (defconstant VT     11
;;   "Moves the printer to the next vertical tab stop.  It remains
;; unspecified how either party determines or establishes where such
;; tab stops are located.")
;; 
;; (defconstant LF     10
;;   "")
;; 
;; (defconstant HT     9
;;   "Moves the printer to the next horizontal tab stop. It remains
;; unspecified how either party determines or establishes where such
;; tab stops are located.")
;; 
;; (defconstant BS     8
;;   "Moves the print head one character position towards the left margin.")
;; 
;; (defconstant BEL    7
;;   "Produces an audible or visible signal (which does NOT move the print head).")
;; 
;; (defconstant NUL    0
;;   "")




(defparameter *option-name-map*
  `((:transmit-binary              ,transmit-binary)
    (:echo                         ,echo)
    (:rcp                          ,rcp)
    (:suppress-go-ahead            ,suppress-go-ahead)
    (:nams                         ,nams)
    (:status                       ,status)
    (:timing-mark                  ,timing-mark)
    (:rcte                         ,rcte)
    (:naol                         ,naol)
    (:naop                         ,naop)
    (:naocrd                       ,naocrd)
    (:naohts                       ,naohts)
    (:naohtd                       ,naohtd)
    (:naoffd                       ,naoffd)
    (:naovts                       ,naovts)
    (:naovtd                       ,naovtd)
    (:naolfd                       ,naolfd)
    (:xascii                       ,xascii)
    (:logout                       ,logout)
    (:bm                           ,bm)
    (:det                          ,det)
    (:supdup                       ,supdup)
    (:supdupoutput                 ,supdupoutput)
    (:sndloc                       ,sndloc)
    (:ttype                        ,ttype)
    (:end-of-record                ,end-of-record)
    (:tuid                         ,tuid)
    (:outmrk                       ,outmrk)
    (:ttyloc                       ,ttyloc)
    (:vt3270regime                 ,vt3270regime)
    (:x3pad                        ,x3pad)
    (:naws                         ,naws)
    (:tspeed                       ,tspeed)
    (:lflow                        ,lflow)
    (:linemode                     ,linemode)
    (:xdisploc                     ,xdisploc)
    (:old-environ                  ,old-environ)
    (:authentication               ,authentication)
    (:encrypt                      ,encrypt)
    (:new-environ                  ,new-environ)
    (:tn3270e                      ,tn3270e)
    (:xauth                        ,xauth)
    (:charset                      ,charset)
    (:rsp                          ,rsp)
    (:com-port-option              ,com-port-option)
    (:suppress-local-echo          ,suppress-local-echo)
    (:tls                          ,tls)
    (:kermit                       ,kermit)
    (:send-url                     ,send-url)
    (:forward-x                    ,forward-x)
    (:pragma-logon                 ,pragma-logon)
    (:sspi-logon                   ,sspi-logon)
    (:pragma-heartbeat             ,pragma-heartbeat)
    (:extended-option-list         ,extended-option-list)))


(defparameter *option-name-table*
  (hashtable :elements *option-name-map*)
  "Maps the option-name to option-code.")

(defparameter *option-code-table*
  (loop
    :with table = (make-array 256 :initial-element nil)
    :for (name code) :in *option-name-map*
    :do (setf (aref table code) name)
    :finally (return table))
  ;; NOTE: EXTENDED-OPTION-LIST may extend this array to 512 slots.
  "Maps the option-code to option-name.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RFC1143: The Q Method of Implementing TELNET Option Negotiation
;;;

(deftype telnet-action     () '(member :do :dont :will :wont))  
(deftype side-option-state () '(member :no :want-no :want-yes :yes))
(deftype side-option-queue () '(member :empty :opposite))
(deftype ubyte () '(unsigned-byte 8))


(defclass option ()
  ((code :initarg :code
         :reader option-code)
   (name :initarg :name)
   (#-sbcl us #+sbcl sbcl-has-a-bug-so-we-cannot-name-our-slot-us-see-|https://bugs.launchpad.net/sbcl/+bug/539540|
           :initform :no    :type side-option-state
           :accessor opt-us)
   (usq  :initform :empty :type side-option-queue
         :accessor opt-usq)
   (him  :initform :no    :type side-option-state
         :accessor opt-him)
   (himq :initform :empty :type side-option-queue
         :accessor opt-himq)))

(defgeneric option-name (option))

(defmethod option-name ((opt option))
  "RETURN: The option name if it has one, otherwise the option code."
  (if (slot-boundp opt 'name)
      (slot-value opt 'name)
      (option-code opt)))

(defun make-option (code name &optional (class 'option))
  (make-instance class :code code :name name))

(defmethod print-object ((self option) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~S (~A) :us ~A :usq ~A :him ~A :himq ~A"
            (option-name self)
            (option-code self)
            (opt-us self)
            (opt-usq self)
            (opt-him self)
            (opt-himq self)))
  self)



(define-condition telnet-protocol-error (telnet-error)
  ((option :initarg  :option  :reader telnet-protocol-error-option
           :type option)
   (action :initarg  :action  :reader telnet-protocol-error-action
           :type telnet-action)
   (message :initarg :message :reader telnet-protocol-error-message))
  (:report (lambda (condition stream)
             (format stream "Telnet Protocol Error on ~A option ~A: ~A"
                     (telnet-protocol-error-action condition)
                     (option-code (telnet-protocol-error-option condition))
                     (telnet-protocol-error-message condition)))))


(defgeneric opt-enabled-p (option &optional who)
  (:documentation "Indicate whether the OPTION is enabled,
if WHO is nil, then for either end, otherwise for the indicated end.
WHO:         (member nil :us :him).")
  (:method ((os option) &optional who)
    (ecase who
      ((nil)  (or (eq (opt-us os) :yes) (eq (opt-him os) :yes)))
      ((:us)  (eq (opt-us  os) :yes))
      ((:him) (eq (opt-him os) :yes)))))

(defgeneric want-do   (option nvt)
  (:documentation "Initiated locally, to enable the OPTION on the remote."))
(defgeneric want-dont (option nvt)
  (:documentation "Initiated locally, to disable the OPTION on the remote."))
(defgeneric want-will (option nvt)
  (:documentation "Initiated locally, to enable the OPTION locally."))
(defgeneric want-wont (option nvt)
  (:documentation "Initiated locally, to disable the OPTION locally."))

(defgeneric receive-do   (option nvt)
  (:documentation "Initiated remotely, to process a DO message."))
(defgeneric receive-dont (option nvt)
  (:documentation "Initiated remotely, to process a DONT message."))
(defgeneric receive-will (option nvt)
  (:documentation "Initiated remotely, to process a WILL message."))
(defgeneric receive-wont (option nvt)
  (:documentation "Initiated remotely, to process a WONT message."))

(defgeneric agree (nvt option-code)
  (:documentation "Whether the NVT agrees to enable the option.")
  (:method (nvt option-code) (declare (ignore nvt option-code)) nil))

(defgeneric send-do   (nvt option-code)
  (:documentation "Implemented by the NVT object to send a DO message."))
(defgeneric send-dont (nvt option-code)
  (:documentation "Implemented by the NVT object to send a DONT message."))
(defgeneric send-will (nvt option-code)
  (:documentation "Implemented by the NVT object to send a WILL message."))
(defgeneric send-wont (nvt option-code)
  (:documentation "Implemented by the NVT object to send a WONT message."))




(defmethod want-do ((os option) nvt)
  (case (opt-him os)
    ((:no)
     (setf (opt-him os) :want-yes)
     (send-do nvt (option-code os)))
    ((:yes)
     (error 'telnet-protocol-error :nvt nvt :option os :action :do :message "already enabled"))
    ((:want-no)
     (case (opt-himq os)
       ((:empty)
        (setf (opt-himq os) :opposite))
       ((:opposite)
        (error 'telnet-protocol-error :nvt nvt :option os :action :do :message "request is already enqueued"))))
    ((:want-yes)
     (case (opt-himq os)
       ((:empty)
        (error 'telnet-protocol-error :nvt nvt :option os :action :do :message "already negotiating"))
       ((:opposite)
        (setf (opt-himq os) :empty))))))


(defmethod want-dont ((os option) nvt)
  (case (opt-him os)
    ((:no)
     (error 'telnet-protocol-error :nvt nvt :option os :action :do :message "already disabled"))
    ((:yes)
     (setf (opt-him os) :want-no)
     (send-dont nvt (option-code os)))
    ((:want-no)
     (case (opt-himq os)
       ((:empty)
        (error 'telnet-protocol-error :nvt nvt :option os :action :dont :message "already negotiating"))
       ((:opposite)
        (setf (opt-himq os) :empty))))
    ((:want-yes)
     (case (opt-himq os)
       ((:empty)
        (setf (opt-himq os) :opposite))
       ((:opposite)
        (error 'telnet-protocol-error :nvt nvt :option os :action :dont :message "request is already enqueued "))))))



(defmethod want-will ((os option) nvt)
  (case (opt-us os)
    ((:no)
     (setf (opt-us os) :want-yes)
     (send-will nvt (option-code os)))
    ((:yes)
     (error 'telnet-protocol-error :nvt nvt :option os :action :will :message "already enabled"))
    ((:want-no)
     (case (opt-usq os)
       ((:empty)
        (setf (opt-usq os) :opposite))
       ((:opposite)
        (error 'telnet-protocol-error :nvt nvt :option os :action :will :message "request is already enqueued"))))
    ((:want-yes)
     (case (opt-usq os)
       ((:empty)
        (error 'telnet-protocol-error :nvt nvt :option os :action :will :message "already negotiating"))
       ((:opposite)
        (setf (opt-usq os) :empty))))))


(defmethod want-wont ((os option) nvt)
  (case (opt-us os)
    ((:no)
     (error 'telnet-protocol-error :nvt nvt :option os :action :will :message "already disabled"))
    ((:yes)
     (setf (opt-us os) :want-no)
     (send-wont nvt (option-code os)))
    ((:want-no)
     (case (opt-usq os)
       ((:empty)
        (error 'telnet-protocol-error :nvt nvt :option os :action :wont :message "already negotiating"))
       ((:opposite)
        (setf (opt-usq os) :empty))))
    ((:want-yes)
     (case (opt-usq os)
       ((:empty)
        (setf (opt-usq os) :opposite))
       ((:opposite)
        (error 'telnet-protocol-error :nvt nvt :option os :action :wont :message "request is already enqueued "))))))




(defmethod receive-will ((os option) nvt)
  (case (opt-him os)
    ((:no)
     (if (agree nvt (option-code os))
         (progn (setf (opt-him os) :yes)
                (send-do nvt (option-code os)))
         (send-dont nvt (option-code os))))
    ((:yes)
     #|ignore|#)
    ((:want-no)
     (warn "DONT ~A answered by WILL ~:*~A" (option-code os))
     (case (opt-himq os)
       ((:empty)
        (setf (opt-him os) :no))
       ((:opposite)
        (setf (opt-him os) :yes
              (opt-himq os) :empty))))
    ((:want-yes)
     (case (opt-himq os)
       ((:empty)
        (setf (opt-him os) :yes))
       ((:opposite)
        (setf (opt-him os) :want-no
              (opt-himq os) :empty)
        (send-dont nvt (option-code os)))))))


(defmethod receive-wont ((os option) nvt)
  (case (opt-him os)
    ((:no)
     #|ignore|#)
    ((:yes)
     (setf (opt-him os) :no)
     (send-dont nvt (option-code os)))
    ((:want-no)
     (case (opt-himq os)
       ((:empty)    (setf (opt-him os) :no))
       ((:opposite) (setf (opt-him os) :want-yes
                          (opt-himq os) :empty)
        (send-do nvt (option-code os)))))
    ((:want-yes)
     (case (opt-himq os)
       ((:empty)    (setf (opt-him os) :no))
       ((:opposite) (setf (opt-him os) :no
                          (opt-himq os) :empty))))))


(defmethod receive-do ((os option) nvt)
  (case (opt-us os)
    ((:no)
     (if (agree nvt (option-code os))
         (progn (setf (opt-us os) :yes)
                (send-will nvt (option-code os)))
         (send-wont nvt (option-code os))))
    ((:yes)
     #|ignore|#)
    ((:want-no)
     (warn "WONT ~A answered by DO ~:*~A" (option-code os))
     (case (opt-usq os)
       ((:empty)
        (setf (opt-us os) :no))
       ((:opposite)
        (setf (opt-us os) :yes
              (opt-usq os) :empty))))
    ((:want-yes)
     (case (opt-usq os)
       ((:empty)
        (setf (opt-us os) :yes))
       ((:opposite)
        (setf (opt-us os) :want-no
              (opt-usq os) :empty)
        (send-wont nvt (option-code os)))))))


(defmethod receive-dont ((os option) nvt)
  (case (opt-us os)
    ((:no)
     #|ignore|#)
    ((:yes)
     (setf (opt-us os) :no)
     (send-wont nvt (option-code os)))
    ((:want-no)
     (case (opt-usq os)
       ((:empty)    (setf (opt-us os) :no))
       ((:opposite) (setf (opt-us os) :want-yes
                          (opt-usq os) :empty)
        (send-will nvt (option-code os)))))
    ((:want-yes)
     (case (opt-usq os)
       ((:empty)    (setf (opt-us os) :no))
       ((:opposite) (setf (opt-us os) :no
                          (opt-usq os) :empty))))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUFFER
;;;
;;; The following functionnal abstraction specifies buffers, that are
;;; used to gather bytes and eat messages in FIFO order.
;;;
;;; This implementation moves eaten bytes down the vector, assuming
;;; there won't be a lot of remaining bytes to move.  If this
;;; assumption reveals itself false, then another implementation,
;;; could be written.  

(defun make-buffer (initial-size)
  (make-array initial-size
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defun buffer-length (buffer)
  (length buffer))

(defun buffer-ref (buffer index)
  (aref buffer index))

(defun buffer-subseq (buffer start end)
  (subseq buffer start end))

(defun buffer-search (subsequence buffer)
  (search subsequence buffer))

(defun buffer-clear (buffer)
  (setf (fill-pointer buffer) 0))

(declaim (inline make-buffer buffer-length buffer-ref buffer-subseq
                 buffer-search buffer-clear))

(defun buffer-delete-from-head (buffer size-to-remove)
  (replace buffer buffer :start2 size-to-remove)
  (decf (fill-pointer buffer) size-to-remove)
  buffer)

(defun buffer-append (buffer bytes start end)
  (let* ((old-size (length buffer))
         (new-size (+ old-size (- end start))))
    (loop
      :while (< (array-dimension buffer 0) new-size)
      :do (setf buffer (adjust-array buffer
                                     (* 2 (array-dimension buffer 0))
                                     :element-type (array-element-type buffer)
                                     :fill-pointer (fill-pointer buffer))))
    (setf (fill-pointer buffer) new-size)
    (replace buffer bytes :start1 old-size :start2 start :end2 end)
    buffer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NETWORK-VIRTUAL-TERMINAL
;;;

(defclass network-virtual-terminal ()
  ((name           :initform "NVT"
                   :initarg :name
                   :reader nvt-name)
   (client         :initform t :initarg :client :reader nvt-client-p
                   :documentation "
Whether the NVT is the client (ie. is on the side that has
initiated the connection).")
   (options        :initform (make-hash-table)
                   :documentation "
The set of options that have been negotiated;
keys are option-code, values are option-class or OPTION instances.")
   (up-sender      :initarg :up-sender
                   :reader up-sender
                   :reader nvt-up-sender
                   :documentation "
The object to which the bytes received from the remote NVT are forwarded.")
   (down-sender    :initarg :down-sender
                   :reader down-sender
                   :reader nvt-down-sender
                   :documentation "
The object to which the bytes to be sent to the remote NVT are given.")
   (send-wait-p    :initform nil
                   :accessor send-wait-p
                   :reader nvt-send-wait-p
                   :documentation "
Whether sending should be suspended (bytes are kept in the send-buffer).")
   (send-buffer    :initform (make-buffer 80)
                   :reader send-buffer
                   :documentation "
Bytes received from up, waiting to be sent to the remote NVT.")
   (receive-buffer :initform (make-buffer 80)
                   :reader receive-buffer
                   :documentation "
Bytes received from down, waiting to be parsed by the local NVT.")
   (urgent-mode-p  :initform nil
                   :accessor urgent-mode-p
                   :reader nvt-urgent-mode-p
                   :documentation "Urgent mode: we've received an urgent notification
and are discarding text bytes till the next IAC DM."))
  (:documentation "Represents a telnet end-point (both 'client' and 'server')."))

(defgeneric init-option-name (nvt option-name))
(defgeneric init-option-code (nvt option-code &optional option-name))
(defgeneric get-option (nvt option-name))

(defmethod print-object ((self network-virtual-terminal) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~S (~:[server~;client~]) ~@[waiting~]"
            (nvt-name self)
            (nvt-client-p self)
            (nvt-send-wait-p self)))
  self)

(defgeneric nvt-options (nvt))

(defmethod nvt-options ((nvt network-virtual-terminal))
  "RETURN: A fresh list of the current OPTION instance in the NVT."
  (remove-if (function symbolp) (hash-table-values (slot-value nvt 'options))))


(defun process-binary (bytes)
  "Escape IAC with IAC."
  (let ((iacnt (count IAC bytes)))
    (if (plusp iacnt)
        (loop
          :with out =  (make-array (+ (length bytes) iacnt)
                                   :element-type 'ubyte)
          :with j = -1
          :for byte :across bytes
          :do (if (= IAC byte)
                  (setf (aref out (incf j)) IAC
                        (aref out (incf j)) IAC)
                  (setf (aref out (incf j)) byte))
          :finally (return out))
        bytes)))


(defun process-text (text)
  "Convert to ASCII bytes."
  (ascii-bytes text :newline :crlf))


;; Up interface (from up):

;; TODO: When LINE-MODE we should keep in a buffer until an end of
;;       record (CRLF, EOR, FORW1 FORW2, etc) is sent.  On the other
;;       hand, this may be done by the terminal layer itself?
(defgeneric send-raw-bytes  (nvt  bytes))
(defgeneric send-urgent-notification  (nvt))

(defmethod send-raw-bytes  ((nvt network-virtual-terminal) bytes)
  "Send the binary bytes.
NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
BYTE: a VECTOR of (UNSIGNED-BYTE 8)."
  (if (send-wait-p nvt)
      (buffer-append (send-buffer nvt) bytes 0 (length bytes))
      (send (down-sender nvt) bytes)))


(defmethod send-urgent-notification ((nvt network-virtual-terminal))
  (send-urgent-notification (down-sender nvt)))


(defmethod send-binary  ((nvt network-virtual-terminal) bytes)
  "Send the binary text.
NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
BYTE: a VECTOR of (UNSIGNED-BYTE 8)."
  (let ((processed-bytes (process-binary bytes)))
    (send-raw-bytes nvt processed-bytes)))


(defmethod send-text    ((nvt network-virtual-terminal) text)
  "Send the ASCII text.
NOTE: To send other characters than printable ASCII characters, use SEND-BINARY.
NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
TEXT: a string containing only printable ASCII characters and #\newline."
  (let ((processed-bytes (process-text text)))
    (send-binary nvt processed-bytes))) 


(defmethod send-control ((nvt network-virtual-terminal) control)
  "Send a function control code.
NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
CONTROL: (member :synch :are-you-there :abort-output :interrupt-process :go-ahead
                 :erase-line :erase-character
                 :break :cr :ff :vt :lf :ht :bs :bel :nul
                 :end-of-record)."
  (cond
    ((eq control :synch)
     (send-raw-bytes nvt (vector IAC DM))
     (send-urgent-notification nvt))
    ((or (and (option-enabled-p nvt :suppress-go-ahead :us)
              (eq control :go-ahead))
         (and (option-enabled-p nvt :end-of-record :us)
              (eq control :end-of-record)))
     #|don't send it|#)
    (t
     (send-raw-bytes nvt (if (typep control 'ubyte)
                             (if (or (<= 0 control 31)
                                     (<= 127 control))
                                 (vector control)
                                 (error 'telnet-invalid-control-error
                                        :nvt nvt
                                        :control control))
                             (case control
                               (:erase-line         #(#.IAC #.EL))
                               (:erase-character    #(#.IAC #.EC))
                               (:are-you-there      #(#.IAC #.AYT))
                               (:abort-output       #(#.IAC #.AO))
                               (:interrupt-process  #(#.IAC #.IP))
                               (:go-ahead           #(#.IAC #.GA))
                               (:end-of-record      #(#.IAC #.EOR))
                               (:break              #(#.IAC #.BRK))
                               (:cr                 #(#.CR))
                               (:ff                 #(#.FF))
                               (:vt                 #(#.VT))
                               (:lf                 #(#.LF))
                               (:ht                 #(#.HT))
                               (:bs                 #(#.BS))
                               (:bel                #(#.BEL))
                               (:nul                #(#.NUL))
                               (otherwise
                                (error 'telnet-invalid-control-error
                                       :nvt nvt
                                       :control control))))))))


;; Down interface (from down):

(defgeneric receive-urgent-notification (nvt))

(defmethod receive-urgent-notification ((nvt network-virtual-terminal))
  (setf (urgent-mode-p nvt) t))


;; We process the input buffer avoiding to make any needless copy
;; (hence the use of START and END parameters).
;; We may modify this input buffer (eg. to remove IAC IAC).
;; However if a buffer contains part or several messages (a sequence
;; of text code, or a telnet command), we copy it to the
;; receive-buffer of the nvt.


(defun parse-message (buffer start end)
  "Return: (values kind len)
KIND: (member nil :bytes :iac :iac-sb) indicates the kind of data at the START.
LEN:  number of bytes from START that belong to the message."
  (if (= start end)
      (values nil 0) ; no data yet.
      (let ((i start)
            (iac-iac nil))
        (cond
          ((or (/= (aref buffer i) IAC)
               (and (< (1+ i) end) (= (aref buffer (1+ i)) IAC)))
           ;; A data byte.
           (loop
             (cond
               ((<= end i)
                (return-from parse-message (values :bytes (- i start) iac-iac)))
               ((/= (aref buffer i) IAC)
                (incf i))
               ((and (< (1+ i) end) (= (aref buffer (1+ i)) IAC))
                ;; IAC IAC
                (incf i 2)
                (setf iac-iac t))
               (t
                (return-from parse-message (values :bytes (- i start) iac-iac))))))
          ((or (<= end (1+ i))
               (and (<= end (+ 2 i))
                    (case (aref buffer (1+ i))
                      ((#.do #.dont #.will #.wont #.sb) t)
                      (otherwise nil))))
           ;; just IAC: wait for more data.
           (return-from parse-message (values nil 0)))
          ((= (aref buffer (1+ i)) SB)
           ;; IAC SB
           (incf i 3)
           (loop
             (cond
               ((<= end i)
                ;; wait for more data, we don't have IAC SE.
                (return-from parse-message (values nil 0)))
               ((/= (aref buffer i) IAC)
                (incf i))
               ((<= end (1+ i))
                ;; wait for more data, we don't have IAC SE.
                (return-from parse-message (values nil 0)))
               ((= (aref buffer (1+ i)) IAC)
                ;; IAC IAC
                (incf i 2))
               ((= (aref buffer (1+ i)) SE)
                ;; IAC SE
                (incf i 2)
                (return-from parse-message (values :iac-sb (- i start))))
               (t
                ;; Should not occur?
                (incf i 2)))))
           ;; IAC commands:
          ((let ((command (aref buffer (1+ i))))
             (or (= command WILL) (= command WONT)
                 (= command DO)   (= command DONT)))
           (return-from parse-message (values :iac 3)))
          (t 
           (return-from parse-message (values :iac 2)))))))



(defun remove-iac-iac (buffer start end)
  "
PRE:  (/= IAC (aref buffer (- end 1)))
      Actually: buffer contains no other occurence of IAC than IAC IAC pairs.
RETURN: the new lengnth. 
"
  (loop
    :with j = start
    :with i = start
    :while (< i end)
    :do (progn
          (when (= IAC (aref buffer i)) (incf i))
          (setf (aref buffer j) (aref buffer i))
          (incf j)
          (incf i))
    :finally (return (- j start))))



(defun get-next-chunk (bytes start end)
  "
Split the text into printable chunks and control chunks.
Control chunks contain only a single control code.
Printable chunks may contain CR-LF sequences.
BYTES:  contains telnet text from START to END. 
RETURN: (values :incomplete next) if the bytes are incomplete (CR alone).
        (values :text next) if the bytes contain only ASCII printable codes and CR-LF.
        (values :control next code) if the bytes contain a control code.
        (values :done end) when end≤start.
NEXT:   the index of the first unprocessed byte. (<= START NEXT END)
"
  (when (<= end start)
    (return-from get-next-chunk (values :done end)))
  (let ((i start))
    (loop
      (cond
        ((<= end i)
         (if (< start i)
             (return-from get-next-chunk (values :text i))
             (return)))
        ((ascii-printable-code-p (aref bytes i))
         (incf i))
        ((= cr (aref bytes i))
         (if (<= end (1+ i))
             (return-from get-next-chunk (values :incomplete i))
             (if (= lf (aref bytes (1+ i)))
                 ;; cr-lf goes to the string.
                 (incf i 2) 
                 ;; cr-nul, or cr alone is a CR control code.
                 (if (< start i)
                     (return-from get-next-chunk (values :text i))
                     (return)))))
        (t ;; control code.
         (if (< start i)
             (return-from get-next-chunk (values :text i))
             (return))))))
  ;; control code
  (if (and (= cr (aref bytes start))
           (< (1+ start) end)
           (= nul (aref bytes (1+ start))))
      (values :control (+ 2 start) cr)
      (values :control (1+ start)  (aref bytes start))))




(defun convert-control (code)
  (case code
    (#.NUL :nul)
    (#.BEL :bel)
    (#.BS  :bs)
    (#.HT  :ht)
    (#.LF  :lf)
    (#.VT  :vt)
    (#.FF  :ff)
    (#.CR  :cr)
    (otherwise nil)))

(defgeneric dispatch-message (nvt bytes start end))
(defmethod dispatch-message ((nvt network-virtual-terminal) bytes start end)
  "
RETURN: the length of bytes processed.
"
  ;; if (urgent-mode-p nvt) discard text till next IAC DM.
  ;; interesting signals: IP AO AYT (not EC EL), other IAC.
  ;; if (option-enabled-p nvt :echo) the echo back the
  ;; text to the remote.
  
  (multiple-value-bind (kind len iac-iac) (parse-message bytes start end)
    (case kind
      ((nil)
       len)
      (:bytes
       (when (option-enabled-p nvt :echo :us)
         (send (down-sender nvt) bytes start (+ start len)))
       (if (urgent-mode-p nvt)
           ;; ignore text bytes
           len
           (let* ((newlen (if iac-iac
                              (remove-iac-iac bytes start (+ start len))
                              len))
                  (newend (+ start newlen)))
             (if (option-enabled-p nvt :transmit-binary :him)
                 (progn
                   (receive-binary (up-sender nvt) bytes :start start :end newend)
                   len)
                 (loop
                   :with processed = start
                   :do (multiple-value-bind (kind next code) (get-next-chunk bytes processed newend)
                         (ecase kind
                           ((:done :incomplete)
                            (when (< newend end)
                              (replace bytes bytes
                                       :start1 (+ processed (- end newend))
                                       :end1 newend
                                       :start2 processed
                                       :end2 end))
                            (return (- (+ processed (- end newend)) start)))
                           ((:text)
                            (receive-text    (up-sender nvt) (ascii-string bytes :newline :crlf :start processed :end newend)))
                           ((:control)
                            (let ((control (convert-control code)))
                              (when control
                                (receive-control (up-sender nvt) control)))))
                         (setf processed next)))))))
      (:iac
       (case (aref bytes (1+ start))
         (#.DONT   (receive-dont (init-option-code nvt (aref bytes (+ 2 start))) nvt))
         (#.DO     (receive-do   (init-option-code nvt (aref bytes (+ 2 start))) nvt))
         (#.WONT   (receive-wont (init-option-code nvt (aref bytes (+ 2 start))) nvt))
         (#.WILL   (receive-will (init-option-code nvt (aref bytes (+ 2 start))) nvt))
         (#.AYT    (receive-control (up-sender nvt) :are-you-there))
         (#.AO     (receive-control (up-sender nvt) :abort-output))
         (#.IP     (receive-control (up-sender nvt) :interrupt-process))
         (#.GA     (unless (option-enabled-p nvt :suppress-go-ahead :him)
                     (receive-control (up-sender nvt) :go-ahead)))
         (#.DM     (if (urgent-mode-p nvt)
                       (setf (urgent-mode-p nvt) nil)
                       #|ignored in non-urgent mode|#))          
         (#.EL     (unless (urgent-mode-p nvt) (receive-control (up-sender nvt) :erase-line)))
         (#.EC     (unless (urgent-mode-p nvt) (receive-control (up-sender nvt) :erase-character)))
         (#.BRK    (unless (urgent-mode-p nvt) (receive-control (up-sender nvt) :break)))
         (#.EOR    (unless (urgent-mode-p nvt)
                     ;; TODO: Do we need to echo EOR?
                     ;; TODO: Can we echo EOR without having negotiated it?
                     (when (option-enabled-p nvt :echo :us)
                       (send (down-sender nvt) bytes start (+ start len)))
                     (when (option-enabled-p nvt :end-of-record :him)
                       (receive-control (up-sender nvt) :end-of-record))))
         (#.IAC    (unless (urgent-mode-p nvt)
                     ;; note: IAC IAC should be processed in :bytes
                     ;; this is just in case of a change in parse-message.
                     (when (option-enabled-p nvt :echo :us)
                       (send (down-sender nvt) bytes start (+ start len)))
                     (receive-binary (up-sender nvt) iac)))
         (#.NOP)         
         (otherwise (cerror "Ignore the command."
                            'telnet-unknown-command-error
                            :nvt nvt
                            :command (aref bytes (1+ start)))))
       len)
      (:iac-sb
       (let ((opt (init-option-code nvt (aref bytes (+ 2 start)))))
         (receive-subnegotiation opt nvt bytes :start start :end len))
       len))))


(defmethod receive ((nvt network-virtual-terminal) bytes &key (start 0) (end (length bytes)))
  "Receive bytes from the remote NVT.
NOTE: this may modify the BYTES vector between START and END, eg. in presence of IAC IAC.
NVT:  a NETWORK-VIRTUAL-TERMINAL instance.
BYTE: a VECTOR of (UNSIGNED-BYTE 8)."
  (when (< start end)
    (let ((buf (receive-buffer nvt)))
      (if (plusp (buffer-length buf))
          ;; We already have bytes in the buffer.
          ;; Append the new one and try to parse the buffer.
          (progn
            (buffer-append buf bytes start end)
            (loop
              (let ((processed (dispatch-message nvt buf 0 (buffer-length buf))))
                (incf start processed)
                (if (plusp processed)
                    (buffer-delete-from-head buf processed)
                    (return)))))
          ;; The buffer is empty, try to parse the bytes directly.
          (loop
            :while (< start end)
            :do (let ((processed (dispatch-message nvt bytes start end)))
                  (incf start processed)
                  (when (zerop processed)
                    (buffer-append buf bytes start end)
                    (return))))))))


;; Down interface (to down):

(defmethod send-do   ((nvt network-virtual-terminal) option-code)
  "Implemented by the REMOTE object to send a DO message."
  (send (down-sender nvt) (vector IAC DO option-code)))

(defmethod send-dont ((nvt network-virtual-terminal) option-code)
  "Implemented by the REMOTE object to send a DONT message."
  (send (down-sender nvt) (vector IAC DONT option-code)))

(defmethod send-will ((nvt network-virtual-terminal) option-code)
  "Implemented by the REMOTE object to send a WILL message."
  (send (down-sender nvt) (vector IAC WILL option-code)))

(defmethod send-wont ((nvt network-virtual-terminal) option-code)
  "Implemented by the REMOTE object to send a WONT message."
  (send (down-sender nvt) (vector IAC WONT option-code)))


;; Interface for the STATUS option:

(defgeneric decode-subnegotiation (option byte &key start end)
  (:documentation "Returns a sexp describing the STATUS SB (subseq byte start end).

This vector starts with SB option-code and ends with SE.
Any data byte equal to SE or IAC is be duplicated.
The returned sexp must start with (:SB option-name …).

\(Used by RECEIVE-STATUS of class STATUS to decode the SB statuses.)
")
  (:method ((opt option) byte &key (start 0) (end (length byte)))
    (declare (ignorable byte start end)) ; for some implementation byte is not used …
    (cerror "Ignore the subnegotiation status."
            'telnet-option-error
            ;; :nvt nvt ;; TODO: Do we need it? Should we keep the nvt in a dynamic variable?
            :option opt
            :format-control "Option STATUS received an unknown subnegotiation status for option ~:@(~A~)."
            :format-arguments (list (option-name opt)))
    ;; Let's return nothing instead of a (:sb …) list.
    (values)))


(defgeneric encode-subnegotiation (option buffer)
  (:documentation "
If the OPTION has any subnegotiated status, it should encode them in
the buffer.  Each subnegotiated status should start with SB and end
with SE;  Any data byte equal to SE or IAC should be duplicated.

    SB OPTION-CODE … SE

BUFFER: An adjustable vector with a fill-pointer.  

\(Used by SEND-STATUS of class STATUS to encode the SB statuses.)
")
  (:method ((opt option) buffer)
    ;; nothing to add.
    buffer))


;; Option control:

(defun option-name-for-code (option-code)
  (and (array-in-bounds-p *option-code-table* option-code)
       (aref *option-code-table* option-code)))


(defun option-code-for-name (option-name)
  (typecase option-name
    (keyword (gethash option-name *option-name-table*))
    ((integer 0 511)  option-name)
    (t                nil)))


(defmethod agree ((nvt network-virtual-terminal) option-code)
  "Whether the NVT agrees to enable the option."
  (let ((option-name (option-name-for-code option-code)))
    (when option-name
      (want-option-p (up-sender nvt) option-name))))


(defmethod init-option-code ((nvt network-virtual-terminal) option-code &optional option-name)
  (let ((opt (gethash option-code (slot-value nvt 'options)))
        (option-name (or option-name (option-name-for-code option-code))))
    (typecase opt
      (option
        opt)
      ((or symbol class)
       (setf (gethash option-code (slot-value nvt 'options))
             (make-option option-code option-name opt)))
      (t
       (setf (gethash option-code (slot-value nvt 'options))
             (make-option option-code option-name))))))

(defmethod init-option-name ((nvt network-virtual-terminal) option-name)
  (let ((code (option-code-for-name option-name)))
    (if code
        (init-option-code nvt code option-name)
        (error 'telnet-invalid-option-name-error
               :nvt nvt
               :option-name option-name))))


(defmethod get-option ((nvt network-virtual-terminal) option-name)
  "
OPTION-NAME: a keyword or fixnum denoting the option.
RETURN: the OPTION instance named OPTION-NAME, if it has been
        initialized (with ENABLE-OPTION or DISABLE-OPTION), or NIL
        otherwise.
"
  (let ((code (option-code-for-name option-name)))
    (if code
        (let ((opt (gethash code (slot-value nvt 'options))))
          (typecase opt
            (option opt)
            (t      nil)))
        (error 'telnet-invalid-option-name-error
               :nvt nvt
               :option-name option-name))))


(defmethod option-enabled-p ((nvt network-virtual-terminal) (option-name t) &optional who)
  "Whether the option is currently enabled.
OPTION-NAME: a keyword or fixnum denoting the option."
  (let ((opt (get-option nvt option-name)))
    (and opt (opt-enabled-p opt who))))

(defmethod option-enabled-p ((nvt network-virtual-terminal) (option option) &optional who)
  "Whether the option is currently enabled.
OPTION: an OPTION instance"
   (opt-enabled-p option who))


(defmethod option-negotiating-p ((nvt network-virtual-terminal) option-name &optional who)
  "Whether the option is currently being negotiated."
  (let ((opt (get-option nvt option-name)))
    (and opt
         (ecase who
           ((nil) (and (not (member (opt-us  opt) '(:no :yes)))
                       (not (member (opt-him opt) '(:no :yes)))))
           ((:us)  (not (member (opt-us  opt) '(:no :yes))))
           ((:him) (not (member (opt-him opt) '(:no :yes))))))))

(defmethod enable-option    ((nvt network-virtual-terminal) option-name &optional who)
  "Initiate the negotiation to enable the option.
OPTION-NAME: a keyword or fixnum denoting the option.
RETURN: The OPTION instance." 
  (let ((opt (init-option-name nvt option-name)))
    (ecase who
      ((nil)  (want-do opt nvt) (want-will opt nvt))
      ((:us)  (want-will opt nvt))
      ((:him) (want-do opt nvt)))
    opt))

(defmethod disable-option   ((nvt network-virtual-terminal) option-name &optional who)
  "Initiate the negotiation to disable the option.
OPTION-NAME: a keyword or fixnum denoting the option.
RETURN: The OPTION instance."
  (let ((opt (init-option-name nvt option-name)))
    (ecase who
      ((nil)  (want-dont opt nvt) (want-wont opt nvt))
      ((:us)  (want-wont opt nvt))
      ((:him) (want-dont opt nvt)))
    opt))



(defmethod option-register-class ((nvt network-virtual-terminal) option-name option-class)
   "Register OPTION-CLASS as the class for a given OPTION-NAME.

NOTE:         If the option is already initialized with a different
              class, then CHANGE-CLASS is called on the OPTION
              instance.

OPTION-NAME:  a keyword or fixnum denoting an option.
OPTION-CLASS: a class designator."
   (let ((class (etypecase option-class
                  (symbol (find-class option-class))
                  (class  option-class)))
         (code (option-code-for-name option-name)))
     (if code
         (let ((opt (gethash code (slot-value nvt 'options))))
           (typecase opt
             (option (change-class opt class))
             (t (setf (gethash code (slot-value nvt 'options)) class))))
        (error 'telnet-invalid-option-name-error
               :nvt nvt
               :option-name option-name))))


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


(defmethod option-register-default-classes ((nvt network-virtual-terminal) option-names)
  (let ((in-default-classes '()))
    (dolist (option-name option-names in-default-classes)
      (let* ((code        (option-code-for-name option-name))
             (option-name (aref *option-code-table* code))
             (entry       (assoc option-name *default-classes*)))
        (when entry
          (push option-name in-default-classes)
          (option-register-class nvt option-name (cdr entry)))))))


(defmethod initialize-instance ((nvt network-virtual-terminal) &key &allow-other-keys)
  (call-next-method)
  (option-register-default-classes nvt (mapcar (function car) *default-classes*))
  nvt)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
