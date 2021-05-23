(defpackage "COM.INFORMATIMAGO.CLEXT.TELNET.TEST.STUB-NVT"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.TELNET")
  (:export "STUB-NVT"))
(in-package "COM.INFORMATIMAGO.CLEXT.TELNET.TEST.STUB-NVT")

(defclass stub-nvt ()
  ((urgent-mode-p  :initform nil
                   :accessor urgent-mode-p
                   :reader nvt-urgent-mode-p
                   :documentation "Urgent mode: we've received an urgent notification
and are discarding text bytes till the next IAC DM.")
   (ascii-decoder-enabled-p :initform t
                            :accessor ascii-decoder-enabled-p
                            :documentation "Whether received text messages are decoded from ASCII.")))

(defmethod initialize-instance :before ((nvt stub-nvt) &key &allow-other-keys))

(defmethod send-binary  ((nvt stub-nvt) bytes)
  (format *trace-output* "~&~S ~S ~S~%"
          'stub-nvt 'send-binary bytes))

(defmethod send-text    ((nvt stub-nvt) text)
  (format *trace-output* "~&~S ~S ~S~%"
          'stub-nvt 'send-text text))

(defmethod send-control ((nvt stub-nvt) control)
  (format *trace-output* "~&~S ~S ~S~%"
          'stub-nvt 'send-control control))

(defmethod ascii-decoder-enabled-p :before ((nvt stub-nvt))
  (format *trace-output* "~&~S ~S -> ~S~%"
          'stub-nvt 'ascii-decoder-enabled-p (slot-value nvt 'ascii-decoder-enable-p)))

(defmethod (setf ascii-decoder-enabled-p) :before (flag (nvt stub-nvt))
  (format *trace-output* "~&~S ~S ~S~%"
          'stub-nvt '(setf ascii-decoder-enabled-p) flag))


(defmethod receive ((nvt stub-nvt) bytes &key start end)
  (format *trace-output* "~&~S ~S ~S :start ~S :end ~S~%"
          'stub-nvt 'receive bytes start end))


;; ;; option control:
;;
;; (defmethod option-enabled-p ((nvt stub-nvt) option-name &optional who)
;;   (:documentation "Whether the option is currently enabled,
;; if WHO is nil, then for either end, otherwise for the indicated end.
;; OPTION-NAME: a keyword or fixnum  denoting the option.
;; WHO:         (member nil :us :him)."))
;;
;; (defmethod option-negotiating-p ((nvt stub-nvt) option-name &optional who)
;;   (:documentation "Whether the option is currently being negotiated,
;; if WHO is nil, then for either end, otherwise for the indicated end.
;; OPTION-NAME: a keyword or fixnum  denoting the option.
;; WHO:         (member nil :us :him)."))
;;
;; (defmethod enable-option    ((nvt stub-nvt) option-name &optional who)
;;   (:documentation "Initiate the negotiation to enable the option.
;; OPTION-NAME: a keyword or fixnum  denoting the option.
;; WHO:         (member nil :us :him)."))
;;
;; (defmethod disable-option   ((nvt stub-nvt) option-name &optional who)
;;   (:documentation "Initiate the negotiation to disable the option.
;; OPTION-NAME: a keyword or fixnum  denoting the option.
;; WHO:         (member nil :us :him)."))
;;
;;
;; (defun (setf option-enabled-p) (flag (nvt stub-nvt) option-name &optional who)
;;   "Enable or disable the option according to the boolean FLAG.
;; OPTION-NAME: a keyword or fixnum denoting an option."
;;   (if flag
;;       (enable-option  (nvt stub-nvt) option-name who)
;;       (disable-option (nvt stub-nvt) option-name who)))
;;
;;
;;
;; (defmethod option-register-class ((nvt stub-nvt) option-name option-class)
;;   (:documentation "Register OPTION-CLASS as the class for a given OPTION-NAME.
;; NOTE:         If the option is already initialized with a different
;;               class, then CHANGE-CLASS is called on the instance.
;; OPTION-NAME:  a keyword or fixnum denoting an option.
;; OPTION-CLASS: a class designator, should be a subclass of OPTION."))
;;
;;
;; (defmethod option-register-default-classes ((nvt stub-nvt) option-names)
;;   (:documentation "Register the default option-classes for the option given in OPTION-NAMES.
;; NOTE:         If the option is already initialized with a different
;;               class, then CHANGE-CLASS is called on the instance.
;; OPTION-NAMES: a list of keyword or fixnum denoting options.
;; RETURN:       The subset of OPTION-NAMES (codes are converted into
;;               option-names) for which a specific default class
;;               exists."))
;;
;;
;; ;; Implemented by subclasses of OPTION:
;;
;; (defmethod receive-subnegotiation (option (nvt stub-nvt) bytes &key start end)
;;   (:documentation "Processes the subnegotiation packet (subseq bytes start end)
;; starting with IAC SB and ending with IAC SE."))
