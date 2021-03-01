(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.IRCLOG"
  (:use "COMMON-LISP")
  (:documentation "This package fetches new lines from irclogs.")
  (:export "GET-NEW-MESSAGES"
           "*IRCLOG-BASE-URL*"
           "*CHANNELS*"
           "*IGNORE-COMMANDS*"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.IRCLOG")

(defvar *irclog-base-url* "https://ccl.clozure.com/irc-logs/")
(defvar *channels* '("lisp") #|'("lisp" "scheme")|#
  "A list of channels to fetch from the irclog server.")
(defparameter *ignore-commands* '("joined" "left" "quit"))

(defun log-url (channel)
  ;; https://ccl.clozure.com/irc-logs/lisp/lisp-2020-10.txt
  (multiple-value-bind (se mi ho da month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore se mi ho da))
    (format nil "~A~A/~:*~A-~4,'0D-~2,'0D.txt"
            *irclog-base-url* channel year month)))

(defclass cached-resource ()
  ((url             :initarg  :url :reader   cached-resource-url)
   (previous-length :initform 0    :accessor cached-resource-previous-length)
   (contents        :initform nil  :accessor cached-resource-contents)
   (headers         :initform '()  :accessor cached-resource-headers)
   (last-modified   :initform 0    :accessor cached-resource-last-modified)))

(defmethod fetch-resource ((resource cached-resource))
  (multiple-value-bind (contents status headers uri stream do-close reason)
      (drakma:http-request (cached-resource-url resource)
                           :external-format-in :latin-1)
    (declare (ignore uri))
    (unwind-protect
         (if (= status 200)
             (setf (cached-resource-previous-length resource) (length (cached-resource-contents resource))
                   (cached-resource-contents resource) contents
                   (cached-resource-headers  resource) headers)
             (error "Could not fetch the resource ~S for ~D ~A~%"
                    (cached-resource-url resource) status reason))
      (when do-close (close stream)))))

(defvar *cached-resources* nil
  "maps the channel to the cached resources.")

(defun initialize-cached-resources ()
  (let ((table (make-hash-table :test (function equal))))
    (dolist (channel *channels* (setf *cached-resources* table))
      (fetch-resource
       (setf (gethash channel table)
             (make-instance 'cached-resource
                            :url (log-url channel)))))))

(defun third-word (line)
  (ignore-errors
   (let* ((start (1+ (position #\space line :start (1+ (position #\space line)))))
          (end   (position #\space line :start start)))
     (subseq line start end))))

(defun get-new-messages ()
  (unless *cached-resources*
    (setf *cached-resources* (initialize-cached-resources)))
  (mapcan (lambda (channel)
            (let ((resource (gethash channel *cached-resources*)))
              (fetch-resource resource)
              (let ((lines (subseq (cached-resource-contents resource) (cached-resource-previous-length resource))))
                (when (plusp (length lines))
                  (let ((messages (remove-if
                                   (lambda (line)
                                     (or (zerop (length line))
                                         (member (third-word line) *ignore-commands*
                                                 :test (function equal))))
                                   (split-sequence:split-sequence #\newline lines))))
                    (when messages (list (list channel messages))))))))
          *channels*))

;;;; THE END ;;;;
