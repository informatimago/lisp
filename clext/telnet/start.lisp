(push :debug-condition-variables *features*)
;; (push :debug-trace-telnet *features*)

(ql:quickload :com.informatimago.clext.telnet.repl.test)


(in-package "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM")
#+debug-trace-telnet
(trace %input-buffer-free-space
       %input-buffer-length
       %input-buffer-ranges
       %stream-read-char
       %wait-for-input-data-present
       %wait-for-input-free-space
       (setf stream-echo-mode)
       (setf stream-element-type)
       (setf stream-external-format)
       (setf stream-input-buffering)
       (setf stream-output-buffering)
       buffer-append
       call-with-telnet-on-stream
       check-sequence-arguments
       check-stream-open
       configure-mode
       encode-string-to-output-buffer
       flush-output-buffer
       input-buffer-append-octet
       input-buffer-append-octets
       input-buffer-append-text
       input-buffer-erase-character
       input-buffer-erase-line
       input-buffer-fetch-octet
       input-buffer-peek-octet
       input-buffer-read-octet
       input-loop
       make-binary-buffer
       make-input-buffer
       make-string-buffer
       receive-binary
       receive-control
       receive-option
       receive-text
       send
       send-binary
       send-control
       send-text

       type-equal-p
       update-column
       want-option-p)


#+debug-trace-telnet
(trace (stream-read-char         :if (typep (first ccl::args) 'telnet-stream))
       (stream-read-char-no-hang :if (typep (first ccl::args) 'telnet-stream))
       (stream-peek-char         :if (typep (first ccl::args) 'telnet-stream))
       (stream-read-line         :if (typep (first ccl::args) 'telnet-stream))
       (stream-listen            :if (typep (first ccl::args) 'telnet-stream))
       (stream-unread-char       :if (typep (first ccl::args) 'telnet-stream))
       (stream-read-byte         :if (typep (first ccl::args) 'telnet-stream))
       (stream-read-sequence     :if (typep (first ccl::args) 'telnet-stream))
       (stream-advance-to-column :if (typep (first ccl::args) 'telnet-stream))
       (stream-finish-output     :if (typep (first ccl::args) 'telnet-stream))
       (stream-line-column       :if (typep (first ccl::args) 'telnet-stream))
       (stream-start-line-p      :if (typep (first ccl::args) 'telnet-stream))
       (stream-terpri            :if (typep (first ccl::args) 'telnet-stream))
       (stream-write-byte        :if (typep (first ccl::args) 'telnet-stream))
       (stream-write-char        :if (typep (first ccl::args) 'telnet-stream))
       (stream-write-sequence    :if (typep (first ccl::args) 'telnet-stream))
       (stream-write-string      :if (typep (first ccl::args) 'telnet-stream)))



(in-package "COM.INFORMATIMAGO.CLEXT.TELNET.REPL")
#+debug-trace-telnet
(trace make-repl-readtable
       make-repl-package
       telnet-repl
       run-client-loop
       %clean-up
       %add-client
       remove-client
       wait-for-free-client-slot
       run-server-loop
       start-repl-server
       stop-repl-server)



(in-package "COM.INFORMATIMAGO.CLEXT.TELNET.REPL")

(defparameter *repl-server* (com.informatimago.clext.telnet.repl:start-repl-server
                                   :banner-function (lambda (stream cn name)
                                                      (format stream "~&REPL SERVER #~A ~A~%" cn name)
                                                      (force-output stream))
                                   :login-function nil))

(com.informatimago.tools.thread:list-threads)

