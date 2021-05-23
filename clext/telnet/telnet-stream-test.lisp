(defpackage "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM.TEST"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS"
        "TRIVIAL-GRAY-STREAMS"
        "COM.INFORMATIMAGO.COMMON-LISP.TELNET"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.CLEXT.CHARACTER-SETS"
        "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION"
        "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM"
        "COM.INFORMATIMAGO.CLEXT.TELNET.TEST.STUB-NVT")
  (:import-from "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM"
                "MAKE-BINARY-BUFFER" "NVT" "OUTPUT-BUFFER"
                "ENCODE-STRING-TO-OUTPUT-BUFFER"
                )
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM.TEST")

(define-test test/replace-octets-by-string ()
  (let ((buffer (make-binary-buffer 100)))
    (let* ((string "Hello World")
           (bytes (map 'vector 'ascii-code string)))

      (setf (fill-pointer buffer) (array-dimension buffer 0))
      (multiple-value-bind (filledp size)
          (replace-octets-by-string buffer string
                                    :start1 0
                                    :encoding :us-ascii)

        (assert-true filledp (buffer string :us-ascii)
                     "~S could not fill the buffer."
                     'replace-octets-by-string)

        (assert-true (= (length string) size)
                     (buffer string :us-ascii)
                     "~S didn't filled only ~D bytes instead of expected ~D bytes."
                     'replace-octets-by-string
                     size
                     (length string))

        (assert-true (= size (mismatch buffer bytes :end1 size)))

        (setf (fill-pointer buffer) size)
        (check equalp buffer bytes)))))


(define-test test/encode-string-to-output-buffer ()
  (let* ((stream (make-instance 'telnet-stream
                                :element-type 'character))
         (nvt    (make-instance 'stub-nvt
                                :name "STUB NVT"
                                :client nil
                                :up-sender stream
                                :down-sender nil)))
    (setf (slot-value stream 'nvt) nvt)
    (let* ((buffer (output-buffer stream))
           (s1 "Hello ")
           (s2 "World!")
           (s1+s2 (concatenate 'string s1 s2))
           (bytes (map 'vector 'ascii-code s1+s2)))
      (check = (fill-pointer buffer) 0 (buffer))
      (encode-string-to-output-buffer stream s1)
      (check = (fill-pointer buffer) (length s1) (buffer))
      (encode-string-to-output-buffer stream s2)
      (check = (fill-pointer buffer) (length s1+s2) (buffer))
      (check equalp buffer bytes (buffer bytes)))))


(define-test test/all ()
  (test/replace-octets-by-string)
  (test/encode-string-to-output-buffer))

(test/all)
