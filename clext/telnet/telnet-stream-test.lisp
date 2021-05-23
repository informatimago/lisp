(defpackage "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM.TEST"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS"
        "TRIVIAL-GRAY-STREAMS"
        "COM.INFORMATIMAGO.COMMON-LISP.TELNET"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.CLEXT.CHARACTER-SETS"
        "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION"
        "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM"
        "COM.INFORMATIMAGO.CLEXT.TELNET.TEST.STUB-NVT")
  (:import-from "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM"
                "OCTET"
                "MAKE-BINARY-BUFFER" "NVT"
                "OUTPUT-BUFFER" "ENCODE-STRING-TO-OUTPUT-BUFFER"
                "INPUT-BUFFER"  "INPUT-BUFFER-APPEND-OCTETS"
                "INPUT-BUFFER-READ-OCTET"
                "+INPUT-BUFFER-SIZE+"
                "INPUT-BUFFER-DATA"
                "INPUT-BUFFER-HEAD"
                "INPUT-BUFFER-TAIL")
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


(define-test test/input-buffer-append-octets ()
  (let* ((stream (make-instance 'telnet-stream
                                :element-type 'character))
         (nvt    (make-instance 'stub-nvt
                                :name "STUB NVT"
                                :client nil
                                :up-sender stream
                                :down-sender nil)))
    (setf (slot-value stream 'nvt) nvt)
    (let* ((smallest 32)
           (bytes (coerce (iota 95 smallest) '(vector octet))))
      (assert-true (= +input-buffer-size+
                      (length (input-buffer-data (input-buffer stream)))))
      (assert-true (plusp (rem +input-buffer-size+ (length bytes))) ()
                   "We expected (plusp (rem +input-buffer-size+ ~A)) but it's zero."
                   (length bytes))

      (loop ; fill the buffer (truncate 4096 95) -> 43 times, remains 11 free.
            :repeat (truncate (length (input-buffer-data (input-buffer stream)))
                              (length bytes))

            :for head := (input-buffer-head (input-buffer stream))
            :for tail := (input-buffer-tail (input-buffer stream))
            :do (input-buffer-append-octets stream bytes 0 (length bytes))
                (let* ((buffer (input-buffer stream)))
                  (check = (input-buffer-head buffer) 0                       (buffer bytes))
                  (check = (input-buffer-head buffer) head                    (buffer bytes))
                  (check = (input-buffer-tail buffer) (+ tail (length bytes)) (buffer bytes))
                  (check equalp
                         (subseq (input-buffer-data buffer) tail (input-buffer-tail buffer))
                         bytes (buffer bytes))))

      (let ((size 1000))

        (loop
          :repeat size
          :with head := (input-buffer-head (input-buffer stream))
          :with tail := (input-buffer-tail (input-buffer stream))
          :for i :from 0
          :for expected := (+ smallest (mod i (length bytes)))
          :for byte := (input-buffer-read-octet stream)
          :do (let* ((buffer (input-buffer stream)))
                (assert-true (= byte expected) (byte expected))
                (check = (input-buffer-head buffer) (+ head i 1)   (buffer bytes))
                (check = (input-buffer-tail buffer) tail           (buffer bytes))))

        (let* ((remains (rem (length (input-buffer-data (input-buffer stream)))
                             (length bytes)))
               (head (input-buffer-head (input-buffer stream)))
               (tail (input-buffer-tail (input-buffer stream))))
          (input-buffer-append-octets stream bytes 0 (length bytes))
          (let* ((buffer (input-buffer stream)))
            (check = (input-buffer-head buffer) size                     (buffer bytes))
            (check = (input-buffer-head buffer) head                     (buffer bytes))
            (check = (input-buffer-tail buffer)
                   (mod (+ tail (length bytes)) (length (input-buffer-data buffer)))
                   (buffer bytes))
            (check < (input-buffer-tail buffer) (input-buffer-head buffer)
                   (buffer bytes))
            (check equalp
                   (subseq (input-buffer-data buffer) tail (length (input-buffer-data buffer)))
                   (subseq bytes 0 remains)
                   (buffer bytes))
            (check equalp
                   (subseq (input-buffer-data buffer) 0 (- (length bytes) remains))
                   (subseq bytes remains)
                   (buffer bytes))))))))


(define-test test/all ()
  (test/replace-octets-by-string)
  (test/encode-string-to-output-buffer)
  (test/input-buffer-append-octets))

;; (setf *DEBUG-ON-FAILURE* t)
(test/all)
