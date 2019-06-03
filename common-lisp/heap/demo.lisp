(defpackage "DEMO"
  (:use "CL"
        "COM.INFORMATIMAGO.COMMON-LISP.HEAP.MEMORY"
        "COM.INFORMATIMAGO.COMMON-LISP.HEAP.HEAP"))
(in-package "DEMO")
(defparameter *mem* (make-instance 'memory-vector-64 :base 0 :size 32768))
(common-initialize *mem*)
(defcommon *fruits*)
(setf *fruits* '((2 apple) (3 orange) (18 cherry)))
(setf *fruits* '((10 apple) (2 orange) (19 cherry)))
*fruits* ; -> ((10 apple) (2 orange) (19 cherry))
(macroexpand '*fruits*) ; -> (get-common '*fruits*) ; t
com.informatimago.common-lisp.heap.heap::*defined-common-variables* ; -> (*fruits* *common-variables*)
(subseq (com.informatimago.common-lisp.heap.memory::bytes *mem*) 0 8) ; -> #(4597283572347515282 0 2305843009213693960 72057594037927942 2594073385365405895 1008806316530995200 2594073385365405706 2594073385365405953)
(dump *mem* 0 80 :stream *standard-output* :margin "" :byte-size 8)
