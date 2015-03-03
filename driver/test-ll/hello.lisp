(defpackage "H"
  (:use "CL")
  (:export "ELLO"))
(in-package "H")
(defun ello ()
  (prin1 'hello))
