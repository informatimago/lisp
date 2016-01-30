;;;; -*- coding:utf-8 -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(unless (find-package "ASDF") (load "LOADER:ASDF"))
(pushnew "PACKAGES:NET;SOURCEFORGE;LISA;LISA;" asdf::*central-registry*)
(asdf:oos 'asdf:load-op :lisa)

;;;; clocc.lisp                       --                     --          ;;;;
