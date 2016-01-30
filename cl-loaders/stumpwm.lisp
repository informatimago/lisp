;;;; -*- coding:utf-8 -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(load "loaders:clocc")
(load "port:shell")
(load "loaders:cclan")
(push "/usr/local/src/stumpwm/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'stumpwm)
(stumpwm:stumpwm "" :display 1)

