;;;; -*- coding:utf-8 -*-

(load "loaders:clocc")
(load "port:shell")
(load "loaders:cclan")
(push "/usr/local/src/stumpwm/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'stumpwm)
(stumpwm:stumpwm "" :display 1)

