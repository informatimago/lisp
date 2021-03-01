
(pushnew #P"~/src/public/lisp/small-cl-pgms/irclog-prompter/" asdf:*central-registry* :test (function equalp))
(ql:quickload "com.informatimago.small-cl-pgms.irclog")
(require :cocoa)
