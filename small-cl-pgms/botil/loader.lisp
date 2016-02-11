
(push (pwd) asdf:*central-registry*)
(ql:quickload :com.informatimago.small-cl-pgms.botil)
(in-package :com.informatimago.small-cl-pgms.botil)
(com.informatimago.small-cl-pgms.botil:main)
