;; (push #P"~/src/public/test/pkcs11/" asdf:*central-registry*)
(ql:quickload :com.informatimago.clext.pkcs11)
(com.informatimago.clext.pkcs11:load-library)
