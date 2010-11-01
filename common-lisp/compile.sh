#!/bin/sh

ccl   -n               -e '(load "compile-with-asdf.lisp")'  -e    '(ccl:quit)'
clisp -norc            -x '(load "compile-with-asdf.lisp")'  -x    '(ext:quit)'
ecl   -norc         -eval '(load "compile-with-asdf.lisp")'  -eval '(ext:quit)'
sbcl --no-userinit --eval '(load "compile-with-asdf.lisp")' --eval '(sb-ext:quit)'

#### THE END ####
