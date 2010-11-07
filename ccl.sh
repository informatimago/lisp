#!/bin/bash
LC_CTYPE=en_US.UTF-8 ccl     --quiet --no-init     --eval '(DECLAIM (OPTIMIZE (SAFETY 3) (DEBUG 3) (SPEED 0) (SPACE 0)))' --eval '(load "compile.lisp")' --eval '(ccl:quit)'  
