#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           make
#SYSTEM:             posix
#USER-INTERFACE:     none
#DESCRIPTION
#    
#    This makefile compiles my pure Common-Lisp packages.
#    
#AUTHORS
#    <PJB> Pascal Bourguignon
#MODIFICATIONS
#    2003-05-04 <PJB> Added this header.
#BUGS
#LEGAL
#    GPL
#    
#    Copyright Pascal Bourguignon 2003 - 2003
#    mailto:pjb@informatimago.com
#    
#    This program is free software; you can redistribute it and/or
#    modify it under the terms of the GNU General Public License
#    as published by the Free Software Foundation; either version
#    2 of the License, or (at your option) any later version.
#    
#    This program is distributed in the hope that it will be
#    useful, but WITHOUT ANY WARRANTY; without even the implied
#    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#    PURPOSE.  See the GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public
#    License along with this program; if not, write to the Free
#    Software Foundation, Inc., 59 Temple Place, Suite 330,
#    Boston, MA 02111-1307 USA
#*****************************************************************************
SHELL=/bin/bash
TARGET  := $(shell uname)
PREFIX  := /usr/local
MODULE=clmisc
PACKAGES:=$(shell get-directory SHARE_LISP)/packages
PACKAGE_PATH=com/informatimago/clmisc
#UNAMES:='(defparameter uname "'$(shell uname)'")(defparameter uname-m "'$(shell uname -m)'")'
UNAMES:=''
AWK = $(shell which awk)

#UNAME    :=$(shell uname)
#PREFIX   :=$(shell pwd)/../install
#BASE     :=$(shell pwd)/..
#PACKAGES :=$(BASE)/packages

SBCL  := sbcl
CLISP := clisp
WISH  := wish

CLISP_FLAGS= -ansi -q -norc -Kfull -E iso-8859-1 -Efile UTF-8 -Eterminal UTF-8 -on-error debug
LC_CTYPE=en_US.UTF-8
# LC_CTYPE=$(LC_CTYPE) 

LINE="\n\n;;========================================================================\n"
all:
	@echo 'Please choose the compiler: make clisp sbcl cmucl openmcl'

clisp summary summary.html:
	@printf $(LINE)
	@ [ -x $(CLISP) ] && ( $(CLISP) $(CLISP_FLAGS) -Efile ISO-8859-1 \
		-x '(progn '$(UNAMES)' (load "compile.lisp") (ext:quit))' 2>&1 \
	| $(AWK) -f ../common-lisp/post-clisp.awk )||true
	@printf $(LINE)
# clisp: NO -K full since we want plain COMMON-LISP

allegro:
	@printf $(LINE)
	@ type -p alisp && ( LC_CTYPE=$(LC_CTYPE) alisp \
		-e '(progn '$(UNAMES)' (load "compile.lisp"))' \
		-e '(EXCL::EXIT-LISP 0)' \
	)
	@printf $(LINE)


sbcl:
	@printf $(LINE)
	@ type -p sbcl && ( LC_CTYPE=$(LC_CTYPE)  sbcl \
		--noinform \
		--sysinit /dev/null --userinit /dev/null  \
		--eval '(DECLAIM (OPTIMIZE (SAFETY 3)))'   \
		--eval '(DECLAIM (OPTIMIZE (SB-EXT::INHIBIT-WARNINGS 3)))' \
		--eval '(progn '$(UNAMES)' (load "compile.lisp"))' \
		--eval '(sb-ext:quit)' 2>&1 \
		| egrep -v '^(; compiling top level form|; recognizing )' ) || true
	@printf $(LINE)

cmucl:
	@printf $(LINE)
	@ type -p cmucl && ( ( LC_CTYPE=$(LC_CTYPE)  cmucl \
		-noinit -nositeinit \
		-eval '(setf extensions:*gc-verbose* nil)' \
		-eval '(progn '$(UNAMES)' (load "compile.lisp"))' \
		-eval '(EXTENSIONS:quit)' 2>&1 )\
		| $(AWK) '/^; Converted /{next;} /^; Byte Compiling /{next;} /^; Compiling Creation Form/{next;} /^; Converted /{next;} {print;}' ) || true
	@printf $(LINE)

ecl:
	@printf $(LINE)
	@ type -p ecl && ( LC_CTYPE=$(LC_CTYPE)  ecl \
		-norc \
		-eval '(progn '$(UNAMES)' (load "compile.lisp"))' \
		-eval '(system:quit)' 2>&1 )
	@printf $(LINE)


openmcl:
	@printf $(LINE)
	@ type -p openmcl && ( LC_CTYPE=$(LC_CTYPE) openmcl \
		--eval '(progn '$(UNAMES)' (load "compile.lisp"))' \
		--eval '(ccl:quit)' )\
		|| true
	@printf $(LINE)

install:
	-mkdir -p $(PACKAGES)/$(PACKAGE_PATH)
	install -m 644 *.lisp        *.DAT *.asd $(PACKAGES)/$(PACKAGE_PATH)/
#	-publish OBJ-*
#	-umask 022 ; cp -r OBJ-*                 $(PACKAGES)/$(PACKAGE_PATH)/

clean:
	-rm -rf obj-* OBJ-* *.lib *.fas *.fasl *.x86f

distclean:clean

clean-install:
	-rm -rf $(PACKAGES)/$(PACKAGE_PATH)/{obj-*,OBJ-*,*.lib,*.fas,*.fasl,*.x86f}

vars:
	@echo TARGET=$(TARGET)
	@echo PREFIX=$(PREFIX)
	@echo MODULE=$(MODULE)
	@echo PACKAGE_PATH=$(PACKAGE_PATH)
	@echo "    Note: use 'make VAR=value' to override." 


etags tags TAGS:
	find $$(pwd) -name \[^#.]\*.lisp -print \
	| tee -a /dev/stderr -a /dev/stdout \
	| etags -
	@echo Done.

#### Makefile                         --                     --          ####
