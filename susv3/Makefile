#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           make
#SYSTEM:             posix
#USER-INTERFACE:     none
#DESCRIPTION
#    
#    This makefile compiles my SUSv3 packages: packages that depend 
#    on SUSV3 API.
#    
#AUTHORS
#    <PJB> Pascal Bourguignon
#MODIFICATIONS
#    2005-01-06 <PJB> Created.
#BUGS
#LEGAL
#    GPL
#    
#    Copyright Pascal Bourguignon 2004 - 2005
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
MAKEDIR := $(HOME)/src/common/makedir
PREFIX  := /usr/local
MODULE=susv3
PACKAGES:=$(shell get-directory SHARE_LISP)/packages
PACKAGE_PATH=com/informatimago/susv3
#UNAMES:='(defparameter uname "'$(shell uname)'")(defparameter uname-m "'$(shell uname -m)'")'
UNAMES:=''
AWK = awk

#			| $(AWK) '/Compiling file.*\.\.\./{l=gensub("file /.*/","","",$$0);next;} /Loading/{next;} /Loaded/{next;} /^$$/{if(l!=""){print l;l="";next;}} {print $$0}' \
#			| $(AWK) -v file="$<" ' / in lines /{ ei=index($$0," in "); error=substr($$0,1,ei); pi=index($$0," in lines "); proc=substr($$0,ei+4,pi-ei-4); li=index($$0,".."); from=substr($$0,pi+10,li-pi-10); to=substr($$0,li+2); printf("\n%s:%d: In function %s\n%s:%d: %s ",file,(1+from),proc,file,to,error); next; } { print $$0; }' \
#			| $(AWK) -v file="$<" '/ in .* in lines /{split($$6,n,"\\.");printf "%s:%s: %s in %s\n%s:%s: %s in %s\n",file,n[1],$$1,$$3,file,n[3],$$1,$$3;next;}{print $$0;}' \
#

#UNAME    :=$(shell uname)
#PREFIX   :=$(shell pwd)/../install
#BASE     :=$(shell pwd)/..
#PACKAGES :=$(BASE)/packages

SBCL  := sbcl
CLISP := clisp
WISH  := wish

CLISP_FLAGS= -ansi -q -norc -Kfull -E iso-8859-1 -Efile UTF-8 -Eterminal UTF-8  -on-error debug
LC_CTYPE=en_US.UTF-8

LINE="\n\n;;========================================================================\n"
all:
	@echo 'Please choose the compiler: make clisp sbcl cmucl openmcl'

clisp summary summary.html:
	@printf $(LINE)
	 type -p clisp && ( $(CLISP) $(CLISP_FLAGS) -Efile ISO-8859-1 \
		-x '(progn '$(UNAMES)' (load "compile.lisp") (ext:quit))' 2>&1 \
	| $(AWK) -f post-clisp.awk ) || true
	@printf $(LINE)
# clisp: NO -K full since we want plain COMMON-LISP

sbcl:
	@printf $(LINE)
	@ type -p sbcl && ( LC_CTYPE=$(LC_CTYPE) sbcl \
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
	@ type -p cmucl && ( ( LC_CTYPE=$(LC_CTYPE) cmucl \
		-noinit -nositeinit \
		-eval '(progn (setf extensions:*gc-verbose* nil) '$(UNAMES)' (load "compile.lisp"))' \
		-eval '(EXTENSIONS:quit)' 2>&1 )\
		| awk '/^; Converted /{next;} /^; Byte Compiling /{next;} /^; Compiling Creation Form/{next;} /^; Converted /{next;} {print;}' ) || true
	@printf $(LINE)

openmcl:
	@printf $(LINE)
	@ type -p openmcl && ( LC_CTYPE=$(LC_CTYPE) openmcl \
		-eval '(progn '$(UNAMES)' (load "compile.lisp"))' \
		-eval '(ccl:quit)' )\
		|| true
	@printf $(LINE)


install:
	-mkdir -p $(PACKAGES)/$(PACKAGE_PATH)
	install -m 644 *.lisp *.asd        $(PACKAGES)/$(PACKAGE_PATH)/
#	 -publish OBJ-*
#	 -umask 022 ; cp -r OBJ-*           $(PACKAGES)/$(PACKAGE_PATH)/

clean:
	-rm -rf obj-* OBJ-* *.lib *.fas *.fasl *.x86f

distclean:clean

clean-install:
	-rm -rf $(PACKAGES)/$(PACKAGE_PATH)/{obj-*,OBJ-*,*.lib,*.fas,*.fasl,*.x86f}

vars:
	@echo TARGET=$(TARGET)
	@echo MAKEDIR=$(MAKEDIR)
	@echo PREFIX=$(PREFIX)
	@echo MODULE=$(MODULE)
	@echo PACKAGE_PATH=$(PACKAGE_PATH)
	@echo "    Note: use 'make VAR=value' to override." 


#### Makefile                         --                     --          ####
