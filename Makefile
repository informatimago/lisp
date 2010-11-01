#******************************************************************************
#FILE:				$(COMMON)/Makefile
#LANGUAGE:			make
#SYSTEM:			UNIX
#USER-INTERFACE:	None
#DESCRIPTION
#	This Makefile tells how to compile the lisp libraries.
#
#	Input variables are:
#
#       $(MAKEDIR)      where the makefiles are located.
#
#       $(PREFIX)       where the libraries are installed.
#                       It will be created a subdirectory in this
#                       $(PREFIX) for each library compiled by
#                       this makefile.
#
#	Output:
#
#		$(PREFIX)/bin/$(PROGRAM)               the executables.
#		$(PREFIX)/lib/$(MODULE)/interfaces/    the interface of the module,
#		$(PREFIX)/lib/$(MODULE)/documentation/ the documentation of the module,
#		$(PREFIX)/lib/$(MODULE)/libraries/     the libraries of the module.
#
#AUTHORS
#	<PJB> Pascal J. Bourguignon
#MODIFICATIONS
#   2001-06-19 <PJB> Reorganized and simplified.
#LEGAL
#	Copyright Pascal J. Bourguignon 1992 - 2003
#
#   This script is free software; you can redistribute it and/or
#   modify it under the terms of the GNU  General Public
#   License as published by the Free Software Foundation; either
#   version 2 of the License, or (at your option) any later version.
#
#   This script is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this library; see the file COPYING.LIB.
#   If not, write to the Free Software Foundation,
#   59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#******************************************************************************

TARGET  := $(shell uname)
PREFIX  := /usr/local
MAKEDIR := $(HOME)/src/public/common/makedir

MODULES = common-lisp clext clmisc sbcl clisp susv3


LINE    = ";;;;;;====================================================================\n"



all: install clean lisp

#include $(MAKEDIR)/project

help::
	@echo ""
	@echo "Some targets:"
	@echo "    make lisp          # make and install all the subprojects"
	@echo "    make lisp-clean    # clean, make and install all the subprojects"
	@echo "    make install-clisp # make and install the clisp subprojects"
	@echo "    make tags          # make the TAGS"
	@echo ""

vars variables:
	@echo "Input variables are:"
	@echo "    TARGET   ($(TARGET))"
	@echo "    MAKEDIR  ($(MAKEDIR))"
	@echo "    PREFIX   ($(PREFIX))"
	@echo ""
	@echo "They can be changed passing them as make argument:"
	@echo "    make TARGET=new-target MAKEDIR=/make/dir PREFIX=/usr/local install"


install-clisp:
	make PREFIX=$(PREFIX) \
		 COMMON=`pwd`  \
		 MAKEDIR=`pwd`/makedir \
		-C common-lisp clisp install
	make PREFIX=$(PREFIX) \
		 COMMON=`pwd`  \
		 MAKEDIR=`pwd`/makedir \
		-C clisp clisp install
	make PREFIX=$(PREFIX) \
		 COMMON=`pwd`  \
		 MAKEDIR=`pwd`/makedir \
		-C susv3 clisp install

m1:
	make PREFIX=$(HOME)/install \
		 COMMON=`pwd`  \
		 MAKEDIR=`pwd`/makedir \
		cvsclean depend install
m1i:
	make PREFIX=$(HOME)/install \
		 COMMON=`pwd`  \
		 MAKEDIR=`pwd`/makedir \
		install
m2:
	make PREFIX=/local \
		 COMMON=`pwd` \
		 MAKEDIR=`pwd`/makedir \
		cvsclean depend install


etags tags:
	find $$(pwd) \( -name \*.lisp \
					-o -name \*.[hc] \
					-o -name \*.hh -o -name \*.cc \
				\) -print | etags -


CLEAN=clean-install clean
A=allegro
C=clisp
U=cmucl
E=ecl
S=sbcl
O=openmcl
ALL:=$(C) $(E) $(S) 


MM=$(MAKE) $(MFLAGS)
clean:
	@for module in $(MODULES) ; do \
		printf $(LINE) ;\
		printf ";;;;;; CLEANING $$module\n" ;\
		$(MM) -C $$module $(CLEAN) ;\
	 done
	@printf $(LINE)

lisp-clean:
	@for module in $(MODULES) ; do \
		printf $(LINE) ;\
		printf ";;;;;; CLEANING, COMPILING and INSTALLING $$module\n" ;\
		$(MM) -C $$module $(CLEAN) all install ;\
	 done
	@printf $(LINE)

lisp:
	@for module in $(MODULES) ; do \
		printf $(LINE) ;\
		printf ";;;;;; COMPILING and INSTALLING $$module\n" ;\
		$(MM) -C $$module all install ;\
	 done
	@printf $(LINE)

install:
	@for module in $(MODULES) ; do \
		printf $(LINE) ;\
		printf ";;;;;; INSTALLING $$module\n" ;\
		$(MM) -C $$module all install ;\
	 done
	@printf $(LINE)


subprojects modules:
	@echo $(MODULES) 

systems:
	mcp  \*/system.asd \#1/com.informatimago.\#1.asd

#### THE END ####

