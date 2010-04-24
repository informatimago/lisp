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

MODULES = common-lisp clext clisp sbcl susv3 clmisc

all:install-clisp lisp-clean

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
C=clisp
S=sbcl
U=cmucl
A=allegro
ALL=$(C) $(S)


MM=$(MAKE) $(MFLAGS)
clean:
	$(MM) -C common-lisp $(CLEAN)
	$(MM) -C clext       $(CLEAN)
	$(MM) -C clmisc      $(CLEAN)
	$(MM) -C sbcl        $(CLEAN)
	$(MM) -C clisp       $(CLEAN)
	$(MM) -C susv3       $(CLEAN)

lisp-clean:
	$(MM) -C common-lisp $(CLEAN) $(ALL) install
	$(MM) -C clext       $(CLEAN) $(ALL) install
	$(MM) -C clmisc      $(CLEAN) $(ALL) install
	$(MM) -C sbcl        $(CLEAN) $(S)   install
	$(MM) -C clisp       $(CLEAN) $(C)   install
	$(MM) -C susv3       $(CLEAN) $(C)   install

lisp:
	$(MM) -C common-lisp          $(ALL) install
	$(MM) -C clext                $(ALL) install
	$(MM) -C clmisc               $(ALL) install
	$(MM) -C sbcl                 $(S)   install
	$(MM) -C clisp                $(C)   install
	$(MM) -C susv3                $(C)   install

install:
	$(MM) -C common-lisp                 install
	$(MM) -C clext                       install
	$(MM) -C clmisc                      install
	$(MM) -C sbcl                        install
	$(MM) -C clisp                       install
	$(MM) -C susv3                       install

#### Makefile                         --                     --          ####
