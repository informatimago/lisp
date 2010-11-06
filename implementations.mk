#******************************************************************************
#FILE:				implementations.mk
#LANGUAGE:			make
#SYSTEM:			UNIX
#USER-INTERFACE:	None
#DESCRIPTION
#	This Makefile defines targets to compile my lisp sources with
#	various implementations.
#AUTHORS
#	<PJB> Pascal J. Bourguignon
#MODIFICATIONS
#   2010-06-27 <PJB> Factorized out from */Makefile.
#   2001-06-19 <PJB> Reorganized and simplified.
#LEGAL
#	Copyright Pascal J. Bourguignon 1992 - 2010
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

all::


CC            := gcc
AWK           := awk
EGREP         := egrep
WISH          := wish


COMPILERS      = abcl allegro ccl clisp cmucl ecl openmcl sbcl

ABCL		  := abcl
ALLEGRO       := alisp
CCL	          := ccl
CLISP         := clisp
ECL           := ecl
SBCL          := sbcl
CMUCL         := cmucl
OPENMCL       := openmcl

ABCL_FLAGS    := 
ALLEGRO_FLAGS := 
CCL_FLAGS     := 
CLISP_FLAGS   := -ansi -q -norc -Kfull -E iso-8859-1 -Efile UTF-8 -Eterminal UTF-8  -on-error debug
CMUCL_FLAGS   := -noinit -nositeinit 
ECL_FLAGS     := -norc 
OPENMCL_FLAGS := 
SBCL_FLAGS    := --noinform --sysinit /dev/null --userinit /dev/null 


LC_CTYPE       = en_US.UTF-8


DECLAIMS       = '(DECLAIM (OPTIMIZE (SAFETY 3) (DEBUG 3) (SPEED 0) (SPACE 0)))'
COMPILES       = '(load "compile.lisp")'

LINE           = ";;;;======================================================================\n"

all::show-compilers

show-compilers:
	@printf ";;;; Absent compilers:   "
	@for compiler in $(COMPILERS) ; do\
		if  type -p $$compiler >/dev/null 2>&1 ; then \
			true ;\
		else \
			printf "%s "  $$compiler ;\
		fi ;\
	 done
	@printf "\n;;;; Will compile with:  "
	@for compiler in $(COMPILERS) ; do \
		if  type -p $$compiler >/dev/null 2>&1 ; then \
			printf  "%s "  $$compiler ;\
		fi ;\
	 done
	@printf "\n"


allegro:
	@printf "\n\n\n\n"$(LINE)
	@if  type -p $(ALLEGRO) >/dev/null 2>&1 ; then \
		echo ";;;; Compiling with Allegro Common Lisp" ;\
	 	( LC_CTYPE=$(LC_CTYPE) $(ALLEGRO) $(ALLEGRO_FLAGS) \
		-e $(DECLAIMS) \
		-e $(COMPILES) \
		-e '(EXCL::EXIT-LISP 0)' \
		) ;\
	fi

abcl:
	@printf "\n\n\n\n"$(LINE)
	@if  type -p $(ABCL) >/dev/null 2>&1 ; then \
		echo ";;;; Compiling with Armed Bear Common Lisp" ;\
	 	( LC_CTYPE=$(LC_CTYPE) $(ABCL) $(ABCL_FLAGS) \
		--eval $(DECLAIMS) \
		--eval $(COMPILES) \
		--eval '(EXTENSIONS:QUIT)' \
		) ;\
	fi


clisp:
	@printf "\n\n\n\n"$(LINE)
	@if  type -p $(CLISP)  >/dev/null 2>&1 ; then \
		echo ";;;; Compiling with clisp Common Lisp" ;\
		( LC_CTYPE=$(LC_CTYPE) $(CLISP) $(CLISP_FLAGS) \
		-x $(DECLAIMS) \
		-x $(COMPILES) \
		-x '(EXT:QUIT 0)' \
	    ) 2>&1 | $(AWK) -f ../post-clisp.awk ;\
	fi

cmucl:
	@printf "\n\n\n\n"$(LINE)
	@if type -p $(CMUCL)  >/dev/null 2>&1 ; then \
		echo ";;;; Compiling with Carmegie-Mellon University Common Lisp" ;\
		( LC_CTYPE=$(LC_CTYPE) $(CMUCL) $(CMUCL_FLAGS) \
	    -eval '(setf extensions:*gc-verbose* nil)' \
		-eval $(DECLAIMS) \
		-eval $(COMPILES) \
		-eval '(EXTENSIONS:quit)' \
	 	) 2>&1 | $(AWK) '/^; Converted /{next;} /^; Byte Compiling /{next;} /^; Compiling Creation Form/{next;} /^; Converted /{next;} {print;}' ;\
	fi

ecl:
	@printf "\n\n\n\n"$(LINE)
	@if type -p $(ECL)  >/dev/null 2>&1 ; then \
		echo ";;;; Compiling with Embeddable Common Lisp" ;\
		( LC_CTYPE=$(LC_CTYPE) $(ECL) $(ECL_FLAGS) \
		-eval $(DECLAIMS) \
		-eval $(COMPILES) \
		-eval '(system:quit)' \
		) ;\
	fi

ccl:
	@printf "\n\n\n\n"$(LINE)
	@if type -p $(CCL)  >/dev/null 2>&1 ; then \
		echo ";;;; Compiling with Clozure Common Lisp" ;\
		( LC_CTYPE=$(LC_CTYPE) $(CCL) $(CCL_FLAGS)  \
		-eval $(DECLAIMS) \
		-eval $(COMPILES) \
		-eval '(ccl:quit)' \
		) ;\
	fi

openmcl:
	@printf "\n\n\n\n"$(LINE)
	@if type -p $(OPENMCL)  >/dev/null 2>&1 ; then \
		echo ";;;; Compiling with Open Macintosh Common Lisp" ;\
		( LC_CTYPE=$(LC_CTYPE) $(OPENMCL) $(OPENMCL_FLAGS)  \
		-eval $(DECLAIMS) \
		-eval $(COMPILES) \
		-eval '(ccl:quit)' \
		) ;\
	fi

sbcl:
	@printf "\n\n\n\n"$(LINE)
	@if  type -p $(SBCL)  >/dev/null 2>&1 ; then \
		echo ";;;; Compiling with Steel-Banks Common Lisp" ;\
		( LC_CTYPE=$(LC_CTYPE) $(SBCL) $(SBCL_FLAGS) \
		--eval '(DECLAIM (OPTIMIZE (SB-EXT::INHIBIT-WARNINGS 3)))' \
		--eval $(DECLAIMS) \
		--eval $(COMPILES) \
		--eval '(SB-EXT:QUIT)' \
		) 2>&1 | $(EGREP) -v '^(; compiling top level form|; recognizing )' ;\
	fi


system.asd summary summary.html:clisp

help::
	@echo 'Please choose the compiler: make $(COMPILERS)'
	@echo 'make system.asd # generates the ASDF system.'
	@echo 'make summary    # generates the summary.html file.'



install::
	-mkdir -p $(PACKAGES)/$(PACKAGE_PATH)
	install -m 644 *.lisp *.asd        $(PACKAGES)/$(PACKAGE_PATH)/

#	 -publish OBJ-*
#	 -umask 022 ; cp -r OBJ-*           $(PACKAGES)/$(PACKAGE_PATH)/

clean::
	-rm -rf obj-* OBJ-* *.lib *.fas *.fasl *.x86f

distclean::clean

clean-install::
	-rm -rf $(PACKAGES)/$(PACKAGE_PATH)/{obj-*,OBJ-*,*.lib,*.fas,*.fasl,*.x86f}

vars::
	@echo TARGET=$(TARGET)
	@echo MAKEDIR=$(MAKEDIR)
	@echo PREFIX=$(PREFIX)
	@echo MODULE=$(MODULE)
	@echo PACKAGE_PATH=$(PACKAGE_PATH)
	@echo "    Note: use 'make VAR=value' to override." 

etags tags TAGS:
	find $$(pwd) -name \[^#.]\*.lisp -print \
	| tee -a /dev/stderr -a /dev/stdout \
	| etags -
	@echo Done.

#### THE END ####
