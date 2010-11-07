#******************************************************************************
#FILE:              implementations.mk
#LANGUAGE:          make
#SYSTEM:            UNIX
#USER-INTERFACE:    None
#DESCRIPTION
#    This Makefile defines targets to batch compile lisp programs with
#    various implementations.
#AUTHORS
#    <PJB> Pascal J. Bourguignon
#MODIFICATIONS
#    2010-11-07 <PJB> Removed cruft.
#    2010-06-27 <PJB> Factorized out from */Makefile.
#    2001-06-19 <PJB> Reorganized and simplified.
#LEGAL
#    Copyright Pascal J. Bourguignon 1992 - 2010
#
#    This script is free software; you can redistribute it and/or
#    modify it under the terms of the GNU  General Public
#    License as published by the Free Software Foundation; either
#    version 2 of the License, or (at your option) any later version.
#
#    This script is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    General Public License for more details.
#
#    You should have received a copy of the GNU General Public
#    License along with this library; see the file COPYING.LIB.
#    If not, write to the Free Software Foundation,
#    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#******************************************************************************
# SHELL=/bin/bash -x

all::
help::


LINE           = ';;;;;;======================================================================\n'
HELP_FMT       = 'make %-25s \# %s\n'
HELP_FMT_2     = '                               \# %s\n'
VAR_FMT        = '\# %s\n\n%20s = %s\n\n'

help::
	@printf $(HELP_FMT) 'show-variables' 'Prints each parameter make variable its value.'
show-variables::




LC_CTYPE       = en_US.UTF-8
DECLAIMS       = '(DECLAIM (OPTIMIZE (SAFETY 3) (DEBUG 3) (SPEED 0) (SPACE 0)))'
COMPILES       = '(load "compile.lisp")'
# COMPILES       = '(load "'$$(pwd)/'compile.lisp")'

show-variables::
	@printf $(VAR_FMT) 'LC_CTYPE setting used to compile.'     LC_CTYPE $(LC_CTYPE)
	@printf $(VAR_FMT) 'a DECLAIM form used set optimization.' DECLAIMS $(DECLAIMS)
	@printf $(VAR_FMT) 'a load form to compile the library.'   COMPILES $(COMPILES)



CC            := gcc
AWK           := awk
EGREP         := egrep
WISH          := wish

COMPILERS      = abcl alisp ccl clisp cmucl ecl openmcl sbcl

ABCL		  := abcl
ALLEGRO       := alisp
CCL	          := ccl
CLISP         := clisp
CMUCL         := cmucl
ECL           := ecl
GCL		      := gcl
OPENMCL       := openmcl
SBCL          := sbcl

ABCL_FLAGS    := 
ALLEGRO_FLAGS :=       -q
CCL_FLAGS     :=      --quiet --no-init
CLISP_FLAGS   := -ansi -q      -norc -Kfull -E iso-8859-1 -Efile UTF-8 -Eterminal UTF-8 -on-error debug
CMUCL_FLAGS   :=               -noinit -nositeinit -eval '(setf extensions:*gc-verbose* nil)' 
ECL_FLAGS     :=               -norc 
GCL_FLAGS     :=               -norc
OPENMCL_FLAGS := 
SBCL_FLAGS    := --noinform --sysinit /dev/null --userinit /dev/null --eval '(DECLAIM (OPTIMIZE (SB-EXT::INHIBIT-WARNINGS 3)))' 

ABCL_EXIT     :=   --eval '(EXTENSIONS:QUIT)'   
ALLEGRO_EXIT  :=       -e '(EXCL::EXIT-LISP 0)'
CLISP_EXIT    :=       -x '(EXT:QUIT 0)'
CMUCL_EXIT    :=    -eval '(EXTENSIONS:quit)'
ECL_EXIT      :=    -eval '(system:quit)'
GCL_EXIT      :=    -eval '(error "How do we quit from gcl?")'
CCL_EXIT      :=   --eval '(ccl:quit)'
OPENMCL_EXIT  :=   --eval '(ccl:quit)'
SBCL_EXIT     :=    -eval '(SB-EXT:QUIT)'


ABCL_COMMAND    = LC_CTYPE=$(LC_CTYPE) $(ABCL)    $(ABCL_FLAGS)    --eval $(DECLAIMS) --eval $(COMPILES) $(ABCL_EXIT)   
ALLEGRO_COMMAND = LC_CTYPE=$(LC_CTYPE) $(ALLEGRO) $(ALLEGRO_FLAGS)     -e $(DECLAIMS)     -e $(COMPILES) $(ALLEGRO_EXIT)
CLISP_COMMAND   = LC_CTYPE=$(LC_CTYPE) $(CLISP)   $(CLISP_FLAGS)       -x $(DECLAIMS)     -x $(COMPILES) $(CLISP_EXIT)  
CMUCL_COMMAND   = LC_CTYPE=$(LC_CTYPE) $(CMUCL)   $(CMUCL_FLAGS)    -eval $(DECLAIMS)  -eval $(COMPILES) $(CMUCL_EXIT)  
ECL_COMMAND     = LC_CTYPE=$(LC_CTYPE) $(ECL)     $(ECL_FLAGS)      -eval $(DECLAIMS)  -eval $(COMPILES) $(ECL_EXIT)    
GCL_COMMAND     = LC_CTYPE=$(LC_CTYPE) $(GCL)     $(GCL_FLAGS)      -eval $(DECLAIMS)  -eval $(COMPILES) $(GCL_EXIT)    
CCL_COMMAND     = LC_CTYPE=$(LC_CTYPE) $(CCL)     $(CCL_FLAGS)     --eval $(DECLAIMS) --eval $(COMPILES) $(CCL_EXIT)    
OPENMCL_COMMAND = LC_CTYPE=$(LC_CTYPE) $(OPENMCL) $(OPENMCL_FLAGS) --eval $(DECLAIMS) --eval $(COMPILES) $(OPENMCL_EXIT)
SBCL_COMMAND    = LC_CTYPE=$(LC_CTYPE) $(SBCL)    $(SBCL_FLAGS)    --eval $(DECLAIMS) --eval $(COMPILES) $(SBCL_EXIT)   


abcl-command:    ; @echo $(ABCL_COMMAND)
allegro-command: ; @echo $(ALLEGRO_COMMAND)
clisp-command:   ; @echo $(CLISP_COMMAND)
cmucl-command:   ; @echo $(CMUCL_COMMAND)
ecl-command:     ; @echo $(ECL_COMMAND)
gcl-command:     ; @echo $(GCL_COMMAND)
ccl-command:     ; @echo $(CCL_COMMAND)
openmcl-command: ; @echo $(OPENMCL_COMMAND)
sbcl-command:    ; @echo $(SBCL_COMMAND)


all:: show-compilers


help::
	@printf $(HELP_FMT) 'show-compilers' 'Shows the list of available compilers.'
show-compilers:
	@printf ';;;; Absent compilers:   '
	@for compiler in $(COMPILERS) ; do\
		if  type -p $$compiler >/dev/null 2>&1 ; then \
			true ;\
		else \
			printf '%s '  $$compiler ;\
		fi ;\
	 done
	@printf '\n;;;; Will compile with:  '
	@for compiler in $(COMPILERS) ; do \
		if  type -p $$compiler >/dev/null 2>&1 ; then \
			printf  '%s '  $$compiler ;\
		fi ;\
	 done
	@printf '\n'

abcl:
	@printf '\n\n\n\n'$(LINE)
	@if  type -p $(ABCL) >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Armed Bear Common Lisp' ;\
		$(ABCL_COMMAND) ;\
	fi

alisp:
	@printf '\n\n\n\n'$(LINE)
	@if  type -p $(ALLEGRO) >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Allegro Common Lisp' ;\
		$(ALLEGRO_COMMAND) ;\
	fi

clisp:
	@printf '\n\n\n\n'$(LINE)
	@if  type -p $(CLISP)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with clisp Common Lisp' ;\
		$(CLISP_COMMAND) 2>&1 | $(AWK) -f ../post-clisp.awk ;\
	fi

cmucl:
	@printf '\n\n\n\n'$(LINE)
	@if type -p $(CMUCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Carmegie-Mellon University Common Lisp' ;\
		$(CMUCL_COMMAND) 2>&1 | $(AWK) '/^; Converted /{next;} /^; Byte Compiling /{next;} /^; Compiling Creation Form/{next;} /^; Converted /{next;} {print;}' ;\
	fi

ecl:
	@printf '\n\n\n\n'$(LINE)
	@if type -p $(ECL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Embeddable Common Lisp' ;\
		$(ECL_COMMAND) ;\
	fi

gcl:
	@printf '\n\n\n\n'$(LINE)
	@printf 'gcl is not implemented yet.'
	@false
	@if type -p $(GCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with GNU Common Lisp' ;\
		$(GCL_COMMAND) ;\
	fi

ccl:
	@printf '\n\n\n\n'$(LINE)
	@if type -p $(CCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Clozure Common Lisp' ;\
		"$(CCL_COMMAND)" ;\
	fi

openmcl:
	@printf '\n\n\n\n'$(LINE)
	@if type -p $(OPENMCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Open Macintosh Common Lisp' ;\
		$(OPENMCL_COMMAND) ;\
	fi

sbcl:
	@printf '\n\n\n\n'$(LINE)
	@if  type -p $(SBCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Steel-Banks Common Lisp' ;\
		$(SBCL_COMMAND) 2>&1 | $(EGREP) -v '^(; compiling top level form|; recognizing )' ;\
	fi

#### THE END ####
