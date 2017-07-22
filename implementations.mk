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
TOP:=$(dir $(lastword $(MAKEFILE_LIST)))
SRCROOT=$(shell cd $(TOP) ; pwd)

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
DECLAIMS       = '(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))'
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

ABCL		  := abcl
ALLEGRO       := alisp
CCL	          := ccl
CLISP         := clisp
CMUCL         := lisp
ECL           := ecl
GCL		      := gcl
OPENMCL       := openmcl
SBCL          := sbcl

COMPILERS      = $(ABCL) $(ALISP) $(CCL) $(CLISP) $(CMUCL) $(ECL) $(OPENMCL) $(SBCL)

ABCL_FLAGS    :=      --batch --noinform --noinit
ALLEGRO_FLAGS :=       -q
CCL_FLAGS     :=      --quiet --no-init
CLISP_FLAGS   := -ansi -q      -norc -Kfull -E iso-8859-1 -Efile UTF-8 -Eterminal UTF-8 -on-error debug
CMUCL_FLAGS   :=               -noinit -nositeinit -eval '(setf extensions:*gc-verbose* nil)'
ECL_FLAGS     :=               -norc
GCL_FLAGS     :=               -norc
OPENMCL_FLAGS :=
SBCL_FLAGS    := --noinform --sysinit /dev/null --userinit /dev/null --eval '(declaim (optimize (sb-ext::inhibit-warnings 3)))'

ABCL_EVAL     :=   --eval
ALLEGRO_EVAL  :=       -e
CLISP_EVAL    :=       -x
CMUCL_EVAL    :=    -eval
ECL_EVAL      :=    -eval
GCL_EVAL      :=    -eval
CCL_EVAL      :=   --eval
OPENMCL_EVAL  :=   --eval
SBCL_EVAL     :=   --eval

ABCL_EXIT     :=   --eval '(extensions:quit)'
ALLEGRO_EXIT  :=       -e '(excl::exit-lisp 0)'
CLISP_EXIT    :=       -x '(ext:quit 0)'
CMUCL_EXIT    :=    -eval '(extensions:quit)'
ECL_EXIT      :=    -eval '(system:quit)'
GCL_EXIT      :=    -eval '(error "How do we quit from gcl?")'
CCL_EXIT      :=   --eval '(ccl:quit)'
OPENMCL_EXIT  :=   --eval '(ccl:quit)'
SBCL_EXIT     :=   --eval '(sb-ext:quit)'

eval_with_abcl     = LC_CTYPE=$(LC_CTYPE) $(ABCL)    $(ABCL_FLAGS)    --eval $(DECLAIMS) --eval $(1) $(ABCL_EXIT)
eval_with_allegro  = LC_CTYPE=$(LC_CTYPE) $(ALLEGRO) $(ALLEGRO_FLAGS)     -e $(DECLAIMS)     -e $(1) $(ALLEGRO_EXIT)
eval_with_clisp    = LC_CTYPE=$(LC_CTYPE) $(CLISP)   $(CLISP_FLAGS)       -x $(DECLAIMS)     -x $(1) $(CLISP_EXIT)
eval_with_cmucl    = LC_CTYPE=$(LC_CTYPE) $(CMUCL)   $(CMUCL_FLAGS)    -eval $(DECLAIMS)  -eval $(1) $(CMUCL_EXIT)
eval_with_ecl      = LC_CTYPE=$(LC_CTYPE) $(ECL)     $(ECL_FLAGS)      -eval $(DECLAIMS)  -eval $(1) $(ECL_EXIT)
eval_with_gcl      = LC_CTYPE=$(LC_CTYPE) $(GCL)     $(GCL_FLAGS)      -eval $(DECLAIMS)  -eval $(1) $(GCL_EXIT)
eval_with_ccl      = LC_CTYPE=$(LC_CTYPE) $(CCL)     $(CCL_FLAGS)     --eval $(DECLAIMS) --eval $(1) $(CCL_EXIT)
eval_with_openmcl  = LC_CTYPE=$(LC_CTYPE) $(OPENMCL) $(OPENMCL_FLAGS) --eval $(DECLAIMS) --eval $(1) $(OPENMCL_EXIT)
eval_with_sbcl     = LC_CTYPE=$(LC_CTYPE) $(SBCL)    $(SBCL_FLAGS)    --eval $(DECLAIMS) --eval $(1) $(SBCL_EXIT)

ABCL_COMMAND       = $(call	eval_with_abcl,$(COMPILES))
ALLEGRO_COMMAND    = $(call	eval_with_allegro,$(COMPILES))
CLISP_COMMAND      = $(call	eval_with_clisp,$(COMPILES))
CMUCL_COMMAND      = $(call	eval_with_cmucl,$(COMPILES))
ECL_COMMAND        = $(call	eval_with_ecl,$(COMPILES))
GCL_COMMAND        = $(call	eval_with_gcl,$(COMPILES))
CCL_COMMAND        = $(call	eval_with_ccl,$(COMPILES))
OPENMCL_COMMAND    = $(call	eval_with_openmcl,$(COMPILES))
SBCL_COMMAND       = $(call	eval_with_sbcl,$(COMPILES))


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
	@printf '\n;;;; Would compile with:  '
	@for compiler in $(COMPILERS) ; do \
		if  type -p $$compiler >/dev/null 2>&1 ; then \
			printf  '%s '  $$compiler ;\
		fi ;\
	 done
	@printf '\n'

help::
	@printf $(HELP_FMT) 'show-compilers-verbose' 'Shows the list of available compilers, their versions, and the command used to compile.'
show-compilers-verbose:
	@printf ';;;; Absent compilers:   '
	@for compiler in $(COMPILERS) ; do\
		if  type -p $$compiler >/dev/null 2>&1 ; then \
			true ;\
		else \
			printf '%s '  $$compiler ;\
		fi ;\
	 done
	@printf '\n;;;; Would compile with these compiler versions:  \n'
	@for compiler in $(COMPILERS) ; do $(MAKE) -s $${compiler}-version ; done
	@printf '\n;;;; Would compile with these commands:  \n'
	@for compiler in $(COMPILERS) ; do $(MAKE) -s $${compiler}-command ; done
	@printf '\n'

LISP_VERSION='(progn (format t "~A ~A~%" (lisp-implementation-type) (lisp-implementation-version)) (finish-output))'
$(ABCL)-version:    ; -@type -p $(ABCL)     >/dev/null 2>&1 && $(ABCL)    $(ABCL_FLAGS)    --eval $(LISP_VERSION) $(ABCL_EXIT)
$(ALLEGRO)-version: ; -@type -p $(ALLEGRO)  >/dev/null 2>&1 && $(ALLEGRO) $(ALLEGRO_FLAGS)     -e $(LISP_VERSION) $(ALLEGRO_EXIT)
$(CLISP)-version:   ; -@type -p $(CLISP)    >/dev/null 2>&1 && $(CLISP)   $(CLISP_FLAGS)       -x $(LISP_VERSION) $(CLISP_EXIT)
$(CMUCL)-version:   ; -@type -p $(CMUCL)    >/dev/null 2>&1 && $(CMUCL)   $(CMUCL_FLAGS)    -eval $(LISP_VERSION) $(CMUCL_EXIT)
$(ECL)-version:     ; -@type -p $(ECL)      >/dev/null 2>&1 && $(ECL)     $(ECL_FLAGS)      -eval $(LISP_VERSION) $(ECL_EXIT)
$(GCL)-version:     ; -@type -p $(GCL)      >/dev/null 2>&1 && $(GCL)     $(GCL_FLAGS)      -eval $(LISP_VERSION) $(GCL_EXIT)
$(CCL)-version:     ; -@type -p $(CCL)      >/dev/null 2>&1 && $(CCL)     $(CCL_FLAGS)     --eval $(LISP_VERSION) $(CCL_EXIT)
$(OPENMCL)-version: ; -@type -p $(OPENMCL)  >/dev/null 2>&1 && $(OPENMCL) $(OPENMCL_FLAGS) --eval $(LISP_VERSION) $(OPENMCL_EXIT)
$(SBCL)-version:    ; -@type -p $(SBCL)     >/dev/null 2>&1 && $(SBCL)    $(SBCL_FLAGS)    --eval $(LISP_VERSION) $(SBCL_EXIT)

help::
	@for compiler in $(COMPILERS) ; do printf $(HELP_FMT) "$${compiler}-command" "Shows the command used to compile with $${compiler}." ; done
$(ABCL)-command:    ; @echo "$(ABCL_COMMAND)"
$(ALLEGRO)-command: ; @echo "$(ALLEGRO_COMMAND)"
$(CLISP)-command:   ; @echo "$(CLISP_COMMAND)"
$(CMUCL)-command:   ; @echo "$(CMUCL_COMMAND)"
$(ECL)-command:     ; @echo "$(ECL_COMMAND)"
$(GCL)-command:     ; @echo "$(GCL_COMMAND)"
$(CCL)-command:     ; @echo "$(CCL_COMMAND)"
$(OPENMCL)-command: ; @echo "$(OPENMCL_COMMAND)"
$(SBCL)-command:    ; @echo "$(SBCL_COMMAND)"




compile-with-$(ABCL):
	@printf '\n\n\n\n'$(LINE)
	@if  type -p $(ABCL) >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Armed Bear Common Lisp' ;\
		$(ABCL_COMMAND) ;\
	fi

compile-with-$(ALLEGRO):
	@printf '\n\n\n\n'$(LINE)
	@if  type -p $(ALLEGRO) >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Allegro Common Lisp' ;\
		$(ALLEGRO_COMMAND) ;\
	fi

compile-with-$(CLISP):
	@printf '\n\n\n\n'$(LINE)
	@if  type -p $(CLISP)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with clisp Common Lisp' ;\
		$(CLISP_COMMAND) 2>&1 | $(AWK) -f $(SRCROOT)/post-clisp.awk ;\
	fi

compile-with-$(CMUCL):
	@printf '\n\n\n\n'$(LINE)
	@if type -p $(CMUCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Carmegie-Mellon University Common Lisp' ;\
		$(CMUCL_COMMAND) 2>&1 | $(AWK) '/^; Converted /{next;} /^; Byte Compiling /{next;} /^; Compiling Creation Form/{next;} /^; Converted /{next;} {print;}' ;\
	fi

compile-with-$(ECL):
	@printf '\n\n\n\n'$(LINE)
	@if type -p $(ECL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Embeddable Common Lisp' ;\
		$(ECL_COMMAND) ;\
	fi

compile-with-$(GCL):
	@printf '\n\n\n\n'$(LINE)
	@printf 'gcl is not implemented yet.'
	@false
	@if type -p $(GCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with GNU Common Lisp' ;\
		$(GCL_COMMAND) ;\
	fi

compile-with-$(CCL):
	@printf '\n\n\n\n'$(LINE)
	@if type -p $(CCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Clozure Common Lisp' ;\
		$(CCL_COMMAND) ;\
	fi

compile-with-$(OPENMCL):
	@printf '\n\n\n\n'$(LINE)
	@if type -p $(OPENMCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Open Macintosh Common Lisp' ;\
		$(OPENMCL_COMMAND) ;\
	fi

compile-with-$(SBCL):
	@printf '\n\n\n\n'$(LINE)
	@if  type -p $(SBCL)  >/dev/null 2>&1 ; then \
		echo ';;;; Compiling with Steel-Banks Common Lisp' ;\
		$(SBCL_COMMAND) 2>&1 | $(EGREP) -v '^(; compiling top level form|; recognizing )' ;\
	fi

#### THE END ####
