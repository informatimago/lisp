#******************************************************************************
#FILE:				Makefile
#LANGUAGE:			make
#SYSTEM:			UNIX
#USER-INTERFACE:	None
#DESCRIPTION
#
#	This Makefile tells how to compile the lisp libraries.
#
#	Input variables are:
#
#       $(PREFIX)       where the non-lisp stuff will be installed
#                       such as in $(PREFIX)/bin/, $(PREFIX)/lib/.
#
#   
#                       It will be created a subdirectory in this
#                       $(PREFIX) named after the library:
#                       
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
all::

include implementations.mk


help::
	@for c in $(COMPILERS) ; do printf $(HELP_FMT) "compile-with-$$c" "Compile with $$c." ; done
	@printf $(HELP_FMT) 'all' 'Compile with all the available compilers.'
# Let's compile with all the available compilers ( $(GCL) not yet ).
# all:: $(ABCL) $(ALLEGRO) $(CCL) $(CLISP) $(ECL) $(SBCL) $(CMUCL) $(OPENMCL)
all::  compile-with-$(CLISP) compile-with-$(ECL) compile-with-$(SBCL) compile-with-$(OPENMCL) 
# compile-with-$(CMUCL)     breaks on decode-raw-cardinal in bencode...
# compile-with-$(ALLEGRO)  fails on posix-regexp out of memory...
# compile-with-$(ABCL)     chokes on unicode!
# compile-with-$(CCL)      doesn't run from Makefile (it runs well from the shell!).



#PREFIX=/usr/local
PREFIX=$(HOME)/quicklisp/local-projects
PACKAGES:=$(shell get-directory SHARE_LISP | sed -e 's-/$$--')/packages
PACKAGE_PATH=com/informatimago
MODULES= common-lisp clext clmisc  sbcl  clisp  susv3



show-variables::
	@printf $(VAR_FMT) 'Where non-lisp stuff will be installed:'  PREFIX         "$(PREFIX)"
	@printf $(VAR_FMT) 'Where lisp packages are installed.'       PACKAGES       "$(PACKAGES)"
	@printf $(VAR_FMT) 'Subpath for this library.'                PACKAGE_PATH   "$(PACKAGE_PATH)"
	@printf $(VAR_FMT) 'List of sub-modules of this project.'     MODULES        "$(MODULES)"


MM=$(MAKE) $(MFLAGS) PREFIX=$(PREFIX)

help::
	@printf $(HELP_FMT) 'clean' 'Clean in each submodule directory.'
clean::
	@for module in $(MODULES) ; do \
		printf $(LINE) ;\
		printf ";;;;;; CLEANING $$module\n" ;\
		$(MM) MODULE_PATH=$(PACKAGES)/$(PACKAGE_PATH)/$$module -C $$module $(CLEAN) ;\
	 done
	@printf $(LINE)

help::
	@printf $(HELP_FMT) 'install' 'Install each submodule directory specific stuff,'
	@printf $(HELP_FMT_2) 'then copy the whole library to '"$(PACKAGES)/$(PACKAGE_PATH)"
install::
	@for module in $(MODULES) ; do \
		printf $(LINE) ;\
		printf ";;;;;; INSTALLING STUFF SPECIFIC TO $$module \n" ;\
		$(MM) MODULE_PATH=$(PACKAGES)/$(PACKAGE_PATH)/$$module  -C $$module install ;\
	 done
	@printf $(LINE)
	@printf 'Installing the whole sources to %s\n' "$(PACKAGES)/$(PACKAGE_PATH)"
	-@mkdir -p "$(PACKAGES)/$(PACKAGE_PATH)"
	-@rm -rf   "$(PACKAGES)/$(PACKAGE_PATH)/"*
	@tar --exclude \*~ -cf - . | tar -C "$(PACKAGES)/$(PACKAGE_PATH)/" -xvf -
	@printf $(LINE)


help::
	@printf $(HELP_FMT)  'make systems'   'Analyses the sources and generates the ASDF systems.'
	@printf $(HELP_FMT)  'make summaries' 'Analyses the sources and generates the summary.html files.'
systems system system.asd summaries summary summary.html:
	@echo not implemented yet
	@false



help::
	@printf $(HELP_FMT)  'tags' 'Generate the TAGS file, for emacs.'
etags tags TAGS::
	find $$(pwd) \( \
		   -name '[^#.]*.lisp' -o -name '[^#.]*.lsp'  -o -name '[^#.]*.cl' \
		-o -name '[^#.]*.h'    -o -name '[^#.]*.c'    -o -name '[^#.]*.m'  \
		-o -name '[^#.]*.hh'   -o -name '[^#.]*.cc'   -o -name '[^#.]*.mm' \
		\) -print \
	| tee -a /dev/stderr -a /dev/stdout \
	| etags -
	@printf ';; Done.\n'

#### THE END ####

