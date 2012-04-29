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
#    AGPL3
#    
#    Copyright Pascal J. Bourguignon 2012 - 2012
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Affero General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Affero General Public License for more details.
#    
#    You should have received a copy of the GNU Affero General Public License
#    along with this program.  If not, see http://www.gnu.org/licenses/
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
#PACKAGES:=$(shell get-directory SHARE_LISP | sed -e 's-/$$--')/packages
PREFIX=$(HOME)/quicklisp/local-projects
PACKAGES=$(PREFIX)

PACKAGE_PATH=com/informatimago
MODULES= common-lisp clext clmisc  clisp  susv3  rdp



help::
	@printf $(HELP_FMT) 'show-variables' 'Shows the variables'
	@printf $(HELP_FMT_2) 'then copy the whole library to '"$(PACKAGES)/$(PACKAGE_PATH)"
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
	@for module in $(MODULES) ; do \
	 printf 'Installing the sources to %s\n' "$(PACKAGES)/$(PACKAGE_PATH)/$$module" ;\
	 mkdir -p "$(PACKAGES)/$(PACKAGE_PATH)/$$module" || true ;\
	 rm -rf   "$(PACKAGES)/$(PACKAGE_PATH)/$$module/"* ;\
	 tar  --exclude \*~ -cf - $$module | tar -C "$(PACKAGES)/$(PACKAGE_PATH)/" -xf - ;\
	 done
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




help::
	@printf $(HELP_FMT)  'documentation' 'Generates the README.pdf file.'
documentation: README.pdf README.html

README.html:README
	rst2html.py README > README.html

README.pdf:README
	rst2pdf README

showpdf show-pdfs:README.pdf
	open README.pdf


#### THE END ####

