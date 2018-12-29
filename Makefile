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
#    Copyright Pascal J. Bourguignon 2012 - 2016
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
#    along with this program.  If not, see <http://www.gnu.org/licenses/>
#******************************************************************************
all::

include implementations.mk

help::
	@printf 'NOTE: Most of this makefile is obsolete.\n      Please use ASDF or QUICKLISP to compile and load these systems.\n'
help::
	@for c in $(COMPILERS) ; do printf $(HELP_FMT) "compile-with-$$c" "Compile with $$c." ; done
	@printf $(HELP_FMT) 'all' 'Compile with all the available compilers.'
# Let's compile with all the available compilers ( $(GCL) not yet ).
all::  \
	compile-with-$(CLISP)     \
	compile-with-$(ECL)       \
	compile-with-$(SBCL)      \
	compile-with-$(OPENMCL)   \
	compile-with-$(CMUCL)     \
	compile-with-$(ALLEGRO)   \
	compile-with-$(ABCL)      \
	compile-with-$(CCL)

# compile-with-$(CMUCL)    breaks on decode-raw-cardinal in bencode...
# compile-with-$(ALLEGRO)  fails on posix-regexp out of memory...
# compile-with-$(ABCL)     chokes on unicode!
# compile-with-$(CCL)      doesn't run from Makefile (it runs well from the shell!).

#PREFIX=/usr/local
#PACKAGES:=$(shell get-directory SHARE_LISP | sed -e 's-/$$--')/packages
PREFIX=$(HOME)/quicklisp/local-projects
PACKAGES=$(PREFIX)

PACKAGE_PATH=com/informatimago
MODULES= announces common-lisp clext clmisc  clisp  susv3  rdp small-cl-pgms/botihn



help::
	@printf $(HELP_FMT) 'show-variables' 'Shows the variables'
show-variables::
	@printf $(VAR_FMT) 'Where non-lisp stuff will be installed:'  PREFIX         "$(PREFIX)"
	@printf $(VAR_FMT) 'Where lisp packages are installed.'       PACKAGES       "$(PACKAGES)"
	@printf $(VAR_FMT) 'Subpath for this library.'                PACKAGE_PATH   "$(PACKAGE_PATH)"
	@printf $(VAR_FMT) 'List of sub-modules of this project.'     MODULES        "$(MODULES)"
.PHONY::show-variables


MM=$(MAKE) $(MFLAGS) PREFIX=$(PREFIX)

clean::
	find . \( -name \*.fasl  -o -name \*.o -o -name \*.[dl]x64fsl \) -exec rm {} \;

help::
	@printf $(HELP_FMT) 'clean' 'Clean in each submodule directory.'
clean::
	@for module in $(MODULES) ; do \
		if [ -r "$$module/Makefile" ] ; then \
		  printf $(LINE) ;\
		  printf ";;;;;; CLEANING $$module\n" ;\
		  $(MM) MODULE_PATH=$(PACKAGES)/$(PACKAGE_PATH)/$$module -C $$module clean ;\
		fi ;\
	 done
	@find . \( -name \*.abcl-tmp \) -exec rm {} \;
	@printf $(LINE)

help::
	@printf $(HELP_FMT) 'install' 'Install each submodule directory specific stuff,'
	@printf $(HELP_FMT_2) 'then copy the whole library to '"$(PACKAGES)/$(PACKAGE_PATH)"
install::
	@echo "Obsolete.  Use quicklisp."
	@exit 1
#	 @for module in $(MODULES) ; do \
#	 	printf $(LINE) ;\
#	 	printf ";;;;;; INSTALLING STUFF SPECIFIC TO $$module \n" ;\
#	 	$(MM) MODULE_PATH=$(PACKAGES)/$(PACKAGE_PATH)/$$module  -C $$module install ;\
#	  done
#	 @printf $(LINE)
#	 @for module in $(MODULES) ; do \
#	  printf 'Installing the sources to %s\n' "$(PACKAGES)/$(PACKAGE_PATH)/$$module" ;\
#	  mkdir -p "$(PACKAGES)/$(PACKAGE_PATH)/$$module" || true ;\
#	  rm -rf   "$(PACKAGES)/$(PACKAGE_PATH)/$$module/"* ;\
#	  tar  --exclude \*~ -cf - $$module | tar -C "$(PACKAGES)/$(PACKAGE_PATH)/" -xf - ;\
#	  done
#	 @printf $(LINE)


help::
	@printf $(HELP_FMT)  'systems'   'Analyses the sources and generates the ASDF systems.'
	@printf $(HELP_FMT)  'summaries' 'Analyses the sources and generates the summary.html files.'
systems system system.asd summaries summary summary.html:
	@echo not implemented yet
	@false
.PHONY::systems system summaries summary


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
.PHONY::etags tags TAGS




WEBDOCDIR="$(HOME)/public_html/sites/com.informatimago.www"

help::
	@printf $(HELP_FMT)  'doc'           'Generates documentation lispdoc and upload.'
	@printf $(HELP_FMT)  'documentation' 'Generates the README.pdf file.'
	@printf $(HELP_FMT)  'html'          'Generates the html files.'
	@printf $(HELP_FMT)  'lispdoc'       "Generates the lispdoc documentation (in $(WEBDOCDIR))."
	@printf $(HELP_FMT)  'upload'        "Uploads $(WEBDOCDIR) to the web hosting server."

doc:documentation
documentation:readme
readme: README.pdf README.html README.md
doc-upload:documentation lispdoc upload
lispdoc:$(WEBDOCDIR)/develop/lisp/doc
html:: README.html
	$(MAKE) $(MFLAGS) -C clext/           html
	$(MAKE) $(MFLAGS) -C small-cl-pgms/   html

.PHONY::doc documentation readme doc-upload lispdoc html

$(WEBDOCDIR)/develop/lisp/doc:
	$(MAKE) $(MFLAGS) -C lispdoc

upload:
	$(MAKE) $(MFLAGS) -C $(WEBDOCDIR) update upload
.PHONY::upload

README.md:README
	pandoc -f rst -t markdown_github  -o README.md README

README.html:README
	( rst2html.py README > out.html && mv out.html README.html ) \
	|| ( rst2html README > out.html && mv out.html README.html )

README.pdf:README
	rst2pdf README

help::
	@printf $(HELP_FMT)  'show-pdfs'         'Opens README.pdf'
showpdf show-pdfs:README.pdf
	open README.pdf
.PHONY::showpdf show

help::
	@printf $(HELP_FMT)  'quicklisp-tag'    'Update the quicklisp tag on the remote repositories.'

help::
	@printf $(HELP_FMT)  'try'    'Try to compile all systems with sbcl and write report.'
try:
	$(call eval_with_sbcl,'(progn (load #P"~/quicklisp/setup.lisp") (funcall (find-symbol "QUICKLOAD" "QL") :com.informatimago.tools.try-systems) (funcall (find-symbol "TRY-SYSTEMS" "COM.INFORMATIMAGO.TOOLS.TRY-SYSTEMS")))')


help::
	@printf $(HELP_FMT)  'system-index.txt'    'Builds a list of all asd files.'
clean::
	-rm -f system-index.txt
system-index.txt:
	@find * -name \*.asd | while read line ; do echo "$${#line}:$${line}" ; done | sort -t: -k 1n | awk -F: '{print $$2;}' > system-index.txt

# quicklisp-tag: quicklisp-tag-remove quicklisp-tag-add
#
# quicklisp-tag-remove:
# 	git tag -d quicklisp
# 	git push origin    :refs/tags/quicklisp
# 	git push github    :refs/tags/quicklisp
# 	git push gitlab    :refs/tags/quicklisp
# 	git push framasoft :refs/tags/quicklisp
#
# quicklisp-tag-add:
# 	git tag -f -s quicklisp -m 'current version for quicklisp'
# 	git push --tags framasoft
# 	git push --tags gitlab
# 	git push --tags github
# 	git push --tags origin
# .PHONY::quicklisp-tag quicklisp-tag-remove quicklisp-tag-add

#### THE END ####

