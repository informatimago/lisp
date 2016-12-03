#### -*- mode:makefile-gmake;coding:utf-8 -*-
####****************************************************************************
####FILE:               Makefile
####LANGUAGE:           makefile-gmake
####SYSTEM:             POSIX
####USER-INTERFACE:     NONE
####DESCRIPTION
####
####    Makefile for the small-cl-pgms.
####
####AUTHORS
####    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
####MODIFICATIONS
####    2013-01-12 <PJB> Added this header. Cleaned up targets.
####BUGS
####LEGAL
####    AGPL3
####
####    Copyright Pascal J. Bourguignon 2013 - 2013
####
####    This program is free software: you can redistribute it and/or modify
####    it under the terms of the GNU Affero General Public License as published by
####    the Free Software Foundation, either version 3 of the License, or
####    (at your option) any later version.
####
####    This program is distributed in the hope that it will be useful,
####    but WITHOUT ANY WARRANTY; without even the implied warranty of
####    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
####    GNU Affero General Public License for more details.
####
####    You should have received a copy of the GNU Affero General Public License
####    along with this program.  If not, see <http://www.gnu.org/licenses/>.
####****************************************************************************

PREFIX=/usr/local
HELPFORMAT="make %-20s \# %s\n"

all::help

# html target is one of the main target: it prepares the files needed
# for the web site.
generate::html
.PHONY:: generate

help::
	@printf $(HELPFORMAT) html "Generates html files from .html.in documents."

index.html.in:init.lisp index.lisp Makefile
	clisp -norc -q -ansi  -E utf-8  \
		-x '(progn (setf *load-verbose* nil) (load "index.lisp") (values))' \
	| tr -d '\015'\
	| sed -e 's/^;.*//' -e '/^NIL$$/d' \
	> index.html.in

html-files:index.html.in
	$(MAKE) $(MFLAGS) -C botihn html
	find . -name \*.html.in -print \
	| while read file ; do \
		echo ../tools/html-wrap-document.lisp \< "$$file" \> "$$(echo $$file | sed -e 's/\.in$$//')" ;\
		../tools/html-wrap-document.lisp < "$$file" > "$$(echo $$file | sed -e 's/\.in$$//')" ;\
	  done
html: html-files access-rights
.PHONY::html html-files


help::
	@printf $(HELPFORMAT) access-rights "Sets the access-rights for publication."
access-rights:
	publish .
.PHONY:: access-rights

help::
	@printf $(HELPFORMAT) clean "Erase backup and fasl files."
clean:
	-rm -rf  *~ *.[ld]x*fsl what-implementation

help::
	@printf $(HELPFORMAT) cleanall "make clean and erase targets."
cleanall:clean
	-rm -rf $(TARGETS)
.PHONY:: cleanall

help::
	@printf $(HELPFORMAT) install "Installs everything in PREFIX=$(PREFIX)"
install:install-what-implementation


help::
	@printf $(HELPFORMAT) what-implementation "Builds the what-implementation executable."
wi what-implementation:what-implementation.lisp
	( echo '(load (compile-file "what-implementation.lisp"))';\
	  echo '(com.informatimago.small-cl-pgms.what-implementation:generate-server-executable  "what-implementation")' ) | ccl -n
.PHONY:: wi

help::
	@printf $(HELPFORMAT) install-what-implementation "Install the what-implementation executable."
install-what-implementation:what-implementation
	chmod 755 what-implementation
	cp -p what-implementation $(PREFIX)/bin 2> /dev/null || ( rm $(PREFIX)/bin/what-implementation && cp -p what-implementation $(PREFIX)/bin )
.PHONY:: install-what-implementation


#### THE END ####

