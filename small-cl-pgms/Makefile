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
TARGETS=index.html what-implementation
HELPFORMAT="make %-20s \# %s\n"

all::help

help::
	@printf $(HELPFORMAT) generate "Generate the index.html page."
generate:$(TARGETS) access-rights clean


help::
	@printf $(HELPFORMAT) access-rights "Sets the access-rights for publication."
access-rights:
	publish .

help::
	@printf $(HELPFORMAT) clean "Erase backup and fasl files."

clean:
	-rm -rf  *~ *.lx*fsl

help::
	@printf $(HELPFORMAT) cleanall "make clean and erase targets."
cleanall:clean
	-rm -rf $(TARGETS)


help::
	@printf $(HELPFORMAT) install "Installs everything in PREFIX=$(PREFIX)"
install:install-what-implementation


help::
	@printf $(HELPFORMAT) index.html.in "Generates index.html.in"
index.html.in:init.lisp index.lisp Makefile
	clisp -norc -q -ansi  -E iso-8859-1  \
		-x '(progn (setf *load-verbose* nil) (load "index.lisp") (values))' \
	| tr -d '\015'\
	| sed -e 's/^;.*//' -e '/^NIL$$/d' \
	> index.html.in
index.html:index.html.in
	cp index.html.in index.html


#(with-open-file (*STANDARD-OUTPUT* "index.html" :direction :output :if-exists :supersede :if-does-not-exist :create)

help::
	@printf $(HELPFORMAT) what-implementation "Builds the what-implementation executable."
wi what-implementation:what-implementation.lisp
	echo '(load (compile-file "what-implementation.lisp"))' | ccl -n 


help::
	@printf $(HELPFORMAT) install-what-implementation "Install the what-implementation executable."
install-what-implementation:what-implementation
	cp what-implementation $PREFIX/bin
	chmod 755


#### THE END ####

