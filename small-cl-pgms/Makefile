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
TARGETS=index.html.in what-implementation

help::
	@echo "$(MAKE) generate                     # generates $(TARGETS)"
all:$(TARGETS) access-rights clean

help::
	@echo "$(MAKE) install                      # Installs everything PREFIX=$(PREFIX)"
install:install-what-implementation



help::
	@echo "$(MAKE) access-rights                # publishes this subdirectory."
access-rights:
	publish .


help::
	@echo "$(MAKE) clean                        # Remove backups."
	@echo "$(MAKE) cleanall                     # Remove backups and generated files."
clean:
	-rm -rf  *~
cleanall:clean
	-rm -rf $(TARGETS)

#(with-open-file (*STANDARD-OUTPUT* "index.html" :direction :output :if-exists :supersede :if-does-not-exist :create)


help::
	@echo "$(MAKE) index                        # Generates index.html.in"
index index.html.in:init.lisp index.lisp Makefile
	clisp -norc -q -ansi  -E iso-8859-1  \
		-x '(progn (setf *load-verbose* nil) (load "index.lisp") (values))' \
	| tr -d '\015'\
	| sed -e 's/^;.*//' -e '/^NIL$$/d' \
	> index.html.in




help::
	@echo "$(MAKE) what-implementation          # Compile the what-implementation program."
wi what-implementation:what-implementation.lisp
	echo '(load (compile-file "what-implementation.lisp"))' | ccl -n 
help::
	@echo "$(MAKE) install-what-implementation  # Installs the what-implementation program PREFIX=$(PREFIX)"
install-what-implementation:what-implementation
	cp what-implementation $PREFIX/bin
	chmod 755




#### THE END ####

