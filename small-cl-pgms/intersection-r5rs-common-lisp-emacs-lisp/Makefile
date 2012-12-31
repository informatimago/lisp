#### -*- mode:makefile-gmake;coding:utf-8 -*-
####****************************************************************************
####FILE:               Makefile
####LANGUAGE:           makefile-gmake
####SYSTEM:             POSIX
####USER-INTERFACE:     NONE
####DESCRIPTION
####    
####    Runs the intersection programs with various lisps/schemes.
####    
####AUTHORS
####    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
####MODIFICATIONS
####    2012-12-31 <PJB> Created this Makefile.
####BUGS
####LEGAL
####    AGPL3
####    
####    Copyright Pascal J. Bourguignon 2012 - 2012
####    
####    This program is free software: you can redistribute it and/or
####    modify it under the terms of the GNU Affero General Public
####    License as published by the Free Software Foundation, either
####    version 3 of the License, or (at your option) any later
####    version.
####    
####    This program is distributed in the hope that it will be useful,
####    but WITHOUT ANY WARRANTY; without even the implied warranty of
####    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
####    GNU Affero General Public License for more details.
####    
####    You should have received a copy of the GNU Affero General Public License
####    along with this program.  If not, see <http://www.gnu.org/licenses/>.
####****************************************************************************


LISP=clisp -ansi -norc -q
LISP_TAIL=
EMACS=emacs -Q --batch -l
EMACS_TAIL=-q
SCHEME=bigloo -no-hello -q -w -load 
SCHEME_TAIL=-eval '(quit)'
LINE="======================================================================"

all: test-intersection-cl-el-r5rs test-intersection-cl-r5rs
test-intersection-cl-el-r5rs:
	@printf "\n\n;;%s\n;; Test %s\n;;\n" "$(LINE)" "intersection-cl-el-r5rs"
	@printf "\n;; %s\n" "Common Lisp"
	$(LISP) intersection-cl-el-r5rs.lisp $(LISP_TAIL)
	@printf "\n;; %s\n" "Emacs Lisp"
	$(EMACS) intersection-cl-el-r5rs.lisp $(EMACS_TAIL)
	@printf "\n;; %s\n" "Scheme"
	$(SCHEME) intersection-cl-el-r5rs.lisp $(SCHEME_TAIL)

test-intersection-cl-r5rs:
	@printf "\n\n;;%s\n;; Test %s\n;;\n" "$(LINE)" "intersection-cl-r5rs"
	$(LISP) intersection-cl-r5rs.lisp

#### THE END ####
