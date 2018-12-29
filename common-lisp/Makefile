#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           make
#SYSTEM:             posix
#USER-INTERFACE:     none
#DESCRIPTION
#
#    This makefile compiles my pure Common-Lisp packages.
#
#AUTHORS
#    <PJB> Pascal Bourguignon
#MODIFICATIONS
#    2010-06-27 <PJB> Factorized out into ../implementations.mk
#    2003-05-04 <PJB> Added this header.
#BUGS
#LEGAL
#    AGPL3
#
#    Copyright Pascal Bourguignon 2003 - 2016
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
#*****************************************************************************
PREFIX=/usr/local
all:: doc

include ../implementations.mk

help::
	@printf $(HELP_FMT) 'doc'  'Generate documentation.'
doc::documentation
documentation:: README.pdf
html:: README.html

help::
	@printf $(HELP_FMT) 'iana-character-sets.data' 'Downloads the IANA character sets data file.'
iana-character-sets.data:
	wget http://www.iana.org/assignments/character-sets -O $@

help::
	@printf $(HELP_FMT) 'system-graph.ps'  'Generate graph of systems.'
	@printf $(HELP_FMT) 'system-graph.eps' 'Generate graph of systems.'
	@printf $(HELP_FMT) 'system-graph.dot' 'Generate graph of systems.'
clean::
	-@rm -f system-graph.dot system-graph.ps system-graph.eps
system-graph.dot:Makefile gen-lib-graph.lisp $(shell find . -name \*.asd)
	ccl -n -e '(handler-case (progn  (load "gen-lib-graph.lisp") (ccl:quit)) (error (err) (princ err *error-output*) (terpri *error-output*) (ccl:quit 1)))'
system-graph.ps:system-graph.dot
	dot -Tps  -o system-graph.ps system-graph.dot
system-graph.eps:system-graph.ps
	ps2epsi system-graph.ps system-graph.eps
	lc -u system-graph.eps

help::
	@printf $(HELP_FMT) 'README.pdf'  'Generate README.'
clean::
	-@rm -f README.html README.pdf
README.pdf:README system-graph.eps
	rst2pdf < README > README.pdf
README.html:README system-graph.eps
	rst2html < README > README.html


compile::
	./compile.sh

install::
.PHONY:doc documentation html compile

#### THE END ####
