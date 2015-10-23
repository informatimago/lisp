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
#    Copyright Pascal Bourguignon 2003 - 2015
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

help::
	@echo "make iana-character-sets.data # downloads the IANA character sets data file."
iana-character-sets.data:
	wget http://www.iana.org/assignments/character-sets -O $@




system-graph.ps:system-graph.dot
	dot -Tps  -o system-graph.ps system-graph.dot
system-graph.eps:system-graph.ps 
	ps2epsi system-graph.ps system-graph.eps
	lc -u system-graph.eps

README.pdf:README system-graph.eps
	rst2pdf < README > README.pdf
README.html:README system-graph.eps
	rst2html < README > README.html

doc::documentation
documentation:: README.pdf
html:: README.html

compile::
	./compile.sh

install::
.PHONY:doc documentation html compile

#### THE END ####
