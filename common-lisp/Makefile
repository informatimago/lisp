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
#    GPL
#    
#    Copyright Pascal Bourguignon 2003 - 2010
#    mailto:pjb@informatimago.com
#    
#    This program is free software; you can redistribute it and/or
#    modify it under the terms of the GNU General Public License
#    as published by the Free Software Foundation; either version
#    2 of the License, or (at your option) any later version.
#    
#    This program is distributed in the hope that it will be
#    useful, but WITHOUT ANY WARRANTY; without even the implied
#    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#    PURPOSE.  See the GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public
#    License along with this program; if not, write to the Free
#    Software Foundation, Inc., 59 Temple Place, Suite 330,
#    Boston, MA 02111-1307 USA
#*****************************************************************************
SHELL=/bin/bash
TARGET  := $(shell uname)
PREFIX  := /usr/local
MODULE=common-lisp
PACKAGES:=$(shell get-directory SHARE_LISP)/packages
PACKAGE_PATH=com/informatimago/common-lisp

include ../implementations.mk

COMPILES = '(load "compile-with-asdf.lisp")'
all:: $(COMPILERS) documents


install::
	-mkdir -p $(PACKAGES)/$(PACKAGE_PATH)
	tar --exclude \*~  -cf - . | tar -C $(PACKAGES)/$(PACKAGE_PATH)/ -xvf -



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

documents: README.pdf

compile:
	./compile.sh

#### THE END ####
