#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           make
#SYSTEM:             posix
#USER-INTERFACE:     none
#DESCRIPTION
#    
#    This makefile compiles additionnal code for the clisp specific packages.
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
#    Copyright Pascal Bourguignon 2003 - 2010
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
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#*****************************************************************************
PREFIX = /usr/local

all:: libraw-memory.so

# Libraries are loaded at run-time, so they must be installed 
# in some public place, or distributed along with the applications.
# There would be no point to keeping them in asdf-binary-location places.

raw-memory-lib.o:raw-memory-lib.c
	$(CC) -shared -fPIC -g -O3 -c -o $@ $<

libraw-memory.so:raw-memory-lib.o
	if [ $(shell uname) = Darwin ] ;\
	then $(LD) -dynamic                -o $@ $< ;\
	else $(LD) -shared -fPIC -g        -o $@ $< ;\
	fi

install::
	-mkdir $(PREFIX)/lib
	install -m 644 libraw-memory.so  $(PREFIX)/lib/libraw-memory.so

#### THE END ####
