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
MODULE=clmisc
PACKAGES:=$(shell get-directory SHARE_LISP)/packages
PACKAGE_PATH=com/informatimago/clmisc

include ../implementations.mk

all:: $(COMPILERS)

#### THE END ####
