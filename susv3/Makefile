#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           make
#SYSTEM:             posix
#USER-INTERFACE:     none
#DESCRIPTION
#    
#    This makefile compiles my SUSv3 packages: packages that depend 
#    on SUSV3 API.
#    
#AUTHORS
#    <PJB> Pascal Bourguignon
#MODIFICATIONS
#    2010-06-27 <PJB> Factorized out into ../implementations.mk
#    2005-01-06 <PJB> Created.
#BUGS
#LEGAL
#    GPL
#    
#    Copyright Pascal Bourguignon 2004 - 2010
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
MAKEDIR := $(HOME)/src/common/makedir
PREFIX  := /usr/local
MODULE=susv3
PACKAGES:=$(shell get-directory SHARE_LISP)/packages
PACKAGE_PATH=com/informatimago/susv3

include ../implementations.mk

all:: clisp

#### THE END ####
