#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           makefile
#SYSTEM:             unix
#USER-INTERFACE:     none
#DESCRIPTION
#    
#    Makefile for the SBCL specific lisp programs and packages.
#    
#AUTHORS
#    <PJB> Pascal Bourguignon
#MODIFICATIONS
#    2010-06-27 <PJB> Factorized out into ../implementations.mk
#    2003-05-08 <PJB> Created.
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
MODULE=sbcl
PACKAGES:=$(shell get-directory SHARE_LISP)/packages
PACKAGE_PATH=com/informatimago/sbcl

include ../implementations.mk

all:: sbcl

#### THE END ####
