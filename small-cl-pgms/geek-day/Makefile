#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           makefile
#SYSTEM:             POSIX
#USER-INTERFACE:     NONE
#DESCRIPTION
#    
#    XXX
#    
#AUTHORS
#    <PJB> Pascal Bourguignon <pjb@informatimago.com>
#MODIFICATIONS
#    2004-01-17 <PJB> Fixed Makefile.
#    2004-01-17 <PJB> Created
#BUGS
#LEGAL
#    AGPL3
#    
#    Copyright Pascal Bourguignon 2004 - 2015
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

CLISP_SCRIPTS = geek-day
CLISP_SOURCES = geek-day.lisp

geek-day_PKGNAME = COM.INFORMATIMAGO.COMMON-LISP.GEEK-DAY
geek-day_ROOT    = geek-day.fas
geek-day_CLISP_OBJECTS = $(geek-day_ROOT)


SBCL_PROGRAMS = geek-day-sb
SBCL_SOURCES  = geek-day.lisp

geek-day-sb_PKGNAME = COM.INFORMATIMAGO.COMMON-LISP.GEEK-DAY
geek-day-sb_ROOT    = geek-day.fasl
geek-day-sb_SBCL_OBJECTS = $(geek-day-sb_ROOT)


CLISP_LINKINGSET = full
CLISP_FLAGS = $(CLISP_FLAGS_DEFAULT) -K $(CLISP_LINKINGSET)
CMUCL_FLAGS = $(CMUCL_FLAGS_DEFAULT)
SBCL_FLAGS = $(SBCL_FLAGS_DEFAULT)

all :: scripts programs
install :: all install-scripts install-programs

include $(MAKEDIR)/lisp
-include Makefile.depend


#### Makefile                         --                     --          ####
