/*****************************************************************************
FILE:               raw-memory-lib.c
LANGUAGE:           c
SYSTEM:             POSIX
USER-INTERFACE:     NONE
DESCRIPTION

    Peek and poke.

AUTHORS
    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
MODIFICATIONS
    2010-11-04 <PJB> Beautified.  Made use of uint*_t types from <stdint.h>.
    2004-12-05 <PJB> Created.
BUGS
LEGAL
    GPL

    Copyright Pascal J. Bourguignon 2004 - 2010

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
    PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this program; if not, write to the Free
    Software Foundation, Inc., 59 Temple Place, Suite 330,
    Boston, MA 02111-1307 USA
*****************************************************************************/

#include <stdint.h>

uint8_t  peek8 (const uint8_t * address){ return(*address); }
uint16_t peek16(const uint16_t* address){ return(*address); }
uint32_t peek32(const uint32_t* address){ return(*address); }
uint64_t peek64(const uint64_t* address){ return(*address); }

void     poke8 (uint8_t * address,uint8_t  value){ (*address)=value; }
void     poke16(uint16_t* address,uint16_t value){ (*address)=value; }
void     poke32(uint32_t* address,uint32_t value){ (*address)=value; }
void     poke64(uint64_t* address,uint64_t value){ (*address)=value; }

/**** THE END ****/
