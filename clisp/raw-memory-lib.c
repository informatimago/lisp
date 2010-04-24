/*****************************************************************************
FILE:               raw-memory-lib.c
LANGUAGE:           c
SYSTEM:             POSIX
USER-INTERFACE:     NONE
DESCRIPTION
    
    Peek and poke.
    
AUTHORS
    <PJB> Pascal Bourguignon <pjb@informatimago.com>
MODIFICATIONS
    2004-12-05 <PJB> Created.
BUGS
LEGAL
    GPL
    
    Copyright Pascal Bourguignon 2004 - 2004
    
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
    
#include <stdio.h>
    unsigned char      peek8 (unsigned char*      address){return(*address);}
    unsigned short     peek16(unsigned short*     address){return(*address);}
    unsigned long      peek32(unsigned long*      address){return(*address);}
    unsigned long long peek64(unsigned long long* address){
        /* printf("peek64(%ulld)=%ulld\n",address,*address);*/
        return(*address);}
    void poke8(unsigned char* address,unsigned char value){
        (*address)=value;}
    void poke16(unsigned short* address,unsigned short value){
        (*address)=value;}
    void poke32(unsigned long* address,unsigned long value){
        (*address)=value;}
    void poke64(unsigned long long* address,unsigned long long value){
        /* printf("poke64(%ulld,%ulld)\n",address,value); */
        (*address)=value;}

/*** raw-memory-lib.c                 --                     --          ***/
