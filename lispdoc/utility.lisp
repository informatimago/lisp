;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               utility.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Exports a few utility operators.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-04 <PJB> Extracted from lispdoc.lisp
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
;;;;    
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;    
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;    
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;    
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(in-package "COM.INFORMATIMAGO.LISPDOC.UTILITY")


(define-modify-macro appendf (&rest args) 
    append "Append onto list")


;;;; THE END ;;;;

