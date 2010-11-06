#!/bin/bash
#******************************************************************************
#FILE:               make-depends
#LANGUAGE:           bash shell script
#SYSTEM:             POSIX
#USER-INTERFACE:     POSIX
#DESCRIPTION
#
#    This scripts run make-depends.lisp script to create a Makefile.depend
#
#USAGE
#
#    make-depends [ -Iinclude-dir | lisp-object-file ] ...
#
#AUTHORS
#    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
#MODIFICATIONS
#    2002-12-04 <PJB> Created.
#BUGS
#LEGAL
#    Copyright Pascal J. Bourguignon 2002 - 2002
#
#    This script is free software; you can redistribute it and/or
#    modify it under the terms of the GNU  General Public
#    License as published by the Free Software Foundation; either
#    version 2 of the License, or (at your option) any later version.
#
#    This script is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    General Public License for more details.
#
#    You should have received a copy of the GNU General Public
#    License along with this library; see the file COPYING.LIB.
#    If not, write to the Free Software Foundation,
#    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#******************************************************************************
pname="$(basename $0)"
pblan="${pname//?/ }"
dname="$(dirname $0)"

function usage () {
    echo "${pname} usage:"
    echo "    ${pname} [--help|-h"
    echo "    ${pblan} |-Iinclude-dir"
    echo "    ${pblan} |-Ppreloaded-package"
    echo "    ${pblan} |-T host logical-path physical-path"
    echo "    ${pblan} |--independent-dependency-files|-i"
    echo "    ${pblan} |--trace|-t|--closure|-c"
    echo "    ${pblan} |lisp-object-file]... "
}

DIR="$(dirname $0)"
IDIRES=""
PPACKS=""
LPTRAN=""
OFILES=""
IDF=NIL
VERBOSE=""
closure=0

while [ $# -gt 0 ] ; do
    arg="$1"
    case "$arg" in
    -h|--help)
        usage
        exit 0
        ;;
    -t|--trace)
        VERBOSE=" :VERBOSE T "
        ;;
    -c|--closure) 
        closure=1
        ;;
    -I*)
        IDIRES="$IDIRES \"${arg/-I}\""
        ;;
    -P*)
        PPACKS="$PPACKS ${arg/-P}"
        ;;
    -T)
        if [ $# -gt 3 ] ; then
            LPTRAN="$LPTRAN ( \"$2\"  \"$3\" \"$4\" )"
            shift 3
        else
            echo "${pname}: Missing arguments after -T."
            usage
            exit 1
        fi
        ;;
    --independent-dependency-files|-i)
        IDF=T
        ;;
    -*)
        echo "${pname}: Invalid option '$arg'."
        usage
        exit 1
        ;;
    *)
        OFILES="$OFILES \"$arg\""
        ;;
    esac
    shift
done

function docmd () {
    echo "$@" | sed -e 's/^/### /'
    "$@" | sed -e 's/^\*\*\* /#!!! /'
}

if [ $closure -ne 0 ] ; then
    docmd clisp -ansi -q  -x "(SETF *LOAD-VERBOSE* NIL) (PROG1 (VALUES) (LOAD \"$dname/make-depends.lisp\")) (MAPC (LAMBDA (ITEM) (PRINC ITEM) (PRINC \" \")) (NREVERSE (LIST-TO-SET (NREVERSE (COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS:GET-CLOSED-DEPENDENCIES '($OFILES) '($PPACKS) '($LPTRAN) '($IDIRES) $VERBOSE)))) (EXT:QUIT)"
else
    docmd clisp -ansi -q  -x "(SETF *LOAD-VERBOSE* NIL) (PROG1 (VALUES) (LOAD \"$dname/make-depends.lisp\")) (PROG1 (VALUES) (FORMAT T \"~&\") (COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS:MAKE-DEPENDS '($OFILES) '($PPACKS) '($LPTRAN) '($IDIRES) :IDF $IDF $VERBOSE)) (EXT:QUIT)"
fi | egrep -v '^NIL|^;; Load'


#### make-depends.sh                  --                     --          ####
