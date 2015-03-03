;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;AUTHORS
;;;;    <XACH> Zachary Beane <xach@xach.com>,
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-03 <PJB> Completed corrections to pass package ansi-tests.
;;;;    2012-03-30 <PJB> Added checks, made the API conforming to CL.
;;;;    2012-03-30 <PJB> Added this header; Removed "Z" prefix to CL
;;;;                     symbol names; shadowed and exported them.
;;;;BUGS
;;;;
;;;;    make-load-form for packages should probably return two forms, since
;;;;    packages can have circular dependencies.
;;;;
;;;;    Are missing some standard restarts to correct
;;;;    conflicts. (choosing one or the other symbol, doing the same
;;;;    for all conflicts, etc).
;;;;
;;;;LEGAL
;;;;    Copyright (c) 2012 Zachary Beane <xach@xach.com>, All Rights Reserved
;;;;    Copyright (c) 2012 Pascal J. Bourguignon <pjb@informatimago.com>, All Rights Reserved
;;;;
;;;;    Redistribution and use in source and binary forms, with or without
;;;;    modification, are permitted provided that the following conditions
;;;;    are met:
;;;;
;;;;      * Redistributions of source code must retain the above copyright
;;;;        notice, this list of conditions and the following disclaimer.
;;;;
;;;;      * Redistributions in binary form must reproduce the above
;;;;        copyright notice, this list of conditions and the following
;;;;        disclaimer in the documentation and/or other materials
;;;;        provided with the distribution.
;;;;
;;;;    THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;;    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;;    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;;    ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;;    DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;;    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;;    GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;;    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;;    WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;;    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;;    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;**************************************************************************


(cl:defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.PACKAGE"
  (:use "COMMON-LISP")
  (:nicknames "ZPACK")
  (:shadow "SIMPLE-TYPE-ERROR"
           "PRINT-NOT-READABLE" "PRINT-NOT-READABLE-OBJECT")
  (:shadow . #1=("SYMBOL"
                 "SYMBOLP" "MAKE-SYMBOL" "SYMBOL-NAME" "SYMBOL-PACKAGE" 
                 "SYMBOL-VALUE" "SYMBOL-FUNCTION" "SYMBOL-PLIST"
                 "BOUNDP" "FBOUNDP"
                 "KEYWORD" "KEYWORDP"
                 "PACKAGE"
                 "PACKAGEP"  "MAKE-PACKAGE" "FIND-PACKAGE" "DELETE-PACKAGE"
                 "FIND-SYMBOL" "IMPORT" "INTERN" "SHADOW" "SHADOWING-IMPORT"
                 "EXPORT" "UNEXPORT" "UNINTERN" "USE-PACKAGE"
                 "UNUSE-PACKAGE" "PACKAGE-NAME" "PACKAGE-NICKNAMES"
                 "PACKAGE-USE-LIST" "PACKAGE-USED-BY-LIST" "PACKAGE-SHADOWING-SYMBOLS"
                 "LIST-ALL-PACKAGES" "FIND-ALL-SYMBOLS" "RENAME-PACKAGE"
                 "*PACKAGE*"
                 "WITH-PACKAGE-ITERATOR"
                 "DO-SYMBOLS" "DO-EXTERNAL-SYMBOLS" "DO-ALL-SYMBOLS"
                 "DEFPACKAGE" "IN-PACKAGE" 
                 "PACKAGE-ERROR" "PACKAGE-ERROR-PACKAGE"))
  (:export . #1#)
  ;; Additionnal conditions:
  (:export "PACKAGE-EXISTS-ERROR"
           "PACKAGE-DOES-NOT-EXIST-ERROR"
           "SYMBOL-CONFLICT-ERROR"
           "SYMBOL-CONFLICT-EXISTING-SYMBOL"
           "SYMBOL-CONFLICT-IMPORTED-SYMBOL"
           "PACKAGE-DOCUMENTATION")
  (:documentation "
This package implements the Common Lisp package system.

<Xach> The basic idea of that file is that the semantics of the CL
package system can be implemented by an object with three special
kinds of tables (present-table, shadowing-table, external-table)
and two lists (used-packs, used-by-packs). The rest is
implementation.

It shadows the CL symbols dealing with packages, and exports
replacements that implement the package system anew.


Additionnal symbol exported:

    PACKAGE-EXISTS-ERROR
    PACKAGE-DOES-NOT-EXIST-ERROR
    SYMBOL-CONFLICT-ERROR
    SYMBOL-CONFLICT-EXISTING-SYMBOL
    SYMBOL-CONFLICT-IMPORTED-SYMBOL
    PACKAGE-DOCUMENTATION


License:

    BSD

    Copyright (c) 2012 Zachary Beane <xach@xach.com>, All Rights Reserved
    Copyright (c) 2012 Pascal J. Bourguignon <pjb@informatimago.com>, All Rights Reserved

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

      * Redistributions in binary form must reproduce the above
        copyright notice, this list of conditions and the following
        disclaimer in the documentation and/or other materials
        provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
    ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
    DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
    GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
    WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"))


;;;; THE END ;;;;
