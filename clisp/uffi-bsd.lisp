;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               uffi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             clisp
;;;;USER-INTERFACE:     NONE
;;;;NOWEB:              T
;;;;DESCRIPTION
;;;;
;;;;    This API is obsolete. See: CFFI
;;;;
;;;;    This is a UFFI layer over the clisp native FFI.
;;;;
;;;;
;;;;    Programs running on CLISP may set CUSTOM:*FOREING-ENCODING*
;;;;    and this will be honored in the conversion of strings between
;;;;    Lisp and C by the underlying FFI.
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    <KMR> Kevin M. Rosenberg
;;;;MODIFICATIONS
;;;;    2004-07-29 <PJB> Implemented LOAD-FOREIGN-LIBRARY, FIND-FOREIGN-LIBRARY.
;;;;    2003-06-03 <PJB> Created.
;;;;                     Some code taken from Kevin M. Rosenberg's UFFI 1.2.15.
;;;;BUGS
;;;;    Not tested yet.
;;;;
;;;;    FIND-FOREIGN-LIBRARY can't do its work portably for the ill definition
;;;;    of COMMON-LISP:DIRECTORY.  Only a unix implementation is provided.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2015
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.CLISP.UFFI"
  (:nicknames "UFFI")
  (:documentation "
This package implements over clisp native FFI the UFFI API as defined in
'UFFI Reference Guide' by Kevin M. Rosenberg, Heart Hospital of New Mexico.

The version of the UFFI implemented here is uffi-1.2.15.

URL:    <http://uffi.b9.com/manual/book1.html>
URL:    <http://uffi.b9.com/>

LEGAL:  Copyright Pascal J. Bourguignon 2003 - 2004

      Redistribution and use in source and binary forms, with or
      without modification, are permitted provided that the following
      conditions are met:
      
         1. Redistributions of source code must retain the above
            copyright notice, this list of conditions and the
            following disclaimer.
      
         2. Redistributions in binary form must reproduce the above
            copyright notice, this list of conditions and the
            following disclaimer in the documentation and/or other
            materials provided with the distribution.
      
         3. The name of the author may not be used to endorse or
            promote products derived from this software without
            specific prior written permission.
      
      THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY
      EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
      THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
      PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR
      BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
      EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
      TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
      DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
      ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
      LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
      IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
      THE POSSIBILITY OF SUCH DAMAGE.
")
  (:use "COMMON-LISP") ;; actually: FROM COMMON-LISP IMPORT ALL;
  ;; really: USE FFI,CUSTOM,EXT;
  (:export

   ;; immediate types
   "DEF-TYPE"
   "DEF-CONSTANT" ;; Don't use it!
   "DEF-FOREIGN-TYPE"
   "NULL-CHAR-P"

   ;; aggregate types
   "DEF-ENUM"
   "DEF-STRUCT"
   "GET-SLOT-VALUE"
   "GET-SLOT-POINTER"
   "DEF-ARRAY-POINTER"
   "DEREF-ARRAY"
   "DEF-UNION"

   ;; objects
   "ALLOCATE-FOREIGN-OBJECT"
   "FREE-FOREIGN-OBJECT"
   "WITH-FOREIGN-OBJECT"
   "WITH-FOREIGN-OBJECTS"
   "SIZE-OF-FOREIGN-TYPE"
   "POINTER-ADDRESS"
   "DEREF-POINTER"
   "ENSURE-CHAR-CHARACTER"
   "ENSURE-CHAR-INTEGER"
   "NULL-POINTER-P"
   "MAKE-NULL-POINTER"
   "+NULL-CSTRING-POINTER"+
   "CHAR-ARRAY-TO-POINTER"

   ;; string functions
   "CONVERT-FROM-CSTRING"
   "CONVERT-TO-CSTRING"
   "FREE-CSTRING"
   "WITH-CSTRING"
   "WITH-CSTRINGS"
   "CONVERT-FROM-FOREIGN-STRING"
   "CONVERT-TO-FOREIGN-STRING"
   "ALLOCATE-FOREIGN-STRING"
   "WITH-FOREIGN-STRING"

   ;; function call
   "DEF-FUNCTION"

   ;; Libraries
   "FIND-FOREIGN-LIBRARY"
   "LOAD-FOREIGN-LIBRARY"
   "DEFAULT-FOREIGN-LIBRARY-TYPE"

   ;; OS
   "RUN-SHELL-COMMAND" ;; This is not used anywhere by UFFI: what's the use?
   ;; Don't use it: if you need such a function, use a POSIX or an OS API,
   ;; not a UFFI.

   )) ;;COM.INFORMATIMAGO.CLISP.UFFI
(in-package "COM.INFORMATIMAGO.CLISP.UFFI")
(provide :uffi) ;; Some client code use REQUIRE! Can you imagine that?



;; In general functions defined with UFFI (DEF-FUNCTION) do not convert
;; between Lisp objects and C objects. This is to be done by the client
;; code.


;; FFI provides for specification of the external language, with :C (K&C),
;; :STDC (ANSI C) or :STDC-STDCALL (ANSI C with stdcall).
;; UFFI does not.


;; UFFI does not allow the definition of a C variable (extern).
;; FFI does, with def-c-var.
;; However, we could play C tricks to get a UFFI pointer to a C variable.
;; (Is there any other such variable than errno?)



;; In FFI, c-type  as defined with def-c-type are symbols  used as key in
;; an internal hash table where c-type objects are stored.


;; (uffi:def-type the-struct-type-def the-struct-type)
;; (let ((a-foreign-struct (allocate-foreign-object 'the-struct-type)))
;;   (declare 'the-struct-type-def a-foreign-struct)
;;   (get-slot-value a-foreign-struct 'the-struct-type 'field-name))


;; There's no UFFI type to specify function types.

;; UFFI:DEF-FUNCTION corresponds to FFI:DEF-CALL-OUT
;; There's none corresponding to FFI:DEF-CALL-IN


;; UFFI has only :IN arguments.
;; FFI has also :INOUT and :OUT arguments.
;; We'll use :ALLOCATION :NONE and :IN arguments, and manage our own buffers.

;; FFI:
;; Passing FFI:C-STRUCT, FFI:C-UNION, FFI:C-ARRAY, FFI:C-ARRAY-MAX values
;; as arguments (not via pointers) is only possible to the extent the C
;; compiler supports it. Most C compilers do it right, but some C
;; compilers (such as gcc on hppa) have problems with this.


;; UFFI: The values must be converted between Lisp types and C types by the
;;       client code.
;; FFI: The values are converted automatically between Lisp and C, depending
;;      on the FFI-C-TYPE-TO-CL-TYPE table and specifications of C types.
;;
;; Therefore: UFFI :CSTRING are implemented as CL:STRING
;;            UFFI :CENUM   are implemented as CL:INTEGER
;;            UFFI :CSTRUCT are implemented as CL:DEFSTRUCT
;;            UFFI :CARRAY  are implemented as CL:ARRAY ?






;; (* TYPE)
;; (ENUM   (FIELD VALUE)...)
;; (STRUCT (FIELD TYPE)...)
;; (UNION  (FIELD TYPE)...)
;; (* (ARRAY TYPE))
;; :struct-pointer
;; :pointer-self


;; ;; immediate types
;;    "DEF-TYPE"             --> LISP DEFTYPE  WITH CONVERSION TO :CL
;;
;;    "DEF-FOREIGN-TYPE"     --> C    typedef type name;
;;                           --> DEF-C-TYPE
;;
;;    "NULL-CHAR-P"
;;
;;    ;; aggregate types
;;    "DEF-ENUM"             --> C    typedef enum {...} name;
;;                           --> DEF-C-ENUM DEFTYPE
;;                             
;;    "DEF-STRUCT"           --> C    typedef struct name {...} name;
;;                           --> DEF-C-STRUCT
;;
;;    "GET-SLOT-VALUE"
;;    "GET-SLOT-POINTER"
;;    "DEF-ARRAY-POINTER"    --> C    typedef type* name;
;;                           --> DEF-C-TYPE
;;
;;    "DEREF-ARRAY"
;;
;;    "DEF-UNION"            --> C    typedef union {...} name;
;;                           --> DEF-C-TYPE
;;
;;
;;    ;; objects
;;    "ALLOCATE-FOREIGN-OBJECT"
;;    "FREE-FOREIGN-OBJECT"
;;    "WITH-FOREIGN-OBJECT"   --> FFI:WITH-FOREIGN-OBJECT
;;    "WITH-FOREIGN-OBJECTS"
;;    "SIZE-OF-FOREIGN-TYPE"
;;    "POINTER-ADDRESS"
;;    "DEREF-POINTER"
;;    "ENSURE-CHAR-CHARACTER"
;;    "ENSURE-CHAR-INTEGER"
;;    "NULL-POINTER-P"
;;    "MAKE-NULL-POINTER"
;;    "+NULL-CSTRING-POINTER"+
;;    "CHAR-ARRAY-TO-POINTER"
;;
;;    ;; string functions
;;    "CONVERT-FROM-CSTRING"
;;    "CONVERT-TO-CSTRING"
;;    "FREE-CSTRING"
;;    "WITH-CSTRING"
;;    "WITH-CSTRINGS"
;;    "CONVERT-FROM-FOREIGN-STRING"
;;    "CONVERT-TO-FOREIGN-STRING"          --> (FFI:C-ARRAY-MAX FFI:UCHAR SIZE)
;;    "ALLOCATE-FOREIGN-STRING"            --> (FFI:C-ARRAY-MAX FFI:UCHAR SIZE)
;;                                      OR --> (FFI:C-ARRAY-MAX FFI:CHAR  SIZE)
;;    "WITH-FOREIGN-STRING"                --> FFI:WITH-FOREIGN-OBJECT





;; Because of (:struct name) and (:struct-pointer name) we must keep
;; a separate list of structure names (even if def-struct creates both
;; a typedef and a struct).
;;
;; Because of :pointer-self, when we convert a struct, we must keep the
;; current structure name at hand.
;;
;; We should check that a structure does not contain a (:struct self)
;; if not encapsulated into a (* ).






;;;  FFI-C-TYPE-TO-CL-TYPE
;;;     (BOOLEAN             . BOOLEAN)
;;;     (CHARACTER           . CHARACTER)
;;;     (FFI:SHORT           . INTEGER)
;;;     (FFI:USHORT          . INTEGER)
;;;     (FFI:INT             . INTEGER)
;;;     (FFI:UINT            . INTEGER)
;;;     (FFI:LONG            . INTEGER)
;;;     (FFI:ULONG           . INTEGER)
;;;     (SINGLE-FLOAT        . SINGLE-FLOAT)
;;;     (DOUBLE-FLOAT        . DOUBLE-FLOAT)


(defstruct (type-conv (:type list) (:conc-name nil))
  uffi-type ffi-type cl-type)


(defconstant +type-conversion-list+
  '( ;; :UFFI                :FFI                    :CL
    (:char                  ffi:character           character)
    (:unsigned-char         ffi:character           character)
    (:byte                  ffi:sint8               (signed-byte    8))
    (:unsigned-byte         ffi:uint8               (unsigned-byte  9))
    (:short                 ffi:sint16              (signed-byte   16))
    (:unsigned-short        ffi:uint16              (unsigned-byte 16))
    (:int                   ffi:sint32              (signed-byte   32))
    (:unsigned-int          ffi:uint32              (unsigned-byte 32))
    (:long                  ffi:sint32              (signed-byte   32))
    (:unsigned-long         ffi:uint32              (unsigned-byte 32))
    (:float                 single-float            single-float)
    (:double                double-float            double-float)
    (:cstring               ffi:c-pointer           string)
    (:pointer-void          ffi:c-pointer           t)
    (:void                  nil                     nil)
;;;
;;; (:ENUM                  FFI:INT                 INTEGER)
;;; ((:STRUCT name)         FFI:C-STRUCT            STRUCTURE)
;;; ((:STRUCT-POINTER name) (FFI:C-PTR-NULL FFI:C-STRUCT)   STRUCTURE)
;;; ;;   FOR LISP TYPE: WE BUILD A DEFSTRUCT
;;; (:UNION                 FFI:C-UNION             UNION)
;;; ;;   FOR LISP TYPE: FFI CONSIDER IT TO BE OF THE TYPE OF THE FIRST FIELD.
;;; ((:ARRAY TYPE)          (FFI:C-ARRAY-PTR TYPE)  (ARRAY :ELEMENT-TYPE TYPE))
;;;
    )
  "A LIST OF: (UFFI-TYPE  FFI-TYPE  CL-TYPE)"
  ) ;;+TYPE-CONVERSION-LIST+


(defvar +type-conversion-hash+ 
  (let ((table (make-hash-table :size 23)))
    (dolist (record +type-conversion-list+)
      (setf (gethash (uffi-type record) table) record))
    table)
  "A hash uffi-type --> (uffi-type  ffi-type  cl-type)."
  ) ;;+TYPE-CONVERSION-HASH+


(proclaim '(inline get-type-conversion-record))
(defun get-type-conversion-record (uffi-type)
  "
PRIVATE
RETURN:             THE RECORD FROM +TYPE-CONVERSION-HASH+ CORRESPONDING
                    TO UFFI-TYPE, OR NIL IF NONE EXISTS.
"
  (gethash uffi-type +type-conversion-hash+)
  ) ;;GET-TYPE-CONVERSION-RECORD


(defvar *foreign-types-hash* (make-hash-table :size 23)
  "A HASH TABLE OF THE NAMED FOREIGN TYPES: NAME --> UFFI-TYPE."
  ) ;;*FOREIGN-TYPES-HASH*


(defvar *foreign-structs-hash* (make-hash-table :size 23)
  "A HASH TABLE OF THE NAMED FOREIGN STRUCTS: NAME --> UFFI-STRUCT-TYPE."
  ) ;;*FOREIGN-STRUCTS-HASH*



;;; PRIMITIVE-UFFI-TYPE
;;; :POINTER-SELF
;;; (:STRUCT-POINTER STRUCT-NAME)
;;; (:STRUCT STRUCT-NAME)
;;; (:STRUCT STRUCT-NAME (FNAME FTYPE)...)
;;; 'TYPE
;;;
;;; (:UNION UNION-NAME (FNAME FTYPE)...)
;;; (:ARRAY-PTR TYPE)
;;; (:ARRAY     TYPE SIZE)


(defun clean-uffi-type (uffi-type &optional current-struct)
  "
PRIVATE
DO:                 REPLACE :POINTER-SELF BY (* (:STRUCT CURRENT-STRUCT),)
                            (:STRUCT-POINTER NAME) BY (* (:STRUCT NAME)),
                    AND CHECK THAT A STRUCTURE EXISTS FOR (:STRUCT NAME).
                    REPLACE (* :UNSIGNED-CHAR) and (* :CHAR) BY :CSTRING,
                    SINCE IT SEEMS UFFI CLIENT CODE ERRONEOUSLY 
                    USE (* :UNSIGNED-CHAR) INSTEAD OF :CSTRING...
RETURN:             A CLEANED UFFI-TYPE.
TODO:               CHECK OF (STRUCT X (FIELD (STRUCT X))).
"
  (if (atom uffi-type)
      (if (eq uffi-type :pointer-self)
          (if current-struct
              `(* (:struct ,current-struct))
              (error "FOUND :POINTER-SELF OUT OF A STRUCTURE."))
          uffi-type)
      (case (first uffi-type)
        (:struct-pointer
         (unless (= 2 (length uffi-type))
           (error "INVALID UFFI TYPE: ~S." uffi-type))
         `(* ,(clean-uffi-type (second uffi-type))))
        (:struct
            (cond
              ((= 2 (length uffi-type))
               (unless (gethash (second uffi-type) *foreign-structs-hash*)
                 (error "UNKNOWN STRUCT TYPE: ~S." uffi-type))
               uffi-type)
              ((< 2 (length uffi-type))
               (let ((struct-name (second uffi-type)))
                 (unless (symbolp struct-name)
                   (error "EXPECTED A SYMBOL AS STRUCT NAME INSTEAD OF ~S."
                          struct-name))
                 `(:struct ,struct-name
                    ,@(mapcar (lambda (field)
                                (let ((name (first field))
                                      (type (second field)))
                                  (unless (= 2 (length field))
                                    (error "INVALID STRUCT FIELD ~S." field))
                                  (list name (clean-uffi-type type struct-name))))
                              (cddr uffi-type)))))
              (t
               (error "INVALID STRUCT TYPE: ~S." uffi-type))))
        (common-lisp:quote
         (clean-uffi-type (second uffi-type) current-struct))
        (:union
         (unless (< 2 (length uffi-type))
           (error "MISSING FIELDS IN UNION TYPE ~S." uffi-type))
         `(:union ,(second uffi-type)
                  ,@(mapcar (lambda (field)
                              (let ((name (first field))
                                    (type (second field)))
                                (unless (= 2 (length field))
                                  (error "INVALID UNION FIELD ~S." field))
                                (list name
                                      (clean-uffi-type type current-struct))))
                            (cddr uffi-type))))
        (:array-ptr
         (unless (= 2 (length uffi-type))
           (error "INVALID ARRAY-PTR TYPE: ~S." uffi-type))
         `(:array-ptr ,(clean-uffi-type (second uffi-type) current-struct)))
        (:array
         (unless (= 3 (length uffi-type))
           (error "INVALID ARRAY TYPE: ~S." uffi-type))
         (let ((size (third uffi-type)))
           (unless (and (integerp size) (< 0 size))
             (error "INVALID ARRAY SIZE: ~S." size))
           `(:array ,(clean-uffi-type (second uffi-type) current-struct)
                    ,size)))
        (*
         (unless (= 2 (length uffi-type))
           (error "INVALID POINTER TYPE: ~S." uffi-type))
         `(* ,(clean-uffi-type (second uffi-type))))
        ;;(if (member (second uffi-type) '(:unsigned-char :char))
        ;;'FFI:C-POINTER
        (otherwise
         (error "INVALID TYPE: ~S." uffi-type))))
  ) ;;CLEAN-UFFI-TYPE

      
(defun convert-from-uffi-type (uffi-type context)
  "
PRIVATE
DO:                 Converts from a uffi type to an implementation
                    specific type.
UFFI-TYPE:          A UFFI TYPE.
CONTEXT:            :FFI OR :CL
RETURN:             A FFI TYPE (C-TYPE), OR A COMMON-LISP TYPE,
                    DEPENDING ON THE CONTEXT.
"
  (unless (or (eq context :ffi) (eq context :cl))
    (error "UNEXPECTED CONTEXT ~S, SHOULD BE EITHER :FFI OR :CL." context))
  (if (atom uffi-type)
      (let ((record (get-type-conversion-record uffi-type)))
        (if record
            ;; primitive types
            (if (eq context :ffi)
                (ffi-type record)
                (cl-type  record))
            ;; named types
            (let ((type (gethash uffi-type *foreign-types-hash*)))
              (if type
                  (convert-from-uffi-type type context)
                  (error "UNKNOWN UFFI TYPE ~S." uffi-type)))))
      (let ((sub-type (first uffi-type)))
        (case sub-type
          (:struct
              (let ((name (second uffi-type))
                    (fields
                     (mapcar
                      (lambda (field)
                        (let ((name (first field))
                              (type (second field)))
                          (list name (convert-from-uffi-type type context))))
                      (cddr uffi-type))))
                ;; TODO: SEE GENERATION OF  (:STRUCT NAME)
                ;;       VS. GENERATION OF: (:STRUCT NAME (FIELD TYPE)...)
                (if (null fields)
                    (let ((type (gethash name *foreign-structs-hash*)))
                      (if type
                          (if (eq context :ffi)
                              `(ffi:c-struct ,name)
                              name) ;; (CONVERT-FROM-UFFI-TYPE TYPE CONTEXT)
                          (error "UNKNOWN UFFI STRUCTURE ~S." name)))
                    (if (eq context :ffi)
                        `(ffi:c-struct ,name ,@fields)
                        `(defstruct ,name ,@fields)))))
          (:union
           (if (eq context :ffi)
               `(:c-union ,@(mapcar
                             (lambda (field)
                               (let ((name (first field))
                                     (type (second field)))
                                 (list name
                                       (convert-from-uffi-type type context))))
                             (cddr uffi-type)))
               `(convert-from-uffi-type (second (second uffi-type)) context)))
          (:array-ptr
           (let ((element-type
                  (convert-from-uffi-type  (second uffi-type) context)))
             (if (eq context :ffi)
                 `(ffi:c-array-ptr ,element-type)
                 `(array ,element-type *))))
          (:array
           (let ((element-type
                  (convert-from-uffi-type (second uffi-type) context))
                 (array-size   (cddr  uffi-type)))
             (if (eq context :ffi)
                 `(ffi:c-array ,element-type ,array-size)
                 `(array ,element-type (,array-size)))))
          (*
           (if (eq context :ffi)
               `(ffi:c-ptr ,(convert-from-uffi-type (second uffi-type) :ffi))
               ;;'FFI:C-POINTER
               (error "I don't know what a ~S is in Lisp.")))
          (otherwise
           (error "INVALID TYPE ~S." uffi-type)))))
  ) ;;CONVERT-FROM-UFFI-TYPE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I. Declarations ;;
;;;;;;;;;;;;;;;;;;;;;


(defmacro def-type (name type)
  "
DO:                 Defines a Common Lisp type based on a UFFI type.
NAME:               A symbol naming the type
TYPE:               A form that is evaluated that specifies the UFFI type.
IMPLEMENTATION:     For now, we generate `(DEFTYPE ,NAME T).
URL:                <http://uffi.b9.com/manual/def-type.html>
URL:                <http://www.lisp.org/HyperSpec/Body/mac_deftype.html>
"
  (setf type (clean-uffi-type type))
  `(deftype ,name t ,(convert-from-uffi-type type :cl))) ;;DEF-TYPE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; II. Primitive Types ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro def-constant (name value &key (export nil))
  "
DO:                 This is a thin wrapper around defconstant.
                    It evaluates at compile-time and optionally
                    exports the symbol from the package.
NAME:               A symbol that will be bound to the value.
VALUE:              An evaluated form that is bound the the name.
EXPORT:             EXPORT <=> The name is exported from the current package.
                    The default is NIL
NOTE:               I would not advise using this macro, since it does not
                    allow to attach a documentation string!
URL:                <http://uffi.b9.com/manual/def-constant.html>
URL:                <http://www.lisp.org/HyperSpec/Body/mac_defconstant.html>
URL:                <http://www.lisp.org/HyperSpec/Body/fun_export.html>
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name ,value)
     ,(when export (list 'export `(quote ,name)))
     ',name)
  ) ;;DEF-CONSTANT



(defmacro def-foreign-type (name type)
  "
DO:                 Defines a new foreign type.
NAME:               A symbol naming the new foreign type.
VALUE:              A form that is not evaluated that defines
                    the new foreign type.
URL:                <http://uffi.b9.com/manual/def-foreign-type.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#def-c-type>
"
  (let* ((name name)
         (uffi-type (clean-uffi-type type name))
         (ffi-type  (convert-from-uffi-type uffi-type :ffi)) )
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *foreign-types-hash*) ',uffi-type)
       (ffi:def-c-type ,name ,ffi-type)))) ;;DEF-FOREIGN-TYPE



(defmacro null-char-p (val)
  "
DO:                 Tests if a character or integer is NULL.
                    This abstracts the difference in implementations where
                    some return a character and some return an integer
                    when dereferencing a C character pointer.
CHAR:               A character or integer.
RETURN:             A boolean flag indicating if char is a NULL value.
URL:                <http://uffi.b9.com/manual/null-char-p.html>
"
  `(let ((val ,val)) (if (characterp val) (zerop (char-code val)) (zerop val)))
  ) ;;NULL-CHAR-P



(defun make-constant-name (enum-name separator-string constant-id)
  "
PRIVATE
DO:                 Builds an enum constant name.
"
  (intern (with-standard-io-syntax
            (format nil "~A~A~A" enum-name separator-string constant-id)))) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; III. Aggregate Types ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro def-enum (name constants &key (separator-string "#"))
  "
DO:                 Declares a C enumeration.
                    It generates constants with integer values for the
                    elements of the enumeration. The symbols for the these
                    constant values are created by the concatenation of
                    the enumeration name, separator-string, and field
                    symbol. Also creates a foreign type with the name name
                    of type :int.
NAME:               A symbol that names the enumeration.
CONSTANTS:          A list of enum constants definitions.
                    Each definition can be a symbol or a list of two
                    elements. Symbols get assigned a value of the current
                    counter which starts at 0 and increments by 1 for each
                    subsequent symbol. It the constants definition is a list,
                    the first position is the symbol and the second
                    position is the value to assign the the symbol. The
                    current counter gets set to 1+ this value.
SEPARATOR-STRING:   A string that governs the creation of constants.
                    The default is \"#\".
IMPLEMENTATION:     We generate both a DEF-C-TYPE for the NAME
                    and a DEF-C-ENUM for the constants.
URL:                <http://uffi.b9.com/manual/def-enum.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#def-c-enum>
URL:                <http://clisp.sourceforge.net/impnotes.html#def-c-type>
"
  (let ((c-constants
         (mapcar
          (lambda (constant)
            (cond
              ((symbolp constant)
               (list (make-constant-name name separator-string constant))  )
              ((and (consp constant)
                    (= 2 (length constant)) (integerp (cadr constant)))
               (list (make-constant-name name separator-string (car constant))
                     (cadr constant)))
              (t
               (error "INVALID ENUM CONSTANT SYNTAX: ~S." constant))))
          constants)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,name *foreign-types-hash*) :int)
       (ffi:def-c-type  ,name  ,(convert-from-uffi-type :int :ffi))
       (ffi:def-c-enum  ,name  ,@c-constants)))
  ) ;;DEF-ENUM


(defmacro def-struct (name &rest fields)
  "
DO:                 Declares a structure.
                    A special type is available as a slot in the field. It is
                    a pointer that points to an instance of the parent
                    structure. It's type is :pointer-self.
NAME:               A symbol that names the structure.
FIELDS:             A variable number of field definitions.
                    Each definition is a list consisting of a symbol naming
                    the field followed by its foreign type.  
IMPLEMENTATION:     Generates a DEF-C-STRUCT which defines both a foreign
                    C type and a Common-Lisp STRUCTURE-CLASS.
URL:                <http://uffi.b9.com/manual/def-struct.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#def-c-struct>
"
  (let* ((name name)
         (uffi-type (clean-uffi-type `(:struct ,name ,@fields) name))
         (ffi-type  (convert-from-uffi-type uffi-type :ffi)) )
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *foreign-types-hash*)
             (setf (gethash ',name *foreign-structs-hash*) ',uffi-type))
       (ffi:def-c-struct ,@(cdr ffi-type))))
  ) ;;DEF-STRUCT

;; ,(CONVERT-FROM-UFFI-TYPE TYPE :CL)
;; (COM.INFORMATIMAGO.CLISP.UFFI::CLEAN-UFFI-TYPE '(* :unsigned-char) 'struct-name)
;; (setf name 'ldap-error fields '((e_code :int) (e_reason (* :unsigned-char))))


(defmacro get-slot-value (obj type field)
  "
DO:                 Accesses a slot value from a structure.
OBJ:                A pointer to foreign structure.
TYPE:               A name of the foreign structure type.
FIELD:              A name of the desired field in foreign structure.
RETURN:             The value of the field in the structure.
SEE ALSO:           GET-SLOT-POINTER
URL:                <http://uffi.b9.com/manual/get-slot-value.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#slot>
"
  (when (and (listp type) (eq 'quote (car type)))
    (setf type (second type)))
  ;; TODO: CHECK CONVERT TYPE.
  `(ffi:slot (ffi:deref (ffi:cast (ffi:foreign-value ,obj) (* ,type)))
             ,field)) ;;GET-SLOT-VALUE



(defmacro get-slot-pointer (obj type field)
  "
DO:                 Accesses a slot value from a structure.
OBJ:                A pointer to foreign structure.
TYPE:               A name of the foreign structure type.
FIELD:              A name of the desired field in foreign structure.
RETURN:             The value of the field in the structure: A POINTER.
NOTE:               This is similar to GET-SLOT-VALUE.
                    It is used when the value of a slot is a pointer type.
SEE ALSO:           GET-SLOT-VALUE
URL:                <http://uffi.b9.com/manual/get-slot-pointer.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#slot>
"
  ;; NO DIFFERENCE TO ACCESS POINTER FIELD THAN TO ACCESS VALUE FIELDS.
  `(get-slot-value ,obj ,type ,field)
  ) ;;GET-SLOT-POINTER



(defmacro def-array-pointer (name type)
  "
DO:                 Defines a type that is a pointer to an array of type.
NAME:               A name of the new foreign type.
TYPE:               The foreign type of the array elements.
URL:                <http://uffi.b9.com/manual/def-array-pointer.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#c-array-ptr>
URL:                <http://clisp.sourceforge.net/impnotes.html#def-c-type>
"
  (let* ((name name)
         (uffi-type (clean-uffi-type `(:array-ptr ,type)))
         (ffi-type  (convert-from-uffi-type uffi-type :ffi)) )
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,name *foreign-types-hash*) ,uffi-type)
       (ffi:def-c-type ,name ,ffi-type)))
  ) ;;DEF-ARRAY-POINTER



(defmacro def-union (name &rest fields)
  "
NAME:               A name of the new union type.
FIELDS:             A list of fields of the union.
DO:                 Defines a foreign union type.
URL:                <http://uffi.b9.com/manual/def-union.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#c-union>
URL:                <http://clisp.sourceforge.net/impnotes.html#def-c-type>
"
  (let* ((name name)
         (uffi-type (clean-uffi-type `(:union ,name ,@fields)))
         (ffi-type  (convert-from-uffi-type uffi-type :ffi)) )
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,name *foreign-types-hash*) ,uffi-type)
       (ffi:def-c-type ,name ,ffi-type)))
  ) ;;DEF-UNION



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IV. Objects ;;
;;;;;;;;;;;;;;;;;


(ffi:def-call-out malloc
    (:name "malloc")
  (:arguments (size ffi:uint32 :in))
  (:return-type ffi:c-pointer)
  (:language :stdc))


(ffi:def-call-out free
    (:name "free")
  (:arguments (ptr ffi:c-pointer :in))
  (:return-type nil)
  (:language :stdc))


(defmacro allocate-foreign-object (type &optional (size 1))
  "
DO:                 Allocates an instance of a foreign object.
TYPE:               The type of foreign object to allocate.
                    This parameter is evaluated.
SIZE:               An optional size parameter that is evaluated.
                    If specified, allocates and returns an array
                    of type that is size members long.
                    This parameter is evaluated.
RETURN:             A pointer to the foreign object.
URL:                <http://uffi.b9.com/manual/allocate-foreign-object.html>
URL:                
IMPLEMENTATION:     
"
  ;; TODO: CHECK IF TYPE IS CONTANT AND THE.N CHECK AND CONVERT
  ;;       IT AT COMPILE TIME.
  `(ffi:allocate-shallow (convert-from-uffi-type
                          (clean-uffi-type ,type) :ffi)
                         :count ,size)) ;;ALLOCATE-FOREIGN-OBJECT


(defmacro free-foreign-object (ptr)
  "
DO:                 Frees the memory used by the allocation of a foreign
                    object.
PTR:                A pointer to the allocated foreign object to free.
URL:                <http://uffi.b9.com/manual/free-foreign-object.html>
URL:
IMPLEMENTATION:     
"
  `(ffi:foreign-free ,ptr)
  ) ;;FREE-FOREIGN-OBJECT


(defmacro with-foreign-object ((var type) &body body)
  "
DO:                 This function wraps the allocation, binding,
                    and destruction of a foreign object. On CMUCL and
                    Lispworks platforms the object is stack allocated
                    for efficiency. Benchmarks show that AllegroCL
                    performs much better with static allocation.
VAR:                The variable name to bind.
TYPE:               The type of foreign object to allocate.
                    This parameter is evaluated.
RETURN:             The result of evaluating the body.
URL:                <http://uffi.b9.com/manual/with-foreign-object.html>
URL:
"
  `(let ((,var (allocate-foreign-object ,type)))
     (unwind-protect
          (progn ,@body)
       (free-foreign-object ,var)))
  ) ;;WITH-FOREIGN-OBJECT


(defmacro size-of-foreign-type (type)
  "
FTYPE:              A foreign type specifier. This parameter is evaluated.
RETURN:             The number of data bytes used by a foreign object type.
                    This does not include any Lisp storage overhead.
URL:                <http://uffi.b9.com/manual/size-of-foreign-type.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#sizeof>
"
  `(ffi:sizeof (convert-from-uffi-type (clean-uffi-type ,type) :ffi))
  ) ;;SIZE-OF-FOREIGN-TYPE


(defmacro pointer-address (ptr)
  "
PTR:                A pointer to a foreign object.
RETURN:             An integer representing the pointer's address.
URL:                <http://uffi.b9.com/manual/pointer-address.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#c-var-addr>
"
  `(let ((ptr ,ptr))
     (declare (type 'ffi:foreign-address ptr))
     (ffi::foreign-address-unsigned ptr))
  ) ;;POINTER-ADDRESS


(defmacro deref-pointer (ptr type)
  "
PTR:                A pointer to a foreign object.
TYPE:               A foreign type of the object being pointed to.
RETURN:             The value of the object where the pointer points.
URL:                <http://uffi.b9.com/manual/deref-pointer.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#deref>
NOTE:               This is an accessor and can be used with SETF .
"
  `(ffi:deref (ffi:cast (ffi:foreign-value ,ptr)
                        (convert-from-uffi-type
                         (clean-uffi-type (list '* ,type)) :ffi)
                        ))
  ) ;;DEREF-POINTER


(defmacro ensure-char-character (object)
  "
DO:                 Ensures that an object obtained by dereferencing
                    a :CHAR pointer is a character.
OBJECT:             Either a character or a integer specifying
                    a character code.
RETURN:             A character.
URL:                <http://uffi.b9.com/manual/ensure-char-character.html>
URL:
"
  `(let ((object ,object))
     (if (characterp object) object (code-char object)))
  ) ;;ENSURE-CHAR-CHARACTER


(defmacro ensure-char-integer (object)
  "
DO:                 Ensures that an object obtained by dereferencing
                    a :CHAR pointer is an integer.
OBJECT:             Either a character or a integer specifying
                    a character code.
RETURN:             An integer.
URL:                <http://uffi.b9.com/manual/ensure-char-integer.html>
URL:
"
  `(let ((object ,object))
     (if (characterp object) (char-code object) object))
  ) ;;ENSURE-CHAR-INTEGER


(defmacro make-null-pointer (type)
  "
DO:                 Creates a NULL pointer of a specified type.
TYPE:               A type of object to which the pointer refers.
RETURN:             The NULL pointer of type TYPE.
URL:                <http://uffi.b9.com/manual/make-null-pointer.html>
URL:
"
  (declare (ignore type))
  (ffi::unsigned-foreign-address 0)
  ;;  `(FFI:CAST (ffi:foreign-value (FFI::UNSIGNED-FOREIGN-ADDRESS 0))
  ;;              (CONVERT-FROM-UFFI-TYPE
  ;;               (CLEAN-UFFI-TYPE (LIST '* ,TYPE)) :FFI))
  ) ;;MAKE-NULL-POINTER



(defmacro null-pointer-p (ptr)
  "
DO:                 Tests if a pointer is has a NULL value.
PTR:                A foreign object pointer.
RETURN:             Whether ptr is NULL.
URL:                <http://uffi.b9.com/manual/null-pointer-p.html>
URL:                <http://clisp.sourceforge.net/impnotes.html#fa-null>
"
  `(ffi:foreign-address-null ,ptr)
  ) ;;NULL-POINTER-P


(defconstant +null-cstring-pointer+
  (ffi::unsigned-foreign-address 0)
  ;;(FFI:CAST (ffi:foreign-value (FFI::UNSIGNED-FOREIGN-ADDRESS 0))
  ;;          (CONVERT-FROM-UFFI-TYPE (CLEAN-UFFI-TYPE :CSTRING) :FFI))
  "A NULL cstring pointer.
This can be used for testing if a cstring returned by a function is NULL.
URL:                <http://uffi.b9.com/manual/null-cstring-pointer.html>
"
  ) ;;+NULL-CSTRING-POINTER+


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; V. Strings ;;
;;;;;;;;;;;;;;;;


(defmacro convert-from-cstring (cstring)
  "
CSTRING:            A cstring.
RETURN:             A Lisp string.
DO:                 Converts a Lisp string to a cstring.
                    This is most often used when processing the
                    results of a foreign function that returns a
                    cstring.
URL:                <http://uffi.b9.com/manual/convert-from-cstring.html>
"
  `,cstring
  ) ;;CONVERT-FROM-CSTRING



(defmacro convert-to-cstring (string)
  "
STRING:             A Lisp string.
RETURN:             A cstring.
DO:                 Converts a Lisp string to a cstring.
                    The cstring should be freed with free-cstring.
URL:                <http://uffi.b9.com/manual/convert-to-cstring.html>
"
  `,string
  ) ;;CONVERT-TO-CSTRING


(defmacro free-cstring (cstring)
  "
CSTRING:            A cstring.
DO:                 Frees any memory possibly allocated by convert-to-cstring.
                    On some implementions, a cstring is just the Lisp
                    string itself.
"
  (declare (ignore cstring))
  ;; NOP
  ) ;;FREE-CSTRING


(defmacro with-cstring ((cstring string) &body body)
  "
CSTRING:            A symbol naming the cstring to be created.
STRING:             A Lisp string that will be translated to a cstring.
BODY:               The body of where the CSTRING will be bound.
DO:                 Binds a symbol to a cstring created from conversion
                    of a string. Automatically frees the cstring.
URL:                <http://uffi.b9.com/manual/with-cstring.html>
"
  ;; `(let ((,cstring (convert-to-cstring ,string)))
  ;;    (unwind-protect
  ;;        (progn ,@body)
  ;;      (free-cstring ,cstring)))
  `(let ((,cstring ,string))
     ,@body)
  ) ;;WITH-CSTRING


(defun foreign-string-length (foreign-string)
  (do ((len 0 (1+ len)))
      ((= 0 (ffi:element (ffi:foreign-value foreign-string) len))
       len))) ;;foreign-string-length


(defun convert-from-foreign-string (foreign-string
                                    &key length (null-terminated-p t))
  "
DO:                 Builds a Lisp string from a foreign string.
                    Can translate ASCII and binary strings.
FOREIGN-STRING:     A foreign string.
LENGTH:             The length of the foreign string to convert.
                    The default is the length of the string until
                    a NULL character is reached.
NULL-TERMINATED-P:  A boolean flag with a default value of T.
                    When true, the string is converted until the first
                    NULL character is reached.
RETURN:             A Lisp string.
URL:        <http://uffi.b9.com/manual/convert-from-foreign-string.html>
URL:        <http://clisp.sourceforge.net/impnotes.html#encoding>
"
  (let ((byte-vector (make-array (list (if (or null-terminated-p (null length))
                                           (foreign-string-length foreign-string)
                                           length))
                                 :element-type '(unsigned-byte 8)))
        (foreign-type `(ffi:c-array ffi:uchar ,(list length))))
    (declare (ignore foreign-type))     ; TODO!
    (dotimes (i (length byte-vector))
      (setf (aref byte-vector i)
            (ffi:element (ffi:foreign-value foreign-string) i)))
    (ext:convert-string-from-bytes byte-vector custom:*foreign-encoding*)  
    )) ;;CONVERT-FROM-FOREIGN-STRING


(defun convert-to-foreign-string (string)
  "
STRING:             A Lisp string.
RETURN:             A foreign string.
DO:                 Converts a Lisp string to a foreign string.
                    Memory should be freed with free-foreign-object.
URL:        <http://uffi.b9.com/manual/convert-to-foreign-string.html>
"
  (let* ((byte-vector
          (ext:convert-string-to-bytes string custom:*foreign-encoding*))
         (result (allocate-foreign-string (1+ (length byte-vector))))
         (foreign-type `(ffi:c-array 
                         ffi:uchar ,(list (1+ (length byte-vector))))))
    (declare (ignore foreign-type))     ; TODO!
    (dotimes (i (length byte-vector))
      (setf (ffi:element (ffi:foreign-value result) i)
            (aref byte-vector i)))
    (setf (ffi:element  (ffi:foreign-value result) (length byte-vector)) 0)
    result)) ;;CONVERT-TO-FOREIGN-STRING


(defun allocate-foreign-string (size &key (unsigned t))
  "
SIZE:               The size of the space to be allocated in bytes.
UNSIGNED:           A boolean flag with a default value of T.
                    When true, marks the pointer as an :UNSIGNED-CHAR.
RETURN:             A foreign string which has undefined contents.
DO:                 Allocates space for a foreign string.
                    Memory should be freed with free-foreign-object.
URL:            <http://uffi.b9.com/manual/allocate-foreign-string.html>
"
  (allocate-foreign-object (if unsigned ':unsigned-char ':char) size)
  ) ;;ALLOCATE-FOREIGN-STRING


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VI. Functions & Libraries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *modules-to-library-map* (make-hash-table :test (function equal))
  "Maps module names to library paths.")


(defmacro def-function (name args &key module returning)
  "
DO:                 Declares a foreign function.
NAME:               A string or list specifying the function name.
                    If it is a string, that names the foreign
                    function. A Lisp name is created by translating
                    #\_ to #\- and by converting to upper-case in
                    case-insensitive Lisp implementations. If it is a
                    list, the first item is a string specifying the
                    foreign function name and the second it is a
                    symbol stating the Lisp name.
ARGS:               A list of argument declarations.  If NIL, indicates
                    that the function does not take any arguments.
MODULE:             A string specifying which module (or library)
                    that the foreign function resides. (Required by Lispworks)
RETURNING:          A declaration specifying the result type of
                    the foreign function. :VOID indicates that this function
                    does not return any value.
URL:                <http://uffi.b9.com/manual/def-function.html>
NOTE:               All Common-Lisp implementations are 'case-insensitive'.
                    <http://www.lisp.org/HyperSpec/Body/sec_2-1-1-2.html>
"
  (let (l-name c-name)
    (if (stringp name)
        (setq c-name name
              l-name (intern (string-upcase
                              (substitute (character "-") (character "_") name))))
        (setq c-name (first name)
              l-name (second name)))
    `(ffi:def-call-out
         ,l-name
         (:name ,c-name)
       ,@(when args
               `((:arguments
                  ,@(mapcar (lambda (arg)
                              `(,(first arg)
                                 ,(convert-from-uffi-type 
                                   (clean-uffi-type (second arg)) :ffi)
                                 :in))
                            args))))
       ,@(when returning
               `((:return-type ,(convert-from-uffi-type
                                 (clean-uffi-type returning) :ffi))))
       ,@(when module
               (let ((library (gethash module *modules-to-library-map*)))
                 `((:library  ,(or library module)))))
       (:language :stdc)))) ;;DEF-FUNCTION


(defun load-foreign-library (filename &key module supporting-libraries)
  "
DO:                 Loads a foreign library. Applies a module name
                    to functions within the library. Ensures that
                    a library is only loaded once during a session.
FILENAME:           A string or pathname specifying the library location
                    in the filesystem. At least one implementation
                    (Lispworks) can not accept a logical pathname.
MODULE:             A string designating the name of the module to
                    apply to functions in this library.
                    (Required for Lispworks)
SUPPORTING-LIBRARIES:
                    A list of strings naming the libraries required to
                    link the foreign library. (Required by CMUCL)
RETURN:             A boolean flag, T if the library was able to be
                    loaded successfully or if the library has been
                    previously loaded, otherwise NIL.
URL:                <http://uffi.b9.com/manual/load-foreign-library.html>
IMPLEMENTATION:     Loading the library is defered to the first function call.
                    Here we just register the mapping between the MODULE and
                    the FILENAME.
TODO:               Should we explicitely load the SUPPORTING-LIBRARIES too?
"
  (declare (ignore supporting-libraries))
  (when module
    (setf (gethash module *modules-to-library-map*) (namestring filename)))
  t) ;;LOAD-FOREIGN-LIBRARY


(defun split-string (string &optional (separators " "))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (unless (simple-string-p string)     (setq string     (copy-seq string)))
  (unless (simple-string-p separators) (setq separators (copy-seq separators)))
  (let ((chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)) )
    (declare (type simple-string string separators))
    (loop while (< position strlen)
       do
       (loop while (and (< nextpos strlen)
                        (not (position (char string nextpos) separators)))
          do (setq nextpos (1+ nextpos))
          ) ;;loop
       (push (subseq string position nextpos) chunks)
       (setq position (1+ nextpos))
       (setq nextpos  position)
       ) ;;loop
    (nreverse chunks)
    )) ;;SPLIT-STRING


(defun find-foreign-library (names directories &key drive-letters types verbose)
  "
NAMES:              A string or list of strings containing the base name
                    of the library file.
DIRECTORIES:        A string or list of strings containing the directory
                    the library file.
DRIVE-LETTERS:      A string or list of strings containing the drive letters
                    for the library file.
TYPES:              A string or list of strings containing the file type
                    of the library file. Default is NIL. If NIL, will use
                    a default type based on the currently running
                    implementation.
VERBOSE:            This is an extension from the UFFI specification.
                    Prints a line on *trace-output* for each path considered.
RETURN:             The path of the first found file, or NIL if the
                    library file was not found.
DO:                 Finds a foreign library by searching through a number
                    of possible locations.
URL:                <http://uffi.b9.com/manual/find-foreign-library.html>
IMPLEMENTATION:     You'd better leave it up to the system to find the library!
                    This implementation can't locate libc because on linux,
                    there's no link named libc.so and this API doesn't allow
                    for a version number (anyway, library versions such as in
                    libc.so.6.0.1 have nothing to do with COMMON-LISP version 
                    that are actually file versions and mere integers).
                    Some people believe the can pass with impunity strings
                    containing dots as types. But that's not so.
"
  (flet ((ensure-list (item) (if (listp item) item (list item))))
    (setf names         (ensure-list names))
    (setf directories   (ensure-list directories))
    (setf drive-letters (ensure-list drive-letters))
    (setf types         (ensure-list types))
    (setf names (mapcan (lambda (name)
                          (if (or (<= (length name) 3)
                                  (string/= "lib" name :end2 3))
                              (list name (concatenate 'string "lib" name))
                              (list name))) names))
    (setf types (or (delete-if (lambda (item) 
                                 (not (every (function alphanumericp) item)))
                               types) '("so")))
    (setf drive-letters (or drive-letters '(nil)))
    (when verbose
      (format *trace-output* "Directories   = ~S~%" directories)
      (format *trace-output* "Types         = ~S~%" types)
      (format *trace-output* "Names         = ~S~%" names)
      (format *trace-output* "Drive-letters = ~S~%" drive-letters))
    (dolist (dir directories)
      (dolist (type types)
        (dolist (name names)
          (dolist (device drive-letters)
            (let ((path (make-pathname
                         :device device
                         :directory ((lambda (items)
                                       (if (char= (character "/") (char dir 0))
                                           (cons :absolute (cdr items))
                                           (cons :relative items)))
                                     (split-string dir "/"))
                         :name name
                         :type type)))
              (when verbose
                (format *trace-output* "; Considering ~S~%" path))
              (when (probe-file path)
                (return-from find-foreign-library path))))))
      nil))) ;;FIND-FOREIGN-LIBRARY

;; Local Variables:
;; eval: (cl-indent 'ffi:with-c-place 1)
;; End:

;;;; uffi.lisp                        --                     --          ;;;;
