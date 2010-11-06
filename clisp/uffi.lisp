;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               uffi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             clisp
;;;;USER-INTERFACE:     NONE
;;;;NOWEB:              T
;;;;DESCRIPTION
;;;;
;;;;    This API is obsolete. See: CFFI.
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
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2003 - 2004
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.CLISP.UFFI"
  (:NICKNAMES "UFFI")
  (:DOCUMENTATION "
This package implements over clisp native FFI the UFFI API as defined in
'UFFI Reference Guide' by Kevin M. Rosenberg, Heart Hospital of New Mexico.

The version of the UFFI implemented here is uffi-1.2.15.

URL:    http://uffi.b9.com/manual/book1.html
URL:    http://uffi.b9.com/

LEGAL:  Copyright Pascal J. Bourguignon 2003 - 2004

        This package is free software; you can redistribute it and/or
        modify it under the terms of the GNU Lesser General Public
        License as published by the Free Software Foundation; either
        version 2 of the License, or (at your option) any later
        version.

        This library is distributed in the hope that it will be
        useful, but WITHOUT ANY WARRANTY; without even the implied
        warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
        PURPOSE.  See the GNU Lesser General Public License for more
        details.

        You should have received a copy of the GNU Lesser General
        Public License along with this library; if not, write to the
        Free Software Foundation, Inc., 59 Temple Place, Suite 330,
        Boston, MA 02111-1307 USA
")
  (:USE "COMMON-LISP") ;; actually: FROM COMMON-LISP IMPORT ALL;
  ;; really: USE FFI,CUSTOM,EXT;
  (:EXPORT

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
(IN-PACKAGE "COM.INFORMATIMAGO.CLISP.UFFI")

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


(DEFCONSTANT +TYPE-CONVERSION-LIST+
  '( ;; :UFFI                :FFI                    :CL
    (:CHAR                  FFI:CHARACTER           CHARACTER)
    (:UNSIGNED-CHAR         FFI:CHARACTER           CHARACTER)
    (:BYTE                  FFI:SINT8               (SIGNED-BYTE    8))
    (:UNSIGNED-BYTE         FFI:UINT8               (UNSIGNED-BYTE  9))
    (:SHORT                 FFI:SINT16              (SIGNED-BYTE   16))
    (:UNSIGNED-SHORT        FFI:UINT16              (UNSIGNED-BYTE 16))
    (:INT                   FFI:SINT32              (SIGNED-BYTE   32))
    (:UNSIGNED-INT          FFI:UINT32              (UNSIGNED-BYTE 32))
    (:LONG                  FFI:SINT32              (SIGNED-BYTE   32))
    (:UNSIGNED-LONG         FFI:UINT32              (UNSIGNED-BYTE 32))
    (:FLOAT                 SINGLE-FLOAT            SINGLE-FLOAT)
    (:DOUBLE                DOUBLE-FLOAT            DOUBLE-FLOAT)
    (:CSTRING               FFI:C-POINTER           STRING)
    (:POINTER-VOID          FFI:C-POINTER           T)
    (:VOID                  NIL                     NIL)
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


(defvar +TYPE-CONVERSION-HASH+ 
  (let ((table (MAKE-HASH-TABLE :SIZE 23)))
    (DOLIST (RECORD +TYPE-CONVERSION-LIST+)
      (SETF (GETHASH (UFFI-TYPE RECORD) table) RECORD))
    table)
  "A hash uffi-type --> (uffi-type  ffi-type  cl-type)."
  ) ;;+TYPE-CONVERSION-HASH+


(PROCLAIM '(INLINE GET-TYPE-CONVERSION-RECORD))
(DEFUN GET-TYPE-CONVERSION-RECORD (UFFI-TYPE)
  "
PRIVATE
RETURN:             THE RECORD FROM +TYPE-CONVERSION-HASH+ CORRESPONDING
                    TO UFFI-TYPE, OR NIL IF NONE EXISTS.
"
  (GETHASH UFFI-TYPE +TYPE-CONVERSION-HASH+)
  ) ;;GET-TYPE-CONVERSION-RECORD


(DEFVAR *FOREIGN-TYPES-HASH* (MAKE-HASH-TABLE :SIZE 23)
  "A HASH TABLE OF THE NAMED FOREIGN TYPES: NAME --> UFFI-TYPE."
  ) ;;*FOREIGN-TYPES-HASH*


(DEFVAR *FOREIGN-STRUCTS-HASH* (MAKE-HASH-TABLE :SIZE 23)
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


(DEFUN CLEAN-UFFI-TYPE (UFFI-TYPE &OPTIONAL CURRENT-STRUCT)
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
  (IF (ATOM UFFI-TYPE)
      (IF (EQ UFFI-TYPE :POINTER-SELF)
          (IF CURRENT-STRUCT
              `(* (:STRUCT ,CURRENT-STRUCT))
              (ERROR "FOUND :POINTER-SELF OUT OF A STRUCTURE."))
          UFFI-TYPE)
      (CASE (FIRST UFFI-TYPE)
        (:STRUCT-POINTER
         (UNLESS (= 2 (LENGTH UFFI-TYPE))
           (ERROR "INVALID UFFI TYPE: ~S." UFFI-TYPE))
         `(* ,(CLEAN-UFFI-TYPE (SECOND UFFI-TYPE))))
        (:STRUCT
            (COND
              ((= 2 (LENGTH UFFI-TYPE))
               (UNLESS (GETHASH (SECOND UFFI-TYPE) *FOREIGN-STRUCTS-HASH*)
                 (ERROR "UNKNOWN STRUCT TYPE: ~S." UFFI-TYPE))
               UFFI-TYPE)
              ((< 2 (LENGTH UFFI-TYPE))
               (LET ((STRUCT-NAME (SECOND UFFI-TYPE)))
                 (UNLESS (SYMBOLP STRUCT-NAME)
                   (ERROR "EXPECTED A SYMBOL AS STRUCT NAME INSTEAD OF ~S."
                          STRUCT-NAME))
                 `(:STRUCT ,STRUCT-NAME
                    ,@(MAPCAR (LAMBDA (FIELD)
                                (LET ((NAME (FIRST FIELD))
                                      (TYPE (SECOND FIELD)))
                                  (UNLESS (= 2 (LENGTH FIELD))
                                    (ERROR "INVALID STRUCT FIELD ~S." FIELD))
                                  (LIST NAME (CLEAN-UFFI-TYPE TYPE STRUCT-NAME))))
                              (CDDR UFFI-TYPE)))))
              (T
               (ERROR "INVALID STRUCT TYPE: ~S." UFFI-TYPE))))
        (COMMON-LISP:QUOTE
         (CLEAN-UFFI-TYPE (SECOND UFFI-TYPE) CURRENT-STRUCT))
        (:UNION
         (UNLESS (< 2 (LENGTH UFFI-TYPE))
           (ERROR "MISSING FIELDS IN UNION TYPE ~S." UFFI-TYPE))
         `(:UNION ,(SECOND UFFI-TYPE)
                  ,@(MAPCAR (LAMBDA (FIELD)
                              (LET ((NAME (FIRST FIELD))
                                    (TYPE (SECOND FIELD)))
                                (UNLESS (= 2 (LENGTH FIELD))
                                  (ERROR "INVALID UNION FIELD ~S." FIELD))
                                (LIST NAME
                                      (CLEAN-UFFI-TYPE TYPE CURRENT-STRUCT))))
                            (CDDR UFFI-TYPE))))
        (:ARRAY-PTR
         (UNLESS (= 2 (LENGTH UFFI-TYPE))
           (ERROR "INVALID ARRAY-PTR TYPE: ~S." UFFI-TYPE))
         `(:ARRAY-PTR ,(CLEAN-UFFI-TYPE (SECOND UFFI-TYPE) CURRENT-STRUCT)))
        (:ARRAY
         (UNLESS (= 3 (LENGTH UFFI-TYPE))
           (ERROR "INVALID ARRAY TYPE: ~S." UFFI-TYPE))
         (LET ((SIZE (THIRD UFFI-TYPE)))
           (UNLESS (AND (INTEGERP SIZE) (< 0 SIZE))
             (ERROR "INVALID ARRAY SIZE: ~S." SIZE))
           `(:ARRAY ,(CLEAN-UFFI-TYPE (SECOND UFFI-TYPE) CURRENT-STRUCT)
                    ,SIZE)))
        (*
         (unless (= 2 (length uffi-type))
           (error "INVALID POINTER TYPE: ~S." uffi-type))
         `(* ,(CLEAN-UFFI-TYPE (SECOND UFFI-TYPE))))
        ;;(if (member (second uffi-type) '(:unsigned-char :char))
        ;;'FFI:C-POINTER
        (OTHERWISE
         (ERROR "INVALID TYPE: ~S." UFFI-TYPE))))
  ) ;;CLEAN-UFFI-TYPE

      
(DEFUN CONVERT-FROM-UFFI-TYPE (UFFI-TYPE CONTEXT)
  "
PRIVATE
DO:                 Converts from a uffi type to an implementation
                    specific type.
UFFI-TYPE:          A UFFI TYPE.
CONTEXT:            :FFI OR :CL
RETURN:             A FFI TYPE (C-TYPE), OR A COMMON-LISP TYPE,
                    DEPENDING ON THE CONTEXT.
"
  (UNLESS (OR (EQ CONTEXT :FFI) (EQ CONTEXT :CL))
    (ERROR "UNEXPECTED CONTEXT ~S, SHOULD BE EITHER :FFI OR :CL." CONTEXT))
  (IF (ATOM UFFI-TYPE)
      (LET ((RECORD (GET-TYPE-CONVERSION-RECORD UFFI-TYPE)))
        (IF RECORD
            ;; primitive types
            (IF (EQ CONTEXT :FFI)
                (FFI-TYPE RECORD)
                (CL-TYPE  RECORD))
            ;; named types
            (LET ((TYPE (GETHASH UFFI-TYPE *FOREIGN-TYPES-HASH*)))
              (IF TYPE
                  (CONVERT-FROM-UFFI-TYPE TYPE CONTEXT)
                  (ERROR "UNKNOWN UFFI TYPE ~S." UFFI-TYPE)))))
      (LET ((SUB-TYPE (FIRST UFFI-TYPE)))
        (CASE SUB-TYPE
          (:STRUCT
              (LET ((NAME (SECOND UFFI-TYPE))
                    (FIELDS
                     (MAPCAR
                      (LAMBDA (FIELD)
                        (LET ((NAME (FIRST FIELD))
                              (TYPE (SECOND FIELD)))
                          (LIST NAME (CONVERT-FROM-UFFI-TYPE TYPE CONTEXT))))
                      (CDDR UFFI-TYPE))))
                ;; TODO: SEE GENERATION OF  (:STRUCT NAME)
                ;;       VS. GENERATION OF: (:STRUCT NAME (FIELD TYPE)...)
                (IF (NULL FIELDS)
                    (LET ((TYPE (GETHASH NAME *FOREIGN-STRUCTS-HASH*)))
                      (IF TYPE
                          (IF (EQ CONTEXT :FFI)
                              `(FFI:C-STRUCT ,NAME)
                              NAME) ;; (CONVERT-FROM-UFFI-TYPE TYPE CONTEXT)
                          (ERROR "UNKNOWN UFFI STRUCTURE ~S." NAME)))
                    (IF (EQ CONTEXT :FFI)
                        `(FFI:C-STRUCT ,NAME ,@FIELDS)
                        `(DEFSTRUCT ,NAME ,@FIELDS)))))
          (:UNION
           (IF (EQ CONTEXT :FFI)
               `(:C-UNION ,@(MAPCAR
                             (LAMBDA (FIELD)
                               (LET ((NAME (FIRST FIELD))
                                     (TYPE (SECOND FIELD)))
                                 (LIST NAME
                                       (CONVERT-FROM-UFFI-TYPE TYPE CONTEXT))))
                             (CDDR UFFI-TYPE)))
               `(CONVERT-FROM-UFFI-TYPE (SECOND (SECOND UFFI-TYPE)) CONTEXT)))
          (:ARRAY-PTR
           (LET ((ELEMENT-TYPE
                  (CONVERT-FROM-UFFI-TYPE  (SECOND UFFI-TYPE) CONTEXT)))
             (IF (EQ CONTEXT :FFI)
                 `(FFI:C-ARRAY-PTR ,ELEMENT-TYPE)
                 `(ARRAY ,ELEMENT-TYPE *))))
          (:ARRAY
           (LET ((ELEMENT-TYPE
                  (CONVERT-FROM-UFFI-TYPE (SECOND UFFI-TYPE) CONTEXT))
                 (ARRAY-SIZE   (CDDR  UFFI-TYPE)))
             (IF (EQ CONTEXT :FFI)
                 `(FFI:C-ARRAY ,ELEMENT-TYPE ,ARRAY-SIZE)
                 `(ARRAY ,ELEMENT-TYPE (,ARRAY-SIZE)))))
          (*
           (if (eq context :ffi)
               `(ffi:c-ptr ,(convert-from-uffi-type (second uffi-type) :ffi))
               ;;'FFI:C-POINTER
               (error "I don't know what a ~S is in Lisp.")))
          (OTHERWISE
           (ERROR "INVALID TYPE ~S." UFFI-TYPE)))))
  ) ;;CONVERT-FROM-UFFI-TYPE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I. Declarations ;;
;;;;;;;;;;;;;;;;;;;;;


(DEFMACRO DEF-TYPE (NAME TYPE)
  "
DO:                 Defines a Common Lisp type based on a UFFI type.
NAME:               A symbol naming the type
TYPE:               A form that is evaluated that specifies the UFFI type.
IMPLEMENTATION:     For now, we generate `(DEFTYPE ,NAME T).
URL:                http://uffi.b9.com/manual/def-type.html
URL:                http://www.lisp.org/HyperSpec/Body/mac_deftype.html
"
  (setf type (clean-uffi-type type))
  `(DEFTYPE ,NAME T ,(CONVERT-FROM-UFFI-TYPE TYPE :CL))) ;;DEF-TYPE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; II. Primitive Types ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFMACRO DEF-CONSTANT (NAME VALUE &KEY (EXPORT NIL))
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
URL:                http://uffi.b9.com/manual/def-constant.html
URL:                http://www.lisp.org/HyperSpec/Body/mac_defconstant.html
URL:                http://www.lisp.org/HyperSpec/Body/fun_export.html
"
  `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (DEFCONSTANT ,NAME ,VALUE)
     ,(WHEN EXPORT (LIST 'EXPORT `(QUOTE ,NAME)))
     ',NAME)
  ) ;;DEF-CONSTANT



(DEFMACRO DEF-FOREIGN-TYPE (NAME TYPE)
  "
DO:                 Defines a new foreign type.
NAME:               A symbol naming the new foreign type.
VALUE:              A form that is not evaluated that defines
                    the new foreign type.
URL:                http://uffi.b9.com/manual/def-foreign-type.html
URL:                http://clisp.sourceforge.net/impnotes.html#def-c-type
"
  (LET* ((NAME NAME)
         (UFFI-TYPE (CLEAN-UFFI-TYPE type NAME))
         (FFI-TYPE  (CONVERT-FROM-UFFI-TYPE UFFI-TYPE :FFI)) )
    `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
       (SETF (GETHASH ',NAME *FOREIGN-TYPES-HASH*) ',UFFI-TYPE)
       (FFI:DEF-C-TYPE ,NAME ,FFI-TYPE)))) ;;DEF-FOREIGN-TYPE



(DEFMACRO NULL-CHAR-P (VAL)
  "
DO:                 Tests if a character or integer is NULL.
                    This abstracts the difference in implementations where
                    some return a character and some return an integer
                    when dereferencing a C character pointer.
CHAR:               A character or integer.
RETURN:             A boolean flag indicating if char is a NULL value.
URL:                http://uffi.b9.com/manual/null-char-p.html
"
  `(LET ((VAL ,VAL)) (IF (CHARACTERP VAL) (ZEROP (CHAR-CODE VAL)) (ZEROP VAL)))
  ) ;;NULL-CHAR-P



(DEFUN MAKE-CONSTANT-NAME (ENUM-NAME SEPARATOR-STRING CONSTANT-ID)
  "
PRIVATE
DO:                 Builds an enum constant name.
"
  (INTERN (FORMAT NIL "~A~A~A" ENUM-NAME SEPARATOR-STRING CONSTANT-ID))
  ) ;;MAKE-CONSTANT-NAME



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; III. Aggregate Types ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFMACRO DEF-ENUM (NAME CONSTANTS &KEY (SEPARATOR-STRING "#"))
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
URL:                http://uffi.b9.com/manual/def-enum.html
URL:                http://clisp.sourceforge.net/impnotes.html#def-c-enum
URL:                http://clisp.sourceforge.net/impnotes.html#def-c-type
"
  (LET ((C-CONSTANTS
         (MAPCAR
          (LAMBDA (CONSTANT)
            (COND
              ((SYMBOLP CONSTANT)
               (LIST (MAKE-CONSTANT-NAME NAME SEPARATOR-STRING CONSTANT))  )
              ((AND (CONSP CONSTANT)
                    (= 2 (LENGTH CONSTANT)) (INTEGERP (CADR CONSTANT)))
               (LIST (MAKE-CONSTANT-NAME NAME SEPARATOR-STRING (CAR CONSTANT))
                     (CADR CONSTANT)))
              (T
               (ERROR "INVALID ENUM CONSTANT SYNTAX: ~S." CONSTANT))))
          CONSTANTS)))
    `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
       (SETF (GETHASH ,NAME *FOREIGN-TYPES-HASH*) :INT)
       (FFI:DEF-C-TYPE  ,NAME  ,(CONVERT-FROM-UFFI-TYPE :INT :FFI))
       (FFI:DEF-C-ENUM  ,NAME  ,@C-CONSTANTS)))
  ) ;;DEF-ENUM


(DEFMACRO DEF-STRUCT (NAME &REST FIELDS)
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
URL:                http://uffi.b9.com/manual/def-struct.html
URL:                http://clisp.sourceforge.net/impnotes.html#def-c-struct
"
  (LET* ((NAME NAME)
         (UFFI-TYPE (CLEAN-UFFI-TYPE `(:STRUCT ,NAME ,@FIELDS) NAME))
         (FFI-TYPE  (CONVERT-FROM-UFFI-TYPE UFFI-TYPE :FFI)) )
    `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
       (SETF (GETHASH ',NAME *FOREIGN-TYPES-HASH*)
             (SETF (GETHASH ',NAME *FOREIGN-STRUCTS-HASH*) ',UFFI-TYPE))
       (FFI:DEF-C-STRUCT ,@(CDR FFI-TYPE))))
  ) ;;DEF-STRUCT

;; ,(CONVERT-FROM-UFFI-TYPE TYPE :CL)
;; (COM.INFORMATIMAGO.CLISP.UFFI::CLEAN-UFFI-TYPE '(* :unsigned-char) 'struct-name)
;; (setf name 'ldap-error fields '((e_code :int) (e_reason (* :unsigned-char))))


(DEFMACRO GET-SLOT-VALUE (OBJ TYPE FIELD)
  "
DO:                 Accesses a slot value from a structure.
OBJ:                A pointer to foreign structure.
TYPE:               A name of the foreign structure type.
FIELD:              A name of the desired field in foreign structure.
RETURN:             The value of the field in the structure.
SEE ALSO:           GET-SLOT-POINTER
URL:                http://uffi.b9.com/manual/get-slot-value.html
URL:                http://clisp.sourceforge.net/impnotes.html#slot
"
  (when (and (listp type) (eq 'quote (car type)))
    (setf type (second type)))
  ;; TODO: CHECK CONVERT TYPE.
  `(FFI:SLOT (FFI:DEREF (FFI:CAST (ffi:foreign-value ,OBJ) (* ,TYPE)))
             ,FIELD)) ;;GET-SLOT-VALUE



(DEFMACRO GET-SLOT-POINTER (OBJ TYPE FIELD)
  "
DO:                 Accesses a slot value from a structure.
OBJ:                A pointer to foreign structure.
TYPE:               A name of the foreign structure type.
FIELD:              A name of the desired field in foreign structure.
RETURN:             The value of the field in the structure: A POINTER.
NOTE:               This is similar to GET-SLOT-VALUE.
                    It is used when the value of a slot is a pointer type.
SEE ALSO:           GET-SLOT-VALUE
URL:                http://uffi.b9.com/manual/get-slot-pointer.html
URL:                http://clisp.sourceforge.net/impnotes.html#slot
"
  ;; NO DIFFERENCE TO ACCESS POINTER FIELD THAN TO ACCESS VALUE FIELDS.
  `(GET-SLOT-VALUE ,OBJ ,TYPE ,FIELD)
  ) ;;GET-SLOT-POINTER



(DEFMACRO DEF-ARRAY-POINTER (NAME TYPE)
  "
DO:                 Defines a type that is a pointer to an array of type.
NAME:               A name of the new foreign type.
TYPE:               The foreign type of the array elements.
URL:                http://uffi.b9.com/manual/def-array-pointer.html
URL:                http://clisp.sourceforge.net/impnotes.html#c-array-ptr
URL:                http://clisp.sourceforge.net/impnotes.html#def-c-type
"
  (LET* ((NAME NAME)
         (UFFI-TYPE (CLEAN-UFFI-TYPE `(:ARRAY-PTR ,TYPE)))
         (FFI-TYPE  (CONVERT-FROM-UFFI-TYPE UFFI-TYPE :FFI)) )
    `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
       (SETF (GETHASH ,NAME *FOREIGN-TYPES-HASH*) ,UFFI-TYPE)
       (FFI:DEF-C-TYPE ,NAME ,FFI-TYPE)))
  ) ;;DEF-ARRAY-POINTER



(DEFMACRO DEF-UNION (NAME &REST FIELDS)
  "
NAME:               A name of the new union type.
FIELDS:             A list of fields of the union.
DO:                 Defines a foreign union type.
URL:                http://uffi.b9.com/manual/def-union.html
URL:                http://clisp.sourceforge.net/impnotes.html#c-union
URL:                http://clisp.sourceforge.net/impnotes.html#def-c-type
"
  (LET* ((NAME NAME)
         (UFFI-TYPE (CLEAN-UFFI-TYPE `(:UNION ,NAME ,@FIELDS)))
         (FFI-TYPE  (CONVERT-FROM-UFFI-TYPE UFFI-TYPE :FFI)) )
    `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
       (SETF (GETHASH ,NAME *FOREIGN-TYPES-HASH*) ,UFFI-TYPE)
       (FFI:DEF-C-TYPE ,NAME ,FFI-TYPE)))
  ) ;;DEF-UNION



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IV. Objects ;;
;;;;;;;;;;;;;;;;;


(FFI:DEF-CALL-OUT MALLOC
    (:NAME "malloc")
  (:ARGUMENTS (SIZE FFI:UINT32 :IN))
  (:RETURN-TYPE FFI:C-POINTER)
  (:LANGUAGE :STDC)
  (:library "/lib/libc.so.6"))


(FFI:DEF-CALL-OUT FREE
    (:NAME "free")
  (:ARGUMENTS (PTR FFI:C-POINTER :IN))
  (:RETURN-TYPE NIL)
  (:LANGUAGE :STDC)
  (:library "/lib/libc.so.6"))


(DEFMACRO ALLOCATE-FOREIGN-OBJECT (TYPE &OPTIONAL (SIZE 1))
  "
DO:                 Allocates an instance of a foreign object.
TYPE:               The type of foreign object to allocate.
                    This parameter is evaluated.
SIZE:               An optional size parameter that is evaluated.
                    If specified, allocates and returns an array
                    of type that is size members long.
                    This parameter is evaluated.
RETURN:             A pointer to the foreign object.
URL:                http://uffi.b9.com/manual/allocate-foreign-object.html
URL:                
IMPLEMENTATION:     
"
  ;; TODO: CHECK IF TYPE IS CONTANT AND THE.N CHECK AND CONVERT
  ;;       IT AT COMPILE TIME.
  `(ffi:allocate-shallow (convert-from-uffi-type
                          (clean-uffi-type ,type) :ffi)
                         :count ,size)) ;;ALLOCATE-FOREIGN-OBJECT


(DEFMACRO FREE-FOREIGN-OBJECT (PTR)
  "
DO:                 Frees the memory used by the allocation of a foreign
                    object.
PTR:                A pointer to the allocated foreign object to free.
URL:                http://uffi.b9.com/manual/free-foreign-object.html
URL:
IMPLEMENTATION:     
"
  `(ffi:foreign-free ,ptr)
  ) ;;FREE-FOREIGN-OBJECT


(DEFMACRO WITH-FOREIGN-OBJECT ((VAR TYPE) &BODY BODY)
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
URL:                http://uffi.b9.com/manual/with-foreign-object.html
URL:
"
  `(LET ((,VAR (ALLOCATE-FOREIGN-OBJECT ,TYPE)))
     (UNWIND-PROTECT
          (PROGN ,@BODY)
       (FREE-FOREIGN-OBJECT ,VAR)))
  ) ;;WITH-FOREIGN-OBJECT


(DEFMACRO SIZE-OF-FOREIGN-TYPE (TYPE)
  "
FTYPE:              A foreign type specifier. This parameter is evaluated.
RETURN:             The number of data bytes used by a foreign object type.
                    This does not include any Lisp storage overhead.
URL:                http://uffi.b9.com/manual/size-of-foreign-type.html
URL:                http://clisp.sourceforge.net/impnotes.html#sizeof
"
  `(FFI:SIZEOF (CONVERT-FROM-UFFI-TYPE (CLEAN-UFFI-TYPE ,TYPE) :FFI))
  ) ;;SIZE-OF-FOREIGN-TYPE


(DEFMACRO POINTER-ADDRESS (PTR)
  "
PTR:                A pointer to a foreign object.
RETURN:             An integer representing the pointer's address.
URL:                http://uffi.b9.com/manual/pointer-address.html
URL:                http://clisp.sourceforge.net/impnotes.html#c-var-addr
"
  `(LET ((PTR ,PTR))
     (DECLARE (TYPE 'FFI:FOREIGN-ADDRESS PTR))
     (FFI::FOREIGN-ADDRESS-UNSIGNED PTR))
  ) ;;POINTER-ADDRESS


(DEFMACRO DEREF-POINTER (PTR TYPE)
  "
PTR:                A pointer to a foreign object.
TYPE:               A foreign type of the object being pointed to.
RETURN:             The value of the object where the pointer points.
URL:                http://uffi.b9.com/manual/deref-pointer.html
URL:                http://clisp.sourceforge.net/impnotes.html#deref
NOTE:               This is an accessor and can be used with SETF .
"
  `(FFI:DEREF (FFI:CAST (ffi:foreign-value ,PTR)
                        (CONVERT-FROM-UFFI-TYPE
                         (CLEAN-UFFI-TYPE (LIST '* ,TYPE)) :FFI)
                        ))
  ) ;;DEREF-POINTER


(DEFMACRO ENSURE-CHAR-CHARACTER (OBJECT)
  "
DO:                 Ensures that an object obtained by dereferencing
                    a :CHAR pointer is a character.
OBJECT:             Either a character or a integer specifying
                    a character code.
RETURN:             A character.
URL:                http://uffi.b9.com/manual/ensure-char-character.html
URL:
"
  `(LET ((OBJECT ,OBJECT))
     (IF (CHARACTERP OBJECT) OBJECT (CODE-CHAR OBJECT)))
  ) ;;ENSURE-CHAR-CHARACTER


(DEFMACRO ENSURE-CHAR-INTEGER (OBJECT)
  "
DO:                 Ensures that an object obtained by dereferencing
                    a :CHAR pointer is an integer.
OBJECT:             Either a character or a integer specifying
                    a character code.
RETURN:             An integer.
URL:                http://uffi.b9.com/manual/ensure-char-integer.html
URL:
"
  `(LET ((OBJECT ,OBJECT))
     (IF (CHARACTERP OBJECT) (CHAR-CODE OBJECT) OBJECT))
  ) ;;ENSURE-CHAR-INTEGER


(DEFMACRO MAKE-NULL-POINTER (TYPE)
  "
DO:                 Creates a NULL pointer of a specified type.
TYPE:               A type of object to which the pointer refers.
RETURN:             The NULL pointer of type TYPE.
URL:                http://uffi.b9.com/manual/make-null-pointer.html
URL:
"
  (declare (ignore type))
  (FFI::UNSIGNED-FOREIGN-ADDRESS 0)
  ;;  `(FFI:CAST (ffi:foreign-value (FFI::UNSIGNED-FOREIGN-ADDRESS 0))
  ;;              (CONVERT-FROM-UFFI-TYPE
  ;;               (CLEAN-UFFI-TYPE (LIST '* ,TYPE)) :FFI))
  ) ;;MAKE-NULL-POINTER



(DEFMACRO NULL-POINTER-P (PTR)
  "
DO:                 Tests if a pointer is has a NULL value.
PTR:                A foreign object pointer.
RETURN:             Whether ptr is NULL.
URL:                http://uffi.b9.com/manual/null-pointer-p.html
URL:                http://clisp.sourceforge.net/impnotes.html#fa-null
"
  `(FFI:FOREIGN-ADDRESS-NULL ,PTR)
  ) ;;NULL-POINTER-P


(DEFCONSTANT +NULL-CSTRING-POINTER+
  (FFI::UNSIGNED-FOREIGN-ADDRESS 0)
  ;;(FFI:CAST (ffi:foreign-value (FFI::UNSIGNED-FOREIGN-ADDRESS 0))
  ;;          (CONVERT-FROM-UFFI-TYPE (CLEAN-UFFI-TYPE :CSTRING) :FFI))
  "A NULL cstring pointer.
This can be used for testing if a cstring returned by a function is NULL.
URL:                http://uffi.b9.com/manual/null-cstring-pointer.html
"
  ) ;;+NULL-CSTRING-POINTER+


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; V. Strings ;;
;;;;;;;;;;;;;;;;


(DEFMACRO CONVERT-FROM-CSTRING (CSTRING)
  "
CSTRING:            A cstring.
RETURN:             A Lisp string.
DO:                 Converts a Lisp string to a cstring.
                    This is most often used when processing the
                    results of a foreign function that returns a
                    cstring.
URL:                http://uffi.b9.com/manual/convert-from-cstring.html
"
  `,CSTRING
  ) ;;CONVERT-FROM-CSTRING



(DEFMACRO CONVERT-TO-CSTRING (STRING)
  "
STRING:             A Lisp string.
RETURN:             A cstring.
DO:                 Converts a Lisp string to a cstring.
                    The cstring should be freed with free-cstring.
URL:                http://uffi.b9.com/manual/convert-to-cstring.html
"
  `,STRING
  ) ;;CONVERT-TO-CSTRING


(DEFMACRO FREE-CSTRING (CSTRING)
  "
CSTRING:            A cstring.
DO:                 Frees any memory possibly allocated by convert-to-cstring.
                    On some implementions, a cstring is just the Lisp
                    string itself.
"
  (declare (ignore cstring))
  ;; NOP
  ) ;;FREE-CSTRING


(DEFMACRO WITH-CSTRING ((CSTRING STRING) &BODY BODY)
  "
CSTRING:            A symbol naming the cstring to be created.
STRING:             A Lisp string that will be translated to a cstring.
BODY:               The body of where the CSTRING will be bound.
DO:                 Binds a symbol to a cstring created from conversion
                    of a string. Automatically frees the cstring.
URL:                http://uffi.b9.com/manual/with-cstring.html
"
  ;; `(let ((,cstring (convert-to-cstring ,string)))
  ;;    (unwind-protect
  ;;        (progn ,@body)
  ;;      (free-cstring ,cstring)))
  `(LET ((,CSTRING ,STRING))
     ,@BODY)
  ) ;;WITH-CSTRING


(defun foreign-string-length (foreign-string)
  (do ((len 0 (1+ len)))
      ((= 0 (ffi:element (ffi:foreign-value foreign-string) len))
       len))) ;;foreign-string-length


(DEFUN CONVERT-FROM-FOREIGN-STRING (FOREIGN-STRING
                                    &KEY LENGTH (NULL-TERMINATED-P T))
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
URL:        http://uffi.b9.com/manual/convert-from-foreign-string.html
URL:        http://clisp.sourceforge.net/impnotes.html#encoding
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
    (EXT:CONVERT-STRING-FROM-BYTES byte-vector CUSTOM:*FOREIGN-ENCODING*)  
    )) ;;CONVERT-FROM-FOREIGN-STRING


(DEFUN CONVERT-TO-FOREIGN-STRING (STRING)
  "
STRING:             A Lisp string.
RETURN:             A foreign string.
DO:                 Converts a Lisp string to a foreign string.
                    Memory should be freed with free-foreign-object.
URL:        http://uffi.b9.com/manual/convert-to-foreign-string.html
"
  (let* ((byte-vector
          (EXT:CONVERT-STRING-TO-BYTES string CUSTOM:*FOREIGN-ENCODING*))
         (result (ALLOCATE-FOREIGN-STRING (1+ (length byte-vector))))
         (foreign-type `(ffi:c-array 
                         ffi:uchar ,(list (1+ (length byte-vector))))))
    (declare (ignore foreign-type))     ; TODO!
    (dotimes (i (length byte-vector))
      (setf (ffi:element (ffi:foreign-value result) i)
            (aref byte-vector i)))
    (setf (ffi:element  (ffi:foreign-value result) (length byte-vector)) 0)
    result)) ;;CONVERT-TO-FOREIGN-STRING


(DEFUN ALLOCATE-FOREIGN-STRING (SIZE &KEY (UNSIGNED T))
  "
SIZE:               The size of the space to be allocated in bytes.
UNSIGNED:           A boolean flag with a default value of T.
                    When true, marks the pointer as an :UNSIGNED-CHAR.
RETURN:             A foreign string which has undefined contents.
DO:                 Allocates space for a foreign string.
                    Memory should be freed with free-foreign-object.
URL:            http://uffi.b9.com/manual/allocate-foreign-string.html
"
  (ALLOCATE-FOREIGN-OBJECT (if unsigned ':unsigned-char ':char) size)
  ) ;;ALLOCATE-FOREIGN-STRING


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VI. Functions & Libraries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFPARAMETER *MODULES-TO-LIBRARY-MAP* (MAKE-HASH-TABLE :TEST (FUNCTION EQUAL))
  "Maps module names to library paths.")


(DEFMACRO DEF-FUNCTION (NAME ARGS &KEY MODULE RETURNING)
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
URL:                http://uffi.b9.com/manual/def-function.html
NOTE:               All Common-Lisp implementations are 'case-insensitive'.
                    http://www.lisp.org/HyperSpec/Body/sec_2-1-1-2.html
"
  (let (l-name c-name)
    (if (stringp name)
        (setq c-name name
              l-name (intern (with-standard-io-syntax
                               (string-upcase
                                (substitute (character "-") (character "_")
                                            name)))))
        (setq c-name (first name)
              l-name (second name)))
    `(FFI:DEF-CALL-OUT
         ,l-name
         (:name ,c-name)
       ,@(when args
               `((:arguments
                  ,@(mapcar (lambda (arg)
                              `(,(first arg)
                                 ,(CONVERT-FROM-UFFI-TYPE 
                                   (clean-uffi-type (second arg)) :FFI)
                                 :in))
                            args))))
       ,@(when returning
               `((:return-type ,(CONVERT-FROM-UFFI-TYPE
                                 (clean-uffi-type returning) :FFI))))
       ,@(when module
               (let ((library (gethash module *MODULES-TO-LIBRARY-MAP*)))
                 `((:library  ,(or library module)))))
       (:LANGUAGE :STDC)))) ;;DEF-FUNCTION


(DEFUN LOAD-FOREIGN-LIBRARY (FILENAME &KEY MODULE SUPPORTING-LIBRARIES)
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
URL:                http://uffi.b9.com/manual/load-foreign-library.html
IMPLEMENTATION:     Loading the library is defered to the first function call.
                    Here we just register the mapping between the MODULE and
                    the FILENAME.
TODO:               Should we explicitely load the SUPPORTING-LIBRARIES too?
"
  (declare (ignore SUPPORTING-LIBRARIES))
  (when module
    (setf (gethash module *MODULES-TO-LIBRARY-MAP*) (namestring filename)))
  t) ;;LOAD-FOREIGN-LIBRARY


(DEFUN SPLIT-STRING (STRING &OPTIONAL (SEPARATORS " "))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (UNLESS (SIMPLE-STRING-P STRING)     (SETQ STRING     (COPY-SEQ STRING)))
  (UNLESS (SIMPLE-STRING-P SEPARATORS) (SETQ SEPARATORS (COPY-SEQ SEPARATORS)))
  (LET ((CHUNKS  '())
        (POSITION 0)
        (NEXTPOS  0)
        (STRLEN   (LENGTH STRING)) )
    (DECLARE (TYPE SIMPLE-STRING STRING SEPARATORS))
    (LOOP WHILE (< POSITION STRLEN)
       DO
       (LOOP WHILE (AND (< NEXTPOS STRLEN)
                        (NOT (POSITION (CHAR STRING NEXTPOS) SEPARATORS)))
          DO (SETQ NEXTPOS (1+ NEXTPOS))
          ) ;;loop
       (PUSH (SUBSEQ STRING POSITION NEXTPOS) CHUNKS)
       (SETQ POSITION (1+ NEXTPOS))
       (SETQ NEXTPOS  POSITION)
       ) ;;loop
    (NREVERSE CHUNKS)
    )) ;;SPLIT-STRING


(DEFUN FIND-FOREIGN-LIBRARY (NAMES DIRECTORIES &KEY DRIVE-LETTERS TYPES VERBOSE)
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
URL:                http://uffi.b9.com/manual/find-foreign-library.html
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


;;;; THE END ;;;;
