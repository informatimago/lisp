;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               gnu.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    CFFI interface to GNU libobjc.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-31 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2015
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")

(defpackage "COM.INFORMATIMAGO.OBJC.GNU"
  (:use)
  (:export

   "BACKEND" "BOOL" "CATEGORY" "CLASS" "CLASS_ADDIVAR"
   "CLASS_ADDMETHOD" "CLASS_ADDPROTOCOL" "CLASS_CONFORMSTOPROTOCOL"
   "CLASS_COPYIVARLIST" "CLASS_COPYMETHODLIST"
   "CLASS_COPYPROPERTYLIST" "CLASS_COPYPROTOCOLLIST"
   "CLASS_CREATEINSTANCE" "CLASS_GETCLASSMETHOD"
   "CLASS_GETCLASSVARIABLE" "CLASS_GETINSTANCEMETHOD"
   "CLASS_GETINSTANCESIZE" "CLASS_GETINSTANCEVARIABLE"
   "CLASS_GETIVARLAYOUT" "CLASS_GETMETHODIMPLEMENTATION"
   "CLASS_GETNAME" "CLASS_GETPROPERTY" "CLASS_GETSUPERCLASS"
   "CLASS_GETVERSION" "CLASS_GETWEAKIVARLAYOUT" "CLASS_ISMETACLASS"
   "CLASS_IVAR_SET_GCINVISIBLE" "CLASS_POINTER" "CLASS_REPLACEMETHOD"
   "CLASS_RESPONDSTOSELECTOR" "CLASS_SETIVARLAYOUT" "CLASS_SETVERSION"
   "CLASS_SETWEAKIVARLAYOUT" "DEPTH" "ID" "IMP" "IVAR" "IVAR_GETNAME"
   "IVAR_GETOFFSET" "IVAR_GETTYPEENCODING" "METHOD"
   "METHOD_COPYARGUMENTTYPE" "METHOD_COPYRETURNTYPE"
   "METHOD_EXCHANGEIMPLEMENTATIONS" "METHOD_GETARGUMENTTYPE"
   "METHOD_GETDESCRIPTION" "METHOD_GETIMPLEMENTATION" "METHOD_GETNAME"
   "METHOD_GETNUMBEROFARGUMENTS" "METHOD_GETRETURNTYPE"
   "METHOD_GETTYPEENCODING" "METHOD_SETIMPLEMENTATION" "NAME"
   "OBJC_ALIGNED_SIZE" "OBJC_ALIGNOF_TYPE" "OBJC_ALLOCATECLASSPAIR"
   "OBJC_ATOMIC_MALLOC" "OBJC_CALLOC" "OBJC_CONDITION"
   "OBJC_CONDITION_ALLOCATE" "OBJC_CONDITION_BROADCAST"
   "OBJC_CONDITION_DEALLOCATE" "OBJC_CONDITION_SIGNAL"
   "OBJC_CONDITION_T" "OBJC_CONDITION_WAIT" "OBJC_COPYPROTOCOLLIST"
   "OBJC_DISPOSECLASSPAIR" "OBJC_ENUMERATIONMUTATION"
   "OBJC_EXCEPTION_MATCHER" "OBJC_EXCEPTION_THROW" "OBJC_FREE"
   "OBJC_GETCLASS" "OBJC_GETCLASSLIST" "OBJC_GETMETACLASS"
   "OBJC_GETPROTOCOL" "OBJC_GETREQUIREDCLASS"
   "OBJC_GET_TYPE_QUALIFIERS" "OBJC_GET_UNKNOWN_CLASS_HANDLER"
   "OBJC_LAYOUT_FINISH_STRUCTURE" "OBJC_LAYOUT_STRUCTURE"
   "OBJC_LAYOUT_STRUCTURE_GET_INFO"
   "OBJC_LAYOUT_STRUCTURE_NEXT_MEMBER" "OBJC_LOOKUPCLASS"
   "OBJC_MALLOC" "OBJC_METHOD_DESCRIPTION" "OBJC_MSG_LOOKUP"
   "OBJC_MSG_LOOKUP_SUPER" "OBJC_MUTEX" "OBJC_MUTEX_ALLOCATE"
   "OBJC_MUTEX_DEALLOCATE" "OBJC_MUTEX_LOCK" "OBJC_MUTEX_T"
   "OBJC_MUTEX_TRYLOCK" "OBJC_MUTEX_UNLOCK" "OBJC_OBJECT"
   "OBJC_PROMOTED_SIZE" "OBJC_PROPERTY_T" "OBJC_REALLOC"
   "OBJC_REGISTERCLASSPAIR" "OBJC_SETENUMERATIONMUTATIONHANDLER"
   "OBJC_SETEXCEPTIONMATCHER" "OBJC_SETGETUNKNOWNCLASSHANDLER"
   "OBJC_SETUNCAUGHTEXCEPTIONHANDLER" "OBJC_SET_THREAD_CALLBACK"
   "OBJC_SIZEOF_TYPE" "OBJC_SKIP_ARGSPEC" "OBJC_SKIP_OFFSET"
   "OBJC_SKIP_TYPESPEC" "OBJC_SKIP_TYPE_QUALIFIERS"
   "OBJC_STRUCT_LAYOUT" "OBJC_SUPER" "OBJC_SYNC_ENTER"
   "OBJC_SYNC_EXIT" "OBJC_SYNC_NOT_INITIALIZED"
   "OBJC_SYNC_NOT_OWNING_THREAD_ERROR" "OBJC_SYNC_SUCCESS"
   "OBJC_SYNC_TIMED_OUT" "OBJC_THREAD_ADD"
   "OBJC_THREAD_BACKGROUND_PRIORITY" "OBJC_THREAD_CALLBACK"
   "OBJC_THREAD_DETACH" "OBJC_THREAD_EXIT" "OBJC_THREAD_GET_DATA"
   "OBJC_THREAD_GET_PRIORITY" "OBJC_THREAD_ID"
   "OBJC_THREAD_INTERACTIVE_PRIORITY" "OBJC_THREAD_LOW_PRIORITY"
   "OBJC_THREAD_REMOVE" "OBJC_THREAD_SET_DATA"
   "OBJC_THREAD_SET_PRIORITY" "OBJC_THREAD_T" "OBJC_THREAD_YIELD"
   "OBJC_UNCAUGHT_EXCEPTION_HANDLER" "OBJECT_COPY" "OBJECT_DISPOSE"
   "OBJECT_GETCLASS" "OBJECT_GETCLASSNAME" "OBJECT_GETINDEXEDIVARS"
   "OBJECT_GETINSTANCEVARIABLE" "OBJECT_GETIVAR" "OBJECT_SETCLASS"
   "OBJECT_SETINSTANCEVARIABLE" "OBJECT_SETIVAR" "ORIGINAL_TYPE"
   "OWNER" "PREV_TYPE" "PROPERTY" "PROPERTY_GETATTRIBUTES"
   "PROPERTY_GETNAME" "PROTOCOL" "PROTOCOL_CONFORMSTOPROTOCOL"
   "PROTOCOL_COPYMETHODDESCRIPTIONLIST" "PROTOCOL_COPYPROPERTYLIST"
   "PROTOCOL_COPYPROTOCOLLIST" "PROTOCOL_GETMETHODDESCRIPTION"
   "PROTOCOL_GETNAME" "PROTOCOL_GETPROPERTY" "PROTOCOL_ISEQUAL"
   "RECORD_ALIGN" "RECORD_SIZE" "SEL" "SELF"
   "SEL_COPYTYPEDSELECTORLIST" "SEL_GETNAME" "SEL_GETTYPEDSELECTOR"
   "SEL_GETTYPEENCODING" "SEL_GETUID" "SEL_ISEQUAL" "SEL_REGISTERNAME"
   "SEL_REGISTERTYPEDNAME" "SUPER_CLASS" "TYPE" "TYPES" "_C_ARY_B"
   "_C_ARY_E" "_C_ATOM" "_C_BFLD" "_C_BOOL" "_C_BYCOPY" "_C_BYREF"
   "_C_CHARPTR" "_C_CHR" "_C_CLASS" "_C_COMPLEX" "_C_CONST" "_C_DBL"
   "_C_FLT" "_C_GCINVISIBLE" "_C_ID" "_C_IN" "_C_INOUT" "_C_INT"
   "_C_LNG" "_C_LNG_DBL" "_C_LNG_LNG" "_C_ONEWAY" "_C_PTR" "_C_SEL"
   "_C_SHT" "_C_STRUCT_B" "_C_STRUCT_E" "_C_UCHR" "_C_UINT" "_C_ULNG"
   "_C_ULNG_LNG" "_C_UNDEF" "_C_UNION_B" "_C_UNION_E" "_C_USHT"
   "_C_VECTOR" "_C_VOID" "_F_BYCOPY" "_F_BYREF" "_F_CONST"
   "_F_GCINVISIBLE" "_F_IN" "_F_INOUT" "_F_ONEWAY" "_F_OUT"
   "_OBJC_LOAD_CALLBACK" "__GNU_LIBOBJC__" "__OBJC_INIT_THREAD_SYSTEM"
   "__OBJC_MSG_FORWARD" "__OBJC_MSG_FORWARD2"
   "__OBJC_THREAD_EXIT_STATUS"))


(defpackage "COM.INFORMATIMAGO.OBJC"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.OBJC.GNU")
  (:shadowing-import-from "COM.INFORMATIMAGO.OBJC.GNU"
                          "CLASS" "METHOD" "TYPE")
  (:export))

(in-package "COM.INFORMATIMAGO.OBJC")


(ql:quickload :cffi)
(pushnew #P"/usr/lib/gcc/x86_64-linux-gnu/4.7/" cffi:*foreign-library-directories* :test (function equalp)) 
(cffi:use-foreign-library "libobjc.so")
(load "objc.lisp")


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defparameter *type-map*
    '((:id . :pointer)
      (:class . :pointer)
      (:sel . :pointer)
      (:imp . :pointer)))

  (defun objc-to-cffi-type (type)
    (or (cdr (assoc type *type-map*)) type))

  (deftype name () '(or string symbol))

  (defun namep (name)
    (typecase name
      (string    name)
      (symbol    (symbol-name name))
      (otherwise nil)))
  
  (defun lisp-to-objc-classname (name)
    (namep name))

  );eval-when


(defmacro define-objc-class (name superclass ivars &rest options)
  (declare (ignore options))
  (check-type name name)
  (check-type superclass name)
  `(create-objc-class ,(namep name) ,(namep superclass)
                      ',(mapcar (lambda (ivar)
                                  (etypecase ivar
                                    (name
                                     (list (namep ivar) :pointer))
                                    (cons
                                     (check-type (first ivar) name)
                                     (check-type (second ivar) (or keyword cons))
                                     (list (namep (first ivar)) (objc-to-cffi-type (second ivar))))))
                                ivars)))

;; (macroexpand '(define-objc-class |Test| "Object"
;;    ((i :int)
;;     (name :string))))
;; (create-objc-class "Test" "Object" '(("I" :int) ("NAME" :string)))


;; (cffi:foreign-type-size :id)


(defparameter *nil* (cffi-sys:null-pointer))

(defun get-class-names ()
  (let ((max (* 128 1024)))
    (cffi:with-foreign-objects
     ((classes 'com.informatimago.objc.gnu:class max))
     (let ((nclasses (com.informatimago.objc.gnu:objc_getclasslist classes max)))
       (loop :for i :below (min max nclasses) 
             :collect (com.informatimago.objc.gnu:class_getname
                      (cffi:mem-aref classes 'com.informatimago.objc.gnu:class i)))))))

(defun get-class (name)
  (com.informatimago.objc.gnu:objc_getclass name))


(let ((test1 (com.informatimago.objc.gnu:objc_allocateclasspair *nil* "Test1" 0)))
  (com.informatimago.objc.gnu:objc_registerclasspair test1)
  test1)

(get-class "Test1")




;; (get-class "Object") ; #<A Foreign Pointer #x7F1C270E0DC0>
;; (get-class-names) ; ("NXConstantString" "Object" "Protocol")

;; (objc_allocateClassPair)


#-(and)
(let ((exports '()))
  (dolist (form (com.informatimago.common-lisp.cesarum.file:sexp-list-file-contents
                 #P"~/src/public/lisp/objcl/gnu/objc.lisp"))
    (case (first form)
      ((in-package defmacro eval-when))
      ((cffi:defcfun)
       (destructuring-bind (d (cn ln) &rest r) form
         (declare (ignore d cn r))
         (push (symbol-name ln) exports)))
      ((cffi:defcvar)
       (destructuring-bind (d (cn ln) &rest r) form
         (declare (ignore d cn r))
         (push (symbol-name ln) exports)))
      ((cffi:defcstruct)
       (destructuring-bind (d n &rest slots) form
         (declare (ignore d))
         (push (symbol-name n) exports)
         (loop :for (n type) :in slots
               :do (push (symbol-name n) exports))))
      ((defconstant cffi:defctype)
       (push (symbol-name (second form)) exports))
      (otherwise
       (if (string= 'defanonenum (first form))
         (loop :for value :in (rest form)
               :do (push (symbol-name (if (listp value)
                                        (first value)
                                        value)) exports))
         (print form)))))
  (sort (remove-duplicates exports :test (function string=))
        (function string<)))


;; (find-package "COM.INFORMATIMAGO.OBJC.GNU")

#||

(cffi:foreign-library-name  *libobjc*)
(dolist (dir cffi:*FOREIGN-LIBRARY-DIRECTORIES*)
  (print (merge-pathnames (cffi:foreign-library-pathname *libobjc*) dir)))
(truename (cffi:foreign-library-pathname *libobjc*))

(dolist (lib (cffi:list-foreign-libraries))
  (princ-to-string lib))
(length "LIBOBJC\\.SO.4-8900")


||#

