;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;
;;;;
;;;;    Note: the DEFINE-PACKAGE macro is obsolete.  Instead,
;;;;    use a CL-USER::ALSO-USE-PACKAGES declaration and a normal DEFPACKAGE form.
;;;;    For example:
;;;;
;;;;        (in-package "COMMON-LISP-USER")
;;;;        (declaim (declaration also-use-packages))
;;;;        (declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ISO3166"))
;;;;        (defpackage "COM.INFORMATIMAGO.EXAMPLE"
;;;;          (:use "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
;;;;          (:export "EXAMPLE"))
;;;;        (in-package "COM.INFORMATIMAGO.EXAMPLE")
;;;;        ... (com.informatimago.common-lisp.cesarum.iso3166:get-country 4)
;;;;
;;;;    This declaration is used by make-depends to build the ASD files,
;;;;    and generate other stuff.  It's better to split declaims, IIRC some
;;;;    implementation like it better like this.
;;;;
;;;;
;;;;    Obsolete:
;;;;
;;;;        Exports a DEFINE-PACKAGE macro and LOAD-PACKAGE function
;;;;        (amongst others) that map packages names (styled as:
;;;;        COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE to logical pathnames in
;;;;        the "PACKAGES" virtual host:
;;;;    
;;;;          "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE.LISP"
;;;;    
;;;;        The object files are mapped to:
;;;;    
;;;;          "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;~
;;;;           OBJ-${IMPL_TYPE}-${IMPL_VERS}-${MACH_TYPE} ;PACKAGE.${OBJECT-TYPE}"
;;;;        
;;;;        Improvements over DEFPACKAGE include:
;;;;     
;;;;            - allow to specify packages refered to (used) while not
;;;;              importing ("inheriting") any of it symbols; (:USE package)
;;;;    
;;;;            - allow to do it while renaming (nicknaming) the package;
;;;;              (:USE package :AS nickname)
;;;;    
;;;;            - allow to specify that all symbols exported by a given package
;;;;              are to be imported. (:FROM package :IMPORT :ALL)
;;;;    
;;;;        The first and second points help declare package dependencies without
;;;;        using the deprecated REQUIRE, PROVIDE and *MODULES*.  This is done
;;;;        by implementing a systematic way to load packages (from a PACKAGE:
;;;;        logical host with logical pathname translations).
;;;;        This allows MAKE-DEPENDS to build automatically the dependency graph, 
;;;;        and LOAD-PACKAGE to load automatically the dependencies without
;;;;        the need to write an ASDF or DEFSYSTEM file.
;;;;    
;;;;        The last point, along with the (:FROM package :IMPORT symbol...) form
;;;;        correct the naming of the :USE clause of DEFPACKAGE.
;;;;    
;;;;        Other more obscure clauses of DEFPACKAGE (:SHADOW,
;;;;        :SHADOWING-IMPORT-FROM, :INTERN) have no equivalent
;;;;        to provide a more controled package management.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-12-27 <PJB> Added double-RENAME-PACKAGE to ADD-NICKNAME.
;;;;    2005-03-17 <PJB> Added LIST-ALL-SYMBOLS LIST-EXTERNAL-SYMBOLS
;;;;                     COPY-PACKAGE
;;;;    2005-01-13 <PJB> Renamed *VERBOSE* to *PACKAGE-VERBOSE*.
;;;;                     Note: LOAD-PACKAGE uses *LOAD-VERBOSE*.
;;;;    2004-12-19 <PJB> LOAD-PACKAGE won't load a package found by FIND-PACKAGE.
;;;;    2003-05-06 <PJB> Created.
;;;;BUGS
;;;;    Nicknames assigned with (:USE package :AS nickname) are created in
;;;;    the global package name space.  They ought to be created in a
;;;;    package name space local to the package being defined...
;;;;
;;;;    We should probably implement a hierarchical package system like Franz's.
;;;;
;;;;    ADD-NICKNAME:  RENAME-PACKAGE cannot not change the name, (undefined consequences)
;;;;                   so we'd have to go thru a temporary name.
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2005
;;;;    mailto:pjb@informatimago.com
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"
  (:DOCUMENTATION
   "This package exports a macro used to declare a package.

    Copyright Pascal J. Bourguignon 2003 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:USE "COMMON-LISP")
  (:EXPORT "PACKAGE-EXPORTS" ;; missing from CL or not?
           "*PACKAGES*" "PACKAGE-PATHNAME" "LOAD-PACKAGE"
           "PACKAGE-SYSTEM-DEFINITION"
           "ADD-TRANSLATIONS" "ADD-NICKNAME" "*PACKAGE-VERBOSE*"
           ;; utility:
           "LIST-SYMBOLS" "LIST-ALL-SYMBOLS" "LIST-EXTERNAL-SYMBOLS"
           "COPY-PACKAGE"
           ;; debugging help:
           "CRACK-OPEN-PACKAGE"
           ;; Obsolete: define-package
           "DEFINE-PACKAGE"))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE")


(defun list-symbols (package &key (sorted t)
                     (homely nil) (exported nil) (shadowing nil) (all nil))
  "
PACKAGE:   A package designator.
SORTED:    (boolean) Whether the resulting list is sorted on symbol-names.
HOMELY:    (boolean) selects the symbols whose home package is PACKAGE.
EXPORTED:  (boolean) selects the exported symbols.
SHADOWING: (boolean) selects the symbols on the shadowing list.
ALL:       (boolean) selects all the symbols interned in the PACKAGE.
RETURN:    A list of the selected symbols.
           By default, only the HOMELY symbols.
"
  (setf homely (or homely (not (or exported shadowing  all))))
  (let ((pack (find-package package)))
    (if pack
      (let ((sl '()))
        (and exported
             (do-external-symbols (s pack)
               (push s sl)))
        (do-symbols (s pack) 
          (when (or all
                    (and homely (eq pack (symbol-package s)))
                    (and shadowing (member s (package-shadowing-symbols pack))))
            (pushnew s sl)))
        (if sorted (sort sl (function string<)) sl))
      (error "No package ~S" package))))

(defun list-all-symbols (package &key (sorted t))
  (list-symbols package :sorted sorted :all t))

(defun list-external-symbols (package &key (sorted t))
  (list-symbols package :sorted sorted :exported t))


(defun copy-package (old-package new-name)
  (let ((new-package (make-package new-name))
        (symbols (list-external-symbols old-package :sorted nil)))
    (import symbols new-package)
    (export symbols new-package)
    new-package))

(defun crack-open-package (package)
  "
NOTE:    USE-PACKAGE only imports exported symbols.
         This function imports into the current package all the symbols
         present in the PACKAGE.
"
  (do-symbols (sym package) (shadowing-import sym)) )

;;----------------------------------------------------------------------


(DEFVAR *PACKAGE-VERBOSE* NIL)
(DEFPARAMETER *VOUT* T "Verbose output stream.")
(defmacro verbose (fctrl &rest args)
  `(when *package-verbose* (format *vout* ,fctrl ,@args)))

(DEFMACRO WHILE (CONDITION &BODY BODY)  `(DO () ((NOT ,CONDITION))  ,@BODY))


(DEFUN PACKAGE-EXPORTS (PACKAGE)
  "
RETURN:   A new list of exported symbols from PACKAGE.
"
  (LET ((RESULT NIL))
    (DO-EXTERNAL-SYMBOLS (SYM PACKAGE RESULT)
      (PUSH SYM RESULT))))
  


(DEFUN PACKAGE-PATHNAME (PACKAGE)
  "
RETURN:  The logical pathname to the given package.
NOTE:    If a nickname is given, then a nickname pathname is returned.
"
  (COND
   ((SIMPLE-STRING-P PACKAGE)
    (#+(or allegro ccl) (lambda (designator)
                    ;; Allegro logical pathnames must be written in lowcase
                    ;; to produce lowcase physical pathnames.
                    ;; TODO: Copy the files to an implementation specific LFS.
                    (if (stringp designator)
                        (let ((colon (position #\: designator)))
                          (format nil "~:@(~A~)~(~A~)"
                                  (subseq designator 0 colon)
                                  (subseq designator colon)))
                        designator))
     #-(or allegro ccl) identity
     (CONCATENATE 'STRING
                  "PACKAGES:"
                  (SUBSTITUTE (CHARACTER ";") (CHARACTER ".") PACKAGE)
                  ".LISP")))
    ((STRINGP PACKAGE)  (PACKAGE-PATHNAME (COPY-SEQ PACKAGE)))
    ((SYMBOLP PACKAGE)  (PACKAGE-PATHNAME (STRING PACKAGE)))
    ((PACKAGEP PACKAGE) (PACKAGE-PATHNAME (PACKAGE-NAME PACKAGE)))
    (T (ERROR "~S is not a package designator." package))))


(defun package-system-definition (system)
  "
This is a system definition search function for ASDF.
It will map a system such as :com.informatimago.common-lisp
to the package path: PACKAGE:COM;INFORMATIMAGO;COMMON-LISP;SYSTEM.ASD
"
  ;; (print `(package-system-definition ,system))
  ;; (values-list (print (multiple-value-list)))
  (let ((name (typecase system
                (symbol (string-downcase (symbol-name system)))
                (string system)
                (t nil))))
    (when (and name (position (character ".") name))
      ;; otherwise process with the normal functions
      (let ((file (make-pathname
                   :type "asd"
                   ;; :case :common ;; strange stuff in allegro...
                   :defaults (PACKAGE-PATHNAME
                              (concatenate 'string name ".system")))))
        (values (when (probe-file file) file) file)))))


(DEFVAR *BUILT-IN-PACKAGES*
  (MAPCAN (LAMBDA (PACK)
            (CONS (PACKAGE-NAME PACK)
                  (COPY-LIST (PACKAGE-NICKNAMES PACK))))
          (LIST-ALL-PACKAGES)))


(DEFUN BUILT-IN-P (PACKAGE)
  (MEMBER PACKAGE *BUILT-IN-PACKAGES* :TEST (FUNCTION STRING=)))


(DEFVAR *PACKAGES* NIL
  "
We cannot use COMMON-LISP:*MODULES* since it's deprecated, so here is our own.
This is an a-list (file-pathname . (package-name)).
Note: the main data item is the file-pathname; the package-name is
kept as a human readable item; caveat: packages can be renamed!
")


(DEFUN CLOSE-PATH (PACKAGE)
  "
RETURN:     Closure of the translation of the package pathname of PACKAGE.
"
  (DO* ((PATH (PACKAGE-PATHNAME PACKAGE) NEXT)
        (NEXT (TRANSLATE-LOGICAL-PATHNAME PATH)
              (TRANSLATE-LOGICAL-PATHNAME PATH))
        (COUNT 0 (1+ COUNT)))
       ((STRING= (NAMESTRING PATH) (NAMESTRING NEXT))   
        PATH)
    (WHEN (< 100 COUNT)
      (ERROR "Cannot close the logical path for package ~S in less ~
              than 100 steps: probably a loop!~%" PACKAGE))))


(declaim (inline REGISTEREDP))
(DEFUN REGISTEREDP (PACKAGE)
  "
RETURN:     Whether the PACKAGE is already registered.
"
  (LET ((PATH (namestring (CLOSE-PATH PACKAGE))))
    (MEMBER PATH *PACKAGES* :key (function car) :TEST (FUNCTION STRING=))))


(DEFUN REGISTER (PACKAGE)
  "
DO:         Force registering the PACKAGE into the loaded *PACKAGES*.
"
  (LET ((PATH (namestring (CLOSE-PATH PACKAGE))))
    (verbose "~&# PACKAGE:REGISTER PACKAGE = ~S~%~
              ~&#                  PATH    = ~S~%~:[~
              ~&#                  IS NEW!~%~;~]"
             PACKAGE path (REGISTEREDP PACKAGE))
    (PUSHNEW (list PATH (if (stringp package)
                            package
                            (package-name package))) *PACKAGES*
             :key (function car) :TEST (FUNCTION STRING=))))


(defun implementation-id ()
  (flet ((first-word 
             (text) (let ((pos (position (character " ") text)))
                      (remove (character ".") (if pos (subseq text 0 pos) text)))))
    (format nil "~A-~A-~A"
            (first-word (lisp-implementation-type))
            (first-word (lisp-implementation-version))
            (first-word (machine-type)))))


(defun implementation-object-type ()
  (pathname-type
   (compile-file-pathname (merge-pathnames "TEST.LSP" (user-homedir-pathname)))))

#||
(handler-case
    (let ((source (merge-pathnames "TEST.LSP" (user-homedir-pathname))))
      (unwind-protect
           (progn 
             (with-open-file (out source :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :error)
               (print '(defun test (&rest args)  args) out))
             (let ((object (compile-file source :verbose nil :print nil)))
               (delete-file object)
               (pathname-type object)))
        (delete-file source)))
  (error () (error "What's the file type for a compiled file?"))))
||#

(defun object-dir (path)
  (merge-pathnames
   (make-pathname
    :directory (list :relative (format nil "OBJ-~:@(~A~)" (implementation-id)))
    :type (implementation-object-type)
    :defaults path)
   path))


(DEFUN LOAD-PACKAGE (PACKAGE-NAME
                     &KEY (VERBOSE *LOAD-VERBOSE*) (PRINT *LOAD-PRINT*)
                     (IF-DOES-NOT-EXIST :ERROR)
                     (EXTERNAL-FORMAT :DEFAULT))
  "
DO:         Unless it's already loaded (listed in *PACKAGES* or found
            by FIND-PACKAGE),  loads the package named PACKAGE-NAME.
NOTE:       We both use REGISTER in DEFINE-PACKAGE  and  in PACKAGE::LOAD
            because we may be loading a nickname.
RETURN:     The package named PACKAGE-NAME if found, or NIL.
"
  (or (find-package package-name)
      (LET ((PATH (CLOSE-PATH (STRING PACKAGE-NAME))))
        (VERBOSE "~&# LOADING PACKAGE NAME ~S FROM ~S~%~:[~
                  ~&#   NEW PACKAGE.~%
                  ~&#   PACKAGE ALREADY KNOWN.~%~;~]" 
                 PACKAGE-NAME PATH (registeredp package-name))
        (UNLESS (registeredp package-name)
          (PROG1
              (or
               (COMMON-LISP:LOAD (object-dir PATH)
                                 :VERBOSE VERBOSE
                                 :PRINT PRINT
                                 :IF-DOES-NOT-EXIST nil
                                 :EXTERNAL-FORMAT EXTERNAL-FORMAT)
               (COMMON-LISP:LOAD PATH
                                 :VERBOSE VERBOSE
                                 :PRINT PRINT
                                 :IF-DOES-NOT-EXIST IF-DOES-NOT-EXIST
                                 :EXTERNAL-FORMAT EXTERNAL-FORMAT))
            (register package-name)
            (VERBOSE "~&# LOAD-PACKAGE ~S DONE~%~
                      ~&# *PACKAGES*= ~S~%" PACKAGE-NAME  *packages*))))))
  

(DEFUN ADD-TRANSLATIONS (&REST TRANSLATIONS)
  "
DO:       Prepend the TRANSLATIONS to the list of logical pathname
          translations of the PACKAGES: logical host.
          These translations may concern either the long names of package:
             the package COM.INFORMATIMAGO.COMMON-LIST.CESARUM.UTILITY
             is loaded from PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;CESARUM;UTILITY
             which could be translated to:
             /usr/share/lisp/packages/com/informatimago/common-lisp/cesarum/utility.lisp
          or abstract, short nicknames:
             the package nicknamed DICTIONARY
             would be loaded from PACKAGES:DICTIONARY
             which could be translated to:
                     PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;HASH-DICT
             or to:  PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;BIN-TREE-DICT
"
  (SETF (LOGICAL-PATHNAME-TRANSLATIONS "PACKAGES")
        (NCONC (mapcar (lambda (item)
                         (list
                          ;; TODO: This is most certainly not portable; check it:
                          (merge-pathnames (first item)
                                           (make-pathname :Host "PACKAGES")
                                           nil)
                          (second item)))
                       TRANSLATIONS)
               (HANDLER-CASE  (LOGICAL-PATHNAME-TRANSLATIONS "PACKAGES")
                 (ERROR NIL)))))


;; (DEFINE-PACKAGE COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS.MAKE-DEPENDS
;;   (:NICKNAMES NAME1 NAM2) ; use (:USE pack :AS nick) in client rather!
;;   (:DOCUMENTATION "BLAH BLAH")
;;   (:FROM COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST   :IMPORT :ALL)
;;   (:FROM COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING :IMPORT SYM1 SYM2 SYM3)
;;   (:USE COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY :AS UTIL)
;;   (:USE COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DICTIONARY)
;;   (:EXPORT EXP1 EXP2 EXP3))
;;
;;
;; package should  be the long name  package, that may be  converted to a
;; package logical pathname.
;;
;; But users may want to use  a nickname, to allow for late (compilation)
;; time selection of the exact package to be used.
;;
;; So, given  a package, either  we can find  it in the PACKAGES: logical
;; volume, or we can find it under this nickname and we load it under its
;; long name.
;;
;; We can find it under this nickname only if it's already loaded, so for
;; compilation file by file, we may need additional directive to make the
;; correspondance:
;;
;; (DEFNICKNAME nickname longname)
;; --> load longname and  rename to nickname (in case  it had not already
;;     that nickname)
;;
;; longnames are converted to package paths as follow:
;;
;;    - the dots in the long name are replaced by semicolons;
;;    - a type of "LISP" is appended;
;;    - the "PACKAGE:" logical host name is prefixed.
;;
;; for all packages in :FROM and in :USE, do
;;    find and load the package.
;;    (we must load it for the macro it may contain)
;;    if the package is :USE :AS, then rename it.
;; done
;; :NICNAMES                       --> DEFPACKAGE :NICNAMES
;; :DOCUMENTATION                  --> DEFPACKAGE :DOCUMENTATION
;; :FROM package :IMPORT :ALL      --> DEFPACKAGE :USE package
;; :FROM package :IMPORT symbol... --> DEFPACKAGE :IMPORT-FROM  package sym...
;; :USE package :AS nickname       --> LOAD "PACKAGES:package" / rename
;; :USE package                    --> LOAD "PACKAGES:package"
;; :EXPORT symbol...               --> DEFPACKAGE :EXPORT symbol...
;;                                 --> IN-PACKAGE package


(DEFUN STRINGIFY (ITEMS)
  "
ITEMS:      A list of symbol, keyword or string.
RETURN:     A new list of strings, the symbol-names
            or the strings given in ITEMS.
"
  (DO* ((ITEMS ITEMS (CDR ITEMS))
        (ITEM (CAR ITEMS) (CAR ITEMS))
        (RESULT NIL))
      ((NULL ITEMS) RESULT)
    (COND
     ((STRINGP ITEM) (PUSH ITEM RESULT))
     ((SYMBOLP ITEM) (PUSH (SYMBOL-NAME ITEM) RESULT))
     (T (ERROR "~S is not a symbol, keyword or string!~%" ITEM)))))


(DEFUN PARSE-PACKAGE-DECLARATIONS (DECLARATIONS)
  "
DO:         Parses and check somewhat the syntax of DECLARATIONS.
RETURN:     a list of dependencies (package names);
            a list of renames (package . nickname);
            arguments for DEFPACKAGE.
"
  (DO* ((DECLARATIONS DECLARATIONS (CDR DECLARATIONS))
        (DECL (CAR DECLARATIONS) (CAR DECLARATIONS))
        (DEPENDENCIES NIL)
        (RENAMES      NIL)
        (NICKNAMES    NIL)
        (USES         NIL)
        (RESULT       NIL))
      ((NULL DECLARATIONS)
       (PROGN
         (WHEN USES      (PUSH (CONS :USE USES) RESULT))
         (WHEN NICKNAMES (PUSH (CONS :NICKNAMES NICKNAMES) RESULT))
         (VALUES DEPENDENCIES  RENAMES  RESULT)))
    (CASE (CAR DECL)
      (:NICKNAMES
       (SETQ NICKNAMES (NCONC (STRINGIFY (CDR DECL)) NICKNAMES)))
      (:DOCUMENTATION
       (PUSH DECL RESULT))
      (:SHADOW
       (PUSH (CONS :SHADOW (STRINGIFY (CDR DECL))) RESULT))
      (:SIZE
       (PUSH DECL RESULT))
      (:FROM
       (LET* ((FROM-PKG-NAME (STRING (NTH 1 DECL))))
         (PUSH FROM-PKG-NAME DEPENDENCIES)
         (UNLESS (EQ :IMPORT (NTH 2 DECL))
           (ERROR "Missing :IMPORT after :FROM ~S.~%" FROM-PKG-NAME))
         (IF (EQ :ALL (NTH 3 DECL))
           (IF (< 4 (LENGTH DECL))
             (ERROR "Unexpected ~S after :ALL." (NTH 4 DECL))
             (PUSH FROM-PKG-NAME USES))
           (PUSH (CONS :IMPORT-FROM (CONS FROM-PKG-NAME
                                          (STRINGIFY (CDDDR DECL)))) RESULT))
         ))
      (:USE
       (LET* ((FROM-PKG-NAME (STRING (NTH 1 DECL))))
         (PUSH FROM-PKG-NAME DEPENDENCIES)
         (WHEN (< 2 (LENGTH DECL))
           (UNLESS (EQ :AS (NTH 2 DECL))
             (ERROR "Expected :AS in :USE clause, not ~S." (NTH 2 DECL)))
           (UNLESS (= 4 (LENGTH DECL))
             (ERROR "Unexpected ~S after :AS ~A." (NTH 4 DECL) (NTH 3 DECL)))
           (PUSH (CONS FROM-PKG-NAME (STRING (NTH 3 DECL))) RENAMES))
         ))
      (:EXPORT
       (PUSH (CONS :EXPORT (STRINGIFY (CDR DECL))) RESULT))
      (OTHERWISE
       (ERROR "Unexpected clause ~S." DECL)))))


(defun remove-nickname (PACKAGE NICKNAME)
  "
DO:      Remove the NICKNAME from the list of nicknames
         of the package designated by PACKAGE.
RETURN:  The package designated by PACKAGE.
"
  (let ((package (FIND-PACKAGE PACKAGE)))
    (rename-package package
                    (package-name package)
                    (remove nickname (package-nicknames package)
                            :test (function string=)))))


(defun gen-old-name (packname)
  (let* ((dash (position #\- packname :from-end t))
         (packname
          (if (and (< 4 (length packname))
                   (string= "OLD-" packname :end2 4)
                   (every (function digit-char-p) (subseq packname (1+ dash))))
              (subseq packname 4 (position #\- packname :from-end t))
              packname)))
    (loop
       :for i :from 0
       :for name = (format nil "OLD-~A-~D" packname i)
       :while (find-package name)
       :finally (return name))))


(defun add-nickname (package nickname &key steal force)
  "
DO:         Add the NICKNAME to the PACKAGE.
STEAL:      If another package has already this nickname, then steal it.
FORCE:      If another package has this nickname as package name, then steal it.
RETURN:     The package designated by PACKAGE.
"
  ;; !!! The consequences are undefined if new-name or any new-nickname
  ;; !!! conflicts with any existing package names.  Therefore we use a temp name.
  (verbose "~&# ADDING TO PACKAGE ~S~%
            ~&#      THE NICKNAME ~S~%" package nickname)
  (let* ((pack     (find-package package))
         (package  (if pack
                       (package-name pack)
                       (error "~S: There is no package named \"~A\"."
                              'add-nickname package)))
         (nickpack (find-package nickname))
         (cnt      0))
    (flet ((temp-name ()
             (loop
                :for name = (format nil "TEMP-~A-~A" package (incf cnt))
                :while (find-package name)
                :finally (return name))))
      (cond
        ((eq nickpack pack)  (verbose "~&#    ALREADY GOT IT~%"))
        ((null nickpack)
         (let ((temp  (temp-name))
               (nicks (cons nickname (copy-seq (package-nicknames pack)))))
          (rename-package pack temp    nicks)
          (rename-package pack package nicks))
         (when (built-in-p package)
           (pushnew nickname *built-in-packages* :test (function string=))))
        ((and force (string= nickname (package-name nickpack)))
         (let ((nicks (or (package-nicknames nickpack)
                          (list (gen-old-name nickname)))))
           (rename-package nickpack (first nicks) (rest nicks))
           (add-nickname package nickname)))
        ((and (or steal force) (string/= nickname (package-name nickpack)))
         (remove-nickname nickpack nickname)
         (add-nickname package nickname))
        (force
         (error "~S is already a nickname of the package ~S" nickname nickpack))
        (t
         (error "~S is the name of an existing package." nickname)))
      pack)))


(DEFUN INSERT-SHARP (STRING)
  (declare (string string))
  (DO ((RESULT '())
       (START  0)
       (END    0))
      ((>= END (LENGTH STRING))
       (PROGN
         (WHEN (<= START END) (PUSH (SUBSEQ STRING START END) RESULT))
         (APPLY (FUNCTION CONCATENATE) 'STRING (IF (CDR RESULT) "# " "")
                (NREVERSE RESULT))))
    (when (prog1 (CHAR= #\NEWLINE (CHAR STRING END)) (incf end))
      (PUSH (SUBSEQ STRING START END) RESULT)
      (SETQ START END)
      (PUSH "# " RESULT))))


(DEFMACRO DEFINE-PACKAGE (NAME &REST DECLARATIONS)
  "
DO:         Declares a package.
            This includes loading the packages depended on,
            adding nicknames to the packages used under these nicknames,
            defining the package, and going into it (with IN-PACKAGE).
"
  (SETQ NAME (STRING NAME))
  (MULTIPLE-VALUE-BIND (DEPENDENCIES RENAMES DEFPACK-ARGS)
      (PARSE-PACKAGE-DECLARATIONS DECLARATIONS)
    (SETQ NAME (STRING NAME))
    (VERBOSE  "~3%# DECLARING PACKAGE ~S~%~
                ~&# -----DEFPACK-ARGS ~A~%~
                ~&# -----RENAMES      ~A~%~
                ~&# -----DEPENDENCIES ~A~%"
              NAME
              (INSERT-SHARP (FORMAT NIL "~S" DEFPACK-ARGS))
              (INSERT-SHARP (FORMAT NIL "~S" RENAMES))
              (INSERT-SHARP (FORMAT NIL "~S" DEPENDENCIES)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (REGISTER ,NAME)
       ,@(unless (null dependencies)
                `((DOLIST (PACK   ',DEPENDENCIES) 
                    (UNLESS (BUILT-IN-P PACK) (LOAD-PACKAGE PACK)))))
       ,@(unless (null renames)
                `((DOLIST (RENAME ',RENAMES)
                    (ADD-NICKNAME (CAR RENAME) (CDR RENAME)))))
       ;; If we try to delete it while it's used,
       ;; we get interned uninterned symbols...
       ;; (ignore-errors (delete-package ,name))
       (DEFPACKAGE ,NAME ,@DEFPACK-ARGS)
       (IN-PACKAGE ,NAME))))




;;;; package.lisp                     --                     --          ;;;;

