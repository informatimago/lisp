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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2012
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"
  (:documentation
   "

Some package utilities.



License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2003 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>


")
  (:use "COMMON-LISP")
  (:export "PACKAGE-EXPORTS" ;; missing from CL or not?
           "*PACKAGES*" "PACKAGE-PATHNAME" "LOAD-PACKAGE"
           "PACKAGE-SYSTEM-DEFINITION"
           "ADD-TRANSLATIONS" "ADD-NICKNAME" "*PACKAGE-VERBOSE*"
           ;; utility:
           "LIST-SYMBOLS" "LIST-ALL-SYMBOLS" "LIST-EXTERNAL-SYMBOLS"
           "COPY-PACKAGE"
           ;; debugging help:
           "CRACK-OPEN-PACKAGE"
           ;; Obsolete: define-package
           ;; "DEFINE-PACKAGE"
           ))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE")


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
        (when exported
          (do-external-symbols (s pack)
            (push s sl)))
        (do-symbols (s pack) 
          (when (or all
                    (and homely    (eq pack (symbol-package s)))
                    (and shadowing (member s (package-shadowing-symbols pack))))
            (pushnew s sl)))
        (if sorted
          (sort sl (function string<))
          sl))
      (error "No package ~S" package))))


(defun list-all-symbols (package &key (sorted t))
  "
RETURN:     A list of all the symbols present in the PACKAGE.
PACKAGE:    A package designator.
SORTED:     Whether the result list is sorted (default T).
"
  (list-symbols package :sorted sorted :all t))


(defun list-external-symbols (package &key (sorted t))
  "
RETURN:     A list of all the symbols exported from the PACKAGE.
PACKAGE:    A package designator.
SORTED:     Whether the result list is sorted (default T).
"
  (list-symbols package :sorted sorted :exported t))


(defun copy-package (old-package new-name)
  "
RETURN:         A new package that exports all the external symbols of the OLD-PACKAGE.
OLD-PACKAGE:    A package designator.
NEW-NAME:       A package name (string designator)
"
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


(defvar *package-verbose* nil

  "Whether some package operation defined in this package shall issue
some verbosity.

SEE ALSO:  REGISTER, LOAD-PACKAGE, ADD-NICKNAME.
")


(defparameter *vout* t "Verbose output stream.")
(defmacro verbose (fctrl &rest args)
  `(when *package-verbose* (format *vout* ,fctrl ,@args)))

(defmacro while (condition &body body)  `(do () ((not ,condition))  ,@body))


(defun package-exports (package)
  "
RETURN:   A new list of exported symbols from PACKAGE.
"
  (let ((result nil))
    (do-external-symbols (sym package result)
      (push sym result))))
  


(defun package-pathname (package)
  "
RETURN:  The logical pathname to the given package.
NOTE:    If a nickname is given, then a nickname pathname is returned.
"
  (cond
   ((simple-string-p package)
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
     (concatenate 'string
                  "PACKAGES:"
                  (substitute (character ";") (character ".") package)
                  ".LISP")))
    ((stringp package)  (package-pathname (copy-seq package)))
    ((symbolp package)  (package-pathname (string package)))
    ((packagep package) (package-pathname (package-name package)))
    (t (error "~S is not a package designator." package))))


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
                   :defaults (package-pathname
                              (concatenate 'string name ".system")))))
        (values (when (probe-file file) file) file)))))


(defvar *built-in-packages*
  (mapcan (lambda (pack)
            (cons (package-name pack)
                  (copy-list (package-nicknames pack))))
          (list-all-packages)))


(defun built-in-p (package)
  (member package *built-in-packages* :test (function string=)))


(defvar *packages* nil
  "
We cannot use COMMON-LISP:*MODULES* since it's deprecated, so here is our own.
This is an a-list (file-pathname . (package-name)).
Note: the main data item is the file-pathname; the package-name is
kept as a human readable item; caveat: packages can be renamed!
")


(defun close-path (package)
  "
RETURN:     Closure of the translation of the package pathname of PACKAGE.
"
  (do* ((path (package-pathname package) next)
        (next (translate-logical-pathname path)
              (translate-logical-pathname path))
        (count 0 (1+ count)))
       ((string= (namestring path) (namestring next))   
        path)
    (when (< 100 count)
      (error "Cannot close the logical path for package ~S in less ~
              than 100 steps: probably a loop!~%" package))))


(declaim (inline registeredp))
(defun registeredp (package)
  "
RETURN:     Whether the PACKAGE is already registered.
"
  (let ((path (namestring (close-path package))))
    (member path *packages* :key (function car) :test (function string=))))


(defun register (package)
  "
DO:         Force registering the PACKAGE into the loaded *PACKAGES*.
"
  (let ((path (namestring (close-path package))))
    (verbose "~&# PACKAGE:REGISTER PACKAGE = ~S~%~
              ~&#                  PATH    = ~S~%~:[~
              ~&#                  IS NEW!~%~;~]"
             package path (registeredp package))
    (pushnew (list path (if (stringp package)
                            package
                            (package-name package))) *packages*
             :key (function car) :test (function string=))))


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


(defun load-package (package-name
                     &key (verbose *load-verbose*) (print *load-print*)
                     (if-does-not-exist :error)
                     (external-format :default))
  "
DO:         Unless it's already loaded (listed in *PACKAGES* or found
            by FIND-PACKAGE),  loads the package named PACKAGE-NAME.
NOTE:       We both use REGISTER in DEFINE-PACKAGE  and  in PACKAGE::LOAD
            because we may be loading a nickname.
RETURN:     The package named PACKAGE-NAME if found, or NIL.
"
  (or (find-package package-name)
      (let ((path (close-path (string package-name))))
        (verbose "~&# LOADING PACKAGE NAME ~S FROM ~S~%~:[~
                  ~&#   NEW PACKAGE.~%
                  ~&#   PACKAGE ALREADY KNOWN.~%~;~]" 
                 package-name path (registeredp package-name))
        (unless (registeredp package-name)
          (prog1
              (or
               (common-lisp:load (object-dir path)
                                 :verbose verbose
                                 :print print
                                 :if-does-not-exist nil
                                 :external-format external-format)
               (common-lisp:load path
                                 :verbose verbose
                                 :print print
                                 :if-does-not-exist if-does-not-exist
                                 :external-format external-format))
            (register package-name)
            (verbose "~&# LOAD-PACKAGE ~S DONE~%~
                      ~&# *PACKAGES*= ~S~%" package-name  *packages*))))))
  

(defun add-translations (&rest translations)
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
  (setf (logical-pathname-translations "PACKAGES")
        (nconc (mapcar (lambda (item)
                         (list
                          ;; TODO: This is most certainly not portable; check it:
                          (merge-pathnames (first item)
                                           (make-pathname :host "PACKAGES")
                                           nil)
                          (second item)))
                       translations)
               (handler-case  (logical-pathname-translations "PACKAGES")
                 (error nil)))))


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


(defun stringify (items)
  "
ITEMS:      A list of symbol, keyword or string.
RETURN:     A new list of strings, the symbol-names
            or the strings given in ITEMS.
"
  (do* ((items items (cdr items))
        (item (car items) (car items))
        (result nil))
      ((null items) result)
    (cond
     ((stringp item) (push item result))
     ((symbolp item) (push (symbol-name item) result))
     (t (error "~S is not a symbol, keyword or string!~%" item)))))


(defun parse-package-declarations (declarations)
  "
DO:         Parses and check somewhat the syntax of DECLARATIONS.
RETURN:     a list of dependencies (package names);
            a list of renames (package . nickname);
            arguments for DEFPACKAGE.
"
  (do* ((declarations declarations (cdr declarations))
        (decl (car declarations) (car declarations))
        (dependencies nil)
        (renames      nil)
        (nicknames    nil)
        (uses         nil)
        (result       nil))
      ((null declarations)
       (progn
         (when uses      (push (cons :use uses) result))
         (when nicknames (push (cons :nicknames nicknames) result))
         (values dependencies  renames  result)))
    (case (car decl)
      (:nicknames
       (setq nicknames (nconc (stringify (cdr decl)) nicknames)))
      (:documentation
       (push decl result))
      (:shadow
       (push (cons :shadow (stringify (cdr decl))) result))
      (:size
       (push decl result))
      (:from
       (let* ((from-pkg-name (string (nth 1 decl))))
         (push from-pkg-name dependencies)
         (unless (eq :import (nth 2 decl))
           (error "Missing :IMPORT after :FROM ~S.~%" from-pkg-name))
         (if (eq :all (nth 3 decl))
           (if (< 4 (length decl))
             (error "Unexpected ~S after :ALL." (nth 4 decl))
             (push from-pkg-name uses))
           (push (cons :import-from (cons from-pkg-name
                                          (stringify (cdddr decl)))) result))
         ))
      (:use
       (let* ((from-pkg-name (string (nth 1 decl))))
         (push from-pkg-name dependencies)
         (when (< 2 (length decl))
           (unless (eq :as (nth 2 decl))
             (error "Expected :AS in :USE clause, not ~S." (nth 2 decl)))
           (unless (= 4 (length decl))
             (error "Unexpected ~S after :AS ~A." (nth 4 decl) (nth 3 decl)))
           (push (cons from-pkg-name (string (nth 3 decl))) renames))
         ))
      (:export
       (push (cons :export (stringify (cdr decl))) result))
      (otherwise
       (error "Unexpected clause ~S." decl)))))


(defun remove-nickname (package nickname)
  "
DO:      Remove the NICKNAME from the list of nicknames
         of the package designated by PACKAGE.
RETURN:  The package designated by PACKAGE.
"
  (let ((package (find-package package)))
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
  (verbose "~&# ADDING TO PACKAGE ~S~%
            ~&#      THE NICKNAME ~S~%" package nickname)
  (let* ((pack     (find-package package))
         (packname (if pack
                     (package-name pack)
                     (error "~S: There is no package named \"~A\"."
                            'add-nickname package)))
         (nickpack (find-package nickname))
         (cnt      0))
    (flet ((temp-name ()
             (loop
                :for name = (format nil "TEMP-~A-~A" packname (incf cnt))
                :while (find-package name)
                :finally (return name))))
      (cond
        ((eq nickpack pack)  (verbose "~&#    ALREADY GOT IT~%"))
        ((null nickpack)
         ;; The consequences are undefined if new-name or any
         ;; new-nickname conflicts with any existing package names.
         ;; Therefore we use a temp name.
         (let ((temp  (temp-name))
               (nicks (cons nickname (copy-seq (package-nicknames pack)))))
           (rename-package pack temp     nicks)
           (rename-package pack packname nicks))
         (when (built-in-p packname)
           (pushnew nickname *built-in-packages* :test (function string=))))
        ((and force (string= nickname (package-name nickpack)))
         (let ((nicks (or (package-nicknames nickpack)
                          (list (gen-old-name nickname)))))
           (rename-package nickpack (first nicks) (rest nicks))
           (add-nickname pack nickname)))
        ((and (or steal force) (string/= nickname (package-name nickpack)))
         (remove-nickname nickpack nickname)
         (add-nickname pack nickname))
        (force
         (error "~S is already a nickname of the package ~S" nickname nickpack))
        (t
         (error "~S is the name of an existing package." nickname)))
      pack)))


(defun insert-sharp (string)
  (declare (string string))
  (do ((result '())
       (start  0)
       (end    0))
      ((>= end (length string))
       (progn
         (when (<= start end) (push (subseq string start end) result))
         (apply (function concatenate) 'string (if (cdr result) "# " "")
                (nreverse result))))
    (when (prog1 (char= #\NEWLINE (char string end)) (incf end))
      (push (subseq string start end) result)
      (setq start end)
      (push "# " result))))


(defmacro define-package (name &rest declarations)
  "
DO:         Declares a package.
            This includes loading the packages depended on,
            adding nicknames to the packages used under these nicknames,
            defining the package, and going into it (with IN-PACKAGE).
"
  (setq name (string name))
  (multiple-value-bind (dependencies renames defpack-args)
      (parse-package-declarations declarations)
    (setq name (string name))
    (verbose  "~3%# DECLARING PACKAGE ~S~%~
                ~&# -----DEFPACK-ARGS ~A~%~
                ~&# -----RENAMES      ~A~%~
                ~&# -----DEPENDENCIES ~A~%"
              name
              (insert-sharp (format nil "~S" defpack-args))
              (insert-sharp (format nil "~S" renames))
              (insert-sharp (format nil "~S" dependencies)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register ,name)
       ,@(unless (null dependencies)
                `((dolist (pack   ',dependencies) 
                    (unless (built-in-p pack) (load-package pack)))))
       ,@(unless (null renames)
                `((dolist (rename ',renames)
                    (add-nickname (car rename) (cdr rename)))))
       ;; If we try to delete it while it's used,
       ;; we get interned uninterned symbols...
       ;; (ignore-errors (delete-package ,name))
       (defpackage ,name ,@defpack-args)
       (in-package ,name))))




;;;; package.lisp                     --                     --          ;;;;

