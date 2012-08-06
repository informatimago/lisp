;;;; Image Based Common Lisp
;;;;**************************************************************************
;;;;FILE:               ibcl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    See :documentation of package below.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-07-01 <PJB> Added deftype, defclass.
;;;;    2006-05-04 <PJB> Added this header. Augmented.
;;;;BUGS
;;;;    Missing some def* macros, like define-symbol-macro,
;;;;    defconditions, defmethod, defgeneric, etc.
;;;;    Missing some functions, like make-package, rename-package, etc.
;;;;    See also MOP functions.
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2012
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(cl:defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.IMAGE-BASED-COMMON-LISP"
  (:nicknames "IMAGE-BASED-COMMON-LISP" "IBCL")
  (:use "COMMON-LISP")
  (:shadow "DEFPACKAGE"
           "DEFCONSTANT" "DEFVAR" "DEFPARAMETER"
           "DEFSTRUCT" "DEFCLASS" 
           "DEFUN" "DEFMACRO" "LAMBDA" "DEFMETHOD"
           "ED"  "DELETE-PACKAGE"
           #| TODO: Add define-symbol-macro, defclass, define-condition, etc...
           make-package, unintern, etc...
           |#)
  (:export "SAVE-SOURCES" "GET-SOURCE" "LIST-PACKAGES-WITH-SOURCES"
           . #.(let ((symbols '()))
                 (do-external-symbols (sym "COMMON-LISP")
                   (push (string sym) symbols))
                 symbols))
  (:documentation "

The package IBCL exports the same symbols as COMMON-LISP, but for
some of the functions of macros modified to track of the source of the
definitions and to be able to edit them from the image, and to save
them in files.

The package IBCL-USER is a virgin package using IBCL instead of CL.

One can work at the REPL, define variables with DEFCONSTANT, DEFVAR,
DEFPARAMETER, macros with DEFMACRO, and functions with DEFUN, edit
macro and function definitions with ED.

The function LIST-PACKAGES-WITH-SOURCES returns a list of packages
where some of these variables or functions are defined.

The function GET-SOURCE returns the source form of the given  variable
or function.

The function SAVE-SOURCES saves the definitions in a package, or all
the definitions to a file or stream.


The IBCL package provides some shadowed CL functions to substittute
sneakily IBCL for CL.  When defpackage is ibcl:defpackage, as long as
CL package operators are not qualified, (cl:defpackage …)
vs. (defpackage …), the IBCL package will be used instead of the CL
package.  This should allow loading libraries using IBCL instead of
CL.


Copyright Pascal J. Bourguignon 2006 - 2012
This package is provided under the Afero General Public License 3.
See the source file for details.

"))


(cl:defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.IMAGE-BASED-COMMON-LISP-USER"
  (:nicknames "IMAGE-BASED-COMMON-LISP-USER" "IBCL-USER")
  (:use "COM.INFORMATIMAGO.COMMON-LISP.LISP.IMAGE-BASED-COMMON-LISP")
  (:documentation "

The package IBCL-USER is a virgin package using IBCL instead of CL.

Copyright Pascal J. Bourguignon 2006 - 2012
This package is provided under the Afero General Public License 3.
See the source file for details.

"))


(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.IMAGE-BASED-COMMON-LISP")


(defparameter *source-types*
  '(:variable :function :method :type ; symbol
    :package                          ; string
    ))

(deftype source-type ()
  `(member ,@*source-types*))

(defun source-type-designator (object)
  (flet ((err ()
           (error 'simple-type-error
                  :datum object
                  :expected-type 'source-type
                  :format-control "~S is not a source-type designator (~{~S~^ ~})."
                  :format-arguments (list object *source-types*))))
    (typecase object
      (source-type
       object)
      ((or character string)
       (source-type-designator (intern (string-upcase object) "KEYWORD")))
      (symbol
       (if (member object *source-types* :test (function string=))
         (source-type-designator (intern (string-upcase object) "KEYWORD"))
         (err)))
      (t
       (err)))))


;;; Sources for symbols are stored in a symbol property.  The property
;;; key is IBCL:SOURCE.  The property value is an a-list mapping
;;; source-type to the source form.


;;; Sources for other objects are stored in the *SOURCES* hash-table.
;;; Keys of
(defparameter *sources* (make-hash-table :test (function equal)))

(defun source (object source-type)
  "
SOURCE-TYPE:    
RETURN:         The source form corresponding to the OBJECT (usually a
                symbol) interpreted as KIND.

"
  )


(defun get-source (name &optional kind)
  "
KIND:           

NAME:           A symbol (for :variable :function :method :type), or
                a function (for :function :method), or
                a string (for :package),
                naming the object of which the source is returned.

RETURN:         The last source form registered for the specified
                object.
"
  ;; TODO: with symbol-package we cannot find fdefinitions...
  (ecase kind
    ((nil)
     (loop
       :for kind :in '(:package :type :variable :function :method) 
       :collect (get-source name kind)))
    ((:package))
    ((:type :variable :function :method)
     (gethash (cons kind name) (definitions (symbol-package name))))))


(defun save-sources (path-or-stream &key (package *package*))
  "
PATH-OR-STREAM: A pathname designator, or a stream, where the sources will be saved.

PACKAGE:        A package designator indicating the package whose
                sources will be saved.  The default is *PACKAGE*.
                NIL may be passed to specify to save all packages.
"
  (labels ((save-one-package (out package)
             (let ((*print-readably* nil)
                   (*package* (find-package package)))
               (loop
                  :with def = (definitions package)
                  :with processed = (make-hash-table :test (function equal))
                  :for item :in (reverse (order package))
                  :initially (pprint `(in-package ,(package-name package)) out)
                  :unless (gethash item processed)
                  :do (progn 
                        (setf (gethash item processed) t)
                        (pprint (gethash item def) out)))))
           (save-packages (out package)
             (if package
                 (save-one-package out package)
                 (dolist (package (list-packages-with-sources))
                   (save-one-package out package)))))
    (if (streamp path-or-stream)
        (save-packages path-or-stream package)
        (with-open-file (out path-or-stream
                             :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (save-packages out package))))
  (values))




(cl:defparameter *map* (make-hash-table) 
  "Maps packages to (cons definitions order)")

(cl:defun delete-package (package-designator)
  (remhash (find-package package-designator) *map*)
  (cl:delete-package package-designator))

(cl:defmacro define-package-attribute
    (name (package-designator record &optional (value nil value-p)) &body body)
  (let ((pack (gensym)))
    `(cl:defun ,name (,@(when value-p `(,value)) ,package-designator)
       (let* ((,pack   (find-package ,package-designator))
              (,record (gethash ,pack *map*)))
         (if ,record
             (progn ,@body)
             (let ((,record (cons (make-hash-table :test (function equal)) '())))
               (setf (gethash ,pack *map*) ,record)
               ,@body))))))


(define-package-attribute definitions  (package-designator record) (car record))
(define-package-attribute order        (package-designator record) (cdr record))
(define-package-attribute (setf order) (package-designator record value)
  (setf (cdr record) value))


(cl:defmacro push-on-top (value place &key (test (function eql)) 
                                &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((vvalue (gensym)))
      `(let* ((,vvalue ,value)
              ,@(mapcar (function list) vars vals)
              (,(car store-vars)  (cons ,vvalue (delete ,vvalue ,reader-form
                                                        :test ,test))))
         ,writer-form))))


;;          makunbound                                 function
;;          fmakunbound                                function
;;          delete-package                             function
;;          ...
;;
;; done     DEFCLASS                                   macro
;; done     DEFCONSTANT                                macro
;;          DEFGENERIC                                 macro
;;          DEFINE-COMPILER-MACRO                      macro
;;          DEFINE-CONDITION                           macro
;;          DEFINE-METHOD-COMBINATION                  macro
;;          DEFINE-MODIFY-MACRO                        macro
;;          DEFINE-SETF-EXPANDER                       macro
;;          DEFINE-SYMBOL-MACRO                        macro
;; done     DEFMACRO                                   macro
;;          DEFMETHOD                                  macro
;; done     DEFPACKAGE                                 macro
;; done     DEFPARAMETER                               macro
;;          DEFSETF                                    macro
;; done     DEFSTRUCT                                  macro
;; done     DEFTYPE                                    macro
;; done     DEFUN                                      macro
;; done     DEFVAR                                     macro


(cl:defmacro defconstant (name value &optional (documentation nil documentation-p))
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons :variable ',name))
           (,def (definitions ',(symbol-package name))))
       
       (setf (gethash ,key ,def)
             (list 'defconstant ',name ',value
                   ,@(when documentation-p `(',documentation))))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       (cl:defconstant ,name ,value
         ,@(when documentation-p `(,documentation))))))


(cl:defmacro defvar (name &optional (value nil value-p) (documentation nil documentation-p))
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons :variable ',name))
           (,def (definitions ,(symbol-package name))))
       (setf (gethash ,key ,def)
             (list 'defvar ',name
                   ,@ (when value-p 
                        `(',value ,@(when documentation-p `(',documentation))))))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       (cl:defvar ,name
         ,@ (when value-p 
              `(,value ,@(when documentation-p `(,documentation))))))))


(cl:defmacro defparameter (name value &optional (documentation nil documentation-p))
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons :variable ',name))
           (,def (definitions ,(symbol-package name))))
       (setf (gethash ,key ,def)
             (list 'defparameter ',name ',value
                   ,@(when documentation-p `(',documentation))))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       (cl:defparameter ,name ,value
         ,@(when documentation-p `(,documentation))))))



(cl:defmacro defstruct (name-and-options &rest fields)
  (let ((key (gensym))
        (def (gensym))
        (name (if (consp name-and-options) 
                  (first name-and-options)
                  name-and-options)))
    `(let ((,key (cons :type ',name))
           (,def (definitions ,(symbol-package name))))
       (cl:defstruct ,name-and-options ,@fields)
       (setf (gethash ,key ,def) '(defstruct ,name-and-options ,@fields))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       ',name)))


(cl:defmacro defclass (name superclasses attributes &rest options)
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons :type ',name))
           (,def (definitions ,(symbol-package name))))
       (cl:defclass ,name ,superclasses ,attributes ,@options)
       (setf (gethash ,key ,def) 
             '(defclass ,name ,superclasses ,attributes ,@options))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       ',name)))


;; Note: we compile the functions immediately, which may not be the
;;       normal behavior when an interpreter is available, to 

(cl:defmacro defmacro (name args &body body)
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons :function ',name))
           (,def (definitions ,(symbol-package name))))
       (cl:defmacro ,name ,args ,@body)
       (eval-when (:execute)
         (compile ',name))
       (unless (compiled-function-p (macro-function ',name))
         )
       (setf (gethash ,key ,def) '(defmacro ,name ,args ,@body)
             (gethash (cons :function (fdefinition ',name)) ,def)  
             (gethash ,key ,def))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       ',name)))


(cl:defmacro defun (name args &body body)
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons :function ',name))
           (,def (definitions ,(symbol-package name))))
       (cl:defun ,name ,args ,@body)
       (eval-when (:execute)
         (compile ',name))
       (unless (compiled-function-p (function ,name))
         ) 
       (setf (gethash ,key ,def) '(defun ,name ,args ,@body)
             (gethash (cons :function (fdefinition ',name)) ,def)  
             (gethash ,key ,def))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       ',name)))


(cl:defmacro defmethod (name &body stuff-and-body)
  (let ((key (gensym))
        (def (gensym)))
    ;; TODO: we should implement the overriding of methods!
    `(let ((,key (cons :method ',name))
           (,def (definitions ,(symbol-package name))))
       (cl:defmethod ,name ,@stuff-and-body)
       (eval-when (:execute)
         (compile ',name))
       (unless (compiled-function-p (function ,name))
         ) 
       (setf (gethash ,key ,def) '(defmethod ,name ,@stuff-and-body)
             (gethash (cons :method (fdefinition ',name) #|add arg types here?|#) ,def)  
             (gethash ,key ,def))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       ',name)))



;; (cl:defmacro lambda (args &body body)
;;   `(cl:function (cl:lambda ,args ,@body)))

(cl:defmacro lambda (args &body body)
  (let ((key (gensym))
        (def (gensym))
        (fun (gensym))
        (src (gensym)))
    `(let ((,key (cons :function ',fun))
           (,def (definitions *package*))
           (,fun (compile nil (cl:lambda ,args ,@body)))
           (,src '(lambda ,args ,@body)))
       (setf (gethash ,key ,def)                  ,src
             (gethash (cons :function ,fun) ,def) ,src)
       ,fun)))



(defun normalize-package-designator (package)
  (package-name (find-package package)))

(defun substitute-package-designator (new old sequence)
  (substitute (normalize-package-designator new)
              (normalize-package-designator old)
              sequence
              :test (function equal)
              :key (function normalize-package-designator)))

(defun substitute-packages (new-old-a-list sequence)
  (loop
    :for (new . old) :in new-old-a-list
    :do (setf sequence (substitute-package-designator new old sequence))
    :finally (return sequence)))

(defvar *package-map* '(("IBCL-USER" . "COMMON-LISP-USER")
                        ("IBCL"      . "COMMON-LISP"))
  "An A-list mapping new packages for old packages.")

(defmacro defpackage (name &rest options)
  "
DO:             Same as CL:DEFPACKAGE, but substitute the packages
                specified by *PACKAGE-MAP*, and record the defpackage
                form in the sources.

RETURN:         The package name.
"
  (let ((key (gensym))
        (def (gensym)))
    `(progn
       (let ((,key (cons :package ',(intern (string name) "KEYWORD")))
             (,def (definitions ,(symbol-package name))))
         (setf (gethash ,key ,def) `(defpackage ,name ,@options)))
       (cl:defpackage ,name
         ,@(mapcar
            (lambda (option)
                (if (consp option)
                  (case (first option)
                    ((:use)
                     (substitute-packages *package-map* option))
                    ((:shadowing-import-from :import-from)
                     (list* (first option)
                            (first (substitute-packages *package-map* (list (second option))))
                            (cddr  option)))
                    (otherwise
                     option))
                  option))
            options)))))


(defun list-packages-with-sources ()
  "
RETURN:         A list of packages that have some sources saved.
"
  (let ((result '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k result)) *map*)
    result))






(cl:defun make-temporary-pathname ()
  "Generate a rather unlikely filename."
  (loop
     :for path = (make-pathname :name (format nil "~36R" (get-universal-time))
                                :type "LISP"
                                :case :common
                                :defaults (user-homedir-pathname))
     :while (probe-file path)
     :finally (return path)))


(cl:defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition 
         (err) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (condition 
         (err) 
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err))))


(cl:defun ed (&optional x)
  (typecase x
    (null                 (cl:ed))      ; edit whatever.
    ((or pathname string) (cl:ed x))    ; edit an external file.
    (otherwise 
     (let ((def (get-source x 'function)))
       (if def
           (let ((path (make-temporary-pathname))
                 ;; TODO: with symbol-package we cannot find fdefinitions...
                 (*package* (symbol-package x)))
             (unwind-protect
                  (progn
                    (with-open-file (out path
                                         :direction :output
                                         :if-does-not-exist :create
                                         :if-exists :error)
                      (pprint def out))
                    (cl:ed path)
                    (handling-errors
                     (with-open-file (in path)
                       (loop
                          :for form = (read in nil in)
                          :until (eq form in)
                          :do
                          (when *load-verbose* (print form *trace-output*))
                          (print (eval form))))))
               (delete-file path)))
           (cl:ed x))))))          ; try to edit the function anyways.


(cl:defun repl ()
  (do ((+eof+ (gensym))
       (hist 1 (1+ hist)))
      (nil)
    (format t "~%~A[~D]> " (package-name *package*) hist)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit)(exit)(continue)) :test (function equal)))
       (return-from repl))
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (setf *** **   ** *   * (first /))
     (format t "~& --> ~{~S~^ ;~%     ~}~%" /))))

;;;; THE END ;;;;










