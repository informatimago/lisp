;;;; Image Based Common Lisp
;;;;**************************************************************************
;;;;FILE:               ibcl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The package IBCL exports the same symbols as COMMON-LISP, but for 
;;;;    some of the functions of macros modified to track of the source
;;;;    of the definitions and to be able to edit them from the image,
;;;;    and to save them in files.
;;;;
;;;;    The package IBCL-USER is a virgin package using IBCL instead of CL.
;;;;
;;;;    One can work at the REPL, define variables with
;;;;    DEFCONSTANT, DEFVAR, DEFPARAMETER, macros with DEFMACRO,
;;;;    and functions with DEFUN, edit macro and function definitions 
;;;;    with ED, and save the image with SAVE-IMAGE.
;;;;
;;;;    The function LIST-PACKAGES-WITH-SOURCES returns a list of packages
;;;;    where some of these variables or functions are defined.
;;;;    The function GET-SOURCE returns the source form of the given 
;;;;    variable or function.
;;;;    The function SAVE-SOURCES saves the definitions in a package,
;;;;    or all the definitions to a file or stream.
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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
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
;;;;**************************************************************************

(cl:defpackage "IMAGE-BASED-COMMON-LISP"
  (:nicknames "IBCL")
  (:use "COMMON-LISP")
  ;; We some symbols from the package #+clisp "EXT" too.
  (:shadow "DEFPACKAGE"
           "DEFCONSTANT" "DEFVAR" "DEFPARAMETER"
           "DEFSTRUCT" "DEFCLASS" 
           "DEFUN" "DEFMACRO" "LAMBDA" "DEFMETHOD"
           "ED"  "DELETE-PACKAGE"
           #| TODO: Add define-symbol-macro, defclass, define-condition, etc...
           make-package, etc...
           |#)
  #| See exports at the end. |#)
(in-package "IMAGE-BASED-COMMON-LISP")


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

#||
(cl:defun definitions (package-designator)
  (let ((record (gethash (find-package package-designator) *map*)))
    (if record
        (car record)
        (let ((record (cons (make-hash-table :test (function equal)) '())))
          (setf (gethash (find-package package-designator) *map*) record)
          (car record)))))

(cl:defun order (package-designator)
  (let ((record (gethash (find-package package-designator) *map*)))
    (if record
        (cdr record)
        (let ((record (cons (make-hash-table :test (function equal)) '())))
          (setf (gethash (find-package package-designator) *map*) record)
          (cdr record)))))

(cl:defun (setf order) (value package-designator)
  (let ((record (gethash (find-package package-designator) *map*)))
    (if record
        (setf (cdr record) value)
        (let ((record (cons (make-hash-table :test (function equal)) '())))
          (setf (gethash (find-package package-designator) *map*) record)
          (setf (cdr record) value)))))
||#

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


(cl:defmacro defconstant (name value 
                               &optional (documentation nil documentation-p))
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons 'variable ',name))
           (,def (definitions ',(symbol-package name))))
       (setf (gethash ,key ,def)
             (list 'defconstant ',name ',value
                   ,@(when documentation-p `(',documentation))))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       (cl:defconstant ,name ,value
         ,@(when documentation-p `(,documentation))))))


(cl:defmacro defvar (name &optional (value nil value-p) 
                          (documentation nil documentation-p))
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons 'variable ',name))
           (,def (definitions ,(symbol-package name))))
       (setf (gethash ,key ,def)
             (list 'defvar ',name
                   ,@ (when value-p 
                        `(',value ,@(when documentation-p `(',documentation))))))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       (cl:defvar ,name
         ,@ (when value-p 
              `(,value ,@(when documentation-p `(,documentation))))))))


(cl:defmacro defparameter (name value 
                                &optional (documentation nil documentation-p))
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons 'variable ',name))
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
    `(let ((,key (cons 'type ',name))
           (,def (definitions ,(symbol-package name))))
       (cl:defstruct ,name-and-options ,@fields)
       (setf (gethash ,key ,def) '(defstruct ,name-and-options ,@fields))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       ',name)))


(cl:defmacro defclass (name superclasses attributes &rest options)
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons 'type ',name))
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
    `(let ((,key (cons 'function ',name))
           (,def (definitions ,(symbol-package name))))
       (cl:defmacro ,name ,args ,@body)
       (eval-when (:execute)
         (compile ',name))
       (unless (compiled-function-p (macro-function ',name))
         )
       (setf (gethash ,key ,def) '(defmacro ,name ,args ,@body)
             (gethash (cons 'function (fdefinition ',name)) ,def)  
             (gethash ,key ,def))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       ',name)))


(cl:defmacro defun (name args &body body)
  (let ((key (gensym))
        (def (gensym)))
    `(let ((,key (cons 'function ',name))
           (,def (definitions ,(symbol-package name))))
       (cl:defun ,name ,args ,@body)
       (eval-when (:execute)
         (compile ',name))
       (unless (compiled-function-p (function ,name))
         ) 
       (setf (gethash ,key ,def) '(defun ,name ,args ,@body)
             (gethash (cons 'function (fdefinition ',name)) ,def)  
             (gethash ,key ,def))
       (pushnew ,key (order ,(symbol-package name)) :test (function equal))
       ',name)))


(cl:defmacro defmethod (name &body stuff-and-body)
  (let ((key (gensym))
        (def (gensym)))
    ;; TODO: we should implement the overriding of methods!
    `(let ((,key (cons 'method ',name))
           (,def (definitions ,(symbol-package name))))
       (cl:defmethod ,name ,@stuff-and-body)
       (eval-when (:execute)
         (compile ',name))
       (unless (compiled-function-p (function ,name))
         ) 
       (setf (gethash ,key ,def) '(defmethod ,name ,@stuff-and-body)
             (gethash (cons 'method (fdefinition ',name) #|add arg types here?|#) ,def)  
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
    `(let ((,key (cons 'function ',fun))
           (,def (definitions *package*))
           (,fun (compile nil (cl:lambda ,args ,@body)))
           (,src '(lambda ,args ,@body)))
       (setf (gethash ,key ,def)                  ,src
             (gethash (cons 'function ,fun) ,def) ,src)
       ,fun)))


(defmacro defpackage (name &rest options)
  `(cl:defpackage ,name
     ,@(mapcar
        (lambda (option)
          (if (listp option)
              (case (first option)
                ((:use) 
                 (substitute "IBCL" "COMMON-LISP"
                             (substitute "IBCL" "CL" option)))
                ((:shadowing-import-from :import-from)
                 (if (member (string (second option))
                             '("CL" "COMMON-LISP")
                             :test (function string=))
                     (list* (first option)
                            "IBCL"
                            (cddr option))
                     option))
                (otherwise option))))
        options)))

(cl:defun list-packages-with-sources ()
  (let ((result '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k result)) *map*)
    result))

(cl:defun get-source (name &optional kind)
  ;; TODO: with symbol-package we cannot find fdefinitions...
  (if (null kind)
      (loop
         :for kind :in '(type variable function) 
         :collect (get-source name kind))
      (gethash (cons kind name) (definitions (symbol-package name)))))

(cl:defun save-sources (path-or-stream &key package)
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
                             :direction :output :if-exists :Supersede
                             :if-does-not-exist :create)
          (save-packages out package))))
  (values))

#+sbcl (require :sb-posix)
(cl:defun save-image (&rest args)
  #+clisp
  (labels ((key-present-p (key plist)
             (and (not (null plist))
                  (or (eq key (car plist)) (key-present-p key (cddr plist))))))
    (let* ((keys (rest args)))
      (unless (key-present-p :start-package keys)
        (setf (getf keys :start-package) (find-package "IBCL-USER")))
      (unless (key-present-p :norc keys)
        (setf (getf keys :norc) t))
      (apply (function ext:saveinitmem) 
             (first args)
             keys)))
  #+sbcl 
  (when (zerop (SB-POSIX:FORK))
      (apply (function sb-ext:SAVE-LISP-AND-DIE) args))
  #-(or clisp sbcl) (error "I don't know how to save an image in ~A" 
                           (lisp-implementation-type))
  (values))


(cl:defun make-temporary-pathname ()
  "Generate a rather unlikely filename."
  (loop
     :for path = (make-pathname :name (format nil "~36R" (get-universal-time))
                                :type "LISP"
                                :case :COMMON
                                :defaults (user-homedir-pathname))
     :while (probe-file path)
     :finally (return path)))


(cl:defmacro handling-errors (&body body)
  `(HANDLER-CASE (progn ,@body)
     (simple-condition 
         (ERR) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (condition 
         (ERR) 
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



;; We must pass the symbol in a list to export CL:NIL.
(export (mapcar (lambda (name) (intern name "IBCL"))
                (append '("SAVE-IMAGE" "SAVE-SOURCES"
                          "GET-SOURCE" "LIST-PACKAGES-WITH-SOURCES")
                        (let ((symbols '()))
                          (do-external-symbols (sym "COMMON-LISP")
                            (push (string sym) symbols))
                          symbols))))



(let ((*error-output* (make-broadcast-stream)))
  (defpackage "IMAGE-BASED-COMMON-LISP-USER"
    (:nicknames "IBCL-USER")
    (:use "IMAGE-BASED-COMMON-LISP")))

(in-package "IBCL-USER")









