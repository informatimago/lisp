(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC.C-SEXP-LANGUAGE")

#|

The C Sexp Sources can be interpreted by two systems:

- Common Lisp usual LOAD and COMPILE-FILE, (wrapped in LOAD-LINC-FILE
  and COMPILE-LINC-FILE to setup the C reader macros and environment)
  which expands the toplevel forms (macros) into Common Lisp code
  interpring them using the C semantics embedded in Common Lisp via a
  C-like runtime.  This allows to develop and debug code in the CL
  environment, to later be translated to C.

- LINC TRANSLATE-LINC-FILE, which generates a C source file.

|#

;; pre-processor directives

(defmacro .ifdef (symbol &body toplevel-forms)
  )

(defmacro .ifndef (symbol &body toplevel-forms)
  )

(defmacro .if (constant-expression &body toplevel-forms)
  "
  .elif constant-expression
  .else
"
  )

(defmacro include (file)
  "
(include <file>)
(include \"file\")
"

  )

(defmacro define-macro     (name &rest optional-lambda-list-and-expansion)
  )



;; type expressions:

(defparameter *type-qualifiers*
  '(const restrict volatile atomic))

(defparameter *type-qualifier-map*
  '((atomic    . |_Atomic|)))

(defparameter *type-specifier*
  '(void char short int long float double signed unsigned bool complex))

(defparameter *type-specifier-map*
  '((bool    . |_Bool|)
    (complex . |_Complex|)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun struct-union-enum (type name-and-slots)
    (let ((name  (if (symbolp (first name-and-slots))
                     (first name-and-slots)
                     nil))
          (slots (if (symbolp (first name-and-slots))
                     (rest name-and-slots)
                     name-and-slots)))
      `(,type ,name ,slots))))

(defmacro struct  (&rest name-and-slots)  (struct-union-enum 'struct name-and-slots))
(defmacro union   (&rest name-and-slots)  (struct-union-enum 'union  name-and-slots))
(defmacro enum    (&rest name-and-values) (struct-union-enum 'enum   name-and-values))
(defmacro atomic  (type) `(atomic  ,type))
(defmacro pointer (type) `(pointer ,type))
(defmacro array   (type &optional size) `(array ,type ,size))

#|
(struct foo)
(struct foo (x int) (y int (bits 3)))
(struct     (x int) (y int (bits 3)))

(union foo)
(union foo (x int) (y int (bits 3)))
(union     (x int) (y int (bits 3)))

(enum bar)
(enum bar   x    y z)
(enum bar  (x 0) y z)
(enum      (x 0) y z)

<identifier>

(atomic type)

(pointer type)

;; char*
(pointer char)

;; char* []
(array (pointer char))
;; char* [42]
(array (pointer char) 42)



     (direct-declarator \[ (opt type-qualifier-list) (opt assignment-expression) \])
     (direct-declarator \[ static (opt type-qualifier-list) assignment-expression \])
     (direct-declarator \[ type-qualifier-list static assignment-expression \])
     (direct-declarator \[ (opt type-qualifier-list) \* \])


(align-as type)
(align-as const-expression)
|#

;; declarations:

(defmacro declare-structure   (name &rest slots)
  "
WILL GENERATE:

    struct ,name {
       ,@slots
    };
"
  `(struct ,name ,@slots))

(defmacro declare-union       (name &rest alternatives)
    "
WILL GENERATE:

    union ,name {
       ,@alternatives
    };
"
  `(union ,name ,@alternatives))

(defmacro declare-type        (name &rest type)
  "
WILL GENERATE:

    typedef ,type ,name;
"
  `(typedef ,name ,type)
  )

(defmacro declare-enumeration (name &rest options-and-values)
  )

(defmacro declare-constant    (name &rest options-and-type)
  )

(defmacro declare-variable    (name &rest options-and-type)
  )

(defmacro declare-function    (name &rest options-and-lambda-list-and-type)
  )

;; definitions:

(defmacro define-constant  (name &rest options-and-type-and-value)
  )
(defmacro define-variable  (name &rest options-and-type-and-value)
  )
(defmacro define-function  (name &rest options-and-lambda-list-and-type-and-body)
  )
