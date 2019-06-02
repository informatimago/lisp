(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(defvar *translate-linc-verbose*  nil)
(defvar *translate-linc-print*    nil)
(defvar *translate-linc-pathname* nil)
(defvar *translate-linc-truename* nil)
(defvar *source-form*             nil)

(defparameter *allow-print-backtrace* t)

(defun print-backtrace (&optional (output *error-output*))
  #+ccl (when *allow-print-backtrace*
          (let ((*allow-print-backtrace* nil))
            (format output "~&~80,,,'-<~>~&~{~A~%~}~80,,,'-<~>~&"
                    (ccl::backtrace-as-list)))))

(define-condition linc-error (simple-error)
  ())

(define-condition linc-program-error (linc-error)
  ((source-form :initarg :source-form :reader linc-program-error-source-form)
   (source-file :initarg :source-file :reader linc-program-error-source-file))
  (:report (lambda (condition stream)
             (let ((*print-readably* nil)
                   (*print-escape* nil)
                   (*print-case* :downcase))
               (format stream "~?~%in form: ~A~%in file: ~S~%"
                       (simple-condition-format-control condition)
                       (simple-condition-format-arguments condition)
                       (linc-program-error-source-form condition)
                       (linc-program-error-source-file condition))))))

(define-condition linc-internal-error (linc-program-error)
  ())

(define-condition linc-stray-atom-error (linc-program-error)
  ())

(define-condition linc-invalid-operator-error (linc-program-error)
  ())

(define-condition linc-not-implemented-yet-error (linc-program-error)
  ((operator :initarg :operator :reader linc-not-implemented-yet-error-operator)))

(defun not-implemented-yet (operator form)
  (error 'linc-not-implemented-yet-error
         :operator operator
         :source-form form
         :source-file *translate-linc-truename*
         :format-control "Not implemented yet: ~S"
         :format-arguments (list operator)))

;;;---------------------------------------------------------------------

(defparameter *c-keywords* '(|*| |auto| |break| |case| |char| |const|
                             |continue| |default| |do| |double| |else|
                             |enum| |extern| |float| |for| |goto| |if|
                             |inline| |int| |long| |register|
                             |restrict| |return| |short| |signed|
                             |sizeof| |static| |struct| |switch|
                             |typedef| |union| |unsigned| |void|
                             |volatile| |while|
                             ;; ----
                             |_Alignas| |_Alignof| |_Atomic| |_Bool|
                             |_Complex| |_Generic| |_Imaginary|
                             |_Noreturn| |_Static_assert|
                             |_Thread_local|
                             ;; ---- aliases:
                             |align-as| |align-of| |atomic| |bool|
                             |complex| |generic| |imaginary|
                             |noreturn| |static-assert|
                             |thread-local|))

(defun c-keyword-p (name)
  (declare (ignore name))
  (find name *c-keywords*))

(defun c-identifier-p (name)
  (and (symbolp name)
       (not (c-keyword-p name))))

(defun check-identifier (name)
  (unless (c-identifier-p name)
    (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                               :format-control "Invalid identifier: ~S"
                               :format-arguments (list name))))

;;;---------------------------------------------------------------------
;;; Pre-processor

(defun parse-include (form)
  ;; (include <foo.h>|"foo.h" …)
  (let ((files (rest form)))
    (when (null files)
      (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                 :format-control "Missing file in include form."))
    (make-instance 'c-sequence
                   :elements (mapcar (lambda (file)
                                       (typecase file
                                         (symbol
                                          (let ((name (string file)))
                                            (if (and (< 2 (length name))
                                                     (char= #\< (aref name 0))
                                                     (char= #\> (aref name (- (length name) 1)))
                                                     (not (find-if (lambda (ch) (find ch "<>"))
                                                                   (subseq name 1 (- (length name) 1)))))
                                                (include :system (subseq name 1 (- (length name) 1)))
                                                (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                                                           :format-control "Invalid file name in include form: ~A"
                                                                           :format-arguments (list (symbol-name file))))))
                                         (string
                                          (if (and (< 1 (length file))
                                                   (not (find #\" file)))
                                              (include :local file)
                                              (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                                                         :format-control "Invalid file file in include form: ~S"
                                                                         :format-arguments (list file))))
                                         (otherwise
                                          (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                                                     :format-control "Invalid file name in include form: ~S"
                                                                     :format-arguments (list file)))))
                                     files))))

(defun parse-ifdef (form)
  (not-implemented-yet 'parse-ifdef form))

(defun parse-ifndef (form)
  (not-implemented-yet 'parse-ifndef form))

(defun parse-if (form)
  (not-implemented-yet 'parse-if form))




;;;---------------------------------------------------------------------
;;; Types

(defparameter *storage-classes*
  '(|typedef| |extern| |static| |thread-local| |auto| |register|))

(defparameter *storage-classes-map*
  '((|thread-local| . |_Thread_local|)))

(defparameter *type-qualifiers*
  '(|const| |restrict| |volatile| |atomic|))

(defparameter *type-qualifiers-map*
  '((|atomic|  . |_Atomic|)))

(defparameter *type-specifiers*
  '(|void| |char| |short| |int| |long| |float| |double| |signed| |unsigned| |bool| |complex|))

(defparameter *type-specifiers-map*
  '((|bool|    . |_Bool|)
    (|complex| . |_Complex|)))

(defparameter *type-constructors*
  '(|struct| |enum| |union| |atomic|))

(defparameter *type-declarators*
  '(|pointer| |array| |function|))

(defparameter *compound-types*  (concatenate 'list *type-constructors* *type-declarators*))

(defun compound-type-form-p (form)
  (and (listp form) (find (first form) *compound-types*)))

 (defun function-specifier-p (item)
   (find item '(|inline| |noreturn|)))

(defparameter *scalar-types* '(((|void|))
                               ((|char|))
                               ((|signed| |char|))
                               ((|unsigned| |char|))
                               ((|short|) (|signed| |short|) (|short| |int|) (|signed| |short| |int|))
                               ((|unsigned| |short|) (|unsigned| |short| |int|))
                               ((|int|) (|signed|) (|signed| |int|))
                               ((|unsigned|) (|unsigned| |int|))
                               ((|long|)  (|signed| |long|)  (|long| |int|)   (|signed| |long| |int|))
                               ((|unsigned| |long|)  (|unsigned| |long| |int|))
                               ((|long| |long|)  (|signed| |long| |long|)  (|long| |long| |int|)  (|signed| |long| |long| |int|))
                               ((|unsigned| |long| |long|)  (|unsigned| |long| |long| |int|))
                               ((|float|))
                               ((|double|))
                               ((|long| |double|))
                               ((|bool|))
                               ((|float| |complex|))
                               ((|double| |complex|))
                               ((|long| |double| |complex|))))


(defun ensure-type-list (type)
  (if (or (atom type)
          (compound-type-form-p type))
      (list type)
      type))

(defun split-storage-classes-and-type-qualifiers (list)
  (loop
    :for (item . rest) :on list
    :while (or (member item *storage-classes*)
               (member item *type-qualifiers*))
    :if (member item *storage-classes*)
      :collect item :into storage-classes
    :else
      :collect item :into type-qualifiers
    :finally (return (values storage-classes type-qualifiers (cons item rest)))))

(defun split-type (tokens)
  (loop
    :for token :in tokens
    :if rest
      :collect token :into rest
    :else :if (member token *storage-classes*)
            :collect token :into storage-classes
    :else :if (member token *type-qualifiers*)
            :collect token :into type-qualifiers
    :else :if (member token *type-specifiers*)
            :collect token :into type-specifiers
    :else :if (compound-type-form-p token)
            :collect token :into compound-types
    :else :if (and (null identifiers)
                   (c-identifier-p token))
            :collect token :into identifiers
    :else
      :collect token :into rest
    :finally (return (values storage-classes
                             type-qualifiers
                             type-specifiers
                             identifiers
                             compound-types
                             rest))))

(defun validate-split-type (storage-classes type-qualifiers type-specifiers identifiers compound-types)
  (declare (ignore type-qualifiers)) ; TODO
  (let* ((thread-local (find '|thread-local| storage-classes))
         (classes (remove thread-local storage-classes)))
    (when (cdr classes)
      (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                 :format-control "Only one storage class can be specified at once: ~A"
                                 :format-arguments (list storage-classes)))
    (when (and thread-local (set-difference classes '(|extern| |static|)))
      (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                 :format-control "thread-local can only be specified with extern or static: ~A"
                                 :format-arguments (list storage-classes))))
  (when type-specifiers
    (unless (validate-type type-specifiers)
      (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                 :format-control "Invalid type specifier: ~A"
                                 :format-arguments (list type-specifiers))))
  (unless (or type-specifiers identifiers compound-types)
    (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                               :format-control "No types specifier given"
                               :format-arguments '()))
  (when (or (and type-specifiers identifiers)
            (and type-specifiers compound-types)
            (and identifiers compound-types))
    (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                               :format-control "Multiple types specified: ~@{~@[~{~A~^ ~}~^, ~]~}"
                               :format-arguments (list type-specifiers identifiers compound-types)))
  (when (cdr identifiers)
    (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                               :format-control "Multiple types specified: ~{~A~^, ~}"
                               :format-arguments (list identifiers)))
  (when (cdr compound-types)
    (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                               :format-control "Multiple compound types specified: ~{~A~^, ~}"
                               :format-arguments (list identifiers))))

(defun map-token (token map) (or (cdr (assoc token map)) token))

(defun count-elements (list)
  (let ((counts '()))
    (dolist (element list (sort counts (function string<) :key (function car)))
      (let ((entry (assoc element counts)))
        (if (null entry)
            (push (cons element 1) counts)
            (incf (cdr entry)))))))

(defun simple-type-equal-p (a b)
  (equal (count-elements a) (count-elements b)))

(defun validate-type (type)
  (find-if (lambda (alternatives)
             (find type alternatives :test (function simple-type-equal-p)))
           *scalar-types*))

;;----------------------------------------
(defclass c-type (c-item)
  ())

(defclass c-named-type (c-type)
  ((name            :initarg :name            :initform nil :reader c-type-name)
   (storage-classes :initarg :storage-classes :initform '() :reader c-type-storage-classes)
   (qualifiers      :initarg :qualifiers      :initform '() :reader c-type-qualifiers)))

(defmethod generate ((item c-named-type))
  (let ((storage-classes (c-type-storage-classes item))
        (qualifiers      (c-type-qualifiers item))
        (sep             ""))
    (dolist (storage-class storage-classes)
      (emit sep (map-token storage-class *storage-classes-map*))
      (setf sep " "))
    (dolist (qualifier qualifiers)
      (emit sep (map-token qualifier *type-qualifiers-map*))
      (setf sep " "))
    (emit sep)))

;;----------------------------------------
(defclass c-simple-type (c-named-type)
  ())

(defun c-simple-type (name storage-classes qualifiers)
  (make-instance 'c-simple-type :name name
                                :storage-classes storage-classes
                                :qualifiers qualifiers))

(defmethod generate ((item c-simple-type))
  (let ((name (c-type-name item)))
    (when (null name)
      (error 'linc-internal-error
             :source-file *translate-linc-truename*
             :format-control "A ~S such as ~S must not have a null name"
             :format-arguments (list 'c-simple-type item)))
    (call-next-method)
    (if (atom name)
        (generate name)
        (let ((sep ""))
          (dolist (item (c-type-name item))
            (emit sep (map-token item *type-specifiers-map*))
            (setf sep " "))))))

;;----------------------------------------
(defclass c-struct-union (c-named-type)
  ((operator :initarg :operator :reader c-struct-union-operator)
   (slots :initarg :slots :initform '()  :reader c-struct-union-slots)))

(defun c-struct-union (name storage-classes qualifiers
                       operator slots)
  (make-instance 'c-struct-union :name name
                                 :storage-classes storage-classes
                                 :qualifiers qualifiers
                                 :operator operator
                                 :slots slots))

(defmethod generate ((item c-struct-union))
  (emit (c-struct-union-operator item))
  (when (c-type-name item)
    (emit " ")
    (generate (c-type-name item)))
  (when (c-struct-union-slots item)
    (emit " ")
    (with-parens "{}"
      (dolist (slot (c-struct-union-slots item))
        (generate slot)))))

;;----------------------------------------
(defclass c-slot (c-item)
  ((name       :initarg :name                :reader c-slot-name)
   (type       :initarg :type                :reader c-slot-type)
   (bits       :initarg :bits  :initform nil :reader c-slot-bits)))

(defun c-slot (name type bits)
  (make-instance 'c-slot :name name :type type :bits bits))

(defmethod generate ((item c-slot))
  (emit :fresh-line)
  (generate (c-slot-type item))
  (emit " ")
  (generate (c-slot-name item))
  (let ((bits (c-slot-bits item)))
    (when bits
      (emit ":")
      (generate bits)))
  (emit ";" :newline))

(defun parse-slot (slot)
  (let* ((current        slot)
         (name           (pop current))
         (bits           (member-if (lambda (item)
                                      (and (listp item)
                                           (eql '|bit| (first item))))
                                    current))
         (type           (ldiff current bits))
         (bit-field-size nil))
    (when bits
      (when (cdr bits)
        (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                   :format-control "Invalid bit field size specifier ~S in slot ~S"
                                   :format-arguments (list (first bits) slot)))
      (setf bit-field-size (parse-expression (second (first bits)))))
    (c-slot name (parse-type (ensure-type-list type)) bit-field-size)))


;; types := void char short int long float double signed unsigned bool complex
;;        | (struct …) | (enum …) | (union …) | (atomic …) | (pointer …) | (array …) | (function …)
;;        | identifier

#|

(type (qualifiers…) (types…))
(type (qualifiers…) identifier)

(struct (qualifiers…) name slots…)
(union (qualifiers…) name slots…)
(enum name values…)

(type (qualifiers…) (struct identifier))
(type (qualifiers…) (struct [identifier] slots…))
(type (qualifiers…) (struct [identifier] slots…))

;; foo  (pointer const atomic unsigned int) -> (type const atomic unsigned int) ; (pointer foo)


qualifier ::=

const
restrict
volatile
atomic

(align-as type)
(align-as size)


type ::=

identfier

(atomic type)
(struct name)
(struct name slots…)
(struct slots…)
(union name)
(union name slots…)
(union slots…)
(enum name)
(enum name values…)
(enum values…)


(pointer qualifiers… type)
(array qualifiers|static… type [static] [expression|*])
(function ((identifer type) (type) ...) type

(declare-function fname ((identifer type) (type) ...) type inline|noreturn…
  (block …))


slot ::=

(name qualifiers… type [(bit size)])
(qualifiers… type [(bit size)])

|#


(defun parse-struct-or-union-type (type)
  ;; (struct|union [name] (slot)…)
  (let ((operator (pop type))
        (name (when (atom (first type))
                (pop type))))
    (when name (check-identifier name))
    (c-struct-union name nil nil operator (mapcar (function parse-slot) type))))


;;----------------------------------------
(defclass c-enum (c-named-type)
  ((values :initarg :values :initform '()  :reader c-enum-values)))

(defun c-enum (name storage-classes qualifiers values)
  (make-instance 'c-enum :name name
                         :storage-classes storage-classes
                         :qualifiers qualifiers
                         :values values))

(defmethod generate ((item c-enum))
  (emit "enum")
  (when (c-type-name item)
    (emit " ")
    (generate (c-type-name item)))
  (when (c-enum-values item)
    (emit " ")
    (with-parens "{}"
      (dolist (value (c-enum-values item))
        (generate value)))))

(defclass c-enum-value (c-item)
  ((name :initarg :name :reader c-enum-value-name)
   (value :initarg :value :initform nil :reader c-enum-value-value)))

(defun c-enum-value (name value)
  (make-instance 'c-enum-value :name name :value value))

(defmethod generate ((item c-enum-value))
  (emit :fresh-line)
  (generate (c-enum-value-name item))
  (when (c-enum-value-value item)
    (emit "=")
    (generate (c-enum-value-value item)))
  (emit ",")
  (emit :newline))

(defun parse-enum-type         (form)
  ;; (enum [name] [storage-classes|type-qualifiers]… constant-variable (constant-variable value-expression))
  (let ((current form))
    (pop current)
    (let ((name (when (let ((item (first current)))
                        (and (atom item)
                             (not (or (member item *storage-classes*)
                                      (member item *type-qualifiers*)))))
                  (pop current))))
      (when name
        (check-identifier name))
      (multiple-value-bind (storage-classes type-qualifiers values) (split-storage-classes-and-type-qualifiers current)
        (c-enum name storage-classes type-qualifiers
                (mapcar (lambda (value-form)
                          (if (atom value-form)
                              (progn
                                (check-identifier value-form)
                                (c-enum-value value-form nil))
                              (let* ((current   value-form)
                                     (name      (pop current))
                                     (constexpr (pop current)))
                                (when current
                                  (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                                             :format-control "Too many items in the enum value: ~S"
                                                             :format-arguments (list value-form)))
                                (check-identifier name)
                                (c-enum-value name (parse-expression constexpr)))))
                        values))))))

;;;-------------------------------------------------------------------------------
(defclass c-atomic (c-type)
  ((type :initarg :type :reader c-atomic-type)))

(defun c-atomic (type)
  (make-instance 'c-atomic :type type))

(defmethod generate ((item c-atomic))
  (emit "_Atomic")
  (with-parens "()" (generate (c-atomic-type item))))

(defun parse-atomic-type       (form)
  (let ((current form))
    (pop current)
    (c-atomic (parse-type current))))

;;;-------------------------------------------------------------------------------
(defclass c-pointer (c-type)
  ((type            :initarg :type                          :reader c-pointer-type)
   (qualifiers      :initarg :qualifiers      :initform '() :reader c-type-qualifiers)))

(defun c-pointer (type qualifiers)
  (make-instance 'c-pointer :type type :qualifiers qualifiers))

(defmethod generate ((item c-pointer))
  (generate (c-pointer-type item))
  (emit " " "*")
  (dolist (qualifier  (c-type-qualifiers item))
    (emit " " (map-token qualifier *type-qualifiers-map*))))

(defun parse-pointer-type      (form)
  ;; (pointer qualifiers… type)
  (let ((current form))
    (pop current)
    (multiple-value-bind (storage-classes type-qualifiers type-specifiers identifiers compound-types rest) (split-type current)
      (validate-split-type storage-classes type-qualifiers type-specifiers identifiers compound-types)
      (when rest
        (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                   :format-control "Superfluous tokens after type: ~S"
                                   :format-arguments (list rest)))
      (unless (null storage-classes)
        (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                   :format-control "Invalid storage class in pointer type: ~S"
                                   :format-arguments (list form)))
      (c-pointer (cond
                   (type-specifiers (c-simple-type       type-specifiers        nil nil))
                   (identifiers     (c-simple-type       (first identifiers)    nil nil))
                   (compound-types  (parse-compound-type (first compound-types) nil nil)))
                 type-qualifiers))))

;;;-------------------------------------------------------------------------------
(defclass c-array (c-type)
  ((qualifiers      :initarg :qualifiers      :initform '() :reader c-type-qualifiers)
   (storage-classes :initarg :storage-classes :initform '() :reader c-type-storage-classes)
   (element-type    :initarg :element-type                  :reader c-array-element-type)
   (element-count   :initarg :element-count   :initform nil :reader c-array-element-count)))

(defun c-array (type-qualifiers storage-classes element-type element-count)
  (make-instance 'c-array :qualifiers type-qualifiers
                          :storage-classes storage-classes
                          :element-type element-type
                          :element-count element-count))

(defmethod generate ((item c-array))
  (generate (c-array-element-type item))
  (with-parens "[]"
    (let ((sep ""))
      (dolist (storage-class (c-type-storage-classes item))
        (emit sep (map-token storage-class *storage-classes-map*))
        (setf sep " "))
      (dolist (qualifier (c-type-qualifiers item))
        (emit sep (map-token qualifier *type-qualifiers-map*))
        (setf sep " "))
      (when (c-array-element-count item)
        (emit sep)
        (generate (c-array-element-count item))))))

(defun parse-array-type        (form)
  ;; (array (type) qualifiers|static… [expression|*])
  (let ((current form))
    (pop current)
    (let* ((type         (pop current))
           (element-type (if (and type (or (symbolp type) (listp type)))
                             (parse-type (ensure-type-list type))
                             (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                                        :format-control "Invalid element-type array type: ~S"
                                                        :format-arguments (list type)))))
      (multiple-value-bind (storage-classes type-qualifiers current) (split-storage-classes-and-type-qualifiers current)
        (unless (or (null storage-classes)
                    (equal storage-classes '(|static|)))
          (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                     :format-control "Invalid storage class in array type: ~S"
                                     :format-arguments (list storage-classes)))
        (let ((element-count (cond
                               ((null current)           nil)
                               ((eql '* (first current)) (pop current))
                               (t      (parse-expression (pop current))))))
          (when current
            (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                       :format-control "Invalid tokens in array type: ~S"
                                       :format-arguments (list current)))
          (c-array type-qualifiers storage-classes element-type element-count))))))

;;;-------------------------------------------------------------------------------
(defclass c-parameter (c-item)
  ((name :initarg :name :initform nil :reader c-parameter-name)
   (type :initarg :type :reader c-parameter-type)))

(defun c-parameter (name type)
  (make-instance 'c-parameter :name name :type type))

(defmethod generate ((item c-parameter))
  (generate (c-parameter-type item))
  (when (c-parameter-name item)
    (emit " ")
    (generate (c-parameter-name item))))

(defun parse-function-parameter (form)
  ;; ([identifer] type)
  (let ((name (first form)))
    (if (and (not (null name))
             (symbolp name)
             (not (member name *storage-classes*))
             (not (member name *type-qualifiers*))
             (not (member name *type-specifiers*))
             (not (null (second form))))
        (c-parameter name (parse-type (rest form)))
        (c-parameter nil  (parse-type form)))))

;;;-------------------------------------------------------------------------------
(defclass c-ftype (c-type)
  ((parameters  :initarg :parameters  :reader c-ftype-parameters)
   (result-type :initarg :result-type :reader c-ftype-result-type)))

(defun c-ftype (parameters result-type)
  (make-instance 'c-ftype :parameters parameters
                          :result-type result-type))

(defmethod generate ((item c-ftype))
  (generate (c-ftype-result-type item))
  (emit " ")
  (with-parens "()"
    (emit "*"))
  (with-parens "()"
    (let ((sep ""))
      (dolist (parameter (c-ftype-parameters item))
        (emit sep)
        (generate parameter)
        (setf sep ", ")))))

(defun parse-function-signature (form)
  ;; ((([identifer] type) |...|) type [inline|noreturn]… body…)
  (check-type form list)
  (let ((current form))
    (unless (listp (first current))
      (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                 :format-control "Invalid parameter list in function signature: ~S"
                                 :format-arguments (list (first current))))
    (let ((parameters (mapcar (lambda (parameter)
                                (if (eql parameter '|...|)
                                    parameter
                                    (parse-function-parameter (if (atom parameter)
                                                                  (list parameter)
                                                                  parameter))))
                              (pop current))))
      (let ((ellipsis  (member '|...| parameters)))
        (when  (rest ellipsis)
          (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                     :format-control "Invalid parameter list in function signature; ellipsis must be last: ~S"
                                     :format-arguments (list ellipsis))))
      (unless current
        (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                   :format-control "Missing return-type in signature signature: ~S"
                                   :format-arguments (list form)))
      (let* ((type        (pop current))
             (return-type (parse-type (ensure-type-list type)))
             (specifiers  (loop :while (function-specifier-p (first current))
                                :collect (pop current) :into specifiers
                                :finally (return (delete-duplicates specifiers))))
             (body        (ensure-block current)))
        (values parameters return-type specifiers body)))))

(defun parse-function-type     (form)
  ;; (function (([identifer] type) |...|) type)
  (let ((current form))
    (pop current)
    (multiple-value-bind (parameters return-type specifiers body) (parse-function-signature current)
      (when specifiers
        (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                   :format-control "Unexpected function specifiers in function type declaration ~S"
                                   :format-arguments (list specifiers)))
      (when body
        (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                   :format-control "Unexpected function body in function type declaration ~S"
                                   :format-arguments (list body)))
      (c-ftype parameters return-type))))

;;;-------------------------------------------------------------------------------
(defun parse-compound-type (form storage-class type-qualifiers)
  ;; (struct …) | (enum …) | (union …) | (atomic …) | (pointer …) | (array …) | (function …)
  (when (atom form)
    (error 'linc-internal-error :source-form *source-form* :source-file *translate-linc-truename*
                                :format-control "Invalid compound type: ~A"
                                :format-arguments (list form)))
  (when (or storage-class type-qualifiers)
    (error 'linc-internal-error :source-form *source-form* :source-file *translate-linc-truename*
                                :format-control "Compound type cannot take storage classes or type qualifiers for now: ~S ~S"
                                :format-arguments (list storage-class type-qualifiers)))
  (case (first form)
    ((|struct| |union|) (parse-struct-or-union-type form))
    ((|enum|)           (parse-enum-type            form))
    ((|atomic|)         (parse-atomic-type          form))
    ((|pointer|)        (parse-pointer-type         form))
    ((|array|)          (parse-array-type           form))
    ((|function|)       (parse-function-type        form))
    (otherwise
     (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                :format-control "Invalid compound type: ~A"
                                :format-arguments (list form)))))

(defun parse-type (type)
  (multiple-value-bind (storage-classes type-qualifiers type-specifiers identifiers compound-types rest) (split-type type)
    (validate-split-type storage-classes type-qualifiers type-specifiers identifiers compound-types)
    (when rest
      (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                 :format-control "Superfluous tokens after type: ~S"
                                 :format-arguments (list rest)))
    (cond
      (type-specifiers (c-simple-type       type-specifiers        storage-classes type-qualifiers))
      (identifiers     (c-simple-type       (first identifiers)    storage-classes type-qualifiers))
      (compound-types  (parse-compound-type (first compound-types) storage-classes type-qualifiers)))))

;;;---------------------------------------------------------------------
(defclass c-declaration (c-item)
  ((declared :initarg :declared :reader c-declaration-declared)))

(defun c-declaration (declared)
  (make-instance 'c-declaration :declared declared))

(defmethod generate ((item c-declaration))
  (emit :fresh-line)
  (generate (c-declaration-declared item))
  (emit ";" :newline))

(defmethod ensure-statement ((item c-declaration))
  item)

;;;---------------------------------------------------------------------
(defclass c-definition (c-item)
  ((defined         :initarg :defined                     :reader c-definition-defined)
   (needs-semicolon :initarg :needs-semicolon :initform t :reader c-definition-needs-semicolon-p)))

(defun c-definition (defined &key (needs-semicolon t))
  (make-instance 'c-definition :defined defined :needs-semicolon needs-semicolon))

(defmethod generate ((item c-definition))
  (emit :fresh-line)
  (generate (c-definition-defined item))
  (when (c-definition-needs-semicolon-p item)
    (emit ";"))
  (emit :newline))

(defmethod ensure-statement ((item c-definition))
  item)

;;;---------------------------------------------------------------------
(defclass c-typedef (c-item)
  ((name :initarg :name :reader c-typedef-name)
   (type :initarg :type :reader c-typedef-type)))

(defun c-typedef (name type)
  (check-type name symbol)
  (check-type type c-type)
  (make-instance 'c-typedef :name name :type type))

(defmethod generate ((item c-typedef))
  (emit "typedef" " ")
  (generate (c-typedef-type item))
  (emit " ")
  (generate (c-typedef-name item)))

(defun parse-declare-struct-or-union (operator form)
  ;; (declare-structure name slots)
  ;; (declare-union     name alternatives)
  (let ((name-and-slots form))
    (pop name-and-slots)
    (let ((name (first name-and-slots)))
      (check-identifier name))
    (c-declaration (parse-struct-or-union-type `(,operator ,@name-and-slots)))))

(defun parse-declare-structure (form)
  ;; (declare-structure name slots)
  (parse-declare-struct-or-union '|struct| form))

(defun parse-declare-union       (form)
  ;; (declare-union     name alternatives)
  (parse-declare-struct-or-union '|union| form))

(defun parse-declare-enumeration (form)
  ;; (declare-enumeration name values)
  (let ((name-and-values form))
    (pop name-and-values)
    (let ((name (first name-and-values)))
      (check-identifier name))
    (c-declaration (parse-enum-type `(|enum| ,@name-and-values)))))

(defun parse-declare-type        (form)
  ;; (declare-type      name type)
  (let ((type form))
    (pop type)
    (let ((name (pop type)))
      (check-identifier name)
      (when (null type)
        (error 'linc-program-error
               :source-form *source-form* :source-file *translate-linc-truename*
               :format-control "Missing type in declare-type form: ~S"
               :format-arguments (list form)))
      (c-declaration (c-typedef name (parse-type type))))))


;;;---------------------------------------------------------------------
(defclass c-constant (c-item)
  ((name  :initarg :name                :reader c-constant-name)
   (type  :initarg :type                :reader c-constant-type)
   (value :initarg :value :initform nil :reader c-constant-value)))

(defun c-constant (name type value)
  (check-type name symbol)
  (check-type type c-type)
  (check-type value (or null c-expression))
  (make-instance 'c-constant :name name :type type :value value))

(defmethod generate ((item c-constant))
  (emit "const" " ")
  (generate (c-constant-type item))
  (emit " ")
  (generate (c-constant-name item))
  (when (c-constant-value item)
    (emit " " "=" " ")
    (generate (c-constant-value item))))

(defun parse-declare-constant    (form)
  ;; (declare-constant      name type)
  (let ((current form))
    (pop current)
    (let ((name (pop current)))
      (check-identifier name)
      (when (null current)
        (error 'linc-program-error
               :source-form *source-form* :source-file *translate-linc-truename*
               :format-control "Missing type in declare-constant form: ~S"
               :format-arguments (list form)))
      (let ((type (pop current)))
        (when current
          (error 'linc-program-error
                 :source-form *source-form* :source-file *translate-linc-truename*
                 :format-control "Superfluous tokens define-constant form: ~S"
                 :format-arguments (list form))))
      (c-declaration (c-constant name
                                 (parse-type (if (listp type)
                                                 type
                                                 (list type)))
                                 nil)))))

(defun parse-define-constant    (form)
  ;; (define-constant      name type value)
  (let ((current form))
    (pop current)
    (let ((name (pop current)))
      (check-identifier name)
      (when (null current)
        (error 'linc-program-error
               :source-form *source-form* :source-file *translate-linc-truename*
               :format-control "Missing type in define-constant form: ~S"
               :format-arguments (list form)))
      (let ((type (pop current)))
        (when (null current)
          (error 'linc-program-error
                 :source-form *source-form* :source-file *translate-linc-truename*
                 :format-control "Missing value in define-constant form: ~S"
                 :format-arguments (list form)))
        (let ((value (pop current)))
          (c-definition (c-constant name (parse-type type) (parse-expression value))))))))

;;;---------------------------------------------------------------------
(defclass c-variable (c-expression)
  ((name  :initarg :name                :reader c-variable-name)
   (type  :initarg :type                :reader c-variable-type)
   (value :initarg :value :initform nil :reader c-variable-value)))

(defun c-variable (name type value)
  (check-type name symbol)
  (check-type type c-type)
  (check-type value (or null c-expression))
  (make-instance 'c-variable :name name :type type :value value))

(defmethod generate ((item c-variable))
  (generate (c-variable-type item))
  (emit " ")
  (generate (c-variable-name item))
  (when (c-variable-value item)
    (emit " " "=" " ")
    (generate (c-variable-value item))))

(defun parse-declare-variable    (form)
  ;; (declare-variable      name type)
  (let ((type form))
    (pop type)
    (let ((name (pop type)))
      (check-identifier name)
      (when (null type)
        (error 'linc-program-error
               :source-form *source-form* :source-file *translate-linc-truename*
               :format-control "Missing type in declare-variable form: ~S"
               :format-arguments (list form)))
      (c-declaration (c-variable name (parse-type type) nil)))))

(defun parse-define-variable    (form)
  ;; (define-variable      name type value)
  (let ((current form))
    (pop current)
    (let ((name (pop current)))
      (check-identifier name)
      (when (null current)
        (error 'linc-program-error
               :source-form *source-form* :source-file *translate-linc-truename*
               :format-control "Missing type in define-variable form: ~S"
               :format-arguments (list form)))
      (let ((type (pop current)))
        (when (null current)
          (error 'linc-program-error
                 :source-form *source-form* :source-file *translate-linc-truename*
                 :format-control "Missing value in define-variable form: ~S"
                 :format-arguments (list form)))
        (let ((value (pop current)))
          (c-definition (c-variable name (parse-type (ensure-type-list type)) (parse-expression value))))))))


;;;---------------------------------------------------------------------
(defclass c-function (c-item)
  ((name        :initarg :name        :reader c-function-name)
   (parameters  :initarg :parameters  :reader c-function-parameters)
   (result-type :initarg :result-type :reader c-function-result-type)
   (specifiers  :initarg :specifiers  :reader c-function-specifiers  :initform nil)
   (body        :initarg :body        :reader c-function-body        :initform nil)))

(defun c-function (name parameters result-type specifiers body)
  (check-type name symbol)
  (check-type parameters list)
  (assert (every (lambda (item) (cl:typep item 'c-parameter)) parameters) (parameters))
  (check-type result-type c-type)
  (check-type specifiers list)
  (check-type body (or null c-statement))
  (make-instance 'c-function :name name
                             :parameters parameters
                             :result-type result-type
                             :specifiers specifiers
                             :body body))

(defmethod generate ((item c-function))
  (emit :fresh-line)
  (let ((sep ""))
    (dolist (specifier  (c-function-specifiers item))
      (emit sep)
      (generate specifier)
      (setf sep " "))
    (emit sep)
    (generate (c-function-result-type item)))
  (emit " ")
  (generate (c-function-name item))
  (with-parens "()"
    (let ((sep ""))
      (dolist (parameter (c-function-parameters item))
        (emit sep)
        (generate parameter)
        (setf sep ", "))))
  (when (c-function-body item)
    (emit :newline)
    (generate (c-function-body item))
    (emit :newline)))

(defun parse-declare-function    (form)
  ;; (declare-function    name lambda-list type [inline] [noreturn])
  (let ((current form))
    (pop current)
    (let ((name (pop current)))
      (check-identifier name)
      (multiple-value-bind (parameters return-type specifiers body) (parse-function-signature current)
        (when body
          (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                     :format-control "Unexpected function body in function declaration ~S"
                                     :format-arguments (list body)))
        (c-declaration (c-function name parameters return-type specifiers nil))))))

(defun parse-define-function    (form)
  ;; (define-function     name lambda-list type [inline] [noreturn] &body body)
  (let ((current form))
    (pop current)
    (let ((name (pop current)))
      (check-identifier name)
      (multiple-value-bind (parameters return-type specifiers body) (parse-function-signature current)
        (unless body
          (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                     :format-control "Missing function body in function definition ~S"
                                     :format-arguments (list form)))
        (c-definition (c-function name parameters return-type specifiers body)
                      :needs-semicolon nil)))))

;;;---------------------------------------------------------------------
(defclass c-macro (c-item)
  ((name        :initarg :name        :reader c-macro-name)
   (parameters  :initarg :parameters  :reader c-macro-parameters   :initform nil)
   (expansion   :initarg :expansion   :reader c-macro-expansion)))

(defun c-macro (name parameters expansion)
  (check-type name symbol)
  (check-type parameters list)
  (check-type expansion string)
  (make-instance 'c-macro :name name
                          :parameters parameters
                          :expansion expansion))

(defmethod generate ((item c-macro))
  (emit :fresh-line "#define" " ")
  (generate (c-macro-name item))
  (when (c-macro-parameters item)
    (with-parens "()"
      (let ((sep ""))
        (dolist (parameter (c-macro-parameters item))
          (emit sep)
          (generate parameter)
          (setf sep ", ")))))
  (emit " ")
  (emit (c-macro-expansion item))
  (emit :newline))


(defun parse-define-macro        (form)
  ;; (define-macro        name [lambda-list] expansion-string)
  (let ((current form))
    (pop current)
    (let ((name (pop current)))
      (check-identifier name)
      (flet ((check-eof (current)
               (when current
                 (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                            :format-control "Superfluous tokens after macro expansion: ~S"
                                            :format-arguments (list current))))
             (check-expansion (expansion)
               (unless (stringp expansion)
                 (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                            :format-control "A C macro expansion must be a string, not ~S"
                                            :format-arguments (list expansion)))))
        (if (listp (first current))
            (let ((parameters (pop current))
                  (expansion  (pop current)))
              (check-eof current)
              (check-expansion expansion)
              (dolist (parameter parameters)
                (check-identifier parameter))
              (c-definition (c-macro name parameters expansion) :needs-semicolon nil))
            (let ((expansion (pop current)))
              (check-eof current)
              (check-expansion expansion)
              (c-definition (c-macro name nil expansion) :needs-semicolon nil)))))))

;;;---------------------------------------------------------------------


(defun parse-expression (expression)
  (if (atom expression)
      (if (symbolp expression)
          (c-varref expression)
          (c-literal expression))
      (let* ((operator  (first expression))
             (arguments (if (member operator '(com.informatimago.languages.linc.c:|cast|))
                            (rest expression)
                            (mapcar (function parse-expression) (rest expression)))))
        (flet ((op-or-call ()
                 (cond
                   ((c-operator-p operator)
                    (apply operator arguments))
                   ((symbolp      operator)
                    (apply (function expr-call) operator arguments))
                   (t (error 'linc-program-error :source-form *source-form* :source-file *translate-linc-truename*
                                                 :format-control "Invalid operator ~S in expression ~S"
                                                 :format-arguments (list operator expression))))))
          (case operator
            ((com.informatimago.languages.linc.c:+)
             (if (= 1 (length arguments))
                 (expr-pos (first arguments))
                 (op-or-call)))
            ((com.informatimago.languages.linc.c:-)
             (if (= 1 (length arguments))
                 (expr-neg (first arguments))
                 (op-or-call)))
            ((com.informatimago.languages.linc.c:*)
             (if (= 1 (length arguments))
                 (expr-deref (first arguments))
                 (op-or-call)))
            ((com.informatimago.languages.linc.c:&)
             (if (= 1 (length arguments))
                 (expr-address (first arguments))
                 (op-or-call)))
            ((com.informatimago.languages.linc.c:|::|)
             (apply (if (= 1 (length arguments))
                        (function absolute-scope)
                        (function expr-scope))
                    arguments))
            ((com.informatimago.languages.linc.c:|cast|)
             (expr-cast (parse-expression (first arguments)) (parse-type (rest arguments))))
            ((com.informatimago.languages.linc.c:|post--|)
             (expr-postdecr (first arguments)))
            ((com.informatimago.languages.linc.c:|post++|)
             (expr-postincr (first arguments)))
            (otherwise
             (op-or-call)))))))


;; (enable-c-sexp-reader-macros)
;; (parse-expression (first '{ (= a (? (== a (cast 0 int)) 1 (+ a (* b (- c d))))) }))
;; (parse-expression (first '{ (cast (+ 1 41) unsigned int) }))

(defun ensure-block (forms)
  ;; ((print 'hi))
  ;; ((print 'hi) (print 'lo))
  ;; ((|block| (print 'hi) (print 'lo)))
  (cond
    ((null forms)
     nil)
    ((and (= 1 (length forms))
          (listp (first forms))
          (member (first (first forms)) '(|block| |let| |let*|)))
     (parse-statement (first forms)))
    (t
     (parse-statement `(|block| ,@forms)))))

(defvar *linc-macros* (make-hash-table))

(defun linc-macro-p (name)
  (gethash name *linc-macros*))

(defun linc-macroexpand (form)
  (funcall (or (gethash (first form) *linc-macros*)
               (error "Not a linc macro ~S" (first form)))
           form))

(defmacro define-linc-macro (name (&rest lambda-list) &body body)
  (let ((whole (gensym)))
    `(progn
       (setf (gethash ',name *linc-macros*)
            (lambda (,whole) (destructuring-bind (,@lambda-list) (rest ,whole)
                               (block ,name ,@body))))
       ',name)))

(define-linc-macro |let| ((&rest bindings) &body body)
  (let ((temps (mapcar (lambda (binding) (gentemp (format nil "temp-~(~A~)" (first binding)) *c-package-name*))
                       bindings)))
    `(|block|
         ,@(mapcar (lambda (temp binding)
                     `(|define-variable| ,temp ,@(rest binding)))
                   temps bindings)
       ,@(mapcar (lambda (temp binding) `(|define-variable| ,@(subseq binding 0 2) ,temp))
                 temps bindings)
       ,@body)))

(define-linc-macro |let*| ((&rest bindings) &body body)
  `(|block|
     ,@(mapcar (lambda (binding) `(|define-variable| ,@binding))
               bindings)
     ,@body))


;; (pprint (linc-macroexpand (first '{(let* ((a int 42) (b int (+ a 2))) (print a b))})))
;; --> (|block|
;;      (|define-variable| COM\.INFORMATIMAGO\.LANGUAGES\.LINC\.C::\a |int| 42)
;;      (|define-variable| COM\.INFORMATIMAGO\.LANGUAGES\.LINC\.C::\b |int| (COM\.INFORMATIMAGO\.LANGUAGES\.LINC\.C:+ COM\.INFORMATIMAGO\.LANGUAGES\.LINC\.C::\a 2))
;;      (COM\.INFORMATIMAGO\.LANGUAGES\.LINC\.C::|print| COM\.INFORMATIMAGO\.LANGUAGES\.LINC\.C::\a COM\.INFORMATIMAGO\.LANGUAGES\.LINC\.C::\b))




(define-linc-macro |cond| (&rest clauses)
  (case (length clauses)
    (0 `(block))
    (1 `(if ,(first (first clauses))
            (block ,@(rest (first clauses)))))
    (otherwise )))


(defun parse-statement (form)
  (if (atom form)
      (stmt-expr (parse-expression form))
      (case (first form)
        ;; 0-ary
        ((|break|)     (stmt-break))
        ((|continue|)  (stmt-continue))
        ;; 1-ary
        ((|label|)     (stmt-label (c-identifier (second form))))
        ((|goto|)      (stmt-goto  (c-identifier (second form))))
        ;; ((asm)       (stmt-asm  (second form)))
        ;; 0/1-ary
        ((|return|)    (if (rest form)
                           (stmt-return (parse-expression (second form)))
                           (stmt-return)))

        ;; any-ary
        ((|block|)     (stmt-block (mapcar (function parse-statement) (rest form))))
        ((|while|)     (stmt-while (ensure-block (rest (rest form)))
                                   (parse-expression (second form))))
        ((|do|)        (let ((while (last form 2))
                             (body  (butlast (rest form) 2)))
                         (unless (eql '|while| (first while))
                           (error "syntax error in (do … while test)"))
                         (stmt-do (ensure-block body)
                                  (parse-expression (second while)))))
        ((|case|)     (stmt-case  (parse-expression (second form))
                                  (ensure-block (rest (rest form)))))
        ((|default|)  (stmt-default (ensure-block (rest (rest form)))))
        ;; syntax
        ((|if|)       (destructuring-bind (if test then &optional else) form
                        (stmt-if (parse-statement then)
                                 (when else (parse-statement else))
                                 (parse-expression test))))
        ((|for|)      (destructuring-bind (for (init test step) &body body) form
                        (stmt-for (parse-expression init) ; TODO declarator!!!
                                  (parse-expression test)
                                  (parse-expression step)
                                  (ensure-block body))))
        ((|switch|)   (destructuring-bind (switch expression &body body) form
                        (stmt-switch (ensure-block body) (parse-expression expression))))
        ;; local declarations or definitions (all but function definitions):
        ((|declare-structure|)   (parse-declare-structure   form))
        ((|declare-union|)       (parse-declare-union       form))
        ((|declare-type|)        (parse-declare-type        form))
        ((|declare-enumeration|) (parse-declare-enumeration form))
        ((|declare-constant|)    (parse-declare-constant    form))
        ((|declare-variable|)    (parse-declare-variable    form))
        ((|declare-function|)    (parse-declare-function    form))
        ((|define-constant|)     (parse-define-constant     form))
        ((|define-variable|)     (parse-define-variable     form))
        ((|define-macro|)        (parse-define-macro        form))
        ;; macros or function calls:
        (otherwise
         (if (linc-macro-p (first form))
             (parse-statement (linc-macroexpand form))
             (stmt-expr (parse-expression form)))))))

;;;---------------------------------------------------------------------

(defun parse-linc-form (form)
  (cond
    ((stringp form) (c-comment form))
    ((atom form)    (error 'linc-stray-atom-error :source-form *source-form* :source-file *translate-linc-truename*
                                                  :format-control "Stray atom in C-sexp source: ~S; ignored."
                                                  :format-arguments (list form)))
    (t (let ((op (first form)))
         (case op
           ((|include|)             (parse-include form))
           ((|#ifdef|)              (parse-ifdef   form))
           ((|#ifndef|)             (parse-ifndef  form))
           ((|#if|)                 (parse-if      form))
           ((|declare-structure|)   (parse-declare-structure   form))
           ((|declare-union|)       (parse-declare-union       form))
           ((|declare-type|)        (parse-declare-type        form))
           ((|declare-enumeration|) (parse-declare-enumeration form))
           ((|declare-constant|)    (parse-declare-constant    form))
           ((|declare-variable|)    (parse-declare-variable    form))
           ((|declare-function|)    (parse-declare-function    form))
           ((|define-constant|)     (parse-define-constant     form))
           ((|define-variable|)     (parse-define-variable     form))
           ((|define-function|)     (parse-define-function     form))
           ((|define-macro|)        (parse-define-macro        form))
           (otherwise
            (error 'linc-invalid-operator-error :source-form *source-form*
                                                :source-file *translate-linc-truename*
                                                :format-control "Invalid operator: ~S"
                                                :format-arguments (list op))))))))

(defun translate-linc-form (*source-form*)
  (let ((item (parse-linc-form *source-form*)))
    (with-output-to-string (*c-out*)
      (generate item))))

(defun translate-linc-file (input-file &key output-file
                                         (verbose *translate-linc-verbose*)
                                         (print   *translate-linc-print*)
                                         (external-format :default))
  (with-open-file (input input-file :external-format external-format)
    (with-open-file (output (or output-file (make-pathname :type "c" :case :local :defaults input-file))
                            :direction :output
                            :external-format external-format
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (write-line    "/* ------------------------- DO NOT EDIT! --------------------------------- */" output)
      (write-line    "/* WARNING: This file is generated automatically by LINC from the source    */" output)
      (let ((name (namestring input-file)))
        (format output "/* file ~VA */~%" (- 78 3 5 3) name))
      (write-line    "/* ------------------------- DO NOT EDIT! --------------------------------- */" output)
      (terpri output)
      (let ((temp-package  (let ((*package* *package*))
                             (com.informatimago.common-lisp.interactive.interactive:mkupack
                              :name (format nil "com.informatimago.languages.linc.c.~(~A~)." (file-namestring input))
                              :use '("COM.INFORMATIMAGO.LANGUAGES.LINC.C")))))
        (unwind-protect
             (let ((*package*         temp-package)
                   (*readtable*       (copy-readtable com.informatimago.languages.linc::*c-readtable*))
                   (*translate-linc-verbose*  verbose)
                   (*translate-linc-print*    print)
                   (*translate-linc-pathname* (pathname input))
                   (*translate-linc-truename* (truename input))
                   (warnings-p        nil)
                   (failures-p        nil))
               (handler-bind ((warning       (lambda (condition)
                                               (declare (ignore condition))
                                               (if warnings-p
                                                   (incf warnings-p)
                                                   (setf warnings-p 1))
                                               nil))
                              (style-warning (lambda (condition)
                                               (declare (ignore condition))
                                               nil))
                              (linc-error    (lambda (condition)
                                               (if failures-p
                                                   (incf failures-p)
                                                   (setf failures-p 1))
                                               (format *error-output* "~&ERROR: ~{~A~%~^       ~}"
                                                       (split-sequence #\newline (princ-to-string condition)))
                                               (finish-output *error-output*)
                                               (invoke-restart (find-restart 'continue-translation condition)))))
                 (loop
                   :for form := (read input nil input)
                   :until (eql form input)
                   :do (when print
                         (let ((*print-pretty* t)
                               (*print-right-margin* 120))
                           (format t "~&~S~%" form)))
                       (with-simple-restart (continue-translation "Continue Translation")
                         (let ((code (translate-linc-form form)))
                           (when verbose
                             (format t "~&~{;;    ~A~%~}" (split-sequence #\newline code)))
                           (write-string code output)))
                   :finally (return (values (truename output) warnings-p failures-p)))))
          (delete-package temp-package))))))

;;;; THE END ;;;;
