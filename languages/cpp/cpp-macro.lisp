;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cpp-macro.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the cpp macros.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-06-28 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.CPP")

(defvar *context* nil) ; the current instance of context.


;; Cpp macro definitions are kept in the environment hash-table, keyed
;; by the macro name (string).

(defgeneric environment-macro-definedp (environment macro-name))
(defgeneric environment-macro-undefine (environment macro-name))
(defgeneric environment-macro-definition (environment macro-name))
(defgeneric (setf environment-macro-definition) (definition environment macro-name))


(defun make-environment ()
   (make-hash-table :test 'equal))

(defmethod environment-macro-definedp ((environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (nth-value 1 (gethash macro-name environment)))

(defmethod environment-macro-undefine ((environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (remhash macro-name environment))

(defmethod environment-macro-definition ((environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (gethash macro-name environment))

(defmethod (setf environment-macro-definition) (definition (environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (setf (gethash macro-name environment) definition))

;; The values kept in the environment are instances of subclasses of macro-definition.

(defclass macro-definition ()
  ((name :initarg :name :accessor macro-definition-name)))

(defgeneric expand-macro-definition (macro-definition &optional arguments)
  (:documentation
   "Binds the arguments (list of list of tokens) to the macro
parameters if any, and expands the macro.

object-like macros don't take arguments.

function-like macros take an argument list, possibly empty, but if it
contains a single empty list of tokens and the function-like macro
takes no parameters, then it's still a match (at this stage, we don't
distinguish f() from f( ) anymore.
"))

(defclass macro-definition/object (macro-definition)
  ((expansion :initarg :expansion :accessor macro-definition-expansion))
  (:documentation "Normal object-like macro."))

(defmethod print-object ((macro macro-definition/object) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (macro stream :type t :identity t)
      (format stream ":name ~S :expansion ~S"
              (macro-definition-name macro)
              (macro-definition-expansion macro)))
    macro))

(defmethod expand-macro-definition ((macro-definition macro-definition/object) &optional (arguments '() argumentsp))
  (declare (ignore arguments))
  (when argumentsp
    (error "~S cannot take arguments for object-like macro ~A" 'expand-macro-definition  (macro-definition-name macro-definition)))
  (substitute-concatenates (macro-definition-expansion macro-definition)))

(defclass macro-definition/function (macro-definition)
  ((parameters :initarg :parameters :initform '() :accessor macro-definition-parameters)
   (expansion :initarg :expansion :accessor macro-definition-expansion))
  (:documentation "Normal function-like macro."))

(defmethod print-object ((macro macro-definition/function) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (macro stream :type t :identity t)
      (format stream ":name ~S :parameters ~S :expansion ~S"
              (macro-definition-name macro)
              (macro-definition-parameters macro)
              (macro-definition-expansion macro)))
    macro))

(defun ellipsis-parameter-p (parameter)
  (and (consp parameter)
       (eq :ellipsis (first parameter))))

;;; ------------------------------------------------------------
;;; The expand-macro-definition method for normal macros.
;;; ------------------------------------------------------------

#|

object-like macros
------------------

- The macro definition is a single line of token.

- When the macro is expanded, the expansion is macroexpanded:

       #undef FOO
       #undef BAR
       #define FOO BAR
       int i[]={ FOO,
       #undef BAR
       #define BAR 1
                 FOO,
       #undef BAR
       #define BAR 2
                 FOO };
    -->
        int i[]={ BAR,


                  1,


                  2 };

- But happily, an object-like macro cannot expand to a partial
  function-like macro invocation:

        #define LEFT F("l",
        #define RIGHT ,"r")
        #define FOO LEFT "foo" RIGHT
        1: FOO; /* ok, FOO expands to F("l","foo","r") */
        #define F(a,b,c) a##b##c
        // 2: FOO; /* good: error: unterminated argument list invoking macro "F" (in expanding LEFT from FOO)*/

- Recursive expansion is detected and prevented.

Therefore we can process object-like macro expansions independently.



function-like macros
--------------------

This is crazy.

- macro arguments are macro-expanded before they are substituted in the macro body,
  but not for their stringify and token concatenation use:

        #define VOO 42
        #define F(X) #X[X]

        F(33)
        F(VOO)
        F(C(VOO,VOO))
    -->
        "33"[33]
        "VOO"[42]
        "C(VOO,VOO)"[VOOVOO]

   So basically, we need to pass the arguments under two forms.


   "If an argument is stringified or concatenated, the prescan does
    not occur. If you want to expand a macro, then stringify or
    concatenate its expansion, you can do that by causing one macro to
    call another macro that does the stringification or
    concatenation."
  This doesn't sound right, from the above test.


- after substitution the entire macro expansion is again scanned for
  macros to be expanded.

- The self-references that do not expand in the first scan are marked
  so that they will not expand in the second scan either. !!!

- but happily again, a function-like macro cannot expand to a partial
  function-like macro invocation, so we can also perform this later
  expansion independently.

  But "toplevel" function-like macro calls can span several lines,
  including pre-processor directives (#ifs, #defines, #includes, etc).
  So parsing function-like macros calls must take into account several
  lines, and may have to perform recursive directive processing 

  
        #undef LEFT
        #undef RIGHT
        #undef FOO
        #undef F
        #define FOO(E) F(l,E,r)
        1: FOO(foo); /* ok */
        #define F(a,b,c) a##b##c
        2: FOO(bar); /* ok since FOO(E) contains the whole F() call. */
        3: FOO(
        #define F(a,b,c) a##a
               baz
        #undef F
        #define F(a,b,c) c##c
               ); 
        4: FOO(
        #undef F 
        #define F(a,b,c) a##a
               FOO(baz)
        #undef F
        #define F(a,b,c) c##c
               ); 
    -->
        1: F(l,foo,r);

        2: lbarr;

        3: rr




                ;
        4: rr





                ;

    Note: the result 4!  The arguments are macro expanded only when
    the macro call F() is being computed, ie. after we've seen the
    closing parenthesis.


- macro arguments must be parenthesis-balanced (only (), not {} or []).
  Within (), commas don't split arguments:

        #define F(X) ((X)+(X))
        F((1,2))
    -->
        ((1,2)+(1,2)) /* == 4 */

- unshielded commas in macro arguments are used as argument separators:

      #define foo a,b
      #define bar(x) lose(x)
      #define lose(x) (1+(x))
      bar(foo) --> lose(a,b) /* wrong argument count */

- arguments may be empty.

      #define foo(a,b) {(0,a),(1,b)}
    -->
      foo(,) --> {(0,),(1,)}


- cf. variadic parameters.

ambiguity, with:
    #define f() foo
    #define g(x) bar
then
    f() g()
have different argument counts :-)

concatenation
-------------

    However, two tokens that don't together form a valid token cannot
    be pasted together. For example, you cannot concatenate x with +
    in either order. If you try, the preprocessor issues a warning and
    emits the two tokens.

    Also, ## concatenates only the first or last token of the argument:
        #define CONCAT(A,B) A ## B
        CONCAT(a b,c d)
    -->
        a bc d /* 3 tokens */

    If the argument is empty, that ‘##’ has no effect. 

|#


;; While macroexpanding function-like macros, we need to bind
;; arguments to parameters.  Arguments are reified in this argument
;; structure, keeping both the tokens of the arguments, and their
;; macro-expanded form, and with lazy initialization of the
;; stringified version.

(defstruct argument
  tokens
  expanded
  %stringified)

(defun stringify (tokens)
  (let ((first-token (find-if-not (function null) tokens)))
    (apply (function make-string-literal) (format nil "~S" (mapconcat (function token-text) tokens ""))
           (if first-token
               (list (token-column first-token)
                     (token-line first-token)
                     (token-file first-token))
               (list (context-column *context*)
                     (context-line *context*)
                     (context-file *context*))))))

(defmethod argument-stringified ((argument argument))
  (or (argument-%stringified argument)
      (setf (argument-%stringified argument) (stringify (argument-tokens argument)))))

(defmethod argument-stringified ((argument list))
  (let ((strings     (mapcar (lambda (arg)
                               (mapconcat (function token-text) (argument-tokens arg) ""))
                             argument))
        (first-token (loop
                       :with args = argument
                       :while (and args (null (argument-tokens (first args))))
                       :do (pop args)
                       :finally (return (when args
                                          (find-if-not (function null) (argument-tokens (first args))))))))
    (apply (function make-string-literal)
           (format nil "~S" (mapconcat (function identity) strings ","))
           (if first-token
               (list (token-column first-token)
                     (token-line first-token)
                     (token-file first-token))
               (list (context-column *context*)
                     (context-line *context*)
                     (context-file *context*))))))

(defun concatenate-tokens (tokens)
  (let* ((first-token (find-if-not (function null) tokens))
         (concatenated
           (tokenize-line (cons (mapconcat (function token-text) (remove nil tokens) "")
                                (if first-token
                                    (list (token-line first-token)
                                          (token-file first-token))
                                    (list (context-line *context*)
                                          (context-file *context*))))
                         :accept-unicode-escapes (option *context* :accept-unicode-escapes)
                         :dollar-is-punctuation  (option *context* :dollar-is-punctuation))))
    
    (when (rest concatenated)
      (cpp-error (first tokens)
                 "Pasting ~{~A~^ ~} does not give a valid preprocessing token" (mapcar (function token-text) tokens)))
    concatenated))

(defun substitute-concatenates (line)
  (when line
   (if (sharpsharpp (first line))
       (progn
         (cpp-error (first line) "'##' cannot appear at either end of a macro expansion")
         (substitute-concatenates (rest line)))
       (loop
         :with result = ()
         :while line
         :do (let ((curr (pop line)))
               (if (sharpsharpp (first line))
                   (let ((file (token-file (first line)))
                         (lino (token-line (first line))))
                     (loop
                       :with concat = (list curr)
                       :while (sharpsharpp (first line))
                       :do (pop line)
                           (unless line
                             (cpp-error (pseudo-token file lino) "'##' cannot appear at either end of a macro expansion"))
                           (unless (sharpsharpp (first line)) 
                             (push (pop line) concat))
                       :finally (setf result (nreconc (concatenate-tokens (nreverse concat)) result))))
                   (push curr result)))
         :finally (return (nreverse result))))))

(defun macro-bind (name parameters arguments)
  (loop
    :with bindings := '()
    :with args := arguments
    :with pars := parameters
    :with no-pars = (null pars)
    :while pars
    :do (let ((par (pop pars)))
          (cond
            ((ellipsis-parameter-p par)
             (push (list* (second par) :ellipsis args) bindings)
             (setf args nil))
            ((null args)
             (cpp-error *context* "Missing argument to function-like macro call ~S" (token-text name))
             (return :error))
            (t
             (let ((arg (pop args)))
               (push (cons par arg) bindings)))))
    :finally (when (and args
                        (not (and no-pars
                                  (null (cdr args))
                                  (null (car args)))))
               (cpp-error *context*
                          "Too many args for function-like macro call ~S, parameters = ~S, arguments = ~S"
                          (token-text name) parameters arguments)
               (return :error))
             (return bindings)))

(defmacro skip-nil (block-name macro-name-token-var line-var tokenized-lines-var)
  `(loop :while (null ,line-var)
         :do (if (null ,tokenized-lines-var)
                 (progn
                   (cpp-error *context* "Reached end of file in function-like macro call ~A"
                              (token-text ,macro-name-token-var))
                   (return-from ,block-name nil))
                 (progn
                   (setf (context-input-lines *context*)  ,tokenized-lines-var)
                   (unwind-protect
                        (loop
                          :do (setf (context-current-line *context*)  (pop (context-input-lines *context*)))
                          :while (sharpp (first (context-current-line *context*)))
                          :do (process-directive *context* (context-current-line *context*)))
                     (setf ,line-var (context-current-line *context*)
                           ,tokenized-lines-var  (context-input-lines *context*)))))))

(defun macro-bindings-expand-arguments (bindings)
  (flet ((marg (tokens)
           (make-argument
            :tokens tokens
            :expanded (reduce (function nconc) (macro-expand-macros *context* tokens '() '() nil
                                                                    (context-macros-being-expanded *context*))))))
    (dolist (binding bindings bindings)
      (if (and (listp (cdr binding))
               (eq :ellipsis (second binding)))
          (setf (cddr binding) (mapcar (function marg) (cddr binding)))
          (setf (cdr  binding) (marg (cdr  binding)))))))

(defun substitute-parameters (definition bindings) 
  (flet ((get-entry (ident)
           (assoc (token-text ident) bindings
                  :test (function string=)
                  :key (function token-text))))
    (loop :with result := '()
          :while definition
          :do (let ((item (pop definition)))
                (if (atom item)
                    (cond
                      ((and (commap item) ;; , ## __VA_ARGS__
                            (cdr definition)
                            (sharpsharpp (first definition))
                            (identifierp (second definition))
                            (listp (cdr (get-entry (second definition))))
                            (eq :ellipsis (cadr (get-entry (second definition)))))
                       ;;DEBUG;; (print (list ", ##" item (first definition) (second definition) '/ (get-entry (second definition))))
                       (if (cddr (get-entry (second definition)))
                           (progn (push item result)
                                  (pop definition)) ; pop ##
                           (setf definition (cddr definition))))
                      ((identifierp item)
                       (let ((entry (get-entry item)))
                         (if entry
                             (if (and (listp (cdr entry))
                                      (eq :ellipsis (cadr entry)))
                                 (setf result (revappend (rest (loop
                                                                 :with comma = (make-punctuation "," 0 0 "-")
                                                                 :for item :in (cddr entry)
                                                                 :collect comma
                                                                 :append (argument-expanded item)))
                                                         result))
                                 (setf result (revappend (or (argument-expanded (cdr entry)) '(())) result)))
                             (push item result))))
                      (t
                       (push item result)))
                    (ecase (first item)
                      (:stringify (let ((entry (get-entry (second item))))
                                    (if entry
                                        (push (argument-stringified (if (and (listp (cdr entry))
                                                                             (eq :ellipsis (cadr entry)))
                                                                        (cddr entry)
                                                                        (cdr entry))) result)
                                        (cpp-error (second item) "'#' is not followed by a macro parameter")))))))
          :finally (return (nreverse result)))))

(defmethod expand-macro-definition ((macro-definition macro-definition/function) &optional (arguments '() argumentsp))
  (unless argumentsp
    (error "~S needs arguments for function-like macro ~A()" 'expand-macro-definition (macro-definition-name macro-definition)))
  (let* ((name          (macro-definition-name macro-definition))
         (parameters    (macro-definition-parameters macro-definition))
         (definition    (macro-definition-expansion macro-definition))
         ;; bind arguments to parameters, checking variadic parameters:
         (bindings      (macro-bind name parameters arguments)))
    (when (eq :error bindings)
      (return-from expand-macro-definition '()))
    ;; macroexpand arguments
    ;; we do that after the bindings loop so we don't macro-expand-macros if there's an argcount error.
    ;;DEBUG;; (print (list :before :bindings bindings))
    (setf bindings (macro-bindings-expand-arguments bindings))
    ;; stringify arguments will be done lazily by argument-stringified.
    ;;DEBUG;; (print (list :name name :bindings bindings))
    ;; substitute parameters in definition.
    (remove nil (substitute-concatenates (substitute-parameters definition bindings)))))

;;; ------------------------------------------------------------
;;; built-ins, computed macros.
;;; ------------------------------------------------------------

(defclass macro-definition/computed-mixin ()
  ((compute-expansion-function :initarg :compute-expansion-function
                               :accessor macro-definition-compute-expansion-function)))

(defclass macro-definition/object/computed (macro-definition/object macro-definition/computed-mixin)
  ()
  (:documentation "Built-in, computed object-like macro."))

(defmethod print-object ((macro macro-definition/object/computed) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (macro stream :type t :identity t)
      (format stream ":name ~S"
              (macro-definition-name macro)))
    macro))

(defmethod expand-macro-definition ((macro-definition macro-definition/object/computed) &optional (arguments '() argumentsp))
  (declare (ignore arguments))
  (when argumentsp
    (error "~S cannot take arguments for object-like macro ~A" 'expand-macro-definition (macro-definition-name macro-definition)))
  (funcall (macro-definition-compute-expansion-function macro-definition) macro-definition))

(defclass macro-definition/function/computed (macro-definition/function macro-definition/computed-mixin)
  ()
  (:documentation "Built-in, computed function-like macro."))

(defmethod print-object ((macro macro-definition/function/computed) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (macro stream :type t :identity t)
      (format stream ":name ~S :parameters ~S"
              (macro-definition-name macro)
              (macro-definition-parameters macro)))
    macro))

(defmethod expand-macro-definition ((macro-definition macro-definition/function/computed) &optional (arguments '() argumentsp))
  (unless argumentsp
    (error "~S needs arguments for function-like macro ~A()" 'expand-macro-definition (macro-definition-name macro-definition)))
  (funcall (macro-definition-compute-expansion-function macro-definition) macro-definition arguments))

;;; ------------------------------------------------------------
;;; context
;;; ------------------------------------------------------------

(deftype option-key ()
  `(member :warn-date-time
           ;; Warns when using built-in __DATE__ __TIME__ and __TIMESTAMP__
           ;; as them may produce artificially different executables.
           
           :directives-only
           
           :substitute-trigraphs
           ;; allow trigraph substitutions.
           
           :warn-on-trigraph
           ;; when trigraphs are substituted, warn about it.
           
           :warn-spaces-in-continued-lines

           :single-line-comments
           ;; allow // comments.
           
           :accept-unicode-escapes
           ;;
           
           :dollar-is-punctuation
           ;; when true, $ is considered punctuation.
           ;; when NIL, $ is considered a letter for identifiers.
           
           :warn-on-undefined-identifier
           ;; in #if expressions warns about undefined identifiers

           :generate-sharp-line
           ;; #line generates '# NN "file"' token lines.
           
           :include-disable-current-directory
           ;; When true, files are not searched in the current directory.
           ;; NOTE: current directory is defined as:
           ;;        (or *load-truename* *compile-file-truename*
           ;;            *default-pathname-defaults*)

           :include-quote-directories
           ;; Directories where #include \"\" files are searched.

           :include-bracket-directories
           ;; Directories where #include <> files are searched.
           ;; May contain keywords indexing search functions in the following a-list:

           :include-search-functions
           ;; An a-list mapping keywords to search functions (lambda (path kind directive) …)
           ;; kind (member :quote :bracket), directive (member :include :import)
           ;; RETURN: NIL if include-file is not found,
           ;;         T   if include-file is already included, or
           ;;         a pathname to the include-file to be loaded.

           ))

(defparameter *default-options*
  '((:warn-date-time . t)
    (:directives-only . nil)
    (:substitute-trigraphs . t)
    (:warn-on-trigraph . t) 
    (:warn-spaces-in-continued-lines . t) 
    (:single-line-comments . t)
    (:accept-unicode-escapes . t) 
    (:dollar-is-punctuation . nil)
    (:warn-on-undefined-identifier . nil)
    (:trace-includes . nil)
    (:include-disable-current-directory . nil)
    (:include-quote-directories . ())
    (:include-bracket-directories . ())
    (:include-search-functions . ())
    (:external-format . :default)
    (:generate-sharp-line . nil)))

(defvar *default-environment*         (make-environment))
(defvar *default-pragma-interpreters* (make-hash-table :test 'equal))

(defclass context ()
  ((base-file             :initarg :base-file             
                          :initform "-"                                     
                          :accessor context-base-file)
   (directory             :initarg :directory
                          :initform nil
                          :accessor context-directory
                          :documentation "Include directory of the currently included/imported file, for #include_next.")
   (file                  :initarg :file                  
                          :initform "-"                                     
                          :accessor context-file)
   (line                  :initarg :line                  
                          :initform 1                                       
                          :accessor context-line)
   (column                :initarg :column                
                          :initform 1                                       
                          :accessor context-column)
   (token                 :initarg :token                 
                          :initform nil                                     
                          :accessor context-token)
   (if-level              :initarg :if-level              
                          :initform 0                                       
                          :accessor context-if-level)
   (file-stack            :initarg :file-stack
                          :initform '()
                          :accessor context-file-stack)
   ;; file-stack saves (list file line column token if-level input-lines current-line)
   (input-lines           :initarg :input-lines
                          :initform '()
                          :accessor context-input-lines)
   (current-line          :initarg :current-line
                          :initform '()
                          :accessor context-current-line)
   (output-lines          :initarg :output
                          :initform '()
                          :accessor context-output-lines)

   (counter               :initarg :counter               
                          :initform 0                                       
                          :accessor context-counter)
   (macros-being-expanded :initarg :macros-being-expanded 
                          :initform '()                                     
                          :accessor context-macros-being-expanded)
   (options               :initarg :options               
                          :initform (copy-tree       *default-options*)     
                          :accessor context-options)
   (environment           :initarg :environment           
                          :initform (copy-hash-table *default-environment*) 
                          :accessor context-environment)
   (pragma-interpreters   :initarg :pragma-interpreters   
                          :initform (copy-hash-table *default-pragma-interpreters*)                                     
                          :accessor context-pragma-interpreters
                          :documentation "An a-list mapping module name string to a function taking two arguments: the context and a list of tokens.")
   (pragmas               :initarg :pragmas               
                          :initform (make-hash-table :test 'equal)          
                          :accessor context-pragmas
                          :documentation "An equal hash-table for pragmas defined by the program. Keys may be symbols or lists of symbols.")))

(defun option (context option)
  (cdr (assoc option (context-options context))))
   
(defmethod context-include-level ((context context))
  (length (context-file-stack context)))

(defmethod context-push-file ((context context) path directory input-lines)
  (push (list (context-directory context)
              (context-file context)
              (context-line context)
              (context-column context)
              (context-token context)
              (context-if-level context)
              (context-input-lines context)
              (context-current-line context))
        (context-file-stack context))
  (setf (context-directory context) directory
        (context-file context) path
        (context-line context) 1
        (context-column context) 1
        (context-token context) nil
        (context-if-level context) 0
        (context-input-lines context) input-lines
        (context-current-line context) nil)
  context)

(defmethod context-pop-file ((context context))
  (let ((data (pop (context-file-stack context))))
    (setf (context-directory context) (pop data)
          (context-file context) (pop data)
          (context-line context) (pop data)
          (context-column context) (pop data)
          (context-token context) (pop data)
          (context-if-level context) (pop data)
          (context-input-lines context) (pop data)
          (context-current-line context) (pop data)))
  context)

(defmethod update-context ((context context) &key
                          (token         nil tokenp)
                          (line          nil linep)
                          (column        nil columnp)
                          (file          nil filep))
  (when tokenp          (setf (context-token         context) token))
  (when linep           (setf (context-line          context) line))
  (when columnp         (setf (context-column        context) column))
  (when filep           (setf (context-file          context) file))
  context)

;;; ------------------------------------------------------------
;;; macro-expand-macros, expands macro on one or more tokenized lines.
;;; ------------------------------------------------------------

(defmacro expect (macro-name-token-var token-predicate-name line-var tokenized-lines-var)
  `(block expect
     (skip-nil expect ,macro-name-token-var ,line-var ,tokenized-lines-var)
     (if (,token-predicate-name (first ,line-var))
         (return-from expect (pop ,line-var))
         (progn
           (cpp-error (first ,line-var) "Expected a ~A in function-like macro call ~A, not ~A"
                      (token-predicate-label ',token-predicate-name)
                      (token-text ,macro-name-token-var)
                      (token-text (first ,line-var)))
           (return-from expect nil)))))

(defun parse-function-macro-call-arguments (macro-name-token line tokenized-lines)
  ;; function-macro-call     ::= macro-name '(' arglist  ')' .
  ;; arglist                 ::= argument | argument ',' arglist .
  ;; argument                ::= | argument-item argument .
  ;; argument-item           ::= non-parenthesis-or-comma-token | parenthesized-item-list .
  ;; parenthesized-item-list ::= '(' item-list ')' .
  ;; item-list               ::= | non-parenthesis-or-comma-token | ',' | parenthesized-item-list .
  (labels ((skip-nil-not-eof-p ()
             (block skip
               (skip-nil skip macro-name-token line tokenized-lines)
               t))
           (arglist ()
             (loop
               :collect (parse-argument)
               :while (and (skip-nil-not-eof-p)
                           (commap (first line)))
               :do (pop line)))
           (parenthesized-item-list ()
             (let ((left  (expect macro-name-token openp line tokenized-lines))
                   (items (item-list))
                   (right (or (expect macro-name-token closep line tokenized-lines)
                              (list (make-punctuation ")" 1 1 "-")))))
               (nconc (list left) items (list right))))
           (item-list ()
             (loop
               :while (and (skip-nil-not-eof-p)
                           (not (closep (first line))))
               :if (openp (first line))
                 :append (parenthesized-item-list)
               :else
                 :collect (pop line)))
           (parse-argument-item ()
             (cond
               ((openp (first line))
                (parenthesized-item-list))
               ((commap (first line))
                '())
               (t
                (list (pop line)))))
           (parse-argument ()
             (when (skip-nil-not-eof-p)
               (if (commap (first line))
                   '()
                   (loop
                     :while (and (skip-nil-not-eof-p)
                                 (not (commap (first line)))
                                 (not (closep (first line))))
                     :nconc (parse-argument-item))))))
    (if (expect macro-name-token openp line tokenized-lines)
        (values (let ((arglist (arglist)))
                  (unless (expect macro-name-token closep line tokenized-lines)
                    (setf line nil))
                  arglist)
                line
                tokenized-lines)
        (values '() line tokenized-lines))))

(defmethod macro-expand-macros ((context context) line tokenized-lines output-lines allow-defined already-expanded)
  (loop
    :with environment = (context-environment context)
    :with out-line := '()
    :while line
    :do (flet ((definedp (identifier)
                 (make-number (if (environment-macro-definedp (context-environment context) (token-text identifier))
                                  "1" "0"))))
          (let* ((token (pop line))
                 (name  (and token (token-text token))))
            (if (identifierp token)
                (cond ((and allow-defined (string= "defined" name))
                       (let ((next (first line)))
                         (cond ((openp next)
                                (pop line)
                                (let ((name (pop line)))
                                  (if (identifierp name)
                                      (progn
                                        (if (closep (first line))
                                            (pop line)
                                            (cpp-error name "Missing a closing parenthesis after defined(~A" (token-text name)))
                                        (push (definedp name) out-line))
                                      (progn
                                        (cpp-error (or next context) "operator \"defined\" requires an identifier")
                                        (push (make-number "0") out-line)))))
                               ((identifierp next)
                                (pop line)
                                (push (definedp next) out-line))
                               (t
                                (cpp-error (or next context) "operator \"defined\" requires an identifier")
                                (push (make-number "0") out-line)))))
                      ((environment-macro-definedp environment name)
                       (let ((definition (environment-macro-definition environment name)))
                         (etypecase definition
                           (macro-definition/object
                            (if (member name already-expanded :test (function string=))
                                (push token out-line)
                                (setf out-line (nreconc (first (macro-expand-macros context (expand-macro-definition definition)
                                                                                    '() '() allow-defined
                                                                                    (cons name already-expanded)))
                                                        out-line))))
                           (macro-definition/function
                            (if (and line (openp (first line)))
                                (if (member name already-expanded :test (function string=))
                                    (push token out-line)
                                    (let (arguments)
                                      (multiple-value-setq (arguments line tokenized-lines) (parse-function-macro-call-arguments token line tokenized-lines))
                                      (setf out-line (nreconc (first (macro-expand-macros context (expand-macro-definition definition arguments)
                                                                                          '() '() allow-defined
                                                                                          (cons name already-expanded)))
                                                              out-line))))
                                (push token out-line))))))
                      (t
                       (push token out-line)))
                (push token out-line))))
    :finally (push (nreverse out-line) output-lines))
  (values output-lines tokenized-lines))

;;;; THE END ;;;;
