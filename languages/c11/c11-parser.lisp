;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               c11-parser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    C11 parser.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER")
(declaim (declaration stepper))

(defclass pre-scanned-scanner (buffered-scanner)
  ((tokens :initform '() :initarg :tokens :accessor pre-scanned-tokens)
   (actual-current :accessor pre-scanner-actual-current-token))
  (:default-initargs :source ""))

(defmethod (setf scanner-source) (new-source (scanner pre-scanned-scanner))
  new-source)

(defmethod scanner-file ((scanner pre-scanned-scanner))
  (token-file (pre-scanner-actual-current-token scanner)))

(defmethod scanner-source ((scanner pre-scanned-scanner))
  (ignore-errors (pathname (scanner-file scanner))))

(defmethod scanner-line ((scanner pre-scanned-scanner))
  (token-line (pre-scanner-actual-current-token scanner)))

(defmethod scanner-column ((scanner pre-scanned-scanner))
  (token-column (pre-scanner-actual-current-token scanner)))

(defmethod scan-next-token ((scanner pre-scanned-scanner) &optional parser-data)
  (declare (stepper disable))
  (declare (ignore parser-data))
  (let ((token (pop (pre-scanned-tokens scanner))))
    (when token
      (upgrade-c11-token token)
      #| TODO: handle [*] -> [ STAR ] |#
      ;; (format *trace-output* "~&scan-next-token -> ~A~%" token)
      (setf (pre-scanner-actual-current-token scanner) token
            (scanner-current-text  scanner) (token-text token)
            ;; result:
            (scanner-current-token scanner) (token-kind token)))))

(defmethod scanner-end-of-source-p ((scanner pre-scanned-scanner))
  (declare (stepper disable))
  (null (pre-scanned-tokens scanner)))

(defmethod scanner-end-of-line-p ((scanner pre-scanned-scanner))
  (scanner-end-of-source-p scanner))

(defmethod advance-line ((scanner pre-scanned-scanner))
  "RETURN: The new current token = old next token"
  (if (scanner-end-of-source-p scanner)
      #|End of File -- don't move.|#
      nil
      (scan-next-token scanner)))

(defmethod accept ((scanner pre-scanned-scanner) token)
  (if (word-equal token (scanner-current-token scanner))
      (prog1 (list (scanner-current-token scanner)
                   (scanner-current-text scanner)
                   (pre-scanner-actual-current-token scanner))
        (scan-next-token scanner))
      (error 'unexpected-token-error
             :file   (scanner-file scanner)
             :line   (scanner-line scanner)
             :column (scanner-column scanner)
             :scanner scanner
             :expected-token token
             :format-control "Expected ~S, not ~A (~S)"
             :format-arguments (list token
                                     (scanner-current-token scanner)
                                     (scanner-current-text scanner)))))

;;;---------------------------------------------------------------------
(declaim (declaration stepper))
(progn
  (defmethod print-object ((self (eql '\()) stream) (declare (stepper disable)) (princ "\\(" stream) self)
  (defmethod print-object ((self (eql '\))) stream) (declare (stepper disable)) (princ "\\)" stream) self)
  (defmethod print-object ((self (eql '\,)) stream) (declare (stepper disable)) (princ "\\," stream) self)
  (defmethod print-object ((self (eql '\:)) stream) (declare (stepper disable)) (princ "\\:" stream) self)
  (defmethod print-object ((self (eql '\;)) stream) (declare (stepper disable)) (princ "\\;" stream) self)
  (defmethod print-object ((self (eql '|.|)) stream) (declare (stepper disable)) (princ "\\." stream) self)
  (defmethod print-object ((self (eql '\[)) stream) (declare (stepper disable)) (princ "\\[" stream) self)
  (defmethod print-object ((self (eql '\])) stream) (declare (stepper disable)) (princ "\\]" stream) self)
  (defmethod print-object ((self (eql '\{)) stream) (declare (stepper disable)) (princ "\\{" stream) self)
  (defmethod print-object ((self (eql '\})) stream) (declare (stepper disable)) (princ "\\}" stream) self)
  (defmethod print-object ((self (eql '\|)) stream) (declare (stepper disable)) (princ "\\|" stream) self))




(defstruct (compound-literal
            :named
            (:type list)
            (:constructor make-compound-literal (type-name initializer-list)))
  type-name initializer-list)

(progn
  #1=(defgrammar c11
       ;; rdp
       :scanner nil    ; we use the pre-scanned-scanner defined above.
       :trace nil

       ;; Note: since we don't generate a scanner, the following terminals are not used,
       ;;       but they are what is expected from the cpp scanner.
       :terminals ((identifier     "identifier")
                   (typedef-name   "typedef_name")
                   (func-name      "func_name")
                   (string-literal "string_literal")
                   (i-constant     "i_constant")
                   (f-constant     "f_constant")
                   (enum-name      "enum_name")
                   (alignas        "_Alignas")
                   (alignof        "_Alignof")
                   (atomic         "_Atomic")
                   (complex        "_Complex")
                   (imaginary      "_Imaginary")
                   (generic        "_Generic")
                   (noreturn       "_Noreturn")
                   (static-assert  "_Static_assert")
                   (thread-local   "_Thread_local")
                   (auto           "auto")
                   (bool           "bool")
                   (break          "break")
                   (case           "case")
                   (char           "char")
                   (const          "const")
                   (continue       "continue")
                   (default        "default")
                   (do             "do")
                   (double         "double")
                   (else           "else")
                   (enum           "enum")
                   (extern         "extern")
                   (float          "float")
                   (for            "for")
                   (goto           "goto")
                   (if             "if")
                   (inline         "inline")
                   (int            "int")
                   (long           "long")
                   (register       "register")
                   (restrict       "restrict")
                   (return         "return")
                   (short          "short")
                   (signed         "signed")
                   (sizeof         "sizeof")
                   (static         "static")
                   (struct         "struct")
                   (switch         "switch")
                   (typedef        "typedef")
                   (union          "union")
                   (unsigned       "unsigned")
                   (void           "void")
                   (volatile       "volatile")
                   (while          "while")
                   
                   (ellipsis "...")
                   (^=       "^=")
                   (\|=      "|=")
                   (-=       "-=")
                   (<<=      "<<=")
                   (>>=      ">>=")
                   (&=       "&=")
                   (&&       "&&")
                   (|\|\||   "||")
                   (*=       "*=")
                   (/=       "/=")
                   (%=       "%=")
                   (+=       "+=")
                   (->       "->")
                   (++       "++")
                   (--       "--")
                   (<<       "<<")
                   (>>       ">>")
                   (<=       "<=")
                   (>=       ">=")
                   (==       "==")
                   (!=       "!=")
                   (\(       "(")
                   (\)       ")")
                   (\,       ",")
                   (\:       ":")
                   (\;       ";")
                   (\.       ".")
                   (\[       "[")
                   (\]       "]")
                   (\{       "{")
                   (\}       "}")
                   (\&       "&")
                   (\*       "*")
                   (\/       "/")
                   (\+       "+")
                   (\-       "-")
                   (\~       "~")
                   (\!       "!")
                   (\%       "%")
                   (\<       "<")
                   (\>       ">")
                   (\=       "=")
                   (\^       "^")
                   (\|       "|")
                   (\?       "?")
                   
                   (STAR     "*") ;; (seq [ (opt type_qualifier_list) * ])
                   )

       :start translation-unit

       :rules (

               (--> OR-ASSIGN            \|=      :action $1)
               (--> XOR-ASSIGN           ^=       :action $1)
               (--> SUB-ASSIGN           -=       :action $1)
               (--> LEFT-ASSIGN          <<=      :action $1)
               (--> RIGHT-ASSIGN         >>=      :action $1)
               (--> AND-ASSIGN           &=       :action $1)
               (--> AND-OP               &&       :action $1)
               (--> OR-OP                \|\|     :action $1)
               (--> MUL-ASSIGN           *=       :action $1)
               (--> DIV-ASSIGN           /=       :action $1)
               (--> MOD-ASSIGN           %=       :action $1)
               (--> ADD-ASSIGN           +=       :action $1)
               (--> PTR-OP               ->       :action $1)
               (--> INC-OP               ++       :action $1)
               (--> DEC-OP               --       :action $1)
               (--> LEFT-OP              <<       :action $1)
               (--> RIGHT-OP             >>       :action $1)
               (--> LE-OP                <=       :action $1)
               (--> GE-OP                >=       :action $1)
               (--> EQ-OP                ==       :action $1)
               (--> NE-OP                !=       :action $1)
               

               (--> constant
                    (alt (seq I-CONSTANT :action (token-value (third i-constant)))
                         (seq F-CONSTANT :action (read-from-string (second f-constant))))
                    :action $1)
               
               (--> string
                    (alt (seq STRING-LITERAL :action (token-value (third string-literal)))
                         FUNC-NAME)
                    :action $1)

               (--> ident
                    identifier
                    :action
                    ;; (intern (second identifier) (load-time-value (find-package "COM.INFORMATIMAGO.LANGUAGES.C11.C")) )
                    (third identifier))
               
               (--> simple-primary-expression
                    (alt ident
                         constant
                         string
                         generic-selection)
                    :action $1)

               (--> primary-expression
                    (alt simple-primary-expression
                         (seq \( expression \) :action expression))
                    :action $1)

               (--> generic-selection
                    (seq GENERIC \( assignment-expression \, generic-assoc-list \)
                         :action (list 'generic assignment-expression generic-assoc-list))
                    :action $1)

               (--> generic-assoc-list
                    (seq generic-association (rep \, generic-association :action $2)
                         :action (cons $1 $2))
                    :action $1)

               (--> generic-association
                    (alt (seq type-name \: assignment-expression :action (list type-name assignment-expression))
                         (seq DEFAULT   \: assignment-expression :action (list 'default  assignment-expression)))
                    :action $1)

               ;; postfix is left-to-right:

               (--> postfix-expression
                    (seq postfix-expression-head (rep postfix-expression-item :action $1)
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> postfix-expression-head
                    (alt simple-primary-expression
                         (seq \( (alt (seq expression \) ; = primary-expression
                                           :action expression)
                                      (seq type-name  \) { initializer-list (opt \,)}
                                           :action `(compound-literal ,type-name ,initializer-list))
                                      (seq \) :action nil #|TODO: WHAT IS THIS?|#))
                              :action $2))
                    :action $1)
               
               (--> postfix-expression-item
                    (alt (seq [ expression ]                       :action `(aref ,expression))
                         (seq \( (opt argument-expression-list) \) :action `(call ,(first $2)))
                         (seq |.| ident                            :action `(dot ,ident))
                         (seq PTR-OP ident                         :action `(ptr-op ,ident))
                         (seq INC-OP                               :action '(post-increment))
                         (seq DEC-OP                               :action '(post-decrement)))
                    :action $1)


               (--> argument-expression-list
                    (seq assignment-expression (rep \, assignment-expression :action $2)
                         :action (cons $1 $2))
                    :action $1)


               ;; unary is right-to-left:

               (--> simple-unary-expression
                    (alt (seq INC-OP unary-expression          :action `(pre-increment ,unary-expression))
                         (seq DEC-OP unary-expression          :action `(pre-decrement ,unary-expression))
                         (seq unary-operator cast-expression   :action `(,unary-operator ,cast-expression))
                         (seq SIZEOF sizeof-argument           :action `(size-of  ,sizeof-argument))
                         (seq ALIGNOF \( type-name \)          :action `(align-of ,type-name)))
                    :action $1)

               (--> unary-operator
                    (alt & * + - ~ !)
                    :action (first $1))

               (--> sizeof-argument
                    (alt simple-unary-expression
                         (seq (alt simple-primary-expression
                                   (seq \( (alt (seq expression \)
                                                     :action expression)
                                                (seq type-name  \) (opt { initializer-list (opt \,) }
                                                                        :action initializer-list)
                                                     :action `(compound-literal ,type-name ,(first $3)))
                                                (seq \) :action nil #|TODO: WHAT IS THIS?|#))
                                        :action $2))
                              (rep postfix-expression-item)
                              :action (wrap-left-to-right $1 $2))))

               (--> unary-expression
                    (alt postfix-expression
                         simple-unary-expression)
                    :action `(unary ,$1))
               
               (--> cast-expression
                    (alt (seq simple-unary-expression
                              :action `(unary ,$1))
                         (seq simple-primary-expression (rep postfix-expression-item :action $1)
                              :action `(unary ,(wrap-left-to-right `(unary ,$1) $2)))
                         (seq \( (alt (seq expression \) (rep postfix-expression-item :action $1)
                                           :action `(unary ,(wrap-left-to-right expression $3)))
                                      (seq type-name \) (alt (seq  { initializer-list (opt \,) } (rep postfix-expression-item :action $1)
                                                                   :action `(compound-literal ,initializer-list ,$5))
                                                             (seq cast-expression
                                                                  :action `(cast ,cast-expression)))
                                           :action (if (and (eq 'compound-literal (first $3))
                                                            (third $3))
                                                       (wrap-left-to-right `(,(first $3) ,type-name ,(second $3)) (third $3))
                                                       `(,(first $3) ,type-name ,@(rest $3))))
                                      ;; (seq \) :action '|()| #|WHAT IS THIS?|#)
                                      )
                              :action $2))
                    :action $1)

               ;; left-to-right:

               (--> multiplicative-expression
                    (seq cast-expression (rep (alt * / %) cast-expression :action (list (first $1) $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> additive-expression
                    (seq multiplicative-expression (rep (alt + -) multiplicative-expression :action (list (first $1) $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> shift-expression
                    (seq additive-expression (rep (alt LEFT-OP RIGHT-OP) additive-expression :action (list (first $1) $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> relational-expression
                    (seq shift-expression (rep (alt < > LE-OP GE-OP) shift-expression :action (list (first $1) $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> equality-expression
                    (seq relational-expression (rep (alt EQ-OP NE-OP) relational-expression :action (list (first $1) $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> and-expression
                    (seq equality-expression (rep & equality-expression :action `(& ,$2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> exclusive-or-expression
                    (seq and-expression (rep ^ and-expression :action `(^ ,$2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> inclusive-or-expression
                    (seq exclusive-or-expression (rep \| exclusive-or-expression :action `(\| ,$2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> logical-and-expression
                    (seq inclusive-or-expression (rep AND-OP inclusive-or-expression :action `(&& ,$2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> logical-or-expression
                    (seq logical-and-expression (rep OR-OP logical-and-expression :action `(\|\| ,$2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               ;; ternary if is right-to-left:

               (--> conditional-expression
                    (seq logical-or-expression (opt ? expression \: conditional-expression
                                                    :action (list expression conditional-expression))
                         :action (if $2 `(if ,$1 ,@$2) $1))
                    :action $1)

               ;; right-to-left:

               #-(and)
               (--> assignment-expression
                    (alt conditional-expression
                         (seq unary-expression assignment-operator assignment-expression)))

               (--> assignment-expression
                    conditional-expression
                    (opt assignment-operator assignment-expression :action (list $1 $2))
                    :action (if $2
                                (progn
                                  (check-unary $1)
                                  `(,(first (first $2)) ,(second $1) ,@(rest (first $2))))
                                $1))

               (--> assignment-operator
                    (alt (seq = :action '(setf))
                         MUL-ASSIGN DIV-ASSIGN MOD-ASSIGN ADD-ASSIGN SUB-ASSIGN LEFT-ASSIGN RIGHT-ASSIGN AND-ASSIGN
                         XOR-ASSIGN OR-ASSIGN)
                    :action (first $1))

               ;; comma is left-to-right:

               (--> expression
                    (seq assignment-expression (rep \, assignment-expression :action $2)
                         :action (if $2
                                     `(progn ,assignment-expression ,@$2)
                                     assignment-expression))
                    :action $1)

               (--> constant-expression
                    conditional-expression
                    :action (progn (check-constant-expression conditional-expression)
                                   conditional-expression))

               ;; ---

               (--> alignment-specifier
                    ALIGNAS \( (alt (seq type-name           :action `(type ,type-name))
                                    (seq constant-expression :action `(size ,constant-expression))) \)
                    :action `(alignment-specifier ,$3))

               (--> function-specifier
                    (alt (seq INLINE   :action 'inline)
                         (seq NORETURN :action 'noreturn))
                    :action `(function-specifier ,$1))

               (--> storage-class-specifier
                    (alt (seq TYPEDEF      :action 'typedef)
                         (seq EXTERN       :action 'extern)
                         (seq STATIC       :action 'static)
                         (seq THREAD-LOCAL :action 'thread-load)
                         (seq AUTO         :action 'auto)
                         (seq REGISTER     :action 'register))
                    :action `(storage-class-specifier ,$1))

               (--> type-qualifier
                    (alt simple-type-qualifier
                         (seq ATOMIC :action '(type-qualifier atomic)))
                    :action $1)

               (--> simple-type-qualifier
                    (alt (seq CONST    :action 'const)
                         (seq RESTRICT :action 'restrict)
                         (seq VOLATILE :action 'volatile))
                    :action `(type-qualifier ,$1))

               (--> type-specifier
                    (alt simple-type-specifier
                         atomic-type-specifier)
                    :action $1)

               (--> atomic-type-specifier
                    ATOMIC \( type-name \)
                    :action `(type-specifier (type-qualifier atomic) ,$3))

               (--> simple-type-specifier
                    (alt (seq VOID      :action 'void)
                         (seq CHAR      :action 'char)
                         (seq SHORT     :action 'short)
                         (seq INT       :action 'int)
                         (seq LONG      :action 'long)
                         (seq FLOAT     :action 'float)
                         (seq DOUBLE    :action 'double)
                         (seq SIGNED    :action 'signed)
                         (seq UNSIGNED  :action 'unsigned)
                         (seq BOOL      :action 'bool)
                         (seq COMPLEX   :action 'complex)
                         (seq IMAGINARY :action 'imaginary)
                         struct-or-union-specifier
                         enum-specifier
                         TYPEDEF-NAME)
                    :action `(type-specifier ,$1))

               (--> specifier-qualifier
                    (alt (seq ATOMIC (opt \( type-name \) :action type-name)
                              :action (if $2
                                          `(type-specifier (type-qualifier atomic) ,@$2)
                                          `(type-qualifier atomic)))
                         simple-type-qualifier
                         simple-type-specifier)
                    :action $1)

               (--> declaration-specifier
                    (alt alignment-specifier
                         function-specifier
                         storage-class-specifier
                         specifier-qualifier)
                    :action $1)

               (--> declaration-specifiers
                    (seq declaration-specifier (rep declaration-specifier :action $1)
                         :action (cons $1 $2))
                    :action (classify-declaration-specifiers $1))

               ;; ---

               (--> init-declarator-list
                    init-declarator (rep \, init-declarator :action init-declarator)
                    :action (cons init-declarator $2))

               (--> init-declarator
                    declarator (opt = initializer :action initializer)
                    :action `(declarator ,declarator ,@$2))

               ;; ---

               (--> struct-or-union-specifier
                    struct-or-union (alt (seq { struct-declaration-list }
                                              :action (cons nil struct-declaration-list))
                                         (seq ident (opt { struct-declaration-list }
                                                         :action struct-declaration-list)
                                              :action (cons ident (first $2))))
                    :action (cons (first struct-or-union) $2))

               (--> struct-or-union
                    (alt STRUCT UNION)
                    :action $1)

               (--> struct-declaration-list
                    (seq struct-declaration (rep struct-declaration :action $1)
                         :action (cons $1 $2))
                    :action $1)

               (--> struct-declaration
                    (alt (seq specifier-qualifier-list (opt struct-declarator-list :action struct-declarator-list) \;
                              :action `(struct-declaration ,specifier-qualifier-list ,@$2))
                         static-assert-declaration)
                    :action $1)

               (--> specifier-qualifier-list
                    specifier-qualifier (rep specifier-qualifier :action $1)
                    :action (cons $1 $2))

               (--> struct-declarator-list
                    struct-declarator (rep \, struct-declarator :action $2)
                    :action (cons $1 $2))

               (--> struct-declarator
                    (alt (seq \: constant-expression
                              :action (list 'struct-declarator nil constant-expression))
                         (seq declarator (opt \: constant-expression :action constant-expression)
                              :action `(struct-declarator ,declarator ,@$2)))
                    :action $1)

               (--> enum-specifier
                    ENUM (alt (seq { enumerator-list (opt \,) }
                                   :action `(enum nil ,@enumerator-list) )
                              (seq ident (opt { enumerator-list (opt \,) }
                                              :action enumerator-list)
                                   :action `(enum ,ident ,@$2)))
                    :action $2)

               (--> enumerator-list
                    enumerator (rep \, enumerator :action enumerator)
                    :action (cons $1 $2))

               (--> enumeration-constant
                    ident :action ident)

               (--> enumerator
                    enumeration-constant (opt = constant-expression :action constant-expression)
                    :action (if $2
                                `(,enumeration-constant ,@$2)
                                enumeration-constant))

               ;; ---

               (--> declarator
                    (alt (seq pointer direct-declarator
                              :action (wrap-pointers direct-declarator pointer))
                         (seq direct-declarator
                              :action $1))
                    :action $1)

               (--> direct-declarator
                    (seq simple-direct-declarator (rep direct-declarator-item :action $1)
                         :action (wrap-declarator simple-direct-declarator $2))
                    :action $1)

               (--> simple-direct-declarator
                    (alt (seq ident             :action (register-declarator ident))
                         (seq \( declarator \)  :action declarator))
                    :action $1)

               (--> direct-declarator-item
                    (alt (seq \( (opt direct-declarator-in-parentheses
                                      :action direct-declarator-in-parentheses)
                              \)
                              :action `(parameters ,@$2))
                         (seq \[ (alt (seq (opt STAR)
                                           :action `(array nil ,(if $1 '* nil)))
                                      (seq direct-declarator-in-brackets
                                           :action direct-declarator-in-brackets))
                              \]
                              :action $2))
                    :action $1)

               (--> direct-declarator-in-parentheses
                    (alt (seq identifier-list     :action identifier-list)
                         (seq parameter-type-list :action parameter-type-list))
                    :action $1)

               (--> direct-declarator-in-brackets
                    (alt (seq assignment-expression
                              :action `(array nil ,assignment-expression))
                         (seq STATIC (opt type-qualifier-list :action type-qualifier-list) assignment-expression
                              :action `(array ,(cons 'static $2) ,assignment-expression))
                         (seq type-qualifier-list (opt (alt ;; * ;; TODO
                                                            (seq (opt STATIC :action 'static) assignment-expression
                                                                 :action (list $1 $2)))
                                                       :action $1)
                              :action `(array ,(append type-qualifier-list (first $2)) ,(second $2))))
                    :action $1)

               ;; ---
               
               (--> parameter-type-list
                    parameter-list (opt \, ELLIPSIS)
                    :action (if $2
                                (append parameter-list '(ellipsis))
                                parameter-list))

               (--> parameter-list
                    parameter-declaration (rep \, parameter-declaration :action parameter-declaration)
                    :action (cons parameter-declaration $2))

               (--> parameter-declaration
                    (seq declaration-specifiers
                         :action (push-declaration-specifiers declaration-specifiers))
                    (opt declarator--or--abstract-declarator
                         :action declarator--or--abstract-declarator)
                    :action (prog1 (if $2
                                       `(parameter ,$1 ,@$2)
                                       `(parameter ,$1))
                              (pop-declaration-specifiers)))

               (--> identifier-list
                    ident (rep \, ident :action ident)
                    :action (cons ident $2))

               (--> type-qualifier-list
                    type-qualifier (rep type-qualifier :action type-qualifier)
                    :action `(type-qualifier ,@(cons type-qualifier $2)))

               (--> declarator--or--abstract-declarator
                    (alt direct-declarator--or--direct-abstract-declarator
                         (seq pointer (opt direct-declarator--or--direct-abstract-declarator
                                           :action direct-declarator--or--direct-abstract-declarator)
                              :action (wrap-pointers (first $2) pointer)))
                    :action $1)

               (--> direct-declarator--or--direct-abstract-declarator
                    simple-direct-declarator--or--simple-direct-abstract-declarator
                    (rep direct-declarator-item--or--direct-abstract-declarator-item :action $1)
                    :action (cons $1 $2))


               (--> simple-direct-declarator--or--simple-direct-abstract-declarator
                    (alt (seq ident :action (register-declarator ident))
                         (seq \( (opt (alt (seq declarator--or--abstract-declarator
                                                (rep \, ident)
                                                :action (progn #|check declarator is ident
                                                          if we have rep ident.|#))
                                           (seq parameter-type-list))
                                      :action $1)
                              \))
                         bracket-direct-abstract-declarator)
                    :action $1)

               (--> direct-declarator-item--or--direct-abstract-declarator-item
                    (alt (seq \( (opt direct-declarator-in-parentheses
                                      :action direct-declarator-in-parentheses) \)
                              :action `(parameters ,@$2))
                         bracket-direct-abstract-declarator)
                    :action $1)

               (--> bracket-direct-abstract-declarator
                    \[
                    (opt (alt (seq STAR)
                              (seq direct-declarator-in-brackets
                                   :action (progn #| check no [*] |#)))
                         :action `(aref ,$1))
                    \]
                    :action $2)

               (--> pointer
                    *
                    (opt type-qualifier-list
                         :action type-qualifier-list)
                    (rep * (opt type-qualifier-list :action $1)
                         :action (first $2))
                    :action (cons $2 $3))

               (--> type-name
                    specifier-qualifier-list (opt abstract-declarator)
                    :action `(type-name ,specifier-qualifier-list ,@$2))

               (--> abstract-declarator
                    (alt (seq pointer (opt direct-abstract-declarator)
                              :action (wrap-pointers $2 pointer))
                         (seq direct-abstract-declarator
                              :action $1))
                    :action $1)

               (--> direct-abstract-declarator
                    simple-direct-abstract-declarator
                    (rep direct-abstract-declarator-item)
                    :action (cons $1 $2))

               (--> simple-direct-abstract-declarator
                    (alt (seq \( (opt direct-abstract-declarator-in-parentheses) \))
                         bracket-direct-abstract-declarator)
                    :action $1)

               (--> direct-abstract-declarator-in-parentheses
                    (alt (seq abstract-declarator)
                         (seq parameter-type-list))
                    :action $1)

               (--> direct-abstract-declarator-item
                    (alt (seq \( (opt parameter-type-list) \))
                         bracket-direct-abstract-declarator)
                    :action $1)


               (--> initializer
                    (alt (seq { initializer-list (opt \,) }
                              :action `(compound-literal nil ,initializer-list))
                         assignment-expression)
                    :action $1)

               (--> initializer-list
                    initializer-item (rep \, initializer-item  :action initializer-item)
                    :action (cons initializer-item $2))
               
               (--> initializer-item
                    (alt (seq designation initializer
                              :action `(designation ,designation ,initializer))
                         (seq initializer
                              :action initializer))
                    :action $1)

               (--> designation
                    designator-list =
                    :action designator-list)

               (--> designator-list
                    designator (rep designator :action designator)
                    :action (cons designator $2))

               (--> designator
                    (alt (seq \[ constant-expression \]
                              :action `(aref ,constant-expression))
                         (seq |.| ident
                              :action `(dref ,ident)))
                    :action $1)

               (--> static-assert-declaration
                    STATIC-ASSERT \( constant-expression \, STRING-LITERAL \) \;
                    :action `(static-assert ,constant-expression ,string-literal))

               ;; ---
               
               (--> statement
                    (alt simple-labeled-statement
                         expression-statement-or-label
                         compound-statement
                         selection-statement
                         iteration-statement
                         jump-statement)
                    :action $1)

               (--> expression-statement-or-label
                    (alt \;
                         (seq expression (alt (seq \; :action 'expression-statement) ; expression-statement
                                              (seq \: statement :action statement))  ; label
                              :action (if (eq $2 'expression-statement)
                                          expression
                                          `(label ,expression
                                             ,$2))))
                    :action $1)

               (--> expression-statement
                    (alt (seq \;            :action '(progn))
                         (seq expression \; :action expression))
                    :action $1)

               (--> simple-labeled-statement
                    (alt (seq CASE constant-expression \: statement :action `(label ,constant-expression ,statement))
                         (seq DEFAULT                  \: statement :action `(label :default             ,statement)))
                    :action $1)

               (--> compound-statement
                    (seq { (opt block-item-list :action block-item-list) }
                         :action `(tagbody ,@$2))
                    :action $1)

               (--> block-item-list
                    (seq block-item (rep block-item :action block-item)
                         :action (cons block-item $2))
                    :action $1)

               (--> block-item
                    (alt declaration
                         statement)
                    :action $1)

               (--> selection-statement
                    (alt (seq IF \( expression \) statement (opt ELSE statement :action statement)
                              :action `(if ,expression ,statement ,@$6))
                         (seq SWITCH \( expression \) statement
                              :action `(switch ,expression ,statement)))
                    :action $1)

               (--> iteration-statement
                    (alt (seq WHILE \( expression \) statement
                              :action `(while ,expression ,statement))
                         (seq DO statement WHILE \( expression \) \;
                              :action `(do-while ,statement ,expression))
                         (seq FOR \( (alt (seq expression-statement  expression-statement (opt expression) \) statement
                                               :action `(for ,expression-statement.1 ,expression-statement.2 ,@$3 ,statement))
                                          (seq declaration           expression-statement (opt expression) \) statement
                                               :action `(for ,declaration ,expression-statement ,@$3 ,statement)))
                              :action $3))
                    :action $1)

               (--> jump-statement
                    (alt (seq GOTO ident \;              :action `(go ,ident))
                         (seq CONTINUE \;                :action `(continue))
                         (seq BREAK \;                   :action `(break))
                         (seq RETURN (opt expression) \; :action `(return ,@$2)))
                    :action $1)

               ;; ---

               (--> translation-unit
                    external-declaration (rep external-declaration :action external-declaration)
                    :action `(translation-unit ,$1 ,@$2))

               (--> external-declaration
                    (alt (seq static-assert-declaration
                              :action `(external-declaration ,$1))

                         (seq (seq declaration-specifiers
                                   :action (push-declaration-specifiers declaration-specifiers))
                              (alt (seq \; :action '())
                                   (seq declarator
                                        (alt (seq (opt =  initializer     :action $2)
                                                  (rep \, init-declarator :action $2)
                                                  \;
                                                  :action `(:initializer ,(first $1) ,$2))
                                             (seq (rep declaration) compound-statement
                                                  ;; declaration-list are K&R parameters.
                                                  :action `(:function-declarator ,$1 ,$2)))
                                        :action (ecase (first $2)
                                                  (:initializer
                                                   (cons `(declarator ,declarator ,@(second $2))
                                                         (third $2)))
                                                  (:function-declarator
                                                   `((function-declarator ,declarator ,(second $2) ,(third  $2)))))))
                              :action (let ((declaration-specifiers (pop-declaration-specifiers))
                                            (declarators            $2))
                                        `(external-declaration ,declaration-specifiers ,@declarators))))
                    :action $1)

               (--> declaration
                    (alt  (seq static-assert-declaration
                               :action `(external-declaration ,$1))
                          
                          (seq (seq declaration-specifiers
                                    :action (push-declaration-specifiers declaration-specifiers))
                               (opt init-declarator-list)
                               \;
                               :action (let ((declaration-specifiers (pop-declaration-specifiers))
                                             (init-declarator-list   (first $2)))
                                         `(declaration ,declaration-specifiers ,@init-declarator-list))))
                    :action $1)))
  
  (defparameter *c* '#1#))



(defun classify-declaration-specifiers (specifiers)
  (let (alignment-specifiers
        function-specifiers
        storage-class-specifiers
        type-qualifiers
        type-specifiers)
    (loop
      :for specifier :in specifiers
      :do (ecase (first specifier)
            (alignment-specifier     (push (second specifier) alignment-specifiers))
            (function-specifier      (push (second specifier) function-specifiers))
            (storage-class-specifier (push (second specifier) storage-class-specifiers))
            (type-qualifier          (push (second specifier) type-qualifiers))
            (type-specifier          (push (second specifier) type-specifiers))))
    (append (when storage-class-specifiers
              `((storage-class-specifier ,@storage-class-specifiers)))
            (when function-specifiers
              `((function-specifier ,@function-specifiers)))
            (when alignment-specifiers
              `((alignment-specifier ,@alignment-specifiers)))
            (when type-qualifiers
              `((type-qualifier ,@type-qualifiers)))
            (when type-specifiers
              `((type-specifier ,@type-specifiers)))
            '())))


(defun push-declaration-specifiers (specifiers)
  "
DO:     Push the specifiers onto the (context-declaration-specifiers *context*) stack.
RETURN: specifiers
NOTE:   if the specifiers is a typedef, then pushes above it :typedef.
"
  (push specifiers (context-declaration-specifiers *context*))
  (when (member  'typedef (find 'storage-class-specifier specifiers :key (function first)))
    (push :typedef (context-declaration-specifiers *context*)))
  ;; (print `(specifiers --> ,specifiers)) (terpri)
  specifiers)

(defun pop-declaration-specifiers ()
  "
DO:     Pops (context-declaration-specifiers *context*) stack.
RETURN: The old top of stack specifier.
NOTE:   if the top-of-stack is :typedef then pop it as well as the specifiers.
"
  (let ((top  (pop (context-declaration-specifiers *context*))))
    (if (eq :typedef top)
        (pop (context-declaration-specifiers *context*))
        top)))

(defun unwrap-declarator (type declarator)
  ;; (print `(unwrap-declarator ,type ,declarator))
  (loop
    :while (listp declarator)
    :do (case (first declarator)
          (pointer  (setf type `(pointer ,(second declarator) ,type)
                          declarator (third declarator)))
          (array    (setf type `(array ,(second declarator) ,(third declarator) ,type)
                          declarator (fourth declarator)))
          (function (setf type `(function ,(second declarator) ,type)
                          declarator (third declarator)))
          (otherwise (loop-finish)))
    :finally (return (values declarator type))))

(defun declarator-name (declarator)
  ;; unwrap identifier:
  (unwrap-declarator nil declarator))

(defun register-declarator (declarator)
  (let ((name        (declarator-name declarator))
        (kind        (first  (context-declaration-specifiers *context*)))
        (declaration (second (context-declaration-specifiers *context*))))
    (case kind
      (:typedef   (enter-typedef              *context* name declaration))
      (:enum      (enter-enumeration-constant *context* name declaration))
      (:function  (enter-function             *context* name declaration))))
  declarator)


(defun check-constant-expression (expression)
  (declare (ignore expression))
  #|TODO|#
  (values))

(defun check-unary (expression)
  (unless (and (listp expression)
               (eq 'unary (first expression)))
    (error "Expected an unary expression as left-hand-side of an assignment, instead of ~S"
           expression)))

(defun wrap-left-to-right (expression partial-expressions)
  (loop
    :for (op . arguments) :in partial-expressions
    :do (setf expression `(,op ,expression ,@arguments))
    :finally (return expression)))


(defun wrap-pointers (declarator type-attributes)
  (loop
    :for type-attribute :in (reverse type-attributes)
    :do (setf declarator `(pointer ,type-attribute ,declarator))
    :finally (return declarator)))

(defun wrap-declarator (declarator items) 
  ;; (print `(wrap-declarator ,declarator ,items))
  (loop
    :for item :in items
    :do (setf declarator (ecase (first item)
                           (parameters `(function ,declarator ,item))
                           (array      `(array    ,declarator ,@(rest item)))))
    :finally (return declarator)))


;; (wrap-pointers 'a '(() (const) (volatile const)))
;; (pointer nil (pointer (const) (pointer (volatile const) a)))


(defun ^cons (a b)
  (cons a (reduce (function append) b)))

(defun flatten-repeat (list)
  (reduce (function append) list))

(defun remove-unary (tree)
  (cond
    ((atom tree)
     tree)
    ((eql 'unary (first tree))
     (remove-unary (second tree)))
    (t 
     (mapcar (function remove-unary) tree))))


;; NOT. Instead we return a parse tree close enough to the C syntax,
;; and will let post-processing deal with this:
;;
;; const int *ptc;      (const int) ((pointer) ptc) -> (declarator ((pointer) (const int)) ptc)
;; int* const cp;       (int) ((pointer const) cp)  -> (declarator ((pointer const) (int)) cp)
;; typedef int *ip;     (int) ((pointer) ip)        -> (typedef    ((pointer) (int))       ip)
;; const ip cp;         (const ip)  cp              -> (declarator (const ip)              cp)
;;
;; int      *a   [2]  [3]       (int) (pointer (array 3 (array 2 a))) --> (declarator  (array 2 (array 3 (pointer int))) a)
;; a = array [2] of array [3] of pointer to int


;;;; THE END ;;;;
