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
(in-package "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER")


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
    (upgrade-c11-token token)
    #| TODO: handle [*] -> [ STAR ] |#
    (format *trace-output* "~&scan-next-token -> ~A~%" token)
    (setf (pre-scanner-actual-current-token scanner) token
          (scanner-current-text  scanner) (token-text token)
          ;; result:
          (scanner-current-token scanner) (token-kind token))))

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


(progn
  #1=(defgrammar c11
       ;; rdp
       :scanner nil    ; we use the pre-scanned-scanner defined above.
       :trace t

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

               (--> XOR-ASSIGN     (seq ^=     :action $1) :action $1)
               (--> OR-ASSIGN      (seq \|=    :action $1) :action $1)
               (--> SUB-ASSIGN     (seq -=     :action $1) :action $1)
               (--> LEFT-ASSIGN    (seq <<=    :action $1) :action $1)
               (--> RIGHT-ASSIGN   (seq >>=    :action $1) :action $1)
               (--> AND-ASSIGN     (seq &=     :action $1) :action $1)
               (--> AND-OP         (seq &&     :action $1) :action $1)
               (--> OR-OP          (seq |\|\|| :action $1) :action $1)
               (--> MUL-ASSIGN     (seq *=     :action $1) :action $1)
               (--> DIV-ASSIGN     (seq /=     :action $1) :action $1)
               (--> MOD-ASSIGN     (seq %=     :action $1) :action $1)
               (--> ADD-ASSIGN     (seq +=     :action $1) :action $1)
               (--> PTR-OP         (seq ->     :action $1) :action $1)
               (--> INC-OP         (seq ++     :action $1) :action $1)
               (--> DEC-OP         (seq --     :action $1) :action $1)
               (--> LEFT-OP        (seq <<     :action $1) :action $1)
               (--> RIGHT-OP       (seq >>     :action $1) :action $1)
               (--> LE-OP          (seq <=     :action $1) :action $1)
               (--> GE-OP          (seq >=     :action $1) :action $1)
               (--> EQ-OP          (seq ==     :action $1) :action $1)
               (--> NE-OP          (seq !=     :action $1) :action $1)


               (--> constant       (alt I-CONSTANT     F-CONSTANT) :action $1)
               (--> string         (alt STRING-LITERAL FUNC-NAME)  :action $1)


               (--> simple-primary-expression
                    (alt IDENTIFIER
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
                         (seq \( (opt argument-expression-list) \) :action `(call ,$2))
                         (seq |.| IDENTIFIER                       :action `(dot ,identifier))
                         (seq PTR-OP IDENTIFIER                    :action `(ptr-op ,identifier))
                         (seq INC-OP                               :action '(post-increment))
                         (seq DEC-OP                               :action '(post-decrement)))
                    :action $1)


               (--> argument-expression-list
                    (seq assignment-expression (rep \, assignment-expression :action $2)
                         :action (cons $1 $2))
                    :action $1)


               ;; unary is right-to-left:

               (--> simple-unary-expression
                    (alt (seq INC-OP unary-expression          :action `(,inc-op ,unary-expression))
                         (seq DEC-OP unary-expression          :action `(,dec-op ,unary-expression))
                         (seq unary-operator cast-expression   :action `(,unary-operator ,cast-expression))
                         (seq SIZEOF sizeof-argument           :action `(size-of  ,sizeof-argument))
                         (seq ALIGNOF \( type-name \)          :action `(align-of ,type-name)))
                    :action $1)

               (--> sizeof-argument
                    (alt simple-unary-expression
                         (seq (alt simple-primary-expression
                                   (seq \( (alt (seq expression \)
                                                     :action expression)
                                                (seq type-name  \) (opt { initializer-list (opt \,) }
                                                                        :action initializer-list)
                                                     :action `(compound-literal ,type-name ,$3) )
                                                (seq \) :action nil #|TODO: WHAT IS THIS?|#))))
                              (rep postfix-expression-item)
                              :action (wrap-left-to-right $1 $2))))

               (--> unary-expression
                    (alt postfix-expression
                         simple-unary-expression)
                    :action `(unary ,$1))

               (--> unary-operator
                    (alt & * + - ~ !)
                    :action $1)

               (--> cast-expression
                    (alt (seq simple-unary-expression    :action `(unary ,$1))
                         (seq simple-primary-expression  :action `(unary ,$1))
                         (seq \( (alt (seq expression \)
                                           :action expression)
                                      (seq type-name  \)
                                           (alt (seq  { initializer-list (opt \,) }
                                                      :action `(compound-literal ,initializer-list))
                                                (seq cast-expression
                                                     :action `(cast ,cast-expression)))
                                           :action `(,(first $2) ,type-name ,@(rest $2)))
                                      (seq \) :action nil #|TODO: WHAT IS THIS?|#))))
                    :action $1)

               ;; left-to-right:

               (--> multiplicative-expression
                    (seq cast-expression (rep (alt * / %) cast-expression :action (list $1 $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> additive-expression
                    (seq multiplicative-expression (rep (alt + -) multiplicative-expression :action (list $1 $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> shift-expression
                    (seq additive-expression (rep (alt LEFT-OP RIGHT-OP) additive-expression :action (list $1 $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> relational-expression
                    (seq shift-expression (rep (alt < > LE-OP GE-OP) shift-expression :action (list $1 $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> equality-expression
                    (seq relational-expression (rep (alt EQ-OP NE-OP) relational-expression :action (list $1 $2))
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> and-expression
                    (seq equality-expression (rep & equality-expression :action $2)
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> exclusive-or-expression
                    (seq and-expression (rep ^ and-expression :action $2)
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> inclusive-or-expression
                    (seq exclusive-or-expression (rep \| exclusive-or-expression :action $2)
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> logical-and-expression
                    (seq inclusive-or-expression (rep AND-OP inclusive-or-expression :action $2)
                         :action (wrap-left-to-right $1 $2))
                    :action $1)

               (--> logical-or-expression
                    (seq logical-and-expression (rep OR-OP logical-and-expression :action $2)
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
                    (seq conditional-expression
                         (opt (seq assignment-operator assignment-expression :action (list $1 $2)))
                         :action   (if $2
                                       (progn
                                         (check-unary $1)
                                         `(,(first $2) $1 ,@(rest $2)))
                                       $1))
                    :action (progn (print `(assignment-expression -> ,$1))
                                   $1))

               (--> assignment-operator
                    (alt (seq = :action 'setf)
                         MUL-ASSIGN DIV-ASSIGN MOD-ASSIGN ADD-ASSIGN SUB-ASSIGN LEFT-ASSIGN RIGHT-ASSIGN AND-ASSIGN
                         XOR-ASSIGN OR-ASSIGN))

               ;; comma is left-to-right:

               (--> expression
                    (seq assignment-expression (rep \, assignment-expression :action $2)
                         :action `(progn ,$1 ,@$2))
                    :action $1)

               (--> constant-expression
                    conditional-expression
                    :action (progn (check-constant-expression conditional-expression)
                                   conditional-expression))

               ;; ---

               (--> alignment-specifier
                    (seq ALIGNAS \( (opt type-name constant-expression) \)))

               (--> function-specifier
                    (alt (seq INLINE   :action 'inline)
                         (seq NORETURN :action 'noreturn))
                    :action $1)

               (--> storage-class-specifier
                    (alt (seq TYPEDEF      :action 'typedef)
                         (seq EXTERN       :action 'extern)
                         (seq STATIC       :action 'static)
                         (seq THREAD-LOCAL :action 'thread-load)
                         (seq AUTO         :action 'auto)
                         (seq REGISTER     :action 'register))
                    :action $1)

               (--> type-qualifier
                    (alt simple-type-qualifier
                         (seq ATOMIC :action '(:type-qualifier :atomic)))
                    :action $1)

               (--> simple-type-qualifier
                    (seq (alt (seq CONST    :action 'const)
                              (seq RESTRICT :action 'restrict)
                              (seq VOLATILE :action 'volatile))
                         :action (list :type-qualifier $1))
                    :action $1)

               (--> type-specifier
                    (alt simple-type-specifier
                         atomic-type-specifier)
                    :action $1)

               (--> atomic-type-specifier
                    (seq ATOMIC \( type-name \) :action (list :type-specifier $1 $3))
                    :action $1)

               (--> simple-type-specifier
                    (seq (alt (seq VOID      :action 'void)
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
                         :action (list :type-specifier $1))
                    :action $1)

               (--> specifier-qualifier
                    (alt (seq ATOMIC (opt \( type-name \) :action $2)
                              :action (if $2
                                          (list :type-specifier $1 $2)
                                          (list :type-qualifier $1)))
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
                    :action $1)

               (--> init-declarator-list
                    (seq init-declarator (rep \, init-declarator :action $2)
                         :action (cons $1 $2))
                    :action $1)

               (--> init-declarator
                    (seq declarator
                         (opt = initializer :action $2)
                         :action (declarator $1 $2))
                    :action $1)

               (--> struct-or-union-specifier
                    (seq struct-or-union (alt (seq { struct-declaration-list }
                                                   :action (list nil struct-declaration-list))
                                              (seq IDENTIFIER (opt { struct-declaration-list }
                                                                   :action struct-declaration-list)
                                                   :action (list $1 $2)))
                         :action (cons (first struct-or-union) $2)))

               (--> struct-or-union
                    (alt STRUCT UNION)
                    :action $1)

               (--> struct-declaration-list
                    (seq struct-declaration (rep struct-declaration :action $1)
                         :action (cons $1 $2))
                    :action $1)

               (--> struct-declaration
                    (alt (seq specifier-qualifier-list (opt struct-declarator-list) \;
                              :action )
                         static-assert-declaration)
                    :action $1)

               (--> specifier-qualifier-list
                    (seq specifier-qualifier (rep specifier-qualifier :action $1) :action (cons $1 $2)))

               (--> struct-declarator-list
                    (seq struct-declarator (rep \, struct-declarator :action $2)
                         :action (cons $1 $2))
                    :action $1)

               (--> struct-declarator
                    (alt (seq \: constant-expression
                              :action (list 'struct-declarator nil $2))
                         (seq declarator (opt \: constant-expression :action $2)
                              :action (list 'struct-declarator $1 $2)))
                    :action $1)

               (--> enum-specifier
                    ENUM (alt (seq { enumerator-list (opt \,)})
                              (seq IDENTIFIER (opt { enumerator-list (opt \,)}))))

               (--> enumerator-list
                    (seq enumerator (rep \, enumerator :action $2) :action (cons $1 $2)))

               (--> enumeration-constant
                    IDENTIFIER)

               (--> enumerator
                    (seq enumeration-constant (opt = constant-expression)))




               (--> declarator
                    (alt (seq pointer direct-declarator
                              :action (wrap-pointers $2 $1))
                         (seq direct-declarator
                              :action $1))
                    :action $1)

               ;; const int *ptc;      (const int) ((pointer) ptc) -> (declarator ((pointer) (const int)) ptc)
               ;; int* const cp;       (int) ((pointer const) cp)  -> (declarator ((pointer const) (int)) cp)
               ;; typedef int *ip;     (int) ((pointer) ip)        -> (typedef    ((pointer) (int))       ip)
               ;; const ip cp;         (const ip)  cp              -> (declarator (const ip)              cp)

               ;; int      *a   [2]  [3]       (int) (pointer (array 3 (array 2 a))) --> (declarator  (array 2 (array 3 (pointer int))) a)
               ;; a = array [2] of array [3] of pointer to int

               (--> direct-declarator
                    (seq simple-direct-declarator (rep direct-declarator-item :action $1)
                         :action (wrap-declarator $1 $2))
                    :action $1)

               (--> simple-direct-declarator
                    (alt (seq identifier        :action (register-declarator $1))
                         (seq \( declarator \)  :action $2))
                    :action $1)

               (--> direct-declarator-item
                    (alt (seq \( (opt direct-declarator-in-parentheses) \)
                              :action `(parameters ,$2))
                         (seq \[ (alt (seq (opt STAR) :action `(array nil (if $1 '* nil)))
                                      direct-declarator-in-brackets) \]
                                      :action $2))
                    :action $1)

               (--> direct-declarator-in-parentheses
                    (alt (seq identifier-list     :action $1)
                         (seq parameter-type-list :action $1))
                    :action $1)

               (--> direct-declarator-in-brackets
                    (alt (seq assignment-expression
                              :action `(array nil ,assignment-expression))
                         (seq STATIC (opt type-qualifier-list) assignment-expression
                              :action `(array ,(cons 'static $2) ,assignment-expression))
                         (seq type-qualifier-list (opt (alt ;; * ;; TODO
                                                            (seq (opt STATIC :action '(static)) assignment-expression
                                                                 :action (list $1 $2))))
                              :action `(array ,(append type-qualifier-list (first $2)) ,(second $2))))
                    :action $1)

               (--> parameter-type-list
                    (seq parameter-list (opt \, ELLIPSIS)
                         :action (if $2
                                     (append parameter-list '(ellipsis))
                                     parameter-list))
                    :action $1)

               (--> parameter-list
                    (seq parameter-declaration (rep \, parameter-declaration :action $2)
                         :action (cons $1 $2))
                    :action $1)

               (--> parameter-declaration
                    (seq (seq declaration-specifiers :action (push-declaration-specifiers $1))
                         (opt declarator--or--abstract-declarator)
                         :action (prog1 (if $2
                                            `(parameter ,$1 ,$2)
                                            `(parameter ,$1))
                                   (pop-declaration-specifiers)))
                    :action $1)

               (--> identifier-list
                    (seq IDENTIFIER (rep \, IDENTIFIER :action $2)
                         :action (cons $1 $2))
                    :action $1)

               (--> type-qualifier-list
                    (seq type-qualifier (rep type-qualifier :action $1)
                         :action (cons $1 $2))
                    :action $1)




               (--> declarator--or--abstract-declarator
                    (alt direct-declarator--or--direct-abstract-declarator
                         (seq pointer (opt direct-declarator--or--direct-abstract-declarator)
                              :action (wrap-pointers $2 $1)))
                    :action $1)

               (--> direct-declarator--or--direct-abstract-declarator
                    (seq simple-direct-declarator--or--simple-direct-abstract-declarator
                         (rep direct-declarator-item--or--direct-abstract-declarator-item :action $1)
                         :action (cons $1 $2))
                    :action $1)


               (--> simple-direct-declarator--or--simple-direct-abstract-declarator
                    (alt (seq IDENTIFIER :action (register-declarator identifier))
                         (seq \( (opt (alt (seq declarator--or--abstract-declarator
                                                (rep \, IDENTIFIER)
                                                :action (progn #|check declarator is identifier
                                                          if we have rep identifiers.|#))
                                           (seq parameter-type-list)))
                              \))
                         bracket-direct-abstract-declarator))

               (--> direct-declarator-item--or--direct-abstract-declarator-item
                    (alt (seq \( (opt direct-declarator-in-parentheses) \))
                         bracket-direct-abstract-declarator))

               (--> bracket-direct-abstract-declarator
                    (seq \[
                         (opt (alt (seq STAR)
                                   (seq direct-declarator-in-brackets
                                        :action (progn #| check no [*] |#))))
                         \]))



               (--> pointer
                    (seq *
                         (opt type-qualifier-list)
                         (rep * (opt type-qualifier-list) :action $2)
                         :action (cons $2 $3)))


               (--> type-name
                    (seq specifier-qualifier-list (opt abstract-declarator)))





               (--> abstract-declarator
                    (alt (seq pointer (opt direct-abstract-declarator)
                              :action (wrap-pointers $2 $1))
                         (seq direct-abstract-declarator
                              :action $1))
                    :action $1)

               (--> direct-abstract-declarator
                    (seq simple-direct-abstract-declarator
                         (rep direct-abstract-declarator-item
                              :action $1) :action (cons $1 $2)))

               (--> simple-direct-abstract-declarator
                    (alt (seq \( (opt direct-abstract-declarator-in-parentheses) \))
                         bracket-direct-abstract-declarator))

               (--> direct-abstract-declarator-in-parentheses
                    (alt (seq abstract-declarator)
                         (seq parameter-type-list)))

               (--> direct-abstract-declarator-item
                    (alt (seq \( (opt parameter-type-list) \))
                         bracket-direct-abstract-declarator))




               (--> initializer
                    (alt (seq { initializer-list (opt \,) }
                              :action `(compound-literal nil ,initializer-list))
                         assignment-expression)
                    :action $1)

               (--> initializer-list
                    (seq (alt (seq designation initializer)
                              (seq initializer))
                         (rep \, initializer-list :action $2)
                         :action (cons $1 $2))
                    :action $1)

               (--> designation
                    (seq designator-list =))

               (--> designator-list
                    designator (rep designator :action $1) :action (cons $1 $2))

               (--> designator
                    (alt (seq \[ constant-expression \])
                         (seq |.| IDENTIFIER)))

               (--> static-assert-declaration
                    (seq STATIC-ASSERT \( constant-expression \, STRING-LITERAL \) \;))

               (--> statement
                    (alt simple-labeled-statement
                         expression-statement-or-label
                         compound-statement
                         selection-statement
                         iteration-statement
                         jump-statement
                         ))


               (--> expression-statement-or-label
                    (alt \;
                         (seq expression (alt (seq \;) ; expression-statement
                                              (seq \: statement))) ; label
                         ))

               (--> expression-statement
                    (alt \;
                         (seq expression \;)))

               (--> simple-labeled-statement
                    (alt (seq CASE constant-expression \: statement)
                         (seq DEFAULT \: statement)))

               (--> compound-statement
                    (seq { (opt block-item-list) }))

               (--> block-item-list
                    block-item (rep block-item  :action $1) :action (cons $1 $2))

               (--> block-item
                    (alt declaration
                         statement))


               (--> selection-statement
                    (alt (seq IF \( expression \) statement (opt ELSE statement))
                         (seq SWITCH \( expression \) statement)))

               (--> iteration-statement
                    (alt (seq WHILE \( expression \) statement)
                         (seq DO statement WHILE \( expression \) \;)
                         (seq FOR \( (alt (seq expression-statement  expression-statement  (opt  expression)  \) statement)
                                          (seq declaration expression-statement (opt expression) \) statement)))))

               (--> jump-statement
                    (alt (seq GOTO IDENTIFIER \;)
                         (seq CONTINUE \;)
                         (seq BREAK \;)
                         (seq RETURN (opt  expression) \;)))

               (--> translation-unit
                    (seq external-declaration (rep external-declaration :action $1)
                         :action (cons $1 $2))
                    :action $1)


;;; external_declaration
;;;     : function_definition
;;;     | declaration
;;;     ;
;;; 
;;; function_definition
;;;     : declaration_specifiers declarator declaration_list compound_statement
;;;     | declaration_specifiers declarator                  compound_statement
;;;     ;
;;;
;;; declaration
;;;     : declaration_specifiers ';'
;;;     | declaration_specifiers init_declarator_list ';'
;;;     | static_assert_declaration
;;;     ;
                         

;;; init_declarator_list
;;;     : init_declarator
;;;     | init_declarator_list ',' init_declarator
;;;     ;
;;; 
;;; init_declarator
;;;     : declarator '=' initializer
;;;     | declarator
;;;     ;

;;; init_declarator_list
;;;     :
;;;     | ',' init_declarator init_declarator_list
;;;     ;

;;; external_declaration
;;;     : static_assert_declaration
;;;     | declaration_specifiers                                                 ';'
;;;     | declaration_specifiers declarator '=' initializer init_declarator_list ';'
;;;     | declaration_specifiers declarator                  compound_statement
;;;     | declaration_specifiers declarator declaration_list compound_statement
;;;     ;               

               (--> external-declaration
                    (alt (seq static-assert-declaration)

                         (seq (seq declaration-specifiers :action (push-declaration-specifiers $1))
                              (alt (seq \; :action :specifier)
                                   (seq declarator
                                        (alt (seq (opt = initializer :action $2) (rep \, init-declarator :action $2) \;
                                                  :action (if $1
                                                              `(:initializer ,$1 ,$2)
                                                              `(:simple          ,$2)))
                                             (seq (rep declaration) compound-statement
                                                  ;; declaration-list are K&R parameters!
                                                  :action `(:function-declarator ,$1 ,$2)))
                                        :action (ecase (first $2)
                                                  (:simple
                                                   (declarator $1 (second $2)))
                                                  (:initializer
                                                   (cons (declarator $1 (second $2)) (third $2)))
                                                  (:function-declarator
                                                   (list :declarator $1 $2)))))
                              :action (progn
                                        (print `(declaration-specifiers ,$1))
                                        (print `(declarator ,$2))
                                        (pop-declaration-specifiers)
                                        (if (eql $2 :specifier)
                                            $1
                                            $2))))
                    :action (print `(external-declaration ,$1)))

               #-(and)
               (--> declaration-list
                    (seq declaration (rep declaration :action $1)
                         :action (cons $1 $2))
                    :action $1)

               (--> declaration
                    (alt (seq (seq declaration-specifiers :action (push-declaration-specifiers $1))
                              (opt init-declarator-list)
                              \;
                              :action (prog1 (if $2
                                                 (mapcar (lambda (declarator)
                                                           (destructuring-bind (op declarator initializer) declarator
                                                             `(,op ,(unwrap-declarator declarator $1) ,initializer)))
                                                         $2)
                                                 $1)
                                        (pop-declaration-specifiers)))
                         static-assert-declaration)
                    :action $1)

               ))
  
  (defparameter *c* '#1#))


(defun push-declaration-specifiers (specifiers)
  "
DO:     Push the specifiers onto the (context-declaration-specifiers *context*) stack.
RETURN: specifiers
NOTE:   if the specifiers is a typedef, then pushes above it :typedef.
"
  (push specifiers (context-declaration-specifiers *context*))
  (when (member 'typedef specifiers)
    (push :typedef (context-declaration-specifiers *context*)))
  (print `(specifiers --> ,specifiers)) (terpri)
  specifiers)

(defun pop-declaration-specifiers ()
  "
DO:     Pops (context-declaration-specifiers *context*) stack.
RETURN: The old top of stack specifier.
NOTE:   if the top-of-stack is :typedef then pop it as well as the specifiers.
"
  (when (eq :typedef (pop (context-declaration-specifiers *context*)))
    (pop (context-declaration-specifiers *context*))))

(defun declarator-name (declarator)
  (print `(declarator --> ,declarator)) (terpri)
  (third (unwrap-declarator nil declarator)))


(defun register-declarator (declarator)
  (let ((name        (declarator-name declarator))
        (kind        (first  (context-declaration-specifiers *context*)))
        (declaration (second (context-declaration-specifiers *context*))))
    (case kind
      (:typedef   (enter-typedef              *context* name declaration))
      (:enum      (enter-enumeration-constant *context* name declaration))
      (:function  (enter-function             *context* name declaration))))
  declarator)

(defun declarator (declarator initializer)
  (let ((name (declarator-name declarator))
        (kind       (first  (context-declaration-specifiers *context*))))
    (when initializer
      (cerror "Continue" "Invalid initializer in a ~A ~S" kind initializer))
    `(:declarator ,name ,declarator ,initializer)))

(defun check-constant-expression (expression)
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

(defun wrap-pointers (expression pointers)
  (loop
    :for pointer :in (reverse pointers)
    :do (setf expression `(pointer ,pointer ,expression))
    :finally (return expression)))

;; (wrap-pointers 'a '(() (const) (volatile const)))
;; (pointer nil (pointer (const) (pointer (volatile const) a)))

(defun wrap-declarator (declarator items)
  (loop
    :for item :in items
    :do (setf declarator (ecase (first item)
                           (parameters `(function ,(second item) ,declarator))
                           (array      `(array ,(second item) ,(third item) ,declarator))))
    :finally (return declarator)))

(defun unwrap-declarator (type declarator)
  (loop
    :while (listp declarator)
    :do (let ((type (first declarator)))
          (case type
            (pointer  (setf type `(pointer ,(second declarator) ,type)
                            declarator (third declarator)))
            (array    (setf type `(array ,(second declarator) ,(third declarator) ,type)
                            declarator (fourth declarator)))
            (function (setf type `(function ,(second declarator) ,type)
                            declarator (third declarator)))
            (otherwise (return-from unwrap-declarator (values declarator type)))))))



;;;; THE END ;;;;
