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

(defmethod scanner-line ((scanner pre-scanned-scanner))
  (token-line (pre-scanner-actual-current-token scanner)))

(defmethod scanner-column ((scanner pre-scanned-scanner))
  (token-column (pre-scanner-actual-current-token scanner)))

(defmethod scan-next-token ((scanner pre-scanned-scanner) &optional parser-data)
  (declare (stepper disable))
  (declare (ignore parser-data))
  (let* ((token (pop (pre-scanned-tokens scanner)))
         (kind  (token-kind token)))
    (case kind
      (|identifier|
       (cond
         ((typedef-name-p *context* token)
          (setf (token-kind token) (setf kind '|typedef_name|)))
         ((function-name-p *context* token)
          (setf (token-kind token) (setf kind '|func_name|)))
         ((enumeration-constant-name-p *context* token)
          (setf (token-kind token) (setf kind '|enum_name|)))))
      #| TODO: handle [*] -> [ STAR ] |#)
    (format *trace-output* "~A~%" token)
    (setf (pre-scanner-actual-current-token scanner) token
          (scanner-current-text  scanner) (token-text token)
          ;; result:
          (scanner-current-token scanner) kind)))

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

(defmethod accept ((scanner scanner) token)
  (if (word-equal token (scanner-current-token scanner))
      (prog1 (list (scanner-current-token scanner)
                   (scanner-current-text scanner)
                   (scanner-column scanner))
        (scan-next-token scanner))
      (error 'unexpected-token-error
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
 (defmethod print-object ((self (eql '\()) stream) (declare (stepper disable)) (princ "\\( " stream) self)
 (defmethod print-object ((self (eql '\))) stream) (declare (stepper disable)) (princ "\\) " stream) self)
 (defmethod print-object ((self (eql '\,)) stream) (declare (stepper disable)) (princ "\\, " stream) self)
 (defmethod print-object ((self (eql '\:)) stream) (declare (stepper disable)) (princ "\\: " stream) self)
 (defmethod print-object ((self (eql '\;)) stream) (declare (stepper disable)) (princ "\\; " stream) self)
 (defmethod print-object ((self (eql '|.|)) stream) (declare (stepper disable)) (princ "\\. " stream) self)
 (defmethod print-object ((self (eql '\[)) stream) (declare (stepper disable)) (princ "\\[ " stream) self)
 (defmethod print-object ((self (eql '\])) stream) (declare (stepper disable)) (princ "\\] " stream) self)
 (defmethod print-object ((self (eql '\{)) stream) (declare (stepper disable)) (princ "\\{ " stream) self)
 (defmethod print-object ((self (eql '\})) stream) (declare (stepper disable)) (princ "\\} " stream) self)
 (defmethod print-object ((self (eql '\|)) stream) (declare (stepper disable)) (princ "\\| " stream) self))

(progn
  #1=(defgrammar c11
       ;; rdp
       :scanner nil
       :terminals ((|identifier| "identifier")
                   (|typedef_name| "typedef_name")
                   (|func_name| "func_name")
                   (|string_literal| "string_literal")
                   (|i_constant| "i_constant")
                   (|f_constant| "f_constant")
                   (|enum_name| "enum_name")
                   (|alignas| "alignas")
                   (|alignof| "alignof")
                   (|atomic| "atomic")
                   (|generic| "generic")
                   (|noreturn| "noreturn")
                   (|static_assert| "static_assert")
                   (|thread_local| "thread_local")
                   (|case| "case")
                   (|default| "default")
                   (|if| "if")
                   (|else| "else")
                   (|switch| "switch")
                   (|while| "while")
                   (|do| "do")
                   (|for| "for")
                   (|goto| "goto")
                   (|continue| "continue")
                   (|break| "break")
                   (|return| "return")
                   (|struct| "struct")
                   (|union| "union")
                   (|enum| "enum")
                   (|...| "...")
                   (|complex| "complex")
                   (|imaginary| "imaginary")
                   (|bool| "bool")
                   (|char| "char")
                   (|short| "short")
                   (|int| "int")
                   (|long| "long")
                   (|signed| "signed")
                   (|unsigned| "unsigned")
                   (|float| "float")
                   (|double| "double")
                   (|void| "void")
                   (|const| "const")
                   (|restrict| "restrict")
                   (|volatile| "volatile")
                   (|typedef| "typedef")
                   (|extern| "extern")
                   (|static| "static")
                   (|auto| "auto")
                   (|register| "register")
                   (|inline| "inline")
                   (|sizeof| "sizeof")
                   (^= "^=")
                   (\|= "|=")
                   (-= "-=")
                   (<<= "<<=")
                   (>>= ">>=")
                   (&= "&=")
                   (&& "&&")
                   (|\|\|| "||")
                   (*= "*=")
                   (/= "/=")
                   (%= "%=")
                   (+= "+=")
                   (-> "->")
                   (++ "++")
                   (-- "--")
                   (<< "<<")
                   (>> ">>")
                   (<= "<=")
                   (>= ">=")
                   (== "==")
                   (!= "!=")
                   (\( "(")
                   (\) ")")
                   (\, ",")
                   (\: ":")
                   (\; ";")
                   (\. ".")
                   (\[ "[")
                   (\] "]")
                   (\{ "{")
                   (\} "}")
                   (\& "&")
                   (\* "*")
                   (\/ "/")
                   (\+ "+")
                   (\- "-")
                   (\~ "~")
                   (\! "!")
                   (\% "%")
                   (\< "<")
                   (\> ">")
                   (\= "=")
                   (\^ "^")
                   (\| "|")
                   (\? "?")
                   (STAR "*") ;; (seq [ (opt type_qualifier_list) * ])
                   )
  
       :start |translation_unit|
       :rules (

               (--> IDENTIFIER     (seq |identifier| :action $1))     
               (--> TYPEDEF_NAME   (seq |typedef_name| :action $1))   
               (--> FUNC_NAME      (seq |func_name| :action $1))
               (--> STRING_LITERAL (seq |string_literal| :action $1)) 
               (--> I_CONSTANT     (seq |i_constant| :action $1))     
               (--> F_CONSTANT     (seq |f_constant| :action $1))
               (--> ALIGNAS        (seq |alignas| :action $1))        
               (--> ALIGNOF        (seq |alignof| :action $1))        
               (--> ATOMIC         (seq |atomic| :action $1))         
               (--> GENERIC        (seq |generic| :action $1))
               (--> NORETURN       (seq |noreturn| :action $1))       
               (--> STATIC_ASSERT  (seq |static_assert| :action $1))  
               (--> THREAD_LOCAL   (seq |thread_local| :action $1))
               (--> CASE           (seq |case| :action $1))           
               (--> DEFAULT        (seq |default| :action $1))        
               (--> IF             (seq |if| :action $1))             
               (--> ELSE           (seq |else| :action $1))           
               (--> SWITCH         (seq |switch| :action $1))
               (--> WHILE          (seq |while| :action $1))          
               (--> DO             (seq |do| :action $1))             
               (--> FOR            (seq |for| :action $1))            
               (--> GOTO           (seq |goto| :action $1))           
               (--> CONTINUE       (seq |continue| :action $1))
               (--> BREAK          (seq |break| :action $1))          
               (--> RETURN         (seq |return| :action $1))         
               (--> STRUCT         (seq |struct| :action $1))         
               (--> UNION          (seq |union| :action $1))          
               (--> ENUM           (seq |enum| :action $1))
               (--> ELLIPSIS       (SEQ |...| :ACTION $1))            
               (--> COMPLEX        (seq |complex| :action $1))        
               (--> IMAGINARY      (seq |imaginary| :action $1))      
               (--> BOOL           (seq |bool| :action $1))           
               (--> CHAR           (seq |char| :action $1))
               (--> SHORT          (seq |short| :action $1))          
               (--> INT            (seq |int| :action $1))            
               (--> LONG           (seq |long| :action $1))           
               (--> SIGNED         (seq |signed| :action $1))         
               (--> UNSIGNED       (seq |unsigned| :action $1))
               (--> FLOAT          (seq |float| :action $1))          
               (--> DOUBLE         (seq |double| :action $1))         
               (--> VOID           (seq |void| :action $1))           
               (--> CONST          (seq |const| :action $1))          
               (--> RESTRICT       (seq |restrict| :action $1))
               (--> VOLATILE       (seq |volatile| :action $1))       
               (--> TYPEDEF        (seq |typedef| :action $1))        
               (--> EXTERN         (seq |extern| :action $1))         
               (--> STATIC         (seq |static| :action $1))
               (--> AUTO           (seq |auto| :action $1))           
               (--> REGISTER       (seq |register| :action $1))       
               (--> INLINE         (seq |inline| :action $1))         
               (--> SIZEOF         (seq |sizeof| :action $1))         
               (--> XOR_ASSIGN     (seq ^= :action $1))
               (--> OR_ASSIGN      (seq \|= :action $1))              
               (--> SUB_ASSIGN     (seq -= :action $1))               
               (--> LEFT_ASSIGN    (seq <<= :action $1))              
               (--> RIGHT_ASSIGN   (seq >>= :action $1))              
               (--> AND_ASSIGN     (seq &= :action $1))
               (--> AND_OP         (seq && :action $1))               
               (--> OR_OP          (seq |\|\|| :action $1))           
               (--> MUL_ASSIGN     (seq *= :action $1))               
               (--> DIV_ASSIGN     (seq /= :action $1))               
               (--> MOD_ASSIGN     (seq %= :action $1))
               (--> ADD_ASSIGN     (seq += :action $1))               
               (--> PTR_OP         (seq -> :action $1))               
               (--> INC_OP         (seq ++ :action $1))               
               (--> DEC_OP         (seq -- :action $1))               
               (--> LEFT_OP        (seq << :action $1))               
               (--> RIGHT_OP       (seq >> :action $1))
               (--> LE_OP          (seq <= :action $1))               
               (--> GE_OP          (seq >= :action $1))               
               (--> EQ_OP          (seq == :action $1))               
               (--> NE_OP          (seq != :action $1))
          
          
               (--> |constant|     (alt I_CONSTANT     F_CONSTANT)) 
               (--> |string|       (alt STRING_LITERAL FUNC_NAME))


               (--> |simple_primary_expression|
                    (alt IDENTIFIER
                         |constant|
                         |string|
                         |generic_selection|))
          
               (--> |primary_expression|
                    (alt |simple_primary_expression|
                         (seq \( |expression| \))))

               (--> |generic_selection|
                    (seq GENERIC \( |assignment_expression| \, |generic_assoc_list| \)))

               (--> |generic_assoc_list|
                    (seq |generic_association| (rep \, |generic_association| :action $2) :action (cons $1 $2)))

               (--> |generic_association|
                    (alt (seq |type_name| \: |assignment_expression|) 
                         (seq DEFAULT \: |assignment_expression|)))

               (--> |postfix_expression|
                    (seq |postfix_expression_head| (rep |postfix_expression_item| :action $1) :action (cons $1 $2)))

               (--> |postfix_expression_head|
                    (alt |simple_primary_expression|
                         (seq \( (alt (seq |expression| \))
                                      (seq |type_name|  \)
                                           { |initializer_list| (opt \,)})
                                      \)))))
          
               (--> |postfix_expression_item|
                    (alt (seq [ |expression| ]) 
                         (seq \( (opt |argument_expression_list|) \)) 
                         (seq |.| IDENTIFIER)
                         (seq PTR_OP IDENTIFIER) 
                         (seq INC_OP)
                         (seq DEC_OP)))

               (--> |argument_expression_list|
                    (seq |assignment_expression| (rep \, |assignment_expression| :action $2) :action (cons $1 $2)))

               (--> |simple_unary_expression|
                    (alt (seq INC_OP |unary_expression|) 
                         (seq DEC_OP |unary_expression|)
                         (seq |unary_operator| |cast_expression|) 
                         (seq SIZEOF |sizeof_argument|) 
                         (seq ALIGNOF \( |type_name| \))))

               (--> |sizeof_argument|
                    (alt (seq |simple_unary_expression|)
                         (seq (alt |simple_primary_expression|
                                   (seq \( (alt (seq |expression| \))
                                                (seq |type_name| \) (opt { |initializer_list| (opt \,)}))
                                                \))))
                              (rep |postfix_expression_item|))))
          
               (--> |unary_expression|
                    (alt |postfix_expression|
                         |simple_unary_expression|))

               (--> |unary_operator|
                    (alt & * + - ~ !))

               (--> |cast_expression|
                    (alt |simple_unary_expression|
                         |simple_primary_expression|
                         (seq \( (alt (seq |expression|)
                                      (seq |type_name| \) (alt (seq  { |initializer_list| (opt \,) })
                                                               |cast_expression|))\)))))

               (--> |multiplicative_expression|
                    (seq |cast_expression| (rep (alt * / %) |cast_expression|)))

               (--> |additive_expression|
                    (seq |multiplicative_expression| (rep (alt + -) |multiplicative_expression|)))

               (--> |shift_expression|
                    (seq |additive_expression| (rep (alt LEFT_OP RIGHT_OP) |additive_expression|)))

               (--> |relational_expression|
                    (seq |shift_expression| (rep (alt < > LE_OP GE_OP)  |shift_expression|)))

               (--> |equality_expression|
                    (seq |relational_expression| (rep (alt EQ_OP NE_OP) |relational_expression|)))

               (--> |and_expression|
                    (seq |equality_expression| (rep & |equality_expression|)))

               (--> |exclusive_or_expression|
                    (seq |and_expression| (rep ^ |and_expression|)))

               (--> |inclusive_or_expression|
                    (seq |exclusive_or_expression| (rep \| |exclusive_or_expression|)))

               (--> |logical_and_expression|
                    (seq |inclusive_or_expression| (rep AND_OP |inclusive_or_expression|)))

               (--> |logical_or_expression|
                    (seq |logical_and_expression| (rep OR_OP |logical_and_expression|)))

               (--> |conditional_expression|
                    (seq |logical_or_expression| (opt ? |expression| \: |conditional_expression|)))

               #-(and)
               (--> |assignment_expression|
                    (alt |conditional_expression|
                         (seq |unary_expression| |assignment_operator| |assignment_expression|)))

               (--> |assignment_expression|
                    (seq |conditional_expression|
                         (opt (seq |assignment_operator| |assignment_expression|))
                         :action (progn #|check the conditional_expression is actually unary_expression|#)))

               (--> |assignment_operator|
                    (alt = MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
                         XOR_ASSIGN OR_ASSIGN
                         ))

               (--> |expression|
                    (seq |assignment_expression| (rep \, |assignment_expression| :action $2) :action (cons $1 $2)))

               (--> |constant_expression|
                    |conditional_expression|)

               (--> |declaration|
                    (alt (seq |declaration_specifiers| (opt |init_declarator_list|) \;)
                         |static_assert_declaration|))
          

               (--> |alignment_specifier|
                    (seq ALIGNAS \( (opt |type_name| |constant_expression|) \)))

               (--> |function_specifier|
                    (alt (seq INLINE   :action 'inline)
                         (seq NORETURN :action 'noreturn)))

               (--> |storage_class_specifier|
                    (alt (seq TYPEDEF      :action 'typedef)
                         (seq EXTERN       :action 'extern)
                         (seq STATIC       :action 'static)
                         (seq THREAD_LOCAL :action 'thread-load)
                         (seq AUTO         :action 'auto)
                         (seq REGISTER     :action 'register)))

               (--> |type_qualifier|
                    (alt |simple_type_qualifier|
                         (seq ATOMIC :action (list :type-qualifier :atomic))))
               
               (--> |simple_type_qualifier|
                    (seq (alt (seq CONST    :action 'const)
                              (seq RESTRICT :action 'restrict)
                              (seq VOLATILE :action 'volatile))
                         :action (list :type-qualifier $1))) 
               
               (--> |type_specifier|
                    (alt |simple_type_specifier|
                         |atomic_type_specifier|))

               (--> |atomic_type_specifier|
                    (seq ATOMIC \( |type_name| \) :action (list :type-specifier $1 $3)))

               (--> |simple_type_specifier|
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
                              |struct_or_union_specifier|
                              |enum_specifier|
                              TYPEDEF_NAME)
                         :action (list :type-specifier $1)))

               (--> |specifier_qualifier|
                    (alt (seq ATOMIC (opt \( |type_name| \) :action $2)
                              :action
                              (if $2
                                  (list :type-specifier $1 $2)
                                  (list :type-qualifier $1)))
                         |simple_type_qualifier|
                         |simple_type_specifier|))
          
               (--> |declaration_specifier|
                    (alt |alignment_specifier|
                         |function_specifier|
                         |storage_class_specifier|
                         |specifier_qualifier|))
          
               (--> |declaration_specifiers|
                    (seq |declaration_specifier| (rep |declaration_specifier| :action $1) :action (cons $1 $2)))

               (--> |init_declarator_list|
                    (seq |init_declarator| (rep \, |init_declarator| :action $2) :action (cons $1 $2)))

               (--> |init_declarator|
                    (seq |declarator| (opt = |initializer| :action $2)
                         :action (declarator $1 $2)))

               (--> |struct_or_union_specifier|
                    (seq |struct_or_union| (alt (seq { |struct_declaration_list| })
                                                (seq IDENTIFIER (opt { |struct_declaration_list| })))))

               (--> |struct_or_union|
                    (alt STRUCT UNION
                         ))

               (--> |struct_declaration_list|
                    (seq |struct_declaration| (rep |struct_declaration| :action $1) :action (cons $1 $2)))

               (--> |struct_declaration|
                    (alt (seq |specifier_qualifier_list| (opt |struct_declarator_list|) \;) 
                         |static_assert_declaration|
                         ))

               (--> |specifier_qualifier_list|
                    (seq |specifier_qualifier| (rep |specifier_qualifier| :action $1) :action (cons $1 $2)))

               (--> |struct_declarator_list|
                    (seq |struct_declarator| (rep \, |struct_declarator| :action $2) :action (cons $1 $2)))

               (--> |struct_declarator|
                    (alt (seq \: |constant_expression|) 
                         (seq |declarator| (opt \: |constant_expression|))))

               (--> |enum_specifier|
                    ENUM (alt (seq { |enumerator_list| (opt \,)}) 
                              (seq IDENTIFIER (opt { |enumerator_list| (opt \,)}))))

               (--> |enumerator_list|
                    (seq |enumerator| (rep \, |enumerator| :action $2) :action (cons $1 $2)))

               (--> |enumeration_constant|
                    IDENTIFIER)

               (--> |enumerator|
                    (seq |enumeration_constant| (opt = |constant_expression|)))




               (--> |declarator|
                    (seq (opt |pointer|) |direct_declarator|
                         :action (if $1
                                     `(:pointer $2)
                                     $2))
                    :action $1)

               (--> |direct_declarator|
                    (seq |simple_direct_declarator| (rep |direct_declarator_item| :action $1)
                         :action (cons $1 $2))
                    :action $1)

               (--> |simple_direct_declarator|
                    (alt (seq IDENTIFIER :action $1)
                         (seq \(
                              (opt (alt (seq |declarator| (rep \, IDENTIFIER :action $2)
                                             :action (progn
                                                       #|check declarator is identifier
                                                       if we have rep identifiers.|#
                                                       (if $2
                                                           (progn
                                                             (unless (eq '|identifier| (car $1))
                                                               (error "Invalid simple direct declarator: identifier list ~A following ~A"
                                                                      $2 $1))
                                                             (cons $1 $2))
                                                           $1)))
                                        (seq |parameter_type_list|
                                             :action `(:parameters ,$1))))
                              \)
                              :action $2))                    
                    :action $1)

          (--> |direct_declarator_item|
               (alt (seq \( (opt |direct_declarator_in_parentheses|) \))
                    (seq \[ (opt (alt STAR
                                      |direct_declarator_in_brackets|)) \])))

          (--> |direct_declarator_in_parentheses|
               (alt (seq |identifier_list|)
                    (seq |parameter_type_list|)))

          (--> |direct_declarator_in_brackets|
               (alt (seq |assignment_expression|)
                    (seq STATIC (opt |type_qualifier_list|) |assignment_expression|)
                    (seq |type_qualifier_list| (opt (alt ;; * ;; TODO
                                                         (seq (opt STATIC) |assignment_expression|))))))

          

          (--> |parameter_type_list|
               (seq |parameter_list| (opt \, ELLIPSIS)))

          (--> |parameter_list|
               |parameter_declaration| (rep \, |parameter_declaration| :action $2) :action (cons $1 $2))

          (--> |identifier_list|
               (seq IDENTIFIER (rep \, IDENTIFIER :action $2) :action (cons $1 $2)))

          (--> |type_qualifier_list|
               |type_qualifier| (rep |type_qualifier| :action $1) :action (cons $1 $2))

          




          (--> |parameter_declaration|
               (seq |declaration_specifiers|
                    (opt |declarator__or__abstract_declarator|)))

          (--> |declarator__or__abstract_declarator|
               (alt |direct_declarator__or__direct_abstract_declarator|
                    (seq |pointer| (opt |direct_declarator__or__direct_abstract_declarator|))))

          (--> |direct_declarator__or__direct_abstract_declarator|
               (seq |simple_direct_declarator__or__simple_direct_abstract_declarator|
                    (rep |direct_declarator_item__or__direct_abstract_declarator_item|
                         :action $1) :action (cons $1 $2)))
               
          
          (--> |simple_direct_declarator__or__simple_direct_abstract_declarator|
               (alt (seq IDENTIFIER)
                    (seq \( (opt (alt (seq |declarator__or__abstract_declarator|
                                           (rep \, IDENTIFIER)
                                           :action (progn #|check declarator is identifier
                                                       if we have rep identifiers.|#))
                                      (seq |parameter_type_list|)))
                         \))
                    |bracket_direct_abstract_declarator|
                   ))
          
          (--> |direct_declarator_item__or__direct_abstract_declarator_item|
               (alt (seq \( (opt |direct_declarator_in_parentheses|) \))
                    |bracket_direct_abstract_declarator|
                   ))

          (--> |bracket_direct_abstract_declarator|
               (seq \[
                    (opt (alt (seq STAR)
                              (seq |direct_declarator_in_brackets|
                                   :action (progn #| check no [*] |#))))
                    \]))




          
          (--> |pointer|
               (seq * (opt (alt (seq |type_qualifier_list| (opt |pointer|))
                                |pointer|
                               ))))
          
          
          (--> |type_name|
               (seq |specifier_qualifier_list| (opt |abstract_declarator|)))


          


          (--> |abstract_declarator|
               (alt (seq |pointer| (opt |direct_abstract_declarator|))
                    (seq |direct_abstract_declarator|)))

          (--> |direct_abstract_declarator|
               (seq |simple_direct_abstract_declarator|
                    (rep |direct_abstract_declarator_item|
                         :action $1) :action (cons $1 $2)))
          
          (--> |simple_direct_abstract_declarator|
               (alt (seq \( (opt |direct_abstract_declarator_in_parentheses|) \))
                    |bracket_direct_abstract_declarator|
                   ))

          (--> |direct_abstract_declarator_in_parentheses|
               (alt (seq |abstract_declarator|)
                    (seq |parameter_type_list|)))

          (--> |direct_abstract_declarator_item|
               (alt (seq \( (opt |parameter_type_list|) \))
                    |bracket_direct_abstract_declarator|
                   ))

          
          
          
          

          (--> |initializer|
               (alt (seq { |initializer_list| (opt \,)}) 
                    |assignment_expression|
                   ))

          (--> |initializer_list|
               (seq (alt (seq |designation| |initializer|)
                         (seq |initializer|))
                    (rep \, |initializer_list|)))

          (--> |designation|
               (seq |designator_list| =))

          (--> |designator_list|
               |designator| (rep |designator| :action $1) :action (cons $1 $2))

          (--> |designator|
               (alt (seq \[ |constant_expression| \]) 
                    (seq |.| IDENTIFIER)))

          (--> |static_assert_declaration|
               (seq STATIC_ASSERT \( |constant_expression| \, STRING_LITERAL \) \;))

          (--> |statement|
               (alt |simple_labeled_statement|
                    |expression_statement_or_label|
                    |compound_statement|
                    |selection_statement|
                    |iteration_statement|
                    |jump_statement|
                   ))


          (--> |expression_statement_or_label|
               (alt \;
                    (seq |expression| (alt (seq \;) ; expression_statement
                                           (seq \: |statement|))) ; label
                   ))
          
          (--> |expression_statement|
               (alt \;
                    (seq |expression| \;)))
          
          (--> |simple_labeled_statement|
               (alt (seq CASE |constant_expression| \: |statement|)
                    (seq DEFAULT \: |statement|)))

          (--> |compound_statement|
               (seq { (opt |block_item_list|) }))

          (--> |block_item_list|
               |block_item| (rep |block_item|  :action $1) :action (cons $1 $2))

          (--> |block_item|
               (alt |declaration|
                    |statement|))


          (--> |selection_statement|
               (alt (seq IF \( |expression| \) |statement| (opt ELSE |statement|)) 
                    (seq SWITCH \( |expression| \) |statement|)))

          (--> |iteration_statement|
               (alt (seq WHILE \( |expression| \) |statement|) 
                    (seq DO |statement| WHILE \( |expression| \) \;)
                    (seq FOR \( (alt (seq |expression_statement|  |expression_statement|  (opt  |expression|)  \) |statement|)
                                     (seq |declaration| |expression_statement| (opt |expression|) \) |statement|)))))

          (--> |jump_statement|
               (alt (seq GOTO IDENTIFIER \;) 
                    (seq CONTINUE \;) 
                    (seq BREAK \;) 
                    (seq RETURN (opt  |expression|) \;)))

          (--> |translation_unit|
               (seq |external_declaration| (rep |external_declaration| :action $1) :action (cons $1 $2)))

          (--> |external_declaration|
               (alt (seq |static_assert_declaration|)

                    (seq (seq |declaration_specifiers| :action (push-declaration-specifiers $1))
                         (alt (seq \; )
                              (seq |declarator|
                                   (alt (seq = |initializer| (rep \, |init_declarator| :action $2) \;
                                             :action (list :initializer $2 $3))
                                        (seq (opt |declaration_list|) |compound_statement|
                                             :action (list :function-declarator $1 $2))
                                        (seq \;
                                             :action '(:simple)))
                                   :action (ecase (first $2)
                                             (:simple
                                              (declarator $1 nil))
                                             (:initializer
                                              (cons (declarator $1 (second $2)) (third $2)))
                                             (:function-declarator
                                              (list :declarator $1 $2)))))
                         :action (progn
                                   (print `(declaration-specifiers ,$1))
                                   (print `(declarator ,$2))
                                   (pop-declaration-specifiers)
                                   $2))))

          (--> |declaration_list|
               (seq  |declaration| (rep |declaration| :action $1) :action (cons $1 $2)))))
  (defparameter *c* '#1#))


(defun push-declaration-specifiers (specifiers)
  (push specifiers (context-declaration-specifiers *context*))
  (when (some (lambda (specifier)
                (and (eq '|storage-class-specifier| (caadr specifier))
                     (eq 'typedef (cadar specifier))))
              (second specifiers))
    (push :typedef (context-declaration-specifiers *context*)))
  (print specifiers) (terpri))

(defun pop-declaration-specifiers ()
  (when (eq :typedef (pop (context-declaration-specifiers *context*)))
    (pop (context-declaration-specifiers *context*))))

(defun declarator-name (declarator)
  (print declarator) (terpri))

(defun declarator ($1 $2)
  (let ((name (declarator-name $1)))
    (case (first (print (context-declaration-specifiers *context*)))
      (:typedef
       (when $2
         (cerror "Continue" "Invalid initializer in a typedef"))
       (enter-typedef *context* name (second (context-declaration-specifiers *context*))))
      (:enum
       (when $2
         (cerror "Continue" "Invalid initializer in an enum"))
       ;; TODO: ???
       (enter-enumeration-constant *context* name (second (context-declaration-specifiers *context*)))))
    `(:declarator ,name ,$1 ,$2)))


;;;; THE END ;;;;
