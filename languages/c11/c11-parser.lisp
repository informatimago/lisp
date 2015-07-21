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


(defclass pre-scanned-scanner (scanner)
  ((tokens :initform '() :initarg :tokens :accessor pre-scanned-tokens))
  (:default-initargs :source ""))

(defmethod (setf scanner-source) (new-source (scanner pre-scanned-scanner))
  new-source)

(defmethod scanner-line ((scanner pre-scanned-scanner))
  (token-line (scanner-current-token scanner)))

(defmethod scanner-column ((scanner pre-scanned-scanner))
  (token-column (scanner-current-token scanner)))

(defmethod scan-next-token ((scanner pre-scanned-scanner) &optional parser-data)
  (declare (ignore parser-data))
  (setf (scanner-current-token scanner) (pop (pre-scanned-tokens scanner))))

(defmethod scanner-end-of-source-p ((scanner pre-scanned-scanner))
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
  (if (word-equal token (token-kind (scanner-current-token scanner)))
      (prog1 (list (token-kind (scanner-current-token scanner))
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
                                     (token-kind (scanner-current-token scanner))
                                     (scanner-current-text scanner)))))

;;;---------------------------------------------------------------------

(defgrammar c11
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

          (--> IDENTIFIER     |identifier|)     
          (--> TYPEDEF_NAME   |typedef_name|)   
          (--> FUNC_NAME      |func_name|)
          (--> STRING_LITERAL |string_literal|) 
          (--> I_CONSTANT     |i_constant|)     
          (--> F_CONSTANT     |f_constant|)
          (--> ALIGNAS        |alignas|)        
          (--> ALIGNOF        |alignof|)        
          (--> ATOMIC         |atomic|)         
          (--> GENERIC        |generic|)
          (--> NORETURN       |noreturn|)       
          (--> STATIC_ASSERT  |static_assert|)  
          (--> THREAD_LOCAL   |thread_local|)
          (--> CASE           |case|)           
          (--> DEFAULT        |default|)        
          (--> IF             |if|)             
          (--> ELSE           |else|)           
          (--> SWITCH         |switch|)
          (--> WHILE          |while|)          
          (--> DO             |do|)             
          (--> FOR            |for|)            
          (--> GOTO           |goto|)           
          (--> CONTINUE       |continue|)
          (--> BREAK          |break|)          
          (--> RETURN         |return|)         
          (--> STRUCT         |struct|)         
          (--> UNION          |union|)          
          (--> ENUM           |enum|)
          (--> ELLIPSIS       |...|)            
          (--> COMPLEX        |complex|)        
          (--> IMAGINARY      |imaginary|)      
          (--> BOOL           |bool|)           
          (--> CHAR           |char|)
          (--> SHORT          |short|)          
          (--> INT            |int|)            
          (--> LONG           |long|)           
          (--> SIGNED         |signed|)         
          (--> UNSIGNED       |unsigned|)
          (--> FLOAT          |float|)          
          (--> DOUBLE         |double|)         
          (--> VOID           |void|)           
          (--> CONST          |const|)          
          (--> RESTRICT       |restrict|)
          (--> VOLATILE       |volatile|)       
          (--> TYPEDEF        |typedef|)        
          (--> EXTERN         |extern|)         
          (--> STATIC         |static|)
          (--> AUTO           |auto|)           
          (--> REGISTER       |register|)       
          (--> INLINE         |inline|)         
          (--> SIZEOF         |sizeof|)         
          (--> XOR_ASSIGN     ^=)
          (--> OR_ASSIGN      \|=)              
          (--> SUB_ASSIGN     -=)               
          (--> LEFT_ASSIGN    <<=)              
          (--> RIGHT_ASSIGN   >>=)              
          (--> AND_ASSIGN     &=)
          (--> AND_OP         &&)               
          (--> OR_OP          |\|\||)           
          (--> MUL_ASSIGN     *=)               
          (--> DIV_ASSIGN     /=)               
          (--> MOD_ASSIGN     %=)
          (--> ADD_ASSIGN     +=)               
          (--> PTR_OP         ->)               
          (--> INC_OP         ++)               
          (--> DEC_OP         --)               
          (--> LEFT_OP        <<)               
          (--> RIGHT_OP       >>)
          (--> LE_OP          <=)               
          (--> GE_OP          >=)               
          (--> EQ_OP          ==)               
          (--> NE_OP          !=)
          
          
          (--> |constant|     (ALT I_CONSTANT     F_CONSTANT)) 
          (--> |string|       (ALT STRING_LITERAL FUNC_NAME))


          (--> |simple_primary_expression|
               (ALT IDENTIFIER
                    |constant|
                    |string|
                    |generic_selection|))
          
          (--> |primary_expression|
               (ALT |simple_primary_expression|
                    (SEQ \( |expression| \))))

          (--> |generic_selection|
               (SEQ GENERIC \( |assignment_expression| \, |generic_assoc_list| \)))

          (--> |generic_assoc_list|
               (SEQ |generic_association| (REP \, |generic_association|)))

          (--> |generic_association|
               (ALT (SEQ |type_name| \: |assignment_expression|) 
                    (SEQ DEFAULT \: |assignment_expression|)))

          (--> |postfix_expression|
               (SEQ |postfix_expression_head| (REP |postfix_expression_item|)))

          (--> |postfix_expression_head|
               (ALT |simple_primary_expression|
                    (SEQ \( (ALT (SEQ |expression| \))
                                 (SEQ |type_name|  \)
                                      { |initializer_list| (OPT \,)})
                                 \)))))
          
          (--> |postfix_expression_item|
               (ALT (SEQ [ |expression| ]) 
                    (SEQ \( (OPT |argument_expression_list|) \)) 
                    (SEQ |.| IDENTIFIER)
                    (SEQ PTR_OP IDENTIFIER) 
                    (SEQ INC_OP)
                    (SEQ DEC_OP)))

          (--> |argument_expression_list|
               (SEQ |assignment_expression| (REP \, |assignment_expression|)))

          (--> |simple_unary_expression|
               (ALT (SEQ INC_OP |unary_expression|) 
                    (SEQ DEC_OP |unary_expression|)
                    (SEQ |unary_operator| |cast_expression|) 
                    (SEQ SIZEOF |sizeof_argument|) 
                    (SEQ ALIGNOF \( |type_name| \))))

          (--> |sizeof_argument|
               (ALT (seq |simple_unary_expression|)
                    (seq (ALT |simple_primary_expression|
                              (SEQ \( (ALT (SEQ |expression| \))
                                           (SEQ |type_name| \) (OPT { |initializer_list| (OPT \,)}))
                                           \))))
                         (REP |postfix_expression_item|))))
          
          (--> |unary_expression|
               (ALT |postfix_expression|
                    |simple_unary_expression|))

          (--> |unary_operator|
               (ALT & * + - ~ !))

          (--> |cast_expression|
               (ALT |simple_unary_expression|
                    |simple_primary_expression|
                    (SEQ \( (ALT (SEQ |expression|)
                                 (SEQ |type_name| \) (ALT (SEQ  { |initializer_list| (OPT \,) })
                                                          |cast_expression|))\)))))

          (--> |multiplicative_expression|
               (SEQ |cast_expression| (REP (ALT * / %) |cast_expression|)))

          (--> |additive_expression|
               (SEQ |multiplicative_expression| (REP (ALT + -) |multiplicative_expression|)))

          (--> |shift_expression|
               (SEQ |additive_expression| (REP (ALT LEFT_OP RIGHT_OP) |additive_expression|)))

          (--> |relational_expression|
               (SEQ |shift_expression| (REP (ALT < > LE_OP GE_OP)  |shift_expression|)))

          (--> |equality_expression|
               (SEQ |relational_expression| (REP (ALT EQ_OP NE_OP) |relational_expression|)))

          (--> |and_expression|
               (SEQ |equality_expression| (REP & |equality_expression|)))

          (--> |exclusive_or_expression|
               (SEQ |and_expression| (REP ^ |and_expression|)))

          (--> |inclusive_or_expression|
               (SEQ |exclusive_or_expression| (REP \| |exclusive_or_expression|)))

          (--> |logical_and_expression|
               (SEQ |inclusive_or_expression| (REP AND_OP |inclusive_or_expression|)))

          (--> |logical_or_expression|
               (SEQ |logical_and_expression| (REP OR_OP |logical_and_expression|)))

          (--> |conditional_expression|
               (SEQ |logical_or_expression| (OPT ? |expression| \: |conditional_expression|)))

          #-(and)
          (--> |assignment_expression|
               (ALT |conditional_expression|
                    (SEQ |unary_expression| |assignment_operator| |assignment_expression|)))

          (--> |assignment_expression|
               (SEQ |conditional_expression|
                    (OPT (SEQ |assignment_operator| |assignment_expression|))
                    :action (progn #|check the conditional_expression is actually unary_expression|#)))

          (--> |assignment_operator|
               (ALT = MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
                    XOR_ASSIGN OR_ASSIGN))

          (--> |expression|
               (SEQ |assignment_expression| (REP \, |assignment_expression|)))

          (--> |constant_expression|
               |conditional_expression|)

          (--> |declaration|
               (ALT (SEQ |declaration_specifiers| (OPT |init_declarator_list|) \;)
                    |static_assert_declaration|))
          
          (--> |type_qualifier|
               (ALT CONST RESTRICT VOLATILE ATOMIC)) 

          (--> |atomic_type_specifier|
               (SEQ ATOMIC \( |type_name| \)))

          (--> |alignment_specifier|
               (SEQ ALIGNAS \( (OPT |type_name| |constant_expression|) \)))

          (--> |function_specifier|
               (ALT INLINE NORETURN))

          (--> |storage_class_specifier|
               (ALT TYPEDEF EXTERN STATIC THREAD_LOCAL AUTO REGISTER))

          (--> |specifier_qualifier|
               (ALT CONST
                    RESTRICT
                    VOLATILE
                    (SEQ ATOMIC (OPT \( |type_name| \)))))
          
          (--> |declaration_specifier|
               (ALT
                |alignment_specifier|
                |function_specifier|
                |storage_class_specifier|
                |specifier_qualifier|))
          
          (--> |declaration_specifiers|
               (SEQ |declaration_specifier| (REP |declaration_specifier|)))

          (--> |init_declarator_list|
               (SEQ |init_declarator| (REP \, |init_declarator|)))

          (--> |init_declarator|
               (SEQ |declarator| (OPT = |initializer|)))


          (--> |type_specifier|
               (ALT VOID CHAR SHORT INT LONG FLOAT DOUBLE SIGNED UNSIGNED BOOL
                    COMPLEX IMAGINARY
                    |atomic_type_specifier|
                    |struct_or_union_specifier|
                    |enum_specifier|
                    TYPEDEF_NAME))

          (--> |struct_or_union_specifier|
               (SEQ |struct_or_union| (ALT (SEQ { |struct_declaration_list| })
                                           (SEQ IDENTIFIER (OPT { |struct_declaration_list| })) )))

          (--> |struct_or_union|
               (ALT STRUCT UNION))

          (--> |struct_declaration_list|
               (SEQ |struct_declaration| (REP  |struct_declaration|)))

          (--> |struct_declaration|
               (ALT (SEQ |specifier_qualifier_list| (OPT |struct_declarator_list|) \;) 
                    |static_assert_declaration|))

          (--> |specifier_qualifier_list|
               (SEQ |specifier_qualifier| (REP |specifier_qualifier|)))

          (--> |struct_declarator_list|
               (SEQ |struct_declarator| (REP \, |struct_declarator|)))

          (--> |struct_declarator|
               (ALT (SEQ \: |constant_expression|) 
                    (SEQ |declarator| (OPT \: |constant_expression|))))

          (--> |enum_specifier|
               ENUM (ALT (SEQ { |enumerator_list| (OPT \,)}) 
                         (SEQ IDENTIFIER (OPT { |enumerator_list| (OPT \,)})) ))

          (--> |enumerator_list|
               (SEQ |enumerator| (REP \, |enumerator|)))

          (--> |enumeration_constant|
               IDENTIFIER)

          (--> |enumerator|
               (SEQ |enumeration_constant| (OPT = |constant_expression|)))




          (--> |declarator|
               (SEQ (OPT |pointer|) |direct_declarator|))

          (--> |direct_declarator|
               (SEQ |simple_direct_declarator|
                    (REP |direct_declarator_item|)))

          (--> |simple_direct_declarator|
               (ALT (SEQ IDENTIFIER )
                    (SEQ \( (OPT (ALT (SEQ |declarator| (REP \, IDENTIFIER)
                                           :action (progn #|check declarator is identifier
                                                     if we have rep identifiers.|#))
                                      (SEQ |parameter_type_list|)))
                         \))))

          (--> |direct_declarator_item|
               (ALT (SEQ \( (OPT |direct_declarator_in_parentheses|) \))
                    (SEQ \[ (OPT (ALT STAR
                                      |direct_declarator_in_brackets|)) \])))

          (--> |direct_declarator_in_parentheses|
               (ALT (SEQ |identifier_list|)
                    (SEQ |parameter_type_list|)))

          (--> |direct_declarator_in_brackets|
               (ALT (SEQ |assignment_expression|)
                    (SEQ STATIC (OPT |type_qualifier_list|) |assignment_expression|)
                    (SEQ |type_qualifier_list| (OPT (ALT ;; * ;; TODO
                                                         (SEQ (OPT STATIC) |assignment_expression|))))))

          

          (--> |parameter_type_list|
               (SEQ |parameter_list| (OPT \, ELLIPSIS)))

          (--> |parameter_list|
               |parameter_declaration| (REP \, |parameter_declaration|))

          (--> |identifier_list|
               (SEQ IDENTIFIER (REP \, IDENTIFIER)))

          (--> |type_qualifier_list|
               |type_qualifier| (REP |type_qualifier|))

          




          (--> |parameter_declaration|
               (SEQ |declaration_specifiers|
                    (OPT |declarator__or__abstract_declarator|)))

          (--> |declarator__or__abstract_declarator|
               (alt |direct_declarator__or__direct_abstract_declarator|
                    (seq |pointer| (opt |direct_declarator__or__direct_abstract_declarator|))))

          (--> |direct_declarator__or__direct_abstract_declarator|
               (seq |simple_direct_declarator__or__simple_direct_abstract_declarator|
                    (rep |direct_declarator_item__or__direct_abstract_declarator_item|)))
          
          
          (--> |simple_direct_declarator__or__simple_direct_abstract_declarator|
               (ALT (SEQ IDENTIFIER)
                    (SEQ \( (OPT (ALT (seq |declarator__or__abstract_declarator|
                                           (REP \, IDENTIFIER)
                                           :action (progn #|check declarator is identifier
                                                     if we have rep identifiers.|#))
                                      (seq |parameter_type_list|)))
                         \))
                    |bracket_direct_abstract_declarator|))
          
          (--> |direct_declarator_item__or__direct_abstract_declarator_item|
               (ALT (SEQ \( (OPT |direct_declarator_in_parentheses|) \))
                    |bracket_direct_abstract_declarator|))

          (--> |bracket_direct_abstract_declarator|
               (SEQ \[
                    (OPT (ALT (SEQ STAR)
                              (SEQ |direct_declarator_in_brackets|
                                   :action (progn #| check no [*] |#))))
                    \]))




          
          (--> |pointer|
               (SEQ * (OPT (ALT (SEQ |type_qualifier_list| (OPT |pointer|))
                                |pointer|))))
          
          
          (--> |type_name|
               (SEQ |specifier_qualifier_list| (OPT |abstract_declarator|)))


          


          (--> |abstract_declarator|
               (ALT (SEQ |pointer| (OPT |direct_abstract_declarator|))
                    (SEQ |direct_abstract_declarator|)))

          (--> |direct_abstract_declarator|
               (SEQ |simple_direct_abstract_declarator|
                    (REP |direct_abstract_declarator_item|)))
          
          (--> |simple_direct_abstract_declarator|
               (ALT (SEQ \( (OPT |direct_abstract_declarator_in_parentheses|) \))
                    |bracket_direct_abstract_declarator|))

          (--> |direct_abstract_declarator_in_parentheses|
               (ALT (SEQ |abstract_declarator|)
                    (SEQ |parameter_type_list|)))

          (--> |direct_abstract_declarator_item|
               (ALT (SEQ \( (OPT |parameter_type_list|) \))
                    |bracket_direct_abstract_declarator|))

          
          
          
          

          (--> |initializer|
               (ALT (SEQ { |initializer_list| (OPT \,)}) 
                    |assignment_expression|))

          (--> |initializer_list|
               (SEQ (ALT (SEQ |designation| |initializer|)
                         (SEQ |initializer|))
                    (REP \, |initializer_list|)))

          (--> |designation|
               (SEQ |designator_list| =))

          (--> |designator_list|
               |designator| (REP |designator|))

          (--> |designator|
               (ALT (SEQ [ |constant_expression| ]) 
                    (SEQ |.| IDENTIFIER)))

          (--> |static_assert_declaration|
               (SEQ STATIC_ASSERT \( |constant_expression| \, STRING_LITERAL \) \;))

          (--> |statement|
               (ALT |simple_labeled_statement|
                    |expression_statement_or_label|
                    |compound_statement|
                    |selection_statement|
                    |iteration_statement|
                    |jump_statement|))


          (--> |expression_statement_or_label|
               (ALT \;
                    (SEQ |expression| (ALT (SEQ \;) ; expression_statement
                                           (SEQ \: |statement|))))) ; label
          
          (--> |expression_statement|
               (ALT \;
                    (SEQ |expression| \;)))
          
          (--> |simple_labeled_statement|
               (ALT (SEQ CASE |constant_expression| \: |statement|)
                    (SEQ DEFAULT \: |statement|)))

          (--> |compound_statement|
               (SEQ { (OPT |block_item_list|) }))

          (--> |block_item_list|
               |block_item| (REP |block_item|))

          (--> |block_item|
               (ALT |declaration|
                    |statement|))


          (--> |selection_statement|
               (ALT (SEQ IF \( |expression| \) |statement| (OPT ELSE |statement|)) 
                    (SEQ SWITCH \( |expression| \) |statement|)))

          (--> |iteration_statement|
               (ALT (SEQ WHILE \( |expression| \) |statement|) 
                    (SEQ DO |statement| WHILE \( |expression| \) \;)
                    (SEQ FOR \( (ALT (SEQ |expression_statement|  |expression_statement|  (OPT  |expression|)  \) |statement|)
                                     (SEQ |declaration| |expression_statement| (OPT |expression|) \) |statement|)))))

          (--> |jump_statement|
               (ALT (SEQ GOTO IDENTIFIER \;) 
                    (SEQ CONTINUE \;) 
                    (SEQ BREAK \;) 
                    (SEQ RETURN (OPT  |expression|) \;)))

          (--> |translation_unit|
               (SEQ |external_declaration| (REP |external_declaration|)))

          (--> |external_declaration|
               (ALT |static_assert_declaration|
                    (SEQ |declaration_specifiers|
                         |declarator|
                         (ALT
                          (SEQ = |initializer| (REP \, |init_declarator|) \;) ; declaration
                          (SEQ (OPT |declaration_list|) ; function_definition
                               |compound_statement|)
                          ))))


          (--> |declaration_list|
               (SEQ  |declaration| (REP |declaration|)))))


(defun test/parse-stream (tokens)
  (let ((*scanner* (make-instance 'pre-scanned-scanner :tokens tokens)))
    (loop
      :until (scanner-end-of-source-p *scanner*)
      :collect  (parse-c11 *scanner*))))


;;;; THE END ;;;;
