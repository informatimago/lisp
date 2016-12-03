%%

primary_expression
: simple_primary_expression
| '(' expression ')'
;


simple_primary_expression
: IDENTIFIER
| constant
| string
| generic_selection
;

postfix_expression
: primary_expression
| postfix_expression '[' expression ']'
| postfix_expression '(' ')'
| postfix_expression '(' argument_expression_list ')'
| postfix_expression '.' IDENTIFIER
| postfix_expression PTR_OP IDENTIFIER
| postfix_expression INC_OP
| postfix_expression DEC_OP
| '(' type_name ')' '{' initializer_list '}'
| '(' type_name ')' '{' initializer_list ',' '}'
;






postfix_expression
: postfix_expression_head postfix_expression_item*
;

postfix_expression_head
:  primary_expression
| '(' type_name ')' '{' initializer_list ','? '}'
;

postfix_expression_item
| '[' expression ']'
| '(' ')'
| '(' argument_expression_list ')'
| '.' IDENTIFIER
| PTR_OP IDENTIFIER
| INC_OP
| DEC_OP
;

unary_expression
: postfix_expression
| simple_unary_expression
;

simple_unary_expression
: unary_operator cast_expression
| INC_OP unary_expression
| DEC_OP unary_expression
| SIZEOF unary_expression
| SIZEOF '(' type_name ')'
| ALIGNOF '(' type_name ')'
;

cast_expression
: simple_unary_expression
| simple_primary_expression postfix_expression_item*
| '(' expression ')' postfix_expression_item*
| '(' type_name ')' cast_expression_tail
;

cast_expression_tail
: '{' initializer_list ','? '}'  postfix_expression_item*
|  cast_expression
;



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
