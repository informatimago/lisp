;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               c++.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;; C++ grammar.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-02 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")
    ;; hex-quad:
    ;; 	hexadecimal-digit hexadecimal-digit hexadecimal-digit hexadecimal-digit
    ;;
    ;; universal-character-name:
    ;; 	\u hex-quad
    ;; 	\U hex-quad hex-quad
    ;;
    ;; preprocessing-token:
    ;; 	header-name
    ;; 	identifier
    ;; 	pp-number
    ;; 	character-literal
    ;; 	string-literal
    ;; 	preprocessing-op-or-punc
    ;; 	each non-white-space character that cannot be one of the above
    ;;
    ;; token:
    ;; 	identifier
    ;; 	keyword
    ;; 	literal
    ;; 	operator
    ;; 	punctuator
    ;;
    ;; header-name:
    ;; 	<h-char-sequence>
    ;; 	"q-char-sequence"
    ;;
    ;; h-char-sequence:
    ;; 	h-char
    ;; 	h-char-sequence h-char
    ;;
    ;; h-char:
    ;; 	any member of the source character set except
    ;; 		new-line and >
    ;;
    ;; q-char-sequence:
    ;; 	q-char
    ;; 	q-char-sequence q-char
    ;;
    ;; q-char:
    ;; 	any member of the source character set except
    ;; 		new-line and "
    ;;
    ;; pp-number:
    ;; 	digit
    ;; 	. digit
    ;; 	pp-number digit
    ;; 	pp-number nondigit
    ;; 	pp-number e sign
    ;; 	pp-number E sign
    ;; 	pp-number .
    ;;
    ;; identifier:
    ;; 	nondigit
    ;; 	identifier nondigit
    ;; 	identifier digit
    ;;
    ;; nondigit: one of
    ;; 	universal-character-name
    ;; 	_ a b c d e f g h i j k l m
    ;; 	  n o p q r s t u v w x y z
    ;; 	  A B C D E F G H I J K L M
    ;; 	  N O P Q R S T U V W X Y Z
    ;;
    ;; digit: one of
    ;; 	0 1 2 3 4 5 6 7 8 9
    ;;
    ;; preprocessing-op-or-punc: one of
    ;; 	{	}	[	]	#	##	(	)
    ;; 	<:	:>	<%	%>	%:	%:%:	;	:	...
    ;; 	new	delete	?	::	.	.*
    ;; 	+	-	*	/	%	^	&	|	~
    ;; 	!	=	<	>	+=	-=	*=	/=	%=
    ;; 	^=	&=	|=	<<	>>	>>=	<<=	==	!=
    ;; 	<=	>=	&&	||	++	--	,	->*	->
    ;; 	and	and_eq	bitand	bitor	compl	not	not_eq
    ;; 	or	or_eq	xor	xor_eq
    ;;
    ;; literal:
    ;; 	integer-literal
    ;; 	character-literal
    ;; 	floating-literal
    ;; 	string-literal
    ;; 	boolean-literal
    ;;
    ;; integer-literal:
    ;; 	decimal-literal integer-suffixopt
    ;; 	octal-literal integer-suffixopt
    ;; 	hexadecimal-literal integer-suffixopt
    ;;
    ;; decimal-literal:
    ;; 	nonzero-digit
    ;; 	decimal-literal digit
    ;;
    ;; octal-literal:
    ;; 	0
    ;; 	octal-literal octal-digit
    ;;
    ;; hexadecimal-literal:
    ;; 	0x hexadecimal-digit
    ;; 	0X hexadecimal-digit
    ;; 	hexadecimal-literal hexadecimal-digit
    ;;
    ;; nonzero-digit: one of
    ;; 	1  2  3  4  5  6  7  8  9
    ;;
    ;; octal-digit: one of
    ;; 	0  1  2  3  4  5  6  7
    ;;
    ;; hexadecimal-digit: one of
    ;; 	0  1  2  3  4  5  6  7  8  9
    ;; 	a  b  c  d  e  f
    ;; 	A  B  C  D  E  F
    ;;
    ;; integer-suffix:
    ;; 	unsigned-suffix long-suffixopt
    ;; 	long-suffix unsigned-suffixopt
    ;;
    ;; unsigned-suffix: one of
    ;; 	u  U
    ;;
    ;; long-suffix: one of
    ;; 	l  L
    ;;
    ;; character-literal:
    ;; 	'c-char-sequence'
    ;; 	L'c-char-sequence'
    ;;
    ;; c-char-sequence:
    ;; 	c-char
    ;; 	c-char-sequence c-char
    ;;
    ;; c-char:
    ;; 	any member of the source character set except
    ;; 		the single-quote  ', backslash  \, or new-line character
    ;; 	escape-sequence
    ;; 	universal-character-name
    ;;
    ;; escape-sequence:
    ;; 	simple-escape-sequence
    ;; 	octal-escape-sequence
    ;; 	hexadecimal-escape-sequence
    ;;
    ;; simple-escape-sequence: one of
    ;; 	\'  \"  \?  \\
    ;; 	\a  \b  \f  \n  \r  \t  \v
    ;;
    ;; octal-escape-sequence:
    ;; 	\ octal-digit
    ;; 	\ octal-digit octal-digit
    ;; 	\ octal-digit octal-digit octal-digit
    ;;
    ;; hexadecimal-escape-sequence:
    ;; 	\x hexadecimal-digit
    ;; 	hexadecimal-escape-sequence hexadecimal-digit
    ;;
    ;; floating-literal:
    ;; 	fractional-constant exponent-partopt floating-suffixopt
    ;; 	digit-sequence exponent-part floating-suffixopt
    ;;
    ;; fractional-constant:
    ;; 	digit-sequenceopt . digit-sequence
    ;; 	digit-sequence .
    ;;
    ;; exponent-part:
    ;; 	e signopt digit-sequence
    ;; 	E signopt digit-sequence
    ;;
    ;; sign: one of
    ;; 	+  -
    ;;
    ;; digit-sequence:
    ;; 	digit
    ;; 	digit-sequence digit
    ;;
    ;; floating-suffix: one of
    ;; 	f  l  F  L
    ;;
    ;; string-literal:
    ;; 	"s-char-sequenceopt"
    ;; 	L"s-char-sequenceopt"
    ;;
    ;; s-char-sequence:
    ;; 	s-char
    ;; 	s-char-sequence s-char
    ;;
    ;; s-char:
    ;; 	any member of the source character set except
    ;; 		the double-quote ", backslash \, or new-line character
    ;; 	escape-sequence
    ;; 	universal-character-name
    ;;
    ;; boolean-literal:
    ;; 	false
    ;; 	true


(translation-unit
 ::=
 (  declaration-seq[opt]  ))

(primary-expression
 ::=
 (  literal  )
 (  "this"  )
 (  "(" expression ")"  )
 (  id-expression  ))

(id-expression
 ::=
 (  unqualified-id  )
 (  qualified-id  ))

(unqualified-id
 ::=
 (  identifier  )
 (  operator-function-id  )
 (  conversion-function-id  )
 (  "~" class-name  )
 (  template-id  ))

;; (qualify name name name (template name arg...) name ~ name)
;; (absolute-qualify name name name (template name arg...) name ~ name)

(qualified-id
 ::=
 (  "::"[opt] nested-name-specifier "template"[opt] unqualified-id  )
 (  "::" identifier  )
 (  "::" operator-function-id  )
 (  "::" template-id  ))

(nested-name-specifier
 ::=
 (  class-or-namespace-name "::" nested-name-specifier[opt]   )
 (  class-or-namespace-name "::" "template" nested-name-specifier  ))

(class-or-namespace-name
 ::=
 (  class-name  )
 (  namespace-name  ))

(postfix-expression
 ::=
 (  primary-expression  )
 (  Postfix-expression "[" expression "]"  )
 (  postfix-expression "(" expression-list[opt] ")"  )
 (  simple-type-specifier "(" expression-list[opt] ")"  )
 (  "typename" "::"[opt]  nested-name-specifier identifier "(" expression-list[opt] ")"  )
 (  "typename" "::"[opt]  nested-name-specifier "template"[opt]  template-id "(" expression-list[opt] ")"  )
 (  postfix-expression "." "template"[opt] id-expression  )
 (  postfix-expression "->" "template"[opt] id-expression  )
 (  postfix-expression "." pseudo-destructor-name  )
 (  postfix-expression "->" pseudo-destructor-name  )
 (  postfix-expression "++"  )
 (  postfix-expression "--"  )
 (  "dynamic_cast" "<" type-id ">" "(" expression ")"  )
 (  "static_cast" "<" type-id ">" "(" expression ")"  )
 (  "reinterpret_cast" "<" type-id ">" "(" expression ")"  )
 (  "const_cast" "<" type-id ">" "(" expression ")"  )
 (  "typeid" "(" expression ")"  )
 (  "typeid" "(" type-id ")"  ))

(expression-list
 ::=
 (  assignment-expression  )
 (  expression-list "," assignment-expression  ))

(pseudo-destructor-name
 ::=
 (  "::"[opt]  nested-name-specifier[opt] type-name "::" "~" type-name  )
 (  "::"[opt]  nested-name-specifier "template" template-id "::" "~" type-name  )
 (  "::"[opt]  nested-name-specifier[opt] "~" type-name  ))

(unary-expression
 ::=
 (  postfix-expression  )
 (  "++"  cast-expression  )
 (  "--"  cast-expression  )
 (  unary-operator cast-expression  )
 (  "sizeof" unary-expression  )
 (  "sizeof" "(" type-id ")"  )
 (  new-expression  )
 (  delete-expression  ))

(unary-operator
 ::=
 "*"  "&"  "+"  "-"  "!"  "~")

(new-expression
 ::=
 (  "::"[opt] "new" new-placement[opt] new-type-id new-initializer[opt]  )
 (  "::"[opt] "new" new-placement[opt] "(" type-id ")" new-initializer[opt]  ))

(new-placement
 ::=
 (  "(" expression-list ")"  ))

(new-type-id
 ::=
 (  type-specifier-seq new-declarator[opt]  ))

(new-declarator
 ::=
 (  ptr-operator new-declarator[opt]  )
 (  direct-new-declarator  ))

(direct-new-declarator
 ::=
 (  "[" expression "]"  )
 (  direct-new-declarator "[" constant-expression "]"  ))

(new-initializer
 ::=
 (  "(" expression-list[opt] ")"  ))

(delete-expression
 ::=
 (  "::"[opt] "delete" cast-expression  )
 (  "::"[opt] "delete" "[" "]" cast-expression  ))

(cast-expression
 ::=
 (  unary-expression  )
 (  "(" type-id ")" cast-expression  ))

(pm-expression
 ::=
 (  cast-expression  )
 (  pm-expression ".*" cast-expression  )
 (  pm-expression "->*" cast-expression  ))

(multiplicative-expression
 ::=
 (  pm-expression  )
 (  multiplicative-expression "*" pm-expression  )
 (  multiplicative-expression "/" pm-expression  )
 (  multiplicative-expression "%" pm-expression  ))

(additive-expression
 ::=
 (  multiplicative-expression  )
 (  additive-expression "+" multiplicative-expression  )
 (  additive-expression "-" multiplicative-expression  ))

(shift-expression
 ::=
 (  additive-expression  )
 (  shift-expression "<<" additive-expression  )
 (  shift-expression ">>" additive-expression  ))

(relational-expression
 ::=
 (  shift-expression  )
 (  relational-expression "<" shift-expression  )
 (  relational-expression ">" shift-expression  )
 (  relational-expression "<=" shift-expression  )
 (  relational-expression ">=" shift-expression  ))

(equality-expression
 ::=
 (  relational-expression  )
 (  equality-expression "==" relational-expression  )
 (  equality-expression "!=" relational-expression  ))

(and-expression
 ::=
 (  equality-expression  )
 (  and-expression "&" equality-expression  ))

(exclusive-or-expression
 ::=
 (  and-expression  )
 (  exclusive-or-expression "^" and-expression  ))

(inclusive-or-expression
 ::=
 (  exclusive-or-expression  )
 (  inclusive-or-expression "|" exclusive-or-expression  ))

(logical-and-expression
 ::=
 (  inclusive-or-expression  )
 (  logical-and-expression "&&" inclusive-or-expression  ))

(logical-or-expression
 ::=
 (  logical-and-expression  )
 (  logical-or-expression "||" logical-and-expression  ))

(conditional-expression
 ::=
 (  logical-or-expression  )
 (  logical-or-expression "?" expression ":" assignment-expression  ))

(assignment-expression
 ::=
 (  conditional-expression  )
 (  logical-or-expression assignment-operator assignment-expression  )
 (  throw-expression  ))

(assignment-operator
 ::=
 "="  "*="  "/="  "%="   "+="  "-="  ">>="  "<<="
 "&="  "^="  "|="   )

(expression
 ::=
 (  assignment-expression  )
 (  expression "," assignment-expression  ))

(constant-expression
 ::=
 (  conditional-expression  ))

(statement
 ::=
 (  labeled-statement  )
 (  expression-statement  )
 (  compound-statement  )
 (  selection-statement  )
 (  iteration-statement  )
 (  jump-statement  )
 (  declaration-statement  )
 (  try-block  ))

(labeled-statement
 ::=
 (  identifier ":" statement  )
 (  "case" constant-expression ":" statement  )
 (  "default" ":" statement  ))

(expression-statement
 ::=
 (  expression[opt] ";"  ))

(compound-statement
 ::=
 (  "{" statement-seq[opt] "}"  ))

(statement-seq
 ::=
 (  statement  )
 (  statement-seq statement  ))

(selection-statement
 ::=
 (  "if" "(" condition ")" statement  )
 (  "if" "(" condition ")" statement "else" statement  )
 (  "switch" "(" condition ")" statement  ))

(condition
 ::=
 (  expression  )
 (  type-specifier-seq declarator "=" assignment-expression  ))

(iteration-statement
 ::=
 (  "while" "(" condition ")" statement  )
 (  "do" statement  "while" "(" expression ")" ";"  )
 (  "for" "(" for-init-statement condition[opt] ";" expression[opt] ")" statement  ))

(for-init-statement
 ::=
 (  expression-statement  )
 (  simple-declaration  ))

(jump-statement
 ::=
 (  "break" ";"  )
 (  "continue" ";"  )
 (  "return" expression[opt] ";"  )
 (  "goto" identifier ";"  ))

(declaration-statement
 ::=
 (  block-declaration  ))

(declaration-seq
 ::=
 (  declaration  )
 (  declaration-seq declaration  ))

(declaration
 ::=
 (  block-declaration  )
 (  function-definition  )
 (  template-declaration  )
 (  explicit-instantiation  )
 (  explicit-specialization  )
 (  linkage-specification  )
 (  namespace-definition  ))

(block-declaration
 ::=
 (  simple-declaration  )
 (  asm-definition  )
 (  namespace-alias-definition  )
 (  using-declaration  )
 (  using-directive  ))

(simple-declaration
 ::=
 (  decl-specifier-seq[opt] init-declarator-list[opt] ";"  ))

(decl-specifier
 ::=
 (  storage-class-specifier  )
 (  type-specifier  )
 (  function-specifier  )
 (  "friend"  )
 (  "typedef"  ))

(decl-specifier-seq
 ::=
 (  decl-specifier-seq[opt] decl-specifier  ))

(storage-class-specifier
 ::=
 (  "auto"  )
 (  "register"  )
 (  "static"  )
 (  "extern"  )
 (  "mutable"  ))

(function-specifier
 ::=
 (  "inline"  )
 (  "virtual"  )
 (  "explicit"  ))

(typedef-name
 ::=
 (  identifier  ))

(type-specifier
 ::=
 (  simple-type-specifier  )
 (  class-specifier  )
 (  enum-specifier  )
 (  elaborated-type-specifier  )
 (  cv-qualifier  ))

(simple-type-specifier
 ::=
 (  "::"[opt] nested-name-specifier[opt] type-name  )
 (  "::"[opt] nested-name-specifier "template" template-id  )
 (  "char"  )
 (  "wchar"_"t"  )
 (  "bool"  )
 (  "short"  )
 (  "int"  )
 (  "long"  )
 (  "signed"  )
 (  "unsigned"  )
 (  "float"  )
 (  "double"  )
 (  "void"  ))

(type-name
 ::=
 (  class-name  )
 (  enum-name  )
 (  typedef-name  ))

(elaborated-type-specifier
 ::=
 (  class-key "::"[opt] nested-name-specifier[opt] identifier  )
 (  "enum" "::"[opt] nested-name-specifier[opt] identifier  )
 (  "typename" "::"[opt]  nested-name-specifier identifier  )
 (  "typename" "::"[opt]  nested-name-specifier "template"[opt] template-id  ))

(enum-name
 ::=
 (  identifier  ))

(enum-specifier
 ::=
 (  "enum" identifier[opt] "{" enumerator-list[opt] "}"  ))

(enumerator-list
 ::=
 (  enumerator-definition  )
 (  enumerator-list "," enumerator-definition  ))

(enumerator-definition
 ::=
 (  enumerator  )
 (  enumerator "=" constant-expression  ))

(enumerator
 ::=
 (  identifier  ))

(namespace-name
 ::=
 (  original-namespace-name  )
 (  namespace-alias  ))

(original-namespace-name
 ::=
 (  identifier  ))

(namespace-definition
 ::=
 (  named-namespace-definition  )
 (  unnamed-namespace-definition  ))

(named-namespace-definition
 ::=
 (  original-namespace-definition  )
 (  extension-namespace-definition  ))

(original-namespace-definition
 ::=
 (  "namespace" identifier "{" namespace-body "}"  ))

(extension-namespace-definition
 ::=
 (  "namespace" original-namespace-name  "{" namespace-body "}"  ))

(unnamed-namespace-definition
 ::=
 (  "namespace" "{" namespace-body "}"  ))

(namespace-body
 ::=
 (  declaration-seq[opt]  ))

(namespace-alias
 ::=
 (  identifier  ))

(namespace-alias-definition
 ::=
 (  "namespace" identifier "=" qualified-namespace-specifier ";"  ))

(qualified-namespace-specifier
 ::=
 (  "::"[opt] nested-name-specifier[opt] namespace-name  ))

(using-declaration
 ::=
 (  "using" "typename"[opt] "::"[opt] nested-name-specifier unqualified-id ";"  )
 (  "using" "::"  unqualified-id ";"  ))

(using-directive
 ::=
 (  "using"  "namespace"  "::"[opt] nested-name-specifier[opt] namespace-name ";"  ))

(asm-definition
 ::=
 (  "asm" "(" string-literal ")" ";"  ))

(linkage-specification
 ::=
 (  "extern" string-literal "{" declaration-seq[opt] "}"  )
 (  "extern" string-literal declaration  ))

(init-declarator-list
 ::=
 (  init-declarator  )
 (  init-declarator-list "," init-declarator  ))

(init-declarator
 ::=
 (  declarator initializer[opt]  ))

(declarator
 ::=
 (  direct-declarator  )
 (  ptr-operator declarator  ))

(direct-declarator
 ::=
 (  declarator-id  )
 (  direct-declarator "(" parameter-declaration-clause ")" cv-qualifier-seq[opt] exception-specification[opt]  )
 (  direct-declarator "[" constant-expression[opt] "]"  )
 (  "(" declarator ")"  ))


(ptr-operator
 ::=
 ( "*" cv-qualifier-seq[opt] )
 ( "&" ::[opt]  nested-name-specifier "*" cv-qualifier-seq[opt] ))

(cv-qualifier-seq
 ::=
 (  cv-qualifier cv-qualifier-seq[opt]  ))

(cv-qualifier
 ::=
 (  "const"  )
 (  "volatile"  ))


(declarator-id
 ::=
 (  id-expression  )
 (  "::"[opt] nested-name-specifier[opt] type-name  ))

(type-id
 ::=
 (  type-specifier-seq abstract-declarator[opt]  ))

(type-specifier-seq
 ::=
 (  type-specifier type-specifier-seq[opt]  ))

(abstract-declarator
 ::=
 (  ptr-operator abstract-declarator[opt]  )
 (  direct-abstract-declarator  ))

(direct-abstract-declarator
 ::=
 (  direct-abstract-declarator[opt]  )
 (  "(" parameter-declaration-clause ")" cv-qualifier-seq[opt] exception-specification[opt]  )
 (  direct-abstract-declarator[opt] "[" constant-expression[opt] "]"  )
 (  "(" abstract-declarator ")"  ))

(parameter-declaration-clause
 ::=
 (  parameter-declaration-list[opt] "..." [opt]  )
 (  parameter-declaration-list "," "..."  ))

(parameter-declaration-list
 ::=
 (  parameter-declaration  )
 (  parameter-declaration-list "," parameter-declaration  ))

(parameter-declaration
 ::=
 (  decl-specifier-seq declarator  )
 (  decl-specifier-seq declarator "=" assignment-expression  )
 (  decl-specifier-seq abstract-declarator[opt]  )
 (  decl-specifier-seq abstract-declarator[opt] "=" assignment-expression  ))

(function-definition
 ::=
 (  decl-specifier-seq[opt] declarator ctor-initializer[opt] function-body  )
 (  decl-specifier-seq[opt] declarator function-try-block  ))

(function-body
 ::=
 (  compound-statement  ))

(initializer
 ::=
 (  "=" initializer-clause  )
 (  "(" expression-list ")"  ))

(initializer-clause
 ::=
 (  assignment-expression  )
 (  "{" initializer-list ","[opt] "}"  )
 (  "{" "}"  ))

(initializer-list
 ::=
 (  initializer-clause  )
 (  initializer-list "," initializer-clause  ))

(class-name
 ::=
 (  identifier  )
 (  template-id  ))

(class-specifier
 ::=
 (  class-head "{" member-specification[opt] "}"  ))

(class-head
 ::=
 (  class-key identifier[opt] base-clause[opt]  )
 (  class-key nested-name-specifier identifier base-clause[opt]  )
 (  class-key nested-name-specifier[opt] template-id base-clause[opt]  ))

(class-key
 ::=
 (  "class"  )
 (  "struct"  )
 (  "union"  ))

(member-specification
 ::=
 (  member-declaration member-specification[opt]  )
 (  access-specifier ":" member-specification[opt]  ))

(member-declaration
 ::=
 (  decl-specifier-seq[opt] member-declarator-list[opt] ";"  )
 (  function-definition ";"[opt]  )
 (  "::"[opt] nested-name-specifier "template"[opt] unqualified-id ";"  )
 (  using-declaration  )
 (  template-declaration  ))

(member-declarator-list
 ::=
 (  member-declarator  )
 (  member-declarator-list "," member-declarator  ))

(member-declarator
 ::=
 (  declarator pure-specifier[opt]  )
 (  declarator constant-initializer[opt]  )
 (  identifier[opt] ":" constant-expression  ))

(pure-specifier
 ::=
 (  "=" "0"  ))

(constant-initializer
 ::=
 (  "=" constant-expression  ))

(base-clause
 ::=
 (  ":" base-specifier-list  ))

(base-specifier-list
 ::=
 (  base-specifier  )
 (  base-specifier-list "," base-specifier  ))

(base-specifier
 ::=
 (  "::"[opt] nested-name-specifier[opt] class-name  )
 (  "virtual" access-specifier[opt] "::"[opt] nested-name-specifier[opt] class-name  )
 (  access-specifier "virtual"[opt] "::"[opt] nested-name-specifier[opt] class-name  ))

(access-specifier
 ::=
 (  "private"  )
 (  "protected"  )
 (  "public"  ))

(conversion-function-id
 ::=
 (  operator conversion-type-id  ))

(conversion-type-id
 ::=
 (  type-specifier-seq conversion-declarator[opt]  ))

(conversion-declarator
 ::=
 (  ptr-operator conversion-declarator[opt]  ))

(ctor-initializer
 ::=
 (  ":" mem-initializer-list  ))

(mem-initializer-list
 ::=
 (  mem-initializer  )
 (  mem-initializer "," mem-initializer-list  ))

(mem-initializer
 ::=
 (  mem-initializer-id "(" expression-list[opt] ")"  ))

(mem-initializer-id
 ::=
 (  "::"[opt] nested-name-specifier[opt] class-name  )
 (  identifier  ))

(operator-function-id
 ::=
 (  operator operator  ))

(operator
 ::=
 "new" "delete" "new[]" "delete[]" "+" "-" "*" "/" "%" "^"
 "&" "|" "~" "!"  "=" "<" ">" "+=" "-=" "*=" "/=" "%=" "^="
 "&=" "|=" "<<" ">>" ">>=" "<<=" "==" "!=" "<=" ">=" "&&"
 "||" "++" "--" "","" "->*" "->" "()" "[]" )

(template-declaration
 ::=
 (  "export"[opt] "template" "<" template-parameter-list ">" declaration  ))

(template-parameter-list
 ::=
 (  template-parameter  )
 (  template-parameter-list "," template-parameter  ))

(template-parameter
 ::=
 (  type-parameter  )
 (  parameter-declaration  ))

(type-parameter
 ::=
 (  "class" identifier[opt]  )
 (  "class" identifier[opt] "=" type-id  )
 (  "typename" identifier[opt]  )
 (  "typename" identifier[opt] "=" type-id  )
 (  "template" "<" template-parameter-list ">" "class"  identifier[opt]  )
 (  "template" "<" template-parameter-list ">" "class"  identifier[opt] "=" id-expression  ))

(template-id
 ::=
 (  template-name "<" template-argument-list[opt] ">"  ))

(template-name
 ::=
 (  identifier  ))

(template-argument-list
 ::=
 (  template-argument  )
 (  template-argument-list "," template-argument  ))

(template-argument
 ::=
 (  assignment-expression  )
 (  type-id  )
 (  id-expression  ))

(explicit-instantiation
 ::=
 (  "template" declaration  ))

(explicit-specialization
 ::=
 (  "template" "<" ">" declaration  ))

(try-block
 ::=
 (  "try" compound-statement handler-seq  ))

(function-try-block
 ::=
 (  "try"  ctor-initializer[opt] function-body handler-seq  ))

(handler-seq
 ::=
 (  handler handler-seq[opt]  ))

(handler
 ::=
 (  "catch" "(" exception-declaration ")" compound-statement  ))

(exception-declaration
 ::=
 (  type-specifier-seq declarator  )
 (  type-specifier-seq abstract-declarator  )
 (  type-specifier-seq  )
 (  "..."  ))

(throw-expression
 ::=
 (  "throw" assignment-expression[opt]  ))

(exception-specification
 ::=
 (  "throw" "(" type-id-list[opt] ")"  ))

(type-id-list
 ::=
 (  type-id  )
 (  type-id-list ","  type-id  ))

(preprocessing-file
 ::=
 (  group[opt]  ))

(group
 ::=
 (  group-part  )
 (  group group-part  ))

(group-part
 ::=
 (  pp-tokens[opt] new-line  )
 (  if-section  )
 (  control-line  ))

(if-section
 ::=
 (  if-group elif-groups[opt] else-group[opt] endif-line  ))

(if-group
 ::=
 (  "#" "if"     constant-expression new-line group[opt]  )
 (  "#" "ifdef"  identifier new-line group[opt]  )
 (  "#" "ifndef" identifier new-line group[opt]  ))

(elif-groups
 ::=
 (  elif-group  )
 (  elif-groups elif-group  ))

(elif-group
 ::=
 (  "#" "elif"   constant-expression new-line group[opt]  ))

(else-group
 ::=
 (  "#" "else"   new-line group[opt]  ))

(endif-line
 ::=
 (  "#" "endif"  new-line  ))

(control-line
 ::=
 (  "#" "include" pp-tokens new-line  )
 (  "#" "define"  identifier replacement-list new-line  )
 (  "#" "define"  identifier lparen identifier-list[opt] ")" replacement-list new-line  )
 (  "#" "undef"   identifier new-line  )
 (  "#" "line"    pp-tokens new-line  )
 (  "#" "error"   pp-tokens[opt] new-line  )
 (  "#" "pragma"  pp-tokens[opt] new-line  )
 (  "#"           new-line  ))

(lparen
 ::=
 (  "(" #|the left-parenthesis character without preceding white-space |# ))

(replacement-list
 ::=
 (  pp-tokens[opt]  ))

(pp-tokens
 ::=
 (  preprocessing-token  )
 (  pp-tokens preprocessing-token  ))

(new-line
 ::=
 (  #\newline #|the new-line character|#  ))
