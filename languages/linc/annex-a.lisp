(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(defparameter *ansi-c-grammar*
  '(("(6.4)" token -->
     keyword
     identifier
     constant
     string-literal
     punctuator)

    ("(6.4)" preprocessing-token -->
     header-name
     identifier
     pp-number
     character-constant
     string-literal
     punctuator
     |each non-white-space character that cannot be one of the above|)

    ("(6.4.1)" keyword -->
     |*|
     |auto|
     |break|
     |case|
     |char|
     |const|
     |continue|
     |default|
     |do|
     |double|
     |else|
     |enum|
     |extern|
     |float|
     |for|
     |goto|
     |if|
     |inline|
     |int|
     |long|
     |register|
     |restrict|
     |return|
     |short|
     |signed|
     |sizeof|
     |static|
     |struct|
     |switch|
     |typedef|
     |union|
     |unsigned|
     |void|
     |volatile|
     |while|
     |_Alignas|
     |_Alignof|
     |_Atomic|
     |_Bool|
     |_Complex|
     |_Generic|
     |_Imaginary|
     |_Noreturn|
     |_Static_assert|
     |_Thread_local|)

    ("(6.4.2.1)" identifier -->
     identifier-nondigit
     (identifier identifier-nondigit)
     (identifier digit))

    ("(6.4.2.1)" identifier-nondigit -->
     nondigit
     universal-character-name
     |other implementation-defined characters|)

    ("(6.4.2.1)" nondigit -->
     \_
     \a \b \c \d \e \f \g \h \i \j \k \l \m
     \n \o \p \q \r \s \t \u \v \w \x \y \z
     \A \B \C \D \E \F \G \H \I \J \K \L \M
     \N \O \P \Q \R \S \T \U \V \W \X \Y \Z)

    ("(6.4.2.1)" digit -->
     \0 \1 \2 \3 \4 \5 \6 \7 \8 \9)

    ("(6.4.3)" universal-character-name -->
     (|\\u| hex-quad)
     (|\\U| hex-quad hex-quad))

    ("(6.4.3)" hex-quad -->
     (hexadecimal-digit hexadecimal-digit)
     (hexadecimal-digit hexadecimal-digit))

    ("(6.4.4)" constant -->
     integer-constant
     floating-constant
     enumeration-constant
     character-constant)

    ("(6.4.4.1)" integer-constant -->
     (decimal-constant (opt integer-suffix))
     (octal-constant (opt integer-suffix))
     (hexadecimal-constant (opt integer-suffix)))

    ("(6.4.4.1)" decimal-constant -->
     nonzero-digit
     (decimal-constant digit))

    ("(6.4.4.1)" octal-constant -->
     \0
     (octal-constant octal-digit))

    ("(6.4.4.1)" hexadecimal-constant -->
     (hexadecimal-prefix hexadecimal-digit)
     (hexadecimal-constant hexadecimal-digit))

    ("(6.4.4.1)" hexadecimal-prefix -->
     |0x|
     |0X|)

    ("(6.4.4.1)" nonzero-digit -->
     \1 \2 \3 \4 \5 \6 \7 \8 \9)

    ("(6.4.4.1)" octal-digit -->
     \0 \1 \2 \3 \4 \5 \6 \7)

    ("(6.4.4.1)" hexadecimal-digit -->
     \0 \1 \2 \3 \4 \5 \6 \7 \8 \9
     \a \b \c \d \e \f
     \A \B \C \D \E \F)

    ("(6.4.4.1)" integer-suffix -->
     (unsigned-suffix  (opt long-suffix))       ; u ul
     (unsigned-suffix  long-long-suffix)        ; ull
     (long-suffix      (opt unsigned-suffix))   ; lu
     (long-long-suffix (opt unsigned-suffix)))  ; llu

    ("(6.4.4.1)" unsigned-suffix -->
     \u
     \U)

    ("(6.4.4.1)" long-suffix -->
     \l
     \L)

    ("(6.4.4.1)" long-long-suffix -->
     |ll|
     |LL|)

    ("(6.4.4.2)" floating-constant -->
     decimal-floating-constant
     hexadecimal-floating-constant)

    ("(6.4.4.2)" decimal-floating-constant -->
     (fractional-constant (opt exponent-part) (opt floating-suffix))
     (digit-sequence exponent-part (opt floating-suffix)))

    ("(6.4.4.2)" hexadecimal-floating-constant -->
     (hexadecimal-prefix hexadecimal-fractional-constant binary-exponent-part (opt floating-suffix))
     (hexadecimal-prefix hexadecimal-digit-sequence      binary-exponent-part (opt floating-suffix)))

    ("(6.4.4.2)" fractional-constant -->
     ((opt digit-sequence) \. digit-sequence)
     (digit-sequence \.))

    ("(6.4.4.2)" exponent-part -->
     (|e| (opt sign) digit-sequence)
     (|E| (opt sign) digit-sequence))

    ("(6.4.4.2)" sign -->
     \+ \-)

    ("(6.4.4.2)" digit-sequence -->
     digit
     (digit-sequence digit))

    ("(6.4.4.2)" hexadecimal-fractional-constant -->
     ((opt hexadecimal-digit-sequence) \. hexadecimal-digit-sequence)
     (hexadecimal-digit-sequence \.))

    ("(6.4.4.2)" binary-exponent-part -->
     (|p| (opt sign) digit-sequence)
     (|P| (opt sign) digit-sequence))

    ("(6.4.4.2)" hexadecimal-digit-sequence -->
     hexadecimal-digit
     (hexadecimal-digit-sequence hexadecimal-digit))

    ("(6.4.4.2)" floating-suffix -->
     \f \l \F \L)

    ("(6.4.4.3)" enumeration-constant -->
     identifier)

    ("(6.4.4.4)" character-constant -->
     (\' c-char-sequence \')
     (\L\' c-char-sequence \')
     (\u\' c-char-sequence \')
     (\U\' c-char-sequence \'))

    ("(6.4.4.4)" c-char-sequence -->
     c-char
     (c-char-sequence c-char))

    ("(6.4.4.4)" c-char -->
     |any member of the source character set except the single-quote ', backslash \, or new-line character|
     escape-sequence)

    ("(6.4.4.4)" escape-sequence -->
     simple-escape-sequence
     octal-escape-sequence
     hexadecimal-escape-sequence
     universal-character-name)

    ("(6.4.4.4)" simple-escape-sequence -->
     \\\' \\\" \\\? \\\\ \\\a  \\\b  \\\f  \\\n  \\\r  \\\t  \\\v)

    ("(6.4.4.4)" octal-escape-sequence -->
     (\\ octal-digit)
     (\\ octal-digit octal-digit)
     (\\ octal-digit octal-digit octal-digit))

    ("(6.4.4.4)" hexadecimal-escape-sequence -->
     (|\\x| hexadecimal-digit)
     (hexadecimal-escape-sequence hexadecimal-digit))

    ("(6.4.5)" string-literal -->
     ((opt encoding-prefix) \" (opt s-char-sequence) \"))

    ("(6.4.5)" encoding-prefix -->
     |u8|
     |u|
     |U|
     |L|)

    ("(6.4.5)" s-char-sequence -->
     s-char
     (s-char-sequence s-char))

    ("(6.4.5)" s-char -->
     |any member of the source character set except the double-quote ", backslash \, or new-line character escape-sequence|)

    ("(6.4.6)" punctuator -->
     \[ \] \( \) \{ \} \. \-\>
     \+\+  \-\-  \&  \*  \+  \-  \~  \!
     \/  \%  \<\<  \>\>  \<  \>  \<\=  \>\=  \=\=  \!\=  \^  \|  \&\&  \|\|
     \?  \:  \;  \.\.\.
     \=  \*\=  \/\=  \%\=  \+\=  \-\=  \<\<\=  \>\>\=  \&\=  \^\=  \|\=
     \,  \#  \#\#
     \<\:  \:\>  \<\%  \%\>  \%\:  \%\:\%\:)

    ("(6.4.7)" header-name -->
     (\< h-char-sequence \>)
     (\" q-char-sequence \"))

    ("(6.4.7)" h-char-sequence -->
     h-char
     (h-char-sequence h-char))

    ("(6.4.7)" h-char -->
     |any member of the source character set except the new-line character and >|)

    ("(6.4.7)" q-char-sequence -->
     q-char
     (q-char-sequence q-char))

    ("(6.4.7)" q-char -->
     |any member of the source character set except the new-line character and "|)

    ("(6.4.8)" pp-number -->
     digit
     (\. digit)
     (pp-number digit)
     (pp-number identifier-nondigit)
     (pp-number \e sign)
     (pp-number \E sign)
     (pp-number \p sign)
     (pp-number \P sign)
     (pp-number \.))

    ("(6.5.1)" primary-expression -->
     identifier
     constant
     string-literal
     (\( expression \))
     generic-selection)

    ("(6.5.1.1)" generic-selection -->
     (|_Generic| \( assignment-expression \, generic-assoc-list \)))

    ("(6.5.1.1)" generic-assoc-list -->
     generic-association
     (generic-assoc-list \, generic-association))

    ("(6.5.1.1)" generic-association -->
     (type-name \: assignment-expression)
     (|default| \: assignment-expression))

    ("(6.5.2)" postfix-expression -->
     primary-expression
     (postfix-expression \[ expression \])
     (postfix-expression \( (opt argument-expression-list) \))
     (postfix-expression \. identifier)
     (postfix-expression |->| identifier)
     (postfix-expression |++|)
     (postfix-expression |--|)
     (\( type-name \) \{ initializer-list \})
     (\( type-name \) \{ initializer-list \, \}))

    ("(6.5.2)" argument-expression-list -->
     assignment-expression
     (argument-expression-list \, assignment-expression))

    ("(6.5.3)" unary-expression -->
     postfix-expression
     (|++| unary-expression)
     (|--| unary-expression)
     (unary-operator cast-expression)
     (|sizeof| unary-expression)
     (|sizeof| \( type-name \))
     (|_Alignof| \( type-name \)))

    ("(6.5.3)" unary-operator -->
     \& \* \+ \- \~ \!)
    ("(6.5.4)" cast-expression -->
     unary-expression
     (\( type-name \) cast-expression))

    ("(6.5.5)" multiplicative-expression -->
     cast-expression
     (multiplicative-expression \* cast-expression)
     (multiplicative-expression \/ cast-expression)
     (multiplicative-expression \% cast-expression))

    ("(6.5.6)" additive-expression -->
     multiplicative-expression
     (additive-expression \+ multiplicative-expression)
     (additive-expression \- multiplicative-expression))

    ("(6.5.7)" shift-expression -->
     additive-expression
     (shift-expression |<<| additive-expression)
     (shift-expression |>>| additive-expression))

    ("(6.5.8)" relational-expression -->
     shift-expression
     (relational-expression |<|  shift-expression)
     (relational-expression |>|  shift-expression)
     (relational-expression |<=| shift-expression)
     (relational-expression |>=| shift-expression))

    ("(6.5.9)" equality-expression -->
     relational-expression
     (equality-expression |==| relational-expression)
     (equality-expression |!=| relational-expression))

    ("(6.5.10)" AND-expression -->
     equality-expression
     (AND-expression |&| equality-expression))

    ("(6.5.11)" exclusive-OR-expression -->
     AND-expression
     (exclusive-OR-expression |^| AND-expression))

    ("(6.5.12)" inclusive-OR-expression -->
     exclusive-OR-expression
     (inclusive-OR-expression \| exclusive-OR-expression))

    ("(6.5.13)" logical-AND-expression -->
     inclusive-OR-expression
     (logical-AND-expression |&&| inclusive-OR-expression))

    ("(6.5.14)" logical-OR-expression -->
     logical-AND-expression
     (logical-OR-expression \|\| logical-AND-expression))

    ("(6.5.15)" conditional-expression -->
     logical-OR-expression
     (logical-OR-expression |?| expression |:| conditional-expression))

    ("(6.5.16)" assignment-expression -->
     conditional-expression
     (unary-expression assignment-operator assignment-expression))

    ("(6.5.16)" assignment-operator -->
     \= \*\= \/\= \%\= \+\= \-\= \<\<\= \>\>\= \&\= \^\= \|\=)

    ("(6.5.17)" expression -->
     assignment-expression
     (expression \, assignment-expression))

    ("(6.6)" constant-expression -->
     conditional-expression)

    ("(6.7)" declaration -->
     (declaration-specifiers (opt init-declarator-list) \;)
     static_assert-declaration)

    ("(6.7)" declaration-specifiers -->
     (storage-class-specifier (opt declaration-specifiers))
     (type-specifier          (opt declaration-specifiers))
     (type-qualifier          (opt declaration-specifiers))
     (function-specifier      (opt declaration-specifiers))
     (alignment-specifier     (opt declaration-specifiers)))

    ("(6.7)" init-declarator-list -->
     init-declarator
     (init-declarator-list \, init-declarator))

    ("(6.7)" init-declarator -->
     declarator
     (declarator \= initializer))

    ("(6.7.1)" storage-class-specifier -->
     |typedef|
     |extern|
     |static|
     |_Thread_local|
     |auto|
     |register|)

    ("(6.7.2)" type-specifier -->
     |void|
     |char|
     |short|
     |int|
     |long|
     |float|
     |double|
     |signed|
     |unsigned|
     |_Bool|
     |_Complex|
     atomic-type-specifier
     struct-or-union-specifier
     enum-specifier
     typedef-name)

    ("(6.7.2.1)" struct-or-union-specifier -->
     (struct-or-union (opt identifier) \{ struct-declaration-list \})
     struct-or-union identifier)

    ("(6.7.2.1)" struct-or-union -->
     |struct|
     |union|)

    ("(6.7.2.1)" struct-declaration-list -->
     struct-declaration
     (struct-declaration-list struct-declaration))

    ("(6.7.2.1)" struct-declaration -->
     (specifier-qualifier-list (opt struct-declarator-list) \;)
     static_assert-declaration)

    ("(6.7.2.1)" specifier-qualifier-list -->
     (type-specifier (opt specifier-qualifier-list))
     (type-qualifier (opt specifier-qualifier-list)))

    ("(6.7.2.1)" struct-declarator-list -->
     struct-declarator
     (struct-declarator-list \, struct-declarator))

    ("(6.7.2.1)" struct-declarator -->
     declarator
     ((opt declarator) \: constant-expression))

    ("(6.7.2.2)" enum-specifier -->
     (|enum| (opt identifier) \{ enumerator-list \})
     (|enum| (opt identifier) \{ enumerator-list \, \})
     (|enum| identifier))

    ("(6.7.2.2)" enumerator-list -->
     enumerator
     (enumerator-list \, enumerator))

    ("(6.7.2.2)" enumerator -->
     enumeration-constant
     (enumeration-constant \= constant-expression))

    ("(6.7.2.4)" atomic-type-specifier -->
     (|_Atomic| \( type-name \)))

    ("(6.7.3)" type-qualifier -->
     |const|
     |restrict|
     |volatile|
     |_Atomic|)

    ("(6.7.4)" function-specifier -->
     |inline|
     |_Noreturn|)

    ("(6.7.5)" alignment-specifier -->
     (|_Alignas| \( type-name \))
     (|_Alignas| \( constant-expression \)))

    ("(6.7.6)" declarator -->
     ((opt pointer) direct-declarator))

    ("(6.7.6)" direct-declarator -->
     identifier
     (\( declarator \))
     (direct-declarator \[ (opt type-qualifier-list) (opt assignment-expression) \])
     (direct-declarator \[ static (opt type-qualifier-list) assignment-expression \])
     (direct-declarator \[ type-qualifier-list static assignment-expression \])
     (direct-declarator \[ (opt type-qualifier-list) \* \])
     (direct-declarator \( parameter-type-list \))
     (direct-declarator \( (opt identifier-list) \)))

    ("(6.7.6)" pointer -->
     (\* (opt type-qualifier-list))
     (\* (opt type-qualifier-list) pointer))

    ("(6.7.6)" type-qualifier-list -->
     type-qualifier
     (type-qualifier-list type-qualifier))

    ("(6.7.6)" parameter-type-list -->
     parameter-list
     (parameter-list \, |...|))

    ("(6.7.6)" parameter-list -->
     parameter-declaration
     (parameter-list \, parameter-declaration))

    ("(6.7.6)" parameter-declaration -->
     (declaration-specifiers declarator)
     (declaration-specifiers (opt abstract-declarator)))

    ("(6.7.6)" identifier-list -->
     identifier
     (identifier-list \, identifier))

    ("(6.7.7)" type-name -->
     (specifier-qualifier-list (opt abstract-declarator)))

    ("(6.7.7)" abstract-declarator -->
     pointer
     ((opt pointer) direct-abstract-declarator))

    ("(6.7.7)" direct-abstract-declarator -->
     (\( abstract-declarator \))
     ((opt direct-abstract-declarator) \[ (opt type-qualifier-list) (opt assignment-expression) \])
     ((opt direct-abstract-declarator) \[ |static| (opt type-qualifier-list) assignment-expression \])
     ((opt direct-abstract-declarator) \[ type-qualifier-list |static| assignment-expression \])
     ((opt direct-abstract-declarator) \[ \* \])
     ((opt direct-abstract-declarator) \( (opt parameter-type-list) \)))

    ("(6.7.8)" typedef-name -->
     identifier)

    ("(6.7.9)" initializer -->
     assignment-expression
     (\{ initializer-list \})
     (\{ initializer-list \, \}))

    ("(6.7.9)" initializer-list -->
     ((opt designation) initializer)
     (initializer-list \, (opt designation) initializer))

    ("(6.7.9)" designation -->
     (designator-list \=))

    ("(6.7.9)" designator-list -->
     designator
     (designator-list designator))

    ("(6.7.9)" designator -->
     (\[ constant-expression \])
     (\. identifier))

    ("(6.7.10)" static_assert-declaration -->
     (|_Static_assert| \( constant-expression \, string-literal \) \;))

    ("(6.8)" statement -->
     labeled-statement
     compound-statement
     expression-statement
     selection-statement
     iteration-statement
     jump-statement)

    ("(6.8.1)" labeled-statement -->
     (identifier \: statement)
     (|case| constant-expression \: statement)
     (|default| \: statement))

    ("(6.8.2)" compound-statement -->
     (\{ (opt block-item-list) \}))

    ("(6.8.2)" block-item-list -->
     block-item
     (block-item-list block-item))

    ("(6.8.2)" block-item -->
     declaration
     statement)

    ("(6.8.3)" expression-statement -->
     ((opt expression) \;))

    ("(6.8.4)" selection-statement -->
     (|if|     \( expression \) statement)
     (|if|     \( expression \) statement |else| statement)
     (|switch| \( expression \) statement))

    ("(6.8.5)" iteration-statement -->
     (|while| \( expression \) statement)
     (|do| statement |while| \( expression \) \;)
     (|for| \( (opt expression) \; (opt expression) \; (opt expression) \) statement)
     (|for| \( declaration (opt expression) \; (opt expression) \) statement))

    ("(6.8.6)" jump-statement -->
     (|goto| identifier \;)
     (|continue| \;)
     (|break| \;)
     (|return| (opt expression) \;))

    ("(6.9)" translation-unit -->
     external-declaration
     (translation-unit external-declaration))

    ("(6.9)" external-declaration -->
     function-definition
     declaration)

    ("(6.9.1)" function-definition -->
     (declaration-specifiers declarator (opt declaration-list) compound-statement))

    ("(6.9.1)" declaration-list -->
     declaration
     (declaration-list declaration))

    ("(6.10)" preprocessing-file -->
     (opt group))

    ("(6.10)" group -->
     group-part
     (group group-part))

    ("(6.10)" group-part -->
     if-section
     (control-line text-line)
     (\# non-directive))

    ("(6.10)" if-section -->
     (if-group (opt elif-groups) (opt else-group) endif-line))

    ("(6.10)" if-group -->
     (\# |if| constant-expression new-line (opt group))
     (\# |ifdef| identifier new-line (opt group))
     (\# |ifndef| identifier new-line (opt group)))

    ("(6.10)" elif-groups -->
     elif-group
     (elif-groups elif-group))

    ("(6.10)" elif-group -->
     (\# |elif| constant-expression new-line (opt group)))

    ("(6.10)" else-group -->
     (\# |else| new-line (opt group)))

    ("(6.10)" endif-line -->
     (\# |endif|))

    ("(6.10)" control-line -->
     (\# |include| pp-tokens new-line)
     (\# |define|  identifier replacement-list new-line)
     (\# |define|  identifier lparen (opt identifier-list) \) replacement-list new-line)
     (\# |define|  identifier lparen |...| \) replacement-list new-line)
     (\# |define|  identifier lparen identifier-list \, |...| \) replacement-list new-line)
     (\# |undef|   identifier new-line)
     (\# |line|    pp-tokens new-line)
     (\# |error|   (opt pp-tokens) new-line)
     (\# |pragma|  (opt pp-tokens) new-line)
     (\#           new-line))

    ("(6.10)" text-line -->
     ((opt pp-tokens) new-line))

    ("(6.10)" non-directive -->
     (pp-tokens new-line))

    ("(6.10)" lparen -->
     |a ( character not immediately preceded by white-space|)

    ("(6.10)" replacement-list -->
     (opt pp-tokens))

    ("(6.10)" pp-tokens -->
     preprocessing-token
     (pp-tokens preprocessing-token))

    ("(6.10)" new-line -->
     |the new-line character|)))




