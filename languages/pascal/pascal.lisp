(eval-when (:compile-toplevel :load-toplevel :execute)

  (ql:quickload :com.informatimago.rdp)
  (pushnew :pascal-with-unit *features*))

(defpackage "PASCAL-GRAMMAR"
  (:use "CL" "COM.INFORMATIMAGO.RDP"))
(in-package "PASCAL-GRAMMAR")


(defvar *pascal-types* (make-hash-table))
(defun fill-default-types ()
  (setf (gethash '|integer| *pascal-types*)))
(defun pascal-string-token-to-lisp (pstring)
  (with-output-to-string (*standard-output*)
    (loop :with quote := nil
          :for ch :across (subseq pstring 1 (1- (length pstring)))
          :do (if quote
                  (case ch
                    (#\'
                     (write-char #\')
                     (setf quote nil))
                    (otherwise
                     (error "Invalid pascal string syntax ~S" pstring)))
                  (case ch
                    (#\'
                     (setf quote t))
                    (otherwise
                     (write-char ch)))))))


;; (with-open-file (*standard-output* 
;;                  #P"~/src/public/lisp/languages/pascal/generated-pascal-parser.lisp"
;;                  :direction :output
;;                  :if-does-not-exist :create
;;                  :if-exists :supersede)
;;   (pprint (macroexpand '

(defgrammar pascal
  :scanner t
  :trace nil
  :terminals (
              
              (dotdot    "\\.\\.")
              (assign    ":=")
              (le        "<=")
              (ne        "<>")
              (ge        ">=")
              (opar      "\\(")
              (clop      "\\)")
              (times     "\\*")
              (plus      "\\+")
              (comma     ",")
              (semi      ";")
              (minus     "-")
              (dot       "\\.")
              (slash     "/")
              (colon     ":")
              (lt        "<")
              (eq        "=")
              (gt        ">")
              (obra      "\\[")
              (clob      "\\]")
              (expo      "\\^")
              (and       "\\band\\b")
              (array     "\\barray\\b")
              (begin     "\\bbegin\\b")
              (case      "\\bcase\\b")
              (const     "\\bconst\\b")
              (div       "\\bdiv\\b")
              (do        "\\bdo\\b")
              (downto    "\\bdownto\\b")
              (else      "\\belse\\b")
              (end       "\\bend\\b")
              (file      "\\bfile\\b")
              (for       "\\bfor\\b")
              (forward   "\\bforward\\b")
              (function  "\\bfunction\\b")
              (goto      "\\bgoto\\b")
              (if        "\\bif\\b")
              (in        "\\bin\\b")
              (label     "\\blabel\\b")
              (mod       "\\bmod\\b")
              (nile      "\\bnil\\b")
              (not       "\\bnot\\b")
              (of        "\\bof\\b")
              (or        "\\bor\\b")
              (packed    "\\bpacked\\b")
              (procedure "\\bprocedure\\b")
              (program   "\\bprogram\\b")
              (record    "\\brecord\\b")
              (repeat    "\\brepeat\\b")
              (set       "\\bset\\b")
              (then      "\\bthen\\b")
              (to        "\\bto\\b")
              (type      "\\btype\\b")
              #+pascal-with-unit (unit "\\bunit\\b")
              (until     "\\buntil\\b")
              (var       "\\bvar\\b")
              (while     "\\bwhile\\b")
              (with      "\\bwith\\b")

              ;; identifier  =  letter { letter | digit } .
              ;; integer     =  digit { digit } | digit { digit }  CONTEXT ("..") .
              ;; real        =  digit { digit } "." digit { digit }
              ;;                [ "E" ["+" | "-"] digit { digit } ]
              ;;                |  digit { digit } "E" ["+" | "-"] digit { digit }  .
              ;; string      = "'" { noQuote1 | "''" } "'" .

              (identifier "[A-Za-z][A-Za-z0-9]*")
              (integer    "[0-9][0-9]*")
              (real       "([0-9][0-9]*\\.[0-9][0-9]*(E[-+]?[0-9][0-9]*)?)|([0-9][0-9]*E[-+]?[0-9][0-9]*)")
              (string     "'([^'\\n]|'')*'")
              )


  ;; COMMENTS FROM "(*" TO "*)"
  ;; COMMENTS FROM "{" TO "}"

  :start translation-unit

  :rules

  ((--> translation-unit
        #-pascal-with-unit pascal-program
        #+pascal-with-unit (alt pascal-program
                                pascal-unit))

   ;;   Pascal            = "program"  NewIdent [ ExternalFiles ] ";"
   ;;                       Block "." .
   (--> pascal-program
        program new-ident (opt external-files) semi block dot
        :action (list 'program new-ident
                      (list :external-files $3)
                      block))

   #+pascal-with-unit
   (--> pascal-unit
        unit new-ident semi block dot
        :action (list 'unit new-ident
                      (list)
                      block))

   ;;   ExternalFiles     = "(" NewIdentList ")" .
   (--> external-files
        opar new-ident-list clop)

   ;;   Block             =  DeclarationPart StatementPart .
   (--> block
        declaration-part statement-part)

   ;;   DeclarationPart   = LabelDeclarations ConstDefinitions
   ;;                       TypeDefinitions VarDeclarations
   ;;                       { ProcDeclarations } .
   (--> declaration-part
        label-declarations const-definitions
        type-definitions var-declarations
        (rep proc-declarations))

   ;;   LabelDeclarations = [ "label" Labels ";" ] .
   (--> label-declarations
        (opt label labels semi))
   
   ;;   Labels            = Label { "," Label } .
   (--> labels
        a-label (rep comma a-label))
   
   ;;   Label             = UnsignedInt .
   (--> a-label
        unsigned-int)

   ;;   ConstDefinitions  = [ "const" ConstDef { ConstDef } ] .
   (--> const-definitions
        (opt const const-def (rep const-def)))

   ;;   ConstDef          = NewIdent "=" Constant ";" .
   (--> const-def
        new-ident eq constant semi)
   
   ;;   Constant          = [ "+" | "-" ] ( UnsignedNumber | ConstIdent ) | String .
   (--> constant
        (alt (seq (opt (alt plus minus)) (alt unsigned-number const-ident))
             a-string))
   
   ;;   UnsignedNumber    = UnsignedInt | UnsignedReal .
   (--> unsigned-number
        (alt unsigned-int unsigned-real))
   
   ;;   ConstIdent        = identifier .
   (--> const-ident
        identifier)
   
   ;;   TypeDefinitions   = [ "type" TypeDef { TypeDef } ] .
   (--> type-definitions
        (opt type type-def (rep type-def)))
   
   ;;   TypeDef           = NewIdent "=" Type ";" .
   (--> type-def
        new-ident eq type semi)
   
   ;;   Type              = SimpleType | [ "packed" ] StructType | "^" TypeIdent .
   (--> type
        (alt simple-type
             (seq (opt packed) struct-type)
             (seq expo type-ident)))

   ;;   SimpleType        = TypeIdent | EnumerationType | SubrangeType .
   ;; (--> simple-type
   ;;      (alt type-ident
   ;;           enumeration-type
   ;;           subrange-type))
   (--> simple-type
        (alt (seq                       ; alt type-ident subrange-type
                  identifier (opt dotdot constant))
             (seq (alt plus minus) (alt unsigned-number const-ident)
                  dotdot constant)
             (seq unsigned-number
                  dotdot constant)
             enumeration-type))
   
   ;;   TypeIdent         = identifier .
   (--> type-ident
        identifier)
   
   ;;   EnumerationType   = "(" NewIdentList ")" .
   (--> enumeration-type
        opar new-ident-list clop)
   
   ;;   SubrangeType      = Constant ".." Constant .
   (--> subrange-type
        constant dotdot constant)
   
   ;;   StructType        = ArrayType | RecordType | SetType | FileType .
   (--> struct-type
        (alt array-type
             record-type
             set-type
             file-type))
   
   ;;   ArrayType         = "array" "[" IndexList "]" "of" Type .
   (--> array-type
        array obra index-list clob of type)
   
   ;;   IndexList         = SimpleType { "," SimpleType } .
   (--> index-list
        simple-type (rep comma simple-type))
   
   ;;   RecordType        = "record" FieldList "end" .
   (--> record-type
        record field-list end)
   
   ;;   SetType           = "set" "of" SimpleType .
   (--> set-type
        set of simple-type)
   
   ;;   FileType          = "file" "of" Type .
   (--> file-type
        file of type)
   
   ;;   FieldList         = [ ( fixedPart [ ";" VariantPart ] | VariantPart ) [ ";" ] ] .
   (--> field-list
        (opt (seq (alt (seq fixed-part (opt semi variant-part))
                       variant-part)
                  (opt semi))))
   
   ;;   fixedPart         = RecordSection { ";" RecordSection } .
   (--> fixed-part
        record-section (rep semi record-section))
   
   ;;   RecordSection     = NewIdentList ":" Type .
   (--> record-section
        new-ident-list colon type)
   
   ;;   VariantPart       = "case" VariantSelector "of" Variant { ";" Variant } .
   (--> variant-part
        case variant-selector of variant (rep semi variant))
   
   ;;   VariantSelector   = [ NewIdent ":" ] TypeIdent .
   (--> variant-selector
        (opt new-ident colon) type-ident)
   
   ;;   Variant           = CaseLabelList ":" "(" FieldList ")" .
   (--> variant
        case-label-list colon opar field-list clop)
   
   ;;   VarDeclarations   = [ "var" VarDecl { VarDecl } ] .
   (--> var-declarations
        (opt var var-decl (rep var-decl)))
   
   ;;   VarDecl           = NewIdentList ":" Type ";" .
   (--> var-decl
        new-ident-list colon type semi)
   
   ;;   ProcDeclarations  = ( ProcHeading | FuncHeading ) ";" Body ";" .
   (--> proc-declarations
        (alt proc-heading func-heading) semi body semi)
   
   ;;   ProcHeading       = "procedure" NewIdent [ FormalParams ] .
   (--> proc-heading
        procedure new-ident (opt formal-params))
   
   ;;   FuncHeading       = "function" NewIdent [ FormalParams ] ReturnType .
   (--> func-heading
        function new-ident (opt formal-params) return-type)
   
   ;;   ReturnType        = [ (* empty if forward referenced *) ":" TypeIdent ] .
   (--> return-type
        (opt colon type-ident))
   
   ;;   Body              = Block | "forward" .
   (--> body
        (alt block forward))
   
   ;;   FormalParams      = "(" FormalSection { ";" FormalSection } ")"  .
   (--> formal-params
        opar formal-section (rep semi formal-section) clop)
   
   ;;   FormalSection     = [ "var" ] ParamGroup | ProcHeading | FuncHeading .
   (--> formal-section
        (opt var) (alt param-group
                       proc-heading
                       func-heading))
   
   ;;   ParamGroup        = NewIdentList ":" ParamType .
   (--> param-group
        new-ident-list colon param-type)
   
   ;;   ParamType         =   TypeIdent
   ;;                       | "array" "[" IndexSpecList "]" "of" ParamType
   ;;                       | "packed" "array" "[" IndexSpec "]" "of" TypeIdent .
   (--> param-type
        (alt type-ident
             (seq array obra index-spec-list clob of param-type)
             (seq packed array obra index-spec clob of type-ident)))
   
   ;;   IndexSpecList     = IndexSpec { ";" IndexSpec } .
   (--> index-spec-list
        index-spec (rep semi index-spec))
   
   ;;   IndexSpec         = NewIdent ".." NewIdent ":" TypeIdent .
   (--> index-spec
        new-ident dotdot new-ident colon type-ident)
   
   ;;   StatementPart     = CompoundStatement .
   (--> statement-part
        compound-statement :action compound-statement)
   
   ;;   CompoundStatement = "begin" StatementSequence "end" .
   (--> compound-statement
        begin statement-sequence end :action `(progn ,@statement-sequence))
   
   ;;   StatementSequence = Statement { ";" Statement } .
   (--> statement-sequence
        statement (rep (seq semi statement :action statement))
        :action (append statement (mapcan (function copy-list) $2)))
   
   ;;   Statement         = [ Label ":" ]
   ;;                       [   AssignmentOrCall
   ;;                         | CompoundStatement
   ;;                         | GotoStatement
   ;;                         | WhileStatement
   ;;                         | RepeatStatement
   ;;                         | IfStatement
   ;;                         | CaseStatement
   ;;                         | ForStatement
   ;;                         | WithStatement ] .
   (--> statement
        (opt a-label colon :action a-label)
        (alt assignment-or-call
             compound-statement
             goto-statement
             while-statement
             repeat-statement
             if-statement
             case-statement
             for-statement
             with-statement)
        :action (if $1
                    (list $1 $2)
                    (list $2)))
   
   ;;   AssignmentOrCall  = Designator ( ":=" Expression | [ ActualParams ] ) .
   (--> assignment-or-call
        designator (alt (seq assign expression :action `(setf ,expression))
                        (opt actual-params     :action `(funcall ,@actual-params)))
        :action (cond
                  ((null $2) designator)
                  ((eql 'funcall (first $2))
                   (if (symbolp designator)
                       (cons designator (rest $2))
                       (list* 'funcall designator (rest $2))))
                  (t
                   (list* (first $2) designator (rest $2)))))
   
   ;;   ActualParams      = "(" ActualParameter { "," ActualParameter } ")" .
   (--> actual-params
        opar
        actual-parameter
        (rep (seq comma actual-parameter :action actual-parameter))
        clop
        :action (cons actual-parameter $3))
   
   ;;   ActualParameter   = Expression [ FieldWidth (* only in i/o *) ] .
   (--> actual-parameter
        expression (opt field-width)
        :action (if $2
                    `(field ,expression ,@$2)
                    expression))
   
   ;;   FieldWidth        = ":" IntegerExpression [ ":" IntegerExpression ] .
   (--> field-width
        colon integer-expression (opt colon integer-expression
                                      :action integer-expression)
        :action (if $3
                    (list integer-expression $3)
                    (list integer-expression 0)))
   
   ;;   GotoStatement     = "goto" Label .
   (--> goto-statement
        goto a-label
        :action `(go ,a-label))
   
   ;;   WhileStatement    = "while" BooleanExpression "do" Statement .
   (--> while-statement
        while boolean-expression do statement
        :action `(loop :while ,boolean-expression :do ,statement))
   
   ;;   RepeatStatement   = "repeat" StatementSequence "until" BooleanExpression .
   (--> repeat-statement
        repeat statement-sequence until boolean-expression
        :action `(loop :do ,statement-sequence :until ,boolean-expression))
   
   ;;   IfStatement       = "if" BooleanExpression "then" Statement
   ;;                       [ "else" Statement ] .
   (--> if-statement
        if boolean-expression then statement (opt else statement :action statement)
        :action `(if ,boolean-expression ,statement ,$5))
   
   ;;   CaseStatement     = "case" OrdinalExpression "of" CaseList "end" .
   (--> case-statement
        case ordinal-expression of case-list end
        :action `(case ,ordinal-expression
                   ,@case-list))
   
   ;;   CaseList          = OneCase { ";" OneCase } [ ";" ] .
   (--> case-list
        one-case (rep (seq semi one-case :action one-case)) (opt semi)
        :action (cons one-case $2))
   
   ;;   OneCase           = CaseLabelList ":" Statement .
   (--> one-case
        case-label-list colon statement
        :action (list case-label-list statement))
   
   ;;   CaseLabelList     = CaseLabel { "," CaseLabel } .
   (--> case-label-list
        case-label (rep (seq comma case-label :action case-label))
        :action (cons case-label $2))
   
   ;;   CaseLabel         = Constant .
   (--> case-label
        constant :action constant)
   
   ;;   ForStatement      = "for" ControlVariable ":=" OrdinalExpression
   ;;                       ( "to" | "downto" ) OrdinalExpression "do" Statement .
   (--> for-statement
        for control-variable assign ordinal-expression
        (alt (seq to :action :upto)
             (seq downto :action :downto))
        ordinal-expression do statement
        :action `(loop :for ,control-variable :from ,ordinal-expression.1
                       ,$5 ,ordinal-expression.2 :do statement))
   
   ;;   ControlVariable   = identifier .
   (--> control-variable
        identifier :action identifier)
   
   ;;   WithStatement     = "with" RecVarList "do" Statement .
   (--> with-statement
        with rec-var-list do statement
        :action `(with-slots (,@rec-var-list) ,statement))
   
   ;;   RecVarList        = Designator { "," Designator } .
   (--> rec-var-list
        designator (rep (seq comma designator :action designator))
        :action (cons designator $2))
   
   ;;   IntegerExpression = Expression .
   ;;   BooleanExpression = Expression .
   ;;   OrdinalExpression = Expression .
   (--> integer-expression expression :action expression)
   (--> boolean-expression expression :action expression)
   (--> ordinal-expression expression :action expression)
   
   ;;   Expression        = SimpleExpression [ RelOp SimpleExpression ] .
   (--> expression
        simple-expression (opt rel-op simple-expression)
        :action (if $2
                    (list (first $2) $1 (second $2))
                    $1))
   
   ;;   RelOp             = "=" | "<" | ">" | "<=" | ">=" | "<>" | "in" .
   (--> rel-op
        (alt (seq eq :action '=)
             (seq lt :action '<)
             (seq gt :action '>)
             (seq le :action '<=)
             (seq ge :action '>=)
             (seq ne :action '/=)
             (seq in :action 'member)))
   
   ;;   SimpleExpression  = ( "+" Term | "-" Term | Term ) { AddOp Term } .
   (--> simple-expression
        (opt (alt (seq plus  :action '+)
                  (seq minus :action '-)))
        term
        (rep add-op term)
        :action (loop :for exp := (if $1
                                      (list $1 term)
                                      term)
                        :then (list op exp term.2)
                      :for (op term.2) :in $3
                      :finally (return exp)))
   
   ;;   AddOp             = "+" | "-" | "or" .
   (--> add-op
        (alt (seq plus  :action '+)
             (seq minus :action '-)
             (seq or    :action 'or)))
   
   ;;   Term              = Factor { MulOp Factor } .
   (--> term
        factor (rep mul-op factor)
        :action (loop :for exp := factor
                        :then (list op exp factor.2)
                      :for (op factor.2) :in $2
                      :finally (return exp)))
   
   ;;   MulOp             = "*" | "/" | "div" | "mod" | "and" .
   (--> mul-op
        (alt (seq times :action '*)
             (seq slash :action '/)
             (seq div   :action 'truncate)
             (seq mod   :action 'mod)
             (seq and   :action 'and)))
   
   ;;   Factor            =   Designator [ ActualParams ]
   ;;                       | UnsignedLiteral
   ;;                       | SetConstructor
   ;;                       | "(" Expression ")"
   ;;                       | "not" Factor .
   (--> factor
        (alt (seq designator (opt actual-params))
             unsigned-literal
             set-constructor
             (seq opar expression clop)
             (seq not factor)))
   
   ;;   Designator        = identifier { "." identifier | "[" ExpList "]" | "^" } .
   (--> designator
        identifier (rep (alt (seq dot identifier
                                  :action `(slot-ref ,identifier))
                             (seq obra exp-list clob
                                  :action `(aref ,@exp-list))
                             (seq expo
                                  :action `(deref))))
        :action (loop
                  :for des := identifier
                    :then `(,op ,des ,@args)
                  :for (op . args) :in $2
                  :finally (return des)))
   
   ;;   ExpList           = Expression { "," Expression } .
   (--> exp-list
        expression (rep comma expression))
   
   ;;   UnsignedLiteral   = UnsignedNumber | "nil" | String .
   (--> unsigned-literal
        (alt (seq unsigned-number :action $1)
             (seq nile            :action 'nil)
             (seq a-string        :action $1)))
    
   ;;   SetConstructor    = "[" Member { "," Member } "]" .
   (--> set-constructor
        obra member (rep (seq comma member :action member)) clob
        :action (cons member $3))
   
   ;;   Member            = Expression [ ".." Expression ] .
   (--> member
        expression (opt dotdot expression))
   
   ;;   NewIdentList      = NewIdent { "," NewIdent } .
   (--> new-ident-list
        new-ident (rep (seq comma new-ident :action new-ident))
        :action (cons new-ident $2))

   ;;   NewIdent          = identifier .
   ;;   UnsignedInt       = integer .
   ;;   UnsignedReal      = real .
   ;;   String            = string .
   (--> new-ident identifier
        :action (intern (token-text (second identifier))))
   (--> unsigned-int integer
        :action (parse-integer (second integer)))
   (--> unsigned-real real
        :action (read-from-string (second real)))
   (--> a-string string
        :action (pascal-string-token-to-lisp (token-text (second string))))))

;; )))







(defun scan-pascal (source)
  "
SOURCE: When the grammar has a scanner generated, or a scanner class
        name, SOURCE can be either a string, or a stream that will be
        scanned with the generated scanner.  Otherwise, it should be a
        SCANNER instance.
"
  (let ((scanner (make-instance 'pascal-scanner :source source)))
    (advance-line scanner)
    (loop
      :for token := (scanner-current-token scanner)
        :then (scan-next-token scanner)
      :until (scanner-end-of-source-p scanner)
      :do (print token))
    #-(and)
    (com.informatimago.rdp::with-non-terminal (pascal scanner)
      (prog1 (pascal/parse-translation-unit scanner)
        (unless (scanner-end-of-source-p scanner)
          (cerror "Continue"
                  'parser-end-of-source-not-reached
                  :file (scanner-file scanner)
                  :line (scanner-line scanner)
                  :column (scanner-column scanner)
                  :grammar (grammar-named 'pascal)
                  :scanner scanner
                  :non-terminal-stack (copy-list *non-terminal-stack*)))))))


#|

(pascal-grammar::scan-pascal
                  "
unit foo; 
    function dbli(x:integer):integer;
    begin 
       dbli := x*2
    end;
begin
 writeln('unit foo loaded!')
end.
")

|#
