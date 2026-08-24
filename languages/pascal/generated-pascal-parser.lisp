
(let ((com.informatimago.rdp::*eof-symbol* '#:eof))
  (register-grammar
    (com.informatimago.rdp::make-normalized-grammar
      :name
      'pascal
      :terminals
      '((opar "\\(") (clop "\\)") (times "\\*") (plus "\\+") (comma ",") (minus "-") (dot "\\.") (dotdot "\\.\\.")
        (slash "/") (colon ":") (assign ":=") (semi ";") (lt "<") (le "<=") (ne "<>") (eq "=") (gt ">") (ge ">=")
        (obra "\\[") (clob "\\]") (expo "\\^") (and "\\<and\\>") (array "\\<array\\>") (begin "\\<begin\\>")
        (case "\\<case\\>") (const "\\<const\\>") (div "\\<div\\>") (do "\\<do\\>") (downto "\\<downto\\>")
        (else "\\<else\\>") (end "\\<end\\>") (file "\\<file\\>") (for "\\<for\\>") (forward "\\<forward\\>")
        #'"\\<function\\>" (goto "\\<goto\\>") (if "\\<if\\>") (in "\\<in\\>") (label "\\<label\\>") (mod "\\<mod\\>")
        (nile "\\<nil\\>") (not "\\<not\\>") (of "\\<of\\>") (or "\\<or\\>") (packed "\\<packed\\>")
        (procedure "\\<procedure\\>") (program "\\<program\\>") (record "\\<record\\>") (repeat "\\<repeat\\>")
        (set "\\<set\\>") (then "\\<then\\>") (to "\\<to\\>") (type "\\<type\\>") (unit "\\<unit\\>")
        (until "\\<until\\>") (var "\\<var\\>") (while "\\<while\\>") (with "\\<with\\>")
        (identifier "[A-Za-z][A-Za-z0-9]*") (integer "[0-9][0-9]*")
        (real "([0-9][0-9]*\\.[0-9][0-9]*(E[-+]?[0-9][0-9]*)?)|([0-9][0-9]*E[-+]?[0-9][0-9]*)")
        (string "'([^'\\n]|'')*'"))
      :start
      'translation-unit
      :rules
      '((translation-unit (alt (pascal-program pascal-unit)))
        (pascal-program
         (seq (program new-ident (opt ((seq (external-files) ($0)))) semi block dot)
          ((list 'program new-ident (list :external-files $3) block))))
        (pascal-unit (seq (unit new-ident semi block dot) ((list 'unit new-ident (list) block))))
        (external-files (seq (opar new-ident-list clop) ((list* 'external-files $0))))
        (block (seq (declaration-part statement-part) ((list* 'block $0))))
        (declaration-part
         (seq
          (label-declarations const-definitions type-definitions var-declarations
           (rep ((seq (proc-declarations) ($0)))))
          ((list* 'declaration-part $0))))
        (label-declarations (opt ((seq (label labels semi) ($0)))))
        (labels (seq (a-label (rep ((seq (comma a-label) ($0))))) ((list* 'labels $0))))
        (a-label (seq (unsigned-int) ((list* 'a-label $0))))
        (const-definitions (opt ((seq (const const-def (rep ((seq (const-def) ($0))))) ($0)))))
        (const-def (seq (new-ident eq constant semi) ((list* 'const-def $0))))
        (constant
         (alt ((seq ((opt ((seq ((alt (plus minus))) ($0)))) (alt (unsigned-number const-ident))) ($0)) a-string)))
        (unsigned-number (alt (unsigned-int unsigned-real))) (const-ident (seq (identifier) ((list* 'const-ident $0))))
        (type-definitions (opt ((seq (type type-def (rep ((seq (type-def) ($0))))) ($0)))))
        (type-def (seq (new-ident eq type semi) ((list* 'type-def $0))))
        (type (alt (simple-type (seq ((opt ((seq (packed) ($0)))) struct-type) ($0)) (seq (expo type-ident) ($0)))))
        (simple-type
         (alt
          ((seq (identifier (opt ((seq (dotdot constant) ($0))))) ($0))
           (seq ((alt (plus minus)) (alt (unsigned-number const-ident)) dotdot constant) ($0))
           (seq (unsigned-number dotdot constant) ($0)) enumeration-type)))
        (type-ident (seq (identifier) ((list* 'type-ident $0))))
        (enumeration-type (seq (opar new-ident-list clop) ((list* 'enumeration-type $0))))
        (subrange-type (seq (constant dotdot constant) ((list* 'subrange-type $0))))
        (struct-type (alt (array-type record-type set-type file-type)))
        (array-type (seq (array obra index-list clob of type) ((list* 'array-type $0))))
        (index-list (seq (simple-type (rep ((seq (comma simple-type) ($0))))) ((list* 'index-list $0))))
        (record-type (seq (record field-list end) ((list* 'record-type $0))))
        (set-type (seq (set of simple-type) ((list* 'set-type $0))))
        (file-type (seq (file of type) ((list* 'file-type $0))))
        (field-list
         (opt
          ((seq
            ((seq
              ((alt ((seq (fixed-part (opt ((seq (semi variant-part) ($0))))) ($0)) variant-part))
               (opt ((seq (semi) ($0)))))
              ($0)))
            ($0)))))
        (fixed-part (seq (record-section (rep ((seq (semi record-section) ($0))))) ((list* 'fixed-part $0))))
        (record-section (seq (new-ident-list colon type) ((list* 'record-section $0))))
        (variant-part
         (seq (case variant-selector of variant (rep ((seq (semi variant) ($0))))) ((list* 'variant-part $0))))
        (variant-selector (seq ((opt ((seq (new-ident colon) ($0)))) type-ident) ((list* 'variant-selector $0))))
        (variant (seq (case-label-list colon opar field-list clop) ((list* 'variant $0))))
        (var-declarations (opt ((seq (var var-decl (rep ((seq (var-decl) ($0))))) ($0)))))
        (var-decl (seq (new-ident-list colon type semi) ((list* 'var-decl $0))))
        (proc-declarations (seq ((alt (proc-heading func-heading)) semi body semi) ((list* 'proc-declarations $0))))
        (proc-heading (seq (procedure new-ident (opt ((seq (formal-params) ($0))))) ((list* 'proc-heading $0))))
        (func-heading
         (seq (function new-ident (opt ((seq (formal-params) ($0)))) return-type) ((list* 'func-heading $0))))
        (return-type (opt ((seq (colon type-ident) ($0))))) (body (alt (block forward)))
        (formal-params
         (seq (opar formal-section (rep ((seq (semi formal-section) ($0)))) clop) ((list* 'formal-params $0))))
        (formal-section
         (seq ((opt ((seq (var) ($0)))) (alt (param-group proc-heading func-heading))) ((list* 'formal-section $0))))
        (param-group (seq (new-ident-list colon param-type) ((list* 'param-group $0))))
        (param-type
         (alt
          (type-ident (seq (array obra index-spec-list clob of param-type) ($0))
           (seq (packed array obra index-spec clob of type-ident) ($0)))))
        (index-spec-list (seq (index-spec (rep ((seq (semi index-spec) ($0))))) ((list* 'index-spec-list $0))))
        (index-spec (seq (new-ident dotdot new-ident colon type-ident) ((list* 'index-spec $0))))
        (statement-part (seq (compound-statement) ((list* 'statement-part $0))))
        (compound-statement (seq (begin statement-sequence end) ((list* 'compound-statement $0))))
        (statement-sequence (seq (statement (rep ((seq (semi statement) ($0))))) ((list* 'statement-sequence $0))))
        (statement
         (seq
          ((opt ((seq (a-label colon) ($0))))
           (alt
            (assignment-or-call compound-statement goto-statement while-statement repeat-statement if-statement
             case-statement for-statement with-statement)))
          ((list* 'statement $0))))
        (assignment-or-call
         (seq (designator (alt ((seq (assign expression) ($0)) (opt ((seq (actual-params) ($0)))))))
          ((list* 'assignment-or-call $0))))
        (actual-params
         (seq (opar actual-parameter (opt ((seq ((seq (comma actual-parameter) ($0))) ($0)))) clop)
          ((list* 'actual-params $0))))
        (actual-parameter (seq (expression (opt ((seq (field-width) ($0))))) ((list* 'actual-parameter $0))))
        (field-width
         (seq (colon integer-expression (opt ((seq (colon integer-expression) ($0))))) ((list* 'field-width $0))))
        (goto-statement (seq (goto a-label) ((list* 'goto-statement $0))))
        (while-statement (seq (while boolean-expression do statement) ((list* 'while-statement $0))))
        (repeat-statement (seq (repeat statement-sequence until boolean-expression) ((list* 'repeat-statement $0))))
        (if-statement
         (seq (if boolean-expression then statement (opt ((seq (else statement) ($0))))) ((list* 'if-statement $0))))
        (case-statement (seq (case ordinal-expression of case-list end) ((list* 'case-statement $0))))
        (case-list
         (seq (one-case (rep ((seq (semi one-case) ($0)))) (opt ((seq (semi) ($0))))) ((list* 'case-list $0))))
        (one-case (seq (case-label-list colon statement) ((list* 'one-case $0))))
        (case-label-list (seq (case-label (rep ((seq (comma case-label) ($0))))) ((list* 'case-label-list $0))))
        (case-label (seq (constant) ((list* 'case-label $0))))
        (for-statement
         (seq (for control-variable assign ordinal-expression (alt (to downto)) ordinal-expression do statement)
          ((list* 'for-statement $0))))
        (control-variable (seq (identifier) ((list* 'control-variable $0))))
        (with-statement (seq (with rec-var-list do statement) ((list* 'with-statement $0))))
        (rec-var-list (seq (designator (rep ((seq (comma designator) ($0))))) ((list* 'rec-var-list $0))))
        (integer-expression (seq (expression) ((list* 'integer-expression $0))))
        (boolean-expression (seq (expression) ((list* 'boolean-expression $0))))
        (ordinal-expression (seq (expression) ((list* 'ordinal-expression $0))))
        (expression (seq (simple-expression (opt ((seq (rel-op simple-expression) ($0))))) ((list* 'expression $0))))
        (rel-op (alt (eq lt gt le ge ne in)))
        (simple-expression
         (seq ((opt ((seq ((alt (plus minus))) ($0)))) term (rep ((seq (add-op term) ($0)))))
          ((list* 'simple-expression $0))))
        (add-op (alt (plus minus or))) (term (seq (factor (rep ((seq (mul-op factor) ($0))))) ((list* 'term $0))))
        (mul-op (alt (times slash div mod and)))
        (factor
         (alt
          ((seq (designator (opt ((seq (actual-params) ($0))))) ($0)) unsigned-literal set-constructor
           (seq (opar expression clop) ($0)) (seq (not factor) ($0)))))
        (designator
         (seq
          (identifier (rep ((seq ((alt ((seq (dot identifier) ($0)) (seq (obra exp-list clob) ($0)) expo))) ($0)))))
          ((list* 'designator $0))))
        (exp-list (seq (expression (rep ((seq (comma expression) ($0))))) ((list* 'exp-list $0))))
        (unsigned-literal (alt (unsigned-number nile a-string)))
        (set-constructor (seq (obra member (rep ((seq (comma member) ($0)))) clob) ((list* 'set-constructor $0))))
        (member (seq (expression (opt ((seq (dotdot expression) ($0))))) ((list* 'member $0))))
        (new-ident-list (seq (new-ident (rep ((seq (comma new-ident) ($0))))) ((list* 'new-ident-list $0))))
        (new-ident (seq (identifier) ((intern (token-text (second identifier))))))
        (unsigned-int (seq (integer) ((parse-integer (second integer)))))
        (unsigned-real (seq (real) ((read-from-string (second real)))))
        (a-string (seq (string) ((pascal-string-token-to-lisp (token-text (second string)))))))
      :scanner
      't
      :skip-spaces
      't))
  nil
  (progn (progn (defclass pascal-scanner (com.informatimago.common-lisp.parser.scanner:buffered-scanner)
                  nil
                  (:default-initargs :spaces " 
	"
                   :token-kind-package (load-time-value (find-package "PASCAL-GRAMMAR")) :token-symbol-package
                   (load-time-value (find-package "PASCAL-GRAMMAR"))))
                (defmethod scan-next-token ((com.informatimago.common-lisp.parser.scanner:scanner pascal-scanner)
                                            &optional com.informatimago.common-lisp.parser.scanner::parser-data)
                  "RETURN: (scanner-current-token scanner)"
                  (declare (ignore com.informatimago.common-lisp.parser.scanner::parser-data))
                  (let (com.informatimago.common-lisp.parser.scanner::match)
                    (setf com.informatimago.common-lisp.parser.scanner::match
                          (com.informatimago.common-lisp.regexp.regexp:string-match (format
                                                                                     nil
                                                                                     "^([~A]+)"
                                                                                     (coerce
                                                                                      (scanner-spaces
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      'string))
                                                                                    (scanner-buffer
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    :start
                                                                                    (1-
                                                                                     (scanner-column
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner))))
                    (when com.informatimago.common-lisp.parser.scanner::match
                      (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                            (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                       com.informatimago.common-lisp.parser.scanner::match))))
                    (let ((com.informatimago.common-lisp.parser.scanner::pos
                           (1- (scanner-column com.informatimago.common-lisp.parser.scanner:scanner))))
                      (cond ((scanner-end-of-source-p com.informatimago.common-lisp.parser.scanner:scanner)
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (length (scanner-buffer com.informatimago.common-lisp.parser.scanner:scanner)))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   "<END OF SOURCE>"
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.parser.scanner:token-end-of-source-kind)))
                            ((com.informatimago.common-lisp.parser.scanner:scanner-end-of-line-p
                               com.informatimago.common-lisp.parser.scanner:scanner)
                             (advance-line com.informatimago.common-lisp.parser.scanner:scanner))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\()"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'opar))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\))"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'clop))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\*)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'times))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\+)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'plus))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(,)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'comma))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(-)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'minus))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\.)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'dot))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\.\\.)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'dotdot))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(/)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'slash))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(:)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'colon))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(:=)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'assign))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(;)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'semi))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(<)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'lt))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(<=)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'le))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(<>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'ne))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(=)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'eq))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'gt))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(>=)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'ge))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\[)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'obra))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\])"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'clob))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\^)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'expo))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<and\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'and))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<array\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'array))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<begin\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'begin))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<case\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'case))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<const\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'const))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<div\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'div))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<do\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'do))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<downto\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'downto))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<else\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'else))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<end\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'end))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<file\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'file))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<for\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'for))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<forward\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'forward))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<function\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'function))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<goto\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'goto))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<if\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'if))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<in\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'in))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<label\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'label))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<mod\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'mod))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<nil\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'nile))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<not\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'not))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<of\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'of))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<or\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'or))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<packed\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'packed))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<procedure\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'procedure))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<program\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'program))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<record\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'record))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<repeat\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'repeat))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<set\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'set))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<then\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'then))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<to\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'to))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<type\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'type))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<unit\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'unit))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<until\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'until))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<var\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'var))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<while\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'while))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(\\<with\\>)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'with))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^([A-Za-z][A-Za-z0-9]*)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'identifier))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^([0-9][0-9]*)"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'integer))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^(([0-9][0-9]*\\.[0-9][0-9]*(E[-+]?[0-9][0-9]*)?)|([0-9][0-9]*E[-+]?[0-9][0-9]*))"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner) 'real))
                            ((setf com.informatimago.common-lisp.parser.scanner::match
                                   (com.informatimago.common-lisp.regexp.regexp:string-match '"^('([^'\\n]|'')*')"
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             :start
                                                                                             com.informatimago.common-lisp.parser.scanner::pos))
                             (setf (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                   (1+ (com.informatimago.common-lisp.regexp.regexp:match-end 1
                                                                                              com.informatimago.common-lisp.parser.scanner::match))
                                   (scanner-current-text com.informatimago.common-lisp.parser.scanner:scanner)
                                   (com.informatimago.common-lisp.regexp.regexp:match-string 1
                                                                                             (scanner-buffer
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             com.informatimago.common-lisp.parser.scanner::match)
                                   (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   'string))
                            (t
                             (error 'com.informatimago.common-lisp.parser.scanner:scanner-error-invalid-character
                                    :file
                                    (com.informatimago.common-lisp.parser.scanner:scanner-file com.informatimago.common-lisp.parser.scanner:scanner)
                                    :line
                                    (scanner-line com.informatimago.common-lisp.parser.scanner:scanner)
                                    :column
                                    (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)
                                    :state
                                    (scanner-state com.informatimago.common-lisp.parser.scanner:scanner)
                                    :current-token
                                    (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                    :scanner
                                    com.informatimago.common-lisp.parser.scanner:scanner
                                    :invalid-character
                                    (aref (scanner-buffer com.informatimago.common-lisp.parser.scanner:scanner)
                                          com.informatimago.common-lisp.parser.scanner::pos)
                                    :format-control
                                    "Invalid character ~S at position: ~D~%"
                                    :format-arguments
                                    (list (aref (scanner-buffer com.informatimago.common-lisp.parser.scanner:scanner)
                                                com.informatimago.common-lisp.parser.scanner::pos)
                                          (scanner-column com.informatimago.common-lisp.parser.scanner:scanner)))))))
                  (setf (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                        (com.informatimago.common-lisp.parser.scanner:make-current-token
                          com.informatimago.common-lisp.parser.scanner:scanner))))
         (setf (com.informatimago.rdp::grammar-scanner (grammar-named 'pascal)) 'pascal-scanner)
         'pascal-scanner)
  (progn (fmakunbound 'pascal/parse-new-ident)
         (defun pascal/parse-new-ident (com.informatimago.common-lisp.parser.scanner:scanner)
           "(--> new-ident (seq (identifier) ((intern (token-text (second identifier))))))"
           (com.informatimago.rdp::with-non-terminal
             (new-ident com.informatimago.common-lisp.parser.scanner:scanner)
             (cond ((word-equal (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                'identifier)
                    (let* (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner 'identifier))
                           (identifier $1)
                           (identifier.1 $1)
                           ($0 (list $1)))
                      (declare (ignorable $0 identifier.1 identifier $1))
                      (intern (token-text (second identifier)))))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                      com.informatimago.common-lisp.parser.scanner:scanner
                      '(identifier)
                      '(--> new-ident (alt (seq (identifier) ((intern (token-text (second identifier)))))))))))))
  (progn (fmakunbound 'pascal/parse-new-ident-list-1)
         (defun pascal/parse-new-ident-list-1 (com.informatimago.common-lisp.parser.scanner:scanner)
           "(--> new-ident-list-1 (alt ((seq nil ('nil)) (seq (new-ident-list-1-1 new-ident-list-1) ((cons $1 $2))))))"
           (com.informatimago.rdp::with-non-terminal
             (new-ident-list-1 com.informatimago.common-lisp.parser.scanner:scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond ((word-equal (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                          'comma)
                              (push (let* (($1
                                            (pascal/parse-new-ident-list-1-1
                                              com.informatimago.common-lisp.parser.scanner:scanner))
                                           ($2 (let* (($0 (list))) (declare (ignorable $0)) 'nil))
                                           ($0 (list $1 $2)))
                                      (declare (ignorable $0 $1 $2))
                                      (cons $1 $2))
                                    com.informatimago.rdp::$items))
                             (t (loop-finish)))
                   :finally (return (reduce #'append (nreverse com.informatimago.rdp::$items) :initial-value nil))))))
         (progn (fmakunbound 'pascal/parse-new-ident-list-1-1)
                (defun pascal/parse-new-ident-list-1-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                  "(--> new-ident-list-1-1 (seq (comma new-ident) ($0)))"
                  (com.informatimago.rdp::with-non-terminal
                    (new-ident-list-1-1 com.informatimago.common-lisp.parser.scanner:scanner)
                    (cond ((word-equal (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                       'comma)
                           (let* (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner 'comma))
                                  (comma $1)
                                  (comma.1 $1)
                                  ($2 (pascal/parse-new-ident com.informatimago.common-lisp.parser.scanner:scanner))
                                  (new-ident $2)
                                  (new-ident.1 $2)
                                  ($0 (list $1 $2)))
                             (declare (ignorable $0 new-ident.1 new-ident comma.1 comma $1 $2))
                             $0))
                          (t
                           (com.informatimago.rdp::error-unexpected-token
                             com.informatimago.common-lisp.parser.scanner:scanner
                             '(comma)
                             '(--> new-ident-list-1-1 (alt (seq (comma new-ident) ($0))))))))))
         (progn (fmakunbound 'pascal/parse-member-1)
                (defun pascal/parse-member-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                  "(--> member-1 (alt ((seq (dotdot expression) ($0)) (seq nil ('nil)))))"
                  (com.informatimago.rdp::with-non-terminal
                    (member-1 com.informatimago.common-lisp.parser.scanner:scanner)
                    (cond ((word-equal (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                       'dotdot)
                           (let* (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner 'dotdot))
                                  (dotdot $1)
                                  (dotdot.1 $1)
                                  ($2 (pascal/parse-expression com.informatimago.common-lisp.parser.scanner:scanner))
                                  (expression $2)
                                  (expression.1 $2)
                                  ($0 (list $1 $2)))
                             (declare (ignorable $0 expression.1 expression dotdot.1 dotdot $1 $2))
                             $0))))))
         (progn (fmakunbound 'pascal/parse-expression)
                (defun pascal/parse-expression (com.informatimago.common-lisp.parser.scanner:scanner)
                  "(--> expression (seq (simple-expression expression-1) ((list* 'expression $0))))"
                  (com.informatimago.rdp::with-non-terminal
                    (expression com.informatimago.common-lisp.parser.scanner:scanner)
                    (cond ((member (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   '(integer real string nile identifier not opar obra minus plus)
                                   :test
                                   #'word-equal)
                           (let* (($1
                                   (pascal/parse-simple-expression
                                     com.informatimago.common-lisp.parser.scanner:scanner))
                                  (simple-expression $1)
                                  (simple-expression.1 $1)
                                  ($2 (pascal/parse-expression-1 com.informatimago.common-lisp.parser.scanner:scanner))
                                  ($0 (list $1 $2)))
                             (declare (ignorable $0 simple-expression.1 simple-expression $1 $2))
                             (list* 'expression $0)))
                          (t
                           (com.informatimago.rdp::error-unexpected-token
                             com.informatimago.common-lisp.parser.scanner:scanner
                             '(integer real string nile identifier not opar obra minus plus)
                             '(--> expression
                               (alt (seq (simple-expression expression-1) ((list* 'expression $0)))))))))))
         (progn (fmakunbound 'pascal/parse-member)
                (defun pascal/parse-member (com.informatimago.common-lisp.parser.scanner:scanner)
                  "(--> member (seq (expression member-1) ((list* 'member $0))))"
                  (com.informatimago.rdp::with-non-terminal
                    (member com.informatimago.common-lisp.parser.scanner:scanner)
                    (cond ((member (scanner-current-token com.informatimago.common-lisp.parser.scanner:scanner)
                                   '(plus minus obra opar not identifier nile string real integer)
                                   :test
                                   #'word-equal)
                           (let* (($1 (pascal/parse-expression com.informatimago.common-lisp.parser.scanner:scanner))
                                  (expression $1)
                                  (expression.1 $1)
                                  ($2 (pascal/parse-member-1 com.informatimago.common-lisp.parser.scanner:scanner))
                                  ($0 (list $1 $2)))
                             (declare (ignorable $0 expression.1 expression $1 $2))
                             (list* 'member $0)))
                          (t
                           (com.informatimago.rdp::error-unexpected-token
                             com.informatimago.common-lisp.parser.scanner:scanner
                             '(plus minus obra opar not identifier nile string real integer)
                             '(--> member (alt (seq (expression member-1) ((list* 'member $0)))))))))))
         (progn (fmakunbound 'pascal/parse-set-constructor-1)
                (defun pascal/parse-set-constructor-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                  "(--> set-constructor-1 (alt ((seq nil ('nil)) (seq (set-constructor-1-1 set-constructor-1) ((cons $1 $2))))))"
                  (com.informatimago.rdp::with-non-terminal
                    (set-constructor-1 com.informatimago.common-lisp.parser.scanner:scanner)
                    (loop :with com.informatimago.rdp::$items := 'nil
                          :do (cond ((word-equal (scanner-current-token
                                                   com.informatimago.common-lisp.parser.scanner:scanner)
                                                 'comma)
                                     (push (let* (($1
                                                   (pascal/parse-set-constructor-1-1
                                                     com.informatimago.common-lisp.parser.scanner:scanner))
                                                  ($2 (let* (($0 (list))) (declare (ignorable $0)) 'nil))
                                                  ($0 (list $1 $2)))
                                             (declare (ignorable $0 $1 $2))
                                             (cons $1 $2))
                                           com.informatimago.rdp::$items))
                                    (t (loop-finish)))
                          :finally (return (reduce #'append
                                                   (nreverse com.informatimago.rdp::$items)
                                                   :initial-value
                                                   nil))))))
                (progn (fmakunbound 'pascal/parse-set-constructor-1-1)
                       (defun pascal/parse-set-constructor-1-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                         "(--> set-constructor-1-1 (seq (comma member) ($0)))"
                         (com.informatimago.rdp::with-non-terminal
                           (set-constructor-1-1 com.informatimago.common-lisp.parser.scanner:scanner)
                           (cond ((word-equal (scanner-current-token
                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                              'comma)
                                  (let* (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner 'comma))
                                         (comma $1)
                                         (comma.1 $1)
                                         ($2
                                          (pascal/parse-member com.informatimago.common-lisp.parser.scanner:scanner))
                                         (member $2)
                                         (member.1 $2)
                                         ($0 (list $1 $2)))
                                    (declare (ignorable $0 member.1 member comma.1 comma $1 $2))
                                    $0))
                                 (t
                                  (com.informatimago.rdp::error-unexpected-token
                                    com.informatimago.common-lisp.parser.scanner:scanner
                                    '(comma)
                                    '(--> set-constructor-1-1 (alt (seq (comma member) ($0))))))))))
                (progn (fmakunbound 'pascal/parse-a-string)
                       (defun pascal/parse-a-string (com.informatimago.common-lisp.parser.scanner:scanner)
                         "(--> a-string (seq (string) ((pascal-string-token-to-lisp (token-text (second string))))))"
                         (com.informatimago.rdp::with-non-terminal
                           (a-string com.informatimago.common-lisp.parser.scanner:scanner)
                           (cond ((word-equal (scanner-current-token
                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                              'string)
                                  (let* (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner 'string))
                                         (string $1)
                                         (string.1 $1)
                                         ($0 (list $1)))
                                    (declare (ignorable $0 string.1 string $1))
                                    (pascal-string-token-to-lisp (token-text (second string)))))
                                 (t
                                  (com.informatimago.rdp::error-unexpected-token
                                    com.informatimago.common-lisp.parser.scanner:scanner
                                    '(string)
                                    '(--> a-string
                                      (alt
                                       (seq (string)
                                        ((pascal-string-token-to-lisp (token-text (second string)))))))))))))
                (progn (fmakunbound 'pascal/parse-unsigned-number)
                       (defun pascal/parse-unsigned-number (com.informatimago.common-lisp.parser.scanner:scanner)
                         "(--> unsigned-number (alt ((seq (real) ((read-from-string (second real)))) (seq (unsigned-int) ($1)))))"
                         (com.informatimago.rdp::with-non-terminal
                           (unsigned-number com.informatimago.common-lisp.parser.scanner:scanner)
                           (cond ((word-equal (scanner-current-token
                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                              'real)
                                  (let* (($1 (accept com.informatimago.common-lisp.parser.scanner:scanner 'real))
                                         (real $1)
                                         (real.1 $1)
                                         ($0 (list $1)))
                                    (declare (ignorable $0 real.1 real $1))
                                    (read-from-string (second real))))
                                 ((word-equal (scanner-current-token
                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                              'integer)
                                  (let* (($1
                                          (pascal/parse-unsigned-int
                                            com.informatimago.common-lisp.parser.scanner:scanner))
                                         (unsigned-int $1)
                                         (unsigned-int.1 $1)
                                         ($0 (list $1)))
                                    (declare (ignorable $0 unsigned-int.1 unsigned-int $1))
                                    $1))
                                 (t
                                  (com.informatimago.rdp::error-unexpected-token
                                    com.informatimago.common-lisp.parser.scanner:scanner
                                    '(real integer)
                                    '(--> unsigned-number
                                      (alt (seq (real) ((read-from-string (second real))))
                                       (seq (unsigned-int) ($1))))))))))
                (progn (fmakunbound 'pascal/parse-exp-list-1)
                       (defun pascal/parse-exp-list-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                         "(--> exp-list-1 (alt ((seq nil ('nil)) (seq (exp-list-1-1 exp-list-1) ((cons $1 $2))))))"
                         (com.informatimago.rdp::with-non-terminal
                           (exp-list-1 com.informatimago.common-lisp.parser.scanner:scanner)
                           (loop :with com.informatimago.rdp::$items := 'nil
                                 :do (cond ((word-equal (scanner-current-token
                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                        'comma)
                                            (push (let* (($1
                                                          (pascal/parse-exp-list-1-1
                                                            com.informatimago.common-lisp.parser.scanner:scanner))
                                                         ($2 (let* (($0 (list))) (declare (ignorable $0)) 'nil))
                                                         ($0 (list $1 $2)))
                                                    (declare (ignorable $0 $1 $2))
                                                    (cons $1 $2))
                                                  com.informatimago.rdp::$items))
                                           (t (loop-finish)))
                                 :finally (return (reduce #'append
                                                          (nreverse com.informatimago.rdp::$items)
                                                          :initial-value
                                                          nil))))))
                       (progn (fmakunbound 'pascal/parse-exp-list-1-1)
                              (defun pascal/parse-exp-list-1-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                "(--> exp-list-1-1 (seq (comma expression) ($0)))"
                                (com.informatimago.rdp::with-non-terminal
                                  (exp-list-1-1 com.informatimago.common-lisp.parser.scanner:scanner)
                                  (cond ((word-equal (scanner-current-token
                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                     'comma)
                                         (let* (($1
                                                 (accept com.informatimago.common-lisp.parser.scanner:scanner 'comma))
                                                (comma $1)
                                                (comma.1 $1)
                                                ($2
                                                 (pascal/parse-expression
                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                (expression $2)
                                                (expression.1 $2)
                                                ($0 (list $1 $2)))
                                           (declare (ignorable $0 expression.1 expression comma.1 comma $1 $2))
                                           $0))
                                        (t
                                         (com.informatimago.rdp::error-unexpected-token
                                           com.informatimago.common-lisp.parser.scanner:scanner
                                           '(comma)
                                           '(--> exp-list-1-1 (alt (seq (comma expression) ($0))))))))))
                       (progn (fmakunbound 'pascal/parse-designator-1)
                              (defun pascal/parse-designator-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                "(--> designator-1 (alt ((seq nil ('nil)) (seq (designator-1-1 designator-1) ((cons (progn $0) $2))))))"
                                (com.informatimago.rdp::with-non-terminal
                                  (designator-1 com.informatimago.common-lisp.parser.scanner:scanner)
                                  (loop :with com.informatimago.rdp::$items := 'nil
                                        :do (cond ((member (scanner-current-token
                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                           '(expo dot obra)
                                                           :test
                                                           #'word-equal)
                                                   (push (let* (($1
                                                                 (pascal/parse-designator-1-1
                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                ($2 (let* (($0 (list))) (declare (ignorable $0)) 'nil))
                                                                ($0 (list $1 $2)))
                                                           (declare (ignorable $0 $1 $2))
                                                           (cons (progn $0) $2))
                                                         com.informatimago.rdp::$items))
                                                  (t (loop-finish)))
                                        :finally (return (reduce #'append
                                                                 (nreverse com.informatimago.rdp::$items)
                                                                 :initial-value
                                                                 nil))))))
                              (progn (fmakunbound 'pascal/parse-designator-1-1)
                                     (defun pascal/parse-designator-1-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                       "(--> designator-1-1 (alt ((seq (obra exp-list clob) ($0)) (seq (dot identifier) ($0)) (seq (expo) ($1)))))"
                                       (com.informatimago.rdp::with-non-terminal
                                         (designator-1-1 com.informatimago.common-lisp.parser.scanner:scanner)
                                         (cond ((word-equal (scanner-current-token
                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                            'obra)
                                                (let* (($1
                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                'obra))
                                                       (obra $1)
                                                       (obra.1 $1)
                                                       ($2
                                                        (pascal/parse-exp-list
                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                       (exp-list $2)
                                                       (exp-list.1 $2)
                                                       ($3
                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                'clob))
                                                       (clob $3)
                                                       (clob.1 $3)
                                                       ($0 (list $1 $2 $3)))
                                                  (declare
                                                   (ignorable $0 clob.1 clob exp-list.1 exp-list obra.1 obra $1 $2 $3))
                                                  $0))
                                               ((word-equal (scanner-current-token
                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                            'dot)
                                                (let* (($1
                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                'dot))
                                                       (dot $1)
                                                       (dot.1 $1)
                                                       ($2
                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                'identifier))
                                                       (identifier $2)
                                                       (identifier.1 $2)
                                                       ($0 (list $1 $2)))
                                                  (declare (ignorable $0 identifier.1 identifier dot.1 dot $1 $2))
                                                  $0))
                                               ((word-equal (scanner-current-token
                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                            'expo)
                                                (let* (($1
                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                'expo))
                                                       (expo $1)
                                                       (expo.1 $1)
                                                       ($0 (list $1)))
                                                  (declare (ignorable $0 expo.1 expo $1))
                                                  $1))
                                               (t
                                                (com.informatimago.rdp::error-unexpected-token
                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                  '(obra dot expo)
                                                  '(--> designator-1-1
                                                    (alt (seq (obra exp-list clob) ($0)) (seq (dot identifier) ($0))
                                                     (seq (expo) ($1))))))))))
                              (progn (fmakunbound 'pascal/parse-actual-params)
                                     (defun pascal/parse-actual-params (com.informatimago.common-lisp.parser.scanner:scanner)
                                       "(--> actual-params (seq (opar actual-parameter actual-params-1 clop) ((list* 'actual-params $0))))"
                                       (com.informatimago.rdp::with-non-terminal
                                         (actual-params com.informatimago.common-lisp.parser.scanner:scanner)
                                         (cond ((word-equal (scanner-current-token
                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                            'opar)
                                                (let* (($1
                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                'opar))
                                                       (opar $1)
                                                       (opar.1 $1)
                                                       ($2
                                                        (pascal/parse-actual-parameter
                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                       (actual-parameter $2)
                                                       (actual-parameter.1 $2)
                                                       ($3
                                                        (pascal/parse-actual-params-1
                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                       ($4
                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                'clop))
                                                       (clop $4)
                                                       (clop.1 $4)
                                                       ($0 (list $1 $2 $3 $4)))
                                                  (declare
                                                   (ignorable $0 clop.1 clop actual-parameter.1 actual-parameter opar.1
                                                    opar $1 $2 $3 $4))
                                                  (list* 'actual-params $0)))
                                               (t
                                                (com.informatimago.rdp::error-unexpected-token
                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                  '(opar)
                                                  '(--> actual-params
                                                    (alt
                                                     (seq (opar actual-parameter actual-params-1 clop)
                                                      ((list* 'actual-params $0)))))))))))
                              (progn (fmakunbound 'pascal/parse-unsigned-literal)
                                     (defun pascal/parse-unsigned-literal (com.informatimago.common-lisp.parser.scanner:scanner)
                                       "(--> unsigned-literal (alt ((seq (unsigned-number) ($1)) (seq (a-string) ($1)) (seq (nile) ($1)))))"
                                       (com.informatimago.rdp::with-non-terminal
                                         (unsigned-literal com.informatimago.common-lisp.parser.scanner:scanner)
                                         (cond ((member (scanner-current-token
                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                        '(integer real)
                                                        :test
                                                        #'word-equal)
                                                (let* (($1
                                                        (pascal/parse-unsigned-number
                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                       (unsigned-number $1)
                                                       (unsigned-number.1 $1)
                                                       ($0 (list $1)))
                                                  (declare (ignorable $0 unsigned-number.1 unsigned-number $1))
                                                  $1))
                                               ((word-equal (scanner-current-token
                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                            'string)
                                                (let* (($1
                                                        (pascal/parse-a-string
                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                       (a-string $1)
                                                       (a-string.1 $1)
                                                       ($0 (list $1)))
                                                  (declare (ignorable $0 a-string.1 a-string $1))
                                                  $1))
                                               ((word-equal (scanner-current-token
                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                            'nile)
                                                (let* (($1
                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                'nile))
                                                       (nile $1)
                                                       (nile.1 $1)
                                                       ($0 (list $1)))
                                                  (declare (ignorable $0 nile.1 nile $1))
                                                  $1))
                                               (t
                                                (com.informatimago.rdp::error-unexpected-token
                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                  '(integer real string nile)
                                                  '(--> unsigned-literal
                                                    (alt (seq (unsigned-number) ($1)) (seq (a-string) ($1))
                                                     (seq (nile) ($1))))))))))
                              (progn (fmakunbound 'pascal/parse-factor)
                                     (defun pascal/parse-factor (com.informatimago.common-lisp.parser.scanner:scanner)
                                       "(--> factor (alt ((seq (obra member set-constructor-1 clob) ((list* 'set-constructor $0))) (seq (opar expression clop) ($0)) (seq (not factor) ($0)) (seq (designator factor-1-1) ($0)) (seq (unsigned-literal) ($1)))))"
                                       (com.informatimago.rdp::with-non-terminal
                                         (factor com.informatimago.common-lisp.parser.scanner:scanner)
                                         (loop :with com.informatimago.rdp::$items := 'nil
                                               :do (cond ((word-equal (scanner-current-token
                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                      'not)
                                                          (push (let* (($1
                                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                                'not))
                                                                       (not $1)
                                                                       (not.1 $1)
                                                                       ($2
                                                                        (let* (($0 (list)))
                                                                          (declare (ignorable $0))
                                                                          'nil))
                                                                       ($0 (list $1 $2)))
                                                                  (declare (ignorable $0 not.1 not $1 $2))
                                                                  $0)
                                                                com.informatimago.rdp::$items))
                                                         ((word-equal (scanner-current-token
                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                      'obra)
                                                          (push (let* (($1
                                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                                'obra))
                                                                       (obra $1)
                                                                       (obra.1 $1)
                                                                       ($2
                                                                        (pascal/parse-member
                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                       (member $2)
                                                                       (member.1 $2)
                                                                       ($3
                                                                        (pascal/parse-set-constructor-1
                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                       ($4
                                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                                'clob))
                                                                       (clob $4)
                                                                       (clob.1 $4)
                                                                       ($0 (list $1 $2 $3 $4)))
                                                                  (declare
                                                                   (ignorable $0 clob.1 clob member.1 member obra.1
                                                                    obra $1 $2 $3 $4))
                                                                  (list* 'set-constructor $0))
                                                                com.informatimago.rdp::$items)
                                                          (loop-finish))
                                                         ((word-equal (scanner-current-token
                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                      'opar)
                                                          (push (let* (($1
                                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                                'opar))
                                                                       (opar $1)
                                                                       (opar.1 $1)
                                                                       ($2
                                                                        (pascal/parse-expression
                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                       (expression $2)
                                                                       (expression.1 $2)
                                                                       ($3
                                                                        (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                                'clop))
                                                                       (clop $3)
                                                                       (clop.1 $3)
                                                                       ($0 (list $1 $2 $3)))
                                                                  (declare
                                                                   (ignorable $0 clop.1 clop expression.1 expression
                                                                    opar.1 opar $1 $2 $3))
                                                                  $0)
                                                                com.informatimago.rdp::$items)
                                                          (loop-finish))
                                                         ((word-equal (scanner-current-token
                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                      'identifier)
                                                          (push (let* (($1
                                                                        (pascal/parse-designator
                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                       (designator $1)
                                                                       (designator.1 $1)
                                                                       ($2
                                                                        (pascal/parse-factor-1-1
                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                       ($0 (list $1 $2)))
                                                                  (declare
                                                                   (ignorable $0 designator.1 designator $1 $2))
                                                                  $0)
                                                                com.informatimago.rdp::$items)
                                                          (loop-finish))
                                                         ((member (scanner-current-token
                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                  '(nile string real integer)
                                                                  :test
                                                                  #'word-equal)
                                                          (push (let* (($1
                                                                        (pascal/parse-unsigned-literal
                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                       (unsigned-literal $1)
                                                                       (unsigned-literal.1 $1)
                                                                       ($0 (list $1)))
                                                                  (declare
                                                                   (ignorable $0 unsigned-literal.1 unsigned-literal
                                                                    $1))
                                                                  $1)
                                                                com.informatimago.rdp::$items)
                                                          (loop-finish))
                                                         (t
                                                          (com.informatimago.rdp::error-unexpected-token
                                                            com.informatimago.common-lisp.parser.scanner:scanner
                                                            '(obra opar identifier nile string real integer not)
                                                            '(--> factor
                                                              (alt
                                                               (seq (obra member set-constructor-1 clob)
                                                                ((list* 'set-constructor $0)))
                                                               (seq (opar expression clop) ($0))
                                                               (seq (not factor) ($0))
                                                               (seq (designator factor-1-1) ($0))
                                                               (seq (unsigned-literal) ($1)))))))
                                               :finally (return (reduce #'append
                                                                        (nreverse com.informatimago.rdp::$items)
                                                                        :initial-value
                                                                        nil))))))
                                     (progn (fmakunbound 'pascal/parse-mul-op)
                                            (defun pascal/parse-mul-op (com.informatimago.common-lisp.parser.scanner:scanner)
                                              "(--> mul-op (alt ((seq (times) ($1)) (seq (and) ($1)) (seq (mod) ($1)) (seq (div) ($1)) (seq (slash) ($1)))))"
                                              (com.informatimago.rdp::with-non-terminal
                                                (mul-op com.informatimago.common-lisp.parser.scanner:scanner)
                                                (cond ((word-equal (scanner-current-token
                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                   'times)
                                                       (let* (($1
                                                               (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                       'times))
                                                              (times $1)
                                                              (times.1 $1)
                                                              ($0 (list $1)))
                                                         (declare (ignorable $0 times.1 times $1))
                                                         $1))
                                                      ((word-equal (scanner-current-token
                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                   'and)
                                                       (let* (($1
                                                               (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                       'and))
                                                              (and $1)
                                                              (and.1 $1)
                                                              ($0 (list $1)))
                                                         (declare (ignorable $0 and.1 and $1))
                                                         $1))
                                                      ((word-equal (scanner-current-token
                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                   'mod)
                                                       (let* (($1
                                                               (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                       'mod))
                                                              (mod $1)
                                                              (mod.1 $1)
                                                              ($0 (list $1)))
                                                         (declare (ignorable $0 mod.1 mod $1))
                                                         $1))
                                                      ((word-equal (scanner-current-token
                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                   'div)
                                                       (let* (($1
                                                               (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                       'div))
                                                              (div $1)
                                                              (div.1 $1)
                                                              ($0 (list $1)))
                                                         (declare (ignorable $0 div.1 div $1))
                                                         $1))
                                                      ((word-equal (scanner-current-token
                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                   'slash)
                                                       (let* (($1
                                                               (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                       'slash))
                                                              (slash $1)
                                                              (slash.1 $1)
                                                              ($0 (list $1)))
                                                         (declare (ignorable $0 slash.1 slash $1))
                                                         $1))
                                                      (t
                                                       (com.informatimago.rdp::error-unexpected-token
                                                         com.informatimago.common-lisp.parser.scanner:scanner
                                                         '(times and mod div slash)
                                                         '(--> mul-op
                                                           (alt (seq (times) ($1)) (seq (and) ($1)) (seq (mod) ($1))
                                                            (seq (div) ($1)) (seq (slash) ($1))))))))))
                                     (progn (fmakunbound 'pascal/parse-term-1)
                                            (defun pascal/parse-term-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                              "(--> term-1 (alt ((seq nil ('nil)) (seq (term-1-1 term-1) ((cons $1 $2))))))"
                                              (com.informatimago.rdp::with-non-terminal
                                                (term-1 com.informatimago.common-lisp.parser.scanner:scanner)
                                                (loop :with com.informatimago.rdp::$items := 'nil
                                                      :do (cond ((member (scanner-current-token
                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                         '(times and mod div slash)
                                                                         :test
                                                                         #'word-equal)
                                                                 (push (let* (($1
                                                                               (pascal/parse-term-1-1
                                                                                com.informatimago.common-lisp.parser.scanner:scanner))
                                                                              ($2
                                                                               (let*
                                                                                (($0 (list)))
                                                                                (declare (ignorable $0))
                                                                                'nil))
                                                                              ($0 (list $1 $2)))
                                                                         (declare (ignorable $0 $1 $2))
                                                                         (cons $1 $2))
                                                                       com.informatimago.rdp::$items))
                                                                (t (loop-finish)))
                                                      :finally (return (reduce #'append
                                                                               (nreverse com.informatimago.rdp::$items)
                                                                               :initial-value
                                                                               nil))))))
                                            (progn (fmakunbound 'pascal/parse-term-1-1)
                                                   (defun pascal/parse-term-1-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                     "(--> term-1-1 (seq (mul-op factor) ($0)))"
                                                     (com.informatimago.rdp::with-non-terminal
                                                       (term-1-1 com.informatimago.common-lisp.parser.scanner:scanner)
                                                       (cond ((member (scanner-current-token
                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                      '(slash div mod and times)
                                                                      :test
                                                                      #'word-equal)
                                                              (let* (($1
                                                                      (pascal/parse-mul-op
                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                     (mul-op $1)
                                                                     (mul-op.1 $1)
                                                                     ($2
                                                                      (pascal/parse-factor
                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                     (factor $2)
                                                                     (factor.1 $2)
                                                                     ($0 (list $1 $2)))
                                                                (declare
                                                                 (ignorable $0 factor.1 factor mul-op.1 mul-op $1 $2))
                                                                $0))
                                                             (t
                                                              (com.informatimago.rdp::error-unexpected-token
                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                '(slash div mod and times)
                                                                '(--> term-1-1 (alt (seq (mul-op factor) ($0))))))))))
                                            (progn (fmakunbound 'pascal/parse-simple-expression-1-1)
                                                   (defun pascal/parse-simple-expression-1-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                     "(--> simple-expression-1-1 (alt ((seq (plus) ($1)) (seq (minus) ($1)))))"
                                                     (com.informatimago.rdp::with-non-terminal
                                                       (simple-expression-1-1
                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                       (cond ((word-equal (scanner-current-token
                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          'plus)
                                                              (let* (($1
                                                                      (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                              'plus))
                                                                     (plus $1)
                                                                     (plus.1 $1)
                                                                     ($0 (list $1)))
                                                                (declare (ignorable $0 plus.1 plus $1))
                                                                $1))
                                                             ((word-equal (scanner-current-token
                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          'minus)
                                                              (let* (($1
                                                                      (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                              'minus))
                                                                     (minus $1)
                                                                     (minus.1 $1)
                                                                     ($0 (list $1)))
                                                                (declare (ignorable $0 minus.1 minus $1))
                                                                $1))
                                                             (t
                                                              (com.informatimago.rdp::error-unexpected-token
                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                '(plus minus)
                                                                '(--> simple-expression-1-1
                                                                  (alt (seq (plus) ($1)) (seq (minus) ($1))))))))))
                                            (progn (fmakunbound 'pascal/parse-term)
                                                   (defun pascal/parse-term (com.informatimago.common-lisp.parser.scanner:scanner)
                                                     "(--> term (seq (factor term-1) ((list* 'term $0))))"
                                                     (com.informatimago.rdp::with-non-terminal
                                                       (term com.informatimago.common-lisp.parser.scanner:scanner)
                                                       (cond ((member (scanner-current-token
                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                      '(integer real string nile identifier not opar
                                                                        obra)
                                                                      :test
                                                                      #'word-equal)
                                                              (let* (($1
                                                                      (pascal/parse-factor
                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                     (factor $1)
                                                                     (factor.1 $1)
                                                                     ($2
                                                                      (pascal/parse-term-1
                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                     ($0 (list $1 $2)))
                                                                (declare (ignorable $0 factor.1 factor $1 $2))
                                                                (list* 'term $0)))
                                                             (t
                                                              (com.informatimago.rdp::error-unexpected-token
                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                '(integer real string nile identifier not opar obra)
                                                                '(--> term
                                                                  (alt (seq (factor term-1) ((list* 'term $0)))))))))))
                                            (progn (fmakunbound 'pascal/parse-add-op)
                                                   (defun pascal/parse-add-op (com.informatimago.common-lisp.parser.scanner:scanner)
                                                     "(--> add-op (alt ((seq (plus) ($1)) (seq (or) ($1)) (seq (minus) ($1)))))"
                                                     (com.informatimago.rdp::with-non-terminal
                                                       (add-op com.informatimago.common-lisp.parser.scanner:scanner)
                                                       (cond ((word-equal (scanner-current-token
                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          'plus)
                                                              (let* (($1
                                                                      (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                              'plus))
                                                                     (plus $1)
                                                                     (plus.1 $1)
                                                                     ($0 (list $1)))
                                                                (declare (ignorable $0 plus.1 plus $1))
                                                                $1))
                                                             ((word-equal (scanner-current-token
                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          'or)
                                                              (let* (($1
                                                                      (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                              'or))
                                                                     (or $1)
                                                                     (or.1 $1)
                                                                     ($0 (list $1)))
                                                                (declare (ignorable $0 or.1 or $1))
                                                                $1))
                                                             ((word-equal (scanner-current-token
                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          'minus)
                                                              (let* (($1
                                                                      (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                              'minus))
                                                                     (minus $1)
                                                                     (minus.1 $1)
                                                                     ($0 (list $1)))
                                                                (declare (ignorable $0 minus.1 minus $1))
                                                                $1))
                                                             (t
                                                              (com.informatimago.rdp::error-unexpected-token
                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                '(plus or minus)
                                                                '(--> add-op
                                                                  (alt (seq (plus) ($1)) (seq (or) ($1))
                                                                   (seq (minus) ($1))))))))))
                                            (progn (fmakunbound 'pascal/parse-simple-expression-2)
                                                   (defun pascal/parse-simple-expression-2 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                     "(--> simple-expression-2 (alt ((seq nil ('nil)) (seq (simple-expression-2-1 simple-expression-2) ((cons $1 $2))))))"
                                                     (com.informatimago.rdp::with-non-terminal
                                                       (simple-expression-2
                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                       (loop :with com.informatimago.rdp::$items := 'nil
                                                             :do (cond ((member (scanner-current-token
                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                '(plus or minus)
                                                                                :test
                                                                                #'word-equal)
                                                                        (push (let* (($1
                                                                                      (pascal/parse-simple-expression-2-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($2
                                                                                      (let*
                                                                                       (($0 (list)))
                                                                                       (declare (ignorable $0))
                                                                                       'nil))
                                                                                     ($0 (list $1 $2)))
                                                                                (declare (ignorable $0 $1 $2))
                                                                                (cons $1 $2))
                                                                              com.informatimago.rdp::$items))
                                                                       (t (loop-finish)))
                                                             :finally (return (reduce #'append
                                                                                      (nreverse
                                                                                       com.informatimago.rdp::$items)
                                                                                      :initial-value
                                                                                      nil))))))
                                                   (progn (fmakunbound 'pascal/parse-simple-expression-2-1)
                                                          (defun pascal/parse-simple-expression-2-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                            "(--> simple-expression-2-1 (seq (add-op term) ($0)))"
                                                            (com.informatimago.rdp::with-non-terminal
                                                              (simple-expression-2-1
                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                              (cond ((member (scanner-current-token
                                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                                             '(minus or plus)
                                                                             :test
                                                                             #'word-equal)
                                                                     (let* (($1
                                                                             (pascal/parse-add-op
                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                            (add-op $1)
                                                                            (add-op.1 $1)
                                                                            ($2
                                                                             (pascal/parse-term
                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                            (term $2)
                                                                            (term.1 $2)
                                                                            ($0 (list $1 $2)))
                                                                       (declare
                                                                        (ignorable $0 term.1 term add-op.1 add-op $1
                                                                         $2))
                                                                       $0))
                                                                    (t
                                                                     (com.informatimago.rdp::error-unexpected-token
                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                       '(minus or plus)
                                                                       '(--> simple-expression-2-1
                                                                         (alt (seq (add-op term) ($0))))))))))
                                                   (progn (fmakunbound 'pascal/parse-simple-expression-1)
                                                          (defun pascal/parse-simple-expression-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                            "(--> simple-expression-1 (alt ((seq nil ('nil)) (seq (simple-expression-1-1) ($0)))))"
                                                            (com.informatimago.rdp::with-non-terminal
                                                              (simple-expression-1
                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                              (cond ((member (scanner-current-token
                                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                                             '(minus plus)
                                                                             :test
                                                                             #'word-equal)
                                                                     (let* (($1
                                                                             (pascal/parse-simple-expression-1-1
                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                            ($0 (list $1)))
                                                                       (declare (ignorable $0 $1))
                                                                       $0))))))
                                                   (progn (fmakunbound 'pascal/parse-expression-1)
                                                          (defun pascal/parse-expression-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                            "(--> expression-1 (alt ((seq (rel-op simple-expression) ($0)) (seq nil ('nil)))))"
                                                            (com.informatimago.rdp::with-non-terminal
                                                              (expression-1
                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                              (cond ((member (scanner-current-token
                                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                                             '(lt gt le ge ne in eq)
                                                                             :test
                                                                             #'word-equal)
                                                                     (let* (($1
                                                                             (pascal/parse-rel-op
                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                            (rel-op $1)
                                                                            (rel-op.1 $1)
                                                                            ($2
                                                                             (pascal/parse-simple-expression
                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                            (simple-expression $2)
                                                                            (simple-expression.1 $2)
                                                                            ($0 (list $1 $2)))
                                                                       (declare
                                                                        (ignorable $0 simple-expression.1
                                                                         simple-expression rel-op.1 rel-op $1 $2))
                                                                       $0))))))
                                                   (progn (fmakunbound 'pascal/parse-simple-expression)
                                                          (defun pascal/parse-simple-expression (com.informatimago.common-lisp.parser.scanner:scanner)
                                                            "(--> simple-expression (seq (simple-expression-1 term simple-expression-2) ((list* 'simple-expression $0))))"
                                                            (com.informatimago.rdp::with-non-terminal
                                                              (simple-expression
                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                              (cond ((member (scanner-current-token
                                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                                             '(nil minus plus integer real string nile
                                                                               identifier not opar obra clop comma
                                                                               dotdot colon to downto do of clob until
                                                                               end semi else then eq in ne ge le gt lt)
                                                                             :test
                                                                             #'word-equal)
                                                                     (let* (($1
                                                                             (pascal/parse-simple-expression-1
                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                            ($2
                                                                             (pascal/parse-term
                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                            (term $2)
                                                                            (term.1 $2)
                                                                            ($3
                                                                             (pascal/parse-simple-expression-2
                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                            ($0 (list $1 $2 $3)))
                                                                       (declare (ignorable $0 term.1 term $1 $2 $3))
                                                                       (list* 'simple-expression $0)))
                                                                    (t
                                                                     (com.informatimago.rdp::error-unexpected-token
                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                       '(nil minus plus integer real string nile
                                                                         identifier not opar obra clop comma dotdot
                                                                         colon to downto do of clob until end semi else
                                                                         then eq in ne ge le gt lt)
                                                                       '(--> simple-expression
                                                                         (alt
                                                                          (seq
                                                                           (simple-expression-1 term
                                                                            simple-expression-2)
                                                                           ((list* 'simple-expression $0)))))))))))
                                                   (progn (fmakunbound 'pascal/parse-designator)
                                                          (defun pascal/parse-designator (com.informatimago.common-lisp.parser.scanner:scanner)
                                                            "(--> designator (seq (identifier designator-1) ((list* 'designator $0))))"
                                                            (com.informatimago.rdp::with-non-terminal
                                                              (designator
                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                              (cond ((word-equal (scanner-current-token
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 'identifier)
                                                                     (let* (($1
                                                                             (accept com.informatimago.common-lisp.parser.scanner:scanner
                                                                                     'identifier))
                                                                            (identifier $1)
                                                                            (identifier.1 $1)
                                                                            ($2
                                                                             (pascal/parse-designator-1
                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                            ($0 (list $1 $2)))
                                                                       (declare
                                                                        (ignorable $0 identifier.1 identifier $1 $2))
                                                                       (list* 'designator $0)))
                                                                    (t
                                                                     (com.informatimago.rdp::error-unexpected-token
                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                       '(identifier)
                                                                       '(--> designator
                                                                         (alt
                                                                          (seq (identifier designator-1)
                                                                           ((list* 'designator $0)))))))))))
                                                   (progn (fmakunbound 'pascal/parse-rec-var-list-1)
                                                          (defun pascal/parse-rec-var-list-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                            "(--> rec-var-list-1 (alt ((seq nil ('nil)) (seq (rec-var-list-1-1 rec-var-list-1) ((cons $1 $2))))))"
                                                            (com.informatimago.rdp::with-non-terminal
                                                              (rec-var-list-1
                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                              (loop :with com.informatimago.rdp::$items := 'nil
                                                                    :do (cond ((word-equal
                                                                                (scanner-current-token
                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                'comma)
                                                                               (push
                                                                                (let*
                                                                                 (($1
                                                                                   (pascal/parse-rec-var-list-1-1
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                  ($2
                                                                                   (let*
                                                                                    (($0 (list)))
                                                                                    (declare (ignorable $0))
                                                                                    'nil))
                                                                                  ($0 (list $1 $2)))
                                                                                 (declare (ignorable $0 $1 $2))
                                                                                 (cons $1 $2))
                                                                                com.informatimago.rdp::$items))
                                                                              (t (loop-finish)))
                                                                    :finally (return (reduce
                                                                                      #'append
                                                                                      (nreverse
                                                                                       com.informatimago.rdp::$items)
                                                                                      :initial-value
                                                                                      nil))))))
                                                          (progn (fmakunbound 'pascal/parse-rec-var-list-1-1)
                                                                 (defun pascal/parse-rec-var-list-1-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                   "(--> rec-var-list-1-1 (seq (comma designator) ($0)))"
                                                                   (com.informatimago.rdp::with-non-terminal
                                                                     (rec-var-list-1-1
                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                     (cond ((word-equal (scanner-current-token
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        'comma)
                                                                            (let* (($1
                                                                                    (accept
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner
                                                                                     'comma))
                                                                                   (comma $1)
                                                                                   (comma.1 $1)
                                                                                   ($2
                                                                                    (pascal/parse-designator
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                   (designator $2)
                                                                                   (designator.1 $2)
                                                                                   ($0 (list $1 $2)))
                                                                              (declare
                                                                               (ignorable
                                                                                $0
                                                                                designator.1
                                                                                designator
                                                                                comma.1
                                                                                comma
                                                                                $1
                                                                                $2))
                                                                              $0))
                                                                           (t
                                                                            (com.informatimago.rdp::error-unexpected-token
                                                                              com.informatimago.common-lisp.parser.scanner:scanner
                                                                              '(comma)
                                                                              '(-->
                                                                                rec-var-list-1-1
                                                                                (alt
                                                                                 (seq (comma designator) ($0))))))))))
                                                          (progn (fmakunbound 'pascal/parse-constant)
                                                                 (defun pascal/parse-constant (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                   "(--> constant (alt ((seq (constant-1-1 constant-1-2) ($0)) (seq (a-string) ($1)))))"
                                                                   (com.informatimago.rdp::with-non-terminal
                                                                     (constant
                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                     (cond ((member (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    '(nil
                                                                                      minus
                                                                                      plus
                                                                                      integer
                                                                                      real
                                                                                      identifier
                                                                                      colon
                                                                                      comma
                                                                                      clob
                                                                                      dotdot
                                                                                      semi)
                                                                                    :test
                                                                                    #'word-equal)
                                                                            (let* (($1
                                                                                    (pascal/parse-constant-1-1
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                   ($2
                                                                                    (pascal/parse-constant-1-2
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                   ($0 (list $1 $2)))
                                                                              (declare (ignorable $0 $1 $2))
                                                                              $0))
                                                                           ((word-equal (scanner-current-token
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        'string)
                                                                            (let* (($1
                                                                                    (pascal/parse-a-string
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                   (a-string $1)
                                                                                   (a-string.1 $1)
                                                                                   ($0 (list $1)))
                                                                              (declare
                                                                               (ignorable $0 a-string.1 a-string $1))
                                                                              $1))
                                                                           (t
                                                                            (com.informatimago.rdp::error-unexpected-token
                                                                              com.informatimago.common-lisp.parser.scanner:scanner
                                                                              '(nil
                                                                                minus
                                                                                plus
                                                                                integer
                                                                                real
                                                                                identifier
                                                                                colon
                                                                                comma
                                                                                clob
                                                                                dotdot
                                                                                semi
                                                                                string)
                                                                              '(-->
                                                                                constant
                                                                                (alt
                                                                                 (seq (constant-1-1 constant-1-2) ($0))
                                                                                 (seq (a-string) ($1))))))))))
                                                          (progn (fmakunbound 'pascal/parse-case-label)
                                                                 (defun pascal/parse-case-label (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                   "(--> case-label (seq (constant) ((list* 'case-label $0))))"
                                                                   (com.informatimago.rdp::with-non-terminal
                                                                     (case-label
                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                     (cond ((member (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    '(string
                                                                                      integer
                                                                                      real
                                                                                      identifier
                                                                                      minus
                                                                                      plus)
                                                                                    :test
                                                                                    #'word-equal)
                                                                            (let* (($1
                                                                                    (pascal/parse-constant
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                   (constant $1)
                                                                                   (constant.1 $1)
                                                                                   ($0 (list $1)))
                                                                              (declare
                                                                               (ignorable $0 constant.1 constant $1))
                                                                              (list* 'case-label $0)))
                                                                           (t
                                                                            (com.informatimago.rdp::error-unexpected-token
                                                                              com.informatimago.common-lisp.parser.scanner:scanner
                                                                              '(string
                                                                                integer
                                                                                real
                                                                                identifier
                                                                                minus
                                                                                plus)
                                                                              '(-->
                                                                                case-label
                                                                                (alt
                                                                                 (seq
                                                                                  (constant)
                                                                                  ((list* 'case-label $0)))))))))))
                                                          (progn (fmakunbound 'pascal/parse-case-label-list-1)
                                                                 (defun pascal/parse-case-label-list-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                   "(--> case-label-list-1 (alt ((seq nil ('nil)) (seq (case-label-list-1-1 case-label-list-1) ((cons $1 $2))))))"
                                                                   (com.informatimago.rdp::with-non-terminal
                                                                     (case-label-list-1
                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                     (loop :with com.informatimago.rdp::$items := 'nil
                                                                           :do (cond
                                                                                ((word-equal
                                                                                  (scanner-current-token
                                                                                   com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  'comma)
                                                                                 (push
                                                                                  (let*
                                                                                   (($1
                                                                                     (pascal/parse-case-label-list-1-1
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                    ($2
                                                                                     (let*
                                                                                      (($0 (list)))
                                                                                      (declare (ignorable $0))
                                                                                      'nil))
                                                                                    ($0 (list $1 $2)))
                                                                                   (declare (ignorable $0 $1 $2))
                                                                                   (cons $1 $2))
                                                                                  com.informatimago.rdp::$items))
                                                                                (t (loop-finish)))
                                                                           :finally (return
                                                                                     (reduce
                                                                                      #'append
                                                                                      (nreverse
                                                                                       com.informatimago.rdp::$items)
                                                                                      :initial-value
                                                                                      nil))))))
                                                                 (progn (fmakunbound 'pascal/parse-case-label-list-1-1)
                                                                        (defun pascal/parse-case-label-list-1-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          "(--> case-label-list-1-1 (seq (comma case-label) ($0)))"
                                                                          (com.informatimago.rdp::with-non-terminal
                                                                            (case-label-list-1-1
                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                            (cond ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'comma)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'comma))
                                                                                     (comma $1)
                                                                                     (comma.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-case-label
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (case-label $2)
                                                                                     (case-label.1 $2)
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      case-label.1
                                                                                      case-label
                                                                                      comma.1
                                                                                      comma
                                                                                      $1
                                                                                      $2))
                                                                                    $0))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(comma)
                                                                                    '(-->
                                                                                      case-label-list-1-1
                                                                                      (alt
                                                                                       (seq
                                                                                        (comma case-label)
                                                                                        ($0))))))))))
                                                                 (progn (fmakunbound 'pascal/parse-statement)
                                                                        (defun pascal/parse-statement (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          "(--> statement (seq (statement-1 statement-2) ((list* 'statement $0))))"
                                                                          (com.informatimago.rdp::with-non-terminal
                                                                            (statement
                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                            (cond ((member
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    '(integer
                                                                                      nil
                                                                                      goto
                                                                                      while
                                                                                      repeat
                                                                                      if
                                                                                      case
                                                                                      for
                                                                                      with
                                                                                      identifier
                                                                                      begin
                                                                                      until
                                                                                      end
                                                                                      semi
                                                                                      else)
                                                                                    :test
                                                                                    #'word-equal)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-statement-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($2
                                                                                      (pascal/parse-statement-2
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare (ignorable $0 $1 $2))
                                                                                    (list* 'statement $0)))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(integer
                                                                                      nil
                                                                                      goto
                                                                                      while
                                                                                      repeat
                                                                                      if
                                                                                      case
                                                                                      for
                                                                                      with
                                                                                      identifier
                                                                                      begin
                                                                                      until
                                                                                      end
                                                                                      semi
                                                                                      else)
                                                                                    '(-->
                                                                                      statement
                                                                                      (alt
                                                                                       (seq
                                                                                        (statement-1 statement-2)
                                                                                        ((list*
                                                                                          'statement
                                                                                          $0)))))))))))
                                                                 (progn (fmakunbound 'pascal/parse-case-label-list)
                                                                        (defun pascal/parse-case-label-list (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          "(--> case-label-list (seq (case-label case-label-list-1) ((list* 'case-label-list $0))))"
                                                                          (com.informatimago.rdp::with-non-terminal
                                                                            (case-label-list
                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                            (cond ((member
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    '(plus
                                                                                      minus
                                                                                      identifier
                                                                                      real
                                                                                      integer
                                                                                      string)
                                                                                    :test
                                                                                    #'word-equal)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-case-label
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (case-label $1)
                                                                                     (case-label.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-case-label-list-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      case-label.1
                                                                                      case-label
                                                                                      $1
                                                                                      $2))
                                                                                    (list* 'case-label-list $0)))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(plus
                                                                                      minus
                                                                                      identifier
                                                                                      real
                                                                                      integer
                                                                                      string)
                                                                                    '(-->
                                                                                      case-label-list
                                                                                      (alt
                                                                                       (seq
                                                                                        (case-label case-label-list-1)
                                                                                        ((list*
                                                                                          'case-label-list
                                                                                          $0)))))))))))
                                                                 (progn (fmakunbound 'pascal/parse-one-case)
                                                                        (defun pascal/parse-one-case (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          "(--> one-case (seq (case-label-list colon statement) ((list* 'one-case $0))))"
                                                                          (com.informatimago.rdp::with-non-terminal
                                                                            (one-case
                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                            (cond ((member
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    '(string
                                                                                      integer
                                                                                      real
                                                                                      identifier
                                                                                      minus
                                                                                      plus)
                                                                                    :test
                                                                                    #'word-equal)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-case-label-list
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (case-label-list $1)
                                                                                     (case-label-list.1 $1)
                                                                                     ($2
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'colon))
                                                                                     (colon $2)
                                                                                     (colon.1 $2)
                                                                                     ($3
                                                                                      (pascal/parse-statement
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (statement $3)
                                                                                     (statement.1 $3)
                                                                                     ($0 (list $1 $2 $3)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      statement.1
                                                                                      statement
                                                                                      colon.1
                                                                                      colon
                                                                                      case-label-list.1
                                                                                      case-label-list
                                                                                      $1
                                                                                      $2
                                                                                      $3))
                                                                                    (list* 'one-case $0)))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(string
                                                                                      integer
                                                                                      real
                                                                                      identifier
                                                                                      minus
                                                                                      plus)
                                                                                    '(-->
                                                                                      one-case
                                                                                      (alt
                                                                                       (seq
                                                                                        (case-label-list
                                                                                         colon
                                                                                         statement)
                                                                                        ((list* 'one-case $0)))))))))))
                                                                 (progn (fmakunbound 'pascal/parse-case-list-1)
                                                                        (defun pascal/parse-case-list-1 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                          "(--> case-list-1 (alt ((seq nil ('nil)) (seq (case-list-1-1 case-list-1) ((cons $1 $2))))))"
                                                                          (com.informatimago.rdp::with-non-terminal
                                                                            (case-list-1
                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                            (loop :with com.informatimago.rdp::$items
                                                                                  :=
                                                                                  'nil
                                                                                  :do (cond
                                                                                       ((word-equal
                                                                                         (scanner-current-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         'semi)
                                                                                        (push
                                                                                         (let*
                                                                                          (($1
                                                                                            (pascal/parse-case-list-1-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           ($2
                                                                                            (let*
                                                                                             (($0 (list)))
                                                                                             (declare (ignorable $0))
                                                                                             'nil))
                                                                                           ($0 (list $1 $2)))
                                                                                          (declare
                                                                                           (ignorable $0 $1 $2))
                                                                                          (cons $1 $2))
                                                                                         com.informatimago.rdp::$items))
                                                                                       (t (loop-finish)))
                                                                                  :finally (return
                                                                                            (reduce
                                                                                             #'append
                                                                                             (nreverse
                                                                                              com.informatimago.rdp::$items)
                                                                                             :initial-value
                                                                                             nil))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-case-list-1-1)
                                                                               (defun
                                                                                pascal/parse-case-list-1-1
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> case-list-1-1 (seq (semi one-case) ($0)))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (case-list-1-1
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'semi)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'semi))
                                                                                     (semi $1)
                                                                                     (semi.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-one-case
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (one-case $2)
                                                                                     (one-case.1 $2)
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      one-case.1
                                                                                      one-case
                                                                                      semi.1
                                                                                      semi
                                                                                      $1
                                                                                      $2))
                                                                                    $0))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(semi)
                                                                                    '(-->
                                                                                      case-list-1-1
                                                                                      (alt
                                                                                       (seq
                                                                                        (semi one-case)
                                                                                        ($0))))))))))
                                                                        (progn (fmakunbound 'pascal/parse-case-list-2)
                                                                               (defun
                                                                                pascal/parse-case-list-2
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> case-list-2 (alt ((seq nil ('nil)) (seq (semi) ($0)))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (case-list-2
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'semi)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'semi))
                                                                                     (semi $1)
                                                                                     (semi.1 $1)
                                                                                     ($0 (list $1)))
                                                                                    (declare
                                                                                     (ignorable $0 semi.1 semi $1))
                                                                                    $0))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-field-width-1)
                                                                               (defun
                                                                                pascal/parse-field-width-1
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> field-width-1 (alt ((seq (colon integer-expression) ($0)) (seq nil ('nil)))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (field-width-1
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'colon)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'colon))
                                                                                     (colon $1)
                                                                                     (colon.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-integer-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (integer-expression $2)
                                                                                     (integer-expression.1 $2)
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      integer-expression.1
                                                                                      integer-expression
                                                                                      colon.1
                                                                                      colon
                                                                                      $1
                                                                                      $2))
                                                                                    $0))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-integer-expression)
                                                                               (defun
                                                                                pascal/parse-integer-expression
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> integer-expression (seq (expression) ((list* 'integer-expression $0))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (integer-expression
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((member
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    '(plus
                                                                                      minus
                                                                                      obra
                                                                                      opar
                                                                                      not
                                                                                      identifier
                                                                                      nile
                                                                                      string
                                                                                      real
                                                                                      integer)
                                                                                    :test
                                                                                    #'word-equal)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (expression $1)
                                                                                     (expression.1 $1)
                                                                                     ($0 (list $1)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      expression.1
                                                                                      expression
                                                                                      $1))
                                                                                    (list* 'integer-expression $0)))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(plus
                                                                                      minus
                                                                                      obra
                                                                                      opar
                                                                                      not
                                                                                      identifier
                                                                                      nile
                                                                                      string
                                                                                      real
                                                                                      integer)
                                                                                    '(-->
                                                                                      integer-expression
                                                                                      (alt
                                                                                       (seq
                                                                                        (expression)
                                                                                        ((list*
                                                                                          'integer-expression
                                                                                          $0)))))))))))
                                                                        (progn (fmakunbound 'pascal/parse-field-width)
                                                                               (defun
                                                                                pascal/parse-field-width
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> field-width (seq (colon integer-expression field-width-1) ((list* 'field-width $0))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (field-width
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'colon)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'colon))
                                                                                     (colon $1)
                                                                                     (colon.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-integer-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (integer-expression $2)
                                                                                     (integer-expression.1 $2)
                                                                                     ($3
                                                                                      (pascal/parse-field-width-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($0 (list $1 $2 $3)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      integer-expression.1
                                                                                      integer-expression
                                                                                      colon.1
                                                                                      colon
                                                                                      $1
                                                                                      $2
                                                                                      $3))
                                                                                    (list* 'field-width $0)))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(colon)
                                                                                    '(-->
                                                                                      field-width
                                                                                      (alt
                                                                                       (seq
                                                                                        (colon
                                                                                         integer-expression
                                                                                         field-width-1)
                                                                                        ((list*
                                                                                          'field-width
                                                                                          $0)))))))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-actual-parameter-1)
                                                                               (defun
                                                                                pascal/parse-actual-parameter-1
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> actual-parameter-1 (alt ((seq nil ('nil)) (seq (field-width) ($0)))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (actual-parameter-1
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'colon)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-field-width
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (field-width $1)
                                                                                     (field-width.1 $1)
                                                                                     ($0 (list $1)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      field-width.1
                                                                                      field-width
                                                                                      $1))
                                                                                    $0))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-actual-parameter)
                                                                               (defun
                                                                                pascal/parse-actual-parameter
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> actual-parameter (seq (expression actual-parameter-1) ((list* 'actual-parameter $0))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (actual-parameter
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((member
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    '(plus
                                                                                      minus
                                                                                      obra
                                                                                      opar
                                                                                      not
                                                                                      identifier
                                                                                      nile
                                                                                      string
                                                                                      real
                                                                                      integer)
                                                                                    :test
                                                                                    #'word-equal)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (expression $1)
                                                                                     (expression.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-actual-parameter-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      expression.1
                                                                                      expression
                                                                                      $1
                                                                                      $2))
                                                                                    (list* 'actual-parameter $0)))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(plus
                                                                                      minus
                                                                                      obra
                                                                                      opar
                                                                                      not
                                                                                      identifier
                                                                                      nile
                                                                                      string
                                                                                      real
                                                                                      integer)
                                                                                    '(-->
                                                                                      actual-parameter
                                                                                      (alt
                                                                                       (seq
                                                                                        (expression actual-parameter-1)
                                                                                        ((list*
                                                                                          'actual-parameter
                                                                                          $0)))))))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-actual-params-1-1)
                                                                               (defun
                                                                                pascal/parse-actual-params-1-1
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> actual-params-1-1 (seq (comma actual-parameter) ($0)))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (actual-params-1-1
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'comma)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'comma))
                                                                                     (comma $1)
                                                                                     (comma.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-actual-parameter
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (actual-parameter $2)
                                                                                     (actual-parameter.1 $2)
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      actual-parameter.1
                                                                                      actual-parameter
                                                                                      comma.1
                                                                                      comma
                                                                                      $1
                                                                                      $2))
                                                                                    $0))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(comma)
                                                                                    '(-->
                                                                                      actual-params-1-1
                                                                                      (alt
                                                                                       (seq
                                                                                        (comma actual-parameter)
                                                                                        ($0))))))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-actual-params-1)
                                                                               (defun
                                                                                pascal/parse-actual-params-1
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> actual-params-1 (alt ((seq nil ('nil)) (seq (actual-params-1-1) ($0)))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (actual-params-1
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'comma)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-actual-params-1-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($0 (list $1)))
                                                                                    (declare (ignorable $0 $1))
                                                                                    $0))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-assignment-or-call-1-2)
                                                                               (defun
                                                                                pascal/parse-assignment-or-call-1-2
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> assignment-or-call-1-2 (alt ((seq nil ('nil)) (seq (actual-params) ($0)))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (assignment-or-call-1-2
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'opar)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-actual-params
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (actual-params $1)
                                                                                     (actual-params.1 $1)
                                                                                     ($0 (list $1)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      actual-params.1
                                                                                      actual-params
                                                                                      $1))
                                                                                    $0))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-compound-statement)
                                                                               (defun
                                                                                pascal/parse-compound-statement
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> compound-statement (seq (begin statement-sequence end) ((list* 'compound-statement $0))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (compound-statement
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'begin)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'begin))
                                                                                     (begin $1)
                                                                                     (begin.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-statement-sequence
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (statement-sequence $2)
                                                                                     (statement-sequence.1 $2)
                                                                                     ($3
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'end))
                                                                                     (end $3)
                                                                                     (end.1 $3)
                                                                                     ($0 (list $1 $2 $3)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      end.1
                                                                                      end
                                                                                      statement-sequence.1
                                                                                      statement-sequence
                                                                                      begin.1
                                                                                      begin
                                                                                      $1
                                                                                      $2
                                                                                      $3))
                                                                                    (list* 'compound-statement $0)))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(begin)
                                                                                    '(-->
                                                                                      compound-statement
                                                                                      (alt
                                                                                       (seq
                                                                                        (begin statement-sequence end)
                                                                                        ((list*
                                                                                          'compound-statement
                                                                                          $0)))))))))))
                                                                        (progn (fmakunbound 'pascal/parse-statement-2)
                                                                               (defun
                                                                                pascal/parse-statement-2
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> statement-2 (alt ((seq (goto a-label) ((list* 'goto-statement $0))) (seq (while boolean-expression do statement) ((list* 'while-statement $0))) (seq (repeat statement-sequence until boolean-expression) ((list* 'repeat-statement $0))) (seq (if boolean-expression then statement if-statement-1) ((list* 'if-statement $0))) (seq (case ordinal-expression of case-list end) ((list* 'case-statement $0))) (seq (for control-variable assign ordinal-expression for-statement-1 ordinal-expression do statement) ((list* 'for-statement $0))) (seq (with rec-var-list do statement) ((list* 'with-statement $0))) (seq (designator assignment-or-call-1) ((list* 'assignment-or-call $0))) (seq (compound-statement) ($1)))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (statement-2
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'goto)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'goto))
                                                                                     (goto $1)
                                                                                     (goto.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-a-label
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (a-label $2)
                                                                                     (a-label.1 $2)
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      a-label.1
                                                                                      a-label
                                                                                      goto.1
                                                                                      goto
                                                                                      $1
                                                                                      $2))
                                                                                    (list* 'goto-statement $0)))
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'while)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'while))
                                                                                     (while $1)
                                                                                     (while.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-boolean-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (boolean-expression $2)
                                                                                     (boolean-expression.1 $2)
                                                                                     ($3
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'do))
                                                                                     (do $3)
                                                                                     (do.1 $3)
                                                                                     ($4
                                                                                      (pascal/parse-statement
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (statement $4)
                                                                                     (statement.1 $4)
                                                                                     ($0 (list $1 $2 $3 $4)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      statement.1
                                                                                      statement
                                                                                      do.1
                                                                                      do
                                                                                      boolean-expression.1
                                                                                      boolean-expression
                                                                                      while.1
                                                                                      while
                                                                                      $1
                                                                                      $2
                                                                                      $3
                                                                                      $4))
                                                                                    (list* 'while-statement $0)))
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'repeat)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'repeat))
                                                                                     (repeat $1)
                                                                                     (repeat.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-statement-sequence
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (statement-sequence $2)
                                                                                     (statement-sequence.1 $2)
                                                                                     ($3
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'until))
                                                                                     (until $3)
                                                                                     (until.1 $3)
                                                                                     ($4
                                                                                      (pascal/parse-boolean-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (boolean-expression $4)
                                                                                     (boolean-expression.1 $4)
                                                                                     ($0 (list $1 $2 $3 $4)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      boolean-expression.1
                                                                                      boolean-expression
                                                                                      until.1
                                                                                      until
                                                                                      statement-sequence.1
                                                                                      statement-sequence
                                                                                      repeat.1
                                                                                      repeat
                                                                                      $1
                                                                                      $2
                                                                                      $3
                                                                                      $4))
                                                                                    (list* 'repeat-statement $0)))
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'if)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'if))
                                                                                     (if $1)
                                                                                     (if.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-boolean-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (boolean-expression $2)
                                                                                     (boolean-expression.1 $2)
                                                                                     ($3
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'then))
                                                                                     (then $3)
                                                                                     (then.1 $3)
                                                                                     ($4
                                                                                      (pascal/parse-statement
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (statement $4)
                                                                                     (statement.1 $4)
                                                                                     ($5
                                                                                      (pascal/parse-if-statement-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($0 (list $1 $2 $3 $4 $5)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      statement.1
                                                                                      statement
                                                                                      then.1
                                                                                      then
                                                                                      boolean-expression.1
                                                                                      boolean-expression
                                                                                      if.1
                                                                                      if
                                                                                      $1
                                                                                      $2
                                                                                      $3
                                                                                      $4
                                                                                      $5))
                                                                                    (list* 'if-statement $0)))
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'case)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'case))
                                                                                     (case $1)
                                                                                     (case.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-ordinal-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (ordinal-expression $2)
                                                                                     (ordinal-expression.1 $2)
                                                                                     ($3
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'of))
                                                                                     (of $3)
                                                                                     (of.1 $3)
                                                                                     ($4
                                                                                      (pascal/parse-case-list
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (case-list $4)
                                                                                     (case-list.1 $4)
                                                                                     ($5
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'end))
                                                                                     (end $5)
                                                                                     (end.1 $5)
                                                                                     ($0 (list $1 $2 $3 $4 $5)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      end.1
                                                                                      end
                                                                                      case-list.1
                                                                                      case-list
                                                                                      of.1
                                                                                      of
                                                                                      ordinal-expression.1
                                                                                      ordinal-expression
                                                                                      case.1
                                                                                      case
                                                                                      $1
                                                                                      $2
                                                                                      $3
                                                                                      $4
                                                                                      $5))
                                                                                    (list* 'case-statement $0)))
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'for)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'for))
                                                                                     (for $1)
                                                                                     (for.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-control-variable
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (control-variable $2)
                                                                                     (control-variable.1 $2)
                                                                                     ($3
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'assign))
                                                                                     (assign $3)
                                                                                     (assign.1 $3)
                                                                                     ($4
                                                                                      (pascal/parse-ordinal-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (ordinal-expression $4)
                                                                                     (ordinal-expression.1 $4)
                                                                                     ($5
                                                                                      (pascal/parse-for-statement-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($6
                                                                                      (pascal/parse-ordinal-expression
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (ordinal-expression.2 $6)
                                                                                     ($7
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'do))
                                                                                     (do $7)
                                                                                     (do.1 $7)
                                                                                     ($8
                                                                                      (pascal/parse-statement
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (statement $8)
                                                                                     (statement.1 $8)
                                                                                     ($0
                                                                                      (list $1 $2 $3 $4 $5 $6 $7 $8)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      statement.1
                                                                                      statement
                                                                                      do.1
                                                                                      do
                                                                                      ordinal-expression.2
                                                                                      ordinal-expression.1
                                                                                      ordinal-expression
                                                                                      assign.1
                                                                                      assign
                                                                                      control-variable.1
                                                                                      control-variable
                                                                                      for.1
                                                                                      for
                                                                                      $1
                                                                                      $2
                                                                                      $3
                                                                                      $4
                                                                                      $5
                                                                                      $6
                                                                                      $7
                                                                                      $8))
                                                                                    (list* 'for-statement $0)))
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'with)
                                                                                   (let*
                                                                                    (($1
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'with))
                                                                                     (with $1)
                                                                                     (with.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-rec-var-list
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (rec-var-list $2)
                                                                                     (rec-var-list.1 $2)
                                                                                     ($3
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'do))
                                                                                     (do $3)
                                                                                     (do.1 $3)
                                                                                     ($4
                                                                                      (pascal/parse-statement
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (statement $4)
                                                                                     (statement.1 $4)
                                                                                     ($0 (list $1 $2 $3 $4)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      statement.1
                                                                                      statement
                                                                                      do.1
                                                                                      do
                                                                                      rec-var-list.1
                                                                                      rec-var-list
                                                                                      with.1
                                                                                      with
                                                                                      $1
                                                                                      $2
                                                                                      $3
                                                                                      $4))
                                                                                    (list* 'with-statement $0)))
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'identifier)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-designator
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (designator $1)
                                                                                     (designator.1 $1)
                                                                                     ($2
                                                                                      (pascal/parse-assignment-or-call-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      designator.1
                                                                                      designator
                                                                                      $1
                                                                                      $2))
                                                                                    (list* 'assignment-or-call $0)))
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'begin)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-compound-statement
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (compound-statement $1)
                                                                                     (compound-statement.1 $1)
                                                                                     ($0 (list $1)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      compound-statement.1
                                                                                      compound-statement
                                                                                      $1))
                                                                                    $1))
                                                                                  (t
                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                    '(goto
                                                                                      while
                                                                                      repeat
                                                                                      if
                                                                                      case
                                                                                      for
                                                                                      with
                                                                                      identifier
                                                                                      begin)
                                                                                    '(-->
                                                                                      statement-2
                                                                                      (alt
                                                                                       (seq
                                                                                        (goto a-label)
                                                                                        ((list* 'goto-statement $0)))
                                                                                       (seq
                                                                                        (while
                                                                                         boolean-expression
                                                                                         do
                                                                                         statement)
                                                                                        ((list* 'while-statement $0)))
                                                                                       (seq
                                                                                        (repeat
                                                                                         statement-sequence
                                                                                         until
                                                                                         boolean-expression)
                                                                                        ((list* 'repeat-statement $0)))
                                                                                       (seq
                                                                                        (if
                                                                                         boolean-expression
                                                                                         then
                                                                                         statement
                                                                                         if-statement-1)
                                                                                        ((list* 'if-statement $0)))
                                                                                       (seq
                                                                                        (case
                                                                                         ordinal-expression
                                                                                         of
                                                                                         case-list
                                                                                         end)
                                                                                        ((list* 'case-statement $0)))
                                                                                       (seq
                                                                                        (for
                                                                                         control-variable
                                                                                         assign
                                                                                         ordinal-expression
                                                                                         for-statement-1
                                                                                         ordinal-expression
                                                                                         do
                                                                                         statement)
                                                                                        ((list* 'for-statement $0)))
                                                                                       (seq
                                                                                        (with
                                                                                         rec-var-list
                                                                                         do
                                                                                         statement)
                                                                                        ((list* 'with-statement $0)))
                                                                                       (seq
                                                                                        (designator
                                                                                         assignment-or-call-1)
                                                                                        ((list*
                                                                                          'assignment-or-call
                                                                                          $0)))
                                                                                       (seq
                                                                                        (compound-statement)
                                                                                        ($1))))))))))
                                                                        (progn (fmakunbound 'pascal/parse-statement-1)
                                                                               (defun
                                                                                pascal/parse-statement-1
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> statement-1 (alt ((seq (a-label colon) ($0)) (seq nil ('nil)))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (statement-1
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (cond
                                                                                  ((word-equal
                                                                                    (scanner-current-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    'integer)
                                                                                   (let*
                                                                                    (($1
                                                                                      (pascal/parse-a-label
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                     (a-label $1)
                                                                                     (a-label.1 $1)
                                                                                     ($2
                                                                                      (accept
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       'colon))
                                                                                     (colon $2)
                                                                                     (colon.1 $2)
                                                                                     ($0 (list $1 $2)))
                                                                                    (declare
                                                                                     (ignorable
                                                                                      $0
                                                                                      colon.1
                                                                                      colon
                                                                                      a-label.1
                                                                                      a-label
                                                                                      $1
                                                                                      $2))
                                                                                    $0))))))
                                                                        (progn (fmakunbound
                                                                                'pascal/parse-statement-sequence-1)
                                                                               (defun
                                                                                pascal/parse-statement-sequence-1
                                                                                (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                "(--> statement-sequence-1 (alt ((seq nil ('nil)) (seq (statement-sequence-1-1 statement-sequence-1) ((cons $1 $2))))))"
                                                                                (com.informatimago.rdp::with-non-terminal
                                                                                 (statement-sequence-1
                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 (loop :with com.informatimago.rdp::$items
                                                                                       :=
                                                                                       'nil
                                                                                  :do (cond
                                                                                       ((word-equal
                                                                                         (scanner-current-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         'semi)
                                                                                        (push
                                                                                         (let*
                                                                                          (($1
                                                                                            (pascal/parse-statement-sequence-1-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           ($2
                                                                                            (let*
                                                                                             (($0 (list)))
                                                                                             (declare (ignorable $0))
                                                                                             'nil))
                                                                                           ($0 (list $1 $2)))
                                                                                          (declare
                                                                                           (ignorable $0 $1 $2))
                                                                                          (cons $1 $2))
                                                                                         com.informatimago.rdp::$items))
                                                                                       (t (loop-finish)))
                                                                                  :finally (return
                                                                                            (reduce
                                                                                             #'append
                                                                                             (nreverse
                                                                                              com.informatimago.rdp::$items)
                                                                                             :initial-value
                                                                                             nil))))))
                                                                               (progn
                                                                                (fmakunbound
                                                                                 'pascal/parse-statement-sequence-1-1)
                                                                                (defun
                                                                                 pascal/parse-statement-sequence-1-1
                                                                                 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 "(--> statement-sequence-1-1 (seq (semi statement) ($0)))"
                                                                                 (com.informatimago.rdp::with-non-terminal
                                                                                  (statement-sequence-1-1
                                                                                   com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  (cond
                                                                                   ((word-equal
                                                                                     (scanner-current-token
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     'semi)
                                                                                    (let*
                                                                                     (($1
                                                                                       (accept
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                        'semi))
                                                                                      (semi $1)
                                                                                      (semi.1 $1)
                                                                                      ($2
                                                                                       (pascal/parse-statement
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                      (statement $2)
                                                                                      (statement.1 $2)
                                                                                      ($0 (list $1 $2)))
                                                                                     (declare
                                                                                      (ignorable
                                                                                       $0
                                                                                       statement.1
                                                                                       statement
                                                                                       semi.1
                                                                                       semi
                                                                                       $1
                                                                                       $2))
                                                                                     $0))
                                                                                   (t
                                                                                    (com.informatimago.rdp::error-unexpected-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner
                                                                                     '(semi)
                                                                                     '(-->
                                                                                       statement-sequence-1-1
                                                                                       (alt
                                                                                        (seq
                                                                                         (semi statement)
                                                                                         ($0))))))))))
                                                                               (progn
                                                                                (fmakunbound
                                                                                 'pascal/parse-statement-sequence)
                                                                                (defun
                                                                                 pascal/parse-statement-sequence
                                                                                 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 "(--> statement-sequence (seq (statement statement-sequence-1) ((list* 'statement-sequence $0))))"
                                                                                 (com.informatimago.rdp::with-non-terminal
                                                                                  (statement-sequence
                                                                                   com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  (cond
                                                                                   ((member
                                                                                     (scanner-current-token
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     '(goto
                                                                                       while
                                                                                       repeat
                                                                                       if
                                                                                       case
                                                                                       for
                                                                                       with
                                                                                       identifier
                                                                                       begin
                                                                                       integer)
                                                                                     :test
                                                                                     #'word-equal)
                                                                                    (let*
                                                                                     (($1
                                                                                       (pascal/parse-statement
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                      (statement $1)
                                                                                      (statement.1 $1)
                                                                                      ($2
                                                                                       (pascal/parse-statement-sequence-1
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                      ($0 (list $1 $2)))
                                                                                     (declare
                                                                                      (ignorable
                                                                                       $0
                                                                                       statement.1
                                                                                       statement
                                                                                       $1
                                                                                       $2))
                                                                                     (list* 'statement-sequence $0)))
                                                                                   (t
                                                                                    (com.informatimago.rdp::error-unexpected-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner
                                                                                     '(goto
                                                                                       while
                                                                                       repeat
                                                                                       if
                                                                                       case
                                                                                       for
                                                                                       with
                                                                                       identifier
                                                                                       begin
                                                                                       integer)
                                                                                     '(-->
                                                                                       statement-sequence
                                                                                       (alt
                                                                                        (seq
                                                                                         (statement
                                                                                          statement-sequence-1)
                                                                                         ((list*
                                                                                           'statement-sequence
                                                                                           $0)))))))))))
                                                                               (progn
                                                                                (fmakunbound 'pascal/parse-type-ident)
                                                                                (defun
                                                                                 pascal/parse-type-ident
                                                                                 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 "(--> type-ident (seq (identifier) ((list* 'type-ident $0))))"
                                                                                 (com.informatimago.rdp::with-non-terminal
                                                                                  (type-ident
                                                                                   com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  (cond
                                                                                   ((word-equal
                                                                                     (scanner-current-token
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     'identifier)
                                                                                    (let*
                                                                                     (($1
                                                                                       (accept
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                        'identifier))
                                                                                      (identifier $1)
                                                                                      (identifier.1 $1)
                                                                                      ($0 (list $1)))
                                                                                     (declare
                                                                                      (ignorable
                                                                                       $0
                                                                                       identifier.1
                                                                                       identifier
                                                                                       $1))
                                                                                     (list* 'type-ident $0)))
                                                                                   (t
                                                                                    (com.informatimago.rdp::error-unexpected-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner
                                                                                     '(identifier)
                                                                                     '(-->
                                                                                       type-ident
                                                                                       (alt
                                                                                        (seq
                                                                                         (identifier)
                                                                                         ((list*
                                                                                           'type-ident
                                                                                           $0)))))))))))
                                                                               (progn
                                                                                (fmakunbound 'pascal/parse-index-spec)
                                                                                (defun
                                                                                 pascal/parse-index-spec
                                                                                 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 "(--> index-spec (seq (new-ident dotdot new-ident colon type-ident) ((list* 'index-spec $0))))"
                                                                                 (com.informatimago.rdp::with-non-terminal
                                                                                  (index-spec
                                                                                   com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  (cond
                                                                                   ((word-equal
                                                                                     (scanner-current-token
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     'identifier)
                                                                                    (let*
                                                                                     (($1
                                                                                       (pascal/parse-new-ident
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                      (new-ident $1)
                                                                                      (new-ident.1 $1)
                                                                                      ($2
                                                                                       (accept
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                        'dotdot))
                                                                                      (dotdot $2)
                                                                                      (dotdot.1 $2)
                                                                                      ($3
                                                                                       (pascal/parse-new-ident
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                      (new-ident.2 $3)
                                                                                      ($4
                                                                                       (accept
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                        'colon))
                                                                                      (colon $4)
                                                                                      (colon.1 $4)
                                                                                      ($5
                                                                                       (pascal/parse-type-ident
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                      (type-ident $5)
                                                                                      (type-ident.1 $5)
                                                                                      ($0 (list $1 $2 $3 $4 $5)))
                                                                                     (declare
                                                                                      (ignorable
                                                                                       $0
                                                                                       type-ident.1
                                                                                       type-ident
                                                                                       colon.1
                                                                                       colon
                                                                                       new-ident.2
                                                                                       dotdot.1
                                                                                       dotdot
                                                                                       new-ident.1
                                                                                       new-ident
                                                                                       $1
                                                                                       $2
                                                                                       $3
                                                                                       $4
                                                                                       $5))
                                                                                     (list* 'index-spec $0)))
                                                                                   (t
                                                                                    (com.informatimago.rdp::error-unexpected-token
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner
                                                                                     '(identifier)
                                                                                     '(-->
                                                                                       index-spec
                                                                                       (alt
                                                                                        (seq
                                                                                         (new-ident
                                                                                          dotdot
                                                                                          new-ident
                                                                                          colon
                                                                                          type-ident)
                                                                                         ((list*
                                                                                           'index-spec
                                                                                           $0)))))))))))
                                                                               (progn
                                                                                (fmakunbound
                                                                                 'pascal/parse-index-spec-list-1)
                                                                                (defun
                                                                                 pascal/parse-index-spec-list-1
                                                                                 (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                 "(--> index-spec-list-1 (alt ((seq nil ('nil)) (seq (index-spec-list-1-1 index-spec-list-1) ((cons $1 $2))))))"
                                                                                 (com.informatimago.rdp::with-non-terminal
                                                                                  (index-spec-list-1
                                                                                   com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  (loop :with com.informatimago.rdp::$items
                                                                                        :=
                                                                                        'nil
                                                                                   :do (cond
                                                                                        ((word-equal
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          'semi)
                                                                                         (push
                                                                                          (let*
                                                                                           (($1
                                                                                             (pascal/parse-index-spec-list-1-1
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                            ($2
                                                                                             (let*
                                                                                              (($0 (list)))
                                                                                              (declare (ignorable $0))
                                                                                              'nil))
                                                                                            ($0 (list $1 $2)))
                                                                                           (declare
                                                                                            (ignorable $0 $1 $2))
                                                                                           (cons $1 $2))
                                                                                          com.informatimago.rdp::$items))
                                                                                        (t (loop-finish)))
                                                                                   :finally (return
                                                                                             (reduce
                                                                                              #'append
                                                                                              (nreverse
                                                                                               com.informatimago.rdp::$items)
                                                                                              :initial-value
                                                                                              nil))))))
                                                                                (progn
                                                                                 (fmakunbound
                                                                                  'pascal/parse-index-spec-list-1-1)
                                                                                 (defun
                                                                                  pascal/parse-index-spec-list-1-1
                                                                                  (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  "(--> index-spec-list-1-1 (seq (semi index-spec) ($0)))"
                                                                                  (com.informatimago.rdp::with-non-terminal
                                                                                   (index-spec-list-1-1
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   (cond
                                                                                    ((word-equal
                                                                                      (scanner-current-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      'semi)
                                                                                     (let*
                                                                                      (($1
                                                                                        (accept
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner
                                                                                         'semi))
                                                                                       (semi $1)
                                                                                       (semi.1 $1)
                                                                                       ($2
                                                                                        (pascal/parse-index-spec
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       (index-spec $2)
                                                                                       (index-spec.1 $2)
                                                                                       ($0 (list $1 $2)))
                                                                                      (declare
                                                                                       (ignorable
                                                                                        $0
                                                                                        index-spec.1
                                                                                        index-spec
                                                                                        semi.1
                                                                                        semi
                                                                                        $1
                                                                                        $2))
                                                                                      $0))
                                                                                    (t
                                                                                     (com.informatimago.rdp::error-unexpected-token
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner
                                                                                      '(semi)
                                                                                      '(-->
                                                                                        index-spec-list-1-1
                                                                                        (alt
                                                                                         (seq
                                                                                          (semi index-spec)
                                                                                          ($0))))))))))
                                                                                (progn
                                                                                 (fmakunbound
                                                                                  'pascal/parse-proc-heading)
                                                                                 (defun
                                                                                  pascal/parse-proc-heading
                                                                                  (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  "(--> proc-heading (seq (procedure new-ident proc-heading-1) ((list* 'proc-heading $0))))"
                                                                                  (com.informatimago.rdp::with-non-terminal
                                                                                   (proc-heading
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   (cond
                                                                                    ((word-equal
                                                                                      (scanner-current-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      'procedure)
                                                                                     (let*
                                                                                      (($1
                                                                                        (accept
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner
                                                                                         'procedure))
                                                                                       (procedure $1)
                                                                                       (procedure.1 $1)
                                                                                       ($2
                                                                                        (pascal/parse-new-ident
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       (new-ident $2)
                                                                                       (new-ident.1 $2)
                                                                                       ($3
                                                                                        (pascal/parse-proc-heading-1
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       ($0 (list $1 $2 $3)))
                                                                                      (declare
                                                                                       (ignorable
                                                                                        $0
                                                                                        new-ident.1
                                                                                        new-ident
                                                                                        procedure.1
                                                                                        procedure
                                                                                        $1
                                                                                        $2
                                                                                        $3))
                                                                                      (list* 'proc-heading $0)))
                                                                                    (t
                                                                                     (com.informatimago.rdp::error-unexpected-token
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner
                                                                                      '(procedure)
                                                                                      '(-->
                                                                                        proc-heading
                                                                                        (alt
                                                                                         (seq
                                                                                          (procedure
                                                                                           new-ident
                                                                                           proc-heading-1)
                                                                                          ((list*
                                                                                            'proc-heading
                                                                                            $0)))))))))))
                                                                                (progn
                                                                                 (fmakunbound
                                                                                  'pascal/parse-func-heading)
                                                                                 (defun
                                                                                  pascal/parse-func-heading
                                                                                  (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  "(--> func-heading (seq (function new-ident func-heading-1 return-type) ((list* 'func-heading $0))))"
                                                                                  (com.informatimago.rdp::with-non-terminal
                                                                                   (func-heading
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   (cond
                                                                                    ((word-equal
                                                                                      (scanner-current-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      'function)
                                                                                     (let*
                                                                                      (($1
                                                                                        (accept
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner
                                                                                         'function))
                                                                                       (function $1)
                                                                                       (function.1 $1)
                                                                                       ($2
                                                                                        (pascal/parse-new-ident
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       (new-ident $2)
                                                                                       (new-ident.1 $2)
                                                                                       ($3
                                                                                        (pascal/parse-func-heading-1
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       ($4
                                                                                        (pascal/parse-return-type
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       (return-type $4)
                                                                                       (return-type.1 $4)
                                                                                       ($0 (list $1 $2 $3 $4)))
                                                                                      (declare
                                                                                       (ignorable
                                                                                        $0
                                                                                        return-type.1
                                                                                        return-type
                                                                                        new-ident.1
                                                                                        new-ident
                                                                                        function.1
                                                                                        function
                                                                                        $1
                                                                                        $2
                                                                                        $3
                                                                                        $4))
                                                                                      (list* 'func-heading $0)))
                                                                                    (t
                                                                                     (com.informatimago.rdp::error-unexpected-token
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner
                                                                                      '(function)
                                                                                      '(-->
                                                                                        func-heading
                                                                                        (alt
                                                                                         (seq
                                                                                          (function
                                                                                           new-ident
                                                                                           func-heading-1
                                                                                           return-type)
                                                                                          ((list*
                                                                                            'func-heading
                                                                                            $0)))))))))))
                                                                                (progn
                                                                                 (fmakunbound
                                                                                  'pascal/parse-formal-section-2)
                                                                                 (defun
                                                                                  pascal/parse-formal-section-2
                                                                                  (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  "(--> formal-section-2 (alt ((seq (new-ident-list colon param-type) ((list* 'param-group $0))) (seq (func-heading) ($1)) (seq (proc-heading) ($1)))))"
                                                                                  (com.informatimago.rdp::with-non-terminal
                                                                                   (formal-section-2
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   (cond
                                                                                    ((word-equal
                                                                                      (scanner-current-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      'identifier)
                                                                                     (let*
                                                                                      (($1
                                                                                        (pascal/parse-new-ident-list
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       (new-ident-list $1)
                                                                                       (new-ident-list.1 $1)
                                                                                       ($2
                                                                                        (accept
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner
                                                                                         'colon))
                                                                                       (colon $2)
                                                                                       (colon.1 $2)
                                                                                       ($3
                                                                                        (pascal/parse-param-type
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       (param-type $3)
                                                                                       (param-type.1 $3)
                                                                                       ($0 (list $1 $2 $3)))
                                                                                      (declare
                                                                                       (ignorable
                                                                                        $0
                                                                                        param-type.1
                                                                                        param-type
                                                                                        colon.1
                                                                                        colon
                                                                                        new-ident-list.1
                                                                                        new-ident-list
                                                                                        $1
                                                                                        $2
                                                                                        $3))
                                                                                      (list* 'param-group $0)))
                                                                                    ((word-equal
                                                                                      (scanner-current-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      'function)
                                                                                     (let*
                                                                                      (($1
                                                                                        (pascal/parse-func-heading
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       (func-heading $1)
                                                                                       (func-heading.1 $1)
                                                                                       ($0 (list $1)))
                                                                                      (declare
                                                                                       (ignorable
                                                                                        $0
                                                                                        func-heading.1
                                                                                        func-heading
                                                                                        $1))
                                                                                      $1))
                                                                                    ((word-equal
                                                                                      (scanner-current-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      'procedure)
                                                                                     (let*
                                                                                      (($1
                                                                                        (pascal/parse-proc-heading
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       (proc-heading $1)
                                                                                       (proc-heading.1 $1)
                                                                                       ($0 (list $1)))
                                                                                      (declare
                                                                                       (ignorable
                                                                                        $0
                                                                                        proc-heading.1
                                                                                        proc-heading
                                                                                        $1))
                                                                                      $1))
                                                                                    (t
                                                                                     (com.informatimago.rdp::error-unexpected-token
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner
                                                                                      '(identifier function procedure)
                                                                                      '(-->
                                                                                        formal-section-2
                                                                                        (alt
                                                                                         (seq
                                                                                          (new-ident-list
                                                                                           colon
                                                                                           param-type)
                                                                                          ((list* 'param-group $0)))
                                                                                         (seq (func-heading) ($1))
                                                                                         (seq
                                                                                          (proc-heading)
                                                                                          ($1))))))))))
                                                                                (progn
                                                                                 (fmakunbound
                                                                                  'pascal/parse-formal-section-1)
                                                                                 (defun
                                                                                  pascal/parse-formal-section-1
                                                                                  (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  "(--> formal-section-1 (alt ((seq nil ('nil)) (seq (var) ($0)))))"
                                                                                  (com.informatimago.rdp::with-non-terminal
                                                                                   (formal-section-1
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   (cond
                                                                                    ((word-equal
                                                                                      (scanner-current-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      'var)
                                                                                     (let*
                                                                                      (($1
                                                                                        (accept
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner
                                                                                         'var))
                                                                                       (var $1)
                                                                                       (var.1 $1)
                                                                                       ($0 (list $1)))
                                                                                      (declare
                                                                                       (ignorable $0 var.1 var $1))
                                                                                      $0))))))
                                                                                (progn
                                                                                 (fmakunbound
                                                                                  'pascal/parse-formal-section)
                                                                                 (defun
                                                                                  pascal/parse-formal-section
                                                                                  (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  "(--> formal-section (seq (formal-section-1 formal-section-2) ((list* 'formal-section $0))))"
                                                                                  (com.informatimago.rdp::with-non-terminal
                                                                                   (formal-section
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   (cond
                                                                                    ((member
                                                                                      (scanner-current-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      '(nil
                                                                                        var
                                                                                        identifier
                                                                                        function
                                                                                        procedure
                                                                                        clop
                                                                                        semi)
                                                                                      :test
                                                                                      #'word-equal)
                                                                                     (let*
                                                                                      (($1
                                                                                        (pascal/parse-formal-section-1
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       ($2
                                                                                        (pascal/parse-formal-section-2
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                       ($0 (list $1 $2)))
                                                                                      (declare (ignorable $0 $1 $2))
                                                                                      (list* 'formal-section $0)))
                                                                                    (t
                                                                                     (com.informatimago.rdp::error-unexpected-token
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner
                                                                                      '(nil
                                                                                        var
                                                                                        identifier
                                                                                        function
                                                                                        procedure
                                                                                        clop
                                                                                        semi)
                                                                                      '(-->
                                                                                        formal-section
                                                                                        (alt
                                                                                         (seq
                                                                                          (formal-section-1
                                                                                           formal-section-2)
                                                                                          ((list*
                                                                                            'formal-section
                                                                                            $0)))))))))))
                                                                                (progn
                                                                                 (fmakunbound
                                                                                  'pascal/parse-formal-params-1)
                                                                                 (defun
                                                                                  pascal/parse-formal-params-1
                                                                                  (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                  "(--> formal-params-1 (alt ((seq nil ('nil)) (seq (formal-params-1-1 formal-params-1) ((cons $1 $2))))))"
                                                                                  (com.informatimago.rdp::with-non-terminal
                                                                                   (formal-params-1
                                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   (loop :with com.informatimago.rdp::$items
                                                                                         :=
                                                                                         'nil
                                                                                    :do (cond
                                                                                         ((word-equal
                                                                                           (scanner-current-token
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           'semi)
                                                                                          (push
                                                                                           (let*
                                                                                            (($1
                                                                                              (pascal/parse-formal-params-1-1
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                             ($2
                                                                                              (let*
                                                                                               (($0 (list)))
                                                                                               (declare (ignorable $0))
                                                                                               'nil))
                                                                                             ($0 (list $1 $2)))
                                                                                            (declare
                                                                                             (ignorable $0 $1 $2))
                                                                                            (cons $1 $2))
                                                                                           com.informatimago.rdp::$items))
                                                                                         (t (loop-finish)))
                                                                                    :finally (return
                                                                                              (reduce
                                                                                               #'append
                                                                                               (nreverse
                                                                                                com.informatimago.rdp::$items)
                                                                                               :initial-value
                                                                                               nil))))))
                                                                                 (progn
                                                                                  (fmakunbound
                                                                                   'pascal/parse-formal-params-1-1)
                                                                                  (defun
                                                                                   pascal/parse-formal-params-1-1
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> formal-params-1-1 (seq (semi formal-section) ($0)))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (formal-params-1-1
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (cond
                                                                                     ((word-equal
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       'semi)
                                                                                      (let*
                                                                                       (($1
                                                                                         (accept
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          'semi))
                                                                                        (semi $1)
                                                                                        (semi.1 $1)
                                                                                        ($2
                                                                                         (pascal/parse-formal-section
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (formal-section $2)
                                                                                        (formal-section.1 $2)
                                                                                        ($0 (list $1 $2)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         formal-section.1
                                                                                         formal-section
                                                                                         semi.1
                                                                                         semi
                                                                                         $1
                                                                                         $2))
                                                                                       $0))
                                                                                     (t
                                                                                      (com.informatimago.rdp::error-unexpected-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       '(semi)
                                                                                       '(-->
                                                                                         formal-params-1-1
                                                                                         (alt
                                                                                          (seq
                                                                                           (semi formal-section)
                                                                                           ($0))))))))))
                                                                                 (progn
                                                                                  (fmakunbound 'pascal/parse-block)
                                                                                  (defun
                                                                                   pascal/parse-block
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> block (seq (declaration-part statement-part) ((list* 'block $0))))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (block
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (cond
                                                                                     ((member
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       '(label
                                                                                         const
                                                                                         type
                                                                                         var
                                                                                         function
                                                                                         procedure
                                                                                         nil
                                                                                         begin
                                                                                         semi
                                                                                         dot)
                                                                                       :test
                                                                                       #'word-equal)
                                                                                      (let*
                                                                                       (($1
                                                                                         (pascal/parse-declaration-part
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (declaration-part $1)
                                                                                        (declaration-part.1 $1)
                                                                                        ($2
                                                                                         (pascal/parse-statement-part
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (statement-part $2)
                                                                                        (statement-part.1 $2)
                                                                                        ($0 (list $1 $2)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         statement-part.1
                                                                                         statement-part
                                                                                         declaration-part.1
                                                                                         declaration-part
                                                                                         $1
                                                                                         $2))
                                                                                       (list* 'block $0)))
                                                                                     (t
                                                                                      (com.informatimago.rdp::error-unexpected-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       '(label
                                                                                         const
                                                                                         type
                                                                                         var
                                                                                         function
                                                                                         procedure
                                                                                         nil
                                                                                         begin
                                                                                         semi
                                                                                         dot)
                                                                                       '(-->
                                                                                         block
                                                                                         (alt
                                                                                          (seq
                                                                                           (declaration-part
                                                                                            statement-part)
                                                                                           ((list* 'block $0)))))))))))
                                                                                 (progn
                                                                                  (fmakunbound
                                                                                   'pascal/parse-formal-params)
                                                                                  (defun
                                                                                   pascal/parse-formal-params
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> formal-params (seq (opar formal-section formal-params-1 clop) ((list* 'formal-params $0))))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (formal-params
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (cond
                                                                                     ((word-equal
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       'opar)
                                                                                      (let*
                                                                                       (($1
                                                                                         (accept
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          'opar))
                                                                                        (opar $1)
                                                                                        (opar.1 $1)
                                                                                        ($2
                                                                                         (pascal/parse-formal-section
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (formal-section $2)
                                                                                        (formal-section.1 $2)
                                                                                        ($3
                                                                                         (pascal/parse-formal-params-1
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        ($4
                                                                                         (accept
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          'clop))
                                                                                        (clop $4)
                                                                                        (clop.1 $4)
                                                                                        ($0 (list $1 $2 $3 $4)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         clop.1
                                                                                         clop
                                                                                         formal-section.1
                                                                                         formal-section
                                                                                         opar.1
                                                                                         opar
                                                                                         $1
                                                                                         $2
                                                                                         $3
                                                                                         $4))
                                                                                       (list* 'formal-params $0)))
                                                                                     (t
                                                                                      (com.informatimago.rdp::error-unexpected-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       '(opar)
                                                                                       '(-->
                                                                                         formal-params
                                                                                         (alt
                                                                                          (seq
                                                                                           (opar
                                                                                            formal-section
                                                                                            formal-params-1
                                                                                            clop)
                                                                                           ((list*
                                                                                             'formal-params
                                                                                             $0)))))))))))
                                                                                 (progn
                                                                                  (fmakunbound
                                                                                   'pascal/parse-return-type)
                                                                                  (defun
                                                                                   pascal/parse-return-type
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> return-type (alt ((seq (colon type-ident) ($0)) (seq nil ('nil)))))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (return-type
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (cond
                                                                                     ((word-equal
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       'colon)
                                                                                      (let*
                                                                                       (($1
                                                                                         (accept
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          'colon))
                                                                                        (colon $1)
                                                                                        (colon.1 $1)
                                                                                        ($2
                                                                                         (pascal/parse-type-ident
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (type-ident $2)
                                                                                        (type-ident.1 $2)
                                                                                        ($0 (list $1 $2)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         type-ident.1
                                                                                         type-ident
                                                                                         colon.1
                                                                                         colon
                                                                                         $1
                                                                                         $2))
                                                                                       $0))))))
                                                                                 (progn
                                                                                  (fmakunbound
                                                                                   'pascal/parse-func-heading-1)
                                                                                  (defun
                                                                                   pascal/parse-func-heading-1
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> func-heading-1 (alt ((seq nil ('nil)) (seq (formal-params) ($0)))))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (func-heading-1
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (cond
                                                                                     ((word-equal
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       'opar)
                                                                                      (let*
                                                                                       (($1
                                                                                         (pascal/parse-formal-params
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (formal-params $1)
                                                                                        (formal-params.1 $1)
                                                                                        ($0 (list $1)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         formal-params.1
                                                                                         formal-params
                                                                                         $1))
                                                                                       $0))))))
                                                                                 (progn
                                                                                  (fmakunbound
                                                                                   'pascal/parse-proc-heading-1)
                                                                                  (defun
                                                                                   pascal/parse-proc-heading-1
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> proc-heading-1 (alt ((seq nil ('nil)) (seq (formal-params) ($0)))))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (proc-heading-1
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (cond
                                                                                     ((word-equal
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       'opar)
                                                                                      (let*
                                                                                       (($1
                                                                                         (pascal/parse-formal-params
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (formal-params $1)
                                                                                        (formal-params.1 $1)
                                                                                        ($0 (list $1)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         formal-params.1
                                                                                         formal-params
                                                                                         $1))
                                                                                       $0))))))
                                                                                 (progn
                                                                                  (fmakunbound 'pascal/parse-body)
                                                                                  (defun
                                                                                   pascal/parse-body
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> body (alt ((seq (block) ($1)) (seq (forward) ($1)))))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (body
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (cond
                                                                                     ((member
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       '(begin
                                                                                         label
                                                                                         const
                                                                                         type
                                                                                         var
                                                                                         function
                                                                                         procedure)
                                                                                       :test
                                                                                       #'word-equal)
                                                                                      (let*
                                                                                       (($1
                                                                                         (pascal/parse-block
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (block $1)
                                                                                        (block.1 $1)
                                                                                        ($0 (list $1)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         block.1
                                                                                         block
                                                                                         $1))
                                                                                       $1))
                                                                                     ((word-equal
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       'forward)
                                                                                      (let*
                                                                                       (($1
                                                                                         (accept
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          'forward))
                                                                                        (forward $1)
                                                                                        (forward.1 $1)
                                                                                        ($0 (list $1)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         forward.1
                                                                                         forward
                                                                                         $1))
                                                                                       $1))
                                                                                     (t
                                                                                      (com.informatimago.rdp::error-unexpected-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       '(begin
                                                                                         label
                                                                                         const
                                                                                         type
                                                                                         var
                                                                                         function
                                                                                         procedure
                                                                                         forward)
                                                                                       '(-->
                                                                                         body
                                                                                         (alt
                                                                                          (seq (block) ($1))
                                                                                          (seq (forward) ($1))))))))))
                                                                                 (progn
                                                                                  (fmakunbound
                                                                                   'pascal/parse-proc-declarations-1)
                                                                                  (defun
                                                                                   pascal/parse-proc-declarations-1
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> proc-declarations-1 (alt ((seq (proc-heading) ($1)) (seq (func-heading) ($1)))))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (proc-declarations-1
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (cond
                                                                                     ((word-equal
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       'procedure)
                                                                                      (let*
                                                                                       (($1
                                                                                         (pascal/parse-proc-heading
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (proc-heading $1)
                                                                                        (proc-heading.1 $1)
                                                                                        ($0 (list $1)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         proc-heading.1
                                                                                         proc-heading
                                                                                         $1))
                                                                                       $1))
                                                                                     ((word-equal
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       'function)
                                                                                      (let*
                                                                                       (($1
                                                                                         (pascal/parse-func-heading
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (func-heading $1)
                                                                                        (func-heading.1 $1)
                                                                                        ($0 (list $1)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         func-heading.1
                                                                                         func-heading
                                                                                         $1))
                                                                                       $1))
                                                                                     (t
                                                                                      (com.informatimago.rdp::error-unexpected-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       '(procedure function)
                                                                                       '(-->
                                                                                         proc-declarations-1
                                                                                         (alt
                                                                                          (seq (proc-heading) ($1))
                                                                                          (seq
                                                                                           (func-heading)
                                                                                           ($1))))))))))
                                                                                 (progn
                                                                                  (fmakunbound
                                                                                   'pascal/parse-new-ident-list)
                                                                                  (defun
                                                                                   pascal/parse-new-ident-list
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> new-ident-list (seq (new-ident new-ident-list-1) ((list* 'new-ident-list $0))))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (new-ident-list
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (cond
                                                                                     ((word-equal
                                                                                       (scanner-current-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       'identifier)
                                                                                      (let*
                                                                                       (($1
                                                                                         (pascal/parse-new-ident
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        (new-ident $1)
                                                                                        (new-ident.1 $1)
                                                                                        ($2
                                                                                         (pascal/parse-new-ident-list-1
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                        ($0 (list $1 $2)))
                                                                                       (declare
                                                                                        (ignorable
                                                                                         $0
                                                                                         new-ident.1
                                                                                         new-ident
                                                                                         $1
                                                                                         $2))
                                                                                       (list* 'new-ident-list $0)))
                                                                                     (t
                                                                                      (com.informatimago.rdp::error-unexpected-token
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner
                                                                                       '(identifier)
                                                                                       '(-->
                                                                                         new-ident-list
                                                                                         (alt
                                                                                          (seq
                                                                                           (new-ident new-ident-list-1)
                                                                                           ((list*
                                                                                             'new-ident-list
                                                                                             $0)))))))))))
                                                                                 (progn
                                                                                  (fmakunbound
                                                                                   'pascal/parse-var-declarations-1-1)
                                                                                  (defun
                                                                                   pascal/parse-var-declarations-1-1
                                                                                   (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                   "(--> var-declarations-1-1 (alt ((seq nil ('nil)) (seq (var-decl var-declarations-1-1) ((cons (progn $0) $2))))))"
                                                                                   (com.informatimago.rdp::with-non-terminal
                                                                                    (var-declarations-1-1
                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    (loop :with com.informatimago.rdp::$items
                                                                                          :=
                                                                                          'nil
                                                                                     :do (cond
                                                                                          ((word-equal
                                                                                            (scanner-current-token
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            'identifier)
                                                                                           (push
                                                                                            (let*
                                                                                             (($1
                                                                                               (pascal/parse-var-decl
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                              (var-decl $1)
                                                                                              (var-decl.1 $1)
                                                                                              ($2
                                                                                               (let*
                                                                                                (($0 (list)))
                                                                                                (declare
                                                                                                 (ignorable $0))
                                                                                                'nil))
                                                                                              ($0 (list $1 $2)))
                                                                                             (declare
                                                                                              (ignorable
                                                                                               $0
                                                                                               var-decl.1
                                                                                               var-decl
                                                                                               $1
                                                                                               $2))
                                                                                             (cons (progn $0) $2))
                                                                                            com.informatimago.rdp::$items))
                                                                                          (t (loop-finish)))
                                                                                     :finally (return
                                                                                               (reduce
                                                                                                #'append
                                                                                                (nreverse
                                                                                                 com.informatimago.rdp::$items)
                                                                                                :initial-value
                                                                                                nil))))))
                                                                                  (progn
                                                                                   (fmakunbound 'pascal/parse-var-decl)
                                                                                   (defun
                                                                                    pascal/parse-var-decl
                                                                                    (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    "(--> var-decl (seq (new-ident-list colon type semi) ((list* 'var-decl $0))))"
                                                                                    (com.informatimago.rdp::with-non-terminal
                                                                                     (var-decl
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     (cond
                                                                                      ((word-equal
                                                                                        (scanner-current-token
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        'identifier)
                                                                                       (let*
                                                                                        (($1
                                                                                          (pascal/parse-new-ident-list
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                         (new-ident-list $1)
                                                                                         (new-ident-list.1 $1)
                                                                                         ($2
                                                                                          (accept
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner
                                                                                           'colon))
                                                                                         (colon $2)
                                                                                         (colon.1 $2)
                                                                                         ($3
                                                                                          (accept
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner
                                                                                           'type))
                                                                                         (type $3)
                                                                                         (type.1 $3)
                                                                                         ($4
                                                                                          (accept
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner
                                                                                           'semi))
                                                                                         (semi $4)
                                                                                         (semi.1 $4)
                                                                                         ($0 (list $1 $2 $3 $4)))
                                                                                        (declare
                                                                                         (ignorable
                                                                                          $0
                                                                                          semi.1
                                                                                          semi
                                                                                          type.1
                                                                                          type
                                                                                          colon.1
                                                                                          colon
                                                                                          new-ident-list.1
                                                                                          new-ident-list
                                                                                          $1
                                                                                          $2
                                                                                          $3
                                                                                          $4))
                                                                                        (list* 'var-decl $0)))
                                                                                      (t
                                                                                       (com.informatimago.rdp::error-unexpected-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                        '(identifier)
                                                                                        '(-->
                                                                                          var-decl
                                                                                          (alt
                                                                                           (seq
                                                                                            (new-ident-list
                                                                                             colon
                                                                                             type
                                                                                             semi)
                                                                                            ((list*
                                                                                              'var-decl
                                                                                              $0)))))))))))
                                                                                  (progn
                                                                                   (fmakunbound
                                                                                    'pascal/parse-field-list)
                                                                                   (defun
                                                                                    pascal/parse-field-list
                                                                                    (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    "(--> field-list (alt ((seq nil ('nil)) (seq (field-list-1) ($0)))))"
                                                                                    (com.informatimago.rdp::with-non-terminal
                                                                                     (field-list
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     (cond
                                                                                      ((member
                                                                                        (scanner-current-token
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        '(identifier case)
                                                                                        :test
                                                                                        #'word-equal)
                                                                                       (let*
                                                                                        (($1
                                                                                          (pascal/parse-field-list-1
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                         ($0 (list $1)))
                                                                                        (declare (ignorable $0 $1))
                                                                                        $0))))))
                                                                                  (progn
                                                                                   (fmakunbound
                                                                                    'pascal/parse-variant-selector-1)
                                                                                   (defun
                                                                                    pascal/parse-variant-selector-1
                                                                                    (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    "(--> variant-selector-1 (alt ((seq (new-ident colon) ($0)) (seq nil ('nil)))))"
                                                                                    (com.informatimago.rdp::with-non-terminal
                                                                                     (variant-selector-1
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     (cond
                                                                                      ((word-equal
                                                                                        (scanner-current-token
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        'identifier)
                                                                                       (let*
                                                                                        (($1
                                                                                          (pascal/parse-new-ident
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                         (new-ident $1)
                                                                                         (new-ident.1 $1)
                                                                                         ($2
                                                                                          (accept
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner
                                                                                           'colon))
                                                                                         (colon $2)
                                                                                         (colon.1 $2)
                                                                                         ($0 (list $1 $2)))
                                                                                        (declare
                                                                                         (ignorable
                                                                                          $0
                                                                                          colon.1
                                                                                          colon
                                                                                          new-ident.1
                                                                                          new-ident
                                                                                          $1
                                                                                          $2))
                                                                                        $0))))))
                                                                                  (progn
                                                                                   (fmakunbound 'pascal/parse-variant)
                                                                                   (defun
                                                                                    pascal/parse-variant
                                                                                    (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    "(--> variant (seq (case-label-list colon opar field-list clop) ((list* 'variant $0))))"
                                                                                    (com.informatimago.rdp::with-non-terminal
                                                                                     (variant
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     (cond
                                                                                      ((member
                                                                                        (scanner-current-token
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        '(string
                                                                                          integer
                                                                                          real
                                                                                          identifier
                                                                                          minus
                                                                                          plus)
                                                                                        :test
                                                                                        #'word-equal)
                                                                                       (let*
                                                                                        (($1
                                                                                          (pascal/parse-case-label-list
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                         (case-label-list $1)
                                                                                         (case-label-list.1 $1)
                                                                                         ($2
                                                                                          (accept
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner
                                                                                           'colon))
                                                                                         (colon $2)
                                                                                         (colon.1 $2)
                                                                                         ($3
                                                                                          (accept
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner
                                                                                           'opar))
                                                                                         (opar $3)
                                                                                         (opar.1 $3)
                                                                                         ($4
                                                                                          (pascal/parse-field-list
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                         (field-list $4)
                                                                                         (field-list.1 $4)
                                                                                         ($5
                                                                                          (accept
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner
                                                                                           'clop))
                                                                                         (clop $5)
                                                                                         (clop.1 $5)
                                                                                         ($0 (list $1 $2 $3 $4 $5)))
                                                                                        (declare
                                                                                         (ignorable
                                                                                          $0
                                                                                          clop.1
                                                                                          clop
                                                                                          field-list.1
                                                                                          field-list
                                                                                          opar.1
                                                                                          opar
                                                                                          colon.1
                                                                                          colon
                                                                                          case-label-list.1
                                                                                          case-label-list
                                                                                          $1
                                                                                          $2
                                                                                          $3
                                                                                          $4
                                                                                          $5))
                                                                                        (list* 'variant $0)))
                                                                                      (t
                                                                                       (com.informatimago.rdp::error-unexpected-token
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                        '(string
                                                                                          integer
                                                                                          real
                                                                                          identifier
                                                                                          minus
                                                                                          plus)
                                                                                        '(-->
                                                                                          variant
                                                                                          (alt
                                                                                           (seq
                                                                                            (case-label-list
                                                                                             colon
                                                                                             opar
                                                                                             field-list
                                                                                             clop)
                                                                                            ((list*
                                                                                              'variant
                                                                                              $0)))))))))))
                                                                                  (progn
                                                                                   (fmakunbound
                                                                                    'pascal/parse-variant-part-1)
                                                                                   (defun
                                                                                    pascal/parse-variant-part-1
                                                                                    (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                    "(--> variant-part-1 (alt ((seq nil ('nil)) (seq (variant-part-1-1 variant-part-1) ((cons $1 $2))))))"
                                                                                    (com.informatimago.rdp::with-non-terminal
                                                                                     (variant-part-1
                                                                                      com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     (loop :with com.informatimago.rdp::$items
                                                                                           :=
                                                                                           'nil
                                                                                      :do (cond
                                                                                           ((word-equal
                                                                                             (scanner-current-token
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             'semi)
                                                                                            (push
                                                                                             (let*
                                                                                              (($1
                                                                                                (pascal/parse-variant-part-1-1
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                               ($2
                                                                                                (let*
                                                                                                 (($0 (list)))
                                                                                                 (declare
                                                                                                  (ignorable $0))
                                                                                                 'nil))
                                                                                               ($0 (list $1 $2)))
                                                                                              (declare
                                                                                               (ignorable $0 $1 $2))
                                                                                              (cons $1 $2))
                                                                                             com.informatimago.rdp::$items))
                                                                                           (t (loop-finish)))
                                                                                      :finally (return
                                                                                                (reduce
                                                                                                 #'append
                                                                                                 (nreverse
                                                                                                  com.informatimago.rdp::$items)
                                                                                                 :initial-value
                                                                                                 nil))))))
                                                                                   (progn
                                                                                    (fmakunbound
                                                                                     'pascal/parse-variant-part-1-1)
                                                                                    (defun
                                                                                     pascal/parse-variant-part-1-1
                                                                                     (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     "(--> variant-part-1-1 (seq (semi variant) ($0)))"
                                                                                     (com.informatimago.rdp::with-non-terminal
                                                                                      (variant-part-1-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      (cond
                                                                                       ((word-equal
                                                                                         (scanner-current-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         'semi)
                                                                                        (let*
                                                                                         (($1
                                                                                           (accept
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner
                                                                                            'semi))
                                                                                          (semi $1)
                                                                                          (semi.1 $1)
                                                                                          ($2
                                                                                           (pascal/parse-variant
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                          (variant $2)
                                                                                          (variant.1 $2)
                                                                                          ($0 (list $1 $2)))
                                                                                         (declare
                                                                                          (ignorable
                                                                                           $0
                                                                                           variant.1
                                                                                           variant
                                                                                           semi.1
                                                                                           semi
                                                                                           $1
                                                                                           $2))
                                                                                         $0))
                                                                                       (t
                                                                                        (com.informatimago.rdp::error-unexpected-token
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner
                                                                                         '(semi)
                                                                                         '(-->
                                                                                           variant-part-1-1
                                                                                           (alt
                                                                                            (seq
                                                                                             (semi variant)
                                                                                             ($0))))))))))
                                                                                   (progn
                                                                                    (fmakunbound
                                                                                     'pascal/parse-variant-selector)
                                                                                    (defun
                                                                                     pascal/parse-variant-selector
                                                                                     (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     "(--> variant-selector (seq (variant-selector-1 type-ident) ((list* 'variant-selector $0))))"
                                                                                     (com.informatimago.rdp::with-non-terminal
                                                                                      (variant-selector
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      (cond
                                                                                       ((member
                                                                                         (scanner-current-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         '(identifier nil of)
                                                                                         :test
                                                                                         #'word-equal)
                                                                                        (let*
                                                                                         (($1
                                                                                           (pascal/parse-variant-selector-1
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                          ($2
                                                                                           (pascal/parse-type-ident
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                          (type-ident $2)
                                                                                          (type-ident.1 $2)
                                                                                          ($0 (list $1 $2)))
                                                                                         (declare
                                                                                          (ignorable
                                                                                           $0
                                                                                           type-ident.1
                                                                                           type-ident
                                                                                           $1
                                                                                           $2))
                                                                                         (list* 'variant-selector $0)))
                                                                                       (t
                                                                                        (com.informatimago.rdp::error-unexpected-token
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner
                                                                                         '(identifier nil of)
                                                                                         '(-->
                                                                                           variant-selector
                                                                                           (alt
                                                                                            (seq
                                                                                             (variant-selector-1
                                                                                              type-ident)
                                                                                             ((list*
                                                                                               'variant-selector
                                                                                               $0)))))))))))
                                                                                   (progn
                                                                                    (fmakunbound
                                                                                     'pascal/parse-record-section)
                                                                                    (defun
                                                                                     pascal/parse-record-section
                                                                                     (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     "(--> record-section (seq (new-ident-list colon type) ((list* 'record-section $0))))"
                                                                                     (com.informatimago.rdp::with-non-terminal
                                                                                      (record-section
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      (cond
                                                                                       ((word-equal
                                                                                         (scanner-current-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         'identifier)
                                                                                        (let*
                                                                                         (($1
                                                                                           (pascal/parse-new-ident-list
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                          (new-ident-list $1)
                                                                                          (new-ident-list.1 $1)
                                                                                          ($2
                                                                                           (accept
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner
                                                                                            'colon))
                                                                                          (colon $2)
                                                                                          (colon.1 $2)
                                                                                          ($3
                                                                                           (accept
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner
                                                                                            'type))
                                                                                          (type $3)
                                                                                          (type.1 $3)
                                                                                          ($0 (list $1 $2 $3)))
                                                                                         (declare
                                                                                          (ignorable
                                                                                           $0
                                                                                           type.1
                                                                                           type
                                                                                           colon.1
                                                                                           colon
                                                                                           new-ident-list.1
                                                                                           new-ident-list
                                                                                           $1
                                                                                           $2
                                                                                           $3))
                                                                                         (list* 'record-section $0)))
                                                                                       (t
                                                                                        (com.informatimago.rdp::error-unexpected-token
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner
                                                                                         '(identifier)
                                                                                         '(-->
                                                                                           record-section
                                                                                           (alt
                                                                                            (seq
                                                                                             (new-ident-list
                                                                                              colon
                                                                                              type)
                                                                                             ((list*
                                                                                               'record-section
                                                                                               $0)))))))))))
                                                                                   (progn
                                                                                    (fmakunbound
                                                                                     'pascal/parse-fixed-part-1)
                                                                                    (defun
                                                                                     pascal/parse-fixed-part-1
                                                                                     (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                     "(--> fixed-part-1 (alt ((seq nil ('nil)) (seq (fixed-part-1-1 fixed-part-1) ((cons $1 $2))))))"
                                                                                     (com.informatimago.rdp::with-non-terminal
                                                                                      (fixed-part-1
                                                                                       com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      (loop :with com.informatimago.rdp::$items
                                                                                            :=
                                                                                            'nil
                                                                                       :do (cond
                                                                                            ((word-equal
                                                                                              (scanner-current-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                              'semi)
                                                                                             (push
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-fixed-part-1-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($2
                                                                                                 (let*
                                                                                                  (($0 (list)))
                                                                                                  (declare
                                                                                                   (ignorable $0))
                                                                                                  'nil))
                                                                                                ($0 (list $1 $2)))
                                                                                               (declare
                                                                                                (ignorable $0 $1 $2))
                                                                                               (cons $1 $2))
                                                                                              com.informatimago.rdp::$items))
                                                                                            (t (loop-finish)))
                                                                                       :finally (return
                                                                                                 (reduce
                                                                                                  #'append
                                                                                                  (nreverse
                                                                                                   com.informatimago.rdp::$items)
                                                                                                  :initial-value
                                                                                                  nil))))))
                                                                                    (progn
                                                                                     (fmakunbound
                                                                                      'pascal/parse-fixed-part-1-1)
                                                                                     (defun
                                                                                      pascal/parse-fixed-part-1-1
                                                                                      (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      "(--> fixed-part-1-1 (seq (semi record-section) ($0)))"
                                                                                      (com.informatimago.rdp::with-non-terminal
                                                                                       (fixed-part-1-1
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       (cond
                                                                                        ((word-equal
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          'semi)
                                                                                         (let*
                                                                                          (($1
                                                                                            (accept
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             'semi))
                                                                                           (semi $1)
                                                                                           (semi.1 $1)
                                                                                           ($2
                                                                                            (pascal/parse-record-section
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           (record-section $2)
                                                                                           (record-section.1 $2)
                                                                                           ($0 (list $1 $2)))
                                                                                          (declare
                                                                                           (ignorable
                                                                                            $0
                                                                                            record-section.1
                                                                                            record-section
                                                                                            semi.1
                                                                                            semi
                                                                                            $1
                                                                                            $2))
                                                                                          $0))
                                                                                        (t
                                                                                         (com.informatimago.rdp::error-unexpected-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          '(semi)
                                                                                          '(-->
                                                                                            fixed-part-1-1
                                                                                            (alt
                                                                                             (seq
                                                                                              (semi record-section)
                                                                                              ($0))))))))))
                                                                                    (progn
                                                                                     (fmakunbound
                                                                                      'pascal/parse-variant-part)
                                                                                     (defun
                                                                                      pascal/parse-variant-part
                                                                                      (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      "(--> variant-part (seq (case variant-selector of variant variant-part-1) ((list* 'variant-part $0))))"
                                                                                      (com.informatimago.rdp::with-non-terminal
                                                                                       (variant-part
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       (cond
                                                                                        ((word-equal
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          'case)
                                                                                         (let*
                                                                                          (($1
                                                                                            (accept
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             'case))
                                                                                           (case $1)
                                                                                           (case.1 $1)
                                                                                           ($2
                                                                                            (pascal/parse-variant-selector
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           (variant-selector $2)
                                                                                           (variant-selector.1 $2)
                                                                                           ($3
                                                                                            (accept
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             'of))
                                                                                           (of $3)
                                                                                           (of.1 $3)
                                                                                           ($4
                                                                                            (pascal/parse-variant
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           (variant $4)
                                                                                           (variant.1 $4)
                                                                                           ($5
                                                                                            (pascal/parse-variant-part-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           ($0 (list $1 $2 $3 $4 $5)))
                                                                                          (declare
                                                                                           (ignorable
                                                                                            $0
                                                                                            variant.1
                                                                                            variant
                                                                                            of.1
                                                                                            of
                                                                                            variant-selector.1
                                                                                            variant-selector
                                                                                            case.1
                                                                                            case
                                                                                            $1
                                                                                            $2
                                                                                            $3
                                                                                            $4
                                                                                            $5))
                                                                                          (list* 'variant-part $0)))
                                                                                        (t
                                                                                         (com.informatimago.rdp::error-unexpected-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          '(case)
                                                                                          '(-->
                                                                                            variant-part
                                                                                            (alt
                                                                                             (seq
                                                                                              (case
                                                                                               variant-selector
                                                                                               of
                                                                                               variant
                                                                                               variant-part-1)
                                                                                              ((list*
                                                                                                'variant-part
                                                                                                $0)))))))))))
                                                                                    (progn
                                                                                     (fmakunbound
                                                                                      'pascal/parse-field-list-1-2)
                                                                                     (defun
                                                                                      pascal/parse-field-list-1-2
                                                                                      (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      "(--> field-list-1-2 (alt ((seq nil ('nil)) (seq (semi) ($0)))))"
                                                                                      (com.informatimago.rdp::with-non-terminal
                                                                                       (field-list-1-2
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       (cond
                                                                                        ((word-equal
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          'semi)
                                                                                         (let*
                                                                                          (($1
                                                                                            (accept
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             'semi))
                                                                                           (semi $1)
                                                                                           (semi.1 $1)
                                                                                           ($0 (list $1)))
                                                                                          (declare
                                                                                           (ignorable
                                                                                            $0
                                                                                            semi.1
                                                                                            semi
                                                                                            $1))
                                                                                          $0))))))
                                                                                    (progn
                                                                                     (fmakunbound
                                                                                      'pascal/parse-field-list-1-1)
                                                                                     (defun
                                                                                      pascal/parse-field-list-1-1
                                                                                      (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      "(--> field-list-1-1 (alt ((seq (fixed-part field-list-1-1-1-1) ($0)) (seq (variant-part) ($1)))))"
                                                                                      (com.informatimago.rdp::with-non-terminal
                                                                                       (field-list-1-1
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       (cond
                                                                                        ((word-equal
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          'identifier)
                                                                                         (let*
                                                                                          (($1
                                                                                            (pascal/parse-fixed-part
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           (fixed-part $1)
                                                                                           (fixed-part.1 $1)
                                                                                           ($2
                                                                                            (pascal/parse-field-list-1-1-1-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           ($0 (list $1 $2)))
                                                                                          (declare
                                                                                           (ignorable
                                                                                            $0
                                                                                            fixed-part.1
                                                                                            fixed-part
                                                                                            $1
                                                                                            $2))
                                                                                          $0))
                                                                                        ((word-equal
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          'case)
                                                                                         (let*
                                                                                          (($1
                                                                                            (pascal/parse-variant-part
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           (variant-part $1)
                                                                                           (variant-part.1 $1)
                                                                                           ($0 (list $1)))
                                                                                          (declare
                                                                                           (ignorable
                                                                                            $0
                                                                                            variant-part.1
                                                                                            variant-part
                                                                                            $1))
                                                                                          $1))
                                                                                        (t
                                                                                         (com.informatimago.rdp::error-unexpected-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          '(identifier case)
                                                                                          '(-->
                                                                                            field-list-1-1
                                                                                            (alt
                                                                                             (seq
                                                                                              (fixed-part
                                                                                               field-list-1-1-1-1)
                                                                                              ($0))
                                                                                             (seq
                                                                                              (variant-part)
                                                                                              ($1))))))))))
                                                                                    (progn
                                                                                     (fmakunbound
                                                                                      'pascal/parse-field-list-1)
                                                                                     (defun
                                                                                      pascal/parse-field-list-1
                                                                                      (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      "(--> field-list-1 (seq (field-list-1-1 field-list-1-2) ($0)))"
                                                                                      (com.informatimago.rdp::with-non-terminal
                                                                                       (field-list-1
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       (cond
                                                                                        ((member
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          '(case identifier)
                                                                                          :test
                                                                                          #'word-equal)
                                                                                         (let*
                                                                                          (($1
                                                                                            (pascal/parse-field-list-1-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           ($2
                                                                                            (pascal/parse-field-list-1-2
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           ($0 (list $1 $2)))
                                                                                          (declare
                                                                                           (ignorable $0 $1 $2))
                                                                                          $0))
                                                                                        (t
                                                                                         (com.informatimago.rdp::error-unexpected-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          '(case identifier)
                                                                                          '(-->
                                                                                            field-list-1
                                                                                            (alt
                                                                                             (seq
                                                                                              (field-list-1-1
                                                                                               field-list-1-2)
                                                                                              ($0))))))))))
                                                                                    (progn
                                                                                     (fmakunbound
                                                                                      'pascal/parse-simple-type)
                                                                                     (defun
                                                                                      pascal/parse-simple-type
                                                                                      (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      "(--> simple-type (alt ((seq (simple-type-2-1 simple-type-2-2 dotdot constant) ($0)) (seq (unsigned-number dotdot constant) ($0)) (seq (opar new-ident-list clop) ((list* 'enumeration-type $0))) (seq (identifier simple-type-1-1) ($0)))))"
                                                                                      (com.informatimago.rdp::with-non-terminal
                                                                                       (simple-type
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       (cond
                                                                                        ((member
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          '(minus plus)
                                                                                          :test
                                                                                          #'word-equal)
                                                                                         (let*
                                                                                          (($1
                                                                                            (pascal/parse-simple-type-2-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           ($2
                                                                                            (pascal/parse-simple-type-2-2
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           ($3
                                                                                            (accept
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             'dotdot))
                                                                                           (dotdot $3)
                                                                                           (dotdot.1 $3)
                                                                                           ($4
                                                                                            (pascal/parse-constant
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           (constant $4)
                                                                                           (constant.1 $4)
                                                                                           ($0 (list $1 $2 $3 $4)))
                                                                                          (declare
                                                                                           (ignorable
                                                                                            $0
                                                                                            constant.1
                                                                                            constant
                                                                                            dotdot.1
                                                                                            dotdot
                                                                                            $1
                                                                                            $2
                                                                                            $3
                                                                                            $4))
                                                                                          $0))
                                                                                        ((member
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          '(integer real)
                                                                                          :test
                                                                                          #'word-equal)
                                                                                         (let*
                                                                                          (($1
                                                                                            (pascal/parse-unsigned-number
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           (unsigned-number $1)
                                                                                           (unsigned-number.1 $1)
                                                                                           ($2
                                                                                            (accept
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             'dotdot))
                                                                                           (dotdot $2)
                                                                                           (dotdot.1 $2)
                                                                                           ($3
                                                                                            (pascal/parse-constant
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           (constant $3)
                                                                                           (constant.1 $3)
                                                                                           ($0 (list $1 $2 $3)))
                                                                                          (declare
                                                                                           (ignorable
                                                                                            $0
                                                                                            constant.1
                                                                                            constant
                                                                                            dotdot.1
                                                                                            dotdot
                                                                                            unsigned-number.1
                                                                                            unsigned-number
                                                                                            $1
                                                                                            $2
                                                                                            $3))
                                                                                          $0))
                                                                                        ((word-equal
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          'opar)
                                                                                         (let*
                                                                                          (($1
                                                                                            (accept
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             'opar))
                                                                                           (opar $1)
                                                                                           (opar.1 $1)
                                                                                           ($2
                                                                                            (pascal/parse-new-ident-list
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           (new-ident-list $2)
                                                                                           (new-ident-list.1 $2)
                                                                                           ($3
                                                                                            (accept
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             'clop))
                                                                                           (clop $3)
                                                                                           (clop.1 $3)
                                                                                           ($0 (list $1 $2 $3)))
                                                                                          (declare
                                                                                           (ignorable
                                                                                            $0
                                                                                            clop.1
                                                                                            clop
                                                                                            new-ident-list.1
                                                                                            new-ident-list
                                                                                            opar.1
                                                                                            opar
                                                                                            $1
                                                                                            $2
                                                                                            $3))
                                                                                          (list*
                                                                                           'enumeration-type
                                                                                           $0)))
                                                                                        ((word-equal
                                                                                          (scanner-current-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          'identifier)
                                                                                         (let*
                                                                                          (($1
                                                                                            (accept
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             'identifier))
                                                                                           (identifier $1)
                                                                                           (identifier.1 $1)
                                                                                           ($2
                                                                                            (pascal/parse-simple-type-1-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                           ($0 (list $1 $2)))
                                                                                          (declare
                                                                                           (ignorable
                                                                                            $0
                                                                                            identifier.1
                                                                                            identifier
                                                                                            $1
                                                                                            $2))
                                                                                          $0))
                                                                                        (t
                                                                                         (com.informatimago.rdp::error-unexpected-token
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner
                                                                                          '(minus
                                                                                            plus
                                                                                            integer
                                                                                            real
                                                                                            opar
                                                                                            identifier)
                                                                                          '(-->
                                                                                            simple-type
                                                                                            (alt
                                                                                             (seq
                                                                                              (simple-type-2-1
                                                                                               simple-type-2-2
                                                                                               dotdot
                                                                                               constant)
                                                                                              ($0))
                                                                                             (seq
                                                                                              (unsigned-number
                                                                                               dotdot
                                                                                               constant)
                                                                                              ($0))
                                                                                             (seq
                                                                                              (opar
                                                                                               new-ident-list
                                                                                               clop)
                                                                                              ((list*
                                                                                                'enumeration-type
                                                                                                $0)))
                                                                                             (seq
                                                                                              (identifier
                                                                                               simple-type-1-1)
                                                                                              ($0))))))))))
                                                                                    (progn
                                                                                     (fmakunbound
                                                                                      'pascal/parse-index-list-1)
                                                                                     (defun
                                                                                      pascal/parse-index-list-1
                                                                                      (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                      "(--> index-list-1 (alt ((seq nil ('nil)) (seq (index-list-1-1 index-list-1) ((cons $1 $2))))))"
                                                                                      (com.informatimago.rdp::with-non-terminal
                                                                                       (index-list-1
                                                                                        com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       (loop :with com.informatimago.rdp::$items
                                                                                             :=
                                                                                             'nil
                                                                                        :do (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'comma)
                                                                                              (push
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-index-list-1-1
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 ($2
                                                                                                  (let*
                                                                                                   (($0 (list)))
                                                                                                   (declare
                                                                                                    (ignorable $0))
                                                                                                   'nil))
                                                                                                 ($0 (list $1 $2)))
                                                                                                (declare
                                                                                                 (ignorable $0 $1 $2))
                                                                                                (cons $1 $2))
                                                                                               com.informatimago.rdp::$items))
                                                                                             (t (loop-finish)))
                                                                                        :finally (return
                                                                                                  (reduce
                                                                                                   #'append
                                                                                                   (nreverse
                                                                                                    com.informatimago.rdp::$items)
                                                                                                   :initial-value
                                                                                                   nil))))))
                                                                                     (progn
                                                                                      (fmakunbound
                                                                                       'pascal/parse-index-list-1-1)
                                                                                      (defun
                                                                                       pascal/parse-index-list-1-1
                                                                                       (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       "(--> index-list-1-1 (seq (comma simple-type) ($0)))"
                                                                                       (com.informatimago.rdp::with-non-terminal
                                                                                        (index-list-1-1
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        (cond
                                                                                         ((word-equal
                                                                                           (scanner-current-token
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           'comma)
                                                                                          (let*
                                                                                           (($1
                                                                                             (accept
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner
                                                                                              'comma))
                                                                                            (comma $1)
                                                                                            (comma.1 $1)
                                                                                            ($2
                                                                                             (pascal/parse-simple-type
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                            (simple-type $2)
                                                                                            (simple-type.1 $2)
                                                                                            ($0 (list $1 $2)))
                                                                                           (declare
                                                                                            (ignorable
                                                                                             $0
                                                                                             simple-type.1
                                                                                             simple-type
                                                                                             comma.1
                                                                                             comma
                                                                                             $1
                                                                                             $2))
                                                                                           $0))
                                                                                         (t
                                                                                          (com.informatimago.rdp::error-unexpected-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner
                                                                                           '(comma)
                                                                                           '(-->
                                                                                             index-list-1-1
                                                                                             (alt
                                                                                              (seq
                                                                                               (comma simple-type)
                                                                                               ($0))))))))))
                                                                                     (progn
                                                                                      (fmakunbound
                                                                                       'pascal/parse-const-ident)
                                                                                      (defun
                                                                                       pascal/parse-const-ident
                                                                                       (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       "(--> const-ident (seq (identifier) ((list* 'const-ident $0))))"
                                                                                       (com.informatimago.rdp::with-non-terminal
                                                                                        (const-ident
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        (cond
                                                                                         ((word-equal
                                                                                           (scanner-current-token
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           'identifier)
                                                                                          (let*
                                                                                           (($1
                                                                                             (accept
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner
                                                                                              'identifier))
                                                                                            (identifier $1)
                                                                                            (identifier.1 $1)
                                                                                            ($0 (list $1)))
                                                                                           (declare
                                                                                            (ignorable
                                                                                             $0
                                                                                             identifier.1
                                                                                             identifier
                                                                                             $1))
                                                                                           (list* 'const-ident $0)))
                                                                                         (t
                                                                                          (com.informatimago.rdp::error-unexpected-token
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner
                                                                                           '(identifier)
                                                                                           '(-->
                                                                                             const-ident
                                                                                             (alt
                                                                                              (seq
                                                                                               (identifier)
                                                                                               ((list*
                                                                                                 'const-ident
                                                                                                 $0)))))))))))
                                                                                     (progn
                                                                                      (fmakunbound
                                                                                       'pascal/parse-type-definitions-1-1)
                                                                                      (defun
                                                                                       pascal/parse-type-definitions-1-1
                                                                                       (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                       "(--> type-definitions-1-1 (alt ((seq nil ('nil)) (seq (type-def type-definitions-1-1) ((cons (progn $0) $2))))))"
                                                                                       (com.informatimago.rdp::with-non-terminal
                                                                                        (type-definitions-1-1
                                                                                         com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        (loop :with com.informatimago.rdp::$items
                                                                                              :=
                                                                                              'nil
                                                                                         :do (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'identifier)
                                                                                               (push
                                                                                                (let*
                                                                                                 (($1
                                                                                                   (pascal/parse-type-def
                                                                                                    com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                  (type-def $1)
                                                                                                  (type-def.1 $1)
                                                                                                  ($2
                                                                                                   (let*
                                                                                                    (($0 (list)))
                                                                                                    (declare
                                                                                                     (ignorable $0))
                                                                                                    'nil))
                                                                                                  ($0 (list $1 $2)))
                                                                                                 (declare
                                                                                                  (ignorable
                                                                                                   $0
                                                                                                   type-def.1
                                                                                                   type-def
                                                                                                   $1
                                                                                                   $2))
                                                                                                 (cons (progn $0) $2))
                                                                                                com.informatimago.rdp::$items))
                                                                                              (t (loop-finish)))
                                                                                         :finally (return
                                                                                                   (reduce
                                                                                                    #'append
                                                                                                    (nreverse
                                                                                                     com.informatimago.rdp::$items)
                                                                                                    :initial-value
                                                                                                    nil))))))
                                                                                      (progn
                                                                                       (fmakunbound
                                                                                        'pascal/parse-type-def)
                                                                                       (defun
                                                                                        pascal/parse-type-def
                                                                                        (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        "(--> type-def (seq (new-ident eq type semi) ((list* 'type-def $0))))"
                                                                                        (com.informatimago.rdp::with-non-terminal
                                                                                         (type-def
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         (cond
                                                                                          ((word-equal
                                                                                            (scanner-current-token
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            'identifier)
                                                                                           (let*
                                                                                            (($1
                                                                                              (pascal/parse-new-ident
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                             (new-ident $1)
                                                                                             (new-ident.1 $1)
                                                                                             ($2
                                                                                              (accept
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               'eq))
                                                                                             (eq $2)
                                                                                             (eq.1 $2)
                                                                                             ($3
                                                                                              (accept
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               'type))
                                                                                             (type $3)
                                                                                             (type.1 $3)
                                                                                             ($4
                                                                                              (accept
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               'semi))
                                                                                             (semi $4)
                                                                                             (semi.1 $4)
                                                                                             ($0 (list $1 $2 $3 $4)))
                                                                                            (declare
                                                                                             (ignorable
                                                                                              $0
                                                                                              semi.1
                                                                                              semi
                                                                                              type.1
                                                                                              type
                                                                                              eq.1
                                                                                              eq
                                                                                              new-ident.1
                                                                                              new-ident
                                                                                              $1
                                                                                              $2
                                                                                              $3
                                                                                              $4))
                                                                                            (list* 'type-def $0)))
                                                                                          (t
                                                                                           (com.informatimago.rdp::error-unexpected-token
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner
                                                                                            '(identifier)
                                                                                            '(-->
                                                                                              type-def
                                                                                              (alt
                                                                                               (seq
                                                                                                (new-ident
                                                                                                 eq
                                                                                                 type
                                                                                                 semi)
                                                                                                ((list*
                                                                                                  'type-def
                                                                                                  $0)))))))))))
                                                                                      (progn
                                                                                       (fmakunbound
                                                                                        'pascal/parse-unsigned-int)
                                                                                       (defun
                                                                                        pascal/parse-unsigned-int
                                                                                        (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        "(--> unsigned-int (seq (integer) ((parse-integer (second integer)))))"
                                                                                        (com.informatimago.rdp::with-non-terminal
                                                                                         (unsigned-int
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         (cond
                                                                                          ((word-equal
                                                                                            (scanner-current-token
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            'integer)
                                                                                           (let*
                                                                                            (($1
                                                                                              (accept
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               'integer))
                                                                                             (integer $1)
                                                                                             (integer.1 $1)
                                                                                             ($0 (list $1)))
                                                                                            (declare
                                                                                             (ignorable
                                                                                              $0
                                                                                              integer.1
                                                                                              integer
                                                                                              $1))
                                                                                            (parse-integer
                                                                                             (second integer))))
                                                                                          (t
                                                                                           (com.informatimago.rdp::error-unexpected-token
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner
                                                                                            '(integer)
                                                                                            '(-->
                                                                                              unsigned-int
                                                                                              (alt
                                                                                               (seq
                                                                                                (integer)
                                                                                                ((parse-integer
                                                                                                  (second
                                                                                                   integer))))))))))))
                                                                                      (progn
                                                                                       (fmakunbound
                                                                                        'pascal/parse-constant-1-1-1)
                                                                                       (defun
                                                                                        pascal/parse-constant-1-1-1
                                                                                        (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        "(--> constant-1-1-1 (alt ((seq (plus) ($1)) (seq (minus) ($1)))))"
                                                                                        (com.informatimago.rdp::with-non-terminal
                                                                                         (constant-1-1-1
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         (cond
                                                                                          ((word-equal
                                                                                            (scanner-current-token
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            'plus)
                                                                                           (let*
                                                                                            (($1
                                                                                              (accept
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               'plus))
                                                                                             (plus $1)
                                                                                             (plus.1 $1)
                                                                                             ($0 (list $1)))
                                                                                            (declare
                                                                                             (ignorable
                                                                                              $0
                                                                                              plus.1
                                                                                              plus
                                                                                              $1))
                                                                                            $1))
                                                                                          ((word-equal
                                                                                            (scanner-current-token
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            'minus)
                                                                                           (let*
                                                                                            (($1
                                                                                              (accept
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               'minus))
                                                                                             (minus $1)
                                                                                             (minus.1 $1)
                                                                                             ($0 (list $1)))
                                                                                            (declare
                                                                                             (ignorable
                                                                                              $0
                                                                                              minus.1
                                                                                              minus
                                                                                              $1))
                                                                                            $1))
                                                                                          (t
                                                                                           (com.informatimago.rdp::error-unexpected-token
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner
                                                                                            '(plus minus)
                                                                                            '(-->
                                                                                              constant-1-1-1
                                                                                              (alt
                                                                                               (seq (plus) ($1))
                                                                                               (seq
                                                                                                (minus)
                                                                                                ($1))))))))))
                                                                                      (progn
                                                                                       (fmakunbound
                                                                                        'pascal/parse-const-definitions-1-1)
                                                                                       (defun
                                                                                        pascal/parse-const-definitions-1-1
                                                                                        (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                        "(--> const-definitions-1-1 (alt ((seq nil ('nil)) (seq (const-def const-definitions-1-1) ((cons (progn $0) $2))))))"
                                                                                        (com.informatimago.rdp::with-non-terminal
                                                                                         (const-definitions-1-1
                                                                                          com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         (loop :with com.informatimago.rdp::$items
                                                                                               :=
                                                                                               'nil
                                                                                          :do (cond
                                                                                               ((word-equal
                                                                                                 (scanner-current-token
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                 'identifier)
                                                                                                (push
                                                                                                 (let*
                                                                                                  (($1
                                                                                                    (pascal/parse-const-def
                                                                                                     com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                   (const-def $1)
                                                                                                   (const-def.1 $1)
                                                                                                   ($2
                                                                                                    (let*
                                                                                                     (($0 (list)))
                                                                                                     (declare
                                                                                                      (ignorable $0))
                                                                                                     'nil))
                                                                                                   ($0 (list $1 $2)))
                                                                                                  (declare
                                                                                                   (ignorable
                                                                                                    $0
                                                                                                    const-def.1
                                                                                                    const-def
                                                                                                    $1
                                                                                                    $2))
                                                                                                  (cons (progn $0) $2))
                                                                                                 com.informatimago.rdp::$items))
                                                                                               (t (loop-finish)))
                                                                                          :finally (return
                                                                                                    (reduce
                                                                                                     #'append
                                                                                                     (nreverse
                                                                                                      com.informatimago.rdp::$items)
                                                                                                     :initial-value
                                                                                                     nil))))))
                                                                                       (progn
                                                                                        (fmakunbound
                                                                                         'pascal/parse-const-def)
                                                                                        (defun
                                                                                         pascal/parse-const-def
                                                                                         (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         "(--> const-def (seq (new-ident eq constant semi) ((list* 'const-def $0))))"
                                                                                         (com.informatimago.rdp::with-non-terminal
                                                                                          (const-def
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          (cond
                                                                                           ((word-equal
                                                                                             (scanner-current-token
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             'identifier)
                                                                                            (let*
                                                                                             (($1
                                                                                               (pascal/parse-new-ident
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                              (new-ident $1)
                                                                                              (new-ident.1 $1)
                                                                                              ($2
                                                                                               (accept
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                'eq))
                                                                                              (eq $2)
                                                                                              (eq.1 $2)
                                                                                              ($3
                                                                                               (pascal/parse-constant
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                              (constant $3)
                                                                                              (constant.1 $3)
                                                                                              ($4
                                                                                               (accept
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                'semi))
                                                                                              (semi $4)
                                                                                              (semi.1 $4)
                                                                                              ($0 (list $1 $2 $3 $4)))
                                                                                             (declare
                                                                                              (ignorable
                                                                                               $0
                                                                                               semi.1
                                                                                               semi
                                                                                               constant.1
                                                                                               constant
                                                                                               eq.1
                                                                                               eq
                                                                                               new-ident.1
                                                                                               new-ident
                                                                                               $1
                                                                                               $2
                                                                                               $3
                                                                                               $4))
                                                                                             (list* 'const-def $0)))
                                                                                           (t
                                                                                            (com.informatimago.rdp::error-unexpected-token
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             '(identifier)
                                                                                             '(-->
                                                                                               const-def
                                                                                               (alt
                                                                                                (seq
                                                                                                 (new-ident
                                                                                                  eq
                                                                                                  constant
                                                                                                  semi)
                                                                                                 ((list*
                                                                                                   'const-def
                                                                                                   $0)))))))))))
                                                                                       (progn
                                                                                        (fmakunbound
                                                                                         'pascal/parse-a-label)
                                                                                        (defun
                                                                                         pascal/parse-a-label
                                                                                         (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         "(--> a-label (seq (unsigned-int) ((list* 'a-label $0))))"
                                                                                         (com.informatimago.rdp::with-non-terminal
                                                                                          (a-label
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          (cond
                                                                                           ((word-equal
                                                                                             (scanner-current-token
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             'integer)
                                                                                            (let*
                                                                                             (($1
                                                                                               (pascal/parse-unsigned-int
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                              (unsigned-int $1)
                                                                                              (unsigned-int.1 $1)
                                                                                              ($0 (list $1)))
                                                                                             (declare
                                                                                              (ignorable
                                                                                               $0
                                                                                               unsigned-int.1
                                                                                               unsigned-int
                                                                                               $1))
                                                                                             (list* 'a-label $0)))
                                                                                           (t
                                                                                            (com.informatimago.rdp::error-unexpected-token
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner
                                                                                             '(integer)
                                                                                             '(-->
                                                                                               a-label
                                                                                               (alt
                                                                                                (seq
                                                                                                 (unsigned-int)
                                                                                                 ((list*
                                                                                                   'a-label
                                                                                                   $0)))))))))))
                                                                                       (progn
                                                                                        (fmakunbound
                                                                                         'pascal/parse-labels-1)
                                                                                        (defun
                                                                                         pascal/parse-labels-1
                                                                                         (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                         "(--> labels-1 (alt ((seq nil ('nil)) (seq (labels-1-1 labels-1) ((cons $1 $2))))))"
                                                                                         (com.informatimago.rdp::with-non-terminal
                                                                                          (labels-1
                                                                                           com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          (loop :with com.informatimago.rdp::$items
                                                                                                :=
                                                                                                'nil
                                                                                           :do (cond
                                                                                                ((word-equal
                                                                                                  (scanner-current-token
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                  'comma)
                                                                                                 (push
                                                                                                  (let*
                                                                                                   (($1
                                                                                                     (pascal/parse-labels-1-1
                                                                                                      com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                    ($2
                                                                                                     (let*
                                                                                                      (($0 (list)))
                                                                                                      (declare
                                                                                                       (ignorable $0))
                                                                                                      'nil))
                                                                                                    ($0 (list $1 $2)))
                                                                                                   (declare
                                                                                                    (ignorable
                                                                                                     $0
                                                                                                     $1
                                                                                                     $2))
                                                                                                   (cons $1 $2))
                                                                                                  com.informatimago.rdp::$items))
                                                                                                (t (loop-finish)))
                                                                                           :finally (return
                                                                                                     (reduce
                                                                                                      #'append
                                                                                                      (nreverse
                                                                                                       com.informatimago.rdp::$items)
                                                                                                      :initial-value
                                                                                                      nil))))))
                                                                                        (progn
                                                                                         (fmakunbound
                                                                                          'pascal/parse-labels-1-1)
                                                                                         (defun
                                                                                          pascal/parse-labels-1-1
                                                                                          (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          "(--> labels-1-1 (seq (comma a-label) ($0)))"
                                                                                          (com.informatimago.rdp::with-non-terminal
                                                                                           (labels-1-1
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           (cond
                                                                                            ((word-equal
                                                                                              (scanner-current-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                              'comma)
                                                                                             (let*
                                                                                              (($1
                                                                                                (accept
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                 'comma))
                                                                                               (comma $1)
                                                                                               (comma.1 $1)
                                                                                               ($2
                                                                                                (pascal/parse-a-label
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                               (a-label $2)
                                                                                               (a-label.1 $2)
                                                                                               ($0 (list $1 $2)))
                                                                                              (declare
                                                                                               (ignorable
                                                                                                $0
                                                                                                a-label.1
                                                                                                a-label
                                                                                                comma.1
                                                                                                comma
                                                                                                $1
                                                                                                $2))
                                                                                              $0))
                                                                                            (t
                                                                                             (com.informatimago.rdp::error-unexpected-token
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner
                                                                                              '(comma)
                                                                                              '(-->
                                                                                                labels-1-1
                                                                                                (alt
                                                                                                 (seq
                                                                                                  (comma a-label)
                                                                                                  ($0))))))))))
                                                                                        (progn
                                                                                         (fmakunbound
                                                                                          'pascal/parse-declaration-part-1)
                                                                                         (defun
                                                                                          pascal/parse-declaration-part-1
                                                                                          (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                          "(--> declaration-part-1 (alt ((seq nil ('nil)) (seq (proc-declarations declaration-part-1) ((cons (progn $0) $2))))))"
                                                                                          (com.informatimago.rdp::with-non-terminal
                                                                                           (declaration-part-1
                                                                                            com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           (loop :with com.informatimago.rdp::$items
                                                                                                 :=
                                                                                                 'nil
                                                                                            :do (cond
                                                                                                 ((member
                                                                                                   (scanner-current-token
                                                                                                    com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                   '(procedure
                                                                                                     function)
                                                                                                   :test
                                                                                                   #'word-equal)
                                                                                                  (push
                                                                                                   (let*
                                                                                                    (($1
                                                                                                      (pascal/parse-proc-declarations
                                                                                                       com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                     (proc-declarations
                                                                                                      $1)
                                                                                                     (proc-declarations.1
                                                                                                      $1)
                                                                                                     ($2
                                                                                                      (let*
                                                                                                       (($0 (list)))
                                                                                                       (declare
                                                                                                        (ignorable $0))
                                                                                                       'nil))
                                                                                                     ($0 (list $1 $2)))
                                                                                                    (declare
                                                                                                     (ignorable
                                                                                                      $0
                                                                                                      proc-declarations.1
                                                                                                      proc-declarations
                                                                                                      $1
                                                                                                      $2))
                                                                                                    (cons
                                                                                                     (progn $0)
                                                                                                     $2))
                                                                                                   com.informatimago.rdp::$items))
                                                                                                 (t (loop-finish)))
                                                                                            :finally (return
                                                                                                      (reduce
                                                                                                       #'append
                                                                                                       (nreverse
                                                                                                        com.informatimago.rdp::$items)
                                                                                                       :initial-value
                                                                                                       nil))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-proc-declarations)
                                                                                          (defun
                                                                                           pascal/parse-proc-declarations
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> proc-declarations (seq (proc-declarations-1 semi body semi) ((list* 'proc-declarations $0))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (proc-declarations
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((member
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               '#'procedure
                                                                                               :test
                                                                                               #'word-equal)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-proc-declarations-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($2
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'semi))
                                                                                                (semi $2)
                                                                                                (semi.1 $2)
                                                                                                ($3
                                                                                                 (pascal/parse-body
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (body $3)
                                                                                                (body.1 $3)
                                                                                                ($4
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'semi))
                                                                                                (semi.2 $4)
                                                                                                ($0
                                                                                                 (list $1 $2 $3 $4)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 semi.2
                                                                                                 body.1
                                                                                                 body
                                                                                                 semi.1
                                                                                                 semi
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3
                                                                                                 $4))
                                                                                               (list*
                                                                                                'proc-declarations
                                                                                                $0)))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '#'procedure
                                                                                               '(-->
                                                                                                 proc-declarations
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (proc-declarations-1
                                                                                                    semi
                                                                                                    body
                                                                                                    semi)
                                                                                                   ((list*
                                                                                                     'proc-declarations
                                                                                                     $0)))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-var-declarations)
                                                                                          (defun
                                                                                           pascal/parse-var-declarations
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> var-declarations (alt ((seq (var var-decl var-declarations-1-1) ($0)) (seq nil ('nil)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (var-declarations
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'var)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'var))
                                                                                                (var $1)
                                                                                                (var.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-var-decl
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (var-decl $2)
                                                                                                (var-decl.1 $2)
                                                                                                ($3
                                                                                                 (pascal/parse-var-declarations-1-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($0 (list $1 $2 $3)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 var-decl.1
                                                                                                 var-decl
                                                                                                 var.1
                                                                                                 var
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3))
                                                                                               $0))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-type-definitions)
                                                                                          (defun
                                                                                           pascal/parse-type-definitions
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> type-definitions (alt ((seq (type type-def type-definitions-1-1) ($0)) (seq nil ('nil)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (type-definitions
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'type)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'type))
                                                                                                (type $1)
                                                                                                (type.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-type-def
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (type-def $2)
                                                                                                (type-def.1 $2)
                                                                                                ($3
                                                                                                 (pascal/parse-type-definitions-1-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($0 (list $1 $2 $3)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 type-def.1
                                                                                                 type-def
                                                                                                 type.1
                                                                                                 type
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3))
                                                                                               $0))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-const-definitions)
                                                                                          (defun
                                                                                           pascal/parse-const-definitions
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> const-definitions (alt ((seq (const const-def const-definitions-1-1) ($0)) (seq nil ('nil)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (const-definitions
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'const)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'const))
                                                                                                (const $1)
                                                                                                (const.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-const-def
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (const-def $2)
                                                                                                (const-def.1 $2)
                                                                                                ($3
                                                                                                 (pascal/parse-const-definitions-1-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($0 (list $1 $2 $3)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 const-def.1
                                                                                                 const-def
                                                                                                 const.1
                                                                                                 const
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3))
                                                                                               $0))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-label-declarations)
                                                                                          (defun
                                                                                           pascal/parse-label-declarations
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> label-declarations (alt ((seq (label labels semi) ($0)) (seq nil ('nil)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (label-declarations
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'label)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'label))
                                                                                                (label $1)
                                                                                                (label.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-labels
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (labels $2)
                                                                                                (labels.1 $2)
                                                                                                ($3
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'semi))
                                                                                                (semi $3)
                                                                                                (semi.1 $3)
                                                                                                ($0 (list $1 $2 $3)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 semi.1
                                                                                                 semi
                                                                                                 labels.1
                                                                                                 labels
                                                                                                 label.1
                                                                                                 label
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3))
                                                                                               $0))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-statement-part)
                                                                                          (defun
                                                                                           pascal/parse-statement-part
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> statement-part (seq (compound-statement) ((list* 'statement-part $0))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (statement-part
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'begin)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-compound-statement
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (compound-statement $1)
                                                                                                (compound-statement.1
                                                                                                 $1)
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 compound-statement.1
                                                                                                 compound-statement
                                                                                                 $1))
                                                                                               (list*
                                                                                                'statement-part
                                                                                                $0)))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(begin)
                                                                                               '(-->
                                                                                                 statement-part
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (compound-statement)
                                                                                                   ((list*
                                                                                                     'statement-part
                                                                                                     $0)))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-declaration-part)
                                                                                          (defun
                                                                                           pascal/parse-declaration-part
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> declaration-part (seq (label-declarations const-definitions type-definitions var-declarations declaration-part-1) ((list* 'declaration-part $0))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (declaration-part
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((member
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               '(label
                                                                                                 nil
                                                                                                 const
                                                                                                 type
                                                                                                 var
                                                                                                 procedure
                                                                                                 function
                                                                                                 begin)
                                                                                               :test
                                                                                               #'word-equal)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-label-declarations
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (label-declarations $1)
                                                                                                (label-declarations.1
                                                                                                 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-const-definitions
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (const-definitions $2)
                                                                                                (const-definitions.1
                                                                                                 $2)
                                                                                                ($3
                                                                                                 (pascal/parse-type-definitions
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (type-definitions $3)
                                                                                                (type-definitions.1 $3)
                                                                                                ($4
                                                                                                 (pascal/parse-var-declarations
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (var-declarations $4)
                                                                                                (var-declarations.1 $4)
                                                                                                ($5
                                                                                                 (pascal/parse-declaration-part-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($0
                                                                                                 (list
                                                                                                  $1
                                                                                                  $2
                                                                                                  $3
                                                                                                  $4
                                                                                                  $5)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 var-declarations.1
                                                                                                 var-declarations
                                                                                                 type-definitions.1
                                                                                                 type-definitions
                                                                                                 const-definitions.1
                                                                                                 const-definitions
                                                                                                 label-declarations.1
                                                                                                 label-declarations
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3
                                                                                                 $4
                                                                                                 $5))
                                                                                               (list*
                                                                                                'declaration-part
                                                                                                $0)))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(label
                                                                                                 nil
                                                                                                 const
                                                                                                 type
                                                                                                 var
                                                                                                 procedure
                                                                                                 function
                                                                                                 begin)
                                                                                               '(-->
                                                                                                 declaration-part
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (label-declarations
                                                                                                    const-definitions
                                                                                                    type-definitions
                                                                                                    var-declarations
                                                                                                    declaration-part-1)
                                                                                                   ((list*
                                                                                                     'declaration-part
                                                                                                     $0)))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-external-files)
                                                                                          (defun
                                                                                           pascal/parse-external-files
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> external-files (seq (opar new-ident-list clop) ((list* 'external-files $0))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (external-files
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'opar)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'opar))
                                                                                                (opar $1)
                                                                                                (opar.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-new-ident-list
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (new-ident-list $2)
                                                                                                (new-ident-list.1 $2)
                                                                                                ($3
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'clop))
                                                                                                (clop $3)
                                                                                                (clop.1 $3)
                                                                                                ($0 (list $1 $2 $3)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 clop.1
                                                                                                 clop
                                                                                                 new-ident-list.1
                                                                                                 new-ident-list
                                                                                                 opar.1
                                                                                                 opar
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3))
                                                                                               (list*
                                                                                                'external-files
                                                                                                $0)))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(opar)
                                                                                               '(-->
                                                                                                 external-files
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (opar
                                                                                                    new-ident-list
                                                                                                    clop)
                                                                                                   ((list*
                                                                                                     'external-files
                                                                                                     $0)))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-pascal-program-1)
                                                                                          (defun
                                                                                           pascal/parse-pascal-program-1
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> pascal-program-1 (alt ((seq nil ('nil)) (seq (external-files) ($0)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (pascal-program-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'opar)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-external-files
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (external-files $1)
                                                                                                (external-files.1 $1)
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 external-files.1
                                                                                                 external-files
                                                                                                 $1))
                                                                                               $0))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-labels)
                                                                                          (defun
                                                                                           pascal/parse-labels
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> labels (seq (a-label labels-1) ((list* 'labels $0))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (labels
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'integer)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-a-label
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (a-label $1)
                                                                                                (a-label.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-labels-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($0 (list $1 $2)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 a-label.1
                                                                                                 a-label
                                                                                                 $1
                                                                                                 $2))
                                                                                               (list* 'labels $0)))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(integer)
                                                                                               '(-->
                                                                                                 labels
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (a-label labels-1)
                                                                                                   ((list*
                                                                                                     'labels
                                                                                                     $0)))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-constant-1-2)
                                                                                          (defun
                                                                                           pascal/parse-constant-1-2
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> constant-1-2 (alt ((seq (unsigned-number) ($1)) (seq (const-ident) ($1)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (constant-1-2
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((member
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               '(integer real)
                                                                                               :test
                                                                                               #'word-equal)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-unsigned-number
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (unsigned-number $1)
                                                                                                (unsigned-number.1 $1)
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 unsigned-number.1
                                                                                                 unsigned-number
                                                                                                 $1))
                                                                                               $1))
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'identifier)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-const-ident
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (const-ident $1)
                                                                                                (const-ident.1 $1)
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 const-ident.1
                                                                                                 const-ident
                                                                                                 $1))
                                                                                               $1))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(integer
                                                                                                 real
                                                                                                 identifier)
                                                                                               '(-->
                                                                                                 constant-1-2
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (unsigned-number)
                                                                                                   ($1))
                                                                                                  (seq
                                                                                                   (const-ident)
                                                                                                   ($1))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-constant-1-1)
                                                                                          (defun
                                                                                           pascal/parse-constant-1-1
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> constant-1-1 (alt ((seq nil ('nil)) (seq (constant-1-1-1) ($0)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (constant-1-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((member
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               '(minus plus)
                                                                                               :test
                                                                                               #'word-equal)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-constant-1-1-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable $0 $1))
                                                                                               $0))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-struct-type)
                                                                                          (defun
                                                                                           pascal/parse-struct-type
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> struct-type (alt ((seq (record field-list end) ((list* 'record-type $0))) (seq (set of simple-type) ((list* 'set-type $0))) (seq (file of type) ((list* 'file-type $0))) (seq (array obra index-list clob of type) ((list* 'array-type $0))))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (struct-type
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'record)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'record))
                                                                                                (record $1)
                                                                                                (record.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-field-list
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (field-list $2)
                                                                                                (field-list.1 $2)
                                                                                                ($3
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'end))
                                                                                                (end $3)
                                                                                                (end.1 $3)
                                                                                                ($0 (list $1 $2 $3)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 end.1
                                                                                                 end
                                                                                                 field-list.1
                                                                                                 field-list
                                                                                                 record.1
                                                                                                 record
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3))
                                                                                               (list*
                                                                                                'record-type
                                                                                                $0)))
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'set)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'set))
                                                                                                (set $1)
                                                                                                (set.1 $1)
                                                                                                ($2
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'of))
                                                                                                (of $2)
                                                                                                (of.1 $2)
                                                                                                ($3
                                                                                                 (pascal/parse-simple-type
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (simple-type $3)
                                                                                                (simple-type.1 $3)
                                                                                                ($0 (list $1 $2 $3)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 simple-type.1
                                                                                                 simple-type
                                                                                                 of.1
                                                                                                 of
                                                                                                 set.1
                                                                                                 set
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3))
                                                                                               (list* 'set-type $0)))
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'file)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'file))
                                                                                                (file $1)
                                                                                                (file.1 $1)
                                                                                                ($2
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'of))
                                                                                                (of $2)
                                                                                                (of.1 $2)
                                                                                                ($3
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'type))
                                                                                                (type $3)
                                                                                                (type.1 $3)
                                                                                                ($0 (list $1 $2 $3)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 type.1
                                                                                                 type
                                                                                                 of.1
                                                                                                 of
                                                                                                 file.1
                                                                                                 file
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3))
                                                                                               (list* 'file-type $0)))
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'array)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'array))
                                                                                                (array $1)
                                                                                                (array.1 $1)
                                                                                                ($2
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'obra))
                                                                                                (obra $2)
                                                                                                (obra.1 $2)
                                                                                                ($3
                                                                                                 (pascal/parse-index-list
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (index-list $3)
                                                                                                (index-list.1 $3)
                                                                                                ($4
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'clob))
                                                                                                (clob $4)
                                                                                                (clob.1 $4)
                                                                                                ($5
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'of))
                                                                                                (of $5)
                                                                                                (of.1 $5)
                                                                                                ($6
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'type))
                                                                                                (type $6)
                                                                                                (type.1 $6)
                                                                                                ($0
                                                                                                 (list
                                                                                                  $1
                                                                                                  $2
                                                                                                  $3
                                                                                                  $4
                                                                                                  $5
                                                                                                  $6)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 type.1
                                                                                                 type
                                                                                                 of.1
                                                                                                 of
                                                                                                 clob.1
                                                                                                 clob
                                                                                                 index-list.1
                                                                                                 index-list
                                                                                                 obra.1
                                                                                                 obra
                                                                                                 array.1
                                                                                                 array
                                                                                                 $1
                                                                                                 $2
                                                                                                 $3
                                                                                                 $4
                                                                                                 $5
                                                                                                 $6))
                                                                                               (list* 'array-type $0)))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(record set file array)
                                                                                               '(-->
                                                                                                 struct-type
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (record
                                                                                                    field-list
                                                                                                    end)
                                                                                                   ((list*
                                                                                                     'record-type
                                                                                                     $0)))
                                                                                                  (seq
                                                                                                   (set of simple-type)
                                                                                                   ((list*
                                                                                                     'set-type
                                                                                                     $0)))
                                                                                                  (seq
                                                                                                   (file of type)
                                                                                                   ((list*
                                                                                                     'file-type
                                                                                                     $0)))
                                                                                                  (seq
                                                                                                   (array
                                                                                                    obra
                                                                                                    index-list
                                                                                                    clob
                                                                                                    of
                                                                                                    type)
                                                                                                   ((list*
                                                                                                     'array-type
                                                                                                     $0)))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-type-1-1)
                                                                                          (defun
                                                                                           pascal/parse-type-1-1
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> type-1-1 (alt ((seq nil ('nil)) (seq (packed) ($0)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (type-1-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'packed)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'packed))
                                                                                                (packed $1)
                                                                                                (packed.1 $1)
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 packed.1
                                                                                                 packed
                                                                                                 $1))
                                                                                               $0))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-simple-type-1-1)
                                                                                          (defun
                                                                                           pascal/parse-simple-type-1-1
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> simple-type-1-1 (alt ((seq (dotdot constant) ($0)) (seq nil ('nil)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (simple-type-1-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'dotdot)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'dotdot))
                                                                                                (dotdot $1)
                                                                                                (dotdot.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-constant
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (constant $2)
                                                                                                (constant.1 $2)
                                                                                                ($0 (list $1 $2)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 constant.1
                                                                                                 constant
                                                                                                 dotdot.1
                                                                                                 dotdot
                                                                                                 $1
                                                                                                 $2))
                                                                                               $0))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-simple-type-2-2)
                                                                                          (defun
                                                                                           pascal/parse-simple-type-2-2
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> simple-type-2-2 (alt ((seq (unsigned-number) ($1)) (seq (const-ident) ($1)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (simple-type-2-2
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((member
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               '(integer real)
                                                                                               :test
                                                                                               #'word-equal)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-unsigned-number
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (unsigned-number $1)
                                                                                                (unsigned-number.1 $1)
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 unsigned-number.1
                                                                                                 unsigned-number
                                                                                                 $1))
                                                                                               $1))
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'identifier)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-const-ident
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (const-ident $1)
                                                                                                (const-ident.1 $1)
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 const-ident.1
                                                                                                 const-ident
                                                                                                 $1))
                                                                                               $1))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(integer
                                                                                                 real
                                                                                                 identifier)
                                                                                               '(-->
                                                                                                 simple-type-2-2
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (unsigned-number)
                                                                                                   ($1))
                                                                                                  (seq
                                                                                                   (const-ident)
                                                                                                   ($1))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-simple-type-2-1)
                                                                                          (defun
                                                                                           pascal/parse-simple-type-2-1
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> simple-type-2-1 (alt ((seq (plus) ($1)) (seq (minus) ($1)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (simple-type-2-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'plus)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'plus))
                                                                                                (plus $1)
                                                                                                (plus.1 $1)
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 plus.1
                                                                                                 plus
                                                                                                 $1))
                                                                                               $1))
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'minus)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'minus))
                                                                                                (minus $1)
                                                                                                (minus.1 $1)
                                                                                                ($0 (list $1)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 minus.1
                                                                                                 minus
                                                                                                 $1))
                                                                                               $1))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(plus minus)
                                                                                               '(-->
                                                                                                 simple-type-2-1
                                                                                                 (alt
                                                                                                  (seq (plus) ($1))
                                                                                                  (seq
                                                                                                   (minus)
                                                                                                   ($1))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-index-list)
                                                                                          (defun
                                                                                           pascal/parse-index-list
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> index-list (seq (simple-type index-list-1) ((list* 'index-list $0))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (index-list
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((member
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               '(identifier
                                                                                                 opar
                                                                                                 real
                                                                                                 integer
                                                                                                 plus
                                                                                                 minus)
                                                                                               :test
                                                                                               #'word-equal)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-simple-type
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (simple-type $1)
                                                                                                (simple-type.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-index-list-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($0 (list $1 $2)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 simple-type.1
                                                                                                 simple-type
                                                                                                 $1
                                                                                                 $2))
                                                                                               (list* 'index-list $0)))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(identifier
                                                                                                 opar
                                                                                                 real
                                                                                                 integer
                                                                                                 plus
                                                                                                 minus)
                                                                                               '(-->
                                                                                                 index-list
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (simple-type
                                                                                                    index-list-1)
                                                                                                   ((list*
                                                                                                     'index-list
                                                                                                     $0)))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-field-list-1-1-1-1)
                                                                                          (defun
                                                                                           pascal/parse-field-list-1-1-1-1
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> field-list-1-1-1-1 (alt ((seq (semi variant-part) ($0)) (seq nil ('nil)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (field-list-1-1-1-1
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'semi)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (accept
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                  'semi))
                                                                                                (semi $1)
                                                                                                (semi.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-variant-part
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (variant-part $2)
                                                                                                (variant-part.1 $2)
                                                                                                ($0 (list $1 $2)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 variant-part.1
                                                                                                 variant-part
                                                                                                 semi.1
                                                                                                 semi
                                                                                                 $1
                                                                                                 $2))
                                                                                               $0))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-fixed-part)
                                                                                          (defun
                                                                                           pascal/parse-fixed-part
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> fixed-part (seq (record-section fixed-part-1) ((list* 'fixed-part $0))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (fixed-part
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (cond
                                                                                             ((word-equal
                                                                                               (scanner-current-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               'identifier)
                                                                                              (let*
                                                                                               (($1
                                                                                                 (pascal/parse-record-section
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                (record-section $1)
                                                                                                (record-section.1 $1)
                                                                                                ($2
                                                                                                 (pascal/parse-fixed-part-1
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                ($0 (list $1 $2)))
                                                                                               (declare
                                                                                                (ignorable
                                                                                                 $0
                                                                                                 record-section.1
                                                                                                 record-section
                                                                                                 $1
                                                                                                 $2))
                                                                                               (list* 'fixed-part $0)))
                                                                                             (t
                                                                                              (com.informatimago.rdp::error-unexpected-token
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               '(identifier)
                                                                                               '(-->
                                                                                                 fixed-part
                                                                                                 (alt
                                                                                                  (seq
                                                                                                   (record-section
                                                                                                    fixed-part-1)
                                                                                                   ((list*
                                                                                                     'fixed-part
                                                                                                     $0)))))))))))
                                                                                         (progn
                                                                                          (fmakunbound
                                                                                           'pascal/parse-param-type)
                                                                                          (defun
                                                                                           pascal/parse-param-type
                                                                                           (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                           "(--> param-type (alt ((seq (array obra index-spec-list clob of param-type) ($0)) (seq (packed array obra index-spec clob of type-ident) ($0)) (seq (type-ident) ($1)))))"
                                                                                           (com.informatimago.rdp::with-non-terminal
                                                                                            (param-type
                                                                                             com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            (loop :with com.informatimago.rdp::$items
                                                                                                  :=
                                                                                                  'nil
                                                                                             :do (cond
                                                                                                  ((word-equal
                                                                                                    (scanner-current-token
                                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                    'array)
                                                                                                   (push
                                                                                                    (let*
                                                                                                     (($1
                                                                                                       (accept
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                        'array))
                                                                                                      (array $1)
                                                                                                      (array.1 $1)
                                                                                                      ($2
                                                                                                       (accept
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                        'obra))
                                                                                                      (obra $2)
                                                                                                      (obra.1 $2)
                                                                                                      ($3
                                                                                                       (pascal/parse-index-spec-list
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                      (index-spec-list
                                                                                                       $3)
                                                                                                      (index-spec-list.1
                                                                                                       $3)
                                                                                                      ($4
                                                                                                       (accept
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                        'clob))
                                                                                                      (clob $4)
                                                                                                      (clob.1 $4)
                                                                                                      ($5
                                                                                                       (accept
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                        'of))
                                                                                                      (of $5)
                                                                                                      (of.1 $5)
                                                                                                      ($6
                                                                                                       (let*
                                                                                                        (($0 (list)))
                                                                                                        (declare
                                                                                                         (ignorable
                                                                                                          $0))
                                                                                                        'nil))
                                                                                                      ($0
                                                                                                       (list
                                                                                                        $1
                                                                                                        $2
                                                                                                        $3
                                                                                                        $4
                                                                                                        $5
                                                                                                        $6)))
                                                                                                     (declare
                                                                                                      (ignorable
                                                                                                       $0
                                                                                                       of.1
                                                                                                       of
                                                                                                       clob.1
                                                                                                       clob
                                                                                                       index-spec-list.1
                                                                                                       index-spec-list
                                                                                                       obra.1
                                                                                                       obra
                                                                                                       array.1
                                                                                                       array
                                                                                                       $1
                                                                                                       $2
                                                                                                       $3
                                                                                                       $4
                                                                                                       $5
                                                                                                       $6))
                                                                                                     $0)
                                                                                                    com.informatimago.rdp::$items))
                                                                                                  ((word-equal
                                                                                                    (scanner-current-token
                                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                    'packed)
                                                                                                   (push
                                                                                                    (let*
                                                                                                     (($1
                                                                                                       (accept
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                        'packed))
                                                                                                      (packed $1)
                                                                                                      (packed.1 $1)
                                                                                                      ($2
                                                                                                       (accept
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                        'array))
                                                                                                      (array $2)
                                                                                                      (array.1 $2)
                                                                                                      ($3
                                                                                                       (accept
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                        'obra))
                                                                                                      (obra $3)
                                                                                                      (obra.1 $3)
                                                                                                      ($4
                                                                                                       (pascal/parse-index-spec
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                      (index-spec $4)
                                                                                                      (index-spec.1 $4)
                                                                                                      ($5
                                                                                                       (accept
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                        'clob))
                                                                                                      (clob $5)
                                                                                                      (clob.1 $5)
                                                                                                      ($6
                                                                                                       (accept
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                        'of))
                                                                                                      (of $6)
                                                                                                      (of.1 $6)
                                                                                                      ($7
                                                                                                       (pascal/parse-type-ident
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                      (type-ident $7)
                                                                                                      (type-ident.1 $7)
                                                                                                      ($0
                                                                                                       (list
                                                                                                        $1
                                                                                                        $2
                                                                                                        $3
                                                                                                        $4
                                                                                                        $5
                                                                                                        $6
                                                                                                        $7)))
                                                                                                     (declare
                                                                                                      (ignorable
                                                                                                       $0
                                                                                                       type-ident.1
                                                                                                       type-ident
                                                                                                       of.1
                                                                                                       of
                                                                                                       clob.1
                                                                                                       clob
                                                                                                       index-spec.1
                                                                                                       index-spec
                                                                                                       obra.1
                                                                                                       obra
                                                                                                       array.1
                                                                                                       array
                                                                                                       packed.1
                                                                                                       packed
                                                                                                       $1
                                                                                                       $2
                                                                                                       $3
                                                                                                       $4
                                                                                                       $5
                                                                                                       $6
                                                                                                       $7))
                                                                                                     $0)
                                                                                                    com.informatimago.rdp::$items)
                                                                                                   (loop-finish))
                                                                                                  ((word-equal
                                                                                                    (scanner-current-token
                                                                                                     com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                    'identifier)
                                                                                                   (push
                                                                                                    (let*
                                                                                                     (($1
                                                                                                       (pascal/parse-type-ident
                                                                                                        com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                      (type-ident $1)
                                                                                                      (type-ident.1 $1)
                                                                                                      ($0 (list $1)))
                                                                                                     (declare
                                                                                                      (ignorable
                                                                                                       $0
                                                                                                       type-ident.1
                                                                                                       type-ident
                                                                                                       $1))
                                                                                                     $1)
                                                                                                    com.informatimago.rdp::$items)
                                                                                                   (loop-finish))
                                                                                                  (t
                                                                                                   (com.informatimago.rdp::error-unexpected-token
                                                                                                    com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                    '(packed
                                                                                                      identifier
                                                                                                      array)
                                                                                                    '(-->
                                                                                                      param-type
                                                                                                      (alt
                                                                                                       (seq
                                                                                                        (array
                                                                                                         obra
                                                                                                         index-spec-list
                                                                                                         clob
                                                                                                         of
                                                                                                         param-type)
                                                                                                        ($0))
                                                                                                       (seq
                                                                                                        (packed
                                                                                                         array
                                                                                                         obra
                                                                                                         index-spec
                                                                                                         clob
                                                                                                         of
                                                                                                         type-ident)
                                                                                                        ($0))
                                                                                                       (seq
                                                                                                        (type-ident)
                                                                                                        ($1)))))))
                                                                                             :finally (return
                                                                                                       (reduce
                                                                                                        #'append
                                                                                                        (nreverse
                                                                                                         com.informatimago.rdp::$items)
                                                                                                        :initial-value
                                                                                                        nil))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-index-spec-list)
                                                                                           (defun
                                                                                            pascal/parse-index-spec-list
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> index-spec-list (seq (index-spec index-spec-list-1) ((list* 'index-spec-list $0))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (index-spec-list
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'identifier)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-index-spec
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (index-spec $1)
                                                                                                 (index-spec.1 $1)
                                                                                                 ($2
                                                                                                  (pascal/parse-index-spec-list-1
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 ($0 (list $1 $2)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  index-spec.1
                                                                                                  index-spec
                                                                                                  $1
                                                                                                  $2))
                                                                                                (list*
                                                                                                 'index-spec-list
                                                                                                 $0)))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(identifier)
                                                                                                '(-->
                                                                                                  index-spec-list
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (index-spec
                                                                                                     index-spec-list-1)
                                                                                                    ((list*
                                                                                                      'index-spec-list
                                                                                                      $0)))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-assignment-or-call-1)
                                                                                           (defun
                                                                                            pascal/parse-assignment-or-call-1
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> assignment-or-call-1 (alt ((seq (assign expression) ($0)) (seq (assignment-or-call-1-2) ($1)))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (assignment-or-call-1
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'assign)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'assign))
                                                                                                 (assign $1)
                                                                                                 (assign.1 $1)
                                                                                                 ($2
                                                                                                  (pascal/parse-expression
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (expression $2)
                                                                                                 (expression.1 $2)
                                                                                                 ($0 (list $1 $2)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  expression.1
                                                                                                  expression
                                                                                                  assign.1
                                                                                                  assign
                                                                                                  $1
                                                                                                  $2))
                                                                                                $0))
                                                                                              ((member
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                '(nil
                                                                                                  opar
                                                                                                  until
                                                                                                  end
                                                                                                  semi
                                                                                                  else)
                                                                                                :test
                                                                                                #'word-equal)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-assignment-or-call-1-2
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable $0 $1))
                                                                                                $1))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(assign
                                                                                                  nil
                                                                                                  opar
                                                                                                  until
                                                                                                  end
                                                                                                  semi
                                                                                                  else)
                                                                                                '(-->
                                                                                                  assignment-or-call-1
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (assign expression)
                                                                                                    ($0))
                                                                                                   (seq
                                                                                                    (assignment-or-call-1-2)
                                                                                                    ($1))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-rec-var-list)
                                                                                           (defun
                                                                                            pascal/parse-rec-var-list
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> rec-var-list (seq (designator rec-var-list-1) ((list* 'rec-var-list $0))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (rec-var-list
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'identifier)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-designator
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (designator $1)
                                                                                                 (designator.1 $1)
                                                                                                 ($2
                                                                                                  (pascal/parse-rec-var-list-1
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 ($0 (list $1 $2)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  designator.1
                                                                                                  designator
                                                                                                  $1
                                                                                                  $2))
                                                                                                (list*
                                                                                                 'rec-var-list
                                                                                                 $0)))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(identifier)
                                                                                                '(-->
                                                                                                  rec-var-list
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (designator
                                                                                                     rec-var-list-1)
                                                                                                    ((list*
                                                                                                      'rec-var-list
                                                                                                      $0)))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-ordinal-expression)
                                                                                           (defun
                                                                                            pascal/parse-ordinal-expression
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> ordinal-expression (seq (expression) ((list* 'ordinal-expression $0))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (ordinal-expression
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((member
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                '(plus
                                                                                                  minus
                                                                                                  obra
                                                                                                  opar
                                                                                                  not
                                                                                                  identifier
                                                                                                  nile
                                                                                                  string
                                                                                                  real
                                                                                                  integer)
                                                                                                :test
                                                                                                #'word-equal)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-expression
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (expression $1)
                                                                                                 (expression.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  expression.1
                                                                                                  expression
                                                                                                  $1))
                                                                                                (list*
                                                                                                 'ordinal-expression
                                                                                                 $0)))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(plus
                                                                                                  minus
                                                                                                  obra
                                                                                                  opar
                                                                                                  not
                                                                                                  identifier
                                                                                                  nile
                                                                                                  string
                                                                                                  real
                                                                                                  integer)
                                                                                                '(-->
                                                                                                  ordinal-expression
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (expression)
                                                                                                    ((list*
                                                                                                      'ordinal-expression
                                                                                                      $0)))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-for-statement-1)
                                                                                           (defun
                                                                                            pascal/parse-for-statement-1
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> for-statement-1 (alt ((seq (to) ($1)) (seq (downto) ($1)))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (for-statement-1
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'to)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'to))
                                                                                                 (to $1)
                                                                                                 (to.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  to.1
                                                                                                  to
                                                                                                  $1))
                                                                                                $1))
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'downto)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'downto))
                                                                                                 (downto $1)
                                                                                                 (downto.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  downto.1
                                                                                                  downto
                                                                                                  $1))
                                                                                                $1))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(to downto)
                                                                                                '(-->
                                                                                                  for-statement-1
                                                                                                  (alt
                                                                                                   (seq (to) ($1))
                                                                                                   (seq
                                                                                                    (downto)
                                                                                                    ($1))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-control-variable)
                                                                                           (defun
                                                                                            pascal/parse-control-variable
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> control-variable (seq (identifier) ((list* 'control-variable $0))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (control-variable
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'identifier)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'identifier))
                                                                                                 (identifier $1)
                                                                                                 (identifier.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  identifier.1
                                                                                                  identifier
                                                                                                  $1))
                                                                                                (list*
                                                                                                 'control-variable
                                                                                                 $0)))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(identifier)
                                                                                                '(-->
                                                                                                  control-variable
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (identifier)
                                                                                                    ((list*
                                                                                                      'control-variable
                                                                                                      $0)))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-case-list)
                                                                                           (defun
                                                                                            pascal/parse-case-list
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> case-list (seq (one-case case-list-1 case-list-2) ((list* 'case-list $0))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (case-list
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((member
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                '(plus
                                                                                                  minus
                                                                                                  identifier
                                                                                                  real
                                                                                                  integer
                                                                                                  string)
                                                                                                :test
                                                                                                #'word-equal)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-one-case
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (one-case $1)
                                                                                                 (one-case.1 $1)
                                                                                                 ($2
                                                                                                  (pascal/parse-case-list-1
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 ($3
                                                                                                  (pascal/parse-case-list-2
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 ($0 (list $1 $2 $3)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  one-case.1
                                                                                                  one-case
                                                                                                  $1
                                                                                                  $2
                                                                                                  $3))
                                                                                                (list* 'case-list $0)))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(plus
                                                                                                  minus
                                                                                                  identifier
                                                                                                  real
                                                                                                  integer
                                                                                                  string)
                                                                                                '(-->
                                                                                                  case-list
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (one-case
                                                                                                     case-list-1
                                                                                                     case-list-2)
                                                                                                    ((list*
                                                                                                      'case-list
                                                                                                      $0)))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-if-statement-1)
                                                                                           (defun
                                                                                            pascal/parse-if-statement-1
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> if-statement-1 (alt ((seq (else statement) ($0)) (seq nil ('nil)))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (if-statement-1
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'else)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'else))
                                                                                                 (else $1)
                                                                                                 (else.1 $1)
                                                                                                 ($2
                                                                                                  (pascal/parse-statement
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (statement $2)
                                                                                                 (statement.1 $2)
                                                                                                 ($0 (list $1 $2)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  statement.1
                                                                                                  statement
                                                                                                  else.1
                                                                                                  else
                                                                                                  $1
                                                                                                  $2))
                                                                                                $0))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-boolean-expression)
                                                                                           (defun
                                                                                            pascal/parse-boolean-expression
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> boolean-expression (seq (expression) ((list* 'boolean-expression $0))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (boolean-expression
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((member
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                '(plus
                                                                                                  minus
                                                                                                  obra
                                                                                                  opar
                                                                                                  not
                                                                                                  identifier
                                                                                                  nile
                                                                                                  string
                                                                                                  real
                                                                                                  integer)
                                                                                                :test
                                                                                                #'word-equal)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-expression
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (expression $1)
                                                                                                 (expression.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  expression.1
                                                                                                  expression
                                                                                                  $1))
                                                                                                (list*
                                                                                                 'boolean-expression
                                                                                                 $0)))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(plus
                                                                                                  minus
                                                                                                  obra
                                                                                                  opar
                                                                                                  not
                                                                                                  identifier
                                                                                                  nile
                                                                                                  string
                                                                                                  real
                                                                                                  integer)
                                                                                                '(-->
                                                                                                  boolean-expression
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (expression)
                                                                                                    ((list*
                                                                                                      'boolean-expression
                                                                                                      $0)))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-rel-op)
                                                                                           (defun
                                                                                            pascal/parse-rel-op
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> rel-op (alt ((seq (eq) ($1)) (seq (in) ($1)) (seq (ne) ($1)) (seq (ge) ($1)) (seq (le) ($1)) (seq (gt) ($1)) (seq (lt) ($1)))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (rel-op
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'eq)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'eq))
                                                                                                 (eq $1)
                                                                                                 (eq.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  eq.1
                                                                                                  eq
                                                                                                  $1))
                                                                                                $1))
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'in)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'in))
                                                                                                 (in $1)
                                                                                                 (in.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  in.1
                                                                                                  in
                                                                                                  $1))
                                                                                                $1))
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'ne)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'ne))
                                                                                                 (ne $1)
                                                                                                 (ne.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  ne.1
                                                                                                  ne
                                                                                                  $1))
                                                                                                $1))
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'ge)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'ge))
                                                                                                 (ge $1)
                                                                                                 (ge.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  ge.1
                                                                                                  ge
                                                                                                  $1))
                                                                                                $1))
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'le)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'le))
                                                                                                 (le $1)
                                                                                                 (le.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  le.1
                                                                                                  le
                                                                                                  $1))
                                                                                                $1))
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'gt)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'gt))
                                                                                                 (gt $1)
                                                                                                 (gt.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  gt.1
                                                                                                  gt
                                                                                                  $1))
                                                                                                $1))
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'lt)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'lt))
                                                                                                 (lt $1)
                                                                                                 (lt.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  lt.1
                                                                                                  lt
                                                                                                  $1))
                                                                                                $1))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(eq in ne ge le gt lt)
                                                                                                '(-->
                                                                                                  rel-op
                                                                                                  (alt
                                                                                                   (seq (eq) ($1))
                                                                                                   (seq (in) ($1))
                                                                                                   (seq (ne) ($1))
                                                                                                   (seq (ge) ($1))
                                                                                                   (seq (le) ($1))
                                                                                                   (seq (gt) ($1))
                                                                                                   (seq
                                                                                                    (lt)
                                                                                                    ($1))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-factor-1-1)
                                                                                           (defun
                                                                                            pascal/parse-factor-1-1
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> factor-1-1 (alt ((seq nil ('nil)) (seq (actual-params) ($0)))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (factor-1-1
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'opar)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-actual-params
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (actual-params $1)
                                                                                                 (actual-params.1 $1)
                                                                                                 ($0 (list $1)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  actual-params.1
                                                                                                  actual-params
                                                                                                  $1))
                                                                                                $0))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-exp-list)
                                                                                           (defun
                                                                                            pascal/parse-exp-list
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> exp-list (seq (expression exp-list-1) ((list* 'exp-list $0))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (exp-list
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((member
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                '(plus
                                                                                                  minus
                                                                                                  obra
                                                                                                  opar
                                                                                                  not
                                                                                                  identifier
                                                                                                  nile
                                                                                                  string
                                                                                                  real
                                                                                                  integer)
                                                                                                :test
                                                                                                #'word-equal)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-expression
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (expression $1)
                                                                                                 (expression.1 $1)
                                                                                                 ($2
                                                                                                  (pascal/parse-exp-list-1
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 ($0 (list $1 $2)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  expression.1
                                                                                                  expression
                                                                                                  $1
                                                                                                  $2))
                                                                                                (list* 'exp-list $0)))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(plus
                                                                                                  minus
                                                                                                  obra
                                                                                                  opar
                                                                                                  not
                                                                                                  identifier
                                                                                                  nile
                                                                                                  string
                                                                                                  real
                                                                                                  integer)
                                                                                                '(-->
                                                                                                  exp-list
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (expression
                                                                                                     exp-list-1)
                                                                                                    ((list*
                                                                                                      'exp-list
                                                                                                      $0)))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-subrange-type)
                                                                                           (defun
                                                                                            pascal/parse-subrange-type
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> subrange-type (seq (constant dotdot constant) ((list* 'subrange-type $0))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (subrange-type
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((member
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                '(string
                                                                                                  integer
                                                                                                  real
                                                                                                  identifier
                                                                                                  minus
                                                                                                  plus)
                                                                                                :test
                                                                                                #'word-equal)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (pascal/parse-constant
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (constant $1)
                                                                                                 (constant.1 $1)
                                                                                                 ($2
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'dotdot))
                                                                                                 (dotdot $2)
                                                                                                 (dotdot.1 $2)
                                                                                                 ($3
                                                                                                  (pascal/parse-constant
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (constant.2 $3)
                                                                                                 ($0 (list $1 $2 $3)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  constant.2
                                                                                                  dotdot.1
                                                                                                  dotdot
                                                                                                  constant.1
                                                                                                  constant
                                                                                                  $1
                                                                                                  $2
                                                                                                  $3))
                                                                                                (list*
                                                                                                 'subrange-type
                                                                                                 $0)))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(string
                                                                                                  integer
                                                                                                  real
                                                                                                  identifier
                                                                                                  minus
                                                                                                  plus)
                                                                                                '(-->
                                                                                                  subrange-type
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (constant
                                                                                                     dotdot
                                                                                                     constant)
                                                                                                    ((list*
                                                                                                      'subrange-type
                                                                                                      $0)))))))))))
                                                                                          (progn
                                                                                           (fmakunbound
                                                                                            'pascal/parse-translation-unit)
                                                                                           (defun
                                                                                            pascal/parse-translation-unit
                                                                                            (com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                            "(--> translation-unit (alt ((seq (unit new-ident semi block dot) ((list 'unit new-ident (list) block))) (seq (program new-ident pascal-program-1 semi block dot) ((list 'program new-ident (list :external-files $3) block))))))"
                                                                                            (com.informatimago.rdp::with-non-terminal
                                                                                             (translation-unit
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (cond
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'unit)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'unit))
                                                                                                 (unit $1)
                                                                                                 (unit.1 $1)
                                                                                                 ($2
                                                                                                  (pascal/parse-new-ident
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (new-ident $2)
                                                                                                 (new-ident.1 $2)
                                                                                                 ($3
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'semi))
                                                                                                 (semi $3)
                                                                                                 (semi.1 $3)
                                                                                                 ($4
                                                                                                  (pascal/parse-block
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (block $4)
                                                                                                 (block.1 $4)
                                                                                                 ($5
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'dot))
                                                                                                 (dot $5)
                                                                                                 (dot.1 $5)
                                                                                                 ($0
                                                                                                  (list
                                                                                                   $1
                                                                                                   $2
                                                                                                   $3
                                                                                                   $4
                                                                                                   $5)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  dot.1
                                                                                                  dot
                                                                                                  block.1
                                                                                                  block
                                                                                                  semi.1
                                                                                                  semi
                                                                                                  new-ident.1
                                                                                                  new-ident
                                                                                                  unit.1
                                                                                                  unit
                                                                                                  $1
                                                                                                  $2
                                                                                                  $3
                                                                                                  $4
                                                                                                  $5))
                                                                                                (list
                                                                                                 'unit
                                                                                                 new-ident
                                                                                                 (list)
                                                                                                 block)))
                                                                                              ((word-equal
                                                                                                (scanner-current-token
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                'program)
                                                                                               (let*
                                                                                                (($1
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'program))
                                                                                                 (program $1)
                                                                                                 (program.1 $1)
                                                                                                 ($2
                                                                                                  (pascal/parse-new-ident
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (new-ident $2)
                                                                                                 (new-ident.1 $2)
                                                                                                 ($3
                                                                                                  (pascal/parse-pascal-program-1
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 ($4
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'semi))
                                                                                                 (semi $4)
                                                                                                 (semi.1 $4)
                                                                                                 ($5
                                                                                                  (pascal/parse-block
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner))
                                                                                                 (block $5)
                                                                                                 (block.1 $5)
                                                                                                 ($6
                                                                                                  (accept
                                                                                                   com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                   'dot))
                                                                                                 (dot $6)
                                                                                                 (dot.1 $6)
                                                                                                 ($0
                                                                                                  (list
                                                                                                   $1
                                                                                                   $2
                                                                                                   $3
                                                                                                   $4
                                                                                                   $5
                                                                                                   $6)))
                                                                                                (declare
                                                                                                 (ignorable
                                                                                                  $0
                                                                                                  dot.1
                                                                                                  dot
                                                                                                  block.1
                                                                                                  block
                                                                                                  semi.1
                                                                                                  semi
                                                                                                  new-ident.1
                                                                                                  new-ident
                                                                                                  program.1
                                                                                                  program
                                                                                                  $1
                                                                                                  $2
                                                                                                  $3
                                                                                                  $4
                                                                                                  $5
                                                                                                  $6))
                                                                                                (list
                                                                                                 'program
                                                                                                 new-ident
                                                                                                 (list
                                                                                                  :external-files
                                                                                                  $3)
                                                                                                 block)))
                                                                                              (t
                                                                                               (com.informatimago.rdp::error-unexpected-token
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                '(unit program)
                                                                                                '(-->
                                                                                                  translation-unit
                                                                                                  (alt
                                                                                                   (seq
                                                                                                    (unit
                                                                                                     new-ident
                                                                                                     semi
                                                                                                     block
                                                                                                     dot)
                                                                                                    ((list
                                                                                                      'unit
                                                                                                      new-ident
                                                                                                      (list)
                                                                                                      block)))
                                                                                                   (seq
                                                                                                    (program
                                                                                                     new-ident
                                                                                                     pascal-program-1
                                                                                                     semi
                                                                                                     block
                                                                                                     dot)
                                                                                                    ((list
                                                                                                      'program
                                                                                                      new-ident
                                                                                                      (list
                                                                                                       :external-files
                                                                                                       $3)
                                                                                                      block)))))))))))
                                                                                          (progn
                                                                                           (fmakunbound 'parse-pascal)
                                                                                           (defun
                                                                                            parse-pascal
                                                                                            (com.informatimago.rdp::source)
                                                                                            "
SOURCE: When the grammar has a scanner generated, or a scanner class
        name, SOURCE can be either a string, or a stream that will be
        scanned with the generated scanner.  Otherwise, it should be a
        SCANNER instance.
"
                                                                                            (let
                                                                                             ((com.informatimago.common-lisp.parser.scanner:scanner
                                                                                               (make-instance
                                                                                                'pascal-scanner
                                                                                                :source
                                                                                                com.informatimago.rdp::source)))
                                                                                             (advance-line
                                                                                              com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                             (com.informatimago.rdp::with-non-terminal
                                                                                              (pascal
                                                                                               com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                              (prog1
                                                                                               (pascal/parse-translation-unit
                                                                                                com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                               (unless
                                                                                                (scanner-end-of-source-p
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                (cerror
                                                                                                 "Continue"
                                                                                                 'parser-end-of-source-not-reached
                                                                                                 :file
                                                                                                 (com.informatimago.common-lisp.parser.scanner:scanner-file
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                 :line
                                                                                                 (scanner-line
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                 :column
                                                                                                 (scanner-column
                                                                                                  com.informatimago.common-lisp.parser.scanner:scanner)
                                                                                                 :grammar
                                                                                                 (grammar-named
                                                                                                  'pascal)
                                                                                                 :scanner
                                                                                                 com.informatimago.common-lisp.parser.scanner:scanner
                                                                                                 :non-terminal-stack
                                                                                                 (copy-list
                                                                                                  *non-terminal-stack*))))))))
                                                                                          'pascal)