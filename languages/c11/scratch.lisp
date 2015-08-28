(setf *readtable* (copy-readtable nil))
(ql:quickload :com.informatimago.languages.c11)

(in-package "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER")


(defun emacs-c-tokens ()
  (reduce (function append)
          (reverse (com.informatimago.languages.cpp::context-output-lines
                    (let ((*identifier-package*
                            (load-time-value (find-package "COM.INFORMATIMAGO.LANGUAGES.C11.C"))))
                      (cpp-e "/Users/pjb/src/public/lisp/languages/cpp/tests/emacs.c"
                             :trace-includes t
                             :defines '( ;; "__GNUC__" "4"
                                        "__STDC__" "1"
                                        "__x86_64__" "1")
                             :includes '("/Users/pjb/src/macosx/emacs-24.5/src/")
                             :include-bracket-directories '("/Users/pjb/src/macosx/emacs-24.5/src/"
                                                            "/Users/pjb/src/macosx/emacs-24.5/lib/"
                                                            "/Users/pjb/src/macosx/gcc-4.9.2/gcc/ginclude/" 
                                                            "/usr/include/")
                             :write-processed-lines nil))))
          :initial-value '()))

;; 7 seconds.
#-(and) (defparameter *tc* (emacs-c-tokens))


(defun gen-p ()
  (with-open-file (out "p.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (pprint '(in-package "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER") out)
    (destructuring-bind (grammar &rest rest) (cdr (macroexpand-1 *c*))
      (let ((*print-circle*       t)
            (*print-right-margin* 200))
        (pprint grammar out))
      (let ((*print-circle*       nil)
            (*print-right-margin* 200))
        (dolist (form rest)
          (pprint form out)))))
  (load "p.lisp"))

(defvar *tokens*  nil)
(defvar *scanner* nil)
(defvar *context* nil)
(defvar *cpp-context* nil)

(defun test/parse-stream (&key tokens source-file)
  (declare (stepper disable))
  (let ((tokens (or tokens
                    (reduce (function append)
                            (reverse (com.informatimago.languages.cpp::context-output-lines
                                      (let ((*identifier-package*
                                              (load-time-value (find-package "COM.INFORMATIMAGO.LANGUAGES.C11.C"))))
                                        (setf *cpp-context*
                                              (cpp-e (or source-file "/Users/pjb/src/public/lisp/languages/cpp/tests/emacs.c")
                                                     :trace-includes t
                                                     :defines '( ;; "__GNUC__" "4"
                                                                "__STDC__" "1"
                                                                "__x86_64__" "1")
                                                     :includes '("/Users/pjb/src/macosx/emacs-24.5/src/")
                                                     :include-bracket-directories '("/Users/pjb/src/macosx/emacs-24.5/src/"
                                                                                    "/Users/pjb/src/macosx/emacs-24.5/lib/"
                                                                                    "/Users/pjb/src/macosx/gcc-4.9.2/gcc/ginclude/" 
                                                                                    "/usr/include/")
                                                     :write-processed-lines nil)))))
                            :initial-value '()))))
    (setf *tokens*  tokens)
    (setf *scanner* (make-instance 'pre-scanned-scanner :tokens tokens))
    (setf *context* (make-instance 'context))
    (loop
      :until (scanner-end-of-source-p *scanner*)
      :collect (handler-bind ((parser-end-of-source-not-reached 
                                ;; #'continue
                                #'invoke-debugger))
                 (parse-c11 *scanner*)))))




(defun print-tokens (tokens &key (start 0) (end nil))
  (dolist (token (subseq tokens start end) (values))
    (princ (token-text token))
    (if (find (token-text token) '("{" "}" ";") :test (function string=))
        (terpri)
        (princ " ")))) 

(defun print-typedefs (&optional (context *context*))
  (com.informatimago.common-lisp.cesarum.utility:print-hashtable (context-typedefs context)))
(defun print-enum-constants (&optional (context *context*))
  (com.informatimago.common-lisp.cesarum.utility:print-hashtable (context-enumeration-constants context)))
(defun print-func-names (&optional (context *context*))
  (com.informatimago.common-lisp.cesarum.utility:print-hashtable (context-functions context)))




#-(and) (progn

          (ql:quickload :com.informatimago.rdp)
          (ql:quickload :com.informatimago.languages.c11)
          (gen-p)
          (pprint (remove-unary (test/parse-stream :source-file "tests/expressions.c")))


          (untrace compute-token-kind)

          (untrace typedef-name-p function-name-p enumeration-constant-name-p
                   enter-typedef enter-function enter-enumeration-constant
                   push-declaration-specifiers pop-declaration-specifiers register-declarator
                   scan-next-token)


          
          (progn
            (print-typedefs)       (terpri)
            (print-enum-constants) (terpri)
            (print-func-names)     (terpri))

          
          (print-tokens *tokens* :start (- (length (pre-scanned-tokens *scanner*)) 40) :end (length (pre-scanned-tokens *scanner*)))

          void unblock_tty_out_signal ( void )             ;
          extern void init_sys_modes ( struct tty_display_info * ) ;
          extern void reset_sys_modes ( struct tty_display_info * ) ;
          extern void init_all_sys_modes ( void )                   ;
          extern void reset_all_sys_modes ( void )                  ;
          extern void 

          )


#-(and) (progn
          (defparameter *c* (quote
                             
                             ))

          #.*c*

          (pprint (macroexpand-1 *c*))

          (with-open-file (out "p.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
            (dolist (form (cdr (macroexpand-1 *c*)))
              (pprint form out)))
          )



#-(and) (progn
          
          (map nil 'print (subseq *tc* 0 30))

          (let ((*context* (make-instance 'context)))
            (values (parse-with-lexer (make-list-lexer *tc*) *c11-parser*)
                    *context*))


          
          )



#-(and) (
         ;; yacc

         (defun make-list-lexer (tokens)
           (lambda ()
             (if tokens
                 (let ((token (pop tokens)))
                   (values (token-kind token) token))
                 (values nil nil))))

         (let ((*context* (make-instance 'context)))
           (values (parse-with-lexer (make-list-lexer *tc*) *c11-parser*)
                   *context*))
         )


#-(and) (
         ;; rdp:
         (defun test/parse-stream (src)
           (let ((*scanner* (make-instance '-scanner :source src)))
             (loop
               :until (typep (scanner-current-token *scanner*) 'tok-eof)
               :collect  (lse-parser *scanner*))))
         )

#-(and) (
         (mapcar 'compute-token-kind (subseq *tc* 0 100))
         (|typedef| |unsigned| |int| identifier \; |typedef| |signed| |char| identifier \; |typedef| |unsigned| |char| identifier \; |typedef| |short| identifier \; |typedef| |unsigned| |short| identifier \; |typedef| |int| identifier \; |typedef| |unsigned| |int| identifier \; |typedef| |long| |long| identifier \; |typedef| |unsigned| |long| |long| identifier \; |typedef| |long| identifier \; |typedef| |unsigned| |int| identifier \; |typedef| |int| identifier \; |typedef| |union| { |char| identifier [ dec ] \; |long| |long| identifier \; } identifier \; |typedef| identifier identifier \; |typedef| |int| identifier \; |typedef| |unsigned| |long| identifier \; |typedef| |__builtin_va_list| identifier \; |typedef| identifier identifier \; |typedef| identifier identifier \; |typedef| identifier)

         ("typedef" "unsigned" "int" "bool_bf" ";" "typedef" "signed" "char" "__int8_t" ";" "typedef" "unsigned" "char" "__uint8_t" ";" "typedef" "short" "__int16_t" ";" "typedef" "unsigned" "short" "__uint16_t" ";" "typedef" "int" "__int32_t" ";" "typedef" "unsigned" "int" "__uint32_t" ";" "typedef" "long" "long" "__int64_t" ";" "typedef" "unsigned" "long" "long" "__uint64_t" ";" "typedef" "long" "__darwin_intptr_t" ";" "typedef" "unsigned" "int" "__darwin_natural_t" ";" "typedef" "int" "__darwin_ct_rune_t" ";" "typedef" "union" "{" "char" "__mbstate8" "[" "128" "]" ";" "long" "long" "_mbstateL" ";" "}" "__mbstate_t" ";" "typedef" "__mbstate_t" "__darwin_mbstate_t" ";" "typedef" "int" "__darwin_ptrdiff_t" ";" "typedef" "unsigned" "long" "__darwin_size_t" ";" "typedef" "__builtin_va_list" "__darwin_va_list" ";" "typedef" "__darwin_ct_rune_t" "__darwin_wchar_t" ";" "typedef" "__darwin_wchar_t" "__darwin_rune_t" ";" "typedef" "__darwin_ct_rune_t")



         (let ((*readtable*               vacietis:c-readtable)
               (vacietis:*compiler-state* (vacietis:make-compiler-state)))
           (with-open-file (src #P"~/src/lisp/c/duff-device.c")
             (read src)))

         (defparameter *s* (make-instance 'c11-scanner :source (com.informatimago.common-lisp.cesarum.file:text-file-contents
                                                                #P"~/src/public/lisp/languages/cpp/tests/out.c")))
         (defparameter *t*
           (let ((scanner  (make-instance 'c11-scanner :source (com.informatimago.common-lisp.cesarum.file:text-file-contents
                                                                #P"~/src/public/lisp/languages/cpp/tests/out.c"))))
             (loop for token =  (scan-next-token scanner)
                   until (eq (token-kind token) 'com.informatimago.common-lisp.parser.scanner::<END\ OF\ SOURCE>)
                   collect (print token))))

         (defparameter *tc*
           (mapcar (lambda (token)
                     (setf (token-kind token) (compute-token-kind token))
                     token)
                   (reduce (function append)
                           (reverse (com.informatimago.languages.cpp::context-output-lines
                                     (let ((*identifier-package*
                                             (load-time-value (find-package "COM.INFORMATIMAGO.LANGUAGES.C11.C"))))
                                       (cpp-e "/Users/pjb/src/public/lisp/languages/cpp/tests/emacs.c"
                                              :trace-includes t
                                              :defines '("__GNUC__" "4" "__STDC__" "1" "__x86_64__" "1")
                                              :includes '("/Users/pjb/src/macosx/emacs-24.5/src/")
                                              :include-bracket-directories '("/Users/pjb/src/macosx/emacs-24.5/src/"
                                                                             "/Users/pjb/src/macosx/emacs-24.5/lib/"
                                                                             "/Users/pjb/src/macosx/gcc-4.9.2/gcc/ginclude/" 
                                                                             "/usr/include/")
                                              :write-processed-lines nil))))
                           :initial-value '())))

         (dolist (token *tc*)
           (setf (token-kind token) (compute-token-kind token)))

         (defparameter *yacc*
           (let ((scanner  (make-instance 'c11-scanner :source (com.informatimago.common-lisp.cesarum.file:text-file-contents
                                                                #P"scanner.yacc"))))
             (loop for token =  (scan-next-token scanner)
                   until (eq (token-kind token) 'com.informatimago.common-lisp.parser.scanner::<END\ OF\ SOURCE>)
                   collect (print token))))

         )



#-(and)
(let (ss)
  (do-symbols (s "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER" ss)
    (when (prefixp "C11/PARSE-" (string s)) (push s ss))))

#-(and) (untrace c11/parse-alignment-specifier-1-1 c11/parse-static-assert-declaration-1 c11/parse-exclusive-or-expression-1-1-1 c11/parse-compound-statement c11/parse-external-declaration-1 c11/parse-right-assign c11/parse-external-declaration-1-2-1 c11/parse-declaration-specifiers-1 c11/parse-enum-specifier c11/parse-struct-declaration-1-1 c11/parse-simple-type-specifier-1-1-1 c11/parse-sizeof-argument-1-1-2 c11/parse-sizeof-argument-1-1-1 c11/parse-postfix-expression-item-1 c11/parse-external-declaration-1-2 c11/parse-parameter-declaration-1-1 c11/parse-iteration-statement-1-1 c11/parse-block-item-1 c11/parse-sizeof-argument-1-1 c11/parse-cast-expression-1-3-1-2-1-2 c11/parse-struct-declarator c11/parse-simple-type-specifier-1-1-8 c11/parse-initializer-1-1-1 c11/parse-simple-type-specifier-1-1-3 c11/parse-struct-or-union-specifier-1-1-1 c11/parse-mul-assign-1 c11/parse-iteration-statement-1-2 c11/parse-eq-op c11/parse-simple-unary-expression-1-1 c11/parse-storage-class-specifier-1-2 c11/parse-parameter-declaration-1-2 c11/parse-abstract-declarator-1 c11/parse-inc-op-1 c11/parse-le-op-1 c11/parse-and-expression-1 c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator-1-2-1-1-1-1-1 c11/parse-initializer-list-1-1 c11/parse-cast-expression-1-1 c11/parse-identifier-list-1-1-1 c11/parse-bracket-direct-abstract-declarator c11/parse-initializer-1-1 c11/parse-simple-type-specifier c11/parse-enum-specifier-1-2-1 c11/parse-or-assign-1 c11/parse-expression-1 c11/parse-simple-unary-expression c11/parse-inclusive-or-expression-1-1-1 c11/parse-declaration c11/parse-jump-statement-1-3 c11/parse-direct-declarator--or--direct-abstract-declarator c11/parse-add-assign c11/parse-specifier-qualifier-1 c11/parse-type-name-1-1 c11/parse-selection-statement-1 c11/parse-equality-expression-1-1 c11/parse-simple-direct-declarator c11/parse-pointer c11/parse-simple-type-specifier-1-1-2 c11/parse-declaration-specifier-1 c11/parse-exclusive-or-expression-1 c11/parse-argument-expression-list-1-1-1 c11/parse-struct-declaration-1-1-1 c11/parse-struct-declaration-1 c11/parse-simple-labeled-statement c11/parse-relational-expression-1-1-1 c11/parse-initializer-list-1 c11/parse-mod-assign-1 c11/parse-struct-declaration-list-1-1 c11/parse-storage-class-specifier-1-1 c11/parse-alignment-specifier-1-1-1 c11/parse-cast-expression-1-3-1 c11/parse-ge-op c11/parse-generic-assoc-list-1 c11/parse-pointer-1-2-1 c11/parse-direct-declarator-in-brackets-1-3 c11/parse-multiplicative-expression-1-1-1-1 c11/parse-direct-declarator-item c11/parse-simple-unary-expression-1-2 c11/parse-unary-expression-1 c11/parse-right-op c11/parse-enum-specifier-1-1 c11/parse-pointer-1-2-1-1 c11/parse-sizeof-argument c11/parse-postfix-expression-head-1 c11/parse-direct-declarator-item-1-1 c11/parse-direct-declarator-item-1-1-1 c11/parse-enumerator c11/parse-direct-declarator-in-brackets-1-2-1 c11/parse-generic-association c11/parse-declarator-1-2 c11/parse-designator-1 c11/parse-direct-abstract-declarator c11/parse-selection-statement-1-1 c11/parse-exclusive-or-expression-1-1 c11/parse-cast-expression-1-3-1-2-1-1 c11/parse-type-qualifier-list-1-1 c11/parse-declaration-1-1 c11/parse-postfix-expression-item-1-2-1 c11/parse-multiplicative-expression-1 c11/parse-parameter-list c11/parse-simple-direct-abstract-declarator c11/parse-expression-statement-or-label-1-1 c11/parse-string c11/parse-simple-type-specifier-1-1-7 c11/parse-expression-1-1-1 c11/parse-external-declaration-1-2-2 c11/parse-div-assign c11/parse-parameter-type-list-1-1 c11/parse-external-declaration-1-2-2-2-1-2-1 c11/parse-direct-abstract-declarator-item-1 c11/parse-storage-class-specifier-1-3 c11/parse-simple-type-specifier-1-1-6 c11/parse-block-item-list c11/parse-type-qualifier c11/parse-simple-unary-expression-1 c11/parse-external-declaration-1-2-2-2-1-1-1 c11/parse-ne-op c11/parse-designator-list c11/parse-function-specifier-1-1 c11/parse-logical-or-expression c11/parse-simple-type-specifier-1-1-5 c11/parse-expression-statement-or-label-1-1-1-2 c11/parse-initializer-list-1-1-2 c11/parse-primary-expression-1-1 c11/parse-equality-expression c11/parse-init-declarator-list c11/parse-external-declaration-1-2-2-2-1-1-2-1 c11/parse-sizeof-argument-1-1-1-1 c11/parse-struct-declarator-1-2-1 c11/parse-direct-abstract-declarator-in-parentheses-1 c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator c11/parse-primary-expression-1 c11/parse-simple-type-specifier-1 c11/parse-enumerator-list-1 c11/parse-alignment-specifier c11/parse-postfix-expression-head c11/parse-equality-expression-1-1-1-1 c11/parse-function-specifier-1 c11/parse-selection-statement c11/parse-identifier-list-1-1 c11/parse-init-declarator-1-1 c11/parse-specifier-qualifier-list-1 c11/parse-constant c11/parse-generic-selection c11/parse-enum-specifier-1-2 c11/parse-struct-declarator-list-1-1 c11/parse-constant-expression c11/parse-type-qualifier-list c11/parse-cast-expression-1-3-1-2 c11/parse-relational-expression-1-1-1-1 c11/parse-abstract-declarator-1-1-1 c11/parse-expression-statement c11/parse-storage-class-specifier c11/parse-argument-expression-list-1-1 c11/parse-initializer c11/parse-direct-declarator-in-brackets-1-3-1-1-1 c11/parse-inclusive-or-expression-1-1 c11/parse-direct-abstract-declarator-in-parentheses-1-1 c11/parse-bracket-direct-abstract-declarator-1-1-1-2 c11/parse-exclusive-or-expression c11/parse-mul-assign c11/parse-direct-abstract-declarator-1 c11/parse-assignment-operator-1 c11/parse-assignment-expression-1 c11/parse-simple-direct-declarator-1-2 c11/parse-simple-primary-expression c11/parse-specifier-qualifier-1-1 c11/parse-cast-expression-1 c11/parse-enumerator-list c11/parse-init-declarator c11/parse-simple-type-qualifier-1-1 c11/parse-logical-or-expression-1-1 c11/parse-struct-or-union c11/parse-multiplicative-expression-1-1-1 c11/parse-storage-class-specifier-1 c11/parse-sizeof-argument-1-1-1-1-1-2 c11/parse-declaration-specifier c11/parse-simple-type-qualifier-1-1-3 c11/parse-specifier-qualifier-list-1-1 c11/parse-postfix-expression-head-1-1-1-2 c11/parse-direct-declarator-in-parentheses c11/parse-specifier-qualifier c11/parse-direct-abstract-declarator-in-parentheses-1-2 c11/parse-abstract-declarator-1-2 c11/parse-and-expression c11/parse-additive-expression-1-1 c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator-1-2-1-1-2 c11/parse-direct-declarator-in-brackets-1-1 c11/parse-selection-statement-1-2 c11/parse-enum-specifier-1-2-1-1 c11/parse-direct-abstract-declarator-item-1-1 c11/parse-cast-expression-1-3-1-2-1-1-1 c11/parse-conditional-expression-1-1-1 c11/parse-struct-or-union-specifier-1 c11/parse-generic-selection-1 c11/parse-external-declaration-1-2-2-2 c11/parse-string-1 c11/parse-logical-and-expression c11/parse-inclusive-or-expression c11/parse-bracket-direct-abstract-declarator-1-1-1-1 c11/parse-assignment-operator c11/parse-initializer-list-1-2 c11/parse-block-item-list-1 c11/parse-bracket-direct-abstract-declarator-1-1-1 c11/parse-struct-declaration-list c11/parse-external-declaration-1-2-2-2-1-2 c11/parse-additive-expression c11/parse-simple-type-specifier-1-1-10 c11/parse-storage-class-specifier-1-4 c11/parse-postfix-expression-item-1-6 c11/parse-designator-1-1 c11/parse-direct-declarator-item--or--direct-abstract-declarator-item-1-1-1 c11/parse-direct-declarator-item-1 c11/parse-struct-declaration-list-1 c11/parse-abstract-declarator c11/parse-direct-declarator-1 c11/parse-function-specifier-1-2 c11/parse-pointer-1-1 c11/parse-struct-or-union-specifier-1-1-2 c11/parse-inc-op c11/parse-cast-expression c11/parse-cast-expression-1-3 c11/parse-initializer-list c11/parse-generic-association-1 c11/parse-iteration-statement-1-3-1 c11/parse-and-op c11/parse-initializer-list-1-2-1 c11/parse-init-declarator-list-1 c11/parse-div-assign-1 c11/parse-declarator--or--abstract-declarator c11/parse-direct-declarator-in-parentheses-1-2 c11/parse-struct-or-union-specifier c11/parse-jump-statement-1-4-1 c11/parse-unary-expression c11/parse-specifier-qualifier-1-1-1-1 c11/parse-inclusive-or-expression-1 c11/parse-postfix-expression-1 c11/parse-simple-primary-expression-1 c11/parse-left-assign c11/parse-enumeration-constant c11/parse-equality-expression-1-1-1 c11/parse-external-declaration-1-2-2-2-1-1 c11/parse-declaration-specifiers c11/parse-init-declarator-list-1-1-1 c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator-1-2-1-1 c11/parse-relational-expression-1 c11/parse-type-specifier c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator-1-2-1-1-1 c11/parse-iteration-statement-1-3-1-2-1 c11/parse-additive-expression-1 c11/parse-cast-expression-1-3-1-3 c11/parse-simple-type-specifier-1-1-4 c11/parse-logical-and-expression-1-1 c11/parse-bracket-direct-abstract-declarator-1 c11/parse-type-qualifier-list-1 c11/parse-simple-type-qualifier-1 c11/parse-postfix-expression-item-1-2 c11/parse-external-declaration-1-2-2-1 c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator-1-1 c11/parse-external-declaration-1-2-2-2-1-1-1-1 c11/parse-struct-declarator-list c11/parse-jump-statement-1 c11/parse-type-qualifier-1-1 c11/parse-jump-statement-1-2 c11/parse-specifier-qualifier-list c11/parse-shift-expression-1-1-1-1 c11/parse-initializer-1 c11/parse-simple-labeled-statement-1-2 c11/parse-dec-op-1 c11/parse-sub-assign-1 c11/parse-expression c11/parse-direct-declarator-in-brackets-1-3-1 c11/parse-declarator-1-1 c11/parse-shift-expression-1-1-1 c11/parse-translation-unit c11/parse-struct-declarator-list-1-1-1 c11/parse-declaration-1-1-1 c11/parse-logical-and-expression-1 c11/parse-direct-declarator-in-parentheses-1-1 c11/parse-simple-labeled-statement-1 c11/parse-type-specifier-1 c11/parse-sizeof-argument-1-1-1-1-1-2-1 c11/parse-storage-class-specifier-1-6 c11/parse-direct-declarator c11/parse-parameter-list-1-1 c11/parse-relational-expression-1-1 c11/parse-left-op-1 c11/parse-declaration-1 c11/parse-jump-statement c11/parse-type-name-1 c11/parse-expression-statement-1 c11/parse-type-name c11/parse-init-declarator-list-1-1 c11/parse-postfix-expression-item-1-1 c11/parse-storage-class-specifier-1-5 c11/parse-external-declaration-1-2-2-2-1-1-2 c11/parse-expression-1-1 c11/parse-iteration-statement-1-3 c11/parse-struct-or-union-specifier-1-1-2-1-1 c11/parse-direct-declarator--or--direct-abstract-declarator-1-1 c11/parse-declarator--or--abstract-declarator-1-1-1 c11/parse-generic-assoc-list c11/parse-generic-association-1-1 c11/parse-direct-abstract-declarator-in-parentheses c11/parse-declarator-1 c11/parse-simple-direct-abstract-declarator-1-1-1 c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator-1-2 c11/parse-iteration-statement-1-3-1-2 c11/parse-assignment-operator-1-1 c11/parse-and-expression-1-1-1 c11/parse-multiplicative-expression c11/parse-relational-expression c11/parse-sizeof-argument-1-1-1-1-1-1 c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator-1-2-1 c11/parse-simple-direct-abstract-declarator-1-1 c11/parse-multiplicative-expression-1-1 c11/parse-jump-statement-1-1 c11/parse-simple-direct-declarator-1 c11/parse-additive-expression-1-1-1 c11/parse-struct-or-union-specifier-1-1 c11/parse-designation c11/parse-jump-statement-1-4 c11/parse-simple-type-specifier-1-1 c11/parse-assignment-expression c11/parse-pointer-1 c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator-1 c11/parse-struct-declarator-1-2-1-1 c11/parse-struct-declaration c11/parse-cast-expression-1-3-1-2-1 c11/parse-direct-declarator-in-brackets-1 c11/parse-argument-expression-list c11/parse-declaration-specifiers-1-1 c11/parse-ge-op-1 c11/parse-postfix-expression-item c11/parse-declaration-1-1-2 c11/parse-sizeof-argument-1-1-1-1-1 c11/parse-struct-or-union-1 c11/parse-direct-abstract-declarator-item-1-1-1 c11/parse-direct-abstract-declarator-item c11/parse-right-op-1 c11/parse-eq-op-1 c11/parse-iteration-statement-1-3-1-1-1 c11/parse-direct-declarator-item-1-2-1-1-1 c11/parse-statement-1 c11/parse-postfix-expression-head-1-1 c11/parse-direct-declarator--or--direct-abstract-declarator-1 c11/parse-dec-op c11/parse-ne-op-1 c11/parse-block-item c11/parse-parameter-list-1-1-1 c11/parse-sub-assign c11/parse-iteration-statement-1 c11/parse-and-assign-1 c11/parse-simple-type-specifier-1-1-11 c11/parse-simple-type-qualifier-1-1-2 c11/parse-specifier-qualifier-1-1-1 c11/parse-right-assign-1 c11/parse-simple-type-qualifier-1-1-1 c11/parse-translation-unit-1-1 c11/parse-struct-declarator-1-2 c11/parse-atomic-type-specifier-1 c11/parse-iteration-statement-1-3-1-1 c11/parse-direct-declarator-in-parentheses-1 c11/parse-enumerator-1-1 c11/parse-assignment-expression-1-1 c11/parse-init-declarator-1-1-1 c11/parse-generic-assoc-list-1-1-1 c11/parse-shift-expression-1 c11/parse-direct-declarator-item--or--direct-abstract-declarator-item-1-1 c11/parse-external-declaration-1-1 c11/parse-identifier-list c11/parse-function-specifier c11/parse-le-op c11/parse-sizeof-argument-1-1-1-1-1-2-1-1-1 c11/parse-and-assign c11/parse-simple-type-specifier-1-1-9 c11/parse-direct-declarator-item--or--direct-abstract-declarator-item-1 c11/parse-sizeof-argument-1-1-1-1-1-2-1-1 c11/parse-alignment-specifier-1 c11/parse-postfix-expression-head-1-1-1 c11/parse-or-assign c11/parse-parameter-declaration c11/parse-logical-or-expression-1-1-1 c11/parse-enum-specifier-1-2-1-1-1 c11/parse-designator-list-1 c11/parse-designator-1-2 c11/parse-designator c11/parse-constant-1 c11/parse-direct-declarator-in-brackets-1-2 c11/parse-statement c11/parse-argument-expression-list-1 c11/parse-shift-expression-1-1 c11/parse-postfix-expression-1-1 c11/parse-expression-statement-or-label c11/parse-external-declaration c11/parse-init-declarator-1 c11/parse-primary-expression c11/parse-assignment-expression-1-1-1 c11/parse-selection-statement-1-1-1 c11/parse-postfix-expression-head-1-1-1-3 c11/parse-unary-operator c11/parse-static-assert-declaration c11/parse-simple-labeled-statement-1-1 c11/parse-enum-specifier-1 c11/parse-conditional-expression-1 c11/parse-postfix-expression-item-1-4 c11/parse-postfix-expression-head-1-1-1-1 c11/parse-generic-assoc-list-1-1 c11/parse-parameter-list-1 c11/parse-conditional-expression c11/parse-direct-declarator-1-1 c11/parse-expression-statement-or-label-1-1-1 c11/parse-xor-assign c11/parse-type-qualifier-1 c11/parse-postfix-expression-head-1-1-1-2-1 c11/parse-pointer-1-2 c11/parse-additive-expression-1-1-1-1 c11/parse-simple-unary-expression-1-4 c11/parse-unary-operator-1 c11/parse-logical-and-expression-1-1-1 c11/parse-mod-assign c11/parse-sizeof-argument-1-1-1-1-1-3 c11/parse-conditional-expression-1-1 c11/parse-simple-unary-expression-1-5 c11/parse-compound-statement-1-1 c11/parse-enumerator-list-1-1 c11/parse-generic-association-1-2 c11/parse-iteration-statement c11/parse-struct-declarator-1 c11/parse-direct-declarator-in-brackets-1-3-1-1 c11/parse-cast-expression-1-2 c11/parse-parameter-type-list c11/parse-simple-direct-declarator-1-1 c11/parse-direct-declarator-in-brackets c11/parse-initializer-list-1-1-1 c11/parse-designation-1 c11/parse-struct-or-union-specifier-1-1-2-1 c11/parse-simple-type-qualifier c11/parse-shift-expression c11/parse-simple-unary-expression-1-3 c11/parse-direct-declarator-item--or--direct-abstract-declarator-item c11/parse-postfix-expression-item-1-5 c11/parse-simple-direct-declarator--or--simple-direct-abstract-declarator-1-2-1-1-1-1 c11/parse-translation-unit-1 c11/parse-left-op c11/parse-direct-declarator-item-1-2-1 c11/parse-expression-statement-or-label-1 c11/parse-direct-declarator-item-1-2-1-1 c11/parse-atomic-type-specifier c11/parse-simple-direct-abstract-declarator-1 c11/parse-struct-declarator-list-1 c11/parse-left-assign-1 c11/parse-postfix-expression c11/parse-cast-expression-1-3-1-1 c11/parse-ptr-op c11/parse-postfix-expression-item-1-3 c11/parse-external-declaration-1-2-2-2-1 c11/parse-or-op-1 c11/parse-or-op c11/parse-simple-type-specifier-1-1-12 c11/parse-ptr-op-1 c11/parse-parameter-type-list-1-1-1 c11/parse-parameter-declaration-1 c11/parse-add-assign-1 c11/parse-identifier-list-1 c11/parse-sizeof-argument-1 c11/parse-declarator c11/parse-enumerator-1-1-1 c11/parse-direct-declarator-item-1-2 c11/parse-compound-statement-1 c11/parse-bracket-direct-abstract-declarator-1-1 c11/parse-and-expression-1-1 c11/parse-expression-statement-or-label-1-1-1-1 c11/parse-equality-expression-1 c11/parse-struct-declarator-1-1 c11/parse-enum-specifier-1-1-1 c11/parse-direct-abstract-declarator-1-1 c11/parse-xor-assign-1 c11/parse-expression-statement-1-1 c11/parse-abstract-declarator-1-1 c11/parse-and-op-1 c11/parse-declarator--or--abstract-declarator-1-1 c11/parse-selection-statement-1-1-1-1 c11/parse-logical-or-expression-1 c11/parse-enumerator-1 c11/parse-enumerator-list-1-1-1 c11/parse-declarator--or--abstract-declarator-1 c11/parse-parameter-type-list-1)

;;;; THE END ;;;;
