
(setf *readtable* (copy-readtable nil))
(ql:quickload :com.informatimago.languages.c11)

(in-package "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER")

;; (untrace compute-token-kind)
;; 7 seconds.
(defparameter *tc* (reduce (function append)
                        (reverse (com.informatimago.languages.cpp::context-output-lines
                                  (let ((*identifier-package*
                                          (load-time-value (find-package "COM.INFORMATIMAGO.LANGUAGES.C11.C"))))
                                    (cpp-e "/Users/pjb/src/public/lisp/languages/cpp/tests/emacs.c"
                                           :trace-includes t
                                           :defines '(;; "__GNUC__" "4"
                                                      "__STDC__" "1"
                                                      "__x86_64__" "1")
                                           :includes '("/Users/pjb/src/macosx/emacs-24.5/src/")
                                           :include-bracket-directories '("/Users/pjb/src/macosx/emacs-24.5/src/"
                                                                          "/Users/pjb/src/macosx/emacs-24.5/lib/"
                                                                          "/Users/pjb/src/macosx/gcc-4.9.2/gcc/ginclude/" 
                                                                          "/usr/include/")
                                           :write-processed-lines nil))))
                        :initial-value '()))


(progn
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

(untrace typedef-name-p function-name-p enumeration-constant-name-p
         enter-typedef enter-function enter-enumeration-constant
         push-declaration-specifiers pop-declaration-specifiers register-declarator
         scan-next-token)

(defvar *tokens*  nil)
(defvar *scanner* nil)
(defvar *context* nil)

(defun test/parse-stream (&optional tokens)
  (declare (stepper disable))
  (let ((tokens (or tokens
                    (reduce (function append)
                            (reverse (com.informatimago.languages.cpp::context-output-lines
                                      (let ((*identifier-package*
                                              (load-time-value (find-package "COM.INFORMATIMAGO.LANGUAGES.C11.C"))))
                                        (cpp-e "/Users/pjb/src/public/lisp/languages/cpp/tests/emacs.c"
                                               :trace-includes t
                                               :defines '(;; "__GNUC__" "4"
                                                          "__STDC__" "1"
                                                          "__x86_64__" "1")
                                               :includes '("/Users/pjb/src/macosx/emacs-24.5/src/")
                                               :include-bracket-directories '("/Users/pjb/src/macosx/emacs-24.5/src/"
                                                                              "/Users/pjb/src/macosx/emacs-24.5/lib/"
                                                                              "/Users/pjb/src/macosx/gcc-4.9.2/gcc/ginclude/" 
                                                                              "/usr/include/")
                                               :write-processed-lines nil))))
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

          (external-declaration (seq ((alt ((seq (static-assert-declaration) ($0))
                                            (seq ((seq (declaration-specifiers) ((push-declaration-specifiers $1)))
                                                  (alt ((seq (\;) (:specifier))
                                                        (seq (declarator (alt ((seq ((opt ((seq (= initializer) ($2)))) (rep ((seq (\, init-declarator) ($2)))) \;)
                                                                                    ((if $1 '(:initializer $1 $2) '(:simple $2))))
                                                                               (seq ((opt ((seq (declaration-list) ($0)))) compound-statement)
                                                                                    ((list :function-declarator $1 $2))))))
                                                             ((ecase (first $2)
                                                                (:simple (declarator $1 (second $2)))
                                                                (:initializer (cons (declarator $1 (second $2)) (third $2)))
                                                                (:function-declarator (list :declarator $1 $2))))))))
                                                 ((progn (print (list* 'declaration-specifiers (list $1)))
                                                         (print (list* 'declarator (list $2)))
                                                         (pop-declaration-specifiers)
                                                         (if (eql $2 :specifier) $1 $2)))))))
                                     ((print (list* 'external-declaration (list $1))))))

          
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

;; rdp:
(defun test/parse-stream (src)
  (let ((*scanner* (make-instance '-scanner :source src)))
   (loop
     :until (typep (scanner-current-token *scanner*) 'tok-eof)
     :collect  (lse-parser *scanner*))))


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
