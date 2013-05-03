;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               reader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Reader macros to read ruby sexps as lisp sexps.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-02-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
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
  (ignore-errors (delete-package "COM.INFORMATIMAGO.RUBY.READER"))
  (ignore-errors (delete-package "COM.INFORMATIMAGO.RUBY.RUBY"))
  (ignore-errors (delete-package "COM.INFORMATIMAGO.RUBY.IDENTIFIERS")))

(defpackage "COM.INFORMATIMAGO.RUBY.IDENTIFIERS"
  (:use)
  (:nicknames "RIDS"))

(defpackage "COM.INFORMATIMAGO.RUBY.RUBY"
  (:use)
  (:nicknames "RUBY")
  (:export "DEF" "VAR-REF" "VAR-FIELD"
           "IDENT" "CONST" "FLOAT" "INT" "CHAR"
           "RETURN" "COMMAND" "COMMAND-CALL" "CALL" "VCALL" "FCALL"))

(defpackage "COM.INFORMATIMAGO.RUBY.READER"
  (:use "COMMON-LISP")
  (:export "ENABLE-RUBY-READTABLE" "DISABLE-RUBY-READTABLE"))
(in-package "COM.INFORMATIMAGO.RUBY.READER")


(defun read-bracket-list (stream character)
  (declare (ignore character))
  (values (read-delimited-list #\] stream)))

(defun read-symbol (stream character)
  (declare (ignore character))
  (let ((*package* (load-time-value (find-package "KEYWORD"))))
    (let ((symbol (read stream)))
      (values (if (stringp symbol)
                (intern symbol)
                symbol)))))

(defun make-ruby-readtable ()
  (let ((*readtable* (copy-readtable nil)))
    (set-syntax-from-char #\, #\space)
    (set-syntax-from-char #\] #\))
    (set-syntax-from-char #\| #\a)
    (set-macro-character #\[ 'read-bracket-list nil)
    (set-macro-character #\: 'read-symbol nil)
    *readtable*))

(defparameter *ruby-readtable* (make-ruby-readtable))

(defmacro enable-ruby-readtable ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* *ruby-readtable*)))

(defmacro disable-ruby-readtable ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (copy-readtable nil))))



(defun collect-keywords (object)
  (let ((kw (make-hash-table)))
    (labels ((collect (object)
               (typecase object
                 (cons    (collect (car object)) (collect (cdr object)))
                 (keyword (setf (gethash object kw) kw)))))
      (collect object))
    (let ((r '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k r)) kw)
      (sort r (function string<)))))



;; (defparameter *ruby-asts*
;;   (with-open-file (inp #P"~/src/public/lisp/ruby/ruby-1.9-1-library.ast")
;;     (let ((*readtable* *ruby-readtable*))
;;       (loop
;;         :for sexp = (read inp nil inp)
;;         :until (eq sexp inp)
;;         :collect sexp))))
;;
;; (collect-keywords (mapcar (function third) *ruby-asts*))
;; 
;; (:! :!= :!~ :% :& :&& :* :** :+ :+@ :- :-@ :|.| :/ :|::| :< :<< :<=
;; :<=> :== :=== :=~ :> :>= :>> :@backref :@char :@const :@cvar :@float
;; :@gvar :@ident :@int :@ivar :@kw :@label :@op :@period :@regexp_end
;; :@tstring_content :alias :and :aref :aref_field :args_add
;; :args_add_block :args_add_star :args_new :arg_paren :array :assign
;; :assoclist_from_args :assoc_new :bare_assoc_hash :begin :binary
;; :blockarg :block_var :bodystmt :brace_block :break :call :case :class
;; :command :command_call :const_path_field :const_path_ref :const_ref
;; :def :defined :defs :dot2 :dot3 :do_block :dyna_symbol :else :elsif
;; :end :ensure :fcall :field :for :hash :if :ifop :if_mod :lambda
;; :massign :method_add_arg :method_add_block :mlhs_add :mlhs_add_star
;; :mlhs_new :mlhs_paren :module :mrhs_add :mrhs_add_star :mrhs_new
;; :mrhs_new_from_args :next :not :opassign :or :params :paren :program
;; :qwords_add :qwords_new :redo :regexp_add :regexp_literal :regexp_new
;; :rescue :rescue_mod :rest_param :retry :return :return0 :sclass
;; :stmts_add :stmts_new :string_add :string_concat :string_content
;; :string_dvar :string_embexpr :string_literal :super :symbol
;; :symbol_literal :top_const_ref :unary :undef :unless :unless_mod
;; :until :until_mod :var_alias :var_field :var_ref :vcall :void_stmt
;; :when :while :while_mod :xstring_add :xstring_literal :xstring_new
;; :yield :yield0 :zsuper :^ :\| :|\|\|| :~)



(defun ruby-ast (ruby-text)
  (let ((tempfile (format nil "/tmp/ruby-ast-~8,'0X.rb" ()))))
  (with-output-to-file ()))




(defun ripper (ruby-text)
  (let ((buffer (make-array 4096 :element-type 'character))
        (status nil)
        out err)
    (flet ((copy (in out)
             (let ((read-size (read-sequence buffer in)))
               (write-sequence buffer out :end read-size))))
      (setf out (with-output-to-string (out)
                  (setf err (with-output-to-string (err)
                              (let* ((p (ccl:run-program "ruby"
                                          (list "-e" "require 'ripper'"
                                                "-e" (format nil "print Ripper.sexp_raw(~S)" ruby-text))
                                          :input nil
                                          :output :stream
                                          :error :stream
                                          :element-type 'character
                                          :external-format :utf-8))
                                     (pout (ccl:external-process-output-stream p))
                                     (perr (ccl:external-process-error-stream p)))
                                (loop
                                  :do
                                  (when (listen pout)
                                    (copy pout out))
                                  (when (listen perr)
                                    (copy perr err))
                                  :until (setf status (ccl:external-process-status p))))))))
      (when (string/= out "")
        out))))

(defun ripper-sexp (ruby-text)
  (let ((rexp-text (ripper ruby-text)))
    (when rexp-text
      (let ((*readtable* *ruby-readtable*))
        (read-from-string rexp-text)))))


(defun simplify-lisp-form (sexp)
  (if (atom sexp)
    sexp
    (case (first sexp)
      ((progn) (loop
                 :with body = '()
                 :for item :in (rest sexp)
                 :for sitem = (simplify-lisp-form item)
                 :do (if (and (listp sitem) (eq 'progn (first sitem)))
                       (setf body (nconc body (rest sitem)))
                       (setf body (nconc body (list sitem))))
                 :finally (return (if (null (cdr body))
                                    (first body)
                                    `(progn ,@body)))))
      (otherwise
       `(,(first sexp) ,@(mapcar (function simplify-lisp-form) (rest sexp)))))))


(defun lispify-ruby-sexp (rexp)
  (labels ((lispify (rexp)
             (if (atom rexp)
               (case rexp
                 ((:!)      'not)              
                 ((:!=)     '/=)               
                 ((:!~)     'regexp-match-not) 
                 ((:%)      'mod)              
                 ((:&)      'bit-and)          
                 ((:&&)     'and)              
                 ((:*)      '*)                
                 ((:**)     'expt)             
                 ((:+)      '+)                
                 ((:+@)     '+) ; unary                       
                 ((:-)      '-)                
                 ((:-@)     '-) ; unary                       
                 ((:|.|)    'dot)              
                 ((:/)      '/)                
                 ((:|::|)   'scope)            
                 ((:<)      '<)                
                 ((:<<)     'shift-left)       
                 ((:<=)     '<=)               
                 ((:<=>))                      
                 ((:==)     '=)                
                 ((:===)    'typep)            
                 ((:=~)     'regexp-match)     
                 ((:>)      '>)                
                 ((:>=)     '>=)               
                 ((:>>)     'shift-right)
                 ((:^)      'bit-xor)
                 ((:\|)     'bit-ior)
                 ((:|\|\||) 'or)
                 ((:~)      'bit-not)
                 (otherwise rexp))
               (ecase (first rexp)
                 ((:symbol_literal)  (lispify (second rexp)))
                 ((:symbol)     (intern (string (second (lispify (second rexp)))) (load-time-value (find-package "KEYWORD"))))
                 ((:@ident)     `(ruby:ident ,(intern (second rexp) (load-time-value (find-package "COM.INFORMATIMAGO.RUBY.IDENTIFIERS")))))
                 ((:@const)     `(ruby:const ,(intern (second rexp) (load-time-value (find-package "COM.INFORMATIMAGO.RUBY.IDENTIFIERS")))))
                 ((:@char)      `(ruby:char ,(second rexp)))
                 ((:@int)       `(ruby:int  ,(parse-integer (second rexp))))
                 ((:@float)     `(ruby:int  ,(let ((*read-default-float-format* 'double-float))
                                                  (read-from-string (second rexp)))))
                 ((:unary)     (assert (null (cdddr rexp)))
                  `(,(lispify (second rexp))
                     ,(lispify (third rexp))))
                 ((:binary)     (assert (null (cddddr rexp)))
                  `(,(lispify (third rexp))
                     ,(lispify (second rexp))
                     ,(lispify (fourth rexp))))

                 ((:void_stmt)        `(progn))
                 ((:stmts_new)        `(progn))
                 ((:stmts_add)        (append (lispify (second rexp))
                                              (list (lispify (third rexp)))))
                 ((:array)            `(vector ,@(lispify (second rexp))))
                 ((:args_add_block)   (destructuring-bind (op arguments block) rexp
                                        (if (eq block 'false)
                                          (lispify arguments)
                                          (append (lispify arguments)
                                                  `(lambda () ,(lispify block))))))
                 ((:args_new)         '())
                 ((:args_add)         (append (lispify (second rexp))
                                              (list (lispify (third rexp)))))
                 ((:def)              `(ruby:def ,@(mapcar (function lispify) (rest rexp))))
                 ((:params)           (destructuring-bind (op mandatory optional rest pars2 block) rexp
                                        (append (mapcar (function lispify) mandatory)
                                                (when optional
                                                  `(&optional ,@(mapcar (function lispify) optional)))
                                                (when rest
                                                  `(&rest ,(lispify rest)))
                                                (when pars2
                                                  `(&pars2 ,@(mapcar (function lispify) pars2)))
                                                (when block
                                                  `(&block ,(lispify block))))))
                 ((:var_ref)          `(ruby:var-ref   ,@(mapcar (function lispify) (rest rexp))))
                 ((:var_field)        `(ruby:var-field ,@(mapcar (function lispify) (rest rexp))))
                 ((:paren :arg_paren) (if (null (cddr rexp))
                                        (lispify (second rexp))
                                        `(list ,@(mapcar (function lispify) (rest rexp)))))
                 ((:return0)          `(ruby:return))
                 ((:return)           (destructuring-bind (op arguments) rexp
                                       `(ruby:return ,@(lispify arguments))))
                 ((:command)          (destructuring-bind (op function arguments) rexp
                                        `(ruby:command ,(lispify function) ,@(lispify arguments))))
                 ((:command_call)     (destructuring-bind (op function arguments) rexp
                                        `(ruby:command-call ,(lispify function) ,@(lispify arguments))))
                 ((:vcall)            (destructuring-bind (op function) rexp
                                        `(ruby:vcall ,(lispify function))))
                 ((:call)             (destructuring-bind (op function) rexp
                                        `(ruby:call ,(lispify function))))
                 ((:fcall)            (destructuring-bind (op function) rexp
                                        `(ruby:fcall ,(lispify function))))
                 ((:method_add_arg)   (destructuring-bind (op call arguments) rexp
                                        (append (lispify call) (lispify arguments))))
                 ((:method_add_block) (destructuring-bind (op call arguments) rexp
                                        (append (lispify call) (lispify arguments))))
                 ((:program)          `(progn ,@(mapcar (function lispify) (rest rexp))))
                 ((:bodystmt)         (destructuring-bind (op body rescue else ensure) rexp
                                        (flet ((generate (body rescue else)
                                                 (if (or rescue else)
                                                   `(handler-case
                                                        ,(lispify body)
                                                      ,@(lispify rescue)
                                                      ,@(when else
                                                              `((t ,(lispify else)))))
                                                   (lispify body))))
                                          (if ensure
                                            `(unwind-protect
                                                 ,(generate body rescue else)
                                               ,(lispify ensure))
                                            (generate body rescue else)))))
                 ((:rescue)          (destructuring-bind (op conditions variable expression rest) rexp
                                       (let ((conditions (mapcar (function lispify) conditions)))
                                         `((,(if (null (cdr conditions))
                                                 (first conditions)
                                                 `(or ,@conditions))
                                             (,(lispify variable))
                                             ,(lispify expression))
                                           ,@(lispify rest)))))
                 ((:else)           `(progn ,@(mapcar (function lispify) (rest rexp))))
                 ((:ensure)         `(progn ,@(mapcar (function lispify) (rest rexp))))
                 (otherwise         (mapcar (function lispify) rexp))
                 ((:@backref
                   :@cvar 
                   :@gvar :@ivar :@kw :@label :@op :@period :@regexp_end
                   :@tstring_content :alias :and :aref :aref_field 
                   :args_add_star  :assign
                   :assoclist_from_args :assoc_new :bare_assoc_hash :begin :binary
                   :blockarg :block_var  :brace_block :break  :case :class
                   :const_path_field :const_path_ref :const_ref
                   :defined :defs :dot2 :dot3 :do_block :dyna_symbol :elsif
                   :end :field :for :hash :if :ifop :if_mod :lambda
                   :massign  :mlhs_add :mlhs_add_star
                   :mlhs_new :mlhs_paren :module :mrhs_add :mrhs_add_star :mrhs_new
                   :mrhs_new_from_args :next :not :opassign :or 
                   :qwords_add :qwords_new :redo :regexp_add :regexp_literal :regexp_new
                   :rescue_mod :rest_param :retry  :sclass
                   :string_add :string_concat :string_content
                   :string_dvar :string_embexpr :string_literal :super 
                   :top_const_ref :unary :undef :unless :unless_mod
                   :until :until_mod :var_alias 
                   :when :while :while_mod :xstring_add :xstring_literal :xstring_new
                   :yield :yield0 :zsuper)
                  rexp)))))
    (simplify-lisp-form (lispify rexp))))


(pprint (ripper-sexp (com.informatimago.common-lisp.cesarum.file:text-file-contents "test3.rb")))


(pprint (lispify-ruby-sexp (ripper-sexp (com.informatimago.common-lisp.cesarum.file:text-file-contents "ast.txt"))))

(defparameter *ast* (let ((*readtable* *ruby-readtable*))
                      (with-open-file (in "ast.txt")
                        (loop
                          :for sexp = (read in nil in)
                          :until (eq sexp in)
                          :collect sexp))))

(loop
  :for (file path program) :in *ast*
  :do (terpri) (pprint (lispify-ruby-sexp program)))





