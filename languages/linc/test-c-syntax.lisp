(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(assert (equal (camel-case (symbol-name 'com.informatimago.languages.linc.c::my-var)
                           :capitalize-initial nil)
               "myVar"))

(defmacro check-emited (form expected)
  `(let ((result (with-output-to-string (*c-out*) ,form)))
     (assert (equal result ,expected)
             ()
             "Evaluating ~S~% returned   ~S~% instead of ~S"
             ',form result ,expected)))

(check-emited (emit (camel-case (symbol-name 'com.informatimago.languages.linc.c::printf)
                                :capitalize-initial nil))
              "printf")

(check-emited (generate 'com.informatimago.languages.linc.c::my-var)
              "myVar")

(assert (equal (c-sexp 'com.informatimago.languages.linc.c::my-var)
               ''com.informatimago.languages.linc.c::my-var))

(check-emited (generate "hello world\\r")
              "\"hello world\\r\"")

(check-emited
 (dolist (x (list 0 -1 1 -128 127 128
                  -32768 32767 32768
                  (- (expt 2 31)) (1- (expt 2 31)) (expt 2 31)
                  (- (expt 2 32)) (1- (expt 2 32)) (expt 2 32)
                  (- (expt 2 63)) (1- (expt 2 63)) (expt 2 63)
                  (- (expt 2 64)) (1- (expt 2 64)) (expt 2 64)
                  (- (expt 2 127)) (1- (expt 2 127)) (expt 2 127)
                  (- (expt 2 128)) (1- (expt 2 128)) (expt 2 128)))
   (block go-on
     (handler-bind ((error (lambda (err)
                             (declare (ignore err))
                             ;; (princ err) (terpri)
                             (return-from go-on))))
       (generate x)))
   (emit ", "))
 "0, -1, 1, -128, 127, 128, -32768, 32767, 32768, -2147483648, 2147483647, 2147483648L, -4294967296L, 4294967295L, 4294967296L, -9223372036854775808L, 9223372036854775807L, 9223372036854775808L, -18446744073709551616L, 18446744073709551615L, 18446744073709551616L, -170141183460469231731687303715884105728LL, 170141183460469231731687303715884105727LL, 170141183460469231731687303715884105728LL, -340282366920938463463374607431768211456LL, 340282366920938463463374607431768211455LL, , ")


(check-emited (generate pi)
              "3.141592653589793E+0")

(check-emited (generate (coerce pi 'short-float))
              "3.1415927E+0F")

(check-emited (generate (coerce (expt pi 130) 'long-float))
              "4.260724468298572E+64")

(check-emited (generate 123.456l89)
              "1.23456E+91")

(check-emited (generate #\a)
              "'a'")

(check-emited (generate #\newline)
              "'\\12'")

(check-emited (dolist (item (list

                             (expr-seq      (assign 'a 1) (assign 'b 2) (assign 'c 3))
                             (expr-callargs (assign 'a 1) (assign 'b 2) (assign 'c 3))
                             (expr-callargs (expr-seq  (assign 'a 1) (assign 'b 2) (assign 'c 3)))
                             ;; expr-args above expr-seq to force parens in: fun(arg,(f(a),g(b)),arg);
                             (expr-callargs (assign 'a 1)
                                            (expr-seq (assign 'b 2) (assign 'c 3)))
                             (expr-if (expr-eq 'a 1) 2 3)
                             (assign               'var 42)
                             (assign-times         'var 42)
                             (assign-divided       'var 42)
                             (assign-modulo        'var 42)
                             (assign-plus          'var 42)
                             (assign-minus         'var 42)
                             (assign-right-shift   'var 42)
                             (assign-left-shift    'var 42)
                             (assign-bitand        'var 42)
                             (assign-bitor         'var 42)
                             (assign-bitxor        'var 42)

                             (expr-logor  (expr-eq 'p 0) (expr-ne 'q 0))
                             (expr-logand (expr-eq 'p 0) (expr-ne 'q 0))
                             (expr-bitor  (expr-eq 'p 0) (expr-ne 'q 0))
                             (expr-bitxor (expr-eq 'p 0) (expr-ne 'q 0))
                             (expr-bitand (expr-eq 'p 0) (expr-ne 'q 0))
                             (expr-eq 'p 0)
                             (expr-ne 'p 0)
                             (expr-lt 'p 'q)
                             (expr-le 'p 'q)
                             (expr-gt 'p 'q)
                             (expr-ge 'p 'q)

                             (expr-left-shift        'var 42)
                             (expr-right-shift       'var 42)

                             (expr-plus    'a 'b)
                             (expr-minus   'a 'b)
                             (expr-times   'a 'b)
                             (expr-divided 'a 'b)
                             (expr-modulo  'a 'b)

                             (expr-memptr-deref    'p 'mem)
                             (expr-ptrmemptr-deref 'p 'mem)

                             ;; (expr-cast   'p TYPE???)

                             (expr-preincr   'a)
                             (expr-predecr   'a)
                             (expr-postincr  'a)
                             (expr-postdecr  'a)
                             (expr-lognot    'p)
                             (expr-bitnot    'q)

                             (expr-deref     'p)
                             (expr-address   'a)
                             (expr-deref     (expr-address 'a))
                             (expr-pos       'a)
                             (expr-neg       'a)
                             (expr-sizeof    'a)
                             (expr-new       'a)
                             (expr-new[]     'a)
                             (expr-delete    'a)
                             (expr-delete[]  'a)

                             (cpp-stringify  'foo)

                             (expr-field   'p 'a)
                             (expr-field   'p 'a 'b 'c)
                             (expr-ptrfield   'p 'a)
                             (expr-ptrfield   'p 'q 'r 'a)
                             (expr-aref 'a 1 2 3)
                             (expr-call 'f 1 2 3)
                             (expr-call 'f (expr-seq 1 2) 3) ;; TODO

                             (absolute-scope 'a)
                             (expr-scope 'b 'c)
                             (expr-scope 'a)
                             (cpp-join 'foo 'bar)

                             ))
                ;; (print item)
                ;; (terpri)
                (generate item)
                (terpri *c-out*))
              "a=1,b=2,c=3
a=1,b=2,c=3
a=1,b=2,c=3
a=1,b=2,c=3
a==1?2:3
var=42
var*=42
var/=42
var%=42
var+=42
var-=42
var>>=42
var<<=42
var&=42
var|=42
var^=42
p==0||q!=0
p==0&&q!=0
p==0|q!=0
p==0^q!=0
p==0&q!=0
p==0
p!=0
p<q
p<=q
p>q
p>=q
var<<42
var>>42
a+b
a-b
a*b
a/b
a%b
p.*mem
p->*mem
++a
--a
a++
a--
!p
~q
(*(p))
(&(a))
(*((&(a))))
(+(a))
(-(a))
sizeof(a)
new a
new[] a
delete a
delete[] a
#foo
p.a
p.a.b.c
p->a
p->q->r->a
a[1][2][3]
f(1,2,3)
f(1,2,3)
::a
b::c
a
foo##bar
")



(check-emited (generate (stmt-expr (assign 'a 1)))
"
a=1;
")

(check-emited (generate (ensure-statement (assign 'a 1)))
              "a=1;
")

(check-emited (generate (stmt-label 'foo (ensure-statement (assign 'a 1))))
              "foo:
a=1;
")

(check-emited (generate (stmt-case  'bar (ensure-statement (assign 'a 1))))
              "case bar:
a=1;
")

(check-emited (generate (stmt-default    (ensure-statement (assign 'a 1))))
              "default:
a=1;
")

(check-emited (generate (stmt-block (list
                                     (ensure-statement (assign 'a 1))
                                     (ensure-statement (assign 'b 2))
                                     (ensure-statement (assign 'c 3)))))
              "{
    a=1;
    b=2;
    c=3;
}")


(check-emited (generate (stmt-let (list
                                   (ensure-statement (assign 'a 'x))
                                   (ensure-statement (assign 'b 'y))
                                   (ensure-statement (assign 'c 'z)))
                                  (list
                                   ;; (decl-var 'x 'int 1)
                                   ;; (decl-var 'y 'int 2)
                                   ;; (decl-var 'z 'int 2)
                                   )))
              "
{
a=x;
b=y;
c=z;
}")


(check-emited (generate (stmt-if (expr-call 'print 'a)
                                 (expr-call 'print 'b)
                                 (expr-eq 'a 'b)))
              "
if(a==b)
    CommonLisp_print(a);
else
    CommonLisp_print(b);
")

(check-emited (generate (stmt-if (expr-call 'print 'a)
                                 nil
                                 (expr-eq 'a 'b)))
              "if(a==b)
    CommonLisp_print(a);
")

(check-emited (generate (stmt-switch (stmt-block (list
                                                  (stmt-case  'foo (ensure-statement (assign 'a 1)))
                                                  (stmt-break)
                                                  (stmt-case  'bar (ensure-statement (assign 'a 2)))
                                                  (stmt-break)
                                                  (stmt-default    (ensure-statement (assign 'a 3)))))
                                     (expr-seq 'x)))
              "switch(x){
    case foo:
    a=1;
    break;
    case bar:
    a=2;
    break;
    default:
    a=3;
}")

(check-emited (generate (stmt-while (ensure-statement (assign-plus 'a 'b))
                                    (expr-eq 'a 'b)))
              "
while(a==b)
    a+=b;
")

(check-emited (generate (stmt-do (stmt-block (list (assign-plus 'a 'b)))
                                 (expr-eq 'a 'b)))
              "do{
    a+=b;
}while(a==b)")

(check-emited (generate (stmt-for (assign 'a '0) (expr-lt 'a '100) (expr-postincr 'a)
                                  (stmt-block (list (assign-plus 'a 'b)))))
              "
for(a=0;a<100;a++){
    a+=b;
}")

(check-emited (progn
                (generate (stmt-break))
                (generate (stmt-continue))
                (generate (stmt-return nil))
                (generate (stmt-return 42))
                (generate (stmt-goto 'foo)))
              "
break;
continue;
return ;
return 42;
goto foo;
")


(check-emited (generate (asm "move.l d0,d1"))
              "asm(\"move.l d0,d1\");
")

(check-emited (generate (extern1 "C" (asm "move.l d0,d1")))
              "extern \"C\"
asm(\"move.l d0,d1\");
")

(check-emited (generate (extern "C" (list (asm "move.l d0,d1")
                                          (asm "move.l d2,d0"))))
              "extern \"C\"{
    asm(\"move.l d0,d1\");
    asm(\"move.l d2,d0\");
}
")

(check-emited (generate (with-extern "C"
                          (asm "move.l d0,d1")
                          (asm "move.l d2,d0")))
              "extern \"C\"{
    asm(\"move.l d0,d1\");
    asm(\"move.l d2,d0\");
}
")


(check-emited (generate (pointer 'foo :const t :volatile t))
              "* const volatile foo")

;; C++
(check-emited (generate (reference 'foo))
              "&foo")

;; C++
(check-emited (generate (member-pointer 'bar 'foo :const t :volatile t))
              "bar* const volatile foo")

(check-emited (generate (c-function 'foo (list 'int)
                                    :const t :volatile t :throw-list '(error warning)))
              "foo(int) const volatile throw(CommonLisp_error,CommonLisp_warning) ")

(check-emited (generate (c-vector 'com.informatimago.languages.linc.c::char 42))
              "char[42]")

