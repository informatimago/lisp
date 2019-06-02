(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
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

(defmacro check-item-and-emited (form expected)
  `(progn
     ;; TODO: implement an equal method on c-item
     (assert (equal (read-from-string (prin1-to-string ,form))
                    (read-from-string (prin1-to-string (eval ',form)))))
     (check-emited (generate ,form) ,expected)))

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

(check-item-and-emited (expr-seq (assign 'a 1) (assign 'b 2) (assign 'c 3)) "a=1,b=2,c=3")

(check-item-and-emited (expr-callargs (assign 'a 1) (assign 'b 2) (assign 'c 3)) "a=1,b=2,c=3")

(check-item-and-emited (expr-callargs (expr-seq (assign 'a 1) (assign 'b 2) (assign 'c 3))) "a=1,b=2,c=3")
;; TODO: expr-args above expr-seq to force parens in: fun(arg,(f(a),g(b)),arg);

(check-item-and-emited (expr-callargs (assign 'a 1) (expr-seq (assign 'b 2) (assign 'c 3))) "a=1,b=2,c=3")

(check-item-and-emited (expr-if (expr-eq 'a 1) 2 3) "a==1?2:3")

(check-item-and-emited (assign 'var 42) "var=42")

(check-item-and-emited (assign-times 'var 42) "var*=42")

(check-item-and-emited (assign-divided 'var 42) "var/=42")

(check-item-and-emited (assign-modulo 'var 42) "var%=42")

(check-item-and-emited (assign-plus 'var 42) "var+=42")

(check-item-and-emited (assign-minus 'var 42) "var-=42")

(check-item-and-emited (assign-right-shift 'var 42) "var>>=42")

(check-item-and-emited (assign-left-shift 'var 42) "var<<=42")

(check-item-and-emited (assign-bitand 'var 42) "var&=42")

(check-item-and-emited (assign-bitor 'var 42) "var|=42")

(check-item-and-emited (assign-bitxor 'var 42) "var^=42")

(check-item-and-emited (expr-logor (expr-eq 'p 0) (expr-ne 'q 0)) "p==0||q!=0")

(check-item-and-emited (expr-logand (expr-eq 'p 0) (expr-ne 'q 0)) "p==0&&q!=0")

(check-item-and-emited (expr-bitor (expr-eq 'p 0) (expr-ne 'q 0)) "p==0|q!=0")

(check-item-and-emited (expr-bitxor (expr-eq 'p 0) (expr-ne 'q 0)) "p==0^q!=0")

(check-item-and-emited (expr-bitand (expr-eq 'p 0) (expr-ne 'q 0)) "p==0&q!=0")

(check-item-and-emited (expr-eq 'p 0) "p==0")

(check-item-and-emited (expr-ne 'p 0) "p!=0")

(check-item-and-emited (expr-lt 'p 'q) "p<q")

(check-item-and-emited (expr-le 'p 'q) "p<=q")

(check-item-and-emited (expr-gt 'p 'q) "p>q")

(check-item-and-emited (expr-ge 'p 'q) "p>=q")

(check-item-and-emited (expr-left-shift 'var 42) "var<<42")

(check-item-and-emited (expr-right-shift 'var 42) "var>>42")

(check-item-and-emited (expr-plus 'a 'b) "a+b")

(check-item-and-emited (expr-minus 'a 'b) "a-b")

(check-item-and-emited (expr-times 'a 'b) "a*b")

(check-item-and-emited (expr-divided 'a 'b) "a/b")

(check-item-and-emited (expr-modulo 'a 'b) "a%b")

(check-item-and-emited (expr-memptr-deref 'p 'mem) "p.*mem")

(check-item-and-emited (expr-ptrmemptr-deref 'p 'mem) "p->*mem")

(check-item-and-emited (expr-preincr 'a) "++a")

(check-item-and-emited (expr-predecr 'a) "--a")

(check-item-and-emited (expr-postincr 'a) "a++")

(check-item-and-emited (expr-postdecr 'a) "a--")

(check-item-and-emited (expr-lognot 'p) "!p")

(check-item-and-emited (expr-bitnot 'q) "~q")

(check-item-and-emited (expr-deref 'p) "(*(p))")

(check-item-and-emited (expr-address 'a) "(&(a))")

(check-item-and-emited (expr-deref (expr-address 'a)) "(*((&(a))))")

(check-item-and-emited (expr-pos 'a) "(+(a))")

(check-item-and-emited (expr-neg 'a) "(-(a))")

(check-item-and-emited (expr-sizeof 'a) "sizeof(a)")

(check-item-and-emited (expr-new 'a) "new a")

(check-item-and-emited (expr-new[] 'a) "new[] a")

(check-item-and-emited (expr-delete 'a) "delete a")

(check-item-and-emited (expr-delete[] 'a) "delete[] a")

(check-item-and-emited (cpp-stringify 'foo) "#foo")

(check-item-and-emited (expr-field 'p 'a) "p.a")

(check-item-and-emited (expr-field 'p 'a 'b 'c) "p.a.b.c")

(check-item-and-emited (expr-ptrfield 'p 'a) "p->a")

(check-item-and-emited (expr-ptrfield 'p 'q 'r 'a) "p->q->r->a")

(check-item-and-emited (expr-aref 'a 1 2 3) "a[1][2][3]")

(check-item-and-emited (expr-call 'f 1 2 3) "f(1,2,3)")

(check-item-and-emited (expr-call 'f (expr-seq 1 2) 3) "f(1,2,3)")

(check-item-and-emited (absolute-scope 'a) "::a")

(check-item-and-emited (expr-scope 'b 'c) "b::c")

(check-item-and-emited (expr-scope 'a) "a")

(check-item-and-emited (cpp-join 'foo 'bar) "foo##bar")

(check-item-and-emited (stmt-expr (assign 'a 1))
                       "
a=1;
")

(check-item-and-emited (ensure-statement (assign 'a 1))
                       "a=1;
")

(check-item-and-emited (stmt-label 'foo (ensure-statement (assign 'a 1)))
                       "foo:
a=1;
")

(check-item-and-emited (stmt-case 'bar (ensure-statement (assign 'a 1)))
                       "case bar:
a=1;
")

(check-item-and-emited (stmt-default (ensure-statement (assign 'a 1)))
                       "default:
a=1;
")

(check-item-and-emited
 (stmt-block (list (ensure-statement (assign 'a 1)) (ensure-statement (assign 'b 2)) (ensure-statement (assign 'c 3))))
 "{
    a=1;
    b=2;
    c=3;
}")

(check-item-and-emited
 (stmt-let (list (ensure-statement (assign 'a 'x)) (ensure-statement (assign 'b 'y)) (ensure-statement (assign 'c 'z)))
           (list))
 "
{
a=x;
b=y;
c=z;
}")

(check-item-and-emited (stmt-if (expr-call 'print 'a) (expr-call 'print 'b) (expr-eq 'a 'b))
                       "
if(a==b)
    CommonLisp_print(a);
else
    CommonLisp_print(b);
")

(check-item-and-emited (stmt-if (expr-call 'print 'a) nil (expr-eq 'a 'b))
                       "if(a==b)
    CommonLisp_print(a);
")

(check-item-and-emited
 (stmt-switch (stmt-block (list (stmt-case 'foo (ensure-statement (assign 'a 1)))
                                (stmt-break)
                                (stmt-case 'bar (ensure-statement (assign 'a 2)))
                                (stmt-break)
                                (stmt-default (ensure-statement (assign 'a 3)))))
              (expr-seq 'x))
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

(check-item-and-emited (stmt-while (ensure-statement (assign-plus 'a 'b)) (expr-eq 'a 'b))
                       "
while(a==b)
    a+=b;
")

(check-item-and-emited (stmt-do (stmt-block (list (assign-plus 'a 'b))) (expr-eq 'a 'b))
                       "do{
    a+=b;
}while(a==b)")

(check-item-and-emited
 (stmt-for (assign 'a '0) (expr-lt 'a '100) (expr-postincr 'a) (stmt-block (list (assign-plus 'a 'b))))
 "
for(a=0;a<100;a++){
    a+=b;
}")

(check-item-and-emited (stmt-break)
"
break;
")

(check-item-and-emited (stmt-continue)
"continue;
")

(check-item-and-emited (stmt-return nil)
"return;
")

(check-item-and-emited (stmt-return 42)
"return 42;
")

(check-item-and-emited (stmt-goto 'foo)
"goto foo;
")

(check-item-and-emited (asm "move.l d0,d1")
                       "asm(\"move.l d0,d1\");
")

(check-item-and-emited (extern1 "C" (asm "move.l d0,d1"))
                       "extern \"C\"
asm(\"move.l d0,d1\");
")

(check-item-and-emited (extern "C" (list (asm "move.l d0,d1") (asm "move.l d2,d0")))
                       "extern \"C\"{
    asm(\"move.l d0,d1\");
    asm(\"move.l d2,d0\");
}
")

(check-item-and-emited (with-extern "C" (asm "move.l d0,d1") (asm "move.l d2,d0"))
                       "extern \"C\"{
    asm(\"move.l d0,d1\");
    asm(\"move.l d2,d0\");
}
")

(check-item-and-emited (pointer 'foo :const t :volatile t)
                       "* const volatile foo")

(check-item-and-emited (reference 'foo)
                       "&foo")

(check-item-and-emited (member-pointer 'bar 'foo :const t :volatile t)
                       "bar* const volatile foo")

(check-item-and-emited (c-function 'foo (list 'int) :const t :volatile t :throw '(error warning))
                       "foo(int) const volatile throw (CommonLisp_error,CommonLisp_warning)")

(check-item-and-emited (c-vector 'com.informatimago.languages.linc.c::|char| 42)
                       "char[42]")


;;;; THE END ;;;;
