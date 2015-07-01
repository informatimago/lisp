#define VOO BAR + 1
#define BAR VOO - 1

#define f(x,y,z) x,f(y,z,x)
#define G(x,y,z) g(y,z,x)
#define g(x,y,z) x,G(x,y,z)

int x=VOO;
int[] y={f(1,2,3)};
int[] z={g(1,2,3)};


#define SUCC(X) (1+(X))
int f2=SUCC(SUCC(0));

#define TWO SUCC(SUCC(0))
int f3=SUCC(TWO);

#undef FOO
#undef BAR

#define FOO BAR
int i[]={ FOO,
#undef BAR
#define BAR 1
          FOO,
#undef BAR
#define BAR 2
          FOO };


#undef LEFT
#undef RIGHT
#undef FOO
#undef F
#define LEFT F("l",
#define RIGHT ,"r")
#define FOO LEFT "foo" RIGHT
1: FOO; /* ok */
#define F(a,b,c) a##b##c
// 2: FOO; /* good: error: unterminated argument list invoking macro "F" */

#undef LEFT
#undef RIGHT
#undef FOO
#undef F
#define FOO(E) F(l,E,r)
1: FOO(foo); /* ok */
#define F(a,b,c) a##b##c
2: FOO(bar); /* ok */
#undef F
3: FOO(
#define F(a,b,c) a##a
       baz
#undef F
#define F(a,b,c) c##c
       ); 
4: FOO(
#undef F 
#define F(a,b,c) a##a
       FOO(baz)
#undef F
#define F(a,b,c) c##c
       ); 
