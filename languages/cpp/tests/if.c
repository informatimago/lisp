#define DEFINED

#if defined(UNDEFINED)
defined.bad.1;
#endif
#if defined (UNDEFINED)
defined.bad.2;
#endif
#if defined UNDEFINED
defined.bad.3;
#endif


#if defined(DEFINED)  // doesn't macroexpand DEFINED.
defined.good.1;
#endif
#if defined (DEFINED)
defined.good.2;
#endif
#if defined DEFINED
defined.good.3;
#endif

#define GOOD_SYM GOOD_SYM
#define DEF(X) defined(X)
#if DEF(GOOD_SYM) // substitues GOOD_SYM as argument when macroexpanding DEF(GOOD_SYM)
def.good;
#endif
#if DEF(UNDEFINED)
def.bad;
#endif


#define STRANGE "Hello"

#if STRANGE
strange.bad.then;
#else
strange.good.else;
#endif

#if foo
foo.bad.then;
#else
foo.good.else;
#endif

#define GOOD 1
#if GOOD will
good.good.then;
#endif

#define height 24
#define width 80
#if !((height-1)*(width/2) < 3<<4) || (1/0)
expr.good.then;
#else
expr.bad.else;
#endif

#define a 1
#define b 2
#define c 3
#define d 4
#if (-a + +b + ~c + !d) - a - b + c + d * d / c % b << 3 - c >> 1 < a <= b > c >= d == a != b & c ^ d | a && 1 || 0
guess.then;
#else
guess.else;
#endif

int x=defined(GOOD_SYM);
int y=defined(GOOD);
