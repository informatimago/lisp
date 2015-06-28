#define S(X) SS(X)
#define SS(X) # X
#define C(X,Y,Z) X##Y##Z
#define F(X,Y) S(X##Y)
#include F(tests/def,ine.h)
#include F(tests/def,\
           ine.h)
