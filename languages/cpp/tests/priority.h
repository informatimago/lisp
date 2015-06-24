#define C(X,Y) X ## Y
#define S(X) SS(X)
#define SS(X) # X
#define M(X,Y) S(C(X,Y))

char* a=M(HELLO,KITTY);

        
