#define EMPTY
#define VOO 42
#define OOO (X)
#define FOO(X) ((X)+42)
#define SOO(X) #X
#define COO(X,Y) X##Y
#define TO_BE_UNDEFINED_1
#define TO_BE_UNDEFINED_2 X
#define TO_BE_UNDEFINED_3(X) ((X)+(X))
#undef TO_BE_UNDEFINED_1
#undef TO_BE_UNDEFINED_2
#undef TO_BE_UNDEFINED_3
