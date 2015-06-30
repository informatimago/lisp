// #define DOUBLE(X,Y) X ## ## Y
// #define DANGLING(X) X ##
// #define STRINGY_DANGLING(X) # ## X
// #define STRINGY_TOKEN(X) # ahah ## X // concat.h:4:24: error: '#' is not followed by a macro parameter
#define S(X) #X
#define C(A,B) A ## B
#define DOUBLE(A,B) S(C(A,B))
#define SHARP_CONCAT_O # object ## entity
#define SHARP_CONCAT_IND_O S( object ## entity )
#define SHARP_CONCAT_F(object,entity) S(object ## entity)
#define CONCAT(A,B,C) A ## B ## C
#define SHARP_SHARP(A,B) A ## ## B // valid because A ## empty = A

