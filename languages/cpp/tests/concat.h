#define DOUBLE(X,Y) X ## ## Y
#define DANGLING(X) X ##

#define STRINGY_DANGLING(X) # ## X
#define STRINGY_TOKEN(X) # ahah ## X
#define STRINGIFY_OBJECT # object ## entity

double(hello,kitty) DOUBLE(HELLO,KITTY)
stringify_object STRINGIFY_OBJECT
