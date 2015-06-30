#define PROC this-proc
#warning "I warn you, in " PROC ", it won't be pretty."
#error "I told you, there's a snafu in " PROC
#pragma GCC warning "I warn you,  it won't be pretty."
#pragma GCC error "I told you, there's a snafu"
#define W() _Pragma("GCC warning \"I warn you, it won't be pretty.\"")
#define E() _Pragma("GCC error \"I told you, there's a snafu\"")
W()
E()


