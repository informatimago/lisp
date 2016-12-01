#define __asm(X)
#define __attribute__(X)
#define __inline__ inline

typedef void* builtin_va_list;
extern builtin_va_start(v,l);
extern builtin_va_end(v);
extern builtin_va_arg(v,l);
extern builtin_va_copy(d,s);

#define __builtin_va_list void*
#define __builtin_va_start(v,l)  builtin_va_start(v,l)
#define __builtin_va_end(v)      builtin_va_end(v)
#define __builtin_va_arg(v,l)    builtin_va_arg(v,l)
#define __builtin_va_copy(d,s)   builtin_va_copy(d,s)

#define __builtin_offsetof(TYPE, MEMBER) ((size_t)(&((TYPE*)0)->MEMBER))

#include <config.h>
#include <lisp.h>
