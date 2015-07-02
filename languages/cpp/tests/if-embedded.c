#define YES 1
#define NO  0
#define DEFINED
#undef UNDEFINED

#if YES
good: yes;
#if YES
good: yes.yes;
#elif defined(UNDEFINED)
bad: yes.defined(UNDEFINED);
#else
bad: yes.no;
#endif
#else
bad:no;
#endif

#ifdef YES
good: yes;
#ifdef YES
good: yes.yes;
#elif defined(UNDEFINED)
bad: yes.defined(UNDEFINED);
#else
bad: yes.no;
#endif
#else
bad:no;
#endif

#ifndef UNDEFINED
good: yes;
#ifndef UNDEFINED
good: yes.yes;
#elif defined(UNDEFINED)
bad: yes.defined(UNDEFINED);
#else
bad: yes.no;
#endif
#else
bad:no;
#endif
