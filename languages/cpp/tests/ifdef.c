#define YES
#define TRUE 1

#ifdef YES
ifdef.yes.good1;
#endif

#ifdef YES
ifdef.yes.good2;
#else
ifdef.yes.bad2;
#endif

#ifdef YES
ifdef.yes.good3;
#else
ifdef.yes.bad3;
#else
ifdef.yes.bad3b;
#endif

#ifdef YES
ifdef.yes.good4;
#elif TRUE
ifdef.yes.bad4b;
#else
ifdef.yes.bad4c;
#endif


#ifdef NO
ifdef.no.bad5;
#endif
no good5;

#ifdef NO
ifdef.no.bad6;
#else
ifdef.no.good6;
#endif

#ifdef NO
ifdef.no.bad7;
#else
ifdef.no.good7;
#else
ifdef.no.bad7b;
#endif

#ifdef NO
ifdef.no.bad8;
#elif TRUE
ifdef.no.good8b;
#else
ifdef.no.good8;
#endif



#ifndef NO
ifndef.no.good1;
#endif

#ifndef NO
ifndef.no.good2;
#else
ifndef.no.bad2;
#endif

#ifndef NO
ifndef.no.good3;
#else
ifndef.no.bad3;
#else
ifndef.no.bad3b;
#endif

#ifndef NO
ifndef.no.good4;
#elif TRUE
ifndef.no.bad4b;
#else
ifndef.no.bad4c;
#endif


#ifndef YES
ifndef.yes.bad5;
#endif
no good5;

#ifndef YES
ifndef.yes.bad6;
#else
ifndef.yes.good6;
#endif

#ifndef YES
ifndef.yes.bad7;
#else
ifndef.yes.good7;
#else
ifndef.yes.bad7b;
#endif

#ifndef YES
ifndef.yes.bad8;
#elif TRUE
ifndef.yes.good8b;
#else
ifndef.yes.good8c;
#endif


