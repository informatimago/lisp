#define sym(package,name) package##_##name

#define interface(package) \
   typedef void* sym(package,t);\
   extern sym(package,t) make_##package();\
   extern int sym(package,count)(sym(package,t) self);\
   extern void sym(package,put)(sym(package,t) self,char* key,void* object);\
   extern void* sym(package,get)(sym(package,t) self,char* key);

interface(dict)
interface(list)
interface(vector)

