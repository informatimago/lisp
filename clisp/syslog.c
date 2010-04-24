#include "clisp.h"

extern object module__syslog__object_tab[];

subr_t module__syslog__subr_tab[1];
uintC module__syslog__subr_tab_size = 0;
subr_initdata_t module__syslog__subr_tab_initdata[1];

object module__syslog__object_tab[1];
object_initdata_t module__syslog__object_tab_initdata[1];
uintC module__syslog__object_tab_size = 0;

extern void (openlog)();
extern void (syslog)();
extern void (closelog)();

void module__syslog__init_function_1(module)
  var module_t* module;
{ }

void module__syslog__init_function_2(module)
  var module_t* module;
{
  register_foreign_function(&openlog,"openlog",1024);
  register_foreign_function(&syslog,"syslog",1024);
  register_foreign_function(&closelog,"closelog",1024);
}
