#include "clisp.h"

extern object module__uffi__object_tab[];

subr_t module__uffi__subr_tab[1];
uintC module__uffi__subr_tab_size = 0;
subr_initdata_t module__uffi__subr_tab_initdata[1];

object module__uffi__object_tab[1];
object_initdata_t module__uffi__object_tab_initdata[1];
uintC module__uffi__object_tab_size = 0;


void module__uffi__init_function_1 (module_t* module)
{ }

void module__uffi__init_function_2 (module_t* module)
{
  register_foreign_function((void*)&malloc,"malloc",1024);
  register_foreign_function((void*)&free,"free",1024);
}
