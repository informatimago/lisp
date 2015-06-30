#include "variadic.h"
int poo(){
    POO0("hello\n",);
    POO0("%d\n",1+1);
    POO1("hello\n");
    POO1("%d %d\n",1+1,2*2);
    POO2("hello\n");
    POO2("%d %d\n",1+1,2*(1+1,42));
    POO3("hello\n",);
    // POO3("%d\n",1+1);
}
