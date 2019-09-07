#include <stdio.h>

typedef struct {
} empty_struct;

typedef union {
} empty_union;


int main(){
    empty_union  u;
    empty_struct s;
    printf("u=%p s=%p\n",&u,&s);
    return 0;}
