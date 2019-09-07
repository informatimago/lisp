#include <stdio.h>

typedef struct {
    int a:1,b:2,c:3;
} s1;

typedef struct {
    int a:1;
    int b:2;
    int c:3;
} s2;

int main(){
    printf("sizeof(s1)=%lu\n",sizeof(s1));
    printf("sizeof(s2)=%lu\n",sizeof(s2));
    return 0;}
