#include <stdio.h>

static void dump(unsigned char* mem,int length){
    unsigned char* p=mem+length;
    printf("#x");
    while(p!=mem){
        p--;
        printf("%02x",(*p));
    }
}

static void test_float(float f){
    int* fp=(int*)(&f);
    dump((unsigned char*)fp,4);
    printf(" %f\n",f);
}

static void test_double(double d){
    long* dp=(long*)(&d);
    dump((unsigned char*)dp,8);
    printf(" %lf\n",d);
}

int main(){
    test_float(0.0e0f);
    test_float(1.0e30f);
    test_double(0.0e0);
    test_double(1.0e30);
    return(0);
}
