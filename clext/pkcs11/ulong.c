#include <stdio.h>
#include <limits.h>
int main()
{
    unsigned long x = ~0;
    unsigned long y = (unsigned long) -1;
    printf("%lu\n", x);
    printf("%lu\n", y);
    printf("%lu\n", ULONG_MAX);
    printf("%lu bytes\n", sizeof(x));
    printf("%lu bits\n", sizeof(x) *  CHAR_BIT);
    return 0;
}
