
typedef int Lisp_Object;
void typedef **intptr_t;
const int DATA_SEG_BITS=42;
extern long XLI(long);

static void * ( XPNTR ) ( Lisp_Object a ) {
    return ( ( void * ) ( intptr_t ) ( ( XLI ( a ) & ( 1 ? - ( 1 << 3 ) : ( 2147483647 >> ( 3 - 1 ) ) ) )
                                      | ( DATA_SEG_BITS & ~ ( 1 ? - ( 1 << 3 ) : ( 2147483647 >> ( 3 - 1 ) ) ) ) ) ) ;
}
