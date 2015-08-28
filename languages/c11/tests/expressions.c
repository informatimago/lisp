// invalid: int a,fa(int x){ return a+x; }


void  f_void();
_Bool f_bool();
char  f_char();
short f_short();
short int f_short_int();
long f_long();
long int f_long_int();
long long f_long_long();
signed char  f_signed_char();
signed short f_signed_short();
signed short int f_signed_short_int();
signed long f_signed_long();
signed long int f_signed_long_int();
signed long long f_signed_long_long();
unsigned char  f_unsigned_char();
unsigned short f_unsigned_short();
unsigned short int f_unsigned_short_int();
unsigned long f_unsigned_long();
unsigned long int f_unsigned_long_int();
unsigned long long f_unsigned_long_long();
float f_float();
double f_double();
_Complex f_complex();
_Imaginary f_imaginary();


inline _Noreturn void  f_in_void();
inline _Noreturn _Bool f_in_bool();
inline _Noreturn char  f_in_char();
inline _Noreturn short f_in_short();
short int inline _Noreturn f_in_short_int();
long inline _Noreturn f_in_long();
long int inline _Noreturn f_in_long_int();
long long inline _Noreturn f_in_long_long();
inline _Noreturn signed char  f_in_signed_char();
inline _Noreturn signed short f_in_signed_short();
inline _Noreturn signed short int f_in_signed_short_int();
inline _Noreturn signed long f_in_signed_long();
inline _Noreturn signed long int f_in_signed_long_int();
inline _Noreturn signed long long f_in_signed_long_long();
unsigned char  inline _Noreturn f_in_unsigned_char();
unsigned short inline _Noreturn f_in_unsigned_short();
unsigned short int inline _Noreturn f_in_unsigned_short_int();
inline _Noreturn unsigned long f_in_unsigned_long();
inline _Noreturn unsigned long int f_in_unsigned_long_int();
inline _Noreturn unsigned long long f_in_unsigned_long_long();
inline _Noreturn float f_in_float();
inline _Noreturn double f_in_double();
inline _Noreturn _Complex f_in_complex();
inline _Noreturn _Imaginary f_in_imaginary();


extern static void  v_es_void;
extern static _Bool v_es_bool;
extern static char  v_es_char;
extern static short v_es_short;
extern static short int v_es_short_int;
extern static long v_es_long;
extern static long int v_es_long_int;
extern static long long v_es_long_long;
extern static signed char  v_es_signed_char;
extern static signed short v_es_signed_short;
extern static signed short int v_es_signed_short_int;
extern static signed long v_es_signed_long;
extern static signed long int v_es_signed_long_int;
extern static signed long long v_es_signed_long_long;
extern static unsigned char  v_es_unsigned_char;
extern static unsigned short v_es_unsigned_short;
extern static unsigned short int v_es_unsigned_short_int;
extern static unsigned long v_es_unsigned_long;
extern static unsigned long int v_es_unsigned_long_int;
extern static unsigned long long v_es_unsigned_long_long;
extern static float v_es_float;
extern static double v_es_double;
extern static _Complex v_es_complex;
extern static _Imaginary v_es_imaginary;

void f_register(){
    register void  v_r_void;
    register _Bool v_r_bool;
    register char  v_r_char;
    register short v_r_short;
    register short int v_r_short_int;
    register long v_r_long;
    register long int v_r_long_int;
    register long long v_r_long_long;
    register signed char  v_r_signed_char;
    register signed short v_r_signed_short;
    register signed short int v_r_signed_short_int;
    register signed long v_r_signed_long;
    register signed long int v_r_signed_long_int;
    register signed long long v_r_signed_long_long;
    register unsigned char  v_r_unsigned_char;
    register unsigned short v_r_unsigned_short;
    register unsigned short int v_r_unsigned_short_int;
    register unsigned long v_r_unsigned_long;
    register unsigned long int v_r_unsigned_long_int;
    register unsigned long long v_r_unsigned_long_long;
    register float v_r_float;
    register double v_r_double;
    register _Complex v_r_complex;
    register _Imaginary v_r_imaginary;
}

int f(int argument,char ch,long lg)
{
    int aa,bb=2,cc=3,*dd=&aa,(*g)(int x,char c);
    int   vector[3]={1,2,3};
    struct point {
        int x;
        int y;
        char* color_name;
        int color_rgb[3];
        struct point* next;
        int(*method)(int);
    }     point;
    int   ivar;
    int*  pvar;
    char* svar;
    char  sliteral[]="literal";
    int a=1;
    int b=2;
    int c=3;
    int x=42;
    int primary_expressions;
    ivar=argument;
    ivar=42;
    svar="hello";
    ivar=(1+2);
    svar=("Hello World");
    // TODO: generic_selection

    int unary_expressions;
    svar=&sliteral[0];
    pvar=&ivar;
    ivar=*pvar;
    ivar=+argument;
    ivar=(+argument);
    ivar=+(+argument);
    ivar=-argument;
    ivar=(-argument);
    ivar=-(-argument);
    ivar=~argument;
    ivar=(~argument);
    ivar=!argument;
    ivar=(!argument);
    ivar=sizeof argument;
    ivar=sizeof(argument);
    ivar=sizeof(int);
    ivar=_Alignof(int);
    ivar=++argument;
    ivar=--argument;

    int postfix_expressions;
    ivar=point.x;
    ivar=point.color_rgb[0];
    ivar=point.next->x;
    ivar=point.next->color_name[0];
    ivar=point.method(42);
    ivar=argument++;
    ivar=argument--;
    ivar=point.next->x++;
    ivar=point.next->y--;

    int cast_expressions;
    ivar=(int)sliteral[0];
    ivar=(int)a;
    ivar=(int)(a);
    ivar=(int)f(0,'a',1L);

    int multiplicative_expressions;
    ivar=a*x;
    ivar=2*3;
    ivar=3*x;
    ivar=x*2;

    ivar=a/x;
    ivar=2/3;
    ivar=3/x;
    ivar=x/2;

    ivar=a%x;
    ivar=2%3;
    ivar=3%x;
    ivar=x%2;

    int additive_expressions;
    ivar=a+x;
    ivar=2+3;
    ivar=3+x;
    ivar=x+2;
    
    ivar=a-x;
    ivar=2-3;
    ivar=3-x;
    ivar=x-2;

    int shift_expressions;
    ivar=a<<x;
    ivar=2<<3;
    ivar=3<<x;
    ivar=x<<2;
    ivar=a>>x;
    ivar=2>>3;
    ivar=3>>x;
    ivar=x>>2;

    int relational_expressions;
    ivar=a>x;
    ivar=2>3;
    ivar=3>x;
    ivar=x>2;

    ivar=a<x;
    ivar=2<3;
    ivar=3<x;
    ivar=x<2;

    ivar=a>=x;
    ivar=2>=3;
    ivar=3>=x;
    ivar=x>=2;

    ivar=a<=x;
    ivar=2<=3;
    ivar=3<=x;
    ivar=x<=2;

    int equality_expressions;
    ivar=a==x;
    ivar=2==3;
    ivar=3==x;
    ivar=x==2;

    ivar=a!=x;
    ivar=2!=3;
    ivar=3!=x;
    ivar=x!=2;

    int logic_expressions;
    ivar=a&b;
    ivar=a^b;
    ivar=a|b;
    ivar=a&&b;
    ivar=a||b;
    ivar=a?b:c;
    
    int assignment_expressions;
    ivar=argument;
    ivar+=argument;
    ivar-=argument;
    ivar*=argument;
    ivar/=argument;
    ivar%=argument;
    ivar<<=argument;
    ivar>>=argument;
    ivar&=argument;
    ivar^=argument;
    ivar|=argument;

    int sequence_expressions;
    ivar=a,b,c;
    
    ivar=((a++),
          f(b,'c',42L),
          a*x*x+b*x+c),
            (a*x+b)*x+c;

    int z=1+2*3/4;
    return z;
}

int main(){
    return 0;
}
    
