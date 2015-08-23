int f(int argument)
{
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
    svar=("Hello" " " "World");
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
    ivar=(int)f(0);

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
    
    ivar=(a++),
            f(b),
            a*x*x+b*x+c,
            (a*x+b)*x+c;

    int z=1+2*3/4;
    return z;
}

int main(){
    return 0;
}
    
