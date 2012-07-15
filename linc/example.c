
/*
  Here is a little function
*/
string_t string_add(string_t a,string_t b){
    {
        int av;
        int bv;
        string_t res=malloc(2+max(strlen(a),strlen(b)));

        sscanf(a,"%d",&av);
        sscanf(b,"%d",&bv);
        sprintf(res,"%d",a+b);
        return(res);
    }
}
/*
  Here is another function.
  Slightly bigger this time.
  (* 42 12)
*/
void test(){
    if(nAllocation>1){
        nData=BcMem_Allocate(sizeof(char)*nAllocation);
        if(copy){
            nLength=MINIMUM(nAllocation-1,this->dlength);
            BcMem_Copy(this->data,nData,nLength*sizeof(char));
        }else
            nLength=0;
    }else{
        nAllocation=1;
        nData=BcMem_Allocate(sizeof(char)*nAllocation);
        nLength=0;
    }
    nData[nLength]=(char)0;
    BcMem_Deallocate((void**)&this->data);
    this->data=nData;
    this->dlength=nLength;
    this->allocation=nAllocation;
    {
        double test=(1+2+3+4)*5/4/3/2*(5-4-3-2)*+a*-b**c*~d*!e*+(1+a)*-(2+b)**(c+3)*~(4+d)*!(e<0);

        printf("%d\n",test);
    }
    return(this);
}
