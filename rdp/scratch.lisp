(com.informatimago.rdp.example:parse-example
  "
    const abc = 123,
          pi=3.141592e+0;
    var a,b,c;
    procedure gcd;
    begin
        while a # b do
        begin
             if a<b then b:=b-a ;
             if a>b then a:=a-b
        end
    end;
begin
    a:=42;
    b:=30.0;
    call gcd
end.")

(com.informatimago.rdp.example-without-action:parse-example-without-action
 "
    const abc = 123,
          pi=3.141592e+0;
    var a,b,c;
    procedure gcd;
    begin
        while a # b do
        begin
             if a<b then b:=b-a ;
             if a>b then a:=a-b
        end
    end;
begin
    a:=42;
    b:=30.0;
    call gcd
end.")




