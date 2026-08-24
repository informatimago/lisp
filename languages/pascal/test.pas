unit test;

function dbli(x:integer):integer;
begin 
   dbli := x*2
end;

procedure test;
var x,y : integer;
begin
   x:=42;
   y:=2*x*x+4x-1;
begin
   write('Hello ');
   writeln('World!');
end;
   repeat
      while x>0 do begin
         if x mod 15 = 0 then
            writeln('fizzbuzz')
         else if x mod 3 = 0 then
            writeln('fizz')
         else if x mod 5 = 0 then
            writeln('buzz')
         else
            writeln(x);
         x:=pred(x);
      end;
      for y := 1 to 10 do 
         case y of
           1,2,3       : writeln('small');
           100,200,300 : writeln('big')
         end;
   until x=0;
end;

begin
   writeln('unit test loaded!')
end.
