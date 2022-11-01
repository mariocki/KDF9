program COLLATZ;

const
   start = 1234567890123;

type
   natural = 0 .. maxint;

var
   x, count, max : natural;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%listing on

begin
open(1, 1);

x := start;
count := 0;
max := x;

repeat
   if odd(x) then
      x := 3*x + 1
   else
      x := x div 2;
   count := count + 1;
   if max < x then
      max := x;
   until x = 1;
put_alfa16(1, 'starting at     '); put_int(1, start); new_line(1, 1);
put_alfa16(1, 'largest  found  '); put_int(1, max);   new_line(1, 1);
put_alfa16(1, '# of iterations '); put_int(1, count); new_line(1, 1);
new_line(1, 1);
end .
