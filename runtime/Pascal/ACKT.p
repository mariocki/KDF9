%storage = 32768
%ystores = 32500
%checks = no
program ackt;

   const
      n = 8;

   var
      j, calls, a : integer;
      t1, t2, i1, i2 : real;

   function Ackermann(m, n : integer) : integer;
      label
         1;
      begin
      1: if m = 0 then
         Ackermann := n + 1
      else
      if n = 0 then
         begin
         n := 1;
         m := m - 1;
         goto 1;
         end
      else
         begin
         n := Ackermann(m, n - 1);
         m := m - 1;
         goto 1;
         end;
      end;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%include PASKAL_timing
%listing on

begin
open(1, 3);
calls := (128*power(4, n)  - 120*power(2, n) + 9*n + 37) div 3;
put_alfa8(1, 'CALLS   ');
put_small_int(1, calls); new_line(1, 1);
t1 := Time;
i1 := ICR;
j := Ackermann(3, n);
i2 := ICR - i1;
t2 := (Time - t1);

put_alfa8(1, 'RESULT  ');
put_small_int(1, j); new_line(1, 1);
put_alfa8(1, 'us/call ');
put_small_int(1, round(t2 / calls)); new_line(1, 1);
put_alfa8(1, 'ins/call');
put_small_int(1, round(i2 / calls)); new_line(1, 1);
put_alfa8(1, 'ns/ins  ');
put_small_int(1, round(t2 * 1e3 / i2)); new_line(1, 1);
end .
