%checks = no

program powers;

const
   max_x = 31;

var
   n : 0 .. maxint;
   p, q, r, s : real;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%include PASKAL_real_IO
%listing on

begin
for n := 0 to 127 do
   begin
   p := n;
   put_small_int(0, n);
   put_float(0, power(2.0, n));
   put_float(0, power(2.0, p));
   q := power(2.0, p) - power(2.0, n);
   put_float(0, q / power(2.0, n));
   if n < 47 then
      begin
      put_int(0, power(2, n));
      s := power(2.0, p) - power(2, n);
      put_float(0, s / power(2, n));
      end;
   new_line(0, 1);
   end;
end .
