%checks = yes
program Series;

label
   1;

const
   max_x = 1000;
   x = 0.85;

var
   sum1, sum2 : real;
   old : real;
   limit : real;
   n   : 0 .. max_x;
   i : integer;
   denom : real;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%include PASKAL_real_IO
%listing on

begin
open(1, 3);
sum1 := 0.0;
old := 0.0;
limit := 1.0/(1.0 - x);
for n := 0 to max_x do
   begin
   sum1 := sum1 + power(x, n);
   if sum1 = old then goto 1;
   old := sum1;
   end;

1:
put_alfa8(1, 'terms:  ');
put_int(1, n); new_line(1, 1);
put_alfa8(1, 'sum1:   ');
put_fixed(1, sum1); new_line(1, 1);
put_alfa8(1, 'limit:  ');
put_fixed(1, limit); new_line(1, 1);
put_alfa8(1, 'error:  ');
put_fixed(1, (sum1 - limit)/limit); new_line(1, 1);

sum1 := x;
denom := 1;
for i := 1 to 10 do
   begin
   denom := - denom * 2*i * (2*i + 1);
   sum1 := sum1 + power(x, 2*i + 1) / denom;
   end;
put_alfa8(1, 'sum1,sin');
put_fixed(1, sum1);
put_fixed(1, sin(x));
new_line(1, 1);

sum2 := 1;
denom := 1;
for i := 1 to 10 do
   begin
   denom := - denom * 2*i * (2*i - 1);
   sum2 := sum2 + power(x, 2*i) / denom;
   end;
put_alfa8(1, 'sum2,cos');
put_fixed(1, sum2);
put_fixed(1, cos(x));
new_line(1, 1);

put_alfa16(1, 'should be 1.0:  ');
put_fixed(1, sqr(sum1) + sqr(sum2));
new_line(1, 1);

end .
