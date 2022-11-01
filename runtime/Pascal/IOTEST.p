program T;
type
   z = record f1, f2 : integer end;
var
   r, s : z;
   i : integer;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%include PASKAL_real_IO
%listing on

procedure q (i : integer);
   forward;

procedure p (t : z; var v : z; var u : integer; w : integer);
   begin
   with t do
     begin
     f1 := 0;
     f2 := 1;
     end;
   with v do
     begin
     f1 := 0;
     f2 := 1;
     end;
   u := w;
   q(2);
   end;

procedure q;
   begin
   halt(i);
   end;

begin
open(1, 1);
put_int(1, +12345678);
put_int(1, -12345678);
new_line(1, 1);
put_int(1, +1234567890);
put_int(1, -1234567890);
new_line(1, 1);
put_int_left_justified(1, +12345678);
put_int_left_justified(1, -12345678);
new_line(1, 1);
put_int_left_justified(1, +1234567890);
put_int_left_justified(1, -1234567890);
new_line(1, 2);

for i := 0 to 126 do
   begin
   put_small_int(1, i);
   put_float(1, power(2.0, i));
   put_float(1, power(2.0, -i));
   if power(2.0, i)*power(2.0, -i) <> 1 then
      put_char(1, '*');
   new_line(1, 1);
   end;
for i := 0 to 126 do
   begin
   put_small_int(1, i);
   put_fixed(1, power(2.0, i));
   put_fixed(1, power(2.0, -i));
   if power(2.0, i)*power(2.0, -i) <> 1 then
      put_char(1, '*');
   new_line(1, 1);
   end;
for i := 0 to 14 do
   begin
   put_small_int(1, i);
   put_float(1, power(10.0, i));
   put_float(1, power(10.0, -i));
   new_line(1, 1);
   end;
for i := 0 to 14 do
   begin
   put_small_int(1, i);
   put_fixed(1, power(10.0, i));
   put_fixed(1, power(10.0, -i));
   if power(2.0, i)*power(2.0, -i) <> 1 then
      put_char(1, '*');
   new_line(1, 1);
   end;
for i := 0 to 14 do
   begin
   put_small_int(1, i);
   put_float(1, (1-1e-8) * power(10.0, i));
   put_float(1, (1-1e-8) * power(10.0, -i));
   if power(2.0, i)*power(2.0, -i) <> 1 then
      put_char(1, '*');
   new_line(1, 1);
   end;
for i := 0 to 14 do
   begin
   put_small_int(1, i);
   put_fixed(1, (1-1e-8) * power(10.0, i));
   put_fixed(1, (1-1e-8) * power(10.0, -i));
   if power(2.0, i)*power(2.0, -i) <> 1 then
      put_char(1, '*');
   new_line(1, 1);
   end;
for i := 0 to 14 do
   begin
   put_small_int(1, i*power(-1, i));
   put_fixed(1, (1-1e-8) * power(10.0, i));
   put_fixed(1, (1-1e-8) * power(10.0, -i));
   if power(2.0, i)*power(2.0, -i) <> 1 then
      put_char(1, '*');
   new_line(1, 1);
   end;
for i := 0 to 14 do
   begin
   put_small_int_left_justified(1, i*power(-1, i));
   put_fixed(1, (1-1e-8) * power(10.0, i));
   put_fixed(1, (1-1e-8) * power(10.0, -i));
   if power(2.0, i)*power(2.0, -i) <> 1 then
      put_char(1, '*');
   new_line(1, 1);
   end;

space(1, 9);
put_float(1, 0);
put_fixed(1, 0);
new_line(1, 1);
space(1, 9);
put_float(1, 1.0e38);
put_float(1, 1.0e38);
new_line(1, 1);
space(1, 9);
put_float(1, 0.12345678987654321);
put_fixed(1, 0.12345678987654321);
new_line(1, 1);
space(1, 9);
put_float(1, 0.12345678087654321);
put_fixed(1, 0.12345678087654321);
new_line(1, 1);

put_alfa8(1, ' )()()()');
case_normal(1);
put_alfa16(1, ' "®©¬#%'':=()£*,/'); new_line(1,1);
case_shift(1);
put_alfa16(1, ' "®©¬#ßñ&?!%''$~:'); new_line(1,1);
case_normal(1);
put_alfa16(1, '0123456789_º;+-.'); new_line(1,1);
put_alfa16(1, '^[]<>=×÷()_£;±*,'); new_line(1,1);
put_alfa16(1, '@ABCDEFGHIJKLMNO'); new_line(1,1);
put_alfa16(1, '@abcdefghijklmno'); new_line(1,1);
PUT_ALFA16(1, '@PQRSTUVWXYZ..|\'); new_line(1,1);
put_alfa16(1, '@pqrstuvwxyz..|\'); new_line(1,1);
case_shift(1);
put_alfa16(1, '0123456789_º;+-.'); new_line(1,1);
put_alfa16(1, '^[]<>=×÷()_£;±*,'); new_line(1,1);
put_alfa16(1, '@ABCDEFGHIJKLMNO'); new_line(1,1);
put_alfa16(1, '@abcdefghijklmno'); new_line(1,1);
PUT_ALFA16(1, '@PQRSTUVWXYZ..|\'); new_line(1,1);
put_alfa16(1, '@pqrstuvwxyz..|\'); new_line(1,1);
with s do
  begin
  f1 := 0;
  f2 := 1;
  end;
with r do
  begin
  f1 := 0;
  f2 := 1;
  end;
p(r, s, r.f1, s.f2);
q(1);

end .
