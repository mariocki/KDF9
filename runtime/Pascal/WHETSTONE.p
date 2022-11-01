%checks = no
program WHETSTONE;

type
  four_of = array [1..4] of real;

var
ModuleNo, value, number : integer;
i, j , k, l, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, reps, begins, starts, takes : integer;
x1, x2, x3, x4, x, y, z , t, t1, t2, a, b, c : real;
e : four_of;
fail : Boolean;

%listing off
%include paskal_char_io
%include paskal_int_io
%include paskal_real_io
%include paskal_timing
%listing on

procedure  pa{(ep : four_of)};
   var
      j : integer;
   begin
   j := 0;
   repeat
{this: }
      e[1] := (e[1] + e[2] + e[3] - e[4])  * t;
      e[2] := (e[1] + e[2] - e[3] + e[4])  * t;
      e[3] := (e[1] - e[2] + e[3] + e[4])  * t;
      e[4] := (-e[1] + e[2] + e[3] + e[4]) / t2;
{should be this: }
{
      ep[1] := (ep[1] + ep[2] + ep[3] - ep[4])  * t;
      ep[2] := (ep[1] + ep[2] - ep[3] + ep[4])  * t;
      ep[3] := (ep[1] - ep[2] + ep[3] + ep[4])  * t;
      ep[4] := (-ep[1] + ep[2] + ep[3] + ep[4]) / t2;
 }
      j := j + 1;
      until j = 6;
   end;

procedure  p0;
   begin
{this: }
   usercode
      @e[1]; 'NEG; NOT; =M8;'; k; '=M9; M8M9;';
      @e[1]; 'NEG; NOT; =M7;'; j; '=M6; =M7M6;';
      @e[1]; 'NEG; NOT; =M8;'; l; '=M9; M8M9;';
      @e[1]; 'NEG; NOT; =M7;'; k; '=M6; =M7M6;';
      @e[1]; 'NEG; NOT; =M8;'; j; '=M9; M8M9;';
      @e[1]; 'NEG; NOT; =M7;'; l; '=M6; =M7M6;';
      end;
{should be: }
{
   e[j] := e[k];
   e[k] := e[l];
   e[l] := e[j];
}   end;

procedure  p3(x, y : real; var z : real );
   begin
   x := t * (x + y);
   y := t * (x + y);
   z := (x + y) / t2;
   end;

procedure pout(n, j , k : integer; x1, x2, x3, x4 : real);
   begin
   put_alfa8(1, 'n=ииииии'); put_small_int_field(1, n, 4);
   put_alfa8(1, ' j=иииии'); put_small_int_field(1, j, 4);
   put_alfa8(1, ' k=иииии'); put_small_int_field(1, k, 4);
   put_alfa8(1, ' x1=ииии'); put_fixed(1, x1);
   put_alfa8(1, 'иx2=ииии'); put_fixed(1, x2);
   put_alfa8(1, 'иx3=ииии'); put_fixed(1, x3);
   put_alfa8(1, 'иx4=ииии'); put_fixed(1, x4);
   new_line(1, 1);
   end;

begin
open(1, 3);
begins := Time;


{ initialise constants }
t  := 0.499975;
t1 := 0.50025;
t2 := 2.0;

{  read i, controlling total weight:
   if i=1 the total weight is one million Whetstone instructions:
   Walgol ran at 2.4KWIPS, so this should take 417 KDF9 CPU seconds }
i  := 1;
i  := i  * 10;
n1 := 0;
n2 := 12  * i;
n3 := 14  * i;
n4 := 345 * i;
n5 := 0;
n6 := 210 * i;
n7 := 32  * i;
n8 := 899 * i;
n9 := 616 * i;
n10 := 0;
n11 := 93 * i;


{  module 1: simple identifiers }
x1 := +1.0;
x2 := -1.0;
x3 := -1.0;
x4 := -1.0;
for  i := 1 to n1 do
   begin
   x1 := (x1 + x2 + x3 - x4)  * t;
   x2 := (x1 + x2 - x3 + x4)  * t;
   x3 := (x1 - x2 + x3 + x4)  * t;
   x4 := (-x1 + x2 + x3 + x4) * t;
   end;
pout(n1, n1, n1, x1, x2, x3, x4);


{  module 2: array elements }
e[1] := +1.0;
e[2] := -1.0;
e[3] := -1.0;
e[4] := -1.0;
for  i := 1 to n2 do
   begin
   e[1] := (e[1] + e[2] + e[3] - e[4])  * t;
   e[2] := (e[1] + e[2] - e[3] + e[4])  * t;
   e[3] := (e[1] - e[2] + e[3] + e[4])  * t;
   e[4] := (-e[1] + e[2] + e[3] + e[4]) * t;

   end;
pout(n2, n3, n2, e[1], e[2], e[3], e[4]);


{  module 3: array  as parameter }
for  i := 1 to n3 do
   pa{(e)};
pout(n3, n2, n2, e[1], e[2], e[3], e[4]);


{  module 4: conditional jumps }
j := 1;
for  i := 1 to n4 do
   begin
   if  j = 1 then
      j := 2
   else
      j := 3;
   if  j > 2 then
      j := 0
   else
      j := 1;
   if  j < 1 then
      j := 1
   else
      j := 0;
   end;
pout(n4, j , j , x1, x2, x3, x4);


{  module 5: omitted }


{  module 6: integer  arithmetic }
j := 1;
k := 2;
l := 3;
for  i := 1 to 2100 do
   begin
   j := j * (k - j ) * (l - k);
   k := l * k - (l - j ) * k;
   l := (l - k) * (k + j );
{this: }
   usercode
   j + k + l; 'SET47; FLOAT;'; @e[1]-1; l-1; '+; =M8; =E0M8;';
   j * k * l; 'SET47; FLOAT;'; @e[1]-1; k-1; '+; =M8; =E0M8;';
   end;
{should be this: }
{
   e[l - 1] := j + k + l;
   e[k - 1] := j * k * l;
}
   end;
pout(n6, j , k, e[1], e[2], e[3], e[4]);


{  module 7: trig. functions }
x := 0.5;
y := 0.5;
for  i := 1 to n7 do
   begin
   x := t * arctan(t2 * sin(x) * cos(x) / (cos(x + y) + cos(x - y) - 1.0));
   y := t * arctan(t2 * sin(y) * cos(y) / (cos(x + y) + cos(x - y) - 1.0));
   end;
pout(n7, j , k, x, x, y, y);


{  module 8: procedure  calls }

x := 1.0;
y := 1.0;
z := 1.0;
for  i := 1 to n8 do
   begin
{put_int(1, i); put_int(1, n8); new_line(1, 1);}
   p3(x, y, z );
   end;
pout(n8, j , k, x, y, z , z );


{  module 9: array  references }

j := 1;
k := 2;
l := 3;
e[1] := 1.0;
e[2] := 2.0;
e[3] := 3.0;
for  i := 1 to n9 do
   p0;
pout(n9, j , k, e[1], e[2], e[3], e[4]);


{  module 10: integer  arithmetic }
j := 2;
k := 3;
for  i := 1 to n10 do
   begin
   j := j + k;
   k := j + k;
   j := k - j ;
   k := k - j - j ;
   end;
pout(n10, j , k, x1, x2, x3, x4);


{  module 11: standard functions }
x := 0.75;
for  i := 1 to n11 do
   begin
   x := sqrt(exp(ln(x)/t1));
   end;
pout(n11, j , k, x, x, x, x);

takes := Time - begins;
put_alfa8(1, 'KWI/sec:');
put_small_int_field(1, round(1.0e9 / takes), 3);
new_line(1, 1);
end.
