%checks= yes
program reals;
label
   1;
const zero = 0;
      fp_zero = 0.0;
      one = 1;
      fp_one = 1.0;
      two = 2;
      fp_two = 2.0;
      a_half = 0.5;
      pi = 3.14159265358979;
      pu = 3.54159265358979;
      e  = 2.71828182845904;
      big = 1.701411834604692e+38;
var
   x, y, z, t, u, v : real;
   i    : integer;
   k    :  0 .. maxint;
   l    :  1 .. maxint;
   n1, n2, n3 : integer;
   b : Boolean;
   a : alfa;

% include

begin
open(1, 3);

x := -1.14;
x := round(x);
x := -1.94;
x := round(x);
x := +1.14;
x := round(x);
x := +1.94;
x := round(x);
put_real(1, x);

x := -1.14;
y := trunc(x);
x := -1.94;
y := trunc(x);
x := +1.14;
y := trunc(x);
x := +1.94;
y := trunc(x);
put_real(1, y);
new_line(1, 1);

x := power(0.0, 0);
x := power(0.0, 1);
x := power(0.0, 2);
x := power(0.0, k);

x := power(1.0, k);

x := power(1.0e3, -1);
x := power(1.0e3, +1);

x := power(2.0, -1);
x := power(2.0, -10);

x := power(9.0, 0);
x := power(9.0, 1);
x := power(9.0, 2);
x := power(9.0, 5);
x := power(9.0, k);

x := power(10.0, -40);

x := power(y, 0);
x := power(y, 1);
x := power(y, 2);
x := power(y, 3);
x := power(y, 4);
x := power(y, i);
x := power(y, k);
x := power(y, z);
x := power(y, 0);
x := power(y, 1.0);
x := power(y, 2.0);
x := power(y, 3.0);
x := power(y, 4.0);

k := i;
x := 1.0;
y := 0;

v := +1234567890012345678;
v := +1234567890.0123456789e20;
v := -1234567890.0123456789e-20;
y := 0.35735/x;
z := 0.065/x;


x := sqr(3.14159265358979);
x := sqr(trunc(3.14159265358979));
x := trunc(sqr(3.14159265358979));
x := trunc(3.14159265358979);
x := trunc(pi);
x := round(pi);
x := round(pu);

x := trunc(v);
y := round(x);
z := cos(t);
v := sqrt(arctan(x));
y := exp(z);
x := ln(y);

i := 1;
k := 1;
k := i*777;
x := i*777;
y := 777;
x := 21 * 37;
y := 21 * 37.0;
x := 21.0 * 37;
y := 21.0 * 37.0;
x := k;

k := +1234567890012;
v := abs(-123.456789);


x := y * 3;
y := 3 * x;
y := k div power(2, 18);
x := y * z;

x := 0;
y := 1;
x := -1;

y := 32767;
x := -32768;
y := 32768;

v := 2.0;
v := 2.0;
v := -123.456789;

x := y;

%checks off

x := y + zero;
x := y + fp_zero;
x := zero;
x := fp_zero;

x := y - zero;
x := y - fp_zero;
x := zero - y;
x := fp_zero - y;

x := y * zero;
x := y * fp_zero;
x := zero * y;
x := fp_zero * y;


x := zero / y;
x := fp_zero / y;

x := y + one;
x := y + fp_one;
x := one;
x := fp_one;

x := y - one;
x := y - fp_one;
x := one - y;
x := fp_one - y;

x := y * one;
x := y * fp_one;
x := one * y;
x := fp_one * y;

x := y / one;
x := y / fp_one;
x := one / y;
x := fp_one / y;

x := y - two;
x := y - fp_two;
x := two - y;
x := fp_two - y;

x := y * two;
x := y * fp_two;
x := two * y;
x := fp_two * y;

x := y / two;
x := y / fp_two;
x := two / y;
x := fp_two / y;

x := y * a_half;
x := a_half * y;

x := a_half / y;
x := y / a_half;

x := y + z;
x := y - z;
x := (y - x) * z;
x := (y - x) + z;
x := y * (x + y);
x := y  /  (x + y);

x := y + k;
x := y - k;
x := (y - x) * k;
x := (y - x) + k;
x := y * (x + y);
x := y  /  (x + y);

x := y * k;
x := y  /  k;
x := t * arctan(a_half * sin(x) * cos(x) / (cos(x + y) + cos(x - y) - 1.0));


x := power(y, -4);
x := power(10.0, +40);
x := power(0.0, -2);
y := sqr(big);
y := exp(130.0);
y := exp(-130.0);
v := sqrt(-4096);
v := sqrt(-4096.0);
y := ln(0.0);
y := ln(-1.0);
x := y / zero;
x := y / fp_zero;
end.
