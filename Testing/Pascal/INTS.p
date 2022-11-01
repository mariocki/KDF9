%checks= yes
program ints;
const
   zero = 0;
   one  = 1;
   three = 3;
var
   i, j : integer;
   l    : -32768 .. 0;
   m    : 1 .. maxint;
   n    : -32768 .. -1;
   k, p, q :  0 .. maxint;
   r : real;
   c : char;
begin
i := trunc(power(i, 0));
j := ord(c);
k := trunc(ord(c));

i := trunc(i);
i := trunc(r);
i := trunc(power(r, 0));
i := trunc(power(r, 0.0));
i := trunc(power(r, 1.0));
i := trunc(power(r, 2.0));
i := trunc(power(2.0, 5.0));
i := trunc(power(r, i));
i := trunc(power(r, r));

i := #0;
i := #1;
i := #101;
i := #7777777777777777;

i := j + zero;

k := k + (k + 1);
k := k - (k + 1);
k := k div (k + 1);
k := k mod (k + 1);
k := (k - 1) + (k + 1);
k := (k - 1) - (k + 1);
k := (k - 1) div (k + 1);
k := (k - 1) mod (k + 1);

j := zero;
k := 1;
n := -1;

i := +32767;
n := -32768;
j := +32768;
j := -32769;
k := +32768;
m := maxint;

i := j;
k := i;
m := k;
i := m;

i := trunc(3.14159);
i := round(3.14159);
i := trunc(2.0 * 3.0);
i := 2 + 3;
i := 2 * 3;

j := i + zero;
j := i + 1;
j := i + 2;
j := i + 3;

i := zero - j;
i := 1 - j;
i := 2 - j;
i := 3 - j;

l := i - zero;
l := j - 1;
l := k - 2;
l := i - 3;

i := j - zero;
i := j - 1;
i := j - 2;
i := j - 3;

l := j - zero;
l := j - 1;
l := j - 2;
l := j - 3;

%checks on
l := i - zero;
l := j - 1;
l := k - 2;
l := i - 3;
%checks off

j := i * zero;
j := i * 1;
j := i * 2;
j := i * 3;
j := i * 4;
j := i * 5;
j := i * 6;
j := i * 7;
j := i * 8;
j := i * 9;
j := i * 10;
j := i * 12;
j := i * 15;
j := i * 17;
j := i * 18;

i := zero * j;
i := 1 * j;
i := 2 * j;
i := 3 * j;
i := 4 * j;
i := 5 * j;
i := 6 * j;
i := 7 * j;
i := 8 * j;
i := 9 * j;
i :=10 * j;
i :=11 * j;
i :=12 * j;
i :=13 * j;
i :=60 * j;
i :=68 * j;


l := i * zero;
l := j * 1;
l := k * 2;
l := i * 3;

%checks off
l := i * zero;
l := j * 1;
l := k * 2;
l := i * 3;
%checks on

j := i div 1;
j := i div 2;
j := i div 3;
j := i div 64;

i := 1 div j;
i := 2 div j;
i := 3 div j;

l := j mod 1;
l := k mod 2;
l := i mod 3;
l := i mod 4;

%checks off
l := j mod 1;
l := k mod 2;
l := i mod 3;
l := i mod 4;

j := i mod 1;
j := i mod 2;
j := i mod 3;
j := i mod 4;
j := 1 mod 4;
j := 1 mod k;

k := 1;
k := j;
k := l;
k := j div 4;
k := j mod 4;
k := j * 8;

j := k div 4;
j := k mod 4;
j := k * 8;

j := k div 4;
j := i * 8;

k := j mod 7;
k := i mod 8;


l := 1;
m := 0;
n := 0;
k := 10;
k := power(0, 0);
k := power(0, 1);
k := power(0, 2);
k := power(0, i);
k := power(0, l);
k := power(1, k);
k := power(2, 3);
k := power(9, 0);
k := power(9, 1);
k := power(9, 2);
k := power(9, 5);
k := power(9, k);
k := power(k, 0);
k := power(k, 1);
k := power(k, 2);
k := power(k, 3);
k := power(k, 4);
k := power(k, k);
k := power(k, i);

%checks off
i := i - (j*k - 1) div sqr(abs(j - k) + l);
i := i + j - 7 * ((k + 11*i + 19*j) div 433);
q := q - (p*k - 1) div sqr(abs(p - k) + l);
i := q + p - 7 * ((k + 11*p + 19*q) div 433);
i := q + p - 7 * ((k + 11*p + 19*q) div (400+33));
i := q + p - 7 * (k div (400+33));
%checks on

i := i - (j*k - 1) div sqr(abs(j - k) + l);
i := i + j - 7 * ((k + 11*i + 19*j) div 433);
q := q - (p*k - 1) div sqr(abs(p - k) + l);
i := q + p - 7 * ((k + 11*p + 19*q) div 433);
i := q + p - 7 * ((k + 11*p + 19*q) div (400+30+three)*one);
i := q + p - 7 * (k div (400+33));

j := i div 0;
i := j div zero;
j := k div 0;

k := power(0, -2);
k := power(2, 48);
k := power(2, -1);
k := power(k, -4);
{i := trunc(power(i, 1.0));}
{i := trunc(power(i, r));}

end .
