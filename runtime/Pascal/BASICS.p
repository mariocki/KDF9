%checks = yes
program BASICS;
{ These are the statements Brian Wichmann used in his  }
{ 1973 paper "Basic statement times for ALGOL 60".     }
{ I have added variables r and s to show the benefit   }
{ of subrange types in generating better code for KDF9 }
label
   1;
var
   k, l, m :integer;
   r, s : 1 .. +maxint;
   x, y, z : real;

   e1 : array [1..1] of integer;
   e2 : array [1..1, 1..1] of integer;
   e3 : array [1..1, 1..1, 1..1] of integer;

   procedure p0; begin end;
   procedure p1 (x : real); begin end;
   procedure p2 (x, y : real); begin end;

begin
x := y + z;
x := 1;
x := 1.0;
x := y;
k := l + m;
x := y / z;
x := y * z;
x := y / m;
x := y / z;
k := 1;
k := round(1.0);
k := l + m;
k := l * m;
k := l div m;
k := r div s;
k := l;
x := 1;
l := round(y);
x := power(y, 2);
x := power(y, 3);
x := power(y, z);
e1[1] := 1;
e2[1,1] := 1;
e3[1,1,1] := 1;
l := e1[1];
l := e2[1,1];
l := e3[1,1,1];
goto 1;
{ NYI:
case i of
   1: ;
   2: ;
   end;
}
p0;
p1(x);
p2(x,y);
x := sin(y);
x := cos(y);
x := abs(y);
x := exp(y);
x := ln(y);
x := sqrt(y);
x := arctan(y);
usercode y; 'SIGN; '; =x end; { x := sign(y); }
if y >= 0.0 then x := trunc(y) else x := -trunc(-y+0.5); { x := entier(y); }

%checks off
x := 1;
x := 1.0;
x := y;
x := y + z;
x := y * z;
x := y / z;
k := 1;
k := round(1.0);
k := l + m;
k := l * m;
k := l div m;
k := r div s;
k := l;
x := 1;
l := round(y);
x := power(y, 2);
x := power(y, 3);
x := power(y, z);
e1[1] := 1;
e2[1,1] := 1;
e3[1,1,1] := 1;
l := e1[1];
l := e2[1,1];
l := e3[1,1,1];
goto 1;
{ NYI:
case i of
   1: ;
   2: ;
   end;
}
p0;
p1(x);
p2(x,y);
x := sin(y);
x := cos(y);
x := abs(y);
x := exp(y);
x := ln(y);
x := sqrt(y);
x := arctan(y);
usercode y; 'SIGN; '; =x end; { x := sign(y); }
if y >= 0.0 then x := trunc(y) else x := -trunc(-y+0.5); { x := entier(y); }

1:
end .
