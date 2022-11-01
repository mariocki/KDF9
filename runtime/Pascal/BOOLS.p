%checks = yes
program bools;
var
   i, j : integer;
   k, l, m, n : 0 .. maxint;
   b, c, d, x: boolean;

begin
b := false = false;
b := false = true;
b := true  = false;
b := true  = true;

b := false <> false;
b := false <> true;
b := true  <> false;
b := true  <> true;

b := false <= false;
b := false <= true;
b := true  <= false;
b := true  <= true;

b := false <  false;
b := false <  true;
b := true  <  false;
b := true  <  true;

b := false >= false;
b := false >= true;
b := true  >= false;
b := true  >= true;

b := false >  false;
b := false >  true;
b := true  >  false;
b := true  >  true;

b := true and x;
b := true or x;
b := x or true;
b := x and true;

b := false and x;
b := false or x;
b := x or false;
b := x and false;

b := c and x;
b := c or x;
b := x or c;
b := x and c;

c := not x or true;
c := not x and true;
c := true and not x;
d := not true or not x;
d := not true and not x;
c := not true or x;

b := false;
b:= true;

b := not false;
b:= not true;

b := true or false;
b := true and false;
c := not true or false;
c := true and not false;
d := not true or not false;
d := not true and not false;

b := x or  false;
b := x and false;
c := x or  true;
c := x and true;
c := not x or false;
c := not x and false;
c := x and not false;
d := not x or not false;
d := not x and not false;

b := (c and d) or true;
b := (c or d) and false;

b := (c and d) or false;
b := (c or d) and true;

end .
