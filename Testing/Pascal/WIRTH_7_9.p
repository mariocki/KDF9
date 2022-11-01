program WIRTH_7_9;
{ static data structures }
type
   index = 1..20;
   row = array [index] of real;
   complex = record re, im : real end;
   item = record x : real; y : row; z : complex end;
var
   x : real;
   z : complex;
   i, j : index;
   p : boolean;
   d : item;
   a, b : row;
   c : array [index] of row;
   u : array [index] of item;
   r, s, t : set of index;
begin

{T7: arrays}
x := a[5];
x := a[i];
x := a[i+1];
x := c[5, 10];
x := c[i, j];
x := c[i+1, j-1];

{T8: records and arrays}
x := d.x;
x := d.z.im;
x := d.y[i];
x := u[i].x;
x := u[i].y[j];
z := d.z;
{d := u[i];}

{T9: sets}
r := [5];
s := [2, 3, 5, 7, 11, 13, 17, 19];
t := [i];
t := [i, j];
r := s + t;                         {syl 10, ins  3,    us 19}
r := s * t;                         {syl 10, ins  3,    us 19}
r := s - t;                         {syl 11, ins  5,    us 20}
if i in s then;                     {syl 15, ins  6,    us 34/27}
p := i in s;                        {syl 15, ins  6,    us 29+t}
end.
