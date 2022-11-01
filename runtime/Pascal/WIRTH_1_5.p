program WIRTH_1_5;
var
   i, j, k : integer;
   m, n : 0..9999;
   w1 : -17592186044415 .. 0; { - maxint div 8 }
   w2 : -17592186044416 .. 0; { - maxint div 8 + 1 }
   p, q : boolean;
   c, d : char;
   x, y, z : real;
begin

{T1: integer expressions}
i := 0;                    {}
j := 10;
k := i;
k := -i;
k := i + j;
k := i * j;
k := i div j;
k := i mod j;
k := abs(i);
k := sqr(i);
k := trunc(x);
k := ord(c);

{T2: boolean expressions}
p := true;
p := q;
p := not q;
p := p or q;
p := p and q;
p := not p or q;
p := not p and q;
p := p and not q;
p := p or not q;

p := (p and not q) or (p or not q);
p := p and not q or not q;

p := i < j;
p := i <= j;
p := i = j;
p := p < q;
p := (not p and q) <= (not p or q);
p := p > q;
p := p >= q;
p := p <> q;
p := p = q;

{T3: character expressions}
c := 'D';
c := d;
c := chr(i);

{T4: real expressions}
x := 1.0;
x := y;
x := -y;
x := i;
z := x + y;
z := x * y;
z := z / y;
z := abs(x);
z := sqr(x);

{T5: optimization of integer arithmetic}
i := 2 * j;

i := 4 * j;

i := 8 * j;

i := 7 * w1;
i := 7 * w2;
i := 7 * m;
i := 7 * j;

i := 66 * w1;
i := 66 * w2;
i := 66 * m;
i := 66 * j;

i := j div 2;
i := n div 2;
m := n mod 8;
end.
