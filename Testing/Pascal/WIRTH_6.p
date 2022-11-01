program wirth_6;
var
   i, j : integer;
   p : boolean;
begin

{T6: control structures, in comments n = # of repetitions}

if p then;                 {syl  6, ins  2,    us 17/10}
if i = j then;             {syl 10, ins  4,    us 25/18}
if i < j then;             {syl 10, ins  4,    us 25/18}
if p then else;            {syl  9, ins  3,    us 21/17}

while p do;                {syl  9, ins  3n-1, us 21n-4}
repeat until p;            {syl  6, ins  2n,   us 17n-7}

for i := 0 to  9 do;       {syl 20, ins  8n+2, us 35n+7}
for i := 1 to 10 do;       {syl 22, ins  8n+4, us 35n+9}
for i := 0 to  0 do;       {syl  4, ins  2,    us 7}
for i := 10 downto 1 do;   {syl 22, ins  8n+2, us 35n+10}
for i := 9  downto 0 do;   {syl 18, ins  6n+2, us 28n+10}

case i of
   1:;
   2:;
   3:
   end;
end.
