program controls;
label
   1;
const
   deffo = true;
   noway = false;
var
   i, j : integer;
   k, l, m, n : 0 .. maxint;
   b, c, d: boolean;
   p, q : char;
begin
for p := 'a' to 'z' do
   begin
   i := ord(p);
   q := chr(i);
   end;

if false then
   halt(888);

if true then
   halt(777);

if b then
   halt(888);

if false then
   halt(888)
else
   halt(777);

if true then
   halt(999)
else
   halt(666);

i := 2;
j := 1;
k := 3;

b := i > j;

if b or odd(sqr(k)) then
   goto 1;

while deffo or b do
   i := pred(i);

while noway and c do
   i := pred(i);

while i > 1 do
   i := pred(i);

while i < 0 do
  while k = 0 do
     begin
     i := i + 1;
     k := k div 2;
     end;

repeat
   j := j - 1;
   i := i + k
until deffo or c;

repeat
   j := j - 1;
   i := i + k
until noway and b;

repeat
   j := j - 1;
   i := i + k
until j = 0;

if 7*i > 9*j then
   while i <> j do
     while k > 0 do
        i := i + 1
else
   begin
   j := i - k;
   goto 1;
   end;

1:
end .
