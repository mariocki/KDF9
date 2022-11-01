%checks = yes
program forl;
label
   1;
var
   i, j : integer;
   k, l, m, n : 0 .. maxint;
begin

for i := 1 to 0 do
   for i := 0 downto 1 do goto 1;

for i := 0 to 1 do
   for i := 0 downto 1 do goto 1;
goto 1;
for i := 3 to j do;
for i := k to j do;

for i := 0 to 9 do
   k := i;

for i := 1 to 10 do
   k := i;

for i := 9 downto 0 do
   k := i;

for i := 10 downto 1 do
   k := i;

for i := 0 to 0 do
   k := i;

for i := 0 downto 0 do
   k := i;

for i := 1 to 1 do
   k := i;

for i := 1 downto 1 do
   k := i;

for m := -2 to -1 do
   k := i;

for j := 1 to 0 do
   k := i;

for m := -2 downto +1 do
   k := i;

for j := 0 to j do
   m := j;

for j := i to 0 do
   m := j;

for j := 0 downto j do
   m := j;

for j := i downto 0 do
   m := j;

for i := 0 to 9 do
   for j := m to i do
      k := i;

for j := -2 to -1 do
   k := i;

for i := 9 downto 1 do
   m := i;

for j := i downto m do
   i := j;

for i := m downto 0 do
   for j :=  i downto m do
      n := (i + j);

for i := 9 downto j do
   for j :=  i downto m do
      n := (i + j);

for j := 1 to 0 do
   k := i;

for k := j+1 to j*2 do
   for j := k div 2 to i-1 do
      begin
      for i := j to k do
         halt(0);
      k := abs(i);
      for i := j to k do
         halt(0);
      end;

for i := 0 to 9 do
   k := sqr(i);

for j := k to i-1 do
   k := abs(i);

for i := 0 to 9 do
   for j := k to i do
      k := (i + j);

for i := 9 downto 1 do
   k := sqr(i);

for j := i downto k do
   i := abs(j);

for i := 9 downto 1 do
   for j :=  i downto k do
      l := (i + j);

%checks off

for i := 0 to 9 do
   k := i;

for i := 1 to 10 do
   k := i;

for i := 9 downto 0 do
   k := i;

for i := 10 downto 1 do
   k := i;

for i := 0 to 0 do
   k := i;

for i := 0 downto 0 do
   k := i;

for i := 1 to 1 do
   k := i;

for i := 1 downto 1 do
   k := i;

for m := -2 to -1 do
   k := i;

for j := 1 to 0 do
   k := i;

for m := -2 downto +1 do
   k := i;

for j := 0 to j do
   m := j;

for j := i to 0 do
   m := j;

for j := 0 downto j do
   m := j;

for j := i downto 0 do
   m := j;

for i := 0 to 9 do
   for j := m to i do
      k := i;

for j := -2 to -1 do
   k := i;

for i := 9 downto 1 do
   m := i;

for j := i downto m do
   i := j;

for i := m downto 0 do
   for j :=  i downto m do
      n := (i + j);

for i := 9 downto j do
   for j :=  i downto m do
      n := (i + j);

for j := 1 to 0 do
   k := i;

for k := j+1 to j*2 do
   for j := k div 2 to i-1 do
      begin
      for i := j to k do
         halt(0);
      k := abs(i);
      for i := j to k do
         halt(0);
      end;

for i := 0 to 9 do
   k := sqr(i);

for j := k to i-1 do
   k := abs(i);

for i := 0 to 9 do
   for j := k to i do
      k := (i + j);

for i := 9 downto 1 do
   k := sqr(i);

for j := i downto k do
   i := abs(j);

for i := 9 downto 1 do
   for j :=  i downto k do
      l := (i + j);

1:
for i := 1 to 1000000 do ;
for i := 1 to 1000000 do ;
end .
