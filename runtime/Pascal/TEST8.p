program test8 {(output)};
const
   size = 128;
type
   t = 1..1;
   a = array[t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t] of integer;
   s = packed array[1..size] of char;

var
   deeplynestedrecord :
      record
      f : array[t,t,t,t,t,t,t,t,t,t] of
             array[t,t,t,t,t,t,t,t,t,t] of
                array[t,t,t,t,t,t,t,t,t,t] of a
      end;
   string : s;
   n : 1..size;

begin
for n := 1 to size do
   string[n] := chr((n + 1) mod 64);
n := ord(deeplynestedrecord.f[1,1,1,1,1,1,1,1,1,1][1,1,1,1,1,1,1,1,1,1][1,1,1,1,1,1,1,1,1,1][1])
end (* test8 *) .
