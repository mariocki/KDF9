%storage = 32768
%ystores = 32500
%checks = no
program ackt;

   const
      n = 8;
   function Ackermann(m, n : integer) : integer;
      label
         1;
      begin
      1: if m = 0 then
         Ackermann := n + 1
      else
      if n = 0 then
         begin
         n := 1;
         m := m - 1;
         goto 1;
         end
      else
         begin
         n := Ackermann(m, n - 1);
         m := m - 1;
         goto 1;
         end;
      end;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%include PASKAL_timing
%listing on

begin
usercode Ackermann(3, n) end;
halt(1);
end .
