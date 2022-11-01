program p;
   var
      ip : integer;

   procedure p1;
      var i1 : integer;

      procedure  p2;

         procedure  p3;
         var i3 : integer;

            procedure  p4;
            var i4, i5 : integer;
            begin
            i4 := i3 + 1000;
            i5 := i1 +10000;
            usercode 'M2; '; ip; i1; i3; i4; -i5 end;
            p1;
            p2;
            p3;
            p4;
            end {p3};

         begin
         i3 := i1 + 100;
         usercode ip; i1; -i3; end;
         p4;
         p3;
         end {p3};

      begin
      p3;
      p2;
      end {p2};

   begin
   if ip > 1 then
      halt(12345);
   i1 := ip + 10;
   usercode ip; -i1; end;
   ip := 2;
   p2;
   p1;
   end {p1};

begin
ip := 1;
p1;
end .
