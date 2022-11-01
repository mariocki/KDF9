program nesting;
   var
      i1, a : integer;

   procedure  p2;
      var i2 : integer;

      procedure  p3;
         var i3 : integer;

      begin
      i3 := i2 + 100;
      halt(1);
      end {p3};

   begin
   i2 := i1 + 10;
   p3;
   end {p2};

begin
i1 := 1;
p2;
end .
