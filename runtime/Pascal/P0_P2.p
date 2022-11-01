program p0_p2;

   var
      global_a, global_b : integer;

   procedure p0;
   begin global_a := global_b + 1 end;

   procedure p1 (x : integer);
   begin global_a := x + 1 end;

   procedure p2 (x, y : integer);
   begin global_a := x+1; global_b := y-1; end; {p2}

begin
global_a := 1;
global_b := 2;
p0;
p1(global_a);
p2(global_b, global_a);
end.
