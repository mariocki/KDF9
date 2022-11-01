program vpars;

   var
      i, j : 0 .. maxint;

procedure varpar (var p : integer);

   var q : integer;

   procedure inner (var v : integer);
      begin
      v := p + 10000;
      end;

   begin
   p := 45;
   p := i + p;       { p/i = 90 }
   j := p + sqr(i);  { j = 8190 }
   inner(i);         { p/i = 10090 } usercode i end;
   inner(p);         { p/i = 20090 } usercode p end;
   inner(q);         {   q = 30090 } usercode q end;
   usercode p end;
   end;

begin
i := 0;
j := 100;
i := 200;
varpar(i);
usercode i; j end;  { 20090, 8190, 30090, 20090, 8190 }

end .
