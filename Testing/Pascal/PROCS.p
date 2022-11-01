program procs;
var i0, j0 : integer;

procedure leaf1b;
   var x : integer;
   procedure leafc;
      begin usercode x end end;
   begin usercode -j0 end end;

procedure leaf0;
   begin usercode 1 end  end;

procedure leaf1 (x : integer);
   begin usercode x end end;

procedure leaf2 (x, y : integer);
   begin usercode x; y end end;

procedure p1;
   var i1, j1 : integer;

     procedure p2;

     var i2, j2 : integer;

        procedure p3;

        var i3, j3, k3, l3 : integer;


        procedure p4;

        var i4 : integer;

        begin
        i4 := i3;
        i4 := i2;
        i4 := i1;
        i4 := i0;

        i3 := i4;
        i2 := i4;
        i1 := i4;
        i0 := i4;
        end;


        begin
        j3 := j2;
        i2 := j1;
        j1 := i0;
        j0 := i3;
        i3 := 30;
        leaf2(i3, i2);
        end;

     begin
     i2 := 0;
     i1 := 1;
     i0 := 3;
     j2 := j1;
     j1 := j0;
     i2 := j2;
     p3;
     end;

   begin
   i1 := 4;
   i0 := 5;
   j1 := j0;
   i1 := j1;
   p2;
   end;

begin
i0 := 6;
j0 := 31;
leaf0;
leaf1(11);
leaf2(21, 22);
p1;
halt(0);
end.
