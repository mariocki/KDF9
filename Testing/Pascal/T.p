program T;
type
   z = record f1, f2 : integer end;
   y = ^z;

var
   r, s : z;
   i : integer;
   p : y;

procedure q (t : z; var v : z);
   begin
   with t do
     begin
     f1 := 0;
     f2 := 1;
     end;
   with v do
     begin
     f1 := 0;
     f2 := 1;
     end;
   with s do
      begin
      f1 := 1;
      f2 := -2;
      end;
   end;

begin
with s do
  begin
  f1 := 0;
  f2 := 1;
  end;
with r do
  begin
  f1 := 0;
  f2 := 1;
  end;
with p^ do
   begin
   f1 := 1;
   f2 := -2;
   end;
end .
