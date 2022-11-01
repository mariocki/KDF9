program nesting;
   var
      i1, a : integer;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%listing on

   procedure  p2;
      var i2 : integer;

      procedure  p3;
         var i3 : integer;

         procedure  p4;
            var i4 : integer;

            procedure p5;
               var i5 : integer;

               begin
               new_line(1, 1); new_line(1, 1);
               put_alfa8(1, 'in   p5 '); new_line(1, 1);
               usercode 'M1;'; =a; end; put_small_int(1, a);
               usercode 'M2;'; =a; end; put_small_int(1, a);
               usercode 'E2M1;'; =a; end; put_small_int(1, a); new_line(1, 1);

               put_small_int(1, i1); new_line(1, 1);
               put_small_int(1, i2); new_line(1, 1);
               put_small_int(1, i3); new_line(1, 1);
               put_small_int(1, i4); new_line(1, 1);
               i5 := i4 + 10000;
               put_small_int(1, i5); new_line(1, 1);
               i1 := 12345;
               end {p5};

         begin
         new_line(1, 1); new_line(1, 1);
         put_alfa8(1, 'in   p4 '); new_line(1, 1);
         usercode 'M1;'; =a; end; put_small_int(1, a);
         usercode 'M2;'; =a; end; put_small_int(1, a);
         usercode 'E2M1;'; =a; end; put_small_int(1, a); new_line(1, 1);

         put_small_int(1, i1);  new_line(1, 1);
         put_small_int(1, i2);  new_line(1, 1);
         put_small_int(1, i3);  new_line(1, 1);
         i4 := i3 + 1000;
         put_small_int(1, i4); new_line(1, 1);
         p5;
         end {p4};

      begin
      new_line(1, 1); new_line(1, 1);
      put_alfa8(1, 'in   p3 '); new_line(1, 1);
      usercode 'M1;'; =a; end; put_small_int(1, a);
      usercode 'M2;'; =a; end; put_small_int(1, a);
      usercode 'E2M1;'; =a; end; put_small_int(1, a); new_line(1, 1);

      put_small_int(1, i1);  new_line(1, 1);
      put_small_int(1, i2);  new_line(1, 1);
      i3 := i2 + 100;
      put_small_int(1, i3); new_line(1, 1);
      p4;
      end {p3};

   begin
   new_line(1, 1); new_line(1, 1);
   put_alfa8(1, 'in   p2 '); new_line(1, 1);
   usercode 'M1;'; =a; end; put_small_int(1, a);
   usercode 'M2;'; =a; end; put_small_int(1, a);
   usercode 'E2M1;'; =a; end; put_small_int(1, a); new_line(1, 1);

   put_small_int(1, i1); new_line(1, 1);
   i2 := i1 + 10;
   put_small_int(1, i2); new_line(1, 1);
   p3;
   end {p2};

begin
open(1, 3);
i1 := 1;
   put_alfa8(1, 'pre  p2 '); new_line(1, 1);
   usercode 'M1;'; =a; end; put_small_int(1, a);
   usercode 'M2;'; =a; end; put_small_int(1, a);
   usercode 'E2M1;'; =a; end; put_small_int(1, a); new_line(1, 1);

   put_small_int(1, i1); new_line(1, 1);
p2;
   new_line(1, 1); new_line(1, 1);
   put_small_int(1, i1); new_line(1, 1);
end .
