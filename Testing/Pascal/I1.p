%checks = no
program I1;

type
   natural = 0 .. maxint;

var
   t1, t2, t3 : natural;
   i1, i2, i3 : natural;

   x, y, z, t: real;
   a, b, c, d : real;

   e1 : array [1..1] of integer;
   e2 : array [1..1, 1..1] of integer;
   e3 : array [1..1, 1..1, 1..1] of integer;
   i, j, k, l, m, n : natural;

procedure open (stream, device_type : integer);
   {stream in 0..9; device_type as in the TSD: 0=FW, 1=TP, 3=LP, 5=TP, 7=CP }
   begin
   if stream <> 0 then
      usercode
      stream;
      'DUP; SET 9; SIGN; J2P117>Z; =M15;';
      device_type;
      'SET 5; OUT; =V0P119M15;';
      end;
   end;

procedure put_char (stream : integer; ch : char);
   begin
   usercode
      ch;
      'ZERO; NOT; SHL+6; OR; =V20P119; V10P119; =Q15;';
      stream;
      '=M14; V0P119M14; =C15; POAQ15;';
      end;
   end;

procedure new_line (stream, count : integer);
   begin
   for count := 1 to count do
      put_char(stream, chr(#02));
   end;

procedure space (stream, count : integer);
   begin
   for count := 1 to count do
      put_char(stream, ' ');
   end;

procedure put_alfa8 (stream : integer; a : alfa8);
   begin
   usercode a; '=V20P119; V10P119; =Q15;'; stream; '=M14; V0P119M14; =C15; POAQ15;'; end;
   end;

procedure put_alfa16 (stream : integer; a : alfa16);
   begin
   usercode a; '=V20P119; = V21P119; V11P119; =Q15;'; stream; '=M14; V0P119M14; =C15; POAQ15;'; end;
   end;

procedure put_int (stream, number : integer);
   var
      abs_val  : 0 .. maxint;
      digits   : integer;
      negative : boolean;
   begin
   negative := number < 0;
   abs_val := abs(number) div 100000000;
   if abs_val <> 0 then
      begin
      digits := 7;
      repeat
         digits := digits - 1;
         until (digits = 0) or (abs_val div power(10, digits) <> 0);
      space(stream, 6-digits);
      if digits <> 6 then
         begin
         if number < 0 then
            put_char(stream, '-')
         else
            put_char(stream, ' ');
         usercode
            abs_val div 1000000;
            'V19P119; REV; FRB; V18P119; OR;';
            (8-digits) * 6;
            '=C15; SHLC15; NC15; SHLC15; =V20P119; V10P119; =Q15;';
            stream;
            '=M14; V0P119M14; =C15; POAQ15;';
            end;
         end;
      end
   else
      space(stream, 6);
   abs_val := abs(number) mod 100000000;
   digits := 9;
   repeat
      digits := digits - 1;
      until (digits = 0) or (abs_val div power(10, digits) <> 0);
   space(stream, 8-digits);
   if negative then
      put_char(stream, '-')
   else
      put_char(stream, ' ');
   usercode
      abs_val;
      'V19P119; REV; FRB; V18P119; OR;';
      (7-digits) * 6;
      '=C15; SHLC15; ZERO; NOT; SET -48; =+C15;; SHLC15; OR; =V20P119; V10P119; =Q15;';
      stream;
      '=M14; V0P119M14; =C15; POAQ15;';
      end;
   end;

procedure put_real (stream : integer; number : real);
   var
      abs_val  : 0 .. maxint;
      digits   : integer;
      negative : boolean;
   begin
   put_int(stream, trunc(number));
   number := abs(number);
   put_char(stream,  '.');
   {put_int(stream, trunc((number - trunc(number)) * 100000000));}
   usercode
      trunc((number - trunc(number)) * 100000000);
      'V19P119; REV; FRB; V18P119; OR; =V20P119; V10P119; =Q15;';
      stream;
      '=M14; V0P119M14; =C15; POAQ15;';
      end;
   end;

function ICR : integer;
   begin
   usercode 'SET 99; OUT;'; =ICR end;
   end;

function Time : integer;
   begin
   usercode 'SET 17; OUT; REV; ERASE; SET 23; FLOAT;'; 1.0e6; 'XF; FIX; SET47; -; =C15; SHAC15;';  =Time; end;
   end;

procedure pre (s : alfa8);
   begin
   new_line(1, 1);
   put_alfa8(1, s);
   i1 := ICR;
   t1 := Time;
   end ;

procedure post;
   var r2 : real;
   begin
   i2 := ICR - i1 - i3;
   t2 := Time - t1 - t3;
   put_int(1, i2 div 12);
   put_int(1, t2 div 12);
   new_line(1, 1);
   end;

begin
open(1, 3);

x := 1.0;
y := 1.0;
z := 1.0;
l := 1;
k := 1;
m := 1;

i3 := 0; t3 := 0;
pre('null    ');
;
post;
i3 := i2; t3 := t2;

pre('x := 1  ');
   x :=1;x :=1;x :=1;x :=1;x :=1;x :=1;
   x :=1;x :=1;x :=1;x :=1;x :=1;x :=1;
post;

pre('x := 1.0');
   x :=1.0;x :=1.0;x :=1.0;x :=1.0;x :=1.0;x :=1.0;
   x :=1.0;x :=1.0;x :=1.0;x :=1.0;x :=1.0;x :=1.0;
post;

pre('x := y  ');
   x := y;x := y;x := y;x := y;x := y;x := y;
   x := y;x := y;x := y;x := y;x := y;x := y;
post;

pre('x := y+z');
   x := y  + z;x := y  + z;x := y  + z;x := y  + z;x := y  + z;x := y  + z;
   x := y  + z;x := y  + z;x := y  + z;x := y  + z;x := y  + z;x := y  + z;
post;

pre('x := y*z');
   x := y  * z;x := y  * z;x := y  * z;x := y  * z;x := y  * z;x := y  * z;
   x := y  * z;x := y  * z;x := y  * z;x := y  * z;x := y  * z;x := y  * z;
post;

pre('x := y/z');
   x := y  / z;x := y  / z;x := y  / z;x := y  / z;x := y  / z;x := y  / z;
   x := y  / z;x := y  / z;x := y  / z;x := y  / z;x := y  / z;x := y  / z;
post;

pre('k := 1  ');
   k := 1;k := 1;k := 1;k := 1;k := 1;k := 1;
   k := 1;k := 1;k := 1;k := 1;k := 1;k := 1;
post;

pre('k := 1.0');
   k := round(1.0);k := round(1.0);k := round(1.0);k := round(1.0);k := round(1.0);k := round(1.0);
   k := round(1.0);k := round(1.0);k := round(1.0);k := round(1.0);k := round(1.0);k := round(1.0);
post;

pre('k := l+m');
   k := l + m;k := l + m;k := l + m;k := l + m;k := l + m;k := l + m;
   k := l + m;k := l + m;k := l + m;k := l + m;k := l + m;k := l + m;
post;

pre('k := l*m');
   k := l * m;k := l * m;k := l * m;k := l * m;k := l * m;k := l * m;
   k := l * m;k := l * m;k := l * m;k := l * m;k := l * m;k := l * m;
post;

pre('k := l÷m');
   k := l div m;k := l div m;k := l div m;k := l div m;k := l div m;k := l div m;
   k := l div m;k := l div m;k := l div m;k := l div m;k := l div m;k := l div m;
post;

pre('k := l  ');
   k := l;k := l;k := l;k := l;k := l;k := l;
   k := l;k := l;k := l;k := l;k := l;k := l;
post;

pre('x := l  ');
   x := l;x := l;x := l;x := l;x := l;x := l;
   x := l;x := l;x := l;x := l;x := l;x := l;
post;

pre('l := y  ');
   l := round(y);l := round(y);l := round(y);l := round(y);l := round(y);l := round(y);
   l := round(y);l := round(y);l := round(y);l := round(y);l := round(y);l := round(y);
post;


pre('x := y:2');
   x := sqr(y);x := sqr(y);x := sqr(y);x := sqr(y);x := sqr(y);x := sqr(y);
   x := sqr(y);x := sqr(y);x := sqr(y);x := sqr(y);x := sqr(y);x := sqr(y);
post;

pre('x := y:3');
   x := sqr(y)*y;x := sqr(y)*y;x := sqr(y)*y;x := sqr(y)*y;x := sqr(y)*y;x := sqr(y)*y;
   x := sqr(y)*y;x := sqr(y)*y;x := sqr(y)*y;x := sqr(y)*y;x := sqr(y)*y;x := sqr(y)*y;
post;

pre('x := y:z');
   x := power(y, z);x := power(y, z);x := power(y, z);x := power(y, z);x := power(y, z);x := power(y, z);
   x := power(y, z);x := power(y, z);x := power(y, z);x := power(y, z);x := power(y, z);x := power(y, z);
post;

pre('e1(1):=1');
   e1 [ 1 ] := 1;e1 [ 1 ] := 1;e1 [ 1 ] := 1;e1 [ 1 ] := 1;e1 [ 1 ] := 1;e1 [ 1 ] := 1;
   e1 [ 1 ] := 1;e1 [ 1 ] := 1;e1 [ 1 ] := 1;e1 [ 1 ] := 1;e1 [ 1 ] := 1;e1 [ 1 ] := 1;
post;

pre('e2(1,1) ');
   e2 [1,1 ] := 1;e2 [1,1 ] := 1;e2 [1,1 ] := 1;e2 [1,1 ] := 1;e2 [1,1 ] := 1;e2 [1,1 ] := 1;
   e2 [1,1 ] := 1;e2 [1,1 ] := 1;e2 [1,1 ] := 1;e2 [1,1 ] := 1;e2 [1,1 ] := 1;e2 [1,1 ] := 1;
post;

pre('e3 1,1,1');
   e3 [1,1,1] := 1;e3 [1,1,1] := 1;e3 [1,1,1] := 1;e3 [1,1,1] := 1;e3 [1,1,1] := 1;e3 [1,1,1] := 1;
   e3 [1,1,1] := 1;e3 [1,1,1] := 1;e3 [1,1,1] := 1;e3 [1,1,1] := 1;e3 [1,1,1] := 1;e3 [1,1,1] := 1;
post;

end .
