program GAMM10K;
{
 GAMM in FORTRAN - single precision version, mark 3;
 National Physical Laboratory Benchmark Gamm F;
 This program has a single parameter N;
 Set N := 10000 for about 60s on a machine like System 360/65;
 Output is by one write statement to stream 30;
 It takes KDF9 about 480s with unoptimised KAlgol, and about 154s when optimised;
}
const
   N = 10000;

var
   I, J, REP : integer;
   ACC, ACC1, DIVN, ROOT, X, Y : real;
   A, B, C : array [1 .. 30] of real;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%include PASKAL_real_IO
%listing on

begin
DIVN := 1.0 / N;
X := 0.1;
ACC := 0.0;


{ INITIALISE A AND B }
Y := 1.0;
for I := 1 to 30 do
begin
  A[I] := I;
  B[I] := - Y;
  Y := - Y;
end;

{ ONE PASS OF THIS LOOP CORRESPONDS TO 300 GAMM UNITS }

for REP := 1 to N do
   begin

{ FIRST ADDITION/SUBTRACTION LOOP }
   I := 30;
   for J := 1 to 30 do
     begin
     C[I] := A[I] + B[I];
     I := I - 1;
     end;

{ FIRST POLYNOMIAL LOOP }
   Y := 0.0;
   for I := 1 to 10 do
     begin
     Y := (Y + C[I]) * X;
     end;
   ACC1 := Y * DIVN; { not I, which is undefined in Algol }

{ FIRST MAXIMUM ELEMENT LOOP }
   Y := C[11];
   for I := 12 to 20 do
     begin
     if (C[I] > Y) then Y := C[I];
     end;

{ FIRST SOUARE ROOT LOOP }
   ROOT := 1.0;
   for I := 1 to 5 do
     begin
     ROOT := 0.5 * (ROOT + Y/ROOT);
     end;
   ACC1 := ACC1 + ROOT * DIVN;

{ SECOND ADDITION/SUBTRACTION LOOP }
   for I := 1 to 30 do
     begin
     A[I] := C[I] - B[I];
     end;

{ SECOND POLYNOMIAL LOOP }
   Y := 0.0;
   for I := 1 to 10 do
     begin
     Y := (Y + A[I]) * X;
     end;

{ SECOND SQUARE ROOT LOOP }
   ROOT := 1.0;
   for I := 1 to 5 do
     begin
     ROOT := 0.5 * (ROOT + Y/ROOT);
     end;
   ACC1 := ACC1 + ROOT * DIVN;

{ FIRST MULTIPLICATION LOOP }
   for I := 1 to 30 do
     begin
     C[I] := C[I] * B[I];
     end;

{ SECOND MAXIMUM ELEMENT LOOP }
   Y := C[20];
   for I := 21 to 30 do
     begin
     if (C[I] > Y) then Y := C[I];
     end;

{ THIRD SQUARE ROOT LOOP }
   ROOT := 1.0;
   for I := 1 to 5 do
     begin
     ROOT := 0.5 * (ROOT + Y/ROOT);
     end;
   ACC1 := ACC1 + ROOT * DIVN;

{ THIRD POLYNOMIAL LOOP }
   Y := 0.0;
   for I := 1 to 10 do
     begin
     Y := (Y + C[I]) * X;
     end;
   ACC1 := ACC1 + Y * DIVN;

{ THIRD MAXIMUM ELEMENT LOOP }
   Y := C[1];
   for I := 2 to 10 do
     begin
     if (C[I] > Y) then Y := C[I];
     end;

{ FOURTH SQUARE ROOT LOOP }
   ROOT := 1.0;
   for I := 1 to 5 do
     begin
     ROOT := 0.5 * (ROOT + Y/ROOT);
     end;
   ACC1 := ACC1 + ROOT * DIVN;
   ACC := ACC + ACC1;
   end;

open(1, 3);
put_int(1, N);
put_fixed(1, ACC1*N);

{ Should print N then  1.6733 4322 4109 0064 7168 4801E1 }
{ KDF9 actually prints 16.733 4322 411 }
end .
