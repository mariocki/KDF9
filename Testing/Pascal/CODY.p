program cody;
{      ALGORITHM 665, COLLECTED ALGORITHMS FROM ACM.}
{      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,}
{      VOL. 14, NO. 4, PP. 303-311.}
{ Heavily modified for PASKAL by Bill Findlay, 2022-08-11 }

var
   ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,maxexp : integer;
   eps,epsneg,xmin,xmax : real;

%listing off
%include paskal_char_io
%include paskal_int_io
%include paskal_real_io
%listing on

procedure machar;
{-----------------------------------------------------------------------}
{  This Pascal subroutine is intended to determine the parameters}
{   of the floating-point arithmetic system specified below.  The}
{   determination of the first three uses an extension of an algorithm}
{   due to M. Malcolm, CACM 15 (1972), pp. 949-951, incorporating some,}
{   but not all, of the improvements suggested by M. Gentleman and S.}
{   Marovich, CACM 17 (1974), pp. 276-277.  An earlier version of this}
{   program was published in the book Software Manual for the}
{   Elementary Functions by W. J. Cody and W. Waite, Prentice-Hall,}
{   Englewood Cliffs, NJ, 1980.}

{  Result values reported are as follows:}

{       ibeta   - the radix for the floating-point representation: 2 in the case of KDF9}
{       it      - the number of base ibeta digits in the floating-point}
{                 significand}
{       irnd    - 0 if floating-point addition chops}
{                 1 if floating-point addition rounds, but not in the}
{                   ieee style}
{                 2 if floating-point addition rounds in the ieee style}
{                 3 if floating-point addition chops, and there is}
{                   partial underflow}
{                 4 if floating-point addition rounds, but not in the}
{                   ieee style, and there is partial underflow}
{                 5 if floating-point addition rounds in the ieee style,}
{                   and there is partial underflow}
{       ngrd    - the number of guard digits for multiplication with}
{                 truncating arithmetic.  it is}
{                 0 if floating-point arithmetic rounds, or if it}
{                   truncates and only  it  base  ibeta digits}
{                   participate in the post-normalization shift of the}
{                   floating-point significand in multiplication;}
{                 1 if floating-point arithmetic truncates and more}
{                   than  it  base  ibeta  digits participate in the}
{                   post-normalization shift of the floating-point}
{                   significand in multiplication.}
{       machep  - the largest negative integer such that}
{                 1.0+float(ibeta)**machep <> 1.0, except that}
{                 machep is bounded below by  -(it+3)}
{       negeps  - the largest negative integer such that}
{                 1.0-float(ibeta)**negeps <> 1.0, except that}
{                 negeps is bounded below by  -(it+3)}
{       iexp    - the number of bits (decimal places if ibeta := 10)}
{                 reserved for the representation of the exponent}
{                 (including the bias or sign) of a floating-point}
{                 number}
{       minexp  - the largest in magnitude negative integer such that}
{                 float(ibeta)**minexp is positive and normalized}
{       maxexp  - the smallest positive power of beta that overflows}
{       eps     - the smallest positive floating-point number such}
{                 that  1.0+eps <> 1.0. in particular, if either}
{                 ibeta := 2  or  irnd := 0, eps := float(ibeta)**machep.}
{                 otherwise,  eps := (float(ibeta)**machep)/2}
{       epsneg  - a small positive floating-point number such that}
{                 1.0-epsneg <> 1.0. in particular, if ibeta := 2}
{                 or  irnd := 0, epsneg := float(ibeta)**negeps.}
{                 otherwise,  epsneg := (ibeta**negeps)/2.  because}
{                 negeps is bounded below by -(it+3), epsneg may not}
{                 be the smallest number that can alter 1.0 by}
{                 subtraction.}
{       xmin    - the smallest non-vanishing normalized floating-point}
{                 power of the radix, i.e.,  xmin := float(ibeta)**minexp}
{       xmax    - the largest finite floating-point number.  in}
{                 particular  xmax := (1.0-epsneg)*float(ibeta)**maxexp}
{                 note - on some machines  xmax  will be only the}
{                 second, or perhaps third, largest number, being}
{                 too small by 1 or 2 units in the last digit of}
{                 the significand.}

{     Latest revision - April    20, 1987}
{     PASKAL version  - September 2, 2022}

{     Author - W. J. Cody}
{              Argonne National Laboratory}

   label
      410, 460;
   var
      i,itemp,iz,j,k,mx,nxres : integer;
      a,b,beta,betain,betah,conv,one,t,temp,tempa, temp1,two,y,z,zero : real;

   begin {machar}
   one := 1;
   two := one + one;
   zero := one - one;
{-----------------------------------------------------------------------}
{  determine ibeta, beta ala malcolm.}
{-----------------------------------------------------------------------}
   a := one;
   repeat
      a := a + a;
      temp := a+one;
      temp1 := temp-a;
      until (temp1-one <>  zero);
   b := one;
   repeat
      b := b + b;
      temp := a+b;
      itemp := trunc(temp-a); {int()}
      until (itemp <>  0);
   ibeta := itemp;
   beta := ibeta;
{-----------------------------------------------------------------------}
{  determine it, irnd.}
{-----------------------------------------------------------------------}
   it := 0;
   b := one;
   repeat
      it := it + 1;
      b := b*beta;
      temp := b + one;
      temp1 := temp - b;
      until (temp1-one <> zero);
   irnd := 0;
   betah := beta/two;
   temp := a + betah;
   if (temp-a <> zero) then
      irnd := 1;
   tempa := a + beta;
   temp := tempa + betah;
   if ((irnd =  0)  and  (temp-tempa <> zero)) then
      irnd := 2;
{-----------------------------------------------------------------------}
{  determine negep, epsneg.}
{-----------------------------------------------------------------------}
   negep := it + 3;
   betain := one / beta;
   a := one;
   for i := 1 to negep do
      a := a * betain;
   b := a;
   temp := one-a;
   while (temp-one = zero) do
      begin
      a := a * beta;
      negep := negep - 1;
      temp := one-a;
      end;
   negep := -negep;
   epsneg := a;
   if ((ibeta <> 2)  and  (irnd <>  0)) then
      begin
      a := (a*(one+a)) / two;
      temp := one-a;
      if (temp-one <> zero) then
         epsneg := a;
      end;
{-----------------------------------------------------------------------}
{  determine machep, eps.}
{-----------------------------------------------------------------------}
   machep := -it - 3;
   a := b;
   temp := one+a;
   while (temp-one = zero) do
      begin
      a := a * beta;
      machep := machep + 1;
      temp := one+a;
      end;
   eps := a;
   temp := tempa+beta*(one+eps);
   if ((ibeta <>  2)  and  (irnd <>  0)) then
      begin
      a := (a*(one+a)) / two;
      temp := one+a;
      if (temp-one <> zero) then
         eps := a;
      end;
{-----------------------------------------------------------------------}
{  determine ngrd.}
{-----------------------------------------------------------------------}
   ngrd := 0;
   temp := one+eps;
   if ((irnd =  0)  and  (temp*one-one <> zero)) then
      ngrd := 1;
{-----------------------------------------------------------------------}
{  determine iexp, minexp, xmin.}

{  loop to determine largest i and k := 2**i such that}
{         (1/beta) ** (2**(i))}
{  does not underflow.}
{  exit from loop is signaled by an underflow.}
{-----------------------------------------------------------------------}
   i := 0;
   k := 1;
   z := betain;
   t := one + eps;
   nxres := 0;
   for i := 0 to maxint do
      begin
      y := z;
      z := y * y;
{-----------------------------------------------------------------------}
{  check for underflow here.}
{-----------------------------------------------------------------------}
      a := z * one;
      temp := z * t;
      if ((a+a =  zero)  or  (abs(z) >= y)) then
         goto 410;
      temp1 := temp * betain;
      if (temp1*beta =  z) then
         goto 410;
      k := k + k;
      end;
410:  iexp := i + 1;
      mx := k + k;
{-----------------------------------------------------------------------}
{  loop to determine minexp, xmin.}
{  exit from loop is signaled by an underflow.}
{-----------------------------------------------------------------------}
   repeat
      xmin := y;
      y := y * betain;
{-----------------------------------------------------------------------}
{  check for underflow here.}
{-----------------------------------------------------------------------}
      a := y * one;
      temp := y * t;
      if (((a+a) =  zero)  or  (abs(y) >= xmin)) then
         goto 460;
      k := k + 1;
      temp1 := temp * betain;
      until temp1*beta = y;
   nxres := 3;
   xmin := y;
460:
   minexp := -k;
{-----------------------------------------------------------------------}
{  determine maxexp, xmax. On KDF9 beta = 2.}
{-----------------------------------------------------------------------}
   if (mx <= k+k-3) then
      begin
      mx := mx + mx;
      iexp := iexp + 1;
      end;
   maxexp := mx + minexp;
{-----------------------------------------------------------------}
{  adjust irnd to reflect partial underflow.}
{-----------------------------------------------------------------}
   irnd := irnd + nxres;
{-----------------------------------------------------------------}
{  adjust for ieee-style machines.}
{-----------------------------------------------------------------}
   if ((irnd =  2)  or  (irnd =  5)) then
      maxexp := maxexp - 2;
{-----------------------------------------------------------------}
{  adjust for non-ieee machines with partial underflow.}
{-----------------------------------------------------------------}
   if ((irnd =  3)  or  (irnd =  4)) then
      maxexp := maxexp - it;
{-----------------------------------------------------------------}
{  On KDF9 beta = 2.}
{-----------------------------------------------------------------}
      xmax := power(2.0, 126);
      xmax := xmax*(2 - 1/power(2.0, 39));
   end {machar};

begin {cody}
open(1, 3);
put_alfa16(1, 'calling machar  '); new_line(1,2);
machar;
put_alfa16(1, 'output of machar'); new_line(1,1);
put_alfa8(1, 'beta =  '); put_small_int(1, ibeta);  new_line(1,1);
put_alfa8(1, 't    =  '); put_small_int(1, it);     new_line(1,1);
put_alfa8(1, 'irnd  = '); put_small_int(1, irnd);   new_line(1,1);
put_alfa8(1, 'ngrd  = '); put_small_int(1, ngrd);   new_line(1,1);
put_alfa8(1, 'macep = '); put_small_int(1, machep); new_line(1,1);
put_alfa8(1, 'negep = '); put_small_int(1, negep);  new_line(1,1);
put_alfa8(1, 'iexp  = '); put_small_int(1, iexp);   new_line(1,1);
put_alfa8(1, 'minexp= '); put_small_int(1, minexp); new_line(1,1);
put_alfa8(1, 'maxexp= '); put_small_int(1, maxexp); new_line(1,1);
put_alfa8(1, 'eps   = '); put_float(1, eps);        new_line(1,1);
put_alfa8(1, 'epsneg= '); put_float(1, epsneg);     new_line(1,1);
put_alfa8(1, 'xmin  = '); put_float(1, xmin);       new_line(1,1);
put_alfa8(1, 'xmax  = '); put_float(1, xmax);       new_line(1,1);
end {cody} .
