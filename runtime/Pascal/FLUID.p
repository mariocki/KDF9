program FLUID;

{
  Translated to Pascal from the specimen ALGOL program in Appendix 1 of KDF9 ALGOL manual.
  This is a program for a practically arising problem.
  It illustrates the use of many of the facilities available to the PASKAL user.
}

   label
      8, 9;

   const
      delta_alpha = 0.1;
      eps = 1.0e-5;

   var
      alpha, lambda, L1, L2 : real;
      i : integer;

%listing off
%include PASKAL_char_IO
%include PASKAL_int_IO
%include PASKAL_real_IO
%listing on

function erfc (z : real) : real;
   label
      1;
   var
      x, h, nh, J0, J1 : real;
      n, i : integer;
    begin
    {
      This procedure evaluates the complementary error function of z using the trapezoidal rule.
      It halves the interval until the required accuracy is attained, but avoids repeating
      the evaluation of ordinates more than once.
    }

      h := 0.1;
      J0 := 0;
      n := 0;
      nh := n * h;
      i := 1;

   1:	J1 := J0;

      while nh * (nh + 2*z) < 11.51 do
         begin
         x := z + nh;
         if n = 0 then
            J0 := J0 + exp(-sqr(x)) * 0.5
         else
            J0 := J0 + exp(-sqr(x));
         n := n + i;
         nh := n * h;
         end;

      if abs(1 - 2*J1/J0) > 0.00001 then
         begin
         i := 2;
         n := 1;
         h := h/2;
         nh := n * h;
         goto 1;
         end;

      erfc := 1.128379 * J0 * h;
   end {erfc};

begin
open (1, 3);

put_alfa16(1, 'Propagation of a');
put_alfa16(1, 'n Impulse into a');
put_alfa16(1, ' Viscous-locking');
put_alfa8(1, ' Medium ');
new_line(1, 4);

space(1, 8);
put_alfa16(1, 'Delta alpha = ии');
put_fixed(1, delta_alpha);
new_line(1, 3);

space(1, 8);
put_alfa8(1, 'alpha   ');
space(1, 14);
put_alfa8(1, 'lambda  ');
space(1, 14);
put_alfa16(1,'lambda*sqrt(2alp');
put_alfa8(1, 'ha)иииии');
space(1, 3);
put_alfa8(1, 'gиииииии');
new_line(1, 2);

space(1, 7);
put_fixed(1, 0);
space(1, 8);
put_alfa8(1, 'INFINITY');
space(1, 13);
put_fixed(1, 1.0);
space(1, 7);
put_fixed(1, 1.0);
new_line(1, 1);

i := 1;
alpha := delta_alpha;

while alpha <= 1.0 do
   begin
   i := i + 1;
   lambda := (1 - alpha)/sqrt (2*alpha);
   if alpha >= 1.0 then
      goto 8;
9:
   L1 := lambda;
   L2 := sqrt (0.886227 * (1 - alpha)/alpha * L1 * erfc(L1) * exp(sqr(L1)));
   lambda := L2 + 0.835 * alpha * (L2-L1);

   if abs(1 - L2/lambda) > eps then
      goto 9;
8:
   space(1, 7);
   put_fixed(1, alpha);
   space(1, 7);
   put_fixed(1, lambda);
   space(1, 7);
   put_fixed(1, lambda * sqrt(2*alpha));
   space(1, 7);
   if lambda >= 0.70710678118655 then
      put_fixed(1, 2 * sqr(lambda) * alpha * (1-alpha))
   else
   if lambda <> 0 then
      put_fixed(1, alpha * (1 - alpha) * exp (sqr(lambda)) / (lambda * 2.3282180))
   else
      put_fixed(1, 0.48394);
   if i mod 5 = 0 then
      new_line(1, 2)
   else
      new_line(1, 1);
   alpha := alpha + delta_alpha;
   end ;

new_line(1, 1);
end .
