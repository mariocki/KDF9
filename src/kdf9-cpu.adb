-- kdf9-cpu.adb
--
-- Support for KDF9 CPU/ALU operations that are not automatically inherited from
--   Ada types; and for types used in the internal functioning of the microcode.
--
-- This file is part of ee9 (V5.2b), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2021, W. Findlay; all rights reserved.
--
-- The ee9 program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. This program is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. You should have
-- received a copy of the GNU General Public License distributed with
-- this program; see file COPYING. If not, see <http://www.gnu.org/licenses/>.
--
--

with exceptions;

use  exceptions;

package body KDF9.CPU is

--
--
   --
   -- KDF9 Arithmetic Control (AC) primitives representing Shift Control and associated units.
   -- These units implement shifting, multiplication, division, and floating-point arithmetic.
   --
--
--

   -- Count the leading zeros of the absolute value of y, omitting the sign bit.
   -- If y is 0, return 47.
   function nr_leading_zeros (y : KDF9.word)
   return Natural is
      x : CPU.u_64;
      r : Natural;
   begin
      if y = 0 then return 47; end if;
      if resign(y) < 0 then
         x := CPU.u_64(16#FFFF_FFFF_FFFF# and not y);
      else
         x := CPU.u_64(y);
      end if;
      -- Only 48 bits of x need be tested.
      if (x and 16#FFFF_0000_0000#) /= 0 then
         r := 32; x := shift_right(x, 32);
      elsif (x and 16#FFFF_0000#) /= 0 then
         r := 16; x := shift_right(x, 16);
      else
         r := 0;
      end if;
      if (x and 16#FF00#) /= 0 then
         r := r + 8; x := shift_right(x, 8);
      end if;
      if (x and 16#F0#) /= 0 then
         r := r + 4; x := shift_right(x, 4);
      end if;
      if (x and 16#C#) /= 0 then
         r := r + 2; x := shift_right(x, 2);
      end if;
      if (x and 16#2#) /= 0 then
         r := r + 1;
      end if;
      r := 47 - r - 1;  -- -1 discounts the sign bit.
      return r;
   end nr_leading_zeros;

   function nr_one_bits (u : CPU.u_64)
   return CPU.u_64 is
      n : CPU.u_64 := shift_right(u, 1) and 16#77_77_77_77_77_77_77_77#;
      x : CPU.u_64 := u - n;
   begin
      n := shift_right(n, 1) and 16#77_77_77_77_77_77_77_77#;
      x := x - n;
      n := shift_right(n, 1) and 16#77_77_77_77_77_77_77_77#;
      x := x - n;
      x := (x + shift_right(x, 4)) and 16#0F_0F_0F_0F_0F_0F_0F_0F#;
      x := x * 16#01_01_01_01_01_01_01_01#;
      return shift_right(x, CPU.u_64'Size-8);
   end nr_one_bits;

   KDF9_max_signed : constant CPU.s_64 := CPU.s_64(CPU.signed'Last);
   KDF9_min_signed : constant CPU.s_64 := CPU.s_64(CPU.signed'First);

   function as_word (u : CPU.u_64)
   return KDF9.word
   is (KDF9.word(u and KDF9.word_mask));

   function as_word (s : CPU.s_64)
   return KDF9.word is
   begin
      if s > KDF9_max_signed or s < KDF9_min_signed then
         the_V_bit_is_set := True;
      end if;
      return as_word(unsign(s));
   end as_word;

   function contracted (msw, lsw : KDF9.word)
   return KDF9.word is
   begin
      if resign(lsw) < 0 or (msw+1) > 1 then
         the_V_bit_is_set := True;
      end if;
      return (lsw and not_sign_bit) or (msw and sign_bit);
   end contracted;

   function contracted (P : KDF9.pair)
   return KDF9.word
   is (contracted(msw => P.msw, lsw => P.lsw));

   function shift_time (amount : Natural)
   return KDF9.us
   is (KDF9.us(amount/16 + amount/8 mod 2 + (if amount mod 8 > 0 then 1 else 0)));

   function normalize_time (amount : Natural)
   return KDF9.us
   is (KDF9.us(amount/8 + (if amount mod 8 > 0 then 1 else 0)));

   function shift_word_left (W : KDF9.word; amount : word_shift_length)
   return KDF9.word
   is (as_word(shift_left(CPU.u_64(W), amount)));

   function shift_word_right (W : KDF9.word; amount : word_shift_length)
   return KDF9.word
   is (KDF9.word(shift_right(CPU.u_64(W), amount)));  -- This cannot be out of range.

   function rotate_word_left (W : KDF9.word; amount : word_shift_length)
   return KDF9.word
   is (shift_word_left(W, amount) or shift_word_right(W, 48-amount));

   function rotate_word_right (W : KDF9.word; amount : word_shift_length)
   return KDF9.word
   is (shift_word_right(W, amount) or shift_word_left(W, 48-amount));

   function shift_circular (W : KDF9.word; L : CPU.signed_Q_part)
   return KDF9.word
      -- The logic here conforms to ¶1.1 of EE Report K/GD.y.80, entitled
      --    "KDF 9: SHIFTING AND SHIFT CONTROL".
      -- Circular shifts were implemented by duplicating the operand, doing a double-length
      --    shift of the two words, and selecting the appropriate word from the result.
   is (
       if abs L > 95 then 0
       elsif L < -48 then shift_word_right(W, Natural(-L-48))
       elsif L > +48 then shift_word_left(W, Natural(+L-48))
       elsif L < 0   then rotate_word_right(W, Natural(-L))
       else               rotate_word_left(W, Natural(L))
      );

   function shift_logical (W : KDF9.word; L : CPU.signed_Q_part)
   return KDF9.word
   is
      (
       if abs L > 47 then 0
       elsif  L < 0  then shift_word_right(W, Natural(-L))
       else               shift_word_left(W, Natural(L))
      );

   function shift_pair_left (P : KDF9.pair; L : Natural)
   return KDF9.pair
      with Inline;

   function shift_pair_left (P : KDF9.pair; L : Natural)
   return KDF9.pair is
      result    : KDF9.pair;
      crossover : KDF9.word;
   begin
      -- The logic here conforms to ¶3.2 of EE Report K/GD.y.80.
      if L < 48 then
         result.lsw := shift_word_left(P.lsw, L);
         crossover  := shift_word_right(P.lsw, 48-L);
         result.msw := shift_word_left(P.msw, L) or crossover;
      else
         result.lsw := 0;
         result.msw := shift_word_left(P.lsw, L-48);
      end if;
      return result;
   end shift_pair_left;

   function shift_pair_right (P : KDF9.pair; L : Natural)
   return KDF9.pair
      with Inline;

   function shift_pair_right (P : KDF9.pair; L : Natural)
   return KDF9.pair is
      result    : KDF9.pair;
      crossover : KDF9.word;
   begin
      -- The logic here conforms to ¶3.2 of EE Report K/GD.y.80.
      if L < 48 then
         result.msw := shift_word_right(P.msw, L);
         crossover  := shift_word_left(P.msw, 48-L);
         result.lsw := shift_word_right(P.lsw, L) or crossover;
      else
         result.msw := 0;
         result.lsw := shift_word_right(P.msw, L-48);
      end if;
      return result;
   end shift_pair_right;

   function shift_logical (P : KDF9.pair; L : CPU.signed_Q_part)
   return KDF9.pair
   is (
       if    L > 0 then shift_pair_left(P, Natural(L))
       elsif L < 0 then shift_pair_right(P, Natural(-L))
       else        P
      );

   function scale_down (W : KDF9.word; amount : Natural)
   return KDF9.word is
      unrounded, clearing : CPU.u_64;
   begin
      if amount = 0 then
         return W;
      elsif amount > 46 then
         if resign(W) < 0 then
            return KDF9.all_one_bits;
         else
            return 0;
         end if;
      else
         -- It is undefined whether the intrinsic shift_right_arithmetic function,
         --    operating on CPU.u_64, yields a rounded result.
         -- So, any rounding it might do is completely suppressed.
         unrounded := shift_right_arithmetic(shift_left(CPU.u_64(W),16), 16);
         clearing  := - shift_left(1, amount);
         return as_word(shift_right_arithmetic(unrounded and clearing, amount));
      end if;
   end scale_down;

   function scale_down_and_round (W : KDF9.word; amount : Natural)
   return KDF9.word is
      unrounded, clearing, rounding : CPU.u_64;
   begin
      if amount = 0 then
         return W;
      elsif amount > 46 then
         if resign(W) < 0 then
            return KDF9.all_one_bits;
         else
            return 0;
         end if;
      else
         -- It is undefined whether the intrinsic shift_right_arithmetic,
         --    operating on CPU.u_64, yields a rounded result.
         -- So, any rounding it might do is suppressed,
         --    and correct rounding is explicitly computed.
         unrounded := shift_right_arithmetic(shift_left(CPU.u_64(W),16), 16);
         rounding  := shift_right(unrounded, amount-1) and 1;
         clearing  := - shift_left(1, amount);
         unrounded := unrounded and clearing;
         return as_word(shift_right_arithmetic(unrounded, amount) + rounding);
      end if;
   end scale_down_and_round;

   function scale_up (W : KDF9.word; amount : Natural)
   return KDF9.word is
      M : constant Natural := Natural'Min(amount, 47);
   begin
      if resign(W) < 0 then
         if scale_down(W, 47-M) /= all_one_bits or
               resign(shift_word_left(W, M)) >= 0 then
            -- See EE Report K/GD.y.80., ¶ 1.1.
            the_V_bit_is_set := True;
         end if;
         return shift_word_left(W, M);
      else
         if shift_word_right(W, 47-M) /= all_zero_bits or
               resign(shift_word_left(W, M)) < 0 then
            -- See EE Report K/GD.y.80., ¶ 1.1.
            the_V_bit_is_set := True;
         end if;
         return shift_word_left(W, M);
      end if;
   end scale_up;

   function shift_arithmetic (I : KDF9.word; L : CPU.signed_Q_part)
   return KDF9.word
   is (
       if L < 0 then scale_down_and_round(I, Natural(-L))
       else          scale_up(I, Natural(L))
      );

   function scale_up (P : KDF9.pair; L : Natural)
   return KDF9.pair is
      result    : KDF9.pair;
      crossover : KDF9.word;
   begin
      -- The logic here conforms to ¶3.2 of EE Report K/GD.y.80.
      if L < 48 then
         result.lsw := shift_word_left(P.lsw, L) and KDF9.max_word;
         crossover  := shift_word_right(P.lsw and KDF9.max_word, 47-L);
         result.msw := scale_up(P.msw, L) or crossover;
      else
         result.lsw := 0;
         result.msw := scale_up(P.msw, 47) or P.lsw;
         result.msw := scale_up(result.msw, Natural'Min(L, 94)-47);
     end if;
      return result;
   end scale_up;

   function scale_down (P : KDF9.pair; L : Natural)
   return KDF9.pair is
      result    : KDF9.pair;
      crossover : KDF9.word;
   begin
      -- The logic here conforms to ¶3.2 of EE Report K/GD.y.80.
      -- SHAD-n does NOT round, according to the Manual.
      if L < 48 then
         result.msw := scale_down(P.msw, L);
         crossover  := shift_word_left(P.msw, 47-L) and KDF9.max_word;
         -- It is not clear whether D0 of the lsw should be cleared before or after shifting.
         -- I now clear it before shifting for compatibility with D. Holdsworth's "kdf9".
         result.lsw := shift_word_right(P.lsw and KDF9.max_word, L) or crossover;
      else
         result.msw := scale_down(P.msw, 47);
         result.lsw := shift_word_right(P.msw, Natural'Min(L, +94)-47) and KDF9.max_word;
      end if;
      return result;
   end scale_down;

   function shift_arithmetic (P : KDF9.pair; L : CPU.signed_Q_part)
   return KDF9.pair
   is (
       if    L < 0 then scale_down(P, Natural(-L))
       elsif L > 0 then scale_up(P, Natural(L))
       else             P -- See ¶1.1 of EE Report K/GD.y.80: this avoids clearing D0 of P.lsw.
      );

   procedure normalize (fraction, exponent : in out KDF9.word) is
      sign_flag  : constant KDF9.word := shift_word_right(fraction and sign_bit, 1);
      normalizer : Natural;
   begin
      if fraction = 0 then
         exponent := 2#10_000_000#;  -- This yields 0 when biased positive.
         return;
      end if;

      normalizer := nr_leading_zeros(fraction);
      exponent := exponent - KDF9.word(normalizer);

      -- shift_word_left is used, not _arithmetic, as D[1..normalizer] = D0
      fraction := shift_word_left(fraction, normalizer);

      the_CPU_delta := the_CPU_delta + normalize_time(normalizer);

      -- scale_down_and_round may round up and overflow the fraction bits ...
      fraction := scale_down_and_round(fraction, 8);
      if (fraction and overflow_mask) /= shift_word_right(sign_flag, 7) then
          -- ... so re-normalize; scale_down cannot round here.
         fraction := scale_down(fraction, 1);
         the_CPU_delta := the_CPU_delta + normalize_time(1);
         exponent := exponent + 1;
         the_CPU_delta := the_CPU_delta + 1;
      end if;
      fraction := fraction and mantissa_mask;

      if resign(exponent) < -128 then
         -- Deal with underflow.
         fraction := 0;
         exponent := 2#10_000_000#;  -- This yields 0 when biased positive.
      elsif resign(exponent) > +127 then
         -- Deal with overflow.
         the_V_bit_is_set := True;
         exponent := 2#01_111_111#;
      end if;
   end normalize;

   function fraction_word (mantissa : CPU.f48)
   return KDF9.word
    -- shift_word_left must be used instead of scale_up to avoid a spurious overflow.
   is (
       (shift_word_left(as_word(mantissa), 8) and KDF9.max_word)
          or
       (as_word(mantissa) and sign_bit)
      );

   function masked_mantissa (F : CPU.f48)
   return CPU.f48
   is (as_f48(as_word(F) and mantissa_mask));

   function scaler (F : CPU.f48)
   return KDF9.word
   is ((shift_word_right(as_word(F), 39) and 2#11_111_111#) - 128);

   function normalized (full_fraction, scaler : KDF9.word)
   return CPU.f48 is
      E : KDF9.word := scaler;
      F : KDF9.word := full_fraction;
   begin
      normalize(fraction => F, exponent => E);
      return CPU.f48(shift_word_left((E + 128) and 2#11_111_111#, 39) or F);
   end normalized;

   function normalized  (R : CPU.f48)
   return CPU.f48
   is (normalized(full_fraction => fraction_word(R), scaler => scaler(R)));

   function cardinality (W : KDF9.word)
   return KDF9.word
   is (KDF9.word(nr_one_bits(CPU.u_64(W))));

   function "-" (I : CPU.signed)
   return KDF9.word
   is (as_word(-CPU.s_64(I)));

   function "abs" (I : CPU.signed)
   return KDF9.word
   is (as_word(abs CPU.s_64(I)));

   function "+" (L, R : CPU.signed)
   return KDF9.word
   is (as_word(CPU.s_64(L) + CPU.s_64(R)));

   function "-" (L, R : CPU.signed)
   return KDF9.word
   is (as_word(CPU.s_64(L) - CPU.s_64(R)));

   function "*" (L, R : CPU.signed)
   return KDF9.word
   is (contracted(KDF9.pair'(unsign(L) * unsign(R))));

   procedure do_DIVI (L : in KDF9.word;
                      R : in KDF9.word;
                      Quotient, Remainder : out KDF9.word) is
   begin
      if R /= 0 then
         Remainder := as_word(CPU.s_64(resign(L)) mod CPU.s_64(resign(R)));
         Quotient  :=
            as_word((CPU.s_64(resign(L)) - CPU.s_64(resign(Remainder))) / CPU.s_64(resign(R)));
      else
         the_V_bit_is_set := True;
         Quotient  := L;  -- ??
         Remainder := R;  -- ??
      end if;
   end do_DIVI;

   function "*" (L, R : KDF9.word)
   return CPU.fraction is
   begin
      if L = sign_bit and R = sign_bit then
         the_V_bit_is_set := True;
         return as_fraction(sign_bit);  -- The only case is L = R = -1.0 = L*R.
      else
         return as_fraction(L) * as_fraction(R);
      end if;
   end "*";

   function "/" (L, R : KDF9.word)
   return CPU.fraction is
   begin
      if R = 0 or L = sign_bit then
         the_V_bit_is_set := True;
         return as_fraction(L); -- ??
      elsif R = sign_bit then
         return -as_fraction(L);
      elsif abs as_fraction(L) < abs as_fraction(R) then  -- abs is safe now.
         return as_fraction(L) / as_fraction(R);
      else
         the_V_bit_is_set := True;
         return as_fraction(L); -- ??
      end if;
   end "/";

   function "+" (L, R : KDF9.pair)
   return KDF9.pair is
      carry, sum : CPU.s_64;
      result     : KDF9.pair;
   begin
      sum := CPU.s_64(L.lsw) + CPU.s_64(R.lsw);
      if unsign(sum) > KDF9.max_word then -- carry into msw
         carry := 1;
         result.lsw := KDF9.word(unsign(sum) and KDF9.max_word);
      else
         carry := 0;
         result.lsw := KDF9.word(sum);
      end if;
      sum := CPU.s_64(resign(L.msw)) + CPU.s_64(resign(R.msw)) + carry;
      result.msw := as_word(sum);
      return result;
   end "+";

   function "-" (J : KDF9.pair)
   return KDF9.pair is
      borrow,
      negative : CPU.s_64;
      result   : KDF9.pair;
   begin
      negative := - CPU.s_64(J.lsw);
      if unsign(negative) > KDF9.max_word then -- borrow from msw
         borrow := 1;
         result.lsw := KDF9.word(unsign(negative) and KDF9.max_word);
      else
         borrow := 0;
         result.lsw := KDF9.word(negative);
      end if;
      negative := - CPU.s_64(resign(J.msw)) - borrow;
      result.msw := as_word(negative);
      return result;
   end "-";

   function "-" (L, R : KDF9.pair)
   return KDF9.pair is
      borrow,
      difference : CPU.s_64;
      result     : KDF9.pair;
   begin
      difference := CPU.s_64(L.lsw) - CPU.s_64(R.lsw);
      if unsign(difference) > KDF9.max_word then -- borrow from msw
         borrow := 1;
         result.lsw := KDF9.word(unsign(difference) and KDF9.max_word);
      else
         borrow := 0;
         result.lsw := KDF9.word(difference);
      end if;
      difference := CPU.s_64(resign(L.msw)) - CPU.s_64(resign(R.msw)) - borrow;
      result.msw := as_word(difference);
      return result;
   end "-";

   function "*" (L, R : KDF9.word)
   return KDF9.pair is
      S, T, U, V, W : KDF9.word;
      H, M, B       : KDF9.pair;
   begin
      if L = sign_bit then
         if R = L then
            -- L*R = (+1.0), which is not a valid fraction, so deal with overflow.
            the_V_bit_is_set := True;
            return (L, 0);
         else
            -- L*R = -R.
            return - (R, 0);
         end if;
      end if;
      if R = sign_bit then
         -- L*R = -L.
         return - (L, 0);
      end if;
      -- Now it is safe to take absolute values, as they cannot overflow.
      S := scale_down(abs resign(L), 24);
      T := abs resign(L) and halfword_mask;
      U := scale_down(abs resign(R), 24);
      V := abs resign(R) and halfword_mask;
      H := ((S*U)*2, 0);
      M := scale_down((KDF9.word'(S*V), 0), 1) + scale_down((KDF9.word'(T*U), 0), 1);
      M := scale_down(M, 22);
      W := rotate_word_left(KDF9.word'(T*V), 1);
      B := (W and 1, shift_word_right(W, 1));
      if resign(L xor R) < 0 then
         return - (H + M + B);
      else
         return    H + M + B;
      end if;
   end "*";

   f_64_small : constant := 2.0**(-63);
   type f_64 is delta f_64_small range -1.0 .. +1.0 - f_64_small with Size => 64;

   function scale_down (f : CPU.f_64; N : Natural)
   return f_64
   is (if N > 62 then 0.0 else f / 2**N);

   function to_f_64 (w : KDF9.word)
   return CPU.f_64
   is (CPU.f_64(as_fraction(w)));

   function to_word (f : CPU.f_64)
   return KDF9.word
   is (as_word(CPU.fraction(f)));

   procedure do_DIVD (L : in KDF9.pair;
                      R : in KDF9.word;
                      Q : out KDF9.word
                     ) is
      to_normalize_L : Natural;
      to_normalize_R : Natural;
      to_normalize_Q : Integer;
      N              : KDF9.pair;
      D              : KDF9.word;
      Ls, Rs, Qs     : CPU.f_64;
   begin
      -- Deal very quickly with a zero result.
      if (L.msw or L.lsw) = 0 then
         Q := 0;
         return;
      end if;

      -- Deal with division by 0.
      if R = 0 then
         the_V_bit_is_set := True;
         Q := L.msw;  -- This is a guess at the result for division by zero ?? !!
         return;
      end if;

      -- Check for an invalid numerator; D0 of L.lsw must be 0.
      if resign(L.lsw) < 0 then -- L is not a valid double-length number.
         the_V_bit_is_set := True;
         Q := L.msw;  -- This is a guess at the result for an invalid numerator ?? !!
         return;
      end if;

      to_normalize_L := nr_leading_zeros(L.msw);
      if to_normalize_L > 46 then -- insignificant top half
         N := scale_up(L, 47);
         to_normalize_L := nr_leading_zeros(N.msw);
         N := scale_up(N, to_normalize_L);
         to_normalize_L := to_normalize_L + 47;
      else
         N := scale_up(L, to_normalize_L);
      end if;

      to_normalize_R := nr_leading_zeros(R);
      D := scale_up(R, to_normalize_R);

      -- Scale Ls and Rs so that the Ada fractional division cannot overflow.
      Ls := scale_down(to_f_64(N.msw), 2);
      Rs := scale_down(to_f_64(D), 1);

      Qs := Ls / Rs;  -- "/" cannot overflow here.

      to_normalize_Q := 1 + to_normalize_R - to_normalize_L;

      if to_normalize_Q <= 0 then
         -- Overflow is impossible.
         Qs := scale_down(Qs, -to_normalize_Q);
         Q := to_word(Qs);
      else
         -- If Qs >= 0.5, then L/R >= 1.0 is not a representable result fraction.
         -- If Qs < -0.5, then L/R < -1.0 is not a representable result fraction.
         if Qs >= 0.5 or Qs < -0.5 then
            the_V_bit_is_set := True;
            Q := L.msw / R;  -- This is a guess at the result when it overflows ?? !!
            return;
         end if;
         Q := scale_up(to_word(Qs), to_normalize_Q);
      end if;
   end do_DIVD;

   procedure do_DIVR (L : in KDF9.pair;
                      R : in KDF9.word;
                      Quotient, Remainder : out KDF9.word
                     ) is
      correction_count_limit : constant := 3;
      correction_count       : Natural  := 0;
      V  : constant Boolean := the_V_bit_is_set;
      N  : KDF9.pair := L;
      D  : KDF9.word := R;
      S  : KDF9.word := +1;
      P,
      T  : KDF9.pair;
   begin
      if (N.msw or N.lsw) = 0 then
         Quotient  := 0;
         Remainder := 0;
         return;
      end if;

      if D = 0 then
         the_V_bit_is_set := True;
         Quotient  := L.msw;  -- This is a guess at the result for division by zero ?? !!
         Remainder := L.lsw;  -- This is a guess at the result for division by zero ?? !!
         return;
      end if;

      -- Check for an invalid numerator; D0 of N1 must be 0.
      if resign(L.lsw) < 0 then -- L is not a valid double-length number.
         the_V_bit_is_set := True;
         Quotient  := L.msw;  -- This is a guess at the result for invalid numerator ?? !!
         Remainder := L.lsw;  -- This is a guess at the result for invalid numerator ?? !!
         return;
      end if;

      -- Convert to an unsigned division problem, and note whether it needs to be be converted back.
      if resign(N.msw) < 0 then -- L is negative.
         N := - N;
         S := - S;
      end if;

      if resign(D) < 0 then  -- R is negative.
         D := - D;
         S := - S;
      end if;

      -- Check for inevitable overflow, and deal with it separately.
      if N.msw > D then
         Quotient := N.msw / D * S;  -- This is a guess at the result when it overflows ?? !!
         T := L - (msw => Quotient*R, lsw => 0);
         Remainder := T.msw;         -- This is a guess at the result when it overflows ?? !!
         the_V_bit_is_set := True;
         return;
      end if;

      -- Overflow should not now be possible.
      -- Ensure that an overflow in DIVD is trapped as a failure
      the_V_bit_is_set := False;
      do_DIVD(N, D, Quotient);
      if the_V_bit_is_set then
         raise emulation_failure with "DIVR overflows in DIVD";
      end if;
      -- Restore the input value of the overflow register.
      the_V_bit_is_set := V;

      -- Adjust Quotient until the difference between N and Quotient*D fits in one word.
      correction_count := 0;
      loop
         P := Quotient * D;
         T := N - P;
      exit when T.msw = 0;
         correction_count := correction_count + 1;
         if correction_count > correction_count_limit then
             raise emulation_failure with "DIVR exceeds correction_count_limit A";
         end if;
         Quotient := Quotient + 1;
      end loop;

      Remainder := T.lsw;

      -- Adjust Quotient and Remainder until Remainder is less than the divisor in absolute value.
      correction_count := 0;
      while Remainder >= D loop
         correction_count := correction_count + 1;
         if resign(Remainder) > 0 then
            Remainder := Remainder - D;
            Quotient := Quotient + 1;
         else
            Remainder := Remainder + D;
            Quotient := Quotient - 1;
         end if;
         if correction_count > correction_count_limit then
             raise emulation_failure with "DIVR exceeds correction_count_limit B";
         end if;
      end loop;

      Quotient  := Quotient * S;
      Remainder := contracted(L - Quotient*R);
   end do_DIVR;

   function host_float (X : CPU.f48)
   return Long_Float is
      -- Warn if Long_Float does not have at least the range of a KDF9 floating point number.
      pragma Compile_Time_Warning(Long_Float'Last < 2.0**127,
                                  "Long_Float does not have enough range for KDF9 f.p.");
      -- Warn if Long_Float does not have at least the precision of a KDF9 floating point number.
      pragma Compile_Time_Warning(Long_Float'Small > 1.0/2.0**39,
                                  "Long_Float does not have enough precision for KDF9 f.p.");
      W : constant KDF9.word  := fraction_word(masked_mantissa(X));
      S : constant Long_Float := 2.0**Integer(resign(scaler(X)));
   begin
      return Long_Float(as_fraction(W)) * S;  -- Cannot overflow if warnings are absent.
   end host_float;

   -- Round a 48-bit floating-point number to 24-bit format.
   function narrowed (R : CPU.f48)
   return CPU.f48
   is (normalized(fraction_word(R) + 2**23, scaler(R)));

   overriding
   function "-" (R : CPU.f48)
   return CPU.f48 is
      -- F is made half of a true fraction to prevent overflow when negating:
      --    the result exponent is offset by 1, accordingly.
      E : constant KDF9.word := scaler(R) + 1;
      F : KDF9.word := scale_down_and_round(fraction_word(R), 1);
   begin
      F := as_word(CPU.u_64(-F));  -- "-" cannot overflow here.
      return normalized(full_fraction => F, scaler => E);
   end "-";

   overriding
   function "abs" (R : CPU.f48)
   return CPU.f48
   is (if resign(KDF9.word(R)) < 0 then - R else + R);

   overriding
   function "+" (L, R : CPU.f48)
   return CPU.f48 is
      -- B and D are made half of a true fraction to prevent overflow when
      --    adding; the result exponent is offset by 1, accordingly.
      A : constant KDF9.word := scaler(R);
      B : KDF9.word := scale_down(fraction_word(R), 1);
      C : constant KDF9.word := scaler(L);
      D : KDF9.word := scale_down(fraction_word(L), 1);
      E : KDF9.word;
      F : KDF9.word;
      N : Natural;
   begin
      if resign(A) >= resign(C) then
         N := Natural'Min(Natural(resign(A-C)), 48);
         D := scale_down_and_round(D, N);
         E := A + 1;
      else
         N := Natural'Min(Natural(resign(C-A)), 48);
         B := scale_down_and_round(B, N);
         E := C + 1;
      end if;
      the_CPU_delta := the_CPU_delta + shift_time(N);
      F := as_word(CPU.u_64(D + B));  -- "+" cannot overflow here.
      return normalized(full_fraction => F, scaler => E);
   end "+";

   overriding
   function "-" (L, R : CPU.f48)
   return CPU.f48 is
      -- See "+".
      A : constant KDF9.word := scaler(R);
      B : KDF9.word := scale_down(fraction_word(R), 1);
      C : constant KDF9.word := scaler(L);
      D : KDF9.word := scale_down(fraction_word(L), 1);
      E : KDF9.word;
      F : KDF9.word;
      N : Natural;
   begin
      if resign(A) >= resign(C) then
         N := Natural'Min(Natural(resign(A-C)), 48);
         D := scale_down_and_round(D, N);
         E := A + 1;
      else
         N := Natural'Min(Natural(resign(C-A)), 48);
         B := scale_down_and_round(B, N);
         E := C + 1;
      end if;
      the_CPU_delta := the_CPU_delta + shift_time(N);
      F := as_word(CPU.u_64(D - B));  -- "-" cannot overflow here.
      return normalized(full_fraction => F, scaler => E);
   end "-";

   overriding
   function "*" (L, R : CPU.f48)
   return CPU.f48 is
      B, D, E, F : KDF9.word;
   begin
      if (KDF9.word(L) or KDF9.word(R)) = 0 then
         return 0;
      end if;
      B := fraction_word(R);
      D := fraction_word(L);
      E := scaler(L) + scaler(R);
      if (B = sign_bit) and (B = D) then
          -- D*B = (+1), which is not a valid fraction, so treat specially.
          B := B / 2;
          D := D / 2;
          E := E + 2;
       end if;
      F := as_word(as_fraction(D) * as_fraction(B));  -- "*" cannot overflow here.
      return normalized(full_fraction => F, scaler => E);
   end "*";

   overriding
   function "/" (L, R : CPU.f48)
   return CPU.f48 is
      D, N   : CPU.fraction;
      Ls, Rs : KDF9.word;
      E, F   : KDF9.word;
   begin
      if R = 0 then
         the_V_bit_is_set := True;
         return L;  -- ?? This result is not well defined in the Manual.
      end if;
      -- If L>=R, L/R>= 1, which is not a valid fraction; so Ls and Rs are
      --    scaled so that the division cannot overflow.
      Ls := scale_down(fraction_word(L), 3);
      Rs := scale_down(fraction_word(R), 1);
      N := abs as_fraction(Ls);  -- Ls is scaled down by 1/8, so "abs" cannot overflow.
      D := abs as_fraction(Rs);  -- Rs is scaled down by 1/2, so "abs" cannot overflow.
      -- E is increased by 2 to compensate the quotient's scaling by 1/4.
      E := scaler(L) - scaler(R) + 2;
      F := as_word(N / D);  -- "/" cannot overflow here.
      if resign(KDF9.word(L) xor KDF9.word(R)) < 0 then
         -- The result is negative.
         F := -F;
      end if;
      return normalized(full_fraction => F, scaler => E);
   end "/";

   overriding
   function "<" (L, R : CPU.f48)
   return Boolean is
      s : constant KDF9.word := KDF9.word(L) xor KDF9.word(R);
   begin
      if resign(s) < 0 then
         -- The signs differ: L<R iff L is negative.
         return resign(KDF9.word(L)) < 0;
      elsif resign(KDF9.word(L)) < 0 then
         -- L and R are both negative, so invert lexicographical order.
         return not (KDF9.word(L) < KDF9.word(R));
      else
         -- L and R are both non-negative: so use lexicographical order.
         return KDF9.word(L) < KDF9.word(R);
      end if;
   end "<";

   function fraction_pair (DF : CPU.f96)
   return KDF9.pair
   is (scale_up((msw => scale_down(fraction_word(DF.msw), 8), lsw => fraction_word(DF.lsw)), 8));

   function scaler (DF : CPU.f96)
   return KDF9.word
   is (scaler(DF.msw));

   function narrowed (DF : CPU.f96)
   return CPU.f48 is
      fraction : KDF9.pair := fraction_pair(DF) + (0, 2**46);
   begin
      reconstruct(fraction, scaler(DF));
      return CPU.f48(fraction.msw);
   end narrowed;

   procedure reconstruct (frac   : in out KDF9.pair;
                          scaler : in KDF9.word) is
      KDF9_exponent :  KDF9.word := scaler + 128;
      normalizer    : Natural;
   begin
      if (frac.msw or frac.lsw) = 0 then
         return; -- frac is already normalized.
      end if;

      normalizer := nr_leading_zeros(frac.msw);

      if normalizer = 47 then  -- frac.msw is zero, so frac.lsw is non-zero.
         normalizer := 47 + nr_leading_zeros(frac.lsw);
      end if;

      KDF9_exponent := KDF9_exponent - KDF9.word(normalizer);

      frac := scale_up(frac, normalizer);
      the_CPU_delta := the_CPU_delta + shift_time(normalizer);

      -- 96-bit shift_arithmetic does not round and so cannot overflow here.
      frac := scale_down(frac, 8);
      frac.lsw := scale_down(frac.lsw, 8);
      -- Clear both scaler fields.
      frac.msw := frac.msw and mantissa_mask;
      frac.lsw := frac.lsw and mantissa_mask;
      if resign(KDF9_exponent) < 0 then
         -- Deal with underflow.
         frac := (0, 0);
         return;
      elsif KDF9_exponent > 255 then
         -- Deal with overflow.
         the_V_bit_is_set := True;
         KDF9_exponent := 255;
      end if;

      frac.msw := frac.msw or shift_word_left(KDF9_exponent and 8#377#, 39);
      if KDF9_exponent < 39 then
         frac.lsw := 0;
      else
         frac.lsw := frac.lsw or shift_word_left((KDF9_exponent-39) and 8#377#, 39);
      end if;
   end reconstruct;

   function "-" (R : CPU.f96)
   return CPU.f96
   is (CPU.f96'(0, 0) - R);

   function "+" (L, R : CPU.f96)
   return CPU.f96 is
      -- Scale fractions to prevent overflow; must adjust exponent accordingly.
      L_exponent : constant KDF9.word := scaler(L);
      R_exponent : constant KDF9.word := scaler(R);
      L_fraction : KDF9.pair := scale_down(fraction_pair(L), 1);
      R_fraction : KDF9.pair := scale_down(fraction_pair(R), 1);
      exponent   : KDF9.word;
      the_result : KDF9.pair;
      aligner    : Natural;
   begin
      if resign(R_exponent) >= resign(L_exponent) then
         aligner := Natural(resign(R_exponent-L_exponent));
         aligner := Natural'Min(95, aligner);
         L_fraction := scale_down(L_fraction, aligner);
         exponent := R_exponent + 1;
      else
         aligner := Natural(resign(L_exponent-R_exponent));
         aligner := Natural'Min(95, aligner);
         R_fraction := scale_down(R_fraction, aligner);
         exponent := L_exponent + 1;
      end if;
      the_CPU_delta := the_CPU_delta + shift_time(aligner);
      the_result := L_fraction + R_fraction;  -- "+" cannot overflow here.
      reconstruct(the_result, scaler => exponent);
      return as_f96(the_result);
   end "+";

   function "-" (L, R : CPU.f96)
   return CPU.f96 is
      -- See "+".
      L_exponent : constant KDF9.word := scaler(L);
      R_exponent : constant KDF9.word := scaler(R);
      L_fraction : KDF9.pair := scale_down(fraction_pair(L), 1);
      R_fraction : KDF9.pair := scale_down(fraction_pair(R), 1);
      exponent   : KDF9.word;
      the_result : KDF9.pair;
      aligner    : Natural;
   begin
      if resign(R_exponent) >= resign(L_exponent) then
         aligner := Natural(resign(R_exponent-L_exponent));
         aligner := Natural'Min(95, aligner);
         L_fraction := scale_down(L_fraction, aligner);
         exponent := R_exponent + 1;
      else
         aligner := Natural(resign(L_exponent-R_exponent));
         aligner := Natural'Min(95, aligner);
         R_fraction := scale_down(R_fraction, aligner);
         exponent := L_exponent + 1;
      end if;
      the_CPU_delta := the_CPU_delta + shift_time(aligner);
      the_result := L_fraction - R_fraction;  -- "-" cannot overflow here.
      reconstruct(the_result, scaler => exponent);
      return as_f96(the_result);
   end "-";

   function "*" (L, R : CPU.f48)
   return CPU.f96 is
      V_was_set : constant Boolean := the_V_bit_is_set;
      LR        : KDF9.pair;
   begin
      the_V_bit_is_set := False;
      LR := fraction_word(L) * fraction_word(R);
      if the_V_bit_is_set then
         -- The product is not a valid fixed-point fraction, but is actually OK,
         --    so restore the orginal overflow state, and  ...
         the_V_bit_is_set := V_was_set;
         --  ... construct +1.0 in double-precision floating-point.
         return as_f96((shift_word_left(2#0_10_000_001_1#, 38), 0));
      end if;
      reconstruct(LR, scaler => scaler(L) + scaler(R));
      return as_f96(LR);
   end "*";

   function "/" (L : CPU.f96;
                 R : CPU.f48)
   return CPU.f48 is  -- aka DIVDF
      -- If L>=R, L/R>= 1, which is not a valid fraction; so Ls and Rs are
      --    scaled so that the division cannot overflow.
      Ls     : constant KDF9.pair := scale_down(fraction_pair(L), 3);
      Rs     : constant KDF9.word := scale_down(fraction_word(R), 1);
      -- E is increased by 2 to compensate the quotient's scaling by 1/4.
      E      : constant KDF9.word := scaler(L) - scaler(R) + 2;
      F      : KDF9.word;
   begin
      if R = 0 then
         the_V_bit_is_set := True;
         return L.msw;  -- ?? This result is not well defined in the Manual.
      end if;
      do_DIVD(Ls, Rs, F);  -- Division cannot overflow here.
      return normalized(full_fraction => F, scaler => E);
   end "/";

   procedure push (F : in CPU.f48) is
   begin
      push(KDF9.word(F));
   end push;

   function pop
   return CPU.f48
   is (CPU.f48(KDF9.word'(pop)));

   function read_top
   return CPU.f48
   is (CPU.f48(KDF9.word'(read_top)));

   procedure write_top (F : in CPU.f48) is
   begin
      write_top(KDF9.word(F));
   end write_top;

   procedure push (DF : in CPU.f96) is
      AB : constant KDF9.pair := as_pair(DF);
   begin
      push(AB);
   end push;

   function pop
   return CPU.f96
   is (as_f96(pop));

   function read_top
   return CPU.f96
   is (as_f96(read_top));

   procedure write_top (DF : in CPU.f96) is
      AB : constant KDF9.pair := as_pair(DF);
   begin
      write_top(AB);
   end write_top;

end KDF9.CPU;
