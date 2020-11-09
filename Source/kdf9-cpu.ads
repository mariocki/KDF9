-- kdf9-cpu.ads
--
-- Support for KDF9 CPU/ALU operations that are not automatically inherited from
--   Ada types; and for types used in the internal functioning of the microcode.
--
-- This file is part of ee9 (V2.0r), the GNU Ada emulator of the English Electric KDF9.
-- Copyright (C) 2015, W. Findlay; all rights reserved.
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

with Ada.Unchecked_Conversion;

package KDF9.CPU is

   --
   -- 48-bit integer and fractional ALU
   --

   type signed is range -2**47 .. +2**47 - 1;
   for  signed'Size use KDF9.word'Size;

   function unsign is new Ada.Unchecked_Conversion (CPU.signed, KDF9.word);

   function resign is new Ada.Unchecked_Conversion (KDF9.word, CPU.signed);

   function "-" (I : CPU.signed)
   return KDF9.word;
   pragma Inline("-");

   function "abs" (I : CPU.signed)
   return KDF9.word;
   pragma Inline("abs");

   function "+" (L, R : CPU.signed)
   return KDF9.word;
   pragma Inline("+");

   function "-" (L, R : CPU.signed)
   return KDF9.word;
   pragma Inline("-");

   function "*" (L, R : CPU.signed)
   return KDF9.word;

   -- Determine the Quotient and Remainder of L/R, where:
   --    sign(Remainder) = sign(R) and |Remainder| < |R|, i.e. Remainder = L mod R;
   --    Quotient = (L - Remainder) / R.
   procedure do_DIVI (L : in KDF9.word;
                      R : in KDF9.word;
                      Quotient, Remainder : out KDF9.word);

   -- Signed single-length integer division is removed from consideration.
   function "/" (L, R : CPU.signed)
   return KDF9.word is abstract;

   function "mod" (L, R : CPU.signed)
   return KDF9.word is abstract;

   -- Contract a double-word, setting the V bit if necessary.
   function contracted (P : KDF9.pair)
   return KDF9.word;
   pragma Inline(contracted);

   -- Contract a double-word, represented by its components, setting the V bit if necessary.
   function contracted (msw, lsw : KDF9.word)
   return KDF9.word;
   pragma Inline(contracted);

   -- KDF9-semantics shifting operations.

   type signed_Q_part is range  -2**15 .. +2**15 - 1;
   for  signed_Q_part'Size use KDF9.Q_part'Size;

   function resign is new Ada.Unchecked_Conversion (KDF9.Q_part, CPU.signed_Q_part);

   -- L>0 for left-shift, L<0 for right two_shift.

   function shift_logical (W : KDF9.word; L : CPU.signed_Q_part)
   return KDF9.word;
   pragma Inline(shift_logical);

   function shift_circular (W : KDF9.word; L : CPU.signed_Q_part)
   return KDF9.word;
   pragma Inline(shift_circular);

   -- shift_arithmetic rounds the result correctly.
   function shift_arithmetic (I : KDF9.word; L : CPU.signed_Q_part)
   return KDF9.word;
   pragma Inline(shift_arithmetic);

   -- cardinality yields the number of 1-bits in W.
   function cardinality (W : KDF9.word)
   return KDF9.word;
   pragma Inline(cardinality);

   -- A fraction is a word W interpreted as the value W / 2**47;

   KDF9_small : constant := 2.0**(-47);
   type fraction is delta KDF9_small range -1.0 .. +1.0 - KDF9_small;
   for fraction'Size use KDF9.word'Size;

   function fractional is new Ada.Unchecked_Conversion (KDF9.word, CPU.fraction);

   function integral is new Ada.Unchecked_Conversion (CPU.fraction, KDF9.word);

   -- These operations treat the KDF9.word operands as full-word fractions,

   function "*" (L, R : KDF9.word)
   return CPU.fraction;

   function "/" (L, R : KDF9.word)
   return CPU.fraction;


   --
   -- double-word (95- and 96-bit) ALU
   --

   function "+" (L, R : KDF9.pair)
   return KDF9.pair;
   pragma Inline("+");

   function "-" (J : KDF9.pair)
   return KDF9.pair;
   pragma Inline("-");

   function "-" (L, R : KDF9.pair)
   return KDF9.pair;
   pragma Inline("-");

   -- 48 * 48 -> 96-bit, for XD, etc.
   function "*" (L, R : KDF9.word)
   return KDF9.pair;

   -- 96 / 48 -> 48-bit, for DIVD, DIVR and DIVDF.
   procedure do_DIVD (L : in KDF9.pair;
                      R : in KDF9.word;
                      Q : out KDF9.word;
                      round : in Boolean := True
                     );

   procedure do_DIVR (L : in KDF9.pair;
                      R : in KDF9.word;
                      Quotient, Remainder : out KDF9.word
                     );

   function shift_logical (P : KDF9.pair; L : CPU.signed_Q_part)
   return KDF9.pair;
   pragma Inline(shift_logical);

   function shift_arithmetic (P : KDF9.pair; L : CPU.signed_Q_part)
   return KDF9.pair;
   pragma Inline(shift_arithmetic);


   --
   -- 48-bit floating point ALU
   --

   type float is mod 2**48;  -- This is a substrate for KDF9 f.p., not an Ada f.p. type.
   for  float'Size use KDF9.word'Size;

   -- Remove useless substrate modular operations.

   overriding
   function "not" (R : CPU.float)
   return CPU.float is abstract;

   overriding
   function "and" (L, R : CPU.float)
   return CPU.float is abstract;

   overriding
   function "or" (L, R : CPU.float)
   return CPU.float is abstract;

   overriding
   function "xor" (L, R : CPU.float)
   return CPU.float is abstract;

   overriding
   function "mod" (L, R : CPU.float)
   return CPU.float is abstract;

   function as_word is new Ada.Unchecked_Conversion (CPU.float, KDF9.word);

   function as_float is new Ada.Unchecked_Conversion (KDF9.word, CPU.float);

   procedure push (F : in CPU.float);
   pragma Inline(push);

   function pop
   return CPU.float;
   pragma Inline(pop);

   procedure write_top (F : in CPU.float);
   pragma Inline(write_top);

   function read_top
   return CPU.float;
   pragma Inline(read_top);

    -- Standardize a (possibly) non-normalized floating-point number.
   function normalized  (R : CPU.float)
   return CPU.float;

   -- Convert a 47-bit fraction to a rounded, standardized 39-bit mantissa,
   --    and adjust its exponent accordingly.
   procedure normalize (fraction, exponent : in out KDF9.word);
   pragma Inline(normalize);

   -- Convert a 39-bit mantissa to a 47-bit fraction, preserving the sign.
   function fraction_word (mantissa : CPU.float)
   return KDF9.word;
   pragma Inline(fraction_word);

   -- The floating-point number with the exponent field set to 0.
   function masked_mantissa (F : CPU.float)
   return CPU.float;
   pragma Inline(masked_mantissa);

   -- The algebraic scale-factor,  not the hardware exponent, -128 <= scaler < +128.
   function scaler (F : CPU.float)
   return KDF9.word;
   pragma Inline(scaler);

    -- Synthesize a normalized floating-point number from its components.
   function normalized (full_fraction, scaler : KDF9.word)
   return CPU.float;
   pragma Inline(normalized);

   -- Round a 48-bit floating-point number to 24-bit format.
   function rounded (R : CPU.float)
   return CPU.float;

   overriding
   function "-" (R : CPU.float)
   return CPU.float;

   overriding
   function "abs" (R : CPU.float)
   return CPU.float;

   overriding
   function "+" (L, R : CPU.float)
   return CPU.float;

   overriding
   function "-" (L, R : CPU.float)
   return CPU.float;

   overriding
   function "*" (L, R : CPU.float)
   return CPU.float;

   overriding
   function "/" (L, R : CPU.float)
   return CPU.float;

   overriding
   function "<" (L, R : CPU.float)
   return Boolean;

   function host_float (X : CPU.float)
   return Long_Float;

   function KDF9_float (X : Long_Float)
   return CPU.float;

   exponent_mask : constant KDF9.word := KDF9.word'(2#11_111_111#) * 2**39;
   mantissa_mask : constant KDF9.word := not exponent_mask;
   frac_msb_mask : constant KDF9.word := 2**46;  -- M.S.B. of a 47-bit fraction
   mant_msb_mask : constant KDF9.word := 2**38;  -- M.S.B. of a 39-bit mantissa
   overflow_mask : constant KDF9.word := 2**39;  -- bit set on rounding overflow


   --
   -- 96-bit floating point ALU
   --

   type double is
      record
         msw, lsw : CPU.float;
      end record;

   function as_pair is new Ada.Unchecked_Conversion (CPU.double, KDF9.pair);

   function as_double is new Ada.Unchecked_Conversion (KDF9.pair, CPU.double);

   procedure push (DF : in CPU.double);
   pragma Inline(push);

   function pop
   return CPU.double;
   pragma Inline(pop);

   procedure write_top (DF : in CPU.double);
   pragma Inline(write_top);

   function read_top
   return CPU.double;
   pragma Inline(read_top);

   -- The algebraic scale-factor,  not the hardware exponent, -128 <= scaler < +128.
   function scaler (DF : CPU.double)
   return KDF9.word;
   pragma Inline(scaler);

   -- Round a 96-bit double-precision floating-point number to 48 bit format.
   function rounded (DF : CPU.double)
   return CPU.float;
   pragma Inline(rounded);

   -- Derive a 96-bit fraction from the double-precision floating-point number,
   --    with the mantissa bits in D9-D47 and D49-D87,
   --       and with D1-D8 copies of the sign, D48 zero, and D87-D95 zero.
   function fraction_pair (DF : CPU.double)
   return KDF9.pair;
   pragma Inline(fraction_pair);

   -- Convert 96-bit fraction, and an algebraic scale-factor exponent,
   --    into a 96-bit floating point number, setting overflow when necessary.
   procedure reconstruct (frac   : in out KDF9.pair;
                          scaler : in KDF9.word);

   function "-" (R : CPU.double)
   return CPU.double;

   function "+" (L, R : CPU.double)
   return CPU.double;

   function "-" (L, R : CPU.double)
   return CPU.double;

   function "*" (L, R : CPU.float)
   return CPU.double;

   function "/" (L : CPU.double;
                 R : CPU.float)
   return CPU.float;

   function host_double (X : CPU.double)
   return Long_Float;

   function KDF9_double (X : Long_Float)
   return CPU.double;


   --
   -- These are the emulation host's register types and their operations.
   --

   type u_64 is mod 2**64;
   for  u_64'Size use 64;

   function as_word (u : CPU.u_64)
   return KDF9.word;
   pragma Inline(as_word);

   function rotate_right (u : CPU.u_64; amount : Natural)
   return CPU.u_64;

   function shift_right (u : CPU.u_64; amount : Natural)
   return CPU.u_64;

   function shift_right_arithmetic (u : CPU.u_64; amount : Natural)
   return CPU.u_64;

   function rotate_left (u : CPU.u_64; amount : Natural)
   return CPU.u_64;

   function shift_left (u : CPU.u_64; amount : Natural)
   return CPU.u_64;

   pragma Import(Intrinsic, rotate_left);
   pragma Import(Intrinsic, rotate_right);
   pragma Import(Intrinsic, shift_left);
   pragma Import(Intrinsic, shift_right);
   pragma Import(Intrinsic, shift_right_arithmetic);

   type s_64 is range -2**63 .. +2**63-1;
   for  s_64'Size use CPU.u_64'Size;

   -- The signed as_word sets the V bit if necessary.
   function as_word (s : CPU.s_64)
   return KDF9.word;
   pragma Inline(as_word);

   function unsign is new Ada.Unchecked_Conversion(CPU.s_64, CPU.u_64);

   function resign is new Ada.Unchecked_Conversion(CPU.u_64, CPU.s_64);


   --
   -- These are KDF9's 48-bit primitive, fixed-direction, shift operations.
   --

   function shift_time (amount : Natural)
   return KDF9.microseconds;
   pragma Inline(shift_time);

   subtype word_shift_length is Natural range 0..48;

   function shift_word_left (W : KDF9.word; amount : word_shift_length)
   return KDF9.word;
   pragma Inline(shift_word_left);

   function shift_word_right (W : KDF9.word; amount : word_shift_length)
   return KDF9.word;
   pragma Inline(shift_word_right);

   function rotate_word_left (W : KDF9.word; amount : word_shift_length)
   return KDF9.word;
   pragma Inline(rotate_word_left);

   function rotate_word_right (W : KDF9.word; amount : word_shift_length)
   return KDF9.word;
   pragma Inline(rotate_word_right);

   -- scale_up may set the V bit.
   function scale_up (W : KDF9.word; amount : Natural)
   return KDF9.word;
   pragma Inline(scale_up);

   -- scale_down_and_round rounds correctly.
   function scale_down_and_round (W : KDF9.word; amount : Natural)
   return KDF9.word;
   pragma Inline(scale_down_and_round);

   -- scale_down never rounds.
   function scale_down (W : KDF9.word; amount : Natural)
   return KDF9.word;
   pragma Inline(scale_down);

end KDF9.CPU;
